from __future__ import annotations

import json
import re
import sys
from typing import Any, BinaryIO

from .linter import SemanticChecker, lint_source
from .parser import ParseError, parse


class LSPServer:
    """Very small stdio LSP server.

    It supports initialize/shutdown plus didOpen/didChange diagnostics. This keeps the project
    dependency-free while giving editors a real LSP endpoint through `mdl lsp`.
    """

    def __init__(self, stdin: BinaryIO | None = None, stdout: BinaryIO | None = None):
        self.stdin = stdin or sys.stdin.buffer
        self.stdout = stdout or sys.stdout.buffer
        self.documents: dict[str, str] = {}
        self.shutdown_requested = False

    def run(self) -> int:
        while True:
            message = self.read_message()
            if message is None:
                break
            self.handle(message)
            if self.shutdown_requested and message.get("method") == "exit":
                break
        return 0

    def read_message(self) -> dict[str, Any] | None:
        headers: dict[str, str] = {}
        while True:
            line = self.stdin.readline()
            if not line:
                return None
            line = line.rstrip(b"\r\n")
            if not line:
                break
            key, _, value = line.decode("utf-8").partition(":")
            headers[key.lower()] = value.strip()
        length = int(headers.get("content-length", "0"))
        if length <= 0:
            return None
        payload = self.stdin.read(length)
        return json.loads(payload.decode("utf-8"))

    def send(self, payload: dict[str, Any]) -> None:
        data = json.dumps(payload, ensure_ascii=False, separators=(",", ":")).encode("utf-8")
        self.stdout.write(f"Content-Length: {len(data)}\r\n\r\n".encode("ascii"))
        self.stdout.write(data)
        self.stdout.flush()

    def response(self, id_: Any, result: Any = None, error: Any = None) -> None:
        payload = {"jsonrpc": "2.0", "id": id_}
        if error is not None:
            payload["error"] = error
        else:
            payload["result"] = result
        self.send(payload)

    def notification(self, method: str, params: Any) -> None:
        self.send({"jsonrpc": "2.0", "method": method, "params": params})

    def handle(self, message: dict[str, Any]) -> None:
        method = message.get("method")
        id_ = message.get("id")
        params = message.get("params") or {}
        if method == "initialize":
            self.response(id_, {
                "capabilities": {
                    "textDocumentSync": 1,
                    "diagnosticProvider": False,
                    "completionProvider": {
                        "triggerCharacters": [".", ":", " "],
                    },
                },
                "serverInfo": {"name": "mdl", "version": "0.1.0"},
            })
        elif method == "initialized":
            return
        elif method == "shutdown":
            self.shutdown_requested = True
            self.response(id_, None)
        elif method == "exit":
            self.shutdown_requested = True
        elif method == "textDocument/didOpen":
            doc = params.get("textDocument", {})
            uri = doc.get("uri", "")
            text = doc.get("text", "")
            self.documents[uri] = text
            self.publish_diagnostics(uri, text)
        elif method == "textDocument/didChange":
            doc = params.get("textDocument", {})
            uri = doc.get("uri", "")
            changes = params.get("contentChanges", [])
            if changes:
                self.documents[uri] = changes[-1].get("text", self.documents.get(uri, ""))
            self.publish_diagnostics(uri, self.documents.get(uri, ""))
        elif method == "textDocument/completion":
            doc = params.get("textDocument", {})
            uri = doc.get("uri", "")
            position = params.get("position", {})
            text = self.documents.get(uri, "")
            self.response(id_, self.completion_items(text, int(position.get("line", 0)), int(position.get("character", 0))))
        elif id_ is not None:
            self.response(id_, error={"code": -32601, "message": f"unsupported method {method}"})

    def publish_diagnostics(self, uri: str, text: str) -> None:
        diagnostics = [d.to_lsp() for d in lint_source(text, path=uri)]
        self.notification("textDocument/publishDiagnostics", {"uri": uri, "diagnostics": diagnostics})

    def completion_items(self, text: str, line: int, character: int) -> list[dict[str, Any]]:
        prefix = self.line_prefix(text, line, character)
        field_target = self.field_completion_target(prefix)
        try:
            module = parse(text)
        except ParseError:
            repaired = self.repair_completion_source(text, line, character, prefix)
            if repaired is None:
                return []
            try:
                module = parse(repaired)
            except ParseError:
                return []
        checker = SemanticChecker(module)
        checker.check()
        if field_target:
            fields = checker.fields_for_reference(field_target) or {}
            return [
                {"label": name, "kind": 5, "detail": "field"}
                for name in sorted(fields)
            ]
        lsp_line = line + 1
        lsp_column = character + 1
        if self.is_type_context(prefix):
            return [
                {"label": name, "kind": 7, "detail": "type"}
                for name in checker.visible_type_names(lsp_line, lsp_column)
            ]
        return [
            {"label": name, "kind": self.completion_kind(symbol.kind), "detail": symbol.kind}
            for name, symbol in sorted(checker.terms.items())
            if checker.is_visible(symbol, lsp_line, lsp_column) and "." not in name
        ]

    def line_prefix(self, text: str, line: int, character: int) -> str:
        lines = text.splitlines()
        if line < 0 or line >= len(lines):
            return ""
        return lines[line][:max(0, character)]

    def field_completion_target(self, prefix: str) -> str | None:
        match = re.search(r"([A-Za-z_][A-Za-z0-9_']*(?:\.[A-Za-z_][A-Za-z0-9_']*)*)\.[A-Za-z0-9_']*$", prefix)
        return match.group(1) if match else None

    def repair_completion_source(self, text: str, line: int, character: int, prefix: str) -> str | None:
        insertion = None
        if self.field_completion_target(prefix):
            insertion = "__completion__"
        elif self.is_type_context(prefix):
            insertion = "unit"
        if insertion is None:
            return None
        return self.insert_at_position(text, line, character, insertion)

    def insert_at_position(self, text: str, line: int, character: int, insertion: str) -> str:
        lines = text.splitlines(keepends=True)
        if line < 0 or line >= len(lines):
            return text + insertion
        offset = sum(len(item) for item in lines[:line]) + min(character, len(lines[line]))
        return text[:offset] + insertion + text[offset:]

    def is_type_context(self, prefix: str) -> bool:
        stripped = prefix.strip()
        if re.search(r"\b(entity|val|let)\s+[A-Za-z_][A-Za-z0-9_']*\s*:\s*[A-Za-z0-9_'.]*$", stripped):
            return True
        if re.search(r"\bfunc\b.*(?:\(|,)\s*[A-Za-z_][A-Za-z0-9_']*\s*:\s*[A-Za-z0-9_'.]*$", stripped):
            return True
        if re.search(r"->\s*[A-Za-z0-9_'.]*$", stripped):
            return True
        if re.search(r"\btype\s+[A-Za-z_][A-Za-z0-9_']*(?:<[^>]*>)?\s*=\s*[A-Za-z0-9_'.]*$", stripped):
            return True
        return False

    def completion_kind(self, kind: str) -> int:
        if kind == "function":
            return 3
        if kind == "entity":
            return 6
        if kind == "event":
            return 3
        if kind == "constructor":
            return 4
        return 6


def run_stdio() -> int:
    return LSPServer().run()
