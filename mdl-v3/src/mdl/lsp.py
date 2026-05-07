from __future__ import annotations

import json
import sys
from typing import Any, BinaryIO

from .linter import lint_source


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
        elif id_ is not None:
            self.response(id_, error={"code": -32601, "message": f"unsupported method {method}"})

    def publish_diagnostics(self, uri: str, text: str) -> None:
        diagnostics = [d.to_lsp() for d in lint_source(text, path=uri)]
        self.notification("textDocument/publishDiagnostics", {"uri": uri, "diagnostics": diagnostics})


def run_stdio() -> int:
    return LSPServer().run()
