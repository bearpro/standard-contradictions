from __future__ import annotations

import json
import re
import sys
from typing import Any, BinaryIO

from . import ast as A
from .lexer import KEYWORDS
from .linter import ImportResolver, SemanticChecker, lint_source
from .loader import is_python_dsl_path
from .lsp_model import EditorSnapshot, SEMANTIC_TOKEN_MODIFIERS, SEMANTIC_TOKEN_TYPES
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
                    "documentSymbolProvider": True,
                    "hoverProvider": True,
                    "definitionProvider": True,
                    "semanticTokensProvider": {
                        "legend": {
                            "tokenTypes": SEMANTIC_TOKEN_TYPES,
                            "tokenModifiers": SEMANTIC_TOKEN_MODIFIERS,
                        },
                        "full": True,
                    },
                },
                "serverInfo": {"name": "mdl", "version": "0.0.0"},
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
            self.response(
                id_,
                self.completion_items(
                    text,
                    int(position.get("line", 0)),
                    int(position.get("character", 0)),
                    uri=uri,
                ),
            )
        elif method == "textDocument/documentSymbol":
            doc = params.get("textDocument", {})
            uri = doc.get("uri", "")
            self.response(id_, [] if is_python_dsl_path(uri) else self.snapshot(uri).document_symbols())
        elif method == "textDocument/hover":
            doc = params.get("textDocument", {})
            uri = doc.get("uri", "")
            if is_python_dsl_path(uri):
                self.response(id_, None)
                return
            position = params.get("position", {})
            self.response(
                id_,
                self.snapshot(uri).hover(
                    int(position.get("line", 0)),
                    int(position.get("character", 0)),
                ),
            )
        elif method == "textDocument/definition":
            doc = params.get("textDocument", {})
            uri = doc.get("uri", "")
            if is_python_dsl_path(uri):
                self.response(id_, None)
                return
            position = params.get("position", {})
            self.response(
                id_,
                self.snapshot(uri).definition(
                    int(position.get("line", 0)),
                    int(position.get("character", 0)),
                ),
            )
        elif method == "textDocument/semanticTokens/full":
            doc = params.get("textDocument", {})
            uri = doc.get("uri", "")
            self.response(id_, {"data": []} if is_python_dsl_path(uri) else self.snapshot(uri).semantic_tokens())
        elif method == "mdl/modelSummary":
            doc = params.get("textDocument", {})
            uri = doc.get("uri", "")
            self.response(id_, {"module": None, "diagnostics": []} if is_python_dsl_path(uri) else self.snapshot(uri).model_summary())
        elif id_ is not None:
            self.response(id_, error={"code": -32601, "message": f"unsupported method {method}"})

    def snapshot(self, uri: str) -> EditorSnapshot:
        return EditorSnapshot(self.documents.get(uri, ""), uri, self.documents)

    def publish_diagnostics(self, uri: str, text: str) -> None:
        diagnostics = [d.to_lsp() for d in lint_source(text, path=uri, documents=self.documents)]
        self.notification("textDocument/publishDiagnostics", {"uri": uri, "diagnostics": diagnostics})

    def completion_items(
        self,
        text: str,
        line: int,
        character: int,
        *,
        uri: str | None = None,
    ) -> list[dict[str, Any]]:
        prefix = self.line_prefix(text, line, character)
        if is_python_dsl_path(uri):
            return []
        field_target = self.field_completion_target(prefix)
        record_target = self.record_constructor_completion_target(prefix)
        try:
            module = parse(text)
        except ParseError:
            repaired = self.repair_completion_source(text, line, character, prefix)
            if repaired is None:
                if self.is_type_context(prefix):
                    return self.primitive_type_items()
                return self.keyword_items()
            try:
                module = parse(repaired)
            except ParseError:
                if self.is_type_context(prefix):
                    return self.primitive_type_items()
                return self.keyword_items()
        checker = SemanticChecker(module, uri, resolver=ImportResolver(uri, self.documents))
        checker.check()
        lsp_line = line + 1
        lsp_column = character + 1
        if field_target:
            fields = (
                checker.fields_for_reference(field_target)
                or self.local_fields_for_reference(module, checker, field_target, lsp_line)
                or {}
            )
            return [
                {"label": name, "kind": 5, "detail": "field"}
                for name in sorted(fields)
            ]
        if record_target:
            fields = checker.fields_for_type(A.TypeRef(name=record_target)) or {}
            return [
                {"label": name, "kind": 5, "detail": "field"}
                for name in sorted(fields)
            ]
        if self.is_type_context(prefix):
            return [
                {"label": name, "kind": 7, "detail": "type"}
                for name in checker.visible_type_names(lsp_line, lsp_column)
            ]
        return self.with_keywords([
            {"label": name, "kind": self.completion_kind(symbol.kind), "detail": symbol.kind}
            for name, symbol in sorted(checker.terms.items())
            if checker.is_visible(symbol, lsp_line, lsp_column) and "." not in name
        ])

    def line_prefix(self, text: str, line: int, character: int) -> str:
        lines = text.splitlines()
        if line < 0 or line >= len(lines):
            return ""
        return lines[line][:max(0, character)]

    def field_completion_target(self, prefix: str) -> str | None:
        parenthesized = re.search(r"\(([A-Za-z_][A-Za-z0-9_']*(?:\.[A-Za-z_][A-Za-z0-9_']*)*)\)\.[A-Za-z0-9_']*$", prefix)
        if parenthesized:
            return parenthesized.group(1)
        match = re.search(r"([A-Za-z_][A-Za-z0-9_']*(?:\.[A-Za-z_][A-Za-z0-9_']*)*)\.[A-Za-z0-9_']*$", prefix)
        return match.group(1) if match else None

    def local_fields_for_reference(
        self,
        module: A.Module,
        checker: SemanticChecker,
        name: str,
        line: int,
    ) -> dict[str, A.TypeExpr] | None:
        for decl in module.declarations:
            if decl.line > line:
                continue
            typ = None
            if isinstance(decl, A.FuncDecl):
                env: dict[str, A.TypeExpr | None] = {}
                for param in decl.params:
                    checker.bind_pattern(param.pattern, env, param.type_annotation)
                typ = self.local_reference_type_in_block(checker, decl.body, name, line, env)
            elif isinstance(decl, A.ValueDecl):
                typ = self.local_reference_type_in_expr(checker, decl.value, name, line, {})
            elif isinstance(decl, A.FactDecl):
                typ = self.local_reference_type_in_expr(checker, decl.value, name, line, {})
            elif isinstance(decl, A.RuleDecl):
                typ = (
                    self.local_reference_type_in_expr(checker, decl.antecedent, name, line, {})
                    or self.local_reference_type_in_expr(checker, decl.body, name, line, {})
                    or self.local_reference_type_in_expr(checker, decl.otherwise, name, line, {})
                )
            fields = checker.fields_for_type(typ)
            if fields:
                return fields
        return None

    def local_reference_type_in_block(
        self,
        checker: SemanticChecker,
        block: A.Block | None,
        name: str,
        line: int,
        env: dict[str, A.TypeExpr | None],
    ) -> A.TypeExpr | None:
        if block is None:
            return checker.infer_name_type(name, env)
        local = dict(env)
        for stmt in block.statements:
            if stmt.line > line:
                break
            nested = self.local_reference_type_in_expr(checker, stmt.value, name, line, local)
            if nested is not None:
                return nested
            checker.bind_pattern(stmt.pattern, local, stmt.type_annotation or checker.infer_expr_type(stmt.value, local))
        nested = self.local_reference_type_in_expr(checker, block.result, name, line, local)
        return nested or checker.infer_name_type(name, local)

    def local_reference_type_in_expr(
        self,
        checker: SemanticChecker,
        expr: A.Expr | None,
        name: str,
        line: int,
        env: dict[str, A.TypeExpr | None],
    ) -> A.TypeExpr | None:
        if expr is None:
            return None
        if isinstance(expr, A.MatchExpr):
            subject_type = checker.infer_expr_type(expr.subject, env)
            for idx, arm in enumerate(expr.arms):
                next_line = expr.arms[idx + 1].line if idx + 1 < len(expr.arms) else 10**9
                if arm.line <= line < next_line:
                    local = dict(env)
                    checker.bind_pattern(arm.pattern, local, subject_type)
                    guard_type = self.local_reference_type_in_expr(checker, arm.guard, name, line, local)
                    if guard_type is not None:
                        return guard_type
                    return self.local_reference_type_in_block(checker, arm.body, name, line, local)
            return None
        if isinstance(expr, A.LetExpr) and expr.line <= line:
            local = dict(env)
            checker.bind_pattern(expr.pattern, local, checker.infer_expr_type(expr.value, env))
            return self.local_reference_type_in_expr(checker, expr.body, name, line, local) or checker.infer_name_type(name, local)
        return checker.infer_name_type(name, env)

    def record_constructor_completion_target(self, prefix: str) -> str | None:
        match = re.search(r"(?:^|[\s:=,(])([A-Za-z_][A-Za-z0-9_']*(?:\.[A-Za-z_][A-Za-z0-9_']*)*)\s*\{[^{}]*$", prefix)
        return match.group(1) if match else None

    def repair_completion_source(self, text: str, line: int, character: int, prefix: str) -> str | None:
        insertion = None
        if self.field_completion_target(prefix):
            insertion = "__completion__"
        elif self.record_constructor_completion_target(prefix):
            tail = prefix.rsplit("{", 1)[1].rsplit(",", 1)[-1]
            if "=" in tail:
                insertion = "() }"
            elif tail.strip():
                insertion = " = () }"
            else:
                insertion = "__completion__ = () }"
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
        if re.search(r"\b(entity|let)\s+[A-Za-z_][A-Za-z0-9_']*\s*:\s*[A-Za-z0-9_'.]*$", stripped):
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
        if kind == "constructor":
            return 4
        return 6

    def keyword_items(self) -> list[dict[str, Any]]:
        return [
            {"label": keyword, "kind": 14, "detail": "keyword"}
            for keyword in sorted(KEYWORDS)
        ]

    def primitive_type_items(self) -> list[dict[str, Any]]:
        return [
            {"label": name, "kind": 7, "detail": "type"}
            for name in ["bool", "decimal", "int", "rat", "string", "unit"]
        ]

    def with_keywords(self, items: list[dict[str, Any]]) -> list[dict[str, Any]]:
        seen = {item["label"] for item in items}
        return items + [item for item in self.keyword_items() if item["label"] not in seen]


def run_stdio() -> int:
    return LSPServer().run()
