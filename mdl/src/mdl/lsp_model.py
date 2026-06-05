from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any
from urllib.parse import urlparse

from . import ast as A
from .core import CoreTranslator
from .diagnostics import Diagnostic
from .lexer import Token, tokenize
from .linter import ImportResolver, SemanticChecker, Symbol, lint_source
from .names import local_name, split_qualified
from .parser import ParseError, parse


SEMANTIC_TOKEN_TYPES = [
    "namespace",
    "type",
    "typeParameter",
    "function",
    "variable",
    "parameter",
    "property",
    "enumMember",
    "label",
]
SEMANTIC_TOKEN_MODIFIERS = ["declaration", "defaultLibrary"]


@dataclass(frozen=True)
class EditorSymbol:
    name: str
    kind: str
    node: A.Node
    type_expr: A.TypeExpr | None = None
    uri: str | None = None


@dataclass(frozen=True)
class SemanticToken:
    line: int
    column: int
    length: int
    token_type: str
    modifiers: tuple[str, ...] = ()


class EditorSnapshot:
    def __init__(self, text: str, uri: str | None, documents: dict[str, str]):
        self.text = text
        self.uri = uri
        self.documents = documents
        self.tokens: list[Token] = []
        self.module: A.Module | None = None
        self.checker: SemanticChecker | None = None
        self.diagnostics: list[Diagnostic] = []
        self.locals: list[EditorSymbol] = []
        self.module_paths: dict[str, str] = {}
        self._parse()

    def _parse(self) -> None:
        try:
            self.tokens = tokenize(self.text)
        except ParseError:
            self.tokens = []
        try:
            self.module = parse(self.text)
        except ParseError as exc:
            self.diagnostics = [exc.to_diagnostic(self.uri)]
            return
        resolver = ImportResolver(self.uri, self.documents)
        self.checker = SemanticChecker(self.module, self.uri, resolver=resolver)
        semantic_diagnostics = self.checker.check()
        self.diagnostics = lint_source(self.text, path=self.uri, documents=self.documents)
        self.module_paths = self.collect_module_paths(resolver)
        self.locals = self.collect_locals()
        self.enrich_diagnostic_ranges(semantic_diagnostics)

    def collect_module_paths(self, resolver: ImportResolver) -> dict[str, str]:
        paths: dict[str, str] = {}
        for resolved in resolver.stdlib_modules().values():
            if resolved.path:
                paths[resolved.module.name] = resolved.path
        for resolved in resolver.cache.values():
            if resolved and resolved.path:
                paths[resolved.module.name] = resolved.path
        for uri, text in self.documents.items():
            try:
                module = parse(text)
            except ParseError:
                continue
            paths.setdefault(module.name, uri)
        return paths

    def collect_locals(self) -> list[EditorSymbol]:
        if self.module is None:
            return []
        symbols: list[EditorSymbol] = []
        for decl in self.module.declarations:
            if isinstance(decl, A.FuncDecl):
                env: dict[str, A.TypeExpr | None] = {}
                for param in decl.params:
                    self.bind_pattern_symbols(param.pattern, param.type_annotation, "parameter", symbols)
                    self.bind_pattern_types(param.pattern, param.type_annotation, env)
                self.collect_block_locals(decl.body, env, symbols)
            elif isinstance(decl, A.RuleDecl):
                self.collect_expr_locals(decl.antecedent, {}, symbols)
                self.collect_expr_locals(decl.body, {}, symbols)
                self.collect_expr_locals(decl.otherwise, {}, symbols)
            elif isinstance(decl, A.ValueDecl):
                self.collect_expr_locals(decl.value, {}, symbols)
            elif isinstance(decl, A.FactDecl):
                self.collect_expr_locals(decl.value, {}, symbols)
            elif isinstance(decl, A.AssertDecl):
                self.collect_expr_locals(decl.expr, {}, symbols)
        return symbols

    def collect_block_locals(
        self,
        block: A.Block | None,
        env: dict[str, A.TypeExpr | None],
        symbols: list[EditorSymbol],
    ) -> None:
        if block is None:
            return
        local = dict(env)
        for stmt in block.statements:
            self.collect_expr_locals(stmt.value, local, symbols)
            typ = stmt.type_annotation
            self.bind_pattern_symbols(stmt.pattern, typ, "variable", symbols)
            self.bind_pattern_types(stmt.pattern, typ, local)
        self.collect_expr_locals(block.result, local, symbols)

    def collect_expr_locals(
        self,
        expr: A.Expr | None,
        env: dict[str, A.TypeExpr | None],
        symbols: list[EditorSymbol],
    ) -> None:
        if expr is None:
            return
        if isinstance(expr, A.LetExpr):
            self.collect_expr_locals(expr.value, env, symbols)
            typ = self.checker.infer_expr_type(expr.value, env) if self.checker else None
            local = dict(env)
            self.bind_pattern_symbols(expr.pattern, typ, "variable", symbols)
            self.bind_pattern_types(expr.pattern, typ, local)
            self.collect_expr_locals(expr.body, local, symbols)
            return
        if isinstance(expr, A.MatchExpr):
            self.collect_expr_locals(expr.subject, env, symbols)
            subject_type = self.checker.infer_expr_type(expr.subject, env) if self.checker else None
            for arm in expr.arms:
                local = dict(env)
                self.bind_pattern_symbols(arm.pattern, subject_type, "variable", symbols)
                self.bind_pattern_types(arm.pattern, subject_type, local)
                self.collect_expr_locals(arm.guard, local, symbols)
                self.collect_block_locals(arm.body, local, symbols)
            return
        if isinstance(expr, A.QuantifierExpr):
            self.collect_expr_locals(expr.domain, env, symbols)
            item_type = self.checker.collection_item_type(self.checker.infer_expr_type(expr.domain, env)) if self.checker else None
            local = dict(env)
            self.bind_pattern_symbols(expr.pattern, item_type, "parameter", symbols)
            self.bind_pattern_types(expr.pattern, item_type, local)
            self.collect_expr_locals(expr.body, local, symbols)
            return
        for child in self.child_exprs(expr):
            self.collect_expr_locals(child, env, symbols)

    def child_exprs(self, expr: A.Expr) -> list[A.Expr | None]:
        if isinstance(expr, A.Call):
            return [expr.func, *expr.args]
        if isinstance(expr, A.FieldAccess):
            return [expr.target]
        if isinstance(expr, A.IndexAccess):
            return [expr.target, expr.index]
        if isinstance(expr, A.BinaryOp):
            return [expr.left, expr.right]
        if isinstance(expr, A.UnaryOp):
            return [expr.operand]
        if isinstance(expr, A.IfExpr):
            return [expr.condition, expr.then_branch, expr.else_branch]
        if isinstance(expr, A.RecordConstructor):
            return [value for _, value in expr.fields]
        if isinstance(expr, A.TupleLiteral):
            return list(expr.items)
        if isinstance(expr, A.TemporalUnary):
            return [expr.operand]
        if isinstance(expr, A.TemporalBinary):
            return [expr.left, expr.right]
        return []

    def bind_pattern_symbols(
        self,
        pattern: A.Pattern | None,
        typ: A.TypeExpr | None,
        kind: str,
        symbols: list[EditorSymbol],
    ) -> None:
        if isinstance(pattern, A.VarPattern):
            symbols.append(EditorSymbol(pattern.name, kind, pattern, typ, self.uri))
        elif isinstance(pattern, A.RecordPattern):
            fields = self.checker.fields_for_type(typ) if self.checker else None
            for name, nested in pattern.fields:
                field_type = fields.get(name) if fields else None
                if nested is None:
                    symbols.append(EditorSymbol(name, kind, pattern, field_type, self.uri))
                else:
                    self.bind_pattern_symbols(nested, field_type, kind, symbols)
        elif isinstance(pattern, A.TuplePattern):
            items = typ.items if isinstance(typ, A.TupleType) else []
            for idx, item in enumerate(pattern.items):
                self.bind_pattern_symbols(item, items[idx] if idx < len(items) else None, kind, symbols)
        elif isinstance(pattern, A.ListPattern):
            item_type = self.checker.collection_item_type(typ) if self.checker else None
            for item in pattern.items:
                self.bind_pattern_symbols(item, item_type, kind, symbols)
        elif isinstance(pattern, A.ConstructorPattern):
            field_types = self.checker.constructor_field_types(pattern.name, typ) if self.checker else []
            for idx, item in enumerate(pattern.args):
                self.bind_pattern_symbols(item, field_types[idx] if idx < len(field_types) else None, kind, symbols)

    def bind_pattern_types(
        self,
        pattern: A.Pattern | None,
        typ: A.TypeExpr | None,
        env: dict[str, A.TypeExpr | None],
    ) -> None:
        if self.checker:
            self.checker.bind_pattern(pattern, env, typ)

    def enrich_diagnostic_ranges(self, diagnostics: list[Diagnostic]) -> None:
        by_key = {(d.line, d.column, d.code, d.message): d for d in diagnostics}
        for diagnostic in self.diagnostics:
            source = by_key.get((diagnostic.line, diagnostic.column, diagnostic.code, diagnostic.message))
            if source is not None and source.end_line is not None:
                object.__setattr__(diagnostic, "end_line", source.end_line)
                object.__setattr__(diagnostic, "end_column", source.end_column)

    def document_symbols(self) -> list[dict[str, Any]]:
        if self.module is None:
            return []
        children = [self.declaration_symbol(decl) for decl in self.module.declarations]
        return [{
            "name": self.module.name,
            "kind": 2,
            "range": self.node_range(self.module),
            "selectionRange": self.name_selection_range(self.module, self.module.name),
            "children": [item for item in children if item],
        }]

    def declaration_symbol(self, decl: A.Declaration) -> dict[str, Any] | None:
        name = A.declaration_name(decl)
        if not name and not isinstance(decl, (A.PriorityDecl, A.FactDecl, A.AssertDecl, A.AlignDecl)):
            return None
        if isinstance(decl, A.TypeDecl):
            kind, label = 5, decl.name
        elif isinstance(decl, A.FuncDecl):
            kind, label = 12, decl.name
        elif isinstance(decl, A.ValueDecl):
            kind, label = 13, decl.name
        elif isinstance(decl, A.EntityDecl):
            kind, label = 23, decl.name
        elif isinstance(decl, A.EventDecl):
            kind, label = 12, decl.name
        elif isinstance(decl, A.RuleDecl):
            kind, label = 6, decl.name
        elif isinstance(decl, A.PriorityDecl):
            kind, label = 6, "priority"
        elif isinstance(decl, A.FactDecl):
            kind, label = 13, decl.target or "fact"
        elif isinstance(decl, A.AssertDecl):
            kind, label = 6, "assert"
        elif isinstance(decl, A.AlignDecl):
            kind, label = 6, f"align {decl.subject}"
        else:
            return None
        return {
            "name": label,
            "kind": kind,
            "range": self.node_range(decl),
            "selectionRange": self.name_selection_range(decl, name or label),
        }

    def hover(self, line: int, character: int) -> dict[str, Any] | None:
        resolved = self.resolve_at(line, character)
        if resolved is None:
            return None
        label, kind, typ, node, uri = resolved
        parts = [f"**{kind}** `{label}`"]
        if typ is not None and self.checker is not None:
            parts.append(f"`{self.checker.format_type(typ)}`")
        if isinstance(node, A.RuleDecl):
            parts.append(f"modality `{node.modality or 'none'}`, strength `{node.strength}`")
        annotations = getattr(node, "annotations", None)
        if annotations:
            parts.append("\n".join(f"@{item}" for item in annotations))
        return {"contents": {"kind": "markdown", "value": "\n\n".join(parts)}, "range": self.node_range(node)}

    def definition(self, line: int, character: int) -> dict[str, Any] | None:
        resolved = self.resolve_at(line, character)
        if resolved is None:
            return None
        _, _, _, node, uri = resolved
        return {"uri": uri or self.uri, "range": self.node_range(node)}

    def resolve_at(self, line: int, character: int) -> tuple[str, str, A.TypeExpr | None, A.Node, str | None] | None:
        token_index = self.token_index_at(line, character)
        if token_index is None or self.checker is None:
            return None
        token = self.tokens[token_index]
        qualified = self.qualified_name_at(token_index)
        if self.is_field_position(token_index) and "." in qualified:
            typ = self.checker.infer_name_type(qualified, {})
            node = self.field_definition_node(qualified) or self.node_for_token(token)
            return qualified, "field", typ, node, self.uri
        local = self.resolve_local(token.value, line + 1, character + 1)
        if local is not None:
            return local.name, local.kind, local.type_expr, local.node, local.uri
        symbol = self.symbol_for(qualified) or self.symbol_for(token.value)
        if symbol is not None and symbol.node is not None:
            return symbol.name, symbol.kind, symbol.type_expr, symbol.node, self.uri_for_symbol(symbol)
        if qualified in self.checker.types:
            symbol = self.checker.types[qualified]
            return symbol.name, "type", symbol.type_expr, symbol.node or self.node_for_token(token), self.uri_for_symbol(symbol)
        if token.value in self.checker.types:
            symbol = self.checker.types[token.value]
            return symbol.name, "type", symbol.type_expr, symbol.node or self.node_for_token(token), self.uri_for_symbol(symbol)
        if qualified in self.checker.constructors:
            type_name, variant = self.checker.constructors[qualified]
            return qualified, "constructor", A.TypeRef(name=type_name), variant, self.uri
        return None

    def symbol_for(self, name: str) -> Symbol | None:
        if self.checker is None:
            return None
        return self.checker.symbol_for_name(name) or self.checker.terms.get(name)

    def resolve_local(self, name: str, line: int, column: int) -> EditorSymbol | None:
        visible = [
            symbol for symbol in self.locals
            if symbol.name == name and before(symbol.node, line, column)
        ]
        return max(visible, key=lambda item: (item.node.line, item.node.column), default=None)

    def uri_for_symbol(self, symbol: Symbol) -> str | None:
        if "." not in symbol.name:
            return self.uri
        module = ".".join(split_qualified(symbol.name)[:-1])
        if module in self.module_paths:
            return path_to_uri(self.module_paths[module])
        node_module = self.find_node_module(symbol.node)
        if node_module in self.module_paths:
            return path_to_uri(self.module_paths[node_module])
        return self.uri

    def find_node_module(self, node: A.Node | None) -> str | None:
        if node is None or self.checker is None:
            return None
        for name, module in self.checker.available_modules.items():
            if any(decl is node for decl in module.declarations):
                return name
            for decl in module.declarations:
                if isinstance(decl, A.TypeDecl) and isinstance(decl.definition, A.SumType):
                    if any(variant is node for variant in decl.definition.variants):
                        return name
        return self.module.name if self.module else None

    def semantic_tokens(self) -> dict[str, list[int]]:
        semantic: list[SemanticToken] = []
        declarations = {(node.line, node.column, name) for name, node in self.declaration_names()}
        for idx, token in enumerate(self.tokens):
            token_type = self.semantic_type_for_token(idx)
            if token_type is None:
                continue
            modifiers: list[str] = []
            if (token.line, token.column, token.value) in declarations:
                modifiers.append("declaration")
            semantic.append(SemanticToken(
                token.line - 1,
                token.column - 1,
                max(1, token.end_column - token.column),
                token_type,
                tuple(modifiers),
            ))
        return {"data": encode_semantic_tokens(semantic)}

    def semantic_type_for_token(self, index: int) -> str | None:
        token = self.tokens[index]
        if token.type not in {"IDENT", "KEYWORD"}:
            return None
        if self.is_field_position(index):
            return "property"
        if self.checker is None:
            return None
        value = token.value
        qualified = self.qualified_name_at(index)
        if value in {"O", "P", "F"}:
            return "label"
        if self.resolve_local(value, token.line, token.column):
            return "parameter"
        if qualified in self.checker.types or value in self.checker.types:
            return "type"
        if qualified in self.checker.constructors or value in self.checker.constructors:
            return "enumMember"
        symbol = self.symbol_for(qualified) or self.symbol_for(value)
        if symbol:
            if symbol.kind == "module":
                return "namespace"
            if symbol.kind == "function":
                return "function"
            if symbol.kind == "event":
                return "function"
            if symbol.kind == "rule":
                return "label"
            return "variable"
        if value[:1].isupper():
            return "type"
        return None

    def model_summary(self) -> dict[str, Any]:
        if self.module is None:
            return {"module": None, "diagnostics": [d.to_dict() for d in self.diagnostics]}
        core: dict[str, Any] | None = None
        try:
            core = CoreTranslator().translate(self.module)
        except Exception:  # pragma: no cover - defensive summary fallback
            core = None
        return {
            "module": self.module.name,
            "imports": [imp.path for imp in self.module.imports],
            "opens": [opened.module for opened in self.module.opens],
            "declarations": [self.declaration_summary(decl) for decl in self.module.declarations],
            "symbols": self.symbol_summary(),
            "types": self.type_summary(),
            "rules": [self.rule_summary(decl) for decl in self.module.declarations if isinstance(decl, A.RuleDecl)],
            "diagnostics": [d.to_dict() for d in self.diagnostics],
            "core": {
                "rules": core.get("rules", []) if core else [],
                "atoms": core.get("atoms", {}) if core else {},
                "alignments": core.get("alignments", []) if core else [],
            },
        }

    def declaration_summary(self, decl: A.Declaration) -> dict[str, Any]:
        return {
            "kind": type(decl).__name__,
            "name": A.declaration_name(decl),
            "range": self.node_range(decl),
            "annotations": getattr(decl, "annotations", []),
        }

    def symbol_summary(self) -> list[dict[str, Any]]:
        if self.checker is None:
            return []
        rows = []
        for name, symbol in sorted({**self.checker.types, **self.checker.terms, **self.checker.rules}.items()):
            rows.append({
                "name": name,
                "kind": symbol.kind,
                "type": self.checker.format_type(symbol.type_expr) if symbol.type_expr else None,
                "range": self.node_range(symbol.node) if symbol.node else None,
            })
        return rows

    def type_summary(self) -> list[dict[str, Any]]:
        if self.checker is None:
            return []
        rows = []
        for name, definition in sorted(self.checker.type_definitions.items()):
            fields = self.checker.fields_for_type(A.TypeRef(name=name))
            rows.append({
                "name": name,
                "fields": {field: self.checker.format_type(typ) for field, typ in fields.items()} if fields else None,
                "definition": A.node_to_dict(definition),
            })
        return rows

    def rule_summary(self, rule: A.RuleDecl) -> dict[str, Any]:
        return {
            "name": rule.name,
            "modality": rule.modality,
            "strength": rule.strength,
            "anonymous": rule.anonymous,
            "range": self.node_range(rule),
            "annotations": rule.annotations,
        }

    def declaration_names(self) -> list[tuple[str, A.Node]]:
        if self.module is None:
            return []
        result: list[tuple[str, A.Node]] = [(self.module.name, self.module)]
        for decl in self.module.declarations:
            name = A.declaration_name(decl)
            if name:
                result.append((name, decl))
        for local in self.locals:
            result.append((local.name, local.node))
        return result

    def field_definition_node(self, qualified: str) -> A.Node | None:
        if self.checker is None:
            return None
        parts = split_qualified(qualified)
        if len(parts) < 2:
            return None
        prefix = ".".join(parts[:-1])
        target_type = self.checker.infer_name_type(prefix, {})
        if isinstance(target_type, A.TypeRef):
            symbol = self.checker.types.get(target_type.name)
            return symbol.node if symbol else None
        return None

    def name_selection_range(self, node: A.Node, name: str) -> dict[str, Any]:
        token = self.find_token_in_node(node, local_name(name))
        return self.token_range(token) if token else self.node_range(node)

    def find_token_in_node(self, node: A.Node, value: str) -> Token | None:
        for token in self.tokens:
            if token.value == value and contains_position(node, token.line, token.column):
                return token
        return None

    def token_index_at(self, line: int, character: int) -> int | None:
        source_line = line + 1
        source_column = character + 1
        for idx, token in enumerate(self.tokens):
            if token.line == source_line and token.column <= source_column < token.end_column:
                return idx
        return None

    def qualified_name_at(self, index: int) -> str:
        start = index
        end = index
        while start >= 2 and self.tokens[start - 1].value == "." and is_name_token(self.tokens[start - 2]):
            start -= 2
        while end + 2 < len(self.tokens) and self.tokens[end + 1].value == "." and is_name_token(self.tokens[end + 2]):
            end += 2
        return "".join(token.value for token in self.tokens[start:end + 1])

    def is_field_position(self, index: int) -> bool:
        return index >= 2 and self.tokens[index - 1].value == "." and is_name_token(self.tokens[index])

    def node_for_token(self, token: Token) -> A.Node:
        return A.Node(line=token.line, column=token.column, end_line=token.end_line, end_column=token.end_column)

    def node_range(self, node: A.Node | None) -> dict[str, Any]:
        if node is None:
            return empty_range()
        return {
            "start": {"line": max(0, node.line - 1), "character": max(0, node.column - 1)},
            "end": {
                "line": max(0, (node.end_line or node.line) - 1),
                "character": max(0, (node.end_column or node.column + 1) - 1),
            },
        }

    def token_range(self, token: Token) -> dict[str, Any]:
        return {
            "start": {"line": token.line - 1, "character": token.column - 1},
            "end": {"line": token.end_line - 1, "character": token.end_column - 1},
        }


def encode_semantic_tokens(tokens: list[SemanticToken]) -> list[int]:
    encoded: list[int] = []
    last_line = 0
    last_start = 0
    for token in sorted(tokens, key=lambda item: (item.line, item.column)):
        delta_line = token.line - last_line
        delta_start = token.column - (last_start if delta_line == 0 else 0)
        encoded.extend([
            delta_line,
            delta_start,
            token.length,
            SEMANTIC_TOKEN_TYPES.index(token.token_type),
            modifier_bits(token.modifiers),
        ])
        last_line = token.line
        last_start = token.column
    return encoded


def modifier_bits(modifiers: tuple[str, ...]) -> int:
    bits = 0
    for modifier in modifiers:
        if modifier in SEMANTIC_TOKEN_MODIFIERS:
            bits |= 1 << SEMANTIC_TOKEN_MODIFIERS.index(modifier)
    return bits


def before(node: A.Node, line: int, column: int) -> bool:
    return node.line < line or (node.line == line and node.column <= column)


def contains_position(node: A.Node, line: int, column: int) -> bool:
    if line < node.line or (line == node.line and column < node.column):
        return False
    end_line = node.end_line or node.line
    end_column = node.end_column or node.column + 1
    return line < end_line or (line == end_line and column < end_column)


def is_name_token(token: Token) -> bool:
    return token.type in {"IDENT", "KEYWORD"} and token.value not in {"module", "import", "open"}


def path_to_uri(path: str | None) -> str | None:
    if not path:
        return None
    parsed = urlparse(path)
    if parsed.scheme:
        return path
    try:
        return Path(path).resolve().as_uri()
    except ValueError:
        return path


def empty_range() -> dict[str, Any]:
    return {
        "start": {"line": 0, "character": 0},
        "end": {"line": 0, "character": 1},
    }
