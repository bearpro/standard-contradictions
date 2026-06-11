from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any
from urllib.parse import urlparse

from . import ast as A
from .diagnostics import Diagnostic
from .lexer import Token, tokenize
from .linter import ImportResolver, SemanticChecker, Symbol, lint_source, path_to_file
from .names import local_name, split_qualified
from .parser import ParseError, parse
from .printer import PrettyPrinter


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
    "operator",
]
SEMANTIC_TOKEN_MODIFIERS = ["declaration", "defaultLibrary"]

STANDARD_OPERATOR_VALUES = {
    "O", "P", "F",
    "and", "or", "implies", "not",
    "always", "eventually", "next", "initially", "until", "last",
    "->", "<=", ">=", "!=", "==", "=", "<", ">",
    "+", "-", "*", "/", "%",
}


@dataclass(frozen=True)
class EditorSymbol:
    name: str
    kind: str
    node: A.Node
    type_expr: A.TypeExpr | None = None
    uri: str | None = None
    scope: A.Node | None = None


@dataclass(frozen=True)
class SemanticToken:
    line: int
    column: int
    length: int
    token_type: str
    modifiers: tuple[str, ...] = ()


@dataclass(frozen=True)
class ResolvedTarget:
    label: str
    kind: str
    type_expr: A.TypeExpr | None
    node: A.Node
    uri: str | None = None
    selection_range: dict[str, Any] | None = None


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
                    self.bind_pattern_symbols(param.pattern, param.type_annotation, "parameter", symbols, decl)
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
            self.bind_pattern_symbols(stmt.pattern, typ, "variable", symbols, block)
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
            self.bind_pattern_symbols(expr.pattern, typ, "variable", symbols, expr.body or expr)
            self.bind_pattern_types(expr.pattern, typ, local)
            self.collect_expr_locals(expr.body, local, symbols)
            return
        if isinstance(expr, A.MatchExpr):
            self.collect_expr_locals(expr.subject, env, symbols)
            subject_type = self.checker.infer_expr_type(expr.subject, env) if self.checker else None
            for arm in expr.arms:
                local = dict(env)
                self.bind_pattern_symbols(arm.pattern, subject_type, "variable", symbols, arm)
                self.bind_pattern_types(arm.pattern, subject_type, local)
                self.collect_expr_locals(arm.guard, local, symbols)
                self.collect_block_locals(arm.body, local, symbols)
            return
        for child in self.child_exprs(expr):
            self.collect_expr_locals(child, env, symbols)

    def child_exprs(self, expr: A.Expr) -> list[A.Expr | None]:
        if isinstance(expr, A.Call):
            return [expr.func, *expr.args]
        if isinstance(expr, A.FieldAccess):
            return [expr.target]
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
        scope: A.Node | None = None,
    ) -> None:
        if isinstance(pattern, A.VarPattern):
            symbols.append(EditorSymbol(pattern.name, kind, pattern, typ, self.uri, scope))
        elif isinstance(pattern, A.RecordPattern):
            fields = self.checker.fields_for_type(typ) if self.checker else None
            for name, nested in pattern.fields:
                field_type = fields.get(name) if fields else None
                if nested is None:
                    symbols.append(EditorSymbol(name, kind, pattern, field_type, self.uri, scope))
                else:
                    self.bind_pattern_symbols(nested, field_type, kind, symbols, scope)
        elif isinstance(pattern, A.TuplePattern):
            items = typ.items if isinstance(typ, A.TupleType) else []
            for idx, item in enumerate(pattern.items):
                self.bind_pattern_symbols(item, items[idx] if idx < len(items) else None, kind, symbols, scope)
        elif isinstance(pattern, A.ListPattern):
            item_type = self.checker.collection_item_type(typ) if self.checker else None
            for item in pattern.items:
                self.bind_pattern_symbols(item, item_type, kind, symbols, scope)
        elif isinstance(pattern, A.ConstructorPattern):
            field_types = self.checker.constructor_field_types(pattern.name, typ) if self.checker else []
            for idx, item in enumerate(pattern.args):
                self.bind_pattern_symbols(item, field_types[idx] if idx < len(field_types) else None, kind, symbols, scope)

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
        if not name and not isinstance(decl, (A.PriorityDecl, A.FactDecl)):
            return None
        if isinstance(decl, A.TypeDecl):
            kind, label = 5, decl.name
        elif isinstance(decl, A.FuncDecl):
            kind, label = 12, decl.name
        elif isinstance(decl, A.ValueDecl):
            kind, label = 13, decl.name
        elif isinstance(decl, A.EntityDecl):
            kind, label = 23, decl.name
        elif isinstance(decl, A.RuleDecl):
            kind, label = 6, decl.name
        elif isinstance(decl, A.PriorityDecl):
            kind, label = 6, "override"
        elif isinstance(decl, A.FactDecl):
            kind, label = 13, decl.target or "fact"
        else:
            return None
        symbol = {
            "name": label,
            "kind": kind,
            "range": self.node_range(decl),
            "selectionRange": self.name_selection_range(decl, name or label),
        }
        detail = self.declaration_symbol_detail(decl)
        if detail:
            symbol["detail"] = detail
        return symbol

    def declaration_symbol_detail(self, decl: A.Declaration) -> str | None:
        printer = PrettyPrinter()
        if isinstance(decl, A.TypeDecl):
            return f"= {printer.type_definition(decl.definition)}"
        if isinstance(decl, A.FuncDecl):
            tparams = f"<{', '.join(decl.type_params)}>" if decl.type_params else ""
            params = ", ".join(
                f"{printer.pattern(param.pattern)}: {self.format_symbol_type(param.type_annotation)}"
                for param in decl.params
            )
            return f"{tparams}({params}) -> {self.format_symbol_type(decl.return_type)}"
        if isinstance(decl, A.ValueDecl):
            typ = decl.type_annotation
            if typ is None and self.checker is not None:
                symbol = self.checker.terms.get(decl.name)
                typ = symbol.type_expr if symbol else None
            return f": {self.format_symbol_type(typ)}" if typ is not None else None
        if isinstance(decl, A.EntityDecl):
            return f": {self.format_symbol_type(decl.type_annotation)}"
        if isinstance(decl, A.FactDecl) and decl.target and self.checker is not None:
            typ = self.checker.infer_name_type(decl.target, {})
            return f": {self.format_symbol_type(typ)}" if typ is not None else None
        return None

    def format_symbol_type(self, typ: A.TypeExpr | None) -> str:
        if self.checker is not None:
            return self.checker.format_type(typ)
        return PrettyPrinter().type_expr(typ)

    def hover(self, line: int, character: int) -> dict[str, Any] | None:
        resolved = self.resolve_at(line, character)
        if resolved is None:
            return None
        parts = [f"**{resolved.kind}** `{resolved.label}`"]
        if resolved.type_expr is not None and self.checker is not None:
            parts.append(f"`{self.checker.format_type(resolved.type_expr)}`")
        if isinstance(resolved.node, A.RuleDecl):
            parts.append(f"modality `{resolved.node.modality or 'none'}`, strength `{resolved.node.strength}`")
        annotations = getattr(resolved.node, "annotations", None)
        if annotations:
            parts.append("\n".join(f"@{item}" for item in annotations))
        return {"contents": {"kind": "markdown", "value": "\n\n".join(parts)}, "range": resolved.selection_range or self.node_range(resolved.node)}

    def definition(self, line: int, character: int) -> dict[str, Any] | None:
        resolved = self.resolve_at(line, character)
        if resolved is None:
            return None
        return {
            "uri": resolved.uri or self.uri,
            "range": resolved.selection_range or self.selection_range_for_node(resolved.node, local_name(resolved.label), resolved.uri),
        }

    def resolve_at(self, line: int, character: int) -> ResolvedTarget | None:
        token_index = self.token_index_at(line, character)
        if token_index is None or self.checker is None:
            return None
        token = self.tokens[token_index]
        qualified = self.qualified_name_at(token_index)
        through = self.qualified_name_through(token_index)
        if self.is_field_position(token_index) and "." in qualified:
            constructor = self.constructor_target(through)
            if constructor is not None:
                return constructor
            field = self.field_definition_target(through)
            if field is not None:
                return field
            typ = self.checker.infer_name_type(through, {})
            return ResolvedTarget(through, "field", typ, self.node_for_token(token), self.uri, self.token_range(token))
        local = self.resolve_local(token.value, line + 1, character + 1)
        if local is not None:
            return ResolvedTarget(
                local.name,
                local.kind,
                local.type_expr,
                local.node,
                local.uri,
                self.selection_range_for_node(local.node, local.name, local.uri),
            )
        if through in self.checker.types:
            symbol = self.checker.types[through]
            node = symbol.node or self.node_for_token(token)
            return ResolvedTarget(symbol.name, "type", symbol.type_expr, node, self.uri_for_symbol(symbol), self.selection_range_for_node(node, symbol.name, self.uri_for_symbol(symbol)))
        constructor = self.constructor_target(through)
        if constructor is not None:
            return constructor
        symbol = self.symbol_for(through) or self.symbol_for(token.value)
        if symbol is not None and symbol.node is not None:
            uri = self.uri_for_symbol(symbol)
            return ResolvedTarget(symbol.name, symbol.kind, symbol.type_expr, symbol.node, uri, self.selection_range_for_node(symbol.node, symbol.name, uri))
        if token.value in self.checker.types:
            symbol = self.checker.types[token.value]
            node = symbol.node or self.node_for_token(token)
            uri = self.uri_for_symbol(symbol)
            return ResolvedTarget(symbol.name, "type", symbol.type_expr, node, uri, self.selection_range_for_node(node, symbol.name, uri))
        return None

    def symbol_for(self, name: str) -> Symbol | None:
        if self.checker is None:
            return None
        return self.checker.symbol_for_name(name) or self.checker.terms.get(name)

    def resolve_local(self, name: str, line: int, column: int) -> EditorSymbol | None:
        visible = [
            symbol for symbol in self.locals
            if symbol.name == name and before(symbol.node, line, column)
            and (symbol.scope is None or contains_position(symbol.scope, line, column))
        ]
        return max(visible, key=lambda item: (item.node.line, item.node.column), default=None)

    def uri_for_symbol(self, symbol: Symbol) -> str | None:
        node_module = self.find_node_module(symbol.node)
        if node_module in self.module_paths:
            return path_to_uri(self.module_paths[node_module])
        if "." not in symbol.name:
            return self.uri
        module = ".".join(split_qualified(symbol.name)[:-1])
        if module in self.module_paths:
            return path_to_uri(self.module_paths[module])
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
        value = token.value
        if value in STANDARD_OPERATOR_VALUES:
            return "operator"
        if token.type not in {"IDENT", "KEYWORD"}:
            return None
        if self.checker is None:
            return None
        qualified = self.qualified_name_at(index)
        through = self.qualified_name_through(index)
        if self.resolve_local(value, token.line, token.column):
            return "parameter"
        if through in self.checker.types or value in self.checker.types:
            return "type"
        if through in self.checker.constructors or qualified in self.checker.constructors or value in self.checker.constructors:
            return "enumMember"
        if self.is_field_position(index):
            return "property"
        symbol = self.symbol_for(qualified) or self.symbol_for(value)
        if symbol:
            if symbol.kind == "module":
                return "namespace"
            if symbol.kind == "function":
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
        return {
            "module": self.module.name,
            "imports": [imp.path for imp in self.module.imports],
            "opens": [opened.module for opened in self.module.opens],
            "declarations": [self.declaration_summary(decl) for decl in self.module.declarations],
            "symbols": self.symbol_summary(),
            "types": self.type_summary(),
            "rules": [self.rule_summary(decl) for decl in self.module.declarations if isinstance(decl, A.RuleDecl)],
            "diagnostics": [d.to_dict() for d in self.diagnostics],
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

    def constructor_target(self, qualified: str) -> ResolvedTarget | None:
        if self.checker is None or qualified not in self.checker.constructors:
            return None
        type_name, variant = self.checker.constructors[qualified]
        uri = self.uri_for_node(variant)
        return ResolvedTarget(
            qualified,
            "constructor",
            A.TypeRef(name=type_name),
            variant,
            uri,
            self.selection_range_for_node(variant, local_name(qualified), uri),
        )

    def field_definition_target(self, qualified: str) -> ResolvedTarget | None:
        if self.checker is None:
            return None
        parts = split_qualified(qualified)
        if len(parts) < 2:
            return None
        prefix = ".".join(parts[:-1])
        field_name = parts[-1]
        target_type = self.checker.infer_name_type(prefix, {})
        if isinstance(target_type, A.TypeRef):
            symbol = self.checker.types.get(target_type.name)
            if symbol and symbol.node:
                uri = self.uri_for_symbol(symbol)
                field_node = self.field_node_for_type(symbol.node, field_name, uri) or symbol.node
                return ResolvedTarget(
                    qualified,
                    "field",
                    self.checker.type_after_fields(target_type, [field_name]),
                    field_node,
                    uri,
                    self.selection_range_for_node(field_node, field_name, uri),
                )
        return None

    def field_node_for_type(self, node: A.Node, field_name: str, uri: str | None) -> A.Node | None:
        token = self.find_token_in_node_for_uri(node, field_name, uri)
        return self.node_for_token(token) if token else None

    def uri_for_node(self, node: A.Node | None) -> str | None:
        node_module = self.find_node_module(node)
        if node_module in self.module_paths:
            return path_to_uri(self.module_paths[node_module])
        return self.uri

    def name_selection_range(self, node: A.Node, name: str) -> dict[str, Any]:
        return self.selection_range_for_node(node, name, self.uri)

    def selection_range_for_node(self, node: A.Node, name: str, uri: str | None) -> dict[str, Any]:
        token = self.find_token_in_node_for_uri(node, local_name(name), uri)
        return self.token_range(token) if token else self.node_range(node)

    def find_token_in_node(self, node: A.Node, value: str) -> Token | None:
        for token in self.tokens:
            if token.value == value and contains_position(node, token.line, token.column):
                return token
        return None

    def find_token_in_node_for_uri(self, node: A.Node, value: str, uri: str | None) -> Token | None:
        if uri is None or uri == self.uri:
            return self.find_token_in_node(node, value)
        text = self.text_for_uri(uri)
        if text is None:
            return None
        try:
            tokens = tokenize(text)
        except ParseError:
            return None
        for token in tokens:
            if token.value == value and contains_position(node, token.line, token.column):
                return token
        return None

    def text_for_uri(self, uri: str) -> str | None:
        if uri == self.uri:
            return self.text
        if uri in self.documents:
            return self.documents[uri]
        path = path_to_file(uri)
        if path is None or not path.exists():
            return None
        try:
            return path.read_text(encoding="utf-8")
        except OSError:
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

    def qualified_name_through(self, index: int) -> str:
        start = index
        while start >= 2 and self.tokens[start - 1].value == "." and is_name_token(self.tokens[start - 2]):
            start -= 2
        return "".join(token.value for token in self.tokens[start:index + 1])

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
