from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from urllib.parse import unquote, urlparse
from urllib.request import url2pathname

from . import ast as A
from .diagnostics import Diagnostic, ParseError
from .parser import parse


PRIMITIVE_TYPES = {"bool", "int", "rat", "decimal", "string", "unit"}
BUILTIN_ROOTS: set[str] = set()
BUILTIN_TERMS = {"last"}


@dataclass
class Symbol:
    name: str
    kind: str
    type_expr: A.TypeExpr | None = None
    node: A.Node | None = None


@dataclass
class ResolvedModule:
    module: A.Module
    path: str | None = None


def path_to_file(path: str | None) -> Path | None:
    if not path:
        return None
    parsed = urlparse(path)
    if parsed.scheme == "file":
        return Path(url2pathname(unquote(parsed.path)))
    if parsed.scheme and not (len(parsed.scheme) == 1 and path[1:3] in {":/", ":\\"}):
        return None
    return Path(path)


def import_alias(path: str) -> str:
    normalized = path.replace("\\", "/")
    if normalized.endswith(".mdl") or "/" in normalized:
        return Path(normalized).stem
    return normalized.split(".")[-1]


def stdlib_root() -> Path:
    return Path(__file__).resolve().parent / "stdlib"


def is_file_import(path: str) -> bool:
    normalized = path.replace("\\", "/")
    return normalized.endswith(".mdl") or "/" in normalized


class ImportResolver:
    def __init__(self, path: str | None = None, documents: dict[str, str] | None = None):
        self.path = path
        self.documents = documents or {}
        self.cache: dict[tuple[str | None, str], ResolvedModule | None] = {}

    def resolve(self, import_path: str, base_path: str | None = None) -> ResolvedModule | None:
        base_path = base_path or self.path
        key = (base_path, import_path)
        if key in self.cache:
            return self.cache[key]

        resolved = (
            self.resolve_from_documents(import_path, base_path)
            or self.resolve_from_stdlib(import_path)
            or self.resolve_from_files(import_path, base_path)
        )
        self.cache[key] = resolved
        return resolved

    def resolve_from_documents(self, import_path: str, base_path: str | None) -> ResolvedModule | None:
        candidates = {p.resolve() for p in self.file_candidates(import_path, base_path) if p.is_absolute() or base_path}
        for uri, text in self.documents.items():
            document_path = path_to_file(uri)
            try:
                module = parse(text)
            except ParseError:
                continue
            if document_path is not None and candidates:
                try:
                    if document_path.resolve() in candidates:
                        return ResolvedModule(module, uri)
                except OSError:
                    pass
            if module.name == import_path:
                return ResolvedModule(module, uri)
        return None

    def resolve_from_stdlib(self, import_path: str) -> ResolvedModule | None:
        normalized = import_path.replace("\\", "/")
        if not normalized.startswith("std/"):
            return None
        if ".." in Path(normalized).parts:
            return None
        candidate = stdlib_root() / normalized
        return self.load_file(candidate)

    def resolve_from_files(self, import_path: str, base_path: str | None) -> ResolvedModule | None:
        for candidate in self.file_candidates(import_path, base_path):
            resolved = self.load_file(candidate) if is_file_import(import_path) else self.load_if_module(candidate, import_path)
            if resolved is not None:
                return resolved
        current = path_to_file(base_path)
        if current is None:
            return None
        base_dir = current.resolve().parent if current.suffix else current.resolve()
        fallback_module = Path(import_path.replace("\\", "/")).stem if is_file_import(import_path) else import_path
        for candidate in sorted(base_dir.glob("*.mdl")):
            resolved = self.load_if_module(candidate, fallback_module)
            if resolved is not None:
                return resolved
        return None

    def file_candidates(self, import_path: str, base_path: str | None) -> list[Path]:
        current = path_to_file(base_path)
        if current is None:
            base_dir = Path.cwd()
        else:
            base_dir = current.resolve().parent if current.suffix else current.resolve()
        normalized = import_path.replace("\\", "/")
        if is_file_import(normalized):
            raw = Path(normalized)
            return [raw if raw.is_absolute() else base_dir / raw]
        return [
            base_dir / (import_path.replace(".", "/") + ".mdl"),
            base_dir / f"{import_path}.mdl",
            base_dir / f"{import_path.split('.')[-1]}.mdl",
        ]

    def load_file(self, path: Path) -> ResolvedModule | None:
        if not path.exists() or not path.is_file():
            return None
        try:
            module = parse(path.read_text(encoding="utf-8"))
        except (OSError, ParseError):
            return None
        return ResolvedModule(module, str(path))

    def load_if_module(self, path: Path, import_path: str) -> ResolvedModule | None:
        if not path.exists() or not path.is_file():
            return None
        try:
            module = parse(path.read_text(encoding="utf-8"))
        except (OSError, ParseError):
            return None
        if module.name == import_path:
            return ResolvedModule(module, str(path))
        return None


class SemanticChecker:
    """Order-aware symbol checks and lightweight type information for fields."""

    def __init__(
        self,
        module: A.Module,
        path: str | None = None,
        *,
        resolver: ImportResolver | None = None,
        import_stack: set[str] | None = None,
    ):
        self.module = module
        self.path = path
        self.resolver = resolver or ImportResolver(path)
        self.import_stack = import_stack or {module.name}
        self.diagnostics: list[Diagnostic] = []
        self.imports: set[str] = set()
        self.resolved_imports: set[str] = set()
        self.imported_names: set[str] = set()
        self.types: dict[str, Symbol] = {
            name: Symbol(name, "type", A.TypeRef(name=name)) for name in sorted(PRIMITIVE_TYPES)
        }
        self.terms: dict[str, Symbol] = {
            "last": Symbol("last", "builtin", A.TypeRef(name="bool")),
        }
        self.rules: dict[str, Symbol] = {}
        self.type_definitions: dict[str, A.TypeExpr | A.SumType | None] = {}
        self.constructors: dict[str, tuple[str, A.Variant]] = {}

    def check(self) -> list[Diagnostic]:
        for imp in self.module.imports:
            self.register_import(imp)

        for decl in self.module.declarations:
            self.check_declaration(decl)
        return self.diagnostics

    def register_import(self, imp: A.ImportDecl) -> None:
        alias = imp.alias or import_alias(imp.path)
        self.imports.add(alias)
        self.terms[alias] = Symbol(alias, "import", node=imp)

        resolved = self.resolver.resolve(imp.path, self.path)
        if resolved is None:
            self.error(f"unresolved import {imp.path!r}", imp, "unresolved-import")
            self.register_unresolved_exposing(imp)
            return
        if resolved.module.name in self.import_stack:
            self.register_unresolved_exposing(imp)
            return

        self.resolved_imports.add(alias)
        imported = SemanticChecker(
            resolved.module,
            resolved.path,
            resolver=self.resolver,
            import_stack={*self.import_stack, resolved.module.name},
        )
        imported.check()
        self.register_imported_module(alias, imp, imported)
        self.register_exposing(imp, imported)

    def register_unresolved_exposing(self, imp: A.ImportDecl) -> None:
        for exposed, renamed in imp.exposing:
            name = renamed or exposed
            self.imported_names.add(name)
            self.terms[name] = Symbol(name, "imported", node=imp)
            self.types[name] = Symbol(name, "imported-type", node=imp)

    def register_imported_module(self, alias: str, imp: A.ImportDecl, imported: "SemanticChecker") -> None:
        module_fields: list[tuple[str, A.TypeExpr]] = []
        for decl in imported.module.declarations:
            if not isinstance(decl, (A.TypeDecl, A.ValueDecl, A.FuncDecl, A.EntityDecl, A.EventDecl)):
                continue
            if decl.visibility != "public":
                continue
            if isinstance(decl, A.TypeDecl):
                qualified_name = f"{alias}.{decl.name}"
                qualified_type = A.TypeRef(name=qualified_name, line=decl.line, column=decl.column)
                self.types[qualified_name] = Symbol(qualified_name, "type", qualified_type, decl)
                self.type_definitions[qualified_name] = self.qualify_type_definition(decl.definition, imported, alias)
                module_fields.append((decl.name, qualified_type))
                if isinstance(decl.definition, A.SumType):
                    for variant in decl.definition.variants:
                        module_fields.append((variant.name, qualified_type))
                continue
            symbol = imported.terms.get(decl.name)
            typ = self.qualify_type_expr(symbol.type_expr if symbol else None, imported, alias)
            module_fields.append((decl.name, typ or A.TypeRef(name="unit")))
        self.terms[alias] = Symbol(alias, "import", A.RecordType(fields=module_fields), imp)

    def register_exposing(self, imp: A.ImportDecl, imported: "SemanticChecker") -> None:
        for exposed, renamed in imp.exposing:
            name = renamed or exposed
            self.imported_names.add(name)
            if exposed in imported.types:
                qualified_definition = imported.type_definitions.get(exposed)
                exposed_type = A.TypeRef(name=name)
                self.types[name] = Symbol(name, "imported-type", exposed_type, imp)
                alias = imp.alias or import_alias(imp.path)
                self.type_definitions[name] = self.qualify_type_definition(qualified_definition, imported, alias)
            if exposed in imported.terms:
                symbol = imported.terms[exposed]
                self.terms[name] = Symbol(
                    name,
                    f"imported-{symbol.kind}",
                    self.qualify_type_expr(symbol.type_expr, imported, imp.alias or import_alias(imp.path)),
                    imp,
                )

    def qualify_type_definition(
        self,
        definition: A.TypeExpr | A.SumType | None,
        imported: "SemanticChecker",
        alias: str,
    ) -> A.TypeExpr | A.SumType | None:
        if isinstance(definition, A.SumType):
            return A.SumType(
                variants=[
                    A.Variant(
                        name=variant.name,
                        fields=[(label, self.qualify_type_expr(typ, imported, alias) or typ) for label, typ in variant.fields],
                        line=variant.line,
                        column=variant.column,
                    )
                    for variant in definition.variants
                ],
                line=definition.line,
                column=definition.column,
            )
        return self.qualify_type_expr(definition, imported, alias)

    def qualify_type_expr(
        self,
        typ: A.TypeExpr | None,
        imported: "SemanticChecker",
        alias: str,
    ) -> A.TypeExpr | None:
        if typ is None:
            return None
        if isinstance(typ, A.TypeRef):
            root = typ.name.split(".")[0]
            if typ.name in PRIMITIVE_TYPES or root in BUILTIN_ROOTS:
                name = typ.name
            elif typ.name in imported.types:
                name = f"{alias}.{typ.name}"
            else:
                name = typ.name
            return A.TypeRef(
                name=name,
                args=[self.qualify_type_expr(arg, imported, alias) or arg for arg in typ.args],
                line=typ.line,
                column=typ.column,
            )
        if isinstance(typ, A.RecordType):
            return A.RecordType(
                fields=[(name, self.qualify_type_expr(field_type, imported, alias) or field_type) for name, field_type in typ.fields],
                line=typ.line,
                column=typ.column,
            )
        if isinstance(typ, A.TupleType):
            return A.TupleType(
                items=[self.qualify_type_expr(item, imported, alias) or item for item in typ.items],
                line=typ.line,
                column=typ.column,
            )
        return typ

    def check_declaration(self, decl: A.Declaration) -> None:
        if isinstance(decl, A.TypeDecl):
            self.types[decl.name] = Symbol(decl.name, "type", A.TypeRef(name=decl.name), decl)
            self.type_definitions[decl.name] = decl.definition
            self.check_type_definition(decl.definition, set(decl.params))
            self.add_constructors(decl)
            return
        if isinstance(decl, A.ValueDecl):
            self.check_type_expr(decl.type_annotation)
            self.check_expr(decl.value, {})
            self.terms[decl.name] = Symbol(decl.name, "value", decl.type_annotation, decl)
            return
        if isinstance(decl, A.FuncDecl):
            type_params = set(decl.type_params)
            for param in decl.params:
                self.check_type_expr(param.type_annotation, type_params)
            self.check_type_expr(decl.return_type, type_params)
            self.terms[decl.name] = Symbol(decl.name, "function", decl.return_type, decl)
            env: dict[str, A.TypeExpr | None] = {}
            for param in decl.params:
                self.check_pattern(param.pattern)
                self.bind_pattern(param.pattern, env, param.type_annotation)
            self.check_block(decl.body, env, type_params)
            return
        if isinstance(decl, A.EntityDecl):
            self.check_type_expr(decl.type_annotation)
            self.terms[decl.name] = Symbol(decl.name, "entity", decl.type_annotation, decl)
            env: dict[str, A.TypeExpr | None] = {}
            for _, expr in decl.clauses:
                self.check_expr(expr, env)
            return
        if isinstance(decl, A.EventDecl):
            for _, typ in decl.fields:
                self.check_type_expr(typ)
            self.terms[decl.name] = Symbol(decl.name, "event", A.TypeRef(name="bool"), decl)
            return
        if isinstance(decl, A.RuleDecl):
            self.rules[decl.name] = Symbol(decl.name, "rule", node=decl)
            self.check_expr(decl.antecedent, {})
            self.check_expr(decl.body, {})
            self.check_expr(decl.otherwise, {})
            return
        if isinstance(decl, A.FactDecl):
            if decl.target:
                self.check_name(decl.target, decl, {})
            self.check_expr(decl.value, {})
            return
        if isinstance(decl, A.AssertDecl):
            self.check_expr(decl.expr, {})
            return

    def add_constructors(self, decl: A.TypeDecl) -> None:
        if not isinstance(decl.definition, A.SumType):
            return
        result_type = A.TypeRef(name=decl.name)
        for variant in decl.definition.variants:
            self.constructors[variant.name] = (decl.name, variant)
            self.terms[variant.name] = Symbol(variant.name, "constructor", result_type, variant)

    def check_type_definition(self, definition: A.TypeExpr | A.SumType | None, type_params: set[str]) -> None:
        if isinstance(definition, A.SumType):
            for variant in definition.variants:
                for _, typ in variant.fields:
                    self.check_type_expr(typ, type_params)
        else:
            self.check_type_expr(definition, type_params)

    def check_type_expr(self, typ: A.TypeExpr | None, type_params: set[str] | None = None) -> None:
        type_params = type_params or set()
        if typ is None:
            return
        if isinstance(typ, A.TypeRef):
            root = typ.name.split(".")[0]
            imported_type_missing = root in self.resolved_imports and typ.name not in self.types
            if (
                typ.name not in type_params
                and typ.name not in self.types
                and (root not in self.imports or imported_type_missing)
                and root not in BUILTIN_ROOTS
            ):
                self.error(f"undefined type {typ.name!r}", typ, "undefined-type")
            for arg in typ.args:
                self.check_type_expr(arg, type_params)
            return
        if isinstance(typ, A.RecordType):
            for _, field_type in typ.fields:
                self.check_type_expr(field_type, type_params)
            return
        if isinstance(typ, A.TupleType):
            for item in typ.items:
                self.check_type_expr(item, type_params)

    def check_block(self, block: A.Block | None, env: dict[str, A.TypeExpr | None], type_params: set[str] | None = None) -> None:
        if block is None:
            return
        local = dict(env)
        for stmt in block.statements:
            self.check_type_expr(stmt.type_annotation, type_params or set())
            self.check_expr(stmt.value, local)
            self.check_pattern(stmt.pattern)
            self.bind_pattern(stmt.pattern, local, stmt.type_annotation or self.infer_expr_type(stmt.value, local))
        self.check_expr(block.result, local)

    def check_expr(self, expr: A.Expr | None, env: dict[str, A.TypeExpr | None]) -> None:
        if expr is None or isinstance(expr, A.Literal):
            return
        if isinstance(expr, A.Name):
            self.check_name(expr.name, expr, env)
            return
        if isinstance(expr, A.Call):
            self.check_expr(expr.func, env)
            for arg in expr.args:
                self.check_expr(arg, env)
            return
        if isinstance(expr, A.FieldAccess):
            self.check_expr(expr.target, env)
            target_type = self.infer_expr_type(expr.target, env)
            self.check_field(target_type, expr.field, expr)
            return
        if isinstance(expr, A.IndexAccess):
            self.check_expr(expr.target, env)
            self.check_expr(expr.index, env)
            return
        if isinstance(expr, A.BinaryOp):
            self.check_expr(expr.left, env)
            self.check_expr(expr.right, env)
            return
        if isinstance(expr, A.UnaryOp):
            self.check_expr(expr.operand, env)
            return
        if isinstance(expr, A.IfExpr):
            self.check_expr(expr.condition, env)
            self.check_expr(expr.then_branch, env)
            self.check_expr(expr.else_branch, env)
            return
        if isinstance(expr, A.LetExpr):
            self.check_expr(expr.value, env)
            local = dict(env)
            self.check_pattern(expr.pattern)
            self.bind_pattern(expr.pattern, local, self.infer_expr_type(expr.value, env))
            self.check_expr(expr.body, local)
            return
        if isinstance(expr, A.MatchExpr):
            self.check_expr(expr.subject, env)
            subject_type = self.infer_expr_type(expr.subject, env)
            for arm in expr.arms:
                local = dict(env)
                self.check_pattern(arm.pattern)
                self.bind_pattern(arm.pattern, local, subject_type)
                self.check_expr(arm.guard, local)
                self.check_block(arm.body, local)
            return
        if isinstance(expr, (A.ListLiteral, A.SetLiteral, A.TupleLiteral)):
            for item in expr.items:
                self.check_expr(item, env)
            return
        if isinstance(expr, A.RecordConstructor):
            self.check_record_constructor(expr, env)
            return
        if isinstance(expr, A.TemporalUnary):
            self.check_expr(expr.operand, env)
            return
        if isinstance(expr, A.TemporalBinary):
            self.check_expr(expr.left, env)
            self.check_expr(expr.right, env)
            return
        if isinstance(expr, A.QuantifierExpr):
            self.check_expr(expr.domain, env)
            local = dict(env)
            self.check_pattern(expr.pattern)
            self.bind_pattern(expr.pattern, local, self.collection_item_type(self.infer_expr_type(expr.domain, env)))
            self.check_expr(expr.body, local)

    def check_pattern(self, pattern: A.Pattern | None) -> None:
        if isinstance(pattern, A.ConstructorPattern):
            if pattern.name not in self.constructors and pattern.name not in BUILTIN_TERMS:
                root = pattern.name.split(".")[0]
                if root not in self.imports and root not in BUILTIN_ROOTS:
                    self.error(f"undefined constructor {pattern.name!r}", pattern, "undefined-name")
            for arg in pattern.args:
                self.check_pattern(arg)
        elif isinstance(pattern, A.RecordPattern):
            for _, nested in pattern.fields:
                self.check_pattern(nested)
        elif isinstance(pattern, (A.TuplePattern, A.ListPattern)):
            for item in pattern.items:
                self.check_pattern(item)

    def bind_pattern(self, pattern: A.Pattern | None, env: dict[str, A.TypeExpr | None], typ: A.TypeExpr | None = None) -> None:
        if pattern is None or isinstance(pattern, (A.WildcardPattern, A.LiteralPattern)):
            return
        if isinstance(pattern, A.VarPattern):
            env[pattern.name] = typ
            return
        if isinstance(pattern, A.ConstructorPattern):
            field_types = self.constructor_field_types(pattern.name)
            for idx, arg in enumerate(pattern.args):
                self.bind_pattern(arg, env, field_types[idx] if idx < len(field_types) else None)
            return
        if isinstance(pattern, A.RecordPattern):
            fields = self.fields_for_type(typ)
            for name, nested in pattern.fields:
                field_type = fields.get(name) if fields else None
                if nested is None:
                    env[name] = field_type
                else:
                    self.bind_pattern(nested, env, field_type)
            return
        if isinstance(pattern, A.TuplePattern):
            items = typ.items if isinstance(typ, A.TupleType) else []
            for idx, item in enumerate(pattern.items):
                self.bind_pattern(item, env, items[idx] if idx < len(items) else None)
            return
        if isinstance(pattern, A.ListPattern):
            item_type = self.collection_item_type(typ)
            for item in pattern.items:
                self.bind_pattern(item, env, item_type)

    def check_name(self, name: str, node: A.Node, env: dict[str, A.TypeExpr | None]) -> None:
        if name in BUILTIN_TERMS or name in self.terms or name in env or name in self.imported_names:
            return
        parts = name.split(".")
        root = parts[0]
        if root in env:
            self.check_field_chain(env[root], parts[1:], node)
            return
        if root in self.terms:
            self.check_field_chain(self.terms[root].type_expr, parts[1:], node)
            return
        if root == self.module.name and len(parts) > 1:
            symbol = self.terms.get(parts[1])
            if symbol is None:
                self.error(f"undefined name {name!r}", node, "undefined-name")
                return
            self.check_field_chain(symbol.type_expr, parts[2:], node)
            return
        if root in self.imports or root in BUILTIN_ROOTS:
            return
        self.error(f"undefined name {name!r}", node, "undefined-name")

    def check_field_chain(self, typ: A.TypeExpr | None, fields: list[str], node: A.Node) -> None:
        current = typ
        for field in fields:
            available = self.fields_for_type(current)
            if available is None:
                return
            if field not in available:
                self.error(f"unknown field {field!r}", node, "unknown-field")
                return
            current = available[field]

    def check_field(self, typ: A.TypeExpr | None, field: str, node: A.Node) -> None:
        fields = self.fields_for_type(typ)
        if fields is not None and field not in fields:
            self.error(f"unknown field {field!r}", node, "unknown-field")

    def check_record_constructor(self, expr: A.RecordConstructor, env: dict[str, A.TypeExpr | None]) -> None:
        typ = A.TypeRef(name=expr.type_name, line=expr.line, column=expr.column)
        self.check_type_expr(typ)
        fields = self.fields_for_type(typ)
        type_known = expr.type_name in self.types
        if fields is None and type_known:
            self.error(f"type {expr.type_name!r} is not a record type", expr, "not-record-type")
        seen: set[str] = set()
        for name, value in expr.fields:
            if name in seen:
                self.error(f"duplicate record field {name!r}", expr, "duplicate-record-field")
            seen.add(name)
            if fields is not None and name not in fields:
                self.error(f"unknown field {name!r}", expr, "unknown-field")
            self.check_expr(value, env)
        if fields is not None:
            for name in sorted(set(fields) - seen):
                self.error(f"missing record field {name!r}", expr, "missing-record-field")

    def infer_expr_type(self, expr: A.Expr | None, env: dict[str, A.TypeExpr | None]) -> A.TypeExpr | None:
        if expr is None:
            return None
        if isinstance(expr, A.Literal):
            return A.TypeRef(name=expr.kind if expr.kind in PRIMITIVE_TYPES else "unit")
        if isinstance(expr, A.Name):
            return self.infer_name_type(expr.name, env)
        if isinstance(expr, A.FieldAccess):
            target_type = self.infer_expr_type(expr.target, env)
            fields = self.fields_for_type(target_type)
            return fields.get(expr.field) if fields else None
        if isinstance(expr, A.Call):
            name = self.expr_to_name(expr.func)
            if name and name in self.terms:
                return self.terms[name].type_expr
            if name and name in self.constructors:
                return A.TypeRef(name=self.constructors[name][0])
            if name and (name == "to_list" or name.endswith("strings.to_list")) and expr.args:
                return A.TypeRef(name="List", args=[A.TypeRef(name="string")])
        if isinstance(expr, A.BinaryOp):
            if expr.op in {"and", "or", "implies", "->", "iff", "<->", "=", "!=", "<", "<=", ">", ">="}:
                return A.TypeRef(name="bool")
            return self.infer_expr_type(expr.left, env)
        if isinstance(expr, A.UnaryOp):
            return A.TypeRef(name="bool") if expr.op == "not" else self.infer_expr_type(expr.operand, env)
        if isinstance(expr, A.IfExpr):
            return self.infer_expr_type(expr.then_branch, env)
        if isinstance(expr, A.LetExpr):
            local = dict(env)
            self.bind_pattern(expr.pattern, local, self.infer_expr_type(expr.value, env))
            return self.infer_expr_type(expr.body, local)
        if isinstance(expr, A.RecordConstructor):
            return A.TypeRef(name=expr.type_name)
        if isinstance(expr, A.TupleLiteral):
            return A.TupleType(items=[self.infer_expr_type(item, env) or A.TypeRef(name="unit") for item in expr.items])
        if isinstance(expr, (A.ListLiteral, A.SetLiteral)):
            item_type = self.infer_expr_type(expr.items[0], env) if expr.items else A.TypeRef(name="unit")
            return A.TypeRef(name="List" if isinstance(expr, A.ListLiteral) else "Set", args=[item_type or A.TypeRef(name="unit")])
        if isinstance(expr, (A.TemporalUnary, A.TemporalBinary, A.QuantifierExpr)):
            return A.TypeRef(name="bool")
        return None

    def infer_name_type(self, name: str, env: dict[str, A.TypeExpr | None]) -> A.TypeExpr | None:
        if name in env:
            return env[name]
        if name in self.terms:
            return self.terms[name].type_expr
        parts = name.split(".")
        root = parts[0]
        if root in env:
            return self.type_after_fields(env[root], parts[1:])
        if root in self.terms:
            return self.type_after_fields(self.terms[root].type_expr, parts[1:])
        if root == self.module.name and len(parts) > 1:
            symbol = self.terms.get(parts[1])
            return self.type_after_fields(symbol.type_expr if symbol else None, parts[2:])
        return None

    def type_after_fields(self, typ: A.TypeExpr | None, fields: list[str]) -> A.TypeExpr | None:
        current = typ
        for field in fields:
            available = self.fields_for_type(current)
            if available is None:
                return None
            current = available.get(field)
            if current is None:
                return None
        return current

    def fields_for_reference(self, name: str) -> dict[str, A.TypeExpr] | None:
        return self.fields_for_type(self.infer_name_type(name, {}))

    def fields_for_type(self, typ: A.TypeExpr | None, seen: set[str] | None = None) -> dict[str, A.TypeExpr] | None:
        seen = seen or set()
        if isinstance(typ, A.RecordType):
            return dict(typ.fields)
        if isinstance(typ, A.TupleType):
            return {f"_{idx}": item for idx, item in enumerate(typ.items)}
        if isinstance(typ, A.TypeRef):
            if typ.name in seen:
                return None
            definition = self.type_definitions.get(typ.name)
            if isinstance(definition, A.TypeExpr):
                return self.fields_for_type(definition, {*seen, typ.name})
        return None

    def collection_item_type(self, typ: A.TypeExpr | None) -> A.TypeExpr | None:
        if isinstance(typ, A.TypeRef) and typ.name in {"List", "Set"} and typ.args:
            return typ.args[0]
        return None

    def constructor_field_types(self, name: str) -> list[A.TypeExpr]:
        if name in self.constructors:
            return [typ for _, typ in self.constructors[name][1].fields]
        return []

    def expr_to_name(self, expr: A.Expr | None) -> str | None:
        if isinstance(expr, A.Name):
            return expr.name
        if isinstance(expr, A.FieldAccess):
            base = self.expr_to_name(expr.target)
            return f"{base}.{expr.field}" if base else None
        return None

    def visible_type_names(self, line: int, column: int) -> list[str]:
        return sorted(name for name, symbol in self.types.items() if self.is_visible(symbol, line, column))

    def visible_term_names(self, line: int, column: int) -> list[str]:
        return sorted(name for name, symbol in self.terms.items() if self.is_visible(symbol, line, column))

    def is_visible(self, symbol: Symbol, line: int, column: int) -> bool:
        if symbol.node is None:
            return True
        if symbol.node.line < line:
            return True
        return symbol.node.line == line and symbol.node.column < column

    def error(self, message: str, node: A.Node, code: str) -> None:
        self.diagnostics.append(Diagnostic(
            message,
            line=node.line or 1,
            column=node.column or 1,
            severity="error",
            code=code,
            path=self.path,
        ))


class Linter:
    def lint_module(
        self,
        module: A.Module,
        path: str | None = None,
        *,
        documents: dict[str, str] | None = None,
    ) -> list[Diagnostic]:
        diagnostics: list[Diagnostic] = []
        diagnostics.extend(self.check_duplicates(module, path))
        diagnostics.extend(SemanticChecker(module, path, resolver=ImportResolver(path, documents)).check())
        diagnostics.extend(self.check_rules(module, path))
        diagnostics.extend(self.check_alignments(module, path))
        diagnostics.extend(self.check_functions(module, path))
        return diagnostics

    def check_duplicates(self, module: A.Module, path: str | None) -> list[Diagnostic]:
        seen: dict[tuple[str, str], A.Declaration] = {}
        diagnostics: list[Diagnostic] = []
        for decl in module.declarations:
            name = A.declaration_name(decl)
            if not name:
                continue
            kind = decl.__class__.__name__
            key = (kind, name)
            if key in seen:
                diagnostics.append(Diagnostic(
                    f"duplicate {kind} name {name!r}",
                    line=decl.line or 1,
                    column=decl.column or 1,
                    severity="error",
                    code="duplicate-name",
                    path=path,
                ))
            else:
                seen[key] = decl
        return diagnostics

    def check_rules(self, module: A.Module, path: str | None) -> list[Diagnostic]:
        diagnostics: list[Diagnostic] = []
        rule_names = set()
        for decl in module.declarations:
            if not isinstance(decl, A.RuleDecl):
                continue
            if decl.name in rule_names:
                diagnostics.append(Diagnostic(
                    f"duplicate rule name {decl.name!r}", decl.line or 1, decl.column or 1,
                    severity="error", code="duplicate-rule", path=path,
                ))
            rule_names.add(decl.name)
            if not decl.modality:
                diagnostics.append(Diagnostic(
                    f"rule {decl.name!r} has no deontic modality",
                    decl.line or 1, decl.column or 1,
                    severity="warning", code="missing-modality", path=path,
                ))
            if decl.anonymous:
                diagnostics.append(Diagnostic(
                    "anonymous rule is accepted, but named rules are better for traceability and priorities",
                    decl.line or 1, decl.column or 1,
                    severity="warning", code="anonymous-rule", path=path,
                ))
            if decl.body is not None and not self.has_temporal_operator(decl.body):
                diagnostics.append(Diagnostic(
                    f"rule {decl.name!r} has no explicit temporal operator; consider `always` or `eventually`",
                    decl.line or 1, decl.column or 1,
                    severity="warning", code="rule-without-temporal", path=path,
                ))
        for decl in module.declarations:
            if isinstance(decl, A.PriorityDecl):
                for name in decl.chain:
                    if name not in rule_names:
                        diagnostics.append(Diagnostic(
                            f"priority references unknown rule {name!r}",
                            decl.line or 1, decl.column or 1,
                            severity="warning", code="unknown-priority-rule", path=path,
                        ))
        return diagnostics

    def check_alignments(self, module: A.Module, path: str | None) -> list[Diagnostic]:
        diagnostics: list[Diagnostic] = []
        exported = {A.declaration_name(d) for d in module.declarations if A.declaration_name(d)}
        for decl in module.declarations:
            if isinstance(decl, A.AlignDecl):
                subject_root = decl.subject.split(".")[0]
                if subject_root not in exported and subject_root != module.name:
                    diagnostics.append(Diagnostic(
                        f"alignment subject {decl.subject!r} does not resolve to a declaration in this module",
                        decl.line or 1, decl.column or 1,
                        severity="warning", code="unresolved-alignment-subject", path=path,
                    ))
        return diagnostics

    def check_functions(self, module: A.Module, path: str | None) -> list[Diagnostic]:
        diagnostics: list[Diagnostic] = []
        for decl in module.declarations:
            if isinstance(decl, A.FuncDecl):
                if decl.body is None or decl.body.result is None:
                    diagnostics.append(Diagnostic(
                        f"function {decl.name!r} has no final expression",
                        decl.line or 1, decl.column or 1,
                        severity="error", code="function-without-result", path=path,
                    ))
                if self.has_temporal_operator_in_block(decl.body):
                    diagnostics.append(Diagnostic(
                        f"function {decl.name!r} contains temporal operators; temporal logic belongs in rules/asserts",
                        decl.line or 1, decl.column or 1,
                        severity="error", code="temporal-in-function", path=path,
                    ))
        return diagnostics

    def has_temporal_operator_in_block(self, block: A.Block | None) -> bool:
        if block is None:
            return False
        return any(self.has_temporal_operator(stmt.value) for stmt in block.statements) or self.has_temporal_operator(block.result)

    def has_temporal_operator(self, expr: A.Expr | None) -> bool:
        if expr is None:
            return False
        if isinstance(expr, (A.TemporalUnary, A.TemporalBinary)):
            return True
        if isinstance(expr, A.BinaryOp):
            return self.has_temporal_operator(expr.left) or self.has_temporal_operator(expr.right)
        if isinstance(expr, A.UnaryOp):
            return self.has_temporal_operator(expr.operand)
        if isinstance(expr, A.IfExpr):
            return any(self.has_temporal_operator(e) for e in [expr.condition, expr.then_branch, expr.else_branch])
        if isinstance(expr, A.Call):
            return self.has_temporal_operator(expr.func) or any(self.has_temporal_operator(a) for a in expr.args)
        if isinstance(expr, A.FieldAccess):
            return self.has_temporal_operator(expr.target)
        if isinstance(expr, A.IndexAccess):
            return self.has_temporal_operator(expr.target) or self.has_temporal_operator(expr.index)
        if isinstance(expr, A.MatchExpr):
            return self.has_temporal_operator(expr.subject) or any(
                self.has_temporal_operator(arm.guard) or self.has_temporal_operator_in_block(arm.body)
                for arm in expr.arms
            )
        if isinstance(expr, A.LetExpr):
            return self.has_temporal_operator(expr.value) or self.has_temporal_operator(expr.body)
        if isinstance(expr, A.QuantifierExpr):
            return self.has_temporal_operator(expr.domain) or self.has_temporal_operator(expr.body)
        if isinstance(expr, (A.ListLiteral, A.SetLiteral, A.TupleLiteral)):
            return any(self.has_temporal_operator(i) for i in expr.items)
        if isinstance(expr, A.RecordConstructor):
            return any(self.has_temporal_operator(v) for _, v in expr.fields)
        return False


def lint_source(source: str, path: str | None = None, documents: dict[str, str] | None = None) -> list[Diagnostic]:
    try:
        module = parse(source)
    except ParseError as exc:
        return [exc.to_diagnostic(path)]
    return Linter().lint_module(module, path=path, documents=documents)
