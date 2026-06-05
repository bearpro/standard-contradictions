from __future__ import annotations

import os
from dataclasses import dataclass
from pathlib import Path
from typing import Any
from urllib.parse import unquote, urlparse
from urllib.request import url2pathname

from . import ast as A
from .diagnostics import Diagnostic, ParseError
from .names import local_name, root_name, split_qualified
from .parser import parse
from .type_inference import TypeInference


PRIMITIVE_TYPES = {"bool", "int", "rat", "decimal", "string", "unit"}
BUILTIN_ROOTS: set[str] = set()
BUILTIN_TERMS = {"last"}
STDLIB_ENV = "MDL_STDLIB_PATH"


@dataclass
class Symbol:
    name: str
    kind: str
    type_expr: A.TypeExpr | None = None
    node: A.Node | None = None
    scheme: Any | None = None


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


def stdlib_root(stdlib_path: str | Path | None = None) -> Path | None:
    configured = stdlib_path if stdlib_path is not None else os.environ.get(STDLIB_ENV)
    if not configured:
        return None
    return Path(configured)


def is_file_import(path: str) -> bool:
    normalized = path.replace("\\", "/")
    return normalized.endswith(".mdl") or "/" in normalized


class ImportResolver:
    def __init__(
        self,
        path: str | None = None,
        documents: dict[str, str] | None = None,
        stdlib_path: str | Path | None = None,
    ):
        self.path = path
        self.documents = documents or {}
        self.stdlib_path = stdlib_root(stdlib_path)
        self.cache: dict[tuple[str | None, str], ResolvedModule | None] = {}
        self.stdlib_cache: dict[str, ResolvedModule] | None = None

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
        if self.stdlib_path is None:
            return None
        normalized = import_path.replace("\\", "/")
        if not normalized.startswith("std/"):
            return None
        if ".." in Path(normalized).parts:
            return None
        candidate = self.stdlib_path / normalized
        return self.load_file(candidate)

    def stdlib_modules(self) -> dict[str, ResolvedModule]:
        if self.stdlib_cache is not None:
            return self.stdlib_cache
        modules: dict[str, ResolvedModule] = {}
        if self.stdlib_path is not None:
            for path in sorted(self.stdlib_path.glob("**/*.mdl")):
                resolved = self.load_file(path)
                if resolved is not None:
                    modules[resolved.module.name] = resolved
        self.stdlib_cache = modules
        return modules

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
        self.imported_names: set[str] = set()
        self.available_modules: dict[str, A.Module] = {}
        self.types: dict[str, Symbol] = {
            name: Symbol(name, "type", A.TypeRef(name=name)) for name in sorted(PRIMITIVE_TYPES)
        }
        self.type_params: dict[str, list[str]] = {}
        self.terms: dict[str, Symbol] = {
            "last": Symbol("last", "builtin", A.TypeRef(name="bool")),
        }
        self.rules: dict[str, Symbol] = {}
        self.type_definitions: dict[str, A.TypeExpr | A.SumType | None] = {}
        self.constructors: dict[str, tuple[str, A.Variant]] = {}

    def check(self) -> list[Diagnostic]:
        self.register_stdlib_modules()
        for imp in self.module.imports:
            self.register_import(imp)
        for opened in self.module.opens:
            self.register_open(opened)

        for decl in self.module.declarations:
            self.check_declaration(decl)
        return self.diagnostics

    def register_stdlib_modules(self) -> None:
        for resolved in self.resolver.stdlib_modules().values():
            if resolved.module.name != self.module.name:
                self.register_module_namespace(resolved.module, resolved.path)

    def register_import(self, imp: A.ImportDecl) -> None:
        resolved = self.resolver.resolve(imp.path, self.path)
        if resolved is None:
            self.error(f"unresolved import {imp.path!r}", imp, "unresolved-import")
            return
        if resolved.module.name in self.import_stack:
            return
        self.register_module_namespace(resolved.module, resolved.path, imp)

    def register_module_namespace(self, module: A.Module, path: str | None = None, node: A.Node | None = None) -> None:
        if module.name == self.module.name or module.name in self.available_modules:
            return
        self.available_modules[module.name] = module
        self.imports.add(root_name(module.name))
        declared_types = {decl.name for decl in module.declarations if isinstance(decl, A.TypeDecl)}
        module_fields: list[tuple[str, A.TypeExpr]] = []
        for decl in module.declarations:
            if not isinstance(decl, (A.TypeDecl, A.ValueDecl, A.FuncDecl, A.EntityDecl, A.EventDecl)):
                continue
            if isinstance(decl, A.TypeDecl):
                qualified_name = f"{module.name}.{decl.name}"
                qualified_type = A.TypeRef(name=qualified_name, line=decl.line, column=decl.column)
                self.types[qualified_name] = Symbol(qualified_name, "type", qualified_type, decl)
                self.type_params[qualified_name] = list(decl.params)
                self.type_definitions[qualified_name] = self.qualify_type_definition(decl.definition, module.name, declared_types, set(decl.params))
                module_fields.append((decl.name, qualified_type))
                if isinstance(decl.definition, A.SumType):
                    for variant in decl.definition.variants:
                        ctor_name = f"{qualified_name}.{variant.name}"
                        self.constructors[ctor_name] = (qualified_name, variant)
                continue
            typ = self.declaration_type(decl)
            typ = self.qualify_type_expr(typ, module.name, declared_types, set())
            self.terms[f"{module.name}.{decl.name}"] = Symbol(f"{module.name}.{decl.name}", self.declaration_kind(decl), typ, decl)
            module_fields.append((decl.name, typ or A.TypeRef(name="unit")))
        self.terms[module.name] = Symbol(module.name, "module", A.RecordType(fields=module_fields), node)

    def register_open(self, opened: A.OpenDecl) -> None:
        module = self.available_modules.get(opened.module)
        if module is None:
            self.error(f"unresolved open {opened.module!r}", opened, "unresolved-open")
            return
        local_names = {name for name in (A.declaration_name(decl) for decl in self.module.declarations) if name}
        declared_types = {decl.name for decl in module.declarations if isinstance(decl, A.TypeDecl)}
        for decl in module.declarations:
            name = A.declaration_name(decl)
            if not name or not isinstance(decl, (A.TypeDecl, A.ValueDecl, A.FuncDecl, A.EntityDecl, A.EventDecl)):
                continue
            if name in local_names or name in self.imported_names or name in self.terms or name in self.types:
                self.error(f"open {opened.module!r} introduces conflicting name {name!r}", opened, "ambiguous-open")
                continue
            self.imported_names.add(name)
            if isinstance(decl, A.TypeDecl):
                full = f"{opened.module}.{decl.name}"
                self.types[name] = Symbol(name, "type", A.TypeRef(name=name), decl)
                self.type_params[name] = list(decl.params)
                self.type_definitions[name] = self.type_definitions.get(full)
                if isinstance(decl.definition, A.SumType):
                    for variant in decl.definition.variants:
                        self.constructors[f"{name}.{variant.name}"] = (name, variant)
                continue
            typ = self.declaration_type(decl)
            typ = self.qualify_type_expr(typ, opened.module, declared_types, set())
            self.terms[name] = Symbol(name, self.declaration_kind(decl), typ, decl)

    def declaration_type(self, decl: A.Declaration) -> A.TypeExpr | None:
        if isinstance(decl, A.ValueDecl):
            return decl.type_annotation
        if isinstance(decl, A.FuncDecl):
            return decl.return_type
        if isinstance(decl, A.EntityDecl):
            return decl.type_annotation
        if isinstance(decl, A.EventDecl):
            return A.TypeRef(name="bool")
        return None

    def declaration_kind(self, decl: A.Declaration) -> str:
        if isinstance(decl, A.ValueDecl):
            return "value"
        if isinstance(decl, A.FuncDecl):
            return "function"
        if isinstance(decl, A.EntityDecl):
            return "entity"
        if isinstance(decl, A.EventDecl):
            return "event"
        return "symbol"

    def qualify_type_definition(
        self,
        definition: A.TypeExpr | A.SumType | None,
        module_name: str,
        declared_types: set[str],
        type_params: set[str],
    ) -> A.TypeExpr | A.SumType | None:
        if isinstance(definition, A.SumType):
            return A.SumType(
                variants=[
                    A.Variant(
                        name=variant.name,
                        fields=[(label, self.qualify_type_expr(typ, module_name, declared_types, type_params) or typ) for label, typ in variant.fields],
                        line=variant.line,
                        column=variant.column,
                    )
                    for variant in definition.variants
                ],
                line=definition.line,
                column=definition.column,
            )
        return self.qualify_type_expr(definition, module_name, declared_types, type_params)

    def qualify_type_expr(
        self,
        typ: A.TypeExpr | None,
        module_name: str,
        declared_types: set[str],
        type_params: set[str],
    ) -> A.TypeExpr | None:
        if typ is None:
            return None
        if isinstance(typ, A.TypeRef):
            if typ.name in PRIMITIVE_TYPES or typ.name in type_params or self.is_known_full_type(typ.name):
                name = typ.name
            elif typ.name in declared_types:
                name = f"{module_name}.{typ.name}"
            elif typ.name in self.types:
                type_symbol = self.types[typ.name]
                name = type_symbol.type_expr.name if isinstance(type_symbol.type_expr, A.TypeRef) else typ.name
            else:
                name = typ.name
            return A.TypeRef(
                name=name,
                args=[self.qualify_type_expr(arg, module_name, declared_types, type_params) or arg for arg in typ.args],
                line=typ.line,
                column=typ.column,
            )
        if isinstance(typ, A.RecordType):
            return A.RecordType(
                fields=[(name, self.qualify_type_expr(field_type, module_name, declared_types, type_params) or field_type) for name, field_type in typ.fields],
                line=typ.line,
                column=typ.column,
            )
        if isinstance(typ, A.TupleType):
            return A.TupleType(
                items=[self.qualify_type_expr(item, module_name, declared_types, type_params) or item for item in typ.items],
                line=typ.line,
                column=typ.column,
            )
        return typ

    def is_known_full_type(self, name: str) -> bool:
        return name in self.types or any(name.startswith(module_name + ".") for module_name in self.available_modules)

    def check_declaration(self, decl: A.Declaration) -> None:
        if isinstance(decl, A.TypeDecl):
            self.types[decl.name] = Symbol(decl.name, "type", A.TypeRef(name=decl.name), decl)
            self.type_params[decl.name] = list(decl.params)
            self.type_definitions[decl.name] = decl.definition
            self.check_type_definition(decl.definition, set(decl.params))
            self.add_constructors(decl)
            return
        if isinstance(decl, A.ValueDecl):
            self.check_type_expr(decl.type_annotation)
            inferer = TypeInference(self)
            actual = inferer.check_expr(decl.value, {}, expected=decl.type_annotation)
            typ = decl.type_annotation or actual
            scheme = inferer.generalize(inferer.from_ast(typ), {}) if typ is not None else None
            self.terms[decl.name] = Symbol(decl.name, "value", typ, decl, scheme)
            return
        if isinstance(decl, A.FuncDecl):
            type_params = set(decl.type_params)
            for param in decl.params:
                self.check_type_expr(param.type_annotation, type_params)
            self.check_type_expr(decl.return_type, type_params)
            func_inferer = TypeInference(self, type_params)
            self.terms[decl.name] = Symbol(
                decl.name,
                "function",
                decl.return_type,
                decl,
                func_inferer.function_scheme(decl),
            )
            env: dict[str, A.TypeExpr | None] = {}
            for param in decl.params:
                self.check_pattern(param.pattern, param.type_annotation, type_params)
                self.bind_pattern(param.pattern, env, param.type_annotation)
            self.check_block(decl.body, env, type_params, expected=decl.return_type)
            return
        if isinstance(decl, A.EntityDecl):
            self.check_type_expr(decl.type_annotation)
            self.terms[decl.name] = Symbol(decl.name, "entity", decl.type_annotation, decl)
            return
        if isinstance(decl, A.EventDecl):
            for _, typ in decl.fields:
                self.check_type_expr(typ)
            self.terms[decl.name] = Symbol(decl.name, "event", A.TypeRef(name="bool"), decl)
            return
        if isinstance(decl, A.RuleDecl):
            self.rules[decl.name] = Symbol(decl.name, "rule", node=decl)
            bool_type = A.TypeRef(name="bool")
            self.check_expr(decl.antecedent, {}, expected=bool_type)
            self.check_expr(decl.body, {}, expected=bool_type)
            self.check_expr(decl.otherwise, {}, expected=bool_type)
            return
        if isinstance(decl, A.FactDecl):
            expected = None
            if decl.target:
                self.check_name(decl.target, decl, {})
                expected = self.infer_name_type(decl.target, {})
            else:
                expected = A.TypeRef(name="bool")
            self.check_expr(decl.value, {}, expected=expected)
            return
        if isinstance(decl, A.AssertDecl):
            self.check_expr(decl.expr, {}, expected=A.TypeRef(name="bool"))
            return

    def add_constructors(self, decl: A.TypeDecl) -> None:
        if not isinstance(decl.definition, A.SumType):
            return
        result_type = A.TypeRef(name=decl.name)
        for variant in decl.definition.variants:
            self.constructors[f"{decl.name}.{variant.name}"] = (decl.name, variant)
            self.terms[f"{decl.name}.{variant.name}"] = Symbol(f"{decl.name}.{variant.name}", "constructor", result_type, variant)

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
            if (
                typ.name not in type_params
                and typ.name not in self.types
                and root not in self.imports
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

    def check_block(
        self,
        block: A.Block | None,
        env: dict[str, A.TypeExpr | None],
        type_params: set[str] | None = None,
        *,
        expected: A.TypeExpr | None = None,
    ) -> A.TypeExpr | None:
        return TypeInference(self, type_params or set()).check_block(block, env, expected=expected)

    def check_expr(
        self,
        expr: A.Expr | None,
        env: dict[str, A.TypeExpr | None],
        *,
        expected: A.TypeExpr | None = None,
        type_params: set[str] | None = None,
    ) -> A.TypeExpr | None:
        return TypeInference(self, type_params or set()).check_expr(expr, env, expected=expected)

    def check_pattern(
        self,
        pattern: A.Pattern | None,
        typ: A.TypeExpr | None = None,
        type_params: set[str] | None = None,
    ) -> None:
        type_params = type_params or set()
        if isinstance(pattern, A.LiteralPattern):
            self.check_assignable(self.pattern_literal_type(pattern), typ, pattern)
        if isinstance(pattern, A.ConstructorPattern):
            if pattern.name not in self.constructors and pattern.name not in BUILTIN_TERMS:
                self.error(f"undefined constructor {pattern.name!r}", pattern, "undefined-name")
            field_types = self.constructor_field_types(pattern.name, typ)
            if field_types:
                self.constructor_arity_matches(pattern.name, len(pattern.args), field_types, pattern)
            for idx, arg in enumerate(pattern.args):
                self.check_pattern(arg, field_types[idx] if idx < len(field_types) else None, type_params)
        elif isinstance(pattern, A.RecordPattern):
            fields = self.fields_for_type(typ)
            for name, nested in pattern.fields:
                self.check_pattern(nested, fields.get(name) if fields else None, type_params)
        elif isinstance(pattern, A.TuplePattern):
            items = typ.items if isinstance(typ, A.TupleType) else []
            for idx, item in enumerate(pattern.items):
                self.check_pattern(item, items[idx] if idx < len(items) else None, type_params)
        elif isinstance(pattern, A.ListPattern):
            item_type = self.collection_item_type(typ)
            for item in pattern.items:
                self.check_pattern(item, item_type, type_params)

    def bind_pattern(self, pattern: A.Pattern | None, env: dict[str, A.TypeExpr | None], typ: A.TypeExpr | None = None) -> None:
        if pattern is None or isinstance(pattern, (A.WildcardPattern, A.LiteralPattern)):
            return
        if isinstance(pattern, A.VarPattern):
            env[pattern.name] = typ
            return
        if isinstance(pattern, A.ConstructorPattern):
            field_types = self.constructor_field_types(pattern.name, typ)
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
        parts = split_qualified(name)
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
        module_name, rest = self.split_module_reference(parts)
        if module_name is not None and rest:
            symbol = self.terms.get(f"{module_name}.{rest[0]}")
            if symbol is None:
                self.error(f"undefined name {name!r}", node, "undefined-name")
                return
            self.check_field_chain(symbol.type_expr, rest[1:], node)
            return
        if root in self.imports or root in BUILTIN_ROOTS:
            return
        self.error(f"undefined name {name!r}", node, "undefined-name")

    def split_module_reference(self, parts: list[str]) -> tuple[str | None, list[str]]:
        module_names = [self.module.name, *self.available_modules]
        for module_name in sorted(module_names, key=lambda item: len(split_qualified(item)), reverse=True):
            module_parts = split_qualified(module_name)
            if parts[:len(module_parts)] == module_parts:
                return module_name, parts[len(module_parts):]
        return None, parts

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

    def check_record_constructor(
        self,
        expr: A.RecordConstructor,
        env: dict[str, A.TypeExpr | None],
        type_params: set[str] | None = None,
    ) -> A.TypeExpr | None:
        type_params = type_params or set()
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
            self.check_expr(value, env, expected=fields.get(name) if fields else None, type_params=type_params)
        if fields is not None:
            for name in sorted(set(fields) - seen):
                self.error(f"missing record field {name!r}", expr, "missing-record-field")
        return typ

    def check_call(
        self,
        expr: A.Call,
        env: dict[str, A.TypeExpr | None],
        expected: A.TypeExpr | None,
        type_params: set[str],
    ) -> A.TypeExpr | None:
        name = self.expr_to_name(expr.func)
        if name is None:
            self.check_expr(expr.func, env, type_params=type_params)
            for arg in expr.args:
                self.check_expr(arg, env, type_params=type_params)
            return None

        if name == "to_list" or name.endswith("strings.to_list"):
            self.check_arity(name, len(expr.args), 1, expr)
            if expr.args:
                self.check_expr(expr.args[0], env, expected=A.TypeRef(name="string"), type_params=type_params)
            return A.TypeRef(name="List", args=[A.TypeRef(name="string")])

        symbol = self.symbol_for_name(name)
        if symbol is not None and isinstance(symbol.node, A.EventDecl):
            event = symbol.node
            self.check_arity(name, len(expr.args), len(event.fields), expr)
            for arg, (_, typ) in zip(expr.args, event.fields):
                self.check_expr(arg, env, expected=typ, type_params=type_params)
            return A.TypeRef(name="bool")

        constructor_type = self.constructor_result_type(name, expected)
        if constructor_type is not None:
            field_types = self.constructor_field_types(name, constructor_type)
            self.constructor_arity_matches(name, len(expr.args), field_types, expr)
            for arg, typ in zip(expr.args, field_types):
                self.check_expr(arg, env, expected=typ, type_params=type_params)
            return constructor_type

        if symbol is not None and isinstance(symbol.node, A.FuncDecl):
            func = symbol.node
            self.check_arity(name, len(expr.args), len(func.params), expr)
            substitutions: dict[str, A.TypeExpr] = {}
            actuals: list[A.TypeExpr | None] = []
            for arg, param in zip(expr.args, func.params):
                actual = self.check_expr(arg, env, type_params=type_params)
                actuals.append(actual)
                if actual is not None:
                    self.unify_type_params(param.type_annotation, actual, substitutions, set(func.type_params))
            if expected is not None:
                self.unify_type_params(func.return_type, expected, substitutions, set(func.type_params))
            for actual, param, arg in zip(actuals, func.params, expr.args):
                formal = self.substitute_type_params(param.type_annotation, substitutions) if param.type_annotation else None
                self.check_assignable(actual, formal, arg)
            return self.substitute_type_params(func.return_type, substitutions) if func.return_type else None

        self.check_expr(expr.func, env, type_params=type_params)
        for arg in expr.args:
            self.check_expr(arg, env, type_params=type_params)
        return self.infer_expr_type(expr, env)

    def check_binary_expr(
        self,
        expr: A.BinaryOp,
        env: dict[str, A.TypeExpr | None],
        type_params: set[str],
    ) -> A.TypeExpr:
        bool_type = A.TypeRef(name="bool")
        if expr.op in {"and", "or"}:
            self.check_expr(expr.left, env, expected=bool_type, type_params=type_params)
            self.check_expr(expr.right, env, expected=bool_type, type_params=type_params)
            return bool_type

        left = self.check_expr(expr.left, env, type_params=type_params)
        right = self.check_expr(expr.right, env, type_params=type_params)
        if expr.op in {"=", "!="}:
            if left is not None and right is not None and not self.are_comparable(left, right, type_params):
                self.error(
                    f"cannot compare {self.format_type(left)} and {self.format_type(right)}",
                    expr,
                    "type-mismatch",
                )
            return bool_type

        if expr.op in {"<", "<=", ">", ">="}:
            self.check_numeric_operand(left, expr.left or expr)
            self.check_numeric_operand(right, expr.right or expr)
            return bool_type

        if expr.op in {"+", "-", "*", "/", "%"}:
            self.check_numeric_operand(left, expr.left or expr)
            self.check_numeric_operand(right, expr.right or expr)
            if expr.op == "/" or self.type_name(left) in {"rat", "decimal"} or self.type_name(right) in {"rat", "decimal"}:
                return A.TypeRef(name="rat")
            return A.TypeRef(name="int")

        return bool_type

    def check_assignable(
        self,
        actual: A.TypeExpr | None,
        expected: A.TypeExpr | None,
        node: A.Node,
    ) -> None:
        if actual is None or expected is None:
            return
        if self.is_unknown_type(actual) or self.is_unknown_type(expected):
            return
        if self.is_assignable(actual, expected):
            return
        code = "non-bool-expression" if self.is_bool_type(expected) else "type-mismatch"
        self.error(f"expected {self.format_type(expected)}, got {self.format_type(actual)}", node, code)

    def check_numeric_operand(self, typ: A.TypeExpr | None, node: A.Node) -> None:
        if typ is None or self.is_unknown_type(typ):
            return
        if not self.is_numeric_type(typ):
            self.error(f"expected numeric expression, got {self.format_type(typ)}", node, "non-numeric-expression")

    def check_arity(self, name: str, actual: int, expected: int, node: A.Node) -> None:
        if actual != expected:
            self.error(f"{name!r} expects {expected} args, got {actual}", node, "arity-mismatch")

    def constructor_arity_matches(self, name: str, actual: int, field_types: list[A.TypeExpr], node: A.Node) -> bool:
        if actual == len(field_types):
            return True
        if actual == 0 and len(field_types) == 1 and self.type_name(field_types[0]) == "unit":
            return True
        self.error(f"{name!r} expects {len(field_types)} args, got {actual}", node, "arity-mismatch")
        return False

    def literal_type(self, expr: A.Literal) -> A.TypeExpr:
        return A.TypeRef(name=expr.kind if expr.kind in PRIMITIVE_TYPES else "unit")

    def pattern_literal_type(self, pattern: A.LiteralPattern) -> A.TypeExpr:
        return A.TypeRef(name=pattern.kind if pattern.kind in PRIMITIVE_TYPES else "unit")

    def symbol_for_name(self, name: str) -> Symbol | None:
        if name in self.terms:
            return self.terms[name]
        parts = split_qualified(name)
        if parts and parts[0] == self.module.name and len(parts) > 1:
            return self.terms.get(parts[1])
        module_name, rest = self.split_module_reference(parts)
        if module_name is not None and rest:
            return self.terms.get(f"{module_name}.{rest[0]}")
        return None

    def constructor_result_type(self, name: str, expected: A.TypeExpr | None) -> A.TypeExpr | None:
        if isinstance(expected, A.TypeRef) and name in self.constructors:
            constructor_type = self.constructors[name][0]
            if self.same_type_name(constructor_type, expected.name):
                return expected
        if name in self.constructors:
            return A.TypeRef(name=self.constructors[name][0])
        return None

    def is_assignable(self, actual: A.TypeExpr, expected: A.TypeExpr) -> bool:
        actual_name = self.type_name(actual)
        expected_name = self.type_name(expected)
        if self.same_type_name(actual_name, expected_name):
            if isinstance(actual, A.TypeRef) and isinstance(expected, A.TypeRef):
                return len(actual.args) == len(expected.args) and all(
                    self.is_assignable(a, e) for a, e in zip(actual.args, expected.args)
                )
            return True
        if expected_name in {"rat", "decimal"} and actual_name in {"int", "rat", "decimal"}:
            return True
        if isinstance(actual, A.RecordType) and isinstance(expected, A.RecordType):
            actual_fields = dict(actual.fields)
            expected_fields = dict(expected.fields)
            return set(actual_fields) == set(expected_fields) and all(
                self.is_assignable(actual_fields[name], expected_fields[name]) for name in expected_fields
            )
        if isinstance(actual, A.TupleType) and isinstance(expected, A.TupleType):
            return len(actual.items) == len(expected.items) and all(
                self.is_assignable(a, e) for a, e in zip(actual.items, expected.items)
            )
        return False

    def are_comparable(self, left: A.TypeExpr, right: A.TypeExpr, type_params: set[str]) -> bool:
        if self.is_assignable(left, right) or self.is_assignable(right, left):
            return True
        if self.is_numeric_type(left, type_params) and self.is_numeric_type(right, type_params):
            return True
        left_fields = self.fields_for_type(left)
        right_fields = self.fields_for_type(right)
        if left_fields is not None and right_fields is not None:
            common = sorted(set(left_fields) & set(right_fields))
            return bool(common) and all(self.are_comparable(left_fields[name], right_fields[name], type_params) for name in common)
        return False

    def common_type(
        self,
        left: A.TypeExpr | None,
        right: A.TypeExpr | None,
        type_params: set[str],
    ) -> A.TypeExpr | None:
        if left is None:
            return right
        if right is None:
            return left
        if self.is_assignable(left, right):
            return right
        if self.is_assignable(right, left):
            return left
        if self.is_numeric_type(left, type_params) and self.is_numeric_type(right, type_params):
            if self.type_name(left) in {"rat", "decimal"}:
                return left
            if self.type_name(right) in {"rat", "decimal"}:
                return right
            return A.TypeRef(name="int")
        return None

    def is_bool_type(self, typ: A.TypeExpr | None) -> bool:
        return self.type_name(typ) == "bool"

    def is_numeric_type(self, typ: A.TypeExpr | None, type_params: set[str] | None = None) -> bool:
        name = self.type_name(typ)
        return name in {"int", "rat", "decimal"} or bool(name and type_params and name in type_params)

    def type_name(self, typ: A.TypeExpr | None) -> str | None:
        if isinstance(typ, A.TypeRef):
            return typ.name
        return None

    def same_type_name(self, left: str | None, right: str | None) -> bool:
        if left is None or right is None:
            return False
        if left == right:
            return True
        left_parts = split_qualified(left)
        right_parts = split_qualified(right)
        if not left_parts or not right_parts or left_parts[-1] != right_parts[-1]:
            return False
        left_qualified = len(left_parts) > 1 and left_parts[0] in self.imports
        right_qualified = len(right_parts) > 1 and right_parts[0] in self.imports
        return left_qualified or right_qualified

    def is_unknown_type(self, typ: A.TypeExpr | None) -> bool:
        if typ is None:
            return True
        if isinstance(typ, A.TypeRef):
            root = typ.name.split(".")[0]
            if typ.name in PRIMITIVE_TYPES or typ.name in self.types or root in self.imports or root in BUILTIN_ROOTS:
                return any(self.is_unknown_type(arg) for arg in typ.args)
            return True
        if isinstance(typ, A.RecordType):
            return any(self.is_unknown_type(field_type) for _, field_type in typ.fields)
        if isinstance(typ, A.TupleType):
            return any(self.is_unknown_type(item) for item in typ.items)
        return False

    def format_type(self, typ: A.TypeExpr | None) -> str:
        if typ is None:
            return "unknown"
        if isinstance(typ, A.TypeRef):
            if typ.args:
                return f"{typ.name}<" + ", ".join(self.format_type(arg) for arg in typ.args) + ">"
            return typ.name
        if isinstance(typ, A.RecordType):
            return "{ " + ", ".join(f"{name}: {self.format_type(field_type)}" for name, field_type in typ.fields) + " }"
        if isinstance(typ, A.TupleType):
            return "(" + ", ".join(self.format_type(item) for item in typ.items) + ")"
        return "unknown"

    def unify_type_params(
        self,
        pattern: A.TypeExpr | None,
        actual: A.TypeExpr,
        substitutions: dict[str, A.TypeExpr],
        type_params: set[str],
    ) -> None:
        if pattern is None:
            return
        if isinstance(pattern, A.TypeRef):
            if pattern.name in type_params and not pattern.args:
                substitutions.setdefault(pattern.name, actual)
                return
            if isinstance(actual, A.TypeRef):
                for p_arg, a_arg in zip(pattern.args, actual.args):
                    self.unify_type_params(p_arg, a_arg, substitutions, type_params)
            return
        if isinstance(pattern, A.RecordType) and isinstance(actual, A.RecordType):
            actual_fields = dict(actual.fields)
            for name, field_type in pattern.fields:
                if name in actual_fields:
                    self.unify_type_params(field_type, actual_fields[name], substitutions, type_params)
            return
        if isinstance(pattern, A.TupleType) and isinstance(actual, A.TupleType):
            for p_item, a_item in zip(pattern.items, actual.items):
                self.unify_type_params(p_item, a_item, substitutions, type_params)

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
            if expr.op in {"and", "or", "=", "!=", "<", "<=", ">", ">="}:
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
        if isinstance(expr, (A.TemporalUnary, A.TemporalBinary)):
            return A.TypeRef(name="bool")
        return None

    def infer_name_type(self, name: str, env: dict[str, A.TypeExpr | None]) -> A.TypeExpr | None:
        if name in env:
            return env[name]
        if name in self.terms:
            return self.terms[name].type_expr
        parts = split_qualified(name)
        root = parts[0]
        if root in env:
            return self.type_after_fields(env[root], parts[1:])
        if root in self.terms:
            return self.type_after_fields(self.terms[root].type_expr, parts[1:])
        if root == self.module.name and len(parts) > 1:
            symbol = self.terms.get(parts[1])
            return self.type_after_fields(symbol.type_expr if symbol else None, parts[2:])
        module_name, rest = self.split_module_reference(parts)
        if module_name is not None and rest:
            symbol = self.terms.get(f"{module_name}.{rest[0]}")
            return self.type_after_fields(symbol.type_expr if symbol else None, rest[1:])
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
        if isinstance(typ, A.TypeRef) and local_name(typ.name) in {"List", "Set"} and typ.args:
            return typ.args[0]
        return None

    def constructor_field_types(self, name: str, typ: A.TypeExpr | None = None) -> list[A.TypeExpr]:
        short = local_name(name)
        if isinstance(typ, A.TypeRef):
            definition = self.type_definitions.get(typ.name)
            if isinstance(definition, A.SumType):
                params = self.type_params.get(typ.name, [])
                substitutions = dict(zip(params, typ.args))
                for variant in definition.variants:
                    if variant.name == short:
                        return [self.substitute_type_params(field_type, substitutions) for _, field_type in variant.fields]
        if name in self.constructors:
            return [typ for _, typ in self.constructors[name][1].fields]
        return []

    def substitute_type_params(self, typ: A.TypeExpr, substitutions: dict[str, A.TypeExpr]) -> A.TypeExpr:
        if isinstance(typ, A.TypeRef):
            if typ.name in substitutions and not typ.args:
                return substitutions[typ.name]
            return A.TypeRef(
                name=typ.name,
                args=[self.substitute_type_params(arg, substitutions) for arg in typ.args],
                line=typ.line,
                column=typ.column,
            )
        if isinstance(typ, A.RecordType):
            return A.RecordType(
                fields=[(name, self.substitute_type_params(field_type, substitutions)) for name, field_type in typ.fields],
                line=typ.line,
                column=typ.column,
            )
        if isinstance(typ, A.TupleType):
            return A.TupleType(
                items=[self.substitute_type_params(item, substitutions) for item in typ.items],
                line=typ.line,
                column=typ.column,
            )
        return typ

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
            end_line=node.end_line or None,
            end_column=node.end_column or None,
        ))

    def warning(self, message: str, node: A.Node, code: str) -> None:
        line = node.line or 1
        column = node.column or 1
        if any(
            d.severity == "warning"
            and d.code == code
            and d.message == message
            and d.line == line
            and d.column == column
            for d in self.diagnostics
        ):
            return
        self.diagnostics.append(Diagnostic(
            message,
            line=line,
            column=column,
            severity="warning",
            code=code,
            path=self.path,
            end_line=node.end_line or None,
            end_column=node.end_column or None,
        ))


class Linter:
    def lint_module(
        self,
        module: A.Module,
        path: str | None = None,
        *,
        documents: dict[str, str] | None = None,
        stdlib_path: str | Path | None = None,
    ) -> list[Diagnostic]:
        diagnostics: list[Diagnostic] = []
        diagnostics.extend(self.check_duplicates(module, path))
        diagnostics.extend(SemanticChecker(module, path, resolver=ImportResolver(path, documents, stdlib_path)).check())
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
        if isinstance(expr, A.MatchExpr):
            return self.has_temporal_operator(expr.subject) or any(
                self.has_temporal_operator(arm.guard) or self.has_temporal_operator_in_block(arm.body)
                for arm in expr.arms
            )
        if isinstance(expr, A.LetExpr):
            return self.has_temporal_operator(expr.value) or self.has_temporal_operator(expr.body)
        if isinstance(expr, A.TupleLiteral):
            return any(self.has_temporal_operator(i) for i in expr.items)
        if isinstance(expr, A.RecordConstructor):
            return any(self.has_temporal_operator(v) for _, v in expr.fields)
        return False


def lint_source(
    source: str,
    path: str | None = None,
    documents: dict[str, str] | None = None,
    stdlib_path: str | Path | None = None,
) -> list[Diagnostic]:
    try:
        module = parse(source)
    except ParseError as exc:
        return [exc.to_diagnostic(path)]
    return Linter().lint_module(module, path=path, documents=documents, stdlib_path=stdlib_path)
