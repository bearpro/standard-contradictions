from __future__ import annotations

import json
from dataclasses import dataclass, field
from fractions import Fraction
from pathlib import Path
from typing import TYPE_CHECKING, Any, Iterable

if TYPE_CHECKING:  # pragma: no cover
    import z3
    z3_import_error: BaseException | None = None
else:
    try:  # pragma: no cover - exercised in environments without the optional wheel
        import z3
    except Exception as exc:  # pragma: no cover
        z3_import_error = exc
    else:
        z3_import_error = None

from . import ast as A
from .diagnostics import Diagnostic, MDLError, ParseError
from .linter import ImportResolver
from .names import split_qualified
from .parser import parse
from .printer import format_expr
from .runtime import Runtime


class SolveError(MDLError):
    pass


class UnsupportedExpression(SolveError):
    pass


@dataclass(frozen=True)
class PrimitiveType:
    name: str


@dataclass(frozen=True)
class RecordSpec:
    fields: tuple[tuple[str, "TypeSpec"], ...]


@dataclass(frozen=True)
class TupleSpec:
    items: tuple["TypeSpec", ...]


@dataclass(frozen=True)
class VariantSpec:
    name: str
    fields: tuple[tuple[str | None, "TypeSpec"], ...]


@dataclass(frozen=True)
class SumSpec:
    name: str
    variants: tuple[VariantSpec, ...]


@dataclass(frozen=True)
class NamedSpec:
    name: str


@dataclass(frozen=True)
class OpaqueSpec:
    name: str


TypeSpec = PrimitiveType | RecordSpec | TupleSpec | SumSpec | NamedSpec | OpaqueSpec


BOOL = PrimitiveType("bool")
INT = PrimitiveType("int")
REAL = PrimitiveType("rat")
STRING = PrimitiveType("string")
UNIT = PrimitiveType("unit")


@dataclass
class ZValue:
    typ: TypeSpec
    expr: Any | None = None
    fields: dict[str, "ZValue"] | None = None
    concrete: Any = None
    has_concrete: bool = False


@dataclass
class DatatypeEncoding:
    sort: Any
    constructors: dict[str, Any]
    recognizers: dict[str, Any]
    accessors: dict[str, list[Any]]
    fields: dict[str, list[tuple[str | None, TypeSpec]]]


@dataclass(frozen=True)
class ConstructorInfo:
    sum_type: SumSpec
    variant: VariantSpec
    module: str | None = None
    local: str | None = None
    type_args: tuple[TypeSpec, ...] = ()


@dataclass
class FunctionInfo:
    module: str
    decl: A.FuncDecl
    params: list[TypeSpec]
    return_type: TypeSpec
    type_args: tuple[TypeSpec, ...] = ()


@dataclass
class EntityInfo:
    module: str
    decl: A.EntityDecl
    typ: TypeSpec


@dataclass
class EventInfo:
    module: str
    decl: A.EventDecl
    fields: list[tuple[str, TypeSpec]]


@dataclass
class ModuleScope:
    module: A.Module
    path: Path | None = None
    imports: dict[str, str] = field(default_factory=dict)
    opens: set[str] = field(default_factory=set)
    types: dict[str, A.TypeDecl] = field(default_factory=dict)
    values: dict[str, A.ValueDecl] = field(default_factory=dict)
    funcs: dict[str, FunctionInfo] = field(default_factory=dict)
    entities: dict[str, EntityInfo] = field(default_factory=dict)
    events: dict[str, EventInfo] = field(default_factory=dict)
    priorities: list[A.PriorityDecl] = field(default_factory=list)
    facts: list[A.FactDecl] = field(default_factory=list)
    rules: list[A.RuleDecl] = field(default_factory=list)
    runtime: Runtime | None = None


@dataclass
class RuleInfo:
    index: int
    module: str
    decl: A.RuleDecl
    full_name: str


@dataclass
class TrackedConstraint:
    key: str
    kind: str
    module: str | None
    name: str
    line: int
    column: int

    def to_dict(self) -> dict[str, Any]:
        data = {
            "key": self.key,
            "kind": self.kind,
            "name": self.name,
            "line": self.line,
            "column": self.column,
        }
        if self.module:
            data["module"] = self.module
        return data


@dataclass
class SolveOptions:
    horizon: int | None = None
    max_horizon: int = 3
    permission: str = "strong"
    max_conflicts: int = 20
    stdlib_path: str | Path | None = None


def read_module(path: Path) -> A.Module:
    return parse(path.read_text(encoding="utf-8"))


def solve_paths(paths: Iterable[str | Path], options: SolveOptions | None = None) -> dict[str, Any]:
    opts = options or SolveOptions()
    path_list = [Path(p) for p in paths]
    diagnostics: list[Diagnostic] = []
    modules: list[tuple[A.Module, Path]] = []
    for path in path_list:
        try:
            modules.append((read_module(path), path))
        except ParseError as exc:
            diagnostics.append(exc.to_diagnostic(str(path)))
        except OSError as exc:
            diagnostics.append(Diagnostic(str(exc), severity="error", code="io-error", path=str(path)))
    if diagnostics:
        return error_payload(diagnostics)
    modules = collect_imported_modules(modules, opts.stdlib_path)
    return solve_modules([m for m, _ in modules], opts, paths=[p for _, p in modules])


def collect_imported_modules(
    modules: list[tuple[A.Module, Path]],
    stdlib_path: str | Path | None = None,
) -> list[tuple[A.Module, Path]]:
    result = list(modules)
    seen_modules = {module.name for module, _ in result}
    resolver = ImportResolver(stdlib_path=stdlib_path)
    for resolved in resolver.stdlib_modules().values():
        if resolved.module.name not in seen_modules:
            seen_modules.add(resolved.module.name)
            result.append((resolved.module, Path(resolved.path) if resolved.path else Path("<stdlib>")))
    queue = list(result)
    while queue:
        module, path = queue.pop(0)
        resolver = ImportResolver(str(path), stdlib_path=stdlib_path)
        for imp in module.imports:
            resolved = resolver.resolve(imp.path, str(path))
            if resolved is None or resolved.module.name in seen_modules or resolved.path is None:
                continue
            resolved_path = Path(resolved.path)
            seen_modules.add(resolved.module.name)
            item = (resolved.module, resolved_path)
            result.append(item)
            queue.append(item)
    return result


def solve_modules(modules: list[A.Module], options: SolveOptions | None = None, *, paths: list[Path] | None = None) -> dict[str, Any]:
    if z3_import_error is not None:
        return error_payload([Diagnostic(
            f"z3-solver is not available: {z3_import_error}",
            severity="error",
            code="z3-unavailable",
        )])
    opts = options or SolveOptions()
    if opts.horizon is not None and opts.horizon < 1:
        return error_payload([Diagnostic("--horizon must be >= 1", severity="error", code="invalid-horizon")])
    if opts.horizon is None and opts.max_horizon < 1:
        return error_payload([Diagnostic("--max-horizon must be >= 1", severity="error", code="invalid-horizon")])
    problem = SolverProblem(modules, opts, paths=paths)
    if problem.diagnostics:
        return error_payload(problem.diagnostics)
    return problem.solve()


def error_payload(diagnostics: list[Diagnostic]) -> dict[str, Any]:
    return {
        "status": "error",
        "horizon": None,
        "checked_horizons": [],
        "conflicts": [],
        "model": None,
        "diagnostics": [d.to_dict() for d in diagnostics],
    }


class SolverProblem:
    def __init__(self, modules: list[A.Module], options: SolveOptions, *, paths: list[Path] | None = None):
        stdlib = ImportResolver(stdlib_path=options.stdlib_path).stdlib_modules()
        seen = {module.name for module in modules}
        std_modules = [resolved.module for resolved in stdlib.values() if resolved.module.name not in seen]
        self.modules = [*modules, *std_modules]
        self.options = options
        self.paths = [*(paths or [None] * len(modules)), *[Path(resolved.path) if resolved.path else None for resolved in stdlib.values() if resolved.module.name not in seen]]  # type: ignore[list-item]
        self.diagnostics: list[Diagnostic] = []
        self.scopes: dict[str, ModuleScope] = {}
        self.type_cache: dict[tuple[str, str, tuple[TypeSpec, ...]], TypeSpec] = {}
        self.type_specs_by_name: dict[str, TypeSpec] = {}
        self.datatype_encodings: dict[str, DatatypeEncoding] = {}
        self.opaque_sorts: dict[str, Any] = {}
        self.functions: dict[str, Any] = {}
        self.function_definitions: set[str] = set()
        self._build_scopes()

    # ------------------------------------------------------------------
    # Public solve flow
    # ------------------------------------------------------------------

    def solve(self) -> dict[str, Any]:
        horizons = [self.options.horizon] if self.options.horizon is not None else list(range(1, self.options.max_horizon + 1))
        checked: list[int] = []
        conflicts: list[dict[str, Any]] = []
        for horizon in horizons:
            assert horizon is not None
            checked.append(horizon)
            encoding = BoundedEncoder(self, horizon)
            encoding.encode()
            if encoding.diagnostics:
                return error_payload(encoding.diagnostics)
            result = encoding.solver.check()
            if result == z3.sat:
                return {
                    "status": "sat",
                    "horizon": horizon,
                    "checked_horizons": checked,
                    "conflicts": [],
                    "model": encoding.model_payload(),
                    "diagnostics": [],
                }
            if result == z3.unsat:
                conflicts.append(encoding.conflict_payload(len(conflicts) + 1))
                if len(conflicts) >= self.options.max_conflicts:
                    break
            else:
                return {
                    "status": "unknown",
                    "horizon": horizon,
                    "checked_horizons": checked,
                    "conflicts": conflicts,
                    "model": None,
                    "diagnostics": [Diagnostic(str(encoding.solver.reason_unknown()), severity="warning", code="z3-unknown").to_dict()],
                }
        return {
            "status": "unsat",
            "horizon": None,
            "checked_horizons": checked,
            "conflicts": conflicts,
            "model": None,
            "diagnostics": [],
        }

    # ------------------------------------------------------------------
    # Module and declaration indexing
    # ------------------------------------------------------------------

    def _build_scopes(self) -> None:
        for idx, module in enumerate(self.modules):
            path = self.paths[idx] if idx < len(self.paths) else None
            if module.name in self.scopes:
                self.diagnostics.append(Diagnostic(
                    f"duplicate module {module.name!r}",
                    line=module.line or 1,
                    column=module.column or 1,
                    severity="error",
                    code="duplicate-module",
                    path=str(path) if path else None,
                ))
                continue
            self.scopes[module.name] = ModuleScope(module=module, path=path)

        for scope in self.scopes.values():
            for imp in scope.module.imports:
                target_module = self.resolve_import_module(scope, imp)
                if target_module is not None:
                    scope.imports[target_module] = target_module
                else:
                    self.diagnostics.append(Diagnostic(
                        f"import {imp.path!r} is not present in solve inputs",
                        line=imp.line or 1,
                        column=imp.column or 1,
                        severity="error",
                        code="unresolved-import",
                        path=str(scope.path) if scope.path else None,
                    ))
            for opened in scope.module.opens:
                if opened.module in self.scopes:
                    scope.opens.add(opened.module)
                else:
                    self.diagnostics.append(Diagnostic(
                        f"open {opened.module!r} is not present in solve inputs",
                        line=opened.line or 1,
                        column=opened.column or 1,
                        severity="error",
                        code="unresolved-open",
                        path=str(scope.path) if scope.path else None,
                    ))

        for scope in self.scopes.values():
            self._index_declarations(scope)

        for scope in self.scopes.values():
            for name, decl in scope.types.items():
                self.resolve_type(scope.module.name, A.TypeRef(name=name))
            for name, entity in list(scope.entities.items()):
                entity.typ = self.resolve_type(scope.module.name, entity.decl.type_annotation)
            for name, event in list(scope.events.items()):
                event.fields = [(field_name, self.resolve_type(scope.module.name, field_type)) for field_name, field_type in event.decl.fields]
            for name, decl in list(scope.funcs.items()):
                params = [self.resolve_type(scope.module.name, p.type_annotation) for p in decl.decl.params]
                ret = self.resolve_type(scope.module.name, decl.decl.return_type)
                scope.funcs[name] = FunctionInfo(scope.module.name, decl.decl, params, ret)
            try:
                scope.runtime = Runtime(scope.module)
            except Exception:
                scope.runtime = None

    def resolve_import_module(self, scope: ModuleScope, imp: A.ImportDecl) -> str | None:
        if imp.path in self.scopes:
            return imp.path
        resolver = ImportResolver(str(scope.path) if scope.path else None, stdlib_path=self.options.stdlib_path)
        resolved = resolver.resolve(imp.path, str(scope.path) if scope.path else None)
        if resolved is not None and resolved.module.name in self.scopes:
            return resolved.module.name
        return None

    def _index_declarations(self, scope: ModuleScope) -> None:
        seen: set[tuple[str, str]] = set()
        for decl in scope.module.declarations:
            key: tuple[str, str] | None = None
            if isinstance(decl, A.TypeDecl):
                key = ("type", decl.name)
                scope.types[decl.name] = decl
            elif isinstance(decl, A.ValueDecl):
                key = ("value", decl.name)
                scope.values[decl.name] = decl
            elif isinstance(decl, A.FuncDecl):
                key = ("func", decl.name)
                scope.funcs[decl.name] = FunctionInfo(scope.module.name, decl, [], UNIT)
            elif isinstance(decl, A.EntityDecl):
                key = ("entity", decl.name)
                scope.entities[decl.name] = EntityInfo(scope.module.name, decl, UNIT)
            elif isinstance(decl, A.EventDecl):
                key = ("event", decl.name)
                scope.events[decl.name] = EventInfo(scope.module.name, decl, [])
            elif isinstance(decl, A.PriorityDecl):
                scope.priorities.append(decl)
            elif isinstance(decl, A.FactDecl):
                scope.facts.append(decl)
            elif isinstance(decl, A.RuleDecl):
                scope.rules.append(decl)
            if key is not None:
                if key in seen:
                    self.diagnostics.append(Diagnostic(
                        f"duplicate {key[0]} {key[1]!r}",
                        line=decl.line or 1,
                        column=decl.column or 1,
                        severity="error",
                        code="duplicate-name",
                        path=str(scope.path) if scope.path else None,
                    ))
                seen.add(key)

    # ------------------------------------------------------------------
    # Resolution
    # ------------------------------------------------------------------

    def resolve_type(
        self,
        module_name: str,
        typ: A.TypeExpr | None,
        type_env: dict[str, TypeSpec] | None = None,
    ) -> TypeSpec:
        type_env = type_env or {}
        if typ is None:
            return UNIT
        scope = self.scopes[module_name]
        if isinstance(typ, A.TypeRef):
            name = typ.name
            if name in type_env and not typ.args:
                return type_env[name]
            primitive = self.primitive_type(name)
            if primitive is not None:
                return primitive
            target_module, local = self.resolve_decl(scope, name, expected={"type"})
            if target_module is None or local is None:
                return OpaqueSpec(name)
            arg_specs = tuple(self.resolve_type(module_name, arg, type_env) for arg in typ.args)
            return self.resolve_declared_type(target_module, local, arg_specs)
        return self.resolve_type_expr(module_name, typ, type_env)

    def resolve_declared_type(self, module_name: str, local: str, arg_specs: tuple[TypeSpec, ...]) -> TypeSpec:
        key = (module_name, local, arg_specs)
        if key in self.type_cache:
            return self.type_cache[key]
        decl = self.scopes[module_name].types[local]
        type_params = {
            param: arg_specs[idx] if idx < len(arg_specs) else OpaqueSpec(param)
            for idx, param in enumerate(decl.params)
        }
        full_name = self.type_name(module_name, local, arg_specs)
        placeholder = NamedSpec(full_name)
        self.type_cache[key] = placeholder
        self.type_specs_by_name[full_name] = placeholder
        if isinstance(decl.definition, A.SumType):
            variants = tuple(
                VariantSpec(v.name, tuple((label, self.resolve_type(module_name, item_type, type_params)) for label, item_type in v.fields))
                for v in decl.definition.variants
            )
            resolved: TypeSpec = SumSpec(full_name, variants)
        else:
            resolved = self.resolve_type_expr(module_name, decl.definition, type_params)
        self.type_cache[key] = resolved
        self.type_specs_by_name[full_name] = resolved
        return resolved

    def resolve_type_expr(
        self,
        module_name: str,
        typ: A.TypeExpr | A.SumType | None,
        type_env: dict[str, TypeSpec] | None = None,
    ) -> TypeSpec:
        type_env = type_env or {}
        if typ is None:
            return UNIT
        if isinstance(typ, A.RecordType):
            return RecordSpec(tuple((name, self.resolve_type(module_name, field_type, type_env)) for name, field_type in typ.fields))
        if isinstance(typ, A.TupleType):
            return TupleSpec(tuple(self.resolve_type(module_name, item, type_env) for item in typ.items))
        if isinstance(typ, A.SumType):
            return SumSpec("<anonymous>", tuple(
                VariantSpec(v.name, tuple((label, self.resolve_type(module_name, item_type, type_env)) for label, item_type in v.fields))
                for v in typ.variants
            ))
        return self.resolve_type(module_name, typ, type_env)

    def type_name(self, module_name: str, local: str, args: tuple[TypeSpec, ...]) -> str:
        if not args:
            return f"{module_name}.{local}"
        return f"{module_name}.{local}<{', '.join(repr(arg) for arg in args)}>"

    def primitive_type(self, name: str) -> TypeSpec | None:
        if name in {"bool", "boolean"}:
            return BOOL
        if name in {"int", "integer"}:
            return INT
        if name in {"rat", "rational", "real", "decimal"}:
            return REAL
        if name == "string":
            return STRING
        if name == "unit":
            return UNIT
        return None

    def resolve_decl(
        self,
        scope: ModuleScope,
        name: str,
        *,
        expected: set[str] | None = None,
    ) -> tuple[str | None, str | None]:
        parts = split_qualified(name)
        if not parts:
            return None, None
        local_module, local_name = self._resolve_local_decl(scope, parts, expected)
        if local_module is not None:
            return local_module, local_name
        for opened in sorted(scope.opens):
            target_scope = self.scopes.get(opened)
            if target_scope is None:
                continue
            open_module, open_name = self._resolve_local_decl(target_scope, parts, expected)
            if open_module is not None:
                return open_module, open_name
        for module_name in sorted(self.scopes, key=len, reverse=True):
            module_parts = split_qualified(module_name)
            if parts[:len(module_parts)] == module_parts:
                return self._resolve_local_decl(self.scopes[module_name], parts[len(module_parts):], expected)
        return None, None

    def _resolve_local_decl(
        self,
        scope: ModuleScope,
        parts: list[str],
        expected: set[str] | None,
    ) -> tuple[str | None, str | None]:
        if not parts:
            return None, None
        name = parts[0]
        candidates: list[str] = []
        if name in scope.entities and (expected is None or "entity" in expected):
            candidates.append("entity")
        if name in scope.values and (expected is None or "value" in expected):
            candidates.append("value")
        if name in scope.funcs and (expected is None or "func" in expected):
            candidates.append("func")
        if name in scope.events and (expected is None or "event" in expected):
            candidates.append("event")
        if name in scope.types and (expected is None or "type" in expected):
            candidates.append("type")
        if not candidates:
            return None, None
        return scope.module.name, name

    def all_rules(self) -> list[RuleInfo]:
        rules: list[RuleInfo] = []
        for scope in self.scopes.values():
            for decl in scope.rules:
                index = len(rules)
                rules.append(RuleInfo(index=index, module=scope.module.name, decl=decl, full_name=f"{scope.module.name}.{decl.name}"))
        return rules


class BoundedEncoder:
    def __init__(self, problem: SolverProblem, horizon: int):
        self.problem = problem
        self.horizon = horizon
        self.ctx = z3
        self.solver = z3.Solver()
        self.diagnostics: list[Diagnostic] = []
        self.tracked: dict[str, TrackedConstraint] = {}
        self.track_counter = 0
        self.entity_values: dict[tuple[str, str, int], ZValue] = {}
        self.value_cache: dict[tuple[str, str], ZValue] = {}
        self.event_symbols: dict[tuple[str, str, int], Any] = {}
        self.rule_infos = self.problem.all_rules()
        self.rule_labels: dict[str, Any] = {}
        self.rule_applications: dict[str, Any] = {}
        self.function_definitions_in_progress: set[str] = set()

    # ------------------------------------------------------------------
    # Top-level encoding
    # ------------------------------------------------------------------

    def encode(self) -> None:
        try:
            self.encode_entities()
            self.encode_values()
            self.encode_facts()
            self.encode_rules()
        except SolveError as exc:
            self.diagnostics.append(Diagnostic(str(exc), severity="error", code="solve-error"))

    def encode_entities(self) -> None:
        for scope in self.problem.scopes.values():
            for entity in scope.entities.values():
                for t in range(self.horizon):
                    self.entity_value(entity, t)

    def encode_values(self) -> None:
        for scope in self.problem.scopes.values():
            for name in scope.values:
                self.value_decl(scope.module.name, name)

    def encode_facts(self) -> None:
        for scope in self.problem.scopes.values():
            for index, fact in enumerate(scope.facts, start=1):
                if fact.target:
                    module_name, local = self.problem.resolve_decl(scope, fact.target, expected={"entity", "value"})
                    if module_name is None or local is None:
                        self.unresolved(scope, fact.target, fact)
                        continue
                    if local in self.problem.scopes[module_name].entities:
                        entity = self.problem.scopes[module_name].entities[local]
                        for t in range(self.horizon):
                            target = self.entity_value(entity, t)
                            value = self.compile_expr(fact.value, scope, t, expected=target.typ)
                            self.track(
                                self.equal_values(target, value),
                                kind="fact",
                                module=scope.module.name,
                                name=fact.target,
                                line=fact.line or 1,
                                column=fact.column or 1,
                            )
                    else:
                        target = self.value_decl(module_name, local)
                        value = self.compile_expr(fact.value, scope, 0, expected=target.typ)
                        self.track(
                            self.equal_values(target, value),
                            kind="fact",
                            module=scope.module.name,
                            name=fact.target,
                            line=fact.line or 1,
                            column=fact.column or 1,
                        )
                else:
                    formula = self.compile_formula(fact.value, scope, 0)
                    self.track(
                        formula,
                        kind="fact",
                        module=scope.module.name,
                        name=f"bare_fact_{index}",
                        line=fact.line or 1,
                        column=fact.column or 1,
                    )

    def encode_rules(self) -> None:
        defeats = self.defeat_pairs()
        for info in self.rule_infos:
            self.rule_labels[info.full_name] = z3.Bool(f"lambda__{self.safe(info.full_name)}")

        for info in self.rule_infos:
            scope = self.problem.scopes[info.module]
            label = self.rule_labels[info.full_name]
            app = self.compile_formula(info.decl.antecedent, scope, 0) if info.decl.antecedent is not None else z3.BoolVal(True)
            self.rule_applications[info.full_name] = app
            higher_apps = [self.rule_applications.get(high.full_name) for high, low in defeats if low.full_name == info.full_name]
            # Higher applications may not yet have been encoded when the high rule appears later.
            higher_apps = [
                self.compile_formula(high.decl.antecedent, self.problem.scopes[high.module], 0) if high.decl.antecedent is not None else z3.BoolVal(True)
                for high, low in defeats if low.full_name == info.full_name
            ]
            no_defeat = z3.And([z3.Not(h) for h in higher_apps]) if higher_apps else z3.BoolVal(True)
            self.track(
                z3.Implies(label, app),
                kind="rule-applicability",
                module=info.module,
                name=info.decl.name,
                line=info.decl.line or 1,
                column=info.decl.column or 1,
            )
            self.track(
                z3.Implies(z3.And(app, no_defeat), label),
                kind="rule-maximality",
                module=info.module,
                name=info.decl.name,
                line=info.decl.line or 1,
                column=info.decl.column or 1,
            )
            for high_app in higher_apps:
                self.track(
                    z3.Not(z3.And(label, high_app)),
                    kind="rule-defeat",
                    module=info.module,
                    name=info.decl.name,
                    line=info.decl.line or 1,
                    column=info.decl.column or 1,
                )
            requirement = self.rule_requirement(info)
            self.track(
                z3.Implies(label, requirement),
                kind="rule-requirement",
                module=info.module,
                name=info.decl.name,
                line=info.decl.line or 1,
                column=info.decl.column or 1,
            )
            if info.decl.otherwise is not None:
                otherwise_requirement = self.modality_formula(info, info.decl.otherwise)
                self.track(
                    z3.Implies(z3.Not(app), otherwise_requirement),
                    kind="rule-otherwise",
                    module=info.module,
                    name=info.decl.name,
                    line=info.decl.line or 1,
                    column=info.decl.column or 1,
                )

    def defeat_pairs(self) -> list[tuple[RuleInfo, RuleInfo]]:
        by_name: dict[str, RuleInfo] = {}
        for info in self.rule_infos:
            by_name[info.decl.name] = info
            by_name[info.full_name] = info
        pairs: list[tuple[RuleInfo, RuleInfo]] = []
        for scope in self.problem.scopes.values():
            for priority in scope.priorities:
                resolved = []
                for name in priority.chain:
                    info = by_name.get(name) or by_name.get(f"{scope.module.name}.{name}")
                    if info is None:
                        self.diagnostics.append(Diagnostic(
                            f"priority references unknown rule {name!r}",
                            line=priority.line or 1,
                            column=priority.column or 1,
                            severity="error",
                            code="unknown-priority-rule",
                            path=str(scope.path) if scope.path else None,
                        ))
                    else:
                        resolved.append(info)
                for high, low in zip(resolved, resolved[1:]):
                    if low.decl.strength != "strict":
                        pairs.append((high, low))
        return pairs

    def rule_requirement(self, info: RuleInfo) -> Any:
        if info.decl.strength == "defeater":
            return z3.BoolVal(True)
        if info.decl.modality == "P" and self.problem.options.permission == "ignore":
            return z3.BoolVal(True)
        return self.modality_formula(info, info.decl.body)

    def modality_formula(self, info: RuleInfo, expr: A.Expr | None) -> Any:
        scope = self.problem.scopes[info.module]
        body = self.compile_formula(expr, scope, 0)
        if info.decl.modality == "F":
            return z3.Not(body)
        return body

    # ------------------------------------------------------------------
    # Expression and temporal encoding
    # ------------------------------------------------------------------

    def compile_formula(self, expr: A.Expr | None, scope: ModuleScope, t: int, env: dict[str, ZValue] | None = None) -> Any:
        env = env or {}
        if expr is None:
            return z3.BoolVal(True)
        if isinstance(expr, A.Literal) and expr.kind == "bool":
            return z3.BoolVal(bool(expr.value))
        if isinstance(expr, A.Name) and expr.name == "last":
            return z3.BoolVal(t == self.horizon - 1)
        if isinstance(expr, A.TemporalUnary):
            operand = expr.operand
            if expr.op == "next":
                return self.compile_formula(operand, scope, t + 1, env=env) if t < self.horizon - 1 else z3.BoolVal(False)
            if expr.op == "weak_next":
                return self.compile_formula(operand, scope, t + 1, env=env) if t < self.horizon - 1 else z3.BoolVal(True)
            if expr.op == "eventually":
                return z3.Or([self.compile_formula(operand, scope, u, env=env) for u in range(t, self.horizon)])
            if expr.op == "always":
                return z3.And([self.compile_formula(operand, scope, u, env=env) for u in range(t, self.horizon)])
            if expr.op == "never":
                return z3.Not(z3.Or([self.compile_formula(operand, scope, u, env=env) for u in range(t, self.horizon)]))
        if isinstance(expr, A.TemporalBinary):
            if expr.op == "until":
                return self.until(expr.left, expr.right, scope, t, env)
            if expr.op == "weak_until":
                return z3.Or(
                    self.until(expr.left, expr.right, scope, t, env),
                    z3.And([self.compile_formula(expr.left, scope, u, env=env) for u in range(t, self.horizon)]),
                )
            if expr.op == "release":
                return z3.Not(self.until(A.UnaryOp(op="not", operand=expr.left), A.UnaryOp(op="not", operand=expr.right), scope, t, env))
        if isinstance(expr, A.UnaryOp) and expr.op == "not":
            return z3.Not(self.compile_formula(expr.operand, scope, t, env=env))
        if isinstance(expr, A.BinaryOp) and expr.op in {"and", "or"}:
            left = self.compile_formula(expr.left, scope, t, env=env)
            right = self.compile_formula(expr.right, scope, t, env=env)
            if expr.op == "and":
                return z3.And(left, right)
            return z3.Or(left, right)
        return self.as_bool(self.compile_expr(expr, scope, t, expected=BOOL, env=env))

    def until(self, left: A.Expr | None, right: A.Expr | None, scope: ModuleScope, t: int, env: dict[str, ZValue]) -> Any:
        disjuncts = []
        for u in range(t, self.horizon):
            prefix = [self.compile_formula(left, scope, v, env=env) for v in range(t, u)]
            disjuncts.append(z3.And([*prefix, self.compile_formula(right, scope, u, env=env)]))
        return z3.Or(disjuncts) if disjuncts else z3.BoolVal(False)

    def compile_expr(self, expr: A.Expr | None, scope: ModuleScope, t: int, *, expected: TypeSpec | None = None, env: dict[str, ZValue] | None = None) -> ZValue:
        if expr is None:
            return self.literal_value(None, "unit", expected)
        env = env or {}
        if isinstance(expr, A.Literal):
            return self.literal_value(expr.value, expr.kind, expected)
        if isinstance(expr, A.Name):
            if expr.name in env:
                return env[expr.name]
            constructor = self.find_constructor(expr.name, expected, scope)
            if constructor is not None:
                if constructor.variant.fields:
                    raise UnsupportedExpression(f"constructor {expr.name!r} expects arguments; use {expr.name}(()) for unit payloads")
                return self.constructor_value(constructor, [])
            if isinstance(expected, PrimitiveType):
                concrete = self.try_runtime_eval(expr, scope)
                if concrete is not _NO_CONCRETE:
                    return self.python_value(concrete, expected)
            return self.resolve_value(expr.name, scope, t, expected)
        if isinstance(expr, A.RecordConstructor):
            typ = self.problem.resolve_type(
                scope.module.name,
                A.TypeRef(name=expr.type_name, line=expr.line, column=expr.column),
            )
            resolved = self.resolve_named_type(typ)
            if not isinstance(resolved, RecordSpec):
                raise UnsupportedExpression(f"type {expr.type_name!r} is not a record type")
            expected_fields = dict(resolved.fields)
            provided = {name for name, _ in expr.fields}
            duplicates = [name for name in provided if sum(1 for field, _ in expr.fields if field == name) > 1]
            if duplicates:
                raise UnsupportedExpression(f"duplicate record field {duplicates[0]!r}")
            missing = sorted(set(expected_fields) - provided)
            if missing:
                raise UnsupportedExpression(f"missing record field {missing[0]!r}")
            extra = sorted(provided - set(expected_fields))
            if extra:
                raise UnsupportedExpression(f"unknown field {extra[0]!r}")
            fields = {
                name: self.compile_expr(value_expr, scope, t, expected=expected_fields[name], env=env)
                for name, value_expr in expr.fields
            }
            return self.product_value(typ, fields)
        concrete = self.try_runtime_eval(expr, scope)
        if concrete is not _NO_CONCRETE:
            return self.python_value(concrete, expected)
        if isinstance(expr, A.FieldAccess):
            target = self.compile_expr(expr.target, scope, t, env=env)
            return self.field_value(target, expr.field)
        if isinstance(expr, A.Call):
            return self.call_value(expr, scope, t, expected, env)
        if isinstance(expr, A.UnaryOp):
            value = self.compile_expr(expr.operand, scope, t, env=env)
            if expr.op == "not":
                return ZValue(BOOL, expr=z3.Not(self.as_bool(value)))
            if expr.op == "-":
                return ZValue(value.typ, expr=-self.as_numeric(value))
        if isinstance(expr, A.BinaryOp):
            return self.binary_value(expr, scope, t, expected, env)
        if isinstance(expr, A.IfExpr):
            cond = self.compile_formula(expr.condition, scope, t, env=env)
            then_v = self.compile_expr(expr.then_branch, scope, t, expected=expected, env=env)
            else_v = self.compile_expr(expr.else_branch, scope, t, expected=then_v.typ, env=env)
            return self.if_value(cond, then_v, else_v)
        if isinstance(expr, A.LetExpr):
            value = self.compile_expr(expr.value, scope, t, env=env)
            local = dict(env)
            self.bind_pattern(expr.pattern, value, local, assumptions=[])
            return self.compile_expr(expr.body, scope, t, expected=expected, env=local)
        if isinstance(expr, A.MatchExpr):
            return self.match_value(expr, scope, t, expected, env)
        if isinstance(expr, A.TupleLiteral):
            expected_resolved = self.resolve_named_type(expected) if expected is not None else None
            expected_items = list(expected_resolved.items) if isinstance(expected_resolved, TupleSpec) else []
            items = {
                f"_{idx}": self.compile_expr(item, scope, t, expected=expected_items[idx] if idx < len(expected_items) else None, env=env)
                for idx, item in enumerate(expr.items)
            }
            typ: TypeSpec = expected if isinstance(expected_resolved, TupleSpec) and expected is not None else TupleSpec(tuple(item.typ for item in items.values()))
            return self.product_value(typ, items)
        raise UnsupportedExpression(f"unsupported expression: {format_expr(expr)}")

    def binary_value(self, expr: A.BinaryOp, scope: ModuleScope, t: int, expected: TypeSpec | None, env: dict[str, ZValue]) -> ZValue:
        if expr.op in {"and", "or"}:
            return ZValue(BOOL, expr=self.compile_formula(expr, scope, t, env=env))
        left = self.compile_expr(expr.left, scope, t, env=env)
        right = self.compile_expr(expr.right, scope, t, expected=left.typ, env=env)
        if expr.op == "=":
            return ZValue(BOOL, expr=self.equal_values(left, right))
        if expr.op == "!=":
            return ZValue(BOOL, expr=z3.Not(self.equal_values(left, right)))
        if expr.op in {"<", "<=", ">", ">="}:
            l_num, r_num = self.promote_numeric(left, right)
            if expr.op == "<":
                return ZValue(BOOL, expr=l_num < r_num)
            if expr.op == "<=":
                return ZValue(BOOL, expr=l_num <= r_num)
            if expr.op == ">":
                return ZValue(BOOL, expr=l_num > r_num)
            return ZValue(BOOL, expr=l_num >= r_num)
        if expr.op in {"+", "-", "*", "/", "%"}:
            l_num, r_num = self.promote_numeric(left, right)
            typ = REAL if expr.op == "/" else left.typ
            if expr.op == "+":
                return ZValue(typ, expr=l_num + r_num)
            if expr.op == "-":
                return ZValue(typ, expr=l_num - r_num)
            if expr.op == "*":
                return ZValue(typ, expr=l_num * r_num)
            if expr.op == "/":
                return ZValue(REAL, expr=l_num / r_num)
            return ZValue(INT, expr=l_num % r_num)
        raise UnsupportedExpression(f"unsupported binary operator {expr.op!r}")

    def call_value(self, expr: A.Call, scope: ModuleScope, t: int, expected: TypeSpec | None, env: dict[str, ZValue]) -> ZValue:
        name = self.expr_to_name(expr.func)
        if name == "to_list" or name.endswith("strings.to_list") or name in {"strings.to_list", "std.system.strings.to_list"}:
            arg = self.compile_expr(expr.args[0], scope, t, env=env)
            if arg.has_concrete and isinstance(arg.concrete, str):
                func = self.resolve_func(name, scope)
                return_type = func.return_type if func is not None else expected
                if return_type is None:
                    return_type = self.std_list_type(STRING)
                return self.adt_list_value(list(arg.concrete), return_type)
            raise UnsupportedExpression("strings.to_list requires a concrete string in solve")
        event = self.resolve_event(name, scope)
        if event is not None:
            args = [self.compile_expr(arg, scope, t, expected=typ, env=env) for arg, (_, typ) in zip(expr.args, event.fields)]
            symbol = self.event_symbol(event, t)
            if event.fields:
                return ZValue(BOOL, expr=symbol(*[self.primitive_expr(a) for a in args]))
            return ZValue(BOOL, expr=symbol)
        constructor = self.find_constructor(name, expected, scope)
        if constructor is not None:
            zero_unit = len(expr.args) == 0 and len(constructor.variant.fields) == 1 and constructor.variant.fields[0][1] == UNIT
            if len(expr.args) != len(constructor.variant.fields) and not zero_unit:
                raise UnsupportedExpression(f"constructor {name!r} expects {len(constructor.variant.fields)} args")
            values = [
                self.compile_expr(arg, scope, t, expected=field_type, env=env)
                for arg, (_, field_type) in zip(expr.args, constructor.variant.fields)
            ]
            refined = self.refined_constructor_info(constructor, values)
            if refined.sum_type is not constructor.sum_type:
                values = [
                    self.compile_expr(arg, scope, t, expected=field_type, env=env)
                    for arg, (_, field_type) in zip(expr.args, refined.variant.fields)
                ]
            return self.constructor_value(refined, values)
        func = self.resolve_func(name, scope)
        if func is not None:
            args = [self.compile_expr(arg, scope, t, expected=typ, env=env) for arg, typ in zip(expr.args, func.params)]
            instance = self.instantiate_function(func, args, expected)
            if instance.params != func.params:
                args = [self.compile_expr(arg, scope, t, expected=typ, env=env) for arg, typ in zip(expr.args, instance.params)]
                instance = self.instantiate_function(func, args, expected)
            fn = self.function_symbol(instance)
            return ZValue(instance.return_type, expr=fn(*[self.primitive_expr(arg) for arg in args]))
        return self.opaque_atom(format_expr(expr), t)

    # ------------------------------------------------------------------
    # Values, symbols and name resolution
    # ------------------------------------------------------------------

    def entity_value(self, entity: EntityInfo, t: int) -> ZValue:
        key = (entity.module, entity.decl.name, t)
        if key not in self.entity_values:
            self.entity_values[key] = self.fresh_value(f"{entity.module}.{entity.decl.name}@{t}", entity.typ)
        return self.entity_values[key]

    def value_decl(self, module_name: str, name: str) -> ZValue:
        key = (module_name, name)
        if key in self.value_cache:
            return self.value_cache[key]
        scope = self.problem.scopes[module_name]
        decl = scope.values[name]
        expected = self.problem.resolve_type(module_name, decl.type_annotation) if decl.type_annotation else None
        value = self.compile_expr(decl.value, scope, 0, expected=expected)
        self.value_cache[key] = value
        return value

    def fresh_value(self, prefix: str, typ: TypeSpec) -> ZValue:
        safe = self.safe(prefix)
        if isinstance(typ, PrimitiveType):
            if typ.name == "bool":
                return ZValue(typ, expr=z3.Bool(safe))
            if typ.name == "int":
                return ZValue(typ, expr=z3.Int(safe))
            if typ.name in {"rat", "decimal"}:
                return ZValue(REAL, expr=z3.Real(safe))
            if typ.name == "string":
                return ZValue(STRING, expr=z3.String(safe))
            return ZValue(UNIT, expr=z3.Int(safe))
        sort = self.sort_for(typ)
        return ZValue(typ, expr=z3.Const(safe, sort))

    def resolve_value(self, name: str, scope: ModuleScope, t: int, expected: TypeSpec | None = None) -> ZValue:
        module_name, local = self.problem.resolve_decl(scope, name, expected={"entity", "value", "func", "event"})
        if module_name is None or local is None:
            self.unresolved(scope, name)
            return self.opaque_atom(name, t)
        target_scope = self.problem.scopes.get(module_name)
        if target_scope is None:
            return self.opaque_atom(name, t)
        parts = split_qualified(name)
        module_parts = split_qualified(module_name)
        if parts[:len(module_parts)] == module_parts and len(parts) > len(module_parts) and parts[len(module_parts)] == local:
            field_parts = parts[len(module_parts) + 1:]
        else:
            field_parts = parts[1:]
        if local in target_scope.entities:
            value = self.entity_value(target_scope.entities[local], t)
        elif local in target_scope.values:
            value = self.value_decl(module_name, local)
        elif local in target_scope.events:
            value = ZValue(BOOL, expr=self.event_symbol(target_scope.events[local], t))
        else:
            return self.opaque_atom(name, t)
        for field_name in field_parts:
            value = self.field_value(value, field_name)
        return value

    def field_value(self, value: ZValue, field: str) -> ZValue:
        if value.fields and field in value.fields:
            return value.fields[field]
        if value.has_concrete and isinstance(value.concrete, dict):
            return self.python_value(value.concrete[field], None)
        typ = self.resolve_named_type(value.typ)
        if isinstance(typ, RecordSpec):
            for idx, (name, field_type) in enumerate(typ.fields):
                if name == field:
                    encoding = self.datatype_encoding(value.typ)
                    accessor = encoding.accessors[self.product_constructor_name()][idx]
                    return ZValue(field_type, expr=accessor(self.primitive_expr(value)))
        if isinstance(typ, TupleSpec) and field.startswith("_"):
            try:
                idx = int(field[1:])
            except ValueError:
                idx = -1
            if 0 <= idx < len(typ.items):
                encoding = self.datatype_encoding(value.typ)
                accessor = encoding.accessors[self.product_constructor_name()][idx]
                return ZValue(typ.items[idx], expr=accessor(self.primitive_expr(value)))
        raise UnsupportedExpression(f"field {field!r} is not available on value")

    def resolve_event(self, name: str, scope: ModuleScope) -> EventInfo | None:
        module_name, local = self.problem.resolve_decl(scope, name, expected={"event"})
        if module_name and local and module_name in self.problem.scopes:
            return self.problem.scopes[module_name].events.get(local)
        return None

    def resolve_func(self, name: str, scope: ModuleScope) -> FunctionInfo | None:
        module_name, local = self.problem.resolve_decl(scope, name, expected={"func"})
        if module_name and local and module_name in self.problem.scopes:
            return self.problem.scopes[module_name].funcs.get(local)
        return None

    def instantiate_function(self, func: FunctionInfo, args: list[ZValue], expected: TypeSpec | None) -> FunctionInfo:
        if not func.decl.type_params:
            return func
        params = set(func.decl.type_params)
        bindings: dict[str, TypeSpec] = {}
        for formal, actual in zip(func.params, args):
            self.unify_type_params(formal, actual.typ, bindings, params)
        if expected is not None:
            self.unify_type_params(func.return_type, expected, bindings, params)
        type_args = tuple(bindings.get(param, OpaqueSpec(param)) for param in func.decl.type_params)
        type_env = dict(zip(func.decl.type_params, type_args))
        return FunctionInfo(
            module=func.module,
            decl=func.decl,
            params=[self.substitute_type_params(param, type_env, params) for param in func.params],
            return_type=self.substitute_type_params(func.return_type, type_env, params),
            type_args=type_args,
        )

    def substitute_type_params(
        self,
        typ: TypeSpec,
        bindings: dict[str, TypeSpec],
        params: set[str],
    ) -> TypeSpec:
        if isinstance(typ, OpaqueSpec) and typ.name in params:
            return bindings.get(typ.name, typ)
        if isinstance(typ, PrimitiveType):
            return typ
        origin = self.type_origin(typ)
        if origin is not None:
            module_name, local, args = origin
            substituted_args = tuple(self.substitute_type_params(arg, bindings, params) for arg in args)
            if substituted_args != args:
                return self.problem.resolve_declared_type(module_name, local, substituted_args)
        resolved = self.resolve_named_type(typ)
        if isinstance(resolved, RecordSpec):
            return RecordSpec(tuple(
                (name, self.substitute_type_params(field_type, bindings, params))
                for name, field_type in resolved.fields
            ))
        if isinstance(resolved, TupleSpec):
            return TupleSpec(tuple(self.substitute_type_params(item, bindings, params) for item in resolved.items))
        if isinstance(resolved, SumSpec):
            return SumSpec(resolved.name, tuple(
                VariantSpec(variant.name, tuple(
                    (label, self.substitute_type_params(field_type, bindings, params))
                    for label, field_type in variant.fields
                ))
                for variant in resolved.variants
            ))
        return typ

    def event_symbol(self, event: EventInfo, t: int) -> Any:
        key = (event.module, event.decl.name, t)
        if key in self.event_symbols:
            return self.event_symbols[key]
        name = self.safe(f"event.{event.module}.{event.decl.name}@{t}")
        if event.fields:
            symbol = z3.Function(name, *[self.sort_for(typ) for _, typ in event.fields], z3.BoolSort())
        else:
            symbol = z3.Bool(name)
        self.event_symbols[key] = symbol
        return symbol

    def function_key(self, func: FunctionInfo) -> str:
        key = f"func.{func.module}.{func.decl.name}"
        if func.type_args:
            key += "<" + ",".join(self.type_key(arg) for arg in func.type_args) + ">"
        return key

    def type_key(self, typ: TypeSpec) -> str:
        if isinstance(typ, PrimitiveType):
            return typ.name
        if isinstance(typ, OpaqueSpec):
            return typ.name
        if self.is_datatype_type(typ):
            return self.datatype_key(typ)
        if isinstance(typ, NamedSpec):
            return typ.name
        return repr(typ)

    def function_symbol(self, func: FunctionInfo) -> Any:
        key = self.function_key(func)
        if key not in self.problem.functions:
            factory = z3.RecFunction if self.can_define_function(func) else z3.Function
            self.problem.functions[key] = factory(
                self.safe(key),
                *[self.sort_for(typ) for typ in func.params],
                self.sort_for(func.return_type),
            )
        if (
            self.can_define_function(func)
            and key not in self.problem.function_definitions
            and key not in self.function_definitions_in_progress
        ):
            self.add_function_definition(key, func)
        return self.problem.functions[key]

    def can_define_function(self, func: FunctionInfo) -> bool:
        return all(isinstance(param.pattern, A.VarPattern) for param in func.decl.params)

    def add_function_definition(self, key: str, func: FunctionInfo) -> None:
        self.function_definitions_in_progress.add(key)
        try:
            args = [
                z3.Const(self.safe(f"{key}.arg.{idx}.{param.pattern.name}"), self.sort_for(param_type))
                for idx, (param, param_type) in enumerate(zip(func.decl.params, func.params))
                if isinstance(param.pattern, A.VarPattern)
            ]
            env = {
                param.pattern.name: ZValue(param_type, expr=arg)
                for param, param_type, arg in zip(func.decl.params, func.params, args)
                if isinstance(param.pattern, A.VarPattern)
            }
            scope = self.problem.scopes[func.module]
            body = self.compile_block(func.decl.body, scope, 0, func.return_type, env)
            z3.RecAddDefinition(self.problem.functions[key], args, self.primitive_expr(body))
            self.problem.function_definitions.add(key)
        finally:
            self.function_definitions_in_progress.remove(key)

    def opaque_atom(self, text: str, t: int) -> ZValue:
        return ZValue(BOOL, expr=z3.Bool(self.safe(f"atom.{text}@{t}")))

    def opaque_call(self, name: str, args: list[ZValue], return_type: TypeSpec) -> ZValue:
        sort = self.sort_for(return_type)
        fn = self.problem.functions.setdefault(
            f"opaque.{name}",
            z3.Function(self.safe(f"opaque.{name}"), *[self.sort_for(arg.typ) for arg in args], sort),
        )
        return ZValue(return_type, expr=fn(*[self.primitive_expr(arg) for arg in args]))

    # ------------------------------------------------------------------
    # Pattern matching
    # ------------------------------------------------------------------

    def bind_pattern(self, pattern: A.Pattern | None, value: ZValue, env: dict[str, ZValue], *, assumptions: list[Any]) -> None:
        if pattern is None or isinstance(pattern, A.WildcardPattern):
            return
        if isinstance(pattern, A.VarPattern):
            env[pattern.name] = value
            return
        cond, bindings = self.pattern_condition(pattern, value)
        assumptions.append(cond)
        env.update(bindings)

    def match_value(self, expr: A.MatchExpr, scope: ModuleScope, t: int, expected: TypeSpec | None, env: dict[str, ZValue]) -> ZValue:
        subject = self.compile_expr(expr.subject, scope, t, env=env)
        branches: list[tuple[Any, ZValue]] = []
        for arm in expr.arms:
            cond, bindings = self.pattern_condition(arm.pattern, subject)
            local = dict(env)
            local.update(bindings)
            if arm.guard is not None:
                cond = z3.And(cond, self.compile_formula(arm.guard, scope, t, env=local))
            body = self.compile_block(arm.body, scope, t, expected, local)
            branches.append((cond, body))
        if not branches:
            raise UnsupportedExpression("empty match")
        result = branches[-1][1]
        for cond, value in reversed(branches[:-1]):
            result = self.if_value(cond, value, result)
        return result

    def compile_block(self, block: A.Block | None, scope: ModuleScope, t: int, expected: TypeSpec | None, env: dict[str, ZValue]) -> ZValue:
        local = dict(env)
        if block is None:
            return self.python_value(None, expected)
        for stmt in block.statements:
            value = self.compile_expr(stmt.value, scope, t, expected=self.problem.resolve_type(scope.module.name, stmt.type_annotation) if stmt.type_annotation else None, env=local)
            self.bind_pattern(stmt.pattern, value, local, assumptions=[])
        return self.compile_expr(block.result, scope, t, expected=expected, env=local)

    def pattern_condition(self, pattern: A.Pattern | None, value: ZValue) -> tuple[Any, dict[str, ZValue]]:
        if pattern is None or isinstance(pattern, A.WildcardPattern):
            return z3.BoolVal(True), {}
        if isinstance(pattern, A.VarPattern):
            return z3.BoolVal(True), {pattern.name: value}
        if isinstance(pattern, A.LiteralPattern):
            lit = self.literal_value(pattern.value, pattern.kind, value.typ)
            return self.equal_values(value, lit), {}
        if isinstance(pattern, A.RecordPattern):
            conds = []
            bindings: dict[str, ZValue] = {}
            for field, nested in pattern.fields:
                field_value = self.field_value(value, field)
                if nested is None:
                    bindings[field] = field_value
                else:
                    cond, nested_bindings = self.pattern_condition(nested, field_value)
                    conds.append(cond)
                    bindings.update(nested_bindings)
            return z3.And(conds) if conds else z3.BoolVal(True), bindings
        if isinstance(pattern, A.TuplePattern):
            conds = []
            bindings: dict[str, ZValue] = {}
            for idx, nested in enumerate(pattern.items):
                item_value = self.field_value(value, f"_{idx}")
                cond, nested_bindings = self.pattern_condition(nested, item_value)
                conds.append(cond)
                bindings.update(nested_bindings)
            return z3.And(conds) if conds else z3.BoolVal(True), bindings
        if isinstance(pattern, A.ConstructorPattern):
            sum_type = self.resolve_named_type(value.typ)
            if not isinstance(sum_type, SumSpec):
                raise UnsupportedExpression("constructor pattern requires a sum value")
            variant_name = pattern.name.split(".")[-1]
            variant = next((item for item in sum_type.variants if item.name == variant_name), None)
            if variant is None:
                raise UnsupportedExpression(f"unknown constructor {variant_name!r}")
            zero_unit = len(pattern.args) == 0 and len(variant.fields) == 1 and variant.fields[0][1] == UNIT
            if len(pattern.args) != len(variant.fields) and not zero_unit:
                return z3.BoolVal(False), {}
            encoding = self.datatype_encoding(value.typ)
            conds = [encoding.recognizers[variant_name](self.primitive_expr(value))]
            bindings: dict[str, ZValue] = {}
            if zero_unit:
                return z3.And(conds), bindings
            accessors = encoding.accessors[variant_name]
            for idx, (nested, (_, field_type)) in enumerate(zip(pattern.args, variant.fields)):
                payload = ZValue(field_type, expr=accessors[idx](self.primitive_expr(value)))
                cond, nested_bindings = self.pattern_condition(nested, payload)
                conds.append(cond)
                bindings.update(nested_bindings)
            return z3.And(conds), bindings
        if isinstance(pattern, A.ListPattern) and value.has_concrete and isinstance(value.concrete, list):
            if len(pattern.items) != len(value.concrete):
                return z3.BoolVal(False), {}
            conds = []
            bindings: dict[str, ZValue] = {}
            for nested, item in zip(pattern.items, value.concrete):
                cond, nested_bindings = self.pattern_condition(nested, item)
                conds.append(cond)
                bindings.update(nested_bindings)
            return z3.And(conds) if conds else z3.BoolVal(True), bindings
        raise UnsupportedExpression("unsupported pattern in solve")

    # ------------------------------------------------------------------
    # Constructors, literals and equality
    # ------------------------------------------------------------------

    def find_constructor(self, name: str, expected: TypeSpec | None, scope: ModuleScope | None = None) -> ConstructorInfo | None:
        parts = split_qualified(name)
        if len(parts) < 2:
            return None
        variant_name = parts[-1]
        type_name = ".".join(parts[:-1])
        if scope is None:
            expected_resolved = self.resolve_named_type(expected) if expected is not None else None
            origin = self.type_origin(expected_resolved) if expected_resolved is not None else None
            if origin is None:
                return None
            module_name, local_type, arg_specs = origin
            if type_name not in {local_type, f"{module_name}.{local_type}"}:
                return None
            typ = expected_resolved
            if isinstance(typ, SumSpec):
                for variant in typ.variants:
                    if variant.name == variant_name:
                        return ConstructorInfo(typ, variant, module_name, local_type, arg_specs)
            return None
        if scope is None:
            return None
        module_name, local_type = self.problem.resolve_decl(scope, type_name, expected={"type"})
        if module_name is None or local_type is None:
            return None
        decl = self.problem.scopes[module_name].types.get(local_type)
        if decl is None or not isinstance(decl.definition, A.SumType):
            return None
        expected_resolved = self.resolve_named_type(expected) if expected is not None else None
        origin = self.type_origin(expected_resolved) if expected_resolved is not None else None
        if (
            isinstance(expected_resolved, SumSpec)
            and origin is not None
            and origin[0] == module_name
            and origin[1] == local_type
        ):
            typ = expected_resolved
            arg_specs = origin[2]
        else:
            arg_specs = tuple(OpaqueSpec(param) for param in decl.params)
            typ = self.problem.resolve_declared_type(module_name, local_type, arg_specs)
        if isinstance(typ, SumSpec):
            for variant in typ.variants:
                if variant.name == variant_name:
                    return ConstructorInfo(typ, variant, module_name, local_type, arg_specs)
        return None

    def type_origin(self, typ: TypeSpec) -> tuple[str, str, tuple[TypeSpec, ...]] | None:
        resolved = self.resolve_named_type(typ)
        resolved_name = resolved.name if isinstance(resolved, SumSpec) else None
        for (module_name, local, args), cached in self.problem.type_cache.items():
            cached_resolved = self.resolve_named_type(cached)
            if cached_resolved is resolved:
                return module_name, local, args
            if (
                resolved_name is not None
                and isinstance(cached_resolved, SumSpec)
                and cached_resolved.name == resolved_name
            ):
                return module_name, local, args
        return None

    def refined_constructor_info(self, info: ConstructorInfo, values: list[ZValue]) -> ConstructorInfo:
        if info.module is None or info.local is None:
            return info
        decl = self.problem.scopes[info.module].types[info.local]
        if not decl.params:
            return info
        bindings: dict[str, TypeSpec] = {}
        params = set(decl.params)
        for (_, field_type), value in zip(info.variant.fields, values):
            self.unify_type_params(field_type, value.typ, bindings, params)
        if not bindings:
            return info
        arg_specs = tuple(
            bindings.get(param, info.type_args[idx] if idx < len(info.type_args) else OpaqueSpec(param))
            for idx, param in enumerate(decl.params)
        )
        sum_type = self.problem.resolve_declared_type(info.module, info.local, arg_specs)
        if not isinstance(sum_type, SumSpec):
            return info
        for variant in sum_type.variants:
            if variant.name == info.variant.name:
                return ConstructorInfo(sum_type, variant, info.module, info.local, arg_specs)
        return info

    def unify_type_params(
        self,
        pattern: TypeSpec,
        actual: TypeSpec,
        bindings: dict[str, TypeSpec],
        params: set[str],
        seen: set[tuple[int, int]] | None = None,
    ) -> None:
        seen = seen or set()
        pair = (id(pattern), id(actual))
        if pair in seen:
            return
        seen.add(pair)
        pattern = self.resolve_named_type(pattern)
        actual = self.resolve_named_type(actual)
        if isinstance(pattern, OpaqueSpec) and pattern.name in params:
            if not (isinstance(actual, OpaqueSpec) and actual.name == pattern.name):
                bindings.setdefault(pattern.name, actual)
            return
        if isinstance(pattern, SumSpec) and isinstance(actual, SumSpec) and pattern.name == actual.name:
            return
        if isinstance(pattern, RecordSpec) and isinstance(actual, RecordSpec):
            for (_, p_type), (_, a_type) in zip(pattern.fields, actual.fields):
                self.unify_type_params(p_type, a_type, bindings, params, seen)
            return
        if isinstance(pattern, TupleSpec) and isinstance(actual, TupleSpec):
            for p_type, a_type in zip(pattern.items, actual.items):
                self.unify_type_params(p_type, a_type, bindings, params, seen)
            return
        if isinstance(pattern, SumSpec) and isinstance(actual, SumSpec):
            for p_variant, a_variant in zip(pattern.variants, actual.variants):
                for (_, p_type), (_, a_type) in zip(p_variant.fields, a_variant.fields):
                    self.unify_type_params(p_type, a_type, bindings, params, seen)

    def constructor_value(self, info: ConstructorInfo, values: list[ZValue]) -> ZValue:
        encoding = self.datatype_encoding(info.sum_type)
        constructor = encoding.constructors[info.variant.name]
        if not values and len(info.variant.fields) == 1 and info.variant.fields[0][1] == UNIT:
            values = [self.literal_value(None, "unit", UNIT)]
        exprs = [self.primitive_expr(value) for value in values]
        return ZValue(info.sum_type, expr=constructor(*exprs))

    def std_list_type(self, item_type: TypeSpec) -> TypeSpec:
        if "std.collections" not in self.problem.scopes:
            raise UnsupportedExpression("std.collections is not available")
        return self.problem.resolve_declared_type("std.collections", "List", (item_type,))

    def adt_list_value(self, items: list[Any], expected: TypeSpec | None) -> ZValue:
        if expected is None:
            item_type = self.python_value(items[0], None).typ if items else OpaqueSpec("item")
            list_type = self.std_list_type(item_type)
        else:
            list_type = expected
        sum_type = self.resolve_named_type(list_type)
        if not isinstance(sum_type, SumSpec):
            raise UnsupportedExpression("expected std List ADT")
        variants = {variant.name: variant for variant in sum_type.variants}
        if "Empty" not in variants or "Cons" not in variants:
            raise UnsupportedExpression("expected std List constructors Empty and Cons")
        head_type = variants["Cons"].fields[0][1] if variants["Cons"].fields else OpaqueSpec("item")
        if items and isinstance(self.resolve_named_type(head_type), OpaqueSpec):
            head_type = self.python_value(items[0], None).typ
            list_type = self.std_list_type(head_type)
            sum_type = self.resolve_named_type(list_type)
            if not isinstance(sum_type, SumSpec):
                raise UnsupportedExpression("expected std List ADT")
            variants = {variant.name: variant for variant in sum_type.variants}
            head_type = variants["Cons"].fields[0][1] if variants["Cons"].fields else head_type
        concrete_items = [self.python_value(item, head_type) for item in items]
        empty = ConstructorInfo(sum_type, variants["Empty"])
        empty_values = [
            self.literal_value(None, "unit", field_type)
            for _, field_type in empty.variant.fields
        ]
        tail = self.constructor_value(empty, empty_values)
        for head in reversed(concrete_items):
            tail = self.constructor_value(ConstructorInfo(sum_type, variants["Cons"]), [head, tail])
        tail.concrete = concrete_items
        tail.has_concrete = True
        return tail

    def product_value(self, typ: TypeSpec, fields: dict[str, ZValue]) -> ZValue:
        resolved = self.resolve_named_type(typ)
        encoding = self.datatype_encoding(typ)
        constructor = encoding.constructors[self.product_constructor_name()]
        if isinstance(resolved, RecordSpec):
            ordered = [fields[name] for name, _ in resolved.fields]
        elif isinstance(resolved, TupleSpec):
            ordered = [fields[f"_{idx}"] for idx in range(len(resolved.items))]
        else:
            raise UnsupportedExpression(f"type {typ!r} is not a product ADT")
        return ZValue(typ, expr=constructor(*[self.primitive_expr(value) for value in ordered]), fields=fields)

    def literal_value(self, value: Any, kind: str, expected: TypeSpec | None = None) -> ZValue:
        if kind == "bool" or isinstance(expected, PrimitiveType) and expected.name == "bool":
            return ZValue(BOOL, expr=z3.BoolVal(bool(value)), concrete=bool(value), has_concrete=True)
        if kind == "int" and not (isinstance(expected, PrimitiveType) and expected.name in {"rat", "decimal"}):
            return ZValue(INT, expr=z3.IntVal(int(value)), concrete=int(value), has_concrete=True)
        if kind in {"rat", "decimal"} or isinstance(expected, PrimitiveType) and expected.name in {"rat", "decimal"}:
            return ZValue(REAL, expr=self.real_val(value), concrete=value, has_concrete=True)
        if kind == "string" or isinstance(expected, PrimitiveType) and expected.name == "string":
            return ZValue(STRING, expr=z3.StringVal(str(value)), concrete=str(value), has_concrete=True)
        return ZValue(UNIT, expr=z3.IntVal(0), concrete=value, has_concrete=True)

    def python_value(self, value: Any, expected: TypeSpec | None) -> ZValue:
        if isinstance(value, bool):
            return self.literal_value(value, "bool", expected)
        if isinstance(value, int):
            return self.literal_value(value, "int", expected)
        if isinstance(value, (float, Fraction)):
            return self.literal_value(value, "rat", expected)
        if isinstance(value, str):
            constructor = self.find_constructor(value, expected)
            if constructor is not None:
                field_types = [self.resolve_named_type(field_type) for _, field_type in constructor.variant.fields]
                if all(isinstance(field_type, PrimitiveType) and field_type.name == "unit" for field_type in field_types):
                    values = [
                        self.literal_value(None, "unit", field_type)
                        for _, field_type in constructor.variant.fields
                    ]
                    return self.constructor_value(constructor, values)
            return self.literal_value(value, "string", expected)
        if isinstance(value, dict):
            expected_resolved = self.resolve_named_type(expected) if expected is not None else None
            expected_fields = dict(expected_resolved.fields) if isinstance(expected_resolved, RecordSpec) else {}
            fields = {k: self.python_value(v, expected_fields.get(k)) for k, v in value.items()}
            typ: TypeSpec = expected if isinstance(expected_resolved, RecordSpec) and expected is not None else RecordSpec(tuple((k, v.typ) for k, v in fields.items()))
            result = self.product_value(typ, fields)
            result.concrete = value
            result.has_concrete = True
            return result
        if isinstance(value, tuple) and value and isinstance(value[0], str):
            constructor = self.find_constructor(value[0], expected)
            if constructor is not None:
                raw_args = value[1] if len(value) > 1 else ()
                values = [
                    self.python_value(arg, field_type)
                    for arg, (_, field_type) in zip(raw_args, constructor.variant.fields)
                ]
                result = self.constructor_value(constructor, values)
                result.concrete = value
                result.has_concrete = True
                return result
        if isinstance(value, list):
            return self.adt_list_value(value, expected)
        if isinstance(value, set):
            raise UnsupportedExpression("set runtime values require an explicit ADT constructor")
        return self.literal_value(value, "unit", expected)

    def equal_values(self, left: ZValue, right: ZValue) -> Any:
        if self.is_numeric(left) and self.is_numeric(right):
            l_num, r_num = self.promote_numeric(left, right)
            return l_num == r_num
        left_type = self.resolve_named_type(left.typ)
        right_type = self.resolve_named_type(right.typ)
        if isinstance(left_type, RecordSpec) and isinstance(right_type, RecordSpec):
            left_fields = {name for name, _ in left_type.fields}
            right_fields = {name for name, _ in right_type.fields}
            common = sorted(left_fields & right_fields)
            if not common:
                return z3.BoolVal(True)
            return z3.And([self.equal_values(self.field_value(left, name), self.field_value(right, name)) for name in common])
        if isinstance(left_type, TupleSpec) and isinstance(right_type, TupleSpec):
            if len(left_type.items) != len(right_type.items):
                return z3.BoolVal(False)
            return z3.And([
                self.equal_values(self.field_value(left, f"_{idx}"), self.field_value(right, f"_{idx}"))
                for idx in range(len(left_type.items))
            ])
        if self.is_datatype_type(left.typ) or self.is_datatype_type(right.typ):
            left_expr = self.primitive_expr(left)
            right_expr = self.primitive_expr(right)
            if left_expr.sort().eq(right_expr.sort()):
                return left_expr == right_expr
            return z3.BoolVal(False)
        return self.primitive_expr(left) == self.primitive_expr(right)

    def if_value(self, cond: Any, then_v: ZValue, else_v: ZValue) -> ZValue:
        return ZValue(then_v.typ, expr=z3.If(cond, self.primitive_expr(then_v), self.primitive_expr(else_v)))

    # ------------------------------------------------------------------
    # Z3 helpers
    # ------------------------------------------------------------------

    def resolve_named_type(self, typ: TypeSpec) -> TypeSpec:
        seen: set[str] = set()
        while isinstance(typ, NamedSpec):
            if typ.name in seen:
                return typ
            seen.add(typ.name)
            resolved = self.problem.type_specs_by_name.get(typ.name)
            if resolved is None or resolved == typ:
                return typ
            typ = resolved
        return typ

    def is_datatype_type(self, typ: TypeSpec) -> bool:
        resolved = self.resolve_named_type(typ)
        return isinstance(resolved, (RecordSpec, TupleSpec, SumSpec))

    def datatype_key(self, typ: TypeSpec) -> str:
        if isinstance(typ, NamedSpec):
            return typ.name
        resolved = self.resolve_named_type(typ)
        if isinstance(resolved, SumSpec):
            return resolved.name
        if isinstance(resolved, RecordSpec):
            return f"record:{repr(resolved)}"
        if isinstance(resolved, TupleSpec):
            return f"tuple:{repr(resolved)}"
        raise UnsupportedExpression(f"type {typ!r} is not an ADT")

    def product_constructor_name(self) -> str:
        return "__value__"

    def datatype_constructor_specs(
        self,
        key: str,
        typ: TypeSpec,
    ) -> list[tuple[str, str, list[tuple[str | None, TypeSpec]]]]:
        resolved = self.resolve_named_type(typ)
        if isinstance(resolved, SumSpec):
            return [
                (variant.name, self.safe(f"{key}.{variant.name}"), list(variant.fields))
                for variant in resolved.variants
            ]
        if isinstance(resolved, RecordSpec):
            return [(self.product_constructor_name(), self.safe(f"{key}.value"), list(resolved.fields))]
        if isinstance(resolved, TupleSpec):
            fields: list[tuple[str | None, TypeSpec]] = [(f"_{idx}", item) for idx, item in enumerate(resolved.items)]
            return [(self.product_constructor_name(), self.safe(f"{key}.value"), fields)]
        raise UnsupportedExpression(f"type {typ!r} is not an ADT")

    def collect_datatypes(self, typ: TypeSpec, pending: dict[str, TypeSpec]) -> None:
        if not self.is_datatype_type(typ):
            return
        key = self.datatype_key(typ)
        if key in self.problem.datatype_encodings or key in pending:
            return
        resolved = self.resolve_named_type(typ)
        if isinstance(resolved, NamedSpec):
            return
        pending[key] = resolved
        for _, _, fields in self.datatype_constructor_specs(key, resolved):
            for _, field_type in fields:
                self.collect_datatypes(field_type, pending)

    def ensure_datatypes(self, typ: TypeSpec) -> None:
        pending: dict[str, TypeSpec] = {}
        self.collect_datatypes(typ, pending)
        if not pending:
            return
        ordered = list(pending.items())
        builders = {
            key: z3.Datatype(f"DT__{self.safe(key)}")
            for key, _ in ordered
        }
        for key, spec in ordered:
            builder = builders[key]
            for public_name, z3_name, fields in self.datatype_constructor_specs(key, spec):
                z3_fields = []
                for idx, (label, field_type) in enumerate(fields):
                    label_text = label if label is not None else f"_{idx}"
                    accessor_name = self.safe(f"{key}.{public_name}.{label_text}.{idx}")
                    z3_fields.append((accessor_name, self.datatype_field_sort(field_type, builders)))
                builder.declare(z3_name, *z3_fields)
        created = z3.CreateDatatypes(*[builders[key] for key, _ in ordered])
        if len(ordered) == 1 and not isinstance(created, (tuple, list)):
            created = (created,)
        for (key, spec), sort in zip(ordered, created):
            constructors: dict[str, Any] = {}
            recognizers: dict[str, Any] = {}
            accessors: dict[str, list[Any]] = {}
            fields_by_constructor: dict[str, list[tuple[str | None, TypeSpec]]] = {}
            for idx, (public_name, _, fields) in enumerate(self.datatype_constructor_specs(key, spec)):
                constructors[public_name] = sort.constructor(idx)
                recognizers[public_name] = sort.recognizer(idx)
                accessors[public_name] = [sort.accessor(idx, field_idx) for field_idx in range(len(fields))]
                fields_by_constructor[public_name] = fields
            self.problem.datatype_encodings[key] = DatatypeEncoding(
                sort=sort,
                constructors=constructors,
                recognizers=recognizers,
                accessors=accessors,
                fields=fields_by_constructor,
            )

    def datatype_field_sort(self, typ: TypeSpec, builders: dict[str, Any]) -> Any:
        if isinstance(typ, PrimitiveType):
            return self.primitive_sort(typ)
        if self.is_datatype_type(typ):
            key = self.datatype_key(typ)
            if key in builders:
                return builders[key]
            if key in self.problem.datatype_encodings:
                return self.problem.datatype_encodings[key].sort
            raise UnsupportedExpression(f"datatype dependency {key!r} was not collected")
        return self.opaque_sort_for(typ)

    def datatype_encoding(self, typ: TypeSpec) -> DatatypeEncoding:
        self.ensure_datatypes(typ)
        key = self.datatype_key(typ)
        if key not in self.problem.datatype_encodings:
            raise UnsupportedExpression(f"type {typ!r} is not encoded as an ADT")
        return self.problem.datatype_encodings[key]

    def primitive_sort(self, typ: PrimitiveType) -> Any:
        if typ.name == "bool":
            return z3.BoolSort()
        if typ.name == "int":
            return z3.IntSort()
        if typ.name in {"rat", "decimal"}:
            return z3.RealSort()
        if typ.name == "string":
            return z3.StringSort()
        return z3.IntSort()

    def opaque_sort_for(self, typ: TypeSpec) -> Any:
        key = self.safe(repr(typ))
        if key not in self.problem.opaque_sorts:
            self.problem.opaque_sorts[key] = z3.DeclareSort(f"Sort__{key}")
        return self.problem.opaque_sorts[key]

    def track(self, expr: Any, *, kind: str, module: str | None, name: str, line: int, column: int) -> None:
        self.track_counter += 1
        key = self.safe(f"track.{kind}.{module or 'global'}.{name}.{self.track_counter}")
        lit = z3.Bool(key)
        self.tracked[key] = TrackedConstraint(key, kind, module, name, line, column)
        self.solver.assert_and_track(expr, lit)

    def as_bool(self, value: ZValue) -> Any:
        if isinstance(value.typ, PrimitiveType) and value.typ.name == "bool":
            return self.primitive_expr(value)
        raise UnsupportedExpression("expected bool expression")

    def primitive_expr(self, value: ZValue) -> Any:
        if value.expr is None:
            raise UnsupportedExpression("expected primitive expression")
        return value.expr

    def is_numeric(self, value: ZValue) -> bool:
        return isinstance(value.typ, PrimitiveType) and value.typ.name in {"int", "rat", "decimal"}

    def as_numeric(self, value: ZValue) -> Any:
        if not self.is_numeric(value):
            raise UnsupportedExpression("expected numeric expression")
        return self.primitive_expr(value)

    def promote_numeric(self, left: ZValue, right: ZValue) -> tuple[Any, Any]:
        l_num = self.as_numeric(left)
        r_num = self.as_numeric(right)
        if isinstance(left.typ, PrimitiveType) and left.typ.name == "int" and isinstance(right.typ, PrimitiveType) and right.typ.name in {"rat", "decimal"}:
            l_num = z3.ToReal(l_num)
        if isinstance(right.typ, PrimitiveType) and right.typ.name == "int" and isinstance(left.typ, PrimitiveType) and left.typ.name in {"rat", "decimal"}:
            r_num = z3.ToReal(r_num)
        return l_num, r_num

    def sort_for(self, typ: TypeSpec) -> Any:
        if isinstance(typ, PrimitiveType):
            return self.primitive_sort(typ)
        if self.is_datatype_type(typ):
            return self.datatype_encoding(typ).sort
        return self.opaque_sort_for(typ)

    def real_val(self, value: Any) -> Any:
        if isinstance(value, Fraction):
            return z3.RealVal(f"{value.numerator}/{value.denominator}")
        return z3.RealVal(str(value))

    def safe(self, text: str) -> str:
        chars = [ch if ch.isalnum() else "_" for ch in text]
        result = "".join(chars).strip("_") or "x"
        while "__" in result:
            result = result.replace("__", "_")
        return result

    # ------------------------------------------------------------------
    # Runtime and diagnostics
    # ------------------------------------------------------------------

    def try_runtime_eval(self, expr: A.Expr, scope: ModuleScope) -> Any:
        if scope.runtime is None:
            return _NO_CONCRETE
        if not self.runtime_names_resolvable(expr, scope):
            return _NO_CONCRETE
        if not self.runtime_references_have_values(expr, scope):
            return _NO_CONCRETE
        if self.references_external_module(expr, scope):
            return _NO_CONCRETE
        if self.references_non_list_collection_constructor(expr):
            return _NO_CONCRETE
        try:
            value = scope.runtime.eval_expr(expr, dict(scope.runtime.values))
            if value is None:
                return _NO_CONCRETE
            return value
        except Exception:
            return _NO_CONCRETE

    def runtime_names_resolvable(self, expr: A.Expr | None, scope: ModuleScope) -> bool:
        if expr is None:
            return True
        if isinstance(expr, A.Name):
            root = expr.name.split(".")[0]
            if expr.name == "last":
                return True
            parts = split_qualified(expr.name)
            if len(parts) >= 2 and parts[-2] in {"List", "Set", "Map", "Option"}:
                return True
            if root in scope.entities or root in scope.values or root in scope.funcs or root in scope.events or root in scope.types:
                return True
            if root in scope.imports:
                return True
            if root[:1].isupper():
                return True
            return False
        if isinstance(expr, A.Call):
            return self.runtime_names_resolvable(expr.func, scope) and all(self.runtime_names_resolvable(arg, scope) for arg in expr.args)
        if isinstance(expr, A.FieldAccess):
            return self.runtime_names_resolvable(expr.target, scope)
        if isinstance(expr, A.BinaryOp):
            return self.runtime_names_resolvable(expr.left, scope) and self.runtime_names_resolvable(expr.right, scope)
        if isinstance(expr, A.UnaryOp):
            return self.runtime_names_resolvable(expr.operand, scope)
        if isinstance(expr, A.TemporalUnary):
            return self.runtime_names_resolvable(expr.operand, scope)
        if isinstance(expr, A.TemporalBinary):
            return self.runtime_names_resolvable(expr.left, scope) and self.runtime_names_resolvable(expr.right, scope)
        if isinstance(expr, A.IfExpr):
            return all(self.runtime_names_resolvable(item, scope) for item in [expr.condition, expr.then_branch, expr.else_branch])
        if isinstance(expr, A.LetExpr):
            return self.runtime_names_resolvable(expr.value, scope) and self.runtime_names_resolvable(expr.body, scope)
        if isinstance(expr, A.MatchExpr):
            return self.runtime_names_resolvable(expr.subject, scope) and all(
                self.runtime_names_resolvable(arm.guard, scope) and self.runtime_names_resolvable(arm.body.result if arm.body else None, scope)
                for arm in expr.arms
            )
        if isinstance(expr, A.TupleLiteral):
            return all(self.runtime_names_resolvable(item, scope) for item in expr.items)
        if isinstance(expr, A.RecordConstructor):
            return all(self.runtime_names_resolvable(item, scope) for _, item in expr.fields)
        return True

    def runtime_references_have_values(self, expr: A.Expr | None, scope: ModuleScope) -> bool:
        if expr is None or scope.runtime is None:
            return True
        if isinstance(expr, A.Name):
            root = expr.name.split(".")[0]
            if root in scope.entities or root in scope.values:
                return scope.runtime.values.get(root) is not None
            return True
        if isinstance(expr, A.Call):
            return self.runtime_references_have_values(expr.func, scope) and all(
                self.runtime_references_have_values(arg, scope) for arg in expr.args
            )
        if isinstance(expr, A.FieldAccess):
            return self.runtime_references_have_values(expr.target, scope)
        if isinstance(expr, A.BinaryOp):
            return self.runtime_references_have_values(expr.left, scope) and self.runtime_references_have_values(expr.right, scope)
        if isinstance(expr, A.UnaryOp):
            return self.runtime_references_have_values(expr.operand, scope)
        if isinstance(expr, A.TemporalUnary):
            return self.runtime_references_have_values(expr.operand, scope)
        if isinstance(expr, A.TemporalBinary):
            return self.runtime_references_have_values(expr.left, scope) and self.runtime_references_have_values(expr.right, scope)
        if isinstance(expr, A.IfExpr):
            return all(self.runtime_references_have_values(item, scope) for item in [expr.condition, expr.then_branch, expr.else_branch])
        if isinstance(expr, A.LetExpr):
            return self.runtime_references_have_values(expr.value, scope) and self.runtime_references_have_values(expr.body, scope)
        if isinstance(expr, A.MatchExpr):
            return self.runtime_references_have_values(expr.subject, scope) and all(
                self.runtime_references_have_values(arm.guard, scope)
                and self.runtime_references_have_values(arm.body.result if arm.body else None, scope)
                for arm in expr.arms
            )
        if isinstance(expr, A.TupleLiteral):
            return all(self.runtime_references_have_values(item, scope) for item in expr.items)
        if isinstance(expr, A.RecordConstructor):
            return all(self.runtime_references_have_values(item, scope) for _, item in expr.fields)
        return True

    def references_external_module(self, expr: A.Expr | None, scope: ModuleScope) -> bool:
        if expr is None:
            return False
        if isinstance(expr, A.Name):
            parts = split_qualified(expr.name)
            for module_name in sorted(self.problem.scopes, key=len, reverse=True):
                if module_name == scope.module.name:
                    continue
                module_parts = split_qualified(module_name)
                if parts[:len(module_parts)] == module_parts:
                    return True
            return False
        if isinstance(expr, A.Call):
            return self.references_external_module(expr.func, scope) or any(self.references_external_module(arg, scope) for arg in expr.args)
        if isinstance(expr, A.FieldAccess):
            return self.references_external_module(expr.target, scope)
        if isinstance(expr, A.BinaryOp):
            return self.references_external_module(expr.left, scope) or self.references_external_module(expr.right, scope)
        if isinstance(expr, (A.UnaryOp, A.TemporalUnary)):
            return self.references_external_module(expr.operand, scope)
        if isinstance(expr, A.TemporalBinary):
            return self.references_external_module(expr.left, scope) or self.references_external_module(expr.right, scope)
        if isinstance(expr, A.IfExpr):
            return any(self.references_external_module(item, scope) for item in [expr.condition, expr.then_branch, expr.else_branch])
        if isinstance(expr, A.LetExpr):
            return self.references_external_module(expr.value, scope) or self.references_external_module(expr.body, scope)
        if isinstance(expr, A.MatchExpr):
            return self.references_external_module(expr.subject, scope) or any(
                self.references_external_module(arm.guard, scope) or self.references_external_module(arm.body.result if arm.body else None)
                for arm in expr.arms
            )
        if isinstance(expr, A.TupleLiteral):
            return any(self.references_external_module(item, scope) for item in expr.items)
        if isinstance(expr, A.RecordConstructor):
            return any(self.references_external_module(item, scope) for _, item in expr.fields)
        return False

    def references_non_list_collection_constructor(self, expr: A.Expr | None) -> bool:
        if expr is None:
            return False
        if isinstance(expr, A.Call):
            name = self.expr_to_name(expr.func)
            parts = split_qualified(name or "")
            if len(parts) >= 2 and parts[-2] in {"Set", "Map", "Option"}:
                return True
            return self.references_non_list_collection_constructor(expr.func) or any(
                self.references_non_list_collection_constructor(arg) for arg in expr.args
            )
        if isinstance(expr, A.FieldAccess):
            return self.references_non_list_collection_constructor(expr.target)
        if isinstance(expr, A.BinaryOp):
            return self.references_non_list_collection_constructor(expr.left) or self.references_non_list_collection_constructor(expr.right)
        if isinstance(expr, (A.UnaryOp, A.TemporalUnary)):
            return self.references_non_list_collection_constructor(expr.operand)
        if isinstance(expr, A.TemporalBinary):
            return self.references_non_list_collection_constructor(expr.left) or self.references_non_list_collection_constructor(expr.right)
        if isinstance(expr, A.IfExpr):
            return any(self.references_non_list_collection_constructor(item) for item in [expr.condition, expr.then_branch, expr.else_branch])
        if isinstance(expr, A.LetExpr):
            return self.references_non_list_collection_constructor(expr.value) or self.references_non_list_collection_constructor(expr.body)
        if isinstance(expr, A.MatchExpr):
            return self.references_non_list_collection_constructor(expr.subject) or any(
                self.references_non_list_collection_constructor(arm.guard) or self.references_non_list_collection_constructor(arm.body.result if arm.body else None)
                for arm in expr.arms
            )
        if isinstance(expr, A.TupleLiteral):
            return any(self.references_non_list_collection_constructor(item) for item in expr.items)
        if isinstance(expr, A.RecordConstructor):
            return any(self.references_non_list_collection_constructor(item) for _, item in expr.fields)
        return False

    def unresolved(self, scope: ModuleScope, name: str, node: A.Node | None = None) -> None:
        self.diagnostics.append(Diagnostic(
            f"unresolved name {name!r}",
            line=(node.line if node else 1) or 1,
            column=(node.column if node else 1) or 1,
            severity="error",
            code="unresolved-name",
            path=str(scope.path) if scope.path else None,
        ))

    def expr_to_name(self, expr: A.Expr | None) -> str:
        if isinstance(expr, A.Name):
            return expr.name
        if isinstance(expr, A.FieldAccess):
            return self.expr_to_name(expr.target) + "." + expr.field
        raise UnsupportedExpression(f"expression is not callable: {expr!r}")

    # ------------------------------------------------------------------
    # Output
    # ------------------------------------------------------------------

    def conflict_payload(self, index: int) -> dict[str, Any]:
        core = [str(item) for item in self.solver.unsat_core()]
        constraints = [self.tracked[name].to_dict() for name in core if name in self.tracked]
        rules = sorted({item["name"] for item in constraints if item["kind"].startswith("rule")})
        facts = sorted({item["name"] for item in constraints if item["kind"] == "fact"})
        return {
            "id": f"core_{index:03d}",
            "horizon": self.horizon,
            "rules": rules,
            "facts": facts,
            "constraints": constraints,
            "z3_core": core,
        }

    def model_payload(self) -> dict[str, Any]:
        model = self.solver.model()
        trace = []
        for t in range(self.horizon):
            entities: dict[str, Any] = {}
            events: dict[str, Any] = {}
            for scope in self.problem.scopes.values():
                for entity in scope.entities.values():
                    value = self.entity_value(entity, t)
                    entities[f"{entity.module}.{entity.decl.name}"] = self.model_value(model, value)
                for event in scope.events.values():
                    if not event.fields:
                        events[f"{event.module}.{event.decl.name}"] = self.model_eval(model, self.event_symbol(event, t))
            trace.append({"time": t, "entities": entities, "events": events})
        winning = []
        defeated = []
        for info in self.rule_infos:
            label = self.rule_labels.get(info.full_name)
            app = self.rule_applications.get(info.full_name)
            if label is not None and z3.is_true(model.eval(label, model_completion=True)):
                winning.append(info.full_name)
            elif app is not None and z3.is_true(model.eval(app, model_completion=True)):
                defeated.append(info.full_name)
        return {
            "trace": trace,
            "winning_rules": winning,
            "defeated_rules": defeated,
        }

    def model_value(self, model: Any, value: ZValue, depth: int = 0) -> Any:
        if depth > 50:
            return "<max-depth>"
        typ = self.resolve_named_type(value.typ)
        if isinstance(typ, RecordSpec):
            if value.fields is not None:
                return {name: self.model_value(model, field_value, depth + 1) for name, field_value in value.fields.items()}
            encoding = self.datatype_encoding(value.typ)
            expr = self.primitive_expr(value)
            return {
                name: self.model_value(model, ZValue(field_type, expr=encoding.accessors[self.product_constructor_name()][idx](expr)), depth + 1)
                for idx, (name, field_type) in enumerate(typ.fields)
            }
        if isinstance(typ, TupleSpec):
            if value.fields is not None:
                return [self.model_value(model, value.fields[f"_{idx}"], depth + 1) for idx in range(len(typ.items))]
            encoding = self.datatype_encoding(value.typ)
            expr = self.primitive_expr(value)
            return [
                self.model_value(model, ZValue(item_type, expr=encoding.accessors[self.product_constructor_name()][idx](expr)), depth + 1)
                for idx, item_type in enumerate(typ.items)
            ]
        if isinstance(typ, SumSpec):
            encoding = self.datatype_encoding(value.typ)
            expr = self.primitive_expr(value)
            for variant in typ.variants:
                if z3.is_true(model.eval(encoding.recognizers[variant.name](expr), model_completion=True)):
                    payloads = [
                        self.model_value(model, ZValue(field_type, expr=encoding.accessors[variant.name][idx](expr)), depth + 1)
                        for idx, (_, field_type) in enumerate(variant.fields)
                    ]
                    return {
                        "constructor": variant.name,
                        "values": payloads,
                    }
            return str(model.eval(expr, model_completion=True))
        if value.has_concrete:
            if isinstance(value.concrete, list):
                return [self.model_value(model, item, depth + 1) if isinstance(item, ZValue) else item for item in value.concrete]
            if isinstance(value.concrete, set):
                return sorted(value.concrete)
            return value.concrete
        return self.model_eval(model, self.primitive_expr(value))

    def model_eval(self, model: Any, expr: Any) -> Any:
        value = model.eval(expr, model_completion=True)
        if z3.is_true(value):
            return True
        if z3.is_false(value):
            return False
        if z3.is_int_value(value):
            return int(str(value))
        if z3.is_rational_value(value):
            return str(value)
        if z3.is_string_value(value):
            return value.as_string()
        return str(value)


class _NoConcrete:
    pass


_NO_CONCRETE = _NoConcrete()


def solve_to_json(paths: Iterable[str | Path], options: SolveOptions | None = None, *, indent: int = 2) -> str:
    return json.dumps(solve_paths(paths, options), ensure_ascii=False, indent=indent)
