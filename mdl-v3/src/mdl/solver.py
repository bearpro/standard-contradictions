from __future__ import annotations

import json
from dataclasses import dataclass, field
from fractions import Fraction
from pathlib import Path
from typing import Any, Iterable

try:  # pragma: no cover - exercised in environments without the optional wheel
    import z3
except Exception as exc:  # pragma: no cover
    z3 = None  # type: ignore[assignment]
    _Z3_IMPORT_ERROR = exc
else:
    _Z3_IMPORT_ERROR = None

from . import ast as A
from .diagnostics import Diagnostic, MDLError, ParseError
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
class CollectionSpec:
    kind: str
    item: "TypeSpec"


@dataclass(frozen=True)
class OpaqueSpec:
    name: str


TypeSpec = PrimitiveType | RecordSpec | TupleSpec | SumSpec | CollectionSpec | OpaqueSpec


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
    tag: Any | None = None
    variants: dict[str, list["ZValue"]] | None = None
    concrete: Any = None
    has_concrete: bool = False


@dataclass
class FunctionInfo:
    module: str
    decl: A.FuncDecl
    params: list[TypeSpec]
    return_type: TypeSpec


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
    exposing: dict[str, tuple[str, str]] = field(default_factory=dict)
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
    return solve_modules([m for m, _ in modules], opts, paths=[p for _, p in modules])


def solve_modules(modules: list[A.Module], options: SolveOptions | None = None, *, paths: list[Path] | None = None) -> dict[str, Any]:
    if _Z3_IMPORT_ERROR is not None:
        return error_payload([Diagnostic(
            f"z3-solver is not available: {_Z3_IMPORT_ERROR}",
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
        self.modules = modules
        self.options = options
        self.paths = paths or [None] * len(modules)  # type: ignore[list-item]
        self.diagnostics: list[Diagnostic] = []
        self.scopes: dict[str, ModuleScope] = {}
        self.type_cache: dict[tuple[str, str], TypeSpec] = {}
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
                alias = imp.alias or imp.path.split(".")[-1]
                if imp.path in self.scopes:
                    scope.imports[alias] = imp.path
                elif imp.path.startswith("std."):
                    scope.imports[alias] = imp.path
                else:
                    self.diagnostics.append(Diagnostic(
                        f"import {imp.path!r} is not present in solve inputs",
                        line=imp.line or 1,
                        column=imp.column or 1,
                        severity="error",
                        code="unresolved-import",
                        path=str(scope.path) if scope.path else None,
                    ))
                for exposed, renamed in imp.exposing:
                    scope.exposing[renamed or exposed] = (imp.path, exposed)

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

    def resolve_type(self, module_name: str, typ: A.TypeExpr | None) -> TypeSpec:
        if typ is None:
            return UNIT
        scope = self.scopes[module_name]
        if isinstance(typ, A.TypeRef):
            name = typ.name
            primitive = self.primitive_type(name)
            if primitive is not None:
                if name in {"List", "list"} and typ.args:
                    return CollectionSpec("list", self.resolve_type(module_name, typ.args[0]))
                if name in {"Set", "set"} and typ.args:
                    return CollectionSpec("set", self.resolve_type(module_name, typ.args[0]))
                return primitive
            target_module, local = self.resolve_decl(scope, name, expected={"type"})
            if target_module is None or local is None:
                return OpaqueSpec(name)
            key = (target_module, local)
            if key in self.type_cache:
                return self.type_cache[key]
            decl = self.scopes[target_module].types[local]
            placeholder = OpaqueSpec(f"{target_module}.{local}")
            self.type_cache[key] = placeholder
            if isinstance(decl.definition, A.SumType):
                variants = tuple(
                    VariantSpec(v.name, tuple((label, self.resolve_type(target_module, item_type)) for label, item_type in v.fields))
                    for v in decl.definition.variants
                )
                resolved: TypeSpec = SumSpec(f"{target_module}.{local}", variants)
            else:
                resolved = self.resolve_type_expr(target_module, decl.definition)
            self.type_cache[key] = resolved
            return resolved
        return self.resolve_type_expr(module_name, typ)

    def resolve_type_expr(self, module_name: str, typ: A.TypeExpr | A.SumType | None) -> TypeSpec:
        if typ is None:
            return UNIT
        if isinstance(typ, A.RecordType):
            return RecordSpec(tuple((name, self.resolve_type(module_name, field_type)) for name, field_type in typ.fields))
        if isinstance(typ, A.TupleType):
            return TupleSpec(tuple(self.resolve_type(module_name, item) for item in typ.items))
        if isinstance(typ, A.SumType):
            return SumSpec("<anonymous>", tuple(
                VariantSpec(v.name, tuple((label, self.resolve_type(module_name, item_type)) for label, item_type in v.fields))
                for v in typ.variants
            ))
        return self.resolve_type(module_name, typ)

    def primitive_type(self, name: str) -> TypeSpec | None:
        if name in {"bool", "boolean"}:
            return BOOL
        if name in {"int", "integer"}:
            return INT
        if name in {"rat", "rational", "real", "decimal"}:
            return REAL
        if name == "string":
            return STRING
        if name == "char":
            return STRING
        if name == "unit":
            return UNIT
        if name in {"List", "list"}:
            return CollectionSpec("list", OpaqueSpec("unknown"))
        if name in {"Set", "set"}:
            return CollectionSpec("set", OpaqueSpec("unknown"))
        return None

    def resolve_decl(
        self,
        scope: ModuleScope,
        name: str,
        *,
        expected: set[str] | None = None,
    ) -> tuple[str | None, str | None]:
        parts = name.split(".")
        if not parts:
            return None, None
        if parts[0] in scope.imports:
            target = scope.imports[parts[0]]
            if target not in self.scopes:
                return target, ".".join(parts[1:]) if len(parts) > 1 else None
            return self._resolve_local_decl(self.scopes[target], parts[1:], expected)
        if parts[0] in scope.exposing:
            target, exposed = scope.exposing[parts[0]]
            if target in self.scopes:
                return self._resolve_local_decl(self.scopes[target], [exposed, *parts[1:]], expected)
        local_module, local_name = self._resolve_local_decl(scope, parts, expected)
        if local_module is not None:
            return local_module, local_name
        if parts[0] == scope.module.name:
            return self._resolve_local_decl(scope, parts[1:], expected)
        for module_name in sorted(self.scopes, key=len, reverse=True):
            module_parts = module_name.split(".")
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
            self.encode_entity_clauses()
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

    def encode_entity_clauses(self) -> None:
        for scope in self.problem.scopes.values():
            for entity in scope.entities.values():
                for kind, expr in entity.decl.clauses:
                    if kind != "where":
                        continue
                    for t in range(self.horizon):
                        self.track(
                            self.compile_formula(expr, scope, t),
                            kind="entity-where",
                            module=scope.module.name,
                            name=entity.decl.name,
                            line=entity.decl.line or 1,
                            column=entity.decl.column or 1,
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
        if isinstance(expr, A.BracedExpr):
            return self.compile_formula(expr.expr, scope, t, env=env)
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
        if isinstance(expr, A.BinaryOp) and expr.op in {"and", "or", "implies", "->", "iff", "<->"}:
            left = self.compile_formula(expr.left, scope, t, env=env)
            right = self.compile_formula(expr.right, scope, t, env=env)
            if expr.op == "and":
                return z3.And(left, right)
            if expr.op == "or":
                return z3.Or(left, right)
            if expr.op in {"implies", "->"}:
                return z3.Implies(left, right)
            return left == right
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
            constructor = self.find_constructor(expr.name, expected)
            if constructor is not None:
                sum_type, variant = constructor
                if variant.fields:
                    raise UnsupportedExpression(f"constructor {expr.name!r} expects arguments")
                payloads = {v.name: [] for v in sum_type.variants}
                return ZValue(sum_type, tag=z3.IntVal(self.variant_index(sum_type, variant.name)), variants=payloads)
            concrete = self.try_runtime_eval(expr, scope)
            if concrete is not _NO_CONCRETE:
                return self.python_value(concrete, expected)
            return self.resolve_value(expr.name, scope, t, expected)
        concrete = self.try_runtime_eval(expr, scope)
        if concrete is not _NO_CONCRETE:
            return self.python_value(concrete, expected)
        if isinstance(expr, A.FieldAccess):
            target = self.compile_expr(expr.target, scope, t, env=env)
            return self.field_value(target, expr.field)
        if isinstance(expr, A.IndexAccess):
            if isinstance(expr.target, A.ListLiteral) and isinstance(expr.index, A.Literal) and expr.index.kind == "int":
                return self.compile_expr(expr.target.items[int(expr.index.value)], scope, t, expected=expected, env=env)
            raise UnsupportedExpression(f"symbolic index access is not supported: {format_expr(expr)}")
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
        if isinstance(expr, A.RecordLiteral):
            fields: dict[str, ZValue] = {}
            expected_fields = dict(expected.fields) if isinstance(expected, RecordSpec) else {}
            for name, value_expr in expr.fields:
                fields[name] = self.compile_expr(value_expr, scope, t, expected=expected_fields.get(name), env=env)
            typ = expected if isinstance(expected, RecordSpec) else RecordSpec(tuple((name, value.typ) for name, value in fields.items()))
            return ZValue(typ, fields=fields)
        if isinstance(expr, A.TupleLiteral):
            expected_items = list(expected.items) if isinstance(expected, TupleSpec) else []
            items = {
                f"_{idx}": self.compile_expr(item, scope, t, expected=expected_items[idx] if idx < len(expected_items) else None, env=env)
                for idx, item in enumerate(expr.items)
            }
            typ = expected if isinstance(expected, TupleSpec) else TupleSpec(tuple(item.typ for item in items.values()))
            return ZValue(typ, fields=items)
        if isinstance(expr, A.ListLiteral):
            items = [self.compile_expr(item, scope, t, env=env) for item in expr.items]
            return ZValue(CollectionSpec("list", items[0].typ if items else OpaqueSpec("empty")), concrete=items, has_concrete=True)
        if isinstance(expr, A.SetLiteral):
            items = [self.compile_expr(item, scope, t, env=env) for item in expr.items]
            return ZValue(CollectionSpec("set", items[0].typ if items else OpaqueSpec("empty")), concrete=items, has_concrete=True)
        if isinstance(expr, A.BracedExpr):
            return self.compile_expr(expr.expr, scope, t, expected=expected, env=env)
        if isinstance(expr, A.QuantifierExpr):
            return ZValue(BOOL, expr=self.quantifier_formula(expr, scope, t, env))
        raise UnsupportedExpression(f"unsupported expression: {format_expr(expr)}")

    def binary_value(self, expr: A.BinaryOp, scope: ModuleScope, t: int, expected: TypeSpec | None, env: dict[str, ZValue]) -> ZValue:
        if expr.op in {"and", "or", "implies", "->", "iff", "<->"}:
            return ZValue(BOOL, expr=self.compile_formula(expr, scope, t))
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
        if name.endswith("strings.to_list") or name in {"strings.to_list", "std.system.strings.to_list"}:
            arg = self.compile_expr(expr.args[0], scope, t, env=env)
            if arg.has_concrete and isinstance(arg.concrete, str):
                return self.python_value(list(arg.concrete), expected)
            raise UnsupportedExpression("strings.to_list requires a concrete string in solve")
        event = self.resolve_event(name, scope)
        if event is not None:
            args = [self.compile_expr(arg, scope, t, expected=typ, env=env) for arg, (_, typ) in zip(expr.args, event.fields)]
            symbol = self.event_symbol(event, t)
            if event.fields:
                return ZValue(BOOL, expr=symbol(*[self.primitive_expr(a) for a in args]))
            return ZValue(BOOL, expr=symbol)
        constructor = self.find_constructor(name, expected)
        if constructor is not None:
            sum_type, variant = constructor
            values = [self.compile_expr(arg, scope, t, expected=field_type, env=env) for arg, (_, field_type) in zip(expr.args, variant.fields)]
            payloads = {v.name: [] for v in sum_type.variants}
            payloads[variant.name] = values
            return ZValue(sum_type, tag=z3.IntVal(self.variant_index(sum_type, variant.name)), variants=payloads)
        func = self.resolve_func(name, scope)
        if func is not None:
            args = [self.compile_expr(arg, scope, t, expected=typ, env=env) for arg, typ in zip(expr.args, func.params)]
            if isinstance(func.return_type, PrimitiveType):
                fn = self.function_symbol(func)
                return ZValue(func.return_type, expr=fn(*[self.primitive_expr(arg) for arg in args]))
            return self.opaque_call(name, args, func.return_type)
        return self.opaque_atom(format_expr(expr), t)

    def quantifier_formula(self, expr: A.QuantifierExpr, scope: ModuleScope, t: int, env: dict[str, ZValue]) -> Any:
        domain = self.compile_expr(expr.domain, scope, t, env=env)
        if not domain.has_concrete or not isinstance(domain.concrete, list):
            raise UnsupportedExpression(f"quantifier domain must be a finite collection: {format_expr(expr.domain)}")
        formulas = []
        for item in domain.concrete:
            local = dict(env)
            self.bind_pattern(expr.pattern, item, local, assumptions=[])
            formulas.append(self.compile_formula(expr.body, scope, t, env=local))
        if expr.quantifier == "forall":
            return z3.And(formulas) if formulas else z3.BoolVal(True)
        return z3.Or(formulas) if formulas else z3.BoolVal(False)

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
        if isinstance(typ, RecordSpec):
            return ZValue(typ, fields={name: self.fresh_value(f"{prefix}.{name}", field_type) for name, field_type in typ.fields})
        if isinstance(typ, TupleSpec):
            return ZValue(typ, fields={f"_{idx}": self.fresh_value(f"{prefix}.{idx}", item) for idx, item in enumerate(typ.items)})
        if isinstance(typ, SumSpec):
            tag = z3.Int(f"{safe}__tag")
            allowed = [tag == idx for idx, _ in enumerate(typ.variants)]
            self.track(z3.Or(allowed), kind="type", module=None, name=prefix, line=1, column=1)
            payloads = {
                variant.name: [self.fresh_value(f"{prefix}.{variant.name}.{idx}", field_type) for idx, (_, field_type) in enumerate(variant.fields)]
                for variant in typ.variants
            }
            return ZValue(typ, tag=tag, variants=payloads)
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
        root = name.split(".")[0]
        parts = name.split(".")
        if root in scope.imports:
            field_parts = parts[2:]
        elif parts[0] == module_name and len(parts) > 1 and parts[1] == local:
            field_parts = parts[2:]
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
        for field in field_parts:
            value = self.field_value(value, field)
        return value

    def field_value(self, value: ZValue, field: str) -> ZValue:
        if value.fields and field in value.fields:
            return value.fields[field]
        if value.has_concrete and isinstance(value.concrete, dict):
            return self.python_value(value.concrete[field], None)
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

    def function_symbol(self, func: FunctionInfo) -> Any:
        key = f"func.{func.module}.{func.decl.name}"
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
        if not isinstance(func.return_type, PrimitiveType):
            return False
        if any(not isinstance(param_type, PrimitiveType) for param_type in func.params):
            return False
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
            if not isinstance(value.typ, SumSpec) or value.tag is None or value.variants is None:
                raise UnsupportedExpression("constructor pattern requires a sum value")
            variant_name = pattern.name.split(".")[-1]
            idx = self.variant_index(value.typ, variant_name)
            payloads = value.variants.get(variant_name, [])
            conds = [value.tag == idx]
            bindings: dict[str, ZValue] = {}
            for nested, payload in zip(pattern.args, payloads):
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

    def find_constructor(self, name: str, expected: TypeSpec | None) -> tuple[SumSpec, VariantSpec] | None:
        short = name.split(".")[-1]
        if isinstance(expected, SumSpec):
            for variant in expected.variants:
                if variant.name == short:
                    return expected, variant
        for scope in self.problem.scopes.values():
            for decl in scope.types.values():
                typ = self.problem.resolve_type(scope.module.name, A.TypeRef(name=decl.name))
                if isinstance(typ, SumSpec):
                    for variant in typ.variants:
                        if variant.name == short:
                            return typ, variant
        return None

    def variant_index(self, sum_type: SumSpec, variant_name: str) -> int:
        for idx, variant in enumerate(sum_type.variants):
            if variant.name == variant_name:
                return idx
        raise UnsupportedExpression(f"unknown constructor {variant_name!r}")

    def literal_value(self, value: Any, kind: str, expected: TypeSpec | None = None) -> ZValue:
        if kind == "bool" or isinstance(expected, PrimitiveType) and expected.name == "bool":
            return ZValue(BOOL, expr=z3.BoolVal(bool(value)), concrete=bool(value), has_concrete=True)
        if kind == "int" and not (isinstance(expected, PrimitiveType) and expected.name in {"rat", "decimal"}):
            return ZValue(INT, expr=z3.IntVal(int(value)), concrete=int(value), has_concrete=True)
        if kind in {"rat", "decimal"} or isinstance(expected, PrimitiveType) and expected.name in {"rat", "decimal"}:
            return ZValue(REAL, expr=self.real_val(value), concrete=value, has_concrete=True)
        if kind in {"string", "char"} or isinstance(expected, PrimitiveType) and expected.name == "string":
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
                sum_type, variant = constructor
                payloads = {v.name: [] for v in sum_type.variants}
                return ZValue(
                    sum_type,
                    tag=z3.IntVal(self.variant_index(sum_type, variant.name)),
                    variants=payloads,
                    concrete=value,
                    has_concrete=True,
                )
            return self.literal_value(value, "string", expected)
        if isinstance(value, dict):
            expected_fields = dict(expected.fields) if isinstance(expected, RecordSpec) else {}
            fields = {k: self.python_value(v, expected_fields.get(k)) for k, v in value.items()}
            typ = expected if isinstance(expected, RecordSpec) else RecordSpec(tuple((k, v.typ) for k, v in fields.items()))
            return ZValue(typ, fields=fields, concrete=value, has_concrete=True)
        if isinstance(value, tuple) and value and isinstance(value[0], str):
            constructor = self.find_constructor(value[0], expected)
            if constructor is not None:
                sum_type, variant = constructor
                raw_args = value[1] if len(value) > 1 else ()
                payloads = {v.name: [] for v in sum_type.variants}
                payloads[variant.name] = [
                    self.python_value(arg, field_type)
                    for arg, (_, field_type) in zip(raw_args, variant.fields)
                ]
                return ZValue(sum_type, tag=z3.IntVal(self.variant_index(sum_type, variant.name)), variants=payloads, concrete=value, has_concrete=True)
        if isinstance(value, (list, set)):
            items = [self.python_value(item, None) for item in value]
            kind = "set" if isinstance(value, set) else "list"
            return ZValue(CollectionSpec(kind, items[0].typ if items else OpaqueSpec("empty")), concrete=items, has_concrete=True)
        return self.literal_value(value, "unit", expected)

    def equal_values(self, left: ZValue, right: ZValue) -> Any:
        if left.fields is not None and right.fields is not None:
            keys = sorted(set(left.fields) & set(right.fields))
            if not keys:
                return z3.BoolVal(True)
            return z3.And([self.equal_values(left.fields[k], right.fields[k]) for k in keys])
        if left.tag is not None and right.tag is not None and left.variants is not None and right.variants is not None:
            conds = [left.tag == right.tag]
            if isinstance(left.typ, SumSpec):
                for variant in left.typ.variants:
                    l_payloads = left.variants.get(variant.name, [])
                    r_payloads = right.variants.get(variant.name, [])
                    active = left.tag == self.variant_index(left.typ, variant.name)
                    for l_val, r_val in zip(l_payloads, r_payloads):
                        conds.append(z3.Implies(active, self.equal_values(l_val, r_val)))
            return z3.And(conds)
        if left.has_concrete and right.has_concrete and isinstance(left.concrete, list) and isinstance(right.concrete, list):
            if len(left.concrete) != len(right.concrete):
                return z3.BoolVal(False)
            return z3.And([self.equal_values(l, r) for l, r in zip(left.concrete, right.concrete)])
        if self.is_numeric(left) and self.is_numeric(right):
            l_num, r_num = self.promote_numeric(left, right)
            return l_num == r_num
        return self.primitive_expr(left) == self.primitive_expr(right)

    def if_value(self, cond: Any, then_v: ZValue, else_v: ZValue) -> ZValue:
        if then_v.fields is not None and else_v.fields is not None:
            return ZValue(then_v.typ, fields={k: self.if_value(cond, then_v.fields[k], else_v.fields[k]) for k in then_v.fields})
        if then_v.tag is not None and else_v.tag is not None and then_v.variants is not None and else_v.variants is not None:
            payloads = {
                name: [self.if_value(cond, a, b) for a, b in zip(then_v.variants.get(name, []), else_v.variants.get(name, []))]
                for name in then_v.variants
            }
            return ZValue(then_v.typ, tag=z3.If(cond, then_v.tag, else_v.tag), variants=payloads)
        return ZValue(then_v.typ, expr=z3.If(cond, self.primitive_expr(then_v), self.primitive_expr(else_v)))

    # ------------------------------------------------------------------
    # Z3 helpers
    # ------------------------------------------------------------------

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
            if typ.name == "bool":
                return z3.BoolSort()
            if typ.name == "int":
                return z3.IntSort()
            if typ.name in {"rat", "decimal"}:
                return z3.RealSort()
            if typ.name == "string":
                return z3.StringSort()
            return z3.IntSort()
        key = self.safe(repr(typ))
        if key not in self.problem.opaque_sorts:
            self.problem.opaque_sorts[key] = z3.DeclareSort(f"Sort__{key}")
        return self.problem.opaque_sorts[key]

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
        if self.references_import_alias(expr, scope):
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
            if root in scope.entities or root in scope.values or root in scope.funcs or root in scope.events or root in scope.types:
                return True
            if root in scope.imports:
                return True
            if root in {"List", "std"} or root[:1].isupper():
                return True
            return False
        if isinstance(expr, A.Call):
            return self.runtime_names_resolvable(expr.func, scope) and all(self.runtime_names_resolvable(arg, scope) for arg in expr.args)
        if isinstance(expr, A.FieldAccess):
            return self.runtime_names_resolvable(expr.target, scope)
        if isinstance(expr, A.IndexAccess):
            return self.runtime_names_resolvable(expr.target, scope) and self.runtime_names_resolvable(expr.index, scope)
        if isinstance(expr, A.BinaryOp):
            return self.runtime_names_resolvable(expr.left, scope) and self.runtime_names_resolvable(expr.right, scope)
        if isinstance(expr, A.UnaryOp):
            return self.runtime_names_resolvable(expr.operand, scope)
        if isinstance(expr, A.BracedExpr):
            return self.runtime_names_resolvable(expr.expr, scope)
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
        if isinstance(expr, (A.ListLiteral, A.SetLiteral, A.TupleLiteral)):
            return all(self.runtime_names_resolvable(item, scope) for item in expr.items)
        if isinstance(expr, A.RecordLiteral):
            return all(self.runtime_names_resolvable(item, scope) for _, item in expr.fields)
        if isinstance(expr, A.QuantifierExpr):
            return self.runtime_names_resolvable(expr.domain, scope) and self.runtime_names_resolvable(expr.body, scope)
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
        if isinstance(expr, A.IndexAccess):
            return self.runtime_references_have_values(expr.target, scope) and self.runtime_references_have_values(expr.index, scope)
        if isinstance(expr, A.BinaryOp):
            return self.runtime_references_have_values(expr.left, scope) and self.runtime_references_have_values(expr.right, scope)
        if isinstance(expr, A.UnaryOp):
            return self.runtime_references_have_values(expr.operand, scope)
        if isinstance(expr, A.BracedExpr):
            return self.runtime_references_have_values(expr.expr, scope)
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
        if isinstance(expr, (A.ListLiteral, A.SetLiteral, A.TupleLiteral)):
            return all(self.runtime_references_have_values(item, scope) for item in expr.items)
        if isinstance(expr, A.RecordLiteral):
            return all(self.runtime_references_have_values(item, scope) for _, item in expr.fields)
        if isinstance(expr, A.QuantifierExpr):
            return self.runtime_references_have_values(expr.domain, scope) and self.runtime_references_have_values(expr.body, scope)
        return True

    def references_import_alias(self, expr: A.Expr | None, scope: ModuleScope) -> bool:
        if expr is None:
            return False
        if isinstance(expr, A.Name):
            return expr.name.split(".")[0] in scope.imports
        if isinstance(expr, A.Call):
            return self.references_import_alias(expr.func, scope) or any(self.references_import_alias(arg, scope) for arg in expr.args)
        if isinstance(expr, A.FieldAccess):
            return self.references_import_alias(expr.target, scope)
        if isinstance(expr, A.IndexAccess):
            return self.references_import_alias(expr.target, scope) or self.references_import_alias(expr.index, scope)
        if isinstance(expr, A.BinaryOp):
            return self.references_import_alias(expr.left, scope) or self.references_import_alias(expr.right, scope)
        if isinstance(expr, A.UnaryOp):
            return self.references_import_alias(expr.operand, scope)
        if isinstance(expr, A.BracedExpr):
            return self.references_import_alias(expr.expr, scope)
        if isinstance(expr, A.TemporalUnary):
            return self.references_import_alias(expr.operand, scope)
        if isinstance(expr, A.TemporalBinary):
            return self.references_import_alias(expr.left, scope) or self.references_import_alias(expr.right, scope)
        if isinstance(expr, A.IfExpr):
            return any(self.references_import_alias(item, scope) for item in [expr.condition, expr.then_branch, expr.else_branch])
        if isinstance(expr, A.MatchExpr):
            return self.references_import_alias(expr.subject, scope) or any(
                self.references_import_alias(arm.guard, scope) or self.references_import_alias(arm.body.result if arm.body else None, scope)
                for arm in expr.arms
            )
        if isinstance(expr, (A.ListLiteral, A.SetLiteral, A.TupleLiteral)):
            return any(self.references_import_alias(item, scope) for item in expr.items)
        if isinstance(expr, A.RecordLiteral):
            return any(self.references_import_alias(item, scope) for _, item in expr.fields)
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

    def model_value(self, model: Any, value: ZValue) -> Any:
        if value.fields is not None:
            return {name: self.model_value(model, field_value) for name, field_value in value.fields.items()}
        if value.tag is not None and isinstance(value.typ, SumSpec):
            tag_value = model.eval(value.tag, model_completion=True)
            tag_index = int(str(tag_value))
            variant = value.typ.variants[tag_index]
            payloads = value.variants.get(variant.name, []) if value.variants else []
            return {
                "constructor": variant.name,
                "values": [self.model_value(model, item) for item in payloads],
            }
        if value.has_concrete:
            if isinstance(value.concrete, list):
                return [self.model_value(model, item) if isinstance(item, ZValue) else item for item in value.concrete]
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
