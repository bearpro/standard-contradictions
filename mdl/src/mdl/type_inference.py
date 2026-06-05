from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any

from . import ast as A
from .names import local_name, split_qualified


NUMERIC_TYPES = {"int", "rat", "decimal"}
PRIMITIVE_TYPES = {"bool", "int", "rat", "decimal", "string", "unit"}


class InferenceError(Exception):
    def __init__(self, message: str, code: str = "type-mismatch"):
        super().__init__(message)
        self.code = code


@dataclass(frozen=True)
class TyVar:
    id: int
    name: str = ""


@dataclass(frozen=True)
class TyCon:
    name: str
    args: tuple["Type", ...] = ()


@dataclass(frozen=True)
class TyRecord:
    fields: tuple[tuple[str, "Type"], ...]


@dataclass(frozen=True)
class TyTuple:
    items: tuple["Type", ...]


@dataclass(frozen=True)
class TyFun:
    args: tuple["Type", ...]
    ret: "Type"


Type = TyVar | TyCon | TyRecord | TyTuple | TyFun


@dataclass(frozen=True)
class Scheme:
    vars: frozenset[int]
    typ: Type


@dataclass
class TypeInference:
    """Algorithm-W-style inference over the public MDL AST.

    The host is `SemanticChecker` in practice. It is kept duck-typed so this
    module stays independent from linter imports and avoids circular imports.
    """

    host: Any
    type_params: set[str] = field(default_factory=set)
    substitutions: dict[int, Type] = field(default_factory=dict)
    _next_var: int = 0

    def fresh(self, name: str = "") -> TyVar:
        self._next_var += 1
        return TyVar(self._next_var, name or f"t{self._next_var}")

    def check_expr(
        self,
        expr: A.Expr | None,
        env: dict[str, A.TypeExpr | None] | None = None,
        *,
        expected: A.TypeExpr | None = None,
    ) -> A.TypeExpr | None:
        if expr is None:
            return None
        local = self.initial_env(env or {})
        expected_type = self.from_ast(expected) if expected is not None else None
        actual = self.infer_expr(expr, local, expected_type)
        if expected_type is not None:
            self.expect(actual, expected_type, expr or A.Node())
            actual = expected_type
        return self.to_ast(actual)

    def check_block(
        self,
        block: A.Block | None,
        env: dict[str, A.TypeExpr | None] | None = None,
        *,
        expected: A.TypeExpr | None = None,
    ) -> A.TypeExpr | None:
        local = self.initial_env(env or {})
        expected_type = self.from_ast(expected) if expected is not None else None
        actual = self.infer_block(block, local, expected_type)
        if expected_type is not None:
            self.expect(actual, expected_type, block or A.Node())
            actual = expected_type
        return self.to_ast(actual)

    def infer_expr(self, expr: A.Expr | None, env: dict[str, Scheme], expected: Type | None = None) -> Type:
        if expr is None:
            return TyCon("unit")
        if isinstance(expr, A.Literal):
            typ = TyCon(expr.kind if expr.kind in PRIMITIVE_TYPES else "unit")
            if expected is not None:
                self.expect(typ, expected, expr)
            return typ
        if isinstance(expr, A.Name):
            typ = self.lookup_name(expr.name, expr, env)
            if expected is not None:
                self.expect(typ, expected, expr)
            return typ
        if isinstance(expr, A.Call):
            typ = self.infer_call(expr, env, expected)
            if expected is not None:
                self.expect(typ, expected, expr)
            return typ
        if isinstance(expr, A.FieldAccess):
            target_type = self.infer_expr(expr.target, env)
            field_type = self.field_type(target_type, expr.field, expr)
            if expected is not None:
                self.expect(field_type, expected, expr)
            return field_type
        if isinstance(expr, A.IndexAccess):
            target_type = self.infer_expr(expr.target, env)
            self.expect(self.infer_expr(expr.index, env), TyCon("int"), expr.index or expr)
            result = self.index_type(target_type, expr.index)
            if expected is not None:
                self.expect(result, expected, expr)
            return result
        if isinstance(expr, A.BinaryOp):
            typ = self.infer_binary(expr, env)
            if expected is not None:
                self.expect(typ, expected, expr)
            return typ
        if isinstance(expr, A.UnaryOp):
            typ = self.infer_unary(expr, env)
            if expected is not None:
                self.expect(typ, expected, expr)
            return typ
        if isinstance(expr, A.IfExpr):
            self.expect(self.infer_expr(expr.condition, env), TyCon("bool"), expr.condition or expr)
            then_type = self.infer_expr(expr.then_branch, env, expected)
            else_type = self.infer_expr(expr.else_branch, env, expected or then_type)
            typ = self.common_type(then_type, else_type, expr)
            if expected is not None:
                self.expect(typ, expected, expr)
            return typ
        if isinstance(expr, A.LetExpr):
            value_type = self.infer_expr(expr.value, env)
            local = dict(env)
            self.bind_pattern(expr.pattern, value_type, local, generalize=True)
            typ = self.infer_expr(expr.body, local, expected)
            if expected is not None:
                self.expect(typ, expected, expr)
            return typ
        if isinstance(expr, A.MatchExpr):
            subject_type = self.infer_expr(expr.subject, env)
            result_type: Type | None = None
            for arm in expr.arms:
                local = dict(env)
                self.bind_pattern(arm.pattern, subject_type, local, generalize=False)
                if arm.guard is not None:
                    self.expect(self.infer_expr(arm.guard, local), TyCon("bool"), arm.guard)
                body_type = self.infer_block(arm.body, local, expected)
                if result_type is None:
                    result_type = body_type
                else:
                    result_type = self.common_type(result_type, body_type, arm)
            typ = result_type or TyCon("unit")
            if expected is not None:
                self.expect(typ, expected, expr)
            return typ
        if isinstance(expr, A.RecordConstructor):
            typ = self.record_constructor(expr, env)
            if expected is not None:
                self.expect(typ, expected, expr)
            return typ
        if isinstance(expr, A.TupleLiteral):
            typ = TyTuple(tuple(self.infer_expr(item, env) for item in expr.items))
            if expected is not None:
                self.expect(typ, expected, expr)
            return typ
        if isinstance(expr, A.TemporalUnary):
            self.expect(self.infer_expr(expr.operand, env), TyCon("bool"), expr.operand or expr)
            return TyCon("bool")
        if isinstance(expr, A.TemporalBinary):
            self.expect(self.infer_expr(expr.left, env), TyCon("bool"), expr.left or expr)
            self.expect(self.infer_expr(expr.right, env), TyCon("bool"), expr.right or expr)
            return TyCon("bool")
        if isinstance(expr, A.QuantifierExpr):
            domain_type = self.infer_expr(expr.domain, env)
            item_type = self.collection_item_type(domain_type)
            if item_type is None:
                self.host.error(f"expected finite collection, got {self.format_type(domain_type)}", expr.domain or expr, "type-mismatch")
                item_type = self.fresh("item")
            local = dict(env)
            self.bind_pattern(expr.pattern, item_type, local, generalize=False)
            self.expect(self.infer_expr(expr.body, local), TyCon("bool"), expr.body or expr)
            return TyCon("bool")
        return self.fresh("unknown")

    def infer_block(self, block: A.Block | None, env: dict[str, Scheme], expected: Type | None = None) -> Type:
        if block is None:
            return TyCon("unit")
        local = dict(env)
        for stmt in block.statements:
            self.host.check_type_expr(stmt.type_annotation, self.type_params)
            expected_stmt = self.from_ast(stmt.type_annotation) if stmt.type_annotation is not None else None
            value_type = self.infer_expr(stmt.value, local, expected_stmt)
            if expected_stmt is not None:
                self.expect(value_type, expected_stmt, stmt)
                value_type = expected_stmt
            self.bind_pattern(stmt.pattern, value_type, local, generalize=True)
        return self.infer_expr(block.result, local, expected)

    def infer_call(self, expr: A.Call, env: dict[str, Scheme], expected: Type | None) -> Type:
        name = self.host.expr_to_name(expr.func)
        if name == "to_list" or (name is not None and name.endswith("strings.to_list")):
            self.host.check_arity(name or "<call>", len(expr.args), 1, expr)
            if expr.args:
                self.expect(self.infer_expr(expr.args[0], env), TyCon("string"), expr.args[0])
            return TyCon("List", (TyCon("string"),))

        if name is not None:
            constructor = self.constructor_scheme(name)
            if constructor is not None:
                return self.call_scheme(name, constructor, expr.args, env, expected, expr, allow_unit_payload=True)
            symbol = self.host.symbol_for_name(name)
            if symbol is not None and isinstance(symbol.node, A.EventDecl):
                return self.call_scheme(name, self.event_scheme(symbol.node), expr.args, env, expected, expr)
            if symbol is not None and isinstance(symbol.node, A.FuncDecl):
                return self.call_scheme(name, self.function_scheme(symbol.node), expr.args, env, expected, expr)

        func_type = self.infer_expr(expr.func, env)
        arg_types = tuple(self.infer_expr(arg, env) for arg in expr.args)
        ret = expected or self.fresh("ret")
        try:
            self.unify(func_type, TyFun(arg_types, ret), expr)
        except InferenceError:
            pass
        return ret

    def call_scheme(
        self,
        name: str,
        scheme: Scheme,
        args: list[A.Expr],
        env: dict[str, Scheme],
        expected: Type | None,
        node: A.Node,
        *,
        allow_unit_payload: bool = False,
    ) -> Type:
        typ = self.instantiate(scheme)
        if not isinstance(typ, TyFun):
            return typ
        zero_unit = allow_unit_payload and len(args) == 0 and len(typ.args) == 1 and self.same_type(typ.args[0], TyCon("unit"))
        if len(args) != len(typ.args) and not zero_unit:
            self.host.check_arity(name, len(args), len(typ.args), node)
        if expected is not None:
            self.expect(typ.ret, expected, node)
        for arg_expr, param_type in zip(args, typ.args):
            actual = self.infer_expr(arg_expr, env, param_type)
            self.expect(actual, param_type, arg_expr)
        return expected or typ.ret

    def infer_binary(self, expr: A.BinaryOp, env: dict[str, Scheme]) -> Type:
        if expr.op in {"and", "or", "implies", "->", "iff", "<->"}:
            self.expect(self.infer_expr(expr.left, env), TyCon("bool"), expr.left or expr)
            self.expect(self.infer_expr(expr.right, env), TyCon("bool"), expr.right or expr)
            return TyCon("bool")
        left = self.infer_expr(expr.left, env)
        right = self.infer_expr(expr.right, env)
        if expr.op in {"=", "!="}:
            try:
                self.unify(left, right, expr)
            except InferenceError:
                left_ast = self.to_ast(left)
                right_ast = self.to_ast(right)
                if left_ast is None or right_ast is None or not self.host.are_comparable(left_ast, right_ast, self.type_params):
                    self.host.error(f"cannot compare {self.format_type(left)} and {self.format_type(right)}", expr, "type-mismatch")
            return TyCon("bool")
        if expr.op in {"<", "<=", ">", ">="}:
            self.check_numeric(left, expr.left or expr)
            self.check_numeric(right, expr.right or expr)
            self.warn_numeric_mix(left, right, expr)
            return TyCon("bool")
        if expr.op in {"+", "-", "*", "/", "%"}:
            self.check_numeric(left, expr.left or expr)
            self.check_numeric(right, expr.right or expr)
            self.warn_numeric_mix(left, right, expr)
            if expr.op == "/" or self.type_name(left) in {"rat", "decimal"} or self.type_name(right) in {"rat", "decimal"}:
                return TyCon("rat")
            return TyCon("int")
        return TyCon("bool")

    def infer_unary(self, expr: A.UnaryOp, env: dict[str, Scheme]) -> Type:
        if expr.op == "not":
            self.expect(self.infer_expr(expr.operand, env), TyCon("bool"), expr.operand or expr)
            return TyCon("bool")
        typ = self.infer_expr(expr.operand, env)
        if expr.op == "-":
            self.check_numeric(typ, expr)
        return typ

    def record_constructor(self, expr: A.RecordConstructor, env: dict[str, Scheme]) -> Type:
        typ = TyCon(expr.type_name)
        ast_type = A.TypeRef(name=expr.type_name, line=expr.line, column=expr.column)
        self.host.check_type_expr(ast_type, self.type_params)
        fields = self.host.fields_for_type(ast_type)
        type_known = expr.type_name in self.host.types
        if fields is None and type_known:
            self.host.error(f"type {expr.type_name!r} is not a record type", expr, "not-record-type")
        seen: set[str] = set()
        for name, value in expr.fields:
            if name in seen:
                self.host.error(f"duplicate record field {name!r}", expr, "duplicate-record-field")
            seen.add(name)
            expected = self.from_ast(fields[name]) if fields is not None and name in fields else None
            if fields is not None and name not in fields:
                self.host.error(f"unknown field {name!r}", expr, "unknown-field")
            self.infer_expr(value, env, expected)
        if fields is not None:
            for missing in sorted(set(fields) - seen):
                self.host.error(f"missing record field {missing!r}", expr, "missing-record-field")
        return typ

    def bind_pattern(self, pattern: A.Pattern | None, typ: Type, env: dict[str, Scheme], *, generalize: bool) -> None:
        typ = self.prune(typ)
        if pattern is None or isinstance(pattern, A.WildcardPattern):
            return
        if isinstance(pattern, A.LiteralPattern):
            self.expect(self.literal_pattern_type(pattern), typ, pattern)
            return
        if isinstance(pattern, A.VarPattern):
            env[pattern.name] = self.generalize(typ, env) if generalize else Scheme(frozenset(), typ)
            return
        if isinstance(pattern, A.ConstructorPattern):
            scheme = self.constructor_scheme(pattern.name)
            if scheme is None:
                if pattern.name not in {"last"}:
                    self.host.error(f"undefined constructor {pattern.name!r}", pattern, "undefined-name")
                return
            ctor = self.instantiate(scheme)
            if not isinstance(ctor, TyFun):
                return
            zero_unit = len(pattern.args) == 0 and len(ctor.args) == 1 and self.same_type(ctor.args[0], TyCon("unit"))
            if len(pattern.args) != len(ctor.args) and not zero_unit:
                self.host.check_arity(pattern.name, len(pattern.args), len(ctor.args), pattern)
            self.expect(ctor.ret, typ, pattern)
            if zero_unit:
                return
            for nested, field_type in zip(pattern.args, ctor.args):
                self.bind_pattern(nested, field_type, env, generalize=False)
            return
        if isinstance(pattern, A.RecordPattern):
            fields = self.host.fields_for_type(self.to_ast(typ))
            for name, nested in pattern.fields:
                field_type = self.from_ast(fields[name]) if fields is not None and name in fields else self.fresh(name)
                if nested is None:
                    env[name] = self.generalize(field_type, env) if generalize else Scheme(frozenset(), field_type)
                else:
                    self.bind_pattern(nested, field_type, env, generalize=False)
            return
        if isinstance(pattern, A.TuplePattern):
            item_types = list(self.prune(typ).items) if isinstance(self.prune(typ), TyTuple) else [self.fresh() for _ in pattern.items]
            for nested, item_type in zip(pattern.items, item_types):
                self.bind_pattern(nested, item_type, env, generalize=False)
            return
        if isinstance(pattern, A.ListPattern):
            item_type = self.collection_item_type(typ) or self.fresh("item")
            for nested in pattern.items:
                self.bind_pattern(nested, item_type, env, generalize=False)

    # ------------------------------------------------------------------
    # Schemes and environments
    # ------------------------------------------------------------------

    def initial_env(self, env: dict[str, A.TypeExpr | None]) -> dict[str, Scheme]:
        result: dict[str, Scheme] = {}
        for name, typ in env.items():
            result[name] = Scheme(frozenset(), self.from_ast(typ) if typ is not None else self.fresh(name))
        return result

    def lookup_name(self, name: str, node: A.Node, env: dict[str, Scheme]) -> Type:
        if name == "last":
            return TyCon("bool")
        if name in env:
            return self.instantiate(env[name])
        parts = split_qualified(name)
        if parts and parts[0] in env:
            typ = self.instantiate(env[parts[0]])
            for field in parts[1:]:
                typ = self.field_type(typ, field, node)
            return typ
        self.host.check_name(name, node, {key: self.to_ast(self.instantiate(value)) for key, value in env.items()})
        symbol = self.host.symbol_for_name(name)
        if symbol is not None:
            scheme = getattr(symbol, "scheme", None)
            if isinstance(scheme, Scheme):
                return self.instantiate(scheme)
            if isinstance(symbol.node, A.FuncDecl):
                return self.instantiate(self.function_scheme(symbol.node))
            if isinstance(symbol.node, A.EventDecl):
                return self.instantiate(self.event_scheme(symbol.node))
            if symbol.type_expr is not None:
                return self.from_ast(symbol.type_expr)
        inferred = self.host.infer_name_type(name, {key: self.to_ast(self.instantiate(value)) for key, value in env.items()})
        if inferred is not None:
            return self.from_ast(inferred)
        return self.fresh(local_name(name) or "unknown")

    def function_scheme(self, func: A.FuncDecl) -> Scheme:
        mapping = {name: self.fresh(name) for name in func.type_params}
        args = tuple(self.from_ast(param.type_annotation, mapping) for param in func.params)
        ret = self.from_ast(func.return_type, mapping)
        return Scheme(frozenset(var.id for var in mapping.values()), TyFun(args, ret))

    def event_scheme(self, event: A.EventDecl) -> Scheme:
        args = tuple(self.from_ast(typ) for _, typ in event.fields)
        return Scheme(frozenset(), TyFun(args, TyCon("bool")))

    def constructor_scheme(self, name: str) -> Scheme | None:
        if name not in self.host.constructors:
            return None
        type_name, variant = self.host.constructors[name]
        params = self.host.type_params.get(type_name, [])
        mapping = {param: self.fresh(param) for param in params}
        args = tuple(self.from_ast(field_type, mapping) for _, field_type in variant.fields)
        ret = TyCon(type_name, tuple(mapping[param] for param in params))
        return Scheme(frozenset(var.id for var in mapping.values()), TyFun(args, ret))

    def generalize(self, typ: Type, env: dict[str, Scheme]) -> Scheme:
        typ = self.prune(typ)
        env_vars: set[int] = set()
        for scheme in env.values():
            env_vars.update(self.free_vars(scheme.typ) - set(scheme.vars))
        return Scheme(frozenset(self.free_vars(typ) - env_vars), typ)

    def instantiate(self, scheme: Scheme) -> Type:
        replacements = {var: self.fresh() for var in scheme.vars}

        def replace(typ: Type) -> Type:
            typ = self.prune(typ)
            if isinstance(typ, TyVar):
                return replacements.get(typ.id, typ)
            if isinstance(typ, TyCon):
                return TyCon(typ.name, tuple(replace(arg) for arg in typ.args))
            if isinstance(typ, TyRecord):
                return TyRecord(tuple((name, replace(field_type)) for name, field_type in typ.fields))
            if isinstance(typ, TyTuple):
                return TyTuple(tuple(replace(item) for item in typ.items))
            if isinstance(typ, TyFun):
                return TyFun(tuple(replace(arg) for arg in typ.args), replace(typ.ret))
            return typ

        return replace(scheme.typ)

    # ------------------------------------------------------------------
    # Unification
    # ------------------------------------------------------------------

    def expect(self, actual: Type, expected: Type, node: A.Node) -> Type:
        try:
            return self.unify(actual, expected, node)
        except InferenceError as exc:
            code = "non-bool-expression" if self.same_type(expected, TyCon("bool")) else exc.code
            self.host.error(f"expected {self.format_type(expected)}, got {self.format_type(actual)}", node, code)
            return expected

    def unify(self, left: Type, right: Type, node: A.Node) -> Type:
        left = self.prune(left)
        right = self.prune(right)
        if isinstance(left, TyVar):
            return self.bind_var(left, right, node)
        if isinstance(right, TyVar):
            return self.bind_var(right, left, node)
        if isinstance(left, TyCon) and isinstance(right, TyCon):
            if self.same_type_name(left.name, right.name):
                for l_arg, r_arg in zip(left.args, right.args):
                    self.unify(l_arg, r_arg, node)
                if len(left.args) == len(right.args):
                    return right if right.args else left
            if left.name in NUMERIC_TYPES and right.name in NUMERIC_TYPES and not left.args and not right.args:
                self.warn_numeric_mix(left, right, node)
                return TyCon("rat" if "rat" in {left.name, right.name} or "decimal" in {left.name, right.name} else "int")
            raise InferenceError("type mismatch")
        if isinstance(left, TyRecord) and isinstance(right, TyRecord):
            left_fields = dict(left.fields)
            right_fields = dict(right.fields)
            if set(left_fields) != set(right_fields):
                raise InferenceError("type mismatch")
            for name in left_fields:
                self.unify(left_fields[name], right_fields[name], node)
            return right
        if isinstance(left, TyTuple) and isinstance(right, TyTuple):
            if len(left.items) != len(right.items):
                raise InferenceError("type mismatch")
            for l_item, r_item in zip(left.items, right.items):
                self.unify(l_item, r_item, node)
            return right
        if isinstance(left, TyFun) and isinstance(right, TyFun):
            if len(left.args) != len(right.args):
                raise InferenceError("arity mismatch", "arity-mismatch")
            for l_arg, r_arg in zip(left.args, right.args):
                self.unify(l_arg, r_arg, node)
            self.unify(left.ret, right.ret, node)
            return right
        raise InferenceError("type mismatch")

    def bind_var(self, var: TyVar, typ: Type, node: A.Node) -> Type:
        typ = self.prune(typ)
        if isinstance(typ, TyVar) and typ.id == var.id:
            return typ
        if var.id in self.free_vars(typ):
            self.host.error(f"recursive type {self.format_type(var)} occurs in {self.format_type(typ)}", node, "recursive-type")
            return typ
        self.substitutions[var.id] = typ
        return typ

    def common_type(self, left: Type, right: Type, node: A.Node) -> Type:
        try:
            return self.unify(left, right, node)
        except InferenceError:
            if self.type_name(left) in NUMERIC_TYPES and self.type_name(right) in NUMERIC_TYPES:
                self.warn_numeric_mix(left, right, node)
                if "rat" in {self.type_name(left), self.type_name(right)} or "decimal" in {self.type_name(left), self.type_name(right)}:
                    return TyCon("rat")
                return TyCon("int")
            self.host.error(f"if branches have incompatible types {self.format_type(left)} and {self.format_type(right)}", node, "type-mismatch")
            return left

    def prune(self, typ: Type) -> Type:
        if isinstance(typ, TyVar) and typ.id in self.substitutions:
            resolved = self.prune(self.substitutions[typ.id])
            self.substitutions[typ.id] = resolved
            return resolved
        return typ

    def free_vars(self, typ: Type) -> set[int]:
        typ = self.prune(typ)
        if isinstance(typ, TyVar):
            return {typ.id}
        if isinstance(typ, TyCon):
            return set().union(*(self.free_vars(arg) for arg in typ.args), set())
        if isinstance(typ, TyRecord):
            return set().union(*(self.free_vars(field_type) for _, field_type in typ.fields), set())
        if isinstance(typ, TyTuple):
            return set().union(*(self.free_vars(item) for item in typ.items), set())
        if isinstance(typ, TyFun):
            return set().union(*(self.free_vars(arg) for arg in typ.args), self.free_vars(typ.ret))
        return set()

    # ------------------------------------------------------------------
    # Type helpers
    # ------------------------------------------------------------------

    def field_type(self, typ: Type, field: str, node: A.Node) -> Type:
        ast_type = self.to_ast(typ)
        self.host.check_field(ast_type, field, node)
        field_type = self.host.type_after_fields(ast_type, [field])
        return self.from_ast(field_type) if field_type is not None else self.fresh(field)

    def index_type(self, target_type: Type, index: A.Expr | None) -> Type:
        target_ast = self.to_ast(target_type)
        result = self.host.index_result_type(target_ast, index)
        return self.from_ast(result) if result is not None else self.fresh("item")

    def collection_item_type(self, typ: Type) -> Type | None:
        typ = self.prune(typ)
        if isinstance(typ, TyCon) and local_name(typ.name) in {"List", "Set"} and typ.args:
            return typ.args[0]
        ast_type = self.to_ast(typ)
        item = self.host.collection_item_type(ast_type)
        return self.from_ast(item) if item is not None else None

    def literal_pattern_type(self, pattern: A.LiteralPattern) -> Type:
        return TyCon(pattern.kind if pattern.kind in PRIMITIVE_TYPES else "unit")

    def check_numeric(self, typ: Type, node: A.Node) -> None:
        name = self.type_name(typ)
        if name is None or name in NUMERIC_TYPES:
            return
        self.host.error(f"expected numeric expression, got {self.format_type(typ)}", node, "non-numeric-expression")

    def warn_numeric_mix(self, left: Type, right: Type, node: A.Node) -> None:
        left_name = self.type_name(left)
        right_name = self.type_name(right)
        if left_name in NUMERIC_TYPES and right_name in NUMERIC_TYPES and left_name != right_name:
            self.host.warning(
                f"implicit numeric coercion between {self.format_type(left)} and {self.format_type(right)}",
                node,
                "numeric-coercion",
            )

    def type_name(self, typ: Type) -> str | None:
        typ = self.prune(typ)
        return typ.name if isinstance(typ, TyCon) else None

    def same_type(self, left: Type, right: Type) -> bool:
        left = self.prune(left)
        right = self.prune(right)
        return isinstance(left, TyCon) and isinstance(right, TyCon) and self.same_type_name(left.name, right.name) and len(left.args) == len(right.args)

    def same_type_name(self, left: str | None, right: str | None) -> bool:
        if left is None or right is None:
            return False
        if left == right:
            return True
        try:
            return bool(self.host.same_type_name(left, right))
        except AttributeError:
            left_parts = split_qualified(left)
            right_parts = split_qualified(right)
            return bool(left_parts and right_parts and left_parts[-1] == right_parts[-1])

    def from_ast(self, typ: A.TypeExpr | None, mapping: dict[str, TyVar] | None = None) -> Type:
        mapping = mapping or {}
        if typ is None:
            return TyCon("unit")
        if isinstance(typ, A.TypeRef):
            if typ.name in mapping and not typ.args:
                return mapping[typ.name]
            if typ.name in self.type_params and not typ.args:
                return mapping.setdefault(typ.name, self.fresh(typ.name))
            return TyCon(typ.name, tuple(self.from_ast(arg, mapping) for arg in typ.args))
        if isinstance(typ, A.RecordType):
            return TyRecord(tuple((name, self.from_ast(field_type, mapping)) for name, field_type in typ.fields))
        if isinstance(typ, A.TupleType):
            return TyTuple(tuple(self.from_ast(item, mapping) for item in typ.items))
        return TyCon("unit")

    def to_ast(self, typ: Type | None) -> A.TypeExpr | None:
        if typ is None:
            return None
        typ = self.prune(typ)
        if isinstance(typ, TyVar):
            return A.TypeRef(name=typ.name or f"'{typ.id}")
        if isinstance(typ, TyCon):
            return A.TypeRef(name=typ.name, args=[self.to_ast(arg) or A.TypeRef(name="unit") for arg in typ.args])
        if isinstance(typ, TyRecord):
            return A.RecordType(fields=[(name, self.to_ast(field_type) or A.TypeRef(name="unit")) for name, field_type in typ.fields])
        if isinstance(typ, TyTuple):
            return A.TupleType(items=[self.to_ast(item) or A.TypeRef(name="unit") for item in typ.items])
        if isinstance(typ, TyFun):
            return A.TypeRef(name="<function>")
        return None

    def format_type(self, typ: Type | None) -> str:
        return self.host.format_type(self.to_ast(typ)) if typ is not None else "unknown"
