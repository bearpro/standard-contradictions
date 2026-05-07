from __future__ import annotations

from dataclasses import dataclass, field
from fractions import Fraction
from typing import Any, Callable

from . import ast as A
from .diagnostics import MDLError
from .parser import parse_expr


class RuntimeError(MDLError):
    pass


@dataclass
class Runtime:
    module: A.Module
    values: dict[str, Any] = field(default_factory=dict)
    facts: list[Any] = field(default_factory=list)

    def __post_init__(self) -> None:
        self.functions: dict[str, A.FuncDecl] = {}
        self.entities: dict[str, A.EntityDecl] = {}
        self.builtins: dict[str, Callable[..., Any]] = {
            "strings.to_list": lambda s: list(s),
            "std.system.strings.to_list": lambda s: list(s),
            "List.Cons": lambda head, tail: [head, *list(tail)],
            "List.Empty": lambda: [],
        }
        for decl in self.module.declarations:
            if isinstance(decl, A.FuncDecl):
                self.functions[decl.name] = decl
            elif isinstance(decl, A.EntityDecl):
                self.entities[decl.name] = decl
                self.values.setdefault(decl.name, None)
        # Evaluate top-level values before facts.
        for decl in self.module.declarations:
            if isinstance(decl, A.ValueDecl):
                self.values[decl.name] = self.eval_expr(decl.value, dict(self.values))
        self.apply_facts()

    def apply_facts(self) -> None:
        for decl in self.module.declarations:
            if isinstance(decl, A.FactDecl):
                value = self.eval_expr(decl.value, dict(self.values))
                if decl.target:
                    self.values[decl.target] = value
                else:
                    self.facts.append(value)

    def eval_source_expr(self, source: str) -> Any:
        return self.eval_expr(parse_expr(source), dict(self.values))

    def eval_expr(self, expr: A.Expr | None, env: dict[str, Any] | None = None) -> Any:
        if expr is None:
            return None
        env = env if env is not None else dict(self.values)
        if isinstance(expr, A.Literal):
            return expr.value
        if isinstance(expr, A.Name):
            return self.lookup_name(expr.name, env)
        if isinstance(expr, A.Call):
            func_name = self.expr_to_name(expr.func)
            args = [self.eval_expr(a, env) for a in expr.args]
            return self.call(func_name, args, env)
        if isinstance(expr, A.FieldAccess):
            target = self.eval_expr(expr.target, env)
            if isinstance(target, dict):
                return target[expr.field]
            return getattr(target, expr.field)
        if isinstance(expr, A.IndexAccess):
            return self.eval_expr(expr.target, env)[self.eval_expr(expr.index, env)]
        if isinstance(expr, A.UnaryOp):
            value = self.eval_expr(expr.operand, env)
            if expr.op == "not":
                return not value
            if expr.op == "-":
                return -value
            raise RuntimeError(f"unsupported unary operator {expr.op!r}")
        if isinstance(expr, A.BinaryOp):
            return self.eval_binary(expr, env)
        if isinstance(expr, A.IfExpr):
            return self.eval_expr(expr.then_branch if self.eval_expr(expr.condition, env) else expr.else_branch, env)
        if isinstance(expr, A.LetExpr):
            local = dict(env)
            self.bind_pattern(expr.pattern, self.eval_expr(expr.value, env), local)
            return self.eval_expr(expr.body, local)
        if isinstance(expr, A.MatchExpr):
            return self.eval_match(expr, env)
        if isinstance(expr, A.RecordLiteral):
            return {k: self.eval_expr(v, env) for k, v in expr.fields}
        if isinstance(expr, A.ListLiteral):
            return [self.eval_expr(i, env) for i in expr.items]
        if isinstance(expr, A.SetLiteral):
            return set(self.eval_expr(i, env) for i in expr.items)
        if isinstance(expr, A.TupleLiteral):
            return tuple(self.eval_expr(i, env) for i in expr.items)
        if isinstance(expr, A.BracedExpr):
            return self.eval_expr(expr.expr, env)
        if isinstance(expr, A.TemporalUnary):
            # Runtime is point-wise. Temporal semantics belongs to the translator / model checker.
            return self.eval_expr(expr.operand, env)
        if isinstance(expr, A.TemporalBinary):
            raise RuntimeError("temporal binary operators cannot be evaluated by the point-wise runtime")
        if isinstance(expr, A.QuantifierExpr):
            domain = self.eval_expr(expr.domain, env)
            if expr.quantifier == "forall":
                return all(self.eval_quantified_body(expr, item, env) for item in domain)
            return any(self.eval_quantified_body(expr, item, env) for item in domain)
        raise RuntimeError(f"unsupported expression {expr!r}")

    def eval_quantified_body(self, expr: A.QuantifierExpr, item: Any, env: dict[str, Any]) -> Any:
        local = dict(env)
        self.bind_pattern(expr.pattern, item, local)
        return self.eval_expr(expr.body, local)

    def eval_binary(self, expr: A.BinaryOp, env: dict[str, Any]) -> Any:
        if expr.op == "and":
            return bool(self.eval_expr(expr.left, env)) and bool(self.eval_expr(expr.right, env))
        if expr.op == "or":
            return bool(self.eval_expr(expr.left, env)) or bool(self.eval_expr(expr.right, env))
        left = self.eval_expr(expr.left, env)
        right = self.eval_expr(expr.right, env)
        if expr.op == "=":
            return left == right
        if expr.op == "!=":
            return left != right
        if expr.op == "<":
            return left < right
        if expr.op == "<=":
            return left <= right
        if expr.op == ">":
            return left > right
        if expr.op == ">=":
            return left >= right
        if expr.op == "+":
            return left + right
        if expr.op == "-":
            return left - right
        if expr.op == "*":
            return left * right
        if expr.op == "/":
            return left / right
        if expr.op == "%":
            return left % right
        if expr.op in {"implies", "->"}:
            return (not bool(left)) or bool(right)
        if expr.op in {"iff", "<->"}:
            return bool(left) == bool(right)
        raise RuntimeError(f"unsupported binary operator {expr.op!r}")

    def call(self, func_name: str, args: list[Any], env: dict[str, Any]) -> Any:
        if func_name in self.builtins:
            return self.builtins[func_name](*args)
        # Imported aliases are common in source: strings.to_list.
        if func_name.split(".")[-1] in self.builtins:
            return self.builtins[func_name.split(".")[-1]](*args)
        if func_name in self.functions:
            return self.call_user_function(self.functions[func_name], args, env)
        short = func_name.split(".")[-1]
        if short in self.functions:
            return self.call_user_function(self.functions[short], args, env)
        # ADT constructor fallback.
        if func_name:
            return (func_name.split(".")[-1], tuple(args))
        raise RuntimeError(f"unknown function {func_name!r}")

    def call_user_function(self, func: A.FuncDecl, args: list[Any], outer_env: dict[str, Any]) -> Any:
        if len(args) != len(func.params):
            raise RuntimeError(f"function {func.name} expects {len(func.params)} args, got {len(args)}")
        local = dict(self.values)
        local.update(outer_env)
        for param, arg in zip(func.params, args):
            self.bind_pattern(param.pattern, arg, local)
        return self.eval_block(func.body, local)

    def eval_block(self, block: A.Block | None, env: dict[str, Any]) -> Any:
        if block is None:
            return None
        local = dict(env)
        for stmt in block.statements:
            self.bind_pattern(stmt.pattern, self.eval_expr(stmt.value, local), local)
        return self.eval_expr(block.result, local)

    def eval_match(self, expr: A.MatchExpr, env: dict[str, Any]) -> Any:
        subject = self.eval_expr(expr.subject, env)
        for arm in expr.arms:
            local = dict(env)
            if self.pattern_matches(arm.pattern, subject, local):
                if arm.guard is not None and not self.eval_expr(arm.guard, local):
                    continue
                return self.eval_block(arm.body, local)
        raise RuntimeError("non-exhaustive match at runtime")

    def bind_pattern(self, pattern: A.Pattern | None, value: Any, env: dict[str, Any]) -> None:
        if pattern is None or isinstance(pattern, A.WildcardPattern):
            return
        if isinstance(pattern, A.VarPattern):
            env[pattern.name] = value
            return
        if not self.pattern_matches(pattern, value, env):
            raise RuntimeError(f"pattern {pattern!r} does not match value {value!r}")

    def pattern_matches(self, pattern: A.Pattern | None, value: Any, env: dict[str, Any]) -> bool:
        if pattern is None or isinstance(pattern, A.WildcardPattern):
            return True
        if isinstance(pattern, A.VarPattern):
            env[pattern.name] = value
            return True
        if isinstance(pattern, A.LiteralPattern):
            return pattern.value == value
        if isinstance(pattern, A.ConstructorPattern):
            return self.constructor_pattern_matches(pattern, value, env)
        if isinstance(pattern, A.ListPattern):
            if not isinstance(value, list) or len(value) != len(pattern.items):
                return False
            for p, item in zip(pattern.items, value):
                if not self.pattern_matches(p, item, env):
                    return False
            return True
        if isinstance(pattern, A.TuplePattern):
            if not isinstance(value, tuple) or len(value) != len(pattern.items):
                return False
            for p, item in zip(pattern.items, value):
                if not self.pattern_matches(p, item, env):
                    return False
            return True
        if isinstance(pattern, A.RecordPattern):
            if not isinstance(value, dict):
                return False
            for name, pat in pattern.fields:
                if name not in value:
                    return False
                if pat is not None and not self.pattern_matches(pat, value[name], env):
                    return False
                if pat is None:
                    env[name] = value[name]
            return True
        return False

    def constructor_pattern_matches(self, pattern: A.ConstructorPattern, value: Any, env: dict[str, Any]) -> bool:
        name = pattern.name.split(".")[-1]
        if name == "Cons" and isinstance(value, list) and value:
            if len(pattern.args) != 2:
                return False
            return self.pattern_matches(pattern.args[0], value[0], env) and self.pattern_matches(pattern.args[1], value[1:], env)
        if name == "Empty" and isinstance(value, list):
            return value == []
        if isinstance(value, str):
            return name == value.split(".")[-1]
        if isinstance(value, tuple) and value and isinstance(value[0], str):
            ctor, args = value[0], value[1] if len(value) > 1 else ()
            if name != ctor.split(".")[-1] or len(args) != len(pattern.args):
                return False
            return all(self.pattern_matches(p, arg, env) for p, arg in zip(pattern.args, args))
        if not pattern.args:
            return name == str(value).split(".")[-1]
        return False

    def lookup_name(self, name: str, env: dict[str, Any]) -> Any:
        if name in env:
            return env[name]
        if name in self.values:
            return self.values[name]
        if "." in name:
            parts = name.split(".")
            root = parts[0]
            if root in env or root in self.values:
                value = env.get(root, self.values.get(root))
                for part in parts[1:]:
                    if isinstance(value, dict):
                        value = value[part]
                    else:
                        value = getattr(value, part)
                return value
        # Treat ADT constructors as stable symbolic values.
        if name[:1].isupper() or "." in name:
            return name.split(".")[-1]
        raise RuntimeError(f"unknown name {name!r}")

    def expr_to_name(self, expr: A.Expr | None) -> str:
        if isinstance(expr, A.Name):
            return expr.name
        if isinstance(expr, A.FieldAccess):
            return self.expr_to_name(expr.target) + "." + expr.field
        raise RuntimeError(f"expression is not callable: {expr!r}")


def run_expr(module: A.Module, expression: str) -> Any:
    return Runtime(module).eval_source_expr(expression)
