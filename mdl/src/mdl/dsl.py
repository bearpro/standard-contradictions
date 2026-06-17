from __future__ import annotations

import ast as py_ast
from dataclasses import dataclass as py_dataclass
from fractions import Fraction
from pathlib import Path
from types import NoneType
from typing import Callable, TypeAlias, TypeVar, dataclass_transform, overload

from . import ast as A
from .builder import coerce_type
from .diagnostics import Diagnostic
from .printer import format_module

__all__ = [
    "Bool",
    "Decimal",
    "F",
    "Int",
    "O",
    "P",
    "PythonDslError",
    "Rat",
    "String",
    "Unit",
    "always",
    "compile_file",
    "compile_source",
    "entity",
    "eventually",
    "fact",
    "function",
    "import_",
    "implies",
    "module",
    "next_",
    "now",
    "open_",
    "predicate",
    "record",
    "rule",
    "to_source",
    "until",
]


class PythonDslError(ValueError):
    """Raised when Python syntax is outside the supported MDL DSL subset."""

    def __init__(
        self,
        message: str,
        node: py_ast.AST | None = None,
        filename: str = "<dsl>",
        *,
        line: int | None = None,
        column: int | None = None,
        end_line: int | None = None,
        end_column: int | None = None,
    ):
        self.message = message
        self.filename = filename
        self.line = line if line is not None else getattr(node, "lineno", None)
        raw_column = column if column is not None else getattr(node, "col_offset", None)
        self.column = None if raw_column is None else raw_column + 1
        self.end_line = (
            end_line if end_line is not None else getattr(node, "end_lineno", None)
        )
        raw_end_column = (
            end_column
            if end_column is not None
            else getattr(node, "end_col_offset", None)
        )
        self.end_column = None if raw_end_column is None else raw_end_column + 1
        if self.line is None:
            super().__init__(f"{filename}: {message}")
        else:
            column = 1 if self.column is None else self.column
            super().__init__(f"{filename}:{self.line}:{column}: {message}")

    def to_diagnostic(self, path: str | None = None) -> Diagnostic:
        line = self.line or 1
        column = self.column or 1
        return Diagnostic(
            self.message,
            line=line,
            column=column,
            end_line=self.end_line or line,
            end_column=self.end_column or column + 1,
            severity="error",
            code="python-dsl-error",
            path=path or self.filename,
        )


class _RuntimeExpr:
    """Small no-op runtime object so DSL files can be imported without building ASTs.

    The authoritative Python DSL path is static: `compile_source()` parses Python source
    with `ast.parse()` and lowers the supported subset to `mdl.ast`. These runtime
    placeholders are intentionally tiny and exist only to make examples importable and
    friendlier to editors.
    """

    def __init__(self, label: str = "_"):
        self.label = label

    def __getattr__(self, name: str) -> _RuntimeExpr:  # pragma: no cover - convenience
        return _RuntimeExpr(f"{self.label}.{name}")

    def __call__(
        self, *args: object, **kwargs: object
    ) -> _RuntimeExpr:  # pragma: no cover
        return _RuntimeExpr(f"{self.label}(...)")

    def __bool__(self) -> bool:  # pragma: no cover - defensive runtime guard
        raise TypeError(
            "MDL DSL expressions are compiled statically; use '&' or compile_source(), not Python truthiness"
        )

    def _binary(self, other: object, op: str) -> _RuntimeExpr:  # pragma: no cover
        return _RuntimeExpr(f"({self.label} {op} {other!r})")

    def __add__(self, other: object) -> _RuntimeExpr:  # pragma: no cover
        return self._binary(other, "+")

    def __sub__(self, other: object) -> _RuntimeExpr:  # pragma: no cover
        return self._binary(other, "-")

    def __mul__(self, other: object) -> _RuntimeExpr:  # pragma: no cover
        return self._binary(other, "*")

    def __truediv__(self, other: object) -> _RuntimeExpr:  # pragma: no cover
        return self._binary(other, "/")

    def __mod__(self, other: object) -> _RuntimeExpr:  # pragma: no cover
        return self._binary(other, "%")

    def __and__(self, other: object) -> _RuntimeExpr:  # pragma: no cover
        return self._binary(other, "and")

    def __or__(self, other: object) -> _RuntimeExpr:  # pragma: no cover
        return self._binary(other, "or")

    def __eq__(self, other: object) -> _RuntimeExpr:  # pyright: ignore[reportIncompatibleMethodOverride]
        return self._binary(other, "=")

    def __ne__(self, other: object) -> _RuntimeExpr:  # pyright: ignore[reportIncompatibleMethodOverride]
        return self._binary(other, "!=")

    def __lt__(self, other: object) -> _RuntimeExpr:  # pragma: no cover
        return self._binary(other, "<")

    def __le__(self, other: object) -> _RuntimeExpr:  # pragma: no cover
        return self._binary(other, "<=")

    def __gt__(self, other: object) -> _RuntimeExpr:  # pragma: no cover
        return self._binary(other, ">")

    def __ge__(self, other: object) -> _RuntimeExpr:  # pragma: no cover
        return self._binary(other, ">=")


Bool: TypeAlias = bool
Int: TypeAlias = int
Rat: TypeAlias = int | Fraction
Decimal: TypeAlias = float
String: TypeAlias = str
Unit: TypeAlias = NoneType

O = "O"  # noqa: E741 - public DSL modality symbol
F = "F"
P = "P"

_T = TypeVar("_T", bound=type[object])
_NodeT = TypeVar("_NodeT", bound=A.Node)


# Runtime/editor-facing no-op surface. Static compilation below is authoritative.
def module(name: str, annotations: list[str] | None = None) -> None:  # pragma: no cover
    return None


def import_(path: str) -> None:  # pragma: no cover
    return None


def open_(name: str) -> None:  # pragma: no cover
    return None


@overload
def record(cls: _T, **_: object) -> _T: ...


@overload
def record(cls: str | None = None, **_: object) -> Callable[[_T], _T]: ...


@dataclass_transform()
def record(cls: object | None = None, **_: object) -> object:  # pragma: no cover
    def decorate(inner: _T) -> _T:
        try:
            return py_dataclass(inner)
        except Exception:
            return inner

    return decorate if cls is None or not isinstance(cls, type) else decorate(cls)


def entity(type_annotation: object | None = None) -> _RuntimeExpr:  # pragma: no cover
    return _RuntimeExpr("entity")


def fact(expr: object | None = None, **_: object) -> None:  # pragma: no cover
    return None


def rule(modality: str = O, **_: object):  # pragma: no cover
    def decorate(fn):
        return fn

    return decorate


def function(fn=None, **_: object):  # pragma: no cover
    def decorate(inner):
        return inner

    return decorate if fn is None else decorate(fn)


def predicate(fn=None, **_: object):  # pragma: no cover
    def decorate(inner):
        return inner

    return decorate if fn is None else decorate(fn)


def always(expr: object) -> _RuntimeExpr:  # pragma: no cover
    return _RuntimeExpr("always")


def eventually(expr: object) -> _RuntimeExpr:  # pragma: no cover
    return _RuntimeExpr("eventually")


def next_(expr: object) -> _RuntimeExpr:  # pragma: no cover
    return _RuntimeExpr("next")


def now(expr: object) -> _RuntimeExpr:  # pragma: no cover
    return _RuntimeExpr("now")


def until(left: object, right: object) -> _RuntimeExpr:  # pragma: no cover
    return _RuntimeExpr("until")


def implies(left: object, right: object) -> _RuntimeExpr:  # pragma: no cover
    return _RuntimeExpr("implies")


_TYPE_ALIASES = {
    "Bool": "bool",
    "bool": "bool",
    "Int": "int",
    "int": "int",
    "Rat": "rat",
    "rat": "rat",
    "Decimal": "decimal",
    "float": "decimal",
    "String": "string",
    "str": "string",
    "Unit": "unit",
    "None": "unit",
}

_TEMPORAL_UNARY = {
    "always": "always",
    "eventually": "eventually",
    "next": "next",
    "next_": "next",
    "now": "now",
}

_BINARY_OPS: dict[type[py_ast.AST], str] = {
    py_ast.Add: "+",
    py_ast.Sub: "-",
    py_ast.Mult: "*",
    py_ast.Div: "/",
    py_ast.Mod: "%",
}

_COMPARE_OPS: dict[type[py_ast.AST], str] = {
    py_ast.Eq: "=",
    py_ast.NotEq: "!=",
    py_ast.Lt: "<",
    py_ast.LtE: "<=",
    py_ast.Gt: ">",
    py_ast.GtE: ">=",
}


class _Compiler:
    def __init__(self, filename: str = "<dsl>", default_module_name: str = "model"):
        self.filename = filename
        self.module = A.Module(name=default_module_name)
        self.record_types: set[str] = set()

    def _mark(self, value: _NodeT, node: py_ast.AST | A.Node | None) -> _NodeT:
        if node is None:
            return value
        value.line = getattr(node, "lineno", 0) or 0
        value.column = (getattr(node, "col_offset", 0) or 0) + 1
        value.end_line = getattr(node, "end_lineno", 0) or value.line
        value.end_column = (getattr(node, "end_col_offset", 0) or 0) + 1
        return value

    def compile(self, source: str) -> A.Module:
        try:
            tree = py_ast.parse(source, filename=self.filename)
        except SyntaxError as exc:
            raise PythonDslError(
                exc.msg,
                filename=self.filename,
                line=exc.lineno,
                column=(exc.offset - 1) if exc.offset else None,
                end_line=exc.end_lineno,
                end_column=(exc.end_offset - 1) if exc.end_offset else None,
            ) from exc

        for stmt in tree.body:
            self._top_level(stmt)
        return self.module

    def _top_level(self, stmt: py_ast.stmt) -> None:
        if isinstance(stmt, (py_ast.Import, py_ast.ImportFrom)):
            return
        if self._is_docstring(stmt):
            return
        if isinstance(stmt, py_ast.Expr) and isinstance(stmt.value, py_ast.Call):
            self._top_level_call(stmt.value)
            return
        if isinstance(stmt, py_ast.ClassDef):
            self._class_decl(stmt)
            return
        if isinstance(stmt, (py_ast.FunctionDef, py_ast.AsyncFunctionDef)):
            self._function_like_decl(stmt)
            return
        if isinstance(stmt, py_ast.Assign):
            self._assign_decl(stmt.targets, stmt.value, None, stmt)
            return
        if isinstance(stmt, py_ast.AnnAssign):
            self._assign_decl([stmt.target], stmt.value, stmt.annotation, stmt)
            return
        raise self._error(
            f"unsupported top-level statement {type(stmt).__name__}", stmt
        )

    def _top_level_call(self, call: py_ast.Call) -> None:
        name = self._call_name(call)
        if name == "module":
            self._expect_arg_count(call, 1)
            self._expect_known_keywords(call, {"annotations"})
            self.module.name = self._string_or_name(call.args[0])
            self._mark(self.module, call)
            annotations = self._keyword(call, "annotations")
            if annotations is not None:
                self.module.annotations = self._string_list(annotations)
            return
        if name == "import_":
            self._expect_arg_count(call, 1)
            self._expect_no_keywords(call)
            self.module.imports.append(
                self._mark(A.ImportDecl(path=self._string_or_name(call.args[0])), call)
            )
            return
        if name == "open_":
            self._expect_arg_count(call, 1)
            self._expect_no_keywords(call)
            self.module.opens.append(
                self._mark(A.OpenDecl(module=self._string_or_name(call.args[0])), call)
            )
            return
        if name == "fact":
            self.module.declarations.append(self._fact_decl(call))
            return
        raise self._error(f"unsupported top-level call {name or '<expr>'}", call)

    def _class_decl(self, stmt: py_ast.ClassDef) -> None:
        decorator = self._decorator(stmt.decorator_list, "record")
        if decorator is None:
            raise self._error(
                "classes in MDL Python DSL must be decorated with @record", stmt
            )
        if isinstance(decorator, py_ast.Call):
            self._expect_known_keywords(decorator, {"name"})
            if len(decorator.args) > 1:
                raise self._error(
                    "@record accepts at most one name argument", decorator
                )
        type_name = self._decorator_name_override(decorator, default=stmt.name)
        fields: list[tuple[str, A.TypeExpr]] = []
        for item in self._without_docstring(stmt.body):
            if isinstance(item, py_ast.Pass):
                continue
            if not isinstance(item, py_ast.AnnAssign) or not isinstance(
                item.target, py_ast.Name
            ):
                raise self._error(
                    "record classes may only contain annotated fields", item
                )
            if item.value is not None:
                raise self._error("record fields do not support default values", item)
            fields.append((item.target.id, self._type_expr(item.annotation)))
        self.record_types.add(type_name)
        self.module.declarations.append(
            self._mark(
                A.TypeDecl(
                    name=type_name,
                    definition=self._mark(A.RecordType(fields=fields), stmt),
                ),
                stmt,
            )
        )

    def _function_like_decl(
        self, stmt: py_ast.FunctionDef | py_ast.AsyncFunctionDef
    ) -> None:
        if isinstance(stmt, py_ast.AsyncFunctionDef):
            raise self._error(
                "async functions are not supported in MDL Python DSL", stmt
            )

        entity_decorator = self._decorator(stmt.decorator_list, "entity")
        if entity_decorator is not None:
            self._entity_function_decl(stmt, entity_decorator)
            return

        rule_decorator = self._decorator(stmt.decorator_list, "rule")
        if rule_decorator is not None:
            self._rule_decl(stmt, rule_decorator)
            return

        predicate_decorator = self._decorator(stmt.decorator_list, "predicate")
        if predicate_decorator is not None:
            self._func_decl(stmt, return_fallback=A.TypeRef(name="bool"))
            return

        function_decorator = self._decorator(stmt.decorator_list, "function")
        if function_decorator is not None:
            self._func_decl(stmt, return_fallback=None)
            return

        raise self._error(
            "functions in MDL Python DSL must use @rule, @predicate, @function, or @entity",
            stmt,
        )

    def _entity_function_decl(
        self, stmt: py_ast.FunctionDef, decorator: py_ast.expr
    ) -> None:
        if (
            stmt.args.args
            or stmt.args.kwonlyargs
            or stmt.args.vararg
            or stmt.args.kwarg
        ):
            raise self._error("@entity functions must not declare parameters", stmt)
        if isinstance(decorator, py_ast.Call):
            self._expect_no_keywords(decorator)
            if len(decorator.args) != 1:
                raise self._error(
                    "@entity requires exactly one type argument", decorator
                )
        type_annotation = self._decorator_first_type_arg(decorator)
        self.module.declarations.append(
            self._mark(
                A.EntityDecl(name=stmt.name, type_annotation=type_annotation), stmt
            )
        )

    def _rule_decl(self, stmt: py_ast.FunctionDef, decorator: py_ast.expr) -> None:
        if isinstance(decorator, py_ast.Call):
            self._expect_known_keywords(
                decorator,
                {"antecedent", "modality", "name", "otherwise", "strength", "when"},
            )
            if len(decorator.args) > 1:
                raise self._error(
                    "@rule accepts at most one positional modality argument", decorator
                )
        if (
            stmt.args.args
            or stmt.args.kwonlyargs
            or stmt.args.vararg
            or stmt.args.kwarg
        ):
            raise self._error("@rule functions must not declare parameters", stmt)
        block = self._block_from_body(stmt.body)
        if block.result is None:
            raise self._error("@rule functions must return an expression", stmt)
        name = self._decorator_string_keyword(decorator, "name", default=stmt.name)
        modality = self._rule_modality(decorator)
        strength = self._decorator_string_keyword(
            decorator, "strength", default="defeasible"
        )
        antecedent = self._decorator_expr_keyword(
            decorator, "when"
        ) or self._decorator_expr_keyword(decorator, "antecedent")
        otherwise = self._decorator_expr_keyword(decorator, "otherwise")
        self.module.declarations.append(
            self._mark(
                A.RuleDecl(
                    name=name,
                    modality=modality,
                    body=self._block_as_expr(block),
                    antecedent=antecedent,
                    otherwise=otherwise,
                    strength=strength,
                ),
                stmt,
            )
        )

    def _func_decl(
        self, stmt: py_ast.FunctionDef, return_fallback: A.TypeExpr | None
    ) -> None:
        params: list[A.Param] = []
        for arg in stmt.args.args:
            if arg.annotation is None:
                raise self._error(
                    f"parameter {arg.arg!r} must have a type annotation", arg
                )
            params.append(
                self._mark(
                    A.Param(
                        pattern=self._mark(A.VarPattern(name=arg.arg), arg),
                        type_annotation=self._type_expr(arg.annotation),
                    ),
                    arg,
                )
            )
        if (
            stmt.args.posonlyargs
            or stmt.args.kwonlyargs
            or stmt.args.vararg
            or stmt.args.kwarg
        ):
            raise self._error("only positional function parameters are supported", stmt)
        return_type = (
            self._type_expr(stmt.returns)
            if stmt.returns is not None
            else return_fallback
        )
        if return_type is None:
            raise self._error(
                "@function declarations must have a return type annotation", stmt
            )
        self.module.declarations.append(
            self._mark(
                A.FuncDecl(
                    name=stmt.name,
                    params=params,
                    return_type=return_type,
                    body=self._block_from_body(stmt.body),
                ),
                stmt,
            )
        )

    def _assign_decl(
        self,
        targets: list[py_ast.expr],
        value: py_ast.expr | None,
        annotation: py_ast.expr | None,
        stmt: py_ast.stmt,
    ) -> None:
        if len(targets) != 1 or not isinstance(targets[0], py_ast.Name):
            raise self._error("top-level assignments must target a single name", stmt)
        if value is None:
            raise self._error("top-level annotations must also assign a value", stmt)
        target_name = targets[0].id
        if isinstance(value, py_ast.Call) and self._call_name(value) == "entity":
            type_annotation = self._entity_type_from_call(value, annotation)
            self.module.declarations.append(
                self._mark(
                    A.EntityDecl(name=target_name, type_annotation=type_annotation),
                    stmt,
                )
            )
            return
        raise self._error(
            "top-level assignments only support entity(...) declarations", stmt
        )

    def _fact_decl(self, call: py_ast.Call) -> A.FactDecl:
        self._expect_known_keywords(call, {"value"})
        if len(call.args) > 1:
            raise self._error("fact() accepts at most one positional value", call)
        value_node = call.args[0] if call.args else self._keyword(call, "value")
        if value_node is None:
            raise self._error("fact() requires a value expression", call)
        return self._mark(A.FactDecl(value=self._expr(value_node)), call)

    def _block_from_body(self, body: list[py_ast.stmt]) -> A.Block:
        statements: list[A.LetStmt] = []
        result: A.Expr | None = None
        for stmt in self._without_docstring(body):
            if isinstance(stmt, py_ast.Pass):
                continue
            if isinstance(stmt, py_ast.Return):
                if result is not None:
                    raise self._error(
                        "function body has more than one result expression", stmt
                    )
                result = (
                    self._expr(stmt.value)
                    if stmt.value is not None
                    else A.Literal(value=None, kind="unit")
                )
                continue
            if isinstance(stmt, py_ast.Assign):
                statements.append(self._let_stmt(stmt.targets, stmt.value, None, stmt))
                continue
            if isinstance(stmt, py_ast.AnnAssign):
                statements.append(
                    self._let_stmt([stmt.target], stmt.value, stmt.annotation, stmt)
                )
                continue
            if isinstance(stmt, py_ast.If):
                if result is not None:
                    raise self._error(
                        "if statement cannot appear after the result expression", stmt
                    )
                result = self._if_stmt_expr(stmt)
                continue
            raise self._error(
                f"unsupported statement in function body: {type(stmt).__name__}", stmt
            )
        return A.Block(statements=statements, result=result)

    def _let_stmt(
        self,
        targets: list[py_ast.expr],
        value: py_ast.expr | None,
        annotation: py_ast.expr | None,
        stmt: py_ast.stmt,
    ) -> A.LetStmt:
        if len(targets) != 1 or not isinstance(targets[0], py_ast.Name):
            raise self._error("local assignments must target a single name", stmt)
        if value is None:
            raise self._error("local annotations must also assign a value", stmt)
        return self._mark(
            A.LetStmt(
                pattern=self._mark(A.VarPattern(name=targets[0].id), targets[0]),
                value=self._expr(value),
                type_annotation=self._type_expr(annotation)
                if annotation is not None
                else None,
            ),
            stmt,
        )

    def _if_stmt_expr(self, stmt: py_ast.If) -> A.IfExpr:
        then_expr = self._single_result_block(stmt.body, stmt)
        else_expr = self._single_result_block(stmt.orelse, stmt)
        return self._mark(
            A.IfExpr(
                condition=self._expr(stmt.test),
                then_branch=then_expr,
                else_branch=else_expr,
            ),
            stmt,
        )

    def _single_result_block(self, body: list[py_ast.stmt], stmt: py_ast.If) -> A.Expr:
        if len(body) == 1 and isinstance(body[0], py_ast.Return):
            return self._expr(body[0].value)
        if len(body) == 1 and isinstance(body[0], py_ast.If):
            return self._if_stmt_expr(body[0])
        raise self._error(
            "if statement branches must contain a single return or nested if", stmt
        )

    def _block_as_expr(self, block: A.Block) -> A.Expr:
        if block.result is None:
            return A.Literal(value=None, kind="unit")
        result = block.result
        for stmt in reversed(block.statements):
            result = self._mark(
                A.LetExpr(
                    pattern=stmt.pattern,
                    value=stmt.value,
                    body=result,
                    type_annotation=stmt.type_annotation,
                ),
                stmt,
            )
        return result

    def _expr(self, node: py_ast.expr | None) -> A.Expr:
        if node is None:
            return A.Literal(value=None, kind="unit")
        if isinstance(node, py_ast.Constant):
            return self._literal(node)
        if isinstance(node, py_ast.Name):
            return self._mark(A.Name(name=node.id), node)
        if isinstance(node, py_ast.Attribute):
            return self._mark(
                A.FieldAccess(target=self._expr(node.value), field=node.attr), node
            )
        if isinstance(node, py_ast.Call):
            return self._call_expr(node)
        if isinstance(node, py_ast.BoolOp):
            return self._bool_expr(node)
        if isinstance(node, py_ast.BinOp):
            return self._binary_expr(node)
        if isinstance(node, py_ast.Compare):
            return self._compare_expr(node)
        if isinstance(node, py_ast.UnaryOp):
            return self._unary_expr(node)
        if isinstance(node, py_ast.IfExp):
            return self._mark(
                A.IfExpr(
                    condition=self._expr(node.test),
                    then_branch=self._expr(node.body),
                    else_branch=self._expr(node.orelse),
                ),
                node,
            )
        if isinstance(node, py_ast.Tuple):
            if len(node.elts) == 0:
                return self._mark(A.Literal(value=None, kind="unit"), node)
            if len(node.elts) == 1:
                raise self._error(
                    "single-item tuple literals are not valid MDL values", node
                )
            return self._mark(
                A.TupleLiteral(items=[self._expr(item) for item in node.elts]), node
            )
        raise self._error(f"unsupported expression {type(node).__name__}", node)

    def _literal(self, node: py_ast.Constant) -> A.Literal:
        value = node.value
        if value is None:
            return self._mark(A.Literal(value=None, kind="unit"), node)
        if isinstance(value, bool):
            return self._mark(A.Literal(value=value, kind="bool"), node)
        if isinstance(value, int):
            return self._mark(A.Literal(value=value, kind="int"), node)
        if isinstance(value, float):
            return self._mark(A.Literal(value=value, kind="decimal"), node)
        if isinstance(value, str):
            return self._mark(A.Literal(value=value, kind="string"), node)
        raise self._error(f"unsupported literal {value!r}", node)

    def _call_expr(self, node: py_ast.Call) -> A.Expr:
        name = self._call_name(node)
        if name in _TEMPORAL_UNARY:
            self._expect_arg_count(node, 1)
            self._expect_no_keywords(node)
            return self._mark(
                A.TemporalUnary(
                    op=_TEMPORAL_UNARY[name],
                    operand=self._expr(node.args[0]),
                    position="postfix",
                ),
                node,
            )
        if name == "until":
            self._expect_arg_count(node, 2)
            self._expect_no_keywords(node)
            return self._mark(
                A.TemporalBinary(
                    op="until",
                    left=self._expr(node.args[0]),
                    right=self._expr(node.args[1]),
                ),
                node,
            )
        if name == "implies":
            self._expect_arg_count(node, 2)
            self._expect_no_keywords(node)
            return self._mark(
                A.BinaryOp(
                    op="implies",
                    left=self._expr(node.args[0]),
                    right=self._expr(node.args[1]),
                ),
                node,
            )
        if self._is_record_constructor(node):
            return self._mark(
                A.RecordConstructor(
                    type_name=self._qualified_name(node.func),
                    fields=[
                        (kw.arg or self._raise_keyword_error(kw), self._expr(kw.value))
                        for kw in node.keywords
                    ],
                ),
                node,
            )
        if node.keywords:
            raise self._error(
                "ordinary function calls do not support keyword arguments", node
            )
        return self._mark(
            A.Call(
                func=self._expr(node.func), args=[self._expr(arg) for arg in node.args]
            ),
            node,
        )

    def _bool_expr(self, node: py_ast.BoolOp) -> A.Expr:
        op = "and" if isinstance(node.op, py_ast.And) else "or"
        if len(node.values) < 2:
            raise self._error("boolean operations must have at least two values", node)
        result = self._expr(node.values[0])
        for value in node.values[1:]:
            result = self._mark(
                A.BinaryOp(op=op, left=result, right=self._expr(value)), node
            )
        return result

    def _binary_expr(self, node: py_ast.BinOp) -> A.BinaryOp:
        op = _BINARY_OPS.get(type(node.op))
        if op is None:
            raise self._error(
                f"unsupported binary operator {type(node.op).__name__}", node
            )
        return self._mark(
            A.BinaryOp(op=op, left=self._expr(node.left), right=self._expr(node.right)),
            node,
        )

    def _compare_expr(self, node: py_ast.Compare) -> A.Expr:
        left = node.left
        comparisons: list[A.Expr] = []
        for op_node, right in zip(node.ops, node.comparators, strict=True):
            op = _COMPARE_OPS.get(type(op_node))
            if op is None:
                raise self._error(
                    f"unsupported comparison operator {type(op_node).__name__}", node
                )
            comparisons.append(
                self._mark(
                    A.BinaryOp(op=op, left=self._expr(left), right=self._expr(right)),
                    node,
                )
            )
            left = right
        result = comparisons[0]
        for comparison in comparisons[1:]:
            result = self._mark(
                A.BinaryOp(op="and", left=result, right=comparison), node
            )
        return result

    def _unary_expr(self, node: py_ast.UnaryOp) -> A.Expr:
        if isinstance(node.op, py_ast.Not):
            return self._mark(
                A.UnaryOp(op="not", operand=self._expr(node.operand)), node
            )
        if isinstance(node.op, py_ast.USub):
            return self._mark(A.UnaryOp(op="-", operand=self._expr(node.operand)), node)
        if isinstance(node.op, py_ast.UAdd):
            return self._expr(node.operand)
        raise self._error(f"unsupported unary operator {type(node.op).__name__}", node)

    def _type_expr(self, node: py_ast.expr | None) -> A.TypeExpr:
        if node is None:
            return A.TypeRef(name="unit")
        if isinstance(node, py_ast.Name):
            return self._mark(A.TypeRef(name=_TYPE_ALIASES.get(node.id, node.id)), node)
        if isinstance(node, py_ast.Attribute):
            return self._mark(A.TypeRef(name=self._qualified_name(node)), node)
        if isinstance(node, py_ast.Constant) and isinstance(node.value, str):
            return coerce_type(node.value)
        if isinstance(node, py_ast.Tuple):
            return self._mark(
                A.TupleType(items=[self._type_expr(item) for item in node.elts]), node
            )
        if isinstance(node, py_ast.Subscript):
            name = self._type_name(node.value)
            return self._mark(
                A.TypeRef(
                    name=name,
                    args=[
                        self._type_expr(arg) for arg in self._subscript_args(node.slice)
                    ],
                ),
                node,
            )
        raise self._error(f"unsupported type expression {type(node).__name__}", node)

    def _subscript_args(self, node: py_ast.expr) -> list[py_ast.expr]:
        if isinstance(node, py_ast.Tuple):
            return list(node.elts)
        return [node]

    def _type_name(self, node: py_ast.expr) -> str:
        if isinstance(node, py_ast.Name):
            return _TYPE_ALIASES.get(node.id, node.id)
        if isinstance(node, py_ast.Attribute):
            return self._qualified_name(node)
        raise self._error(f"unsupported generic type name {type(node).__name__}", node)

    def _entity_type_from_call(
        self, call: py_ast.Call, annotation: py_ast.expr | None
    ) -> A.TypeExpr:
        if len(call.args) > 1:
            raise self._error("entity() accepts at most one type argument", call)
        if call.args:
            return self._type_expr(call.args[0])
        if annotation is not None:
            return self._type_expr(annotation)
        raise self._error(
            "entity() requires a type argument or annotated assignment", call
        )

    def _is_record_constructor(self, node: py_ast.Call) -> bool:
        if not node.keywords:
            return False
        name = self._qualified_name(node.func)
        short_name = name.rsplit(".", 1)[-1]
        return short_name in self.record_types or short_name[:1].isupper()

    def _rule_modality(self, decorator: py_ast.expr) -> str | None:
        if isinstance(decorator, py_ast.Call):
            if decorator.args:
                return self._string_or_name(decorator.args[0])
            modality = self._keyword(decorator, "modality")
            if modality is not None:
                return self._string_or_name(modality)
        return "O"

    def _decorator_first_type_arg(self, decorator: py_ast.expr) -> A.TypeExpr:
        if isinstance(decorator, py_ast.Call) and decorator.args:
            return self._type_expr(decorator.args[0])
        raise self._error("@entity requires a type argument", decorator)

    def _decorator_name_override(self, decorator: py_ast.expr, default: str) -> str:
        if isinstance(decorator, py_ast.Call):
            if decorator.args:
                return self._string_or_name(decorator.args[0])
            name = self._keyword(decorator, "name")
            if name is not None:
                return self._string_or_name(name)
        return default

    def _decorator_string_keyword(
        self, decorator: py_ast.expr, keyword: str, default: str
    ) -> str:
        if isinstance(decorator, py_ast.Call):
            value_node = self._keyword(decorator, keyword)
            if value_node is not None:
                return self._string_or_name(value_node)
        return default

    def _decorator_expr_keyword(
        self, decorator: py_ast.expr, keyword: str
    ) -> A.Expr | None:
        if isinstance(decorator, py_ast.Call):
            value_node = self._keyword(decorator, keyword)
            if value_node is not None:
                return self._expr(value_node)
        return None

    def _decorator(
        self, decorators: list[py_ast.expr], name: str
    ) -> py_ast.expr | None:
        for decorator in decorators:
            if self._decorator_base_name(decorator) == name:
                return decorator
        return None

    def _decorator_base_name(self, decorator: py_ast.expr) -> str | None:
        target = decorator.func if isinstance(decorator, py_ast.Call) else decorator
        qualified = self._qualified_name(target)
        return qualified.rsplit(".", 1)[-1] if qualified else None

    def _call_name(self, call: py_ast.Call) -> str | None:
        qualified = self._qualified_name(call.func)
        return qualified.rsplit(".", 1)[-1] if qualified else None

    def _qualified_name(self, node: py_ast.expr) -> str:
        if isinstance(node, py_ast.Name):
            return node.id
        if isinstance(node, py_ast.Attribute):
            prefix = self._qualified_name(node.value)
            return f"{prefix}.{node.attr}" if prefix else node.attr
        return ""

    def _keyword(self, call: py_ast.Call, name: str) -> py_ast.expr | None:
        for keyword in call.keywords:
            if keyword.arg == name:
                return keyword.value
        return None

    def _expect_arg_count(self, call: py_ast.Call, count: int) -> None:
        if len(call.args) != count:
            name = self._call_name(call) or "call"
            raise self._error(f"{name}() expects {count} positional argument(s)", call)
        if any(keyword.arg is None for keyword in call.keywords):
            raise self._error("**kwargs are not supported", call)

    def _expect_no_keywords(self, call: py_ast.Call) -> None:
        if call.keywords:
            name = self._call_name(call) or "call"
            raise self._error(f"{name}() does not accept keyword arguments", call)

    def _expect_known_keywords(self, call: py_ast.Call, allowed: set[str]) -> None:
        for keyword in call.keywords:
            if keyword.arg is None:
                raise self._error("**kwargs are not supported", call)
            if keyword.arg not in allowed:
                name = self._call_name(call) or "call"
                raise self._error(
                    f"{name}() does not accept keyword argument {keyword.arg!r}", call
                )

    def _string_or_name(self, node: py_ast.expr) -> str:
        if isinstance(node, py_ast.Constant) and isinstance(node.value, str):
            return node.value
        if isinstance(node, py_ast.Name):
            return node.id
        if isinstance(node, py_ast.Attribute):
            return self._qualified_name(node)
        raise self._error("expected a string literal or name", node)

    def _string_list(self, node: py_ast.expr) -> list[str]:
        if not isinstance(node, (py_ast.List, py_ast.Tuple)):
            raise self._error("expected a list of strings", node)
        return [self._string_or_name(item) for item in node.elts]

    def _without_docstring(self, body: list[py_ast.stmt]) -> list[py_ast.stmt]:
        if body and self._is_docstring(body[0]):
            return body[1:]
        return body

    def _is_docstring(self, stmt: py_ast.stmt) -> bool:
        return (
            isinstance(stmt, py_ast.Expr)
            and isinstance(stmt.value, py_ast.Constant)
            and isinstance(stmt.value.value, str)
        )

    def _raise_keyword_error(self, keyword: py_ast.keyword) -> str:
        raise self._error(
            "**kwargs are not supported in record constructors", keyword.value
        )

    def _error(self, message: str, node: py_ast.AST | None = None) -> PythonDslError:
        return PythonDslError(message, node=node, filename=self.filename)


def compile_source(
    source: str, filename: str = "<dsl>", default_module_name: str = "model"
) -> A.Module:
    """Compile Python DSL source into the canonical MDL AST.

    The compiler is static and intentionally accepts only a safe, declarative subset of
    Python: imports, `module(...)`, `@record` classes, `entity(...)` assignments,
    `@function`/`@predicate` functions, `@rule(...)` functions, and `fact(...)` calls.
    """

    return _Compiler(
        filename=filename, default_module_name=default_module_name
    ).compile(source)


def compile_file(path: str | Path) -> A.Module:
    file_path = Path(path)
    return compile_source(
        file_path.read_text(encoding="utf-8"),
        filename=str(file_path),
        default_module_name=_default_module_name(file_path),
    )


def to_source(
    source: str, filename: str = "<dsl>", default_module_name: str = "model"
) -> str:
    return format_module(
        compile_source(
            source, filename=filename, default_module_name=default_module_name
        )
    )


def _default_module_name(path: str | Path) -> str:
    name = Path(path).name
    if name.endswith(".mdl.py"):
        return name[: -len(".mdl.py")] or "model"
    return Path(name).stem or "model"
