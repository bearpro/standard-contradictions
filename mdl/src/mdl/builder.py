from __future__ import annotations

from typing import Any

from . import ast as A
from .printer import format_module


def type_ref(name: str, *args: A.TypeExpr | str) -> A.TypeRef:
    return A.TypeRef(name=name, args=[coerce_type(a) for a in args])


def coerce_type(value: A.TypeExpr | str | None) -> A.TypeExpr:
    if value is None:
        return A.TypeRef(name="unit")
    if isinstance(value, A.TypeExpr):
        return value
    text = str(value)
    if any(ch in text for ch in "<>{}(),:"):
        # Keep the builder useful for inferred Python models that store type syntax as strings.
        from .parser import parse_type_expr_source
        return parse_type_expr_source(text)
    return A.TypeRef(name=text)


def ref(name: str) -> A.Name:
    return A.Name(name=name)


def lit(value: Any) -> A.Literal:
    if value is None:
        return A.Literal(value=None, kind="unit")
    if isinstance(value, bool):
        return A.Literal(value=value, kind="bool")
    if isinstance(value, int):
        return A.Literal(value=value, kind="int")
    if isinstance(value, float):
        return A.Literal(value=value, kind="decimal")
    if isinstance(value, str):
        return A.Literal(value=value, kind="string")
    return A.Literal(value=value, kind="unknown")


def call(name_or_expr: str | A.Expr, *args: A.Expr | str | int | bool | float | None) -> A.Call:
    func = ref(name_or_expr) if isinstance(name_or_expr, str) else name_or_expr
    return A.Call(func=func, args=[coerce_expr(a) for a in args])


def field(target: str | A.Expr, name: str) -> A.FieldAccess:
    return A.FieldAccess(target=coerce_expr(target), field=name)


def eq(left: A.Expr | str | int | bool | float | None, right: A.Expr | str | int | bool | float | None) -> A.BinaryOp:
    return binary("=", left, right)


def binary(op: str, left: A.Expr | str | int | bool | float | None, right: A.Expr | str | int | bool | float | None) -> A.BinaryOp:
    return A.BinaryOp(op=op, left=coerce_expr(left), right=coerce_expr(right))


def temporal_binary(op: str, left: A.Expr | str, right: A.Expr | str) -> A.TemporalBinary:
    return A.TemporalBinary(op=op, left=coerce_expr(left), right=coerce_expr(right))


def always(expr: A.Expr | str) -> A.TemporalUnary:
    return A.TemporalUnary(op="always", operand=coerce_expr(expr), position="postfix")


def eventually(expr: A.Expr | str) -> A.TemporalUnary:
    return A.TemporalUnary(op="eventually", operand=coerce_expr(expr), position="postfix")


def next_(expr: A.Expr | str) -> A.TemporalUnary:
    return A.TemporalUnary(op="next", operand=coerce_expr(expr), position="postfix")


def braced(expr: A.Expr | str) -> A.Expr:
    return coerce_expr(expr)


def coerce_expr(value: A.Expr | str | int | bool | float | None) -> A.Expr:
    if isinstance(value, A.Expr):
        return value
    if isinstance(value, str):
        # A small convention for the builder: strings that look like identifiers become refs;
        # all other strings become string literals.
        if value.replace(".", "_").replace("'", "_").isidentifier():
            return ref(value)
        return lit(value)
    return lit(value)


class ModelBuilder:
    """Mutable builder for Python-first model construction.

    The produced object is the same AST used by the parser. This makes it possible to infer
    a model directly in Python and later serialize it to canonical MDL source.
    """

    def __init__(self, module_name: str, annotations: list[str] | None = None):
        self.module = A.Module(name=module_name, annotations=annotations or [])

    def annotation(self, text: str) -> "ModelBuilder":
        self.module.annotations.append(text)
        return self

    def import_(self, path: str) -> "ModelBuilder":
        self.module.imports.append(A.ImportDecl(path=path))
        return self

    def open(self, module: str) -> "ModelBuilder":
        self.module.opens.append(A.OpenDecl(module=module))
        return self

    def type_decl(self, name: str, typ: str | A.TypeExpr) -> A.TypeDecl:
        decl = A.TypeDecl(name=name, definition=coerce_type(typ))
        if not isinstance(decl.definition, A.RecordType):
            raise ValueError("type declarations must define a record or use sum_type()")
        self.module.declarations.append(decl)
        return decl

    def sum_type(self, name: str, variants: list[str | A.Variant]) -> A.TypeDecl:
        normalized = []
        for variant in variants:
            if not isinstance(variant, A.Variant):
                raise ValueError("sum_type variants must be A.Variant values with payload fields")
            if not variant.fields:
                raise ValueError(f"sum type variant {variant.name!r} must declare payload fields")
            normalized.append(variant)
        decl = A.TypeDecl(name=name, definition=A.SumType(variants=normalized))
        self.module.declarations.append(decl)
        return decl

    def value(self, name: str, value: A.Expr | str | int | bool | float | None, typ: str | A.TypeExpr | None = None) -> A.ValueDecl:
        decl = A.ValueDecl(name=name, value=coerce_expr(value), type_annotation=coerce_type(typ) if typ else None)
        self.module.declarations.append(decl)
        return decl

    def entity(self, name: str, typ: str | A.TypeExpr) -> A.EntityDecl:
        decl = A.EntityDecl(name=name, type_annotation=coerce_type(typ))
        self.module.declarations.append(decl)
        return decl

    def event(self, name: str, fields: list[tuple[str, str | A.TypeExpr]] | None = None) -> A.EventDecl:
        decl = A.EventDecl(name=name, fields=[(n, coerce_type(t)) for n, t in (fields or [])])
        self.module.declarations.append(decl)
        return decl

    def func(self, name: str, params: list[tuple[str, str | A.TypeExpr]], return_type: str | A.TypeExpr, body: A.Block | A.Expr) -> A.FuncDecl:
        ps = [A.Param(pattern=A.VarPattern(name=p), type_annotation=coerce_type(t)) for p, t in params]
        block = body if isinstance(body, A.Block) else A.Block(result=body)
        decl = A.FuncDecl(name=name, params=ps, return_type=coerce_type(return_type), body=block)
        self.module.declarations.append(decl)
        return decl

    def rule(self, name: str, modality: str, body: A.Expr | str, strength: str = "defeasible",
             antecedent: A.Expr | None = None, otherwise: A.Expr | None = None) -> A.RuleDecl:
        decl = A.RuleDecl(
            name=name,
            modality=modality,
            body=coerce_expr(body),
            strength=strength,
            antecedent=antecedent,
            otherwise=otherwise,
        )
        self.module.declarations.append(decl)
        return decl

    def fact(self, value: A.Expr | str | int | bool | float | None, target: str | None = None) -> A.FactDecl:
        decl = A.FactDecl(target=target, value=coerce_expr(value))
        self.module.declarations.append(decl)
        return decl

    def to_ast(self) -> A.Module:
        return self.module

    def to_source(self) -> str:
        return format_module(self.module)


def from_python(value: Any) -> A.Module:
    """Coerce a Python-side representation to an MDL module.

    Accepted shapes:
    - an existing `mdl.ast.Module`;
    - a `ModelBuilder`;
    - a dictionary with keys such as `module`, `entities`, `types`, `rules`, `facts`.
    """
    if isinstance(value, A.Module):
        return value
    if isinstance(value, ModelBuilder):
        return value.to_ast()
    if isinstance(value, dict):
        name = str(value.get("module") or value.get("name") or "model")
        builder = ModelBuilder(name, annotations=list(value.get("annotations", [])))
        for imp in value.get("imports", []):
            if isinstance(imp, str):
                builder.import_(imp)
            else:
                builder.import_(imp["path"])
        for opened in value.get("opens", []):
            builder.open(str(opened["module"] if isinstance(opened, dict) else opened))
        for type_name, typ in value.get("types", {}).items():
            if isinstance(typ, list):
                builder.sum_type(type_name, typ)
            else:
                builder.type_decl(type_name, typ)
        for ent, typ in value.get("entities", {}).items():
            builder.entity(ent, typ)
        for event_name, fields in value.get("events", {}).items():
            builder.event(event_name, list(fields))
        for rule in value.get("rules", []):
            builder.rule(
                name=rule["name"],
                modality=rule.get("modality", "O"),
                body=rule["body"] if isinstance(rule["body"], A.Expr) else ref(str(rule["body"])),
                strength=rule.get("strength", "defeasible"),
            )
        for fact in value.get("facts", []):
            if isinstance(fact, dict):
                builder.fact(fact.get("value"), target=fact.get("target"))
            else:
                builder.fact(fact)
        return builder.to_ast()
    raise TypeError(f"cannot coerce {type(value).__name__} to mdl.ast.Module")
