from __future__ import annotations

from dataclasses import dataclass, field, fields, is_dataclass
from typing import Any


@dataclass
class Node:
    line: int = 0
    column: int = 0
    end_line: int = 0
    end_column: int = 0


# ---------------------------
# Type AST
# ---------------------------

@dataclass
class TypeExpr(Node):
    pass


@dataclass
class TypeRef(TypeExpr):
    name: str = ""
    args: list[TypeExpr] = field(default_factory=list)


@dataclass
class RecordType(TypeExpr):
    fields: list[tuple[str, TypeExpr]] = field(default_factory=list)


@dataclass
class TupleType(TypeExpr):
    items: list[TypeExpr] = field(default_factory=list)


@dataclass
class Variant(Node):
    name: str = ""
    fields: list[tuple[str | None, TypeExpr]] = field(default_factory=list)


@dataclass
class SumType(Node):
    variants: list[Variant] = field(default_factory=list)


# ---------------------------
# Pattern AST
# ---------------------------

@dataclass
class Pattern(Node):
    pass


@dataclass
class WildcardPattern(Pattern):
    pass


@dataclass
class VarPattern(Pattern):
    name: str = ""


@dataclass
class LiteralPattern(Pattern):
    value: Any = None
    kind: str = "unknown"


@dataclass
class ConstructorPattern(Pattern):
    name: str = ""
    args: list[Pattern] = field(default_factory=list)


@dataclass
class RecordPattern(Pattern):
    fields: list[tuple[str, Pattern | None]] = field(default_factory=list)


@dataclass
class TuplePattern(Pattern):
    items: list[Pattern] = field(default_factory=list)


@dataclass
class ListPattern(Pattern):
    items: list[Pattern] = field(default_factory=list)


# ---------------------------
# Expression AST
# ---------------------------

@dataclass
class Expr(Node):
    pass


@dataclass
class Literal(Expr):
    value: Any = None
    kind: str = "unknown"


@dataclass
class Name(Expr):
    name: str = ""


@dataclass
class Call(Expr):
    func: Expr | None = None
    args: list[Expr] = field(default_factory=list)


@dataclass
class FieldAccess(Expr):
    target: Expr | None = None
    field: str = ""


@dataclass
class BinaryOp(Expr):
    op: str = ""
    left: Expr | None = None
    right: Expr | None = None


@dataclass
class UnaryOp(Expr):
    op: str = ""
    operand: Expr | None = None


@dataclass
class IfExpr(Expr):
    condition: Expr | None = None
    then_branch: Expr | None = None
    else_branch: Expr | None = None


@dataclass
class LetExpr(Expr):
    pattern: Pattern | None = None
    value: Expr | None = None
    body: Expr | None = None


@dataclass
class MatchArm(Node):
    pattern: Pattern | None = None
    guard: Expr | None = None
    body: Block | None = None


@dataclass
class MatchExpr(Expr):
    subject: Expr | None = None
    arms: list[MatchArm] = field(default_factory=list)


@dataclass
class RecordConstructor(Expr):
    type_name: str = ""
    fields: list[tuple[str, Expr]] = field(default_factory=list)


@dataclass
class TupleLiteral(Expr):
    items: list[Expr] = field(default_factory=list)


@dataclass
class TemporalUnary(Expr):
    op: str = ""
    operand: Expr | None = None
    position: str = "prefix"  # prefix | postfix


@dataclass
class TemporalBinary(Expr):
    op: str = ""
    left: Expr | None = None
    right: Expr | None = None


# ---------------------------
# Blocks and declarations
# ---------------------------

@dataclass
class LetStmt(Node):
    pattern: Pattern | None = None
    value: Expr | None = None
    type_annotation: TypeExpr | None = None


@dataclass
class Block(Node):
    statements: list[LetStmt] = field(default_factory=list)
    result: Expr | None = None


@dataclass
class Declaration(Node):
    annotations: list[str] = field(default_factory=list)


@dataclass
class ImportDecl(Node):
    path: str = ""
    annotations: list[str] = field(default_factory=list)


@dataclass
class OpenDecl(Node):
    module: str = ""
    annotations: list[str] = field(default_factory=list)


@dataclass
class TypeDecl(Declaration):
    name: str = ""
    params: list[str] = field(default_factory=list)
    definition: TypeExpr | SumType | None = None


@dataclass
class ValueDecl(Declaration):
    name: str = ""
    type_annotation: TypeExpr | None = None
    value: Expr | None = None


@dataclass
class Param(Node):
    pattern: Pattern | None = None
    type_annotation: TypeExpr | None = None


@dataclass
class FuncDecl(Declaration):
    name: str = ""
    params: list[Param] = field(default_factory=list)
    return_type: TypeExpr | None = None
    body: Block | None = None
    type_params: list[str] = field(default_factory=list)


@dataclass
class EntityDecl(Declaration):
    name: str = ""
    type_annotation: TypeExpr | None = None


@dataclass
class RuleDecl(Declaration):
    name: str = ""
    modality: str | None = None
    body: Expr | None = None
    antecedent: Expr | None = None
    otherwise: Expr | None = None
    strength: str = "defeasible"
    anonymous: bool = False


@dataclass
class PriorityDecl(Declaration):
    chain: list[str] = field(default_factory=list)


@dataclass
class FactDecl(Declaration):
    target: str | None = None
    value: Expr | None = None


@dataclass
class Module(Node):
    name: str = ""
    annotations: list[str] = field(default_factory=list)
    imports: list[ImportDecl] = field(default_factory=list)
    opens: list[OpenDecl] = field(default_factory=list)
    declarations: list[Declaration] = field(default_factory=list)


def node_to_dict(value: Any) -> Any:
    """Convert dataclass AST nodes to JSON-serialisable dictionaries."""
    if is_dataclass(value):
        data = {"kind": type(value).__name__}
        for f in fields(value):
            data[f.name] = node_to_dict(getattr(value, f.name))
        return data
    if isinstance(value, list):
        return [node_to_dict(item) for item in value]
    if isinstance(value, tuple):
        return [node_to_dict(item) for item in value]
    if isinstance(value, dict):
        return {str(k): node_to_dict(v) for k, v in value.items()}
    return value


def declaration_name(decl: Declaration) -> str | None:
    if isinstance(decl, (TypeDecl, ValueDecl, FuncDecl, EntityDecl, RuleDecl)):
        return decl.name
    return None
