from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass
from fractions import Fraction
from typing import overload


type RuntimeValue = bool | int | Fraction | str | ProductInstance | VariantInstance


@dataclass(frozen=True)
class _MdlObject:
    """
    Any source code object. Mostly used to trace sources
    """
    source: str = ""


@dataclass(frozen=True)
class MdlType:
    name: str


@dataclass(frozen=True)
class ScalarType(MdlType):
    pass


BOOL = ScalarType("bool")
INT = ScalarType("int")
RAT = ScalarType("rat")
STRING = ScalarType("string")


@dataclass(frozen=True, init=False)
class ProductType(MdlType):
    fields: tuple[tuple[str, MdlType], ...]

    def __init__(self, name: str, fields: Mapping[str, MdlType]):
        if not fields:
            raise ValueError("ProductType requires at least one field.")
        super().__init__(name)
        object.__setattr__(self, "fields", tuple(fields.items()))

    def field_type(self, name: str) -> MdlType:
        for field_name, field_type in self.fields:
            if field_name == name:
                return field_type
        raise KeyError(name)


@dataclass(frozen=True, init=False)
class SumType(MdlType):
    variants: tuple[tuple[str, MdlType | None], ...]

    def __init__(self, name: str, variants: Mapping[str, MdlType | None]):
        if not variants:
            raise ValueError("SumType requires at least one variant.")
        super().__init__(name)
        object.__setattr__(self, "variants", tuple(variants.items()))

    def payload_type(self, variant: str) -> MdlType | None:
        for variant_name, payload_type in self.variants:
            if variant_name == variant:
                return payload_type
        raise KeyError(variant)


@dataclass(frozen=True)
class ProductInstance:
    type_name: str
    fields: tuple[tuple[str, RuntimeValue], ...]

    def field_value(self, name: str) -> RuntimeValue:
        for field_name, value in self.fields:
            if field_name == name:
                return value
        raise KeyError(name)


@dataclass(frozen=True)
class VariantInstance:
    type_name: str
    variant: str
    payload: RuntimeValue | None = None


@dataclass(frozen=True, init=False)
class Term(_MdlObject):
    type: MdlType

    def __init__(self, type: MdlType, source: str = ""):
        super().__init__(source)
        object.__setattr__(self, "type", type)

    def __add__(self, other: Term | bool | int | Fraction | str) -> Arithmetic:
        return Arithmetic("add", self, _term(other))

    def __sub__(self, other: Term | bool | int | Fraction | str) -> Arithmetic:
        return Arithmetic("sub", self, _term(other))

    def __mul__(self, other: Term | bool | int | Fraction | str) -> Arithmetic:
        return Arithmetic("mul", self, _term(other))

    def __truediv__(self, other: Term | bool | int | Fraction | str) -> Arithmetic:
        return Arithmetic("div", self, _term(other))

    def __lt__(self, other: Term | bool | int | Fraction | str) -> Relation:
        return Relation("lt", self, _term(other))

    def __le__(self, other: Term | bool | int | Fraction | str) -> Relation:
        return Relation("le", self, _term(other))

    def __gt__(self, other: Term | bool | int | Fraction | str) -> Relation:
        return Relation("gt", self, _term(other))

    def __ge__(self, other: Term | bool | int | Fraction | str) -> Relation:
        return Relation("ge", self, _term(other))

    def eq(self, other: Term | bool | int | Fraction | str) -> Relation:
        return Relation("eq", self, _term(other))

    def ne(self, other: Term | bool | int | Fraction | str) -> Relation:
        return Relation("ne", self, _term(other))

    def field(self, name: str) -> FieldAccess:
        return FieldAccess(self, name)


@dataclass(frozen=True, init=False)
class Variable(Term):
    name: str

    def __init__(self, name: str, type: MdlType, source: str = ""):
        if not name:
            raise ValueError("Variable name must not be empty.")
        super().__init__(type, source)
        object.__setattr__(self, "name", name)


@dataclass(frozen=True, init=False)
class Const(Term):
    value: RuntimeValue

    def __init__(self, value: RuntimeValue, type: MdlType, source: str = ""):
        super().__init__(type, source)
        object.__setattr__(self, "value", value)


@dataclass(frozen=True, init=False)
class Arithmetic(Term):
    op: str
    left: Term
    right: Term

    def __init__(self, op: str, left: Term, right: Term, source: str = ""):
        if not (_is_numeric(left.type) and _is_numeric(right.type)):
            raise TypeError("Arithmetic terms require int or rat operands.")
        result_type = RAT if op == "div" or RAT in {left.type, right.type} else INT
        super().__init__(result_type, source)
        object.__setattr__(self, "op", op)
        object.__setattr__(self, "left", left)
        object.__setattr__(self, "right", right)


@dataclass(frozen=True, init=False)
class StringLength(Term):
    value: Term

    def __init__(self, value: Term, source: str = ""):
        if value.type != STRING:
            raise TypeError("Len requires a string term.")
        super().__init__(INT, source)
        object.__setattr__(self, "value", value)


@dataclass(frozen=True, init=False)
class FieldAccess(Term):
    value: Term
    field_name: str

    def __init__(self, value: Term, field_name: str, source: str = ""):
        if not isinstance(value.type, ProductType):
            raise TypeError("Field access requires a product-typed term.")
        super().__init__(value.type.field_type(field_name), source)
        object.__setattr__(self, "value", value)
        object.__setattr__(self, "field_name", field_name)


@dataclass(frozen=True, init=False)
class ProductValue(Term):
    fields: tuple[tuple[str, Term], ...]

    def __init__(self, type: ProductType, fields: Mapping[str, Term], source: str = ""):
        missing = {name for name, _ in type.fields} - set(fields)
        extra = set(fields) - {name for name, _ in type.fields}
        if missing or extra:
            raise ValueError("Product value fields must exactly match product type fields.")
        for name, field_type in type.fields:
            if fields[name].type != field_type:
                raise TypeError(f"Field {name!r} has incompatible type.")
        super().__init__(type, source)
        object.__setattr__(self, "fields", tuple(fields.items()))


@dataclass(frozen=True, init=False)
class VariantValue(Term):
    variant: str
    payload: Term | None

    def __init__(
        self,
        type: SumType,
        variant: str,
        payload: Term | None = None,
        source: str = "",
    ):
        expected = type.payload_type(variant)
        if expected is None and payload is not None:
            raise TypeError("Payload is not allowed for this variant.")
        if expected is not None and (payload is None or payload.type != expected):
            raise TypeError("Variant payload has incompatible type.")
        super().__init__(type, source)
        object.__setattr__(self, "variant", variant)
        object.__setattr__(self, "payload", payload)


@dataclass(frozen=True, init=False)
class VariantPayload(Term):
    value: Term
    variant: str

    def __init__(self, value: Term, variant: str, source: str = ""):
        if not isinstance(value.type, SumType):
            raise TypeError("VariantPayload requires a sum-typed term.")
        payload_type = value.type.payload_type(variant)
        if payload_type is None:
            raise TypeError("Variant has no payload.")
        super().__init__(payload_type, source)
        object.__setattr__(self, "value", value)
        object.__setattr__(self, "variant", variant)


@dataclass(frozen=True, init=False)
class IfExpr(Term):
    condition: LtlfFormula
    then_branch: Term
    else_branch: Term

    def __init__(
        self,
        condition: LtlfFormula,
        then_branch: Term,
        else_branch: Term,
        source: str = "",
    ):
        if then_branch.type != else_branch.type:
            raise TypeError("If expression branches must have the same type.")
        super().__init__(then_branch.type, source)
        object.__setattr__(self, "condition", condition)
        object.__setattr__(self, "then_branch", then_branch)
        object.__setattr__(self, "else_branch", else_branch)


@dataclass(frozen=True, init=False)
class CaseExpr(Term):
    value: Term
    cases: tuple[tuple[str, Term], ...]

    def __init__(self, value: Term, cases: Mapping[str, Term], source: str = ""):
        if not isinstance(value.type, SumType):
            raise TypeError("Case requires a sum-typed term.")
        _validate_case_coverage(value.type, cases)
        branch_types = {branch.type for branch in cases.values()}
        if len(branch_types) != 1:
            raise TypeError("Case expression branches must have the same type.")
        super().__init__(next(iter(branch_types)), source)
        object.__setattr__(self, "value", value)
        object.__setattr__(self, "cases", tuple(cases.items()))

@dataclass(frozen=True)
class LtlfFormula(_MdlObject):
    def __and__(self, other: LtlfFormula) -> And:
        return And(self, other)

    def __or__(self, other: LtlfFormula) -> Or:
        return Or(self, other)

    def __invert__(self) -> Not:
        return Not(self)

@dataclass(frozen=True, init=False)
class Proposition(LtlfFormula):
    """
    Boolean variable with name
    TODO Proposition may also represent some programmable condition e.x. `x > 5`
    """
    name: str

    def __init__(self, source_or_name: str, name: str | None = None):
        source = "" if name is None else source_or_name
        actual_name = source_or_name if name is None else name
        if not actual_name:
            raise ValueError("Proposition name must not be empty.")
        super().__init__(source)
        object.__setattr__(self, "name", actual_name)

@dataclass(frozen=True, init=False)
class And(LtlfFormula):
    operands: tuple[LtlfFormula, ...]

    def __init__(self, *operands: LtlfFormula, source: str = ""):
        if len(operands) < 2:
            raise ValueError("And requires at least two operands.")
        super().__init__(source)
        object.__setattr__(self, "operands", tuple(operands))

@dataclass(frozen=True, init=False)
class Not(LtlfFormula):
    operand: LtlfFormula

    def __init__(self, operand: LtlfFormula, source: str = ""):
        super().__init__(source)
        object.__setattr__(self, "operand", operand)

@dataclass(frozen=True, init=False)
class Or(LtlfFormula):
    operands: tuple[LtlfFormula, ...]

    def __init__(self, *operands: LtlfFormula, source: str = ""):
        if len(operands) < 2:
            raise ValueError("Or requires at least two operands.")
        super().__init__(source)
        object.__setattr__(self, "operands", tuple(operands))

@dataclass(frozen=True, init=False)
class Next(LtlfFormula):
    operand: LtlfFormula

    def __init__(self, operand: LtlfFormula, source: str = ""):
        super().__init__(source)
        object.__setattr__(self, "operand", operand)

@dataclass(frozen=True, init=False)
class Until(LtlfFormula):
    left: LtlfFormula
    right: LtlfFormula

    def __init__(self, left: LtlfFormula, right: LtlfFormula, source: str = ""):
        super().__init__(source)
        object.__setattr__(self, "left", left)
        object.__setattr__(self, "right", right)

@dataclass(frozen=True, init=False)
class Eventually(LtlfFormula):
    operand: LtlfFormula

    def __init__(self, operand: LtlfFormula, source: str = ""):
        super().__init__(source)
        object.__setattr__(self, "operand", operand)

@dataclass(frozen=True, init=False)
class Always(LtlfFormula):
    operand: LtlfFormula

    def __init__(self, operand: LtlfFormula, source: str = ""):
        super().__init__(source)
        object.__setattr__(self, "operand", operand)

@dataclass(frozen=True, init=False)
class Top(LtlfFormula):
    def __init__(self, source: str = ""):
        super().__init__(source)

@dataclass(frozen=True, init=False)
class Bottom(LtlfFormula):
    def __init__(self, source: str = ""):
        super().__init__(source)


@dataclass(frozen=True, init=False)
class Relation(LtlfFormula):
    op: str
    left: Term
    right: Term

    def __init__(self, op: str, left: Term, right: Term, source: str = ""):
        if op not in {"eq", "ne", "lt", "le", "gt", "ge"}:
            raise ValueError(f"Unsupported relation: {op!r}")
        if op in {"lt", "le", "gt", "ge"} and not (
            _is_numeric(left.type) and _is_numeric(right.type)
        ):
            raise TypeError("Ordering relations require int or rat operands.")
        if op in {"eq", "ne"} and not _types_compatible(left.type, right.type):
            raise TypeError("Equality operands have incompatible types.")
        super().__init__(source)
        object.__setattr__(self, "op", op)
        object.__setattr__(self, "left", left)
        object.__setattr__(self, "right", right)


@dataclass(frozen=True, init=False)
class Truth(LtlfFormula):
    value: Term

    def __init__(self, value: Term, source: str = ""):
        if value.type != BOOL:
            raise TypeError("Truth requires a bool term.")
        super().__init__(source)
        object.__setattr__(self, "value", value)


@dataclass(frozen=True, init=False)
class IsVariant(LtlfFormula):
    value: Term
    variant: str

    def __init__(self, value: Term, variant: str, source: str = ""):
        if not isinstance(value.type, SumType):
            raise TypeError("IsVariant requires a sum-typed term.")
        _ = value.type.payload_type(variant)
        super().__init__(source)
        object.__setattr__(self, "value", value)
        object.__setattr__(self, "variant", variant)


@dataclass(frozen=True, init=False)
class IfFormula(LtlfFormula):
    condition: LtlfFormula
    then_branch: LtlfFormula
    else_branch: LtlfFormula

    def __init__(
        self,
        condition: LtlfFormula,
        then_branch: LtlfFormula,
        else_branch: LtlfFormula,
        source: str = "",
    ):
        super().__init__(source)
        object.__setattr__(self, "condition", condition)
        object.__setattr__(self, "then_branch", then_branch)
        object.__setattr__(self, "else_branch", else_branch)


@dataclass(frozen=True, init=False)
class CaseFormula(LtlfFormula):
    value: Term
    cases: tuple[tuple[str, LtlfFormula], ...]

    def __init__(
        self,
        value: Term,
        cases: Mapping[str, LtlfFormula],
        source: str = "",
    ):
        if not isinstance(value.type, SumType):
            raise TypeError("Case requires a sum-typed term.")
        _validate_case_coverage(value.type, cases)
        super().__init__(source)
        object.__setattr__(self, "value", value)
        object.__setattr__(self, "cases", tuple(cases.items()))

@dataclass(frozen=True, init=False)
class Rule(_MdlObject):
    """
    Represents deontic rule.
    """
    kind: str
    antecedent: LtlfFormula | None   # φ_i
    consequent: LtlfFormula   # ψ_i
    strength: str

    def __init__(
        self,
        source: str,
        kind: str,
        antecedent: LtlfFormula | None,
        consequent: LtlfFormula,
        strength: str = "strict",
    ):
        super().__init__(source)
        object.__setattr__(self, "kind", kind)
        object.__setattr__(self, "antecedent", antecedent)
        object.__setattr__(self, "consequent", consequent)
        object.__setattr__(self, "strength", strength)

@dataclass(frozen=True, init=False)
class Priority(_MdlObject):
    """
    A defeasible priority edge: higher defeats lower when condition holds.
    """
    higher: Rule
    lower: Rule
    condition: LtlfFormula | None

    def __init__(
        self,
        higher: Rule,
        lower: Rule,
        condition: LtlfFormula | None = None,
        source: str = "",
    ):
        super().__init__(source)
        object.__setattr__(self, "higher", higher)
        object.__setattr__(self, "lower", lower)
        object.__setattr__(self, "condition", condition)


def StrictRule(
    source: str,
    kind: str,
    antecedent: LtlfFormula | None,
    consequent: LtlfFormula,
) -> Rule:
    return Rule(source, kind, antecedent, consequent, strength="strict")


def DefeasibleRule(
    source: str,
    kind: str,
    antecedent: LtlfFormula | None,
    consequent: LtlfFormula,
) -> Rule:
    return Rule(source, kind, antecedent, consequent, strength="defeasible")


def Defeater(
    source: str,
    kind: str,
    antecedent: LtlfFormula | None,
    consequent: LtlfFormula | None = None,
) -> Rule:
    return Rule(source, kind, antecedent, consequent or Top(), strength="defeater")


def Var(name: str, type: MdlType) -> Variable:
    return Variable(name, type)


def Bool(value: bool) -> Const:
    return Const(value, BOOL)


def Int(value: int) -> Const:
    return Const(value, INT)


def Rat(numerator: int | Fraction, denominator: int = 1) -> Const:
    if isinstance(numerator, Fraction) and denominator == 1:
        return Const(numerator, RAT)
    return Const(Fraction(numerator, denominator), RAT)


def String(value: str) -> Const:
    return Const(value, STRING)


def Product(type: ProductType, fields: Mapping[str, Term | bool | int | Fraction | str]) -> ProductValue:
    return ProductValue(type, {name: _term(value) for name, value in fields.items()})


def Variant(
    type: SumType,
    variant: str,
    payload: Term | bool | int | Fraction | str | None = None,
) -> VariantValue:
    return VariantValue(type, variant, None if payload is None else _term(payload))


def Len(value: Term) -> StringLength:
    return StringLength(value)


def Eq(left: Term | bool | int | Fraction | str, right: Term | bool | int | Fraction | str) -> Relation:
    return Relation("eq", _term(left), _term(right))


def Ne(left: Term | bool | int | Fraction | str, right: Term | bool | int | Fraction | str) -> Relation:
    return Relation("ne", _term(left), _term(right))


@overload
def If(condition: LtlfFormula, then_branch: Term, else_branch: Term) -> Term:
    ...


@overload
def If(
    condition: LtlfFormula,
    then_branch: LtlfFormula,
    else_branch: LtlfFormula,
) -> LtlfFormula:
    ...


def If(
    condition: LtlfFormula,
    then_branch: Term | LtlfFormula,
    else_branch: Term | LtlfFormula,
) -> Term | LtlfFormula:
    if isinstance(then_branch, Term) and isinstance(else_branch, Term):
        return IfExpr(condition, then_branch, else_branch)
    if isinstance(then_branch, LtlfFormula) and isinstance(else_branch, LtlfFormula):
        return IfFormula(condition, then_branch, else_branch)
    raise TypeError("If branches must both be terms or both be formulas.")


@overload
def Case(value: Term, cases: Mapping[str, Term]) -> Term:
    ...


@overload
def Case(value: Term, cases: Mapping[str, LtlfFormula]) -> LtlfFormula:
    ...


def Case(value: Term, cases: Mapping[str, Term | LtlfFormula]) -> Term | LtlfFormula:
    branches = list(cases.values())
    if not branches:
        raise ValueError("Case requires at least one branch.")
    if all(isinstance(branch, Term) for branch in branches):
        return CaseExpr(value, {name: branch for name, branch in cases.items() if isinstance(branch, Term)})
    if all(isinstance(branch, LtlfFormula) for branch in branches):
        return CaseFormula(
            value,
            {
                name: branch
                for name, branch in cases.items()
                if isinstance(branch, LtlfFormula)
            },
        )
    raise TypeError("Case branches must all be terms or all be formulas.")


def _term(value: Term | bool | int | Fraction | str) -> Term:
    if isinstance(value, Term):
        return value
    if isinstance(value, bool):
        return Bool(value)
    if isinstance(value, int):
        return Int(value)
    if isinstance(value, Fraction):
        return Rat(value)
    return String(value)


def _is_numeric(type: MdlType) -> bool:
    return type in {INT, RAT}


def _types_compatible(left: MdlType, right: MdlType) -> bool:
    return left == right or (_is_numeric(left) and _is_numeric(right))


def _validate_case_coverage(type: SumType, cases: Mapping[str, object]) -> None:
    expected = {name for name, _ in type.variants}
    actual = set(cases)
    if actual != expected:
        raise ValueError("Case branches must exactly cover sum type variants.")


@dataclass(frozen=True)
class Module:
    objects: Mapping[str, object]


__all__ = [
    "Always",
    "And",
    "BOOL",
    "Bottom",
    "Bool",
    "Case",
    "CaseExpr",
    "CaseFormula",
    "Const",
    "DefeasibleRule",
    "Defeater",
    "Eq",
    "Eventually",
    "FieldAccess",
    "If",
    "IfExpr",
    "IfFormula",
    "INT",
    "Int",
    "IsVariant",
    "Len",
    "LtlfFormula",
    "MdlType",
    "Module",
    "Ne",
    "Next",
    "Not",
    "Or",
    "Priority",
    "Proposition",
    "Product",
    "ProductInstance",
    "ProductType",
    "ProductValue",
    "RAT",
    "Rat",
    "Relation",
    "Rule",
    "STRING",
    "ScalarType",
    "StrictRule",
    "String",
    "StringLength",
    "SumType",
    "Term",
    "Top",
    "Truth",
    "Until",
    "Var",
    "Variable",
    "Variant",
    "VariantInstance",
    "VariantPayload",
    "VariantValue",
]
