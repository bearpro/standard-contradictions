from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass
from fractions import Fraction
from typing import overload


type RuntimeValue = (
    bool
    | int
    | Fraction
    | str
    | ProductInstance
    | VariantInstance
)


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


@dataclass(frozen=True, init=False)
class TypeRef(MdlType):
    target: MdlType | None

    def __init__(self, name: str):
        super().__init__(name)
        object.__setattr__(self, "target", None)

    def bind(self, target: MdlType) -> None:
        if target.name != self.name:
            raise ValueError("TypeRef can only bind to a type with the same name.")
        object.__setattr__(self, "target", target)


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
class StringCharAt(Term):
    value: Term
    index: Term

    def __init__(self, value: Term, index: Term, source: str = ""):
        if value.type != STRING:
            raise TypeError("CharAt requires a string term.")
        if index.type != INT:
            raise TypeError("CharAt index must be an int term.")
        super().__init__(STRING, source)
        object.__setattr__(self, "value", value)
        object.__setattr__(self, "index", index)


@dataclass(frozen=True, init=False)
class ProductConstruct(Term):
    type: ProductType
    fields: tuple[tuple[str, Term], ...]

    def __init__(
        self,
        type: ProductType,
        fields: Mapping[str, Term],
        source: str = "",
    ):
        expected = {name: _resolve_type(field_type) for name, field_type in type.fields}
        actual = set(fields)
        if actual != set(expected):
            raise ValueError("Product fields must exactly match the product type.")
        for field_name, field_value in fields.items():
            if not _types_compatible(field_value.type, expected[field_name]):
                raise TypeError(f"Product field {field_name!r} has incompatible type.")
        super().__init__(type, source)
        object.__setattr__(self, "fields", tuple(fields.items()))


@dataclass(frozen=True, init=False)
class VariantConstruct(Term):
    type: SumType
    variant: str
    payload: Term | None

    def __init__(
        self,
        type: SumType,
        variant: str,
        payload: Term | None = None,
        source: str = "",
    ):
        payload_type = type.payload_type(variant)
        if payload_type is None and payload is not None:
            raise TypeError("Variant does not accept a payload.")
        if payload_type is not None and payload is None:
            raise TypeError("Variant requires a payload.")
        if payload_type is not None and payload is not None:
            if not _types_compatible(payload.type, _resolve_type(payload_type)):
                raise TypeError("Variant payload has incompatible type.")
        super().__init__(type, source)
        object.__setattr__(self, "variant", variant)
        object.__setattr__(self, "payload", payload)


@dataclass(frozen=True, init=False)
class FieldAccess(Term):
    value: Term
    field_name: str

    def __init__(self, value: Term, field_name: str, source: str = ""):
        value_type = _resolve_type(value.type)
        if not isinstance(value_type, ProductType):
            raise TypeError("Field access requires a product-typed term.")
        super().__init__(_resolve_type(value_type.field_type(field_name)), source)
        object.__setattr__(self, "value", value)
        object.__setattr__(self, "field_name", field_name)


@dataclass(frozen=True, init=False)
class VariantPayload(Term):
    value: Term
    variant: str

    def __init__(self, value: Term, variant: str, source: str = ""):
        value_type = _resolve_type(value.type)
        if not isinstance(value_type, SumType):
            raise TypeError("VariantPayload requires a sum-typed term.")
        payload_type = value_type.payload_type(variant)
        if payload_type is None:
            raise TypeError("Variant has no payload.")
        super().__init__(_resolve_type(payload_type), source)
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
        if not _types_compatible(then_branch.type, else_branch.type):
            raise TypeError("If expression branches must have the same type.")
        super().__init__(_resolve_type(then_branch.type), source)
        object.__setattr__(self, "condition", condition)
        object.__setattr__(self, "then_branch", then_branch)
        object.__setattr__(self, "else_branch", else_branch)


@dataclass(frozen=True, init=False)
class CaseExpr(Term):
    value: Term
    cases: tuple[tuple[str, Term], ...]

    def __init__(self, value: Term, cases: Mapping[str, Term], source: str = ""):
        value_type = _resolve_type(value.type)
        if not isinstance(value_type, SumType):
            raise TypeError("Case requires a sum-typed term.")
        _validate_case_coverage(value_type, cases)
        branch_types = {_resolve_type(branch.type).name for branch in cases.values()}
        if len(branch_types) != 1:
            raise TypeError("Case expression branches must have the same type.")
        super().__init__(_resolve_type(next(iter(cases.values())).type), source)
        object.__setattr__(self, "value", value)
        object.__setattr__(self, "cases", tuple(cases.items()))


@dataclass(frozen=True)
class FunctionParameter:
    name: str
    type: MdlType


@dataclass(frozen=True, init=False)
class Function(_MdlObject):
    name: str
    parameters: tuple[FunctionParameter, ...]
    return_type: MdlType
    body: object

    def __init__(
        self,
        name: str,
        parameters: Mapping[str, MdlType],
        return_type: MdlType,
        body: object,
        source: str = "",
    ):
        if not name:
            raise ValueError("Function name must not be empty.")
        if not parameters:
            raise ValueError("Function requires at least one parameter.")
        super().__init__(source)
        object.__setattr__(self, "name", name)
        object.__setattr__(
            self,
            "parameters",
            tuple(FunctionParameter(param_name, type) for param_name, type in parameters.items()),
        )
        object.__setattr__(self, "return_type", return_type)
        object.__setattr__(self, "body", body)

    def __call__(self, *arguments: Term | bool | int | Fraction | str) -> FunctionCall:
        return FunctionCall(self, tuple(_term(argument) for argument in arguments))

    def body_term(self) -> Term:
        if isinstance(self.body, Term):
            body = self.body
        elif callable(self.body):
            params = [Variable(param.name, param.type) for param in self.parameters]
            body = self.body(*params)
        else:
            raise TypeError("Function body must be a term or a callable returning a term.")
        if not isinstance(body, Term):
            raise TypeError("Function body must return a term.")
        if not _types_compatible(body.type, self.return_type):
            raise TypeError("Function body type does not match declared return type.")
        return body


@dataclass(frozen=True, init=False)
class FunctionCall(Term):
    function: Function
    arguments: tuple[Term, ...]

    def __init__(self, function: Function, arguments: tuple[Term, ...], source: str = ""):
        if len(arguments) != len(function.parameters):
            raise TypeError("Function call argument count does not match parameters.")
        for argument, parameter in zip(arguments, function.parameters):
            if not _types_compatible(argument.type, parameter.type):
                raise TypeError(
                    f"Function argument {parameter.name!r} has incompatible type."
                )
        super().__init__(function.return_type, source)
        object.__setattr__(self, "function", function)
        object.__setattr__(self, "arguments", arguments)

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
    Unbound Boolean variable with name.
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
        value_type = _resolve_type(value.type)
        if not isinstance(value_type, SumType):
            raise TypeError("IsVariant requires a sum-typed term.")
        _ = value_type.payload_type(variant)
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
        value_type = _resolve_type(value.type)
        if not isinstance(value_type, SumType):
            raise TypeError("Case requires a sum-typed term.")
        _validate_case_coverage(value_type, cases)
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


def Len(value: Term) -> StringLength:
    return StringLength(value)


def CharAt(value: Term, index: Term | int) -> StringCharAt:
    return StringCharAt(value, _term(index))


def Product(
    type: ProductType,
    fields: Mapping[str, Term | bool | int | Fraction | str],
) -> ProductConstruct:
    return ProductConstruct(type, {name: _term(value) for name, value in fields.items()})


def Variant(
    type: SumType,
    variant: str,
    payload: Term | bool | int | Fraction | str | None = None,
) -> VariantConstruct:
    return VariantConstruct(type, variant, None if payload is None else _term(payload))


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


def _resolve_type(type: MdlType) -> MdlType:
    if isinstance(type, TypeRef):
        if type.target is None:
            raise TypeError(f"Unbound recursive type reference: {type.name!r}.")
        return type.target
    return type


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
    return _resolve_type(type).name in {INT.name, RAT.name}


def _types_compatible(left: MdlType, right: MdlType) -> bool:
    left = _resolve_type(left)
    right = _resolve_type(right)
    return left.name == right.name or (_is_numeric(left) and _is_numeric(right))


def _validate_case_coverage(type: SumType, cases: Mapping[str, object]) -> None:
    expected = {name for name, _ in type.variants}
    actual = set(cases)
    if actual != expected:
        raise ValueError("Case branches must exactly cover sum type variants.")


def validate_module_objects(objects: Mapping[str, object]) -> None:
    value_env: dict[str, MdlType] = {}
    for value in objects.values():
        if isinstance(value, Variable):
            existing = value_env.get(value.name)
            if existing is not None and not _types_compatible(existing, value.type):
                raise TypeError(f"Variable {value.name!r} is declared with multiple types.")
            value_env[value.name] = value.type

    for value in objects.values():
        _validate_module_object(value, value_env, frozenset())


def _validate_module_object(
    value: object,
    value_env: Mapping[str, MdlType],
    function_stack: frozenset[str],
) -> None:
    if isinstance(value, MdlType):
        _validate_type(value, frozenset())
    elif isinstance(value, Variable):
        _validate_type(value.type, frozenset())
    elif isinstance(value, Function):
        _validate_function(value, value_env, function_stack)
    elif isinstance(value, Rule):
        if value.antecedent is not None:
            _validate_formula(value.antecedent, value_env, function_stack)
        _validate_formula(value.consequent, value_env, function_stack)
    elif isinstance(value, Priority):
        _validate_module_object(value.higher, value_env, function_stack)
        _validate_module_object(value.lower, value_env, function_stack)
        if value.condition is not None:
            _validate_formula(value.condition, value_env, function_stack)
    elif isinstance(value, LtlfFormula):
        _validate_formula(value, value_env, function_stack)
    elif isinstance(value, Term):
        _validate_term(value, value_env, function_stack)


def _validate_type(type: MdlType, stack: frozenset[str]) -> None:
    if isinstance(type, TypeRef):
        target = _resolve_type(type)
        if type.name in stack:
            return
        _validate_type(target, stack | {type.name})
    elif isinstance(type, ProductType):
        if type.name in stack:
            return
        for _, field_type in type.fields:
            _validate_type(field_type, stack | {type.name})
    elif isinstance(type, SumType):
        if type.name in stack:
            return
        for _, payload_type in type.variants:
            if payload_type is not None:
                _validate_type(payload_type, stack | {type.name})


def _validate_function(
    function: Function,
    value_env: Mapping[str, MdlType],
    function_stack: frozenset[str],
) -> None:
    if function.name in function_stack:
        return

    for parameter in function.parameters:
        _validate_type(parameter.type, frozenset())
    _validate_type(function.return_type, frozenset())

    body = function.body_term()
    if not _types_compatible(body.type, function.return_type):
        raise TypeError(f"Function {function.name!r} body type does not match return type.")

    function_env = dict(value_env)
    for parameter in function.parameters:
        existing = function_env.get(parameter.name)
        if existing is not None and not _types_compatible(existing, parameter.type):
            raise TypeError(
                f"Function parameter {parameter.name!r} conflicts with a module variable."
            )
        function_env[parameter.name] = parameter.type
    _validate_term(body, function_env, function_stack | {function.name})


def _validate_formula(
    formula: LtlfFormula,
    value_env: Mapping[str, MdlType],
    function_stack: frozenset[str],
) -> None:
    if isinstance(formula, (Top, Bottom, Proposition)):
        return
    if isinstance(formula, Relation):
        _validate_term(formula.left, value_env, function_stack)
        _validate_term(formula.right, value_env, function_stack)
        if formula.op in {"lt", "le", "gt", "ge"} and not (
            _is_numeric(formula.left.type) and _is_numeric(formula.right.type)
        ):
            raise TypeError("Ordering relations require int or rat operands.")
        if formula.op in {"eq", "ne"} and not _types_compatible(formula.left.type, formula.right.type):
            raise TypeError("Equality operands have incompatible types.")
    elif isinstance(formula, Truth):
        _validate_term(formula.value, value_env, function_stack)
        if not _types_compatible(formula.value.type, BOOL):
            raise TypeError("Truth requires a bool term.")
    elif isinstance(formula, IsVariant):
        _validate_term(formula.value, value_env, function_stack)
        value_type = _resolve_type(formula.value.type)
        if not isinstance(value_type, SumType):
            raise TypeError("IsVariant requires a sum-typed term.")
        _ = value_type.payload_type(formula.variant)
    elif isinstance(formula, Not):
        _validate_formula(formula.operand, value_env, function_stack)
    elif isinstance(formula, (And, Or)):
        for operand in formula.operands:
            _validate_formula(operand, value_env, function_stack)
    elif isinstance(formula, (Next, Eventually, Always)):
        _validate_formula(formula.operand, value_env, function_stack)
    elif isinstance(formula, Until):
        _validate_formula(formula.left, value_env, function_stack)
        _validate_formula(formula.right, value_env, function_stack)
    elif isinstance(formula, IfFormula):
        _validate_formula(formula.condition, value_env, function_stack)
        _validate_formula(formula.then_branch, value_env, function_stack)
        _validate_formula(formula.else_branch, value_env, function_stack)
    elif isinstance(formula, CaseFormula):
        _validate_term(formula.value, value_env, function_stack)
        value_type = _resolve_type(formula.value.type)
        if not isinstance(value_type, SumType):
            raise TypeError("Case requires a sum-typed term.")
        _validate_case_coverage(value_type, dict(formula.cases))
        for _, branch in formula.cases:
            _validate_formula(branch, value_env, function_stack)
    else:
        raise TypeError(f"Unsupported LTLf formula: {formula!r}")


def _validate_term(
    term: Term,
    value_env: Mapping[str, MdlType],
    function_stack: frozenset[str],
) -> None:
    if isinstance(term, Variable):
        expected_type = value_env.get(term.name)
        if expected_type is None:
            raise NameError(f"Variable {term.name!r} is not declared in the module.")
        if not _types_compatible(term.type, expected_type):
            raise TypeError(f"Variable {term.name!r} is used with an incompatible type.")
    elif isinstance(term, Const):
        _validate_type(term.type, frozenset())
    elif isinstance(term, Arithmetic):
        _validate_term(term.left, value_env, function_stack)
        _validate_term(term.right, value_env, function_stack)
        if not (_is_numeric(term.left.type) and _is_numeric(term.right.type)):
            raise TypeError("Arithmetic terms require int or rat operands.")
    elif isinstance(term, StringLength):
        _validate_term(term.value, value_env, function_stack)
        if not _types_compatible(term.value.type, STRING):
            raise TypeError("Len requires a string term.")
    elif isinstance(term, StringCharAt):
        _validate_term(term.value, value_env, function_stack)
        _validate_term(term.index, value_env, function_stack)
        if not _types_compatible(term.value.type, STRING):
            raise TypeError("CharAt requires a string term.")
        if not _types_compatible(term.index.type, INT):
            raise TypeError("CharAt index must be an int term.")
    elif isinstance(term, ProductConstruct):
        _validate_type(term.type, frozenset())
        expected = {name: _resolve_type(field_type) for name, field_type in term.type.fields}
        actual = {name for name, _ in term.fields}
        if actual != set(expected):
            raise ValueError("Product fields must exactly match the product type.")
        for field_name, field_value in term.fields:
            _validate_term(field_value, value_env, function_stack)
            if not _types_compatible(field_value.type, expected[field_name]):
                raise TypeError(f"Product field {field_name!r} has incompatible type.")
    elif isinstance(term, VariantConstruct):
        _validate_type(term.type, frozenset())
        payload_type = term.type.payload_type(term.variant)
        if payload_type is None and term.payload is not None:
            raise TypeError("Variant does not accept a payload.")
        if payload_type is not None and term.payload is None:
            raise TypeError("Variant requires a payload.")
        if payload_type is not None and term.payload is not None:
            _validate_term(term.payload, value_env, function_stack)
            if not _types_compatible(term.payload.type, payload_type):
                raise TypeError("Variant payload has incompatible type.")
    elif isinstance(term, FieldAccess):
        _validate_term(term.value, value_env, function_stack)
        value_type = _resolve_type(term.value.type)
        if not isinstance(value_type, ProductType):
            raise TypeError("Field access requires a product-typed term.")
        _ = value_type.field_type(term.field_name)
    elif isinstance(term, VariantPayload):
        _validate_term(term.value, value_env, function_stack)
        value_type = _resolve_type(term.value.type)
        if not isinstance(value_type, SumType):
            raise TypeError("VariantPayload requires a sum-typed term.")
        payload_type = value_type.payload_type(term.variant)
        if payload_type is None:
            raise TypeError("Variant has no payload.")
    elif isinstance(term, IfExpr):
        _validate_formula(term.condition, value_env, function_stack)
        _validate_term(term.then_branch, value_env, function_stack)
        _validate_term(term.else_branch, value_env, function_stack)
        if not _types_compatible(term.then_branch.type, term.else_branch.type):
            raise TypeError("If expression branches must have the same type.")
    elif isinstance(term, CaseExpr):
        _validate_term(term.value, value_env, function_stack)
        value_type = _resolve_type(term.value.type)
        if not isinstance(value_type, SumType):
            raise TypeError("Case requires a sum-typed term.")
        _validate_case_coverage(value_type, dict(term.cases))
        branch_type: MdlType | None = None
        for _, branch in term.cases:
            _validate_term(branch, value_env, function_stack)
            if branch_type is None:
                branch_type = branch.type
            elif not _types_compatible(branch_type, branch.type):
                raise TypeError("Case expression branches must have the same type.")
    elif isinstance(term, FunctionCall):
        for argument in term.arguments:
            _validate_term(argument, value_env, function_stack)
        _validate_function(function=term.function, value_env=value_env, function_stack=function_stack)
    else:
        raise TypeError(f"Unsupported term: {term!r}")


_MODULE_BUILDER_TOKEN = object()


@dataclass(frozen=True, init=False)
class Module:
    objects: Mapping[str, object]

    def __init__(
        self,
        objects: Mapping[str, object],
        *,
        _builder_token: object | None = None,
    ):
        if _builder_token is not _MODULE_BUILDER_TOKEN:
            raise TypeError("Module instances must be created through ModuleBuilder.build().")
        object.__setattr__(self, "objects", objects)

    @classmethod
    def _from_builder(cls, objects: Mapping[str, object]) -> Module:
        module_objects = dict(objects)
        validate_module_objects(module_objects)
        return cls(module_objects, _builder_token=_MODULE_BUILDER_TOKEN)

    def __getattr__(self, name: str) -> object:
        try:
            return self.objects[name]
        except KeyError:
            raise AttributeError(name) from None


__all__ = [
    "Always",
    "And",
    "BOOL",
    "Bottom",
    "Bool",
    "Case",
    "CaseExpr",
    "CaseFormula",
    "CharAt",
    "Const",
    "DefeasibleRule",
    "Defeater",
    "Eq",
    "Eventually",
    "FieldAccess",
    "Function",
    "FunctionCall",
    "FunctionParameter",
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
    "Product",
    "ProductConstruct",
    "Proposition",
    "ProductInstance",
    "ProductType",
    "RAT",
    "Rat",
    "Relation",
    "RuntimeValue",
    "Rule",
    "STRING",
    "ScalarType",
    "StrictRule",
    "String",
    "StringCharAt",
    "StringLength",
    "SumType",
    "Term",
    "Top",
    "Truth",
    "TypeRef",
    "Until",
    "Var",
    "Variable",
    "Variant",
    "VariantConstruct",
    "VariantInstance",
    "VariantPayload",
]
