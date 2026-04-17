from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass


@dataclass(frozen=True)
class _MdlObject:
    """
    Any source code object. Mostly used to trace sources
    """
    source: str = ""

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

@dataclass(frozen=True)
class Module:
    objects: Mapping[str, object]


__all__ = [
    "Always",
    "And",
    "Bottom",
    "DefeasibleRule",
    "Defeater",
    "Eventually",
    "LtlfFormula",
    "Module",
    "Next",
    "Not",
    "Or",
    "Priority",
    "Proposition",
    "Rule",
    "StrictRule",
    "Top",
    "Until",
]
