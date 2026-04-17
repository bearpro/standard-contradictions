from dataclasses import dataclass
from typing import Any, Optional


@dataclass(frozen=True)
class _MdlObject:
    """
    Any source code object. Mostly used to trace sources
    """
    source: str

@dataclass(frozen=True)
class _LtlfFormula(_MdlObject):
    pass

@dataclass(frozen=True)
class Proposition(_LtlfFormula):
    """
    Boolean variable with name
    TODO Proposition may also represent some programmable condition e.x. `x > 5`
    """
    name: str

@dataclass(frozen=True)
class And(_LtlfFormula):
    pass

@dataclass(frozen=True)
class Not(_LtlfFormula):
    pass

@dataclass(frozen=True)
class Or(_LtlfFormula):
    pass

@dataclass(frozen=True)
class Next(_LtlfFormula):
    pass

@dataclass(frozen=True)
class Until(_LtlfFormula):
    pass

@dataclass(frozen=True)
class Rule(_MdlObject):
    """
    Represents deontic rule.
    """
    kind: str
    antecedent: Optional[_LtlfFormula]   # φ_i
    consequent: _LtlfFormula   # ψ_i

@dataclass(frozen=True)
class Module:
    objects: _MdlObject
