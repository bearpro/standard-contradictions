from __future__ import annotations

from collections.abc import Iterable
from dataclasses import dataclass
from itertools import product

from mdl.lib.model import (
    Always,
    And,
    Bottom,
    Eventually,
    LtlfFormula,
    Module,
    Next,
    Not,
    Or,
    Priority,
    Proposition,
    Rule,
    Top,
    Until,
)


Trace = tuple[frozenset[str], ...]


@dataclass(frozen=True)
class SolveResult:
    is_consistent: bool
    horizon: int
    trace: Trace | None
    winning_rules: tuple[Rule, ...]

    def __bool__(self) -> bool:
        return self.is_consistent


def solve(*documents: Module, horizon: int = 1) -> SolveResult:
    if horizon < 1:
        raise ValueError("horizon must be greater than or equal to 1.")

    rules, priorities = _collect_norms(documents)
    atoms = sorted(_collect_atoms_from_rules(rules, priorities))

    for trace in _enumerate_traces(atoms, horizon):
        winning_rules = _winning_rules(rules, priorities, trace)
        if _trace_satisfies_requirements(winning_rules, trace):
            return SolveResult(
                is_consistent=True,
                horizon=horizon,
                trace=trace,
                winning_rules=tuple(winning_rules),
            )

    return SolveResult(
        is_consistent=False,
        horizon=horizon,
        trace=None,
        winning_rules=(),
    )


def evaluate(formula: LtlfFormula, trace: Trace, time: int = 0) -> bool:
    if not 0 <= time < len(trace):
        raise ValueError("time must point to an existing trace step.")

    if isinstance(formula, Top):
        return True
    if isinstance(formula, Bottom):
        return False
    if isinstance(formula, Proposition):
        return formula.name in trace[time]
    if isinstance(formula, Not):
        return not evaluate(formula.operand, trace, time)
    if isinstance(formula, And):
        return all(evaluate(operand, trace, time) for operand in formula.operands)
    if isinstance(formula, Or):
        return any(evaluate(operand, trace, time) for operand in formula.operands)
    if isinstance(formula, Next):
        return time < len(trace) - 1 and evaluate(formula.operand, trace, time + 1)
    if isinstance(formula, Until):
        for witness_time in range(time, len(trace)):
            if evaluate(formula.right, trace, witness_time) and all(
                evaluate(formula.left, trace, prefix_time)
                for prefix_time in range(time, witness_time)
            ):
                return True
        return False
    if isinstance(formula, Eventually):
        return any(
            evaluate(formula.operand, trace, future_time)
            for future_time in range(time, len(trace))
        )
    if isinstance(formula, Always):
        return all(
            evaluate(formula.operand, trace, future_time)
            for future_time in range(time, len(trace))
        )

    raise TypeError(f"Unsupported LTLf formula: {formula!r}")


def _collect_norms(documents: Iterable[Module]) -> tuple[list[Rule], list[Priority]]:
    rules: list[Rule] = []
    priorities: list[Priority] = []

    for document in documents:
        for value in document.objects.values():
            if isinstance(value, Rule):
                rules.append(value)
            elif isinstance(value, Priority):
                priorities.append(value)

    return rules, priorities


def _collect_atoms_from_rules(
    rules: Iterable[Rule],
    priorities: Iterable[Priority],
) -> set[str]:
    atoms: set[str] = set()

    for rule in rules:
        if rule.antecedent is not None:
            atoms.update(_collect_atoms(rule.antecedent))
        atoms.update(_collect_atoms(rule.consequent))

    for priority in priorities:
        if priority.condition is not None:
            atoms.update(_collect_atoms(priority.condition))

    return atoms


def _collect_atoms(formula: LtlfFormula) -> set[str]:
    if isinstance(formula, Proposition):
        return {formula.name}
    if isinstance(formula, (Top, Bottom)):
        return set()
    if isinstance(formula, Not):
        return _collect_atoms(formula.operand)
    if isinstance(formula, (Next, Eventually, Always)):
        return _collect_atoms(formula.operand)
    if isinstance(formula, (And, Or)):
        atoms: set[str] = set()
        for operand in formula.operands:
            atoms.update(_collect_atoms(operand))
        return atoms
    if isinstance(formula, Until):
        return _collect_atoms(formula.left) | _collect_atoms(formula.right)

    raise TypeError(f"Unsupported LTLf formula: {formula!r}")


def _enumerate_traces(atoms: list[str], horizon: int) -> Iterable[Trace]:
    names = [(atom, time) for time in range(horizon) for atom in atoms]
    for values in product([False, True], repeat=len(names)):
        steps = [set[str]() for _ in range(horizon)]
        for (atom, time), value in zip(names, values):
            if value:
                steps[time].add(atom)
        yield tuple(frozenset(step) for step in steps)


def _winning_rules(
    rules: list[Rule],
    priorities: list[Priority],
    trace: Trace,
) -> list[Rule]:
    winners: list[Rule] = []

    for rule in rules:
        if not _is_applicable(rule, trace):
            continue
        if _is_defeated(rule, priorities, trace):
            continue
        winners.append(rule)

    return winners


def _is_applicable(rule: Rule, trace: Trace) -> bool:
    return rule.antecedent is None or evaluate(rule.antecedent, trace)


def _is_defeated(rule: Rule, priorities: Iterable[Priority], trace: Trace) -> bool:
    if _normalize_strength(rule.strength) == "strict":
        return False

    for priority in priorities:
        if priority.lower is not rule:
            continue
        if not _is_applicable(priority.higher, trace):
            continue
        condition = priority.condition or Top()
        if evaluate(condition, trace):
            return True

    return False


def _trace_satisfies_requirements(rules: Iterable[Rule], trace: Trace) -> bool:
    return all(evaluate(_requirement(rule), trace) for rule in rules)


def _requirement(rule: Rule) -> LtlfFormula:
    strength = _normalize_strength(rule.strength)
    if strength == "defeater":
        return Top()

    kind = _normalize_kind(rule.kind)
    if kind == "obligation":
        return rule.consequent
    if kind == "forbidden":
        return Not(rule.consequent)
    if kind == "permission":
        return Top()

    raise ValueError(f"Unsupported deontic rule kind: {rule.kind!r}")


def _normalize_kind(kind: str) -> str:
    normalized = kind.strip().lower()
    if normalized in {"o", "obligation", "obligated", "must"}:
        return "obligation"
    if normalized in {"f", "forbidden", "prohibition", "prohibited", "must_not"}:
        return "forbidden"
    if normalized in {"p", "permission", "permitted", "may"}:
        return "permission"
    return normalized


def _normalize_strength(strength: str) -> str:
    normalized = strength.strip().lower()
    if normalized in {"strict", "->"}:
        return "strict"
    if normalized in {"defeasible", "=>"}:
        return "defeasible"
    if normalized in {"defeater", "~>"}:
        return "defeater"
    return normalized


__all__ = ["SolveResult", "Trace", "evaluate", "solve"]
