from __future__ import annotations

from collections.abc import Iterable
from dataclasses import dataclass
from fractions import Fraction
from itertools import product
from typing import override

from mdl.lib.model import (
    BOOL,
    INT,
    RAT,
    STRING,
    Always,
    And,
    Arithmetic,
    Bottom,
    CaseExpr,
    CaseFormula,
    Const,
    Eventually,
    FieldAccess,
    IfExpr,
    IfFormula,
    IsVariant,
    LtlfFormula,
    MdlType,
    Module,
    Next,
    Not,
    Or,
    Priority,
    Proposition,
    ProductInstance,
    ProductType,
    ProductValue,
    Relation,
    Rule,
    StringLength,
    SumType,
    Term,
    Top,
    Truth,
    Until,
    Variable,
    VariantInstance,
    VariantPayload,
    VariantValue,
)


RuntimeValue = bool | int | Fraction | str | ProductInstance | VariantInstance
NumberValue = int | Fraction


@dataclass(frozen=True)
class TraceState:
    propositions: frozenset[str]
    values: dict[str, RuntimeValue]

    def __contains__(self, name: str) -> bool:
        return name in self.propositions

    @override
    def __eq__(self, other: object) -> bool:
        if isinstance(other, frozenset):
            return not self.values and self.propositions == other
        if not isinstance(other, TraceState):
            return False
        return self.propositions == other.propositions and self.values == other.values


Trace = tuple[TraceState, ...]


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
    formulas = _norm_formulas(rules, priorities)
    atoms = sorted(_collect_propositions_from_formulas(formulas))
    domain_hints = _collect_domain_hints(formulas)
    variable_domains = {
        name: _domain_for_type(type, domain_hints)
        for name, type in sorted(domain_hints.variables.items())
    }

    for trace in _enumerate_traces(atoms, variable_domains, horizon):
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
        return formula.name in trace[time].propositions
    if isinstance(formula, Relation):
        return _evaluate_relation(formula, trace, time)
    if isinstance(formula, Truth):
        return bool(_evaluate_term(formula.value, trace, time))
    if isinstance(formula, IsVariant):
        value = _evaluate_term(formula.value, trace, time)
        return isinstance(value, VariantInstance) and value.variant == formula.variant
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
    if isinstance(formula, IfFormula):
        branch = (
            formula.then_branch
            if evaluate(formula.condition, trace, time)
            else formula.else_branch
        )
        return evaluate(branch, trace, time)
    if isinstance(formula, CaseFormula):
        value = _evaluate_term(formula.value, trace, time)
        if not isinstance(value, VariantInstance):
            raise TypeError("CaseFormula value must evaluate to a variant.")
        return evaluate(_case_formula_branch(formula.cases, value.variant), trace, time)

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


def _norm_formulas(
    rules: Iterable[Rule],
    priorities: Iterable[Priority],
) -> list[LtlfFormula]:
    formulas: list[LtlfFormula] = []
    for rule in rules:
        if rule.antecedent is not None:
            formulas.append(rule.antecedent)
        formulas.append(rule.consequent)

    for priority in priorities:
        if priority.condition is not None:
            formulas.append(priority.condition)

    return formulas


def _collect_propositions_from_formulas(formulas: Iterable[LtlfFormula]) -> set[str]:
    propositions: set[str] = set()
    for formula in formulas:
        propositions.update(_collect_propositions(formula))

    return propositions


def _collect_propositions(formula: LtlfFormula) -> set[str]:
    if isinstance(formula, Proposition):
        return {formula.name}
    if isinstance(formula, (Top, Bottom)):
        return set()
    if isinstance(formula, Relation):
        return _collect_term_propositions(formula.left) | _collect_term_propositions(formula.right)
    if isinstance(formula, Truth):
        return _collect_term_propositions(formula.value)
    if isinstance(formula, IsVariant):
        return _collect_term_propositions(formula.value)
    if isinstance(formula, Not):
        return _collect_propositions(formula.operand)
    if isinstance(formula, (Next, Eventually, Always)):
        return _collect_propositions(formula.operand)
    if isinstance(formula, (And, Or)):
        propositions: set[str] = set()
        for operand in formula.operands:
            propositions.update(_collect_propositions(operand))
        return propositions
    if isinstance(formula, Until):
        return _collect_propositions(formula.left) | _collect_propositions(formula.right)
    if isinstance(formula, IfFormula):
        return (
            _collect_propositions(formula.condition)
            | _collect_propositions(formula.then_branch)
            | _collect_propositions(formula.else_branch)
        )
    if isinstance(formula, CaseFormula):
        atoms: set[str] = set()
        for _, branch in formula.cases:
            atoms.update(_collect_propositions(branch))
        return atoms

    raise TypeError(f"Unsupported LTLf formula: {formula!r}")


def _collect_term_propositions(term: Term) -> set[str]:
    if isinstance(term, (Variable, Const)):
        return set()
    if isinstance(term, Arithmetic):
        return _collect_term_propositions(term.left) | _collect_term_propositions(term.right)
    if isinstance(term, StringLength):
        return _collect_term_propositions(term.value)
    if isinstance(term, FieldAccess):
        return _collect_term_propositions(term.value)
    if isinstance(term, ProductValue):
        atoms: set[str] = set()
        for _, field in term.fields:
            atoms.update(_collect_term_propositions(field))
        return atoms
    if isinstance(term, VariantValue):
        return set() if term.payload is None else _collect_term_propositions(term.payload)
    if isinstance(term, VariantPayload):
        return _collect_term_propositions(term.value)
    if isinstance(term, IfExpr):
        return (
            _collect_propositions(term.condition)
            | _collect_term_propositions(term.then_branch)
            | _collect_term_propositions(term.else_branch)
        )
    if isinstance(term, CaseExpr):
        atoms = _collect_term_propositions(term.value)
        for _, branch in term.cases:
            atoms.update(_collect_term_propositions(branch))
        return atoms
    raise TypeError(f"Unsupported term: {term!r}")


@dataclass
class _DomainHints:
    variables: dict[str, MdlType]
    bool_values: set[bool]
    int_values: set[int]
    rat_values: set[Fraction]
    string_values: set[str]


def _collect_domain_hints(formulas: Iterable[LtlfFormula]) -> _DomainHints:
    hints = _DomainHints({}, set(), set(), set(), set())
    for formula in formulas:
        _collect_formula_domain_hints(formula, hints)
    return hints


def _collect_formula_domain_hints(formula: LtlfFormula, hints: _DomainHints) -> None:
    if isinstance(formula, (Top, Bottom, Proposition)):
        return
    if isinstance(formula, Relation):
        _collect_term_domain_hints(formula.left, hints)
        _collect_term_domain_hints(formula.right, hints)
        return
    if isinstance(formula, Truth):
        _collect_term_domain_hints(formula.value, hints)
        return
    if isinstance(formula, IsVariant):
        _collect_term_domain_hints(formula.value, hints)
        return
    if isinstance(formula, Not):
        _collect_formula_domain_hints(formula.operand, hints)
        return
    if isinstance(formula, (Next, Eventually, Always)):
        _collect_formula_domain_hints(formula.operand, hints)
        return
    if isinstance(formula, (And, Or)):
        for operand in formula.operands:
            _collect_formula_domain_hints(operand, hints)
        return
    if isinstance(formula, Until):
        _collect_formula_domain_hints(formula.left, hints)
        _collect_formula_domain_hints(formula.right, hints)
        return
    if isinstance(formula, IfFormula):
        _collect_formula_domain_hints(formula.condition, hints)
        _collect_formula_domain_hints(formula.then_branch, hints)
        _collect_formula_domain_hints(formula.else_branch, hints)
        return
    if isinstance(formula, CaseFormula):
        _collect_term_domain_hints(formula.value, hints)
        for _, branch in formula.cases:
            _collect_formula_domain_hints(branch, hints)
        return

    raise TypeError(f"Unsupported LTLf formula: {formula!r}")


def _collect_term_domain_hints(term: Term, hints: _DomainHints) -> None:
    if isinstance(term, Variable):
        existing = hints.variables.get(term.name)
        if existing is not None and existing != term.type:
            raise TypeError(f"Variable {term.name!r} is used with multiple types.")
        hints.variables[term.name] = term.type
        return
    if isinstance(term, Const):
        _add_constant_hint(term, hints)
        return
    if isinstance(term, Arithmetic):
        _collect_term_domain_hints(term.left, hints)
        _collect_term_domain_hints(term.right, hints)
        return
    if isinstance(term, StringLength):
        _collect_term_domain_hints(term.value, hints)
        return
    if isinstance(term, FieldAccess):
        _collect_term_domain_hints(term.value, hints)
        return
    if isinstance(term, ProductValue):
        for _, field in term.fields:
            _collect_term_domain_hints(field, hints)
        return
    if isinstance(term, VariantValue):
        if term.payload is not None:
            _collect_term_domain_hints(term.payload, hints)
        return
    if isinstance(term, VariantPayload):
        _collect_term_domain_hints(term.value, hints)
        return
    if isinstance(term, IfExpr):
        _collect_formula_domain_hints(term.condition, hints)
        _collect_term_domain_hints(term.then_branch, hints)
        _collect_term_domain_hints(term.else_branch, hints)
        return
    if isinstance(term, CaseExpr):
        _collect_term_domain_hints(term.value, hints)
        for _, branch in term.cases:
            _collect_term_domain_hints(branch, hints)
        return

    raise TypeError(f"Unsupported term: {term!r}")


def _add_constant_hint(term: Const, hints: _DomainHints) -> None:
    if term.type == BOOL and isinstance(term.value, bool):
        hints.bool_values.add(term.value)
    elif term.type == INT and isinstance(term.value, int):
        hints.int_values.add(term.value)
    elif term.type == RAT and isinstance(term.value, Fraction):
        hints.rat_values.add(term.value)
    elif term.type == STRING and isinstance(term.value, str):
        hints.string_values.add(term.value)
    elif isinstance(term.type, ProductType) and isinstance(term.value, ProductInstance):
        for _, value in term.value.fields:
            _add_runtime_constant_hint(value, hints)
    elif isinstance(term.type, SumType) and isinstance(term.value, VariantInstance):
        if term.value.payload is not None:
            _add_runtime_constant_hint(term.value.payload, hints)


def _add_runtime_constant_hint(value: RuntimeValue, hints: _DomainHints) -> None:
    if isinstance(value, bool):
        hints.bool_values.add(value)
    elif isinstance(value, int):
        hints.int_values.add(value)
    elif isinstance(value, Fraction):
        hints.rat_values.add(value)
    elif isinstance(value, str):
        hints.string_values.add(value)
    elif isinstance(value, ProductInstance):
        for _, field_value in value.fields:
            _add_runtime_constant_hint(field_value, hints)
    elif value.payload is not None:
        _add_runtime_constant_hint(value.payload, hints)


def _domain_for_type(type: MdlType, hints: _DomainHints) -> tuple[RuntimeValue, ...]:
    if type == BOOL:
        return (False, True)
    if type == INT:
        int_values: set[int] = set(hints.int_values) | {0, 1}
        for value in list(hints.int_values):
            int_values.update({value - 1, value + 1})
        return tuple(sorted(int_values))
    if type == RAT:
        rat_values: set[Fraction] = set(hints.rat_values) | {Fraction(0), Fraction(1)}
        for value in list(hints.rat_values):
            rat_values.update({value - 1, value + 1})
        return tuple(sorted(rat_values))
    if type == STRING:
        string_values: set[str] = set(hints.string_values) | {"", "x"}
        for length in _candidate_string_lengths(hints):
            if length >= 0:
                string_values.add("x" * length)
        return tuple(sorted(string_values, key=_string_sort_key))
    if isinstance(type, ProductType):
        field_domains = [
            (name, _domain_for_type(field_type, hints))
            for name, field_type in type.fields
        ]
        return tuple(
            ProductInstance(type.name, tuple(zip((name for name, _ in field_domains), values)))
            for values in product(*(domain for _, domain in field_domains))
        )
    if isinstance(type, SumType):
        variant_values: list[RuntimeValue] = []
        for variant, payload_type in type.variants:
            if payload_type is None:
                variant_values.append(VariantInstance(type.name, variant))
            else:
                for payload in _domain_for_type(payload_type, hints):
                    variant_values.append(VariantInstance(type.name, variant, payload))
        return tuple(variant_values)
    raise TypeError(f"Unsupported type: {type!r}")


def _candidate_string_lengths(hints: _DomainHints) -> set[int]:
    lengths = {len(value) for value in hints.string_values}
    for value in hints.int_values:
        lengths.update({value - 1, value, value + 1})
    return lengths


def _string_sort_key(value: str) -> tuple[int, str]:
    return (len(value), value)


def _enumerate_traces(
    atoms: list[str],
    variable_domains: dict[str, tuple[RuntimeValue, ...]],
    horizon: int,
) -> Iterable[Trace]:
    state_options = list(_enumerate_states(atoms, variable_domains))
    for states in product(state_options, repeat=horizon):
        yield tuple(states)


def _enumerate_states(
    atoms: list[str],
    variable_domains: dict[str, tuple[RuntimeValue, ...]],
) -> Iterable[TraceState]:
    variable_names = list(variable_domains)
    for prop_values in product([False, True], repeat=len(atoms)):
        propositions = {
            atom
            for atom, value in zip(atoms, prop_values)
            if value
        }
        domain_product = product(*(variable_domains[name] for name in variable_names))
        for typed_values in domain_product:
            yield TraceState(
                frozenset(propositions),
                dict(zip(variable_names, typed_values)),
            )


def _evaluate_relation(formula: Relation, trace: Trace, time: int) -> bool:
    left = _evaluate_term(formula.left, trace, time)
    right = _evaluate_term(formula.right, trace, time)

    if formula.op == "eq":
        return left == right
    if formula.op == "ne":
        return left != right
    left_number = _as_number(left)
    right_number = _as_number(right)
    if formula.op == "lt":
        return left_number < right_number
    if formula.op == "le":
        return left_number <= right_number
    if formula.op == "gt":
        return left_number > right_number
    if formula.op == "ge":
        return left_number >= right_number
    raise ValueError(f"Unsupported relation: {formula.op!r}")


def _evaluate_term(term: Term, trace: Trace, time: int) -> RuntimeValue:
    state = trace[time]
    if isinstance(term, Variable):
        return state.values[term.name]
    if isinstance(term, Const):
        return term.value
    if isinstance(term, Arithmetic):
        left = _as_number(_evaluate_term(term.left, trace, time))
        right = _as_number(_evaluate_term(term.right, trace, time))
        if term.op == "add":
            return left + right
        if term.op == "sub":
            return left - right
        if term.op == "mul":
            return left * right
        if term.op == "div":
            return Fraction(left, right)
        raise ValueError(f"Unsupported arithmetic operation: {term.op!r}")
    if isinstance(term, StringLength):
        value = _evaluate_term(term.value, trace, time)
        if not isinstance(value, str):
            raise TypeError("Len operand must evaluate to a string.")
        return len(value)
    if isinstance(term, FieldAccess):
        value = _evaluate_term(term.value, trace, time)
        if not isinstance(value, ProductInstance):
            raise TypeError("Field access target must evaluate to a product.")
        return value.field_value(term.field_name)
    if isinstance(term, ProductValue):
        return ProductInstance(
            term.type.name,
            tuple(
                (name, _evaluate_term(field, trace, time))
                for name, field in term.fields
            ),
        )
    if isinstance(term, VariantValue):
        payload = (
            None
            if term.payload is None
            else _evaluate_term(term.payload, trace, time)
        )
        return VariantInstance(term.type.name, term.variant, payload)
    if isinstance(term, VariantPayload):
        value = _evaluate_term(term.value, trace, time)
        if not isinstance(value, VariantInstance) or value.variant != term.variant:
            raise ValueError("Variant payload requested for inactive variant.")
        if value.payload is None:
            raise ValueError("Variant has no payload.")
        return value.payload
    if isinstance(term, IfExpr):
        branch = (
            term.then_branch
            if evaluate(term.condition, trace, time)
            else term.else_branch
        )
        return _evaluate_term(branch, trace, time)
    if isinstance(term, CaseExpr):
        value = _evaluate_term(term.value, trace, time)
        if not isinstance(value, VariantInstance):
            raise TypeError("CaseExpr value must evaluate to a variant.")
        return _evaluate_term(_case_term_branch(term.cases, value.variant), trace, time)

    raise TypeError(f"Unsupported term: {term!r}")


def _case_term_branch(cases: tuple[tuple[str, Term], ...], variant: str) -> Term:
    for case_variant, branch in cases:
        if case_variant == variant:
            return branch
    raise KeyError(variant)


def _case_formula_branch(
    cases: tuple[tuple[str, LtlfFormula], ...],
    variant: str,
) -> LtlfFormula:
    for case_variant, branch in cases:
        if case_variant == variant:
            return branch
    raise KeyError(variant)


def _as_number(value: RuntimeValue) -> NumberValue:
    if isinstance(value, bool):
        raise TypeError("Bool is not a numeric runtime value.")
    if isinstance(value, (int, Fraction)):
        return value
    raise TypeError(f"Expected numeric runtime value, got {value!r}.")


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
