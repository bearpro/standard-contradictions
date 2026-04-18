from __future__ import annotations

from collections.abc import Callable, Iterable
from dataclasses import dataclass
from fractions import Fraction
from itertools import product

from mdl.lib.alignment import Alignment
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
    FunctionCall,
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
    ProductConstruct,
    ProductInstance,
    ProductType,
    Relation,
    RuntimeValue,
    Rule,
    StringCharAt,
    StringLength,
    SumType,
    Term,
    Top,
    Truth,
    TypeRef,
    Until,
    Variable,
    VariantConstruct,
    VariantInstance,
    VariantPayload,
)


NumberValue = int | Fraction
_MAX_RECURSIVE_SUM_DEPTH = 2


@dataclass(frozen=True)
class TraceState:
    propositions: frozenset[str]
    values: dict[str, RuntimeValue]


Trace = tuple[TraceState, ...]


@dataclass(frozen=True)
class SolveResult:
    is_consistent: bool
    horizon: int
    trace: Trace | None
    winning_rules: tuple[Rule, ...]

    def __bool__(self) -> bool:
        return self.is_consistent


def solve(
    *documents: Module,
    horizon: int = 1,
    alignments: Iterable[Alignment] = (),
    max_recursive_depth: int = _MAX_RECURSIVE_SUM_DEPTH,
) -> SolveResult:
    if horizon < 1:
        raise ValueError("horizon must be greater than or equal to 1.")
    if max_recursive_depth < 0:
        raise ValueError("max_recursive_depth must be greater than or equal to 0.")

    rules, priorities = _collect_norms(documents, tuple(alignments))
    formulas = _norm_formulas(rules, priorities)
    atoms = sorted(_collect_propositions_from_formulas(formulas))
    domain_hints = _collect_domain_hints(formulas)
    variable_domains = {
        name: _domain_for_type(type, domain_hints, max_recursive_depth)
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


NameMapper = Callable[[str], str]


def _required_name(name: str | None, kind: str) -> str:
    if name is None:
        raise NameError(f"{kind} is not bound to a module name.")
    return name


def _collect_norms(
    documents: Iterable[Module],
    alignments: tuple[Alignment, ...],
) -> tuple[list[Rule], list[Priority]]:
    rules: list[Rule] = []
    priorities: list[Priority] = []
    documents = tuple(documents)
    if not alignments and len(documents) <= 1:
        for document in documents:
            for value in document.objects.values():
                if isinstance(value, Rule):
                    rules.append(value)
                elif isinstance(value, Priority):
                    priorities.append(value)
        return rules, priorities

    name_mappers = _document_name_mappers(documents, alignments)

    for document, name_mapper in zip(documents, name_mappers):
        rule_map: dict[int, Rule] = {}
        for value in document.objects.values():
            if isinstance(value, Rule):
                renamed_rule = _rename_rule(value, name_mapper)
                rule_map[id(value)] = renamed_rule
                rules.append(renamed_rule)

        for value in document.objects.values():
            if isinstance(value, Priority):
                priorities.append(_rename_priority(value, name_mapper, rule_map))

    return rules, priorities


def _document_name_mappers(
    documents: tuple[Module, ...],
    alignments: tuple[Alignment, ...],
) -> tuple[NameMapper, ...]:
    if not alignments and len(documents) <= 1:
        return tuple(lambda name: name for _ in documents)
    if alignments and len(documents) != 2:
        raise ValueError("alignments are currently supported only for two documents.")

    aligned_names: list[dict[str, str]] = [dict() for _ in documents]
    if alignments:
        left_doc, right_doc = documents
        for alignment in alignments:
            canonical = _alignment_canonical_name(alignment)
            left_name = _alignment_symbol_name(left_doc, alignment.left_name)
            right_name = _alignment_symbol_name(right_doc, alignment.right_name)
            if left_name is not None:
                _add_aligned_name(aligned_names[0], left_name, canonical)
            if right_name is not None:
                _add_aligned_name(aligned_names[1], right_name, canonical)

    return tuple(
        _make_name_mapper(index, aligned)
        for index, aligned in enumerate(aligned_names)
    )


def _make_name_mapper(index: int, aligned_names: dict[str, str]) -> NameMapper:
    def map_name(name: str) -> str:
        try:
            return aligned_names[name]
        except KeyError:
            return f"__mdl_doc_{index}__{name}"

    return map_name


def _alignment_canonical_name(alignment: Alignment) -> str:
    return f"__mdl_alignment__{alignment.left_name}__{alignment.right_name}"


def _alignment_symbol_name(document: Module, name: str) -> str | None:
    value = document.objects.get(name)
    if isinstance(value, (Proposition, Variable)):
        return _required_name(value.name, type(value).__name__)
    if value is None:
        return name
    return None


def _add_aligned_name(
    aligned_names: dict[str, str],
    source_name: str,
    canonical_name: str,
) -> None:
    existing = aligned_names.get(source_name)
    if existing is not None and existing != canonical_name:
        raise ValueError(f"Conflicting alignments for symbol {source_name!r}.")
    aligned_names[source_name] = canonical_name


def _rename_rule(rule: Rule, name_mapper: NameMapper) -> Rule:
    return Rule(
        rule.source,
        rule.kind,
        None if rule.antecedent is None else _rename_formula(rule.antecedent, name_mapper),
        _rename_formula(rule.consequent, name_mapper),
        rule.strength,
    )


def _rename_priority(
    priority: Priority,
    name_mapper: NameMapper,
    rule_map: dict[int, Rule],
) -> Priority:
    return Priority(
        rule_map.get(id(priority.higher)) or _rename_rule(priority.higher, name_mapper),
        rule_map.get(id(priority.lower)) or _rename_rule(priority.lower, name_mapper),
        None if priority.condition is None else _rename_formula(priority.condition, name_mapper),
        priority.source,
    )


def _rename_formula(formula: LtlfFormula, name_mapper: NameMapper) -> LtlfFormula:
    if isinstance(formula, Proposition):
        return Proposition(
            formula.source,
            name_mapper(_required_name(formula.name, "Proposition")),
        )
    if isinstance(formula, (Top, Bottom)):
        return formula
    if isinstance(formula, Relation):
        return Relation(
            formula.op,
            _rename_term(formula.left, name_mapper),
            _rename_term(formula.right, name_mapper),
            formula.source,
        )
    if isinstance(formula, Truth):
        return Truth(_rename_term(formula.value, name_mapper), formula.source)
    if isinstance(formula, IsVariant):
        return IsVariant(
            _rename_term(formula.value, name_mapper),
            formula.variant,
            formula.source,
        )
    if isinstance(formula, Not):
        return Not(_rename_formula(formula.operand, name_mapper), formula.source)
    if isinstance(formula, And):
        return And(
            *(_rename_formula(operand, name_mapper) for operand in formula.operands),
            source=formula.source,
        )
    if isinstance(formula, Or):
        return Or(
            *(_rename_formula(operand, name_mapper) for operand in formula.operands),
            source=formula.source,
        )
    if isinstance(formula, Next):
        return Next(_rename_formula(formula.operand, name_mapper), formula.source)
    if isinstance(formula, Until):
        return Until(
            _rename_formula(formula.left, name_mapper),
            _rename_formula(formula.right, name_mapper),
            formula.source,
        )
    if isinstance(formula, Eventually):
        return Eventually(_rename_formula(formula.operand, name_mapper), formula.source)
    if isinstance(formula, Always):
        return Always(_rename_formula(formula.operand, name_mapper), formula.source)
    if isinstance(formula, IfFormula):
        return IfFormula(
            _rename_formula(formula.condition, name_mapper),
            _rename_formula(formula.then_branch, name_mapper),
            _rename_formula(formula.else_branch, name_mapper),
            formula.source,
        )
    if isinstance(formula, CaseFormula):
        return CaseFormula(
            _rename_term(formula.value, name_mapper),
            {
                name: _rename_formula(branch, name_mapper)
                for name, branch in formula.cases
            },
            formula.source,
        )

    raise TypeError(f"Unsupported LTLf formula: {formula!r}")


def _rename_term(term: Term, name_mapper: NameMapper) -> Term:
    if isinstance(term, Variable):
        return Variable(
            name_mapper(_required_name(term.name, "Variable")),
            term.type,
            term.source,
        )
    if isinstance(term, Const):
        return term
    if isinstance(term, Arithmetic):
        return Arithmetic(
            term.op,
            _rename_term(term.left, name_mapper),
            _rename_term(term.right, name_mapper),
            term.source,
        )
    if isinstance(term, StringLength):
        return StringLength(_rename_term(term.value, name_mapper), term.source)
    if isinstance(term, StringCharAt):
        return StringCharAt(
            _rename_term(term.value, name_mapper),
            _rename_term(term.index, name_mapper),
            term.source,
        )
    if isinstance(term, ProductConstruct):
        return ProductConstruct(
            term.type,
            {
                name: _rename_term(field_value, name_mapper)
                for name, field_value in term.fields
            },
            term.source,
        )
    if isinstance(term, VariantConstruct):
        return VariantConstruct(
            term.type,
            term.variant,
            None if term.payload is None else _rename_term(term.payload, name_mapper),
            term.source,
        )
    if isinstance(term, FieldAccess):
        return FieldAccess(
            _rename_term(term.value, name_mapper),
            term.field_name,
            term.source,
        )
    if isinstance(term, VariantPayload):
        return VariantPayload(
            _rename_term(term.value, name_mapper),
            term.variant,
            term.source,
        )
    if isinstance(term, IfExpr):
        return IfExpr(
            _rename_formula(term.condition, name_mapper),
            _rename_term(term.then_branch, name_mapper),
            _rename_term(term.else_branch, name_mapper),
            term.source,
        )
    if isinstance(term, CaseExpr):
        return CaseExpr(
            _rename_term(term.value, name_mapper),
            {
                name: _rename_term(branch, name_mapper)
                for name, branch in term.cases
            },
            term.source,
        )
    if isinstance(term, FunctionCall):
        return FunctionCall(
            term.function,
            tuple(_rename_term(argument, name_mapper) for argument in term.arguments),
            term.source,
        )

    raise TypeError(f"Unsupported term: {term!r}")


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
        for node in _walk_formula(formula):
            if isinstance(node, Proposition):
                propositions.add(_required_name(node.name, "Proposition"))

    return propositions


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
        for node in _walk_formula(formula):
            if isinstance(node, Variable):
                node_name = _required_name(node.name, "Variable")
                existing = hints.variables.get(node_name)
                if existing is not None and existing != node.type:
                    raise TypeError(f"Variable {node_name!r} is used with multiple types.")
                hints.variables[node_name] = node.type
            elif isinstance(node, Const):
                _add_constant_hint(node, hints)
    return hints


def _walk_formula(
    formula: LtlfFormula,
    function_stack: frozenset[str] | None = None,
) -> Iterable[LtlfFormula | Term]:
    function_stack = function_stack or frozenset()
    yield formula
    if isinstance(formula, (Top, Bottom, Proposition)):
        return
    if isinstance(formula, Relation):
        yield from _walk_term(formula.left, function_stack)
        yield from _walk_term(formula.right, function_stack)
        return
    if isinstance(formula, (Truth, IsVariant)):
        yield from _walk_term(formula.value, function_stack)
        return
    if isinstance(formula, Not):
        yield from _walk_formula(formula.operand, function_stack)
        return
    if isinstance(formula, (Next, Eventually, Always)):
        yield from _walk_formula(formula.operand, function_stack)
        return
    if isinstance(formula, (And, Or)):
        for operand in formula.operands:
            yield from _walk_formula(operand, function_stack)
        return
    if isinstance(formula, Until):
        yield from _walk_formula(formula.left, function_stack)
        yield from _walk_formula(formula.right, function_stack)
        return
    if isinstance(formula, IfFormula):
        yield from _walk_formula(formula.condition, function_stack)
        yield from _walk_formula(formula.then_branch, function_stack)
        yield from _walk_formula(formula.else_branch, function_stack)
        return
    if isinstance(formula, CaseFormula):
        yield from _walk_term(formula.value, function_stack)
        for _, branch in formula.cases:
            yield from _walk_formula(branch, function_stack)
        return

    raise TypeError(f"Unsupported LTLf formula: {formula!r}")


def _walk_term(
    term: Term,
    function_stack: frozenset[str] | None = None,
) -> Iterable[LtlfFormula | Term]:
    function_stack = function_stack or frozenset()
    yield term
    if isinstance(term, (Variable, Const)):
        return
    if isinstance(term, Arithmetic):
        yield from _walk_term(term.left, function_stack)
        yield from _walk_term(term.right, function_stack)
        return
    if isinstance(
        term,
        (
            StringLength,
            StringCharAt,
            FieldAccess,
            VariantPayload,
        ),
    ):
        yield from _walk_term(term.value, function_stack)
        if isinstance(term, StringCharAt):
            yield from _walk_term(term.index, function_stack)
        return
    if isinstance(term, ProductConstruct):
        for _, field_value in term.fields:
            yield from _walk_term(field_value, function_stack)
        return
    if isinstance(term, VariantConstruct):
        if term.payload is not None:
            yield from _walk_term(term.payload, function_stack)
        return
    if isinstance(term, IfExpr):
        yield from _walk_formula(term.condition, function_stack)
        yield from _walk_term(term.then_branch, function_stack)
        yield from _walk_term(term.else_branch, function_stack)
        return
    if isinstance(term, CaseExpr):
        yield from _walk_term(term.value, function_stack)
        for _, branch in term.cases:
            yield from _walk_term(branch, function_stack)
        return
    if isinstance(term, FunctionCall):
        for argument in term.arguments:
            yield from _walk_term(argument, function_stack)
        function_name = _required_name(term.function.name, "Function")
        if function_name in function_stack:
            return
        body = _substitute_function_body(term)
        yield from _walk_term(body, function_stack | {function_name})
        return

    raise TypeError(f"Unsupported term: {term!r}")


def _substitute_function_body(call: FunctionCall) -> Term:
    replacements = {
        parameter.name: argument
        for parameter, argument in zip(call.function.parameters, call.arguments)
    }
    return _substitute_term(call.function.body_term(), replacements)


def _substitute_formula(
    formula: LtlfFormula,
    replacements: dict[str, Term],
) -> LtlfFormula:
    if isinstance(formula, (Top, Bottom, Proposition)):
        return formula
    if isinstance(formula, Relation):
        return Relation(
            formula.op,
            _substitute_term(formula.left, replacements),
            _substitute_term(formula.right, replacements),
            formula.source,
        )
    if isinstance(formula, Truth):
        return Truth(_substitute_term(formula.value, replacements), formula.source)
    if isinstance(formula, IsVariant):
        return IsVariant(
            _substitute_term(formula.value, replacements),
            formula.variant,
            formula.source,
        )
    if isinstance(formula, Not):
        return Not(_substitute_formula(formula.operand, replacements), formula.source)
    if isinstance(formula, And):
        return And(
            *(_substitute_formula(operand, replacements) for operand in formula.operands),
            source=formula.source,
        )
    if isinstance(formula, Or):
        return Or(
            *(_substitute_formula(operand, replacements) for operand in formula.operands),
            source=formula.source,
        )
    if isinstance(formula, Next):
        return Next(_substitute_formula(formula.operand, replacements), formula.source)
    if isinstance(formula, Until):
        return Until(
            _substitute_formula(formula.left, replacements),
            _substitute_formula(formula.right, replacements),
            formula.source,
        )
    if isinstance(formula, Eventually):
        return Eventually(_substitute_formula(formula.operand, replacements), formula.source)
    if isinstance(formula, Always):
        return Always(_substitute_formula(formula.operand, replacements), formula.source)
    if isinstance(formula, IfFormula):
        return IfFormula(
            _substitute_formula(formula.condition, replacements),
            _substitute_formula(formula.then_branch, replacements),
            _substitute_formula(formula.else_branch, replacements),
            formula.source,
        )
    if isinstance(formula, CaseFormula):
        return CaseFormula(
            _substitute_term(formula.value, replacements),
            {
                name: _substitute_formula(branch, replacements)
                for name, branch in formula.cases
            },
            formula.source,
        )
    raise TypeError(f"Unsupported LTLf formula: {formula!r}")


def _substitute_term(term: Term, replacements: dict[str, Term]) -> Term:
    if isinstance(term, Variable):
        return replacements.get(_required_name(term.name, "Variable"), term)
    if isinstance(term, Const):
        return term
    if isinstance(term, Arithmetic):
        return Arithmetic(
            term.op,
            _substitute_term(term.left, replacements),
            _substitute_term(term.right, replacements),
            term.source,
        )
    if isinstance(term, StringLength):
        return StringLength(_substitute_term(term.value, replacements), term.source)
    if isinstance(term, StringCharAt):
        return StringCharAt(
            _substitute_term(term.value, replacements),
            _substitute_term(term.index, replacements),
            term.source,
        )
    if isinstance(term, ProductConstruct):
        return ProductConstruct(
            term.type,
            {
                name: _substitute_term(field_value, replacements)
                for name, field_value in term.fields
            },
            term.source,
        )
    if isinstance(term, VariantConstruct):
        return VariantConstruct(
            term.type,
            term.variant,
            None if term.payload is None else _substitute_term(term.payload, replacements),
            term.source,
        )
    if isinstance(term, FieldAccess):
        return FieldAccess(
            _substitute_term(term.value, replacements),
            term.field_name,
            term.source,
        )
    if isinstance(term, VariantPayload):
        return VariantPayload(
            _substitute_term(term.value, replacements),
            term.variant,
            term.source,
        )
    if isinstance(term, IfExpr):
        return IfExpr(
            _substitute_formula(term.condition, replacements),
            _substitute_term(term.then_branch, replacements),
            _substitute_term(term.else_branch, replacements),
            term.source,
        )
    if isinstance(term, CaseExpr):
        return CaseExpr(
            _substitute_term(term.value, replacements),
            {
                name: _substitute_term(branch, replacements)
                for name, branch in term.cases
            },
            term.source,
        )
    if isinstance(term, FunctionCall):
        return FunctionCall(
            term.function,
            tuple(_substitute_term(argument, replacements) for argument in term.arguments),
            term.source,
        )
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


def _resolve_type(type: MdlType) -> MdlType:
    if isinstance(type, TypeRef):
        if type.target is None:
            raise TypeError(f"Unbound recursive type reference: {type.name!r}.")
        return type.target
    return type


def _domain_for_type(
    type: MdlType,
    hints: _DomainHints,
    max_recursive_depth: int,
    type_depths: dict[str, int] | None = None,
) -> tuple[RuntimeValue, ...]:
    type = _resolve_type(type)
    type_depths = type_depths or {}
    if type.name == BOOL.name:
        return (False, True)
    if type.name == INT.name:
        int_values: set[int] = set(hints.int_values) | {0, 1}
        for value in list(hints.int_values):
            int_values.update({value - 1, value + 1})
        return tuple(sorted(int_values))
    if type.name == RAT.name:
        rat_values: set[Fraction] = set(hints.rat_values) | {Fraction(0), Fraction(1)}
        for value in list(hints.rat_values):
            rat_values.update({value - 1, value + 1})
        return tuple(sorted(rat_values))
    if type.name == STRING.name:
        string_values: set[str] = set(hints.string_values) | {"", "x"}
        for length in _candidate_string_lengths(hints):
            if length >= 0:
                string_values.add("x" * length)
        return tuple(sorted(string_values, key=_string_sort_key))
    if isinstance(type, ProductType):
        type_name = _required_name(type.name, "ProductType")
        field_domains = [
            (
                name,
                _domain_for_type(
                    field_type,
                    hints,
                    max_recursive_depth,
                    type_depths,
                ),
            )
            for name, field_type in type.fields
        ]
        return tuple(
            ProductInstance(type_name, tuple(zip((name for name, _ in field_domains), values)))
            for values in product(*(domain for _, domain in field_domains))
        )
    if isinstance(type, SumType):
        type_name = _required_name(type.name, "SumType")
        current_depth = type_depths.get(type_name, 0)
        next_depths = {**type_depths, type_name: current_depth + 1}
        variant_values: list[RuntimeValue] = []
        for variant, payload_type in type.variants:
            if payload_type is None:
                variant_values.append(VariantInstance(type_name, variant))
            elif current_depth >= max_recursive_depth:
                continue
            else:
                for payload in _domain_for_type(
                    payload_type,
                    hints,
                    max_recursive_depth,
                    next_depths,
                ):
                    variant_values.append(VariantInstance(type_name, variant, payload))
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
        return state.values[_required_name(term.name, "Variable")]
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
    if isinstance(term, StringCharAt):
        value = _evaluate_term(term.value, trace, time)
        index = _evaluate_term(term.index, trace, time)
        if not isinstance(value, str):
            raise TypeError("CharAt value must evaluate to a string.")
        if isinstance(index, bool) or not isinstance(index, int):
            raise TypeError("CharAt index must evaluate to an int.")
        if not 0 <= index < len(value):
            raise IndexError("CharAt index out of bounds.")
        return value[index]
    if isinstance(term, ProductConstruct):
        return ProductInstance(
            _required_name(term.type.name, "ProductType"),
            tuple(
                (name, _evaluate_term(field_value, trace, time))
                for name, field_value in term.fields
            ),
        )
    if isinstance(term, VariantConstruct):
        return VariantInstance(
            _required_name(term.type.name, "SumType"),
            term.variant,
            None if term.payload is None else _evaluate_term(term.payload, trace, time),
        )
    if isinstance(term, FieldAccess):
        value = _evaluate_term(term.value, trace, time)
        if not isinstance(value, ProductInstance):
            raise TypeError("Field access target must evaluate to a product.")
        return value.field_value(term.field_name)
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
    if isinstance(term, FunctionCall):
        argument_values = [
            _evaluate_term(argument, trace, time)
            for argument in term.arguments
        ]
        bound_values = {
            parameter.name: value
            for parameter, value in zip(term.function.parameters, argument_values)
        }
        return _evaluate_term(
            term.function.body_term(),
            _bind_values(trace, bound_values),
            time,
        )

    raise TypeError(f"Unsupported term: {term!r}")


def _bind_values(trace: Trace, values: dict[str, RuntimeValue]) -> Trace:
    return tuple(
        TraceState(state.propositions, {**state.values, **values})
        for state in trace
    )


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
