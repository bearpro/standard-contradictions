from __future__ import annotations

import json
from pathlib import Path
from textwrap import dedent

import pytest

pytest.importorskip("z3")

from mdl.cli import main
from mdl.solver import SolveOptions, solve_paths

from .sample_sources import ALIGNMENT_SOURCE, EMAIL_SOURCE, FIB_SOURCE, PIPE_SOURCE, PWR_SOURCE, TUBE_SOURCE


STDLIB = Path(__file__).resolve().parents[1] / "src" / "mdl" / "stdlib"


def write_module(tmp_path: Path, name: str, source: str) -> Path:
    path = tmp_path / name
    path.write_text(dedent(source).strip() + "\n", encoding="utf-8")
    return path


def test_solve_pipe_example_is_sat(tmp_path):
    pipe = write_module(tmp_path, "pipe.mdl", PIPE_SOURCE)
    payload = solve_paths([pipe], SolveOptions(horizon=1))

    assert payload["status"] == "sat"
    assert payload["model"]["trace"][0]["entities"]["pipe_spec.pipe"]["length"] == "10"
    assert "pipe_spec.pipe_length_positive" in payload["model"]["winning_rules"]


def test_solve_pipe_tube_alignment_modules_are_sat(tmp_path):
    pipe = write_module(tmp_path, "pipe.mdl", PIPE_SOURCE)
    tube = write_module(tmp_path, "tube.mdl", TUBE_SOURCE)
    alignment = write_module(tmp_path, "alignment.mdl", ALIGNMENT_SOURCE)
    payload = solve_paths(
        [pipe, tube, alignment],
        SolveOptions(horizon=1),
    )

    assert payload["status"] == "sat"
    entities = payload["model"]["trace"][0]["entities"]
    assert entities["pipe_spec.pipe"]["length"] == "10"
    assert entities["tube.tube"]["length"] == "10"
    assert entities["tube.tube"]["r"] == "2"


def test_solve_reports_unresolved_unqualified_cross_module_reference(tmp_path):
    bad = write_module(
        tmp_path,
        "tube_bad.mdl",
        """
        module tube_bad

        type Tube = { length: rat }
        entity tube: Tube
        rule O r1: pipe.length > 0 always
        """,
    )

    payload = solve_paths([bad], SolveOptions(horizon=1))

    assert payload["status"] == "error"
    assert any(d["code"] == "unresolved-name" and "pipe.length" in d["message"] for d in payload["diagnostics"])


def test_solve_conflicting_obligation_and_forbidden_rule_returns_unsat_core(tmp_path):
    spec = write_module(
        tmp_path,
        "bad.mdl",
        """
        module bad

        entity x: bool
        rule O must: x always
        rule F forbid: x always
        """,
    )

    payload = solve_paths([spec], SolveOptions(horizon=1))

    assert payload["status"] == "unsat"
    assert payload["conflicts"]
    assert set(payload["conflicts"][0]["rules"]) == {"must", "forbid"}
    assert payload["conflicts"][0]["z3_core"]


def test_solve_defeasible_priority_resolves_conflict(tmp_path):
    spec = write_module(
        tmp_path,
        "ok.mdl",
        """
        module ok

        entity x: bool
        rule O must: x always
        rule F forbid: x always
        override forbid > must
        """,
    )

    payload = solve_paths([spec], SolveOptions(horizon=1))

    assert payload["status"] == "sat"
    assert payload["model"]["trace"][0]["entities"]["ok.x"] is False
    assert payload["model"]["winning_rules"] == ["ok.forbid"]
    assert payload["model"]["defeated_rules"] == ["ok.must"]


def test_solve_strict_rule_is_not_defeated(tmp_path):
    spec = write_module(
        tmp_path,
        "bad.mdl",
        """
        module bad

        entity x: bool
        strict rule O must: x always
        rule F forbid: x always
        override forbid > must
        """,
    )

    payload = solve_paths([spec], SolveOptions(horizon=1))

    assert payload["status"] == "unsat"
    assert set(payload["conflicts"][0]["rules"]) == {"must", "forbid"}


def test_solve_defeater_blocks_lower_rule_without_requirement(tmp_path):
    spec = write_module(
        tmp_path,
        "ok.mdl",
        """
        module ok

        entity x: bool
        rule O must: x always
        defeater rule F block: x always
        override block > must
        """,
    )

    payload = solve_paths([spec], SolveOptions(horizon=1))

    assert payload["status"] == "sat"
    assert payload["model"]["winning_rules"] == ["ok.block"]
    assert payload["model"]["defeated_rules"] == ["ok.must"]


def test_solve_std_list_recursive_predicate_with_temporal_body(tmp_path):
    spec = write_module(
        tmp_path,
        "collections.mdl",
        """
        module collections

        open std.collections

        func all_positive(xs: List<int>) -> bool:
            case xs:
                | List.Cons(x, rest):
                    x > 0 and all_positive(rest)
                | List.Empty():
                    true

        let xs = List.Cons(1, List.Cons(2, List.Cons(3, List.Empty())))
        rule O all_positive_rule: all_positive(xs) always
        """,
    )

    payload = solve_paths([spec], SolveOptions(horizon=2))

    assert payload["status"] == "sat"
    assert payload["model"]["winning_rules"] == ["collections.all_positive_rule"]


def test_solve_uses_explicit_stdlib_path_without_env(tmp_path, monkeypatch):
    monkeypatch.delenv("MDL_STDLIB_PATH", raising=False)
    spec = write_module(
        tmp_path,
        "collections.mdl",
        """
        module collections

        open std.collections

        let xs: List<int> = List.Empty()
        rule O ok: xs = List.Empty() always
        """,
    )

    payload = solve_paths([spec], SolveOptions(horizon=1, stdlib_path=STDLIB))

    assert payload["status"] == "sat"


def test_solve_has_no_embedded_stdlib_fallback(tmp_path, monkeypatch):
    monkeypatch.delenv("MDL_STDLIB_PATH", raising=False)
    spec = write_module(
        tmp_path,
        "collections.mdl",
        """
        module collections

        open std.collections

        let xs: List<int> = List.Empty()
        """,
    )

    payload = solve_paths([spec], SolveOptions(horizon=1))

    assert payload["status"] == "error"
    assert any(d["code"] == "unresolved-open" for d in payload["diagnostics"])


def test_solve_case_pattern_bindings_survive_boolean_operators(tmp_path):
    spec = write_module(
        tmp_path,
        "case_bindings.mdl",
        """
        module case_bindings

        type Pair = Pair(int, int)
        entity pair: Pair

        fact case pair:
                 | Pair.Pair(a, b):
                     a = 1 and b = 2
        """,
    )

    payload = solve_paths([spec], SolveOptions(horizon=1))

    assert payload["status"] == "sat"
    pair = payload["model"]["trace"][0]["entities"]["case_bindings.pair"]
    assert pair["constructor"] == "Pair"
    assert pair["values"] == [1, 2]


def test_solve_std_list_generic_len_is_instantiated_per_argument_type(tmp_path):
    spec = write_module(
        tmp_path,
        "generic_len.mdl",
        """
        module generic_len

        open std.collections

        let xs: List<int> = List.Cons(1, List.Cons(2, List.Empty()))
        rule O length_ok: len(xs) = 2 always
        """,
    )

    payload = solve_paths([spec], SolveOptions(horizon=1))

    assert payload["status"] == "sat"
    assert payload["model"]["winning_rules"] == ["generic_len.length_ok"]


def test_solve_inferred_std_list_value_type(tmp_path):
    spec = write_module(
        tmp_path,
        "inferred_generic_len.mdl",
        """
        module inferred_generic_len

        open std.collections

        let xs = List.Cons(1, List.Empty())
        rule O length_ok: len(xs) = 1 always
        """,
    )

    payload = solve_paths([spec], SolveOptions(horizon=1))

    assert payload["status"] == "sat"
    assert payload["model"]["winning_rules"] == ["inferred_generic_len.length_ok"]


def test_solve_recursive_function_over_std_list_constructors(tmp_path):
    spec = write_module(
        tmp_path,
        "lists.mdl",
        """
        module lists

        open std.collections

        func positive_tags(tags: List<int>) -> bool:
            case tags:
                | List.Cons(tag, rest):
                    if tag > 0 then positive_tags(rest) else false
                | List.Empty():
                    true

        let tags = List.Cons(1, List.Empty())
        rule O tags_positive: positive_tags(tags) always
        """,
    )

    payload = solve_paths([spec], SolveOptions(horizon=1))

    assert payload["status"] == "sat"
    assert payload["model"]["winning_rules"] == ["lists.tags_positive"]


def test_solve_recursive_sum_type_with_nested_patterns(tmp_path):
    spec = write_module(
        tmp_path,
        "nat.mdl",
        """
        module nat

        type Nat = Zero(unit) | Succ(Nat)

        func is_two(n: Nat) -> bool:
            case n:
                | Nat.Succ(Nat.Succ(Nat.Zero())):
                    true
                | _:
                    false

        entity n: Nat

        fact n = Nat.Succ(Nat.Succ(Nat.Zero()))
        rule O n_is_two: is_two(n) always
        """,
    )

    payload = solve_paths([spec], SolveOptions(horizon=1))

    assert payload["status"] == "sat"
    assert payload["model"]["winning_rules"] == ["nat.n_is_two"]
    n = payload["model"]["trace"][0]["entities"]["nat.n"]
    assert n["constructor"] == "Succ"
    assert n["values"][0]["constructor"] == "Succ"
    assert n["values"][0]["values"][0]["constructor"] == "Zero"


def test_solve_std_collections_as_ordinary_adts(tmp_path):
    spec = write_module(
        tmp_path,
        "stdlib_adts.mdl",
        """
        module stdlib_adts

        open std.collections

        entity maybe: Option<int>
        entity numbers: Set<int>
        entity lookup: Map<string, int>

        fact maybe = Option.Some(42)
        fact numbers = Set.Insert(1, Set.Empty())
        fact lookup = Map.Put("answer", 42, Map.Empty())

        rule O option_ok: maybe = Option.Some(42) always
        rule O set_ok: numbers = Set.Insert(1, Set.Empty()) always
        rule O map_ok: lookup = Map.Put("answer", 42, Map.Empty()) always
        """,
    )

    payload = solve_paths([spec], SolveOptions(horizon=1))

    assert payload["status"] == "sat"
    assert payload["model"]["winning_rules"] == [
        "stdlib_adts.option_ok",
        "stdlib_adts.set_ok",
        "stdlib_adts.map_ok",
    ]
    entities = payload["model"]["trace"][0]["entities"]
    assert entities["stdlib_adts.maybe"]["constructor"] == "Some"
    assert entities["stdlib_adts.numbers"]["constructor"] == "Insert"
    assert entities["stdlib_adts.lookup"]["constructor"] == "Put"


def test_solve_email_uses_recursive_runtime_and_ignores_declarative_align(tmp_path):
    email = write_module(tmp_path, "email.mdl", EMAIL_SOURCE)
    payload = solve_paths([email], SolveOptions(horizon=2))

    assert payload["status"] == "sat"
    assert payload["model"]["trace"][0]["entities"]["email.email"] == "ti@example.org"
    assert "email.email_addr_spec_correct" in payload["model"]["winning_rules"]
    assert payload["conflicts"] == []


def test_solve_fib_example_evaluates_recursive_function(tmp_path):
    fib = write_module(tmp_path, "fib.mdl", FIB_SOURCE)
    payload = solve_paths([fib], SolveOptions(horizon=1))

    assert payload["status"] == "sat"
    assert "fib.r1" in payload["model"]["winning_rules"]


def test_solve_pwr_example_uses_recursive_function_definition(tmp_path):
    pwr = write_module(tmp_path, "pwr.mdl", PWR_SOURCE)
    payload = solve_paths([pwr], SolveOptions(horizon=1))

    assert payload["status"] == "sat"
    entities = payload["model"]["trace"][0]["entities"]
    assert entities["pwr.power"] >= 0
    assert entities["pwr.result"] == entities["pwr.number"] ** entities["pwr.power"]
    assert "pwr.result_is_power" in payload["model"]["winning_rules"]


def test_solve_recursive_fibonacci_with_fact_uses_recursive_case(tmp_path):
    spec = write_module(
        tmp_path,
        "fib_pinned.mdl",
        """
        module fib_pinned

        func fib(n: int) -> int:
            if n <= 2 then 1
            else fib(n - 1) + fib(n - 2)

        entity fibonacci_number: int
        entity fibonacci_number_index: int

        fact fibonacci_number_index = 5
        rule O computes_fib: fibonacci_number = fib(fibonacci_number_index) always
        """,
    )

    payload = solve_paths([spec], SolveOptions(horizon=1))

    assert payload["status"] == "sat"
    assert payload["model"]["trace"][0]["entities"]["fib_pinned.fibonacci_number"] == 5


def test_solve_recursive_power_with_facts_uses_recursive_case(tmp_path):
    spec = write_module(
        tmp_path,
        "pwr_pinned.mdl",
        """
        module pwr_pinned

        func pwr(a: int, n: int) -> int:
            if n = 0 then 1
            else a * pwr(a, n - 1)

        entity number: int
        entity power: int
        entity result: int

        fact number = 2
        fact power = 3
        rule O computes_power: result = pwr(number, power) always
        """,
    )

    payload = solve_paths([spec], SolveOptions(horizon=1))

    assert payload["status"] == "sat"
    assert payload["model"]["trace"][0]["entities"]["pwr_pinned.result"] == 8


def test_solve_rule_antecedent_controls_applicability(tmp_path):
    spec = write_module(
        tmp_path,
        "antecedent.mdl",
        """
        module antecedent

        entity enabled: bool
        entity x: bool

        fact enabled = false
        rule O applies when enabled: x always
        """,
    )

    payload = solve_paths([spec], SolveOptions(horizon=1))

    assert payload["status"] == "sat"
    assert payload["model"]["trace"][0]["entities"]["antecedent.x"] is False
    assert payload["model"]["winning_rules"] == []


def test_cli_solve_prints_json(capsys, tmp_path):
    pipe = write_module(tmp_path, "pipe.mdl", PIPE_SOURCE)
    code = main(["solve", str(pipe), "--horizon", "1"])

    assert code == 0
    payload = json.loads(capsys.readouterr().out)
    assert payload["status"] == "sat"
    assert payload["horizon"] == 1
