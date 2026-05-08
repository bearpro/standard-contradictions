from __future__ import annotations

import json
from textwrap import dedent
from pathlib import Path

import pytest

pytest.importorskip("z3")

from mdl.cli import main
from mdl.solver import SolveOptions, solve_paths


ROOT = Path(__file__).resolve().parents[1]


def write_module(tmp_path: Path, name: str, source: str) -> Path:
    path = tmp_path / name
    path.write_text(dedent(source).strip() + "\n", encoding="utf-8")
    return path


def test_solve_pipe_example_is_sat():
    payload = solve_paths([ROOT / "examples" / "pipe.mdl"], SolveOptions(horizon=1))

    assert payload["status"] == "sat"
    assert payload["model"]["trace"][0]["entities"]["pipe_spec.pipe"]["length"] == "10"
    assert "pipe_spec.pipe_length_positive" in payload["model"]["winning_rules"]


def test_solve_pipe_tube_alignment_modules_are_sat():
    payload = solve_paths(
        [
            ROOT / "examples" / "pipe.mdl",
            ROOT / "examples" / "tube.mdl",
            ROOT / "examples" / "alignment.mdl",
        ],
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


def test_solve_finite_collection_quantifier_with_temporal_body(tmp_path):
    spec = write_module(
        tmp_path,
        "collections.mdl",
        """
        module collections

        val xs = [1, 2, 3]
        rule O all_positive: forall x in xs: x > 0 always
        """,
    )

    payload = solve_paths([spec], SolveOptions(horizon=2))

    assert payload["status"] == "sat"
    assert payload["model"]["winning_rules"] == ["collections.all_positive"]


def test_solve_email_uses_recursive_runtime_and_ignores_assert_align():
    payload = solve_paths([ROOT / "examples" / "email.mdl"], SolveOptions(horizon=2))

    assert payload["status"] == "sat"
    assert payload["model"]["trace"][0]["entities"]["email.email"] == "ti@example.org"
    assert "email.email_addr_spec_correct" in payload["model"]["winning_rules"]
    assert payload["conflicts"] == []


def test_solve_fib_example_evaluates_recursive_function():
    payload = solve_paths([ROOT / "examples" / "fib.mdl"], SolveOptions(horizon=1))

    assert payload["status"] == "sat"
    assert "fib.r2" in payload["model"]["winning_rules"]


def test_solve_pwr_example_uses_recursive_function_definition():
    payload = solve_paths([ROOT / "examples" / "pwr.mdl"], SolveOptions(horizon=1))

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


def test_cli_solve_prints_json(capsys):
    code = main(["solve", str(ROOT / "examples" / "pipe.mdl"), "--horizon", "1"])

    assert code == 0
    payload = json.loads(capsys.readouterr().out)
    assert payload["status"] == "sat"
    assert payload["horizon"] == 1
