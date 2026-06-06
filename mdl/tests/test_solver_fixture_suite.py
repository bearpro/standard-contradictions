from __future__ import annotations

import json
from pathlib import Path

import pytest

from mdl import ast as A
from mdl.aligner import AlignmentOptions, align_modules, render_alignment_module
from mdl.parser import parse
from mdl.printer import format_module

pytest.importorskip("z3")

from mdl.solver import SolveOptions, solve_paths


FIXTURES = Path(__file__).resolve().parent / "fixtures" / "solver_suite"
PIPELINE = FIXTURES / "pipeline_spec.mdl"
TUBE = FIXTURES / "tube_model.mdl"
POLICY = FIXTURES / "policy_exceptions.mdl"
ALIGNMENT = FIXTURES / "pipeline_tube_alignment.mdl"
ALIGNMENT_REPORT = FIXTURES / "pipeline_tube_alignment.json"


def test_fixture_modules_parse_and_cover_core_language_features():
    pipeline = parse(PIPELINE.read_text(encoding="utf-8"))
    policy = parse(POLICY.read_text(encoding="utf-8"))

    assert any(isinstance(d, A.TypeDecl) and isinstance(d.definition, A.SumType) for d in pipeline.declarations)
    assert any(isinstance(d, A.FuncDecl) and d.name == "positive_tags" for d in pipeline.declarations)
    assert any(isinstance(d, A.FuncDecl) and d.name == "pipe_ok" for d in pipeline.declarations)
    assert any(isinstance(d, A.EntityDecl) and d.name == "pipe" for d in pipeline.declarations)
    assert any(isinstance(d, A.RuleDecl) and d.name == "pipe_quality" for d in pipeline.declarations)
    assert any(isinstance(d, A.PriorityDecl) for d in policy.declarations)
    assert any(isinstance(d, A.RuleDecl) and d.strength == "defeater" for d in policy.declarations)


def test_saved_alignment_module_matches_builtin_alignment_output():
    left = parse(PIPELINE.read_text(encoding="utf-8"))
    right = parse(TUBE.read_text(encoding="utf-8"))
    report = align_modules(
        left,
        right,
        AlignmentOptions(matcher="builtin", candidate_threshold=0.2, accept_threshold=0.45),
    )
    regenerated = format_module(render_alignment_module(
        report,
        module_name="fixture_pipeline_tube_alignment",
    ))

    assert regenerated == ALIGNMENT.read_text(encoding="utf-8")

    payload = json.loads(ALIGNMENT_REPORT.read_text(encoding="utf-8"))
    accepted = {(item["left"], item["right"]) for item in payload["accepted"]}
    assert accepted == {
        ("fixture_pipeline_spec.pipe.length", "fixture_tube_model.tube.length"),
        ("fixture_pipeline_spec.pipe.pressure", "fixture_tube_model.tube.pressure"),
    }


def test_saved_alignment_participates_in_multi_module_solve():
    payload = solve_paths([PIPELINE, TUBE, ALIGNMENT], SolveOptions(horizon=2))

    assert payload["status"] == "sat"
    entities = payload["model"]["trace"][0]["entities"]
    assert entities["fixture_pipeline_spec.pipe"]["length"] == "12"
    assert entities["fixture_tube_model.tube"]["length"] == "12"
    assert "fixture_pipeline_tube_alignment.alignment_001" in payload["model"]["winning_rules"]
    assert "fixture_pipeline_tube_alignment.alignment_002" in payload["model"]["winning_rules"]


def test_policy_fixture_exercises_defeater_priority():
    payload = solve_paths([POLICY], SolveOptions(horizon=1))

    assert payload["status"] == "sat"
    assert payload["model"]["winning_rules"] == ["fixture_policy_exceptions.maintenance_exception"]
    assert payload["model"]["defeated_rules"] == ["fixture_policy_exceptions.perform_work"]
