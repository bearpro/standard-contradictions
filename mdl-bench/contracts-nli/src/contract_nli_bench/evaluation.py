from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from mdl.diagnostics import MDLError, ParseError
from mdl.linter import lint_source
from mdl.parser import parse
from mdl.solver import SolveOptions, solve_paths

from .cases import ContractNliCase
from .constants import EXPECTED_SOLVE_STATUS
from .paths import artifact_paths


@dataclass(frozen=True)
class CaseEvaluation:
    case_id: str
    split: str
    doc_id: int
    hypothesis_id: str
    choice: str
    artifact: str
    skipped: bool
    expected_status: str | None
    parse_ok: bool
    lint_error_count: int
    solve_status: str | None
    correct: bool | None
    error: str | None = None

    def to_dict(self) -> dict[str, Any]:
        return {
            "case_id": self.case_id,
            "split": self.split,
            "doc_id": self.doc_id,
            "hypothesis_id": self.hypothesis_id,
            "choice": self.choice,
            "artifact": self.artifact,
            "skipped": self.skipped,
            "expected_status": self.expected_status,
            "parse_ok": self.parse_ok,
            "lint_error_count": self.lint_error_count,
            "solve_status": self.solve_status,
            "correct": self.correct,
            "error": self.error,
        }


def validate_file(path: Path) -> tuple[bool, int, str | None]:
    if not path.exists():
        return False, 0, "missing artifact"
    source = path.read_text(encoding="utf-8")
    try:
        parse(source)
    except (MDLError, ParseError) as exc:
        return False, 0, str(exc)
    diagnostics = lint_source(source, path=str(path))
    lint_errors = [d for d in diagnostics if d.severity == "error"]
    error = "; ".join(d.message for d in lint_errors) or None
    return True, len(lint_errors), error


def evaluate_case(
    case: ContractNliCase,
    data_root: Path,
    model: str,
    scenario: str,
    scope: str,
    horizon: int,
) -> CaseEvaluation:
    paths = artifact_paths(
        data_root,
        model,
        scenario,
        scope,
        case.split,
        case.doc_id,
        case.hypothesis_id,
    )
    expected = EXPECTED_SOLVE_STATUS.get(case.choice)
    artifact = str(paths.mdl.relative_to(data_root))
    parse_ok, lint_error_count, error = validate_file(paths.mdl)
    if expected is None:
        return CaseEvaluation(
            case_id=case.case_id,
            split=case.split,
            doc_id=case.doc_id,
            hypothesis_id=case.hypothesis_id,
            choice=case.choice,
            artifact=artifact,
            skipped=True,
            expected_status=None,
            parse_ok=parse_ok,
            lint_error_count=lint_error_count,
            solve_status=None,
            correct=None,
            error=error,
        )
    if not parse_ok or lint_error_count:
        return CaseEvaluation(
            case_id=case.case_id,
            split=case.split,
            doc_id=case.doc_id,
            hypothesis_id=case.hypothesis_id,
            choice=case.choice,
            artifact=artifact,
            skipped=False,
            expected_status=expected,
            parse_ok=parse_ok,
            lint_error_count=lint_error_count,
            solve_status=None,
            correct=False,
            error=error,
        )
    try:
        payload = solve_paths([paths.mdl], SolveOptions(horizon=horizon))
        status = str(payload["status"])
        error = None
    except Exception as exc:  # solver failures are benchmark failures
        status = None
        error = str(exc)
    return CaseEvaluation(
        case_id=case.case_id,
        split=case.split,
        doc_id=case.doc_id,
        hypothesis_id=case.hypothesis_id,
        choice=case.choice,
        artifact=artifact,
        skipped=False,
        expected_status=expected,
        parse_ok=parse_ok,
        lint_error_count=lint_error_count,
        solve_status=status,
        correct=status == expected,
        error=error,
    )


def evaluate_cases(
    cases: list[ContractNliCase],
    data_root: Path,
    model: str,
    scenario: str,
    scope: str,
    horizon: int,
    write: bool = True,
) -> tuple[list[CaseEvaluation], dict[str, Any]]:
    results = [
        evaluate_case(case, data_root, model, scenario, scope, horizon)
        for case in cases
    ]
    summary = summarize(results)
    if write:
        run_root = artifact_paths(
            data_root, model, scenario, scope, "dev", 0, "x"
        ).run_root
        run_root.mkdir(parents=True, exist_ok=True)
        results_path = run_root / "results.jsonl"
        summary_path = run_root / "summary.json"
        results_path.write_text(
            "".join(
                json.dumps(item.to_dict(), ensure_ascii=False, sort_keys=True) + "\n"
                for item in results
            ),
            encoding="utf-8",
        )
        summary_path.write_text(
            json.dumps(summary, ensure_ascii=False, indent=2, sort_keys=True) + "\n",
            encoding="utf-8",
        )
    return results, summary


def summarize(results: list[CaseEvaluation]) -> dict[str, Any]:
    evaluated = [item for item in results if not item.skipped]
    solved_valid = [
        item
        for item in evaluated
        if item.parse_ok and item.lint_error_count == 0 and item.solve_status is not None
    ]
    correct_all = sum(1 for item in evaluated if item.correct is True)
    correct_solved = sum(1 for item in solved_valid if item.correct is True)
    return {
        "total_cases": len(results),
        "evaluated_cases": len(evaluated),
        "skipped_not_mentioned": sum(1 for item in results if item.skipped),
        "parse_errors": sum(1 for item in results if not item.parse_ok),
        "lint_error_cases": sum(1 for item in results if item.lint_error_count > 0),
        "solve_error_cases": sum(
            1
            for item in evaluated
            if item.parse_ok
            and item.lint_error_count == 0
            and item.solve_status is None
        ),
        "correct_evaluated": correct_all,
        "accuracy_evaluated": _ratio(correct_all, len(evaluated)),
        "valid_solved_cases": len(solved_valid),
        "correct_valid_solved": correct_solved,
        "accuracy_valid_solved": _ratio(correct_solved, len(solved_valid)),
    }


def _ratio(numerator: int, denominator: int) -> float | None:
    if denominator == 0:
        return None
    return numerator / denominator
