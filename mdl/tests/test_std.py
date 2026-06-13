from __future__ import annotations

import json
from pathlib import Path
from textwrap import dedent

import pytest

pytest.importorskip("z3")

from mdl.cli import main
from mdl.solver import SolveOptions, solve_paths

STDLIB = Path(__file__).resolve().parents[1] / "src" / "mdl" / "stdlib"

def _write_module(tmp_path: Path, name: str, source: str) -> Path:
    path = tmp_path / name
    path.write_text(dedent(source).strip() + "\n", encoding="utf-8")
    return path

x = {'constructor': 'Cons', 'values': [1, {'constructor': 'Cons', 'values': [2, {'constructor': 'Empty', 'values': [0]}]}]}

def _to_py_list(mdl_list):
    result = []
    def traverse(list):
        if list['constructor'] == "Cons":
            value = list['values'][0]
            tail = list['values'][1]
            result.append(value)
            traverse(tail)
        else:
            return []
    traverse(mdl_list)

    return result

def test_list_concat(tmp_path):
    spec = _write_module(
        tmp_path,
        "rat_division.mdl",
        """
        module test

        open std.collections

        entity concatenated_list: List<int>

        fact concatenated_list = list.concat(
            List.Cons(1, List.Empty()),
            List.Cons(2, List.Empty())
            )
        """,
    )

    payload = solve_paths([spec], SolveOptions(horizon=1))

    assert payload["status"] == "sat"
    assert _to_py_list(payload["model"]["trace"][0]["entities"]["test.concatenated_list"]) == [1, 2]
