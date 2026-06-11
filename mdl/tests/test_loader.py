from __future__ import annotations

import json
from textwrap import dedent

from mdl import ast as A
from mdl.cli import main
from mdl.loader import default_module_name_for_path, load_module


PIPE_DSL = """
from mdl.dsl import *

module("pipe_spec")

@record
class Pipe:
    length: Rat
    radius: Rat

pipe = entity(Pipe)

@rule(O)
def pipe_length_positive():
    return always(pipe.length > 0)

fact(pipe == Pipe(length=10, radius=2))
"""


def test_load_module_compiles_mdl_py_to_ast(tmp_path):
    path = tmp_path / "bare.mdl.py"
    path.write_text(dedent("""
        from mdl.dsl import *

        flag = entity(Bool)
    """).strip() + "\n", encoding="utf-8")

    module = load_module(path)

    assert isinstance(module, A.Module)
    assert module.name == "bare"
    assert default_module_name_for_path(path) == "bare"
    assert isinstance(module.declarations[0], A.EntityDecl)


def test_cli_parse_lint_and_run_accept_mdl_py(tmp_path, capsys):
    path = tmp_path / "pipe.mdl.py"
    path.write_text(dedent(PIPE_DSL).strip() + "\n", encoding="utf-8")

    assert main(["parse", str(path)]) == 0
    parsed = json.loads(capsys.readouterr().out)
    assert parsed["name"] == "pipe_spec"

    assert main(["lint", str(path), "--json"]) == 0
    diagnostics = json.loads(capsys.readouterr().out)
    assert not any(diagnostic["severity"] == "error" for diagnostic in diagnostics)

    assert main(["run", str(path), "--expr", "pipe.length > 0"]) == 0
    assert json.loads(capsys.readouterr().out) is True
