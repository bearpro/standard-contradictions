import json

from mdl.aligner import (
    AlignmentOptions,
    align_modules,
    align_sources,
    project_module,
    render_alignment_module,
)
from mdl.cli import main
from mdl.linter import lint_source
from mdl.parser import parse
from mdl.printer import format_module

from .sample_sources import TUBE_SOURCE


PIPE_DSL = """
from mdl.dsl import *

module("pipe_spec")

@record
class Pipe:
    length: Rat
    radius: Rat

pipe = entity(Pipe)
"""


def test_aligner_suggests_same_entity_name():
    a = "module a\nentity email: string\n"
    b = "module b\nentity email: string\n"
    report = align_sources([a, b])
    assert report.suggestions
    assert report.suggestions[0].score == 1.0


def test_project_module_extracts_entity_record_fields():
    module = parse("""
module pipe_spec

type Pipe = { length: rat, radius: rat, dimensions: { width: int, height: int } }

entity pipe: Pipe
""")

    paths = {element.path for element in project_module(module)}

    assert "pipe" in paths
    assert "pipe.length" in paths
    assert "pipe.radius" in paths
    assert "pipe.dimensions" in paths
    assert "pipe.dimensions.width" in paths
    assert "pipe.dimensions.height" in paths


def test_builtin_aligner_accepts_terminal_fields_and_keeps_entity_evidence():
    left = parse("""
module left_pipe

type Pipe = { length: rat, radius: rat }

entity pipe: Pipe
""")
    right = parse("""
module right_pipe

type PipeRecord = { length: rat, radius: rat }

entity pipe: PipeRecord
""")

    report = align_modules(left, right, AlignmentOptions(matcher="builtin"))
    accepted = {
        (candidate.kind, candidate.left.path, candidate.right.path)
        for candidate in report.accepted
    }
    evidence = {
        (candidate.kind, candidate.left.path, candidate.right.path)
        for candidate in report.evidence
    }

    assert ("entity", "pipe", "pipe") in evidence
    assert ("entity", "pipe", "pipe") not in accepted
    assert ("field", "pipe.length", "pipe.length") in accepted
    assert ("field", "pipe.radius", "pipe.radius") in accepted


def test_render_alignment_module_is_parseable_and_temporal():
    left = parse("""
module left_pipe

type Pipe = { length: rat }

entity pipe: Pipe
""")
    right = parse("""
module right_pipe

type Pipe = { length: rat }

entity pipe: Pipe
""")
    report = align_modules(left, right, AlignmentOptions(matcher="builtin"))

    source = format_module(render_alignment_module(report))
    diagnostics = lint_source(source)
    codes = {diagnostic.code for diagnostic in diagnostics}

    assert parse(source).name == "alignment_left_pipe_right_pipe"
    assert "# align kind=" in source
    assert "@ align" not in source
    assert "rule O alignment_001:" in source
    assert "left_pipe.pipe.length = right_pipe.pipe.length always" in source
    assert "left_pipe.pipe = right_pipe.pipe always" not in source
    assert "rule-without-temporal" not in codes


def test_aligner_rejects_terminal_type_mismatches():
    left = parse("""
module left

type User = { id: string, active: bool }

entity user: User
""")
    right = parse("""
module right

type User = { id: int, active: string }

entity user: User
""")

    report = align_modules(left, right, AlignmentOptions(matcher="builtin"))
    source = format_module(render_alignment_module(report))

    assert not report.accepted
    assert "left.user.id = right.user.id" not in source
    assert "left.user.active = right.user.active" not in source


def test_aligner_does_not_render_entity_equality_for_disjoint_records():
    left = parse("""
module left

type Account = { balance: rat, currency: string }

entity account: Account
""")
    right = parse("""
module right

type Account = { country: string, owner: string }

entity account: Account
""")

    report = align_modules(left, right, AlignmentOptions(matcher="builtin"))
    source = format_module(render_alignment_module(report))

    assert "left.account = right.account always" not in source
    assert all(candidate.kind == "field" for candidate in report.accepted)


def test_aligner_accepts_abbreviated_terminal_names():
    left = parse("""
module left

type Geo = { latitude: rat, longitude: rat, radius: rat }

entity shape: Geo
""")
    right = parse("""
module right

type Geo = { lat: rat, lon: rat, r: rat }

entity shape: Geo
""")

    report = align_modules(left, right, AlignmentOptions(matcher="builtin"))
    accepted = {
        (candidate.left.path, candidate.right.path) for candidate in report.accepted
    }

    assert ("shape.latitude", "shape.lat") in accepted
    assert ("shape.longitude", "shape.lon") in accepted
    assert ("shape.radius", "shape.r") in accepted


def test_render_alignment_module_wraps_adt_payload_alignment_in_case():
    left_source = """
module left

type State = Active(value: int) | Other(unit)

entity state: State
"""
    right_source = """
module right

type Phase = Active(value: int) | Other(unit)

entity state: Phase
"""
    report = align_sources(
        [left_source, right_source],
        options=AlignmentOptions(matcher="builtin"),
    )

    source = format_module(render_alignment_module(report))
    diagnostics = lint_source(
        source,
        path="alignment.mdl",
        documents={"left.mdl": left_source, "right.mdl": right_source},
    )

    assert "case left.state:" in source
    assert "| left.State.Active(left_1):" in source
    assert "| right.Phase.Active(right_1): left_1 = right_1" in source
    assert parse(source).name == "alignment_left_right"
    assert parse(format_module(parse(source))).name == "alignment_left_right"
    assert not any(diagnostic.severity == "error" for diagnostic in diagnostics)


def test_auto_calibrates_low_external_scores_with_builtin_terminal_validation():
    left = parse("""
module pipe_spec

type Pipe = { length: rat, radius: rat }

entity pipe: Pipe
""")
    right = parse("""
module tube

type Tube = { length: rat, r: rat }

entity tube: Tube
""")

    report = align_modules(left, right, AlignmentOptions(matcher="auto"))
    accepted = {
        (candidate.left.path, candidate.right.path) for candidate in report.accepted
    }

    assert ("pipe.length", "tube.length") in accepted
    assert ("pipe.radius", "tube.r") in accepted


def test_cli_align_writes_module_and_report(tmp_path):
    left = tmp_path / "left.mdl"
    right = tmp_path / "right.mdl"
    out = tmp_path / "alignment.mdl"
    report_path = tmp_path / "alignment.json"
    left.write_text(
        "module left_pipe\n\ntype Pipe = { length: rat }\n\nentity pipe: Pipe\n",
        encoding="utf-8",
    )
    right.write_text(
        "module right_pipe\n\ntype Pipe = { length: rat }\n\nentity pipe: Pipe\n",
        encoding="utf-8",
    )

    code = main(
        [
            "align",
            str(left),
            str(right),
            "--matcher",
            "builtin",
            "--output",
            str(out),
            "--report",
            str(report_path),
        ]
    )

    assert code == 0
    assert "module alignment_left_pipe_right_pipe" in out.read_text(encoding="utf-8")
    payload = json.loads(report_path.read_text(encoding="utf-8"))
    assert payload["accepted"]


def test_cli_align_accepts_mdl_py(tmp_path, capsys):
    left = tmp_path / "pipe.mdl.py"
    right = tmp_path / "tube.mdl"
    left.write_text(PIPE_DSL.strip() + "\n", encoding="utf-8")
    right.write_text(TUBE_SOURCE.strip() + "\n", encoding="utf-8")

    code = main(
        [
            "align",
            str(left),
            str(right),
            "--matcher",
            "builtin",
        ]
    )

    assert code == 0
    assert "module alignment_pipe_spec_tube" in capsys.readouterr().out
