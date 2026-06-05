import json

from mdl.aligner import AlignmentOptions, align_modules, align_sources, project_module, render_alignment_module
from mdl.cli import main
from mdl.linter import lint_source
from mdl.parser import parse
from mdl.printer import format_module


def test_aligner_suggests_same_entity_name():
    a = 'module a\nentity email: string\n'
    b = 'module b\nentity email: string\n'
    report = align_sources([a, b])
    assert report.suggestions
    assert report.suggestions[0].score == 1.0


def test_project_module_extracts_entity_record_fields():
    module = parse('''
module pipe_spec

type Pipe = { length: rat, radius: rat, dimensions: { width: int, height: int } }

entity pipe: Pipe
''')

    paths = {element.path for element in project_module(module)}

    assert "pipe" in paths
    assert "pipe.length" in paths
    assert "pipe.radius" in paths
    assert "pipe.dimensions" in paths
    assert "pipe.dimensions.width" in paths
    assert "pipe.dimensions.height" in paths


def test_builtin_aligner_accepts_entities_and_fields():
    left = parse('''
module left_pipe

type Pipe = { length: rat, radius: rat }

entity pipe: Pipe
''')
    right = parse('''
module right_pipe

type PipeRecord = { length: rat, radius: rat }

entity pipe: PipeRecord
''')

    report = align_modules(left, right, AlignmentOptions(matcher="builtin"))
    accepted = {(candidate.kind, candidate.left.path, candidate.right.path) for candidate in report.accepted}

    assert ("entity", "pipe", "pipe") in accepted
    assert ("field", "pipe.length", "pipe.length") in accepted
    assert ("field", "pipe.radius", "pipe.radius") in accepted


def test_render_alignment_module_is_parseable_and_temporal():
    left = parse('''
module left_pipe

type Pipe = { length: rat }

entity pipe: Pipe
''')
    right = parse('''
module right_pipe

type Pipe = { length: rat }

entity pipe: Pipe
''')
    report = align_modules(left, right, AlignmentOptions(matcher="builtin"))

    source = format_module(render_alignment_module(report))
    diagnostics = lint_source(source)
    codes = {diagnostic.code for diagnostic in diagnostics}

    assert parse(source).name == "alignment_left_pipe_right_pipe"
    assert "# align kind=" in source
    assert "@ align" not in source
    assert "rule O alignment_001:" in source
    assert "left_pipe.pipe = right_pipe.pipe always" in source
    assert "rule-without-temporal" not in codes


def test_cli_align_writes_module_and_report(tmp_path):
    left = tmp_path / "left.mdl"
    right = tmp_path / "right.mdl"
    out = tmp_path / "alignment.mdl"
    report_path = tmp_path / "alignment.json"
    left.write_text("module left_pipe\n\ntype Pipe = { length: rat }\n\nentity pipe: Pipe\n", encoding="utf-8")
    right.write_text("module right_pipe\n\ntype Pipe = { length: rat }\n\nentity pipe: Pipe\n", encoding="utf-8")

    code = main([
        "align",
        str(left),
        str(right),
        "--matcher",
        "builtin",
        "--output",
        str(out),
        "--report",
        str(report_path),
    ])

    assert code == 0
    assert "module alignment_left_pipe_right_pipe" in out.read_text(encoding="utf-8")
    payload = json.loads(report_path.read_text(encoding="utf-8"))
    assert payload["accepted"]
