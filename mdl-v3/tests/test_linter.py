from mdl.linter import lint_source


def test_linter_duplicate_rule_and_missing_temporal_warning():
    source = '''
module bad

entity email: string

rule O r: email = "a"
rule O r: email = "b" always
'''
    diagnostics = lint_source(source)
    codes = {d.code for d in diagnostics}
    assert "duplicate-name" in codes or "duplicate-rule" in codes
    assert "rule-without-temporal" in codes


def test_linter_parse_error():
    diagnostics = lint_source('module broken\nfunc x( -> bool: true\n')
    assert diagnostics
    assert diagnostics[0].code == "parse-error"


def test_linter_reports_use_before_definition_and_unknown_field():
    diagnostics = lint_source('''
module bad

rule O before_entity: later always

type Pipe = { length: rat }
entity pipe: Pipe
rule O missing_field: pipe.radius > 0 always

entity later: bool
''')
    codes = {d.code for d in diagnostics}
    assert "undefined-name" in codes
    assert "unknown-field" in codes


def test_linter_reports_type_use_before_definition():
    diagnostics = lint_source('''
module bad

entity pipe: Pipe
type Pipe = { length: rat }
''')
    assert any(d.code == "undefined-type" and "Pipe" in d.message for d in diagnostics)


def test_linter_allows_self_recursion_but_rejects_forward_function_use():
    diagnostics = lint_source('''
module funcs

func fib(n: int) -> int:
    if n <= 2 then 1 else fib(n - 1) + fib(n - 2)

func caller() -> int:
    later()

func later() -> int:
    1
''')
    undefined = [d for d in diagnostics if d.code == "undefined-name"]
    assert len(undefined) == 1
    assert "later" in undefined[0].message


def test_linter_resolves_import_alias_fields(tmp_path):
    imported = tmp_path / "pipe.mdl"
    imported.write_text('''
module pipe_spec

type Pipe = { length: rat }
entity pipe: Pipe
''', encoding="utf-8")
    current = tmp_path / "alignment.mdl"
    source = '''
module alignment

import pipe_spec as m1

rule O ok: m1.pipe.length > 0 always
rule O bad: m1.pipe.radius > 0 always
'''

    diagnostics = lint_source(source, path=str(current))

    assert not any("m1.pipe.length" in d.message for d in diagnostics)
    assert any(d.code == "unknown-field" and "radius" in d.message for d in diagnostics)


def test_linter_resolves_exposed_imported_type(tmp_path):
    imported = tmp_path / "pipe_model.mdl"
    imported.write_text('''
module pipe_spec

type Pipe = { length: rat }
''', encoding="utf-8")
    current = tmp_path / "consumer.mdl"
    source = '''
module consumer

import pipe_spec exposing (Pipe)

entity pipe: Pipe
rule O ok: pipe.length > 0 always
'''

    diagnostics = lint_source(source, path=str(current))

    assert not any(d.severity == "error" for d in diagnostics)
