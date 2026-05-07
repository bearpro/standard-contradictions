from mdl.linter import lint_source


def test_linter_duplicate_rule_and_missing_temporal_warning():
    source = '''
module bad

entity email: string

rule O r = email = "a"
rule O r = email = "b" always
'''
    diagnostics = lint_source(source)
    codes = {d.code for d in diagnostics}
    assert "duplicate-name" in codes or "duplicate-rule" in codes
    assert "rule-without-temporal" in codes


def test_linter_parse_error():
    diagnostics = lint_source('module broken\nfunc x( -> bool: true\n')
    assert diagnostics
    assert diagnostics[0].code == "parse-error"
