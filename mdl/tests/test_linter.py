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

import "pipe.mdl" as m1

rule O ok: m1.pipe.length > 0 always
rule O bad: m1.pipe.radius > 0 always
'''

    diagnostics = lint_source(source, path=str(current))

    assert not any("m1.pipe.length" in d.message for d in diagnostics)
    assert any(d.code == "unknown-field" and "radius" in d.message for d in diagnostics)


def test_linter_checks_record_constructor_fields():
    diagnostics = lint_source('''
module records

type Pipe = { length: rat, radius: rat }

val ok: Pipe = Pipe { length = 1, radius = 2 }
val missing: Pipe = Pipe { length = 1 }
val extra: Pipe = Pipe { length = 1, radius = 2, diameter = 3 }
''')

    assert any(d.code == "missing-record-field" and "radius" in d.message for d in diagnostics)
    assert any(d.code == "unknown-field" and "diameter" in d.message for d in diagnostics)


def test_linter_resolves_exposed_imported_type(tmp_path):
    imported = tmp_path / "pipe_model.mdl"
    imported.write_text('''
module pipe_spec

type Pipe = { length: rat }
''', encoding="utf-8")
    current = tmp_path / "consumer.mdl"
    source = '''
module consumer

import "pipe_model.mdl" exposing (Pipe)

entity pipe: Pipe
rule O ok: pipe.length > 0 always
'''

    diagnostics = lint_source(source, path=str(current))

    assert not any(d.severity == "error" for d in diagnostics)


def test_linter_requires_std_import_for_collections():
    diagnostics = lint_source('''
module collections

entity xs: List<int>
rule O bad: List.Empty(()) = List.Empty(()) always
''')

    assert any(d.code == "undefined-type" and "List" in d.message for d in diagnostics)
    assert any(d.code == "undefined-name" and "List.Empty" in d.message for d in diagnostics)


def test_linter_resolves_std_collection_imports():
    diagnostics = lint_source('''
module collections

import "std/collections/list.mdl" as List exposing (List)
import "std/collections/set.mdl" as Set exposing (Set)
import "std/collections/map.mdl" as Map exposing (Map)
import "std/collections/option.mdl" as Option exposing (Option)

entity xs: List<int>
entity ys: Set<int>
entity lookup: Map<string, int>
entity maybe: Option<int>

rule O ok: List.Empty(()) = List.Empty(()) always
''')

    assert not any(d.severity == "error" for d in diagnostics)


def test_linter_reports_function_return_type_mismatch():
    diagnostics = lint_source('''
module bad

func wrong() -> int:
    true
''')

    assert any(d.code == "type-mismatch" and "expected int, got bool" in d.message for d in diagnostics)


def test_linter_reports_value_and_let_annotation_mismatches():
    diagnostics = lint_source('''
module bad

val wrong: int = "x"

func also_wrong() -> int:
    let x: int = true
    x
''')

    assert sum(1 for d in diagnostics if d.code == "type-mismatch") >= 2


def test_linter_reports_non_bool_formula_positions():
    diagnostics = lint_source('''
module bad

entity x: int

rule O bad_rule: x always
assert x
fact x
''')

    assert sum(1 for d in diagnostics if d.code == "non-bool-expression") >= 3


def test_linter_reports_if_type_errors():
    diagnostics = lint_source('''
module bad

func bad_condition() -> int:
    if 1 then 2 else 3

func bad_branches() -> int:
    if true then 2 else false
''')
    codes = [d.code for d in diagnostics]

    assert "non-bool-expression" in codes
    assert "type-mismatch" in codes


def test_linter_reports_call_arity_and_argument_type_mismatches():
    diagnostics = lint_source('''
module bad

func f(x: int) -> int:
    x

event changed(x: int)

entity y: int
rule O missing_arg: y = f() always
rule O extra_arg: y = f(1, 2) always
rule O wrong_arg: y = f(true) always
rule O event_missing: changed() always
rule O event_wrong: changed(true) always
''')

    assert sum(1 for d in diagnostics if d.code == "arity-mismatch") >= 3
    assert any(d.code == "type-mismatch" and "expected int, got bool" in d.message for d in diagnostics)


def test_linter_reports_record_field_and_numeric_operand_type_mismatches():
    diagnostics = lint_source('''
module bad

type Pipe = { length: int }

val p: Pipe = Pipe { length = true }
entity x: int
rule O bad_numeric: x = (true + 1) always
''')

    assert any(d.code == "type-mismatch" and "expected int, got bool" in d.message for d in diagnostics)
    assert any(d.code == "non-numeric-expression" for d in diagnostics)


def test_linter_preserves_alignment_record_comparison_and_numeric_widening():
    diagnostics = lint_source('''
module ok

type Pipe = { length: rat, radius: rat }
type Tube = { length: rat, r: rat }

entity pipe: Pipe
entity tube: Tube
val length: rat = 1

rule O aligned: (pipe = tube) always
''')

    assert not any(d.severity == "error" for d in diagnostics)
