from mdl import ast as A
from mdl.diagnostics import ParseError
from mdl.parser import parse, parse_expr
from mdl.printer import format_expr

from .sample_sources import EMAIL_SOURCE


def test_parse_email_module_constructs():
    module = parse(EMAIL_SOURCE)
    assert module.name == "email"
    assert module.annotations == ["rfc2822"]
    assert module.imports == []
    assert [opened.module for opened in module.opens] == ["std.collections", "std.system.strings"]
    assert any(isinstance(d, A.TypeDecl) and d.name == "ProcessingState" for d in module.declarations)
    assert any(isinstance(d, A.FuncDecl) and d.name == "process_email" for d in module.declarations)
    assert any(isinstance(d, A.EntityDecl) and d.name == "email" for d in module.declarations)
    assert any(isinstance(d, A.EventDecl) and d.name == "email_received" for d in module.declarations)
    assert any(isinstance(d, A.RuleDecl) and d.name == "email_addr_spec_correct" for d in module.declarations)
    assert any(isinstance(d, A.AlignDecl) for d in module.declarations)


def test_parse_temporal_postfix_grouped_atom():
    expr = parse_expr('(email_received(email) and not email_is_correct(email)) eventually')
    assert isinstance(expr, A.TemporalUnary)
    assert expr.op == "eventually"
    assert isinstance(expr.operand, A.BinaryOp)


def test_parse_record_constructor_syntax():
    module = parse("""
module record

type Complex = { r: rat, i: rat }

func add(a: Complex, b: Complex) -> Complex:
    Complex { r = a.r + b.r, i = a.i + b.i }
""")

    func = next(decl for decl in module.declarations if isinstance(decl, A.FuncDecl))
    assert func.body is not None
    expr = func.body.result
    assert isinstance(expr, A.RecordConstructor)
    assert expr.type_name == "Complex"
    assert [name for name, _ in expr.fields] == ["r", "i"]


def without_locations(value):
    if isinstance(value, dict):
        return {k: without_locations(v) for k, v in value.items() if k not in {"line", "column", "end_line", "end_column"}}
    if isinstance(value, list):
        return [without_locations(item) for item in value]
    return value


def test_format_expr_preserves_precedence_round_trip():
    for source in [
        "a * (b + c)",
        "(a or b) and c",
        "(a implies b) implies c",
        "not (a and b)",
        "(a + b).field",
        "(a and b) eventually",
        "(if a then b else c) always",
        "(let x = true in x) always",
    ]:
        expr = parse_expr(source)
        reparsed = parse_expr(format_expr(expr))
        assert without_locations(A.node_to_dict(reparsed)) == without_locations(A.node_to_dict(expr))


def test_unit_literal_and_final_let_expr_in_function_block():
    expr = parse_expr("()")
    assert isinstance(expr, A.Literal)
    assert expr.kind == "unit"
    assert expr.value is None

    module = parse("""
module lets

func f() -> int:
    let x = 1 in x
""")

    func = next(decl for decl in module.declarations if isinstance(decl, A.FuncDecl))
    assert func.body is not None
    assert func.body.statements == []
    assert isinstance(func.body.result, A.LetExpr)


def test_type_aliases_and_nullary_sum_variants_are_rejected():
    for source in [
        "module bad\ntype T = Existing\n",
        "module bad\ntype T = Only\n",
        "module bad\ntype T = Only()\n",
        "module bad\ntype T = (int, int)\n",
        "module bad\ntype T = A | B(unit)\n",
    ]:
        try:
            parse(source)
        except ParseError:
            pass
        else:  # pragma: no cover - defensive
            raise AssertionError(f"invalid type declaration unexpectedly parsed: {source!r}")


def test_payload_sum_variants_accept_unit_payload_patterns():
    module = parse("""
module states

type State = Local(unit) | Remote(unit)

func is_local(state: State) -> bool:
    case state:
        | State.Local(): true
        | State.Remote(): false
""")

    typ = next(decl for decl in module.declarations if isinstance(decl, A.TypeDecl))
    assert isinstance(typ.definition, A.SumType)
    assert [variant.name for variant in typ.definition.variants] == ["Local", "Remote"]


def test_braced_expressions_are_rejected():
    for source in [
        "{ email_received(email) } eventually",
        "{ r = 1, }",
        "Complex { a.r + b.r }",
    ]:
        try:
            parse_expr(source)
        except ParseError:
            pass
        else:  # pragma: no cover - defensive
            raise AssertionError(f"braced expression unexpectedly parsed: {source!r}")


def test_single_quoted_literals_are_rejected():
    try:
        parse_expr("'x'")
    except ParseError:
        pass
    else:  # pragma: no cover - defensive
        raise AssertionError("single-quoted literal unexpectedly parsed")


def test_parse_quantifier():
    expr = parse_expr('forall pipe in pipes: pipe.length > 0 always')
    assert isinstance(expr, A.QuantifierExpr)
    assert expr.quantifier == "forall"


def test_case_arms_must_be_indented_deeper_than_case_expression():
    try:
        parse("""
module bad

func is_local(state: bool) -> bool:
    case state:
    | true: true
    | false: false
""")
    except ParseError as exc:
        assert "case arms must be indented deeper" in exc.message
    else:  # pragma: no cover - defensive
        raise AssertionError("same-indent case arms unexpectedly parsed")


def test_parse_rule_colon_syntax_with_antecedent():
    module = parse("""
module rules

entity enabled: bool
entity x: bool
rule O applies when enabled: x always
""")

    rule = next(decl for decl in module.declarations if isinstance(decl, A.RuleDecl))
    assert rule.name == "applies"
    assert isinstance(rule.antecedent, A.Name)
    assert rule.antecedent.name == "enabled"
    assert isinstance(rule.body, A.TemporalUnary)


def test_parse_old_rule_equals_syntax_is_rejected():
    try:
        parse("""
module old

entity x: bool
rule O r = x always
""")
    except ParseError as exc:
        assert "expected ':' before rule body" in exc.message
    else:  # pragma: no cover - defensive
        raise AssertionError("old rule syntax unexpectedly parsed")


def test_parse_file_import_path():
    module = parse('''
module imports

import "./pipe.mdl"
open std.collections
''')

    assert module.imports[0].path == "./pipe.mdl"
    assert module.opens[0].module == "std.collections"


def test_parse_nodes_have_antlr_backed_end_spans():
    module = parse("""module spans

type Pipe = { length: rat, radius: rat }
entity pipe: Pipe
rule O ok: pipe.length > 0 always
""")

    assert module.line == 1
    assert module.column == len("module ") + 1
    assert module.end_line == 5
    assert module.end_column == len("rule O ok: pipe.length > 0 always") + 1

    type_decl = next(decl for decl in module.declarations if isinstance(decl, A.TypeDecl))
    assert type_decl.end_line == 3
    assert type_decl.end_column == len("type Pipe = { length: rat, radius: rat }") + 1

    rule = next(decl for decl in module.declarations if isinstance(decl, A.RuleDecl))
    assert rule.body is not None
    assert rule.body.end_column == len("rule O ok: pipe.length > 0 always") + 1


def test_hash_comments_are_supported():
    module = parse('''
module comments

# full-line comment
entity ready: bool # inline comment
entity still_ready: bool # inline comment after a declaration
rule O ok: ready always # trailing comment
''')

    assert module.name == "comments"
    assert any(isinstance(d, A.EntityDecl) and d.name == "ready" for d in module.declarations)
    assert any(isinstance(d, A.EntityDecl) and d.name == "still_ready" for d in module.declarations)
    assert any(isinstance(d, A.RuleDecl) and d.name == "ok" for d in module.declarations)


def test_slash_comments_are_rejected():
    for source in [
        'module bad\n// old comment\nentity x: bool\n',
        'module bad\nentity x: bool /* old block comment */\n',
    ]:
        try:
            parse(source)
        except ParseError:
            pass
        else:  # pragma: no cover - defensive
            raise AssertionError(f"old comment syntax unexpectedly parsed: {source!r}")
