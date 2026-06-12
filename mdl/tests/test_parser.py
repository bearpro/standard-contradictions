from mdl import ast as A
from mdl.diagnostics import ParseError
from mdl.parser import parse, parse_expr, parse_type_expr_source
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
    assert any(isinstance(d, A.RuleDecl) and d.name == "email_addr_spec_correct" for d in module.declarations)


def test_event_is_not_a_declaration_keyword():
    module = parse("""
module names

entity event: bool
""")

    entity = next(decl for decl in module.declarations if isinstance(decl, A.EntityDecl))
    assert entity.name == "event"

    try:
        parse("""
module bad

event started()
""")
    except ParseError:
        pass
    else:  # pragma: no cover - defensive
        raise AssertionError("event declaration unexpectedly parsed")


def test_parse_temporal_postfix_grouped_atom():
    expr = parse_expr('(email_received and not email_is_correct(email)) eventually')
    assert isinstance(expr, A.TemporalUnary)
    assert expr.op == "eventually"
    assert isinstance(expr.operand, A.BinaryOp)


def test_parse_now_as_temporal_postfix():
    expr = parse_expr("email_received now")
    assert isinstance(expr, A.TemporalUnary)
    assert expr.op == "now"
    assert isinstance(expr.operand, A.Name)


def test_parse_implies_is_right_associative_below_or():
    expr = parse_expr("a or b implies c implies d")
    assert isinstance(expr, A.BinaryOp)
    assert expr.op == "implies"
    assert isinstance(expr.left, A.BinaryOp)
    assert expr.left.op == "or"
    assert isinstance(expr.right, A.BinaryOp)
    assert expr.right.op == "implies"


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


def assert_parse_error(source, parser=parse):
    try:
        parser(source)
    except ParseError:
        pass
    else:  # pragma: no cover - defensive
        raise AssertionError(f"source unexpectedly parsed: {source!r}")


def test_priority_is_an_identifier_and_override_declares_rule_priority():
    module = parse("""
module names

entity priority: bool
rule O must: priority always
rule F forbid: priority always
override forbid > must
""")

    entity = next(decl for decl in module.declarations if isinstance(decl, A.EntityDecl))
    priority = next(decl for decl in module.declarations if isinstance(decl, A.PriorityDecl))
    assert entity.name == "priority"
    assert priority.chain == ["forbid", "must"]


def test_defeasible_rule_strength_remains_optional():
    explicit = parse("""
module rules

defeasible rule O r: x always
""")
    implicit = parse("""
module rules

rule O r: x always
""")

    assert without_locations(A.node_to_dict(explicit)) == without_locations(A.node_to_dict(implicit))


def test_forbidden_syntax_aliases_are_rejected():
    for source, parser in [
        ("module bad\n\npriority high > low\n", parse),
        ("a == b", parse_expr),
        ("always x", parse_expr),
        ("eventually x", parse_expr),
        ("next x", parse_expr),
        ("now x", parse_expr),
        ("x initially", parse_expr),
        ("x weak_next", parse_expr),
        ("x never", parse_expr),
        ("a release b", parse_expr),
        ("a weak_until b", parse_expr),
    ]:
        assert_parse_error(source, parser)


def test_trailing_commas_are_rejected():
    for source, parser in [
        ("module bad\n\ntype Box<T,> = { value: T }\n", parse),
        ("module bad\n\ntype State = Local(unit,)\n", parse),
        ("module bad\n\ntype Pipe = { length: rat, }\n", parse),
        ("module bad\n\nfunc f(x: int,) -> int: x\n", parse),
        ("Pipe { length = 1, }", parse_expr),
        ("f(1,)", parse_expr),
        ("case x:\n    | Box(value,): value", parse_expr),
        ("case x:\n    | { value, }: value", parse_expr),
    ]:
        assert_parse_error(source, parser)


def test_format_expr_preserves_precedence_round_trip():
    for source in [
        "a * (b + c)",
        "(a or b) and c",
        "not (a and b)",
        "a implies (b implies c)",
        "(a and b) or c",
        "(a + b).field",
        "(a and b) eventually",
        "(a and b) now",
        "(if a then b else c) always",
        "(let x = true in x) always",
    ]:
        expr = parse_expr(source)
        reparsed = parse_expr(format_expr(expr))
        assert without_locations(A.node_to_dict(reparsed)) == without_locations(A.node_to_dict(expr))


def test_format_expr_prints_temporal_unary_as_postfix():
    expr = A.TemporalUnary(op="always", operand=A.Name(name="x"), position="prefix")
    assert format_expr(expr) == "x always"


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


def test_parse_tuple_expr_and_type():
    expr = parse_expr("(1, 2)")
    assert isinstance(expr, A.TupleLiteral)
    assert len(expr.items) == 2

    typ = parse_type_expr_source("(int, int)")
    assert isinstance(typ, A.TupleType)
    assert len(typ.items) == 2


def test_parse_non_empty_type_params_and_args():
    module = parse("""
module generics

type Box<T> = { value: T }
entity boxed: Box<int>
""")

    typ = next(decl for decl in module.declarations if isinstance(decl, A.TypeDecl))
    assert typ.params == ["T"]

    entity = next(decl for decl in module.declarations if isinstance(decl, A.EntityDecl))
    assert isinstance(entity.type_annotation, A.TypeRef)
    assert entity.type_annotation.name == "Box"
    assert [arg.name for arg in entity.type_annotation.args if isinstance(arg, A.TypeRef)] == ["int"]


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


def test_parse_rule_body_after_newline():
    module = parse("""
module tmp

entity x1: bool
entity x2: bool

rule O r1:
    x1

rule O r2:
    let nx1 = not x1 in
    x2
""")

    rules = [decl for decl in module.declarations if isinstance(decl, A.RuleDecl)]
    assert [rule.name for rule in rules] == ["r1", "r2"]
    assert isinstance(rules[0].body, A.Name)
    assert isinstance(rules[1].body, A.LetExpr)
    assert isinstance(rules[1].body.body, A.Name)


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
