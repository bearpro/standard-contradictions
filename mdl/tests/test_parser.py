import pytest

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
    assert [opened.module for opened in module.opens] == [
        "std.collections",
        "std.strings",
    ]
    assert any(
        isinstance(d, A.TypeDecl) and d.name == "ProcessingState"
        for d in module.declarations
    )
    assert any(
        isinstance(d, A.FuncDecl) and d.name == "process_email"
        for d in module.declarations
    )
    assert any(
        isinstance(d, A.EntityDecl) and d.name == "email" for d in module.declarations
    )
    assert any(
        isinstance(d, A.RuleDecl) and d.name == "email_addr_spec_correct"
        for d in module.declarations
    )


def test_event_is_plain_identifier():
    module = parse("""
module names

entity event: bool
""")

    entity = next(
        decl for decl in module.declarations if isinstance(decl, A.EntityDecl)
    )
    assert entity.name == "event"


def test_parse_temporal_postfix_grouped_atom():
    expr = parse_expr("(email_received and not email_is_correct(email)) eventually")
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


def test_parse_block_sum_type_variants():
    module = parse("""
module variants

type UnionBlock =
    | A(int)
    | B(string)

entity value: UnionBlock
""")

    type_decl = next(
        decl for decl in module.declarations if isinstance(decl, A.TypeDecl)
    )
    entity = next(
        decl for decl in module.declarations if isinstance(decl, A.EntityDecl)
    )

    assert isinstance(type_decl.definition, A.SumType)
    assert [variant.name for variant in type_decl.definition.variants] == ["A", "B"]
    assert type_decl.definition.variants[0].fields[0][0] is None
    assert isinstance(type_decl.definition.variants[0].fields[0][1], A.TypeRef)
    assert type_decl.definition.variants[0].fields[0][1].name == "int"
    assert entity.name == "value"


def test_block_sum_type_matches_inline_sum_type_ast():
    inline = parse("""
module variants

type Result<T> = Ok(value: T) | Error(message: string)
""")
    block = parse("""
module variants

type Result<T> =
    | Ok(value: T)
    | Error(message: string)
""")

    inline_decl = next(
        decl for decl in inline.declarations if isinstance(decl, A.TypeDecl)
    )
    block_decl = next(
        decl for decl in block.declarations if isinstance(decl, A.TypeDecl)
    )

    assert without_locations(A.node_to_dict(block_decl)) == without_locations(
        A.node_to_dict(inline_decl)
    )


def without_locations(value):
    if isinstance(value, dict):
        return {
            k: without_locations(v)
            for k, v in value.items()
            if k not in {"line", "column", "end_line", "end_column"}
        }
    if isinstance(value, list):
        return [without_locations(item) for item in value]
    return value


def test_priority_is_an_identifier_and_override_declares_rule_priority():
    module = parse("""
module names

entity priority: bool
rule O must: priority always
rule F forbid: priority always
override forbid > must
""")

    entity = next(
        decl for decl in module.declarations if isinstance(decl, A.EntityDecl)
    )
    priority = next(
        decl for decl in module.declarations if isinstance(decl, A.PriorityDecl)
    )
    assert entity.name == "priority"
    assert priority.chain == ["forbid", "must"]


def test_last_is_an_identifier():
    module = parse("""
module names

entity last: bool
rule O must: last now
""")

    entity = next(
        decl for decl in module.declarations if isinstance(decl, A.EntityDecl)
    )
    rule = next(decl for decl in module.declarations if isinstance(decl, A.RuleDecl))

    assert entity.name == "last"
    assert isinstance(rule.body, A.TemporalUnary)
    assert isinstance(rule.body.operand, A.Name)
    assert rule.body.operand.name == "last"


def test_defeasible_rule_strength_remains_optional():
    explicit = parse("""
module rules

defeasible rule O r: x always
""")
    implicit = parse("""
module rules

rule O r: x always
""")

    assert without_locations(A.node_to_dict(explicit)) == without_locations(
        A.node_to_dict(implicit)
    )


def test_anonymous_rule_syntax_is_rejected():
    with pytest.raises(ParseError):
        parse("""
module rules

rule O: x always
""")


def test_fact_assignment_syntax_is_an_equality_expression():
    module = parse("""
module facts

entity x: bool
fact x = true
""")

    fact = next(decl for decl in module.declarations if isinstance(decl, A.FactDecl))
    assert isinstance(fact.value, A.BinaryOp)
    assert fact.value.op == "="
    assert isinstance(fact.value.left, A.Name)
    assert fact.value.left.name == "x"


@pytest.mark.parametrize(
    ("source", "op"),
    [
        ("x always", "always"),
        ("x eventually", "eventually"),
        ("x next", "next"),
        ("x now", "now"),
    ],
)
def test_parse_temporal_unary_postfix_operators(source, op):
    expr = parse_expr(source)

    assert isinstance(expr, A.TemporalUnary)
    assert expr.op == op
    assert expr.position == "postfix"


def test_parse_temporal_until_binary_operator():
    expr = parse_expr("a until b")

    assert isinstance(expr, A.TemporalBinary)
    assert expr.op == "until"
    assert isinstance(expr.left, A.Name)
    assert isinstance(expr.right, A.Name)


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
        assert without_locations(A.node_to_dict(reparsed)) == without_locations(
            A.node_to_dict(expr)
        )


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


def test_parse_and_format_let_expr_type_annotation():
    expr = parse_expr("let x: bool = 5 in true")

    assert isinstance(expr, A.LetExpr)
    assert isinstance(expr.type_annotation, A.TypeRef)
    assert expr.type_annotation.name == "bool"
    assert format_expr(expr) == "let x: bool = 5 in true"

    reparsed = parse_expr(format_expr(expr))
    assert without_locations(A.node_to_dict(reparsed)) == without_locations(
        A.node_to_dict(expr)
    )


def test_parse_block_let_with_indented_rhs():
    module = parse("""
module lets

func f() -> bool:
    let x =
        1
    true
""")

    func = next(decl for decl in module.declarations if isinstance(decl, A.FuncDecl))
    assert func.body is not None
    stmt = func.body.statements[0]
    assert isinstance(stmt.value, A.Literal)
    assert stmt.value.value == 1
    assert isinstance(func.body.result, A.Literal)
    assert func.body.result.value is True


def test_parse_parenthesized_multiline_case_in_block_let_rhs():
    module = parse("""
module lets

func f(x: int) -> bool:
    let result = (
        case x:
            | 1: true
            | _: false
    )
    result
""")

    func = next(decl for decl in module.declarations if isinstance(decl, A.FuncDecl))
    assert func.body is not None
    stmt = func.body.statements[0]
    assert isinstance(stmt.value, A.MatchExpr)
    assert len(stmt.value.arms) == 2
    assert isinstance(func.body.result, A.Name)
    assert func.body.result.name == "result"


def test_parse_multiline_case_as_block_let_rhs():
    module = parse("""
module lets

func f(x: int) -> bool:
    let result =
        case x:
            | 1: true
            | _: false
    result
""")

    func = next(decl for decl in module.declarations if isinstance(decl, A.FuncDecl))
    assert func.body is not None
    stmt = func.body.statements[0]
    assert isinstance(stmt.value, A.MatchExpr)
    assert len(stmt.value.arms) == 2
    assert isinstance(func.body.result, A.Name)
    assert func.body.result.name == "result"


def test_parse_inline_let_with_indented_value_and_body():
    expr = parse_expr("""
let x =
    1
in
    x
""")

    assert isinstance(expr, A.LetExpr)
    assert isinstance(expr.value, A.Literal)
    assert expr.value.value == 1
    assert isinstance(expr.body, A.Name)
    assert expr.body.name == "x"


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

    entity = next(
        decl for decl in module.declarations if isinstance(decl, A.EntityDecl)
    )
    assert isinstance(entity.type_annotation, A.TypeRef)
    assert entity.type_annotation.name == "Box"
    assert [
        arg.name for arg in entity.type_annotation.args if isinstance(arg, A.TypeRef)
    ] == ["int"]


def test_case_arms_must_be_indented_deeper_than_case_expression():
    with pytest.raises(ParseError) as exc_info:
        parse("""
module bad

func is_local(state: bool) -> bool:
    case state:
    | true: true
    | false: false
""")

    assert "case arms must be indented deeper" in exc_info.value.message


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
    module = parse("""
module imports

import "./pipe.mdl"
open std.collections
""")

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

    type_decl = next(
        decl for decl in module.declarations if isinstance(decl, A.TypeDecl)
    )
    assert type_decl.end_line == 3
    assert type_decl.end_column == len("type Pipe = { length: rat, radius: rat }") + 1

    rule = next(decl for decl in module.declarations if isinstance(decl, A.RuleDecl))
    assert rule.body is not None
    assert rule.body.end_column == len("rule O ok: pipe.length > 0 always") + 1


def test_hash_comments_are_supported():
    module = parse("""
module comments

# full-line comment
entity ready: bool # inline comment
entity still_ready: bool # inline comment after a declaration
rule O ok: ready always # trailing comment
""")

    assert module.name == "comments"
    assert any(
        isinstance(d, A.EntityDecl) and d.name == "ready" for d in module.declarations
    )
    assert any(
        isinstance(d, A.EntityDecl) and d.name == "still_ready"
        for d in module.declarations
    )
    assert any(
        isinstance(d, A.RuleDecl) and d.name == "ok" for d in module.declarations
    )
