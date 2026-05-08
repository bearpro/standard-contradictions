from pathlib import Path

from mdl import ast as A
from mdl.diagnostics import ParseError
from mdl.parser import parse, parse_expr


SAMPLE = Path(__file__).resolve().parents[1] / "examples" / "email.mdl"


def test_parse_email_module_constructs():
    module = parse(SAMPLE.read_text(encoding="utf-8"))
    assert module.name == "email"
    assert module.annotations == ["rfc2822"]
    assert len(module.imports) == 1
    assert any(isinstance(d, A.TypeDecl) and d.name == "ProcessingState" for d in module.declarations)
    assert any(isinstance(d, A.FuncDecl) and d.name == "process_email" for d in module.declarations)
    assert any(isinstance(d, A.EntityDecl) and d.name == "email" for d in module.declarations)
    assert any(isinstance(d, A.EventDecl) and d.name == "email_received" for d in module.declarations)
    assert any(isinstance(d, A.RuleDecl) and d.name == "email_addr_spec_correct" for d in module.declarations)
    assert any(isinstance(d, A.AlignDecl) for d in module.declarations)


def test_parse_temporal_postfix_and_braced_atom():
    expr = parse_expr('{ email_received(email) and not email_is_correct(email) } eventually')
    assert isinstance(expr, A.TemporalUnary)
    assert expr.op == "eventually"
    assert isinstance(expr.operand, A.BracedExpr)


def test_parse_quantifier():
    expr = parse_expr('forall pipe in pipes: pipe.length > 0 always')
    assert isinstance(expr, A.QuantifierExpr)
    assert expr.quantifier == "forall"


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
