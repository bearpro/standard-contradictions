from pathlib import Path

from mdl import ast as A
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
