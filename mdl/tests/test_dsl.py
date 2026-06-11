import pytest

from mdl import ast as A
from mdl.dsl import PythonDslError, compile_source, to_source
from mdl.parser import parse
from mdl.printer import format_module

from .sample_sources import PIPE_SOURCE


PIPE_DSL = '''
from mdl.dsl import *

module("pipe_spec")

@record
class Pipe:
    length: Rat
    radius: Rat

pipe = entity(Pipe)

@rule(O)
def pipe_length_positive():
    return always(pipe.length > 0)

fact(pipe == Pipe(length=10, radius=2))
'''


def without_locations(value):
    if isinstance(value, dict):
        return {
            k: without_locations(v)
            for k, v in value.items()
            if k not in {"line", "column", "end_line", "end_column", "source_span"}
        }
    if isinstance(value, list):
        return [without_locations(item) for item in value]
    return value


def test_python_dsl_compiles_pipe_model_to_canonical_mdl():
    module = compile_source(PIPE_DSL, filename="pipe.py")

    assert module.name == "pipe_spec"
    assert isinstance(module.declarations[0], A.TypeDecl)
    assert isinstance(module.declarations[1], A.EntityDecl)
    assert isinstance(module.declarations[2], A.RuleDecl)
    assert isinstance(module.declarations[3], A.FactDecl)

    source = format_module(module)
    assert "type Pipe = { length: rat, radius: rat }" in source
    assert "entity pipe: Pipe" in source
    assert "rule O pipe_length_positive:" in source
    assert "fact pipe = Pipe { length = 10, radius = 2 }" in source
    assert parse(source).name == "pipe_spec"


def test_python_dsl_stdlib_types_are_valid_python_annotations():
    from fractions import Fraction
    from types import NoneType
    from typing import get_args

    from mdl.dsl import Bool, Decimal, Int, Rat, String, Unit, record

    @record
    class RuntimePipe:
        enabled: Bool
        length: Rat
        count: Int
        score: Decimal
        label: String
        marker: Unit

    assert Bool is bool
    assert Int is int
    assert set(get_args(Rat)) == {int, Fraction}
    assert Decimal is float
    assert String is str
    assert Unit is NoneType
    assert RuntimePipe.__annotations__ == {
        "enabled": bool,
        "length": Rat,
        "count": int,
        "score": float,
        "label": str,
        "marker": NoneType,
    }

    pipe = RuntimePipe(
        enabled=True,
        length=Fraction(1, 2),
        count=1,
        score=0.5,
        label="pipe",
        marker=None,
    )

    assert pipe.length == Fraction(1, 2)


def test_python_dsl_pipe_model_matches_textual_mdl_ast():
    dsl_module = compile_source(PIPE_DSL, filename="pipe.py")
    dsl_reparsed = parse(format_module(dsl_module))
    textual_module = parse(PIPE_SOURCE)

    assert without_locations(A.node_to_dict(dsl_reparsed)) == without_locations(A.node_to_dict(textual_module))


def test_python_dsl_supports_module_metadata_imports_opens_types_values_and_targeted_facts():
    source = '''
from mdl.dsl import *

module("typed_model", annotations=["source generated", "# raw annotation"])
import_("./pipe.mdl")
open_(std.collections)

@record("Envelope")
class EnvelopeDraft:
    labels: std.collections.List[String]
    coordinates: (Int, Rat)
    payload: "std.collections.List<string>"

message_count: Int = value(3)
envelope = entity(Envelope)

fact(target="message_count", value=3)
'''

    module = compile_source(source, filename="typed_model.py")
    rendered = format_module(module)
    reparsed = parse(rendered)

    assert module.annotations == ["source generated", "# raw annotation"]
    assert module.imports[0].path == "./pipe.mdl"
    assert module.opens[0].module == "std.collections"
    assert "type Envelope = {" in rendered
    assert "labels: std.collections.List<string>" in rendered
    assert "coordinates: (int, rat)" in rendered
    assert "payload: std.collections.List<string>" in rendered
    assert "let message_count: int = 3" in rendered
    assert "entity envelope: Envelope" in rendered
    assert "fact message_count = 3" in rendered
    assert reparsed.name == "typed_model"
    assert reparsed.imports[0].path == "./pipe.mdl"
    assert reparsed.opens[0].module == "std.collections"


def test_python_dsl_supports_predicates_boolops_temporal_rules_and_if_statements():
    source = '''
from mdl.dsl import *

module("email")

email = entity(String)

@predicate
def email_is_correct(email: String):
    return contains(email, "@") and not contains(email, " ")

@function
def normalize_score(score: Rat) -> Rat:
    if score > 1:
        return 1
    else:
        return score

@rule(F)
def malformed_email_received():
    return eventually(email_received(email) and not email_is_correct(email))
'''

    module = compile_source(source, filename="email.py")
    rendered = format_module(module)

    predicate = module.declarations[1]
    assert isinstance(predicate, A.FuncDecl)
    assert isinstance(predicate.return_type, A.TypeRef)
    assert predicate.return_type.name == "bool"
    assert "func normalize_score(score: rat) -> rat:" in rendered
    assert "rule F malformed_email_received:" in rendered
    assert parse(rendered).name == "email"


def test_python_dsl_supports_temporal_binary_implies_and_rule_metadata():
    source = '''
from mdl.dsl import *

module("temporal")

started = entity(Bool)
running = entity(Bool)
done = entity(Bool)
fallback = entity(Bool)

@rule(F, name="eventual_completion", strength="strict", when=started, otherwise=fallback)
def completion():
    return implies(started, until(next_(running), eventually(done)))
'''

    module = compile_source(source, filename="temporal.py")
    rendered = format_module(module)
    reparsed = parse(rendered)
    rule = next(decl for decl in module.declarations if isinstance(decl, A.RuleDecl))

    assert rule.name == "eventual_completion"
    assert rule.modality == "F"
    assert rule.strength == "strict"
    assert isinstance(rule.antecedent, A.Name)
    assert isinstance(rule.otherwise, A.Name)
    assert "strict rule F eventual_completion when started:" in rendered
    assert "started implies (running next) until (done eventually)" in rendered
    assert reparsed.name == "temporal"
    assert isinstance(rule.body, A.BinaryOp)
    assert rule.body.op == "implies"
    assert isinstance(rule.body.right, A.TemporalBinary)
    assert rule.body.right.op == "until"


def test_python_dsl_supports_chained_comparisons_as_boolean_conjunctions():
    source = '''
from mdl.dsl import *

module("ranges")

score = entity(Rat)

@predicate
def valid_score(score: Rat):
    return 0 <= score <= 1
'''

    module = compile_source(source, filename="ranges.py")
    rendered = format_module(module)
    predicate = next(decl for decl in module.declarations if isinstance(decl, A.FuncDecl))

    assert predicate.body is not None
    assert predicate.body.result is not None
    assert isinstance(predicate.body.result, A.BinaryOp)
    assert predicate.body.result.op == "and"
    assert "0 <= score and score <= 1" in rendered
    assert parse(rendered).name == "ranges"


def test_python_dsl_supports_decorator_entity_form():
    source = '''
from mdl.dsl import *

module("pipe_spec")

@record
class Pipe:
    length: Rat

@entity(Pipe)
def pipe():
    pass
'''

    module = compile_source(source)

    assert isinstance(module.declarations[1], A.EntityDecl)
    assert module.declarations[1].name == "pipe"


def test_python_dsl_to_source_helper_returns_mdl_source():
    source = to_source(PIPE_DSL, filename="pipe.py")

    assert source.startswith("module pipe_spec")
    assert parse(source).name == "pipe_spec"


def test_python_dsl_rejects_unsupported_top_level_statements():
    with pytest.raises(PythonDslError, match="unsupported top-level statement For"):
        compile_source('''
from mdl.dsl import *
module("bad")
for item in []:
    pass
''')
