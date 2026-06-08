import pytest

from mdl import ast as A
from mdl.dsl import PythonDslError, compile_source, to_source
from mdl.parser import parse
from mdl.printer import format_module


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
