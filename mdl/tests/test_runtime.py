from mdl.parser import parse
from mdl.runtime import Runtime

from sample_sources import EMAIL_SOURCE, PIPE_SOURCE


def test_runtime_evaluates_recursive_function():
    module = parse(EMAIL_SOURCE)
    runtime = Runtime(module)
    runtime.values["email"] = "a@b"
    assert runtime.eval_source_expr("email_is_correct(email)") is True
    runtime.values["email"] = "a@b@c"
    assert runtime.eval_source_expr("email_is_correct(email)") is False


def test_runtime_record_fact_and_field_access():
    module = parse(PIPE_SOURCE)
    runtime = Runtime(module)
    assert runtime.eval_source_expr("pipe.length > 0") is True
