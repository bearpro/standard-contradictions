from pathlib import Path

from mdl.parser import parse
from mdl.runtime import Runtime


def test_runtime_evaluates_recursive_function():
    module = parse((Path(__file__).resolve().parents[1] / "examples" / "email.mdl").read_text(encoding="utf-8"))
    runtime = Runtime(module)
    runtime.values["email"] = "a@b"
    assert runtime.eval_source_expr("email_is_correct(email)") is True
    runtime.values["email"] = "a@b@c"
    assert runtime.eval_source_expr("email_is_correct(email)") is False


def test_runtime_record_fact_and_field_access():
    module = parse((Path(__file__).resolve().parents[1] / "examples" / "pipe.mdl").read_text(encoding="utf-8"))
    runtime = Runtime(module)
    assert runtime.eval_source_expr("pipe.length > 0") is True
