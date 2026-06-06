from mdl.core import translate
from mdl.parser import parse

from .sample_sources import EMAIL_SOURCE


def test_translate_rule_to_core():
    module = parse(EMAIL_SOURCE)
    core = translate(module)
    rule = next(r for r in core["rules"] if r["name"] == "email_addr_spec_correct")
    assert rule["modality"] == "O"
    assert rule["body"]["op"] == "G"
    assert rule["body"]["arg"]["op"] == "atom"
    assert core["atoms"]


def test_translate_email_rule_only_uses_entities_and_functions():
    module = parse(EMAIL_SOURCE)
    core = translate(module)
    assert "events" not in core
    assert [rule["name"] for rule in core["rules"]] == ["email_addr_spec_correct"]
