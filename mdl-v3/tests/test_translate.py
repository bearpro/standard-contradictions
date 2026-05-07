from pathlib import Path

from mdl.core import translate
from mdl.parser import parse


def test_translate_rule_to_core():
    module = parse((Path(__file__).resolve().parents[1] / "examples" / "email.mdl").read_text(encoding="utf-8"))
    core = translate(module)
    rule = next(r for r in core["rules"] if r["name"] == "email_addr_spec_correct")
    assert rule["modality"] == "O"
    assert rule["body"]["op"] == "G"
    assert rule["body"]["arg"]["op"] == "atom"
    assert core["atoms"]


def test_translate_forbidden_event_rule():
    module = parse((Path(__file__).resolve().parents[1] / "examples" / "email.mdl").read_text(encoding="utf-8"))
    core = translate(module)
    rule = next(r for r in core["rules"] if r["name"] == "malformed_email_received")
    assert rule["modality"] == "F"
    assert rule["body"]["op"] == "F"
