from mdl.builder import ModelBuilder, always, call, ref
from mdl.parser import parse


def test_builder_prints_parseable_source():
    m = ModelBuilder("email")
    m.entity("email", "string")
    m.rule("email_addr_spec_correct", "O", always(call("email_is_correct", ref("email"))))
    source = m.to_source()
    parsed = parse(source)
    assert parsed.name == "email"
    assert "rule O email_addr_spec_correct" in source


def test_builder_from_dict():
    from mdl.builder import from_python

    module = from_python({
        "module": "pipe",
        "types": {"Pipe": "{ length: rat, radius: rat }"},
        "entities": {"pipe": "Pipe"},
    })
    assert module.name == "pipe"
    assert len(module.declarations) == 2
