from mdl.builder import ModelBuilder, always, call, ref
from mdl.parser import parse


def test_builder_prints_parseable_source():
    m = ModelBuilder("email")
    m.entity("email", "string")
    m.rule("email_addr_spec_correct", "O", always(call("email_is_correct", ref("email"))))
    source = m.to_source()
    parsed = parse(source)
    assert parsed.name == "email"
    assert "rule O email_addr_spec_correct:" in source


def test_builder_from_dict():
    from mdl.builder import from_python

    module = from_python({
        "module": "pipe",
        "types": {"Pipe": "{ length: rat, radius: rat }"},
        "entities": {"pipe": "Pipe"},
    })
    assert module.name == "pipe"
    assert len(module.declarations) == 2


def test_builder_from_dict_rejects_events():
    from mdl.builder import from_python

    try:
        from_python({"module": "bad", "events": {"started": []}})
    except ValueError as exc:
        assert "events are no longer supported" in str(exc)
    else:  # pragma: no cover - defensive
        raise AssertionError("events dictionary unexpectedly accepted")


def test_builder_has_no_event_api():
    assert not hasattr(ModelBuilder("bad"), "event")
