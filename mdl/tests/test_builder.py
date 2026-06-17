from mdl.builder import ModelBuilder, always, call, now, ref, temporal_binary
from mdl.parser import parse


def test_builder_prints_parseable_source():
    m = ModelBuilder("email")
    m.entity("email", "string")
    m.rule(
        "email_addr_spec_correct", "O", always(call("email_is_correct", ref("email")))
    )
    source = m.to_source()
    parsed = parse(source)
    assert parsed.name == "email"
    assert "rule O email_addr_spec_correct:" in source


def test_builder_accepts_python_dsl_stdlib_types():
    from mdl.dsl import Bool, Decimal, Int, Rat, String, Unit

    m = ModelBuilder("typed")
    m.entity("flag", Bool)
    m.entity("count", Int)
    m.entity("ratio", Rat)
    m.entity("score", Decimal)
    m.entity("label", String)
    m.entity("marker", Unit)

    source = m.to_source()

    assert "entity flag: bool" in source
    assert "entity count: int" in source
    assert "entity ratio: rat" in source
    assert "entity score: decimal" in source
    assert "entity label: string" in source
    assert "entity marker: unit" in source
    assert parse(source).name == "typed"


def test_builder_from_dict():
    from mdl.builder import from_python

    module = from_python(
        {
            "module": "pipe",
            "types": {"Pipe": "{ length: rat, radius: rat }"},
            "entities": {"pipe": "Pipe"},
        }
    )
    assert module.name == "pipe"
    assert len(module.declarations) == 2


def test_builder_supports_now_and_only_until_temporal_binary():
    assert now(ref("x")).op == "now"
    assert temporal_binary("until", ref("x"), ref("y")).op == "until"
    try:
        temporal_binary("release", ref("x"), ref("y"))
    except ValueError as exc:
        assert "until" in str(exc)
    else:  # pragma: no cover - defensive
        raise AssertionError("release temporal binary unexpectedly accepted")


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
