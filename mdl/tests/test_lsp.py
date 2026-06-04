import io
import json

from mdl.lsp import LSPServer

from sample_sources import LINEQ_RAT_SOURCE


def labels(items):
    return {item["label"] for item in items}


def test_lsp_completes_visible_type_names():
    source = """module pipe

type Pipe = { length: rat, radius: rat }
entity pipe: 
"""
    items = LSPServer().completion_items(source, 3, len("entity pipe: "))

    assert "Pipe" in labels(items)
    assert "rat" in labels(items)


def test_lsp_completes_record_fields():
    source = """module pipe

type Pipe = { length: rat, radius: rat }
entity pipe: Pipe
rule O has_length: pipe.
"""
    items = LSPServer().completion_items(source, 4, len("rule O has_length: pipe."))

    assert labels(items) >= {"length", "radius"}


def test_lsp_completes_record_constructor_fields():
    source = """module complex

type Complex = { r: rat, i: rat }

func zero() -> Complex:
    Complex { 
"""
    items = LSPServer().completion_items(source, 5, len("    Complex { "))

    assert labels(items) >= {"r", "i"}


def test_lsp_reports_braced_expression_parse_error():
    source = """module bad

entity x: bool
rule O r: { x } always
"""
    out = io.BytesIO()
    server = LSPServer(stdout=out)

    server.publish_diagnostics("file:///bad.mdl", source)

    payload = out.getvalue().split(b"\r\n\r\n", 1)[1]
    message = json.loads(payload.decode("utf-8"))
    diagnostics = message["params"]["diagnostics"]
    assert any(item["code"] == "parse-error" for item in diagnostics)


def test_lsp_reports_static_type_error():
    source = """module bad

func wrong() -> int:
    true
"""
    out = io.BytesIO()
    server = LSPServer(stdout=out)

    server.publish_diagnostics("file:///bad.mdl", source)

    payload = out.getvalue().split(b"\r\n\r\n", 1)[1]
    message = json.loads(payload.decode("utf-8"))
    diagnostics = message["params"]["diagnostics"]
    assert any(item["code"] == "type-mismatch" and "expected int, got bool" in item["message"] for item in diagnostics)


def test_lsp_completes_language_keywords():
    source = """module keywords

"""
    items = LSPServer().completion_items(source, 2, 0)
    by_label = {item["label"]: item for item in items}

    assert labels(items) >= {"entity", "rule", "when", "always"}
    assert by_label["rule"]["kind"] == 14


def test_lsp_completes_keywords_in_temporarily_invalid_source():
    source = """module keywords

ru
"""
    items = LSPServer().completion_items(source, 2, len("ru"))

    assert "rule" in labels(items)


def test_lsp_completes_imported_alias_fields(tmp_path):
    imported = tmp_path / "pipe.mdl"
    imported.write_text("""module pipe_spec

type Pipe = { length: rat, radius: rat }
entity pipe: Pipe
""", encoding="utf-8")
    current = tmp_path / "alignment.mdl"
    source = """module alignment

import "pipe.mdl" as m1

rule O aligned: m1.
"""
    server = LSPServer()

    items = server.completion_items(source, 4, len("rule O aligned: m1."), uri=str(current))

    assert "pipe" in labels(items)


def test_lsp_uses_open_document_for_import_completion():
    source = """module alignment

import "pipe.mdl" as m1

rule O aligned: m1.pipe.
"""
    server = LSPServer()
    server.documents["file:///tmp/pipe.mdl"] = """module pipe_spec

type Pipe = { length: rat, radius: rat }
entity pipe: Pipe
"""

    items = server.completion_items(source, 4, len("rule O aligned: m1.pipe."), uri="file:///tmp/alignment.mdl")

    assert labels(items) >= {"length", "radius"}


def test_lsp_lineq_rat_diagnostics_and_pattern_field_completions():
    text = LINEQ_RAT_SOURCE
    uri = "file:///tmp/lineq_rat.mdl"
    out = io.BytesIO()
    server = LSPServer(stdout=out)
    server.documents[uri] = text

    server.publish_diagnostics(uri, text)
    payload = out.getvalue().split(b"\r\n\r\n", 1)[1]
    message = json.loads(payload.decode("utf-8"))

    assert message["params"]["diagnostics"] == []
    assert labels(completion_at(text, server, "(head).", uri)) >= {"coef", "var"}
    assert labels(completion_at(text, server, "((a).", uri)) >= {"coef", "var"}
    assert labels(completion_at(text, server, "((b).", uri)) >= {"coef", "var"}


def completion_at(text: str, server: LSPServer, needle: str, uri: str):
    lines = text.splitlines()
    for line_no, line in enumerate(lines):
        column = line.find(needle)
        if column >= 0:
            return server.completion_items(text, line_no, column + len(needle), uri=uri)
    raise AssertionError(f"needle not found: {needle!r}")
