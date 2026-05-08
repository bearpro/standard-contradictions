from mdl.lsp import LSPServer


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

import pipe_spec as m1

rule O aligned: m1.
"""
    server = LSPServer()

    items = server.completion_items(source, 4, len("rule O aligned: m1."), uri=str(current))

    assert "pipe" in labels(items)


def test_lsp_uses_open_document_for_import_completion():
    source = """module alignment

import pipe_spec as m1

rule O aligned: m1.pipe.
"""
    server = LSPServer()
    server.documents["file:///tmp/pipe.mdl"] = """module pipe_spec

type Pipe = { length: rat, radius: rat }
entity pipe: Pipe
"""

    items = server.completion_items(source, 4, len("rule O aligned: m1.pipe."), uri="file:///tmp/alignment.mdl")

    assert labels(items) >= {"length", "radius"}
