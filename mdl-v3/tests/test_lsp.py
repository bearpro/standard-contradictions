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
