import io
import json
from pathlib import Path

from mdl.lsp import LSPServer

from .sample_sources import LINEQ_RAT_SOURCE


EXAMPLES = Path(__file__).parents[1] / "examples"
STDLIB = Path(__file__).parents[1] / "src" / "mdl" / "stdlib"


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


def test_lsp_reports_mdl_diagnostics_for_mdl_py():
    source = """from mdl.dsl import *

module("bad")

@function
def wrong() -> Int:
    return True
"""
    out = io.BytesIO()
    server = LSPServer(stdout=out)

    server.publish_diagnostics("file:///bad.mdl.py", source)

    payload = out.getvalue().split(b"\r\n\r\n", 1)[1]
    message = json.loads(payload.decode("utf-8"))
    diagnostics = message["params"]["diagnostics"]
    assert any(item["code"] == "type-mismatch" and "expected int, got bool" in item["message"] for item in diagnostics)


def test_lsp_reports_python_dsl_errors_for_mdl_py():
    source = """from mdl.dsl import *

for item in []:
    pass
"""
    out = io.BytesIO()
    server = LSPServer(stdout=out)

    server.publish_diagnostics("file:///bad.mdl.py", source)

    payload = out.getvalue().split(b"\r\n\r\n", 1)[1]
    message = json.loads(payload.decode("utf-8"))
    diagnostics = message["params"]["diagnostics"]
    assert diagnostics[0]["code"] == "python-dsl-error"


def test_lsp_mdl_py_completion_is_diagnostics_only():
    source = """from mdl.dsl import *

module("pipe")
"""

    items = LSPServer().completion_items(source, 2, len('module("pipe")'), uri="file:///tmp/pipe.mdl.py")

    assert items == []


def test_lsp_completes_language_keywords():
    source = """module keywords

"""
    items = LSPServer().completion_items(source, 2, 0)
    by_label = {item["label"]: item for item in items}

    assert labels(items) >= {"entity", "rule", "when", "always", "now", "implies"}
    assert not (labels(items) & {"initially", "weak_next", "never", "release", "weak_until"})
    assert by_label["rule"]["kind"] == 14


def test_lsp_completes_keywords_in_temporarily_invalid_source():
    source = """module keywords

ru
"""
    items = LSPServer().completion_items(source, 2, len("ru"))

    assert "rule" in labels(items)


def test_lsp_completes_imported_module_fields(tmp_path):
    imported = tmp_path / "pipe.mdl"
    imported.write_text("""module pipe_spec

type Pipe = { length: rat, radius: rat }
entity pipe: Pipe
""", encoding="utf-8")
    current = tmp_path / "alignment.mdl"
    source = """module alignment

import "pipe.mdl"

rule O aligned: pipe_spec.
"""
    server = LSPServer()

    items = server.completion_items(source, 4, len("rule O aligned: pipe_spec."), uri=str(current))

    assert "pipe" in labels(items)


def test_lsp_uses_open_document_for_import_completion():
    source = """module alignment

import "pipe.mdl"

rule O aligned: pipe_spec.pipe.
"""
    server = LSPServer()
    server.documents["file:///tmp/pipe.mdl"] = """module pipe_spec

type Pipe = { length: rat, radius: rat }
entity pipe: Pipe
"""

    items = server.completion_items(source, 4, len("rule O aligned: pipe_spec.pipe."), uri="file:///tmp/alignment.mdl")

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

    assert not any(item["severity"] == 1 for item in message["params"]["diagnostics"])
    assert labels(completion_at(text, server, "(head).", uri)) >= {"coef", "var"}
    assert labels(completion_at(text, server, "((a).", uri)) >= {"coef", "var"}
    assert labels(completion_at(text, server, "((b).", uri)) >= {"coef", "var"}


def test_lsp_document_symbols_hover_definition_semantic_tokens_and_summary():
    source = """module pipe

@domain pipe
type Pipe = { length: rat, radius: rat }
let threshold = 1
entity pipe: Pipe
func positive_pipe(p: Pipe) -> bool: p.length > threshold
rule O positive: pipe.length > 0 always
"""
    uri = "file:///tmp/pipe.mdl"
    server = LSPServer()
    server.documents[uri] = source
    snapshot = server.snapshot(uri)

    symbols = snapshot.document_symbols()
    assert symbols[0]["name"] == "pipe"
    children = {child["name"]: child for child in symbols[0]["children"]}
    assert set(children) >= {"Pipe", "threshold", "pipe", "positive_pipe", "positive"}
    assert symbols[0]["children"][0]["selectionRange"]["start"]["character"] == len("type ")
    assert children["Pipe"]["detail"] == "= { length: rat, radius: rat }"
    assert children["threshold"]["detail"] == ": int"
    assert children["pipe"]["detail"] == ": Pipe"
    assert children["positive_pipe"]["detail"] == "(p: Pipe) -> bool"

    hover_line, hover_col = position_of(source, "pipe.length")
    hover = snapshot.hover(hover_line, hover_col)
    assert hover is not None
    assert "**entity** `pipe`" in hover["contents"]["value"]
    assert "`Pipe`" in hover["contents"]["value"]

    definition = snapshot.definition(hover_line, hover_col)
    assert definition is not None
    assert definition["range"]["start"]["line"] == 5
    assert definition["range"]["start"]["character"] == len("entity ")

    semantic = snapshot.semantic_tokens()
    assert semantic["data"]

    summary = snapshot.model_summary()
    assert summary["module"] == "pipe"
    assert summary["rules"][0]["name"] == "positive"
    assert "core" not in summary


def test_lsp_definition_for_opened_type_keeps_correct_line(monkeypatch):
    monkeypatch.setenv("MDL_STDLIB_PATH", str(STDLIB))
    source = """# comment
module tmp

open std.collections

func foo() -> List<int>: List.Empty()
"""
    uri = (EXAMPLES / "1. tmp-wrong-line-open.mdl").as_uri()
    server = LSPServer()
    server.documents[uri] = source
    snapshot = server.snapshot(uri)

    line, col = position_of(source, "List<int>")
    definition = snapshot.definition(line, col)

    assert definition is not None
    assert definition["uri"].endswith("/src/mdl/stdlib/std/collections.mdl")
    assert definition["range"]["start"] == {"line": 2, "character": len("type ")}


def test_lsp_definition_for_function_argument_uses_caller_scope():
    source = """module tmp

func foo(name: string) -> bool: true

entity name: string

fact foo(name)
"""
    uri = (EXAMPLES / "2. tmp-wrong-definition.mdl").as_uri()
    server = LSPServer()
    server.documents[uri] = source
    snapshot = server.snapshot(uri)

    line, col = position_of(source, "fact foo(")
    col += len("fact foo(")
    definition = snapshot.definition(line, col)

    assert definition is not None
    assert definition["range"]["start"] == {"line": 4, "character": len("entity ")}
    assert definition["range"]["end"] == {"line": 4, "character": len("entity name")}

    entity_line, entity_col = position_of(source, "entity name")
    entity_definition = snapshot.definition(entity_line, entity_col + len("entity "))
    assert entity_definition is not None
    assert entity_definition["range"]["start"] == {"line": 4, "character": len("entity ")}


def test_lsp_definition_for_function_body_parameter_uses_function_scope():
    source = """module tmp

func foo(name: unit) -> unit: name

entity name: unit
"""
    uri = "file:///tmp/function-body-parameter.mdl"
    server = LSPServer()
    server.documents[uri] = source
    snapshot = server.snapshot(uri)

    line, col = position_of(source, "name", occurrence=1)
    definition = snapshot.definition(line, col)

    assert definition is not None
    assert definition["range"]["start"] == {"line": 2, "character": len("func foo(")}
    assert definition["range"]["end"] == {"line": 2, "character": len("func foo(name")}


def test_lsp_diagnostic_range_uses_full_missing_symbol_span():
    source = """module tmp

entity t: int

fact time > 0
"""
    uri = (EXAMPLES / "3. tmp-no-span-for-missing-symbol.mdl").as_uri()
    out = io.BytesIO()
    server = LSPServer(stdout=out)

    server.publish_diagnostics(uri, source)

    payload = out.getvalue().split(b"\r\n\r\n", 1)[1]
    message = json.loads(payload.decode("utf-8"))
    diagnostic = next(item for item in message["params"]["diagnostics"] if item["code"] == "undefined-name")
    assert diagnostic["range"] == {
        "start": {"line": 4, "character": len("fact ")},
        "end": {"line": 4, "character": len("fact time")},
    }


def test_lsp_union_type_and_constructor_have_distinct_semantic_tokens_and_definitions():
    source = """module tmp

type MyUnion = CaseA(unit) | CaseB(unit)

func x() -> MyUnion: MyUnion.CaseA()
"""
    uri = (EXAMPLES / "4. tmp-union-costructor-missed-with-type.mdl").as_uri()
    server = LSPServer()
    server.documents[uri] = source
    snapshot = server.snapshot(uri)

    expr_type_line, expr_type_col = position_of(source, "MyUnion.CaseA", occurrence=0)
    case_line, case_col = position_of(source, "CaseA()", occurrence=0)
    type_index = snapshot.token_index_at(expr_type_line, expr_type_col)
    case_index = snapshot.token_index_at(case_line, case_col)

    assert type_index is not None
    assert case_index is not None
    assert snapshot.semantic_type_for_token(type_index) == "type"
    assert snapshot.semantic_type_for_token(case_index) == "enumMember"

    type_definition = snapshot.definition(expr_type_line, expr_type_col)
    case_definition = snapshot.definition(case_line, case_col)

    assert type_definition is not None
    assert type_definition["range"]["start"] == {"line": 2, "character": len("type ")}
    assert case_definition is not None
    assert case_definition["range"]["start"] == {"line": 2, "character": len("type MyUnion = ")}


def test_lsp_standard_operators_have_semantic_operator_tokens():
    source = """module tmp

entity x: int
entity y: int
func f(a: int, b: int) -> bool: a + b - 1 * 2 / 3 % 4 != 0
rule O r: not (x > 0 and x <= 10 or x >= 3) implies x = 1 always
rule F s: x < 5 until y != 2 eventually
rule P t: x = last now
"""
    uri = "file:///tmp/operators.mdl"
    server = LSPServer()
    server.documents[uri] = source
    snapshot = server.snapshot(uri)

    def token_type(needle: str, *, occurrence: int = 0) -> str | None:
        line, col = position_of(source, needle, occurrence=occurrence)
        index = snapshot.token_index_at(line, col)
        assert index is not None
        return snapshot.semantic_type_for_token(index)

    for needle in [
        "->", "+", "-", "*", "/", "%", "!=", "O r", "not", ">", "and", "<=", "or", ">=",
        "implies", "= 1", "always", "F s", "< 5", "until", "eventually", "P t", "last", "now",
    ]:
        assert token_type(needle) == "operator"


def test_lsp_go_to_definition_for_record_field():
    source = """module pipe

type Pipe = { length: rat, radius: rat }
entity pipe: Pipe
rule O positive: pipe.length > 0 always
"""
    uri = "file:///tmp/pipe-fields.mdl"
    server = LSPServer()
    server.documents[uri] = source
    snapshot = server.snapshot(uri)

    line, col = position_of(source, "length >")
    definition = snapshot.definition(line, col)

    assert definition is not None
    assert definition["range"]["start"] == {"line": 2, "character": len("type Pipe = { ")}
    assert definition["range"]["end"] == {"line": 2, "character": len("type Pipe = { length")}


def test_lsp_completion_after_function_return_arrow_suggests_types_only():
    source = """module tmp

func x(x: int) -> _: ()
"""

    items = LSPServer().completion_items(source, 2, len("func x(x: int) -> _"))
    item_labels = labels(items)

    assert item_labels >= {"bool", "int", "unit"}
    assert item_labels.isdisjoint({"true", "false", "O", "F"})


def test_lsp_not_messes_entity_with_function_parameter_1():
    source = """module tmp      # 0
func f(name: unit) -> unit: ()  # 1
entity name: unit               # 2
fact f(name) = ()               # 3
    """

    uri = "file:///tmp/pipe-fields.mdl"
    server = LSPServer()
    server.documents[uri] = source
    snapshot = server.snapshot(uri)

    line, col = position_of(source, "f(name)")
    definition = snapshot.definition(line, col + 3)
    assert definition is not None
    assert definition["range"]["start"]["line"] == 2


def test_lsp_not_messes_entity_with_function_parameter_2():
    source = """module tmp      # 0
entity name: unit               # 1
func f(name: unit) -> unit: ()  # 2
fact f(name) = ()               # 3
    """

    uri = "file:///tmp/pipe-fields.mdl"
    server = LSPServer()
    server.documents[uri] = source
    snapshot = server.snapshot(uri)

    line, col = position_of(source, "f(name)")
    definition = snapshot.definition(line, col + 3)
    assert definition is not None
    assert definition["range"]["start"]["line"] == 1


def completion_at(text: str, server: LSPServer, needle: str, uri: str):
    lines = text.splitlines()
    for line_no, line in enumerate(lines):
        column = line.find(needle)
        if column >= 0:
            return server.completion_items(text, line_no, column + len(needle), uri=uri)
    raise AssertionError(f"needle not found: {needle!r}")


def position_of(text: str, needle: str, *, occurrence: int = 0):
    seen = 0
    lines = text.splitlines()
    for line_no, line in enumerate(lines):
        start = 0
        while True:
            column = line.find(needle, start)
            if column < 0:
                break
            if seen == occurrence:
                return line_no, column
            seen += 1
            start = column + len(needle)
    raise AssertionError(f"needle not found: {needle!r}")
