from __future__ import annotations

import json
import asyncio

from mcp.shared.memory import create_connected_server_and_client_session

from mdl_mcp.server import create_mcp_server, verify_mdl_source, verify_python_dsl_source


def test_mdl_verify_reports_parse_error():
    payload = verify_mdl_source("module broken\nfunc x( -> bool: true\n", solve=False)

    assert payload["ok"] is False
    assert payload["phase"] == "parse"
    assert payload["diagnostics"][0]["code"] == "parse-error"


def test_mdl_verify_reports_lint_error():
    payload = verify_mdl_source(
        """
module bad

rule O missing: later always
""",
        solve=False,
    )

    assert payload["ok"] is False
    assert payload["phase"] == "lint"
    assert any(d["code"] == "undefined-name" for d in payload["diagnostics"])


def test_mdl_verify_formats_successful_source():
    payload = verify_mdl_source(
        """
module ok

entity x: bool
rule O must_hold: x always
""",
        solve=True,
    )

    assert payload["ok"] is True
    assert payload["phase"] == "complete"
    assert payload["solver"]["status"] == "sat"
    assert "rule O must_hold:" in payload["formatted_source"]


def test_mdl_verify_returns_unsat_conflict():
    payload = verify_mdl_source(
        """
module bad

entity x: bool
rule O must: x always
rule F forbid: x always
""",
        solve=True,
    )

    assert payload["ok"] is False
    assert payload["phase"] == "solve"
    assert payload["solver"]["status"] == "unsat"
    assert payload["solver"]["conflicts"]


def test_mdl_verify_resolves_virtual_import():
    payload = verify_mdl_source(
        """
module consumer

import "dep.mdl"

rule O imported_rule: dep.x always
""",
        documents=[
            {
                "path": "dep.mdl",
                "source": """
module dep

entity x: bool
""",
            }
        ],
        solve=True,
    )

    assert payload["ok"] is True
    assert payload["solver"]["status"] == "sat"
    assert not payload["diagnostics"]


def test_mdl_verify_rejects_unsafe_virtual_path():
    payload = verify_mdl_source(
        "module ok\n",
        documents=[{"path": "../dep.mdl", "source": "module dep\n"}],
    )

    assert payload["ok"] is False
    assert payload["phase"] == "input"
    assert payload["diagnostics"][0]["code"] == "invalid-document-path"


def test_python_dsl_verifies_builder_script():
    payload = verify_python_dsl_source(
        """
from mdl.builder import ModelBuilder, always, ref

def build():
    m = ModelBuilder("email")
    m.entity("email", "bool")
    m.rule("email_required", "O", always(ref("email")))
    return m
""",
        solve=True,
    )

    assert payload["ok"] is True
    assert payload["input_kind"] == "python-dsl"
    assert payload["solver"]["status"] == "sat"
    assert "module email" in payload["generated_source"]


def test_python_dsl_verifies_dict_result():
    payload = verify_python_dsl_source(
        """
def build():
    return {
        "module": "pipe",
        "types": {"Pipe": "{ length: rat }"},
        "entities": {"pipe": "Pipe"},
    }
""",
        solve=True,
    )

    assert payload["ok"] is True
    assert "type Pipe = { length: rat }" in payload["generated_source"]


def test_python_dsl_rejects_unapproved_import():
    payload = verify_python_dsl_source(
        """
import os

def build():
    return {"module": "bad"}
""",
        solve=False,
    )

    assert payload["ok"] is False
    assert payload["phase"] == "python-dsl"
    assert payload["diagnostics"][0]["code"] == "python-dsl-import"


def test_python_dsl_times_out():
    payload = verify_python_dsl_source(
        """
while True:
    pass
""",
        timeout_seconds=0.1,
    )

    assert payload["ok"] is False
    assert payload["diagnostics"][0]["code"] == "python-dsl-timeout"


def test_mcp_server_lists_and_calls_tools():
    async def scenario() -> None:
        async with create_connected_server_and_client_session(
            create_mcp_server(),
            raise_exceptions=True,
        ) as client:
            await client.initialize()

            tools = await client.list_tools()
            prompts = await client.list_prompts()
            resources = await client.list_resources()

            assert {tool.name for tool in tools.tools} >= {"mdl_verify", "mdl_verify_python_dsl"}
            assert {prompt.name for prompt in prompts.prompts} >= {"draft_mdl_from_text"}
            assert {str(resource.uri) for resource in resources.resources} >= {"mdl://docs/grammar"}

            result = await client.call_tool(
                "mdl_verify",
                {
                    "source": """
module ok

entity x: bool
rule O r: x always
""",
                    "solve": False,
                },
            )
            payload = tool_payload(result)
            assert payload["ok"] is True
            assert payload["module"] == "ok"

    asyncio.run(scenario())


def tool_payload(result: object) -> dict:
    structured = getattr(result, "structuredContent", None) or getattr(result, "structured_content", None)
    if structured is not None:
        return structured
    for content in getattr(result, "content", []):
        if getattr(content, "type", None) == "text":
            return json.loads(content.text)
    raise AssertionError(f"no structured payload in {result!r}")
