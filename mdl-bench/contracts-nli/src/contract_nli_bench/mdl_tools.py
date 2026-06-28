from __future__ import annotations

import json
from typing import Any

from mdl_mcp.server import verify_mdl_source


MDL_VERIFY_TOOL_NAME = "mdl_verify"

MDL_VERIFY_TOOL: dict[str, Any] = {
    "type": "function",
    "name": MDL_VERIFY_TOOL_NAME,
    "description": (
        "Verify generated MDL source with the MDL parser and linter. "
        "Call this before returning a final answer."
    ),
    "parameters": {
        "type": "object",
        "properties": {
            "source": {
                "type": "string",
                "description": "Complete MDL source to verify.",
            },
            "path": {
                "type": "string",
                "description": "Virtual relative path for diagnostics.",
                "default": "generated.mdl",
            },
        },
        "required": ["source"],
        "additionalProperties": False,
    },
}


def call_mdl_verify_tool(arguments: str) -> dict[str, Any]:
    try:
        payload = json.loads(arguments)
    except json.JSONDecodeError as exc:
        return _tool_input_error(f"tool arguments are not valid JSON: {exc}")

    source = payload.get("source")
    if not isinstance(source, str) or not source.strip():
        return _tool_input_error("tool argument 'source' must be a non-empty string")

    path = payload.get("path", "generated.mdl")
    if not isinstance(path, str) or not path.strip():
        return _tool_input_error("tool argument 'path' must be a non-empty string")

    return verify_mdl_source(source, path=path, solve=False)


def is_successful_mdl_verify(payload: dict[str, Any]) -> bool:
    return payload.get("ok") is True and payload.get("phase") == "complete"


def _tool_input_error(message: str) -> dict[str, Any]:
    return {
        "ok": False,
        "input_kind": "mdl",
        "phase": "input",
        "module": None,
        "diagnostics": [
            {
                "message": message,
                "line": 1,
                "column": 1,
                "severity": "error",
                "code": "tool-input-error",
                "path": None,
            }
        ],
        "formatted_source": None,
        "generated_source": None,
        "solver": None,
        "summary": {"errors": 1, "warnings": 0, "solver_status": None, "conflicts": 0},
    }
