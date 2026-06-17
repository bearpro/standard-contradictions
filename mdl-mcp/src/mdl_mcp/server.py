from __future__ import annotations

import argparse
import ast
import json
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from importlib.resources import files
from pathlib import Path, PurePosixPath
from typing import Any, Literal, cast

from mcp.server.fastmcp import FastMCP

from . import __version__
from mdl.diagnostics import Diagnostic, ParseError
from mdl.linter import lint_source
from mdl.parser import parse
from mdl.printer import format_module
from mdl.solver import SolveOptions, solve_paths


DEFAULT_ROOT_PATH = "main.mdl"
DEFAULT_PYTHON_TIMEOUT = 3.0
McpTransport = Literal["stdio", "sse", "streamable-http"]
ALLOWED_PYTHON_IMPORTS = {"mdl.builder", "mdl.ast"}
BANNED_PYTHON_NAMES = {
    "__import__",
    "compile",
    "delattr",
    "eval",
    "exec",
    "getattr",
    "globals",
    "input",
    "locals",
    "open",
    "setattr",
    "vars",
}
BANNED_PYTHON_NODES = (
    ast.AsyncFunctionDef,
    ast.Await,
    ast.ClassDef,
    ast.Delete,
    ast.Global,
    ast.Lambda,
    ast.Nonlocal,
    ast.Try,
    ast.With,
)


@dataclass(frozen=True)
class Workspace:
    root_file: Path
    root_logical: str
    path_map: dict[str, str]


def create_mcp_server(host: str = "127.0.0.1", port: int = 8000) -> FastMCP:
    server = FastMCP(
        "mdl-verifier",
        instructions=(
            "Verify LLM-generated MDL source or restricted Python builder DSL by "
            "parsing, linting, formatting and optionally solving it."
        ),
        host=host,
        port=port,
        streamable_http_path="/mcp",
        stateless_http=True,
    )

    @server.tool()
    def mdl_verify(
        source: str,
        path: str = DEFAULT_ROOT_PATH,
        documents: list[dict[str, str]] | None = None,
        solve: bool = True,
        horizon: int | None = None,
        max_horizon: int = 3,
        permission: str = "strong",
    ) -> dict[str, Any]:
        """Verify MDL source text and return diagnostics plus optional solver output."""

        return verify_mdl_source(
            source,
            path=path,
            documents=documents,
            solve=solve,
            horizon=horizon,
            max_horizon=max_horizon,
            permission=permission,
        )

    @server.tool()
    def mdl_verify_python_dsl(
        source: str,
        path: str = DEFAULT_ROOT_PATH,
        documents: list[dict[str, str]] | None = None,
        solve: bool = True,
        horizon: int | None = None,
        max_horizon: int = 3,
        permission: str = "strong",
        timeout_seconds: float = DEFAULT_PYTHON_TIMEOUT,
    ) -> dict[str, Any]:
        """Execute a restricted mdl.builder script, then verify its canonical MDL output."""

        return verify_python_dsl_source(
            source,
            path=path,
            documents=documents,
            solve=solve,
            horizon=horizon,
            max_horizon=max_horizon,
            permission=permission,
            timeout_seconds=timeout_seconds,
        )

    @server.tool()
    def mdl_format(source: str, path: str = DEFAULT_ROOT_PATH) -> dict[str, Any]:
        """Format MDL source into canonical syntax."""

        try:
            module = parse(source)
        except ParseError as exc:
            diagnostic = exc.to_diagnostic(path).to_dict()
            return {
                "ok": False,
                "phase": "parse",
                "diagnostics": [diagnostic],
                "formatted_source": None,
                "summary": summarize([diagnostic], None),
            }
        return {
            "ok": True,
            "phase": "complete",
            "diagnostics": [],
            "formatted_source": format_module(module),
            "summary": summarize([], None),
        }

    @server.resource("mdl://docs/language-reference", mime_type="text/markdown")
    def language_reference_doc() -> str:
        return read_resource_text("language-reference.md")

    @server.resource("mdl://docs/quickstart", mime_type="text/markdown")
    def quickstart_doc() -> str:
        return read_resource_text("quickstart.md")

    @server.resource("mdl://grammar/antlr", mime_type="text/plain")
    def antlr_grammar() -> str:
        return read_resource_text("MDL.g4")

    return server


def verify_mdl_source(
    source: str,
    *,
    path: str = DEFAULT_ROOT_PATH,
    documents: list[dict[str, str]] | None = None,
    solve: bool = True,
    horizon: int | None = None,
    max_horizon: int = 3,
    permission: str = "strong",
) -> dict[str, Any]:
    try:
        root_logical = validate_logical_path(path)
        normalized_docs = normalize_documents(documents or [], root_logical)
    except ValueError as exc:
        diagnostic = make_diagnostic(str(exc), code="invalid-document-path", path=path)
        return error_result("input", [diagnostic], input_kind="mdl")

    with tempfile.TemporaryDirectory(prefix="mdl-mcp-") as tmp:
        workspace = write_workspace(Path(tmp), root_logical, source, normalized_docs)
        try:
            module = parse(source)
        except ParseError as exc:
            diagnostic = exc.to_diagnostic(root_logical).to_dict()
            return error_result("parse", [diagnostic], input_kind="mdl")

        diagnostics = [
            remap_paths(diagnostic.to_dict(), workspace.path_map)
            for diagnostic in lint_source(source, path=str(workspace.root_file))
        ]
        if has_errors(diagnostics):
            return {
                "ok": False,
                "input_kind": "mdl",
                "phase": "lint",
                "module": module.name,
                "diagnostics": diagnostics,
                "formatted_source": format_module(module),
                "generated_source": None,
                "solver": None,
                "summary": summarize(diagnostics, None),
            }

        solver_payload: dict[str, Any] | None = None
        phase = "complete"
        ok = True
        if solve:
            solver_payload = solve_paths(
                [workspace.root_file],
                SolveOptions(
                    horizon=horizon,
                    max_horizon=max_horizon,
                    permission=permission,
                ),
            )
            solver_payload = remap_paths(solver_payload, workspace.path_map)

            if solver_payload is None:
                # TODO throw correct error
                return error_result("input_paths", [], input_kind="mdl")

            ok = solver_payload.get("status") == "sat"
            if not ok:
                phase = "solve"

        return {
            "ok": ok,
            "input_kind": "mdl",
            "phase": phase,
            "module": module.name,
            "diagnostics": diagnostics,
            "formatted_source": format_module(module),
            "generated_source": None,
            "solver": solver_payload,
            "summary": summarize(diagnostics, solver_payload),
        }


def verify_python_dsl_source(
    source: str,
    *,
    path: str = DEFAULT_ROOT_PATH,
    documents: list[dict[str, str]] | None = None,
    solve: bool = True,
    horizon: int | None = None,
    max_horizon: int = 3,
    permission: str = "strong",
    timeout_seconds: float = DEFAULT_PYTHON_TIMEOUT,
) -> dict[str, Any]:
    diagnostics = validate_python_dsl(source)
    if diagnostics:
        return error_result("python-dsl", diagnostics, input_kind="python-dsl")

    generated = run_python_dsl(source, timeout_seconds=timeout_seconds)
    if not generated["ok"]:
        return error_result(
            "python-dsl", generated["diagnostics"], input_kind="python-dsl"
        )

    result = verify_mdl_source(
        generated["source"],
        path=path,
        documents=documents,
        solve=solve,
        horizon=horizon,
        max_horizon=max_horizon,
        permission=permission,
    )
    result["input_kind"] = "python-dsl"
    result["generated_source"] = generated["source"]
    return result


def validate_python_dsl(source: str) -> list[dict[str, Any]]:
    try:
        tree = ast.parse(source, filename="<python-dsl>")
    except SyntaxError as exc:
        return [
            make_diagnostic(
                exc.msg,
                line=exc.lineno or 1,
                column=exc.offset or 1,
                code="python-syntax-error",
            )
        ]
    guard = PythonDslGuard()
    guard.visit(tree)
    return guard.diagnostics


class PythonDslGuard(ast.NodeVisitor):
    def __init__(self) -> None:
        self.diagnostics: list[dict[str, Any]] = []

    def visit_Import(self, node: ast.Import) -> Any:
        for alias in node.names:
            if alias.name not in ALLOWED_PYTHON_IMPORTS:
                self.report(
                    node, f"import {alias.name!r} is not allowed", "python-dsl-import"
                )

    def visit_ImportFrom(self, node: ast.ImportFrom) -> Any:
        if node.level or node.module not in ALLOWED_PYTHON_IMPORTS:
            module = "." * node.level + (node.module or "")
            self.report(
                node, f"import from {module!r} is not allowed", "python-dsl-import"
            )

    def visit_Name(self, node: ast.Name) -> Any:
        if node.id.startswith("__") or node.id in BANNED_PYTHON_NAMES:
            self.report(node, f"name {node.id!r} is not allowed", "python-dsl-name")

    def visit_Attribute(self, node: ast.Attribute) -> Any:
        if node.attr.startswith("__") or node.attr in BANNED_PYTHON_NAMES:
            self.report(
                node, f"attribute {node.attr!r} is not allowed", "python-dsl-attribute"
            )
        self.generic_visit(node)

    def visit_Call(self, node: ast.Call) -> Any:
        name = call_name(node.func)
        if name and (name.split(".")[-1] in BANNED_PYTHON_NAMES or "__" in name):
            self.report(node, f"call to {name!r} is not allowed", "python-dsl-call")
        self.generic_visit(node)

    def generic_visit(self, node: ast.AST) -> Any:
        if isinstance(node, BANNED_PYTHON_NODES):
            self.report(
                node, f"{node.__class__.__name__} is not allowed", "python-dsl-node"
            )
        super().generic_visit(node)

    def report(self, node: ast.AST, message: str, code: str) -> None:
        self.diagnostics.append(
            make_diagnostic(
                message,
                line=getattr(node, "lineno", 1) or 1,
                column=getattr(node, "col_offset", 0) + 1,
                code=code,
            )
        )


def call_name(node: ast.AST) -> str | None:
    if isinstance(node, ast.Name):
        return node.id
    if isinstance(node, ast.Attribute):
        parent = call_name(node.value)
        return f"{parent}.{node.attr}" if parent else node.attr
    return None


_PYTHON_DSL_RUNNER = r"""
import json
import sys
import traceback

ALLOWED_IMPORTS = {"mdl.builder", "mdl.ast"}


def safe_import(name, globals=None, locals=None, fromlist=(), level=0):
    if level != 0 or name not in ALLOWED_IMPORTS:
        raise ImportError(f"import {name!r} is not allowed")
    return __import__(name, globals, locals, fromlist, level)


safe_builtins = {
    "__import__": safe_import,
    "abs": abs,
    "all": all,
    "any": any,
    "bool": bool,
    "dict": dict,
    "enumerate": enumerate,
    "False": False,
    "float": float,
    "int": int,
    "isinstance": isinstance,
    "len": len,
    "list": list,
    "max": max,
    "min": min,
    "None": None,
    "range": range,
    "set": set,
    "str": str,
    "sum": sum,
    "True": True,
    "tuple": tuple,
    "zip": zip,
}


def choose_result(namespace):
    build = namespace.get("build")
    if callable(build):
        return build()
    for name in ("module", "model", "m", "result"):
        if name in namespace:
            return namespace[name]
    raise ValueError("Python DSL must define build() or one of: module, model, m, result")


try:
    from mdl.builder import from_python
    from mdl.printer import format_module

    namespace = {"__builtins__": safe_builtins}
    code = sys.stdin.read()
    exec(compile(code, "<python-dsl>", "exec"), namespace, namespace)
    module = from_python(choose_result(namespace))
    print(json.dumps({"ok": True, "source": format_module(module)}, ensure_ascii=False))
except Exception as exc:
    message = "".join(traceback.format_exception_only(type(exc), exc)).strip()
    print(json.dumps({"ok": False, "error": message}, ensure_ascii=False))
"""


def run_python_dsl(
    source: str, *, timeout_seconds: float = DEFAULT_PYTHON_TIMEOUT
) -> dict[str, Any]:
    try:
        completed = subprocess.run(
            [sys.executable, "-I", "-c", _PYTHON_DSL_RUNNER],
            input=source,
            text=True,
            capture_output=True,
            cwd=tempfile.gettempdir(),
            env={"PYTHONNOUSERSITE": "1"},
            timeout=max(0.1, timeout_seconds),
            check=False,
        )
    except subprocess.TimeoutExpired:
        return {
            "ok": False,
            "diagnostics": [
                make_diagnostic(
                    "Python DSL execution timed out", code="python-dsl-timeout"
                )
            ],
        }

    if completed.returncode != 0:
        message = (
            completed.stderr or completed.stdout or "Python DSL process failed"
        ).strip()
        return {
            "ok": False,
            "diagnostics": [make_diagnostic(message, code="python-dsl-process-error")],
        }
    try:
        payload = json.loads(completed.stdout)
    except json.JSONDecodeError as exc:
        return {
            "ok": False,
            "diagnostics": [
                make_diagnostic(
                    f"Python DSL returned invalid JSON: {exc}",
                    code="python-dsl-process-error",
                )
            ],
        }
    if payload.get("ok"):
        return {"ok": True, "source": payload["source"]}
    return {
        "ok": False,
        "diagnostics": [
            make_diagnostic(
                str(payload.get("error") or "Python DSL failed"),
                code="python-dsl-error",
            )
        ],
    }


def normalize_documents(
    documents: list[dict[str, str]], root_logical: str
) -> list[tuple[str, str]]:
    result: list[tuple[str, str]] = []
    seen = {root_logical}
    for index, document in enumerate(documents):
        raw_path = document.get("path")
        source = document.get("source")
        if raw_path is None:
            raise ValueError(f"document {index} is missing path")
        if source is None:
            raise ValueError(f"document {raw_path!r} is missing source")
        logical = validate_logical_path(raw_path)
        if logical in seen:
            raise ValueError(f"duplicate virtual document path {logical!r}")
        seen.add(logical)
        result.append((logical, source))
    return result


def validate_logical_path(path: str) -> str:
    if not path:
        raise ValueError("path must not be empty")
    normalized = path.replace("\\", "/")
    pure = PurePosixPath(normalized)
    if pure.is_absolute():
        raise ValueError(f"path {path!r} must be relative")
    if any(part in {"", ".", ".."} for part in pure.parts):
        raise ValueError(f"path {path!r} must not contain empty, '.', or '..' segments")
    return pure.as_posix()


def write_workspace(
    root: Path, root_logical: str, source: str, documents: list[tuple[str, str]]
) -> Workspace:
    path_map: dict[str, str] = {}
    root_file = write_virtual_file(root, root_logical, source, path_map)
    for logical, text in documents:
        write_virtual_file(root, logical, text, path_map)
    return Workspace(root_file=root_file, root_logical=root_logical, path_map=path_map)


def write_virtual_file(
    root: Path, logical: str, source: str, path_map: dict[str, str]
) -> Path:
    target = root / logical
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(source, encoding="utf-8")
    path_map[str(target)] = logical
    return target


def remap_paths(value: Any, path_map: dict[str, str]) -> Any:
    if isinstance(value, dict):
        return {key: remap_paths(item, path_map) for key, item in value.items()}
    if isinstance(value, list):
        return [remap_paths(item, path_map) for item in value]
    if isinstance(value, str):
        text = value
        for absolute, logical in sorted(
            path_map.items(), key=lambda item: len(item[0]), reverse=True
        ):
            text = text.replace(absolute, logical)
        return text
    return value


def has_errors(diagnostics: list[dict[str, Any]]) -> bool:
    return any(diagnostic.get("severity") == "error" for diagnostic in diagnostics)


def summarize(
    diagnostics: list[dict[str, Any]], solver_payload: dict[str, Any] | None
) -> dict[str, Any]:
    errors = sum(
        1 for diagnostic in diagnostics if diagnostic.get("severity") == "error"
    )
    warnings = sum(
        1 for diagnostic in diagnostics if diagnostic.get("severity") == "warning"
    )
    return {
        "errors": errors,
        "warnings": warnings,
        "solver_status": solver_payload.get("status") if solver_payload else None,
        "conflicts": len(solver_payload.get("conflicts") or [])
        if solver_payload
        else 0,
    }


def make_diagnostic(
    message: str,
    *,
    line: int = 1,
    column: int = 1,
    severity: str = "error",
    code: str | None = None,
    path: str | None = None,
) -> dict[str, Any]:
    return Diagnostic(
        message, line=line, column=column, severity=severity, code=code, path=path
    ).to_dict()


def error_result(
    phase: str, diagnostics: list[dict[str, Any]], *, input_kind: str
) -> dict[str, Any]:
    return {
        "ok": False,
        "input_kind": input_kind,
        "phase": phase,
        "module": None,
        "diagnostics": diagnostics,
        "formatted_source": None,
        "generated_source": None,
        "solver": None,
        "summary": summarize(diagnostics, None),
    }


def read_resource_text(name: str) -> str:
    return files("mdl_mcp").joinpath("resources", name).read_text(encoding="utf-8")


def run_mcp_server(
    transport: McpTransport = "stdio", host: str = "127.0.0.1", port: int = 8000
) -> int:
    server = create_mcp_server(host=host, port=port)
    server.run(transport=transport)
    return 0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="mdl-mcp", description="MDL MCP verification server"
    )
    parser.add_argument("--version", action="version", version=f"mdl-mcp {__version__}")
    parser.add_argument(
        "--transport", choices=["stdio", "streamable-http"], default="stdio"
    )
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=8000)
    return parser


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    return run_mcp_server(cast(McpTransport, args.transport), args.host, args.port)


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
