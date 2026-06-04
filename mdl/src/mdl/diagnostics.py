from __future__ import annotations

from dataclasses import dataclass
from typing import Any


@dataclass(frozen=True)
class Diagnostic:
    message: str
    line: int = 1
    column: int = 1
    severity: str = "error"  # error | warning | information
    code: str | None = None
    path: str | None = None

    def to_dict(self) -> dict[str, Any]:
        data = {
            "message": self.message,
            "line": self.line,
            "column": self.column,
            "severity": self.severity,
        }
        if self.code:
            data["code"] = self.code
        if self.path:
            data["path"] = self.path
        return data

    def to_lsp(self) -> dict[str, Any]:
        severity_map = {"error": 1, "warning": 2, "information": 3, "hint": 4}
        line = max(0, self.line - 1)
        col = max(0, self.column - 1)
        return {
            "range": {
                "start": {"line": line, "character": col},
                "end": {"line": line, "character": col + 1},
            },
            "severity": severity_map.get(self.severity, 1),
            "source": "mdl",
            "code": self.code,
            "message": self.message,
        }


class MDLError(Exception):
    """Base exception for MDL tooling."""


class ParseError(MDLError):
    def __init__(self, message: str, line: int = 1, column: int = 1):
        super().__init__(f"{line}:{column}: {message}")
        self.message = message
        self.line = line
        self.column = column

    def to_diagnostic(self, path: str | None = None) -> Diagnostic:
        return Diagnostic(
            self.message,
            line=self.line,
            column=self.column,
            severity="error",
            code="parse-error",
            path=path,
        )
