from __future__ import annotations

from collections.abc import Iterable
from typing import cast

from . import ast as A
from .antlr_bridge import parse_with_rule
from .ast_builder import AstBuilder
from .diagnostics import ParseError
from .lexer import Token

__all__ = [
    "ParseError",
    "parse",
    "parse_expr",
    "parse_many",
    "parse_tokens",
    "parse_type_expr_source",
]


def parse(source: str) -> A.Module:
    return cast(
        A.Module,
        cast(object, AstBuilder().visit(parse_with_rule(source, "program").tree)),
    )


def parse_tokens(tokens: list[Token]) -> A.Module:
    return parse(_tokens_to_source(tokens))


def parse_expr(source: str) -> A.Expr:
    return cast(
        A.Expr,
        cast(object, AstBuilder().visit(parse_with_rule(source, "exprOnly").tree)),
    )


def parse_many(sources: Iterable[str]) -> list[A.Module]:
    return [parse(source) for source in sources]


def parse_type_expr_source(source: str) -> A.TypeExpr:
    return cast(
        A.TypeExpr,
        cast(object, AstBuilder().visit(parse_with_rule(source, "typeExprOnly").tree)),
    )


def _tokens_to_source(tokens: list[Token]) -> str:
    lines: list[list[str]] = [[]]
    for token in tokens:
        if token.type == "EOF":
            break
        if token.type == "NEWLINE":
            lines.append([])
            continue
        if token.type in {"INDENT", "DEDENT"}:
            continue
        while len(lines) < token.line:
            lines.append([])
        line = lines[token.line - 1]
        text = token.value
        if token.type == "STRING":
            text = '"' + text.replace('"', '\\"') + '"'
        if token.type == "ANNOT":
            text = "@ " + text
        padding = max(0, token.column - 1 - sum(len(part) for part in line))
        line.append(" " * padding + text)
    return "\n".join("".join(line) for line in lines)
