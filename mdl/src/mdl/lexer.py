from __future__ import annotations

from dataclasses import dataclass

from ._antlr.MDLLexer import MDLLexer
from ._antlr.MDLParser import MDLParser
from .antlr_bridge import visible_tokens


@dataclass(frozen=True)
class Token:
    type: str
    value: str
    line: int
    column: int
    end_line: int = 0
    end_column: int = 0

    def __repr__(self) -> str:  # pragma: no cover - debug helper
        return f"Token({self.type!r}, {self.value!r}, {self.line}:{self.column}-{self.end_line}:{self.end_column})"


KEYWORDS = {
    "module", "import", "open",
    "type", "let", "func",
    "entity", "rule", "strict", "defeasible", "defeater",
    "priority", "override", "fact",
    "if", "then", "else", "case", "when", "in",
    "true", "false", "last",
    "and", "or", "not",
    "always", "eventually", "next", "weak_next", "never", "until", "release", "weak_until",
    "otherwise", "O", "P", "F",
}

SYMBOLS = set("(){},:.|+-*/%=<>")
MULTI = ("->", "<=", ">=", "!=", "==")


def _indent_width(prefix: str) -> int:
    width = 0
    for ch in prefix:
        if ch == "\t":
            width += 4
        else:
            width += 1
    return width


def tokenize(source: str) -> list[Token]:
    return [_convert_token(token) for token in visible_tokens(source)]


def _convert_token(token) -> Token:  # noqa: ANN001
    value = token.text or ""
    if token.type == -1:
        typ = "EOF"
    elif token.type in {MDLParser.INDENT, MDLParser.DEDENT}:
        typ = MDLParser.symbolicNames[token.type]
    elif 0 <= token.type < len(MDLLexer.symbolicNames):
        typ = MDLLexer.symbolicNames[token.type]
    else:
        typ = MDLParser.symbolicNames[token.type]
    if value in KEYWORDS:
        typ = "KEYWORD"
    elif typ == "ANNOT":
        value = value[1:].strip()
    elif typ == "STRING":
        value = _string_value(value)
    elif typ in {"ARROW", "LE", "GE", "NE", "EQEQ"}:
        typ = "OP"
    elif typ in {
        "LPAREN", "RPAREN", "LBRACE", "RBRACE", "COMMA", "COLON", "DOT",
        "BAR", "PLUS", "MINUS", "STAR", "SLASH", "PERCENT", "EQ", "LT", "GT", "UNDERSCORE",
    }:
        typ = "SYMBOL"
    text = token.text or value or ""
    end_column = token.column + max(1, len(text)) + 1
    return Token(typ, value, token.line, token.column + 1, token.line, end_column)


def _string_value(text: str) -> str:
    value_chars: list[str] = []
    escaped = False
    for char in text[1:-1]:
        if escaped:
            value_chars.append("\\" + char)
            escaped = False
        elif char == "\\":
            escaped = True
        else:
            value_chars.append(char)
    if escaped:
        value_chars.append("\\")
    return "".join(value_chars)
