from __future__ import annotations

from dataclasses import dataclass

from .diagnostics import ParseError


@dataclass(frozen=True)
class Token:
    type: str
    value: str
    line: int
    column: int

    def __repr__(self) -> str:  # pragma: no cover - debug helper
        return f"Token({self.type!r}, {self.value!r}, {self.line}:{self.column})"


KEYWORDS = {
    "module", "import", "as", "exposing",
    "private", "public", "type", "val", "let", "func",
    "entity", "event", "rule", "strict", "defeasible", "defeater",
    "priority", "override", "fact", "assert", "align", "to",
    "equivalent", "broader", "narrower", "related", "key", "where",
    "if", "then", "else", "case", "switch", "when", "in",
    "forall", "exists", "true", "false", "last",
    "and", "or", "not", "implies", "iff",
    "always", "eventually", "next", "weak_next", "never", "until", "release", "weak_until",
    "otherwise", "O", "P", "F",
}

SYMBOLS = set("(){}[],:.|+-*/%=<>;")
MULTI = ("->", "<-", "<=", ">=", "!=", "==", "=>", "<->")


def _indent_width(prefix: str) -> int:
    width = 0
    for ch in prefix:
        if ch == "\t":
            width += 4
        else:
            width += 1
    return width


def tokenize(source: str) -> list[Token]:
    tokens: list[Token] = []
    indents = [0]
    bracket_depth = 0
    in_block_comment = False

    lines = source.splitlines()
    for line_no, raw in enumerate(lines, start=1):
        line = raw.rstrip("\r")
        stripped = line.strip()

        if in_block_comment:
            end = line.find("*/")
            if end == -1:
                continue
            in_block_comment = False
            line = " " * (end + 2) + line[end + 2:]
            stripped = line.strip()

        if not stripped:
            continue
        if stripped.startswith("#") or stripped.startswith("//"):
            continue

        prefix_len = len(line) - len(line.lstrip(" \t"))
        indent = _indent_width(line[:prefix_len])
        i = prefix_len

        if bracket_depth == 0:
            if indent > indents[-1]:
                indents.append(indent)
                tokens.append(Token("INDENT", "<INDENT>", line_no, 1))
            else:
                while indent < indents[-1]:
                    indents.pop()
                    tokens.append(Token("DEDENT", "<DEDENT>", line_no, 1))
                if indent != indents[-1]:
                    raise ParseError("inconsistent indentation", line_no, 1)

        if line[i:].lstrip().startswith("@") and bracket_depth == 0:
            at_col = line.index("@", i) + 1
            text = line[line.index("@", i) + 1:].strip()
            tokens.append(Token("ANNOT", text, line_no, at_col))
            tokens.append(Token("NEWLINE", "<NEWLINE>", line_no, len(line) + 1))
            continue

        n = len(line)
        while i < n:
            ch = line[i]
            col = i + 1

            if ch in " \t":
                i += 1
                continue
            if ch == "#":
                if i + 1 < n and line[i + 1] == "{":
                    tokens.append(Token("SET_START", "#{", line_no, col))
                    bracket_depth += 1
                    i += 2
                    continue
                break
            if ch == "/" and i + 1 < n and line[i + 1] == "/":
                break
            if ch == "/" and i + 1 < n and line[i + 1] == "*":
                end = line.find("*/", i + 2)
                if end == -1:
                    in_block_comment = True
                    break
                i = end + 2
                continue

            matched = None
            for op in MULTI:
                if line.startswith(op, i):
                    matched = op
                    break
            if matched is not None:
                tokens.append(Token("OP", matched, line_no, col))
                i += len(matched)
                continue

            if ch == '"':
                j = i + 1
                escaped = False
                value_chars: list[str] = []
                while j < n:
                    c = line[j]
                    if escaped:
                        value_chars.append("\\" + c)
                        escaped = False
                        j += 1
                        continue
                    if c == "\\":
                        escaped = True
                        j += 1
                        continue
                    if c == '"':
                        break
                    value_chars.append(c)
                    j += 1
                if j >= n or line[j] != '"':
                    raise ParseError("unterminated string literal", line_no, col)
                tokens.append(Token("STRING", "".join(value_chars), line_no, col))
                i = j + 1
                continue

            if ch == "'":
                j = i + 1
                escaped = False
                value_chars: list[str] = []
                while j < n:
                    c = line[j]
                    if escaped:
                        value_chars.append("\\" + c)
                        escaped = False
                        j += 1
                        continue
                    if c == "\\":
                        escaped = True
                        j += 1
                        continue
                    if c == "'":
                        break
                    value_chars.append(c)
                    j += 1
                if j >= n or line[j] != "'":
                    raise ParseError("unterminated character literal", line_no, col)
                tokens.append(Token("CHAR", "".join(value_chars), line_no, col))
                i = j + 1
                continue

            if ch.isdigit():
                j = i + 1
                while j < n and line[j].isdigit():
                    j += 1
                typ = "INT"
                if j < n and line[j] == "." and j + 1 < n and line[j + 1].isdigit():
                    typ = "DECIMAL"
                    j += 1
                    while j < n and line[j].isdigit():
                        j += 1
                elif j < n and line[j] == "/" and j + 1 < n and line[j + 1].isdigit():
                    typ = "RAT"
                    j += 1
                    while j < n and line[j].isdigit():
                        j += 1
                tokens.append(Token(typ, line[i:j], line_no, col))
                i = j
                continue

            if ch.isalpha() or ch == "_":
                j = i + 1
                while j < n and (line[j].isalnum() or line[j] in "_'"):
                    j += 1
                word = line[i:j]
                typ = "KEYWORD" if word in KEYWORDS else "IDENT"
                tokens.append(Token(typ, word, line_no, col))
                i = j
                continue

            if ch in SYMBOLS:
                tokens.append(Token("SYMBOL", ch, line_no, col))
                if ch in "([{":
                    bracket_depth += 1
                elif ch in ")]}":
                    bracket_depth -= 1
                    if bracket_depth < 0:
                        raise ParseError("unmatched closing delimiter", line_no, col)
                i += 1
                continue

            raise ParseError(f"unexpected character {ch!r}", line_no, col)

        if bracket_depth == 0:
            tokens.append(Token("NEWLINE", "<NEWLINE>", line_no, len(line) + 1))

    eof_line = len(lines) + 1
    while len(indents) > 1:
        indents.pop()
        tokens.append(Token("DEDENT", "<DEDENT>", eof_line, 1))
    tokens.append(Token("EOF", "<EOF>", eof_line, 1))
    return tokens
