from __future__ import annotations

from collections import deque
from dataclasses import dataclass
from collections.abc import Iterable
from typing import cast

from antlr4 import CommonTokenStream, InputStream
from antlr4.ListTokenSource import ListTokenSource
from antlr4.Token import Token as AntlrToken
from antlr4.error.ErrorListener import ErrorListener
from antlr4.Lexer import Lexer
from antlr4.tree.Tree import ParseTree

from ._antlr.MDLLexer import MDLLexer
from ._antlr.MDLParser import MDLParser
from .diagnostics import ParseError


@dataclass(frozen=True)
class SyntaxResult:
    parser: MDLParser
    tree: ParseTree


class RaisingErrorListener(ErrorListener):
    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):  # noqa: ANN001, N802
        raise ParseError(msg, int(line or 1), int(column or 0) + 1)


def make_token(token_type: int, text: str, line: int, column: int) -> AntlrToken:
    token = AntlrToken()
    token.type = token_type
    token.text = text
    token.line = line
    token.column = max(0, column)
    token.channel = AntlrToken.DEFAULT_CHANNEL
    return token


def visible_tokens(source: str) -> list[AntlrToken]:
    lexer = MDLLexer(InputStream(source))
    lexer.removeErrorListeners()
    lexer.addErrorListener(RaisingErrorListener())
    raw = lexer.getAllTokens()
    return inject_layout(raw)


def inject_layout(raw_tokens: Iterable[AntlrToken]) -> list[AntlrToken]:
    tokens: list[AntlrToken] = []
    pending = deque(raw_tokens)
    indents = [0]
    bracket_depth = 0
    last_line = 1

    while pending:
        token = pending.popleft()
        last_line = max(last_line, int(token.line or 1))
        if token.channel == AntlrToken.HIDDEN_CHANNEL:
            continue

        text = token.text or ""
        if token.type == MDLLexer.NEWLINE:
            if bracket_depth > 0:
                continue
            next_token = _pop_next_significant(pending)
            if next_token is None:
                continue
            if next_token.type == AntlrToken.EOF:
                continue
            indent = max(0, int(next_token.column or 0))
            tokens.append(make_token(MDLLexer.NEWLINE, "<NEWLINE>", token.line, len(text)))
            if indent > indents[-1]:
                indents.append(indent)
                tokens.append(make_token(MDLParser.INDENT, "<INDENT>", next_token.line, 0))
            else:
                while indent < indents[-1]:
                    indents.pop()
                    tokens.append(make_token(MDLParser.DEDENT, "<DEDENT>", next_token.line, 0))
                if indent != indents[-1]:
                    raise ParseError("inconsistent indentation", next_token.line, 1)
            pending.appendleft(next_token)
            continue

        if token.type in {MDLLexer.LPAREN, MDLLexer.LBRACE}:
            bracket_depth += 1
        elif token.type in {MDLLexer.RPAREN, MDLLexer.RBRACE}:
            bracket_depth -= 1
            if bracket_depth < 0:
                raise ParseError("unmatched closing delimiter", token.line, token.column + 1)
        tokens.append(token)

    eof_line = last_line + 1
    while len(indents) > 1:
        indents.pop()
        tokens.append(make_token(MDLParser.DEDENT, "<DEDENT>", eof_line, 0))
    tokens.append(make_token(AntlrToken.EOF, "<EOF>", eof_line, 0))
    return tokens


def _pop_next_significant(pending: deque[AntlrToken]) -> AntlrToken | None:
    while pending:
        token = pending.popleft()
        if token.channel == AntlrToken.HIDDEN_CHANNEL:
            continue
        if token.type == MDLLexer.NEWLINE:
            continue
        return token
    return None


def parse_with_rule(source: str, rule: str) -> SyntaxResult:
    token_source = ListTokenSource(visible_tokens(source))
    stream = CommonTokenStream(cast(Lexer, cast(object, token_source)))
    parser = MDLParser(stream)
    listener = RaisingErrorListener()
    parser.removeErrorListeners()
    parser.addErrorListener(listener)
    tree = cast(ParseTree, getattr(parser, rule)())
    return SyntaxResult(parser=parser, tree=tree)
