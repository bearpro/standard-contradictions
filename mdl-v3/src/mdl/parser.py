from __future__ import annotations

from fractions import Fraction
from typing import Iterable

from . import ast as A
from .diagnostics import ParseError
from .lexer import Token, tokenize


NAME_STOPWORDS = {
    "module", "import", "as", "exposing", "private", "public", "type", "val", "let", "func",
    "entity", "event", "rule", "strict", "defeasible", "defeater", "priority", "override",
    "fact", "assert", "align", "to", "if", "then", "else", "case", "switch", "when",
    "in", "forall", "exists", "and", "or", "not", "implies", "iff", "always", "eventually",
    "next", "weak_next", "never", "until", "release", "weak_until", "otherwise",
}

BINARY_PRECEDENCE: dict[str, tuple[int, str]] = {
    "implies": (1, "right"), "->": (1, "right"),
    "iff": (2, "left"), "<->": (2, "left"),
    "or": (3, "left"),
    "and": (4, "left"),
    "until": (5, "left"), "release": (5, "left"), "weak_until": (5, "left"),
    "=": (6, "left"), "==": (6, "left"), "!=": (6, "left"),
    "<": (6, "left"), "<=": (6, "left"), ">": (6, "left"), ">=": (6, "left"),
    "+": (7, "left"), "-": (7, "left"),
    "*": (8, "left"), "/": (8, "left"), "%": (8, "left"),
}

TEMPORAL_PREFIX = {"always", "eventually", "next", "weak_next", "never"}
TEMPORAL_POSTFIX = TEMPORAL_PREFIX
TEMPORAL_BINARY = {"until", "release", "weak_until"}


class Parser:
    def __init__(self, tokens: list[Token]):
        self.tokens = tokens
        self.pos = 0
        self._anonymous_rule_counter = 0

    # ---------------------------
    # Basic token helpers
    # ---------------------------

    def current(self) -> Token:
        return self.tokens[self.pos]

    def peek(self, offset: int = 0) -> Token:
        idx = min(self.pos + offset, len(self.tokens) - 1)
        return self.tokens[idx]

    def at_end(self) -> bool:
        return self.current().type == "EOF"

    def advance(self) -> Token:
        tok = self.current()
        if not self.at_end():
            self.pos += 1
        return tok

    def match_value(self, *values: str) -> Token | None:
        if self.current().value in values:
            return self.advance()
        return None

    def match_type(self, *types: str) -> Token | None:
        if self.current().type in types:
            return self.advance()
        return None

    def expect_value(self, value: str) -> Token:
        tok = self.current()
        if tok.value == value:
            return self.advance()
        raise ParseError(f"expected {value!r}, got {tok.value!r}", tok.line, tok.column)

    def expect_type(self, typ: str) -> Token:
        tok = self.current()
        if tok.type == typ:
            return self.advance()
        raise ParseError(f"expected {typ}, got {tok.value!r}", tok.line, tok.column)

    def skip_newlines(self) -> None:
        while self.current().type == "NEWLINE":
            self.advance()

    def optional_newline(self) -> None:
        if self.current().type == "NEWLINE":
            self.skip_newlines()

    def is_name_token(self, tok: Token | None = None) -> bool:
        tok = tok or self.current()
        if tok.type == "IDENT":
            return True
        if tok.type == "KEYWORD" and tok.value not in NAME_STOPWORDS:
            return True
        return False

    def expect_name_token(self) -> Token:
        tok = self.current()
        if self.is_name_token(tok):
            return self.advance()
        raise ParseError(f"expected identifier, got {tok.value!r}", tok.line, tok.column)

    # ---------------------------
    # Program and declarations
    # ---------------------------

    def parse_program(self) -> A.Module:
        self.skip_newlines()
        module_annotations = self.collect_annotations()
        self.expect_value("module")
        mod_tok = self.current()
        module_name = self.parse_qualified_name()
        module = A.Module(name=module_name, annotations=module_annotations, line=mod_tok.line, column=mod_tok.column)
        self.optional_newline()

        while not self.at_end():
            self.skip_newlines()
            if self.at_end():
                break
            annotations = self.collect_annotations()
            if self.at_end():
                break
            if self.current().value == "import":
                imp = self.parse_import()
                imp.annotations = annotations
                module.imports.append(imp)
            else:
                decl = self.parse_declaration()
                decl.annotations = annotations
                module.declarations.append(decl)
            self.optional_newline()
        return module

    def collect_annotations(self) -> list[str]:
        anns: list[str] = []
        while self.current().type == "ANNOT":
            anns.append(self.advance().value)
            self.optional_newline()
        return anns

    def parse_import(self) -> A.ImportDecl:
        tok = self.expect_value("import")
        path = self.parse_qualified_name()
        alias = None
        exposing: list[tuple[str, str | None]] = []
        if self.match_value("as"):
            alias = self.expect_name_token().value
        if self.match_value("exposing"):
            self.expect_value("(")
            if not self.match_value(")"):
                while True:
                    name = self.expect_name_token().value
                    renamed = None
                    if self.match_value("as"):
                        renamed = self.expect_name_token().value
                    exposing.append((name, renamed))
                    if self.match_value(","):
                        if self.current().value == ")":
                            break
                        continue
                    break
                self.expect_value(")")
        return A.ImportDecl(path=path, alias=alias, exposing=exposing, line=tok.line, column=tok.column)

    def parse_declaration(self) -> A.Declaration:
        visibility = "public"
        if self.current().value in {"public", "private"}:
            visibility = self.advance().value

        strength = None
        if self.current().value in {"strict", "defeasible", "defeater"}:
            strength = self.advance().value

        tok = self.current()
        if tok.value == "type":
            return self.parse_type_decl(visibility)
        if tok.value in {"val", "let"}:
            return self.parse_value_decl(visibility)
        if tok.value == "func":
            return self.parse_func_decl(visibility)
        if tok.value == "entity":
            return self.parse_entity_decl(visibility)
        if tok.value == "event":
            return self.parse_event_decl(visibility)
        if tok.value == "rule":
            return self.parse_rule_decl(visibility, strength or "defeasible")
        if strength is not None:
            raise ParseError("rule strength must be followed by 'rule'", tok.line, tok.column)
        if tok.value in {"priority", "override"}:
            return self.parse_priority_decl(visibility)
        if tok.value == "fact":
            return self.parse_fact_decl(visibility)
        if tok.value == "assert":
            return self.parse_assert_decl(visibility)
        if tok.value == "align":
            return self.parse_align_decl(visibility)
        raise ParseError(f"unexpected declaration {tok.value!r}", tok.line, tok.column)

    def parse_type_decl(self, visibility: str) -> A.TypeDecl:
        tok = self.expect_value("type")
        name = self.expect_name_token().value
        params = self.parse_type_params() if self.current().value == "<" else []
        self.expect_value("=")
        definition = self.parse_type_definition()
        return A.TypeDecl(visibility=visibility, name=name, params=params, definition=definition, line=tok.line, column=tok.column)

    def parse_value_decl(self, visibility: str) -> A.ValueDecl:
        tok = self.advance()  # val | let
        name = self.expect_name_token().value
        ann = self.parse_optional_type_annotation()
        self.expect_value("=")
        value = self.parse_expr()
        return A.ValueDecl(visibility=visibility, name=name, type_annotation=ann, value=value, line=tok.line, column=tok.column)

    def parse_func_decl(self, visibility: str) -> A.FuncDecl:
        tok = self.expect_value("func")
        name = self.expect_name_token().value
        type_params = self.parse_type_params() if self.current().value == "<" else []
        self.expect_value("(")
        params: list[A.Param] = []
        if not self.match_value(")"):
            while True:
                p_tok = self.current()
                pattern = self.parse_pattern()
                self.expect_value(":")
                type_ann = self.parse_type_expr()
                params.append(A.Param(pattern=pattern, type_annotation=type_ann, line=p_tok.line, column=p_tok.column))
                if self.match_value(","):
                    if self.current().value == ")":
                        break
                    continue
                break
            self.expect_value(")")
        self.expect_value("->")
        ret = self.parse_type_expr()
        self.expect_value(":")
        body = self.parse_block()
        return A.FuncDecl(
            visibility=visibility,
            name=name,
            params=params,
            return_type=ret,
            body=body,
            type_params=type_params,
            line=tok.line,
            column=tok.column,
        )

    def parse_entity_decl(self, visibility: str) -> A.EntityDecl:
        tok = self.expect_value("entity")
        name = self.expect_name_token().value
        self.expect_value(":")
        typ = self.parse_type_expr()
        clauses: list[tuple[str, A.Expr]] = []
        while self.current().value in {"key", "where"}:
            kind = self.advance().value
            if kind == "key":
                self.expect_value("(")
                expr = self.parse_expr()
                self.expect_value(")")
            else:
                expr = self.parse_expr()
            clauses.append((kind, expr))
        return A.EntityDecl(visibility=visibility, name=name, type_annotation=typ, clauses=clauses, line=tok.line, column=tok.column)

    def parse_event_decl(self, visibility: str) -> A.EventDecl:
        tok = self.expect_value("event")
        name = self.expect_name_token().value
        fields: list[tuple[str, A.TypeExpr]] = []
        if self.match_value("("):
            if not self.match_value(")"):
                while True:
                    fname = self.expect_name_token().value
                    self.expect_value(":")
                    ftype = self.parse_type_expr()
                    fields.append((fname, ftype))
                    if self.match_value(","):
                        if self.current().value == ")":
                            break
                        continue
                    break
                self.expect_value(")")
        return A.EventDecl(visibility=visibility, name=name, fields=fields, line=tok.line, column=tok.column)

    def parse_rule_decl(self, visibility: str, strength: str) -> A.RuleDecl:
        tok = self.expect_value("rule")
        modality = None
        name = ""
        anonymous = False
        antecedent = None

        if self.current().value in {"O", "P", "F"}:
            modality = self.advance().value
            if self.looks_like_rule_name_after_modality():
                name = self.parse_qualified_name()
                if self.match_value("when"):
                    antecedent = self.parse_expr()
                self.expect_value("=")
                body = self.parse_expr()
            else:
                anonymous = True
                name = self.next_anonymous_rule_name(tok.line)
                body = self.parse_expr()
        else:
            name = self.parse_qualified_name()
            if self.match_value(":"):
                if self.current().value not in {"O", "P", "F"}:
                    raise ParseError("expected deontic modality after ':'", self.current().line, self.current().column)
                modality = self.advance().value
            if self.match_value("when"):
                antecedent = self.parse_expr()
            self.expect_value("=")
            body = self.parse_expr()

        otherwise = None
        if self.match_value("otherwise"):
            otherwise = self.parse_expr()
        return A.RuleDecl(
            visibility=visibility,
            strength=strength,
            modality=modality,
            name=name,
            body=body,
            antecedent=antecedent,
            otherwise=otherwise,
            anonymous=anonymous,
            line=tok.line,
            column=tok.column,
        )

    def looks_like_rule_name_after_modality(self) -> bool:
        if not self.is_name_token():
            return False
        i = self.pos + 1
        # Accept qualified names as names only when followed by '=' or 'when'.
        while i + 1 < len(self.tokens) and self.tokens[i].value == "." and self.is_name_token(self.tokens[i + 1]):
            i += 2
        return self.tokens[i].value in {"=", "when"}

    def next_anonymous_rule_name(self, line: int) -> str:
        self._anonymous_rule_counter += 1
        return f"anonymous_rule_{line}_{self._anonymous_rule_counter}"

    def parse_priority_decl(self, visibility: str) -> A.PriorityDecl:
        tok = self.advance()  # priority | override
        chain = [self.parse_qualified_name()]
        while self.match_value(">"):
            chain.append(self.parse_qualified_name())
        return A.PriorityDecl(visibility=visibility, chain=chain, line=tok.line, column=tok.column)

    def parse_fact_decl(self, visibility: str) -> A.FactDecl:
        tok = self.expect_value("fact")
        target = None
        if self.is_name_token() and self.peek(1).value == "=":
            target = self.advance().value
            self.expect_value("=")
        value = self.parse_expr()
        return A.FactDecl(visibility=visibility, target=target, value=value, line=tok.line, column=tok.column)

    def parse_assert_decl(self, visibility: str) -> A.AssertDecl:
        tok = self.expect_value("assert")
        expr = self.parse_expr()
        return A.AssertDecl(visibility=visibility, expr=expr, line=tok.line, column=tok.column)

    def parse_align_decl(self, visibility: str) -> A.AlignDecl:
        tok = self.expect_value("align")
        subject = self.parse_qualified_name()
        self.expect_value("to")
        if self.current().value == "<":
            # IRI literal, kept with angle brackets.
            start = self.advance()
            parts = []
            while self.current().value != ">":
                if self.current().type == "EOF":
                    raise ParseError("unterminated IRI literal", start.line, start.column)
                parts.append(self.advance().value)
            self.expect_value(">")
            target = "<" + "".join(parts) + ">"
        elif self.current().type == "STRING":
            target = self.advance().value
        else:
            target = self.parse_qualified_name()
        kind = "equivalent"
        if self.current().value in {"equivalent", "broader", "narrower", "related"}:
            kind = self.advance().value
        return A.AlignDecl(visibility=visibility, subject=subject, target=target, kind=kind, line=tok.line, column=tok.column)

    # ---------------------------
    # Types
    # ---------------------------

    def parse_type_params(self) -> list[str]:
        self.expect_value("<")
        params: list[str] = []
        if not self.match_value(">"):
            while True:
                params.append(self.expect_name_token().value)
                if self.match_value(","):
                    if self.current().value == ">":
                        break
                    continue
                break
            self.expect_value(">")
        return params

    def parse_optional_type_annotation(self) -> A.TypeExpr | None:
        if self.match_value(":"):
            return self.parse_type_expr()
        return None

    def parse_type_definition(self) -> A.TypeExpr | A.SumType:
        if self.current().value == "{":
            return self.parse_type_expr()
        save = self.pos
        if self.is_name_token():
            first = self.parse_variant()
            if self.current().value == "|":
                variants = [first]
                while self.match_value("|"):
                    variants.append(self.parse_variant())
                return A.SumType(variants=variants, line=first.line, column=first.column)
            # A constructor with fields is a one-constructor sum type.
            if first.fields:
                return A.SumType(variants=[first], line=first.line, column=first.column)
            self.pos = save
        return self.parse_type_expr()

    def parse_variant(self) -> A.Variant:
        tok = self.expect_name_token()
        fields: list[tuple[str | None, A.TypeExpr]] = []
        if self.match_value("("):
            if not self.match_value(")"):
                while True:
                    label = None
                    # Named variant field: label: Type
                    if self.is_name_token() and self.peek(1).value == ":":
                        label = self.advance().value
                        self.expect_value(":")
                    typ = self.parse_type_expr()
                    fields.append((label, typ))
                    if self.match_value(","):
                        if self.current().value == ")":
                            break
                        continue
                    break
                self.expect_value(")")
        return A.Variant(name=tok.value, fields=fields, line=tok.line, column=tok.column)

    def parse_type_expr(self) -> A.TypeExpr:
        tok = self.current()
        if self.match_value("{"):
            fields: list[tuple[str, A.TypeExpr]] = []
            if not self.match_value("}"):
                while True:
                    name = self.expect_name_token().value
                    self.expect_value(":")
                    typ = self.parse_type_expr()
                    fields.append((name, typ))
                    if self.match_value(","):
                        if self.current().value == "}":
                            break
                        continue
                    break
                self.expect_value("}")
            return A.RecordType(fields=fields, line=tok.line, column=tok.column)
        if self.match_value("("):
            first = self.parse_type_expr()
            if self.match_value(","):
                items = [first, self.parse_type_expr()]
                while self.match_value(","):
                    if self.current().value == ")":
                        break
                    items.append(self.parse_type_expr())
                self.expect_value(")")
                return A.TupleType(items=items, line=tok.line, column=tok.column)
            self.expect_value(")")
            return first
        name = self.parse_qualified_name()
        args: list[A.TypeExpr] = []
        if self.match_value("<"):
            if not self.match_value(">"):
                while True:
                    args.append(self.parse_type_expr())
                    if self.match_value(","):
                        if self.current().value == ">":
                            break
                        continue
                    break
                self.expect_value(">")
        return A.TypeRef(name=name, args=args, line=tok.line, column=tok.column)

    # ---------------------------
    # Blocks and expressions
    # ---------------------------

    def parse_block(self) -> A.Block:
        tok = self.current()
        if self.match_type("NEWLINE"):
            self.expect_type("INDENT")
            statements: list[A.LetStmt] = []
            result: A.Expr | None = None
            while self.current().type not in {"DEDENT", "EOF"}:
                if self.match_type("NEWLINE"):
                    continue
                if self.current().value == "let":
                    stmt_tok = self.advance()
                    pat = self.parse_pattern()
                    ann = self.parse_optional_type_annotation()
                    self.expect_value("=")
                    val = self.parse_expr()
                    statements.append(A.LetStmt(pattern=pat, value=val, type_annotation=ann, line=stmt_tok.line, column=stmt_tok.column))
                    self.optional_newline()
                else:
                    result = self.parse_expr()
                    self.optional_newline()
            self.expect_type("DEDENT")
            return A.Block(statements=statements, result=result, line=tok.line, column=tok.column)
        expr = self.parse_expr()
        return A.Block(result=expr, line=expr.line, column=expr.column)

    def parse_expr(self, min_prec: int = 0) -> A.Expr:
        left = self.parse_prefix_expr()
        while True:
            tok = self.current()
            op = tok.value
            if op not in BINARY_PRECEDENCE:
                break
            prec, assoc = BINARY_PRECEDENCE[op]
            if prec < min_prec:
                break
            self.advance()
            next_min = prec if assoc == "right" else prec + 1
            right = self.parse_expr(next_min)
            if op in TEMPORAL_BINARY:
                left = A.TemporalBinary(op=op, left=left, right=right, line=tok.line, column=tok.column)
            else:
                left = A.BinaryOp(op="=" if op == "==" else op, left=left, right=right, line=tok.line, column=tok.column)
        return left

    def parse_prefix_expr(self) -> A.Expr:
        tok = self.current()
        if tok.value == "if":
            return self.parse_if_expr()
        if tok.value == "let":
            return self.parse_let_expr()
        if tok.value in {"case", "switch"}:
            return self.parse_match_expr()
        if tok.value in {"forall", "exists"}:
            return self.parse_quantifier_expr()
        if tok.value == "not":
            self.advance()
            return A.UnaryOp(op="not", operand=self.parse_prefix_expr(), line=tok.line, column=tok.column)
        if tok.value == "-":
            self.advance()
            return A.UnaryOp(op="-", operand=self.parse_prefix_expr(), line=tok.line, column=tok.column)
        if tok.value in TEMPORAL_PREFIX:
            self.advance()
            return A.TemporalUnary(op=tok.value, operand=self.parse_prefix_expr(), position="prefix", line=tok.line, column=tok.column)
        return self.parse_postfix_expr()

    def parse_if_expr(self) -> A.IfExpr:
        tok = self.expect_value("if")
        condition = self.parse_expr()
        self.skip_newlines()
        self.expect_value("then")
        then_branch = self.parse_expr()
        self.skip_newlines()
        self.expect_value("else")
        else_branch = self.parse_expr()
        return A.IfExpr(condition=condition, then_branch=then_branch, else_branch=else_branch, line=tok.line, column=tok.column)

    def parse_let_expr(self) -> A.LetExpr:
        tok = self.expect_value("let")
        pat = self.parse_pattern()
        self.parse_optional_type_annotation()  # type information is intentionally ignored in expression-level let AST.
        self.expect_value("=")
        value = self.parse_expr()
        self.skip_newlines()
        self.expect_value("in")
        body = self.parse_expr()
        return A.LetExpr(pattern=pat, value=value, body=body, line=tok.line, column=tok.column)

    def parse_match_expr(self) -> A.MatchExpr:
        tok = self.advance()  # case | switch
        subject = self.parse_expr()
        self.expect_value(":")
        self.expect_type("NEWLINE")
        has_arm_indent = False
        if self.current().type == "INDENT":
            has_arm_indent = True
            self.advance()
        arms: list[A.MatchArm] = []
        while not self.at_end():
            if self.match_type("NEWLINE"):
                continue
            if has_arm_indent and self.current().type == "DEDENT":
                break
            if not has_arm_indent and self.current().value != "|":
                break
            arm_tok = self.expect_value("|")
            pattern = self.parse_pattern()
            guard = None
            if self.match_value("when"):
                guard = self.parse_expr()
            self.expect_value(":")
            body = self.parse_block()
            arms.append(A.MatchArm(pattern=pattern, guard=guard, body=body, line=arm_tok.line, column=arm_tok.column))
            self.optional_newline()
        if has_arm_indent:
            self.expect_type("DEDENT")
        if not arms:
            raise ParseError("case expression must contain at least one arm", tok.line, tok.column)
        return A.MatchExpr(subject=subject, arms=arms, line=tok.line, column=tok.column)

    def parse_quantifier_expr(self) -> A.QuantifierExpr:
        tok = self.advance()  # forall | exists
        pattern = self.parse_pattern()
        self.expect_value("in")
        domain = self.parse_expr()
        self.expect_value(":")
        body = self.parse_expr()
        return A.QuantifierExpr(quantifier=tok.value, pattern=pattern, domain=domain, body=body, line=tok.line, column=tok.column)

    def parse_postfix_expr(self) -> A.Expr:
        expr = self.parse_primary_expr()
        while True:
            tok = self.current()
            if self.match_value("("):
                args: list[A.Expr] = []
                if not self.match_value(")"):
                    while True:
                        args.append(self.parse_expr())
                        if self.match_value(","):
                            if self.current().value == ")":
                                break
                            continue
                        break
                    self.expect_value(")")
                expr = A.Call(func=expr, args=args, line=tok.line, column=tok.column)
                continue
            if self.match_value("."):
                name_tok = self.expect_name_token()
                expr = A.FieldAccess(target=expr, field=name_tok.value, line=tok.line, column=tok.column)
                continue
            if self.match_value("["):
                idx = self.parse_expr()
                self.expect_value("]")
                expr = A.IndexAccess(target=expr, index=idx, line=tok.line, column=tok.column)
                continue
            if tok.value in TEMPORAL_POSTFIX:
                self.advance()
                expr = A.TemporalUnary(op=tok.value, operand=expr, position="postfix", line=tok.line, column=tok.column)
                continue
            break
        return expr

    def parse_primary_expr(self) -> A.Expr:
        tok = self.current()
        if tok.type == "STRING":
            self.advance()
            return A.Literal(value=tok.value, kind="string", line=tok.line, column=tok.column)
        if tok.type == "CHAR":
            self.advance()
            return A.Literal(value=tok.value, kind="char", line=tok.line, column=tok.column)
        if tok.type in {"INT", "DECIMAL", "RAT"}:
            self.advance()
            if tok.type == "INT":
                return A.Literal(value=int(tok.value), kind="int", line=tok.line, column=tok.column)
            if tok.type == "DECIMAL":
                return A.Literal(value=float(tok.value), kind="decimal", line=tok.line, column=tok.column)
            return A.Literal(value=Fraction(tok.value), kind="rat", line=tok.line, column=tok.column)
        if tok.value in {"true", "false"}:
            self.advance()
            return A.Literal(value=tok.value == "true", kind="bool", line=tok.line, column=tok.column)
        if tok.value == "last":
            self.advance()
            return A.Name(name="last", line=tok.line, column=tok.column)
        if self.is_name_token(tok) or tok.value in {"O", "P", "F"}:
            return A.Name(name=self.parse_qualified_name(), line=tok.line, column=tok.column)
        if tok.value == "{":
            return self.parse_curly_expr()
        if tok.value == "#{":
            return self.parse_set_literal()
        if tok.value == "[":
            return self.parse_list_literal()
        if tok.value == "(":
            return self.parse_paren_expr()
        raise ParseError(f"expected expression, got {tok.value!r}", tok.line, tok.column)

    def parse_curly_expr(self) -> A.Expr:
        tok = self.expect_value("{")
        if self.match_value("}"):
            return A.RecordLiteral(fields=[], line=tok.line, column=tok.column)
        if self.curly_looks_like_record_literal():
            fields: list[tuple[str, A.Expr]] = []
            while True:
                name = self.expect_name_token().value
                self.expect_value("=")
                value = self.parse_expr()
                fields.append((name, value))
                if self.match_value(","):
                    if self.current().value == "}":
                        break
                    continue
                break
            self.expect_value("}")
            return A.RecordLiteral(fields=fields, line=tok.line, column=tok.column)
        expr = self.parse_expr()
        self.expect_value("}")
        return A.BracedExpr(expr=expr, line=tok.line, column=tok.column)

    def curly_looks_like_record_literal(self) -> bool:
        # To avoid ambiguity with temporal atom braces `{ a = b and c = d }`, one-field records
        # should be written with a trailing comma: `{ field = value, }`.
        if not self.is_name_token():
            return False
        if self.peek(1).value != "=":
            return False
        depth = 0
        i = self.pos
        while i < len(self.tokens):
            v = self.tokens[i].value
            if v in {"(", "[", "{"}:
                depth += 1
            elif v in {")", "]", "}"}:
                if depth == 0:
                    return False
                depth -= 1
                if depth < 0:
                    return False
                if depth == 0 and v == "}":
                    return False
            elif v == "," and depth == 0:
                return True
            elif v == "}" and depth == 0:
                return False
            i += 1
        return False

    def parse_set_literal(self) -> A.SetLiteral:
        tok = self.expect_type("SET_START")
        items = self.parse_expr_list_until("}")
        return A.SetLiteral(items=items, line=tok.line, column=tok.column)

    def parse_list_literal(self) -> A.ListLiteral:
        tok = self.expect_value("[")
        items = self.parse_expr_list_until("]")
        return A.ListLiteral(items=items, line=tok.line, column=tok.column)

    def parse_paren_expr(self) -> A.Expr:
        tok = self.expect_value("(")
        if self.match_value(")"):
            return A.TupleLiteral(items=[], line=tok.line, column=tok.column)
        first = self.parse_expr()
        if self.match_value(","):
            items = [first]
            if self.current().value != ")":
                while True:
                    items.append(self.parse_expr())
                    if self.match_value(","):
                        if self.current().value == ")":
                            break
                        continue
                    break
            self.expect_value(")")
            return A.TupleLiteral(items=items, line=tok.line, column=tok.column)
        self.expect_value(")")
        return first

    def parse_expr_list_until(self, closing: str) -> list[A.Expr]:
        items: list[A.Expr] = []
        if not self.match_value(closing):
            while True:
                items.append(self.parse_expr())
                if self.match_value(","):
                    if self.current().value == closing:
                        break
                    continue
                break
            self.expect_value(closing)
        return items

    # ---------------------------
    # Patterns
    # ---------------------------

    def parse_pattern(self) -> A.Pattern:
        tok = self.current()
        if self.match_value("_"):
            return A.WildcardPattern(line=tok.line, column=tok.column)
        if tok.type == "STRING":
            self.advance()
            return A.LiteralPattern(value=tok.value, kind="string", line=tok.line, column=tok.column)
        if tok.type == "CHAR":
            self.advance()
            return A.LiteralPattern(value=tok.value, kind="char", line=tok.line, column=tok.column)
        if tok.type in {"INT", "DECIMAL", "RAT"}:
            lit = self.parse_primary_expr()
            assert isinstance(lit, A.Literal)
            return A.LiteralPattern(value=lit.value, kind=lit.kind, line=tok.line, column=tok.column)
        if tok.value == "[":
            self.advance()
            items: list[A.Pattern] = []
            if not self.match_value("]"):
                while True:
                    items.append(self.parse_pattern())
                    if self.match_value(","):
                        if self.current().value == "]":
                            break
                        continue
                    break
                self.expect_value("]")
            return A.ListPattern(items=items, line=tok.line, column=tok.column)
        if tok.value == "(":
            self.advance()
            first = self.parse_pattern()
            items = [first]
            if self.match_value(","):
                while True:
                    items.append(self.parse_pattern())
                    if self.match_value(","):
                        if self.current().value == ")":
                            break
                        continue
                    break
                self.expect_value(")")
                return A.TuplePattern(items=items, line=tok.line, column=tok.column)
            self.expect_value(")")
            return first
        if tok.value == "{":
            self.advance()
            fields: list[tuple[str, A.Pattern | None]] = []
            if not self.match_value("}"):
                while True:
                    name = self.expect_name_token().value
                    pat = None
                    if self.match_value("="):
                        pat = self.parse_pattern()
                    fields.append((name, pat))
                    if self.match_value(","):
                        if self.current().value == "}":
                            break
                        continue
                    break
                self.expect_value("}")
            return A.RecordPattern(fields=fields, line=tok.line, column=tok.column)
        if self.is_name_token(tok):
            name = self.parse_qualified_name()
            is_constructor = name.split(".")[-1][:1].isupper() or "." in name
            if is_constructor:
                args: list[A.Pattern] = []
                if self.match_value("("):
                    if not self.match_value(")"):
                        while True:
                            args.append(self.parse_pattern())
                            if self.match_value(","):
                                if self.current().value == ")":
                                    break
                                continue
                            break
                        self.expect_value(")")
                return A.ConstructorPattern(name=name, args=args, line=tok.line, column=tok.column)
            return A.VarPattern(name=name, line=tok.line, column=tok.column)
        raise ParseError(f"expected pattern, got {tok.value!r}", tok.line, tok.column)

    # ---------------------------
    # Names
    # ---------------------------

    def parse_qualified_name(self) -> str:
        parts = [self.expect_name_token().value]
        while self.match_value("."):
            parts.append(self.expect_name_token().value)
        return ".".join(parts)


def parse(source: str) -> A.Module:
    return Parser(tokenize(source)).parse_program()


def parse_tokens(tokens: list[Token]) -> A.Module:
    return Parser(tokens).parse_program()


def parse_expr(source: str) -> A.Expr:
    parser = Parser(tokenize(source))
    parser.skip_newlines()
    expr = parser.parse_expr()
    parser.skip_newlines()
    if not parser.at_end():
        tok = parser.current()
        raise ParseError(f"unexpected token after expression: {tok.value!r}", tok.line, tok.column)
    return expr


def parse_many(sources: Iterable[str]) -> list[A.Module]:
    return [parse(source) for source in sources]


def parse_type_expr_source(source: str) -> A.TypeExpr:
    parser = Parser(tokenize(source))
    parser.skip_newlines()
    typ = parser.parse_type_expr()
    parser.skip_newlines()
    if not parser.at_end():
        tok = parser.current()
        raise ParseError(f"unexpected token after type expression: {tok.value!r}", tok.line, tok.column)
    return typ
