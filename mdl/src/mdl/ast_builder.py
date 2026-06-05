from __future__ import annotations
# pyright: reportIncompatibleMethodOverride=false, reportArgumentType=false, reportAttributeAccessIssue=false, reportReturnType=false, reportGeneralTypeIssues=false, reportOptionalIterable=false, reportOptionalMemberAccess=false, reportCallIssue=false, reportIndexIssue=false, reportOptionalSubscript=false

from fractions import Fraction
from typing import Any

from antlr4.tree.Tree import TerminalNode

from . import ast as A
from ._antlr.MDLLexer import MDLLexer
from ._antlr.MDLParser import MDLParser
from ._antlr.MDLVisitor import MDLVisitor
from .diagnostics import ParseError
from .names import is_qualified, local_name


TEMPORAL_BINARY = {"until", "release", "weak_until"}
LAYOUT_TOKEN_TYPES = {MDLParser.INDENT, MDLParser.DEDENT, MDLLexer.NEWLINE, -1}


class AstBuilder(MDLVisitor):
    def __init__(self) -> None:
        super().__init__()
        self._anonymous_rule_counter = 0

    def visit(self, tree: Any) -> Any:
        result = super().visit(tree)
        if isinstance(result, A.Node):
            self.apply_span(result, tree)
        return result

    def location(self, ctx: Any) -> tuple[int, int]:
        token = ctx.start
        return int(token.line or 1), int(token.column or 0) + 1

    def token_location(self, token: Any) -> tuple[int, int]:
        return int(token.line or 1), int(token.column or 0) + 1

    def apply_span(self, node: A.Node, ctx: Any, *, overwrite_end: bool = False) -> A.Node:
        if not overwrite_end and node.end_line and node.end_column:
            return node
        token = self.last_source_token(ctx) or getattr(ctx, "stop", None) or getattr(ctx, "start", None)
        if token is None:
            return node
        node.end_line, node.end_column = self.token_end_location(token)
        return node

    def token_end_location(self, token: Any) -> tuple[int, int]:
        text = getattr(token, "text", None)
        if text is None:
            start = getattr(token, "start", -1)
            stop = getattr(token, "stop", -1)
            text_length = max(1, int(stop) - int(start) + 1) if start >= 0 and stop >= start else 1
        else:
            text_length = max(1, len(str(text)))
        return int(token.line or 1), int(token.column or 0) + text_length + 1

    def last_source_token(self, tree: Any) -> Any | None:
        if isinstance(tree, TerminalNode):
            token = tree.symbol
            return token if self.is_source_token(token) else None
        children = getattr(tree, "children", None) or []
        for child in reversed(children):
            token = self.last_source_token(child)
            if token is not None:
                return token
        token = getattr(tree, "stop", None)
        return token if self.is_source_token(token) else None

    def is_source_token(self, token: Any | None) -> bool:
        if token is None:
            return False
        return int(getattr(token, "type", -1)) not in LAYOUT_TOKEN_TYPES

    def visitProgram(self, ctx: MDLParser.ProgramContext) -> A.Module:
        module = self.visit(ctx.moduleDecl())
        module.annotations = self.visit(ctx.annotations())
        for item in ctx.topItem():
            annotations, value = self.visit(item)
            value.annotations = annotations
            if isinstance(value, A.ImportDecl):
                module.imports.append(value)
            elif isinstance(value, A.OpenDecl):
                module.opens.append(value)
            else:
                module.declarations.append(value)
        self.apply_span(module, ctx, overwrite_end=True)
        return module

    def visitTopItem(self, ctx: MDLParser.TopItemContext) -> tuple[list[str], Any]:
        annotations = self.visit(ctx.annotations())
        for child in ctx.getChildren():
            if isinstance(child, (MDLParser.ImportDeclContext, MDLParser.OpenDeclContext, MDLParser.DeclarationContext)):
                return annotations, self.visit(child)
        raise self.error(ctx, "expected top-level item")

    def visitAnnotations(self, ctx: MDLParser.AnnotationsContext) -> list[str]:
        return [token.getText()[1:].strip() for token in ctx.ANNOT()]

    def visitModuleDecl(self, ctx: MDLParser.ModuleDeclContext) -> A.Module:
        line, column = self.location(ctx.qualifiedName())
        return A.Module(name=self.visit(ctx.qualifiedName()), line=line, column=column)

    def visitImportDecl(self, ctx: MDLParser.ImportDeclContext) -> A.ImportDecl:
        line, column = self.location(ctx)
        return A.ImportDecl(path=self.string_value(ctx.STRING().getText()), line=line, column=column)

    def visitOpenDecl(self, ctx: MDLParser.OpenDeclContext) -> A.OpenDecl:
        line, column = self.location(ctx)
        return A.OpenDecl(module=self.visit(ctx.qualifiedName()), line=line, column=column)

    def visitDeclaration(self, ctx: MDLParser.DeclarationContext) -> A.Declaration:
        return self.visit(ctx.getChild(0))

    def visitTypeDecl(self, ctx: MDLParser.TypeDeclContext) -> A.TypeDecl:
        line, column = self.location(ctx)
        params = self.visit(ctx.typeParams()) if ctx.typeParams() else []
        return A.TypeDecl(
            name=self.visit(ctx.nameToken()),
            params=params,
            definition=self.visit(ctx.typeDefinition()),
            line=line,
            column=column,
        )

    def visitTypeDefinition(self, ctx: MDLParser.TypeDefinitionContext) -> A.TypeExpr | A.SumType:
        if ctx.recordType():
            return self.visit(ctx.recordType())
        variants = [self.visit(variant) for variant in ctx.variant()]
        line, column = self.location(variants[0]) if False else (variants[0].line, variants[0].column)
        return A.SumType(variants=variants, line=line, column=column)

    def visitTypeParams(self, ctx: MDLParser.TypeParamsContext) -> list[str]:
        return self.visit(ctx.nameList()) if ctx.nameList() else []

    def visitNameList(self, ctx: MDLParser.NameListContext) -> list[str]:
        return [self.visit(name) for name in ctx.nameToken()]

    def visitVariant(self, ctx: MDLParser.VariantContext) -> A.Variant:
        line, column = self.location(ctx)
        name = self.visit(ctx.nameToken())
        fields = self.visit(ctx.variantFieldList())
        return A.Variant(name=name, fields=fields, line=line, column=column)

    def visitVariantFieldList(self, ctx: MDLParser.VariantFieldListContext) -> list[tuple[str | None, A.TypeExpr]]:
        return [self.visit(field) for field in ctx.variantField()]

    def visitVariantField(self, ctx: MDLParser.VariantFieldContext) -> tuple[str | None, A.TypeExpr]:
        if ctx.nameToken():
            return self.visit(ctx.nameToken()), self.visit(ctx.typeExpr())
        return None, self.visit(ctx.typeExpr())

    def visitTypeExpr(self, ctx: MDLParser.TypeExprContext) -> A.TypeExpr:
        return self.visit(ctx.getChild(0))

    def visitRecordType(self, ctx: MDLParser.RecordTypeContext) -> A.RecordType:
        line, column = self.location(ctx)
        fields = self.visit(ctx.typeFieldList()) if ctx.typeFieldList() else []
        return A.RecordType(fields=fields, line=line, column=column)

    def visitTypeFieldList(self, ctx: MDLParser.TypeFieldListContext) -> list[tuple[str, A.TypeExpr]]:
        return [self.visit(field) for field in ctx.typeField()]

    def visitTypeField(self, ctx: MDLParser.TypeFieldContext) -> tuple[str, A.TypeExpr]:
        return self.visit(ctx.nameToken()), self.visit(ctx.typeExpr())

    def visitTupleOrParenType(self, ctx: MDLParser.TupleOrParenTypeContext) -> A.TypeExpr:
        line, column = self.location(ctx)
        items = [self.visit(item) for item in ctx.typeExpr()]
        if not ctx.COMMA():
            return items[0]
        if len(items) < 2:
            raise ParseError("expected type expression after ','", line, column)
        return A.TupleType(items=items, line=line, column=column)

    def visitTypeRef(self, ctx: MDLParser.TypeRefContext) -> A.TypeRef:
        line, column = self.location(ctx)
        args = self.visit(ctx.typeArgs()) if ctx.typeArgs() else []
        return A.TypeRef(name=self.visit(ctx.qualifiedName()), args=args, line=line, column=column)

    def visitTypeArgs(self, ctx: MDLParser.TypeArgsContext) -> list[A.TypeExpr]:
        return self.visit(ctx.typeExprList()) if ctx.typeExprList() else []

    def visitTypeExprList(self, ctx: MDLParser.TypeExprListContext) -> list[A.TypeExpr]:
        return [self.visit(item) for item in ctx.typeExpr()]

    def visitValueDecl(self, ctx: MDLParser.ValueDeclContext) -> A.ValueDecl:
        line, column = self.location(ctx)
        return A.ValueDecl(
            name=self.visit(ctx.nameToken()),
            type_annotation=self.visit(ctx.typeAnnotation()) if ctx.typeAnnotation() else None,
            value=self.visit(ctx.expr()),
            line=line,
            column=column,
        )

    def visitFuncDecl(self, ctx: MDLParser.FuncDeclContext) -> A.FuncDecl:
        line, column = self.location(ctx)
        return A.FuncDecl(
            name=self.visit(ctx.nameToken()),
            params=self.visit(ctx.paramList()) if ctx.paramList() else [],
            return_type=self.visit(ctx.typeExpr()),
            body=self.visit(ctx.block()),
            type_params=self.visit(ctx.typeParams()) if ctx.typeParams() else [],
            line=line,
            column=column,
        )

    def visitParamList(self, ctx: MDLParser.ParamListContext) -> list[A.Param]:
        return [self.visit(param) for param in ctx.param()]

    def visitParam(self, ctx: MDLParser.ParamContext) -> A.Param:
        line, column = self.location(ctx)
        return A.Param(pattern=self.visit(ctx.pattern()), type_annotation=self.visit(ctx.typeExpr()), line=line, column=column)

    def visitEntityDecl(self, ctx: MDLParser.EntityDeclContext) -> A.EntityDecl:
        line, column = self.location(ctx)
        return A.EntityDecl(
            name=self.visit(ctx.nameToken()),
            type_annotation=self.visit(ctx.typeExpr()),
            line=line,
            column=column,
        )

    def visitEventDecl(self, ctx: MDLParser.EventDeclContext) -> A.EventDecl:
        line, column = self.location(ctx)
        return A.EventDecl(
            name=self.visit(ctx.nameToken()),
            fields=self.visit(ctx.typeFieldList()) if ctx.typeFieldList() else [],
            line=line,
            column=column,
        )

    def visitRuleDecl(self, ctx: MDLParser.RuleDeclContext) -> A.RuleDecl:
        line, column = self.location(ctx)
        strength = self.visit(ctx.ruleStrength()) if ctx.ruleStrength() else "defeasible"
        modality, name, anonymous, antecedent, body = self.visit(ctx.ruleBody())
        otherwise = self.visit(ctx.expr()) if ctx.expr() else None
        return A.RuleDecl(
            strength=strength,
            modality=modality,
            name=name,
            body=body,
            antecedent=antecedent,
            otherwise=otherwise,
            anonymous=anonymous,
            line=line,
            column=column,
        )

    def visitRuleStrength(self, ctx: MDLParser.RuleStrengthContext) -> str:
        return ctx.getText()

    def visitRuleBody(self, ctx: MDLParser.RuleBodyContext) -> tuple[str | None, str, bool, A.Expr | None, A.Expr]:
        modality = self.visit(ctx.deonticMod()) if ctx.deonticMod() else None
        if ctx.qualifiedName() is None:
            line = ctx.start.line
            return modality, self.next_anonymous_rule_name(line), True, None, self.visit(ctx.expr(0))
        exprs = ctx.expr()
        antecedent = None
        body_index = 0
        if ctx.WHEN():
            antecedent = self.visit(exprs[0])
            body_index = 1
        return modality, self.visit(ctx.qualifiedName()), False, antecedent, self.visit(exprs[body_index])

    def visitDeonticMod(self, ctx: MDLParser.DeonticModContext) -> str:
        return ctx.getText()

    def next_anonymous_rule_name(self, line: int) -> str:
        self._anonymous_rule_counter += 1
        return f"anonymous_rule_{line}_{self._anonymous_rule_counter}"

    def visitPriorityDecl(self, ctx: MDLParser.PriorityDeclContext) -> A.PriorityDecl:
        line, column = self.location(ctx)
        return A.PriorityDecl(chain=[self.visit(name) for name in ctx.qualifiedName()], line=line, column=column)

    def visitFactDecl(self, ctx: MDLParser.FactDeclContext) -> A.FactDecl:
        line, column = self.location(ctx)
        return A.FactDecl(
            target=self.visit(ctx.nameToken()) if ctx.nameToken() else None,
            value=self.visit(ctx.expr()),
            line=line,
            column=column,
        )

    def visitAssertDecl(self, ctx: MDLParser.AssertDeclContext) -> A.AssertDecl:
        line, column = self.location(ctx)
        return A.AssertDecl(expr=self.visit(ctx.expr()), line=line, column=column)

    def visitAlignDecl(self, ctx: MDLParser.AlignDeclContext) -> A.AlignDecl:
        line, column = self.location(ctx)
        return A.AlignDecl(
            subject=self.visit(ctx.qualifiedName()),
            target=self.visit(ctx.alignTarget()),
            kind=self.visit(ctx.alignKind()) if ctx.alignKind() else "equivalent",
            line=line,
            column=column,
        )

    def visitAlignTarget(self, ctx: MDLParser.AlignTargetContext) -> str:
        if ctx.STRING():
            return self.string_value(ctx.STRING().getText())
        if ctx.qualifiedName():
            return self.visit(ctx.qualifiedName())
        return self.visit(ctx.iriLiteral())

    def visitIriLiteral(self, ctx: MDLParser.IriLiteralContext) -> str:
        return "".join(child.getText() for child in ctx.getChildren())

    def visitAlignKind(self, ctx: MDLParser.AlignKindContext) -> str:
        return ctx.getText()

    def visitBlock(self, ctx: MDLParser.BlockContext) -> A.Block:
        line, column = self.location(ctx)
        if ctx.INDENT():
            return A.Block(
                statements=[self.visit(stmt) for stmt in ctx.blockLetStmt()],
                result=self.visit(ctx.expr()) if ctx.expr() else None,
                line=line,
                column=column,
            )
        expr = self.visit(ctx.expr())
        return A.Block(result=expr, line=expr.line, column=expr.column)

    def visitBlockLetStmt(self, ctx: MDLParser.BlockLetStmtContext) -> A.LetStmt:
        line, column = self.location(ctx)
        return A.LetStmt(
            pattern=self.visit(ctx.pattern()),
            value=self.visit(ctx.expr()),
            type_annotation=self.visit(ctx.typeAnnotation()) if ctx.typeAnnotation() else None,
            line=line,
            column=column,
        )

    def visitTypeAnnotation(self, ctx: MDLParser.TypeAnnotationContext) -> A.TypeExpr:
        return self.visit(ctx.typeExpr())

    def visitExprOnly(self, ctx: MDLParser.ExprOnlyContext) -> A.Expr:
        return self.visit(ctx.expr())

    def visitTypeExprOnly(self, ctx: MDLParser.TypeExprOnlyContext) -> A.TypeExpr:
        return self.visit(ctx.typeExpr())

    def visitExpr(self, ctx: MDLParser.ExprContext) -> A.Expr:
        return self.visit(ctx.temporalPostfix())

    def visitTemporalPostfix(self, ctx: MDLParser.TemporalPostfixContext) -> A.Expr:
        expr = self.visit(ctx.implication())
        for op_ctx in ctx.temporalUnaryOp():
            line, column = self.location(op_ctx)
            expr = A.TemporalUnary(op=self.visit(op_ctx), operand=expr, position="postfix", line=line, column=column)
        return expr

    def visitImplication(self, ctx: MDLParser.ImplicationContext) -> A.Expr:
        return self.visit(ctx.orExpr())

    def visitOrExpr(self, ctx: MDLParser.OrExprContext) -> A.Expr:
        return self.left_assoc(ctx, ctx.andExpr())

    def visitAndExpr(self, ctx: MDLParser.AndExprContext) -> A.Expr:
        return self.left_assoc(ctx, ctx.temporalBinary())

    def visitTemporalBinary(self, ctx: MDLParser.TemporalBinaryContext) -> A.Expr:
        return self.left_assoc(ctx, ctx.comparison())

    def visitComparison(self, ctx: MDLParser.ComparisonContext) -> A.Expr:
        return self.left_assoc(ctx, ctx.additive())

    def visitAdditive(self, ctx: MDLParser.AdditiveContext) -> A.Expr:
        return self.left_assoc(ctx, ctx.multiplicative())

    def visitMultiplicative(self, ctx: MDLParser.MultiplicativeContext) -> A.Expr:
        return self.left_assoc(ctx, ctx.unary())

    def left_assoc(self, ctx: Any, operands: list[Any]) -> A.Expr:
        expr = self.visit(operands[0])
        operand_index = 1
        for child in ctx.getChildren():
            if isinstance(child, TerminalNode):
                token = child.symbol
                op = token.text
                if op in {"<EOF>"}:
                    continue
                line, column = self.token_location(token)
                right = self.visit(operands[operand_index])
                operand_index += 1
                if op in TEMPORAL_BINARY:
                    expr = A.TemporalBinary(op=op, left=expr, right=right, line=line, column=column)
                else:
                    expr = A.BinaryOp(op="=" if op == "==" else op, left=expr, right=right, line=line, column=column)
        return expr

    def visitUnary(self, ctx: MDLParser.UnaryContext) -> A.Expr:
        if ctx.ifExpr() or ctx.letExpr() or ctx.matchExpr() or ctx.postfix():
            return self.visit(ctx.getChild(0))
        token = ctx.getChild(0).symbol
        line, column = self.token_location(token)
        op = token.text
        operand = self.visit(ctx.unary())
        if op in {"always", "eventually", "next", "weak_next", "never"}:
            return A.TemporalUnary(op=op, operand=operand, position="prefix", line=line, column=column)
        return A.UnaryOp(op=op, operand=operand, line=line, column=column)

    def visitIfExpr(self, ctx: MDLParser.IfExprContext) -> A.IfExpr:
        line, column = self.location(ctx)
        exprs = ctx.expr()
        return A.IfExpr(
            condition=self.visit(exprs[0]),
            then_branch=self.visit(exprs[1]),
            else_branch=self.visit(exprs[2]),
            line=line,
            column=column,
        )

    def visitLetExpr(self, ctx: MDLParser.LetExprContext) -> A.LetExpr:
        line, column = self.location(ctx)
        exprs = ctx.expr()
        return A.LetExpr(pattern=self.visit(ctx.pattern()), value=self.visit(exprs[0]), body=self.visit(exprs[1]), line=line, column=column)

    def visitMatchExpr(self, ctx: MDLParser.MatchExprContext) -> A.MatchExpr:
        line, column = self.location(ctx)
        arms = self.visit(ctx.caseBody())
        for arm in arms:
            if arm.column <= column:
                raise ParseError("case arms must be indented deeper than the case expression", arm.line, arm.column)
        return A.MatchExpr(subject=self.visit(ctx.expr()), arms=arms, line=line, column=column)

    def visitCaseBody(self, ctx: MDLParser.CaseBodyContext) -> list[A.MatchArm]:
        arms = [self.visit(arm) for arm in ctx.caseArm()]
        if not arms:
            raise self.error(ctx, "case expression must contain at least one arm")
        return arms

    def visitCaseArm(self, ctx: MDLParser.CaseArmContext) -> A.MatchArm:
        line, column = self.location(ctx)
        return A.MatchArm(
            pattern=self.visit(ctx.pattern()),
            guard=self.visit(ctx.expr()) if ctx.expr() else None,
            body=self.visit(ctx.block()),
            line=line,
            column=column,
        )

    def visitPostfix(self, ctx: MDLParser.PostfixContext) -> A.Expr:
        expr = self.visit(ctx.primary())
        for suffix in ctx.postfixSuffix():
            line, column = self.location(suffix)
            if suffix.recordConstructorFields():
                if not isinstance(expr, A.Name):
                    raise ParseError("record expressions must use `TypeName { field = value }`", line, column)
                expr = A.RecordConstructor(
                    type_name=expr.name,
                    fields=self.visit(suffix.recordConstructorFields()),
                    line=expr.line,
                    column=expr.column,
                )
            elif suffix.exprList() is not None or suffix.LPAREN() is not None:
                args = self.visit(suffix.exprList()) if suffix.exprList() else []
                expr = A.Call(func=expr, args=args, line=line, column=column)
            elif suffix.DOT():
                expr = A.FieldAccess(target=expr, field=self.visit(suffix.nameToken()), line=line, column=column)
        return expr

    def visitRecordConstructorFields(self, ctx: MDLParser.RecordConstructorFieldsContext) -> list[tuple[str, A.Expr]]:
        return self.visit(ctx.recordConstructorFieldList()) if ctx.recordConstructorFieldList() else []

    def visitRecordConstructorFieldList(self, ctx: MDLParser.RecordConstructorFieldListContext) -> list[tuple[str, A.Expr]]:
        return [self.visit(field) for field in ctx.recordConstructorField()]

    def visitRecordConstructorField(self, ctx: MDLParser.RecordConstructorFieldContext) -> tuple[str, A.Expr]:
        return self.visit(ctx.nameToken()), self.visit(ctx.expr())

    def visitPrimary(self, ctx: MDLParser.PrimaryContext) -> A.Expr:
        line, column = self.location(ctx)
        if ctx.STRING():
            return A.Literal(value=self.string_value(ctx.STRING().getText()), kind="string", line=line, column=column)
        if ctx.INT():
            return A.Literal(value=int(ctx.INT().getText()), kind="int", line=line, column=column)
        if ctx.DECIMAL():
            return A.Literal(value=float(ctx.DECIMAL().getText()), kind="decimal", line=line, column=column)
        if ctx.RAT():
            return A.Literal(value=Fraction(ctx.RAT().getText()), kind="rat", line=line, column=column)
        if ctx.TRUE() or ctx.FALSE():
            return A.Literal(value=ctx.getText() == "true", kind="bool", line=line, column=column)
        if ctx.LAST():
            return A.Name(name="last", line=line, column=column)
        if ctx.qualifiedName():
            return A.Name(name=self.visit(ctx.qualifiedName()), line=line, column=column)
        exprs = ctx.expr()
        if not exprs:
            return A.Literal(value=None, kind="unit", line=line, column=column)
        if ctx.COMMA():
            return A.TupleLiteral(items=[self.visit(expr) for expr in exprs], line=line, column=column)
        return self.visit(exprs[0])

    def visitExprList(self, ctx: MDLParser.ExprListContext) -> list[A.Expr]:
        return [self.visit(expr) for expr in ctx.expr()]

    def visitPattern(self, ctx: MDLParser.PatternContext) -> A.Pattern:
        line, column = self.location(ctx)
        if ctx.UNDERSCORE():
            return A.WildcardPattern(line=line, column=column)
        if ctx.STRING():
            return A.LiteralPattern(value=self.string_value(ctx.STRING().getText()), kind="string", line=line, column=column)
        if ctx.INT():
            return A.LiteralPattern(value=int(ctx.INT().getText()), kind="int", line=line, column=column)
        if ctx.DECIMAL():
            return A.LiteralPattern(value=float(ctx.DECIMAL().getText()), kind="decimal", line=line, column=column)
        if ctx.RAT():
            return A.LiteralPattern(value=Fraction(ctx.RAT().getText()), kind="rat", line=line, column=column)
        if ctx.LBRACE():
            fields = self.visit(ctx.recordPatternFieldList()) if ctx.recordPatternFieldList() else []
            return A.RecordPattern(fields=fields, line=line, column=column)
        if ctx.LPAREN() and ctx.qualifiedName() is None:
            patterns = [self.visit(pattern) for pattern in ctx.pattern()]
            if not patterns:
                return A.LiteralPattern(value=None, kind="unit", line=line, column=column)
            if ctx.COMMA():
                return A.TuplePattern(items=patterns, line=line, column=column)
            return patterns[0]
        name = self.visit(ctx.qualifiedName())
        args = self.visit(ctx.patternList()) if ctx.patternList() else []
        has_constructor_args = ctx.patternList() is not None or ctx.LPAREN() is not None
        is_constructor = local_name(name)[:1].isupper() or is_qualified(name)
        if is_constructor:
            return A.ConstructorPattern(name=name, args=args, line=line, column=column)
        if has_constructor_args:
            raise ParseError(f"expected pattern, got {name!r}", line, column)
        return A.VarPattern(name=name, line=line, column=column)

    def visitPatternList(self, ctx: MDLParser.PatternListContext) -> list[A.Pattern]:
        return [self.visit(pattern) for pattern in ctx.pattern()]

    def visitRecordPatternFieldList(self, ctx: MDLParser.RecordPatternFieldListContext) -> list[tuple[str, A.Pattern | None]]:
        return [self.visit(field) for field in ctx.recordPatternField()]

    def visitRecordPatternField(self, ctx: MDLParser.RecordPatternFieldContext) -> tuple[str, A.Pattern | None]:
        return self.visit(ctx.nameToken()), self.visit(ctx.pattern()) if ctx.pattern() else None

    def visitQualifiedName(self, ctx: MDLParser.QualifiedNameContext) -> str:
        return ".".join(self.visit(name) for name in ctx.nameToken())

    def visitNameToken(self, ctx: MDLParser.NameTokenContext) -> str:
        return ctx.getText()

    def visitTemporalUnaryOp(self, ctx: MDLParser.TemporalUnaryOpContext) -> str:
        return ctx.getText()

    def string_value(self, text: str) -> str:
        chars: list[str] = []
        escaped = False
        for char in text[1:-1]:
            if escaped:
                chars.append("\\" + char)
                escaped = False
            elif char == "\\":
                escaped = True
            else:
                chars.append(char)
        if escaped:
            chars.append("\\")
        return "".join(chars)

    def error(self, ctx: Any, message: str) -> ParseError:
        line, column = self.location(ctx)
        return ParseError(message, line, column)
