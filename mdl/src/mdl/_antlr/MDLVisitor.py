# Generated from src/mdl/grammar/MDL.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .MDLParser import MDLParser
else:
    from MDLParser import MDLParser

# This class defines a complete generic visitor for a parse tree produced by MDLParser.

class MDLVisitor(ParseTreeVisitor):

    # Visit a parse tree produced by MDLParser#program.
    def visitProgram(self, ctx:MDLParser.ProgramContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#exprOnly.
    def visitExprOnly(self, ctx:MDLParser.ExprOnlyContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#typeExprOnly.
    def visitTypeExprOnly(self, ctx:MDLParser.TypeExprOnlyContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#topItem.
    def visitTopItem(self, ctx:MDLParser.TopItemContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#annotations.
    def visitAnnotations(self, ctx:MDLParser.AnnotationsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#moduleDecl.
    def visitModuleDecl(self, ctx:MDLParser.ModuleDeclContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#importDecl.
    def visitImportDecl(self, ctx:MDLParser.ImportDeclContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#openDecl.
    def visitOpenDecl(self, ctx:MDLParser.OpenDeclContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#declaration.
    def visitDeclaration(self, ctx:MDLParser.DeclarationContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#typeDecl.
    def visitTypeDecl(self, ctx:MDLParser.TypeDeclContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#typeDefinition.
    def visitTypeDefinition(self, ctx:MDLParser.TypeDefinitionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#typeParams.
    def visitTypeParams(self, ctx:MDLParser.TypeParamsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#nameList.
    def visitNameList(self, ctx:MDLParser.NameListContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#variant.
    def visitVariant(self, ctx:MDLParser.VariantContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#variantFieldList.
    def visitVariantFieldList(self, ctx:MDLParser.VariantFieldListContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#variantField.
    def visitVariantField(self, ctx:MDLParser.VariantFieldContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#typeExpr.
    def visitTypeExpr(self, ctx:MDLParser.TypeExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#recordType.
    def visitRecordType(self, ctx:MDLParser.RecordTypeContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#typeFieldList.
    def visitTypeFieldList(self, ctx:MDLParser.TypeFieldListContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#typeField.
    def visitTypeField(self, ctx:MDLParser.TypeFieldContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#tupleOrParenType.
    def visitTupleOrParenType(self, ctx:MDLParser.TupleOrParenTypeContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#typeRef.
    def visitTypeRef(self, ctx:MDLParser.TypeRefContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#typeArgs.
    def visitTypeArgs(self, ctx:MDLParser.TypeArgsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#typeExprList.
    def visitTypeExprList(self, ctx:MDLParser.TypeExprListContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#valueDecl.
    def visitValueDecl(self, ctx:MDLParser.ValueDeclContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#funcDecl.
    def visitFuncDecl(self, ctx:MDLParser.FuncDeclContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#paramList.
    def visitParamList(self, ctx:MDLParser.ParamListContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#param.
    def visitParam(self, ctx:MDLParser.ParamContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#entityDecl.
    def visitEntityDecl(self, ctx:MDLParser.EntityDeclContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#entityClause.
    def visitEntityClause(self, ctx:MDLParser.EntityClauseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#eventDecl.
    def visitEventDecl(self, ctx:MDLParser.EventDeclContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#ruleDecl.
    def visitRuleDecl(self, ctx:MDLParser.RuleDeclContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#ruleStrength.
    def visitRuleStrength(self, ctx:MDLParser.RuleStrengthContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#ruleBody.
    def visitRuleBody(self, ctx:MDLParser.RuleBodyContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#deonticMod.
    def visitDeonticMod(self, ctx:MDLParser.DeonticModContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#priorityDecl.
    def visitPriorityDecl(self, ctx:MDLParser.PriorityDeclContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#factDecl.
    def visitFactDecl(self, ctx:MDLParser.FactDeclContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#assertDecl.
    def visitAssertDecl(self, ctx:MDLParser.AssertDeclContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#alignDecl.
    def visitAlignDecl(self, ctx:MDLParser.AlignDeclContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#alignTarget.
    def visitAlignTarget(self, ctx:MDLParser.AlignTargetContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#iriLiteral.
    def visitIriLiteral(self, ctx:MDLParser.IriLiteralContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#alignKind.
    def visitAlignKind(self, ctx:MDLParser.AlignKindContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#block.
    def visitBlock(self, ctx:MDLParser.BlockContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#blockLetStmt.
    def visitBlockLetStmt(self, ctx:MDLParser.BlockLetStmtContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#typeAnnotation.
    def visitTypeAnnotation(self, ctx:MDLParser.TypeAnnotationContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#expr.
    def visitExpr(self, ctx:MDLParser.ExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#temporalPostfix.
    def visitTemporalPostfix(self, ctx:MDLParser.TemporalPostfixContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#implication.
    def visitImplication(self, ctx:MDLParser.ImplicationContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#orExpr.
    def visitOrExpr(self, ctx:MDLParser.OrExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#andExpr.
    def visitAndExpr(self, ctx:MDLParser.AndExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#temporalBinary.
    def visitTemporalBinary(self, ctx:MDLParser.TemporalBinaryContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#comparison.
    def visitComparison(self, ctx:MDLParser.ComparisonContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#additive.
    def visitAdditive(self, ctx:MDLParser.AdditiveContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#multiplicative.
    def visitMultiplicative(self, ctx:MDLParser.MultiplicativeContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#unary.
    def visitUnary(self, ctx:MDLParser.UnaryContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#ifExpr.
    def visitIfExpr(self, ctx:MDLParser.IfExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#letExpr.
    def visitLetExpr(self, ctx:MDLParser.LetExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#matchExpr.
    def visitMatchExpr(self, ctx:MDLParser.MatchExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#caseBody.
    def visitCaseBody(self, ctx:MDLParser.CaseBodyContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#caseArm.
    def visitCaseArm(self, ctx:MDLParser.CaseArmContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#postfix.
    def visitPostfix(self, ctx:MDLParser.PostfixContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#postfixSuffix.
    def visitPostfixSuffix(self, ctx:MDLParser.PostfixSuffixContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#recordConstructorFields.
    def visitRecordConstructorFields(self, ctx:MDLParser.RecordConstructorFieldsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#recordConstructorFieldList.
    def visitRecordConstructorFieldList(self, ctx:MDLParser.RecordConstructorFieldListContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#recordConstructorField.
    def visitRecordConstructorField(self, ctx:MDLParser.RecordConstructorFieldContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#primary.
    def visitPrimary(self, ctx:MDLParser.PrimaryContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#exprList.
    def visitExprList(self, ctx:MDLParser.ExprListContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#pattern.
    def visitPattern(self, ctx:MDLParser.PatternContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#patternList.
    def visitPatternList(self, ctx:MDLParser.PatternListContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#recordPatternFieldList.
    def visitRecordPatternFieldList(self, ctx:MDLParser.RecordPatternFieldListContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#recordPatternField.
    def visitRecordPatternField(self, ctx:MDLParser.RecordPatternFieldContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#qualifiedName.
    def visitQualifiedName(self, ctx:MDLParser.QualifiedNameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#nameToken.
    def visitNameToken(self, ctx:MDLParser.NameTokenContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#temporalUnaryOp.
    def visitTemporalUnaryOp(self, ctx:MDLParser.TemporalUnaryOpContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MDLParser#newlines.
    def visitNewlines(self, ctx:MDLParser.NewlinesContext):
        return self.visitChildren(ctx)



del MDLParser