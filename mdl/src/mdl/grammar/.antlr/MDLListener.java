// Generated from /home/bearpro/source/personal/standard-contradictions/mdl/src/mdl/grammar/MDL.g4 by ANTLR 4.13.1
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link MDLParser}.
 */
public interface MDLListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link MDLParser#program}.
	 * @param ctx the parse tree
	 */
	void enterProgram(MDLParser.ProgramContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#program}.
	 * @param ctx the parse tree
	 */
	void exitProgram(MDLParser.ProgramContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#exprOnly}.
	 * @param ctx the parse tree
	 */
	void enterExprOnly(MDLParser.ExprOnlyContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#exprOnly}.
	 * @param ctx the parse tree
	 */
	void exitExprOnly(MDLParser.ExprOnlyContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#typeExprOnly}.
	 * @param ctx the parse tree
	 */
	void enterTypeExprOnly(MDLParser.TypeExprOnlyContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#typeExprOnly}.
	 * @param ctx the parse tree
	 */
	void exitTypeExprOnly(MDLParser.TypeExprOnlyContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#topItem}.
	 * @param ctx the parse tree
	 */
	void enterTopItem(MDLParser.TopItemContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#topItem}.
	 * @param ctx the parse tree
	 */
	void exitTopItem(MDLParser.TopItemContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#annotations}.
	 * @param ctx the parse tree
	 */
	void enterAnnotations(MDLParser.AnnotationsContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#annotations}.
	 * @param ctx the parse tree
	 */
	void exitAnnotations(MDLParser.AnnotationsContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#moduleDecl}.
	 * @param ctx the parse tree
	 */
	void enterModuleDecl(MDLParser.ModuleDeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#moduleDecl}.
	 * @param ctx the parse tree
	 */
	void exitModuleDecl(MDLParser.ModuleDeclContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#importDecl}.
	 * @param ctx the parse tree
	 */
	void enterImportDecl(MDLParser.ImportDeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#importDecl}.
	 * @param ctx the parse tree
	 */
	void exitImportDecl(MDLParser.ImportDeclContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#openDecl}.
	 * @param ctx the parse tree
	 */
	void enterOpenDecl(MDLParser.OpenDeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#openDecl}.
	 * @param ctx the parse tree
	 */
	void exitOpenDecl(MDLParser.OpenDeclContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#declaration}.
	 * @param ctx the parse tree
	 */
	void enterDeclaration(MDLParser.DeclarationContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#declaration}.
	 * @param ctx the parse tree
	 */
	void exitDeclaration(MDLParser.DeclarationContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#typeDecl}.
	 * @param ctx the parse tree
	 */
	void enterTypeDecl(MDLParser.TypeDeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#typeDecl}.
	 * @param ctx the parse tree
	 */
	void exitTypeDecl(MDLParser.TypeDeclContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#typeDefinition}.
	 * @param ctx the parse tree
	 */
	void enterTypeDefinition(MDLParser.TypeDefinitionContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#typeDefinition}.
	 * @param ctx the parse tree
	 */
	void exitTypeDefinition(MDLParser.TypeDefinitionContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#typeParams}.
	 * @param ctx the parse tree
	 */
	void enterTypeParams(MDLParser.TypeParamsContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#typeParams}.
	 * @param ctx the parse tree
	 */
	void exitTypeParams(MDLParser.TypeParamsContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#nameList}.
	 * @param ctx the parse tree
	 */
	void enterNameList(MDLParser.NameListContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#nameList}.
	 * @param ctx the parse tree
	 */
	void exitNameList(MDLParser.NameListContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#variant}.
	 * @param ctx the parse tree
	 */
	void enterVariant(MDLParser.VariantContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#variant}.
	 * @param ctx the parse tree
	 */
	void exitVariant(MDLParser.VariantContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#variantFieldList}.
	 * @param ctx the parse tree
	 */
	void enterVariantFieldList(MDLParser.VariantFieldListContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#variantFieldList}.
	 * @param ctx the parse tree
	 */
	void exitVariantFieldList(MDLParser.VariantFieldListContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#variantField}.
	 * @param ctx the parse tree
	 */
	void enterVariantField(MDLParser.VariantFieldContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#variantField}.
	 * @param ctx the parse tree
	 */
	void exitVariantField(MDLParser.VariantFieldContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#typeExpr}.
	 * @param ctx the parse tree
	 */
	void enterTypeExpr(MDLParser.TypeExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#typeExpr}.
	 * @param ctx the parse tree
	 */
	void exitTypeExpr(MDLParser.TypeExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#recordType}.
	 * @param ctx the parse tree
	 */
	void enterRecordType(MDLParser.RecordTypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#recordType}.
	 * @param ctx the parse tree
	 */
	void exitRecordType(MDLParser.RecordTypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#typeFieldList}.
	 * @param ctx the parse tree
	 */
	void enterTypeFieldList(MDLParser.TypeFieldListContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#typeFieldList}.
	 * @param ctx the parse tree
	 */
	void exitTypeFieldList(MDLParser.TypeFieldListContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#typeField}.
	 * @param ctx the parse tree
	 */
	void enterTypeField(MDLParser.TypeFieldContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#typeField}.
	 * @param ctx the parse tree
	 */
	void exitTypeField(MDLParser.TypeFieldContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#tupleOrParenType}.
	 * @param ctx the parse tree
	 */
	void enterTupleOrParenType(MDLParser.TupleOrParenTypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#tupleOrParenType}.
	 * @param ctx the parse tree
	 */
	void exitTupleOrParenType(MDLParser.TupleOrParenTypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#typeRef}.
	 * @param ctx the parse tree
	 */
	void enterTypeRef(MDLParser.TypeRefContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#typeRef}.
	 * @param ctx the parse tree
	 */
	void exitTypeRef(MDLParser.TypeRefContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#typeArgs}.
	 * @param ctx the parse tree
	 */
	void enterTypeArgs(MDLParser.TypeArgsContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#typeArgs}.
	 * @param ctx the parse tree
	 */
	void exitTypeArgs(MDLParser.TypeArgsContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#typeExprList}.
	 * @param ctx the parse tree
	 */
	void enterTypeExprList(MDLParser.TypeExprListContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#typeExprList}.
	 * @param ctx the parse tree
	 */
	void exitTypeExprList(MDLParser.TypeExprListContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#valueDecl}.
	 * @param ctx the parse tree
	 */
	void enterValueDecl(MDLParser.ValueDeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#valueDecl}.
	 * @param ctx the parse tree
	 */
	void exitValueDecl(MDLParser.ValueDeclContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#funcDecl}.
	 * @param ctx the parse tree
	 */
	void enterFuncDecl(MDLParser.FuncDeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#funcDecl}.
	 * @param ctx the parse tree
	 */
	void exitFuncDecl(MDLParser.FuncDeclContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#paramList}.
	 * @param ctx the parse tree
	 */
	void enterParamList(MDLParser.ParamListContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#paramList}.
	 * @param ctx the parse tree
	 */
	void exitParamList(MDLParser.ParamListContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#param}.
	 * @param ctx the parse tree
	 */
	void enterParam(MDLParser.ParamContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#param}.
	 * @param ctx the parse tree
	 */
	void exitParam(MDLParser.ParamContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#entityDecl}.
	 * @param ctx the parse tree
	 */
	void enterEntityDecl(MDLParser.EntityDeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#entityDecl}.
	 * @param ctx the parse tree
	 */
	void exitEntityDecl(MDLParser.EntityDeclContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#entityClause}.
	 * @param ctx the parse tree
	 */
	void enterEntityClause(MDLParser.EntityClauseContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#entityClause}.
	 * @param ctx the parse tree
	 */
	void exitEntityClause(MDLParser.EntityClauseContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#eventDecl}.
	 * @param ctx the parse tree
	 */
	void enterEventDecl(MDLParser.EventDeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#eventDecl}.
	 * @param ctx the parse tree
	 */
	void exitEventDecl(MDLParser.EventDeclContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#ruleDecl}.
	 * @param ctx the parse tree
	 */
	void enterRuleDecl(MDLParser.RuleDeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#ruleDecl}.
	 * @param ctx the parse tree
	 */
	void exitRuleDecl(MDLParser.RuleDeclContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#ruleStrength}.
	 * @param ctx the parse tree
	 */
	void enterRuleStrength(MDLParser.RuleStrengthContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#ruleStrength}.
	 * @param ctx the parse tree
	 */
	void exitRuleStrength(MDLParser.RuleStrengthContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#ruleBody}.
	 * @param ctx the parse tree
	 */
	void enterRuleBody(MDLParser.RuleBodyContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#ruleBody}.
	 * @param ctx the parse tree
	 */
	void exitRuleBody(MDLParser.RuleBodyContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#ruleSeparator}.
	 * @param ctx the parse tree
	 */
	void enterRuleSeparator(MDLParser.RuleSeparatorContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#ruleSeparator}.
	 * @param ctx the parse tree
	 */
	void exitRuleSeparator(MDLParser.RuleSeparatorContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#deonticMod}.
	 * @param ctx the parse tree
	 */
	void enterDeonticMod(MDLParser.DeonticModContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#deonticMod}.
	 * @param ctx the parse tree
	 */
	void exitDeonticMod(MDLParser.DeonticModContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#priorityDecl}.
	 * @param ctx the parse tree
	 */
	void enterPriorityDecl(MDLParser.PriorityDeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#priorityDecl}.
	 * @param ctx the parse tree
	 */
	void exitPriorityDecl(MDLParser.PriorityDeclContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#factDecl}.
	 * @param ctx the parse tree
	 */
	void enterFactDecl(MDLParser.FactDeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#factDecl}.
	 * @param ctx the parse tree
	 */
	void exitFactDecl(MDLParser.FactDeclContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#assertDecl}.
	 * @param ctx the parse tree
	 */
	void enterAssertDecl(MDLParser.AssertDeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#assertDecl}.
	 * @param ctx the parse tree
	 */
	void exitAssertDecl(MDLParser.AssertDeclContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#alignDecl}.
	 * @param ctx the parse tree
	 */
	void enterAlignDecl(MDLParser.AlignDeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#alignDecl}.
	 * @param ctx the parse tree
	 */
	void exitAlignDecl(MDLParser.AlignDeclContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#alignTarget}.
	 * @param ctx the parse tree
	 */
	void enterAlignTarget(MDLParser.AlignTargetContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#alignTarget}.
	 * @param ctx the parse tree
	 */
	void exitAlignTarget(MDLParser.AlignTargetContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#iriLiteral}.
	 * @param ctx the parse tree
	 */
	void enterIriLiteral(MDLParser.IriLiteralContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#iriLiteral}.
	 * @param ctx the parse tree
	 */
	void exitIriLiteral(MDLParser.IriLiteralContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#alignKind}.
	 * @param ctx the parse tree
	 */
	void enterAlignKind(MDLParser.AlignKindContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#alignKind}.
	 * @param ctx the parse tree
	 */
	void exitAlignKind(MDLParser.AlignKindContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#block}.
	 * @param ctx the parse tree
	 */
	void enterBlock(MDLParser.BlockContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#block}.
	 * @param ctx the parse tree
	 */
	void exitBlock(MDLParser.BlockContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#blockLetStmt}.
	 * @param ctx the parse tree
	 */
	void enterBlockLetStmt(MDLParser.BlockLetStmtContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#blockLetStmt}.
	 * @param ctx the parse tree
	 */
	void exitBlockLetStmt(MDLParser.BlockLetStmtContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#typeAnnotation}.
	 * @param ctx the parse tree
	 */
	void enterTypeAnnotation(MDLParser.TypeAnnotationContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#typeAnnotation}.
	 * @param ctx the parse tree
	 */
	void exitTypeAnnotation(MDLParser.TypeAnnotationContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#expr}.
	 * @param ctx the parse tree
	 */
	void enterExpr(MDLParser.ExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#expr}.
	 * @param ctx the parse tree
	 */
	void exitExpr(MDLParser.ExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#temporalPostfix}.
	 * @param ctx the parse tree
	 */
	void enterTemporalPostfix(MDLParser.TemporalPostfixContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#temporalPostfix}.
	 * @param ctx the parse tree
	 */
	void exitTemporalPostfix(MDLParser.TemporalPostfixContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#implication}.
	 * @param ctx the parse tree
	 */
	void enterImplication(MDLParser.ImplicationContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#implication}.
	 * @param ctx the parse tree
	 */
	void exitImplication(MDLParser.ImplicationContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#iffExpr}.
	 * @param ctx the parse tree
	 */
	void enterIffExpr(MDLParser.IffExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#iffExpr}.
	 * @param ctx the parse tree
	 */
	void exitIffExpr(MDLParser.IffExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#orExpr}.
	 * @param ctx the parse tree
	 */
	void enterOrExpr(MDLParser.OrExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#orExpr}.
	 * @param ctx the parse tree
	 */
	void exitOrExpr(MDLParser.OrExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#andExpr}.
	 * @param ctx the parse tree
	 */
	void enterAndExpr(MDLParser.AndExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#andExpr}.
	 * @param ctx the parse tree
	 */
	void exitAndExpr(MDLParser.AndExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#temporalBinary}.
	 * @param ctx the parse tree
	 */
	void enterTemporalBinary(MDLParser.TemporalBinaryContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#temporalBinary}.
	 * @param ctx the parse tree
	 */
	void exitTemporalBinary(MDLParser.TemporalBinaryContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#comparison}.
	 * @param ctx the parse tree
	 */
	void enterComparison(MDLParser.ComparisonContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#comparison}.
	 * @param ctx the parse tree
	 */
	void exitComparison(MDLParser.ComparisonContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#additive}.
	 * @param ctx the parse tree
	 */
	void enterAdditive(MDLParser.AdditiveContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#additive}.
	 * @param ctx the parse tree
	 */
	void exitAdditive(MDLParser.AdditiveContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#multiplicative}.
	 * @param ctx the parse tree
	 */
	void enterMultiplicative(MDLParser.MultiplicativeContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#multiplicative}.
	 * @param ctx the parse tree
	 */
	void exitMultiplicative(MDLParser.MultiplicativeContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#unary}.
	 * @param ctx the parse tree
	 */
	void enterUnary(MDLParser.UnaryContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#unary}.
	 * @param ctx the parse tree
	 */
	void exitUnary(MDLParser.UnaryContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#ifExpr}.
	 * @param ctx the parse tree
	 */
	void enterIfExpr(MDLParser.IfExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#ifExpr}.
	 * @param ctx the parse tree
	 */
	void exitIfExpr(MDLParser.IfExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#letExpr}.
	 * @param ctx the parse tree
	 */
	void enterLetExpr(MDLParser.LetExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#letExpr}.
	 * @param ctx the parse tree
	 */
	void exitLetExpr(MDLParser.LetExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#matchExpr}.
	 * @param ctx the parse tree
	 */
	void enterMatchExpr(MDLParser.MatchExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#matchExpr}.
	 * @param ctx the parse tree
	 */
	void exitMatchExpr(MDLParser.MatchExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#caseBody}.
	 * @param ctx the parse tree
	 */
	void enterCaseBody(MDLParser.CaseBodyContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#caseBody}.
	 * @param ctx the parse tree
	 */
	void exitCaseBody(MDLParser.CaseBodyContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#caseArm}.
	 * @param ctx the parse tree
	 */
	void enterCaseArm(MDLParser.CaseArmContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#caseArm}.
	 * @param ctx the parse tree
	 */
	void exitCaseArm(MDLParser.CaseArmContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#quantifierExpr}.
	 * @param ctx the parse tree
	 */
	void enterQuantifierExpr(MDLParser.QuantifierExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#quantifierExpr}.
	 * @param ctx the parse tree
	 */
	void exitQuantifierExpr(MDLParser.QuantifierExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#postfix}.
	 * @param ctx the parse tree
	 */
	void enterPostfix(MDLParser.PostfixContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#postfix}.
	 * @param ctx the parse tree
	 */
	void exitPostfix(MDLParser.PostfixContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#postfixSuffix}.
	 * @param ctx the parse tree
	 */
	void enterPostfixSuffix(MDLParser.PostfixSuffixContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#postfixSuffix}.
	 * @param ctx the parse tree
	 */
	void exitPostfixSuffix(MDLParser.PostfixSuffixContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#recordConstructorFields}.
	 * @param ctx the parse tree
	 */
	void enterRecordConstructorFields(MDLParser.RecordConstructorFieldsContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#recordConstructorFields}.
	 * @param ctx the parse tree
	 */
	void exitRecordConstructorFields(MDLParser.RecordConstructorFieldsContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#recordConstructorFieldList}.
	 * @param ctx the parse tree
	 */
	void enterRecordConstructorFieldList(MDLParser.RecordConstructorFieldListContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#recordConstructorFieldList}.
	 * @param ctx the parse tree
	 */
	void exitRecordConstructorFieldList(MDLParser.RecordConstructorFieldListContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#recordConstructorField}.
	 * @param ctx the parse tree
	 */
	void enterRecordConstructorField(MDLParser.RecordConstructorFieldContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#recordConstructorField}.
	 * @param ctx the parse tree
	 */
	void exitRecordConstructorField(MDLParser.RecordConstructorFieldContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#primary}.
	 * @param ctx the parse tree
	 */
	void enterPrimary(MDLParser.PrimaryContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#primary}.
	 * @param ctx the parse tree
	 */
	void exitPrimary(MDLParser.PrimaryContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#exprList}.
	 * @param ctx the parse tree
	 */
	void enterExprList(MDLParser.ExprListContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#exprList}.
	 * @param ctx the parse tree
	 */
	void exitExprList(MDLParser.ExprListContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#pattern}.
	 * @param ctx the parse tree
	 */
	void enterPattern(MDLParser.PatternContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#pattern}.
	 * @param ctx the parse tree
	 */
	void exitPattern(MDLParser.PatternContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#patternList}.
	 * @param ctx the parse tree
	 */
	void enterPatternList(MDLParser.PatternListContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#patternList}.
	 * @param ctx the parse tree
	 */
	void exitPatternList(MDLParser.PatternListContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#recordPatternFieldList}.
	 * @param ctx the parse tree
	 */
	void enterRecordPatternFieldList(MDLParser.RecordPatternFieldListContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#recordPatternFieldList}.
	 * @param ctx the parse tree
	 */
	void exitRecordPatternFieldList(MDLParser.RecordPatternFieldListContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#recordPatternField}.
	 * @param ctx the parse tree
	 */
	void enterRecordPatternField(MDLParser.RecordPatternFieldContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#recordPatternField}.
	 * @param ctx the parse tree
	 */
	void exitRecordPatternField(MDLParser.RecordPatternFieldContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#qualifiedName}.
	 * @param ctx the parse tree
	 */
	void enterQualifiedName(MDLParser.QualifiedNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#qualifiedName}.
	 * @param ctx the parse tree
	 */
	void exitQualifiedName(MDLParser.QualifiedNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#nameToken}.
	 * @param ctx the parse tree
	 */
	void enterNameToken(MDLParser.NameTokenContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#nameToken}.
	 * @param ctx the parse tree
	 */
	void exitNameToken(MDLParser.NameTokenContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#temporalUnaryOp}.
	 * @param ctx the parse tree
	 */
	void enterTemporalUnaryOp(MDLParser.TemporalUnaryOpContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#temporalUnaryOp}.
	 * @param ctx the parse tree
	 */
	void exitTemporalUnaryOp(MDLParser.TemporalUnaryOpContext ctx);
	/**
	 * Enter a parse tree produced by {@link MDLParser#newlines}.
	 * @param ctx the parse tree
	 */
	void enterNewlines(MDLParser.NewlinesContext ctx);
	/**
	 * Exit a parse tree produced by {@link MDLParser#newlines}.
	 * @param ctx the parse tree
	 */
	void exitNewlines(MDLParser.NewlinesContext ctx);
}