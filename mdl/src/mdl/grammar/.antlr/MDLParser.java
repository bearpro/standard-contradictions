// Generated from /home/bearpro/source/personal/standard-contradictions/mdl/src/mdl/grammar/MDL.g4 by ANTLR 4.13.1
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue"})
public class MDLParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.13.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		ARROW=1, LE=2, GE=3, NE=4, MODULE=5, IMPORT=6, OPEN=7, TYPE=8, LET=9, 
		FUNC=10, ENTITY=11, RULE=12, STRICT=13, DEFEASIBLE=14, DEFEATER=15, OVERRIDE=16, 
		FACT=17, IF=18, THEN=19, ELSE=20, CASE=21, WHEN=22, IN=23, TRUE=24, FALSE=25, 
		AND=26, OR=27, IMPLIES=28, NOT=29, ALWAYS=30, EVENTUALLY=31, NEXT=32, 
		NOW=33, UNTIL=34, OTHERWISE=35, O=36, P=37, F=38, ANNOT=39, STRING=40, 
		RAT=41, DECIMAL=42, INT=43, UNDERSCORE=44, IDENT=45, LPAREN=46, RPAREN=47, 
		LBRACE=48, RBRACE=49, COMMA=50, COLON=51, DOT=52, BAR=53, PLUS=54, MINUS=55, 
		STAR=56, SLASH=57, PERCENT=58, EQ=59, LT=60, GT=61, NEWLINE=62, COMMENT=63, 
		WS=64, INDENT=65, DEDENT=66;
	public static final int
		RULE_program = 0, RULE_exprOnly = 1, RULE_typeExprOnly = 2, RULE_topItem = 3, 
		RULE_annotations = 4, RULE_moduleDecl = 5, RULE_importDecl = 6, RULE_openDecl = 7, 
		RULE_declaration = 8, RULE_typeDecl = 9, RULE_typeDefinition = 10, RULE_typeParams = 11, 
		RULE_nameList = 12, RULE_variant = 13, RULE_variantFieldList = 14, RULE_variantField = 15, 
		RULE_typeExpr = 16, RULE_recordType = 17, RULE_typeFieldList = 18, RULE_typeField = 19, 
		RULE_tupleOrParenType = 20, RULE_typeRef = 21, RULE_typeArgs = 22, RULE_typeExprList = 23, 
		RULE_funcDecl = 24, RULE_paramList = 25, RULE_param = 26, RULE_entityDecl = 27, 
		RULE_ruleDecl = 28, RULE_ruleStrength = 29, RULE_ruleBody = 30, RULE_deonticMod = 31, 
		RULE_priorityDecl = 32, RULE_factDecl = 33, RULE_block = 34, RULE_blockLetStmt = 35, 
		RULE_typeAnnotation = 36, RULE_continuedExpr = 37, RULE_expr = 38, RULE_temporalPostfix = 39, 
		RULE_implication = 40, RULE_orExpr = 41, RULE_andExpr = 42, RULE_temporalBinary = 43, 
		RULE_comparison = 44, RULE_additive = 45, RULE_multiplicative = 46, RULE_unary = 47, 
		RULE_ifExpr = 48, RULE_letExpr = 49, RULE_letBodyExpr = 50, RULE_matchExpr = 51, 
		RULE_caseBody = 52, RULE_caseArm = 53, RULE_postfix = 54, RULE_postfixSuffix = 55, 
		RULE_recordConstructorFields = 56, RULE_recordConstructorFieldList = 57, 
		RULE_recordConstructorField = 58, RULE_primary = 59, RULE_exprList = 60, 
		RULE_pattern = 61, RULE_patternList = 62, RULE_recordPatternFieldList = 63, 
		RULE_recordPatternField = 64, RULE_qualifiedName = 65, RULE_nameToken = 66, 
		RULE_temporalUnaryOp = 67, RULE_newlines = 68;
	private static String[] makeRuleNames() {
		return new String[] {
			"program", "exprOnly", "typeExprOnly", "topItem", "annotations", "moduleDecl", 
			"importDecl", "openDecl", "declaration", "typeDecl", "typeDefinition", 
			"typeParams", "nameList", "variant", "variantFieldList", "variantField", 
			"typeExpr", "recordType", "typeFieldList", "typeField", "tupleOrParenType", 
			"typeRef", "typeArgs", "typeExprList", "funcDecl", "paramList", "param", 
			"entityDecl", "ruleDecl", "ruleStrength", "ruleBody", "deonticMod", "priorityDecl", 
			"factDecl", "block", "blockLetStmt", "typeAnnotation", "continuedExpr", 
			"expr", "temporalPostfix", "implication", "orExpr", "andExpr", "temporalBinary", 
			"comparison", "additive", "multiplicative", "unary", "ifExpr", "letExpr", 
			"letBodyExpr", "matchExpr", "caseBody", "caseArm", "postfix", "postfixSuffix", 
			"recordConstructorFields", "recordConstructorFieldList", "recordConstructorField", 
			"primary", "exprList", "pattern", "patternList", "recordPatternFieldList", 
			"recordPatternField", "qualifiedName", "nameToken", "temporalUnaryOp", 
			"newlines"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'->'", "'<='", "'>='", "'!='", "'module'", "'import'", "'open'", 
			"'type'", "'let'", "'func'", "'entity'", "'rule'", "'strict'", "'defeasible'", 
			"'defeater'", "'override'", "'fact'", "'if'", "'then'", "'else'", "'case'", 
			"'when'", "'in'", "'true'", "'false'", "'and'", "'or'", "'implies'", 
			"'not'", "'always'", "'eventually'", "'next'", "'now'", "'until'", "'otherwise'", 
			"'O'", "'P'", "'F'", null, null, null, null, null, "'_'", null, "'('", 
			"')'", "'{'", "'}'", "','", "':'", "'.'", "'|'", "'+'", "'-'", "'*'", 
			"'/'", "'%'", "'='", "'<'", "'>'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, "ARROW", "LE", "GE", "NE", "MODULE", "IMPORT", "OPEN", "TYPE", 
			"LET", "FUNC", "ENTITY", "RULE", "STRICT", "DEFEASIBLE", "DEFEATER", 
			"OVERRIDE", "FACT", "IF", "THEN", "ELSE", "CASE", "WHEN", "IN", "TRUE", 
			"FALSE", "AND", "OR", "IMPLIES", "NOT", "ALWAYS", "EVENTUALLY", "NEXT", 
			"NOW", "UNTIL", "OTHERWISE", "O", "P", "F", "ANNOT", "STRING", "RAT", 
			"DECIMAL", "INT", "UNDERSCORE", "IDENT", "LPAREN", "RPAREN", "LBRACE", 
			"RBRACE", "COMMA", "COLON", "DOT", "BAR", "PLUS", "MINUS", "STAR", "SLASH", 
			"PERCENT", "EQ", "LT", "GT", "NEWLINE", "COMMENT", "WS", "INDENT", "DEDENT"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "MDL.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public MDLParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ProgramContext extends ParserRuleContext {
		public AnnotationsContext annotations() {
			return getRuleContext(AnnotationsContext.class,0);
		}
		public ModuleDeclContext moduleDecl() {
			return getRuleContext(ModuleDeclContext.class,0);
		}
		public TerminalNode EOF() { return getToken(MDLParser.EOF, 0); }
		public List<NewlinesContext> newlines() {
			return getRuleContexts(NewlinesContext.class);
		}
		public NewlinesContext newlines(int i) {
			return getRuleContext(NewlinesContext.class,i);
		}
		public List<TopItemContext> topItem() {
			return getRuleContexts(TopItemContext.class);
		}
		public TopItemContext topItem(int i) {
			return getRuleContext(TopItemContext.class,i);
		}
		public ProgramContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_program; }
	}

	public final ProgramContext program() throws RecognitionException {
		ProgramContext _localctx = new ProgramContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_program);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(139);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(138);
				newlines();
				}
			}

			setState(141);
			annotations();
			setState(142);
			moduleDecl();
			setState(144);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(143);
				newlines();
				}
			}

			setState(149);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & 549756075456L) != 0)) {
				{
				{
				setState(146);
				topItem();
				}
				}
				setState(151);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(152);
			match(EOF);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ExprOnlyContext extends ParserRuleContext {
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode EOF() { return getToken(MDLParser.EOF, 0); }
		public List<NewlinesContext> newlines() {
			return getRuleContexts(NewlinesContext.class);
		}
		public NewlinesContext newlines(int i) {
			return getRuleContext(NewlinesContext.class,i);
		}
		public ExprOnlyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprOnly; }
	}

	public final ExprOnlyContext exprOnly() throws RecognitionException {
		ExprOnlyContext _localctx = new ExprOnlyContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_exprOnly);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(155);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(154);
				newlines();
				}
			}

			setState(157);
			expr();
			setState(159);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(158);
				newlines();
				}
			}

			setState(161);
			match(EOF);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TypeExprOnlyContext extends ParserRuleContext {
		public TypeExprContext typeExpr() {
			return getRuleContext(TypeExprContext.class,0);
		}
		public TerminalNode EOF() { return getToken(MDLParser.EOF, 0); }
		public List<NewlinesContext> newlines() {
			return getRuleContexts(NewlinesContext.class);
		}
		public NewlinesContext newlines(int i) {
			return getRuleContext(NewlinesContext.class,i);
		}
		public TypeExprOnlyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeExprOnly; }
	}

	public final TypeExprOnlyContext typeExprOnly() throws RecognitionException {
		TypeExprOnlyContext _localctx = new TypeExprOnlyContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_typeExprOnly);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(164);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(163);
				newlines();
				}
			}

			setState(166);
			typeExpr();
			setState(168);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(167);
				newlines();
				}
			}

			setState(170);
			match(EOF);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TopItemContext extends ParserRuleContext {
		public AnnotationsContext annotations() {
			return getRuleContext(AnnotationsContext.class,0);
		}
		public ImportDeclContext importDecl() {
			return getRuleContext(ImportDeclContext.class,0);
		}
		public OpenDeclContext openDecl() {
			return getRuleContext(OpenDeclContext.class,0);
		}
		public DeclarationContext declaration() {
			return getRuleContext(DeclarationContext.class,0);
		}
		public NewlinesContext newlines() {
			return getRuleContext(NewlinesContext.class,0);
		}
		public TopItemContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_topItem; }
	}

	public final TopItemContext topItem() throws RecognitionException {
		TopItemContext _localctx = new TopItemContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_topItem);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(172);
			annotations();
			setState(176);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case IMPORT:
				{
				setState(173);
				importDecl();
				}
				break;
			case OPEN:
				{
				setState(174);
				openDecl();
				}
				break;
			case TYPE:
			case FUNC:
			case ENTITY:
			case RULE:
			case STRICT:
			case DEFEASIBLE:
			case DEFEATER:
			case OVERRIDE:
			case FACT:
				{
				setState(175);
				declaration();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(179);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(178);
				newlines();
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class AnnotationsContext extends ParserRuleContext {
		public List<TerminalNode> ANNOT() { return getTokens(MDLParser.ANNOT); }
		public TerminalNode ANNOT(int i) {
			return getToken(MDLParser.ANNOT, i);
		}
		public List<NewlinesContext> newlines() {
			return getRuleContexts(NewlinesContext.class);
		}
		public NewlinesContext newlines(int i) {
			return getRuleContext(NewlinesContext.class,i);
		}
		public AnnotationsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_annotations; }
	}

	public final AnnotationsContext annotations() throws RecognitionException {
		AnnotationsContext _localctx = new AnnotationsContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_annotations);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(187);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==ANNOT) {
				{
				{
				setState(181);
				match(ANNOT);
				setState(183);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(182);
					newlines();
					}
				}

				}
				}
				setState(189);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ModuleDeclContext extends ParserRuleContext {
		public TerminalNode MODULE() { return getToken(MDLParser.MODULE, 0); }
		public QualifiedNameContext qualifiedName() {
			return getRuleContext(QualifiedNameContext.class,0);
		}
		public ModuleDeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_moduleDecl; }
	}

	public final ModuleDeclContext moduleDecl() throws RecognitionException {
		ModuleDeclContext _localctx = new ModuleDeclContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_moduleDecl);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(190);
			match(MODULE);
			setState(191);
			qualifiedName();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ImportDeclContext extends ParserRuleContext {
		public TerminalNode IMPORT() { return getToken(MDLParser.IMPORT, 0); }
		public TerminalNode STRING() { return getToken(MDLParser.STRING, 0); }
		public ImportDeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_importDecl; }
	}

	public final ImportDeclContext importDecl() throws RecognitionException {
		ImportDeclContext _localctx = new ImportDeclContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_importDecl);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(193);
			match(IMPORT);
			setState(194);
			match(STRING);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class OpenDeclContext extends ParserRuleContext {
		public TerminalNode OPEN() { return getToken(MDLParser.OPEN, 0); }
		public QualifiedNameContext qualifiedName() {
			return getRuleContext(QualifiedNameContext.class,0);
		}
		public OpenDeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_openDecl; }
	}

	public final OpenDeclContext openDecl() throws RecognitionException {
		OpenDeclContext _localctx = new OpenDeclContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_openDecl);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(196);
			match(OPEN);
			setState(197);
			qualifiedName();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class DeclarationContext extends ParserRuleContext {
		public TypeDeclContext typeDecl() {
			return getRuleContext(TypeDeclContext.class,0);
		}
		public FuncDeclContext funcDecl() {
			return getRuleContext(FuncDeclContext.class,0);
		}
		public EntityDeclContext entityDecl() {
			return getRuleContext(EntityDeclContext.class,0);
		}
		public RuleDeclContext ruleDecl() {
			return getRuleContext(RuleDeclContext.class,0);
		}
		public PriorityDeclContext priorityDecl() {
			return getRuleContext(PriorityDeclContext.class,0);
		}
		public FactDeclContext factDecl() {
			return getRuleContext(FactDeclContext.class,0);
		}
		public DeclarationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_declaration; }
	}

	public final DeclarationContext declaration() throws RecognitionException {
		DeclarationContext _localctx = new DeclarationContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_declaration);
		try {
			setState(205);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case TYPE:
				enterOuterAlt(_localctx, 1);
				{
				setState(199);
				typeDecl();
				}
				break;
			case FUNC:
				enterOuterAlt(_localctx, 2);
				{
				setState(200);
				funcDecl();
				}
				break;
			case ENTITY:
				enterOuterAlt(_localctx, 3);
				{
				setState(201);
				entityDecl();
				}
				break;
			case RULE:
			case STRICT:
			case DEFEASIBLE:
			case DEFEATER:
				enterOuterAlt(_localctx, 4);
				{
				setState(202);
				ruleDecl();
				}
				break;
			case OVERRIDE:
				enterOuterAlt(_localctx, 5);
				{
				setState(203);
				priorityDecl();
				}
				break;
			case FACT:
				enterOuterAlt(_localctx, 6);
				{
				setState(204);
				factDecl();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TypeDeclContext extends ParserRuleContext {
		public TerminalNode TYPE() { return getToken(MDLParser.TYPE, 0); }
		public NameTokenContext nameToken() {
			return getRuleContext(NameTokenContext.class,0);
		}
		public TerminalNode EQ() { return getToken(MDLParser.EQ, 0); }
		public TypeDefinitionContext typeDefinition() {
			return getRuleContext(TypeDefinitionContext.class,0);
		}
		public TypeParamsContext typeParams() {
			return getRuleContext(TypeParamsContext.class,0);
		}
		public TypeDeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeDecl; }
	}

	public final TypeDeclContext typeDecl() throws RecognitionException {
		TypeDeclContext _localctx = new TypeDeclContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_typeDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(207);
			match(TYPE);
			setState(208);
			nameToken();
			setState(210);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LT) {
				{
				setState(209);
				typeParams();
				}
			}

			setState(212);
			match(EQ);
			setState(213);
			typeDefinition();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TypeDefinitionContext extends ParserRuleContext {
		public RecordTypeContext recordType() {
			return getRuleContext(RecordTypeContext.class,0);
		}
		public List<VariantContext> variant() {
			return getRuleContexts(VariantContext.class);
		}
		public VariantContext variant(int i) {
			return getRuleContext(VariantContext.class,i);
		}
		public List<TerminalNode> BAR() { return getTokens(MDLParser.BAR); }
		public TerminalNode BAR(int i) {
			return getToken(MDLParser.BAR, i);
		}
		public TerminalNode NEWLINE() { return getToken(MDLParser.NEWLINE, 0); }
		public TerminalNode INDENT() { return getToken(MDLParser.INDENT, 0); }
		public TerminalNode DEDENT() { return getToken(MDLParser.DEDENT, 0); }
		public List<NewlinesContext> newlines() {
			return getRuleContexts(NewlinesContext.class);
		}
		public NewlinesContext newlines(int i) {
			return getRuleContext(NewlinesContext.class,i);
		}
		public TypeDefinitionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeDefinition; }
	}

	public final TypeDefinitionContext typeDefinition() throws RecognitionException {
		TypeDefinitionContext _localctx = new TypeDefinitionContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_typeDefinition);
		int _la;
		try {
			int _alt;
			setState(242);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case LBRACE:
				enterOuterAlt(_localctx, 1);
				{
				setState(215);
				recordType();
				}
				break;
			case TRUE:
			case FALSE:
			case O:
			case P:
			case F:
			case IDENT:
				enterOuterAlt(_localctx, 2);
				{
				setState(216);
				variant();
				setState(221);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==BAR) {
					{
					{
					setState(217);
					match(BAR);
					setState(218);
					variant();
					}
					}
					setState(223);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				}
				break;
			case NEWLINE:
				enterOuterAlt(_localctx, 3);
				{
				setState(224);
				match(NEWLINE);
				setState(225);
				match(INDENT);
				setState(226);
				match(BAR);
				setState(227);
				variant();
				setState(234);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,14,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(228);
						newlines();
						setState(229);
						match(BAR);
						setState(230);
						variant();
						}
						} 
					}
					setState(236);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,14,_ctx);
				}
				setState(238);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(237);
					newlines();
					}
				}

				setState(240);
				match(DEDENT);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TypeParamsContext extends ParserRuleContext {
		public TerminalNode LT() { return getToken(MDLParser.LT, 0); }
		public NameListContext nameList() {
			return getRuleContext(NameListContext.class,0);
		}
		public TerminalNode GT() { return getToken(MDLParser.GT, 0); }
		public TypeParamsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeParams; }
	}

	public final TypeParamsContext typeParams() throws RecognitionException {
		TypeParamsContext _localctx = new TypeParamsContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_typeParams);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(244);
			match(LT);
			setState(245);
			nameList();
			setState(246);
			match(GT);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class NameListContext extends ParserRuleContext {
		public List<NameTokenContext> nameToken() {
			return getRuleContexts(NameTokenContext.class);
		}
		public NameTokenContext nameToken(int i) {
			return getRuleContext(NameTokenContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(MDLParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(MDLParser.COMMA, i);
		}
		public NameListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_nameList; }
	}

	public final NameListContext nameList() throws RecognitionException {
		NameListContext _localctx = new NameListContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_nameList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(248);
			nameToken();
			setState(253);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(249);
				match(COMMA);
				setState(250);
				nameToken();
				}
				}
				setState(255);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class VariantContext extends ParserRuleContext {
		public NameTokenContext nameToken() {
			return getRuleContext(NameTokenContext.class,0);
		}
		public TerminalNode LPAREN() { return getToken(MDLParser.LPAREN, 0); }
		public VariantFieldListContext variantFieldList() {
			return getRuleContext(VariantFieldListContext.class,0);
		}
		public TerminalNode RPAREN() { return getToken(MDLParser.RPAREN, 0); }
		public VariantContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_variant; }
	}

	public final VariantContext variant() throws RecognitionException {
		VariantContext _localctx = new VariantContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_variant);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(256);
			nameToken();
			setState(257);
			match(LPAREN);
			setState(258);
			variantFieldList();
			setState(259);
			match(RPAREN);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class VariantFieldListContext extends ParserRuleContext {
		public List<VariantFieldContext> variantField() {
			return getRuleContexts(VariantFieldContext.class);
		}
		public VariantFieldContext variantField(int i) {
			return getRuleContext(VariantFieldContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(MDLParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(MDLParser.COMMA, i);
		}
		public VariantFieldListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_variantFieldList; }
	}

	public final VariantFieldListContext variantFieldList() throws RecognitionException {
		VariantFieldListContext _localctx = new VariantFieldListContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_variantFieldList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(261);
			variantField();
			setState(266);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(262);
				match(COMMA);
				setState(263);
				variantField();
				}
				}
				setState(268);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class VariantFieldContext extends ParserRuleContext {
		public NameTokenContext nameToken() {
			return getRuleContext(NameTokenContext.class,0);
		}
		public TerminalNode COLON() { return getToken(MDLParser.COLON, 0); }
		public TypeExprContext typeExpr() {
			return getRuleContext(TypeExprContext.class,0);
		}
		public VariantFieldContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_variantField; }
	}

	public final VariantFieldContext variantField() throws RecognitionException {
		VariantFieldContext _localctx = new VariantFieldContext(_ctx, getState());
		enterRule(_localctx, 30, RULE_variantField);
		try {
			setState(274);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,19,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(269);
				nameToken();
				setState(270);
				match(COLON);
				setState(271);
				typeExpr();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(273);
				typeExpr();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TypeExprContext extends ParserRuleContext {
		public RecordTypeContext recordType() {
			return getRuleContext(RecordTypeContext.class,0);
		}
		public TupleOrParenTypeContext tupleOrParenType() {
			return getRuleContext(TupleOrParenTypeContext.class,0);
		}
		public TypeRefContext typeRef() {
			return getRuleContext(TypeRefContext.class,0);
		}
		public TypeExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeExpr; }
	}

	public final TypeExprContext typeExpr() throws RecognitionException {
		TypeExprContext _localctx = new TypeExprContext(_ctx, getState());
		enterRule(_localctx, 32, RULE_typeExpr);
		try {
			setState(279);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case LBRACE:
				enterOuterAlt(_localctx, 1);
				{
				setState(276);
				recordType();
				}
				break;
			case LPAREN:
				enterOuterAlt(_localctx, 2);
				{
				setState(277);
				tupleOrParenType();
				}
				break;
			case TRUE:
			case FALSE:
			case O:
			case P:
			case F:
			case IDENT:
				enterOuterAlt(_localctx, 3);
				{
				setState(278);
				typeRef();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class RecordTypeContext extends ParserRuleContext {
		public TerminalNode LBRACE() { return getToken(MDLParser.LBRACE, 0); }
		public TerminalNode RBRACE() { return getToken(MDLParser.RBRACE, 0); }
		public TypeFieldListContext typeFieldList() {
			return getRuleContext(TypeFieldListContext.class,0);
		}
		public RecordTypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_recordType; }
	}

	public final RecordTypeContext recordType() throws RecognitionException {
		RecordTypeContext _localctx = new RecordTypeContext(_ctx, getState());
		enterRule(_localctx, 34, RULE_recordType);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(281);
			match(LBRACE);
			setState(283);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 35665458757632L) != 0)) {
				{
				setState(282);
				typeFieldList();
				}
			}

			setState(285);
			match(RBRACE);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TypeFieldListContext extends ParserRuleContext {
		public List<TypeFieldContext> typeField() {
			return getRuleContexts(TypeFieldContext.class);
		}
		public TypeFieldContext typeField(int i) {
			return getRuleContext(TypeFieldContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(MDLParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(MDLParser.COMMA, i);
		}
		public TypeFieldListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeFieldList; }
	}

	public final TypeFieldListContext typeFieldList() throws RecognitionException {
		TypeFieldListContext _localctx = new TypeFieldListContext(_ctx, getState());
		enterRule(_localctx, 36, RULE_typeFieldList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(287);
			typeField();
			setState(292);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(288);
				match(COMMA);
				setState(289);
				typeField();
				}
				}
				setState(294);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TypeFieldContext extends ParserRuleContext {
		public NameTokenContext nameToken() {
			return getRuleContext(NameTokenContext.class,0);
		}
		public TerminalNode COLON() { return getToken(MDLParser.COLON, 0); }
		public TypeExprContext typeExpr() {
			return getRuleContext(TypeExprContext.class,0);
		}
		public TypeFieldContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeField; }
	}

	public final TypeFieldContext typeField() throws RecognitionException {
		TypeFieldContext _localctx = new TypeFieldContext(_ctx, getState());
		enterRule(_localctx, 38, RULE_typeField);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(295);
			nameToken();
			setState(296);
			match(COLON);
			setState(297);
			typeExpr();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TupleOrParenTypeContext extends ParserRuleContext {
		public TerminalNode LPAREN() { return getToken(MDLParser.LPAREN, 0); }
		public List<TypeExprContext> typeExpr() {
			return getRuleContexts(TypeExprContext.class);
		}
		public TypeExprContext typeExpr(int i) {
			return getRuleContext(TypeExprContext.class,i);
		}
		public TerminalNode RPAREN() { return getToken(MDLParser.RPAREN, 0); }
		public List<TerminalNode> COMMA() { return getTokens(MDLParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(MDLParser.COMMA, i);
		}
		public TupleOrParenTypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_tupleOrParenType; }
	}

	public final TupleOrParenTypeContext tupleOrParenType() throws RecognitionException {
		TupleOrParenTypeContext _localctx = new TupleOrParenTypeContext(_ctx, getState());
		enterRule(_localctx, 40, RULE_tupleOrParenType);
		int _la;
		try {
			setState(316);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,24,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(299);
				match(LPAREN);
				setState(300);
				typeExpr();
				setState(301);
				match(RPAREN);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(303);
				match(LPAREN);
				setState(304);
				typeExpr();
				setState(305);
				match(COMMA);
				setState(306);
				typeExpr();
				setState(311);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(307);
					match(COMMA);
					setState(308);
					typeExpr();
					}
					}
					setState(313);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(314);
				match(RPAREN);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TypeRefContext extends ParserRuleContext {
		public QualifiedNameContext qualifiedName() {
			return getRuleContext(QualifiedNameContext.class,0);
		}
		public TypeArgsContext typeArgs() {
			return getRuleContext(TypeArgsContext.class,0);
		}
		public TypeRefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeRef; }
	}

	public final TypeRefContext typeRef() throws RecognitionException {
		TypeRefContext _localctx = new TypeRefContext(_ctx, getState());
		enterRule(_localctx, 42, RULE_typeRef);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(318);
			qualifiedName();
			setState(320);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LT) {
				{
				setState(319);
				typeArgs();
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TypeArgsContext extends ParserRuleContext {
		public TerminalNode LT() { return getToken(MDLParser.LT, 0); }
		public TypeExprListContext typeExprList() {
			return getRuleContext(TypeExprListContext.class,0);
		}
		public TerminalNode GT() { return getToken(MDLParser.GT, 0); }
		public TypeArgsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeArgs; }
	}

	public final TypeArgsContext typeArgs() throws RecognitionException {
		TypeArgsContext _localctx = new TypeArgsContext(_ctx, getState());
		enterRule(_localctx, 44, RULE_typeArgs);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(322);
			match(LT);
			setState(323);
			typeExprList();
			setState(324);
			match(GT);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TypeExprListContext extends ParserRuleContext {
		public List<TypeExprContext> typeExpr() {
			return getRuleContexts(TypeExprContext.class);
		}
		public TypeExprContext typeExpr(int i) {
			return getRuleContext(TypeExprContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(MDLParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(MDLParser.COMMA, i);
		}
		public TypeExprListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeExprList; }
	}

	public final TypeExprListContext typeExprList() throws RecognitionException {
		TypeExprListContext _localctx = new TypeExprListContext(_ctx, getState());
		enterRule(_localctx, 46, RULE_typeExprList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(326);
			typeExpr();
			setState(331);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(327);
				match(COMMA);
				setState(328);
				typeExpr();
				}
				}
				setState(333);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class FuncDeclContext extends ParserRuleContext {
		public TerminalNode FUNC() { return getToken(MDLParser.FUNC, 0); }
		public NameTokenContext nameToken() {
			return getRuleContext(NameTokenContext.class,0);
		}
		public TerminalNode LPAREN() { return getToken(MDLParser.LPAREN, 0); }
		public TerminalNode RPAREN() { return getToken(MDLParser.RPAREN, 0); }
		public TerminalNode ARROW() { return getToken(MDLParser.ARROW, 0); }
		public TypeExprContext typeExpr() {
			return getRuleContext(TypeExprContext.class,0);
		}
		public TerminalNode COLON() { return getToken(MDLParser.COLON, 0); }
		public BlockContext block() {
			return getRuleContext(BlockContext.class,0);
		}
		public TypeParamsContext typeParams() {
			return getRuleContext(TypeParamsContext.class,0);
		}
		public ParamListContext paramList() {
			return getRuleContext(ParamListContext.class,0);
		}
		public FuncDeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_funcDecl; }
	}

	public final FuncDeclContext funcDecl() throws RecognitionException {
		FuncDeclContext _localctx = new FuncDeclContext(_ctx, getState());
		enterRule(_localctx, 48, RULE_funcDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(334);
			match(FUNC);
			setState(335);
			nameToken();
			setState(337);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LT) {
				{
				setState(336);
				typeParams();
				}
			}

			setState(339);
			match(LPAREN);
			setState(341);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 421594040107008L) != 0)) {
				{
				setState(340);
				paramList();
				}
			}

			setState(343);
			match(RPAREN);
			setState(344);
			match(ARROW);
			setState(345);
			typeExpr();
			setState(346);
			match(COLON);
			setState(347);
			block();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ParamListContext extends ParserRuleContext {
		public List<ParamContext> param() {
			return getRuleContexts(ParamContext.class);
		}
		public ParamContext param(int i) {
			return getRuleContext(ParamContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(MDLParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(MDLParser.COMMA, i);
		}
		public ParamListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_paramList; }
	}

	public final ParamListContext paramList() throws RecognitionException {
		ParamListContext _localctx = new ParamListContext(_ctx, getState());
		enterRule(_localctx, 50, RULE_paramList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(349);
			param();
			setState(354);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(350);
				match(COMMA);
				setState(351);
				param();
				}
				}
				setState(356);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ParamContext extends ParserRuleContext {
		public PatternContext pattern() {
			return getRuleContext(PatternContext.class,0);
		}
		public TerminalNode COLON() { return getToken(MDLParser.COLON, 0); }
		public TypeExprContext typeExpr() {
			return getRuleContext(TypeExprContext.class,0);
		}
		public ParamContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_param; }
	}

	public final ParamContext param() throws RecognitionException {
		ParamContext _localctx = new ParamContext(_ctx, getState());
		enterRule(_localctx, 52, RULE_param);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(357);
			pattern();
			setState(358);
			match(COLON);
			setState(359);
			typeExpr();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class EntityDeclContext extends ParserRuleContext {
		public TerminalNode ENTITY() { return getToken(MDLParser.ENTITY, 0); }
		public NameTokenContext nameToken() {
			return getRuleContext(NameTokenContext.class,0);
		}
		public TerminalNode COLON() { return getToken(MDLParser.COLON, 0); }
		public TypeExprContext typeExpr() {
			return getRuleContext(TypeExprContext.class,0);
		}
		public EntityDeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_entityDecl; }
	}

	public final EntityDeclContext entityDecl() throws RecognitionException {
		EntityDeclContext _localctx = new EntityDeclContext(_ctx, getState());
		enterRule(_localctx, 54, RULE_entityDecl);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(361);
			match(ENTITY);
			setState(362);
			nameToken();
			setState(363);
			match(COLON);
			setState(364);
			typeExpr();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class RuleDeclContext extends ParserRuleContext {
		public TerminalNode RULE() { return getToken(MDLParser.RULE, 0); }
		public RuleBodyContext ruleBody() {
			return getRuleContext(RuleBodyContext.class,0);
		}
		public RuleStrengthContext ruleStrength() {
			return getRuleContext(RuleStrengthContext.class,0);
		}
		public TerminalNode OTHERWISE() { return getToken(MDLParser.OTHERWISE, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public RuleDeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ruleDecl; }
	}

	public final RuleDeclContext ruleDecl() throws RecognitionException {
		RuleDeclContext _localctx = new RuleDeclContext(_ctx, getState());
		enterRule(_localctx, 56, RULE_ruleDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(367);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 57344L) != 0)) {
				{
				setState(366);
				ruleStrength();
				}
			}

			setState(369);
			match(RULE);
			setState(370);
			ruleBody();
			setState(373);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==OTHERWISE) {
				{
				setState(371);
				match(OTHERWISE);
				setState(372);
				expr();
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class RuleStrengthContext extends ParserRuleContext {
		public TerminalNode STRICT() { return getToken(MDLParser.STRICT, 0); }
		public TerminalNode DEFEASIBLE() { return getToken(MDLParser.DEFEASIBLE, 0); }
		public TerminalNode DEFEATER() { return getToken(MDLParser.DEFEATER, 0); }
		public RuleStrengthContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ruleStrength; }
	}

	public final RuleStrengthContext ruleStrength() throws RecognitionException {
		RuleStrengthContext _localctx = new RuleStrengthContext(_ctx, getState());
		enterRule(_localctx, 58, RULE_ruleStrength);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(375);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 57344L) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class RuleBodyContext extends ParserRuleContext {
		public DeonticModContext deonticMod() {
			return getRuleContext(DeonticModContext.class,0);
		}
		public TerminalNode COLON() { return getToken(MDLParser.COLON, 0); }
		public BlockContext block() {
			return getRuleContext(BlockContext.class,0);
		}
		public QualifiedNameContext qualifiedName() {
			return getRuleContext(QualifiedNameContext.class,0);
		}
		public TerminalNode WHEN() { return getToken(MDLParser.WHEN, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public RuleBodyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ruleBody; }
	}

	public final RuleBodyContext ruleBody() throws RecognitionException {
		RuleBodyContext _localctx = new RuleBodyContext(_ctx, getState());
		enterRule(_localctx, 60, RULE_ruleBody);
		int _la;
		try {
			setState(392);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,34,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(377);
				deonticMod();
				setState(378);
				match(COLON);
				setState(379);
				block();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(382);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,32,_ctx) ) {
				case 1:
					{
					setState(381);
					deonticMod();
					}
					break;
				}
				setState(384);
				qualifiedName();
				setState(387);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==WHEN) {
					{
					setState(385);
					match(WHEN);
					setState(386);
					expr();
					}
				}

				setState(389);
				match(COLON);
				setState(390);
				block();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class DeonticModContext extends ParserRuleContext {
		public TerminalNode O() { return getToken(MDLParser.O, 0); }
		public TerminalNode P() { return getToken(MDLParser.P, 0); }
		public TerminalNode F() { return getToken(MDLParser.F, 0); }
		public DeonticModContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_deonticMod; }
	}

	public final DeonticModContext deonticMod() throws RecognitionException {
		DeonticModContext _localctx = new DeonticModContext(_ctx, getState());
		enterRule(_localctx, 62, RULE_deonticMod);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(394);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 481036337152L) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class PriorityDeclContext extends ParserRuleContext {
		public TerminalNode OVERRIDE() { return getToken(MDLParser.OVERRIDE, 0); }
		public List<QualifiedNameContext> qualifiedName() {
			return getRuleContexts(QualifiedNameContext.class);
		}
		public QualifiedNameContext qualifiedName(int i) {
			return getRuleContext(QualifiedNameContext.class,i);
		}
		public List<TerminalNode> GT() { return getTokens(MDLParser.GT); }
		public TerminalNode GT(int i) {
			return getToken(MDLParser.GT, i);
		}
		public PriorityDeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_priorityDecl; }
	}

	public final PriorityDeclContext priorityDecl() throws RecognitionException {
		PriorityDeclContext _localctx = new PriorityDeclContext(_ctx, getState());
		enterRule(_localctx, 64, RULE_priorityDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(396);
			match(OVERRIDE);
			setState(397);
			qualifiedName();
			setState(402);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==GT) {
				{
				{
				setState(398);
				match(GT);
				setState(399);
				qualifiedName();
				}
				}
				setState(404);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class FactDeclContext extends ParserRuleContext {
		public TerminalNode FACT() { return getToken(MDLParser.FACT, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public NameTokenContext nameToken() {
			return getRuleContext(NameTokenContext.class,0);
		}
		public TerminalNode EQ() { return getToken(MDLParser.EQ, 0); }
		public FactDeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_factDecl; }
	}

	public final FactDeclContext factDecl() throws RecognitionException {
		FactDeclContext _localctx = new FactDeclContext(_ctx, getState());
		enterRule(_localctx, 66, RULE_factDecl);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(405);
			match(FACT);
			setState(409);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,36,_ctx) ) {
			case 1:
				{
				setState(406);
				nameToken();
				setState(407);
				match(EQ);
				}
				break;
			}
			setState(411);
			expr();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class BlockContext extends ParserRuleContext {
		public TerminalNode NEWLINE() { return getToken(MDLParser.NEWLINE, 0); }
		public TerminalNode INDENT() { return getToken(MDLParser.INDENT, 0); }
		public TerminalNode DEDENT() { return getToken(MDLParser.DEDENT, 0); }
		public List<BlockLetStmtContext> blockLetStmt() {
			return getRuleContexts(BlockLetStmtContext.class);
		}
		public BlockLetStmtContext blockLetStmt(int i) {
			return getRuleContext(BlockLetStmtContext.class,i);
		}
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public NewlinesContext newlines() {
			return getRuleContext(NewlinesContext.class,0);
		}
		public BlockContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_block; }
	}

	public final BlockContext block() throws RecognitionException {
		BlockContext _localctx = new BlockContext(_ctx, getState());
		enterRule(_localctx, 68, RULE_block);
		int _la;
		try {
			int _alt;
			setState(429);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case NEWLINE:
				enterOuterAlt(_localctx, 1);
				{
				setState(413);
				match(NEWLINE);
				setState(414);
				match(INDENT);
				setState(418);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,37,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(415);
						blockLetStmt();
						}
						} 
					}
					setState(420);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,37,_ctx);
				}
				setState(422);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 36151324435546624L) != 0)) {
					{
					setState(421);
					expr();
					}
				}

				setState(425);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(424);
					newlines();
					}
				}

				setState(427);
				match(DEDENT);
				}
				break;
			case LET:
			case IF:
			case CASE:
			case TRUE:
			case FALSE:
			case NOT:
			case O:
			case P:
			case F:
			case STRING:
			case RAT:
			case DECIMAL:
			case INT:
			case IDENT:
			case LPAREN:
			case MINUS:
				enterOuterAlt(_localctx, 2);
				{
				setState(428);
				expr();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class BlockLetStmtContext extends ParserRuleContext {
		public TerminalNode LET() { return getToken(MDLParser.LET, 0); }
		public PatternContext pattern() {
			return getRuleContext(PatternContext.class,0);
		}
		public TerminalNode EQ() { return getToken(MDLParser.EQ, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public List<NewlinesContext> newlines() {
			return getRuleContexts(NewlinesContext.class);
		}
		public NewlinesContext newlines(int i) {
			return getRuleContext(NewlinesContext.class,i);
		}
		public TypeAnnotationContext typeAnnotation() {
			return getRuleContext(TypeAnnotationContext.class,0);
		}
		public TerminalNode NEWLINE() { return getToken(MDLParser.NEWLINE, 0); }
		public TerminalNode INDENT() { return getToken(MDLParser.INDENT, 0); }
		public TerminalNode DEDENT() { return getToken(MDLParser.DEDENT, 0); }
		public BlockLetStmtContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_blockLetStmt; }
	}

	public final BlockLetStmtContext blockLetStmt() throws RecognitionException {
		BlockLetStmtContext _localctx = new BlockLetStmtContext(_ctx, getState());
		enterRule(_localctx, 70, RULE_blockLetStmt);
		int _la;
		try {
			setState(456);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,45,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(431);
				match(LET);
				setState(432);
				pattern();
				setState(434);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==COLON) {
					{
					setState(433);
					typeAnnotation();
					}
				}

				setState(436);
				match(EQ);
				setState(437);
				expr();
				setState(438);
				newlines();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(440);
				match(LET);
				setState(441);
				pattern();
				setState(443);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==COLON) {
					{
					setState(442);
					typeAnnotation();
					}
				}

				setState(445);
				match(EQ);
				setState(446);
				match(NEWLINE);
				setState(447);
				match(INDENT);
				setState(448);
				expr();
				setState(450);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(449);
					newlines();
					}
				}

				setState(452);
				match(DEDENT);
				setState(454);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,44,_ctx) ) {
				case 1:
					{
					setState(453);
					newlines();
					}
					break;
				}
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TypeAnnotationContext extends ParserRuleContext {
		public TerminalNode COLON() { return getToken(MDLParser.COLON, 0); }
		public TypeExprContext typeExpr() {
			return getRuleContext(TypeExprContext.class,0);
		}
		public TypeAnnotationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeAnnotation; }
	}

	public final TypeAnnotationContext typeAnnotation() throws RecognitionException {
		TypeAnnotationContext _localctx = new TypeAnnotationContext(_ctx, getState());
		enterRule(_localctx, 72, RULE_typeAnnotation);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(458);
			match(COLON);
			setState(459);
			typeExpr();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ContinuedExprContext extends ParserRuleContext {
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode NEWLINE() { return getToken(MDLParser.NEWLINE, 0); }
		public TerminalNode INDENT() { return getToken(MDLParser.INDENT, 0); }
		public TerminalNode DEDENT() { return getToken(MDLParser.DEDENT, 0); }
		public NewlinesContext newlines() {
			return getRuleContext(NewlinesContext.class,0);
		}
		public ContinuedExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_continuedExpr; }
	}

	public final ContinuedExprContext continuedExpr() throws RecognitionException {
		ContinuedExprContext _localctx = new ContinuedExprContext(_ctx, getState());
		enterRule(_localctx, 74, RULE_continuedExpr);
		int _la;
		try {
			setState(470);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case LET:
			case IF:
			case CASE:
			case TRUE:
			case FALSE:
			case NOT:
			case O:
			case P:
			case F:
			case STRING:
			case RAT:
			case DECIMAL:
			case INT:
			case IDENT:
			case LPAREN:
			case MINUS:
				enterOuterAlt(_localctx, 1);
				{
				setState(461);
				expr();
				}
				break;
			case NEWLINE:
				enterOuterAlt(_localctx, 2);
				{
				setState(462);
				match(NEWLINE);
				setState(463);
				match(INDENT);
				setState(464);
				expr();
				setState(466);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(465);
					newlines();
					}
				}

				setState(468);
				match(DEDENT);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ExprContext extends ParserRuleContext {
		public TemporalPostfixContext temporalPostfix() {
			return getRuleContext(TemporalPostfixContext.class,0);
		}
		public ExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expr; }
	}

	public final ExprContext expr() throws RecognitionException {
		ExprContext _localctx = new ExprContext(_ctx, getState());
		enterRule(_localctx, 76, RULE_expr);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(472);
			temporalPostfix();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TemporalPostfixContext extends ParserRuleContext {
		public ImplicationContext implication() {
			return getRuleContext(ImplicationContext.class,0);
		}
		public List<TemporalUnaryOpContext> temporalUnaryOp() {
			return getRuleContexts(TemporalUnaryOpContext.class);
		}
		public TemporalUnaryOpContext temporalUnaryOp(int i) {
			return getRuleContext(TemporalUnaryOpContext.class,i);
		}
		public TemporalPostfixContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_temporalPostfix; }
	}

	public final TemporalPostfixContext temporalPostfix() throws RecognitionException {
		TemporalPostfixContext _localctx = new TemporalPostfixContext(_ctx, getState());
		enterRule(_localctx, 78, RULE_temporalPostfix);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(474);
			implication();
			setState(478);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,48,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(475);
					temporalUnaryOp();
					}
					} 
				}
				setState(480);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,48,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ImplicationContext extends ParserRuleContext {
		public OrExprContext orExpr() {
			return getRuleContext(OrExprContext.class,0);
		}
		public TerminalNode IMPLIES() { return getToken(MDLParser.IMPLIES, 0); }
		public ImplicationContext implication() {
			return getRuleContext(ImplicationContext.class,0);
		}
		public ImplicationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_implication; }
	}

	public final ImplicationContext implication() throws RecognitionException {
		ImplicationContext _localctx = new ImplicationContext(_ctx, getState());
		enterRule(_localctx, 80, RULE_implication);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(481);
			orExpr();
			setState(484);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,49,_ctx) ) {
			case 1:
				{
				setState(482);
				match(IMPLIES);
				setState(483);
				implication();
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class OrExprContext extends ParserRuleContext {
		public List<AndExprContext> andExpr() {
			return getRuleContexts(AndExprContext.class);
		}
		public AndExprContext andExpr(int i) {
			return getRuleContext(AndExprContext.class,i);
		}
		public List<TerminalNode> OR() { return getTokens(MDLParser.OR); }
		public TerminalNode OR(int i) {
			return getToken(MDLParser.OR, i);
		}
		public OrExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_orExpr; }
	}

	public final OrExprContext orExpr() throws RecognitionException {
		OrExprContext _localctx = new OrExprContext(_ctx, getState());
		enterRule(_localctx, 82, RULE_orExpr);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(486);
			andExpr();
			setState(491);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,50,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(487);
					match(OR);
					setState(488);
					andExpr();
					}
					} 
				}
				setState(493);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,50,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class AndExprContext extends ParserRuleContext {
		public List<TemporalBinaryContext> temporalBinary() {
			return getRuleContexts(TemporalBinaryContext.class);
		}
		public TemporalBinaryContext temporalBinary(int i) {
			return getRuleContext(TemporalBinaryContext.class,i);
		}
		public List<TerminalNode> AND() { return getTokens(MDLParser.AND); }
		public TerminalNode AND(int i) {
			return getToken(MDLParser.AND, i);
		}
		public AndExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_andExpr; }
	}

	public final AndExprContext andExpr() throws RecognitionException {
		AndExprContext _localctx = new AndExprContext(_ctx, getState());
		enterRule(_localctx, 84, RULE_andExpr);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(494);
			temporalBinary();
			setState(499);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,51,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(495);
					match(AND);
					setState(496);
					temporalBinary();
					}
					} 
				}
				setState(501);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,51,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TemporalBinaryContext extends ParserRuleContext {
		public List<ComparisonContext> comparison() {
			return getRuleContexts(ComparisonContext.class);
		}
		public ComparisonContext comparison(int i) {
			return getRuleContext(ComparisonContext.class,i);
		}
		public List<TerminalNode> UNTIL() { return getTokens(MDLParser.UNTIL); }
		public TerminalNode UNTIL(int i) {
			return getToken(MDLParser.UNTIL, i);
		}
		public TemporalBinaryContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_temporalBinary; }
	}

	public final TemporalBinaryContext temporalBinary() throws RecognitionException {
		TemporalBinaryContext _localctx = new TemporalBinaryContext(_ctx, getState());
		enterRule(_localctx, 86, RULE_temporalBinary);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(502);
			comparison();
			setState(507);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,52,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(503);
					match(UNTIL);
					setState(504);
					comparison();
					}
					} 
				}
				setState(509);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,52,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ComparisonContext extends ParserRuleContext {
		public List<AdditiveContext> additive() {
			return getRuleContexts(AdditiveContext.class);
		}
		public AdditiveContext additive(int i) {
			return getRuleContext(AdditiveContext.class,i);
		}
		public List<TerminalNode> EQ() { return getTokens(MDLParser.EQ); }
		public TerminalNode EQ(int i) {
			return getToken(MDLParser.EQ, i);
		}
		public List<TerminalNode> NE() { return getTokens(MDLParser.NE); }
		public TerminalNode NE(int i) {
			return getToken(MDLParser.NE, i);
		}
		public List<TerminalNode> LT() { return getTokens(MDLParser.LT); }
		public TerminalNode LT(int i) {
			return getToken(MDLParser.LT, i);
		}
		public List<TerminalNode> LE() { return getTokens(MDLParser.LE); }
		public TerminalNode LE(int i) {
			return getToken(MDLParser.LE, i);
		}
		public List<TerminalNode> GT() { return getTokens(MDLParser.GT); }
		public TerminalNode GT(int i) {
			return getToken(MDLParser.GT, i);
		}
		public List<TerminalNode> GE() { return getTokens(MDLParser.GE); }
		public TerminalNode GE(int i) {
			return getToken(MDLParser.GE, i);
		}
		public ComparisonContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_comparison; }
	}

	public final ComparisonContext comparison() throws RecognitionException {
		ComparisonContext _localctx = new ComparisonContext(_ctx, getState());
		enterRule(_localctx, 88, RULE_comparison);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(510);
			additive();
			setState(515);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,53,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(511);
					_la = _input.LA(1);
					if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 4035225266123964444L) != 0)) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(512);
					additive();
					}
					} 
				}
				setState(517);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,53,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class AdditiveContext extends ParserRuleContext {
		public List<MultiplicativeContext> multiplicative() {
			return getRuleContexts(MultiplicativeContext.class);
		}
		public MultiplicativeContext multiplicative(int i) {
			return getRuleContext(MultiplicativeContext.class,i);
		}
		public List<TerminalNode> PLUS() { return getTokens(MDLParser.PLUS); }
		public TerminalNode PLUS(int i) {
			return getToken(MDLParser.PLUS, i);
		}
		public List<TerminalNode> MINUS() { return getTokens(MDLParser.MINUS); }
		public TerminalNode MINUS(int i) {
			return getToken(MDLParser.MINUS, i);
		}
		public AdditiveContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_additive; }
	}

	public final AdditiveContext additive() throws RecognitionException {
		AdditiveContext _localctx = new AdditiveContext(_ctx, getState());
		enterRule(_localctx, 90, RULE_additive);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(518);
			multiplicative();
			setState(523);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,54,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(519);
					_la = _input.LA(1);
					if ( !(_la==PLUS || _la==MINUS) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(520);
					multiplicative();
					}
					} 
				}
				setState(525);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,54,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class MultiplicativeContext extends ParserRuleContext {
		public List<UnaryContext> unary() {
			return getRuleContexts(UnaryContext.class);
		}
		public UnaryContext unary(int i) {
			return getRuleContext(UnaryContext.class,i);
		}
		public List<TerminalNode> STAR() { return getTokens(MDLParser.STAR); }
		public TerminalNode STAR(int i) {
			return getToken(MDLParser.STAR, i);
		}
		public List<TerminalNode> SLASH() { return getTokens(MDLParser.SLASH); }
		public TerminalNode SLASH(int i) {
			return getToken(MDLParser.SLASH, i);
		}
		public List<TerminalNode> PERCENT() { return getTokens(MDLParser.PERCENT); }
		public TerminalNode PERCENT(int i) {
			return getToken(MDLParser.PERCENT, i);
		}
		public MultiplicativeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_multiplicative; }
	}

	public final MultiplicativeContext multiplicative() throws RecognitionException {
		MultiplicativeContext _localctx = new MultiplicativeContext(_ctx, getState());
		enterRule(_localctx, 92, RULE_multiplicative);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(526);
			unary();
			setState(531);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,55,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(527);
					_la = _input.LA(1);
					if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 504403158265495552L) != 0)) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(528);
					unary();
					}
					} 
				}
				setState(533);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,55,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class UnaryContext extends ParserRuleContext {
		public IfExprContext ifExpr() {
			return getRuleContext(IfExprContext.class,0);
		}
		public LetExprContext letExpr() {
			return getRuleContext(LetExprContext.class,0);
		}
		public MatchExprContext matchExpr() {
			return getRuleContext(MatchExprContext.class,0);
		}
		public UnaryContext unary() {
			return getRuleContext(UnaryContext.class,0);
		}
		public TerminalNode NOT() { return getToken(MDLParser.NOT, 0); }
		public TerminalNode MINUS() { return getToken(MDLParser.MINUS, 0); }
		public PostfixContext postfix() {
			return getRuleContext(PostfixContext.class,0);
		}
		public UnaryContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_unary; }
	}

	public final UnaryContext unary() throws RecognitionException {
		UnaryContext _localctx = new UnaryContext(_ctx, getState());
		enterRule(_localctx, 94, RULE_unary);
		int _la;
		try {
			setState(540);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case IF:
				enterOuterAlt(_localctx, 1);
				{
				setState(534);
				ifExpr();
				}
				break;
			case LET:
				enterOuterAlt(_localctx, 2);
				{
				setState(535);
				letExpr();
				}
				break;
			case CASE:
				enterOuterAlt(_localctx, 3);
				{
				setState(536);
				matchExpr();
				}
				break;
			case NOT:
			case MINUS:
				enterOuterAlt(_localctx, 4);
				{
				setState(537);
				_la = _input.LA(1);
				if ( !(_la==NOT || _la==MINUS) ) {
				_errHandler.recoverInline(this);
				}
				else {
					if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
				setState(538);
				unary();
				}
				break;
			case TRUE:
			case FALSE:
			case O:
			case P:
			case F:
			case STRING:
			case RAT:
			case DECIMAL:
			case INT:
			case IDENT:
			case LPAREN:
				enterOuterAlt(_localctx, 5);
				{
				setState(539);
				postfix();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class IfExprContext extends ParserRuleContext {
		public TerminalNode IF() { return getToken(MDLParser.IF, 0); }
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public TerminalNode THEN() { return getToken(MDLParser.THEN, 0); }
		public TerminalNode ELSE() { return getToken(MDLParser.ELSE, 0); }
		public List<NewlinesContext> newlines() {
			return getRuleContexts(NewlinesContext.class);
		}
		public NewlinesContext newlines(int i) {
			return getRuleContext(NewlinesContext.class,i);
		}
		public IfExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ifExpr; }
	}

	public final IfExprContext ifExpr() throws RecognitionException {
		IfExprContext _localctx = new IfExprContext(_ctx, getState());
		enterRule(_localctx, 96, RULE_ifExpr);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(542);
			match(IF);
			setState(543);
			expr();
			setState(545);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(544);
				newlines();
				}
			}

			setState(547);
			match(THEN);
			setState(548);
			expr();
			setState(550);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(549);
				newlines();
				}
			}

			setState(552);
			match(ELSE);
			setState(553);
			expr();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class LetExprContext extends ParserRuleContext {
		public TerminalNode LET() { return getToken(MDLParser.LET, 0); }
		public PatternContext pattern() {
			return getRuleContext(PatternContext.class,0);
		}
		public TerminalNode EQ() { return getToken(MDLParser.EQ, 0); }
		public ContinuedExprContext continuedExpr() {
			return getRuleContext(ContinuedExprContext.class,0);
		}
		public TerminalNode IN() { return getToken(MDLParser.IN, 0); }
		public LetBodyExprContext letBodyExpr() {
			return getRuleContext(LetBodyExprContext.class,0);
		}
		public TypeAnnotationContext typeAnnotation() {
			return getRuleContext(TypeAnnotationContext.class,0);
		}
		public NewlinesContext newlines() {
			return getRuleContext(NewlinesContext.class,0);
		}
		public LetExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_letExpr; }
	}

	public final LetExprContext letExpr() throws RecognitionException {
		LetExprContext _localctx = new LetExprContext(_ctx, getState());
		enterRule(_localctx, 98, RULE_letExpr);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(555);
			match(LET);
			setState(556);
			pattern();
			setState(558);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COLON) {
				{
				setState(557);
				typeAnnotation();
				}
			}

			setState(560);
			match(EQ);
			setState(561);
			continuedExpr();
			setState(563);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(562);
				newlines();
				}
			}

			setState(565);
			match(IN);
			setState(566);
			letBodyExpr();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class LetBodyExprContext extends ParserRuleContext {
		public TerminalNode NEWLINE() { return getToken(MDLParser.NEWLINE, 0); }
		public TerminalNode INDENT() { return getToken(MDLParser.INDENT, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode DEDENT() { return getToken(MDLParser.DEDENT, 0); }
		public NewlinesContext newlines() {
			return getRuleContext(NewlinesContext.class,0);
		}
		public LetBodyExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_letBodyExpr; }
	}

	public final LetBodyExprContext letBodyExpr() throws RecognitionException {
		LetBodyExprContext _localctx = new LetBodyExprContext(_ctx, getState());
		enterRule(_localctx, 100, RULE_letBodyExpr);
		int _la;
		try {
			setState(580);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,62,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(568);
				match(NEWLINE);
				setState(569);
				match(INDENT);
				setState(570);
				expr();
				setState(572);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(571);
					newlines();
					}
				}

				setState(574);
				match(DEDENT);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(576);
				newlines();
				setState(577);
				expr();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(579);
				expr();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class MatchExprContext extends ParserRuleContext {
		public TerminalNode CASE() { return getToken(MDLParser.CASE, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode COLON() { return getToken(MDLParser.COLON, 0); }
		public CaseBodyContext caseBody() {
			return getRuleContext(CaseBodyContext.class,0);
		}
		public MatchExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_matchExpr; }
	}

	public final MatchExprContext matchExpr() throws RecognitionException {
		MatchExprContext _localctx = new MatchExprContext(_ctx, getState());
		enterRule(_localctx, 102, RULE_matchExpr);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(582);
			match(CASE);
			setState(583);
			expr();
			setState(584);
			match(COLON);
			setState(585);
			caseBody();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class CaseBodyContext extends ParserRuleContext {
		public TerminalNode NEWLINE() { return getToken(MDLParser.NEWLINE, 0); }
		public TerminalNode INDENT() { return getToken(MDLParser.INDENT, 0); }
		public List<CaseArmContext> caseArm() {
			return getRuleContexts(CaseArmContext.class);
		}
		public CaseArmContext caseArm(int i) {
			return getRuleContext(CaseArmContext.class,i);
		}
		public TerminalNode DEDENT() { return getToken(MDLParser.DEDENT, 0); }
		public List<NewlinesContext> newlines() {
			return getRuleContexts(NewlinesContext.class);
		}
		public NewlinesContext newlines(int i) {
			return getRuleContext(NewlinesContext.class,i);
		}
		public CaseBodyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_caseBody; }
	}

	public final CaseBodyContext caseBody() throws RecognitionException {
		CaseBodyContext _localctx = new CaseBodyContext(_ctx, getState());
		enterRule(_localctx, 104, RULE_caseBody);
		int _la;
		try {
			int _alt;
			setState(637);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,74,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(587);
				match(NEWLINE);
				setState(588);
				match(INDENT);
				setState(590);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(589);
					newlines();
					}
				}

				setState(592);
				caseArm();
				setState(599);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,65,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(594);
						_errHandler.sync(this);
						_la = _input.LA(1);
						if (_la==NEWLINE) {
							{
							setState(593);
							newlines();
							}
						}

						setState(596);
						caseArm();
						}
						} 
					}
					setState(601);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,65,_ctx);
				}
				setState(603);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(602);
					newlines();
					}
				}

				setState(605);
				match(DEDENT);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(607);
				match(NEWLINE);
				setState(609);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(608);
					newlines();
					}
				}

				setState(611);
				caseArm();
				setState(618);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,69,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(613);
						_errHandler.sync(this);
						_la = _input.LA(1);
						if (_la==NEWLINE) {
							{
							setState(612);
							newlines();
							}
						}

						setState(615);
						caseArm();
						}
						} 
					}
					setState(620);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,69,_ctx);
				}
				setState(622);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,70,_ctx) ) {
				case 1:
					{
					setState(621);
					newlines();
					}
					break;
				}
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(624);
				caseArm();
				setState(631);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,72,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(626);
						_errHandler.sync(this);
						_la = _input.LA(1);
						if (_la==NEWLINE) {
							{
							setState(625);
							newlines();
							}
						}

						setState(628);
						caseArm();
						}
						} 
					}
					setState(633);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,72,_ctx);
				}
				setState(635);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,73,_ctx) ) {
				case 1:
					{
					setState(634);
					newlines();
					}
					break;
				}
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class CaseArmContext extends ParserRuleContext {
		public TerminalNode BAR() { return getToken(MDLParser.BAR, 0); }
		public PatternContext pattern() {
			return getRuleContext(PatternContext.class,0);
		}
		public TerminalNode COLON() { return getToken(MDLParser.COLON, 0); }
		public BlockContext block() {
			return getRuleContext(BlockContext.class,0);
		}
		public TerminalNode WHEN() { return getToken(MDLParser.WHEN, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public CaseArmContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_caseArm; }
	}

	public final CaseArmContext caseArm() throws RecognitionException {
		CaseArmContext _localctx = new CaseArmContext(_ctx, getState());
		enterRule(_localctx, 106, RULE_caseArm);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(639);
			match(BAR);
			setState(640);
			pattern();
			setState(643);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==WHEN) {
				{
				setState(641);
				match(WHEN);
				setState(642);
				expr();
				}
			}

			setState(645);
			match(COLON);
			setState(646);
			block();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class PostfixContext extends ParserRuleContext {
		public PrimaryContext primary() {
			return getRuleContext(PrimaryContext.class,0);
		}
		public List<PostfixSuffixContext> postfixSuffix() {
			return getRuleContexts(PostfixSuffixContext.class);
		}
		public PostfixSuffixContext postfixSuffix(int i) {
			return getRuleContext(PostfixSuffixContext.class,i);
		}
		public PostfixContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_postfix; }
	}

	public final PostfixContext postfix() throws RecognitionException {
		PostfixContext _localctx = new PostfixContext(_ctx, getState());
		enterRule(_localctx, 108, RULE_postfix);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(648);
			primary();
			setState(652);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & 4855443348258816L) != 0)) {
				{
				{
				setState(649);
				postfixSuffix();
				}
				}
				setState(654);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class PostfixSuffixContext extends ParserRuleContext {
		public RecordConstructorFieldsContext recordConstructorFields() {
			return getRuleContext(RecordConstructorFieldsContext.class,0);
		}
		public TerminalNode LPAREN() { return getToken(MDLParser.LPAREN, 0); }
		public TerminalNode RPAREN() { return getToken(MDLParser.RPAREN, 0); }
		public ExprListContext exprList() {
			return getRuleContext(ExprListContext.class,0);
		}
		public TerminalNode DOT() { return getToken(MDLParser.DOT, 0); }
		public NameTokenContext nameToken() {
			return getRuleContext(NameTokenContext.class,0);
		}
		public PostfixSuffixContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_postfixSuffix; }
	}

	public final PostfixSuffixContext postfixSuffix() throws RecognitionException {
		PostfixSuffixContext _localctx = new PostfixSuffixContext(_ctx, getState());
		enterRule(_localctx, 110, RULE_postfixSuffix);
		int _la;
		try {
			setState(663);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case LBRACE:
				enterOuterAlt(_localctx, 1);
				{
				setState(655);
				recordConstructorFields();
				}
				break;
			case LPAREN:
				enterOuterAlt(_localctx, 2);
				{
				setState(656);
				match(LPAREN);
				setState(658);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 36151324435546624L) != 0)) {
					{
					setState(657);
					exprList();
					}
				}

				setState(660);
				match(RPAREN);
				}
				break;
			case DOT:
				enterOuterAlt(_localctx, 3);
				{
				setState(661);
				match(DOT);
				setState(662);
				nameToken();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class RecordConstructorFieldsContext extends ParserRuleContext {
		public TerminalNode LBRACE() { return getToken(MDLParser.LBRACE, 0); }
		public TerminalNode RBRACE() { return getToken(MDLParser.RBRACE, 0); }
		public RecordConstructorFieldListContext recordConstructorFieldList() {
			return getRuleContext(RecordConstructorFieldListContext.class,0);
		}
		public RecordConstructorFieldsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_recordConstructorFields; }
	}

	public final RecordConstructorFieldsContext recordConstructorFields() throws RecognitionException {
		RecordConstructorFieldsContext _localctx = new RecordConstructorFieldsContext(_ctx, getState());
		enterRule(_localctx, 112, RULE_recordConstructorFields);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(665);
			match(LBRACE);
			setState(667);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 35665458757632L) != 0)) {
				{
				setState(666);
				recordConstructorFieldList();
				}
			}

			setState(669);
			match(RBRACE);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class RecordConstructorFieldListContext extends ParserRuleContext {
		public List<RecordConstructorFieldContext> recordConstructorField() {
			return getRuleContexts(RecordConstructorFieldContext.class);
		}
		public RecordConstructorFieldContext recordConstructorField(int i) {
			return getRuleContext(RecordConstructorFieldContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(MDLParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(MDLParser.COMMA, i);
		}
		public RecordConstructorFieldListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_recordConstructorFieldList; }
	}

	public final RecordConstructorFieldListContext recordConstructorFieldList() throws RecognitionException {
		RecordConstructorFieldListContext _localctx = new RecordConstructorFieldListContext(_ctx, getState());
		enterRule(_localctx, 114, RULE_recordConstructorFieldList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(671);
			recordConstructorField();
			setState(676);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(672);
				match(COMMA);
				setState(673);
				recordConstructorField();
				}
				}
				setState(678);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class RecordConstructorFieldContext extends ParserRuleContext {
		public NameTokenContext nameToken() {
			return getRuleContext(NameTokenContext.class,0);
		}
		public TerminalNode EQ() { return getToken(MDLParser.EQ, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public RecordConstructorFieldContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_recordConstructorField; }
	}

	public final RecordConstructorFieldContext recordConstructorField() throws RecognitionException {
		RecordConstructorFieldContext _localctx = new RecordConstructorFieldContext(_ctx, getState());
		enterRule(_localctx, 116, RULE_recordConstructorField);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(679);
			nameToken();
			setState(680);
			match(EQ);
			setState(681);
			expr();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class PrimaryContext extends ParserRuleContext {
		public TerminalNode STRING() { return getToken(MDLParser.STRING, 0); }
		public TerminalNode INT() { return getToken(MDLParser.INT, 0); }
		public TerminalNode DECIMAL() { return getToken(MDLParser.DECIMAL, 0); }
		public TerminalNode RAT() { return getToken(MDLParser.RAT, 0); }
		public TerminalNode TRUE() { return getToken(MDLParser.TRUE, 0); }
		public TerminalNode FALSE() { return getToken(MDLParser.FALSE, 0); }
		public QualifiedNameContext qualifiedName() {
			return getRuleContext(QualifiedNameContext.class,0);
		}
		public TerminalNode LPAREN() { return getToken(MDLParser.LPAREN, 0); }
		public TerminalNode RPAREN() { return getToken(MDLParser.RPAREN, 0); }
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(MDLParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(MDLParser.COMMA, i);
		}
		public PrimaryContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_primary; }
	}

	public final PrimaryContext primary() throws RecognitionException {
		PrimaryContext _localctx = new PrimaryContext(_ctx, getState());
		enterRule(_localctx, 118, RULE_primary);
		int _la;
		try {
			setState(709);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,82,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(683);
				match(STRING);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(684);
				match(INT);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(685);
				match(DECIMAL);
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(686);
				match(RAT);
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(687);
				match(TRUE);
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(688);
				match(FALSE);
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(689);
				qualifiedName();
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(690);
				match(LPAREN);
				setState(691);
				match(RPAREN);
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(692);
				match(LPAREN);
				setState(693);
				expr();
				setState(694);
				match(RPAREN);
				}
				break;
			case 10:
				enterOuterAlt(_localctx, 10);
				{
				setState(696);
				match(LPAREN);
				setState(697);
				expr();
				setState(698);
				match(COMMA);
				setState(699);
				expr();
				setState(704);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(700);
					match(COMMA);
					setState(701);
					expr();
					}
					}
					setState(706);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(707);
				match(RPAREN);
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ExprListContext extends ParserRuleContext {
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(MDLParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(MDLParser.COMMA, i);
		}
		public ExprListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprList; }
	}

	public final ExprListContext exprList() throws RecognitionException {
		ExprListContext _localctx = new ExprListContext(_ctx, getState());
		enterRule(_localctx, 120, RULE_exprList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(711);
			expr();
			setState(716);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(712);
				match(COMMA);
				setState(713);
				expr();
				}
				}
				setState(718);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class PatternContext extends ParserRuleContext {
		public TerminalNode UNDERSCORE() { return getToken(MDLParser.UNDERSCORE, 0); }
		public TerminalNode STRING() { return getToken(MDLParser.STRING, 0); }
		public TerminalNode INT() { return getToken(MDLParser.INT, 0); }
		public TerminalNode DECIMAL() { return getToken(MDLParser.DECIMAL, 0); }
		public TerminalNode RAT() { return getToken(MDLParser.RAT, 0); }
		public TerminalNode LPAREN() { return getToken(MDLParser.LPAREN, 0); }
		public TerminalNode RPAREN() { return getToken(MDLParser.RPAREN, 0); }
		public List<PatternContext> pattern() {
			return getRuleContexts(PatternContext.class);
		}
		public PatternContext pattern(int i) {
			return getRuleContext(PatternContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(MDLParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(MDLParser.COMMA, i);
		}
		public TerminalNode LBRACE() { return getToken(MDLParser.LBRACE, 0); }
		public TerminalNode RBRACE() { return getToken(MDLParser.RBRACE, 0); }
		public RecordPatternFieldListContext recordPatternFieldList() {
			return getRuleContext(RecordPatternFieldListContext.class,0);
		}
		public QualifiedNameContext qualifiedName() {
			return getRuleContext(QualifiedNameContext.class,0);
		}
		public PatternListContext patternList() {
			return getRuleContext(PatternListContext.class,0);
		}
		public PatternContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_pattern; }
	}

	public final PatternContext pattern() throws RecognitionException {
		PatternContext _localctx = new PatternContext(_ctx, getState());
		enterRule(_localctx, 122, RULE_pattern);
		int _la;
		try {
			setState(756);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,88,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(719);
				match(UNDERSCORE);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(720);
				match(STRING);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(721);
				match(INT);
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(722);
				match(DECIMAL);
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(723);
				match(RAT);
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(724);
				match(LPAREN);
				setState(725);
				match(RPAREN);
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(726);
				match(LPAREN);
				setState(727);
				pattern();
				setState(728);
				match(RPAREN);
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(730);
				match(LPAREN);
				setState(731);
				pattern();
				setState(732);
				match(COMMA);
				setState(733);
				pattern();
				setState(738);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(734);
					match(COMMA);
					setState(735);
					pattern();
					}
					}
					setState(740);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(741);
				match(RPAREN);
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(743);
				match(LBRACE);
				setState(745);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 35665458757632L) != 0)) {
					{
					setState(744);
					recordPatternFieldList();
					}
				}

				setState(747);
				match(RBRACE);
				}
				break;
			case 10:
				enterOuterAlt(_localctx, 10);
				{
				setState(748);
				qualifiedName();
				setState(754);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==LPAREN) {
					{
					setState(749);
					match(LPAREN);
					setState(751);
					_errHandler.sync(this);
					_la = _input.LA(1);
					if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 421594040107008L) != 0)) {
						{
						setState(750);
						patternList();
						}
					}

					setState(753);
					match(RPAREN);
					}
				}

				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class PatternListContext extends ParserRuleContext {
		public List<PatternContext> pattern() {
			return getRuleContexts(PatternContext.class);
		}
		public PatternContext pattern(int i) {
			return getRuleContext(PatternContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(MDLParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(MDLParser.COMMA, i);
		}
		public PatternListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_patternList; }
	}

	public final PatternListContext patternList() throws RecognitionException {
		PatternListContext _localctx = new PatternListContext(_ctx, getState());
		enterRule(_localctx, 124, RULE_patternList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(758);
			pattern();
			setState(763);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(759);
				match(COMMA);
				setState(760);
				pattern();
				}
				}
				setState(765);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class RecordPatternFieldListContext extends ParserRuleContext {
		public List<RecordPatternFieldContext> recordPatternField() {
			return getRuleContexts(RecordPatternFieldContext.class);
		}
		public RecordPatternFieldContext recordPatternField(int i) {
			return getRuleContext(RecordPatternFieldContext.class,i);
		}
		public List<TerminalNode> COMMA() { return getTokens(MDLParser.COMMA); }
		public TerminalNode COMMA(int i) {
			return getToken(MDLParser.COMMA, i);
		}
		public RecordPatternFieldListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_recordPatternFieldList; }
	}

	public final RecordPatternFieldListContext recordPatternFieldList() throws RecognitionException {
		RecordPatternFieldListContext _localctx = new RecordPatternFieldListContext(_ctx, getState());
		enterRule(_localctx, 126, RULE_recordPatternFieldList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(766);
			recordPatternField();
			setState(771);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(767);
				match(COMMA);
				setState(768);
				recordPatternField();
				}
				}
				setState(773);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class RecordPatternFieldContext extends ParserRuleContext {
		public NameTokenContext nameToken() {
			return getRuleContext(NameTokenContext.class,0);
		}
		public TerminalNode EQ() { return getToken(MDLParser.EQ, 0); }
		public PatternContext pattern() {
			return getRuleContext(PatternContext.class,0);
		}
		public RecordPatternFieldContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_recordPatternField; }
	}

	public final RecordPatternFieldContext recordPatternField() throws RecognitionException {
		RecordPatternFieldContext _localctx = new RecordPatternFieldContext(_ctx, getState());
		enterRule(_localctx, 128, RULE_recordPatternField);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(774);
			nameToken();
			setState(777);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==EQ) {
				{
				setState(775);
				match(EQ);
				setState(776);
				pattern();
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class QualifiedNameContext extends ParserRuleContext {
		public List<NameTokenContext> nameToken() {
			return getRuleContexts(NameTokenContext.class);
		}
		public NameTokenContext nameToken(int i) {
			return getRuleContext(NameTokenContext.class,i);
		}
		public List<TerminalNode> DOT() { return getTokens(MDLParser.DOT); }
		public TerminalNode DOT(int i) {
			return getToken(MDLParser.DOT, i);
		}
		public QualifiedNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_qualifiedName; }
	}

	public final QualifiedNameContext qualifiedName() throws RecognitionException {
		QualifiedNameContext _localctx = new QualifiedNameContext(_ctx, getState());
		enterRule(_localctx, 130, RULE_qualifiedName);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(779);
			nameToken();
			setState(784);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,92,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(780);
					match(DOT);
					setState(781);
					nameToken();
					}
					} 
				}
				setState(786);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,92,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class NameTokenContext extends ParserRuleContext {
		public TerminalNode IDENT() { return getToken(MDLParser.IDENT, 0); }
		public TerminalNode TRUE() { return getToken(MDLParser.TRUE, 0); }
		public TerminalNode FALSE() { return getToken(MDLParser.FALSE, 0); }
		public TerminalNode O() { return getToken(MDLParser.O, 0); }
		public TerminalNode P() { return getToken(MDLParser.P, 0); }
		public TerminalNode F() { return getToken(MDLParser.F, 0); }
		public NameTokenContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_nameToken; }
	}

	public final NameTokenContext nameToken() throws RecognitionException {
		NameTokenContext _localctx = new NameTokenContext(_ctx, getState());
		enterRule(_localctx, 132, RULE_nameToken);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(787);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 35665458757632L) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class TemporalUnaryOpContext extends ParserRuleContext {
		public TerminalNode ALWAYS() { return getToken(MDLParser.ALWAYS, 0); }
		public TerminalNode EVENTUALLY() { return getToken(MDLParser.EVENTUALLY, 0); }
		public TerminalNode NEXT() { return getToken(MDLParser.NEXT, 0); }
		public TerminalNode NOW() { return getToken(MDLParser.NOW, 0); }
		public TemporalUnaryOpContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_temporalUnaryOp; }
	}

	public final TemporalUnaryOpContext temporalUnaryOp() throws RecognitionException {
		TemporalUnaryOpContext _localctx = new TemporalUnaryOpContext(_ctx, getState());
		enterRule(_localctx, 134, RULE_temporalUnaryOp);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(789);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 16106127360L) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	@SuppressWarnings("CheckReturnValue")
	public static class NewlinesContext extends ParserRuleContext {
		public List<TerminalNode> NEWLINE() { return getTokens(MDLParser.NEWLINE); }
		public TerminalNode NEWLINE(int i) {
			return getToken(MDLParser.NEWLINE, i);
		}
		public NewlinesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_newlines; }
	}

	public final NewlinesContext newlines() throws RecognitionException {
		NewlinesContext _localctx = new NewlinesContext(_ctx, getState());
		enterRule(_localctx, 136, RULE_newlines);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(792); 
			_errHandler.sync(this);
			_alt = 1;
			do {
				switch (_alt) {
				case 1:
					{
					{
					setState(791);
					match(NEWLINE);
					}
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(794); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,93,_ctx);
			} while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN =
		"\u0004\u0001B\u031d\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001\u0002"+
		"\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004\u0007\u0004\u0002"+
		"\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007\u0007\u0007\u0002"+
		"\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0002\u000b\u0007\u000b\u0002"+
		"\f\u0007\f\u0002\r\u0007\r\u0002\u000e\u0007\u000e\u0002\u000f\u0007\u000f"+
		"\u0002\u0010\u0007\u0010\u0002\u0011\u0007\u0011\u0002\u0012\u0007\u0012"+
		"\u0002\u0013\u0007\u0013\u0002\u0014\u0007\u0014\u0002\u0015\u0007\u0015"+
		"\u0002\u0016\u0007\u0016\u0002\u0017\u0007\u0017\u0002\u0018\u0007\u0018"+
		"\u0002\u0019\u0007\u0019\u0002\u001a\u0007\u001a\u0002\u001b\u0007\u001b"+
		"\u0002\u001c\u0007\u001c\u0002\u001d\u0007\u001d\u0002\u001e\u0007\u001e"+
		"\u0002\u001f\u0007\u001f\u0002 \u0007 \u0002!\u0007!\u0002\"\u0007\"\u0002"+
		"#\u0007#\u0002$\u0007$\u0002%\u0007%\u0002&\u0007&\u0002\'\u0007\'\u0002"+
		"(\u0007(\u0002)\u0007)\u0002*\u0007*\u0002+\u0007+\u0002,\u0007,\u0002"+
		"-\u0007-\u0002.\u0007.\u0002/\u0007/\u00020\u00070\u00021\u00071\u0002"+
		"2\u00072\u00023\u00073\u00024\u00074\u00025\u00075\u00026\u00076\u0002"+
		"7\u00077\u00028\u00078\u00029\u00079\u0002:\u0007:\u0002;\u0007;\u0002"+
		"<\u0007<\u0002=\u0007=\u0002>\u0007>\u0002?\u0007?\u0002@\u0007@\u0002"+
		"A\u0007A\u0002B\u0007B\u0002C\u0007C\u0002D\u0007D\u0001\u0000\u0003\u0000"+
		"\u008c\b\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0003\u0000\u0091\b"+
		"\u0000\u0001\u0000\u0005\u0000\u0094\b\u0000\n\u0000\f\u0000\u0097\t\u0000"+
		"\u0001\u0000\u0001\u0000\u0001\u0001\u0003\u0001\u009c\b\u0001\u0001\u0001"+
		"\u0001\u0001\u0003\u0001\u00a0\b\u0001\u0001\u0001\u0001\u0001\u0001\u0002"+
		"\u0003\u0002\u00a5\b\u0002\u0001\u0002\u0001\u0002\u0003\u0002\u00a9\b"+
		"\u0002\u0001\u0002\u0001\u0002\u0001\u0003\u0001\u0003\u0001\u0003\u0001"+
		"\u0003\u0003\u0003\u00b1\b\u0003\u0001\u0003\u0003\u0003\u00b4\b\u0003"+
		"\u0001\u0004\u0001\u0004\u0003\u0004\u00b8\b\u0004\u0005\u0004\u00ba\b"+
		"\u0004\n\u0004\f\u0004\u00bd\t\u0004\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0007\u0001\u0007\u0001\u0007"+
		"\u0001\b\u0001\b\u0001\b\u0001\b\u0001\b\u0001\b\u0003\b\u00ce\b\b\u0001"+
		"\t\u0001\t\u0001\t\u0003\t\u00d3\b\t\u0001\t\u0001\t\u0001\t\u0001\n\u0001"+
		"\n\u0001\n\u0001\n\u0005\n\u00dc\b\n\n\n\f\n\u00df\t\n\u0001\n\u0001\n"+
		"\u0001\n\u0001\n\u0001\n\u0001\n\u0001\n\u0001\n\u0005\n\u00e9\b\n\n\n"+
		"\f\n\u00ec\t\n\u0001\n\u0003\n\u00ef\b\n\u0001\n\u0001\n\u0003\n\u00f3"+
		"\b\n\u0001\u000b\u0001\u000b\u0001\u000b\u0001\u000b\u0001\f\u0001\f\u0001"+
		"\f\u0005\f\u00fc\b\f\n\f\f\f\u00ff\t\f\u0001\r\u0001\r\u0001\r\u0001\r"+
		"\u0001\r\u0001\u000e\u0001\u000e\u0001\u000e\u0005\u000e\u0109\b\u000e"+
		"\n\u000e\f\u000e\u010c\t\u000e\u0001\u000f\u0001\u000f\u0001\u000f\u0001"+
		"\u000f\u0001\u000f\u0003\u000f\u0113\b\u000f\u0001\u0010\u0001\u0010\u0001"+
		"\u0010\u0003\u0010\u0118\b\u0010\u0001\u0011\u0001\u0011\u0003\u0011\u011c"+
		"\b\u0011\u0001\u0011\u0001\u0011\u0001\u0012\u0001\u0012\u0001\u0012\u0005"+
		"\u0012\u0123\b\u0012\n\u0012\f\u0012\u0126\t\u0012\u0001\u0013\u0001\u0013"+
		"\u0001\u0013\u0001\u0013\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014"+
		"\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014"+
		"\u0005\u0014\u0136\b\u0014\n\u0014\f\u0014\u0139\t\u0014\u0001\u0014\u0001"+
		"\u0014\u0003\u0014\u013d\b\u0014\u0001\u0015\u0001\u0015\u0003\u0015\u0141"+
		"\b\u0015\u0001\u0016\u0001\u0016\u0001\u0016\u0001\u0016\u0001\u0017\u0001"+
		"\u0017\u0001\u0017\u0005\u0017\u014a\b\u0017\n\u0017\f\u0017\u014d\t\u0017"+
		"\u0001\u0018\u0001\u0018\u0001\u0018\u0003\u0018\u0152\b\u0018\u0001\u0018"+
		"\u0001\u0018\u0003\u0018\u0156\b\u0018\u0001\u0018\u0001\u0018\u0001\u0018"+
		"\u0001\u0018\u0001\u0018\u0001\u0018\u0001\u0019\u0001\u0019\u0001\u0019"+
		"\u0005\u0019\u0161\b\u0019\n\u0019\f\u0019\u0164\t\u0019\u0001\u001a\u0001"+
		"\u001a\u0001\u001a\u0001\u001a\u0001\u001b\u0001\u001b\u0001\u001b\u0001"+
		"\u001b\u0001\u001b\u0001\u001c\u0003\u001c\u0170\b\u001c\u0001\u001c\u0001"+
		"\u001c\u0001\u001c\u0001\u001c\u0003\u001c\u0176\b\u001c\u0001\u001d\u0001"+
		"\u001d\u0001\u001e\u0001\u001e\u0001\u001e\u0001\u001e\u0001\u001e\u0003"+
		"\u001e\u017f\b\u001e\u0001\u001e\u0001\u001e\u0001\u001e\u0003\u001e\u0184"+
		"\b\u001e\u0001\u001e\u0001\u001e\u0001\u001e\u0003\u001e\u0189\b\u001e"+
		"\u0001\u001f\u0001\u001f\u0001 \u0001 \u0001 \u0001 \u0005 \u0191\b \n"+
		" \f \u0194\t \u0001!\u0001!\u0001!\u0001!\u0003!\u019a\b!\u0001!\u0001"+
		"!\u0001\"\u0001\"\u0001\"\u0005\"\u01a1\b\"\n\"\f\"\u01a4\t\"\u0001\""+
		"\u0003\"\u01a7\b\"\u0001\"\u0003\"\u01aa\b\"\u0001\"\u0001\"\u0003\"\u01ae"+
		"\b\"\u0001#\u0001#\u0001#\u0003#\u01b3\b#\u0001#\u0001#\u0001#\u0001#"+
		"\u0001#\u0001#\u0001#\u0003#\u01bc\b#\u0001#\u0001#\u0001#\u0001#\u0001"+
		"#\u0003#\u01c3\b#\u0001#\u0001#\u0003#\u01c7\b#\u0003#\u01c9\b#\u0001"+
		"$\u0001$\u0001$\u0001%\u0001%\u0001%\u0001%\u0001%\u0003%\u01d3\b%\u0001"+
		"%\u0001%\u0003%\u01d7\b%\u0001&\u0001&\u0001\'\u0001\'\u0005\'\u01dd\b"+
		"\'\n\'\f\'\u01e0\t\'\u0001(\u0001(\u0001(\u0003(\u01e5\b(\u0001)\u0001"+
		")\u0001)\u0005)\u01ea\b)\n)\f)\u01ed\t)\u0001*\u0001*\u0001*\u0005*\u01f2"+
		"\b*\n*\f*\u01f5\t*\u0001+\u0001+\u0001+\u0005+\u01fa\b+\n+\f+\u01fd\t"+
		"+\u0001,\u0001,\u0001,\u0005,\u0202\b,\n,\f,\u0205\t,\u0001-\u0001-\u0001"+
		"-\u0005-\u020a\b-\n-\f-\u020d\t-\u0001.\u0001.\u0001.\u0005.\u0212\b."+
		"\n.\f.\u0215\t.\u0001/\u0001/\u0001/\u0001/\u0001/\u0001/\u0003/\u021d"+
		"\b/\u00010\u00010\u00010\u00030\u0222\b0\u00010\u00010\u00010\u00030\u0227"+
		"\b0\u00010\u00010\u00010\u00011\u00011\u00011\u00031\u022f\b1\u00011\u0001"+
		"1\u00011\u00031\u0234\b1\u00011\u00011\u00011\u00012\u00012\u00012\u0001"+
		"2\u00032\u023d\b2\u00012\u00012\u00012\u00012\u00012\u00012\u00032\u0245"+
		"\b2\u00013\u00013\u00013\u00013\u00013\u00014\u00014\u00014\u00034\u024f"+
		"\b4\u00014\u00014\u00034\u0253\b4\u00014\u00054\u0256\b4\n4\f4\u0259\t"+
		"4\u00014\u00034\u025c\b4\u00014\u00014\u00014\u00014\u00034\u0262\b4\u0001"+
		"4\u00014\u00034\u0266\b4\u00014\u00054\u0269\b4\n4\f4\u026c\t4\u00014"+
		"\u00034\u026f\b4\u00014\u00014\u00034\u0273\b4\u00014\u00054\u0276\b4"+
		"\n4\f4\u0279\t4\u00014\u00034\u027c\b4\u00034\u027e\b4\u00015\u00015\u0001"+
		"5\u00015\u00035\u0284\b5\u00015\u00015\u00015\u00016\u00016\u00056\u028b"+
		"\b6\n6\f6\u028e\t6\u00017\u00017\u00017\u00037\u0293\b7\u00017\u00017"+
		"\u00017\u00037\u0298\b7\u00018\u00018\u00038\u029c\b8\u00018\u00018\u0001"+
		"9\u00019\u00019\u00059\u02a3\b9\n9\f9\u02a6\t9\u0001:\u0001:\u0001:\u0001"+
		":\u0001;\u0001;\u0001;\u0001;\u0001;\u0001;\u0001;\u0001;\u0001;\u0001"+
		";\u0001;\u0001;\u0001;\u0001;\u0001;\u0001;\u0001;\u0001;\u0001;\u0005"+
		";\u02bf\b;\n;\f;\u02c2\t;\u0001;\u0001;\u0003;\u02c6\b;\u0001<\u0001<"+
		"\u0001<\u0005<\u02cb\b<\n<\f<\u02ce\t<\u0001=\u0001=\u0001=\u0001=\u0001"+
		"=\u0001=\u0001=\u0001=\u0001=\u0001=\u0001=\u0001=\u0001=\u0001=\u0001"+
		"=\u0001=\u0001=\u0005=\u02e1\b=\n=\f=\u02e4\t=\u0001=\u0001=\u0001=\u0001"+
		"=\u0003=\u02ea\b=\u0001=\u0001=\u0001=\u0001=\u0003=\u02f0\b=\u0001=\u0003"+
		"=\u02f3\b=\u0003=\u02f5\b=\u0001>\u0001>\u0001>\u0005>\u02fa\b>\n>\f>"+
		"\u02fd\t>\u0001?\u0001?\u0001?\u0005?\u0302\b?\n?\f?\u0305\t?\u0001@\u0001"+
		"@\u0001@\u0003@\u030a\b@\u0001A\u0001A\u0001A\u0005A\u030f\bA\nA\fA\u0312"+
		"\tA\u0001B\u0001B\u0001C\u0001C\u0001D\u0004D\u0319\bD\u000bD\fD\u031a"+
		"\u0001D\u0000\u0000E\u0000\u0002\u0004\u0006\b\n\f\u000e\u0010\u0012\u0014"+
		"\u0016\u0018\u001a\u001c\u001e \"$&(*,.02468:<>@BDFHJLNPRTVXZ\\^`bdfh"+
		"jlnprtvxz|~\u0080\u0082\u0084\u0086\u0088\u0000\b\u0001\u0000\r\u000f"+
		"\u0001\u0000$&\u0002\u0000\u0002\u0004;=\u0001\u000067\u0001\u00008:\u0002"+
		"\u0000\u001d\u001d77\u0003\u0000\u0018\u0019$&--\u0001\u0000\u001e!\u0352"+
		"\u0000\u008b\u0001\u0000\u0000\u0000\u0002\u009b\u0001\u0000\u0000\u0000"+
		"\u0004\u00a4\u0001\u0000\u0000\u0000\u0006\u00ac\u0001\u0000\u0000\u0000"+
		"\b\u00bb\u0001\u0000\u0000\u0000\n\u00be\u0001\u0000\u0000\u0000\f\u00c1"+
		"\u0001\u0000\u0000\u0000\u000e\u00c4\u0001\u0000\u0000\u0000\u0010\u00cd"+
		"\u0001\u0000\u0000\u0000\u0012\u00cf\u0001\u0000\u0000\u0000\u0014\u00f2"+
		"\u0001\u0000\u0000\u0000\u0016\u00f4\u0001\u0000\u0000\u0000\u0018\u00f8"+
		"\u0001\u0000\u0000\u0000\u001a\u0100\u0001\u0000\u0000\u0000\u001c\u0105"+
		"\u0001\u0000\u0000\u0000\u001e\u0112\u0001\u0000\u0000\u0000 \u0117\u0001"+
		"\u0000\u0000\u0000\"\u0119\u0001\u0000\u0000\u0000$\u011f\u0001\u0000"+
		"\u0000\u0000&\u0127\u0001\u0000\u0000\u0000(\u013c\u0001\u0000\u0000\u0000"+
		"*\u013e\u0001\u0000\u0000\u0000,\u0142\u0001\u0000\u0000\u0000.\u0146"+
		"\u0001\u0000\u0000\u00000\u014e\u0001\u0000\u0000\u00002\u015d\u0001\u0000"+
		"\u0000\u00004\u0165\u0001\u0000\u0000\u00006\u0169\u0001\u0000\u0000\u0000"+
		"8\u016f\u0001\u0000\u0000\u0000:\u0177\u0001\u0000\u0000\u0000<\u0188"+
		"\u0001\u0000\u0000\u0000>\u018a\u0001\u0000\u0000\u0000@\u018c\u0001\u0000"+
		"\u0000\u0000B\u0195\u0001\u0000\u0000\u0000D\u01ad\u0001\u0000\u0000\u0000"+
		"F\u01c8\u0001\u0000\u0000\u0000H\u01ca\u0001\u0000\u0000\u0000J\u01d6"+
		"\u0001\u0000\u0000\u0000L\u01d8\u0001\u0000\u0000\u0000N\u01da\u0001\u0000"+
		"\u0000\u0000P\u01e1\u0001\u0000\u0000\u0000R\u01e6\u0001\u0000\u0000\u0000"+
		"T\u01ee\u0001\u0000\u0000\u0000V\u01f6\u0001\u0000\u0000\u0000X\u01fe"+
		"\u0001\u0000\u0000\u0000Z\u0206\u0001\u0000\u0000\u0000\\\u020e\u0001"+
		"\u0000\u0000\u0000^\u021c\u0001\u0000\u0000\u0000`\u021e\u0001\u0000\u0000"+
		"\u0000b\u022b\u0001\u0000\u0000\u0000d\u0244\u0001\u0000\u0000\u0000f"+
		"\u0246\u0001\u0000\u0000\u0000h\u027d\u0001\u0000\u0000\u0000j\u027f\u0001"+
		"\u0000\u0000\u0000l\u0288\u0001\u0000\u0000\u0000n\u0297\u0001\u0000\u0000"+
		"\u0000p\u0299\u0001\u0000\u0000\u0000r\u029f\u0001\u0000\u0000\u0000t"+
		"\u02a7\u0001\u0000\u0000\u0000v\u02c5\u0001\u0000\u0000\u0000x\u02c7\u0001"+
		"\u0000\u0000\u0000z\u02f4\u0001\u0000\u0000\u0000|\u02f6\u0001\u0000\u0000"+
		"\u0000~\u02fe\u0001\u0000\u0000\u0000\u0080\u0306\u0001\u0000\u0000\u0000"+
		"\u0082\u030b\u0001\u0000\u0000\u0000\u0084\u0313\u0001\u0000\u0000\u0000"+
		"\u0086\u0315\u0001\u0000\u0000\u0000\u0088\u0318\u0001\u0000\u0000\u0000"+
		"\u008a\u008c\u0003\u0088D\u0000\u008b\u008a\u0001\u0000\u0000\u0000\u008b"+
		"\u008c\u0001\u0000\u0000\u0000\u008c\u008d\u0001\u0000\u0000\u0000\u008d"+
		"\u008e\u0003\b\u0004\u0000\u008e\u0090\u0003\n\u0005\u0000\u008f\u0091"+
		"\u0003\u0088D\u0000\u0090\u008f\u0001\u0000\u0000\u0000\u0090\u0091\u0001"+
		"\u0000\u0000\u0000\u0091\u0095\u0001\u0000\u0000\u0000\u0092\u0094\u0003"+
		"\u0006\u0003\u0000\u0093\u0092\u0001\u0000\u0000\u0000\u0094\u0097\u0001"+
		"\u0000\u0000\u0000\u0095\u0093\u0001\u0000\u0000\u0000\u0095\u0096\u0001"+
		"\u0000\u0000\u0000\u0096\u0098\u0001\u0000\u0000\u0000\u0097\u0095\u0001"+
		"\u0000\u0000\u0000\u0098\u0099\u0005\u0000\u0000\u0001\u0099\u0001\u0001"+
		"\u0000\u0000\u0000\u009a\u009c\u0003\u0088D\u0000\u009b\u009a\u0001\u0000"+
		"\u0000\u0000\u009b\u009c\u0001\u0000\u0000\u0000\u009c\u009d\u0001\u0000"+
		"\u0000\u0000\u009d\u009f\u0003L&\u0000\u009e\u00a0\u0003\u0088D\u0000"+
		"\u009f\u009e\u0001\u0000\u0000\u0000\u009f\u00a0\u0001\u0000\u0000\u0000"+
		"\u00a0\u00a1\u0001\u0000\u0000\u0000\u00a1\u00a2\u0005\u0000\u0000\u0001"+
		"\u00a2\u0003\u0001\u0000\u0000\u0000\u00a3\u00a5\u0003\u0088D\u0000\u00a4"+
		"\u00a3\u0001\u0000\u0000\u0000\u00a4\u00a5\u0001\u0000\u0000\u0000\u00a5"+
		"\u00a6\u0001\u0000\u0000\u0000\u00a6\u00a8\u0003 \u0010\u0000\u00a7\u00a9"+
		"\u0003\u0088D\u0000\u00a8\u00a7\u0001\u0000\u0000\u0000\u00a8\u00a9\u0001"+
		"\u0000\u0000\u0000\u00a9\u00aa\u0001\u0000\u0000\u0000\u00aa\u00ab\u0005"+
		"\u0000\u0000\u0001\u00ab\u0005\u0001\u0000\u0000\u0000\u00ac\u00b0\u0003"+
		"\b\u0004\u0000\u00ad\u00b1\u0003\f\u0006\u0000\u00ae\u00b1\u0003\u000e"+
		"\u0007\u0000\u00af\u00b1\u0003\u0010\b\u0000\u00b0\u00ad\u0001\u0000\u0000"+
		"\u0000\u00b0\u00ae\u0001\u0000\u0000\u0000\u00b0\u00af\u0001\u0000\u0000"+
		"\u0000\u00b1\u00b3\u0001\u0000\u0000\u0000\u00b2\u00b4\u0003\u0088D\u0000"+
		"\u00b3\u00b2\u0001\u0000\u0000\u0000\u00b3\u00b4\u0001\u0000\u0000\u0000"+
		"\u00b4\u0007\u0001\u0000\u0000\u0000\u00b5\u00b7\u0005\'\u0000\u0000\u00b6"+
		"\u00b8\u0003\u0088D\u0000\u00b7\u00b6\u0001\u0000\u0000\u0000\u00b7\u00b8"+
		"\u0001\u0000\u0000\u0000\u00b8\u00ba\u0001\u0000\u0000\u0000\u00b9\u00b5"+
		"\u0001\u0000\u0000\u0000\u00ba\u00bd\u0001\u0000\u0000\u0000\u00bb\u00b9"+
		"\u0001\u0000\u0000\u0000\u00bb\u00bc\u0001\u0000\u0000\u0000\u00bc\t\u0001"+
		"\u0000\u0000\u0000\u00bd\u00bb\u0001\u0000\u0000\u0000\u00be\u00bf\u0005"+
		"\u0005\u0000\u0000\u00bf\u00c0\u0003\u0082A\u0000\u00c0\u000b\u0001\u0000"+
		"\u0000\u0000\u00c1\u00c2\u0005\u0006\u0000\u0000\u00c2\u00c3\u0005(\u0000"+
		"\u0000\u00c3\r\u0001\u0000\u0000\u0000\u00c4\u00c5\u0005\u0007\u0000\u0000"+
		"\u00c5\u00c6\u0003\u0082A\u0000\u00c6\u000f\u0001\u0000\u0000\u0000\u00c7"+
		"\u00ce\u0003\u0012\t\u0000\u00c8\u00ce\u00030\u0018\u0000\u00c9\u00ce"+
		"\u00036\u001b\u0000\u00ca\u00ce\u00038\u001c\u0000\u00cb\u00ce\u0003@"+
		" \u0000\u00cc\u00ce\u0003B!\u0000\u00cd\u00c7\u0001\u0000\u0000\u0000"+
		"\u00cd\u00c8\u0001\u0000\u0000\u0000\u00cd\u00c9\u0001\u0000\u0000\u0000"+
		"\u00cd\u00ca\u0001\u0000\u0000\u0000\u00cd\u00cb\u0001\u0000\u0000\u0000"+
		"\u00cd\u00cc\u0001\u0000\u0000\u0000\u00ce\u0011\u0001\u0000\u0000\u0000"+
		"\u00cf\u00d0\u0005\b\u0000\u0000\u00d0\u00d2\u0003\u0084B\u0000\u00d1"+
		"\u00d3\u0003\u0016\u000b\u0000\u00d2\u00d1\u0001\u0000\u0000\u0000\u00d2"+
		"\u00d3\u0001\u0000\u0000\u0000\u00d3\u00d4\u0001\u0000\u0000\u0000\u00d4"+
		"\u00d5\u0005;\u0000\u0000\u00d5\u00d6\u0003\u0014\n\u0000\u00d6\u0013"+
		"\u0001\u0000\u0000\u0000\u00d7\u00f3\u0003\"\u0011\u0000\u00d8\u00dd\u0003"+
		"\u001a\r\u0000\u00d9\u00da\u00055\u0000\u0000\u00da\u00dc\u0003\u001a"+
		"\r\u0000\u00db\u00d9\u0001\u0000\u0000\u0000\u00dc\u00df\u0001\u0000\u0000"+
		"\u0000\u00dd\u00db\u0001\u0000\u0000\u0000\u00dd\u00de\u0001\u0000\u0000"+
		"\u0000\u00de\u00f3\u0001\u0000\u0000\u0000\u00df\u00dd\u0001\u0000\u0000"+
		"\u0000\u00e0\u00e1\u0005>\u0000\u0000\u00e1\u00e2\u0005A\u0000\u0000\u00e2"+
		"\u00e3\u00055\u0000\u0000\u00e3\u00ea\u0003\u001a\r\u0000\u00e4\u00e5"+
		"\u0003\u0088D\u0000\u00e5\u00e6\u00055\u0000\u0000\u00e6\u00e7\u0003\u001a"+
		"\r\u0000\u00e7\u00e9\u0001\u0000\u0000\u0000\u00e8\u00e4\u0001\u0000\u0000"+
		"\u0000\u00e9\u00ec\u0001\u0000\u0000\u0000\u00ea\u00e8\u0001\u0000\u0000"+
		"\u0000\u00ea\u00eb\u0001\u0000\u0000\u0000\u00eb\u00ee\u0001\u0000\u0000"+
		"\u0000\u00ec\u00ea\u0001\u0000\u0000\u0000\u00ed\u00ef\u0003\u0088D\u0000"+
		"\u00ee\u00ed\u0001\u0000\u0000\u0000\u00ee\u00ef\u0001\u0000\u0000\u0000"+
		"\u00ef\u00f0\u0001\u0000\u0000\u0000\u00f0\u00f1\u0005B\u0000\u0000\u00f1"+
		"\u00f3\u0001\u0000\u0000\u0000\u00f2\u00d7\u0001\u0000\u0000\u0000\u00f2"+
		"\u00d8\u0001\u0000\u0000\u0000\u00f2\u00e0\u0001\u0000\u0000\u0000\u00f3"+
		"\u0015\u0001\u0000\u0000\u0000\u00f4\u00f5\u0005<\u0000\u0000\u00f5\u00f6"+
		"\u0003\u0018\f\u0000\u00f6\u00f7\u0005=\u0000\u0000\u00f7\u0017\u0001"+
		"\u0000\u0000\u0000\u00f8\u00fd\u0003\u0084B\u0000\u00f9\u00fa\u00052\u0000"+
		"\u0000\u00fa\u00fc\u0003\u0084B\u0000\u00fb\u00f9\u0001\u0000\u0000\u0000"+
		"\u00fc\u00ff\u0001\u0000\u0000\u0000\u00fd\u00fb\u0001\u0000\u0000\u0000"+
		"\u00fd\u00fe\u0001\u0000\u0000\u0000\u00fe\u0019\u0001\u0000\u0000\u0000"+
		"\u00ff\u00fd\u0001\u0000\u0000\u0000\u0100\u0101\u0003\u0084B\u0000\u0101"+
		"\u0102\u0005.\u0000\u0000\u0102\u0103\u0003\u001c\u000e\u0000\u0103\u0104"+
		"\u0005/\u0000\u0000\u0104\u001b\u0001\u0000\u0000\u0000\u0105\u010a\u0003"+
		"\u001e\u000f\u0000\u0106\u0107\u00052\u0000\u0000\u0107\u0109\u0003\u001e"+
		"\u000f\u0000\u0108\u0106\u0001\u0000\u0000\u0000\u0109\u010c\u0001\u0000"+
		"\u0000\u0000\u010a\u0108\u0001\u0000\u0000\u0000\u010a\u010b\u0001\u0000"+
		"\u0000\u0000\u010b\u001d\u0001\u0000\u0000\u0000\u010c\u010a\u0001\u0000"+
		"\u0000\u0000\u010d\u010e\u0003\u0084B\u0000\u010e\u010f\u00053\u0000\u0000"+
		"\u010f\u0110\u0003 \u0010\u0000\u0110\u0113\u0001\u0000\u0000\u0000\u0111"+
		"\u0113\u0003 \u0010\u0000\u0112\u010d\u0001\u0000\u0000\u0000\u0112\u0111"+
		"\u0001\u0000\u0000\u0000\u0113\u001f\u0001\u0000\u0000\u0000\u0114\u0118"+
		"\u0003\"\u0011\u0000\u0115\u0118\u0003(\u0014\u0000\u0116\u0118\u0003"+
		"*\u0015\u0000\u0117\u0114\u0001\u0000\u0000\u0000\u0117\u0115\u0001\u0000"+
		"\u0000\u0000\u0117\u0116\u0001\u0000\u0000\u0000\u0118!\u0001\u0000\u0000"+
		"\u0000\u0119\u011b\u00050\u0000\u0000\u011a\u011c\u0003$\u0012\u0000\u011b"+
		"\u011a\u0001\u0000\u0000\u0000\u011b\u011c\u0001\u0000\u0000\u0000\u011c"+
		"\u011d\u0001\u0000\u0000\u0000\u011d\u011e\u00051\u0000\u0000\u011e#\u0001"+
		"\u0000\u0000\u0000\u011f\u0124\u0003&\u0013\u0000\u0120\u0121\u00052\u0000"+
		"\u0000\u0121\u0123\u0003&\u0013\u0000\u0122\u0120\u0001\u0000\u0000\u0000"+
		"\u0123\u0126\u0001\u0000\u0000\u0000\u0124\u0122\u0001\u0000\u0000\u0000"+
		"\u0124\u0125\u0001\u0000\u0000\u0000\u0125%\u0001\u0000\u0000\u0000\u0126"+
		"\u0124\u0001\u0000\u0000\u0000\u0127\u0128\u0003\u0084B\u0000\u0128\u0129"+
		"\u00053\u0000\u0000\u0129\u012a\u0003 \u0010\u0000\u012a\'\u0001\u0000"+
		"\u0000\u0000\u012b\u012c\u0005.\u0000\u0000\u012c\u012d\u0003 \u0010\u0000"+
		"\u012d\u012e\u0005/\u0000\u0000\u012e\u013d\u0001\u0000\u0000\u0000\u012f"+
		"\u0130\u0005.\u0000\u0000\u0130\u0131\u0003 \u0010\u0000\u0131\u0132\u0005"+
		"2\u0000\u0000\u0132\u0137\u0003 \u0010\u0000\u0133\u0134\u00052\u0000"+
		"\u0000\u0134\u0136\u0003 \u0010\u0000\u0135\u0133\u0001\u0000\u0000\u0000"+
		"\u0136\u0139\u0001\u0000\u0000\u0000\u0137\u0135\u0001\u0000\u0000\u0000"+
		"\u0137\u0138\u0001\u0000\u0000\u0000\u0138\u013a\u0001\u0000\u0000\u0000"+
		"\u0139\u0137\u0001\u0000\u0000\u0000\u013a\u013b\u0005/\u0000\u0000\u013b"+
		"\u013d\u0001\u0000\u0000\u0000\u013c\u012b\u0001\u0000\u0000\u0000\u013c"+
		"\u012f\u0001\u0000\u0000\u0000\u013d)\u0001\u0000\u0000\u0000\u013e\u0140"+
		"\u0003\u0082A\u0000\u013f\u0141\u0003,\u0016\u0000\u0140\u013f\u0001\u0000"+
		"\u0000\u0000\u0140\u0141\u0001\u0000\u0000\u0000\u0141+\u0001\u0000\u0000"+
		"\u0000\u0142\u0143\u0005<\u0000\u0000\u0143\u0144\u0003.\u0017\u0000\u0144"+
		"\u0145\u0005=\u0000\u0000\u0145-\u0001\u0000\u0000\u0000\u0146\u014b\u0003"+
		" \u0010\u0000\u0147\u0148\u00052\u0000\u0000\u0148\u014a\u0003 \u0010"+
		"\u0000\u0149\u0147\u0001\u0000\u0000\u0000\u014a\u014d\u0001\u0000\u0000"+
		"\u0000\u014b\u0149\u0001\u0000\u0000\u0000\u014b\u014c\u0001\u0000\u0000"+
		"\u0000\u014c/\u0001\u0000\u0000\u0000\u014d\u014b\u0001\u0000\u0000\u0000"+
		"\u014e\u014f\u0005\n\u0000\u0000\u014f\u0151\u0003\u0084B\u0000\u0150"+
		"\u0152\u0003\u0016\u000b\u0000\u0151\u0150\u0001\u0000\u0000\u0000\u0151"+
		"\u0152\u0001\u0000\u0000\u0000\u0152\u0153\u0001\u0000\u0000\u0000\u0153"+
		"\u0155\u0005.\u0000\u0000\u0154\u0156\u00032\u0019\u0000\u0155\u0154\u0001"+
		"\u0000\u0000\u0000\u0155\u0156\u0001\u0000\u0000\u0000\u0156\u0157\u0001"+
		"\u0000\u0000\u0000\u0157\u0158\u0005/\u0000\u0000\u0158\u0159\u0005\u0001"+
		"\u0000\u0000\u0159\u015a\u0003 \u0010\u0000\u015a\u015b\u00053\u0000\u0000"+
		"\u015b\u015c\u0003D\"\u0000\u015c1\u0001\u0000\u0000\u0000\u015d\u0162"+
		"\u00034\u001a\u0000\u015e\u015f\u00052\u0000\u0000\u015f\u0161\u00034"+
		"\u001a\u0000\u0160\u015e\u0001\u0000\u0000\u0000\u0161\u0164\u0001\u0000"+
		"\u0000\u0000\u0162\u0160\u0001\u0000\u0000\u0000\u0162\u0163\u0001\u0000"+
		"\u0000\u0000\u01633\u0001\u0000\u0000\u0000\u0164\u0162\u0001\u0000\u0000"+
		"\u0000\u0165\u0166\u0003z=\u0000\u0166\u0167\u00053\u0000\u0000\u0167"+
		"\u0168\u0003 \u0010\u0000\u01685\u0001\u0000\u0000\u0000\u0169\u016a\u0005"+
		"\u000b\u0000\u0000\u016a\u016b\u0003\u0084B\u0000\u016b\u016c\u00053\u0000"+
		"\u0000\u016c\u016d\u0003 \u0010\u0000\u016d7\u0001\u0000\u0000\u0000\u016e"+
		"\u0170\u0003:\u001d\u0000\u016f\u016e\u0001\u0000\u0000\u0000\u016f\u0170"+
		"\u0001\u0000\u0000\u0000\u0170\u0171\u0001\u0000\u0000\u0000\u0171\u0172"+
		"\u0005\f\u0000\u0000\u0172\u0175\u0003<\u001e\u0000\u0173\u0174\u0005"+
		"#\u0000\u0000\u0174\u0176\u0003L&\u0000\u0175\u0173\u0001\u0000\u0000"+
		"\u0000\u0175\u0176\u0001\u0000\u0000\u0000\u01769\u0001\u0000\u0000\u0000"+
		"\u0177\u0178\u0007\u0000\u0000\u0000\u0178;\u0001\u0000\u0000\u0000\u0179"+
		"\u017a\u0003>\u001f\u0000\u017a\u017b\u00053\u0000\u0000\u017b\u017c\u0003"+
		"D\"\u0000\u017c\u0189\u0001\u0000\u0000\u0000\u017d\u017f\u0003>\u001f"+
		"\u0000\u017e\u017d\u0001\u0000\u0000\u0000\u017e\u017f\u0001\u0000\u0000"+
		"\u0000\u017f\u0180\u0001\u0000\u0000\u0000\u0180\u0183\u0003\u0082A\u0000"+
		"\u0181\u0182\u0005\u0016\u0000\u0000\u0182\u0184\u0003L&\u0000\u0183\u0181"+
		"\u0001\u0000\u0000\u0000\u0183\u0184\u0001\u0000\u0000\u0000\u0184\u0185"+
		"\u0001\u0000\u0000\u0000\u0185\u0186\u00053\u0000\u0000\u0186\u0187\u0003"+
		"D\"\u0000\u0187\u0189\u0001\u0000\u0000\u0000\u0188\u0179\u0001\u0000"+
		"\u0000\u0000\u0188\u017e\u0001\u0000\u0000\u0000\u0189=\u0001\u0000\u0000"+
		"\u0000\u018a\u018b\u0007\u0001\u0000\u0000\u018b?\u0001\u0000\u0000\u0000"+
		"\u018c\u018d\u0005\u0010\u0000\u0000\u018d\u0192\u0003\u0082A\u0000\u018e"+
		"\u018f\u0005=\u0000\u0000\u018f\u0191\u0003\u0082A\u0000\u0190\u018e\u0001"+
		"\u0000\u0000\u0000\u0191\u0194\u0001\u0000\u0000\u0000\u0192\u0190\u0001"+
		"\u0000\u0000\u0000\u0192\u0193\u0001\u0000\u0000\u0000\u0193A\u0001\u0000"+
		"\u0000\u0000\u0194\u0192\u0001\u0000\u0000\u0000\u0195\u0199\u0005\u0011"+
		"\u0000\u0000\u0196\u0197\u0003\u0084B\u0000\u0197\u0198\u0005;\u0000\u0000"+
		"\u0198\u019a\u0001\u0000\u0000\u0000\u0199\u0196\u0001\u0000\u0000\u0000"+
		"\u0199\u019a\u0001\u0000\u0000\u0000\u019a\u019b\u0001\u0000\u0000\u0000"+
		"\u019b\u019c\u0003L&\u0000\u019cC\u0001\u0000\u0000\u0000\u019d\u019e"+
		"\u0005>\u0000\u0000\u019e\u01a2\u0005A\u0000\u0000\u019f\u01a1\u0003F"+
		"#\u0000\u01a0\u019f\u0001\u0000\u0000\u0000\u01a1\u01a4\u0001\u0000\u0000"+
		"\u0000\u01a2\u01a0\u0001\u0000\u0000\u0000\u01a2\u01a3\u0001\u0000\u0000"+
		"\u0000\u01a3\u01a6\u0001\u0000\u0000\u0000\u01a4\u01a2\u0001\u0000\u0000"+
		"\u0000\u01a5\u01a7\u0003L&\u0000\u01a6\u01a5\u0001\u0000\u0000\u0000\u01a6"+
		"\u01a7\u0001\u0000\u0000\u0000\u01a7\u01a9\u0001\u0000\u0000\u0000\u01a8"+
		"\u01aa\u0003\u0088D\u0000\u01a9\u01a8\u0001\u0000\u0000\u0000\u01a9\u01aa"+
		"\u0001\u0000\u0000\u0000\u01aa\u01ab\u0001\u0000\u0000\u0000\u01ab\u01ae"+
		"\u0005B\u0000\u0000\u01ac\u01ae\u0003L&\u0000\u01ad\u019d\u0001\u0000"+
		"\u0000\u0000\u01ad\u01ac\u0001\u0000\u0000\u0000\u01aeE\u0001\u0000\u0000"+
		"\u0000\u01af\u01b0\u0005\t\u0000\u0000\u01b0\u01b2\u0003z=\u0000\u01b1"+
		"\u01b3\u0003H$\u0000\u01b2\u01b1\u0001\u0000\u0000\u0000\u01b2\u01b3\u0001"+
		"\u0000\u0000\u0000\u01b3\u01b4\u0001\u0000\u0000\u0000\u01b4\u01b5\u0005"+
		";\u0000\u0000\u01b5\u01b6\u0003L&\u0000\u01b6\u01b7\u0003\u0088D\u0000"+
		"\u01b7\u01c9\u0001\u0000\u0000\u0000\u01b8\u01b9\u0005\t\u0000\u0000\u01b9"+
		"\u01bb\u0003z=\u0000\u01ba\u01bc\u0003H$\u0000\u01bb\u01ba\u0001\u0000"+
		"\u0000\u0000\u01bb\u01bc\u0001\u0000\u0000\u0000\u01bc\u01bd\u0001\u0000"+
		"\u0000\u0000\u01bd\u01be\u0005;\u0000\u0000\u01be\u01bf\u0005>\u0000\u0000"+
		"\u01bf\u01c0\u0005A\u0000\u0000\u01c0\u01c2\u0003L&\u0000\u01c1\u01c3"+
		"\u0003\u0088D\u0000\u01c2\u01c1\u0001\u0000\u0000\u0000\u01c2\u01c3\u0001"+
		"\u0000\u0000\u0000\u01c3\u01c4\u0001\u0000\u0000\u0000\u01c4\u01c6\u0005"+
		"B\u0000\u0000\u01c5\u01c7\u0003\u0088D\u0000\u01c6\u01c5\u0001\u0000\u0000"+
		"\u0000\u01c6\u01c7\u0001\u0000\u0000\u0000\u01c7\u01c9\u0001\u0000\u0000"+
		"\u0000\u01c8\u01af\u0001\u0000\u0000\u0000\u01c8\u01b8\u0001\u0000\u0000"+
		"\u0000\u01c9G\u0001\u0000\u0000\u0000\u01ca\u01cb\u00053\u0000\u0000\u01cb"+
		"\u01cc\u0003 \u0010\u0000\u01ccI\u0001\u0000\u0000\u0000\u01cd\u01d7\u0003"+
		"L&\u0000\u01ce\u01cf\u0005>\u0000\u0000\u01cf\u01d0\u0005A\u0000\u0000"+
		"\u01d0\u01d2\u0003L&\u0000\u01d1\u01d3\u0003\u0088D\u0000\u01d2\u01d1"+
		"\u0001\u0000\u0000\u0000\u01d2\u01d3\u0001\u0000\u0000\u0000\u01d3\u01d4"+
		"\u0001\u0000\u0000\u0000\u01d4\u01d5\u0005B\u0000\u0000\u01d5\u01d7\u0001"+
		"\u0000\u0000\u0000\u01d6\u01cd\u0001\u0000\u0000\u0000\u01d6\u01ce\u0001"+
		"\u0000\u0000\u0000\u01d7K\u0001\u0000\u0000\u0000\u01d8\u01d9\u0003N\'"+
		"\u0000\u01d9M\u0001\u0000\u0000\u0000\u01da\u01de\u0003P(\u0000\u01db"+
		"\u01dd\u0003\u0086C\u0000\u01dc\u01db\u0001\u0000\u0000\u0000\u01dd\u01e0"+
		"\u0001\u0000\u0000\u0000\u01de\u01dc\u0001\u0000\u0000\u0000\u01de\u01df"+
		"\u0001\u0000\u0000\u0000\u01dfO\u0001\u0000\u0000\u0000\u01e0\u01de\u0001"+
		"\u0000\u0000\u0000\u01e1\u01e4\u0003R)\u0000\u01e2\u01e3\u0005\u001c\u0000"+
		"\u0000\u01e3\u01e5\u0003P(\u0000\u01e4\u01e2\u0001\u0000\u0000\u0000\u01e4"+
		"\u01e5\u0001\u0000\u0000\u0000\u01e5Q\u0001\u0000\u0000\u0000\u01e6\u01eb"+
		"\u0003T*\u0000\u01e7\u01e8\u0005\u001b\u0000\u0000\u01e8\u01ea\u0003T"+
		"*\u0000\u01e9\u01e7\u0001\u0000\u0000\u0000\u01ea\u01ed\u0001\u0000\u0000"+
		"\u0000\u01eb\u01e9\u0001\u0000\u0000\u0000\u01eb\u01ec\u0001\u0000\u0000"+
		"\u0000\u01ecS\u0001\u0000\u0000\u0000\u01ed\u01eb\u0001\u0000\u0000\u0000"+
		"\u01ee\u01f3\u0003V+\u0000\u01ef\u01f0\u0005\u001a\u0000\u0000\u01f0\u01f2"+
		"\u0003V+\u0000\u01f1\u01ef\u0001\u0000\u0000\u0000\u01f2\u01f5\u0001\u0000"+
		"\u0000\u0000\u01f3\u01f1\u0001\u0000\u0000\u0000\u01f3\u01f4\u0001\u0000"+
		"\u0000\u0000\u01f4U\u0001\u0000\u0000\u0000\u01f5\u01f3\u0001\u0000\u0000"+
		"\u0000\u01f6\u01fb\u0003X,\u0000\u01f7\u01f8\u0005\"\u0000\u0000\u01f8"+
		"\u01fa\u0003X,\u0000\u01f9\u01f7\u0001\u0000\u0000\u0000\u01fa\u01fd\u0001"+
		"\u0000\u0000\u0000\u01fb\u01f9\u0001\u0000\u0000\u0000\u01fb\u01fc\u0001"+
		"\u0000\u0000\u0000\u01fcW\u0001\u0000\u0000\u0000\u01fd\u01fb\u0001\u0000"+
		"\u0000\u0000\u01fe\u0203\u0003Z-\u0000\u01ff\u0200\u0007\u0002\u0000\u0000"+
		"\u0200\u0202\u0003Z-\u0000\u0201\u01ff\u0001\u0000\u0000\u0000\u0202\u0205"+
		"\u0001\u0000\u0000\u0000\u0203\u0201\u0001\u0000\u0000\u0000\u0203\u0204"+
		"\u0001\u0000\u0000\u0000\u0204Y\u0001\u0000\u0000\u0000\u0205\u0203\u0001"+
		"\u0000\u0000\u0000\u0206\u020b\u0003\\.\u0000\u0207\u0208\u0007\u0003"+
		"\u0000\u0000\u0208\u020a\u0003\\.\u0000\u0209\u0207\u0001\u0000\u0000"+
		"\u0000\u020a\u020d\u0001\u0000\u0000\u0000\u020b\u0209\u0001\u0000\u0000"+
		"\u0000\u020b\u020c\u0001\u0000\u0000\u0000\u020c[\u0001\u0000\u0000\u0000"+
		"\u020d\u020b\u0001\u0000\u0000\u0000\u020e\u0213\u0003^/\u0000\u020f\u0210"+
		"\u0007\u0004\u0000\u0000\u0210\u0212\u0003^/\u0000\u0211\u020f\u0001\u0000"+
		"\u0000\u0000\u0212\u0215\u0001\u0000\u0000\u0000\u0213\u0211\u0001\u0000"+
		"\u0000\u0000\u0213\u0214\u0001\u0000\u0000\u0000\u0214]\u0001\u0000\u0000"+
		"\u0000\u0215\u0213\u0001\u0000\u0000\u0000\u0216\u021d\u0003`0\u0000\u0217"+
		"\u021d\u0003b1\u0000\u0218\u021d\u0003f3\u0000\u0219\u021a\u0007\u0005"+
		"\u0000\u0000\u021a\u021d\u0003^/\u0000\u021b\u021d\u0003l6\u0000\u021c"+
		"\u0216\u0001\u0000\u0000\u0000\u021c\u0217\u0001\u0000\u0000\u0000\u021c"+
		"\u0218\u0001\u0000\u0000\u0000\u021c\u0219\u0001\u0000\u0000\u0000\u021c"+
		"\u021b\u0001\u0000\u0000\u0000\u021d_\u0001\u0000\u0000\u0000\u021e\u021f"+
		"\u0005\u0012\u0000\u0000\u021f\u0221\u0003L&\u0000\u0220\u0222\u0003\u0088"+
		"D\u0000\u0221\u0220\u0001\u0000\u0000\u0000\u0221\u0222\u0001\u0000\u0000"+
		"\u0000\u0222\u0223\u0001\u0000\u0000\u0000\u0223\u0224\u0005\u0013\u0000"+
		"\u0000\u0224\u0226\u0003L&\u0000\u0225\u0227\u0003\u0088D\u0000\u0226"+
		"\u0225\u0001\u0000\u0000\u0000\u0226\u0227\u0001\u0000\u0000\u0000\u0227"+
		"\u0228\u0001\u0000\u0000\u0000\u0228\u0229\u0005\u0014\u0000\u0000\u0229"+
		"\u022a\u0003L&\u0000\u022aa\u0001\u0000\u0000\u0000\u022b\u022c\u0005"+
		"\t\u0000\u0000\u022c\u022e\u0003z=\u0000\u022d\u022f\u0003H$\u0000\u022e"+
		"\u022d\u0001\u0000\u0000\u0000\u022e\u022f\u0001\u0000\u0000\u0000\u022f"+
		"\u0230\u0001\u0000\u0000\u0000\u0230\u0231\u0005;\u0000\u0000\u0231\u0233"+
		"\u0003J%\u0000\u0232\u0234\u0003\u0088D\u0000\u0233\u0232\u0001\u0000"+
		"\u0000\u0000\u0233\u0234\u0001\u0000\u0000\u0000\u0234\u0235\u0001\u0000"+
		"\u0000\u0000\u0235\u0236\u0005\u0017\u0000\u0000\u0236\u0237\u0003d2\u0000"+
		"\u0237c\u0001\u0000\u0000\u0000\u0238\u0239\u0005>\u0000\u0000\u0239\u023a"+
		"\u0005A\u0000\u0000\u023a\u023c\u0003L&\u0000\u023b\u023d\u0003\u0088"+
		"D\u0000\u023c\u023b\u0001\u0000\u0000\u0000\u023c\u023d\u0001\u0000\u0000"+
		"\u0000\u023d\u023e\u0001\u0000\u0000\u0000\u023e\u023f\u0005B\u0000\u0000"+
		"\u023f\u0245\u0001\u0000\u0000\u0000\u0240\u0241\u0003\u0088D\u0000\u0241"+
		"\u0242\u0003L&\u0000\u0242\u0245\u0001\u0000\u0000\u0000\u0243\u0245\u0003"+
		"L&\u0000\u0244\u0238\u0001\u0000\u0000\u0000\u0244\u0240\u0001\u0000\u0000"+
		"\u0000\u0244\u0243\u0001\u0000\u0000\u0000\u0245e\u0001\u0000\u0000\u0000"+
		"\u0246\u0247\u0005\u0015\u0000\u0000\u0247\u0248\u0003L&\u0000\u0248\u0249"+
		"\u00053\u0000\u0000\u0249\u024a\u0003h4\u0000\u024ag\u0001\u0000\u0000"+
		"\u0000\u024b\u024c\u0005>\u0000\u0000\u024c\u024e\u0005A\u0000\u0000\u024d"+
		"\u024f\u0003\u0088D\u0000\u024e\u024d\u0001\u0000\u0000\u0000\u024e\u024f"+
		"\u0001\u0000\u0000\u0000\u024f\u0250\u0001\u0000\u0000\u0000\u0250\u0257"+
		"\u0003j5\u0000\u0251\u0253\u0003\u0088D\u0000\u0252\u0251\u0001\u0000"+
		"\u0000\u0000\u0252\u0253\u0001\u0000\u0000\u0000\u0253\u0254\u0001\u0000"+
		"\u0000\u0000\u0254\u0256\u0003j5\u0000\u0255\u0252\u0001\u0000\u0000\u0000"+
		"\u0256\u0259\u0001\u0000\u0000\u0000\u0257\u0255\u0001\u0000\u0000\u0000"+
		"\u0257\u0258\u0001\u0000\u0000\u0000\u0258\u025b\u0001\u0000\u0000\u0000"+
		"\u0259\u0257\u0001\u0000\u0000\u0000\u025a\u025c\u0003\u0088D\u0000\u025b"+
		"\u025a\u0001\u0000\u0000\u0000\u025b\u025c\u0001\u0000\u0000\u0000\u025c"+
		"\u025d\u0001\u0000\u0000\u0000\u025d\u025e\u0005B\u0000\u0000\u025e\u027e"+
		"\u0001\u0000\u0000\u0000\u025f\u0261\u0005>\u0000\u0000\u0260\u0262\u0003"+
		"\u0088D\u0000\u0261\u0260\u0001\u0000\u0000\u0000\u0261\u0262\u0001\u0000"+
		"\u0000\u0000\u0262\u0263\u0001\u0000\u0000\u0000\u0263\u026a\u0003j5\u0000"+
		"\u0264\u0266\u0003\u0088D\u0000\u0265\u0264\u0001\u0000\u0000\u0000\u0265"+
		"\u0266\u0001\u0000\u0000\u0000\u0266\u0267\u0001\u0000\u0000\u0000\u0267"+
		"\u0269\u0003j5\u0000\u0268\u0265\u0001\u0000\u0000\u0000\u0269\u026c\u0001"+
		"\u0000\u0000\u0000\u026a\u0268\u0001\u0000\u0000\u0000\u026a\u026b\u0001"+
		"\u0000\u0000\u0000\u026b\u026e\u0001\u0000\u0000\u0000\u026c\u026a\u0001"+
		"\u0000\u0000\u0000\u026d\u026f\u0003\u0088D\u0000\u026e\u026d\u0001\u0000"+
		"\u0000\u0000\u026e\u026f\u0001\u0000\u0000\u0000\u026f\u027e\u0001\u0000"+
		"\u0000\u0000\u0270\u0277\u0003j5\u0000\u0271\u0273\u0003\u0088D\u0000"+
		"\u0272\u0271\u0001\u0000\u0000\u0000\u0272\u0273\u0001\u0000\u0000\u0000"+
		"\u0273\u0274\u0001\u0000\u0000\u0000\u0274\u0276\u0003j5\u0000\u0275\u0272"+
		"\u0001\u0000\u0000\u0000\u0276\u0279\u0001\u0000\u0000\u0000\u0277\u0275"+
		"\u0001\u0000\u0000\u0000\u0277\u0278\u0001\u0000\u0000\u0000\u0278\u027b"+
		"\u0001\u0000\u0000\u0000\u0279\u0277\u0001\u0000\u0000\u0000\u027a\u027c"+
		"\u0003\u0088D\u0000\u027b\u027a\u0001\u0000\u0000\u0000\u027b\u027c\u0001"+
		"\u0000\u0000\u0000\u027c\u027e\u0001\u0000\u0000\u0000\u027d\u024b\u0001"+
		"\u0000\u0000\u0000\u027d\u025f\u0001\u0000\u0000\u0000\u027d\u0270\u0001"+
		"\u0000\u0000\u0000\u027ei\u0001\u0000\u0000\u0000\u027f\u0280\u00055\u0000"+
		"\u0000\u0280\u0283\u0003z=\u0000\u0281\u0282\u0005\u0016\u0000\u0000\u0282"+
		"\u0284\u0003L&\u0000\u0283\u0281\u0001\u0000\u0000\u0000\u0283\u0284\u0001"+
		"\u0000\u0000\u0000\u0284\u0285\u0001\u0000\u0000\u0000\u0285\u0286\u0005"+
		"3\u0000\u0000\u0286\u0287\u0003D\"\u0000\u0287k\u0001\u0000\u0000\u0000"+
		"\u0288\u028c\u0003v;\u0000\u0289\u028b\u0003n7\u0000\u028a\u0289\u0001"+
		"\u0000\u0000\u0000\u028b\u028e\u0001\u0000\u0000\u0000\u028c\u028a\u0001"+
		"\u0000\u0000\u0000\u028c\u028d\u0001\u0000\u0000\u0000\u028dm\u0001\u0000"+
		"\u0000\u0000\u028e\u028c\u0001\u0000\u0000\u0000\u028f\u0298\u0003p8\u0000"+
		"\u0290\u0292\u0005.\u0000\u0000\u0291\u0293\u0003x<\u0000\u0292\u0291"+
		"\u0001\u0000\u0000\u0000\u0292\u0293\u0001\u0000\u0000\u0000\u0293\u0294"+
		"\u0001\u0000\u0000\u0000\u0294\u0298\u0005/\u0000\u0000\u0295\u0296\u0005"+
		"4\u0000\u0000\u0296\u0298\u0003\u0084B\u0000\u0297\u028f\u0001\u0000\u0000"+
		"\u0000\u0297\u0290\u0001\u0000\u0000\u0000\u0297\u0295\u0001\u0000\u0000"+
		"\u0000\u0298o\u0001\u0000\u0000\u0000\u0299\u029b\u00050\u0000\u0000\u029a"+
		"\u029c\u0003r9\u0000\u029b\u029a\u0001\u0000\u0000\u0000\u029b\u029c\u0001"+
		"\u0000\u0000\u0000\u029c\u029d\u0001\u0000\u0000\u0000\u029d\u029e\u0005"+
		"1\u0000\u0000\u029eq\u0001\u0000\u0000\u0000\u029f\u02a4\u0003t:\u0000"+
		"\u02a0\u02a1\u00052\u0000\u0000\u02a1\u02a3\u0003t:\u0000\u02a2\u02a0"+
		"\u0001\u0000\u0000\u0000\u02a3\u02a6\u0001\u0000\u0000\u0000\u02a4\u02a2"+
		"\u0001\u0000\u0000\u0000\u02a4\u02a5\u0001\u0000\u0000\u0000\u02a5s\u0001"+
		"\u0000\u0000\u0000\u02a6\u02a4\u0001\u0000\u0000\u0000\u02a7\u02a8\u0003"+
		"\u0084B\u0000\u02a8\u02a9\u0005;\u0000\u0000\u02a9\u02aa\u0003L&\u0000"+
		"\u02aau\u0001\u0000\u0000\u0000\u02ab\u02c6\u0005(\u0000\u0000\u02ac\u02c6"+
		"\u0005+\u0000\u0000\u02ad\u02c6\u0005*\u0000\u0000\u02ae\u02c6\u0005)"+
		"\u0000\u0000\u02af\u02c6\u0005\u0018\u0000\u0000\u02b0\u02c6\u0005\u0019"+
		"\u0000\u0000\u02b1\u02c6\u0003\u0082A\u0000\u02b2\u02b3\u0005.\u0000\u0000"+
		"\u02b3\u02c6\u0005/\u0000\u0000\u02b4\u02b5\u0005.\u0000\u0000\u02b5\u02b6"+
		"\u0003L&\u0000\u02b6\u02b7\u0005/\u0000\u0000\u02b7\u02c6\u0001\u0000"+
		"\u0000\u0000\u02b8\u02b9\u0005.\u0000\u0000\u02b9\u02ba\u0003L&\u0000"+
		"\u02ba\u02bb\u00052\u0000\u0000\u02bb\u02c0\u0003L&\u0000\u02bc\u02bd"+
		"\u00052\u0000\u0000\u02bd\u02bf\u0003L&\u0000\u02be\u02bc\u0001\u0000"+
		"\u0000\u0000\u02bf\u02c2\u0001\u0000\u0000\u0000\u02c0\u02be\u0001\u0000"+
		"\u0000\u0000\u02c0\u02c1\u0001\u0000\u0000\u0000\u02c1\u02c3\u0001\u0000"+
		"\u0000\u0000\u02c2\u02c0\u0001\u0000\u0000\u0000\u02c3\u02c4\u0005/\u0000"+
		"\u0000\u02c4\u02c6\u0001\u0000\u0000\u0000\u02c5\u02ab\u0001\u0000\u0000"+
		"\u0000\u02c5\u02ac\u0001\u0000\u0000\u0000\u02c5\u02ad\u0001\u0000\u0000"+
		"\u0000\u02c5\u02ae\u0001\u0000\u0000\u0000\u02c5\u02af\u0001\u0000\u0000"+
		"\u0000\u02c5\u02b0\u0001\u0000\u0000\u0000\u02c5\u02b1\u0001\u0000\u0000"+
		"\u0000\u02c5\u02b2\u0001\u0000\u0000\u0000\u02c5\u02b4\u0001\u0000\u0000"+
		"\u0000\u02c5\u02b8\u0001\u0000\u0000\u0000\u02c6w\u0001\u0000\u0000\u0000"+
		"\u02c7\u02cc\u0003L&\u0000\u02c8\u02c9\u00052\u0000\u0000\u02c9\u02cb"+
		"\u0003L&\u0000\u02ca\u02c8\u0001\u0000\u0000\u0000\u02cb\u02ce\u0001\u0000"+
		"\u0000\u0000\u02cc\u02ca\u0001\u0000\u0000\u0000\u02cc\u02cd\u0001\u0000"+
		"\u0000\u0000\u02cdy\u0001\u0000\u0000\u0000\u02ce\u02cc\u0001\u0000\u0000"+
		"\u0000\u02cf\u02f5\u0005,\u0000\u0000\u02d0\u02f5\u0005(\u0000\u0000\u02d1"+
		"\u02f5\u0005+\u0000\u0000\u02d2\u02f5\u0005*\u0000\u0000\u02d3\u02f5\u0005"+
		")\u0000\u0000\u02d4\u02d5\u0005.\u0000\u0000\u02d5\u02f5\u0005/\u0000"+
		"\u0000\u02d6\u02d7\u0005.\u0000\u0000\u02d7\u02d8\u0003z=\u0000\u02d8"+
		"\u02d9\u0005/\u0000\u0000\u02d9\u02f5\u0001\u0000\u0000\u0000\u02da\u02db"+
		"\u0005.\u0000\u0000\u02db\u02dc\u0003z=\u0000\u02dc\u02dd\u00052\u0000"+
		"\u0000\u02dd\u02e2\u0003z=\u0000\u02de\u02df\u00052\u0000\u0000\u02df"+
		"\u02e1\u0003z=\u0000\u02e0\u02de\u0001\u0000\u0000\u0000\u02e1\u02e4\u0001"+
		"\u0000\u0000\u0000\u02e2\u02e0\u0001\u0000\u0000\u0000\u02e2\u02e3\u0001"+
		"\u0000\u0000\u0000\u02e3\u02e5\u0001\u0000\u0000\u0000\u02e4\u02e2\u0001"+
		"\u0000\u0000\u0000\u02e5\u02e6\u0005/\u0000\u0000\u02e6\u02f5\u0001\u0000"+
		"\u0000\u0000\u02e7\u02e9\u00050\u0000\u0000\u02e8\u02ea\u0003~?\u0000"+
		"\u02e9\u02e8\u0001\u0000\u0000\u0000\u02e9\u02ea\u0001\u0000\u0000\u0000"+
		"\u02ea\u02eb\u0001\u0000\u0000\u0000\u02eb\u02f5\u00051\u0000\u0000\u02ec"+
		"\u02f2\u0003\u0082A\u0000\u02ed\u02ef\u0005.\u0000\u0000\u02ee\u02f0\u0003"+
		"|>\u0000\u02ef\u02ee\u0001\u0000\u0000\u0000\u02ef\u02f0\u0001\u0000\u0000"+
		"\u0000\u02f0\u02f1\u0001\u0000\u0000\u0000\u02f1\u02f3\u0005/\u0000\u0000"+
		"\u02f2\u02ed\u0001\u0000\u0000\u0000\u02f2\u02f3\u0001\u0000\u0000\u0000"+
		"\u02f3\u02f5\u0001\u0000\u0000\u0000\u02f4\u02cf\u0001\u0000\u0000\u0000"+
		"\u02f4\u02d0\u0001\u0000\u0000\u0000\u02f4\u02d1\u0001\u0000\u0000\u0000"+
		"\u02f4\u02d2\u0001\u0000\u0000\u0000\u02f4\u02d3\u0001\u0000\u0000\u0000"+
		"\u02f4\u02d4\u0001\u0000\u0000\u0000\u02f4\u02d6\u0001\u0000\u0000\u0000"+
		"\u02f4\u02da\u0001\u0000\u0000\u0000\u02f4\u02e7\u0001\u0000\u0000\u0000"+
		"\u02f4\u02ec\u0001\u0000\u0000\u0000\u02f5{\u0001\u0000\u0000\u0000\u02f6"+
		"\u02fb\u0003z=\u0000\u02f7\u02f8\u00052\u0000\u0000\u02f8\u02fa\u0003"+
		"z=\u0000\u02f9\u02f7\u0001\u0000\u0000\u0000\u02fa\u02fd\u0001\u0000\u0000"+
		"\u0000\u02fb\u02f9\u0001\u0000\u0000\u0000\u02fb\u02fc\u0001\u0000\u0000"+
		"\u0000\u02fc}\u0001\u0000\u0000\u0000\u02fd\u02fb\u0001\u0000\u0000\u0000"+
		"\u02fe\u0303\u0003\u0080@\u0000\u02ff\u0300\u00052\u0000\u0000\u0300\u0302"+
		"\u0003\u0080@\u0000\u0301\u02ff\u0001\u0000\u0000\u0000\u0302\u0305\u0001"+
		"\u0000\u0000\u0000\u0303\u0301\u0001\u0000\u0000\u0000\u0303\u0304\u0001"+
		"\u0000\u0000\u0000\u0304\u007f\u0001\u0000\u0000\u0000\u0305\u0303\u0001"+
		"\u0000\u0000\u0000\u0306\u0309\u0003\u0084B\u0000\u0307\u0308\u0005;\u0000"+
		"\u0000\u0308\u030a\u0003z=\u0000\u0309\u0307\u0001\u0000\u0000\u0000\u0309"+
		"\u030a\u0001\u0000\u0000\u0000\u030a\u0081\u0001\u0000\u0000\u0000\u030b"+
		"\u0310\u0003\u0084B\u0000\u030c\u030d\u00054\u0000\u0000\u030d\u030f\u0003"+
		"\u0084B\u0000\u030e\u030c\u0001\u0000\u0000\u0000\u030f\u0312\u0001\u0000"+
		"\u0000\u0000\u0310\u030e\u0001\u0000\u0000\u0000\u0310\u0311\u0001\u0000"+
		"\u0000\u0000\u0311\u0083\u0001\u0000\u0000\u0000\u0312\u0310\u0001\u0000"+
		"\u0000\u0000\u0313\u0314\u0007\u0006\u0000\u0000\u0314\u0085\u0001\u0000"+
		"\u0000\u0000\u0315\u0316\u0007\u0007\u0000\u0000\u0316\u0087\u0001\u0000"+
		"\u0000\u0000\u0317\u0319\u0005>\u0000\u0000\u0318\u0317\u0001\u0000\u0000"+
		"\u0000\u0319\u031a\u0001\u0000\u0000\u0000\u031a\u0318\u0001\u0000\u0000"+
		"\u0000\u031a\u031b\u0001\u0000\u0000\u0000\u031b\u0089\u0001\u0000\u0000"+
		"\u0000^\u008b\u0090\u0095\u009b\u009f\u00a4\u00a8\u00b0\u00b3\u00b7\u00bb"+
		"\u00cd\u00d2\u00dd\u00ea\u00ee\u00f2\u00fd\u010a\u0112\u0117\u011b\u0124"+
		"\u0137\u013c\u0140\u014b\u0151\u0155\u0162\u016f\u0175\u017e\u0183\u0188"+
		"\u0192\u0199\u01a2\u01a6\u01a9\u01ad\u01b2\u01bb\u01c2\u01c6\u01c8\u01d2"+
		"\u01d6\u01de\u01e4\u01eb\u01f3\u01fb\u0203\u020b\u0213\u021c\u0221\u0226"+
		"\u022e\u0233\u023c\u0244\u024e\u0252\u0257\u025b\u0261\u0265\u026a\u026e"+
		"\u0272\u0277\u027b\u027d\u0283\u028c\u0292\u0297\u029b\u02a4\u02c0\u02c5"+
		"\u02cc\u02e2\u02e9\u02ef\u02f2\u02f4\u02fb\u0303\u0309\u0310\u031a";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}