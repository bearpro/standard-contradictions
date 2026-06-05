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
		ARROW=1, LE=2, GE=3, NE=4, EQEQ=5, MODULE=6, IMPORT=7, OPEN=8, TYPE=9, 
		LET=10, FUNC=11, ENTITY=12, EVENT=13, RULE=14, STRICT=15, DEFEASIBLE=16, 
		DEFEATER=17, PRIORITY=18, OVERRIDE=19, FACT=20, IF=21, THEN=22, ELSE=23, 
		CASE=24, WHEN=25, IN=26, TRUE=27, FALSE=28, LAST=29, AND=30, OR=31, NOT=32, 
		ALWAYS=33, EVENTUALLY=34, NEXT=35, WEAK_NEXT=36, NEVER=37, UNTIL=38, RELEASE=39, 
		WEAK_UNTIL=40, OTHERWISE=41, O=42, P=43, F=44, ANNOT=45, STRING=46, RAT=47, 
		DECIMAL=48, INT=49, UNDERSCORE=50, IDENT=51, LPAREN=52, RPAREN=53, LBRACE=54, 
		RBRACE=55, COMMA=56, COLON=57, DOT=58, BAR=59, PLUS=60, MINUS=61, STAR=62, 
		SLASH=63, PERCENT=64, EQ=65, LT=66, GT=67, NEWLINE=68, COMMENT=69, WS=70, 
		INDENT=71, DEDENT=72;
	public static final int
		RULE_program = 0, RULE_exprOnly = 1, RULE_typeExprOnly = 2, RULE_topItem = 3, 
		RULE_annotations = 4, RULE_moduleDecl = 5, RULE_importDecl = 6, RULE_openDecl = 7, 
		RULE_declaration = 8, RULE_typeDecl = 9, RULE_typeDefinition = 10, RULE_typeParams = 11, 
		RULE_nameList = 12, RULE_variant = 13, RULE_variantFieldList = 14, RULE_variantField = 15, 
		RULE_typeExpr = 16, RULE_recordType = 17, RULE_typeFieldList = 18, RULE_typeField = 19, 
		RULE_tupleOrParenType = 20, RULE_typeRef = 21, RULE_typeArgs = 22, RULE_typeExprList = 23, 
		RULE_valueDecl = 24, RULE_funcDecl = 25, RULE_paramList = 26, RULE_param = 27, 
		RULE_entityDecl = 28, RULE_eventDecl = 29, RULE_ruleDecl = 30, RULE_ruleStrength = 31, 
		RULE_ruleBody = 32, RULE_deonticMod = 33, RULE_priorityDecl = 34, RULE_factDecl = 35, 
		RULE_block = 36, RULE_blockLetStmt = 37, RULE_typeAnnotation = 38, RULE_expr = 39, 
		RULE_temporalPostfix = 40, RULE_implication = 41, RULE_orExpr = 42, RULE_andExpr = 43, 
		RULE_temporalBinary = 44, RULE_comparison = 45, RULE_additive = 46, RULE_multiplicative = 47, 
		RULE_unary = 48, RULE_ifExpr = 49, RULE_letExpr = 50, RULE_matchExpr = 51, 
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
			"typeRef", "typeArgs", "typeExprList", "valueDecl", "funcDecl", "paramList", 
			"param", "entityDecl", "eventDecl", "ruleDecl", "ruleStrength", "ruleBody", 
			"deonticMod", "priorityDecl", "factDecl", "block", "blockLetStmt", "typeAnnotation", 
			"expr", "temporalPostfix", "implication", "orExpr", "andExpr", "temporalBinary", 
			"comparison", "additive", "multiplicative", "unary", "ifExpr", "letExpr", 
			"matchExpr", "caseBody", "caseArm", "postfix", "postfixSuffix", "recordConstructorFields", 
			"recordConstructorFieldList", "recordConstructorField", "primary", "exprList", 
			"pattern", "patternList", "recordPatternFieldList", "recordPatternField", 
			"qualifiedName", "nameToken", "temporalUnaryOp", "newlines"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'->'", "'<='", "'>='", "'!='", "'=='", "'module'", "'import'", 
			"'open'", "'type'", "'let'", "'func'", "'entity'", "'event'", "'rule'", 
			"'strict'", "'defeasible'", "'defeater'", "'priority'", "'override'", 
			"'fact'", "'if'", "'then'", "'else'", "'case'", "'when'", "'in'", "'true'", 
			"'false'", "'last'", "'and'", "'or'", "'not'", "'always'", "'eventually'", 
			"'next'", "'weak_next'", "'never'", "'until'", "'release'", "'weak_until'", 
			"'otherwise'", "'O'", "'P'", "'F'", null, null, null, null, null, "'_'", 
			null, "'('", "')'", "'{'", "'}'", "','", "':'", "'.'", "'|'", "'+'", 
			"'-'", "'*'", "'/'", "'%'", "'='", "'<'", "'>'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, "ARROW", "LE", "GE", "NE", "EQEQ", "MODULE", "IMPORT", "OPEN", 
			"TYPE", "LET", "FUNC", "ENTITY", "EVENT", "RULE", "STRICT", "DEFEASIBLE", 
			"DEFEATER", "PRIORITY", "OVERRIDE", "FACT", "IF", "THEN", "ELSE", "CASE", 
			"WHEN", "IN", "TRUE", "FALSE", "LAST", "AND", "OR", "NOT", "ALWAYS", 
			"EVENTUALLY", "NEXT", "WEAK_NEXT", "NEVER", "UNTIL", "RELEASE", "WEAK_UNTIL", 
			"OTHERWISE", "O", "P", "F", "ANNOT", "STRING", "RAT", "DECIMAL", "INT", 
			"UNDERSCORE", "IDENT", "LPAREN", "RPAREN", "LBRACE", "RBRACE", "COMMA", 
			"COLON", "DOT", "BAR", "PLUS", "MINUS", "STAR", "SLASH", "PERCENT", "EQ", 
			"LT", "GT", "NEWLINE", "COMMENT", "WS", "INDENT", "DEDENT"
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterProgram(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitProgram(this);
		}
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
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & 35184374185856L) != 0)) {
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterExprOnly(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitExprOnly(this);
		}
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterTypeExprOnly(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitTypeExprOnly(this);
		}
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterTopItem(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitTopItem(this);
		}
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
			case LET:
			case FUNC:
			case ENTITY:
			case EVENT:
			case RULE:
			case STRICT:
			case DEFEASIBLE:
			case DEFEATER:
			case PRIORITY:
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterAnnotations(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitAnnotations(this);
		}
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterModuleDecl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitModuleDecl(this);
		}
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterImportDecl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitImportDecl(this);
		}
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterOpenDecl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitOpenDecl(this);
		}
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
		public ValueDeclContext valueDecl() {
			return getRuleContext(ValueDeclContext.class,0);
		}
		public FuncDeclContext funcDecl() {
			return getRuleContext(FuncDeclContext.class,0);
		}
		public EntityDeclContext entityDecl() {
			return getRuleContext(EntityDeclContext.class,0);
		}
		public EventDeclContext eventDecl() {
			return getRuleContext(EventDeclContext.class,0);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterDeclaration(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitDeclaration(this);
		}
	}

	public final DeclarationContext declaration() throws RecognitionException {
		DeclarationContext _localctx = new DeclarationContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_declaration);
		try {
			setState(207);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case TYPE:
				enterOuterAlt(_localctx, 1);
				{
				setState(199);
				typeDecl();
				}
				break;
			case LET:
				enterOuterAlt(_localctx, 2);
				{
				setState(200);
				valueDecl();
				}
				break;
			case FUNC:
				enterOuterAlt(_localctx, 3);
				{
				setState(201);
				funcDecl();
				}
				break;
			case ENTITY:
				enterOuterAlt(_localctx, 4);
				{
				setState(202);
				entityDecl();
				}
				break;
			case EVENT:
				enterOuterAlt(_localctx, 5);
				{
				setState(203);
				eventDecl();
				}
				break;
			case RULE:
			case STRICT:
			case DEFEASIBLE:
			case DEFEATER:
				enterOuterAlt(_localctx, 6);
				{
				setState(204);
				ruleDecl();
				}
				break;
			case PRIORITY:
			case OVERRIDE:
				enterOuterAlt(_localctx, 7);
				{
				setState(205);
				priorityDecl();
				}
				break;
			case FACT:
				enterOuterAlt(_localctx, 8);
				{
				setState(206);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterTypeDecl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitTypeDecl(this);
		}
	}

	public final TypeDeclContext typeDecl() throws RecognitionException {
		TypeDeclContext _localctx = new TypeDeclContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_typeDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(209);
			match(TYPE);
			setState(210);
			nameToken();
			setState(212);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LT) {
				{
				setState(211);
				typeParams();
				}
			}

			setState(214);
			match(EQ);
			setState(215);
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
		public TypeDefinitionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeDefinition; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterTypeDefinition(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitTypeDefinition(this);
		}
	}

	public final TypeDefinitionContext typeDefinition() throws RecognitionException {
		TypeDefinitionContext _localctx = new TypeDefinitionContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_typeDefinition);
		int _la;
		try {
			setState(226);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case LBRACE:
				enterOuterAlt(_localctx, 1);
				{
				setState(217);
				recordType();
				}
				break;
			case TRUE:
			case FALSE:
			case LAST:
			case O:
			case P:
			case F:
			case IDENT:
				enterOuterAlt(_localctx, 2);
				{
				setState(218);
				variant();
				setState(223);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==BAR) {
					{
					{
					setState(219);
					match(BAR);
					setState(220);
					variant();
					}
					}
					setState(225);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterTypeParams(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitTypeParams(this);
		}
	}

	public final TypeParamsContext typeParams() throws RecognitionException {
		TypeParamsContext _localctx = new TypeParamsContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_typeParams);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(228);
			match(LT);
			setState(229);
			nameList();
			setState(230);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterNameList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitNameList(this);
		}
	}

	public final NameListContext nameList() throws RecognitionException {
		NameListContext _localctx = new NameListContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_nameList);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(232);
			nameToken();
			setState(237);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,15,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(233);
					match(COMMA);
					setState(234);
					nameToken();
					}
					} 
				}
				setState(239);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,15,_ctx);
			}
			setState(241);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(240);
				match(COMMA);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterVariant(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitVariant(this);
		}
	}

	public final VariantContext variant() throws RecognitionException {
		VariantContext _localctx = new VariantContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_variant);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(243);
			nameToken();
			setState(244);
			match(LPAREN);
			setState(245);
			variantFieldList();
			setState(246);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterVariantFieldList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitVariantFieldList(this);
		}
	}

	public final VariantFieldListContext variantFieldList() throws RecognitionException {
		VariantFieldListContext _localctx = new VariantFieldListContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_variantFieldList);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(248);
			variantField();
			setState(253);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,17,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(249);
					match(COMMA);
					setState(250);
					variantField();
					}
					} 
				}
				setState(255);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,17,_ctx);
			}
			setState(257);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(256);
				match(COMMA);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterVariantField(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitVariantField(this);
		}
	}

	public final VariantFieldContext variantField() throws RecognitionException {
		VariantFieldContext _localctx = new VariantFieldContext(_ctx, getState());
		enterRule(_localctx, 30, RULE_variantField);
		try {
			setState(264);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,19,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(259);
				nameToken();
				setState(260);
				match(COLON);
				setState(261);
				typeExpr();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(263);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterTypeExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitTypeExpr(this);
		}
	}

	public final TypeExprContext typeExpr() throws RecognitionException {
		TypeExprContext _localctx = new TypeExprContext(_ctx, getState());
		enterRule(_localctx, 32, RULE_typeExpr);
		try {
			setState(269);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case LBRACE:
				enterOuterAlt(_localctx, 1);
				{
				setState(266);
				recordType();
				}
				break;
			case LPAREN:
				enterOuterAlt(_localctx, 2);
				{
				setState(267);
				tupleOrParenType();
				}
				break;
			case TRUE:
			case FALSE:
			case LAST:
			case O:
			case P:
			case F:
			case IDENT:
				enterOuterAlt(_localctx, 3);
				{
				setState(268);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterRecordType(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitRecordType(this);
		}
	}

	public final RecordTypeContext recordType() throws RecognitionException {
		RecordTypeContext _localctx = new RecordTypeContext(_ctx, getState());
		enterRule(_localctx, 34, RULE_recordType);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(271);
			match(LBRACE);
			setState(273);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 2282587078787072L) != 0)) {
				{
				setState(272);
				typeFieldList();
				}
			}

			setState(275);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterTypeFieldList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitTypeFieldList(this);
		}
	}

	public final TypeFieldListContext typeFieldList() throws RecognitionException {
		TypeFieldListContext _localctx = new TypeFieldListContext(_ctx, getState());
		enterRule(_localctx, 36, RULE_typeFieldList);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(277);
			typeField();
			setState(282);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,22,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(278);
					match(COMMA);
					setState(279);
					typeField();
					}
					} 
				}
				setState(284);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,22,_ctx);
			}
			setState(286);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(285);
				match(COMMA);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterTypeField(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitTypeField(this);
		}
	}

	public final TypeFieldContext typeField() throws RecognitionException {
		TypeFieldContext _localctx = new TypeFieldContext(_ctx, getState());
		enterRule(_localctx, 38, RULE_typeField);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(288);
			nameToken();
			setState(289);
			match(COLON);
			setState(290);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterTupleOrParenType(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitTupleOrParenType(this);
		}
	}

	public final TupleOrParenTypeContext tupleOrParenType() throws RecognitionException {
		TupleOrParenTypeContext _localctx = new TupleOrParenTypeContext(_ctx, getState());
		enterRule(_localctx, 40, RULE_tupleOrParenType);
		int _la;
		try {
			setState(309);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,25,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(292);
				match(LPAREN);
				setState(293);
				typeExpr();
				setState(294);
				match(RPAREN);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(296);
				match(LPAREN);
				setState(297);
				typeExpr();
				setState(298);
				match(COMMA);
				setState(299);
				typeExpr();
				setState(304);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(300);
					match(COMMA);
					setState(301);
					typeExpr();
					}
					}
					setState(306);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(307);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterTypeRef(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitTypeRef(this);
		}
	}

	public final TypeRefContext typeRef() throws RecognitionException {
		TypeRefContext _localctx = new TypeRefContext(_ctx, getState());
		enterRule(_localctx, 42, RULE_typeRef);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(311);
			qualifiedName();
			setState(313);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LT) {
				{
				setState(312);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterTypeArgs(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitTypeArgs(this);
		}
	}

	public final TypeArgsContext typeArgs() throws RecognitionException {
		TypeArgsContext _localctx = new TypeArgsContext(_ctx, getState());
		enterRule(_localctx, 44, RULE_typeArgs);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(315);
			match(LT);
			setState(316);
			typeExprList();
			setState(317);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterTypeExprList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitTypeExprList(this);
		}
	}

	public final TypeExprListContext typeExprList() throws RecognitionException {
		TypeExprListContext _localctx = new TypeExprListContext(_ctx, getState());
		enterRule(_localctx, 46, RULE_typeExprList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(319);
			typeExpr();
			setState(324);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(320);
				match(COMMA);
				setState(321);
				typeExpr();
				}
				}
				setState(326);
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
	public static class ValueDeclContext extends ParserRuleContext {
		public TerminalNode LET() { return getToken(MDLParser.LET, 0); }
		public NameTokenContext nameToken() {
			return getRuleContext(NameTokenContext.class,0);
		}
		public TerminalNode EQ() { return getToken(MDLParser.EQ, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TypeAnnotationContext typeAnnotation() {
			return getRuleContext(TypeAnnotationContext.class,0);
		}
		public ValueDeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_valueDecl; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterValueDecl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitValueDecl(this);
		}
	}

	public final ValueDeclContext valueDecl() throws RecognitionException {
		ValueDeclContext _localctx = new ValueDeclContext(_ctx, getState());
		enterRule(_localctx, 48, RULE_valueDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(327);
			match(LET);
			setState(328);
			nameToken();
			setState(330);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COLON) {
				{
				setState(329);
				typeAnnotation();
				}
			}

			setState(332);
			match(EQ);
			setState(333);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterFuncDecl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitFuncDecl(this);
		}
	}

	public final FuncDeclContext funcDecl() throws RecognitionException {
		FuncDeclContext _localctx = new FuncDeclContext(_ctx, getState());
		enterRule(_localctx, 50, RULE_funcDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(335);
			match(FUNC);
			setState(336);
			nameToken();
			setState(338);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LT) {
				{
				setState(337);
				typeParams();
				}
			}

			setState(340);
			match(LPAREN);
			setState(342);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 26982016285147136L) != 0)) {
				{
				setState(341);
				paramList();
				}
			}

			setState(344);
			match(RPAREN);
			setState(345);
			match(ARROW);
			setState(346);
			typeExpr();
			setState(347);
			match(COLON);
			setState(348);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterParamList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitParamList(this);
		}
	}

	public final ParamListContext paramList() throws RecognitionException {
		ParamListContext _localctx = new ParamListContext(_ctx, getState());
		enterRule(_localctx, 52, RULE_paramList);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(350);
			param();
			setState(355);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,31,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(351);
					match(COMMA);
					setState(352);
					param();
					}
					} 
				}
				setState(357);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,31,_ctx);
			}
			setState(359);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(358);
				match(COMMA);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterParam(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitParam(this);
		}
	}

	public final ParamContext param() throws RecognitionException {
		ParamContext _localctx = new ParamContext(_ctx, getState());
		enterRule(_localctx, 54, RULE_param);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(361);
			pattern();
			setState(362);
			match(COLON);
			setState(363);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterEntityDecl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitEntityDecl(this);
		}
	}

	public final EntityDeclContext entityDecl() throws RecognitionException {
		EntityDeclContext _localctx = new EntityDeclContext(_ctx, getState());
		enterRule(_localctx, 56, RULE_entityDecl);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(365);
			match(ENTITY);
			setState(366);
			nameToken();
			setState(367);
			match(COLON);
			setState(368);
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
	public static class EventDeclContext extends ParserRuleContext {
		public TerminalNode EVENT() { return getToken(MDLParser.EVENT, 0); }
		public NameTokenContext nameToken() {
			return getRuleContext(NameTokenContext.class,0);
		}
		public TerminalNode LPAREN() { return getToken(MDLParser.LPAREN, 0); }
		public TerminalNode RPAREN() { return getToken(MDLParser.RPAREN, 0); }
		public TypeFieldListContext typeFieldList() {
			return getRuleContext(TypeFieldListContext.class,0);
		}
		public EventDeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_eventDecl; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterEventDecl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitEventDecl(this);
		}
	}

	public final EventDeclContext eventDecl() throws RecognitionException {
		EventDeclContext _localctx = new EventDeclContext(_ctx, getState());
		enterRule(_localctx, 58, RULE_eventDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(370);
			match(EVENT);
			setState(371);
			nameToken();
			setState(372);
			match(LPAREN);
			setState(374);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 2282587078787072L) != 0)) {
				{
				setState(373);
				typeFieldList();
				}
			}

			setState(376);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterRuleDecl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitRuleDecl(this);
		}
	}

	public final RuleDeclContext ruleDecl() throws RecognitionException {
		RuleDeclContext _localctx = new RuleDeclContext(_ctx, getState());
		enterRule(_localctx, 60, RULE_ruleDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(379);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 229376L) != 0)) {
				{
				setState(378);
				ruleStrength();
				}
			}

			setState(381);
			match(RULE);
			setState(382);
			ruleBody();
			setState(385);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==OTHERWISE) {
				{
				setState(383);
				match(OTHERWISE);
				setState(384);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterRuleStrength(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitRuleStrength(this);
		}
	}

	public final RuleStrengthContext ruleStrength() throws RecognitionException {
		RuleStrengthContext _localctx = new RuleStrengthContext(_ctx, getState());
		enterRule(_localctx, 62, RULE_ruleStrength);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(387);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 229376L) != 0)) ) {
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
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public QualifiedNameContext qualifiedName() {
			return getRuleContext(QualifiedNameContext.class,0);
		}
		public TerminalNode WHEN() { return getToken(MDLParser.WHEN, 0); }
		public RuleBodyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ruleBody; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterRuleBody(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitRuleBody(this);
		}
	}

	public final RuleBodyContext ruleBody() throws RecognitionException {
		RuleBodyContext _localctx = new RuleBodyContext(_ctx, getState());
		enterRule(_localctx, 64, RULE_ruleBody);
		int _la;
		try {
			setState(404);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,38,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(389);
				deonticMod();
				setState(390);
				match(COLON);
				setState(391);
				expr();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(394);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,36,_ctx) ) {
				case 1:
					{
					setState(393);
					deonticMod();
					}
					break;
				}
				setState(396);
				qualifiedName();
				setState(399);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==WHEN) {
					{
					setState(397);
					match(WHEN);
					setState(398);
					expr();
					}
				}

				setState(401);
				match(COLON);
				setState(402);
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
	public static class DeonticModContext extends ParserRuleContext {
		public TerminalNode O() { return getToken(MDLParser.O, 0); }
		public TerminalNode P() { return getToken(MDLParser.P, 0); }
		public TerminalNode F() { return getToken(MDLParser.F, 0); }
		public DeonticModContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_deonticMod; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterDeonticMod(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitDeonticMod(this);
		}
	}

	public final DeonticModContext deonticMod() throws RecognitionException {
		DeonticModContext _localctx = new DeonticModContext(_ctx, getState());
		enterRule(_localctx, 66, RULE_deonticMod);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(406);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 30786325577728L) != 0)) ) {
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
		public List<QualifiedNameContext> qualifiedName() {
			return getRuleContexts(QualifiedNameContext.class);
		}
		public QualifiedNameContext qualifiedName(int i) {
			return getRuleContext(QualifiedNameContext.class,i);
		}
		public TerminalNode PRIORITY() { return getToken(MDLParser.PRIORITY, 0); }
		public TerminalNode OVERRIDE() { return getToken(MDLParser.OVERRIDE, 0); }
		public List<TerminalNode> GT() { return getTokens(MDLParser.GT); }
		public TerminalNode GT(int i) {
			return getToken(MDLParser.GT, i);
		}
		public PriorityDeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_priorityDecl; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterPriorityDecl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitPriorityDecl(this);
		}
	}

	public final PriorityDeclContext priorityDecl() throws RecognitionException {
		PriorityDeclContext _localctx = new PriorityDeclContext(_ctx, getState());
		enterRule(_localctx, 68, RULE_priorityDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(408);
			_la = _input.LA(1);
			if ( !(_la==PRIORITY || _la==OVERRIDE) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			setState(409);
			qualifiedName();
			setState(414);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==GT) {
				{
				{
				setState(410);
				match(GT);
				setState(411);
				qualifiedName();
				}
				}
				setState(416);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterFactDecl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitFactDecl(this);
		}
	}

	public final FactDeclContext factDecl() throws RecognitionException {
		FactDeclContext _localctx = new FactDeclContext(_ctx, getState());
		enterRule(_localctx, 70, RULE_factDecl);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(417);
			match(FACT);
			setState(421);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,40,_ctx) ) {
			case 1:
				{
				setState(418);
				nameToken();
				setState(419);
				match(EQ);
				}
				break;
			}
			setState(423);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterBlock(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitBlock(this);
		}
	}

	public final BlockContext block() throws RecognitionException {
		BlockContext _localctx = new BlockContext(_ctx, getState());
		enterRule(_localctx, 72, RULE_block);
		int _la;
		try {
			int _alt;
			setState(441);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case NEWLINE:
				enterOuterAlt(_localctx, 1);
				{
				setState(425);
				match(NEWLINE);
				setState(426);
				match(INDENT);
				setState(430);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,41,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(427);
						blockLetStmt();
						}
						} 
					}
					setState(432);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,41,_ctx);
				}
				setState(434);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 2313684997684331520L) != 0)) {
					{
					setState(433);
					expr();
					}
				}

				setState(437);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(436);
					newlines();
					}
				}

				setState(439);
				match(DEDENT);
				}
				break;
			case LET:
			case IF:
			case CASE:
			case TRUE:
			case FALSE:
			case LAST:
			case NOT:
			case ALWAYS:
			case EVENTUALLY:
			case NEXT:
			case WEAK_NEXT:
			case NEVER:
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
				setState(440);
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
		public NewlinesContext newlines() {
			return getRuleContext(NewlinesContext.class,0);
		}
		public TypeAnnotationContext typeAnnotation() {
			return getRuleContext(TypeAnnotationContext.class,0);
		}
		public BlockLetStmtContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_blockLetStmt; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterBlockLetStmt(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitBlockLetStmt(this);
		}
	}

	public final BlockLetStmtContext blockLetStmt() throws RecognitionException {
		BlockLetStmtContext _localctx = new BlockLetStmtContext(_ctx, getState());
		enterRule(_localctx, 74, RULE_blockLetStmt);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(443);
			match(LET);
			setState(444);
			pattern();
			setState(446);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COLON) {
				{
				setState(445);
				typeAnnotation();
				}
			}

			setState(448);
			match(EQ);
			setState(449);
			expr();
			setState(450);
			newlines();
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterTypeAnnotation(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitTypeAnnotation(this);
		}
	}

	public final TypeAnnotationContext typeAnnotation() throws RecognitionException {
		TypeAnnotationContext _localctx = new TypeAnnotationContext(_ctx, getState());
		enterRule(_localctx, 76, RULE_typeAnnotation);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(452);
			match(COLON);
			setState(453);
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
	public static class ExprContext extends ParserRuleContext {
		public TemporalPostfixContext temporalPostfix() {
			return getRuleContext(TemporalPostfixContext.class,0);
		}
		public ExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expr; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitExpr(this);
		}
	}

	public final ExprContext expr() throws RecognitionException {
		ExprContext _localctx = new ExprContext(_ctx, getState());
		enterRule(_localctx, 78, RULE_expr);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(455);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterTemporalPostfix(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitTemporalPostfix(this);
		}
	}

	public final TemporalPostfixContext temporalPostfix() throws RecognitionException {
		TemporalPostfixContext _localctx = new TemporalPostfixContext(_ctx, getState());
		enterRule(_localctx, 80, RULE_temporalPostfix);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(457);
			implication();
			setState(461);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,46,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(458);
					temporalUnaryOp();
					}
					} 
				}
				setState(463);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,46,_ctx);
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
		public ImplicationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_implication; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterImplication(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitImplication(this);
		}
	}

	public final ImplicationContext implication() throws RecognitionException {
		ImplicationContext _localctx = new ImplicationContext(_ctx, getState());
		enterRule(_localctx, 82, RULE_implication);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(464);
			orExpr();
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterOrExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitOrExpr(this);
		}
	}

	public final OrExprContext orExpr() throws RecognitionException {
		OrExprContext _localctx = new OrExprContext(_ctx, getState());
		enterRule(_localctx, 84, RULE_orExpr);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(466);
			andExpr();
			setState(471);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,47,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(467);
					match(OR);
					setState(468);
					andExpr();
					}
					} 
				}
				setState(473);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,47,_ctx);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterAndExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitAndExpr(this);
		}
	}

	public final AndExprContext andExpr() throws RecognitionException {
		AndExprContext _localctx = new AndExprContext(_ctx, getState());
		enterRule(_localctx, 86, RULE_andExpr);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(474);
			temporalBinary();
			setState(479);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,48,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(475);
					match(AND);
					setState(476);
					temporalBinary();
					}
					} 
				}
				setState(481);
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
		public List<TerminalNode> RELEASE() { return getTokens(MDLParser.RELEASE); }
		public TerminalNode RELEASE(int i) {
			return getToken(MDLParser.RELEASE, i);
		}
		public List<TerminalNode> WEAK_UNTIL() { return getTokens(MDLParser.WEAK_UNTIL); }
		public TerminalNode WEAK_UNTIL(int i) {
			return getToken(MDLParser.WEAK_UNTIL, i);
		}
		public TemporalBinaryContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_temporalBinary; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterTemporalBinary(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitTemporalBinary(this);
		}
	}

	public final TemporalBinaryContext temporalBinary() throws RecognitionException {
		TemporalBinaryContext _localctx = new TemporalBinaryContext(_ctx, getState());
		enterRule(_localctx, 88, RULE_temporalBinary);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(482);
			comparison();
			setState(487);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,49,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(483);
					_la = _input.LA(1);
					if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 1924145348608L) != 0)) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(484);
					comparison();
					}
					} 
				}
				setState(489);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,49,_ctx);
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
		public List<TerminalNode> EQEQ() { return getTokens(MDLParser.EQEQ); }
		public TerminalNode EQEQ(int i) {
			return getToken(MDLParser.EQEQ, i);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterComparison(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitComparison(this);
		}
	}

	public final ComparisonContext comparison() throws RecognitionException {
		ComparisonContext _localctx = new ComparisonContext(_ctx, getState());
		enterRule(_localctx, 90, RULE_comparison);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(490);
			additive();
			setState(495);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,50,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(491);
					_la = _input.LA(1);
					if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 60L) != 0) || ((((_la - 65)) & ~0x3f) == 0 && ((1L << (_la - 65)) & 7L) != 0)) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(492);
					additive();
					}
					} 
				}
				setState(497);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterAdditive(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitAdditive(this);
		}
	}

	public final AdditiveContext additive() throws RecognitionException {
		AdditiveContext _localctx = new AdditiveContext(_ctx, getState());
		enterRule(_localctx, 92, RULE_additive);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(498);
			multiplicative();
			setState(503);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,51,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(499);
					_la = _input.LA(1);
					if ( !(_la==PLUS || _la==MINUS) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(500);
					multiplicative();
					}
					} 
				}
				setState(505);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterMultiplicative(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitMultiplicative(this);
		}
	}

	public final MultiplicativeContext multiplicative() throws RecognitionException {
		MultiplicativeContext _localctx = new MultiplicativeContext(_ctx, getState());
		enterRule(_localctx, 94, RULE_multiplicative);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(506);
			unary();
			setState(511);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,52,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(507);
					_la = _input.LA(1);
					if ( !(((((_la - 62)) & ~0x3f) == 0 && ((1L << (_la - 62)) & 7L) != 0)) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(508);
					unary();
					}
					} 
				}
				setState(513);
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
		public TemporalUnaryOpContext temporalUnaryOp() {
			return getRuleContext(TemporalUnaryOpContext.class,0);
		}
		public PostfixContext postfix() {
			return getRuleContext(PostfixContext.class,0);
		}
		public UnaryContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_unary; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterUnary(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitUnary(this);
		}
	}

	public final UnaryContext unary() throws RecognitionException {
		UnaryContext _localctx = new UnaryContext(_ctx, getState());
		enterRule(_localctx, 96, RULE_unary);
		try {
			setState(524);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case IF:
				enterOuterAlt(_localctx, 1);
				{
				setState(514);
				ifExpr();
				}
				break;
			case LET:
				enterOuterAlt(_localctx, 2);
				{
				setState(515);
				letExpr();
				}
				break;
			case CASE:
				enterOuterAlt(_localctx, 3);
				{
				setState(516);
				matchExpr();
				}
				break;
			case NOT:
			case ALWAYS:
			case EVENTUALLY:
			case NEXT:
			case WEAK_NEXT:
			case NEVER:
			case MINUS:
				enterOuterAlt(_localctx, 4);
				{
				setState(520);
				_errHandler.sync(this);
				switch (_input.LA(1)) {
				case NOT:
					{
					setState(517);
					match(NOT);
					}
					break;
				case MINUS:
					{
					setState(518);
					match(MINUS);
					}
					break;
				case ALWAYS:
				case EVENTUALLY:
				case NEXT:
				case WEAK_NEXT:
				case NEVER:
					{
					setState(519);
					temporalUnaryOp();
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(522);
				unary();
				}
				break;
			case TRUE:
			case FALSE:
			case LAST:
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
				setState(523);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterIfExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitIfExpr(this);
		}
	}

	public final IfExprContext ifExpr() throws RecognitionException {
		IfExprContext _localctx = new IfExprContext(_ctx, getState());
		enterRule(_localctx, 98, RULE_ifExpr);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(526);
			match(IF);
			setState(527);
			expr();
			setState(529);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(528);
				newlines();
				}
			}

			setState(531);
			match(THEN);
			setState(532);
			expr();
			setState(534);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(533);
				newlines();
				}
			}

			setState(536);
			match(ELSE);
			setState(537);
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
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public TerminalNode IN() { return getToken(MDLParser.IN, 0); }
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterLetExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitLetExpr(this);
		}
	}

	public final LetExprContext letExpr() throws RecognitionException {
		LetExprContext _localctx = new LetExprContext(_ctx, getState());
		enterRule(_localctx, 100, RULE_letExpr);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(539);
			match(LET);
			setState(540);
			pattern();
			setState(542);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COLON) {
				{
				setState(541);
				typeAnnotation();
				}
			}

			setState(544);
			match(EQ);
			setState(545);
			expr();
			setState(547);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(546);
				newlines();
				}
			}

			setState(549);
			match(IN);
			setState(550);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterMatchExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitMatchExpr(this);
		}
	}

	public final MatchExprContext matchExpr() throws RecognitionException {
		MatchExprContext _localctx = new MatchExprContext(_ctx, getState());
		enterRule(_localctx, 102, RULE_matchExpr);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(552);
			match(CASE);
			setState(553);
			expr();
			setState(554);
			match(COLON);
			setState(555);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterCaseBody(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitCaseBody(this);
		}
	}

	public final CaseBodyContext caseBody() throws RecognitionException {
		CaseBodyContext _localctx = new CaseBodyContext(_ctx, getState());
		enterRule(_localctx, 104, RULE_caseBody);
		int _la;
		try {
			int _alt;
			setState(594);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,67,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(557);
				match(NEWLINE);
				setState(558);
				match(INDENT);
				setState(560);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(559);
					newlines();
					}
				}

				setState(562);
				caseArm();
				setState(569);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,61,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(564);
						_errHandler.sync(this);
						_la = _input.LA(1);
						if (_la==NEWLINE) {
							{
							setState(563);
							newlines();
							}
						}

						setState(566);
						caseArm();
						}
						} 
					}
					setState(571);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,61,_ctx);
				}
				setState(573);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(572);
					newlines();
					}
				}

				setState(575);
				match(DEDENT);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(577);
				match(NEWLINE);
				setState(579);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(578);
					newlines();
					}
				}

				setState(581);
				caseArm();
				setState(588);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,65,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(583);
						_errHandler.sync(this);
						_la = _input.LA(1);
						if (_la==NEWLINE) {
							{
							setState(582);
							newlines();
							}
						}

						setState(585);
						caseArm();
						}
						} 
					}
					setState(590);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,65,_ctx);
				}
				setState(592);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,66,_ctx) ) {
				case 1:
					{
					setState(591);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterCaseArm(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitCaseArm(this);
		}
	}

	public final CaseArmContext caseArm() throws RecognitionException {
		CaseArmContext _localctx = new CaseArmContext(_ctx, getState());
		enterRule(_localctx, 106, RULE_caseArm);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(596);
			match(BAR);
			setState(597);
			pattern();
			setState(600);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==WHEN) {
				{
				setState(598);
				match(WHEN);
				setState(599);
				expr();
				}
			}

			setState(602);
			match(COLON);
			setState(603);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterPostfix(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitPostfix(this);
		}
	}

	public final PostfixContext postfix() throws RecognitionException {
		PostfixContext _localctx = new PostfixContext(_ctx, getState());
		enterRule(_localctx, 108, RULE_postfix);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(605);
			primary();
			setState(609);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & 310748374288564224L) != 0)) {
				{
				{
				setState(606);
				postfixSuffix();
				}
				}
				setState(611);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterPostfixSuffix(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitPostfixSuffix(this);
		}
	}

	public final PostfixSuffixContext postfixSuffix() throws RecognitionException {
		PostfixSuffixContext _localctx = new PostfixSuffixContext(_ctx, getState());
		enterRule(_localctx, 110, RULE_postfixSuffix);
		int _la;
		try {
			setState(620);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case LBRACE:
				enterOuterAlt(_localctx, 1);
				{
				setState(612);
				recordConstructorFields();
				}
				break;
			case LPAREN:
				enterOuterAlt(_localctx, 2);
				{
				setState(613);
				match(LPAREN);
				setState(615);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 2313684997684331520L) != 0)) {
					{
					setState(614);
					exprList();
					}
				}

				setState(617);
				match(RPAREN);
				}
				break;
			case DOT:
				enterOuterAlt(_localctx, 3);
				{
				setState(618);
				match(DOT);
				setState(619);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterRecordConstructorFields(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitRecordConstructorFields(this);
		}
	}

	public final RecordConstructorFieldsContext recordConstructorFields() throws RecognitionException {
		RecordConstructorFieldsContext _localctx = new RecordConstructorFieldsContext(_ctx, getState());
		enterRule(_localctx, 112, RULE_recordConstructorFields);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(622);
			match(LBRACE);
			setState(624);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 2282587078787072L) != 0)) {
				{
				setState(623);
				recordConstructorFieldList();
				}
			}

			setState(626);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterRecordConstructorFieldList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitRecordConstructorFieldList(this);
		}
	}

	public final RecordConstructorFieldListContext recordConstructorFieldList() throws RecognitionException {
		RecordConstructorFieldListContext _localctx = new RecordConstructorFieldListContext(_ctx, getState());
		enterRule(_localctx, 114, RULE_recordConstructorFieldList);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(628);
			recordConstructorField();
			setState(633);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,73,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(629);
					match(COMMA);
					setState(630);
					recordConstructorField();
					}
					} 
				}
				setState(635);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,73,_ctx);
			}
			setState(637);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(636);
				match(COMMA);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterRecordConstructorField(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitRecordConstructorField(this);
		}
	}

	public final RecordConstructorFieldContext recordConstructorField() throws RecognitionException {
		RecordConstructorFieldContext _localctx = new RecordConstructorFieldContext(_ctx, getState());
		enterRule(_localctx, 116, RULE_recordConstructorField);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(639);
			nameToken();
			setState(640);
			match(EQ);
			setState(641);
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
		public TerminalNode LAST() { return getToken(MDLParser.LAST, 0); }
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterPrimary(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitPrimary(this);
		}
	}

	public final PrimaryContext primary() throws RecognitionException {
		PrimaryContext _localctx = new PrimaryContext(_ctx, getState());
		enterRule(_localctx, 118, RULE_primary);
		int _la;
		try {
			setState(670);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,76,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(643);
				match(STRING);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(644);
				match(INT);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(645);
				match(DECIMAL);
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(646);
				match(RAT);
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(647);
				match(TRUE);
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(648);
				match(FALSE);
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(649);
				match(LAST);
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(650);
				qualifiedName();
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(651);
				match(LPAREN);
				setState(652);
				match(RPAREN);
				}
				break;
			case 10:
				enterOuterAlt(_localctx, 10);
				{
				setState(653);
				match(LPAREN);
				setState(654);
				expr();
				setState(655);
				match(RPAREN);
				}
				break;
			case 11:
				enterOuterAlt(_localctx, 11);
				{
				setState(657);
				match(LPAREN);
				setState(658);
				expr();
				setState(659);
				match(COMMA);
				setState(660);
				expr();
				setState(665);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(661);
					match(COMMA);
					setState(662);
					expr();
					}
					}
					setState(667);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(668);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterExprList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitExprList(this);
		}
	}

	public final ExprListContext exprList() throws RecognitionException {
		ExprListContext _localctx = new ExprListContext(_ctx, getState());
		enterRule(_localctx, 120, RULE_exprList);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(672);
			expr();
			setState(677);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,77,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(673);
					match(COMMA);
					setState(674);
					expr();
					}
					} 
				}
				setState(679);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,77,_ctx);
			}
			setState(681);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(680);
				match(COMMA);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterPattern(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitPattern(this);
		}
	}

	public final PatternContext pattern() throws RecognitionException {
		PatternContext _localctx = new PatternContext(_ctx, getState());
		enterRule(_localctx, 122, RULE_pattern);
		int _la;
		try {
			setState(720);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,83,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(683);
				match(UNDERSCORE);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(684);
				match(STRING);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(685);
				match(INT);
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(686);
				match(DECIMAL);
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(687);
				match(RAT);
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(688);
				match(LPAREN);
				setState(689);
				match(RPAREN);
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(690);
				match(LPAREN);
				setState(691);
				pattern();
				setState(692);
				match(RPAREN);
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(694);
				match(LPAREN);
				setState(695);
				pattern();
				setState(696);
				match(COMMA);
				setState(697);
				pattern();
				setState(702);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(698);
					match(COMMA);
					setState(699);
					pattern();
					}
					}
					setState(704);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(705);
				match(RPAREN);
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(707);
				match(LBRACE);
				setState(709);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 2282587078787072L) != 0)) {
					{
					setState(708);
					recordPatternFieldList();
					}
				}

				setState(711);
				match(RBRACE);
				}
				break;
			case 10:
				enterOuterAlt(_localctx, 10);
				{
				setState(712);
				qualifiedName();
				setState(718);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==LPAREN) {
					{
					setState(713);
					match(LPAREN);
					setState(715);
					_errHandler.sync(this);
					_la = _input.LA(1);
					if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 26982016285147136L) != 0)) {
						{
						setState(714);
						patternList();
						}
					}

					setState(717);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterPatternList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitPatternList(this);
		}
	}

	public final PatternListContext patternList() throws RecognitionException {
		PatternListContext _localctx = new PatternListContext(_ctx, getState());
		enterRule(_localctx, 124, RULE_patternList);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(722);
			pattern();
			setState(727);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,84,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(723);
					match(COMMA);
					setState(724);
					pattern();
					}
					} 
				}
				setState(729);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,84,_ctx);
			}
			setState(731);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(730);
				match(COMMA);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterRecordPatternFieldList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitRecordPatternFieldList(this);
		}
	}

	public final RecordPatternFieldListContext recordPatternFieldList() throws RecognitionException {
		RecordPatternFieldListContext _localctx = new RecordPatternFieldListContext(_ctx, getState());
		enterRule(_localctx, 126, RULE_recordPatternFieldList);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(733);
			recordPatternField();
			setState(738);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,86,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(734);
					match(COMMA);
					setState(735);
					recordPatternField();
					}
					} 
				}
				setState(740);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,86,_ctx);
			}
			setState(742);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(741);
				match(COMMA);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterRecordPatternField(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitRecordPatternField(this);
		}
	}

	public final RecordPatternFieldContext recordPatternField() throws RecognitionException {
		RecordPatternFieldContext _localctx = new RecordPatternFieldContext(_ctx, getState());
		enterRule(_localctx, 128, RULE_recordPatternField);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(744);
			nameToken();
			setState(747);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==EQ) {
				{
				setState(745);
				match(EQ);
				setState(746);
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterQualifiedName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitQualifiedName(this);
		}
	}

	public final QualifiedNameContext qualifiedName() throws RecognitionException {
		QualifiedNameContext _localctx = new QualifiedNameContext(_ctx, getState());
		enterRule(_localctx, 130, RULE_qualifiedName);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(749);
			nameToken();
			setState(754);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,89,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(750);
					match(DOT);
					setState(751);
					nameToken();
					}
					} 
				}
				setState(756);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,89,_ctx);
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
		public TerminalNode LAST() { return getToken(MDLParser.LAST, 0); }
		public TerminalNode O() { return getToken(MDLParser.O, 0); }
		public TerminalNode P() { return getToken(MDLParser.P, 0); }
		public TerminalNode F() { return getToken(MDLParser.F, 0); }
		public NameTokenContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_nameToken; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterNameToken(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitNameToken(this);
		}
	}

	public final NameTokenContext nameToken() throws RecognitionException {
		NameTokenContext _localctx = new NameTokenContext(_ctx, getState());
		enterRule(_localctx, 132, RULE_nameToken);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(757);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 2282587078787072L) != 0)) ) {
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
		public TerminalNode WEAK_NEXT() { return getToken(MDLParser.WEAK_NEXT, 0); }
		public TerminalNode NEVER() { return getToken(MDLParser.NEVER, 0); }
		public TemporalUnaryOpContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_temporalUnaryOp; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterTemporalUnaryOp(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitTemporalUnaryOp(this);
		}
	}

	public final TemporalUnaryOpContext temporalUnaryOp() throws RecognitionException {
		TemporalUnaryOpContext _localctx = new TemporalUnaryOpContext(_ctx, getState());
		enterRule(_localctx, 134, RULE_temporalUnaryOp);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(759);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 266287972352L) != 0)) ) {
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
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).enterNewlines(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof MDLListener ) ((MDLListener)listener).exitNewlines(this);
		}
	}

	public final NewlinesContext newlines() throws RecognitionException {
		NewlinesContext _localctx = new NewlinesContext(_ctx, getState());
		enterRule(_localctx, 136, RULE_newlines);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(762); 
			_errHandler.sync(this);
			_alt = 1;
			do {
				switch (_alt) {
				case 1:
					{
					{
					setState(761);
					match(NEWLINE);
					}
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(764); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,90,_ctx);
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
		"\u0004\u0001H\u02ff\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001\u0002"+
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
		"\u0001\b\u0001\b\u0001\b\u0001\b\u0001\b\u0001\b\u0001\b\u0001\b\u0003"+
		"\b\u00d0\b\b\u0001\t\u0001\t\u0001\t\u0003\t\u00d5\b\t\u0001\t\u0001\t"+
		"\u0001\t\u0001\n\u0001\n\u0001\n\u0001\n\u0005\n\u00de\b\n\n\n\f\n\u00e1"+
		"\t\n\u0003\n\u00e3\b\n\u0001\u000b\u0001\u000b\u0001\u000b\u0001\u000b"+
		"\u0001\f\u0001\f\u0001\f\u0005\f\u00ec\b\f\n\f\f\f\u00ef\t\f\u0001\f\u0003"+
		"\f\u00f2\b\f\u0001\r\u0001\r\u0001\r\u0001\r\u0001\r\u0001\u000e\u0001"+
		"\u000e\u0001\u000e\u0005\u000e\u00fc\b\u000e\n\u000e\f\u000e\u00ff\t\u000e"+
		"\u0001\u000e\u0003\u000e\u0102\b\u000e\u0001\u000f\u0001\u000f\u0001\u000f"+
		"\u0001\u000f\u0001\u000f\u0003\u000f\u0109\b\u000f\u0001\u0010\u0001\u0010"+
		"\u0001\u0010\u0003\u0010\u010e\b\u0010\u0001\u0011\u0001\u0011\u0003\u0011"+
		"\u0112\b\u0011\u0001\u0011\u0001\u0011\u0001\u0012\u0001\u0012\u0001\u0012"+
		"\u0005\u0012\u0119\b\u0012\n\u0012\f\u0012\u011c\t\u0012\u0001\u0012\u0003"+
		"\u0012\u011f\b\u0012\u0001\u0013\u0001\u0013\u0001\u0013\u0001\u0013\u0001"+
		"\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001"+
		"\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0005\u0014\u012f\b\u0014\n"+
		"\u0014\f\u0014\u0132\t\u0014\u0001\u0014\u0001\u0014\u0003\u0014\u0136"+
		"\b\u0014\u0001\u0015\u0001\u0015\u0003\u0015\u013a\b\u0015\u0001\u0016"+
		"\u0001\u0016\u0001\u0016\u0001\u0016\u0001\u0017\u0001\u0017\u0001\u0017"+
		"\u0005\u0017\u0143\b\u0017\n\u0017\f\u0017\u0146\t\u0017\u0001\u0018\u0001"+
		"\u0018\u0001\u0018\u0003\u0018\u014b\b\u0018\u0001\u0018\u0001\u0018\u0001"+
		"\u0018\u0001\u0019\u0001\u0019\u0001\u0019\u0003\u0019\u0153\b\u0019\u0001"+
		"\u0019\u0001\u0019\u0003\u0019\u0157\b\u0019\u0001\u0019\u0001\u0019\u0001"+
		"\u0019\u0001\u0019\u0001\u0019\u0001\u0019\u0001\u001a\u0001\u001a\u0001"+
		"\u001a\u0005\u001a\u0162\b\u001a\n\u001a\f\u001a\u0165\t\u001a\u0001\u001a"+
		"\u0003\u001a\u0168\b\u001a\u0001\u001b\u0001\u001b\u0001\u001b\u0001\u001b"+
		"\u0001\u001c\u0001\u001c\u0001\u001c\u0001\u001c\u0001\u001c\u0001\u001d"+
		"\u0001\u001d\u0001\u001d\u0001\u001d\u0003\u001d\u0177\b\u001d\u0001\u001d"+
		"\u0001\u001d\u0001\u001e\u0003\u001e\u017c\b\u001e\u0001\u001e\u0001\u001e"+
		"\u0001\u001e\u0001\u001e\u0003\u001e\u0182\b\u001e\u0001\u001f\u0001\u001f"+
		"\u0001 \u0001 \u0001 \u0001 \u0001 \u0003 \u018b\b \u0001 \u0001 \u0001"+
		" \u0003 \u0190\b \u0001 \u0001 \u0001 \u0003 \u0195\b \u0001!\u0001!\u0001"+
		"\"\u0001\"\u0001\"\u0001\"\u0005\"\u019d\b\"\n\"\f\"\u01a0\t\"\u0001#"+
		"\u0001#\u0001#\u0001#\u0003#\u01a6\b#\u0001#\u0001#\u0001$\u0001$\u0001"+
		"$\u0005$\u01ad\b$\n$\f$\u01b0\t$\u0001$\u0003$\u01b3\b$\u0001$\u0003$"+
		"\u01b6\b$\u0001$\u0001$\u0003$\u01ba\b$\u0001%\u0001%\u0001%\u0003%\u01bf"+
		"\b%\u0001%\u0001%\u0001%\u0001%\u0001&\u0001&\u0001&\u0001\'\u0001\'\u0001"+
		"(\u0001(\u0005(\u01cc\b(\n(\f(\u01cf\t(\u0001)\u0001)\u0001*\u0001*\u0001"+
		"*\u0005*\u01d6\b*\n*\f*\u01d9\t*\u0001+\u0001+\u0001+\u0005+\u01de\b+"+
		"\n+\f+\u01e1\t+\u0001,\u0001,\u0001,\u0005,\u01e6\b,\n,\f,\u01e9\t,\u0001"+
		"-\u0001-\u0001-\u0005-\u01ee\b-\n-\f-\u01f1\t-\u0001.\u0001.\u0001.\u0005"+
		".\u01f6\b.\n.\f.\u01f9\t.\u0001/\u0001/\u0001/\u0005/\u01fe\b/\n/\f/\u0201"+
		"\t/\u00010\u00010\u00010\u00010\u00010\u00010\u00030\u0209\b0\u00010\u0001"+
		"0\u00030\u020d\b0\u00011\u00011\u00011\u00031\u0212\b1\u00011\u00011\u0001"+
		"1\u00031\u0217\b1\u00011\u00011\u00011\u00012\u00012\u00012\u00032\u021f"+
		"\b2\u00012\u00012\u00012\u00032\u0224\b2\u00012\u00012\u00012\u00013\u0001"+
		"3\u00013\u00013\u00013\u00014\u00014\u00014\u00034\u0231\b4\u00014\u0001"+
		"4\u00034\u0235\b4\u00014\u00054\u0238\b4\n4\f4\u023b\t4\u00014\u00034"+
		"\u023e\b4\u00014\u00014\u00014\u00014\u00034\u0244\b4\u00014\u00014\u0003"+
		"4\u0248\b4\u00014\u00054\u024b\b4\n4\f4\u024e\t4\u00014\u00034\u0251\b"+
		"4\u00034\u0253\b4\u00015\u00015\u00015\u00015\u00035\u0259\b5\u00015\u0001"+
		"5\u00015\u00016\u00016\u00056\u0260\b6\n6\f6\u0263\t6\u00017\u00017\u0001"+
		"7\u00037\u0268\b7\u00017\u00017\u00017\u00037\u026d\b7\u00018\u00018\u0003"+
		"8\u0271\b8\u00018\u00018\u00019\u00019\u00019\u00059\u0278\b9\n9\f9\u027b"+
		"\t9\u00019\u00039\u027e\b9\u0001:\u0001:\u0001:\u0001:\u0001;\u0001;\u0001"+
		";\u0001;\u0001;\u0001;\u0001;\u0001;\u0001;\u0001;\u0001;\u0001;\u0001"+
		";\u0001;\u0001;\u0001;\u0001;\u0001;\u0001;\u0001;\u0005;\u0298\b;\n;"+
		"\f;\u029b\t;\u0001;\u0001;\u0003;\u029f\b;\u0001<\u0001<\u0001<\u0005"+
		"<\u02a4\b<\n<\f<\u02a7\t<\u0001<\u0003<\u02aa\b<\u0001=\u0001=\u0001="+
		"\u0001=\u0001=\u0001=\u0001=\u0001=\u0001=\u0001=\u0001=\u0001=\u0001"+
		"=\u0001=\u0001=\u0001=\u0001=\u0005=\u02bd\b=\n=\f=\u02c0\t=\u0001=\u0001"+
		"=\u0001=\u0001=\u0003=\u02c6\b=\u0001=\u0001=\u0001=\u0001=\u0003=\u02cc"+
		"\b=\u0001=\u0003=\u02cf\b=\u0003=\u02d1\b=\u0001>\u0001>\u0001>\u0005"+
		">\u02d6\b>\n>\f>\u02d9\t>\u0001>\u0003>\u02dc\b>\u0001?\u0001?\u0001?"+
		"\u0005?\u02e1\b?\n?\f?\u02e4\t?\u0001?\u0003?\u02e7\b?\u0001@\u0001@\u0001"+
		"@\u0003@\u02ec\b@\u0001A\u0001A\u0001A\u0005A\u02f1\bA\nA\fA\u02f4\tA"+
		"\u0001B\u0001B\u0001C\u0001C\u0001D\u0004D\u02fb\bD\u000bD\fD\u02fc\u0001"+
		"D\u0000\u0000E\u0000\u0002\u0004\u0006\b\n\f\u000e\u0010\u0012\u0014\u0016"+
		"\u0018\u001a\u001c\u001e \"$&(*,.02468:<>@BDFHJLNPRTVXZ\\^`bdfhjlnprt"+
		"vxz|~\u0080\u0082\u0084\u0086\u0088\u0000\t\u0001\u0000\u000f\u0011\u0001"+
		"\u0000*,\u0001\u0000\u0012\u0013\u0001\u0000&(\u0002\u0000\u0002\u0005"+
		"AC\u0001\u0000<=\u0001\u0000>@\u0003\u0000\u001b\u001d*,33\u0001\u0000"+
		"!%\u0332\u0000\u008b\u0001\u0000\u0000\u0000\u0002\u009b\u0001\u0000\u0000"+
		"\u0000\u0004\u00a4\u0001\u0000\u0000\u0000\u0006\u00ac\u0001\u0000\u0000"+
		"\u0000\b\u00bb\u0001\u0000\u0000\u0000\n\u00be\u0001\u0000\u0000\u0000"+
		"\f\u00c1\u0001\u0000\u0000\u0000\u000e\u00c4\u0001\u0000\u0000\u0000\u0010"+
		"\u00cf\u0001\u0000\u0000\u0000\u0012\u00d1\u0001\u0000\u0000\u0000\u0014"+
		"\u00e2\u0001\u0000\u0000\u0000\u0016\u00e4\u0001\u0000\u0000\u0000\u0018"+
		"\u00e8\u0001\u0000\u0000\u0000\u001a\u00f3\u0001\u0000\u0000\u0000\u001c"+
		"\u00f8\u0001\u0000\u0000\u0000\u001e\u0108\u0001\u0000\u0000\u0000 \u010d"+
		"\u0001\u0000\u0000\u0000\"\u010f\u0001\u0000\u0000\u0000$\u0115\u0001"+
		"\u0000\u0000\u0000&\u0120\u0001\u0000\u0000\u0000(\u0135\u0001\u0000\u0000"+
		"\u0000*\u0137\u0001\u0000\u0000\u0000,\u013b\u0001\u0000\u0000\u0000."+
		"\u013f\u0001\u0000\u0000\u00000\u0147\u0001\u0000\u0000\u00002\u014f\u0001"+
		"\u0000\u0000\u00004\u015e\u0001\u0000\u0000\u00006\u0169\u0001\u0000\u0000"+
		"\u00008\u016d\u0001\u0000\u0000\u0000:\u0172\u0001\u0000\u0000\u0000<"+
		"\u017b\u0001\u0000\u0000\u0000>\u0183\u0001\u0000\u0000\u0000@\u0194\u0001"+
		"\u0000\u0000\u0000B\u0196\u0001\u0000\u0000\u0000D\u0198\u0001\u0000\u0000"+
		"\u0000F\u01a1\u0001\u0000\u0000\u0000H\u01b9\u0001\u0000\u0000\u0000J"+
		"\u01bb\u0001\u0000\u0000\u0000L\u01c4\u0001\u0000\u0000\u0000N\u01c7\u0001"+
		"\u0000\u0000\u0000P\u01c9\u0001\u0000\u0000\u0000R\u01d0\u0001\u0000\u0000"+
		"\u0000T\u01d2\u0001\u0000\u0000\u0000V\u01da\u0001\u0000\u0000\u0000X"+
		"\u01e2\u0001\u0000\u0000\u0000Z\u01ea\u0001\u0000\u0000\u0000\\\u01f2"+
		"\u0001\u0000\u0000\u0000^\u01fa\u0001\u0000\u0000\u0000`\u020c\u0001\u0000"+
		"\u0000\u0000b\u020e\u0001\u0000\u0000\u0000d\u021b\u0001\u0000\u0000\u0000"+
		"f\u0228\u0001\u0000\u0000\u0000h\u0252\u0001\u0000\u0000\u0000j\u0254"+
		"\u0001\u0000\u0000\u0000l\u025d\u0001\u0000\u0000\u0000n\u026c\u0001\u0000"+
		"\u0000\u0000p\u026e\u0001\u0000\u0000\u0000r\u0274\u0001\u0000\u0000\u0000"+
		"t\u027f\u0001\u0000\u0000\u0000v\u029e\u0001\u0000\u0000\u0000x\u02a0"+
		"\u0001\u0000\u0000\u0000z\u02d0\u0001\u0000\u0000\u0000|\u02d2\u0001\u0000"+
		"\u0000\u0000~\u02dd\u0001\u0000\u0000\u0000\u0080\u02e8\u0001\u0000\u0000"+
		"\u0000\u0082\u02ed\u0001\u0000\u0000\u0000\u0084\u02f5\u0001\u0000\u0000"+
		"\u0000\u0086\u02f7\u0001\u0000\u0000\u0000\u0088\u02fa\u0001\u0000\u0000"+
		"\u0000\u008a\u008c\u0003\u0088D\u0000\u008b\u008a\u0001\u0000\u0000\u0000"+
		"\u008b\u008c\u0001\u0000\u0000\u0000\u008c\u008d\u0001\u0000\u0000\u0000"+
		"\u008d\u008e\u0003\b\u0004\u0000\u008e\u0090\u0003\n\u0005\u0000\u008f"+
		"\u0091\u0003\u0088D\u0000\u0090\u008f\u0001\u0000\u0000\u0000\u0090\u0091"+
		"\u0001\u0000\u0000\u0000\u0091\u0095\u0001\u0000\u0000\u0000\u0092\u0094"+
		"\u0003\u0006\u0003\u0000\u0093\u0092\u0001\u0000\u0000\u0000\u0094\u0097"+
		"\u0001\u0000\u0000\u0000\u0095\u0093\u0001\u0000\u0000\u0000\u0095\u0096"+
		"\u0001\u0000\u0000\u0000\u0096\u0098\u0001\u0000\u0000\u0000\u0097\u0095"+
		"\u0001\u0000\u0000\u0000\u0098\u0099\u0005\u0000\u0000\u0001\u0099\u0001"+
		"\u0001\u0000\u0000\u0000\u009a\u009c\u0003\u0088D\u0000\u009b\u009a\u0001"+
		"\u0000\u0000\u0000\u009b\u009c\u0001\u0000\u0000\u0000\u009c\u009d\u0001"+
		"\u0000\u0000\u0000\u009d\u009f\u0003N\'\u0000\u009e\u00a0\u0003\u0088"+
		"D\u0000\u009f\u009e\u0001\u0000\u0000\u0000\u009f\u00a0\u0001\u0000\u0000"+
		"\u0000\u00a0\u00a1\u0001\u0000\u0000\u0000\u00a1\u00a2\u0005\u0000\u0000"+
		"\u0001\u00a2\u0003\u0001\u0000\u0000\u0000\u00a3\u00a5\u0003\u0088D\u0000"+
		"\u00a4\u00a3\u0001\u0000\u0000\u0000\u00a4\u00a5\u0001\u0000\u0000\u0000"+
		"\u00a5\u00a6\u0001\u0000\u0000\u0000\u00a6\u00a8\u0003 \u0010\u0000\u00a7"+
		"\u00a9\u0003\u0088D\u0000\u00a8\u00a7\u0001\u0000\u0000\u0000\u00a8\u00a9"+
		"\u0001\u0000\u0000\u0000\u00a9\u00aa\u0001\u0000\u0000\u0000\u00aa\u00ab"+
		"\u0005\u0000\u0000\u0001\u00ab\u0005\u0001\u0000\u0000\u0000\u00ac\u00b0"+
		"\u0003\b\u0004\u0000\u00ad\u00b1\u0003\f\u0006\u0000\u00ae\u00b1\u0003"+
		"\u000e\u0007\u0000\u00af\u00b1\u0003\u0010\b\u0000\u00b0\u00ad\u0001\u0000"+
		"\u0000\u0000\u00b0\u00ae\u0001\u0000\u0000\u0000\u00b0\u00af\u0001\u0000"+
		"\u0000\u0000\u00b1\u00b3\u0001\u0000\u0000\u0000\u00b2\u00b4\u0003\u0088"+
		"D\u0000\u00b3\u00b2\u0001\u0000\u0000\u0000\u00b3\u00b4\u0001\u0000\u0000"+
		"\u0000\u00b4\u0007\u0001\u0000\u0000\u0000\u00b5\u00b7\u0005-\u0000\u0000"+
		"\u00b6\u00b8\u0003\u0088D\u0000\u00b7\u00b6\u0001\u0000\u0000\u0000\u00b7"+
		"\u00b8\u0001\u0000\u0000\u0000\u00b8\u00ba\u0001\u0000\u0000\u0000\u00b9"+
		"\u00b5\u0001\u0000\u0000\u0000\u00ba\u00bd\u0001\u0000\u0000\u0000\u00bb"+
		"\u00b9\u0001\u0000\u0000\u0000\u00bb\u00bc\u0001\u0000\u0000\u0000\u00bc"+
		"\t\u0001\u0000\u0000\u0000\u00bd\u00bb\u0001\u0000\u0000\u0000\u00be\u00bf"+
		"\u0005\u0006\u0000\u0000\u00bf\u00c0\u0003\u0082A\u0000\u00c0\u000b\u0001"+
		"\u0000\u0000\u0000\u00c1\u00c2\u0005\u0007\u0000\u0000\u00c2\u00c3\u0005"+
		".\u0000\u0000\u00c3\r\u0001\u0000\u0000\u0000\u00c4\u00c5\u0005\b\u0000"+
		"\u0000\u00c5\u00c6\u0003\u0082A\u0000\u00c6\u000f\u0001\u0000\u0000\u0000"+
		"\u00c7\u00d0\u0003\u0012\t\u0000\u00c8\u00d0\u00030\u0018\u0000\u00c9"+
		"\u00d0\u00032\u0019\u0000\u00ca\u00d0\u00038\u001c\u0000\u00cb\u00d0\u0003"+
		":\u001d\u0000\u00cc\u00d0\u0003<\u001e\u0000\u00cd\u00d0\u0003D\"\u0000"+
		"\u00ce\u00d0\u0003F#\u0000\u00cf\u00c7\u0001\u0000\u0000\u0000\u00cf\u00c8"+
		"\u0001\u0000\u0000\u0000\u00cf\u00c9\u0001\u0000\u0000\u0000\u00cf\u00ca"+
		"\u0001\u0000\u0000\u0000\u00cf\u00cb\u0001\u0000\u0000\u0000\u00cf\u00cc"+
		"\u0001\u0000\u0000\u0000\u00cf\u00cd\u0001\u0000\u0000\u0000\u00cf\u00ce"+
		"\u0001\u0000\u0000\u0000\u00d0\u0011\u0001\u0000\u0000\u0000\u00d1\u00d2"+
		"\u0005\t\u0000\u0000\u00d2\u00d4\u0003\u0084B\u0000\u00d3\u00d5\u0003"+
		"\u0016\u000b\u0000\u00d4\u00d3\u0001\u0000\u0000\u0000\u00d4\u00d5\u0001"+
		"\u0000\u0000\u0000\u00d5\u00d6\u0001\u0000\u0000\u0000\u00d6\u00d7\u0005"+
		"A\u0000\u0000\u00d7\u00d8\u0003\u0014\n\u0000\u00d8\u0013\u0001\u0000"+
		"\u0000\u0000\u00d9\u00e3\u0003\"\u0011\u0000\u00da\u00df\u0003\u001a\r"+
		"\u0000\u00db\u00dc\u0005;\u0000\u0000\u00dc\u00de\u0003\u001a\r\u0000"+
		"\u00dd\u00db\u0001\u0000\u0000\u0000\u00de\u00e1\u0001\u0000\u0000\u0000"+
		"\u00df\u00dd\u0001\u0000\u0000\u0000\u00df\u00e0\u0001\u0000\u0000\u0000"+
		"\u00e0\u00e3\u0001\u0000\u0000\u0000\u00e1\u00df\u0001\u0000\u0000\u0000"+
		"\u00e2\u00d9\u0001\u0000\u0000\u0000\u00e2\u00da\u0001\u0000\u0000\u0000"+
		"\u00e3\u0015\u0001\u0000\u0000\u0000\u00e4\u00e5\u0005B\u0000\u0000\u00e5"+
		"\u00e6\u0003\u0018\f\u0000\u00e6\u00e7\u0005C\u0000\u0000\u00e7\u0017"+
		"\u0001\u0000\u0000\u0000\u00e8\u00ed\u0003\u0084B\u0000\u00e9\u00ea\u0005"+
		"8\u0000\u0000\u00ea\u00ec\u0003\u0084B\u0000\u00eb\u00e9\u0001\u0000\u0000"+
		"\u0000\u00ec\u00ef\u0001\u0000\u0000\u0000\u00ed\u00eb\u0001\u0000\u0000"+
		"\u0000\u00ed\u00ee\u0001\u0000\u0000\u0000\u00ee\u00f1\u0001\u0000\u0000"+
		"\u0000\u00ef\u00ed\u0001\u0000\u0000\u0000\u00f0\u00f2\u00058\u0000\u0000"+
		"\u00f1\u00f0\u0001\u0000\u0000\u0000\u00f1\u00f2\u0001\u0000\u0000\u0000"+
		"\u00f2\u0019\u0001\u0000\u0000\u0000\u00f3\u00f4\u0003\u0084B\u0000\u00f4"+
		"\u00f5\u00054\u0000\u0000\u00f5\u00f6\u0003\u001c\u000e\u0000\u00f6\u00f7"+
		"\u00055\u0000\u0000\u00f7\u001b\u0001\u0000\u0000\u0000\u00f8\u00fd\u0003"+
		"\u001e\u000f\u0000\u00f9\u00fa\u00058\u0000\u0000\u00fa\u00fc\u0003\u001e"+
		"\u000f\u0000\u00fb\u00f9\u0001\u0000\u0000\u0000\u00fc\u00ff\u0001\u0000"+
		"\u0000\u0000\u00fd\u00fb\u0001\u0000\u0000\u0000\u00fd\u00fe\u0001\u0000"+
		"\u0000\u0000\u00fe\u0101\u0001\u0000\u0000\u0000\u00ff\u00fd\u0001\u0000"+
		"\u0000\u0000\u0100\u0102\u00058\u0000\u0000\u0101\u0100\u0001\u0000\u0000"+
		"\u0000\u0101\u0102\u0001\u0000\u0000\u0000\u0102\u001d\u0001\u0000\u0000"+
		"\u0000\u0103\u0104\u0003\u0084B\u0000\u0104\u0105\u00059\u0000\u0000\u0105"+
		"\u0106\u0003 \u0010\u0000\u0106\u0109\u0001\u0000\u0000\u0000\u0107\u0109"+
		"\u0003 \u0010\u0000\u0108\u0103\u0001\u0000\u0000\u0000\u0108\u0107\u0001"+
		"\u0000\u0000\u0000\u0109\u001f\u0001\u0000\u0000\u0000\u010a\u010e\u0003"+
		"\"\u0011\u0000\u010b\u010e\u0003(\u0014\u0000\u010c\u010e\u0003*\u0015"+
		"\u0000\u010d\u010a\u0001\u0000\u0000\u0000\u010d\u010b\u0001\u0000\u0000"+
		"\u0000\u010d\u010c\u0001\u0000\u0000\u0000\u010e!\u0001\u0000\u0000\u0000"+
		"\u010f\u0111\u00056\u0000\u0000\u0110\u0112\u0003$\u0012\u0000\u0111\u0110"+
		"\u0001\u0000\u0000\u0000\u0111\u0112\u0001\u0000\u0000\u0000\u0112\u0113"+
		"\u0001\u0000\u0000\u0000\u0113\u0114\u00057\u0000\u0000\u0114#\u0001\u0000"+
		"\u0000\u0000\u0115\u011a\u0003&\u0013\u0000\u0116\u0117\u00058\u0000\u0000"+
		"\u0117\u0119\u0003&\u0013\u0000\u0118\u0116\u0001\u0000\u0000\u0000\u0119"+
		"\u011c\u0001\u0000\u0000\u0000\u011a\u0118\u0001\u0000\u0000\u0000\u011a"+
		"\u011b\u0001\u0000\u0000\u0000\u011b\u011e\u0001\u0000\u0000\u0000\u011c"+
		"\u011a\u0001\u0000\u0000\u0000\u011d\u011f\u00058\u0000\u0000\u011e\u011d"+
		"\u0001\u0000\u0000\u0000\u011e\u011f\u0001\u0000\u0000\u0000\u011f%\u0001"+
		"\u0000\u0000\u0000\u0120\u0121\u0003\u0084B\u0000\u0121\u0122\u00059\u0000"+
		"\u0000\u0122\u0123\u0003 \u0010\u0000\u0123\'\u0001\u0000\u0000\u0000"+
		"\u0124\u0125\u00054\u0000\u0000\u0125\u0126\u0003 \u0010\u0000\u0126\u0127"+
		"\u00055\u0000\u0000\u0127\u0136\u0001\u0000\u0000\u0000\u0128\u0129\u0005"+
		"4\u0000\u0000\u0129\u012a\u0003 \u0010\u0000\u012a\u012b\u00058\u0000"+
		"\u0000\u012b\u0130\u0003 \u0010\u0000\u012c\u012d\u00058\u0000\u0000\u012d"+
		"\u012f\u0003 \u0010\u0000\u012e\u012c\u0001\u0000\u0000\u0000\u012f\u0132"+
		"\u0001\u0000\u0000\u0000\u0130\u012e\u0001\u0000\u0000\u0000\u0130\u0131"+
		"\u0001\u0000\u0000\u0000\u0131\u0133\u0001\u0000\u0000\u0000\u0132\u0130"+
		"\u0001\u0000\u0000\u0000\u0133\u0134\u00055\u0000\u0000\u0134\u0136\u0001"+
		"\u0000\u0000\u0000\u0135\u0124\u0001\u0000\u0000\u0000\u0135\u0128\u0001"+
		"\u0000\u0000\u0000\u0136)\u0001\u0000\u0000\u0000\u0137\u0139\u0003\u0082"+
		"A\u0000\u0138\u013a\u0003,\u0016\u0000\u0139\u0138\u0001\u0000\u0000\u0000"+
		"\u0139\u013a\u0001\u0000\u0000\u0000\u013a+\u0001\u0000\u0000\u0000\u013b"+
		"\u013c\u0005B\u0000\u0000\u013c\u013d\u0003.\u0017\u0000\u013d\u013e\u0005"+
		"C\u0000\u0000\u013e-\u0001\u0000\u0000\u0000\u013f\u0144\u0003 \u0010"+
		"\u0000\u0140\u0141\u00058\u0000\u0000\u0141\u0143\u0003 \u0010\u0000\u0142"+
		"\u0140\u0001\u0000\u0000\u0000\u0143\u0146\u0001\u0000\u0000\u0000\u0144"+
		"\u0142\u0001\u0000\u0000\u0000\u0144\u0145\u0001\u0000\u0000\u0000\u0145"+
		"/\u0001\u0000\u0000\u0000\u0146\u0144\u0001\u0000\u0000\u0000\u0147\u0148"+
		"\u0005\n\u0000\u0000\u0148\u014a\u0003\u0084B\u0000\u0149\u014b\u0003"+
		"L&\u0000\u014a\u0149\u0001\u0000\u0000\u0000\u014a\u014b\u0001\u0000\u0000"+
		"\u0000\u014b\u014c\u0001\u0000\u0000\u0000\u014c\u014d\u0005A\u0000\u0000"+
		"\u014d\u014e\u0003N\'\u0000\u014e1\u0001\u0000\u0000\u0000\u014f\u0150"+
		"\u0005\u000b\u0000\u0000\u0150\u0152\u0003\u0084B\u0000\u0151\u0153\u0003"+
		"\u0016\u000b\u0000\u0152\u0151\u0001\u0000\u0000\u0000\u0152\u0153\u0001"+
		"\u0000\u0000\u0000\u0153\u0154\u0001\u0000\u0000\u0000\u0154\u0156\u0005"+
		"4\u0000\u0000\u0155\u0157\u00034\u001a\u0000\u0156\u0155\u0001\u0000\u0000"+
		"\u0000\u0156\u0157\u0001\u0000\u0000\u0000\u0157\u0158\u0001\u0000\u0000"+
		"\u0000\u0158\u0159\u00055\u0000\u0000\u0159\u015a\u0005\u0001\u0000\u0000"+
		"\u015a\u015b\u0003 \u0010\u0000\u015b\u015c\u00059\u0000\u0000\u015c\u015d"+
		"\u0003H$\u0000\u015d3\u0001\u0000\u0000\u0000\u015e\u0163\u00036\u001b"+
		"\u0000\u015f\u0160\u00058\u0000\u0000\u0160\u0162\u00036\u001b\u0000\u0161"+
		"\u015f\u0001\u0000\u0000\u0000\u0162\u0165\u0001\u0000\u0000\u0000\u0163"+
		"\u0161\u0001\u0000\u0000\u0000\u0163\u0164\u0001\u0000\u0000\u0000\u0164"+
		"\u0167\u0001\u0000\u0000\u0000\u0165\u0163\u0001\u0000\u0000\u0000\u0166"+
		"\u0168\u00058\u0000\u0000\u0167\u0166\u0001\u0000\u0000\u0000\u0167\u0168"+
		"\u0001\u0000\u0000\u0000\u01685\u0001\u0000\u0000\u0000\u0169\u016a\u0003"+
		"z=\u0000\u016a\u016b\u00059\u0000\u0000\u016b\u016c\u0003 \u0010\u0000"+
		"\u016c7\u0001\u0000\u0000\u0000\u016d\u016e\u0005\f\u0000\u0000\u016e"+
		"\u016f\u0003\u0084B\u0000\u016f\u0170\u00059\u0000\u0000\u0170\u0171\u0003"+
		" \u0010\u0000\u01719\u0001\u0000\u0000\u0000\u0172\u0173\u0005\r\u0000"+
		"\u0000\u0173\u0174\u0003\u0084B\u0000\u0174\u0176\u00054\u0000\u0000\u0175"+
		"\u0177\u0003$\u0012\u0000\u0176\u0175\u0001\u0000\u0000\u0000\u0176\u0177"+
		"\u0001\u0000\u0000\u0000\u0177\u0178\u0001\u0000\u0000\u0000\u0178\u0179"+
		"\u00055\u0000\u0000\u0179;\u0001\u0000\u0000\u0000\u017a\u017c\u0003>"+
		"\u001f\u0000\u017b\u017a\u0001\u0000\u0000\u0000\u017b\u017c\u0001\u0000"+
		"\u0000\u0000\u017c\u017d\u0001\u0000\u0000\u0000\u017d\u017e\u0005\u000e"+
		"\u0000\u0000\u017e\u0181\u0003@ \u0000\u017f\u0180\u0005)\u0000\u0000"+
		"\u0180\u0182\u0003N\'\u0000\u0181\u017f\u0001\u0000\u0000\u0000\u0181"+
		"\u0182\u0001\u0000\u0000\u0000\u0182=\u0001\u0000\u0000\u0000\u0183\u0184"+
		"\u0007\u0000\u0000\u0000\u0184?\u0001\u0000\u0000\u0000\u0185\u0186\u0003"+
		"B!\u0000\u0186\u0187\u00059\u0000\u0000\u0187\u0188\u0003N\'\u0000\u0188"+
		"\u0195\u0001\u0000\u0000\u0000\u0189\u018b\u0003B!\u0000\u018a\u0189\u0001"+
		"\u0000\u0000\u0000\u018a\u018b\u0001\u0000\u0000\u0000\u018b\u018c\u0001"+
		"\u0000\u0000\u0000\u018c\u018f\u0003\u0082A\u0000\u018d\u018e\u0005\u0019"+
		"\u0000\u0000\u018e\u0190\u0003N\'\u0000\u018f\u018d\u0001\u0000\u0000"+
		"\u0000\u018f\u0190\u0001\u0000\u0000\u0000\u0190\u0191\u0001\u0000\u0000"+
		"\u0000\u0191\u0192\u00059\u0000\u0000\u0192\u0193\u0003N\'\u0000\u0193"+
		"\u0195\u0001\u0000\u0000\u0000\u0194\u0185\u0001\u0000\u0000\u0000\u0194"+
		"\u018a\u0001\u0000\u0000\u0000\u0195A\u0001\u0000\u0000\u0000\u0196\u0197"+
		"\u0007\u0001\u0000\u0000\u0197C\u0001\u0000\u0000\u0000\u0198\u0199\u0007"+
		"\u0002\u0000\u0000\u0199\u019e\u0003\u0082A\u0000\u019a\u019b\u0005C\u0000"+
		"\u0000\u019b\u019d\u0003\u0082A\u0000\u019c\u019a\u0001\u0000\u0000\u0000"+
		"\u019d\u01a0\u0001\u0000\u0000\u0000\u019e\u019c\u0001\u0000\u0000\u0000"+
		"\u019e\u019f\u0001\u0000\u0000\u0000\u019fE\u0001\u0000\u0000\u0000\u01a0"+
		"\u019e\u0001\u0000\u0000\u0000\u01a1\u01a5\u0005\u0014\u0000\u0000\u01a2"+
		"\u01a3\u0003\u0084B\u0000\u01a3\u01a4\u0005A\u0000\u0000\u01a4\u01a6\u0001"+
		"\u0000\u0000\u0000\u01a5\u01a2\u0001\u0000\u0000\u0000\u01a5\u01a6\u0001"+
		"\u0000\u0000\u0000\u01a6\u01a7\u0001\u0000\u0000\u0000\u01a7\u01a8\u0003"+
		"N\'\u0000\u01a8G\u0001\u0000\u0000\u0000\u01a9\u01aa\u0005D\u0000\u0000"+
		"\u01aa\u01ae\u0005G\u0000\u0000\u01ab\u01ad\u0003J%\u0000\u01ac\u01ab"+
		"\u0001\u0000\u0000\u0000\u01ad\u01b0\u0001\u0000\u0000\u0000\u01ae\u01ac"+
		"\u0001\u0000\u0000\u0000\u01ae\u01af\u0001\u0000\u0000\u0000\u01af\u01b2"+
		"\u0001\u0000\u0000\u0000\u01b0\u01ae\u0001\u0000\u0000\u0000\u01b1\u01b3"+
		"\u0003N\'\u0000\u01b2\u01b1\u0001\u0000\u0000\u0000\u01b2\u01b3\u0001"+
		"\u0000\u0000\u0000\u01b3\u01b5\u0001\u0000\u0000\u0000\u01b4\u01b6\u0003"+
		"\u0088D\u0000\u01b5\u01b4\u0001\u0000\u0000\u0000\u01b5\u01b6\u0001\u0000"+
		"\u0000\u0000\u01b6\u01b7\u0001\u0000\u0000\u0000\u01b7\u01ba\u0005H\u0000"+
		"\u0000\u01b8\u01ba\u0003N\'\u0000\u01b9\u01a9\u0001\u0000\u0000\u0000"+
		"\u01b9\u01b8\u0001\u0000\u0000\u0000\u01baI\u0001\u0000\u0000\u0000\u01bb"+
		"\u01bc\u0005\n\u0000\u0000\u01bc\u01be\u0003z=\u0000\u01bd\u01bf\u0003"+
		"L&\u0000\u01be\u01bd\u0001\u0000\u0000\u0000\u01be\u01bf\u0001\u0000\u0000"+
		"\u0000\u01bf\u01c0\u0001\u0000\u0000\u0000\u01c0\u01c1\u0005A\u0000\u0000"+
		"\u01c1\u01c2\u0003N\'\u0000\u01c2\u01c3\u0003\u0088D\u0000\u01c3K\u0001"+
		"\u0000\u0000\u0000\u01c4\u01c5\u00059\u0000\u0000\u01c5\u01c6\u0003 \u0010"+
		"\u0000\u01c6M\u0001\u0000\u0000\u0000\u01c7\u01c8\u0003P(\u0000\u01c8"+
		"O\u0001\u0000\u0000\u0000\u01c9\u01cd\u0003R)\u0000\u01ca\u01cc\u0003"+
		"\u0086C\u0000\u01cb\u01ca\u0001\u0000\u0000\u0000\u01cc\u01cf\u0001\u0000"+
		"\u0000\u0000\u01cd\u01cb\u0001\u0000\u0000\u0000\u01cd\u01ce\u0001\u0000"+
		"\u0000\u0000\u01ceQ\u0001\u0000\u0000\u0000\u01cf\u01cd\u0001\u0000\u0000"+
		"\u0000\u01d0\u01d1\u0003T*\u0000\u01d1S\u0001\u0000\u0000\u0000\u01d2"+
		"\u01d7\u0003V+\u0000\u01d3\u01d4\u0005\u001f\u0000\u0000\u01d4\u01d6\u0003"+
		"V+\u0000\u01d5\u01d3\u0001\u0000\u0000\u0000\u01d6\u01d9\u0001\u0000\u0000"+
		"\u0000\u01d7\u01d5\u0001\u0000\u0000\u0000\u01d7\u01d8\u0001\u0000\u0000"+
		"\u0000\u01d8U\u0001\u0000\u0000\u0000\u01d9\u01d7\u0001\u0000\u0000\u0000"+
		"\u01da\u01df\u0003X,\u0000\u01db\u01dc\u0005\u001e\u0000\u0000\u01dc\u01de"+
		"\u0003X,\u0000\u01dd\u01db\u0001\u0000\u0000\u0000\u01de\u01e1\u0001\u0000"+
		"\u0000\u0000\u01df\u01dd\u0001\u0000\u0000\u0000\u01df\u01e0\u0001\u0000"+
		"\u0000\u0000\u01e0W\u0001\u0000\u0000\u0000\u01e1\u01df\u0001\u0000\u0000"+
		"\u0000\u01e2\u01e7\u0003Z-\u0000\u01e3\u01e4\u0007\u0003\u0000\u0000\u01e4"+
		"\u01e6\u0003Z-\u0000\u01e5\u01e3\u0001\u0000\u0000\u0000\u01e6\u01e9\u0001"+
		"\u0000\u0000\u0000\u01e7\u01e5\u0001\u0000\u0000\u0000\u01e7\u01e8\u0001"+
		"\u0000\u0000\u0000\u01e8Y\u0001\u0000\u0000\u0000\u01e9\u01e7\u0001\u0000"+
		"\u0000\u0000\u01ea\u01ef\u0003\\.\u0000\u01eb\u01ec\u0007\u0004\u0000"+
		"\u0000\u01ec\u01ee\u0003\\.\u0000\u01ed\u01eb\u0001\u0000\u0000\u0000"+
		"\u01ee\u01f1\u0001\u0000\u0000\u0000\u01ef\u01ed\u0001\u0000\u0000\u0000"+
		"\u01ef\u01f0\u0001\u0000\u0000\u0000\u01f0[\u0001\u0000\u0000\u0000\u01f1"+
		"\u01ef\u0001\u0000\u0000\u0000\u01f2\u01f7\u0003^/\u0000\u01f3\u01f4\u0007"+
		"\u0005\u0000\u0000\u01f4\u01f6\u0003^/\u0000\u01f5\u01f3\u0001\u0000\u0000"+
		"\u0000\u01f6\u01f9\u0001\u0000\u0000\u0000\u01f7\u01f5\u0001\u0000\u0000"+
		"\u0000\u01f7\u01f8\u0001\u0000\u0000\u0000\u01f8]\u0001\u0000\u0000\u0000"+
		"\u01f9\u01f7\u0001\u0000\u0000\u0000\u01fa\u01ff\u0003`0\u0000\u01fb\u01fc"+
		"\u0007\u0006\u0000\u0000\u01fc\u01fe\u0003`0\u0000\u01fd\u01fb\u0001\u0000"+
		"\u0000\u0000\u01fe\u0201\u0001\u0000\u0000\u0000\u01ff\u01fd\u0001\u0000"+
		"\u0000\u0000\u01ff\u0200\u0001\u0000\u0000\u0000\u0200_\u0001\u0000\u0000"+
		"\u0000\u0201\u01ff\u0001\u0000\u0000\u0000\u0202\u020d\u0003b1\u0000\u0203"+
		"\u020d\u0003d2\u0000\u0204\u020d\u0003f3\u0000\u0205\u0209\u0005 \u0000"+
		"\u0000\u0206\u0209\u0005=\u0000\u0000\u0207\u0209\u0003\u0086C\u0000\u0208"+
		"\u0205\u0001\u0000\u0000\u0000\u0208\u0206\u0001\u0000\u0000\u0000\u0208"+
		"\u0207\u0001\u0000\u0000\u0000\u0209\u020a\u0001\u0000\u0000\u0000\u020a"+
		"\u020d\u0003`0\u0000\u020b\u020d\u0003l6\u0000\u020c\u0202\u0001\u0000"+
		"\u0000\u0000\u020c\u0203\u0001\u0000\u0000\u0000\u020c\u0204\u0001\u0000"+
		"\u0000\u0000\u020c\u0208\u0001\u0000\u0000\u0000\u020c\u020b\u0001\u0000"+
		"\u0000\u0000\u020da\u0001\u0000\u0000\u0000\u020e\u020f\u0005\u0015\u0000"+
		"\u0000\u020f\u0211\u0003N\'\u0000\u0210\u0212\u0003\u0088D\u0000\u0211"+
		"\u0210\u0001\u0000\u0000\u0000\u0211\u0212\u0001\u0000\u0000\u0000\u0212"+
		"\u0213\u0001\u0000\u0000\u0000\u0213\u0214\u0005\u0016\u0000\u0000\u0214"+
		"\u0216\u0003N\'\u0000\u0215\u0217\u0003\u0088D\u0000\u0216\u0215\u0001"+
		"\u0000\u0000\u0000\u0216\u0217\u0001\u0000\u0000\u0000\u0217\u0218\u0001"+
		"\u0000\u0000\u0000\u0218\u0219\u0005\u0017\u0000\u0000\u0219\u021a\u0003"+
		"N\'\u0000\u021ac\u0001\u0000\u0000\u0000\u021b\u021c\u0005\n\u0000\u0000"+
		"\u021c\u021e\u0003z=\u0000\u021d\u021f\u0003L&\u0000\u021e\u021d\u0001"+
		"\u0000\u0000\u0000\u021e\u021f\u0001\u0000\u0000\u0000\u021f\u0220\u0001"+
		"\u0000\u0000\u0000\u0220\u0221\u0005A\u0000\u0000\u0221\u0223\u0003N\'"+
		"\u0000\u0222\u0224\u0003\u0088D\u0000\u0223\u0222\u0001\u0000\u0000\u0000"+
		"\u0223\u0224\u0001\u0000\u0000\u0000\u0224\u0225\u0001\u0000\u0000\u0000"+
		"\u0225\u0226\u0005\u001a\u0000\u0000\u0226\u0227\u0003N\'\u0000\u0227"+
		"e\u0001\u0000\u0000\u0000\u0228\u0229\u0005\u0018\u0000\u0000\u0229\u022a"+
		"\u0003N\'\u0000\u022a\u022b\u00059\u0000\u0000\u022b\u022c\u0003h4\u0000"+
		"\u022cg\u0001\u0000\u0000\u0000\u022d\u022e\u0005D\u0000\u0000\u022e\u0230"+
		"\u0005G\u0000\u0000\u022f\u0231\u0003\u0088D\u0000\u0230\u022f\u0001\u0000"+
		"\u0000\u0000\u0230\u0231\u0001\u0000\u0000\u0000\u0231\u0232\u0001\u0000"+
		"\u0000\u0000\u0232\u0239\u0003j5\u0000\u0233\u0235\u0003\u0088D\u0000"+
		"\u0234\u0233\u0001\u0000\u0000\u0000\u0234\u0235\u0001\u0000\u0000\u0000"+
		"\u0235\u0236\u0001\u0000\u0000\u0000\u0236\u0238\u0003j5\u0000\u0237\u0234"+
		"\u0001\u0000\u0000\u0000\u0238\u023b\u0001\u0000\u0000\u0000\u0239\u0237"+
		"\u0001\u0000\u0000\u0000\u0239\u023a\u0001\u0000\u0000\u0000\u023a\u023d"+
		"\u0001\u0000\u0000\u0000\u023b\u0239\u0001\u0000\u0000\u0000\u023c\u023e"+
		"\u0003\u0088D\u0000\u023d\u023c\u0001\u0000\u0000\u0000\u023d\u023e\u0001"+
		"\u0000\u0000\u0000\u023e\u023f\u0001\u0000\u0000\u0000\u023f\u0240\u0005"+
		"H\u0000\u0000\u0240\u0253\u0001\u0000\u0000\u0000\u0241\u0243\u0005D\u0000"+
		"\u0000\u0242\u0244\u0003\u0088D\u0000\u0243\u0242\u0001\u0000\u0000\u0000"+
		"\u0243\u0244\u0001\u0000\u0000\u0000\u0244\u0245\u0001\u0000\u0000\u0000"+
		"\u0245\u024c\u0003j5\u0000\u0246\u0248\u0003\u0088D\u0000\u0247\u0246"+
		"\u0001\u0000\u0000\u0000\u0247\u0248\u0001\u0000\u0000\u0000\u0248\u0249"+
		"\u0001\u0000\u0000\u0000\u0249\u024b\u0003j5\u0000\u024a\u0247\u0001\u0000"+
		"\u0000\u0000\u024b\u024e\u0001\u0000\u0000\u0000\u024c\u024a\u0001\u0000"+
		"\u0000\u0000\u024c\u024d\u0001\u0000\u0000\u0000\u024d\u0250\u0001\u0000"+
		"\u0000\u0000\u024e\u024c\u0001\u0000\u0000\u0000\u024f\u0251\u0003\u0088"+
		"D\u0000\u0250\u024f\u0001\u0000\u0000\u0000\u0250\u0251\u0001\u0000\u0000"+
		"\u0000\u0251\u0253\u0001\u0000\u0000\u0000\u0252\u022d\u0001\u0000\u0000"+
		"\u0000\u0252\u0241\u0001\u0000\u0000\u0000\u0253i\u0001\u0000\u0000\u0000"+
		"\u0254\u0255\u0005;\u0000\u0000\u0255\u0258\u0003z=\u0000\u0256\u0257"+
		"\u0005\u0019\u0000\u0000\u0257\u0259\u0003N\'\u0000\u0258\u0256\u0001"+
		"\u0000\u0000\u0000\u0258\u0259\u0001\u0000\u0000\u0000\u0259\u025a\u0001"+
		"\u0000\u0000\u0000\u025a\u025b\u00059\u0000\u0000\u025b\u025c\u0003H$"+
		"\u0000\u025ck\u0001\u0000\u0000\u0000\u025d\u0261\u0003v;\u0000\u025e"+
		"\u0260\u0003n7\u0000\u025f\u025e\u0001\u0000\u0000\u0000\u0260\u0263\u0001"+
		"\u0000\u0000\u0000\u0261\u025f\u0001\u0000\u0000\u0000\u0261\u0262\u0001"+
		"\u0000\u0000\u0000\u0262m\u0001\u0000\u0000\u0000\u0263\u0261\u0001\u0000"+
		"\u0000\u0000\u0264\u026d\u0003p8\u0000\u0265\u0267\u00054\u0000\u0000"+
		"\u0266\u0268\u0003x<\u0000\u0267\u0266\u0001\u0000\u0000\u0000\u0267\u0268"+
		"\u0001\u0000\u0000\u0000\u0268\u0269\u0001\u0000\u0000\u0000\u0269\u026d"+
		"\u00055\u0000\u0000\u026a\u026b\u0005:\u0000\u0000\u026b\u026d\u0003\u0084"+
		"B\u0000\u026c\u0264\u0001\u0000\u0000\u0000\u026c\u0265\u0001\u0000\u0000"+
		"\u0000\u026c\u026a\u0001\u0000\u0000\u0000\u026do\u0001\u0000\u0000\u0000"+
		"\u026e\u0270\u00056\u0000\u0000\u026f\u0271\u0003r9\u0000\u0270\u026f"+
		"\u0001\u0000\u0000\u0000\u0270\u0271\u0001\u0000\u0000\u0000\u0271\u0272"+
		"\u0001\u0000\u0000\u0000\u0272\u0273\u00057\u0000\u0000\u0273q\u0001\u0000"+
		"\u0000\u0000\u0274\u0279\u0003t:\u0000\u0275\u0276\u00058\u0000\u0000"+
		"\u0276\u0278\u0003t:\u0000\u0277\u0275\u0001\u0000\u0000\u0000\u0278\u027b"+
		"\u0001\u0000\u0000\u0000\u0279\u0277\u0001\u0000\u0000\u0000\u0279\u027a"+
		"\u0001\u0000\u0000\u0000\u027a\u027d\u0001\u0000\u0000\u0000\u027b\u0279"+
		"\u0001\u0000\u0000\u0000\u027c\u027e\u00058\u0000\u0000\u027d\u027c\u0001"+
		"\u0000\u0000\u0000\u027d\u027e\u0001\u0000\u0000\u0000\u027es\u0001\u0000"+
		"\u0000\u0000\u027f\u0280\u0003\u0084B\u0000\u0280\u0281\u0005A\u0000\u0000"+
		"\u0281\u0282\u0003N\'\u0000\u0282u\u0001\u0000\u0000\u0000\u0283\u029f"+
		"\u0005.\u0000\u0000\u0284\u029f\u00051\u0000\u0000\u0285\u029f\u00050"+
		"\u0000\u0000\u0286\u029f\u0005/\u0000\u0000\u0287\u029f\u0005\u001b\u0000"+
		"\u0000\u0288\u029f\u0005\u001c\u0000\u0000\u0289\u029f\u0005\u001d\u0000"+
		"\u0000\u028a\u029f\u0003\u0082A\u0000\u028b\u028c\u00054\u0000\u0000\u028c"+
		"\u029f\u00055\u0000\u0000\u028d\u028e\u00054\u0000\u0000\u028e\u028f\u0003"+
		"N\'\u0000\u028f\u0290\u00055\u0000\u0000\u0290\u029f\u0001\u0000\u0000"+
		"\u0000\u0291\u0292\u00054\u0000\u0000\u0292\u0293\u0003N\'\u0000\u0293"+
		"\u0294\u00058\u0000\u0000\u0294\u0299\u0003N\'\u0000\u0295\u0296\u0005"+
		"8\u0000\u0000\u0296\u0298\u0003N\'\u0000\u0297\u0295\u0001\u0000\u0000"+
		"\u0000\u0298\u029b\u0001\u0000\u0000\u0000\u0299\u0297\u0001\u0000\u0000"+
		"\u0000\u0299\u029a\u0001\u0000\u0000\u0000\u029a\u029c\u0001\u0000\u0000"+
		"\u0000\u029b\u0299\u0001\u0000\u0000\u0000\u029c\u029d\u00055\u0000\u0000"+
		"\u029d\u029f\u0001\u0000\u0000\u0000\u029e\u0283\u0001\u0000\u0000\u0000"+
		"\u029e\u0284\u0001\u0000\u0000\u0000\u029e\u0285\u0001\u0000\u0000\u0000"+
		"\u029e\u0286\u0001\u0000\u0000\u0000\u029e\u0287\u0001\u0000\u0000\u0000"+
		"\u029e\u0288\u0001\u0000\u0000\u0000\u029e\u0289\u0001\u0000\u0000\u0000"+
		"\u029e\u028a\u0001\u0000\u0000\u0000\u029e\u028b\u0001\u0000\u0000\u0000"+
		"\u029e\u028d\u0001\u0000\u0000\u0000\u029e\u0291\u0001\u0000\u0000\u0000"+
		"\u029fw\u0001\u0000\u0000\u0000\u02a0\u02a5\u0003N\'\u0000\u02a1\u02a2"+
		"\u00058\u0000\u0000\u02a2\u02a4\u0003N\'\u0000\u02a3\u02a1\u0001\u0000"+
		"\u0000\u0000\u02a4\u02a7\u0001\u0000\u0000\u0000\u02a5\u02a3\u0001\u0000"+
		"\u0000\u0000\u02a5\u02a6\u0001\u0000\u0000\u0000\u02a6\u02a9\u0001\u0000"+
		"\u0000\u0000\u02a7\u02a5\u0001\u0000\u0000\u0000\u02a8\u02aa\u00058\u0000"+
		"\u0000\u02a9\u02a8\u0001\u0000\u0000\u0000\u02a9\u02aa\u0001\u0000\u0000"+
		"\u0000\u02aay\u0001\u0000\u0000\u0000\u02ab\u02d1\u00052\u0000\u0000\u02ac"+
		"\u02d1\u0005.\u0000\u0000\u02ad\u02d1\u00051\u0000\u0000\u02ae\u02d1\u0005"+
		"0\u0000\u0000\u02af\u02d1\u0005/\u0000\u0000\u02b0\u02b1\u00054\u0000"+
		"\u0000\u02b1\u02d1\u00055\u0000\u0000\u02b2\u02b3\u00054\u0000\u0000\u02b3"+
		"\u02b4\u0003z=\u0000\u02b4\u02b5\u00055\u0000\u0000\u02b5\u02d1\u0001"+
		"\u0000\u0000\u0000\u02b6\u02b7\u00054\u0000\u0000\u02b7\u02b8\u0003z="+
		"\u0000\u02b8\u02b9\u00058\u0000\u0000\u02b9\u02be\u0003z=\u0000\u02ba"+
		"\u02bb\u00058\u0000\u0000\u02bb\u02bd\u0003z=\u0000\u02bc\u02ba\u0001"+
		"\u0000\u0000\u0000\u02bd\u02c0\u0001\u0000\u0000\u0000\u02be\u02bc\u0001"+
		"\u0000\u0000\u0000\u02be\u02bf\u0001\u0000\u0000\u0000\u02bf\u02c1\u0001"+
		"\u0000\u0000\u0000\u02c0\u02be\u0001\u0000\u0000\u0000\u02c1\u02c2\u0005"+
		"5\u0000\u0000\u02c2\u02d1\u0001\u0000\u0000\u0000\u02c3\u02c5\u00056\u0000"+
		"\u0000\u02c4\u02c6\u0003~?\u0000\u02c5\u02c4\u0001\u0000\u0000\u0000\u02c5"+
		"\u02c6\u0001\u0000\u0000\u0000\u02c6\u02c7\u0001\u0000\u0000\u0000\u02c7"+
		"\u02d1\u00057\u0000\u0000\u02c8\u02ce\u0003\u0082A\u0000\u02c9\u02cb\u0005"+
		"4\u0000\u0000\u02ca\u02cc\u0003|>\u0000\u02cb\u02ca\u0001\u0000\u0000"+
		"\u0000\u02cb\u02cc\u0001\u0000\u0000\u0000\u02cc\u02cd\u0001\u0000\u0000"+
		"\u0000\u02cd\u02cf\u00055\u0000\u0000\u02ce\u02c9\u0001\u0000\u0000\u0000"+
		"\u02ce\u02cf\u0001\u0000\u0000\u0000\u02cf\u02d1\u0001\u0000\u0000\u0000"+
		"\u02d0\u02ab\u0001\u0000\u0000\u0000\u02d0\u02ac\u0001\u0000\u0000\u0000"+
		"\u02d0\u02ad\u0001\u0000\u0000\u0000\u02d0\u02ae\u0001\u0000\u0000\u0000"+
		"\u02d0\u02af\u0001\u0000\u0000\u0000\u02d0\u02b0\u0001\u0000\u0000\u0000"+
		"\u02d0\u02b2\u0001\u0000\u0000\u0000\u02d0\u02b6\u0001\u0000\u0000\u0000"+
		"\u02d0\u02c3\u0001\u0000\u0000\u0000\u02d0\u02c8\u0001\u0000\u0000\u0000"+
		"\u02d1{\u0001\u0000\u0000\u0000\u02d2\u02d7\u0003z=\u0000\u02d3\u02d4"+
		"\u00058\u0000\u0000\u02d4\u02d6\u0003z=\u0000\u02d5\u02d3\u0001\u0000"+
		"\u0000\u0000\u02d6\u02d9\u0001\u0000\u0000\u0000\u02d7\u02d5\u0001\u0000"+
		"\u0000\u0000\u02d7\u02d8\u0001\u0000\u0000\u0000\u02d8\u02db\u0001\u0000"+
		"\u0000\u0000\u02d9\u02d7\u0001\u0000\u0000\u0000\u02da\u02dc\u00058\u0000"+
		"\u0000\u02db\u02da\u0001\u0000\u0000\u0000\u02db\u02dc\u0001\u0000\u0000"+
		"\u0000\u02dc}\u0001\u0000\u0000\u0000\u02dd\u02e2\u0003\u0080@\u0000\u02de"+
		"\u02df\u00058\u0000\u0000\u02df\u02e1\u0003\u0080@\u0000\u02e0\u02de\u0001"+
		"\u0000\u0000\u0000\u02e1\u02e4\u0001\u0000\u0000\u0000\u02e2\u02e0\u0001"+
		"\u0000\u0000\u0000\u02e2\u02e3\u0001\u0000\u0000\u0000\u02e3\u02e6\u0001"+
		"\u0000\u0000\u0000\u02e4\u02e2\u0001\u0000\u0000\u0000\u02e5\u02e7\u0005"+
		"8\u0000\u0000\u02e6\u02e5\u0001\u0000\u0000\u0000\u02e6\u02e7\u0001\u0000"+
		"\u0000\u0000\u02e7\u007f\u0001\u0000\u0000\u0000\u02e8\u02eb\u0003\u0084"+
		"B\u0000\u02e9\u02ea\u0005A\u0000\u0000\u02ea\u02ec\u0003z=\u0000\u02eb"+
		"\u02e9\u0001\u0000\u0000\u0000\u02eb\u02ec\u0001\u0000\u0000\u0000\u02ec"+
		"\u0081\u0001\u0000\u0000\u0000\u02ed\u02f2\u0003\u0084B\u0000\u02ee\u02ef"+
		"\u0005:\u0000\u0000\u02ef\u02f1\u0003\u0084B\u0000\u02f0\u02ee\u0001\u0000"+
		"\u0000\u0000\u02f1\u02f4\u0001\u0000\u0000\u0000\u02f2\u02f0\u0001\u0000"+
		"\u0000\u0000\u02f2\u02f3\u0001\u0000\u0000\u0000\u02f3\u0083\u0001\u0000"+
		"\u0000\u0000\u02f4\u02f2\u0001\u0000\u0000\u0000\u02f5\u02f6\u0007\u0007"+
		"\u0000\u0000\u02f6\u0085\u0001\u0000\u0000\u0000\u02f7\u02f8\u0007\b\u0000"+
		"\u0000\u02f8\u0087\u0001\u0000\u0000\u0000\u02f9\u02fb\u0005D\u0000\u0000"+
		"\u02fa\u02f9\u0001\u0000\u0000\u0000\u02fb\u02fc\u0001\u0000\u0000\u0000"+
		"\u02fc\u02fa\u0001\u0000\u0000\u0000\u02fc\u02fd\u0001\u0000\u0000\u0000"+
		"\u02fd\u0089\u0001\u0000\u0000\u0000[\u008b\u0090\u0095\u009b\u009f\u00a4"+
		"\u00a8\u00b0\u00b3\u00b7\u00bb\u00cf\u00d4\u00df\u00e2\u00ed\u00f1\u00fd"+
		"\u0101\u0108\u010d\u0111\u011a\u011e\u0130\u0135\u0139\u0144\u014a\u0152"+
		"\u0156\u0163\u0167\u0176\u017b\u0181\u018a\u018f\u0194\u019e\u01a5\u01ae"+
		"\u01b2\u01b5\u01b9\u01be\u01cd\u01d7\u01df\u01e7\u01ef\u01f7\u01ff\u0208"+
		"\u020c\u0211\u0216\u021e\u0223\u0230\u0234\u0239\u023d\u0243\u0247\u024c"+
		"\u0250\u0252\u0258\u0261\u0267\u026c\u0270\u0279\u027d\u0299\u029e\u02a5"+
		"\u02a9\u02be\u02c5\u02cb\u02ce\u02d0\u02d7\u02db\u02e2\u02e6\u02eb\u02f2"+
		"\u02fc";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}