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
		VAL=10, LET=11, FUNC=12, ENTITY=13, EVENT=14, RULE=15, STRICT=16, DEFEASIBLE=17, 
		DEFEATER=18, PRIORITY=19, OVERRIDE=20, FACT=21, ALIGN=22, TO=23, EQUIVALENT=24, 
		BROADER=25, NARROWER=26, RELATED=27, IF=28, THEN=29, ELSE=30, CASE=31, 
		WHEN=32, IN=33, TRUE=34, FALSE=35, LAST=36, AND=37, OR=38, NOT=39, ALWAYS=40, 
		EVENTUALLY=41, NEXT=42, WEAK_NEXT=43, NEVER=44, UNTIL=45, RELEASE=46, 
		WEAK_UNTIL=47, OTHERWISE=48, O=49, P=50, F=51, ANNOT=52, STRING=53, RAT=54, 
		DECIMAL=55, INT=56, UNDERSCORE=57, IDENT=58, LPAREN=59, RPAREN=60, LBRACE=61, 
		RBRACE=62, COMMA=63, COLON=64, DOT=65, BAR=66, PLUS=67, MINUS=68, STAR=69, 
		SLASH=70, PERCENT=71, EQ=72, LT=73, GT=74, NEWLINE=75, COMMENT=76, WS=77, 
		INDENT=78, DEDENT=79;
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
		RULE_alignDecl = 36, RULE_alignTarget = 37, RULE_iriLiteral = 38, RULE_alignKind = 39, 
		RULE_block = 40, RULE_blockLetStmt = 41, RULE_typeAnnotation = 42, RULE_expr = 43, 
		RULE_temporalPostfix = 44, RULE_implication = 45, RULE_orExpr = 46, RULE_andExpr = 47, 
		RULE_temporalBinary = 48, RULE_comparison = 49, RULE_additive = 50, RULE_multiplicative = 51, 
		RULE_unary = 52, RULE_ifExpr = 53, RULE_letExpr = 54, RULE_matchExpr = 55, 
		RULE_caseBody = 56, RULE_caseArm = 57, RULE_postfix = 58, RULE_postfixSuffix = 59, 
		RULE_recordConstructorFields = 60, RULE_recordConstructorFieldList = 61, 
		RULE_recordConstructorField = 62, RULE_primary = 63, RULE_exprList = 64, 
		RULE_pattern = 65, RULE_patternList = 66, RULE_recordPatternFieldList = 67, 
		RULE_recordPatternField = 68, RULE_qualifiedName = 69, RULE_nameToken = 70, 
		RULE_temporalUnaryOp = 71, RULE_newlines = 72;
	private static String[] makeRuleNames() {
		return new String[] {
			"program", "exprOnly", "typeExprOnly", "topItem", "annotations", "moduleDecl", 
			"importDecl", "openDecl", "declaration", "typeDecl", "typeDefinition", 
			"typeParams", "nameList", "variant", "variantFieldList", "variantField", 
			"typeExpr", "recordType", "typeFieldList", "typeField", "tupleOrParenType", 
			"typeRef", "typeArgs", "typeExprList", "valueDecl", "funcDecl", "paramList", 
			"param", "entityDecl", "eventDecl", "ruleDecl", "ruleStrength", "ruleBody", 
			"deonticMod", "priorityDecl", "factDecl", "alignDecl", "alignTarget", 
			"iriLiteral", "alignKind", "block", "blockLetStmt", "typeAnnotation", 
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
			"'open'", "'type'", "'val'", "'let'", "'func'", "'entity'", "'event'", 
			"'rule'", "'strict'", "'defeasible'", "'defeater'", "'priority'", "'override'", 
			"'fact'", "'align'", "'to'", "'equivalent'", "'broader'", "'narrower'", 
			"'related'", "'if'", "'then'", "'else'", "'case'", "'when'", "'in'", 
			"'true'", "'false'", "'last'", "'and'", "'or'", "'not'", "'always'", 
			"'eventually'", "'next'", "'weak_next'", "'never'", "'until'", "'release'", 
			"'weak_until'", "'otherwise'", "'O'", "'P'", "'F'", null, null, null, 
			null, null, "'_'", null, "'('", "')'", "'{'", "'}'", "','", "':'", "'.'", 
			"'|'", "'+'", "'-'", "'*'", "'/'", "'%'", "'='", "'<'", "'>'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, "ARROW", "LE", "GE", "NE", "EQEQ", "MODULE", "IMPORT", "OPEN", 
			"TYPE", "VAL", "LET", "FUNC", "ENTITY", "EVENT", "RULE", "STRICT", "DEFEASIBLE", 
			"DEFEATER", "PRIORITY", "OVERRIDE", "FACT", "ALIGN", "TO", "EQUIVALENT", 
			"BROADER", "NARROWER", "RELATED", "IF", "THEN", "ELSE", "CASE", "WHEN", 
			"IN", "TRUE", "FALSE", "LAST", "AND", "OR", "NOT", "ALWAYS", "EVENTUALLY", 
			"NEXT", "WEAK_NEXT", "NEVER", "UNTIL", "RELEASE", "WEAK_UNTIL", "OTHERWISE", 
			"O", "P", "F", "ANNOT", "STRING", "RAT", "DECIMAL", "INT", "UNDERSCORE", 
			"IDENT", "LPAREN", "RPAREN", "LBRACE", "RBRACE", "COMMA", "COLON", "DOT", 
			"BAR", "PLUS", "MINUS", "STAR", "SLASH", "PERCENT", "EQ", "LT", "GT", 
			"NEWLINE", "COMMENT", "WS", "INDENT", "DEDENT"
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
			setState(147);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(146);
				newlines();
				}
			}

			setState(149);
			annotations();
			setState(150);
			moduleDecl();
			setState(152);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(151);
				newlines();
				}
			}

			setState(157);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & 4503599635758976L) != 0)) {
				{
				{
				setState(154);
				topItem();
				}
				}
				setState(159);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(160);
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
			setState(163);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(162);
				newlines();
				}
			}

			setState(165);
			expr();
			setState(167);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(166);
				newlines();
				}
			}

			setState(169);
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
			setState(172);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(171);
				newlines();
				}
			}

			setState(174);
			typeExpr();
			setState(176);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(175);
				newlines();
				}
			}

			setState(178);
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
			setState(180);
			annotations();
			setState(184);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case IMPORT:
				{
				setState(181);
				importDecl();
				}
				break;
			case OPEN:
				{
				setState(182);
				openDecl();
				}
				break;
			case TYPE:
			case VAL:
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
			case ALIGN:
				{
				setState(183);
				declaration();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(187);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(186);
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
			setState(195);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==ANNOT) {
				{
				{
				setState(189);
				match(ANNOT);
				setState(191);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(190);
					newlines();
					}
				}

				}
				}
				setState(197);
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
			setState(198);
			match(MODULE);
			setState(199);
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
			setState(201);
			match(IMPORT);
			setState(202);
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
			setState(204);
			match(OPEN);
			setState(205);
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
		public AlignDeclContext alignDecl() {
			return getRuleContext(AlignDeclContext.class,0);
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
			setState(216);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case TYPE:
				enterOuterAlt(_localctx, 1);
				{
				setState(207);
				typeDecl();
				}
				break;
			case VAL:
			case LET:
				enterOuterAlt(_localctx, 2);
				{
				setState(208);
				valueDecl();
				}
				break;
			case FUNC:
				enterOuterAlt(_localctx, 3);
				{
				setState(209);
				funcDecl();
				}
				break;
			case ENTITY:
				enterOuterAlt(_localctx, 4);
				{
				setState(210);
				entityDecl();
				}
				break;
			case EVENT:
				enterOuterAlt(_localctx, 5);
				{
				setState(211);
				eventDecl();
				}
				break;
			case RULE:
			case STRICT:
			case DEFEASIBLE:
			case DEFEATER:
				enterOuterAlt(_localctx, 6);
				{
				setState(212);
				ruleDecl();
				}
				break;
			case PRIORITY:
			case OVERRIDE:
				enterOuterAlt(_localctx, 7);
				{
				setState(213);
				priorityDecl();
				}
				break;
			case FACT:
				enterOuterAlt(_localctx, 8);
				{
				setState(214);
				factDecl();
				}
				break;
			case ALIGN:
				enterOuterAlt(_localctx, 9);
				{
				setState(215);
				alignDecl();
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
			setState(218);
			match(TYPE);
			setState(219);
			nameToken();
			setState(221);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LT) {
				{
				setState(220);
				typeParams();
				}
			}

			setState(223);
			match(EQ);
			setState(224);
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
	}

	public final TypeDefinitionContext typeDefinition() throws RecognitionException {
		TypeDefinitionContext _localctx = new TypeDefinitionContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_typeDefinition);
		int _la;
		try {
			setState(235);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case LBRACE:
				enterOuterAlt(_localctx, 1);
				{
				setState(226);
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
				setState(227);
				variant();
				setState(232);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==BAR) {
					{
					{
					setState(228);
					match(BAR);
					setState(229);
					variant();
					}
					}
					setState(234);
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
	}

	public final TypeParamsContext typeParams() throws RecognitionException {
		TypeParamsContext _localctx = new TypeParamsContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_typeParams);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(237);
			match(LT);
			setState(238);
			nameList();
			setState(239);
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
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(241);
			nameToken();
			setState(246);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,15,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(242);
					match(COMMA);
					setState(243);
					nameToken();
					}
					} 
				}
				setState(248);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,15,_ctx);
			}
			setState(250);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(249);
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
	}

	public final VariantContext variant() throws RecognitionException {
		VariantContext _localctx = new VariantContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_variant);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(252);
			nameToken();
			setState(253);
			match(LPAREN);
			setState(254);
			variantFieldList();
			setState(255);
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
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(257);
			variantField();
			setState(262);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,17,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(258);
					match(COMMA);
					setState(259);
					variantField();
					}
					} 
				}
				setState(264);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,17,_ctx);
			}
			setState(266);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(265);
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
	}

	public final VariantFieldContext variantField() throws RecognitionException {
		VariantFieldContext _localctx = new VariantFieldContext(_ctx, getState());
		enterRule(_localctx, 30, RULE_variantField);
		try {
			setState(273);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,19,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(268);
				nameToken();
				setState(269);
				match(COLON);
				setState(270);
				typeExpr();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(272);
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
			setState(278);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case LBRACE:
				enterOuterAlt(_localctx, 1);
				{
				setState(275);
				recordType();
				}
				break;
			case LPAREN:
				enterOuterAlt(_localctx, 2);
				{
				setState(276);
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
				setState(277);
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
			setState(280);
			match(LBRACE);
			setState(282);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 292171146084745216L) != 0)) {
				{
				setState(281);
				typeFieldList();
				}
			}

			setState(284);
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
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(286);
			typeField();
			setState(291);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,22,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(287);
					match(COMMA);
					setState(288);
					typeField();
					}
					} 
				}
				setState(293);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,22,_ctx);
			}
			setState(295);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(294);
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
	}

	public final TypeFieldContext typeField() throws RecognitionException {
		TypeFieldContext _localctx = new TypeFieldContext(_ctx, getState());
		enterRule(_localctx, 38, RULE_typeField);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(297);
			nameToken();
			setState(298);
			match(COLON);
			setState(299);
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
			setState(318);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,25,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(301);
				match(LPAREN);
				setState(302);
				typeExpr();
				setState(303);
				match(RPAREN);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(305);
				match(LPAREN);
				setState(306);
				typeExpr();
				setState(307);
				match(COMMA);
				setState(308);
				typeExpr();
				setState(313);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(309);
					match(COMMA);
					setState(310);
					typeExpr();
					}
					}
					setState(315);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(316);
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
			setState(320);
			qualifiedName();
			setState(322);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LT) {
				{
				setState(321);
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
			setState(324);
			match(LT);
			setState(325);
			typeExprList();
			setState(326);
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
			setState(328);
			typeExpr();
			setState(333);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(329);
				match(COMMA);
				setState(330);
				typeExpr();
				}
				}
				setState(335);
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
		public NameTokenContext nameToken() {
			return getRuleContext(NameTokenContext.class,0);
		}
		public TerminalNode EQ() { return getToken(MDLParser.EQ, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode VAL() { return getToken(MDLParser.VAL, 0); }
		public TerminalNode LET() { return getToken(MDLParser.LET, 0); }
		public TypeAnnotationContext typeAnnotation() {
			return getRuleContext(TypeAnnotationContext.class,0);
		}
		public ValueDeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_valueDecl; }
	}

	public final ValueDeclContext valueDecl() throws RecognitionException {
		ValueDeclContext _localctx = new ValueDeclContext(_ctx, getState());
		enterRule(_localctx, 48, RULE_valueDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(336);
			_la = _input.LA(1);
			if ( !(_la==VAL || _la==LET) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			setState(337);
			nameToken();
			setState(339);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COLON) {
				{
				setState(338);
				typeAnnotation();
				}
			}

			setState(341);
			match(EQ);
			setState(342);
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
	}

	public final FuncDeclContext funcDecl() throws RecognitionException {
		FuncDeclContext _localctx = new FuncDeclContext(_ctx, getState());
		enterRule(_localctx, 50, RULE_funcDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(344);
			match(FUNC);
			setState(345);
			nameToken();
			setState(347);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LT) {
				{
				setState(346);
				typeParams();
				}
			}

			setState(349);
			match(LPAREN);
			setState(351);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 3453698084498833408L) != 0)) {
				{
				setState(350);
				paramList();
				}
			}

			setState(353);
			match(RPAREN);
			setState(354);
			match(ARROW);
			setState(355);
			typeExpr();
			setState(356);
			match(COLON);
			setState(357);
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
		enterRule(_localctx, 52, RULE_paramList);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(359);
			param();
			setState(364);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,31,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(360);
					match(COMMA);
					setState(361);
					param();
					}
					} 
				}
				setState(366);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,31,_ctx);
			}
			setState(368);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(367);
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
	}

	public final ParamContext param() throws RecognitionException {
		ParamContext _localctx = new ParamContext(_ctx, getState());
		enterRule(_localctx, 54, RULE_param);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(370);
			pattern();
			setState(371);
			match(COLON);
			setState(372);
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
		enterRule(_localctx, 56, RULE_entityDecl);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(374);
			match(ENTITY);
			setState(375);
			nameToken();
			setState(376);
			match(COLON);
			setState(377);
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
	}

	public final EventDeclContext eventDecl() throws RecognitionException {
		EventDeclContext _localctx = new EventDeclContext(_ctx, getState());
		enterRule(_localctx, 58, RULE_eventDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(379);
			match(EVENT);
			setState(380);
			nameToken();
			setState(386);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LPAREN) {
				{
				setState(381);
				match(LPAREN);
				setState(383);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 292171146084745216L) != 0)) {
					{
					setState(382);
					typeFieldList();
					}
				}

				setState(385);
				match(RPAREN);
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
		enterRule(_localctx, 60, RULE_ruleDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(389);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 458752L) != 0)) {
				{
				setState(388);
				ruleStrength();
				}
			}

			setState(391);
			match(RULE);
			setState(392);
			ruleBody();
			setState(395);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==OTHERWISE) {
				{
				setState(393);
				match(OTHERWISE);
				setState(394);
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
		enterRule(_localctx, 62, RULE_ruleStrength);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(397);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 458752L) != 0)) ) {
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
	}

	public final RuleBodyContext ruleBody() throws RecognitionException {
		RuleBodyContext _localctx = new RuleBodyContext(_ctx, getState());
		enterRule(_localctx, 64, RULE_ruleBody);
		int _la;
		try {
			setState(414);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,39,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(399);
				deonticMod();
				setState(400);
				match(COLON);
				setState(401);
				expr();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(404);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,37,_ctx) ) {
				case 1:
					{
					setState(403);
					deonticMod();
					}
					break;
				}
				setState(406);
				qualifiedName();
				setState(409);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==WHEN) {
					{
					setState(407);
					match(WHEN);
					setState(408);
					expr();
					}
				}

				setState(411);
				match(COLON);
				setState(412);
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
	}

	public final DeonticModContext deonticMod() throws RecognitionException {
		DeonticModContext _localctx = new DeonticModContext(_ctx, getState());
		enterRule(_localctx, 66, RULE_deonticMod);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(416);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 3940649673949184L) != 0)) ) {
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
	}

	public final PriorityDeclContext priorityDecl() throws RecognitionException {
		PriorityDeclContext _localctx = new PriorityDeclContext(_ctx, getState());
		enterRule(_localctx, 68, RULE_priorityDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(418);
			_la = _input.LA(1);
			if ( !(_la==PRIORITY || _la==OVERRIDE) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			setState(419);
			qualifiedName();
			setState(424);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==GT) {
				{
				{
				setState(420);
				match(GT);
				setState(421);
				qualifiedName();
				}
				}
				setState(426);
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
		enterRule(_localctx, 70, RULE_factDecl);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(427);
			match(FACT);
			setState(431);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,41,_ctx) ) {
			case 1:
				{
				setState(428);
				nameToken();
				setState(429);
				match(EQ);
				}
				break;
			}
			setState(433);
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
	public static class AlignDeclContext extends ParserRuleContext {
		public TerminalNode ALIGN() { return getToken(MDLParser.ALIGN, 0); }
		public QualifiedNameContext qualifiedName() {
			return getRuleContext(QualifiedNameContext.class,0);
		}
		public TerminalNode TO() { return getToken(MDLParser.TO, 0); }
		public AlignTargetContext alignTarget() {
			return getRuleContext(AlignTargetContext.class,0);
		}
		public AlignKindContext alignKind() {
			return getRuleContext(AlignKindContext.class,0);
		}
		public AlignDeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_alignDecl; }
	}

	public final AlignDeclContext alignDecl() throws RecognitionException {
		AlignDeclContext _localctx = new AlignDeclContext(_ctx, getState());
		enterRule(_localctx, 72, RULE_alignDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(435);
			match(ALIGN);
			setState(436);
			qualifiedName();
			setState(437);
			match(TO);
			setState(438);
			alignTarget();
			setState(440);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 251658240L) != 0)) {
				{
				setState(439);
				alignKind();
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
	public static class AlignTargetContext extends ParserRuleContext {
		public QualifiedNameContext qualifiedName() {
			return getRuleContext(QualifiedNameContext.class,0);
		}
		public TerminalNode STRING() { return getToken(MDLParser.STRING, 0); }
		public IriLiteralContext iriLiteral() {
			return getRuleContext(IriLiteralContext.class,0);
		}
		public AlignTargetContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_alignTarget; }
	}

	public final AlignTargetContext alignTarget() throws RecognitionException {
		AlignTargetContext _localctx = new AlignTargetContext(_ctx, getState());
		enterRule(_localctx, 74, RULE_alignTarget);
		try {
			setState(445);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case TRUE:
			case FALSE:
			case LAST:
			case O:
			case P:
			case F:
			case IDENT:
				enterOuterAlt(_localctx, 1);
				{
				setState(442);
				qualifiedName();
				}
				break;
			case STRING:
				enterOuterAlt(_localctx, 2);
				{
				setState(443);
				match(STRING);
				}
				break;
			case LT:
				enterOuterAlt(_localctx, 3);
				{
				setState(444);
				iriLiteral();
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
	public static class IriLiteralContext extends ParserRuleContext {
		public TerminalNode LT() { return getToken(MDLParser.LT, 0); }
		public List<TerminalNode> GT() { return getTokens(MDLParser.GT); }
		public TerminalNode GT(int i) {
			return getToken(MDLParser.GT, i);
		}
		public IriLiteralContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_iriLiteral; }
	}

	public final IriLiteralContext iriLiteral() throws RecognitionException {
		IriLiteralContext _localctx = new IriLiteralContext(_ctx, getState());
		enterRule(_localctx, 76, RULE_iriLiteral);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(447);
			match(LT);
			setState(451);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & -2L) != 0) || ((((_la - 64)) & ~0x3f) == 0 && ((1L << (_la - 64)) & 64511L) != 0)) {
				{
				{
				setState(448);
				_la = _input.LA(1);
				if ( _la <= 0 || (_la==GT) ) {
				_errHandler.recoverInline(this);
				}
				else {
					if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
				}
				}
				setState(453);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(454);
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
	public static class AlignKindContext extends ParserRuleContext {
		public TerminalNode EQUIVALENT() { return getToken(MDLParser.EQUIVALENT, 0); }
		public TerminalNode BROADER() { return getToken(MDLParser.BROADER, 0); }
		public TerminalNode NARROWER() { return getToken(MDLParser.NARROWER, 0); }
		public TerminalNode RELATED() { return getToken(MDLParser.RELATED, 0); }
		public AlignKindContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_alignKind; }
	}

	public final AlignKindContext alignKind() throws RecognitionException {
		AlignKindContext _localctx = new AlignKindContext(_ctx, getState());
		enterRule(_localctx, 78, RULE_alignKind);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(456);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 251658240L) != 0)) ) {
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
		enterRule(_localctx, 80, RULE_block);
		int _la;
		try {
			int _alt;
			setState(474);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case NEWLINE:
				enterOuterAlt(_localctx, 1);
				{
				setState(458);
				match(NEWLINE);
				setState(459);
				match(INDENT);
				setState(463);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,45,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(460);
						blockLetStmt();
						}
						} 
					}
					setState(465);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,45,_ctx);
				}
				setState(467);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (((((_la - 11)) & ~0x3f) == 0 && ((1L << (_la - 11)) & 144605312355270657L) != 0)) {
					{
					setState(466);
					expr();
					}
				}

				setState(470);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(469);
					newlines();
					}
				}

				setState(472);
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
				setState(473);
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
	}

	public final BlockLetStmtContext blockLetStmt() throws RecognitionException {
		BlockLetStmtContext _localctx = new BlockLetStmtContext(_ctx, getState());
		enterRule(_localctx, 82, RULE_blockLetStmt);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(476);
			match(LET);
			setState(477);
			pattern();
			setState(479);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COLON) {
				{
				setState(478);
				typeAnnotation();
				}
			}

			setState(481);
			match(EQ);
			setState(482);
			expr();
			setState(483);
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
	}

	public final TypeAnnotationContext typeAnnotation() throws RecognitionException {
		TypeAnnotationContext _localctx = new TypeAnnotationContext(_ctx, getState());
		enterRule(_localctx, 84, RULE_typeAnnotation);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(485);
			match(COLON);
			setState(486);
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
	}

	public final ExprContext expr() throws RecognitionException {
		ExprContext _localctx = new ExprContext(_ctx, getState());
		enterRule(_localctx, 86, RULE_expr);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(488);
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
		enterRule(_localctx, 88, RULE_temporalPostfix);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(490);
			implication();
			setState(494);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,50,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(491);
					temporalUnaryOp();
					}
					} 
				}
				setState(496);
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
	public static class ImplicationContext extends ParserRuleContext {
		public OrExprContext orExpr() {
			return getRuleContext(OrExprContext.class,0);
		}
		public ImplicationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_implication; }
	}

	public final ImplicationContext implication() throws RecognitionException {
		ImplicationContext _localctx = new ImplicationContext(_ctx, getState());
		enterRule(_localctx, 90, RULE_implication);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(497);
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
	}

	public final OrExprContext orExpr() throws RecognitionException {
		OrExprContext _localctx = new OrExprContext(_ctx, getState());
		enterRule(_localctx, 92, RULE_orExpr);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(499);
			andExpr();
			setState(504);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,51,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(500);
					match(OR);
					setState(501);
					andExpr();
					}
					} 
				}
				setState(506);
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
		enterRule(_localctx, 94, RULE_andExpr);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(507);
			temporalBinary();
			setState(512);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,52,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(508);
					match(AND);
					setState(509);
					temporalBinary();
					}
					} 
				}
				setState(514);
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
	}

	public final TemporalBinaryContext temporalBinary() throws RecognitionException {
		TemporalBinaryContext _localctx = new TemporalBinaryContext(_ctx, getState());
		enterRule(_localctx, 96, RULE_temporalBinary);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(515);
			comparison();
			setState(520);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,53,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(516);
					_la = _input.LA(1);
					if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 246290604621824L) != 0)) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(517);
					comparison();
					}
					} 
				}
				setState(522);
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
	}

	public final ComparisonContext comparison() throws RecognitionException {
		ComparisonContext _localctx = new ComparisonContext(_ctx, getState());
		enterRule(_localctx, 98, RULE_comparison);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(523);
			additive();
			setState(528);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,54,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(524);
					_la = _input.LA(1);
					if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 60L) != 0) || ((((_la - 72)) & ~0x3f) == 0 && ((1L << (_la - 72)) & 7L) != 0)) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(525);
					additive();
					}
					} 
				}
				setState(530);
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
		enterRule(_localctx, 100, RULE_additive);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(531);
			multiplicative();
			setState(536);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,55,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(532);
					_la = _input.LA(1);
					if ( !(_la==PLUS || _la==MINUS) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(533);
					multiplicative();
					}
					} 
				}
				setState(538);
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
		enterRule(_localctx, 102, RULE_multiplicative);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(539);
			unary();
			setState(544);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,56,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(540);
					_la = _input.LA(1);
					if ( !(((((_la - 69)) & ~0x3f) == 0 && ((1L << (_la - 69)) & 7L) != 0)) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(541);
					unary();
					}
					} 
				}
				setState(546);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,56,_ctx);
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
	}

	public final UnaryContext unary() throws RecognitionException {
		UnaryContext _localctx = new UnaryContext(_ctx, getState());
		enterRule(_localctx, 104, RULE_unary);
		try {
			setState(557);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case IF:
				enterOuterAlt(_localctx, 1);
				{
				setState(547);
				ifExpr();
				}
				break;
			case LET:
				enterOuterAlt(_localctx, 2);
				{
				setState(548);
				letExpr();
				}
				break;
			case CASE:
				enterOuterAlt(_localctx, 3);
				{
				setState(549);
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
				setState(553);
				_errHandler.sync(this);
				switch (_input.LA(1)) {
				case NOT:
					{
					setState(550);
					match(NOT);
					}
					break;
				case MINUS:
					{
					setState(551);
					match(MINUS);
					}
					break;
				case ALWAYS:
				case EVENTUALLY:
				case NEXT:
				case WEAK_NEXT:
				case NEVER:
					{
					setState(552);
					temporalUnaryOp();
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(555);
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
				setState(556);
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
		enterRule(_localctx, 106, RULE_ifExpr);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(559);
			match(IF);
			setState(560);
			expr();
			setState(562);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(561);
				newlines();
				}
			}

			setState(564);
			match(THEN);
			setState(565);
			expr();
			setState(567);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(566);
				newlines();
				}
			}

			setState(569);
			match(ELSE);
			setState(570);
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
	}

	public final LetExprContext letExpr() throws RecognitionException {
		LetExprContext _localctx = new LetExprContext(_ctx, getState());
		enterRule(_localctx, 108, RULE_letExpr);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(572);
			match(LET);
			setState(573);
			pattern();
			setState(575);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COLON) {
				{
				setState(574);
				typeAnnotation();
				}
			}

			setState(577);
			match(EQ);
			setState(578);
			expr();
			setState(580);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(579);
				newlines();
				}
			}

			setState(582);
			match(IN);
			setState(583);
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
	}

	public final MatchExprContext matchExpr() throws RecognitionException {
		MatchExprContext _localctx = new MatchExprContext(_ctx, getState());
		enterRule(_localctx, 110, RULE_matchExpr);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(585);
			match(CASE);
			setState(586);
			expr();
			setState(587);
			match(COLON);
			setState(588);
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
		enterRule(_localctx, 112, RULE_caseBody);
		int _la;
		try {
			int _alt;
			setState(627);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,71,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(590);
				match(NEWLINE);
				setState(591);
				match(INDENT);
				setState(593);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(592);
					newlines();
					}
				}

				setState(595);
				caseArm();
				setState(602);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,65,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(597);
						_errHandler.sync(this);
						_la = _input.LA(1);
						if (_la==NEWLINE) {
							{
							setState(596);
							newlines();
							}
						}

						setState(599);
						caseArm();
						}
						} 
					}
					setState(604);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,65,_ctx);
				}
				setState(606);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(605);
					newlines();
					}
				}

				setState(608);
				match(DEDENT);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(610);
				match(NEWLINE);
				setState(612);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(611);
					newlines();
					}
				}

				setState(614);
				caseArm();
				setState(621);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,69,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(616);
						_errHandler.sync(this);
						_la = _input.LA(1);
						if (_la==NEWLINE) {
							{
							setState(615);
							newlines();
							}
						}

						setState(618);
						caseArm();
						}
						} 
					}
					setState(623);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,69,_ctx);
				}
				setState(625);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,70,_ctx) ) {
				case 1:
					{
					setState(624);
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
		enterRule(_localctx, 114, RULE_caseArm);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(629);
			match(BAR);
			setState(630);
			pattern();
			setState(633);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==WHEN) {
				{
				setState(631);
				match(WHEN);
				setState(632);
				expr();
				}
			}

			setState(635);
			match(COLON);
			setState(636);
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
		enterRule(_localctx, 116, RULE_postfix);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(638);
			primary();
			setState(642);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (((((_la - 59)) & ~0x3f) == 0 && ((1L << (_la - 59)) & 69L) != 0)) {
				{
				{
				setState(639);
				postfixSuffix();
				}
				}
				setState(644);
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
		enterRule(_localctx, 118, RULE_postfixSuffix);
		int _la;
		try {
			setState(653);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case LBRACE:
				enterOuterAlt(_localctx, 1);
				{
				setState(645);
				recordConstructorFields();
				}
				break;
			case LPAREN:
				enterOuterAlt(_localctx, 2);
				{
				setState(646);
				match(LPAREN);
				setState(648);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (((((_la - 11)) & ~0x3f) == 0 && ((1L << (_la - 11)) & 144605312355270657L) != 0)) {
					{
					setState(647);
					exprList();
					}
				}

				setState(650);
				match(RPAREN);
				}
				break;
			case DOT:
				enterOuterAlt(_localctx, 3);
				{
				setState(651);
				match(DOT);
				setState(652);
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
		enterRule(_localctx, 120, RULE_recordConstructorFields);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(655);
			match(LBRACE);
			setState(657);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 292171146084745216L) != 0)) {
				{
				setState(656);
				recordConstructorFieldList();
				}
			}

			setState(659);
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
		enterRule(_localctx, 122, RULE_recordConstructorFieldList);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(661);
			recordConstructorField();
			setState(666);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,77,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(662);
					match(COMMA);
					setState(663);
					recordConstructorField();
					}
					} 
				}
				setState(668);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,77,_ctx);
			}
			setState(670);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(669);
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
	}

	public final RecordConstructorFieldContext recordConstructorField() throws RecognitionException {
		RecordConstructorFieldContext _localctx = new RecordConstructorFieldContext(_ctx, getState());
		enterRule(_localctx, 124, RULE_recordConstructorField);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(672);
			nameToken();
			setState(673);
			match(EQ);
			setState(674);
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
	}

	public final PrimaryContext primary() throws RecognitionException {
		PrimaryContext _localctx = new PrimaryContext(_ctx, getState());
		enterRule(_localctx, 126, RULE_primary);
		int _la;
		try {
			setState(703);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,80,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(676);
				match(STRING);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(677);
				match(INT);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(678);
				match(DECIMAL);
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(679);
				match(RAT);
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(680);
				match(TRUE);
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(681);
				match(FALSE);
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(682);
				match(LAST);
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(683);
				qualifiedName();
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(684);
				match(LPAREN);
				setState(685);
				match(RPAREN);
				}
				break;
			case 10:
				enterOuterAlt(_localctx, 10);
				{
				setState(686);
				match(LPAREN);
				setState(687);
				expr();
				setState(688);
				match(RPAREN);
				}
				break;
			case 11:
				enterOuterAlt(_localctx, 11);
				{
				setState(690);
				match(LPAREN);
				setState(691);
				expr();
				setState(692);
				match(COMMA);
				setState(693);
				expr();
				setState(698);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(694);
					match(COMMA);
					setState(695);
					expr();
					}
					}
					setState(700);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(701);
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
		enterRule(_localctx, 128, RULE_exprList);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(705);
			expr();
			setState(710);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,81,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(706);
					match(COMMA);
					setState(707);
					expr();
					}
					} 
				}
				setState(712);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,81,_ctx);
			}
			setState(714);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(713);
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
	}

	public final PatternContext pattern() throws RecognitionException {
		PatternContext _localctx = new PatternContext(_ctx, getState());
		enterRule(_localctx, 130, RULE_pattern);
		int _la;
		try {
			setState(753);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,87,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(716);
				match(UNDERSCORE);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(717);
				match(STRING);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(718);
				match(INT);
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(719);
				match(DECIMAL);
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(720);
				match(RAT);
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(721);
				match(LPAREN);
				setState(722);
				match(RPAREN);
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(723);
				match(LPAREN);
				setState(724);
				pattern();
				setState(725);
				match(RPAREN);
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(727);
				match(LPAREN);
				setState(728);
				pattern();
				setState(729);
				match(COMMA);
				setState(730);
				pattern();
				setState(735);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(731);
					match(COMMA);
					setState(732);
					pattern();
					}
					}
					setState(737);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(738);
				match(RPAREN);
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(740);
				match(LBRACE);
				setState(742);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 292171146084745216L) != 0)) {
					{
					setState(741);
					recordPatternFieldList();
					}
				}

				setState(744);
				match(RBRACE);
				}
				break;
			case 10:
				enterOuterAlt(_localctx, 10);
				{
				setState(745);
				qualifiedName();
				setState(751);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==LPAREN) {
					{
					setState(746);
					match(LPAREN);
					setState(748);
					_errHandler.sync(this);
					_la = _input.LA(1);
					if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 3453698084498833408L) != 0)) {
						{
						setState(747);
						patternList();
						}
					}

					setState(750);
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
		enterRule(_localctx, 132, RULE_patternList);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(755);
			pattern();
			setState(760);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,88,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(756);
					match(COMMA);
					setState(757);
					pattern();
					}
					} 
				}
				setState(762);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,88,_ctx);
			}
			setState(764);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(763);
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
	}

	public final RecordPatternFieldListContext recordPatternFieldList() throws RecognitionException {
		RecordPatternFieldListContext _localctx = new RecordPatternFieldListContext(_ctx, getState());
		enterRule(_localctx, 134, RULE_recordPatternFieldList);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(766);
			recordPatternField();
			setState(771);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,90,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(767);
					match(COMMA);
					setState(768);
					recordPatternField();
					}
					} 
				}
				setState(773);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,90,_ctx);
			}
			setState(775);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(774);
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
	}

	public final RecordPatternFieldContext recordPatternField() throws RecognitionException {
		RecordPatternFieldContext _localctx = new RecordPatternFieldContext(_ctx, getState());
		enterRule(_localctx, 136, RULE_recordPatternField);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(777);
			nameToken();
			setState(780);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==EQ) {
				{
				setState(778);
				match(EQ);
				setState(779);
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
		enterRule(_localctx, 138, RULE_qualifiedName);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(782);
			nameToken();
			setState(787);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,93,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(783);
					match(DOT);
					setState(784);
					nameToken();
					}
					} 
				}
				setState(789);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,93,_ctx);
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
	}

	public final NameTokenContext nameToken() throws RecognitionException {
		NameTokenContext _localctx = new NameTokenContext(_ctx, getState());
		enterRule(_localctx, 140, RULE_nameToken);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(790);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 292171146084745216L) != 0)) ) {
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
	}

	public final TemporalUnaryOpContext temporalUnaryOp() throws RecognitionException {
		TemporalUnaryOpContext _localctx = new TemporalUnaryOpContext(_ctx, getState());
		enterRule(_localctx, 142, RULE_temporalUnaryOp);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(792);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 34084860461056L) != 0)) ) {
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
		enterRule(_localctx, 144, RULE_newlines);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(795); 
			_errHandler.sync(this);
			_alt = 1;
			do {
				switch (_alt) {
				case 1:
					{
					{
					setState(794);
					match(NEWLINE);
					}
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(797); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,94,_ctx);
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
		"\u0004\u0001O\u0320\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001\u0002"+
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
		"A\u0007A\u0002B\u0007B\u0002C\u0007C\u0002D\u0007D\u0002E\u0007E\u0002"+
		"F\u0007F\u0002G\u0007G\u0002H\u0007H\u0001\u0000\u0003\u0000\u0094\b\u0000"+
		"\u0001\u0000\u0001\u0000\u0001\u0000\u0003\u0000\u0099\b\u0000\u0001\u0000"+
		"\u0005\u0000\u009c\b\u0000\n\u0000\f\u0000\u009f\t\u0000\u0001\u0000\u0001"+
		"\u0000\u0001\u0001\u0003\u0001\u00a4\b\u0001\u0001\u0001\u0001\u0001\u0003"+
		"\u0001\u00a8\b\u0001\u0001\u0001\u0001\u0001\u0001\u0002\u0003\u0002\u00ad"+
		"\b\u0002\u0001\u0002\u0001\u0002\u0003\u0002\u00b1\b\u0002\u0001\u0002"+
		"\u0001\u0002\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0003\u0003"+
		"\u00b9\b\u0003\u0001\u0003\u0003\u0003\u00bc\b\u0003\u0001\u0004\u0001"+
		"\u0004\u0003\u0004\u00c0\b\u0004\u0005\u0004\u00c2\b\u0004\n\u0004\f\u0004"+
		"\u00c5\t\u0004\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0006\u0001\u0006"+
		"\u0001\u0006\u0001\u0007\u0001\u0007\u0001\u0007\u0001\b\u0001\b\u0001"+
		"\b\u0001\b\u0001\b\u0001\b\u0001\b\u0001\b\u0001\b\u0003\b\u00d9\b\b\u0001"+
		"\t\u0001\t\u0001\t\u0003\t\u00de\b\t\u0001\t\u0001\t\u0001\t\u0001\n\u0001"+
		"\n\u0001\n\u0001\n\u0005\n\u00e7\b\n\n\n\f\n\u00ea\t\n\u0003\n\u00ec\b"+
		"\n\u0001\u000b\u0001\u000b\u0001\u000b\u0001\u000b\u0001\f\u0001\f\u0001"+
		"\f\u0005\f\u00f5\b\f\n\f\f\f\u00f8\t\f\u0001\f\u0003\f\u00fb\b\f\u0001"+
		"\r\u0001\r\u0001\r\u0001\r\u0001\r\u0001\u000e\u0001\u000e\u0001\u000e"+
		"\u0005\u000e\u0105\b\u000e\n\u000e\f\u000e\u0108\t\u000e\u0001\u000e\u0003"+
		"\u000e\u010b\b\u000e\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f\u0001"+
		"\u000f\u0003\u000f\u0112\b\u000f\u0001\u0010\u0001\u0010\u0001\u0010\u0003"+
		"\u0010\u0117\b\u0010\u0001\u0011\u0001\u0011\u0003\u0011\u011b\b\u0011"+
		"\u0001\u0011\u0001\u0011\u0001\u0012\u0001\u0012\u0001\u0012\u0005\u0012"+
		"\u0122\b\u0012\n\u0012\f\u0012\u0125\t\u0012\u0001\u0012\u0003\u0012\u0128"+
		"\b\u0012\u0001\u0013\u0001\u0013\u0001\u0013\u0001\u0013\u0001\u0014\u0001"+
		"\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001"+
		"\u0014\u0001\u0014\u0001\u0014\u0005\u0014\u0138\b\u0014\n\u0014\f\u0014"+
		"\u013b\t\u0014\u0001\u0014\u0001\u0014\u0003\u0014\u013f\b\u0014\u0001"+
		"\u0015\u0001\u0015\u0003\u0015\u0143\b\u0015\u0001\u0016\u0001\u0016\u0001"+
		"\u0016\u0001\u0016\u0001\u0017\u0001\u0017\u0001\u0017\u0005\u0017\u014c"+
		"\b\u0017\n\u0017\f\u0017\u014f\t\u0017\u0001\u0018\u0001\u0018\u0001\u0018"+
		"\u0003\u0018\u0154\b\u0018\u0001\u0018\u0001\u0018\u0001\u0018\u0001\u0019"+
		"\u0001\u0019\u0001\u0019\u0003\u0019\u015c\b\u0019\u0001\u0019\u0001\u0019"+
		"\u0003\u0019\u0160\b\u0019\u0001\u0019\u0001\u0019\u0001\u0019\u0001\u0019"+
		"\u0001\u0019\u0001\u0019\u0001\u001a\u0001\u001a\u0001\u001a\u0005\u001a"+
		"\u016b\b\u001a\n\u001a\f\u001a\u016e\t\u001a\u0001\u001a\u0003\u001a\u0171"+
		"\b\u001a\u0001\u001b\u0001\u001b\u0001\u001b\u0001\u001b\u0001\u001c\u0001"+
		"\u001c\u0001\u001c\u0001\u001c\u0001\u001c\u0001\u001d\u0001\u001d\u0001"+
		"\u001d\u0001\u001d\u0003\u001d\u0180\b\u001d\u0001\u001d\u0003\u001d\u0183"+
		"\b\u001d\u0001\u001e\u0003\u001e\u0186\b\u001e\u0001\u001e\u0001\u001e"+
		"\u0001\u001e\u0001\u001e\u0003\u001e\u018c\b\u001e\u0001\u001f\u0001\u001f"+
		"\u0001 \u0001 \u0001 \u0001 \u0001 \u0003 \u0195\b \u0001 \u0001 \u0001"+
		" \u0003 \u019a\b \u0001 \u0001 \u0001 \u0003 \u019f\b \u0001!\u0001!\u0001"+
		"\"\u0001\"\u0001\"\u0001\"\u0005\"\u01a7\b\"\n\"\f\"\u01aa\t\"\u0001#"+
		"\u0001#\u0001#\u0001#\u0003#\u01b0\b#\u0001#\u0001#\u0001$\u0001$\u0001"+
		"$\u0001$\u0001$\u0003$\u01b9\b$\u0001%\u0001%\u0001%\u0003%\u01be\b%\u0001"+
		"&\u0001&\u0005&\u01c2\b&\n&\f&\u01c5\t&\u0001&\u0001&\u0001\'\u0001\'"+
		"\u0001(\u0001(\u0001(\u0005(\u01ce\b(\n(\f(\u01d1\t(\u0001(\u0003(\u01d4"+
		"\b(\u0001(\u0003(\u01d7\b(\u0001(\u0001(\u0003(\u01db\b(\u0001)\u0001"+
		")\u0001)\u0003)\u01e0\b)\u0001)\u0001)\u0001)\u0001)\u0001*\u0001*\u0001"+
		"*\u0001+\u0001+\u0001,\u0001,\u0005,\u01ed\b,\n,\f,\u01f0\t,\u0001-\u0001"+
		"-\u0001.\u0001.\u0001.\u0005.\u01f7\b.\n.\f.\u01fa\t.\u0001/\u0001/\u0001"+
		"/\u0005/\u01ff\b/\n/\f/\u0202\t/\u00010\u00010\u00010\u00050\u0207\b0"+
		"\n0\f0\u020a\t0\u00011\u00011\u00011\u00051\u020f\b1\n1\f1\u0212\t1\u0001"+
		"2\u00012\u00012\u00052\u0217\b2\n2\f2\u021a\t2\u00013\u00013\u00013\u0005"+
		"3\u021f\b3\n3\f3\u0222\t3\u00014\u00014\u00014\u00014\u00014\u00014\u0003"+
		"4\u022a\b4\u00014\u00014\u00034\u022e\b4\u00015\u00015\u00015\u00035\u0233"+
		"\b5\u00015\u00015\u00015\u00035\u0238\b5\u00015\u00015\u00015\u00016\u0001"+
		"6\u00016\u00036\u0240\b6\u00016\u00016\u00016\u00036\u0245\b6\u00016\u0001"+
		"6\u00016\u00017\u00017\u00017\u00017\u00017\u00018\u00018\u00018\u0003"+
		"8\u0252\b8\u00018\u00018\u00038\u0256\b8\u00018\u00058\u0259\b8\n8\f8"+
		"\u025c\t8\u00018\u00038\u025f\b8\u00018\u00018\u00018\u00018\u00038\u0265"+
		"\b8\u00018\u00018\u00038\u0269\b8\u00018\u00058\u026c\b8\n8\f8\u026f\t"+
		"8\u00018\u00038\u0272\b8\u00038\u0274\b8\u00019\u00019\u00019\u00019\u0003"+
		"9\u027a\b9\u00019\u00019\u00019\u0001:\u0001:\u0005:\u0281\b:\n:\f:\u0284"+
		"\t:\u0001;\u0001;\u0001;\u0003;\u0289\b;\u0001;\u0001;\u0001;\u0003;\u028e"+
		"\b;\u0001<\u0001<\u0003<\u0292\b<\u0001<\u0001<\u0001=\u0001=\u0001=\u0005"+
		"=\u0299\b=\n=\f=\u029c\t=\u0001=\u0003=\u029f\b=\u0001>\u0001>\u0001>"+
		"\u0001>\u0001?\u0001?\u0001?\u0001?\u0001?\u0001?\u0001?\u0001?\u0001"+
		"?\u0001?\u0001?\u0001?\u0001?\u0001?\u0001?\u0001?\u0001?\u0001?\u0001"+
		"?\u0001?\u0005?\u02b9\b?\n?\f?\u02bc\t?\u0001?\u0001?\u0003?\u02c0\b?"+
		"\u0001@\u0001@\u0001@\u0005@\u02c5\b@\n@\f@\u02c8\t@\u0001@\u0003@\u02cb"+
		"\b@\u0001A\u0001A\u0001A\u0001A\u0001A\u0001A\u0001A\u0001A\u0001A\u0001"+
		"A\u0001A\u0001A\u0001A\u0001A\u0001A\u0001A\u0001A\u0005A\u02de\bA\nA"+
		"\fA\u02e1\tA\u0001A\u0001A\u0001A\u0001A\u0003A\u02e7\bA\u0001A\u0001"+
		"A\u0001A\u0001A\u0003A\u02ed\bA\u0001A\u0003A\u02f0\bA\u0003A\u02f2\b"+
		"A\u0001B\u0001B\u0001B\u0005B\u02f7\bB\nB\fB\u02fa\tB\u0001B\u0003B\u02fd"+
		"\bB\u0001C\u0001C\u0001C\u0005C\u0302\bC\nC\fC\u0305\tC\u0001C\u0003C"+
		"\u0308\bC\u0001D\u0001D\u0001D\u0003D\u030d\bD\u0001E\u0001E\u0001E\u0005"+
		"E\u0312\bE\nE\fE\u0315\tE\u0001F\u0001F\u0001G\u0001G\u0001H\u0004H\u031c"+
		"\bH\u000bH\fH\u031d\u0001H\u0000\u0000I\u0000\u0002\u0004\u0006\b\n\f"+
		"\u000e\u0010\u0012\u0014\u0016\u0018\u001a\u001c\u001e \"$&(*,.02468:"+
		"<>@BDFHJLNPRTVXZ\\^`bdfhjlnprtvxz|~\u0080\u0082\u0084\u0086\u0088\u008a"+
		"\u008c\u008e\u0090\u0000\f\u0001\u0000\n\u000b\u0001\u0000\u0010\u0012"+
		"\u0001\u000013\u0001\u0000\u0013\u0014\u0001\u0000JJ\u0001\u0000\u0018"+
		"\u001b\u0001\u0000-/\u0002\u0000\u0002\u0005HJ\u0001\u0000CD\u0001\u0000"+
		"EG\u0003\u0000\"$13::\u0001\u0000(,\u0355\u0000\u0093\u0001\u0000\u0000"+
		"\u0000\u0002\u00a3\u0001\u0000\u0000\u0000\u0004\u00ac\u0001\u0000\u0000"+
		"\u0000\u0006\u00b4\u0001\u0000\u0000\u0000\b\u00c3\u0001\u0000\u0000\u0000"+
		"\n\u00c6\u0001\u0000\u0000\u0000\f\u00c9\u0001\u0000\u0000\u0000\u000e"+
		"\u00cc\u0001\u0000\u0000\u0000\u0010\u00d8\u0001\u0000\u0000\u0000\u0012"+
		"\u00da\u0001\u0000\u0000\u0000\u0014\u00eb\u0001\u0000\u0000\u0000\u0016"+
		"\u00ed\u0001\u0000\u0000\u0000\u0018\u00f1\u0001\u0000\u0000\u0000\u001a"+
		"\u00fc\u0001\u0000\u0000\u0000\u001c\u0101\u0001\u0000\u0000\u0000\u001e"+
		"\u0111\u0001\u0000\u0000\u0000 \u0116\u0001\u0000\u0000\u0000\"\u0118"+
		"\u0001\u0000\u0000\u0000$\u011e\u0001\u0000\u0000\u0000&\u0129\u0001\u0000"+
		"\u0000\u0000(\u013e\u0001\u0000\u0000\u0000*\u0140\u0001\u0000\u0000\u0000"+
		",\u0144\u0001\u0000\u0000\u0000.\u0148\u0001\u0000\u0000\u00000\u0150"+
		"\u0001\u0000\u0000\u00002\u0158\u0001\u0000\u0000\u00004\u0167\u0001\u0000"+
		"\u0000\u00006\u0172\u0001\u0000\u0000\u00008\u0176\u0001\u0000\u0000\u0000"+
		":\u017b\u0001\u0000\u0000\u0000<\u0185\u0001\u0000\u0000\u0000>\u018d"+
		"\u0001\u0000\u0000\u0000@\u019e\u0001\u0000\u0000\u0000B\u01a0\u0001\u0000"+
		"\u0000\u0000D\u01a2\u0001\u0000\u0000\u0000F\u01ab\u0001\u0000\u0000\u0000"+
		"H\u01b3\u0001\u0000\u0000\u0000J\u01bd\u0001\u0000\u0000\u0000L\u01bf"+
		"\u0001\u0000\u0000\u0000N\u01c8\u0001\u0000\u0000\u0000P\u01da\u0001\u0000"+
		"\u0000\u0000R\u01dc\u0001\u0000\u0000\u0000T\u01e5\u0001\u0000\u0000\u0000"+
		"V\u01e8\u0001\u0000\u0000\u0000X\u01ea\u0001\u0000\u0000\u0000Z\u01f1"+
		"\u0001\u0000\u0000\u0000\\\u01f3\u0001\u0000\u0000\u0000^\u01fb\u0001"+
		"\u0000\u0000\u0000`\u0203\u0001\u0000\u0000\u0000b\u020b\u0001\u0000\u0000"+
		"\u0000d\u0213\u0001\u0000\u0000\u0000f\u021b\u0001\u0000\u0000\u0000h"+
		"\u022d\u0001\u0000\u0000\u0000j\u022f\u0001\u0000\u0000\u0000l\u023c\u0001"+
		"\u0000\u0000\u0000n\u0249\u0001\u0000\u0000\u0000p\u0273\u0001\u0000\u0000"+
		"\u0000r\u0275\u0001\u0000\u0000\u0000t\u027e\u0001\u0000\u0000\u0000v"+
		"\u028d\u0001\u0000\u0000\u0000x\u028f\u0001\u0000\u0000\u0000z\u0295\u0001"+
		"\u0000\u0000\u0000|\u02a0\u0001\u0000\u0000\u0000~\u02bf\u0001\u0000\u0000"+
		"\u0000\u0080\u02c1\u0001\u0000\u0000\u0000\u0082\u02f1\u0001\u0000\u0000"+
		"\u0000\u0084\u02f3\u0001\u0000\u0000\u0000\u0086\u02fe\u0001\u0000\u0000"+
		"\u0000\u0088\u0309\u0001\u0000\u0000\u0000\u008a\u030e\u0001\u0000\u0000"+
		"\u0000\u008c\u0316\u0001\u0000\u0000\u0000\u008e\u0318\u0001\u0000\u0000"+
		"\u0000\u0090\u031b\u0001\u0000\u0000\u0000\u0092\u0094\u0003\u0090H\u0000"+
		"\u0093\u0092\u0001\u0000\u0000\u0000\u0093\u0094\u0001\u0000\u0000\u0000"+
		"\u0094\u0095\u0001\u0000\u0000\u0000\u0095\u0096\u0003\b\u0004\u0000\u0096"+
		"\u0098\u0003\n\u0005\u0000\u0097\u0099\u0003\u0090H\u0000\u0098\u0097"+
		"\u0001\u0000\u0000\u0000\u0098\u0099\u0001\u0000\u0000\u0000\u0099\u009d"+
		"\u0001\u0000\u0000\u0000\u009a\u009c\u0003\u0006\u0003\u0000\u009b\u009a"+
		"\u0001\u0000\u0000\u0000\u009c\u009f\u0001\u0000\u0000\u0000\u009d\u009b"+
		"\u0001\u0000\u0000\u0000\u009d\u009e\u0001\u0000\u0000\u0000\u009e\u00a0"+
		"\u0001\u0000\u0000\u0000\u009f\u009d\u0001\u0000\u0000\u0000\u00a0\u00a1"+
		"\u0005\u0000\u0000\u0001\u00a1\u0001\u0001\u0000\u0000\u0000\u00a2\u00a4"+
		"\u0003\u0090H\u0000\u00a3\u00a2\u0001\u0000\u0000\u0000\u00a3\u00a4\u0001"+
		"\u0000\u0000\u0000\u00a4\u00a5\u0001\u0000\u0000\u0000\u00a5\u00a7\u0003"+
		"V+\u0000\u00a6\u00a8\u0003\u0090H\u0000\u00a7\u00a6\u0001\u0000\u0000"+
		"\u0000\u00a7\u00a8\u0001\u0000\u0000\u0000\u00a8\u00a9\u0001\u0000\u0000"+
		"\u0000\u00a9\u00aa\u0005\u0000\u0000\u0001\u00aa\u0003\u0001\u0000\u0000"+
		"\u0000\u00ab\u00ad\u0003\u0090H\u0000\u00ac\u00ab\u0001\u0000\u0000\u0000"+
		"\u00ac\u00ad\u0001\u0000\u0000\u0000\u00ad\u00ae\u0001\u0000\u0000\u0000"+
		"\u00ae\u00b0\u0003 \u0010\u0000\u00af\u00b1\u0003\u0090H\u0000\u00b0\u00af"+
		"\u0001\u0000\u0000\u0000\u00b0\u00b1\u0001\u0000\u0000\u0000\u00b1\u00b2"+
		"\u0001\u0000\u0000\u0000\u00b2\u00b3\u0005\u0000\u0000\u0001\u00b3\u0005"+
		"\u0001\u0000\u0000\u0000\u00b4\u00b8\u0003\b\u0004\u0000\u00b5\u00b9\u0003"+
		"\f\u0006\u0000\u00b6\u00b9\u0003\u000e\u0007\u0000\u00b7\u00b9\u0003\u0010"+
		"\b\u0000\u00b8\u00b5\u0001\u0000\u0000\u0000\u00b8\u00b6\u0001\u0000\u0000"+
		"\u0000\u00b8\u00b7\u0001\u0000\u0000\u0000\u00b9\u00bb\u0001\u0000\u0000"+
		"\u0000\u00ba\u00bc\u0003\u0090H\u0000\u00bb\u00ba\u0001\u0000\u0000\u0000"+
		"\u00bb\u00bc\u0001\u0000\u0000\u0000\u00bc\u0007\u0001\u0000\u0000\u0000"+
		"\u00bd\u00bf\u00054\u0000\u0000\u00be\u00c0\u0003\u0090H\u0000\u00bf\u00be"+
		"\u0001\u0000\u0000\u0000\u00bf\u00c0\u0001\u0000\u0000\u0000\u00c0\u00c2"+
		"\u0001\u0000\u0000\u0000\u00c1\u00bd\u0001\u0000\u0000\u0000\u00c2\u00c5"+
		"\u0001\u0000\u0000\u0000\u00c3\u00c1\u0001\u0000\u0000\u0000\u00c3\u00c4"+
		"\u0001\u0000\u0000\u0000\u00c4\t\u0001\u0000\u0000\u0000\u00c5\u00c3\u0001"+
		"\u0000\u0000\u0000\u00c6\u00c7\u0005\u0006\u0000\u0000\u00c7\u00c8\u0003"+
		"\u008aE\u0000\u00c8\u000b\u0001\u0000\u0000\u0000\u00c9\u00ca\u0005\u0007"+
		"\u0000\u0000\u00ca\u00cb\u00055\u0000\u0000\u00cb\r\u0001\u0000\u0000"+
		"\u0000\u00cc\u00cd\u0005\b\u0000\u0000\u00cd\u00ce\u0003\u008aE\u0000"+
		"\u00ce\u000f\u0001\u0000\u0000\u0000\u00cf\u00d9\u0003\u0012\t\u0000\u00d0"+
		"\u00d9\u00030\u0018\u0000\u00d1\u00d9\u00032\u0019\u0000\u00d2\u00d9\u0003"+
		"8\u001c\u0000\u00d3\u00d9\u0003:\u001d\u0000\u00d4\u00d9\u0003<\u001e"+
		"\u0000\u00d5\u00d9\u0003D\"\u0000\u00d6\u00d9\u0003F#\u0000\u00d7\u00d9"+
		"\u0003H$\u0000\u00d8\u00cf\u0001\u0000\u0000\u0000\u00d8\u00d0\u0001\u0000"+
		"\u0000\u0000\u00d8\u00d1\u0001\u0000\u0000\u0000\u00d8\u00d2\u0001\u0000"+
		"\u0000\u0000\u00d8\u00d3\u0001\u0000\u0000\u0000\u00d8\u00d4\u0001\u0000"+
		"\u0000\u0000\u00d8\u00d5\u0001\u0000\u0000\u0000\u00d8\u00d6\u0001\u0000"+
		"\u0000\u0000\u00d8\u00d7\u0001\u0000\u0000\u0000\u00d9\u0011\u0001\u0000"+
		"\u0000\u0000\u00da\u00db\u0005\t\u0000\u0000\u00db\u00dd\u0003\u008cF"+
		"\u0000\u00dc\u00de\u0003\u0016\u000b\u0000\u00dd\u00dc\u0001\u0000\u0000"+
		"\u0000\u00dd\u00de\u0001\u0000\u0000\u0000\u00de\u00df\u0001\u0000\u0000"+
		"\u0000\u00df\u00e0\u0005H\u0000\u0000\u00e0\u00e1\u0003\u0014\n\u0000"+
		"\u00e1\u0013\u0001\u0000\u0000\u0000\u00e2\u00ec\u0003\"\u0011\u0000\u00e3"+
		"\u00e8\u0003\u001a\r\u0000\u00e4\u00e5\u0005B\u0000\u0000\u00e5\u00e7"+
		"\u0003\u001a\r\u0000\u00e6\u00e4\u0001\u0000\u0000\u0000\u00e7\u00ea\u0001"+
		"\u0000\u0000\u0000\u00e8\u00e6\u0001\u0000\u0000\u0000\u00e8\u00e9\u0001"+
		"\u0000\u0000\u0000\u00e9\u00ec\u0001\u0000\u0000\u0000\u00ea\u00e8\u0001"+
		"\u0000\u0000\u0000\u00eb\u00e2\u0001\u0000\u0000\u0000\u00eb\u00e3\u0001"+
		"\u0000\u0000\u0000\u00ec\u0015\u0001\u0000\u0000\u0000\u00ed\u00ee\u0005"+
		"I\u0000\u0000\u00ee\u00ef\u0003\u0018\f\u0000\u00ef\u00f0\u0005J\u0000"+
		"\u0000\u00f0\u0017\u0001\u0000\u0000\u0000\u00f1\u00f6\u0003\u008cF\u0000"+
		"\u00f2\u00f3\u0005?\u0000\u0000\u00f3\u00f5\u0003\u008cF\u0000\u00f4\u00f2"+
		"\u0001\u0000\u0000\u0000\u00f5\u00f8\u0001\u0000\u0000\u0000\u00f6\u00f4"+
		"\u0001\u0000\u0000\u0000\u00f6\u00f7\u0001\u0000\u0000\u0000\u00f7\u00fa"+
		"\u0001\u0000\u0000\u0000\u00f8\u00f6\u0001\u0000\u0000\u0000\u00f9\u00fb"+
		"\u0005?\u0000\u0000\u00fa\u00f9\u0001\u0000\u0000\u0000\u00fa\u00fb\u0001"+
		"\u0000\u0000\u0000\u00fb\u0019\u0001\u0000\u0000\u0000\u00fc\u00fd\u0003"+
		"\u008cF\u0000\u00fd\u00fe\u0005;\u0000\u0000\u00fe\u00ff\u0003\u001c\u000e"+
		"\u0000\u00ff\u0100\u0005<\u0000\u0000\u0100\u001b\u0001\u0000\u0000\u0000"+
		"\u0101\u0106\u0003\u001e\u000f\u0000\u0102\u0103\u0005?\u0000\u0000\u0103"+
		"\u0105\u0003\u001e\u000f\u0000\u0104\u0102\u0001\u0000\u0000\u0000\u0105"+
		"\u0108\u0001\u0000\u0000\u0000\u0106\u0104\u0001\u0000\u0000\u0000\u0106"+
		"\u0107\u0001\u0000\u0000\u0000\u0107\u010a\u0001\u0000\u0000\u0000\u0108"+
		"\u0106\u0001\u0000\u0000\u0000\u0109\u010b\u0005?\u0000\u0000\u010a\u0109"+
		"\u0001\u0000\u0000\u0000\u010a\u010b\u0001\u0000\u0000\u0000\u010b\u001d"+
		"\u0001\u0000\u0000\u0000\u010c\u010d\u0003\u008cF\u0000\u010d\u010e\u0005"+
		"@\u0000\u0000\u010e\u010f\u0003 \u0010\u0000\u010f\u0112\u0001\u0000\u0000"+
		"\u0000\u0110\u0112\u0003 \u0010\u0000\u0111\u010c\u0001\u0000\u0000\u0000"+
		"\u0111\u0110\u0001\u0000\u0000\u0000\u0112\u001f\u0001\u0000\u0000\u0000"+
		"\u0113\u0117\u0003\"\u0011\u0000\u0114\u0117\u0003(\u0014\u0000\u0115"+
		"\u0117\u0003*\u0015\u0000\u0116\u0113\u0001\u0000\u0000\u0000\u0116\u0114"+
		"\u0001\u0000\u0000\u0000\u0116\u0115\u0001\u0000\u0000\u0000\u0117!\u0001"+
		"\u0000\u0000\u0000\u0118\u011a\u0005=\u0000\u0000\u0119\u011b\u0003$\u0012"+
		"\u0000\u011a\u0119\u0001\u0000\u0000\u0000\u011a\u011b\u0001\u0000\u0000"+
		"\u0000\u011b\u011c\u0001\u0000\u0000\u0000\u011c\u011d\u0005>\u0000\u0000"+
		"\u011d#\u0001\u0000\u0000\u0000\u011e\u0123\u0003&\u0013\u0000\u011f\u0120"+
		"\u0005?\u0000\u0000\u0120\u0122\u0003&\u0013\u0000\u0121\u011f\u0001\u0000"+
		"\u0000\u0000\u0122\u0125\u0001\u0000\u0000\u0000\u0123\u0121\u0001\u0000"+
		"\u0000\u0000\u0123\u0124\u0001\u0000\u0000\u0000\u0124\u0127\u0001\u0000"+
		"\u0000\u0000\u0125\u0123\u0001\u0000\u0000\u0000\u0126\u0128\u0005?\u0000"+
		"\u0000\u0127\u0126\u0001\u0000\u0000\u0000\u0127\u0128\u0001\u0000\u0000"+
		"\u0000\u0128%\u0001\u0000\u0000\u0000\u0129\u012a\u0003\u008cF\u0000\u012a"+
		"\u012b\u0005@\u0000\u0000\u012b\u012c\u0003 \u0010\u0000\u012c\'\u0001"+
		"\u0000\u0000\u0000\u012d\u012e\u0005;\u0000\u0000\u012e\u012f\u0003 \u0010"+
		"\u0000\u012f\u0130\u0005<\u0000\u0000\u0130\u013f\u0001\u0000\u0000\u0000"+
		"\u0131\u0132\u0005;\u0000\u0000\u0132\u0133\u0003 \u0010\u0000\u0133\u0134"+
		"\u0005?\u0000\u0000\u0134\u0139\u0003 \u0010\u0000\u0135\u0136\u0005?"+
		"\u0000\u0000\u0136\u0138\u0003 \u0010\u0000\u0137\u0135\u0001\u0000\u0000"+
		"\u0000\u0138\u013b\u0001\u0000\u0000\u0000\u0139\u0137\u0001\u0000\u0000"+
		"\u0000\u0139\u013a\u0001\u0000\u0000\u0000\u013a\u013c\u0001\u0000\u0000"+
		"\u0000\u013b\u0139\u0001\u0000\u0000\u0000\u013c\u013d\u0005<\u0000\u0000"+
		"\u013d\u013f\u0001\u0000\u0000\u0000\u013e\u012d\u0001\u0000\u0000\u0000"+
		"\u013e\u0131\u0001\u0000\u0000\u0000\u013f)\u0001\u0000\u0000\u0000\u0140"+
		"\u0142\u0003\u008aE\u0000\u0141\u0143\u0003,\u0016\u0000\u0142\u0141\u0001"+
		"\u0000\u0000\u0000\u0142\u0143\u0001\u0000\u0000\u0000\u0143+\u0001\u0000"+
		"\u0000\u0000\u0144\u0145\u0005I\u0000\u0000\u0145\u0146\u0003.\u0017\u0000"+
		"\u0146\u0147\u0005J\u0000\u0000\u0147-\u0001\u0000\u0000\u0000\u0148\u014d"+
		"\u0003 \u0010\u0000\u0149\u014a\u0005?\u0000\u0000\u014a\u014c\u0003 "+
		"\u0010\u0000\u014b\u0149\u0001\u0000\u0000\u0000\u014c\u014f\u0001\u0000"+
		"\u0000\u0000\u014d\u014b\u0001\u0000\u0000\u0000\u014d\u014e\u0001\u0000"+
		"\u0000\u0000\u014e/\u0001\u0000\u0000\u0000\u014f\u014d\u0001\u0000\u0000"+
		"\u0000\u0150\u0151\u0007\u0000\u0000\u0000\u0151\u0153\u0003\u008cF\u0000"+
		"\u0152\u0154\u0003T*\u0000\u0153\u0152\u0001\u0000\u0000\u0000\u0153\u0154"+
		"\u0001\u0000\u0000\u0000\u0154\u0155\u0001\u0000\u0000\u0000\u0155\u0156"+
		"\u0005H\u0000\u0000\u0156\u0157\u0003V+\u0000\u01571\u0001\u0000\u0000"+
		"\u0000\u0158\u0159\u0005\f\u0000\u0000\u0159\u015b\u0003\u008cF\u0000"+
		"\u015a\u015c\u0003\u0016\u000b\u0000\u015b\u015a\u0001\u0000\u0000\u0000"+
		"\u015b\u015c\u0001\u0000\u0000\u0000\u015c\u015d\u0001\u0000\u0000\u0000"+
		"\u015d\u015f\u0005;\u0000\u0000\u015e\u0160\u00034\u001a\u0000\u015f\u015e"+
		"\u0001\u0000\u0000\u0000\u015f\u0160\u0001\u0000\u0000\u0000\u0160\u0161"+
		"\u0001\u0000\u0000\u0000\u0161\u0162\u0005<\u0000\u0000\u0162\u0163\u0005"+
		"\u0001\u0000\u0000\u0163\u0164\u0003 \u0010\u0000\u0164\u0165\u0005@\u0000"+
		"\u0000\u0165\u0166\u0003P(\u0000\u01663\u0001\u0000\u0000\u0000\u0167"+
		"\u016c\u00036\u001b\u0000\u0168\u0169\u0005?\u0000\u0000\u0169\u016b\u0003"+
		"6\u001b\u0000\u016a\u0168\u0001\u0000\u0000\u0000\u016b\u016e\u0001\u0000"+
		"\u0000\u0000\u016c\u016a\u0001\u0000\u0000\u0000\u016c\u016d\u0001\u0000"+
		"\u0000\u0000\u016d\u0170\u0001\u0000\u0000\u0000\u016e\u016c\u0001\u0000"+
		"\u0000\u0000\u016f\u0171\u0005?\u0000\u0000\u0170\u016f\u0001\u0000\u0000"+
		"\u0000\u0170\u0171\u0001\u0000\u0000\u0000\u01715\u0001\u0000\u0000\u0000"+
		"\u0172\u0173\u0003\u0082A\u0000\u0173\u0174\u0005@\u0000\u0000\u0174\u0175"+
		"\u0003 \u0010\u0000\u01757\u0001\u0000\u0000\u0000\u0176\u0177\u0005\r"+
		"\u0000\u0000\u0177\u0178\u0003\u008cF\u0000\u0178\u0179\u0005@\u0000\u0000"+
		"\u0179\u017a\u0003 \u0010\u0000\u017a9\u0001\u0000\u0000\u0000\u017b\u017c"+
		"\u0005\u000e\u0000\u0000\u017c\u0182\u0003\u008cF\u0000\u017d\u017f\u0005"+
		";\u0000\u0000\u017e\u0180\u0003$\u0012\u0000\u017f\u017e\u0001\u0000\u0000"+
		"\u0000\u017f\u0180\u0001\u0000\u0000\u0000\u0180\u0181\u0001\u0000\u0000"+
		"\u0000\u0181\u0183\u0005<\u0000\u0000\u0182\u017d\u0001\u0000\u0000\u0000"+
		"\u0182\u0183\u0001\u0000\u0000\u0000\u0183;\u0001\u0000\u0000\u0000\u0184"+
		"\u0186\u0003>\u001f\u0000\u0185\u0184\u0001\u0000\u0000\u0000\u0185\u0186"+
		"\u0001\u0000\u0000\u0000\u0186\u0187\u0001\u0000\u0000\u0000\u0187\u0188"+
		"\u0005\u000f\u0000\u0000\u0188\u018b\u0003@ \u0000\u0189\u018a\u00050"+
		"\u0000\u0000\u018a\u018c\u0003V+\u0000\u018b\u0189\u0001\u0000\u0000\u0000"+
		"\u018b\u018c\u0001\u0000\u0000\u0000\u018c=\u0001\u0000\u0000\u0000\u018d"+
		"\u018e\u0007\u0001\u0000\u0000\u018e?\u0001\u0000\u0000\u0000\u018f\u0190"+
		"\u0003B!\u0000\u0190\u0191\u0005@\u0000\u0000\u0191\u0192\u0003V+\u0000"+
		"\u0192\u019f\u0001\u0000\u0000\u0000\u0193\u0195\u0003B!\u0000\u0194\u0193"+
		"\u0001\u0000\u0000\u0000\u0194\u0195\u0001\u0000\u0000\u0000\u0195\u0196"+
		"\u0001\u0000\u0000\u0000\u0196\u0199\u0003\u008aE\u0000\u0197\u0198\u0005"+
		" \u0000\u0000\u0198\u019a\u0003V+\u0000\u0199\u0197\u0001\u0000\u0000"+
		"\u0000\u0199\u019a\u0001\u0000\u0000\u0000\u019a\u019b\u0001\u0000\u0000"+
		"\u0000\u019b\u019c\u0005@\u0000\u0000\u019c\u019d\u0003V+\u0000\u019d"+
		"\u019f\u0001\u0000\u0000\u0000\u019e\u018f\u0001\u0000\u0000\u0000\u019e"+
		"\u0194\u0001\u0000\u0000\u0000\u019fA\u0001\u0000\u0000\u0000\u01a0\u01a1"+
		"\u0007\u0002\u0000\u0000\u01a1C\u0001\u0000\u0000\u0000\u01a2\u01a3\u0007"+
		"\u0003\u0000\u0000\u01a3\u01a8\u0003\u008aE\u0000\u01a4\u01a5\u0005J\u0000"+
		"\u0000\u01a5\u01a7\u0003\u008aE\u0000\u01a6\u01a4\u0001\u0000\u0000\u0000"+
		"\u01a7\u01aa\u0001\u0000\u0000\u0000\u01a8\u01a6\u0001\u0000\u0000\u0000"+
		"\u01a8\u01a9\u0001\u0000\u0000\u0000\u01a9E\u0001\u0000\u0000\u0000\u01aa"+
		"\u01a8\u0001\u0000\u0000\u0000\u01ab\u01af\u0005\u0015\u0000\u0000\u01ac"+
		"\u01ad\u0003\u008cF\u0000\u01ad\u01ae\u0005H\u0000\u0000\u01ae\u01b0\u0001"+
		"\u0000\u0000\u0000\u01af\u01ac\u0001\u0000\u0000\u0000\u01af\u01b0\u0001"+
		"\u0000\u0000\u0000\u01b0\u01b1\u0001\u0000\u0000\u0000\u01b1\u01b2\u0003"+
		"V+\u0000\u01b2G\u0001\u0000\u0000\u0000\u01b3\u01b4\u0005\u0016\u0000"+
		"\u0000\u01b4\u01b5\u0003\u008aE\u0000\u01b5\u01b6\u0005\u0017\u0000\u0000"+
		"\u01b6\u01b8\u0003J%\u0000\u01b7\u01b9\u0003N\'\u0000\u01b8\u01b7\u0001"+
		"\u0000\u0000\u0000\u01b8\u01b9\u0001\u0000\u0000\u0000\u01b9I\u0001\u0000"+
		"\u0000\u0000\u01ba\u01be\u0003\u008aE\u0000\u01bb\u01be\u00055\u0000\u0000"+
		"\u01bc\u01be\u0003L&\u0000\u01bd\u01ba\u0001\u0000\u0000\u0000\u01bd\u01bb"+
		"\u0001\u0000\u0000\u0000\u01bd\u01bc\u0001\u0000\u0000\u0000\u01beK\u0001"+
		"\u0000\u0000\u0000\u01bf\u01c3\u0005I\u0000\u0000\u01c0\u01c2\b\u0004"+
		"\u0000\u0000\u01c1\u01c0\u0001\u0000\u0000\u0000\u01c2\u01c5\u0001\u0000"+
		"\u0000\u0000\u01c3\u01c1\u0001\u0000\u0000\u0000\u01c3\u01c4\u0001\u0000"+
		"\u0000\u0000\u01c4\u01c6\u0001\u0000\u0000\u0000\u01c5\u01c3\u0001\u0000"+
		"\u0000\u0000\u01c6\u01c7\u0005J\u0000\u0000\u01c7M\u0001\u0000\u0000\u0000"+
		"\u01c8\u01c9\u0007\u0005\u0000\u0000\u01c9O\u0001\u0000\u0000\u0000\u01ca"+
		"\u01cb\u0005K\u0000\u0000\u01cb\u01cf\u0005N\u0000\u0000\u01cc\u01ce\u0003"+
		"R)\u0000\u01cd\u01cc\u0001\u0000\u0000\u0000\u01ce\u01d1\u0001\u0000\u0000"+
		"\u0000\u01cf\u01cd\u0001\u0000\u0000\u0000\u01cf\u01d0\u0001\u0000\u0000"+
		"\u0000\u01d0\u01d3\u0001\u0000\u0000\u0000\u01d1\u01cf\u0001\u0000\u0000"+
		"\u0000\u01d2\u01d4\u0003V+\u0000\u01d3\u01d2\u0001\u0000\u0000\u0000\u01d3"+
		"\u01d4\u0001\u0000\u0000\u0000\u01d4\u01d6\u0001\u0000\u0000\u0000\u01d5"+
		"\u01d7\u0003\u0090H\u0000\u01d6\u01d5\u0001\u0000\u0000\u0000\u01d6\u01d7"+
		"\u0001\u0000\u0000\u0000\u01d7\u01d8\u0001\u0000\u0000\u0000\u01d8\u01db"+
		"\u0005O\u0000\u0000\u01d9\u01db\u0003V+\u0000\u01da\u01ca\u0001\u0000"+
		"\u0000\u0000\u01da\u01d9\u0001\u0000\u0000\u0000\u01dbQ\u0001\u0000\u0000"+
		"\u0000\u01dc\u01dd\u0005\u000b\u0000\u0000\u01dd\u01df\u0003\u0082A\u0000"+
		"\u01de\u01e0\u0003T*\u0000\u01df\u01de\u0001\u0000\u0000\u0000\u01df\u01e0"+
		"\u0001\u0000\u0000\u0000\u01e0\u01e1\u0001\u0000\u0000\u0000\u01e1\u01e2"+
		"\u0005H\u0000\u0000\u01e2\u01e3\u0003V+\u0000\u01e3\u01e4\u0003\u0090"+
		"H\u0000\u01e4S\u0001\u0000\u0000\u0000\u01e5\u01e6\u0005@\u0000\u0000"+
		"\u01e6\u01e7\u0003 \u0010\u0000\u01e7U\u0001\u0000\u0000\u0000\u01e8\u01e9"+
		"\u0003X,\u0000\u01e9W\u0001\u0000\u0000\u0000\u01ea\u01ee\u0003Z-\u0000"+
		"\u01eb\u01ed\u0003\u008eG\u0000\u01ec\u01eb\u0001\u0000\u0000\u0000\u01ed"+
		"\u01f0\u0001\u0000\u0000\u0000\u01ee\u01ec\u0001\u0000\u0000\u0000\u01ee"+
		"\u01ef\u0001\u0000\u0000\u0000\u01efY\u0001\u0000\u0000\u0000\u01f0\u01ee"+
		"\u0001\u0000\u0000\u0000\u01f1\u01f2\u0003\\.\u0000\u01f2[\u0001\u0000"+
		"\u0000\u0000\u01f3\u01f8\u0003^/\u0000\u01f4\u01f5\u0005&\u0000\u0000"+
		"\u01f5\u01f7\u0003^/\u0000\u01f6\u01f4\u0001\u0000\u0000\u0000\u01f7\u01fa"+
		"\u0001\u0000\u0000\u0000\u01f8\u01f6\u0001\u0000\u0000\u0000\u01f8\u01f9"+
		"\u0001\u0000\u0000\u0000\u01f9]\u0001\u0000\u0000\u0000\u01fa\u01f8\u0001"+
		"\u0000\u0000\u0000\u01fb\u0200\u0003`0\u0000\u01fc\u01fd\u0005%\u0000"+
		"\u0000\u01fd\u01ff\u0003`0\u0000\u01fe\u01fc\u0001\u0000\u0000\u0000\u01ff"+
		"\u0202\u0001\u0000\u0000\u0000\u0200\u01fe\u0001\u0000\u0000\u0000\u0200"+
		"\u0201\u0001\u0000\u0000\u0000\u0201_\u0001\u0000\u0000\u0000\u0202\u0200"+
		"\u0001\u0000\u0000\u0000\u0203\u0208\u0003b1\u0000\u0204\u0205\u0007\u0006"+
		"\u0000\u0000\u0205\u0207\u0003b1\u0000\u0206\u0204\u0001\u0000\u0000\u0000"+
		"\u0207\u020a\u0001\u0000\u0000\u0000\u0208\u0206\u0001\u0000\u0000\u0000"+
		"\u0208\u0209\u0001\u0000\u0000\u0000\u0209a\u0001\u0000\u0000\u0000\u020a"+
		"\u0208\u0001\u0000\u0000\u0000\u020b\u0210\u0003d2\u0000\u020c\u020d\u0007"+
		"\u0007\u0000\u0000\u020d\u020f\u0003d2\u0000\u020e\u020c\u0001\u0000\u0000"+
		"\u0000\u020f\u0212\u0001\u0000\u0000\u0000\u0210\u020e\u0001\u0000\u0000"+
		"\u0000\u0210\u0211\u0001\u0000\u0000\u0000\u0211c\u0001\u0000\u0000\u0000"+
		"\u0212\u0210\u0001\u0000\u0000\u0000\u0213\u0218\u0003f3\u0000\u0214\u0215"+
		"\u0007\b\u0000\u0000\u0215\u0217\u0003f3\u0000\u0216\u0214\u0001\u0000"+
		"\u0000\u0000\u0217\u021a\u0001\u0000\u0000\u0000\u0218\u0216\u0001\u0000"+
		"\u0000\u0000\u0218\u0219\u0001\u0000\u0000\u0000\u0219e\u0001\u0000\u0000"+
		"\u0000\u021a\u0218\u0001\u0000\u0000\u0000\u021b\u0220\u0003h4\u0000\u021c"+
		"\u021d\u0007\t\u0000\u0000\u021d\u021f\u0003h4\u0000\u021e\u021c\u0001"+
		"\u0000\u0000\u0000\u021f\u0222\u0001\u0000\u0000\u0000\u0220\u021e\u0001"+
		"\u0000\u0000\u0000\u0220\u0221\u0001\u0000\u0000\u0000\u0221g\u0001\u0000"+
		"\u0000\u0000\u0222\u0220\u0001\u0000\u0000\u0000\u0223\u022e\u0003j5\u0000"+
		"\u0224\u022e\u0003l6\u0000\u0225\u022e\u0003n7\u0000\u0226\u022a\u0005"+
		"\'\u0000\u0000\u0227\u022a\u0005D\u0000\u0000\u0228\u022a\u0003\u008e"+
		"G\u0000\u0229\u0226\u0001\u0000\u0000\u0000\u0229\u0227\u0001\u0000\u0000"+
		"\u0000\u0229\u0228\u0001\u0000\u0000\u0000\u022a\u022b\u0001\u0000\u0000"+
		"\u0000\u022b\u022e\u0003h4\u0000\u022c\u022e\u0003t:\u0000\u022d\u0223"+
		"\u0001\u0000\u0000\u0000\u022d\u0224\u0001\u0000\u0000\u0000\u022d\u0225"+
		"\u0001\u0000\u0000\u0000\u022d\u0229\u0001\u0000\u0000\u0000\u022d\u022c"+
		"\u0001\u0000\u0000\u0000\u022ei\u0001\u0000\u0000\u0000\u022f\u0230\u0005"+
		"\u001c\u0000\u0000\u0230\u0232\u0003V+\u0000\u0231\u0233\u0003\u0090H"+
		"\u0000\u0232\u0231\u0001\u0000\u0000\u0000\u0232\u0233\u0001\u0000\u0000"+
		"\u0000\u0233\u0234\u0001\u0000\u0000\u0000\u0234\u0235\u0005\u001d\u0000"+
		"\u0000\u0235\u0237\u0003V+\u0000\u0236\u0238\u0003\u0090H\u0000\u0237"+
		"\u0236\u0001\u0000\u0000\u0000\u0237\u0238\u0001\u0000\u0000\u0000\u0238"+
		"\u0239\u0001\u0000\u0000\u0000\u0239\u023a\u0005\u001e\u0000\u0000\u023a"+
		"\u023b\u0003V+\u0000\u023bk\u0001\u0000\u0000\u0000\u023c\u023d\u0005"+
		"\u000b\u0000\u0000\u023d\u023f\u0003\u0082A\u0000\u023e\u0240\u0003T*"+
		"\u0000\u023f\u023e\u0001\u0000\u0000\u0000\u023f\u0240\u0001\u0000\u0000"+
		"\u0000\u0240\u0241\u0001\u0000\u0000\u0000\u0241\u0242\u0005H\u0000\u0000"+
		"\u0242\u0244\u0003V+\u0000\u0243\u0245\u0003\u0090H\u0000\u0244\u0243"+
		"\u0001\u0000\u0000\u0000\u0244\u0245\u0001\u0000\u0000\u0000\u0245\u0246"+
		"\u0001\u0000\u0000\u0000\u0246\u0247\u0005!\u0000\u0000\u0247\u0248\u0003"+
		"V+\u0000\u0248m\u0001\u0000\u0000\u0000\u0249\u024a\u0005\u001f\u0000"+
		"\u0000\u024a\u024b\u0003V+\u0000\u024b\u024c\u0005@\u0000\u0000\u024c"+
		"\u024d\u0003p8\u0000\u024do\u0001\u0000\u0000\u0000\u024e\u024f\u0005"+
		"K\u0000\u0000\u024f\u0251\u0005N\u0000\u0000\u0250\u0252\u0003\u0090H"+
		"\u0000\u0251\u0250\u0001\u0000\u0000\u0000\u0251\u0252\u0001\u0000\u0000"+
		"\u0000\u0252\u0253\u0001\u0000\u0000\u0000\u0253\u025a\u0003r9\u0000\u0254"+
		"\u0256\u0003\u0090H\u0000\u0255\u0254\u0001\u0000\u0000\u0000\u0255\u0256"+
		"\u0001\u0000\u0000\u0000\u0256\u0257\u0001\u0000\u0000\u0000\u0257\u0259"+
		"\u0003r9\u0000\u0258\u0255\u0001\u0000\u0000\u0000\u0259\u025c\u0001\u0000"+
		"\u0000\u0000\u025a\u0258\u0001\u0000\u0000\u0000\u025a\u025b\u0001\u0000"+
		"\u0000\u0000\u025b\u025e\u0001\u0000\u0000\u0000\u025c\u025a\u0001\u0000"+
		"\u0000\u0000\u025d\u025f\u0003\u0090H\u0000\u025e\u025d\u0001\u0000\u0000"+
		"\u0000\u025e\u025f\u0001\u0000\u0000\u0000\u025f\u0260\u0001\u0000\u0000"+
		"\u0000\u0260\u0261\u0005O\u0000\u0000\u0261\u0274\u0001\u0000\u0000\u0000"+
		"\u0262\u0264\u0005K\u0000\u0000\u0263\u0265\u0003\u0090H\u0000\u0264\u0263"+
		"\u0001\u0000\u0000\u0000\u0264\u0265\u0001\u0000\u0000\u0000\u0265\u0266"+
		"\u0001\u0000\u0000\u0000\u0266\u026d\u0003r9\u0000\u0267\u0269\u0003\u0090"+
		"H\u0000\u0268\u0267\u0001\u0000\u0000\u0000\u0268\u0269\u0001\u0000\u0000"+
		"\u0000\u0269\u026a\u0001\u0000\u0000\u0000\u026a\u026c\u0003r9\u0000\u026b"+
		"\u0268\u0001\u0000\u0000\u0000\u026c\u026f\u0001\u0000\u0000\u0000\u026d"+
		"\u026b\u0001\u0000\u0000\u0000\u026d\u026e\u0001\u0000\u0000\u0000\u026e"+
		"\u0271\u0001\u0000\u0000\u0000\u026f\u026d\u0001\u0000\u0000\u0000\u0270"+
		"\u0272\u0003\u0090H\u0000\u0271\u0270\u0001\u0000\u0000\u0000\u0271\u0272"+
		"\u0001\u0000\u0000\u0000\u0272\u0274\u0001\u0000\u0000\u0000\u0273\u024e"+
		"\u0001\u0000\u0000\u0000\u0273\u0262\u0001\u0000\u0000\u0000\u0274q\u0001"+
		"\u0000\u0000\u0000\u0275\u0276\u0005B\u0000\u0000\u0276\u0279\u0003\u0082"+
		"A\u0000\u0277\u0278\u0005 \u0000\u0000\u0278\u027a\u0003V+\u0000\u0279"+
		"\u0277\u0001\u0000\u0000\u0000\u0279\u027a\u0001\u0000\u0000\u0000\u027a"+
		"\u027b\u0001\u0000\u0000\u0000\u027b\u027c\u0005@\u0000\u0000\u027c\u027d"+
		"\u0003P(\u0000\u027ds\u0001\u0000\u0000\u0000\u027e\u0282\u0003~?\u0000"+
		"\u027f\u0281\u0003v;\u0000\u0280\u027f\u0001\u0000\u0000\u0000\u0281\u0284"+
		"\u0001\u0000\u0000\u0000\u0282\u0280\u0001\u0000\u0000\u0000\u0282\u0283"+
		"\u0001\u0000\u0000\u0000\u0283u\u0001\u0000\u0000\u0000\u0284\u0282\u0001"+
		"\u0000\u0000\u0000\u0285\u028e\u0003x<\u0000\u0286\u0288\u0005;\u0000"+
		"\u0000\u0287\u0289\u0003\u0080@\u0000\u0288\u0287\u0001\u0000\u0000\u0000"+
		"\u0288\u0289\u0001\u0000\u0000\u0000\u0289\u028a\u0001\u0000\u0000\u0000"+
		"\u028a\u028e\u0005<\u0000\u0000\u028b\u028c\u0005A\u0000\u0000\u028c\u028e"+
		"\u0003\u008cF\u0000\u028d\u0285\u0001\u0000\u0000\u0000\u028d\u0286\u0001"+
		"\u0000\u0000\u0000\u028d\u028b\u0001\u0000\u0000\u0000\u028ew\u0001\u0000"+
		"\u0000\u0000\u028f\u0291\u0005=\u0000\u0000\u0290\u0292\u0003z=\u0000"+
		"\u0291\u0290\u0001\u0000\u0000\u0000\u0291\u0292\u0001\u0000\u0000\u0000"+
		"\u0292\u0293\u0001\u0000\u0000\u0000\u0293\u0294\u0005>\u0000\u0000\u0294"+
		"y\u0001\u0000\u0000\u0000\u0295\u029a\u0003|>\u0000\u0296\u0297\u0005"+
		"?\u0000\u0000\u0297\u0299\u0003|>\u0000\u0298\u0296\u0001\u0000\u0000"+
		"\u0000\u0299\u029c\u0001\u0000\u0000\u0000\u029a\u0298\u0001\u0000\u0000"+
		"\u0000\u029a\u029b\u0001\u0000\u0000\u0000\u029b\u029e\u0001\u0000\u0000"+
		"\u0000\u029c\u029a\u0001\u0000\u0000\u0000\u029d\u029f\u0005?\u0000\u0000"+
		"\u029e\u029d\u0001\u0000\u0000\u0000\u029e\u029f\u0001\u0000\u0000\u0000"+
		"\u029f{\u0001\u0000\u0000\u0000\u02a0\u02a1\u0003\u008cF\u0000\u02a1\u02a2"+
		"\u0005H\u0000\u0000\u02a2\u02a3\u0003V+\u0000\u02a3}\u0001\u0000\u0000"+
		"\u0000\u02a4\u02c0\u00055\u0000\u0000\u02a5\u02c0\u00058\u0000\u0000\u02a6"+
		"\u02c0\u00057\u0000\u0000\u02a7\u02c0\u00056\u0000\u0000\u02a8\u02c0\u0005"+
		"\"\u0000\u0000\u02a9\u02c0\u0005#\u0000\u0000\u02aa\u02c0\u0005$\u0000"+
		"\u0000\u02ab\u02c0\u0003\u008aE\u0000\u02ac\u02ad\u0005;\u0000\u0000\u02ad"+
		"\u02c0\u0005<\u0000\u0000\u02ae\u02af\u0005;\u0000\u0000\u02af\u02b0\u0003"+
		"V+\u0000\u02b0\u02b1\u0005<\u0000\u0000\u02b1\u02c0\u0001\u0000\u0000"+
		"\u0000\u02b2\u02b3\u0005;\u0000\u0000\u02b3\u02b4\u0003V+\u0000\u02b4"+
		"\u02b5\u0005?\u0000\u0000\u02b5\u02ba\u0003V+\u0000\u02b6\u02b7\u0005"+
		"?\u0000\u0000\u02b7\u02b9\u0003V+\u0000\u02b8\u02b6\u0001\u0000\u0000"+
		"\u0000\u02b9\u02bc\u0001\u0000\u0000\u0000\u02ba\u02b8\u0001\u0000\u0000"+
		"\u0000\u02ba\u02bb\u0001\u0000\u0000\u0000\u02bb\u02bd\u0001\u0000\u0000"+
		"\u0000\u02bc\u02ba\u0001\u0000\u0000\u0000\u02bd\u02be\u0005<\u0000\u0000"+
		"\u02be\u02c0\u0001\u0000\u0000\u0000\u02bf\u02a4\u0001\u0000\u0000\u0000"+
		"\u02bf\u02a5\u0001\u0000\u0000\u0000\u02bf\u02a6\u0001\u0000\u0000\u0000"+
		"\u02bf\u02a7\u0001\u0000\u0000\u0000\u02bf\u02a8\u0001\u0000\u0000\u0000"+
		"\u02bf\u02a9\u0001\u0000\u0000\u0000\u02bf\u02aa\u0001\u0000\u0000\u0000"+
		"\u02bf\u02ab\u0001\u0000\u0000\u0000\u02bf\u02ac\u0001\u0000\u0000\u0000"+
		"\u02bf\u02ae\u0001\u0000\u0000\u0000\u02bf\u02b2\u0001\u0000\u0000\u0000"+
		"\u02c0\u007f\u0001\u0000\u0000\u0000\u02c1\u02c6\u0003V+\u0000\u02c2\u02c3"+
		"\u0005?\u0000\u0000\u02c3\u02c5\u0003V+\u0000\u02c4\u02c2\u0001\u0000"+
		"\u0000\u0000\u02c5\u02c8\u0001\u0000\u0000\u0000\u02c6\u02c4\u0001\u0000"+
		"\u0000\u0000\u02c6\u02c7\u0001\u0000\u0000\u0000\u02c7\u02ca\u0001\u0000"+
		"\u0000\u0000\u02c8\u02c6\u0001\u0000\u0000\u0000\u02c9\u02cb\u0005?\u0000"+
		"\u0000\u02ca\u02c9\u0001\u0000\u0000\u0000\u02ca\u02cb\u0001\u0000\u0000"+
		"\u0000\u02cb\u0081\u0001\u0000\u0000\u0000\u02cc\u02f2\u00059\u0000\u0000"+
		"\u02cd\u02f2\u00055\u0000\u0000\u02ce\u02f2\u00058\u0000\u0000\u02cf\u02f2"+
		"\u00057\u0000\u0000\u02d0\u02f2\u00056\u0000\u0000\u02d1\u02d2\u0005;"+
		"\u0000\u0000\u02d2\u02f2\u0005<\u0000\u0000\u02d3\u02d4\u0005;\u0000\u0000"+
		"\u02d4\u02d5\u0003\u0082A\u0000\u02d5\u02d6\u0005<\u0000\u0000\u02d6\u02f2"+
		"\u0001\u0000\u0000\u0000\u02d7\u02d8\u0005;\u0000\u0000\u02d8\u02d9\u0003"+
		"\u0082A\u0000\u02d9\u02da\u0005?\u0000\u0000\u02da\u02df\u0003\u0082A"+
		"\u0000\u02db\u02dc\u0005?\u0000\u0000\u02dc\u02de\u0003\u0082A\u0000\u02dd"+
		"\u02db\u0001\u0000\u0000\u0000\u02de\u02e1\u0001\u0000\u0000\u0000\u02df"+
		"\u02dd\u0001\u0000\u0000\u0000\u02df\u02e0\u0001\u0000\u0000\u0000\u02e0"+
		"\u02e2\u0001\u0000\u0000\u0000\u02e1\u02df\u0001\u0000\u0000\u0000\u02e2"+
		"\u02e3\u0005<\u0000\u0000\u02e3\u02f2\u0001\u0000\u0000\u0000\u02e4\u02e6"+
		"\u0005=\u0000\u0000\u02e5\u02e7\u0003\u0086C\u0000\u02e6\u02e5\u0001\u0000"+
		"\u0000\u0000\u02e6\u02e7\u0001\u0000\u0000\u0000\u02e7\u02e8\u0001\u0000"+
		"\u0000\u0000\u02e8\u02f2\u0005>\u0000\u0000\u02e9\u02ef\u0003\u008aE\u0000"+
		"\u02ea\u02ec\u0005;\u0000\u0000\u02eb\u02ed\u0003\u0084B\u0000\u02ec\u02eb"+
		"\u0001\u0000\u0000\u0000\u02ec\u02ed\u0001\u0000\u0000\u0000\u02ed\u02ee"+
		"\u0001\u0000\u0000\u0000\u02ee\u02f0\u0005<\u0000\u0000\u02ef\u02ea\u0001"+
		"\u0000\u0000\u0000\u02ef\u02f0\u0001\u0000\u0000\u0000\u02f0\u02f2\u0001"+
		"\u0000\u0000\u0000\u02f1\u02cc\u0001\u0000\u0000\u0000\u02f1\u02cd\u0001"+
		"\u0000\u0000\u0000\u02f1\u02ce\u0001\u0000\u0000\u0000\u02f1\u02cf\u0001"+
		"\u0000\u0000\u0000\u02f1\u02d0\u0001\u0000\u0000\u0000\u02f1\u02d1\u0001"+
		"\u0000\u0000\u0000\u02f1\u02d3\u0001\u0000\u0000\u0000\u02f1\u02d7\u0001"+
		"\u0000\u0000\u0000\u02f1\u02e4\u0001\u0000\u0000\u0000\u02f1\u02e9\u0001"+
		"\u0000\u0000\u0000\u02f2\u0083\u0001\u0000\u0000\u0000\u02f3\u02f8\u0003"+
		"\u0082A\u0000\u02f4\u02f5\u0005?\u0000\u0000\u02f5\u02f7\u0003\u0082A"+
		"\u0000\u02f6\u02f4\u0001\u0000\u0000\u0000\u02f7\u02fa\u0001\u0000\u0000"+
		"\u0000\u02f8\u02f6\u0001\u0000\u0000\u0000\u02f8\u02f9\u0001\u0000\u0000"+
		"\u0000\u02f9\u02fc\u0001\u0000\u0000\u0000\u02fa\u02f8\u0001\u0000\u0000"+
		"\u0000\u02fb\u02fd\u0005?\u0000\u0000\u02fc\u02fb\u0001\u0000\u0000\u0000"+
		"\u02fc\u02fd\u0001\u0000\u0000\u0000\u02fd\u0085\u0001\u0000\u0000\u0000"+
		"\u02fe\u0303\u0003\u0088D\u0000\u02ff\u0300\u0005?\u0000\u0000\u0300\u0302"+
		"\u0003\u0088D\u0000\u0301\u02ff\u0001\u0000\u0000\u0000\u0302\u0305\u0001"+
		"\u0000\u0000\u0000\u0303\u0301\u0001\u0000\u0000\u0000\u0303\u0304\u0001"+
		"\u0000\u0000\u0000\u0304\u0307\u0001\u0000\u0000\u0000\u0305\u0303\u0001"+
		"\u0000\u0000\u0000\u0306\u0308\u0005?\u0000\u0000\u0307\u0306\u0001\u0000"+
		"\u0000\u0000\u0307\u0308\u0001\u0000\u0000\u0000\u0308\u0087\u0001\u0000"+
		"\u0000\u0000\u0309\u030c\u0003\u008cF\u0000\u030a\u030b\u0005H\u0000\u0000"+
		"\u030b\u030d\u0003\u0082A\u0000\u030c\u030a\u0001\u0000\u0000\u0000\u030c"+
		"\u030d\u0001\u0000\u0000\u0000\u030d\u0089\u0001\u0000\u0000\u0000\u030e"+
		"\u0313\u0003\u008cF\u0000\u030f\u0310\u0005A\u0000\u0000\u0310\u0312\u0003"+
		"\u008cF\u0000\u0311\u030f\u0001\u0000\u0000\u0000\u0312\u0315\u0001\u0000"+
		"\u0000\u0000\u0313\u0311\u0001\u0000\u0000\u0000\u0313\u0314\u0001\u0000"+
		"\u0000\u0000\u0314\u008b\u0001\u0000\u0000\u0000\u0315\u0313\u0001\u0000"+
		"\u0000\u0000\u0316\u0317\u0007\n\u0000\u0000\u0317\u008d\u0001\u0000\u0000"+
		"\u0000\u0318\u0319\u0007\u000b\u0000\u0000\u0319\u008f\u0001\u0000\u0000"+
		"\u0000\u031a\u031c\u0005K\u0000\u0000\u031b\u031a\u0001\u0000\u0000\u0000"+
		"\u031c\u031d\u0001\u0000\u0000\u0000\u031d\u031b\u0001\u0000\u0000\u0000"+
		"\u031d\u031e\u0001\u0000\u0000\u0000\u031e\u0091\u0001\u0000\u0000\u0000"+
		"_\u0093\u0098\u009d\u00a3\u00a7\u00ac\u00b0\u00b8\u00bb\u00bf\u00c3\u00d8"+
		"\u00dd\u00e8\u00eb\u00f6\u00fa\u0106\u010a\u0111\u0116\u011a\u0123\u0127"+
		"\u0139\u013e\u0142\u014d\u0153\u015b\u015f\u016c\u0170\u017f\u0182\u0185"+
		"\u018b\u0194\u0199\u019e\u01a8\u01af\u01b8\u01bd\u01c3\u01cf\u01d3\u01d6"+
		"\u01da\u01df\u01ee\u01f8\u0200\u0208\u0210\u0218\u0220\u0229\u022d\u0232"+
		"\u0237\u023f\u0244\u0251\u0255\u025a\u025e\u0264\u0268\u026d\u0271\u0273"+
		"\u0279\u0282\u0288\u028d\u0291\u029a\u029e\u02ba\u02bf\u02c6\u02ca\u02df"+
		"\u02e6\u02ec\u02ef\u02f1\u02f8\u02fc\u0303\u0307\u030c\u0313\u031d";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}