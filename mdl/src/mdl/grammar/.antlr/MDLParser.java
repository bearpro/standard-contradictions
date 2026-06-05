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
		ARROW=1, LEFT_ARROW=2, LE=3, GE=4, NE=5, EQEQ=6, IMPLIES_ARROW=7, BIARROW=8, 
		MODULE=9, IMPORT=10, OPEN=11, TYPE=12, VAL=13, LET=14, FUNC=15, ENTITY=16, 
		EVENT=17, RULE=18, STRICT=19, DEFEASIBLE=20, DEFEATER=21, PRIORITY=22, 
		OVERRIDE=23, FACT=24, ASSERT=25, ALIGN=26, TO=27, EQUIVALENT=28, BROADER=29, 
		NARROWER=30, RELATED=31, KEY=32, WHERE=33, IF=34, THEN=35, ELSE=36, CASE=37, 
		SWITCH=38, WHEN=39, IN=40, FORALL=41, EXISTS=42, TRUE=43, FALSE=44, LAST=45, 
		AND=46, OR=47, NOT=48, IMPLIES=49, IFF=50, ALWAYS=51, EVENTUALLY=52, NEXT=53, 
		WEAK_NEXT=54, NEVER=55, UNTIL=56, RELEASE=57, WEAK_UNTIL=58, OTHERWISE=59, 
		O=60, P=61, F=62, ANNOT=63, STRING=64, RAT=65, DECIMAL=66, INT=67, UNDERSCORE=68, 
		IDENT=69, LPAREN=70, RPAREN=71, LBRACE=72, RBRACE=73, LBRACK=74, RBRACK=75, 
		COMMA=76, COLON=77, DOT=78, BAR=79, PLUS=80, MINUS=81, STAR=82, SLASH=83, 
		PERCENT=84, EQ=85, LT=86, GT=87, SEMI=88, NEWLINE=89, COMMENT=90, WS=91, 
		INDENT=92, DEDENT=93;
	public static final int
		RULE_program = 0, RULE_exprOnly = 1, RULE_typeExprOnly = 2, RULE_topItem = 3, 
		RULE_annotations = 4, RULE_moduleDecl = 5, RULE_importDecl = 6, RULE_openDecl = 7, 
		RULE_declaration = 8, RULE_typeDecl = 9, RULE_typeDefinition = 10, RULE_typeParams = 11, 
		RULE_nameList = 12, RULE_variant = 13, RULE_variantFieldList = 14, RULE_variantField = 15, 
		RULE_typeExpr = 16, RULE_recordType = 17, RULE_typeFieldList = 18, RULE_typeField = 19, 
		RULE_tupleOrParenType = 20, RULE_typeRef = 21, RULE_typeArgs = 22, RULE_typeExprList = 23, 
		RULE_valueDecl = 24, RULE_funcDecl = 25, RULE_paramList = 26, RULE_param = 27, 
		RULE_entityDecl = 28, RULE_entityClause = 29, RULE_eventDecl = 30, RULE_ruleDecl = 31, 
		RULE_ruleStrength = 32, RULE_ruleBody = 33, RULE_ruleSeparator = 34, RULE_deonticMod = 35, 
		RULE_priorityDecl = 36, RULE_factDecl = 37, RULE_assertDecl = 38, RULE_alignDecl = 39, 
		RULE_alignTarget = 40, RULE_iriLiteral = 41, RULE_alignKind = 42, RULE_block = 43, 
		RULE_blockLetStmt = 44, RULE_typeAnnotation = 45, RULE_expr = 46, RULE_temporalPostfix = 47, 
		RULE_implication = 48, RULE_iffExpr = 49, RULE_orExpr = 50, RULE_andExpr = 51, 
		RULE_temporalBinary = 52, RULE_comparison = 53, RULE_additive = 54, RULE_multiplicative = 55, 
		RULE_unary = 56, RULE_ifExpr = 57, RULE_letExpr = 58, RULE_matchExpr = 59, 
		RULE_caseBody = 60, RULE_caseArm = 61, RULE_quantifierExpr = 62, RULE_postfix = 63, 
		RULE_postfixSuffix = 64, RULE_recordConstructorFields = 65, RULE_recordConstructorFieldList = 66, 
		RULE_recordConstructorField = 67, RULE_primary = 68, RULE_exprList = 69, 
		RULE_pattern = 70, RULE_patternList = 71, RULE_recordPatternFieldList = 72, 
		RULE_recordPatternField = 73, RULE_qualifiedName = 74, RULE_nameToken = 75, 
		RULE_temporalUnaryOp = 76, RULE_newlines = 77;
	private static String[] makeRuleNames() {
		return new String[] {
			"program", "exprOnly", "typeExprOnly", "topItem", "annotations", "moduleDecl", 
			"importDecl", "openDecl", "declaration", "typeDecl", "typeDefinition", 
			"typeParams", "nameList", "variant", "variantFieldList", "variantField", 
			"typeExpr", "recordType", "typeFieldList", "typeField", "tupleOrParenType", 
			"typeRef", "typeArgs", "typeExprList", "valueDecl", "funcDecl", "paramList", 
			"param", "entityDecl", "entityClause", "eventDecl", "ruleDecl", "ruleStrength", 
			"ruleBody", "ruleSeparator", "deonticMod", "priorityDecl", "factDecl", 
			"assertDecl", "alignDecl", "alignTarget", "iriLiteral", "alignKind", 
			"block", "blockLetStmt", "typeAnnotation", "expr", "temporalPostfix", 
			"implication", "iffExpr", "orExpr", "andExpr", "temporalBinary", "comparison", 
			"additive", "multiplicative", "unary", "ifExpr", "letExpr", "matchExpr", 
			"caseBody", "caseArm", "quantifierExpr", "postfix", "postfixSuffix", 
			"recordConstructorFields", "recordConstructorFieldList", "recordConstructorField", 
			"primary", "exprList", "pattern", "patternList", "recordPatternFieldList", 
			"recordPatternField", "qualifiedName", "nameToken", "temporalUnaryOp", 
			"newlines"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'->'", "'<-'", "'<='", "'>='", "'!='", "'=='", "'=>'", "'<->'", 
			"'module'", "'import'", "'open'", "'type'", "'val'", "'let'", "'func'", 
			"'entity'", "'event'", "'rule'", "'strict'", "'defeasible'", "'defeater'", 
			"'priority'", "'override'", "'fact'", "'assert'", "'align'", "'to'", 
			"'equivalent'", "'broader'", "'narrower'", "'related'", "'key'", "'where'", 
			"'if'", "'then'", "'else'", "'case'", "'switch'", "'when'", "'in'", "'forall'", 
			"'exists'", "'true'", "'false'", "'last'", "'and'", "'or'", "'not'", 
			"'implies'", "'iff'", "'always'", "'eventually'", "'next'", "'weak_next'", 
			"'never'", "'until'", "'release'", "'weak_until'", "'otherwise'", "'O'", 
			"'P'", "'F'", null, null, null, null, null, "'_'", null, "'('", "')'", 
			"'{'", "'}'", "'['", "']'", "','", "':'", "'.'", "'|'", "'+'", "'-'", 
			"'*'", "'/'", "'%'", "'='", "'<'", "'>'", "';'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, "ARROW", "LEFT_ARROW", "LE", "GE", "NE", "EQEQ", "IMPLIES_ARROW", 
			"BIARROW", "MODULE", "IMPORT", "OPEN", "TYPE", "VAL", "LET", "FUNC", 
			"ENTITY", "EVENT", "RULE", "STRICT", "DEFEASIBLE", "DEFEATER", "PRIORITY", 
			"OVERRIDE", "FACT", "ASSERT", "ALIGN", "TO", "EQUIVALENT", "BROADER", 
			"NARROWER", "RELATED", "KEY", "WHERE", "IF", "THEN", "ELSE", "CASE", 
			"SWITCH", "WHEN", "IN", "FORALL", "EXISTS", "TRUE", "FALSE", "LAST", 
			"AND", "OR", "NOT", "IMPLIES", "IFF", "ALWAYS", "EVENTUALLY", "NEXT", 
			"WEAK_NEXT", "NEVER", "UNTIL", "RELEASE", "WEAK_UNTIL", "OTHERWISE", 
			"O", "P", "F", "ANNOT", "STRING", "RAT", "DECIMAL", "INT", "UNDERSCORE", 
			"IDENT", "LPAREN", "RPAREN", "LBRACE", "RBRACE", "LBRACK", "RBRACK", 
			"COMMA", "COLON", "DOT", "BAR", "PLUS", "MINUS", "STAR", "SLASH", "PERCENT", 
			"EQ", "LT", "GT", "SEMI", "NEWLINE", "COMMENT", "WS", "INDENT", "DEDENT"
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
			setState(157);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(156);
				newlines();
				}
			}

			setState(159);
			annotations();
			setState(160);
			moduleDecl();
			setState(162);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(161);
				newlines();
				}
			}

			setState(167);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & -9223372036720559104L) != 0)) {
				{
				{
				setState(164);
				topItem();
				}
				}
				setState(169);
				_errHandler.sync(this);
				_la = _input.LA(1);
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
			setState(173);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(172);
				newlines();
				}
			}

			setState(175);
			expr();
			setState(177);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(176);
				newlines();
				}
			}

			setState(179);
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
			setState(182);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(181);
				newlines();
				}
			}

			setState(184);
			typeExpr();
			setState(186);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(185);
				newlines();
				}
			}

			setState(188);
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
			setState(190);
			annotations();
			setState(194);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case IMPORT:
				{
				setState(191);
				importDecl();
				}
				break;
			case OPEN:
				{
				setState(192);
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
			case ASSERT:
			case ALIGN:
				{
				setState(193);
				declaration();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(197);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(196);
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
			setState(205);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==ANNOT) {
				{
				{
				setState(199);
				match(ANNOT);
				setState(201);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(200);
					newlines();
					}
				}

				}
				}
				setState(207);
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
			setState(208);
			match(MODULE);
			setState(209);
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
			setState(211);
			match(IMPORT);
			setState(212);
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
			setState(214);
			match(OPEN);
			setState(215);
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
		public AssertDeclContext assertDecl() {
			return getRuleContext(AssertDeclContext.class,0);
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
			setState(227);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case TYPE:
				enterOuterAlt(_localctx, 1);
				{
				setState(217);
				typeDecl();
				}
				break;
			case VAL:
			case LET:
				enterOuterAlt(_localctx, 2);
				{
				setState(218);
				valueDecl();
				}
				break;
			case FUNC:
				enterOuterAlt(_localctx, 3);
				{
				setState(219);
				funcDecl();
				}
				break;
			case ENTITY:
				enterOuterAlt(_localctx, 4);
				{
				setState(220);
				entityDecl();
				}
				break;
			case EVENT:
				enterOuterAlt(_localctx, 5);
				{
				setState(221);
				eventDecl();
				}
				break;
			case RULE:
			case STRICT:
			case DEFEASIBLE:
			case DEFEATER:
				enterOuterAlt(_localctx, 6);
				{
				setState(222);
				ruleDecl();
				}
				break;
			case PRIORITY:
			case OVERRIDE:
				enterOuterAlt(_localctx, 7);
				{
				setState(223);
				priorityDecl();
				}
				break;
			case FACT:
				enterOuterAlt(_localctx, 8);
				{
				setState(224);
				factDecl();
				}
				break;
			case ASSERT:
				enterOuterAlt(_localctx, 9);
				{
				setState(225);
				assertDecl();
				}
				break;
			case ALIGN:
				enterOuterAlt(_localctx, 10);
				{
				setState(226);
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
			setState(229);
			match(TYPE);
			setState(230);
			nameToken();
			setState(232);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LT) {
				{
				setState(231);
				typeParams();
				}
			}

			setState(234);
			match(EQ);
			setState(235);
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
			setState(246);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case LBRACE:
				enterOuterAlt(_localctx, 1);
				{
				setState(237);
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
				setState(238);
				variant();
				setState(243);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==BAR) {
					{
					{
					setState(239);
					match(BAR);
					setState(240);
					variant();
					}
					}
					setState(245);
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
		public TerminalNode GT() { return getToken(MDLParser.GT, 0); }
		public NameListContext nameList() {
			return getRuleContext(NameListContext.class,0);
		}
		public TypeParamsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeParams; }
	}

	public final TypeParamsContext typeParams() throws RecognitionException {
		TypeParamsContext _localctx = new TypeParamsContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_typeParams);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(248);
			match(LT);
			setState(250);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (((((_la - 43)) & ~0x3f) == 0 && ((1L << (_la - 43)) & 68026375L) != 0)) {
				{
				setState(249);
				nameList();
				}
			}

			setState(252);
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
			setState(254);
			nameToken();
			setState(259);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,16,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(255);
					match(COMMA);
					setState(256);
					nameToken();
					}
					} 
				}
				setState(261);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,16,_ctx);
			}
			setState(263);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(262);
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
		public TerminalNode RPAREN() { return getToken(MDLParser.RPAREN, 0); }
		public VariantFieldListContext variantFieldList() {
			return getRuleContext(VariantFieldListContext.class,0);
		}
		public VariantContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_variant; }
	}

	public final VariantContext variant() throws RecognitionException {
		VariantContext _localctx = new VariantContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_variant);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(265);
			nameToken();
			setState(266);
			match(LPAREN);
			setState(268);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (((((_la - 43)) & ~0x3f) == 0 && ((1L << (_la - 43)) & 739115015L) != 0)) {
				{
				setState(267);
				variantFieldList();
				}
			}

			setState(270);
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
			setState(272);
			variantField();
			setState(277);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,19,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(273);
					match(COMMA);
					setState(274);
					variantField();
					}
					} 
				}
				setState(279);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,19,_ctx);
			}
			setState(281);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(280);
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
			setState(288);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,21,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(283);
				nameToken();
				setState(284);
				match(COLON);
				setState(285);
				typeExpr();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(287);
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
			setState(293);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case LBRACE:
				enterOuterAlt(_localctx, 1);
				{
				setState(290);
				recordType();
				}
				break;
			case LPAREN:
				enterOuterAlt(_localctx, 2);
				{
				setState(291);
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
				setState(292);
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
			setState(295);
			match(LBRACE);
			setState(297);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (((((_la - 43)) & ~0x3f) == 0 && ((1L << (_la - 43)) & 68026375L) != 0)) {
				{
				setState(296);
				typeFieldList();
				}
			}

			setState(299);
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
			setState(301);
			typeField();
			setState(306);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,24,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(302);
					match(COMMA);
					setState(303);
					typeField();
					}
					} 
				}
				setState(308);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,24,_ctx);
			}
			setState(310);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(309);
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
			setState(312);
			nameToken();
			setState(313);
			match(COLON);
			setState(314);
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
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(316);
			match(LPAREN);
			setState(317);
			typeExpr();
			setState(322);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,26,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(318);
					match(COMMA);
					setState(319);
					typeExpr();
					}
					} 
				}
				setState(324);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,26,_ctx);
			}
			setState(326);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(325);
				match(COMMA);
				}
			}

			setState(328);
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
			setState(330);
			qualifiedName();
			setState(332);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LT) {
				{
				setState(331);
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
		public TerminalNode GT() { return getToken(MDLParser.GT, 0); }
		public TypeExprListContext typeExprList() {
			return getRuleContext(TypeExprListContext.class,0);
		}
		public TypeArgsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typeArgs; }
	}

	public final TypeArgsContext typeArgs() throws RecognitionException {
		TypeArgsContext _localctx = new TypeArgsContext(_ctx, getState());
		enterRule(_localctx, 44, RULE_typeArgs);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(334);
			match(LT);
			setState(336);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (((((_la - 43)) & ~0x3f) == 0 && ((1L << (_la - 43)) & 739115015L) != 0)) {
				{
				setState(335);
				typeExprList();
				}
			}

			setState(338);
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
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(340);
			typeExpr();
			setState(345);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,30,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(341);
					match(COMMA);
					setState(342);
					typeExpr();
					}
					} 
				}
				setState(347);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,30,_ctx);
			}
			setState(349);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(348);
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
			setState(351);
			_la = _input.LA(1);
			if ( !(_la==VAL || _la==LET) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			setState(352);
			nameToken();
			setState(354);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COLON) {
				{
				setState(353);
				typeAnnotation();
				}
			}

			setState(356);
			match(EQ);
			setState(357);
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
			setState(359);
			match(FUNC);
			setState(360);
			nameToken();
			setState(362);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LT) {
				{
				setState(361);
				typeParams();
				}
			}

			setState(364);
			match(LPAREN);
			setState(366);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (((((_la - 43)) & ~0x3f) == 0 && ((1L << (_la - 43)) & 804126727L) != 0)) {
				{
				setState(365);
				paramList();
				}
			}

			setState(368);
			match(RPAREN);
			setState(369);
			match(ARROW);
			setState(370);
			typeExpr();
			setState(371);
			match(COLON);
			setState(372);
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
			setState(374);
			param();
			setState(379);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,35,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(375);
					match(COMMA);
					setState(376);
					param();
					}
					} 
				}
				setState(381);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,35,_ctx);
			}
			setState(383);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(382);
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
			setState(385);
			pattern();
			setState(386);
			match(COLON);
			setState(387);
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
		public List<EntityClauseContext> entityClause() {
			return getRuleContexts(EntityClauseContext.class);
		}
		public EntityClauseContext entityClause(int i) {
			return getRuleContext(EntityClauseContext.class,i);
		}
		public EntityDeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_entityDecl; }
	}

	public final EntityDeclContext entityDecl() throws RecognitionException {
		EntityDeclContext _localctx = new EntityDeclContext(_ctx, getState());
		enterRule(_localctx, 56, RULE_entityDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(389);
			match(ENTITY);
			setState(390);
			nameToken();
			setState(391);
			match(COLON);
			setState(392);
			typeExpr();
			setState(396);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==KEY || _la==WHERE) {
				{
				{
				setState(393);
				entityClause();
				}
				}
				setState(398);
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
	public static class EntityClauseContext extends ParserRuleContext {
		public TerminalNode KEY() { return getToken(MDLParser.KEY, 0); }
		public TerminalNode LPAREN() { return getToken(MDLParser.LPAREN, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode RPAREN() { return getToken(MDLParser.RPAREN, 0); }
		public TerminalNode WHERE() { return getToken(MDLParser.WHERE, 0); }
		public EntityClauseContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_entityClause; }
	}

	public final EntityClauseContext entityClause() throws RecognitionException {
		EntityClauseContext _localctx = new EntityClauseContext(_ctx, getState());
		enterRule(_localctx, 58, RULE_entityClause);
		try {
			setState(406);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case KEY:
				enterOuterAlt(_localctx, 1);
				{
				setState(399);
				match(KEY);
				setState(400);
				match(LPAREN);
				setState(401);
				expr();
				setState(402);
				match(RPAREN);
				}
				break;
			case WHERE:
				enterOuterAlt(_localctx, 2);
				{
				setState(404);
				match(WHERE);
				setState(405);
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
		enterRule(_localctx, 60, RULE_eventDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(408);
			match(EVENT);
			setState(409);
			nameToken();
			setState(415);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LPAREN) {
				{
				setState(410);
				match(LPAREN);
				setState(412);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (((((_la - 43)) & ~0x3f) == 0 && ((1L << (_la - 43)) & 68026375L) != 0)) {
					{
					setState(411);
					typeFieldList();
					}
				}

				setState(414);
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
		enterRule(_localctx, 62, RULE_ruleDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(418);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 3670016L) != 0)) {
				{
				setState(417);
				ruleStrength();
				}
			}

			setState(420);
			match(RULE);
			setState(421);
			ruleBody();
			setState(424);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==OTHERWISE) {
				{
				setState(422);
				match(OTHERWISE);
				setState(423);
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
		enterRule(_localctx, 64, RULE_ruleStrength);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(426);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 3670016L) != 0)) ) {
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
		public RuleSeparatorContext ruleSeparator() {
			return getRuleContext(RuleSeparatorContext.class,0);
		}
		public TerminalNode WHEN() { return getToken(MDLParser.WHEN, 0); }
		public RuleBodyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ruleBody; }
	}

	public final RuleBodyContext ruleBody() throws RecognitionException {
		RuleBodyContext _localctx = new RuleBodyContext(_ctx, getState());
		enterRule(_localctx, 66, RULE_ruleBody);
		int _la;
		try {
			setState(443);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,45,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(428);
				deonticMod();
				setState(429);
				match(COLON);
				setState(430);
				expr();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(433);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,43,_ctx) ) {
				case 1:
					{
					setState(432);
					deonticMod();
					}
					break;
				}
				setState(435);
				qualifiedName();
				setState(438);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==WHEN) {
					{
					setState(436);
					match(WHEN);
					setState(437);
					expr();
					}
				}

				setState(440);
				ruleSeparator();
				setState(441);
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
	public static class RuleSeparatorContext extends ParserRuleContext {
		public TerminalNode COLON() { return getToken(MDLParser.COLON, 0); }
		public TerminalNode EQ() { return getToken(MDLParser.EQ, 0); }
		public RuleSeparatorContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ruleSeparator; }
	}

	public final RuleSeparatorContext ruleSeparator() throws RecognitionException {
		RuleSeparatorContext _localctx = new RuleSeparatorContext(_ctx, getState());
		enterRule(_localctx, 68, RULE_ruleSeparator);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(445);
			_la = _input.LA(1);
			if ( !(_la==COLON || _la==EQ) ) {
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
		enterRule(_localctx, 70, RULE_deonticMod);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(447);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 8070450532247928832L) != 0)) ) {
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
		enterRule(_localctx, 72, RULE_priorityDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(449);
			_la = _input.LA(1);
			if ( !(_la==PRIORITY || _la==OVERRIDE) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			setState(450);
			qualifiedName();
			setState(455);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==GT) {
				{
				{
				setState(451);
				match(GT);
				setState(452);
				qualifiedName();
				}
				}
				setState(457);
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
		enterRule(_localctx, 74, RULE_factDecl);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(458);
			match(FACT);
			setState(462);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,47,_ctx) ) {
			case 1:
				{
				setState(459);
				nameToken();
				setState(460);
				match(EQ);
				}
				break;
			}
			setState(464);
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
	public static class AssertDeclContext extends ParserRuleContext {
		public TerminalNode ASSERT() { return getToken(MDLParser.ASSERT, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public AssertDeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_assertDecl; }
	}

	public final AssertDeclContext assertDecl() throws RecognitionException {
		AssertDeclContext _localctx = new AssertDeclContext(_ctx, getState());
		enterRule(_localctx, 76, RULE_assertDecl);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(466);
			match(ASSERT);
			setState(467);
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
		enterRule(_localctx, 78, RULE_alignDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(469);
			match(ALIGN);
			setState(470);
			qualifiedName();
			setState(471);
			match(TO);
			setState(472);
			alignTarget();
			setState(474);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 4026531840L) != 0)) {
				{
				setState(473);
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
		enterRule(_localctx, 80, RULE_alignTarget);
		try {
			setState(479);
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
				setState(476);
				qualifiedName();
				}
				break;
			case STRING:
				enterOuterAlt(_localctx, 2);
				{
				setState(477);
				match(STRING);
				}
				break;
			case LT:
				enterOuterAlt(_localctx, 3);
				{
				setState(478);
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
		enterRule(_localctx, 82, RULE_iriLiteral);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(481);
			match(LT);
			setState(485);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & -2L) != 0) || ((((_la - 64)) & ~0x3f) == 0 && ((1L << (_la - 64)) & 1065353215L) != 0)) {
				{
				{
				setState(482);
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
				setState(487);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(488);
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
		enterRule(_localctx, 84, RULE_alignKind);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(490);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 4026531840L) != 0)) ) {
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
		enterRule(_localctx, 86, RULE_block);
		int _la;
		try {
			int _alt;
			setState(508);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case NEWLINE:
				enterOuterAlt(_localctx, 1);
				{
				setState(492);
				match(NEWLINE);
				setState(493);
				match(INDENT);
				setState(497);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,51,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(494);
						blockLetStmt();
						}
						} 
					}
					setState(499);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,51,_ctx);
				}
				setState(501);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 8140606400666550272L) != 0) || ((((_la - 64)) & ~0x3f) == 0 && ((1L << (_la - 64)) & 131183L) != 0)) {
					{
					setState(500);
					expr();
					}
				}

				setState(504);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(503);
					newlines();
					}
				}

				setState(506);
				match(DEDENT);
				}
				break;
			case LET:
			case IF:
			case CASE:
			case SWITCH:
			case FORALL:
			case EXISTS:
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
				setState(507);
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
		enterRule(_localctx, 88, RULE_blockLetStmt);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(510);
			match(LET);
			setState(511);
			pattern();
			setState(513);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COLON) {
				{
				setState(512);
				typeAnnotation();
				}
			}

			setState(515);
			match(EQ);
			setState(516);
			expr();
			setState(517);
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
		enterRule(_localctx, 90, RULE_typeAnnotation);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(519);
			match(COLON);
			setState(520);
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
		enterRule(_localctx, 92, RULE_expr);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(522);
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
		enterRule(_localctx, 94, RULE_temporalPostfix);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(524);
			implication();
			setState(528);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,56,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(525);
					temporalUnaryOp();
					}
					} 
				}
				setState(530);
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
	public static class ImplicationContext extends ParserRuleContext {
		public IffExprContext iffExpr() {
			return getRuleContext(IffExprContext.class,0);
		}
		public ImplicationContext implication() {
			return getRuleContext(ImplicationContext.class,0);
		}
		public TerminalNode IMPLIES() { return getToken(MDLParser.IMPLIES, 0); }
		public TerminalNode ARROW() { return getToken(MDLParser.ARROW, 0); }
		public ImplicationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_implication; }
	}

	public final ImplicationContext implication() throws RecognitionException {
		ImplicationContext _localctx = new ImplicationContext(_ctx, getState());
		enterRule(_localctx, 96, RULE_implication);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(531);
			iffExpr();
			setState(534);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,57,_ctx) ) {
			case 1:
				{
				setState(532);
				_la = _input.LA(1);
				if ( !(_la==ARROW || _la==IMPLIES) ) {
				_errHandler.recoverInline(this);
				}
				else {
					if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
				setState(533);
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
	public static class IffExprContext extends ParserRuleContext {
		public List<OrExprContext> orExpr() {
			return getRuleContexts(OrExprContext.class);
		}
		public OrExprContext orExpr(int i) {
			return getRuleContext(OrExprContext.class,i);
		}
		public List<TerminalNode> IFF() { return getTokens(MDLParser.IFF); }
		public TerminalNode IFF(int i) {
			return getToken(MDLParser.IFF, i);
		}
		public List<TerminalNode> BIARROW() { return getTokens(MDLParser.BIARROW); }
		public TerminalNode BIARROW(int i) {
			return getToken(MDLParser.BIARROW, i);
		}
		public IffExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_iffExpr; }
	}

	public final IffExprContext iffExpr() throws RecognitionException {
		IffExprContext _localctx = new IffExprContext(_ctx, getState());
		enterRule(_localctx, 98, RULE_iffExpr);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(536);
			orExpr();
			setState(541);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,58,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(537);
					_la = _input.LA(1);
					if ( !(_la==BIARROW || _la==IFF) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(538);
					orExpr();
					}
					} 
				}
				setState(543);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,58,_ctx);
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
		enterRule(_localctx, 100, RULE_orExpr);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(544);
			andExpr();
			setState(549);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,59,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(545);
					match(OR);
					setState(546);
					andExpr();
					}
					} 
				}
				setState(551);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,59,_ctx);
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
		enterRule(_localctx, 102, RULE_andExpr);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(552);
			temporalBinary();
			setState(557);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,60,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(553);
					match(AND);
					setState(554);
					temporalBinary();
					}
					} 
				}
				setState(559);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,60,_ctx);
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
		enterRule(_localctx, 104, RULE_temporalBinary);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(560);
			comparison();
			setState(565);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,61,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(561);
					_la = _input.LA(1);
					if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 504403158265495552L) != 0)) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(562);
					comparison();
					}
					} 
				}
				setState(567);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,61,_ctx);
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
		enterRule(_localctx, 106, RULE_comparison);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(568);
			additive();
			setState(573);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,62,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(569);
					_la = _input.LA(1);
					if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 120L) != 0) || ((((_la - 85)) & ~0x3f) == 0 && ((1L << (_la - 85)) & 7L) != 0)) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(570);
					additive();
					}
					} 
				}
				setState(575);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,62,_ctx);
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
		enterRule(_localctx, 108, RULE_additive);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(576);
			multiplicative();
			setState(581);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,63,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(577);
					_la = _input.LA(1);
					if ( !(_la==PLUS || _la==MINUS) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(578);
					multiplicative();
					}
					} 
				}
				setState(583);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,63,_ctx);
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
		enterRule(_localctx, 110, RULE_multiplicative);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(584);
			unary();
			setState(589);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,64,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(585);
					_la = _input.LA(1);
					if ( !(((((_la - 82)) & ~0x3f) == 0 && ((1L << (_la - 82)) & 7L) != 0)) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(586);
					unary();
					}
					} 
				}
				setState(591);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,64,_ctx);
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
		public QuantifierExprContext quantifierExpr() {
			return getRuleContext(QuantifierExprContext.class,0);
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
		enterRule(_localctx, 112, RULE_unary);
		try {
			setState(603);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case IF:
				enterOuterAlt(_localctx, 1);
				{
				setState(592);
				ifExpr();
				}
				break;
			case LET:
				enterOuterAlt(_localctx, 2);
				{
				setState(593);
				letExpr();
				}
				break;
			case CASE:
			case SWITCH:
				enterOuterAlt(_localctx, 3);
				{
				setState(594);
				matchExpr();
				}
				break;
			case FORALL:
			case EXISTS:
				enterOuterAlt(_localctx, 4);
				{
				setState(595);
				quantifierExpr();
				}
				break;
			case NOT:
			case ALWAYS:
			case EVENTUALLY:
			case NEXT:
			case WEAK_NEXT:
			case NEVER:
			case MINUS:
				enterOuterAlt(_localctx, 5);
				{
				setState(599);
				_errHandler.sync(this);
				switch (_input.LA(1)) {
				case NOT:
					{
					setState(596);
					match(NOT);
					}
					break;
				case MINUS:
					{
					setState(597);
					match(MINUS);
					}
					break;
				case ALWAYS:
				case EVENTUALLY:
				case NEXT:
				case WEAK_NEXT:
				case NEVER:
					{
					setState(598);
					temporalUnaryOp();
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(601);
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
				enterOuterAlt(_localctx, 6);
				{
				setState(602);
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
		enterRule(_localctx, 114, RULE_ifExpr);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(605);
			match(IF);
			setState(606);
			expr();
			setState(608);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(607);
				newlines();
				}
			}

			setState(610);
			match(THEN);
			setState(611);
			expr();
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
			match(ELSE);
			setState(616);
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
		enterRule(_localctx, 116, RULE_letExpr);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(618);
			match(LET);
			setState(619);
			pattern();
			setState(621);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COLON) {
				{
				setState(620);
				typeAnnotation();
				}
			}

			setState(623);
			match(EQ);
			setState(624);
			expr();
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
			match(IN);
			setState(629);
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
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode COLON() { return getToken(MDLParser.COLON, 0); }
		public CaseBodyContext caseBody() {
			return getRuleContext(CaseBodyContext.class,0);
		}
		public TerminalNode CASE() { return getToken(MDLParser.CASE, 0); }
		public TerminalNode SWITCH() { return getToken(MDLParser.SWITCH, 0); }
		public MatchExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_matchExpr; }
	}

	public final MatchExprContext matchExpr() throws RecognitionException {
		MatchExprContext _localctx = new MatchExprContext(_ctx, getState());
		enterRule(_localctx, 118, RULE_matchExpr);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(631);
			_la = _input.LA(1);
			if ( !(_la==CASE || _la==SWITCH) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			setState(632);
			expr();
			setState(633);
			match(COLON);
			setState(634);
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
		enterRule(_localctx, 120, RULE_caseBody);
		int _la;
		try {
			int _alt;
			setState(673);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,79,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(636);
				match(NEWLINE);
				setState(637);
				match(INDENT);
				setState(639);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(638);
					newlines();
					}
				}

				setState(641);
				caseArm();
				setState(648);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,73,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(643);
						_errHandler.sync(this);
						_la = _input.LA(1);
						if (_la==NEWLINE) {
							{
							setState(642);
							newlines();
							}
						}

						setState(645);
						caseArm();
						}
						} 
					}
					setState(650);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,73,_ctx);
				}
				setState(652);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(651);
					newlines();
					}
				}

				setState(654);
				match(DEDENT);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(656);
				match(NEWLINE);
				setState(658);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(657);
					newlines();
					}
				}

				setState(660);
				caseArm();
				setState(667);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,77,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(662);
						_errHandler.sync(this);
						_la = _input.LA(1);
						if (_la==NEWLINE) {
							{
							setState(661);
							newlines();
							}
						}

						setState(664);
						caseArm();
						}
						} 
					}
					setState(669);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,77,_ctx);
				}
				setState(671);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,78,_ctx) ) {
				case 1:
					{
					setState(670);
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
		enterRule(_localctx, 122, RULE_caseArm);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(675);
			match(BAR);
			setState(676);
			pattern();
			setState(679);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==WHEN) {
				{
				setState(677);
				match(WHEN);
				setState(678);
				expr();
				}
			}

			setState(681);
			match(COLON);
			setState(682);
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
	public static class QuantifierExprContext extends ParserRuleContext {
		public PatternContext pattern() {
			return getRuleContext(PatternContext.class,0);
		}
		public TerminalNode IN() { return getToken(MDLParser.IN, 0); }
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public TerminalNode COLON() { return getToken(MDLParser.COLON, 0); }
		public TerminalNode FORALL() { return getToken(MDLParser.FORALL, 0); }
		public TerminalNode EXISTS() { return getToken(MDLParser.EXISTS, 0); }
		public QuantifierExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_quantifierExpr; }
	}

	public final QuantifierExprContext quantifierExpr() throws RecognitionException {
		QuantifierExprContext _localctx = new QuantifierExprContext(_ctx, getState());
		enterRule(_localctx, 124, RULE_quantifierExpr);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(684);
			_la = _input.LA(1);
			if ( !(_la==FORALL || _la==EXISTS) ) {
			_errHandler.recoverInline(this);
			}
			else {
				if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
				_errHandler.reportMatch(this);
				consume();
			}
			setState(685);
			pattern();
			setState(686);
			match(IN);
			setState(687);
			expr();
			setState(688);
			match(COLON);
			setState(689);
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
		enterRule(_localctx, 126, RULE_postfix);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(691);
			primary();
			setState(695);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (((((_la - 70)) & ~0x3f) == 0 && ((1L << (_la - 70)) & 277L) != 0)) {
				{
				{
				setState(692);
				postfixSuffix();
				}
				}
				setState(697);
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
		public TerminalNode LBRACK() { return getToken(MDLParser.LBRACK, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode RBRACK() { return getToken(MDLParser.RBRACK, 0); }
		public PostfixSuffixContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_postfixSuffix; }
	}

	public final PostfixSuffixContext postfixSuffix() throws RecognitionException {
		PostfixSuffixContext _localctx = new PostfixSuffixContext(_ctx, getState());
		enterRule(_localctx, 128, RULE_postfixSuffix);
		int _la;
		try {
			setState(710);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case LBRACE:
				enterOuterAlt(_localctx, 1);
				{
				setState(698);
				recordConstructorFields();
				}
				break;
			case LPAREN:
				enterOuterAlt(_localctx, 2);
				{
				setState(699);
				match(LPAREN);
				setState(701);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 8140606400666550272L) != 0) || ((((_la - 64)) & ~0x3f) == 0 && ((1L << (_la - 64)) & 131183L) != 0)) {
					{
					setState(700);
					exprList();
					}
				}

				setState(703);
				match(RPAREN);
				}
				break;
			case DOT:
				enterOuterAlt(_localctx, 3);
				{
				setState(704);
				match(DOT);
				setState(705);
				nameToken();
				}
				break;
			case LBRACK:
				enterOuterAlt(_localctx, 4);
				{
				setState(706);
				match(LBRACK);
				setState(707);
				expr();
				setState(708);
				match(RBRACK);
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
		enterRule(_localctx, 130, RULE_recordConstructorFields);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(712);
			match(LBRACE);
			setState(714);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (((((_la - 43)) & ~0x3f) == 0 && ((1L << (_la - 43)) & 68026375L) != 0)) {
				{
				setState(713);
				recordConstructorFieldList();
				}
			}

			setState(716);
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
		enterRule(_localctx, 132, RULE_recordConstructorFieldList);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(718);
			recordConstructorField();
			setState(723);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,85,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(719);
					match(COMMA);
					setState(720);
					recordConstructorField();
					}
					} 
				}
				setState(725);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,85,_ctx);
			}
			setState(727);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(726);
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
		enterRule(_localctx, 134, RULE_recordConstructorField);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(729);
			nameToken();
			setState(730);
			match(EQ);
			setState(731);
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
		enterRule(_localctx, 136, RULE_primary);
		int _la;
		try {
			int _alt;
			setState(757);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,89,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(733);
				match(STRING);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(734);
				match(INT);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(735);
				match(DECIMAL);
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(736);
				match(RAT);
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(737);
				match(TRUE);
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(738);
				match(FALSE);
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(739);
				match(LAST);
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(740);
				qualifiedName();
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(741);
				match(LPAREN);
				setState(742);
				match(RPAREN);
				}
				break;
			case 10:
				enterOuterAlt(_localctx, 10);
				{
				setState(743);
				match(LPAREN);
				setState(744);
				expr();
				setState(749);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,87,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(745);
						match(COMMA);
						setState(746);
						expr();
						}
						} 
					}
					setState(751);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,87,_ctx);
				}
				setState(753);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==COMMA) {
					{
					setState(752);
					match(COMMA);
					}
				}

				setState(755);
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
		enterRule(_localctx, 138, RULE_exprList);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(759);
			expr();
			setState(764);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,90,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(760);
					match(COMMA);
					setState(761);
					expr();
					}
					} 
				}
				setState(766);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,90,_ctx);
			}
			setState(768);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(767);
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
		enterRule(_localctx, 140, RULE_pattern);
		int _la;
		try {
			int _alt;
			setState(804);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,97,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(770);
				match(UNDERSCORE);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(771);
				match(STRING);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(772);
				match(INT);
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(773);
				match(DECIMAL);
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(774);
				match(RAT);
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(775);
				match(LPAREN);
				setState(776);
				match(RPAREN);
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(777);
				match(LPAREN);
				setState(778);
				pattern();
				setState(783);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,92,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(779);
						match(COMMA);
						setState(780);
						pattern();
						}
						} 
					}
					setState(785);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,92,_ctx);
				}
				setState(787);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==COMMA) {
					{
					setState(786);
					match(COMMA);
					}
				}

				setState(789);
				match(RPAREN);
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(791);
				match(LBRACE);
				setState(793);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (((((_la - 43)) & ~0x3f) == 0 && ((1L << (_la - 43)) & 68026375L) != 0)) {
					{
					setState(792);
					recordPatternFieldList();
					}
				}

				setState(795);
				match(RBRACE);
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(796);
				qualifiedName();
				setState(802);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==LPAREN) {
					{
					setState(797);
					match(LPAREN);
					setState(799);
					_errHandler.sync(this);
					_la = _input.LA(1);
					if (((((_la - 43)) & ~0x3f) == 0 && ((1L << (_la - 43)) & 804126727L) != 0)) {
						{
						setState(798);
						patternList();
						}
					}

					setState(801);
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
		enterRule(_localctx, 142, RULE_patternList);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(806);
			pattern();
			setState(811);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,98,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(807);
					match(COMMA);
					setState(808);
					pattern();
					}
					} 
				}
				setState(813);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,98,_ctx);
			}
			setState(815);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(814);
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
		enterRule(_localctx, 144, RULE_recordPatternFieldList);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(817);
			recordPatternField();
			setState(822);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,100,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(818);
					match(COMMA);
					setState(819);
					recordPatternField();
					}
					} 
				}
				setState(824);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,100,_ctx);
			}
			setState(826);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COMMA) {
				{
				setState(825);
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
		enterRule(_localctx, 146, RULE_recordPatternField);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(828);
			nameToken();
			setState(831);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==EQ) {
				{
				setState(829);
				match(EQ);
				setState(830);
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
		enterRule(_localctx, 148, RULE_qualifiedName);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(833);
			nameToken();
			setState(838);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,103,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(834);
					match(DOT);
					setState(835);
					nameToken();
					}
					} 
				}
				setState(840);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,103,_ctx);
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
		enterRule(_localctx, 150, RULE_nameToken);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(841);
			_la = _input.LA(1);
			if ( !(((((_la - 43)) & ~0x3f) == 0 && ((1L << (_la - 43)) & 68026375L) != 0)) ) {
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
		enterRule(_localctx, 152, RULE_temporalUnaryOp);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(843);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 69805794224242688L) != 0)) ) {
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
		enterRule(_localctx, 154, RULE_newlines);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(846); 
			_errHandler.sync(this);
			_alt = 1;
			do {
				switch (_alt) {
				case 1:
					{
					{
					setState(845);
					match(NEWLINE);
					}
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(848); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,104,_ctx);
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
		"\u0004\u0001]\u0353\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001\u0002"+
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
		"F\u0007F\u0002G\u0007G\u0002H\u0007H\u0002I\u0007I\u0002J\u0007J\u0002"+
		"K\u0007K\u0002L\u0007L\u0002M\u0007M\u0001\u0000\u0003\u0000\u009e\b\u0000"+
		"\u0001\u0000\u0001\u0000\u0001\u0000\u0003\u0000\u00a3\b\u0000\u0001\u0000"+
		"\u0005\u0000\u00a6\b\u0000\n\u0000\f\u0000\u00a9\t\u0000\u0001\u0000\u0001"+
		"\u0000\u0001\u0001\u0003\u0001\u00ae\b\u0001\u0001\u0001\u0001\u0001\u0003"+
		"\u0001\u00b2\b\u0001\u0001\u0001\u0001\u0001\u0001\u0002\u0003\u0002\u00b7"+
		"\b\u0002\u0001\u0002\u0001\u0002\u0003\u0002\u00bb\b\u0002\u0001\u0002"+
		"\u0001\u0002\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0003\u0003"+
		"\u00c3\b\u0003\u0001\u0003\u0003\u0003\u00c6\b\u0003\u0001\u0004\u0001"+
		"\u0004\u0003\u0004\u00ca\b\u0004\u0005\u0004\u00cc\b\u0004\n\u0004\f\u0004"+
		"\u00cf\t\u0004\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0006\u0001\u0006"+
		"\u0001\u0006\u0001\u0007\u0001\u0007\u0001\u0007\u0001\b\u0001\b\u0001"+
		"\b\u0001\b\u0001\b\u0001\b\u0001\b\u0001\b\u0001\b\u0001\b\u0003\b\u00e4"+
		"\b\b\u0001\t\u0001\t\u0001\t\u0003\t\u00e9\b\t\u0001\t\u0001\t\u0001\t"+
		"\u0001\n\u0001\n\u0001\n\u0001\n\u0005\n\u00f2\b\n\n\n\f\n\u00f5\t\n\u0003"+
		"\n\u00f7\b\n\u0001\u000b\u0001\u000b\u0003\u000b\u00fb\b\u000b\u0001\u000b"+
		"\u0001\u000b\u0001\f\u0001\f\u0001\f\u0005\f\u0102\b\f\n\f\f\f\u0105\t"+
		"\f\u0001\f\u0003\f\u0108\b\f\u0001\r\u0001\r\u0001\r\u0003\r\u010d\b\r"+
		"\u0001\r\u0001\r\u0001\u000e\u0001\u000e\u0001\u000e\u0005\u000e\u0114"+
		"\b\u000e\n\u000e\f\u000e\u0117\t\u000e\u0001\u000e\u0003\u000e\u011a\b"+
		"\u000e\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f\u0003"+
		"\u000f\u0121\b\u000f\u0001\u0010\u0001\u0010\u0001\u0010\u0003\u0010\u0126"+
		"\b\u0010\u0001\u0011\u0001\u0011\u0003\u0011\u012a\b\u0011\u0001\u0011"+
		"\u0001\u0011\u0001\u0012\u0001\u0012\u0001\u0012\u0005\u0012\u0131\b\u0012"+
		"\n\u0012\f\u0012\u0134\t\u0012\u0001\u0012\u0003\u0012\u0137\b\u0012\u0001"+
		"\u0013\u0001\u0013\u0001\u0013\u0001\u0013\u0001\u0014\u0001\u0014\u0001"+
		"\u0014\u0001\u0014\u0005\u0014\u0141\b\u0014\n\u0014\f\u0014\u0144\t\u0014"+
		"\u0001\u0014\u0003\u0014\u0147\b\u0014\u0001\u0014\u0001\u0014\u0001\u0015"+
		"\u0001\u0015\u0003\u0015\u014d\b\u0015\u0001\u0016\u0001\u0016\u0003\u0016"+
		"\u0151\b\u0016\u0001\u0016\u0001\u0016\u0001\u0017\u0001\u0017\u0001\u0017"+
		"\u0005\u0017\u0158\b\u0017\n\u0017\f\u0017\u015b\t\u0017\u0001\u0017\u0003"+
		"\u0017\u015e\b\u0017\u0001\u0018\u0001\u0018\u0001\u0018\u0003\u0018\u0163"+
		"\b\u0018\u0001\u0018\u0001\u0018\u0001\u0018\u0001\u0019\u0001\u0019\u0001"+
		"\u0019\u0003\u0019\u016b\b\u0019\u0001\u0019\u0001\u0019\u0003\u0019\u016f"+
		"\b\u0019\u0001\u0019\u0001\u0019\u0001\u0019\u0001\u0019\u0001\u0019\u0001"+
		"\u0019\u0001\u001a\u0001\u001a\u0001\u001a\u0005\u001a\u017a\b\u001a\n"+
		"\u001a\f\u001a\u017d\t\u001a\u0001\u001a\u0003\u001a\u0180\b\u001a\u0001"+
		"\u001b\u0001\u001b\u0001\u001b\u0001\u001b\u0001\u001c\u0001\u001c\u0001"+
		"\u001c\u0001\u001c\u0001\u001c\u0005\u001c\u018b\b\u001c\n\u001c\f\u001c"+
		"\u018e\t\u001c\u0001\u001d\u0001\u001d\u0001\u001d\u0001\u001d\u0001\u001d"+
		"\u0001\u001d\u0001\u001d\u0003\u001d\u0197\b\u001d\u0001\u001e\u0001\u001e"+
		"\u0001\u001e\u0001\u001e\u0003\u001e\u019d\b\u001e\u0001\u001e\u0003\u001e"+
		"\u01a0\b\u001e\u0001\u001f\u0003\u001f\u01a3\b\u001f\u0001\u001f\u0001"+
		"\u001f\u0001\u001f\u0001\u001f\u0003\u001f\u01a9\b\u001f\u0001 \u0001"+
		" \u0001!\u0001!\u0001!\u0001!\u0001!\u0003!\u01b2\b!\u0001!\u0001!\u0001"+
		"!\u0003!\u01b7\b!\u0001!\u0001!\u0001!\u0003!\u01bc\b!\u0001\"\u0001\""+
		"\u0001#\u0001#\u0001$\u0001$\u0001$\u0001$\u0005$\u01c6\b$\n$\f$\u01c9"+
		"\t$\u0001%\u0001%\u0001%\u0001%\u0003%\u01cf\b%\u0001%\u0001%\u0001&\u0001"+
		"&\u0001&\u0001\'\u0001\'\u0001\'\u0001\'\u0001\'\u0003\'\u01db\b\'\u0001"+
		"(\u0001(\u0001(\u0003(\u01e0\b(\u0001)\u0001)\u0005)\u01e4\b)\n)\f)\u01e7"+
		"\t)\u0001)\u0001)\u0001*\u0001*\u0001+\u0001+\u0001+\u0005+\u01f0\b+\n"+
		"+\f+\u01f3\t+\u0001+\u0003+\u01f6\b+\u0001+\u0003+\u01f9\b+\u0001+\u0001"+
		"+\u0003+\u01fd\b+\u0001,\u0001,\u0001,\u0003,\u0202\b,\u0001,\u0001,\u0001"+
		",\u0001,\u0001-\u0001-\u0001-\u0001.\u0001.\u0001/\u0001/\u0005/\u020f"+
		"\b/\n/\f/\u0212\t/\u00010\u00010\u00010\u00030\u0217\b0\u00011\u00011"+
		"\u00011\u00051\u021c\b1\n1\f1\u021f\t1\u00012\u00012\u00012\u00052\u0224"+
		"\b2\n2\f2\u0227\t2\u00013\u00013\u00013\u00053\u022c\b3\n3\f3\u022f\t"+
		"3\u00014\u00014\u00014\u00054\u0234\b4\n4\f4\u0237\t4\u00015\u00015\u0001"+
		"5\u00055\u023c\b5\n5\f5\u023f\t5\u00016\u00016\u00016\u00056\u0244\b6"+
		"\n6\f6\u0247\t6\u00017\u00017\u00017\u00057\u024c\b7\n7\f7\u024f\t7\u0001"+
		"8\u00018\u00018\u00018\u00018\u00018\u00018\u00038\u0258\b8\u00018\u0001"+
		"8\u00038\u025c\b8\u00019\u00019\u00019\u00039\u0261\b9\u00019\u00019\u0001"+
		"9\u00039\u0266\b9\u00019\u00019\u00019\u0001:\u0001:\u0001:\u0003:\u026e"+
		"\b:\u0001:\u0001:\u0001:\u0003:\u0273\b:\u0001:\u0001:\u0001:\u0001;\u0001"+
		";\u0001;\u0001;\u0001;\u0001<\u0001<\u0001<\u0003<\u0280\b<\u0001<\u0001"+
		"<\u0003<\u0284\b<\u0001<\u0005<\u0287\b<\n<\f<\u028a\t<\u0001<\u0003<"+
		"\u028d\b<\u0001<\u0001<\u0001<\u0001<\u0003<\u0293\b<\u0001<\u0001<\u0003"+
		"<\u0297\b<\u0001<\u0005<\u029a\b<\n<\f<\u029d\t<\u0001<\u0003<\u02a0\b"+
		"<\u0003<\u02a2\b<\u0001=\u0001=\u0001=\u0001=\u0003=\u02a8\b=\u0001=\u0001"+
		"=\u0001=\u0001>\u0001>\u0001>\u0001>\u0001>\u0001>\u0001>\u0001?\u0001"+
		"?\u0005?\u02b6\b?\n?\f?\u02b9\t?\u0001@\u0001@\u0001@\u0003@\u02be\b@"+
		"\u0001@\u0001@\u0001@\u0001@\u0001@\u0001@\u0001@\u0003@\u02c7\b@\u0001"+
		"A\u0001A\u0003A\u02cb\bA\u0001A\u0001A\u0001B\u0001B\u0001B\u0005B\u02d2"+
		"\bB\nB\fB\u02d5\tB\u0001B\u0003B\u02d8\bB\u0001C\u0001C\u0001C\u0001C"+
		"\u0001D\u0001D\u0001D\u0001D\u0001D\u0001D\u0001D\u0001D\u0001D\u0001"+
		"D\u0001D\u0001D\u0001D\u0001D\u0005D\u02ec\bD\nD\fD\u02ef\tD\u0001D\u0003"+
		"D\u02f2\bD\u0001D\u0001D\u0003D\u02f6\bD\u0001E\u0001E\u0001E\u0005E\u02fb"+
		"\bE\nE\fE\u02fe\tE\u0001E\u0003E\u0301\bE\u0001F\u0001F\u0001F\u0001F"+
		"\u0001F\u0001F\u0001F\u0001F\u0001F\u0001F\u0001F\u0005F\u030e\bF\nF\f"+
		"F\u0311\tF\u0001F\u0003F\u0314\bF\u0001F\u0001F\u0001F\u0001F\u0003F\u031a"+
		"\bF\u0001F\u0001F\u0001F\u0001F\u0003F\u0320\bF\u0001F\u0003F\u0323\b"+
		"F\u0003F\u0325\bF\u0001G\u0001G\u0001G\u0005G\u032a\bG\nG\fG\u032d\tG"+
		"\u0001G\u0003G\u0330\bG\u0001H\u0001H\u0001H\u0005H\u0335\bH\nH\fH\u0338"+
		"\tH\u0001H\u0003H\u033b\bH\u0001I\u0001I\u0001I\u0003I\u0340\bI\u0001"+
		"J\u0001J\u0001J\u0005J\u0345\bJ\nJ\fJ\u0348\tJ\u0001K\u0001K\u0001L\u0001"+
		"L\u0001M\u0004M\u034f\bM\u000bM\fM\u0350\u0001M\u0000\u0000N\u0000\u0002"+
		"\u0004\u0006\b\n\f\u000e\u0010\u0012\u0014\u0016\u0018\u001a\u001c\u001e"+
		" \"$&(*,.02468:<>@BDFHJLNPRTVXZ\\^`bdfhjlnprtvxz|~\u0080\u0082\u0084\u0086"+
		"\u0088\u008a\u008c\u008e\u0090\u0092\u0094\u0096\u0098\u009a\u0000\u0011"+
		"\u0001\u0000\r\u000e\u0001\u0000\u0013\u0015\u0002\u0000MMUU\u0001\u0000"+
		"<>\u0001\u0000\u0016\u0017\u0001\u0000WW\u0001\u0000\u001c\u001f\u0002"+
		"\u0000\u0001\u000111\u0002\u0000\b\b22\u0001\u00008:\u0002\u0000\u0003"+
		"\u0006UW\u0001\u0000PQ\u0001\u0000RT\u0001\u0000%&\u0001\u0000)*\u0003"+
		"\u0000+-<>EE\u0001\u000037\u038e\u0000\u009d\u0001\u0000\u0000\u0000\u0002"+
		"\u00ad\u0001\u0000\u0000\u0000\u0004\u00b6\u0001\u0000\u0000\u0000\u0006"+
		"\u00be\u0001\u0000\u0000\u0000\b\u00cd\u0001\u0000\u0000\u0000\n\u00d0"+
		"\u0001\u0000\u0000\u0000\f\u00d3\u0001\u0000\u0000\u0000\u000e\u00d6\u0001"+
		"\u0000\u0000\u0000\u0010\u00e3\u0001\u0000\u0000\u0000\u0012\u00e5\u0001"+
		"\u0000\u0000\u0000\u0014\u00f6\u0001\u0000\u0000\u0000\u0016\u00f8\u0001"+
		"\u0000\u0000\u0000\u0018\u00fe\u0001\u0000\u0000\u0000\u001a\u0109\u0001"+
		"\u0000\u0000\u0000\u001c\u0110\u0001\u0000\u0000\u0000\u001e\u0120\u0001"+
		"\u0000\u0000\u0000 \u0125\u0001\u0000\u0000\u0000\"\u0127\u0001\u0000"+
		"\u0000\u0000$\u012d\u0001\u0000\u0000\u0000&\u0138\u0001\u0000\u0000\u0000"+
		"(\u013c\u0001\u0000\u0000\u0000*\u014a\u0001\u0000\u0000\u0000,\u014e"+
		"\u0001\u0000\u0000\u0000.\u0154\u0001\u0000\u0000\u00000\u015f\u0001\u0000"+
		"\u0000\u00002\u0167\u0001\u0000\u0000\u00004\u0176\u0001\u0000\u0000\u0000"+
		"6\u0181\u0001\u0000\u0000\u00008\u0185\u0001\u0000\u0000\u0000:\u0196"+
		"\u0001\u0000\u0000\u0000<\u0198\u0001\u0000\u0000\u0000>\u01a2\u0001\u0000"+
		"\u0000\u0000@\u01aa\u0001\u0000\u0000\u0000B\u01bb\u0001\u0000\u0000\u0000"+
		"D\u01bd\u0001\u0000\u0000\u0000F\u01bf\u0001\u0000\u0000\u0000H\u01c1"+
		"\u0001\u0000\u0000\u0000J\u01ca\u0001\u0000\u0000\u0000L\u01d2\u0001\u0000"+
		"\u0000\u0000N\u01d5\u0001\u0000\u0000\u0000P\u01df\u0001\u0000\u0000\u0000"+
		"R\u01e1\u0001\u0000\u0000\u0000T\u01ea\u0001\u0000\u0000\u0000V\u01fc"+
		"\u0001\u0000\u0000\u0000X\u01fe\u0001\u0000\u0000\u0000Z\u0207\u0001\u0000"+
		"\u0000\u0000\\\u020a\u0001\u0000\u0000\u0000^\u020c\u0001\u0000\u0000"+
		"\u0000`\u0213\u0001\u0000\u0000\u0000b\u0218\u0001\u0000\u0000\u0000d"+
		"\u0220\u0001\u0000\u0000\u0000f\u0228\u0001\u0000\u0000\u0000h\u0230\u0001"+
		"\u0000\u0000\u0000j\u0238\u0001\u0000\u0000\u0000l\u0240\u0001\u0000\u0000"+
		"\u0000n\u0248\u0001\u0000\u0000\u0000p\u025b\u0001\u0000\u0000\u0000r"+
		"\u025d\u0001\u0000\u0000\u0000t\u026a\u0001\u0000\u0000\u0000v\u0277\u0001"+
		"\u0000\u0000\u0000x\u02a1\u0001\u0000\u0000\u0000z\u02a3\u0001\u0000\u0000"+
		"\u0000|\u02ac\u0001\u0000\u0000\u0000~\u02b3\u0001\u0000\u0000\u0000\u0080"+
		"\u02c6\u0001\u0000\u0000\u0000\u0082\u02c8\u0001\u0000\u0000\u0000\u0084"+
		"\u02ce\u0001\u0000\u0000\u0000\u0086\u02d9\u0001\u0000\u0000\u0000\u0088"+
		"\u02f5\u0001\u0000\u0000\u0000\u008a\u02f7\u0001\u0000\u0000\u0000\u008c"+
		"\u0324\u0001\u0000\u0000\u0000\u008e\u0326\u0001\u0000\u0000\u0000\u0090"+
		"\u0331\u0001\u0000\u0000\u0000\u0092\u033c\u0001\u0000\u0000\u0000\u0094"+
		"\u0341\u0001\u0000\u0000\u0000\u0096\u0349\u0001\u0000\u0000\u0000\u0098"+
		"\u034b\u0001\u0000\u0000\u0000\u009a\u034e\u0001\u0000\u0000\u0000\u009c"+
		"\u009e\u0003\u009aM\u0000\u009d\u009c\u0001\u0000\u0000\u0000\u009d\u009e"+
		"\u0001\u0000\u0000\u0000\u009e\u009f\u0001\u0000\u0000\u0000\u009f\u00a0"+
		"\u0003\b\u0004\u0000\u00a0\u00a2\u0003\n\u0005\u0000\u00a1\u00a3\u0003"+
		"\u009aM\u0000\u00a2\u00a1\u0001\u0000\u0000\u0000\u00a2\u00a3\u0001\u0000"+
		"\u0000\u0000\u00a3\u00a7\u0001\u0000\u0000\u0000\u00a4\u00a6\u0003\u0006"+
		"\u0003\u0000\u00a5\u00a4\u0001\u0000\u0000\u0000\u00a6\u00a9\u0001\u0000"+
		"\u0000\u0000\u00a7\u00a5\u0001\u0000\u0000\u0000\u00a7\u00a8\u0001\u0000"+
		"\u0000\u0000\u00a8\u00aa\u0001\u0000\u0000\u0000\u00a9\u00a7\u0001\u0000"+
		"\u0000\u0000\u00aa\u00ab\u0005\u0000\u0000\u0001\u00ab\u0001\u0001\u0000"+
		"\u0000\u0000\u00ac\u00ae\u0003\u009aM\u0000\u00ad\u00ac\u0001\u0000\u0000"+
		"\u0000\u00ad\u00ae\u0001\u0000\u0000\u0000\u00ae\u00af\u0001\u0000\u0000"+
		"\u0000\u00af\u00b1\u0003\\.\u0000\u00b0\u00b2\u0003\u009aM\u0000\u00b1"+
		"\u00b0\u0001\u0000\u0000\u0000\u00b1\u00b2\u0001\u0000\u0000\u0000\u00b2"+
		"\u00b3\u0001\u0000\u0000\u0000\u00b3\u00b4\u0005\u0000\u0000\u0001\u00b4"+
		"\u0003\u0001\u0000\u0000\u0000\u00b5\u00b7\u0003\u009aM\u0000\u00b6\u00b5"+
		"\u0001\u0000\u0000\u0000\u00b6\u00b7\u0001\u0000\u0000\u0000\u00b7\u00b8"+
		"\u0001\u0000\u0000\u0000\u00b8\u00ba\u0003 \u0010\u0000\u00b9\u00bb\u0003"+
		"\u009aM\u0000\u00ba\u00b9\u0001\u0000\u0000\u0000\u00ba\u00bb\u0001\u0000"+
		"\u0000\u0000\u00bb\u00bc\u0001\u0000\u0000\u0000\u00bc\u00bd\u0005\u0000"+
		"\u0000\u0001\u00bd\u0005\u0001\u0000\u0000\u0000\u00be\u00c2\u0003\b\u0004"+
		"\u0000\u00bf\u00c3\u0003\f\u0006\u0000\u00c0\u00c3\u0003\u000e\u0007\u0000"+
		"\u00c1\u00c3\u0003\u0010\b\u0000\u00c2\u00bf\u0001\u0000\u0000\u0000\u00c2"+
		"\u00c0\u0001\u0000\u0000\u0000\u00c2\u00c1\u0001\u0000\u0000\u0000\u00c3"+
		"\u00c5\u0001\u0000\u0000\u0000\u00c4\u00c6\u0003\u009aM\u0000\u00c5\u00c4"+
		"\u0001\u0000\u0000\u0000\u00c5\u00c6\u0001\u0000\u0000\u0000\u00c6\u0007"+
		"\u0001\u0000\u0000\u0000\u00c7\u00c9\u0005?\u0000\u0000\u00c8\u00ca\u0003"+
		"\u009aM\u0000\u00c9\u00c8\u0001\u0000\u0000\u0000\u00c9\u00ca\u0001\u0000"+
		"\u0000\u0000\u00ca\u00cc\u0001\u0000\u0000\u0000\u00cb\u00c7\u0001\u0000"+
		"\u0000\u0000\u00cc\u00cf\u0001\u0000\u0000\u0000\u00cd\u00cb\u0001\u0000"+
		"\u0000\u0000\u00cd\u00ce\u0001\u0000\u0000\u0000\u00ce\t\u0001\u0000\u0000"+
		"\u0000\u00cf\u00cd\u0001\u0000\u0000\u0000\u00d0\u00d1\u0005\t\u0000\u0000"+
		"\u00d1\u00d2\u0003\u0094J\u0000\u00d2\u000b\u0001\u0000\u0000\u0000\u00d3"+
		"\u00d4\u0005\n\u0000\u0000\u00d4\u00d5\u0005@\u0000\u0000\u00d5\r\u0001"+
		"\u0000\u0000\u0000\u00d6\u00d7\u0005\u000b\u0000\u0000\u00d7\u00d8\u0003"+
		"\u0094J\u0000\u00d8\u000f\u0001\u0000\u0000\u0000\u00d9\u00e4\u0003\u0012"+
		"\t\u0000\u00da\u00e4\u00030\u0018\u0000\u00db\u00e4\u00032\u0019\u0000"+
		"\u00dc\u00e4\u00038\u001c\u0000\u00dd\u00e4\u0003<\u001e\u0000\u00de\u00e4"+
		"\u0003>\u001f\u0000\u00df\u00e4\u0003H$\u0000\u00e0\u00e4\u0003J%\u0000"+
		"\u00e1\u00e4\u0003L&\u0000\u00e2\u00e4\u0003N\'\u0000\u00e3\u00d9\u0001"+
		"\u0000\u0000\u0000\u00e3\u00da\u0001\u0000\u0000\u0000\u00e3\u00db\u0001"+
		"\u0000\u0000\u0000\u00e3\u00dc\u0001\u0000\u0000\u0000\u00e3\u00dd\u0001"+
		"\u0000\u0000\u0000\u00e3\u00de\u0001\u0000\u0000\u0000\u00e3\u00df\u0001"+
		"\u0000\u0000\u0000\u00e3\u00e0\u0001\u0000\u0000\u0000\u00e3\u00e1\u0001"+
		"\u0000\u0000\u0000\u00e3\u00e2\u0001\u0000\u0000\u0000\u00e4\u0011\u0001"+
		"\u0000\u0000\u0000\u00e5\u00e6\u0005\f\u0000\u0000\u00e6\u00e8\u0003\u0096"+
		"K\u0000\u00e7\u00e9\u0003\u0016\u000b\u0000\u00e8\u00e7\u0001\u0000\u0000"+
		"\u0000\u00e8\u00e9\u0001\u0000\u0000\u0000\u00e9\u00ea\u0001\u0000\u0000"+
		"\u0000\u00ea\u00eb\u0005U\u0000\u0000\u00eb\u00ec\u0003\u0014\n\u0000"+
		"\u00ec\u0013\u0001\u0000\u0000\u0000\u00ed\u00f7\u0003\"\u0011\u0000\u00ee"+
		"\u00f3\u0003\u001a\r\u0000\u00ef\u00f0\u0005O\u0000\u0000\u00f0\u00f2"+
		"\u0003\u001a\r\u0000\u00f1\u00ef\u0001\u0000\u0000\u0000\u00f2\u00f5\u0001"+
		"\u0000\u0000\u0000\u00f3\u00f1\u0001\u0000\u0000\u0000\u00f3\u00f4\u0001"+
		"\u0000\u0000\u0000\u00f4\u00f7\u0001\u0000\u0000\u0000\u00f5\u00f3\u0001"+
		"\u0000\u0000\u0000\u00f6\u00ed\u0001\u0000\u0000\u0000\u00f6\u00ee\u0001"+
		"\u0000\u0000\u0000\u00f7\u0015\u0001\u0000\u0000\u0000\u00f8\u00fa\u0005"+
		"V\u0000\u0000\u00f9\u00fb\u0003\u0018\f\u0000\u00fa\u00f9\u0001\u0000"+
		"\u0000\u0000\u00fa\u00fb\u0001\u0000\u0000\u0000\u00fb\u00fc\u0001\u0000"+
		"\u0000\u0000\u00fc\u00fd\u0005W\u0000\u0000\u00fd\u0017\u0001\u0000\u0000"+
		"\u0000\u00fe\u0103\u0003\u0096K\u0000\u00ff\u0100\u0005L\u0000\u0000\u0100"+
		"\u0102\u0003\u0096K\u0000\u0101\u00ff\u0001\u0000\u0000\u0000\u0102\u0105"+
		"\u0001\u0000\u0000\u0000\u0103\u0101\u0001\u0000\u0000\u0000\u0103\u0104"+
		"\u0001\u0000\u0000\u0000\u0104\u0107\u0001\u0000\u0000\u0000\u0105\u0103"+
		"\u0001\u0000\u0000\u0000\u0106\u0108\u0005L\u0000\u0000\u0107\u0106\u0001"+
		"\u0000\u0000\u0000\u0107\u0108\u0001\u0000\u0000\u0000\u0108\u0019\u0001"+
		"\u0000\u0000\u0000\u0109\u010a\u0003\u0096K\u0000\u010a\u010c\u0005F\u0000"+
		"\u0000\u010b\u010d\u0003\u001c\u000e\u0000\u010c\u010b\u0001\u0000\u0000"+
		"\u0000\u010c\u010d\u0001\u0000\u0000\u0000\u010d\u010e\u0001\u0000\u0000"+
		"\u0000\u010e\u010f\u0005G\u0000\u0000\u010f\u001b\u0001\u0000\u0000\u0000"+
		"\u0110\u0115\u0003\u001e\u000f\u0000\u0111\u0112\u0005L\u0000\u0000\u0112"+
		"\u0114\u0003\u001e\u000f\u0000\u0113\u0111\u0001\u0000\u0000\u0000\u0114"+
		"\u0117\u0001\u0000\u0000\u0000\u0115\u0113\u0001\u0000\u0000\u0000\u0115"+
		"\u0116\u0001\u0000\u0000\u0000\u0116\u0119\u0001\u0000\u0000\u0000\u0117"+
		"\u0115\u0001\u0000\u0000\u0000\u0118\u011a\u0005L\u0000\u0000\u0119\u0118"+
		"\u0001\u0000\u0000\u0000\u0119\u011a\u0001\u0000\u0000\u0000\u011a\u001d"+
		"\u0001\u0000\u0000\u0000\u011b\u011c\u0003\u0096K\u0000\u011c\u011d\u0005"+
		"M\u0000\u0000\u011d\u011e\u0003 \u0010\u0000\u011e\u0121\u0001\u0000\u0000"+
		"\u0000\u011f\u0121\u0003 \u0010\u0000\u0120\u011b\u0001\u0000\u0000\u0000"+
		"\u0120\u011f\u0001\u0000\u0000\u0000\u0121\u001f\u0001\u0000\u0000\u0000"+
		"\u0122\u0126\u0003\"\u0011\u0000\u0123\u0126\u0003(\u0014\u0000\u0124"+
		"\u0126\u0003*\u0015\u0000\u0125\u0122\u0001\u0000\u0000\u0000\u0125\u0123"+
		"\u0001\u0000\u0000\u0000\u0125\u0124\u0001\u0000\u0000\u0000\u0126!\u0001"+
		"\u0000\u0000\u0000\u0127\u0129\u0005H\u0000\u0000\u0128\u012a\u0003$\u0012"+
		"\u0000\u0129\u0128\u0001\u0000\u0000\u0000\u0129\u012a\u0001\u0000\u0000"+
		"\u0000\u012a\u012b\u0001\u0000\u0000\u0000\u012b\u012c\u0005I\u0000\u0000"+
		"\u012c#\u0001\u0000\u0000\u0000\u012d\u0132\u0003&\u0013\u0000\u012e\u012f"+
		"\u0005L\u0000\u0000\u012f\u0131\u0003&\u0013\u0000\u0130\u012e\u0001\u0000"+
		"\u0000\u0000\u0131\u0134\u0001\u0000\u0000\u0000\u0132\u0130\u0001\u0000"+
		"\u0000\u0000\u0132\u0133\u0001\u0000\u0000\u0000\u0133\u0136\u0001\u0000"+
		"\u0000\u0000\u0134\u0132\u0001\u0000\u0000\u0000\u0135\u0137\u0005L\u0000"+
		"\u0000\u0136\u0135\u0001\u0000\u0000\u0000\u0136\u0137\u0001\u0000\u0000"+
		"\u0000\u0137%\u0001\u0000\u0000\u0000\u0138\u0139\u0003\u0096K\u0000\u0139"+
		"\u013a\u0005M\u0000\u0000\u013a\u013b\u0003 \u0010\u0000\u013b\'\u0001"+
		"\u0000\u0000\u0000\u013c\u013d\u0005F\u0000\u0000\u013d\u0142\u0003 \u0010"+
		"\u0000\u013e\u013f\u0005L\u0000\u0000\u013f\u0141\u0003 \u0010\u0000\u0140"+
		"\u013e\u0001\u0000\u0000\u0000\u0141\u0144\u0001\u0000\u0000\u0000\u0142"+
		"\u0140\u0001\u0000\u0000\u0000\u0142\u0143\u0001\u0000\u0000\u0000\u0143"+
		"\u0146\u0001\u0000\u0000\u0000\u0144\u0142\u0001\u0000\u0000\u0000\u0145"+
		"\u0147\u0005L\u0000\u0000\u0146\u0145\u0001\u0000\u0000\u0000\u0146\u0147"+
		"\u0001\u0000\u0000\u0000\u0147\u0148\u0001\u0000\u0000\u0000\u0148\u0149"+
		"\u0005G\u0000\u0000\u0149)\u0001\u0000\u0000\u0000\u014a\u014c\u0003\u0094"+
		"J\u0000\u014b\u014d\u0003,\u0016\u0000\u014c\u014b\u0001\u0000\u0000\u0000"+
		"\u014c\u014d\u0001\u0000\u0000\u0000\u014d+\u0001\u0000\u0000\u0000\u014e"+
		"\u0150\u0005V\u0000\u0000\u014f\u0151\u0003.\u0017\u0000\u0150\u014f\u0001"+
		"\u0000\u0000\u0000\u0150\u0151\u0001\u0000\u0000\u0000\u0151\u0152\u0001"+
		"\u0000\u0000\u0000\u0152\u0153\u0005W\u0000\u0000\u0153-\u0001\u0000\u0000"+
		"\u0000\u0154\u0159\u0003 \u0010\u0000\u0155\u0156\u0005L\u0000\u0000\u0156"+
		"\u0158\u0003 \u0010\u0000\u0157\u0155\u0001\u0000\u0000\u0000\u0158\u015b"+
		"\u0001\u0000\u0000\u0000\u0159\u0157\u0001\u0000\u0000\u0000\u0159\u015a"+
		"\u0001\u0000\u0000\u0000\u015a\u015d\u0001\u0000\u0000\u0000\u015b\u0159"+
		"\u0001\u0000\u0000\u0000\u015c\u015e\u0005L\u0000\u0000\u015d\u015c\u0001"+
		"\u0000\u0000\u0000\u015d\u015e\u0001\u0000\u0000\u0000\u015e/\u0001\u0000"+
		"\u0000\u0000\u015f\u0160\u0007\u0000\u0000\u0000\u0160\u0162\u0003\u0096"+
		"K\u0000\u0161\u0163\u0003Z-\u0000\u0162\u0161\u0001\u0000\u0000\u0000"+
		"\u0162\u0163\u0001\u0000\u0000\u0000\u0163\u0164\u0001\u0000\u0000\u0000"+
		"\u0164\u0165\u0005U\u0000\u0000\u0165\u0166\u0003\\.\u0000\u01661\u0001"+
		"\u0000\u0000\u0000\u0167\u0168\u0005\u000f\u0000\u0000\u0168\u016a\u0003"+
		"\u0096K\u0000\u0169\u016b\u0003\u0016\u000b\u0000\u016a\u0169\u0001\u0000"+
		"\u0000\u0000\u016a\u016b\u0001\u0000\u0000\u0000\u016b\u016c\u0001\u0000"+
		"\u0000\u0000\u016c\u016e\u0005F\u0000\u0000\u016d\u016f\u00034\u001a\u0000"+
		"\u016e\u016d\u0001\u0000\u0000\u0000\u016e\u016f\u0001\u0000\u0000\u0000"+
		"\u016f\u0170\u0001\u0000\u0000\u0000\u0170\u0171\u0005G\u0000\u0000\u0171"+
		"\u0172\u0005\u0001\u0000\u0000\u0172\u0173\u0003 \u0010\u0000\u0173\u0174"+
		"\u0005M\u0000\u0000\u0174\u0175\u0003V+\u0000\u01753\u0001\u0000\u0000"+
		"\u0000\u0176\u017b\u00036\u001b\u0000\u0177\u0178\u0005L\u0000\u0000\u0178"+
		"\u017a\u00036\u001b\u0000\u0179\u0177\u0001\u0000\u0000\u0000\u017a\u017d"+
		"\u0001\u0000\u0000\u0000\u017b\u0179\u0001\u0000\u0000\u0000\u017b\u017c"+
		"\u0001\u0000\u0000\u0000\u017c\u017f\u0001\u0000\u0000\u0000\u017d\u017b"+
		"\u0001\u0000\u0000\u0000\u017e\u0180\u0005L\u0000\u0000\u017f\u017e\u0001"+
		"\u0000\u0000\u0000\u017f\u0180\u0001\u0000\u0000\u0000\u01805\u0001\u0000"+
		"\u0000\u0000\u0181\u0182\u0003\u008cF\u0000\u0182\u0183\u0005M\u0000\u0000"+
		"\u0183\u0184\u0003 \u0010\u0000\u01847\u0001\u0000\u0000\u0000\u0185\u0186"+
		"\u0005\u0010\u0000\u0000\u0186\u0187\u0003\u0096K\u0000\u0187\u0188\u0005"+
		"M\u0000\u0000\u0188\u018c\u0003 \u0010\u0000\u0189\u018b\u0003:\u001d"+
		"\u0000\u018a\u0189\u0001\u0000\u0000\u0000\u018b\u018e\u0001\u0000\u0000"+
		"\u0000\u018c\u018a\u0001\u0000\u0000\u0000\u018c\u018d\u0001\u0000\u0000"+
		"\u0000\u018d9\u0001\u0000\u0000\u0000\u018e\u018c\u0001\u0000\u0000\u0000"+
		"\u018f\u0190\u0005 \u0000\u0000\u0190\u0191\u0005F\u0000\u0000\u0191\u0192"+
		"\u0003\\.\u0000\u0192\u0193\u0005G\u0000\u0000\u0193\u0197\u0001\u0000"+
		"\u0000\u0000\u0194\u0195\u0005!\u0000\u0000\u0195\u0197\u0003\\.\u0000"+
		"\u0196\u018f\u0001\u0000\u0000\u0000\u0196\u0194\u0001\u0000\u0000\u0000"+
		"\u0197;\u0001\u0000\u0000\u0000\u0198\u0199\u0005\u0011\u0000\u0000\u0199"+
		"\u019f\u0003\u0096K\u0000\u019a\u019c\u0005F\u0000\u0000\u019b\u019d\u0003"+
		"$\u0012\u0000\u019c\u019b\u0001\u0000\u0000\u0000\u019c\u019d\u0001\u0000"+
		"\u0000\u0000\u019d\u019e\u0001\u0000\u0000\u0000\u019e\u01a0\u0005G\u0000"+
		"\u0000\u019f\u019a\u0001\u0000\u0000\u0000\u019f\u01a0\u0001\u0000\u0000"+
		"\u0000\u01a0=\u0001\u0000\u0000\u0000\u01a1\u01a3\u0003@ \u0000\u01a2"+
		"\u01a1\u0001\u0000\u0000\u0000\u01a2\u01a3\u0001\u0000\u0000\u0000\u01a3"+
		"\u01a4\u0001\u0000\u0000\u0000\u01a4\u01a5\u0005\u0012\u0000\u0000\u01a5"+
		"\u01a8\u0003B!\u0000\u01a6\u01a7\u0005;\u0000\u0000\u01a7\u01a9\u0003"+
		"\\.\u0000\u01a8\u01a6\u0001\u0000\u0000\u0000\u01a8\u01a9\u0001\u0000"+
		"\u0000\u0000\u01a9?\u0001\u0000\u0000\u0000\u01aa\u01ab\u0007\u0001\u0000"+
		"\u0000\u01abA\u0001\u0000\u0000\u0000\u01ac\u01ad\u0003F#\u0000\u01ad"+
		"\u01ae\u0005M\u0000\u0000\u01ae\u01af\u0003\\.\u0000\u01af\u01bc\u0001"+
		"\u0000\u0000\u0000\u01b0\u01b2\u0003F#\u0000\u01b1\u01b0\u0001\u0000\u0000"+
		"\u0000\u01b1\u01b2\u0001\u0000\u0000\u0000\u01b2\u01b3\u0001\u0000\u0000"+
		"\u0000\u01b3\u01b6\u0003\u0094J\u0000\u01b4\u01b5\u0005\'\u0000\u0000"+
		"\u01b5\u01b7\u0003\\.\u0000\u01b6\u01b4\u0001\u0000\u0000\u0000\u01b6"+
		"\u01b7\u0001\u0000\u0000\u0000\u01b7\u01b8\u0001\u0000\u0000\u0000\u01b8"+
		"\u01b9\u0003D\"\u0000\u01b9\u01ba\u0003\\.\u0000\u01ba\u01bc\u0001\u0000"+
		"\u0000\u0000\u01bb\u01ac\u0001\u0000\u0000\u0000\u01bb\u01b1\u0001\u0000"+
		"\u0000\u0000\u01bcC\u0001\u0000\u0000\u0000\u01bd\u01be\u0007\u0002\u0000"+
		"\u0000\u01beE\u0001\u0000\u0000\u0000\u01bf\u01c0\u0007\u0003\u0000\u0000"+
		"\u01c0G\u0001\u0000\u0000\u0000\u01c1\u01c2\u0007\u0004\u0000\u0000\u01c2"+
		"\u01c7\u0003\u0094J\u0000\u01c3\u01c4\u0005W\u0000\u0000\u01c4\u01c6\u0003"+
		"\u0094J\u0000\u01c5\u01c3\u0001\u0000\u0000\u0000\u01c6\u01c9\u0001\u0000"+
		"\u0000\u0000\u01c7\u01c5\u0001\u0000\u0000\u0000\u01c7\u01c8\u0001\u0000"+
		"\u0000\u0000\u01c8I\u0001\u0000\u0000\u0000\u01c9\u01c7\u0001\u0000\u0000"+
		"\u0000\u01ca\u01ce\u0005\u0018\u0000\u0000\u01cb\u01cc\u0003\u0096K\u0000"+
		"\u01cc\u01cd\u0005U\u0000\u0000\u01cd\u01cf\u0001\u0000\u0000\u0000\u01ce"+
		"\u01cb\u0001\u0000\u0000\u0000\u01ce\u01cf\u0001\u0000\u0000\u0000\u01cf"+
		"\u01d0\u0001\u0000\u0000\u0000\u01d0\u01d1\u0003\\.\u0000\u01d1K\u0001"+
		"\u0000\u0000\u0000\u01d2\u01d3\u0005\u0019\u0000\u0000\u01d3\u01d4\u0003"+
		"\\.\u0000\u01d4M\u0001\u0000\u0000\u0000\u01d5\u01d6\u0005\u001a\u0000"+
		"\u0000\u01d6\u01d7\u0003\u0094J\u0000\u01d7\u01d8\u0005\u001b\u0000\u0000"+
		"\u01d8\u01da\u0003P(\u0000\u01d9\u01db\u0003T*\u0000\u01da\u01d9\u0001"+
		"\u0000\u0000\u0000\u01da\u01db\u0001\u0000\u0000\u0000\u01dbO\u0001\u0000"+
		"\u0000\u0000\u01dc\u01e0\u0003\u0094J\u0000\u01dd\u01e0\u0005@\u0000\u0000"+
		"\u01de\u01e0\u0003R)\u0000\u01df\u01dc\u0001\u0000\u0000\u0000\u01df\u01dd"+
		"\u0001\u0000\u0000\u0000\u01df\u01de\u0001\u0000\u0000\u0000\u01e0Q\u0001"+
		"\u0000\u0000\u0000\u01e1\u01e5\u0005V\u0000\u0000\u01e2\u01e4\b\u0005"+
		"\u0000\u0000\u01e3\u01e2\u0001\u0000\u0000\u0000\u01e4\u01e7\u0001\u0000"+
		"\u0000\u0000\u01e5\u01e3\u0001\u0000\u0000\u0000\u01e5\u01e6\u0001\u0000"+
		"\u0000\u0000\u01e6\u01e8\u0001\u0000\u0000\u0000\u01e7\u01e5\u0001\u0000"+
		"\u0000\u0000\u01e8\u01e9\u0005W\u0000\u0000\u01e9S\u0001\u0000\u0000\u0000"+
		"\u01ea\u01eb\u0007\u0006\u0000\u0000\u01ebU\u0001\u0000\u0000\u0000\u01ec"+
		"\u01ed\u0005Y\u0000\u0000\u01ed\u01f1\u0005\\\u0000\u0000\u01ee\u01f0"+
		"\u0003X,\u0000\u01ef\u01ee\u0001\u0000\u0000\u0000\u01f0\u01f3\u0001\u0000"+
		"\u0000\u0000\u01f1\u01ef\u0001\u0000\u0000\u0000\u01f1\u01f2\u0001\u0000"+
		"\u0000\u0000\u01f2\u01f5\u0001\u0000\u0000\u0000\u01f3\u01f1\u0001\u0000"+
		"\u0000\u0000\u01f4\u01f6\u0003\\.\u0000\u01f5\u01f4\u0001\u0000\u0000"+
		"\u0000\u01f5\u01f6\u0001\u0000\u0000\u0000\u01f6\u01f8\u0001\u0000\u0000"+
		"\u0000\u01f7\u01f9\u0003\u009aM\u0000\u01f8\u01f7\u0001\u0000\u0000\u0000"+
		"\u01f8\u01f9\u0001\u0000\u0000\u0000\u01f9\u01fa\u0001\u0000\u0000\u0000"+
		"\u01fa\u01fd\u0005]\u0000\u0000\u01fb\u01fd\u0003\\.\u0000\u01fc\u01ec"+
		"\u0001\u0000\u0000\u0000\u01fc\u01fb\u0001\u0000\u0000\u0000\u01fdW\u0001"+
		"\u0000\u0000\u0000\u01fe\u01ff\u0005\u000e\u0000\u0000\u01ff\u0201\u0003"+
		"\u008cF\u0000\u0200\u0202\u0003Z-\u0000\u0201\u0200\u0001\u0000\u0000"+
		"\u0000\u0201\u0202\u0001\u0000\u0000\u0000\u0202\u0203\u0001\u0000\u0000"+
		"\u0000\u0203\u0204\u0005U\u0000\u0000\u0204\u0205\u0003\\.\u0000\u0205"+
		"\u0206\u0003\u009aM\u0000\u0206Y\u0001\u0000\u0000\u0000\u0207\u0208\u0005"+
		"M\u0000\u0000\u0208\u0209\u0003 \u0010\u0000\u0209[\u0001\u0000\u0000"+
		"\u0000\u020a\u020b\u0003^/\u0000\u020b]\u0001\u0000\u0000\u0000\u020c"+
		"\u0210\u0003`0\u0000\u020d\u020f\u0003\u0098L\u0000\u020e\u020d\u0001"+
		"\u0000\u0000\u0000\u020f\u0212\u0001\u0000\u0000\u0000\u0210\u020e\u0001"+
		"\u0000\u0000\u0000\u0210\u0211\u0001\u0000\u0000\u0000\u0211_\u0001\u0000"+
		"\u0000\u0000\u0212\u0210\u0001\u0000\u0000\u0000\u0213\u0216\u0003b1\u0000"+
		"\u0214\u0215\u0007\u0007\u0000\u0000\u0215\u0217\u0003`0\u0000\u0216\u0214"+
		"\u0001\u0000\u0000\u0000\u0216\u0217\u0001\u0000\u0000\u0000\u0217a\u0001"+
		"\u0000\u0000\u0000\u0218\u021d\u0003d2\u0000\u0219\u021a\u0007\b\u0000"+
		"\u0000\u021a\u021c\u0003d2\u0000\u021b\u0219\u0001\u0000\u0000\u0000\u021c"+
		"\u021f\u0001\u0000\u0000\u0000\u021d\u021b\u0001\u0000\u0000\u0000\u021d"+
		"\u021e\u0001\u0000\u0000\u0000\u021ec\u0001\u0000\u0000\u0000\u021f\u021d"+
		"\u0001\u0000\u0000\u0000\u0220\u0225\u0003f3\u0000\u0221\u0222\u0005/"+
		"\u0000\u0000\u0222\u0224\u0003f3\u0000\u0223\u0221\u0001\u0000\u0000\u0000"+
		"\u0224\u0227\u0001\u0000\u0000\u0000\u0225\u0223\u0001\u0000\u0000\u0000"+
		"\u0225\u0226\u0001\u0000\u0000\u0000\u0226e\u0001\u0000\u0000\u0000\u0227"+
		"\u0225\u0001\u0000\u0000\u0000\u0228\u022d\u0003h4\u0000\u0229\u022a\u0005"+
		".\u0000\u0000\u022a\u022c\u0003h4\u0000\u022b\u0229\u0001\u0000\u0000"+
		"\u0000\u022c\u022f\u0001\u0000\u0000\u0000\u022d\u022b\u0001\u0000\u0000"+
		"\u0000\u022d\u022e\u0001\u0000\u0000\u0000\u022eg\u0001\u0000\u0000\u0000"+
		"\u022f\u022d\u0001\u0000\u0000\u0000\u0230\u0235\u0003j5\u0000\u0231\u0232"+
		"\u0007\t\u0000\u0000\u0232\u0234\u0003j5\u0000\u0233\u0231\u0001\u0000"+
		"\u0000\u0000\u0234\u0237\u0001\u0000\u0000\u0000\u0235\u0233\u0001\u0000"+
		"\u0000\u0000\u0235\u0236\u0001\u0000\u0000\u0000\u0236i\u0001\u0000\u0000"+
		"\u0000\u0237\u0235\u0001\u0000\u0000\u0000\u0238\u023d\u0003l6\u0000\u0239"+
		"\u023a\u0007\n\u0000\u0000\u023a\u023c\u0003l6\u0000\u023b\u0239\u0001"+
		"\u0000\u0000\u0000\u023c\u023f\u0001\u0000\u0000\u0000\u023d\u023b\u0001"+
		"\u0000\u0000\u0000\u023d\u023e\u0001\u0000\u0000\u0000\u023ek\u0001\u0000"+
		"\u0000\u0000\u023f\u023d\u0001\u0000\u0000\u0000\u0240\u0245\u0003n7\u0000"+
		"\u0241\u0242\u0007\u000b\u0000\u0000\u0242\u0244\u0003n7\u0000\u0243\u0241"+
		"\u0001\u0000\u0000\u0000\u0244\u0247\u0001\u0000\u0000\u0000\u0245\u0243"+
		"\u0001\u0000\u0000\u0000\u0245\u0246\u0001\u0000\u0000\u0000\u0246m\u0001"+
		"\u0000\u0000\u0000\u0247\u0245\u0001\u0000\u0000\u0000\u0248\u024d\u0003"+
		"p8\u0000\u0249\u024a\u0007\f\u0000\u0000\u024a\u024c\u0003p8\u0000\u024b"+
		"\u0249\u0001\u0000\u0000\u0000\u024c\u024f\u0001\u0000\u0000\u0000\u024d"+
		"\u024b\u0001\u0000\u0000\u0000\u024d\u024e\u0001\u0000\u0000\u0000\u024e"+
		"o\u0001\u0000\u0000\u0000\u024f\u024d\u0001\u0000\u0000\u0000\u0250\u025c"+
		"\u0003r9\u0000\u0251\u025c\u0003t:\u0000\u0252\u025c\u0003v;\u0000\u0253"+
		"\u025c\u0003|>\u0000\u0254\u0258\u00050\u0000\u0000\u0255\u0258\u0005"+
		"Q\u0000\u0000\u0256\u0258\u0003\u0098L\u0000\u0257\u0254\u0001\u0000\u0000"+
		"\u0000\u0257\u0255\u0001\u0000\u0000\u0000\u0257\u0256\u0001\u0000\u0000"+
		"\u0000\u0258\u0259\u0001\u0000\u0000\u0000\u0259\u025c\u0003p8\u0000\u025a"+
		"\u025c\u0003~?\u0000\u025b\u0250\u0001\u0000\u0000\u0000\u025b\u0251\u0001"+
		"\u0000\u0000\u0000\u025b\u0252\u0001\u0000\u0000\u0000\u025b\u0253\u0001"+
		"\u0000\u0000\u0000\u025b\u0257\u0001\u0000\u0000\u0000\u025b\u025a\u0001"+
		"\u0000\u0000\u0000\u025cq\u0001\u0000\u0000\u0000\u025d\u025e\u0005\""+
		"\u0000\u0000\u025e\u0260\u0003\\.\u0000\u025f\u0261\u0003\u009aM\u0000"+
		"\u0260\u025f\u0001\u0000\u0000\u0000\u0260\u0261\u0001\u0000\u0000\u0000"+
		"\u0261\u0262\u0001\u0000\u0000\u0000\u0262\u0263\u0005#\u0000\u0000\u0263"+
		"\u0265\u0003\\.\u0000\u0264\u0266\u0003\u009aM\u0000\u0265\u0264\u0001"+
		"\u0000\u0000\u0000\u0265\u0266\u0001\u0000\u0000\u0000\u0266\u0267\u0001"+
		"\u0000\u0000\u0000\u0267\u0268\u0005$\u0000\u0000\u0268\u0269\u0003\\"+
		".\u0000\u0269s\u0001\u0000\u0000\u0000\u026a\u026b\u0005\u000e\u0000\u0000"+
		"\u026b\u026d\u0003\u008cF\u0000\u026c\u026e\u0003Z-\u0000\u026d\u026c"+
		"\u0001\u0000\u0000\u0000\u026d\u026e\u0001\u0000\u0000\u0000\u026e\u026f"+
		"\u0001\u0000\u0000\u0000\u026f\u0270\u0005U\u0000\u0000\u0270\u0272\u0003"+
		"\\.\u0000\u0271\u0273\u0003\u009aM\u0000\u0272\u0271\u0001\u0000\u0000"+
		"\u0000\u0272\u0273\u0001\u0000\u0000\u0000\u0273\u0274\u0001\u0000\u0000"+
		"\u0000\u0274\u0275\u0005(\u0000\u0000\u0275\u0276\u0003\\.\u0000\u0276"+
		"u\u0001\u0000\u0000\u0000\u0277\u0278\u0007\r\u0000\u0000\u0278\u0279"+
		"\u0003\\.\u0000\u0279\u027a\u0005M\u0000\u0000\u027a\u027b\u0003x<\u0000"+
		"\u027bw\u0001\u0000\u0000\u0000\u027c\u027d\u0005Y\u0000\u0000\u027d\u027f"+
		"\u0005\\\u0000\u0000\u027e\u0280\u0003\u009aM\u0000\u027f\u027e\u0001"+
		"\u0000\u0000\u0000\u027f\u0280\u0001\u0000\u0000\u0000\u0280\u0281\u0001"+
		"\u0000\u0000\u0000\u0281\u0288\u0003z=\u0000\u0282\u0284\u0003\u009aM"+
		"\u0000\u0283\u0282\u0001\u0000\u0000\u0000\u0283\u0284\u0001\u0000\u0000"+
		"\u0000\u0284\u0285\u0001\u0000\u0000\u0000\u0285\u0287\u0003z=\u0000\u0286"+
		"\u0283\u0001\u0000\u0000\u0000\u0287\u028a\u0001\u0000\u0000\u0000\u0288"+
		"\u0286\u0001\u0000\u0000\u0000\u0288\u0289\u0001\u0000\u0000\u0000\u0289"+
		"\u028c\u0001\u0000\u0000\u0000\u028a\u0288\u0001\u0000\u0000\u0000\u028b"+
		"\u028d\u0003\u009aM\u0000\u028c\u028b\u0001\u0000\u0000\u0000\u028c\u028d"+
		"\u0001\u0000\u0000\u0000\u028d\u028e\u0001\u0000\u0000\u0000\u028e\u028f"+
		"\u0005]\u0000\u0000\u028f\u02a2\u0001\u0000\u0000\u0000\u0290\u0292\u0005"+
		"Y\u0000\u0000\u0291\u0293\u0003\u009aM\u0000\u0292\u0291\u0001\u0000\u0000"+
		"\u0000\u0292\u0293\u0001\u0000\u0000\u0000\u0293\u0294\u0001\u0000\u0000"+
		"\u0000\u0294\u029b\u0003z=\u0000\u0295\u0297\u0003\u009aM\u0000\u0296"+
		"\u0295\u0001\u0000\u0000\u0000\u0296\u0297\u0001\u0000\u0000\u0000\u0297"+
		"\u0298\u0001\u0000\u0000\u0000\u0298\u029a\u0003z=\u0000\u0299\u0296\u0001"+
		"\u0000\u0000\u0000\u029a\u029d\u0001\u0000\u0000\u0000\u029b\u0299\u0001"+
		"\u0000\u0000\u0000\u029b\u029c\u0001\u0000\u0000\u0000\u029c\u029f\u0001"+
		"\u0000\u0000\u0000\u029d\u029b\u0001\u0000\u0000\u0000\u029e\u02a0\u0003"+
		"\u009aM\u0000\u029f\u029e\u0001\u0000\u0000\u0000\u029f\u02a0\u0001\u0000"+
		"\u0000\u0000\u02a0\u02a2\u0001\u0000\u0000\u0000\u02a1\u027c\u0001\u0000"+
		"\u0000\u0000\u02a1\u0290\u0001\u0000\u0000\u0000\u02a2y\u0001\u0000\u0000"+
		"\u0000\u02a3\u02a4\u0005O\u0000\u0000\u02a4\u02a7\u0003\u008cF\u0000\u02a5"+
		"\u02a6\u0005\'\u0000\u0000\u02a6\u02a8\u0003\\.\u0000\u02a7\u02a5\u0001"+
		"\u0000\u0000\u0000\u02a7\u02a8\u0001\u0000\u0000\u0000\u02a8\u02a9\u0001"+
		"\u0000\u0000\u0000\u02a9\u02aa\u0005M\u0000\u0000\u02aa\u02ab\u0003V+"+
		"\u0000\u02ab{\u0001\u0000\u0000\u0000\u02ac\u02ad\u0007\u000e\u0000\u0000"+
		"\u02ad\u02ae\u0003\u008cF\u0000\u02ae\u02af\u0005(\u0000\u0000\u02af\u02b0"+
		"\u0003\\.\u0000\u02b0\u02b1\u0005M\u0000\u0000\u02b1\u02b2\u0003\\.\u0000"+
		"\u02b2}\u0001\u0000\u0000\u0000\u02b3\u02b7\u0003\u0088D\u0000\u02b4\u02b6"+
		"\u0003\u0080@\u0000\u02b5\u02b4\u0001\u0000\u0000\u0000\u02b6\u02b9\u0001"+
		"\u0000\u0000\u0000\u02b7\u02b5\u0001\u0000\u0000\u0000\u02b7\u02b8\u0001"+
		"\u0000\u0000\u0000\u02b8\u007f\u0001\u0000\u0000\u0000\u02b9\u02b7\u0001"+
		"\u0000\u0000\u0000\u02ba\u02c7\u0003\u0082A\u0000\u02bb\u02bd\u0005F\u0000"+
		"\u0000\u02bc\u02be\u0003\u008aE\u0000\u02bd\u02bc\u0001\u0000\u0000\u0000"+
		"\u02bd\u02be\u0001\u0000\u0000\u0000\u02be\u02bf\u0001\u0000\u0000\u0000"+
		"\u02bf\u02c7\u0005G\u0000\u0000\u02c0\u02c1\u0005N\u0000\u0000\u02c1\u02c7"+
		"\u0003\u0096K\u0000\u02c2\u02c3\u0005J\u0000\u0000\u02c3\u02c4\u0003\\"+
		".\u0000\u02c4\u02c5\u0005K\u0000\u0000\u02c5\u02c7\u0001\u0000\u0000\u0000"+
		"\u02c6\u02ba\u0001\u0000\u0000\u0000\u02c6\u02bb\u0001\u0000\u0000\u0000"+
		"\u02c6\u02c0\u0001\u0000\u0000\u0000\u02c6\u02c2\u0001\u0000\u0000\u0000"+
		"\u02c7\u0081\u0001\u0000\u0000\u0000\u02c8\u02ca\u0005H\u0000\u0000\u02c9"+
		"\u02cb\u0003\u0084B\u0000\u02ca\u02c9\u0001\u0000\u0000\u0000\u02ca\u02cb"+
		"\u0001\u0000\u0000\u0000\u02cb\u02cc\u0001\u0000\u0000\u0000\u02cc\u02cd"+
		"\u0005I\u0000\u0000\u02cd\u0083\u0001\u0000\u0000\u0000\u02ce\u02d3\u0003"+
		"\u0086C\u0000\u02cf\u02d0\u0005L\u0000\u0000\u02d0\u02d2\u0003\u0086C"+
		"\u0000\u02d1\u02cf\u0001\u0000\u0000\u0000\u02d2\u02d5\u0001\u0000\u0000"+
		"\u0000\u02d3\u02d1\u0001\u0000\u0000\u0000\u02d3\u02d4\u0001\u0000\u0000"+
		"\u0000\u02d4\u02d7\u0001\u0000\u0000\u0000\u02d5\u02d3\u0001\u0000\u0000"+
		"\u0000\u02d6\u02d8\u0005L\u0000\u0000\u02d7\u02d6\u0001\u0000\u0000\u0000"+
		"\u02d7\u02d8\u0001\u0000\u0000\u0000\u02d8\u0085\u0001\u0000\u0000\u0000"+
		"\u02d9\u02da\u0003\u0096K\u0000\u02da\u02db\u0005U\u0000\u0000\u02db\u02dc"+
		"\u0003\\.\u0000\u02dc\u0087\u0001\u0000\u0000\u0000\u02dd\u02f6\u0005"+
		"@\u0000\u0000\u02de\u02f6\u0005C\u0000\u0000\u02df\u02f6\u0005B\u0000"+
		"\u0000\u02e0\u02f6\u0005A\u0000\u0000\u02e1\u02f6\u0005+\u0000\u0000\u02e2"+
		"\u02f6\u0005,\u0000\u0000\u02e3\u02f6\u0005-\u0000\u0000\u02e4\u02f6\u0003"+
		"\u0094J\u0000\u02e5\u02e6\u0005F\u0000\u0000\u02e6\u02f6\u0005G\u0000"+
		"\u0000\u02e7\u02e8\u0005F\u0000\u0000\u02e8\u02ed\u0003\\.\u0000\u02e9"+
		"\u02ea\u0005L\u0000\u0000\u02ea\u02ec\u0003\\.\u0000\u02eb\u02e9\u0001"+
		"\u0000\u0000\u0000\u02ec\u02ef\u0001\u0000\u0000\u0000\u02ed\u02eb\u0001"+
		"\u0000\u0000\u0000\u02ed\u02ee\u0001\u0000\u0000\u0000\u02ee\u02f1\u0001"+
		"\u0000\u0000\u0000\u02ef\u02ed\u0001\u0000\u0000\u0000\u02f0\u02f2\u0005"+
		"L\u0000\u0000\u02f1\u02f0\u0001\u0000\u0000\u0000\u02f1\u02f2\u0001\u0000"+
		"\u0000\u0000\u02f2\u02f3\u0001\u0000\u0000\u0000\u02f3\u02f4\u0005G\u0000"+
		"\u0000\u02f4\u02f6\u0001\u0000\u0000\u0000\u02f5\u02dd\u0001\u0000\u0000"+
		"\u0000\u02f5\u02de\u0001\u0000\u0000\u0000\u02f5\u02df\u0001\u0000\u0000"+
		"\u0000\u02f5\u02e0\u0001\u0000\u0000\u0000\u02f5\u02e1\u0001\u0000\u0000"+
		"\u0000\u02f5\u02e2\u0001\u0000\u0000\u0000\u02f5\u02e3\u0001\u0000\u0000"+
		"\u0000\u02f5\u02e4\u0001\u0000\u0000\u0000\u02f5\u02e5\u0001\u0000\u0000"+
		"\u0000\u02f5\u02e7\u0001\u0000\u0000\u0000\u02f6\u0089\u0001\u0000\u0000"+
		"\u0000\u02f7\u02fc\u0003\\.\u0000\u02f8\u02f9\u0005L\u0000\u0000\u02f9"+
		"\u02fb\u0003\\.\u0000\u02fa\u02f8\u0001\u0000\u0000\u0000\u02fb\u02fe"+
		"\u0001\u0000\u0000\u0000\u02fc\u02fa\u0001\u0000\u0000\u0000\u02fc\u02fd"+
		"\u0001\u0000\u0000\u0000\u02fd\u0300\u0001\u0000\u0000\u0000\u02fe\u02fc"+
		"\u0001\u0000\u0000\u0000\u02ff\u0301\u0005L\u0000\u0000\u0300\u02ff\u0001"+
		"\u0000\u0000\u0000\u0300\u0301\u0001\u0000\u0000\u0000\u0301\u008b\u0001"+
		"\u0000\u0000\u0000\u0302\u0325\u0005D\u0000\u0000\u0303\u0325\u0005@\u0000"+
		"\u0000\u0304\u0325\u0005C\u0000\u0000\u0305\u0325\u0005B\u0000\u0000\u0306"+
		"\u0325\u0005A\u0000\u0000\u0307\u0308\u0005F\u0000\u0000\u0308\u0325\u0005"+
		"G\u0000\u0000\u0309\u030a\u0005F\u0000\u0000\u030a\u030f\u0003\u008cF"+
		"\u0000\u030b\u030c\u0005L\u0000\u0000\u030c\u030e\u0003\u008cF\u0000\u030d"+
		"\u030b\u0001\u0000\u0000\u0000\u030e\u0311\u0001\u0000\u0000\u0000\u030f"+
		"\u030d\u0001\u0000\u0000\u0000\u030f\u0310\u0001\u0000\u0000\u0000\u0310"+
		"\u0313\u0001\u0000\u0000\u0000\u0311\u030f\u0001\u0000\u0000\u0000\u0312"+
		"\u0314\u0005L\u0000\u0000\u0313\u0312\u0001\u0000\u0000\u0000\u0313\u0314"+
		"\u0001\u0000\u0000\u0000\u0314\u0315\u0001\u0000\u0000\u0000\u0315\u0316"+
		"\u0005G\u0000\u0000\u0316\u0325\u0001\u0000\u0000\u0000\u0317\u0319\u0005"+
		"H\u0000\u0000\u0318\u031a\u0003\u0090H\u0000\u0319\u0318\u0001\u0000\u0000"+
		"\u0000\u0319\u031a\u0001\u0000\u0000\u0000\u031a\u031b\u0001\u0000\u0000"+
		"\u0000\u031b\u0325\u0005I\u0000\u0000\u031c\u0322\u0003\u0094J\u0000\u031d"+
		"\u031f\u0005F\u0000\u0000\u031e\u0320\u0003\u008eG\u0000\u031f\u031e\u0001"+
		"\u0000\u0000\u0000\u031f\u0320\u0001\u0000\u0000\u0000\u0320\u0321\u0001"+
		"\u0000\u0000\u0000\u0321\u0323\u0005G\u0000\u0000\u0322\u031d\u0001\u0000"+
		"\u0000\u0000\u0322\u0323\u0001\u0000\u0000\u0000\u0323\u0325\u0001\u0000"+
		"\u0000\u0000\u0324\u0302\u0001\u0000\u0000\u0000\u0324\u0303\u0001\u0000"+
		"\u0000\u0000\u0324\u0304\u0001\u0000\u0000\u0000\u0324\u0305\u0001\u0000"+
		"\u0000\u0000\u0324\u0306\u0001\u0000\u0000\u0000\u0324\u0307\u0001\u0000"+
		"\u0000\u0000\u0324\u0309\u0001\u0000\u0000\u0000\u0324\u0317\u0001\u0000"+
		"\u0000\u0000\u0324\u031c\u0001\u0000\u0000\u0000\u0325\u008d\u0001\u0000"+
		"\u0000\u0000\u0326\u032b\u0003\u008cF\u0000\u0327\u0328\u0005L\u0000\u0000"+
		"\u0328\u032a\u0003\u008cF\u0000\u0329\u0327\u0001\u0000\u0000\u0000\u032a"+
		"\u032d\u0001\u0000\u0000\u0000\u032b\u0329\u0001\u0000\u0000\u0000\u032b"+
		"\u032c\u0001\u0000\u0000\u0000\u032c\u032f\u0001\u0000\u0000\u0000\u032d"+
		"\u032b\u0001\u0000\u0000\u0000\u032e\u0330\u0005L\u0000\u0000\u032f\u032e"+
		"\u0001\u0000\u0000\u0000\u032f\u0330\u0001\u0000\u0000\u0000\u0330\u008f"+
		"\u0001\u0000\u0000\u0000\u0331\u0336\u0003\u0092I\u0000\u0332\u0333\u0005"+
		"L\u0000\u0000\u0333\u0335\u0003\u0092I\u0000\u0334\u0332\u0001\u0000\u0000"+
		"\u0000\u0335\u0338\u0001\u0000\u0000\u0000\u0336\u0334\u0001\u0000\u0000"+
		"\u0000\u0336\u0337\u0001\u0000\u0000\u0000\u0337\u033a\u0001\u0000\u0000"+
		"\u0000\u0338\u0336\u0001\u0000\u0000\u0000\u0339\u033b\u0005L\u0000\u0000"+
		"\u033a\u0339\u0001\u0000\u0000\u0000\u033a\u033b\u0001\u0000\u0000\u0000"+
		"\u033b\u0091\u0001\u0000\u0000\u0000\u033c\u033f\u0003\u0096K\u0000\u033d"+
		"\u033e\u0005U\u0000\u0000\u033e\u0340\u0003\u008cF\u0000\u033f\u033d\u0001"+
		"\u0000\u0000\u0000\u033f\u0340\u0001\u0000\u0000\u0000\u0340\u0093\u0001"+
		"\u0000\u0000\u0000\u0341\u0346\u0003\u0096K\u0000\u0342\u0343\u0005N\u0000"+
		"\u0000\u0343\u0345\u0003\u0096K\u0000\u0344\u0342\u0001\u0000\u0000\u0000"+
		"\u0345\u0348\u0001\u0000\u0000\u0000\u0346\u0344\u0001\u0000\u0000\u0000"+
		"\u0346\u0347\u0001\u0000\u0000\u0000\u0347\u0095\u0001\u0000\u0000\u0000"+
		"\u0348\u0346\u0001\u0000\u0000\u0000\u0349\u034a\u0007\u000f\u0000\u0000"+
		"\u034a\u0097\u0001\u0000\u0000\u0000\u034b\u034c\u0007\u0010\u0000\u0000"+
		"\u034c\u0099\u0001\u0000\u0000\u0000\u034d\u034f\u0005Y\u0000\u0000\u034e"+
		"\u034d\u0001\u0000\u0000\u0000\u034f\u0350\u0001\u0000\u0000\u0000\u0350"+
		"\u034e\u0001\u0000\u0000\u0000\u0350\u0351\u0001\u0000\u0000\u0000\u0351"+
		"\u009b\u0001\u0000\u0000\u0000i\u009d\u00a2\u00a7\u00ad\u00b1\u00b6\u00ba"+
		"\u00c2\u00c5\u00c9\u00cd\u00e3\u00e8\u00f3\u00f6\u00fa\u0103\u0107\u010c"+
		"\u0115\u0119\u0120\u0125\u0129\u0132\u0136\u0142\u0146\u014c\u0150\u0159"+
		"\u015d\u0162\u016a\u016e\u017b\u017f\u018c\u0196\u019c\u019f\u01a2\u01a8"+
		"\u01b1\u01b6\u01bb\u01c7\u01ce\u01da\u01df\u01e5\u01f1\u01f5\u01f8\u01fc"+
		"\u0201\u0210\u0216\u021d\u0225\u022d\u0235\u023d\u0245\u024d\u0257\u025b"+
		"\u0260\u0265\u026d\u0272\u027f\u0283\u0288\u028c\u0292\u0296\u029b\u029f"+
		"\u02a1\u02a7\u02b7\u02bd\u02c6\u02ca\u02d3\u02d7\u02ed\u02f1\u02f5\u02fc"+
		"\u0300\u030f\u0313\u0319\u031f\u0322\u0324\u032b\u032f\u0336\u033a\u033f"+
		"\u0346\u0350";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}