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
		LAST=26, AND=27, OR=28, IMPLIES=29, NOT=30, ALWAYS=31, EVENTUALLY=32, 
		NEXT=33, INITIALLY=34, UNTIL=35, OTHERWISE=36, O=37, P=38, F=39, ANNOT=40, 
		STRING=41, RAT=42, DECIMAL=43, INT=44, UNDERSCORE=45, IDENT=46, LPAREN=47, 
		RPAREN=48, LBRACE=49, RBRACE=50, COMMA=51, COLON=52, DOT=53, BAR=54, PLUS=55, 
		MINUS=56, STAR=57, SLASH=58, PERCENT=59, EQ=60, LT=61, GT=62, NEWLINE=63, 
		COMMENT=64, WS=65, INDENT=66, DEDENT=67;
	public static final int
		RULE_program = 0, RULE_exprOnly = 1, RULE_typeExprOnly = 2, RULE_topItem = 3, 
		RULE_annotations = 4, RULE_moduleDecl = 5, RULE_importDecl = 6, RULE_openDecl = 7, 
		RULE_declaration = 8, RULE_typeDecl = 9, RULE_typeDefinition = 10, RULE_typeParams = 11, 
		RULE_nameList = 12, RULE_variant = 13, RULE_variantFieldList = 14, RULE_variantField = 15, 
		RULE_typeExpr = 16, RULE_recordType = 17, RULE_typeFieldList = 18, RULE_typeField = 19, 
		RULE_tupleOrParenType = 20, RULE_typeRef = 21, RULE_typeArgs = 22, RULE_typeExprList = 23, 
		RULE_valueDecl = 24, RULE_funcDecl = 25, RULE_paramList = 26, RULE_param = 27, 
		RULE_entityDecl = 28, RULE_ruleDecl = 29, RULE_ruleStrength = 30, RULE_ruleBody = 31, 
		RULE_deonticMod = 32, RULE_priorityDecl = 33, RULE_factDecl = 34, RULE_block = 35, 
		RULE_blockLetStmt = 36, RULE_typeAnnotation = 37, RULE_expr = 38, RULE_temporalPostfix = 39, 
		RULE_implication = 40, RULE_orExpr = 41, RULE_andExpr = 42, RULE_temporalBinary = 43, 
		RULE_comparison = 44, RULE_additive = 45, RULE_multiplicative = 46, RULE_unary = 47, 
		RULE_ifExpr = 48, RULE_letExpr = 49, RULE_matchExpr = 50, RULE_caseBody = 51, 
		RULE_caseArm = 52, RULE_postfix = 53, RULE_postfixSuffix = 54, RULE_recordConstructorFields = 55, 
		RULE_recordConstructorFieldList = 56, RULE_recordConstructorField = 57, 
		RULE_primary = 58, RULE_exprList = 59, RULE_pattern = 60, RULE_patternList = 61, 
		RULE_recordPatternFieldList = 62, RULE_recordPatternField = 63, RULE_qualifiedName = 64, 
		RULE_nameToken = 65, RULE_temporalUnaryOp = 66, RULE_newlines = 67;
	private static String[] makeRuleNames() {
		return new String[] {
			"program", "exprOnly", "typeExprOnly", "topItem", "annotations", "moduleDecl", 
			"importDecl", "openDecl", "declaration", "typeDecl", "typeDefinition", 
			"typeParams", "nameList", "variant", "variantFieldList", "variantField", 
			"typeExpr", "recordType", "typeFieldList", "typeField", "tupleOrParenType", 
			"typeRef", "typeArgs", "typeExprList", "valueDecl", "funcDecl", "paramList", 
			"param", "entityDecl", "ruleDecl", "ruleStrength", "ruleBody", "deonticMod", 
			"priorityDecl", "factDecl", "block", "blockLetStmt", "typeAnnotation", 
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
			null, "'->'", "'<='", "'>='", "'!='", "'module'", "'import'", "'open'", 
			"'type'", "'let'", "'func'", "'entity'", "'rule'", "'strict'", "'defeasible'", 
			"'defeater'", "'override'", "'fact'", "'if'", "'then'", "'else'", "'case'", 
			"'when'", "'in'", "'true'", "'false'", "'last'", "'and'", "'or'", "'implies'", 
			"'not'", "'always'", "'eventually'", "'next'", "'initially'", "'until'", 
			"'otherwise'", "'O'", "'P'", "'F'", null, null, null, null, null, "'_'", 
			null, "'('", "')'", "'{'", "'}'", "','", "':'", "'.'", "'|'", "'+'", 
			"'-'", "'*'", "'/'", "'%'", "'='", "'<'", "'>'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, "ARROW", "LE", "GE", "NE", "MODULE", "IMPORT", "OPEN", "TYPE", 
			"LET", "FUNC", "ENTITY", "RULE", "STRICT", "DEFEASIBLE", "DEFEATER", 
			"OVERRIDE", "FACT", "IF", "THEN", "ELSE", "CASE", "WHEN", "IN", "TRUE", 
			"FALSE", "LAST", "AND", "OR", "IMPLIES", "NOT", "ALWAYS", "EVENTUALLY", 
			"NEXT", "INITIALLY", "UNTIL", "OTHERWISE", "O", "P", "F", "ANNOT", "STRING", 
			"RAT", "DECIMAL", "INT", "UNDERSCORE", "IDENT", "LPAREN", "RPAREN", "LBRACE", 
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
			setState(137);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(136);
				newlines();
				}
			}

			setState(139);
			annotations();
			setState(140);
			moduleDecl();
			setState(142);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(141);
				newlines();
				}
			}

			setState(147);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & 1099511889856L) != 0)) {
				{
				{
				setState(144);
				topItem();
				}
				}
				setState(149);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(150);
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
			setState(153);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(152);
				newlines();
				}
			}

			setState(155);
			expr();
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
			setState(162);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(161);
				newlines();
				}
			}

			setState(164);
			typeExpr();
			setState(166);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(165);
				newlines();
				}
			}

			setState(168);
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
			setState(170);
			annotations();
			setState(174);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case IMPORT:
				{
				setState(171);
				importDecl();
				}
				break;
			case OPEN:
				{
				setState(172);
				openDecl();
				}
				break;
			case TYPE:
			case LET:
			case FUNC:
			case ENTITY:
			case RULE:
			case STRICT:
			case DEFEASIBLE:
			case DEFEATER:
			case OVERRIDE:
			case FACT:
				{
				setState(173);
				declaration();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(177);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(176);
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
			setState(185);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==ANNOT) {
				{
				{
				setState(179);
				match(ANNOT);
				setState(181);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(180);
					newlines();
					}
				}

				}
				}
				setState(187);
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
			setState(188);
			match(MODULE);
			setState(189);
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
			setState(191);
			match(IMPORT);
			setState(192);
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
			setState(194);
			match(OPEN);
			setState(195);
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
			setState(204);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case TYPE:
				enterOuterAlt(_localctx, 1);
				{
				setState(197);
				typeDecl();
				}
				break;
			case LET:
				enterOuterAlt(_localctx, 2);
				{
				setState(198);
				valueDecl();
				}
				break;
			case FUNC:
				enterOuterAlt(_localctx, 3);
				{
				setState(199);
				funcDecl();
				}
				break;
			case ENTITY:
				enterOuterAlt(_localctx, 4);
				{
				setState(200);
				entityDecl();
				}
				break;
			case RULE:
			case STRICT:
			case DEFEASIBLE:
			case DEFEATER:
				enterOuterAlt(_localctx, 5);
				{
				setState(201);
				ruleDecl();
				}
				break;
			case OVERRIDE:
				enterOuterAlt(_localctx, 6);
				{
				setState(202);
				priorityDecl();
				}
				break;
			case FACT:
				enterOuterAlt(_localctx, 7);
				{
				setState(203);
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
			setState(206);
			match(TYPE);
			setState(207);
			nameToken();
			setState(209);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LT) {
				{
				setState(208);
				typeParams();
				}
			}

			setState(211);
			match(EQ);
			setState(212);
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
			setState(223);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case LBRACE:
				enterOuterAlt(_localctx, 1);
				{
				setState(214);
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
				setState(215);
				variant();
				setState(220);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==BAR) {
					{
					{
					setState(216);
					match(BAR);
					setState(217);
					variant();
					}
					}
					setState(222);
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
			setState(225);
			match(LT);
			setState(226);
			nameList();
			setState(227);
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
			setState(229);
			nameToken();
			setState(234);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(230);
				match(COMMA);
				setState(231);
				nameToken();
				}
				}
				setState(236);
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
			setState(237);
			nameToken();
			setState(238);
			match(LPAREN);
			setState(239);
			variantFieldList();
			setState(240);
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
			setState(242);
			variantField();
			setState(247);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(243);
				match(COMMA);
				setState(244);
				variantField();
				}
				}
				setState(249);
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
			setState(255);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,17,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(250);
				nameToken();
				setState(251);
				match(COLON);
				setState(252);
				typeExpr();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(254);
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
			setState(260);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case LBRACE:
				enterOuterAlt(_localctx, 1);
				{
				setState(257);
				recordType();
				}
				break;
			case LPAREN:
				enterOuterAlt(_localctx, 2);
				{
				setState(258);
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
				setState(259);
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
			setState(262);
			match(LBRACE);
			setState(264);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 71330934292480L) != 0)) {
				{
				setState(263);
				typeFieldList();
				}
			}

			setState(266);
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
			setState(268);
			typeField();
			setState(273);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(269);
				match(COMMA);
				setState(270);
				typeField();
				}
				}
				setState(275);
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
			setState(276);
			nameToken();
			setState(277);
			match(COLON);
			setState(278);
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
			setState(297);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,22,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(280);
				match(LPAREN);
				setState(281);
				typeExpr();
				setState(282);
				match(RPAREN);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(284);
				match(LPAREN);
				setState(285);
				typeExpr();
				setState(286);
				match(COMMA);
				setState(287);
				typeExpr();
				setState(292);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(288);
					match(COMMA);
					setState(289);
					typeExpr();
					}
					}
					setState(294);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(295);
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
			setState(299);
			qualifiedName();
			setState(301);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LT) {
				{
				setState(300);
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
			setState(303);
			match(LT);
			setState(304);
			typeExprList();
			setState(305);
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
			setState(307);
			typeExpr();
			setState(312);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(308);
				match(COMMA);
				setState(309);
				typeExpr();
				}
				}
				setState(314);
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
	}

	public final ValueDeclContext valueDecl() throws RecognitionException {
		ValueDeclContext _localctx = new ValueDeclContext(_ctx, getState());
		enterRule(_localctx, 48, RULE_valueDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(315);
			match(LET);
			setState(316);
			nameToken();
			setState(318);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COLON) {
				{
				setState(317);
				typeAnnotation();
				}
			}

			setState(320);
			match(EQ);
			setState(321);
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
			setState(323);
			match(FUNC);
			setState(324);
			nameToken();
			setState(326);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==LT) {
				{
				setState(325);
				typeParams();
				}
			}

			setState(328);
			match(LPAREN);
			setState(330);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 843188096991232L) != 0)) {
				{
				setState(329);
				paramList();
				}
			}

			setState(332);
			match(RPAREN);
			setState(333);
			match(ARROW);
			setState(334);
			typeExpr();
			setState(335);
			match(COLON);
			setState(336);
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
			enterOuterAlt(_localctx, 1);
			{
			setState(338);
			param();
			setState(343);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(339);
				match(COMMA);
				setState(340);
				param();
				}
				}
				setState(345);
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
		enterRule(_localctx, 54, RULE_param);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(346);
			pattern();
			setState(347);
			match(COLON);
			setState(348);
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
			setState(350);
			match(ENTITY);
			setState(351);
			nameToken();
			setState(352);
			match(COLON);
			setState(353);
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
		enterRule(_localctx, 58, RULE_ruleDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(356);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 57344L) != 0)) {
				{
				setState(355);
				ruleStrength();
				}
			}

			setState(358);
			match(RULE);
			setState(359);
			ruleBody();
			setState(362);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==OTHERWISE) {
				{
				setState(360);
				match(OTHERWISE);
				setState(361);
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
		enterRule(_localctx, 60, RULE_ruleStrength);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(364);
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
		enterRule(_localctx, 62, RULE_ruleBody);
		int _la;
		try {
			setState(381);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,33,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(366);
				deonticMod();
				setState(367);
				match(COLON);
				setState(368);
				block();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(371);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,31,_ctx) ) {
				case 1:
					{
					setState(370);
					deonticMod();
					}
					break;
				}
				setState(373);
				qualifiedName();
				setState(376);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==WHEN) {
					{
					setState(374);
					match(WHEN);
					setState(375);
					expr();
					}
				}

				setState(378);
				match(COLON);
				setState(379);
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
		enterRule(_localctx, 64, RULE_deonticMod);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(383);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 962072674304L) != 0)) ) {
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
		enterRule(_localctx, 66, RULE_priorityDecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(385);
			match(OVERRIDE);
			setState(386);
			qualifiedName();
			setState(391);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==GT) {
				{
				{
				setState(387);
				match(GT);
				setState(388);
				qualifiedName();
				}
				}
				setState(393);
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
		enterRule(_localctx, 68, RULE_factDecl);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(394);
			match(FACT);
			setState(398);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,35,_ctx) ) {
			case 1:
				{
				setState(395);
				nameToken();
				setState(396);
				match(EQ);
				}
				break;
			}
			setState(400);
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
		enterRule(_localctx, 70, RULE_block);
		int _la;
		try {
			int _alt;
			setState(418);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case NEWLINE:
				enterOuterAlt(_localctx, 1);
				{
				setState(402);
				match(NEWLINE);
				setState(403);
				match(INDENT);
				setState(407);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,36,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(404);
						blockLetStmt();
						}
						} 
					}
					setState(409);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,36,_ctx);
				}
				setState(411);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 72302648885510656L) != 0)) {
					{
					setState(410);
					expr();
					}
				}

				setState(414);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(413);
					newlines();
					}
				}

				setState(416);
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
				setState(417);
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
		enterRule(_localctx, 72, RULE_blockLetStmt);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(420);
			match(LET);
			setState(421);
			pattern();
			setState(423);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COLON) {
				{
				setState(422);
				typeAnnotation();
				}
			}

			setState(425);
			match(EQ);
			setState(426);
			expr();
			setState(427);
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
		enterRule(_localctx, 74, RULE_typeAnnotation);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(429);
			match(COLON);
			setState(430);
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
		enterRule(_localctx, 76, RULE_expr);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(432);
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
			setState(434);
			implication();
			setState(438);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,41,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(435);
					temporalUnaryOp();
					}
					} 
				}
				setState(440);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,41,_ctx);
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
			setState(441);
			orExpr();
			setState(444);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,42,_ctx) ) {
			case 1:
				{
				setState(442);
				match(IMPLIES);
				setState(443);
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
			setState(446);
			andExpr();
			setState(451);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,43,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(447);
					match(OR);
					setState(448);
					andExpr();
					}
					} 
				}
				setState(453);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,43,_ctx);
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
			setState(454);
			temporalBinary();
			setState(459);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,44,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(455);
					match(AND);
					setState(456);
					temporalBinary();
					}
					} 
				}
				setState(461);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,44,_ctx);
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
			setState(462);
			comparison();
			setState(467);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,45,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(463);
					match(UNTIL);
					setState(464);
					comparison();
					}
					} 
				}
				setState(469);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,45,_ctx);
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
			setState(470);
			additive();
			setState(475);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,46,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(471);
					_la = _input.LA(1);
					if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 8070450532247928860L) != 0)) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(472);
					additive();
					}
					} 
				}
				setState(477);
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
			setState(478);
			multiplicative();
			setState(483);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,47,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(479);
					_la = _input.LA(1);
					if ( !(_la==PLUS || _la==MINUS) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(480);
					multiplicative();
					}
					} 
				}
				setState(485);
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
			setState(486);
			unary();
			setState(491);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,48,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(487);
					_la = _input.LA(1);
					if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 1008806316530991104L) != 0)) ) {
					_errHandler.recoverInline(this);
					}
					else {
						if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
						_errHandler.reportMatch(this);
						consume();
					}
					setState(488);
					unary();
					}
					} 
				}
				setState(493);
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
			setState(500);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case IF:
				enterOuterAlt(_localctx, 1);
				{
				setState(494);
				ifExpr();
				}
				break;
			case LET:
				enterOuterAlt(_localctx, 2);
				{
				setState(495);
				letExpr();
				}
				break;
			case CASE:
				enterOuterAlt(_localctx, 3);
				{
				setState(496);
				matchExpr();
				}
				break;
			case NOT:
			case MINUS:
				enterOuterAlt(_localctx, 4);
				{
				setState(497);
				_la = _input.LA(1);
				if ( !(_la==NOT || _la==MINUS) ) {
				_errHandler.recoverInline(this);
				}
				else {
					if ( _input.LA(1)==Token.EOF ) matchedEOF = true;
					_errHandler.reportMatch(this);
					consume();
				}
				setState(498);
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
				setState(499);
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
			setState(502);
			match(IF);
			setState(503);
			expr();
			setState(505);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(504);
				newlines();
				}
			}

			setState(507);
			match(THEN);
			setState(508);
			expr();
			setState(510);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(509);
				newlines();
				}
			}

			setState(512);
			match(ELSE);
			setState(513);
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
		public List<NewlinesContext> newlines() {
			return getRuleContexts(NewlinesContext.class);
		}
		public NewlinesContext newlines(int i) {
			return getRuleContext(NewlinesContext.class,i);
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
			setState(515);
			match(LET);
			setState(516);
			pattern();
			setState(518);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==COLON) {
				{
				setState(517);
				typeAnnotation();
				}
			}

			setState(520);
			match(EQ);
			setState(521);
			expr();
			setState(523);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(522);
				newlines();
				}
			}

			setState(525);
			match(IN);
			setState(527);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==NEWLINE) {
				{
				setState(526);
				newlines();
				}
			}

			setState(529);
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
		enterRule(_localctx, 100, RULE_matchExpr);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(531);
			match(CASE);
			setState(532);
			expr();
			setState(533);
			match(COLON);
			setState(534);
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
		enterRule(_localctx, 102, RULE_caseBody);
		int _la;
		try {
			int _alt;
			setState(573);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,63,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(536);
				match(NEWLINE);
				setState(537);
				match(INDENT);
				setState(539);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(538);
					newlines();
					}
				}

				setState(541);
				caseArm();
				setState(548);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,57,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
						setState(543);
						_errHandler.sync(this);
						_la = _input.LA(1);
						if (_la==NEWLINE) {
							{
							setState(542);
							newlines();
							}
						}

						setState(545);
						caseArm();
						}
						} 
					}
					setState(550);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,57,_ctx);
				}
				setState(552);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(551);
					newlines();
					}
				}

				setState(554);
				match(DEDENT);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(556);
				match(NEWLINE);
				setState(558);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==NEWLINE) {
					{
					setState(557);
					newlines();
					}
				}

				setState(560);
				caseArm();
				setState(567);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,61,_ctx);
				while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
					if ( _alt==1 ) {
						{
						{
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
						caseArm();
						}
						} 
					}
					setState(569);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,61,_ctx);
				}
				setState(571);
				_errHandler.sync(this);
				switch ( getInterpreter().adaptivePredict(_input,62,_ctx) ) {
				case 1:
					{
					setState(570);
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
		enterRule(_localctx, 104, RULE_caseArm);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(575);
			match(BAR);
			setState(576);
			pattern();
			setState(579);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==WHEN) {
				{
				setState(577);
				match(WHEN);
				setState(578);
				expr();
				}
			}

			setState(581);
			match(COLON);
			setState(582);
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
		enterRule(_localctx, 106, RULE_postfix);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(584);
			primary();
			setState(588);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & 9710886696517632L) != 0)) {
				{
				{
				setState(585);
				postfixSuffix();
				}
				}
				setState(590);
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
		enterRule(_localctx, 108, RULE_postfixSuffix);
		int _la;
		try {
			setState(599);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case LBRACE:
				enterOuterAlt(_localctx, 1);
				{
				setState(591);
				recordConstructorFields();
				}
				break;
			case LPAREN:
				enterOuterAlt(_localctx, 2);
				{
				setState(592);
				match(LPAREN);
				setState(594);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 72302648885510656L) != 0)) {
					{
					setState(593);
					exprList();
					}
				}

				setState(596);
				match(RPAREN);
				}
				break;
			case DOT:
				enterOuterAlt(_localctx, 3);
				{
				setState(597);
				match(DOT);
				setState(598);
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
		enterRule(_localctx, 110, RULE_recordConstructorFields);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(601);
			match(LBRACE);
			setState(603);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 71330934292480L) != 0)) {
				{
				setState(602);
				recordConstructorFieldList();
				}
			}

			setState(605);
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
		enterRule(_localctx, 112, RULE_recordConstructorFieldList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(607);
			recordConstructorField();
			setState(612);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(608);
				match(COMMA);
				setState(609);
				recordConstructorField();
				}
				}
				setState(614);
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
		enterRule(_localctx, 114, RULE_recordConstructorField);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(615);
			nameToken();
			setState(616);
			match(EQ);
			setState(617);
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
		enterRule(_localctx, 116, RULE_primary);
		int _la;
		try {
			setState(646);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,71,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(619);
				match(STRING);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(620);
				match(INT);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(621);
				match(DECIMAL);
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(622);
				match(RAT);
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(623);
				match(TRUE);
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(624);
				match(FALSE);
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(625);
				match(LAST);
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(626);
				qualifiedName();
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(627);
				match(LPAREN);
				setState(628);
				match(RPAREN);
				}
				break;
			case 10:
				enterOuterAlt(_localctx, 10);
				{
				setState(629);
				match(LPAREN);
				setState(630);
				expr();
				setState(631);
				match(RPAREN);
				}
				break;
			case 11:
				enterOuterAlt(_localctx, 11);
				{
				setState(633);
				match(LPAREN);
				setState(634);
				expr();
				setState(635);
				match(COMMA);
				setState(636);
				expr();
				setState(641);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(637);
					match(COMMA);
					setState(638);
					expr();
					}
					}
					setState(643);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(644);
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
		enterRule(_localctx, 118, RULE_exprList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(648);
			expr();
			setState(653);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(649);
				match(COMMA);
				setState(650);
				expr();
				}
				}
				setState(655);
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
		enterRule(_localctx, 120, RULE_pattern);
		int _la;
		try {
			setState(693);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,77,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(656);
				match(UNDERSCORE);
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(657);
				match(STRING);
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(658);
				match(INT);
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(659);
				match(DECIMAL);
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(660);
				match(RAT);
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(661);
				match(LPAREN);
				setState(662);
				match(RPAREN);
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(663);
				match(LPAREN);
				setState(664);
				pattern();
				setState(665);
				match(RPAREN);
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(667);
				match(LPAREN);
				setState(668);
				pattern();
				setState(669);
				match(COMMA);
				setState(670);
				pattern();
				setState(675);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==COMMA) {
					{
					{
					setState(671);
					match(COMMA);
					setState(672);
					pattern();
					}
					}
					setState(677);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(678);
				match(RPAREN);
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(680);
				match(LBRACE);
				setState(682);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 71330934292480L) != 0)) {
					{
					setState(681);
					recordPatternFieldList();
					}
				}

				setState(684);
				match(RBRACE);
				}
				break;
			case 10:
				enterOuterAlt(_localctx, 10);
				{
				setState(685);
				qualifiedName();
				setState(691);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==LPAREN) {
					{
					setState(686);
					match(LPAREN);
					setState(688);
					_errHandler.sync(this);
					_la = _input.LA(1);
					if ((((_la) & ~0x3f) == 0 && ((1L << _la) & 843188096991232L) != 0)) {
						{
						setState(687);
						patternList();
						}
					}

					setState(690);
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
		enterRule(_localctx, 122, RULE_patternList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(695);
			pattern();
			setState(700);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(696);
				match(COMMA);
				setState(697);
				pattern();
				}
				}
				setState(702);
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
		enterRule(_localctx, 124, RULE_recordPatternFieldList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(703);
			recordPatternField();
			setState(708);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==COMMA) {
				{
				{
				setState(704);
				match(COMMA);
				setState(705);
				recordPatternField();
				}
				}
				setState(710);
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
		enterRule(_localctx, 126, RULE_recordPatternField);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(711);
			nameToken();
			setState(714);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==EQ) {
				{
				setState(712);
				match(EQ);
				setState(713);
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
		enterRule(_localctx, 128, RULE_qualifiedName);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(716);
			nameToken();
			setState(721);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,81,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(717);
					match(DOT);
					setState(718);
					nameToken();
					}
					} 
				}
				setState(723);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,81,_ctx);
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
		enterRule(_localctx, 130, RULE_nameToken);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(724);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 71330934292480L) != 0)) ) {
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
		public TerminalNode INITIALLY() { return getToken(MDLParser.INITIALLY, 0); }
		public TemporalUnaryOpContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_temporalUnaryOp; }
	}

	public final TemporalUnaryOpContext temporalUnaryOp() throws RecognitionException {
		TemporalUnaryOpContext _localctx = new TemporalUnaryOpContext(_ctx, getState());
		enterRule(_localctx, 132, RULE_temporalUnaryOp);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(726);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & 32212254720L) != 0)) ) {
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
		enterRule(_localctx, 134, RULE_newlines);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(729); 
			_errHandler.sync(this);
			_alt = 1;
			do {
				switch (_alt) {
				case 1:
					{
					{
					setState(728);
					match(NEWLINE);
					}
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				setState(731); 
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,82,_ctx);
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
		"\u0004\u0001C\u02de\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001\u0002"+
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
		"A\u0007A\u0002B\u0007B\u0002C\u0007C\u0001\u0000\u0003\u0000\u008a\b\u0000"+
		"\u0001\u0000\u0001\u0000\u0001\u0000\u0003\u0000\u008f\b\u0000\u0001\u0000"+
		"\u0005\u0000\u0092\b\u0000\n\u0000\f\u0000\u0095\t\u0000\u0001\u0000\u0001"+
		"\u0000\u0001\u0001\u0003\u0001\u009a\b\u0001\u0001\u0001\u0001\u0001\u0003"+
		"\u0001\u009e\b\u0001\u0001\u0001\u0001\u0001\u0001\u0002\u0003\u0002\u00a3"+
		"\b\u0002\u0001\u0002\u0001\u0002\u0003\u0002\u00a7\b\u0002\u0001\u0002"+
		"\u0001\u0002\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0003\u0003"+
		"\u00af\b\u0003\u0001\u0003\u0003\u0003\u00b2\b\u0003\u0001\u0004\u0001"+
		"\u0004\u0003\u0004\u00b6\b\u0004\u0005\u0004\u00b8\b\u0004\n\u0004\f\u0004"+
		"\u00bb\t\u0004\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0006\u0001\u0006"+
		"\u0001\u0006\u0001\u0007\u0001\u0007\u0001\u0007\u0001\b\u0001\b\u0001"+
		"\b\u0001\b\u0001\b\u0001\b\u0001\b\u0003\b\u00cd\b\b\u0001\t\u0001\t\u0001"+
		"\t\u0003\t\u00d2\b\t\u0001\t\u0001\t\u0001\t\u0001\n\u0001\n\u0001\n\u0001"+
		"\n\u0005\n\u00db\b\n\n\n\f\n\u00de\t\n\u0003\n\u00e0\b\n\u0001\u000b\u0001"+
		"\u000b\u0001\u000b\u0001\u000b\u0001\f\u0001\f\u0001\f\u0005\f\u00e9\b"+
		"\f\n\f\f\f\u00ec\t\f\u0001\r\u0001\r\u0001\r\u0001\r\u0001\r\u0001\u000e"+
		"\u0001\u000e\u0001\u000e\u0005\u000e\u00f6\b\u000e\n\u000e\f\u000e\u00f9"+
		"\t\u000e\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f\u0001\u000f\u0003"+
		"\u000f\u0100\b\u000f\u0001\u0010\u0001\u0010\u0001\u0010\u0003\u0010\u0105"+
		"\b\u0010\u0001\u0011\u0001\u0011\u0003\u0011\u0109\b\u0011\u0001\u0011"+
		"\u0001\u0011\u0001\u0012\u0001\u0012\u0001\u0012\u0005\u0012\u0110\b\u0012"+
		"\n\u0012\f\u0012\u0113\t\u0012\u0001\u0013\u0001\u0013\u0001\u0013\u0001"+
		"\u0013\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001"+
		"\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0001\u0014\u0005\u0014\u0123"+
		"\b\u0014\n\u0014\f\u0014\u0126\t\u0014\u0001\u0014\u0001\u0014\u0003\u0014"+
		"\u012a\b\u0014\u0001\u0015\u0001\u0015\u0003\u0015\u012e\b\u0015\u0001"+
		"\u0016\u0001\u0016\u0001\u0016\u0001\u0016\u0001\u0017\u0001\u0017\u0001"+
		"\u0017\u0005\u0017\u0137\b\u0017\n\u0017\f\u0017\u013a\t\u0017\u0001\u0018"+
		"\u0001\u0018\u0001\u0018\u0003\u0018\u013f\b\u0018\u0001\u0018\u0001\u0018"+
		"\u0001\u0018\u0001\u0019\u0001\u0019\u0001\u0019\u0003\u0019\u0147\b\u0019"+
		"\u0001\u0019\u0001\u0019\u0003\u0019\u014b\b\u0019\u0001\u0019\u0001\u0019"+
		"\u0001\u0019\u0001\u0019\u0001\u0019\u0001\u0019\u0001\u001a\u0001\u001a"+
		"\u0001\u001a\u0005\u001a\u0156\b\u001a\n\u001a\f\u001a\u0159\t\u001a\u0001"+
		"\u001b\u0001\u001b\u0001\u001b\u0001\u001b\u0001\u001c\u0001\u001c\u0001"+
		"\u001c\u0001\u001c\u0001\u001c\u0001\u001d\u0003\u001d\u0165\b\u001d\u0001"+
		"\u001d\u0001\u001d\u0001\u001d\u0001\u001d\u0003\u001d\u016b\b\u001d\u0001"+
		"\u001e\u0001\u001e\u0001\u001f\u0001\u001f\u0001\u001f\u0001\u001f\u0001"+
		"\u001f\u0003\u001f\u0174\b\u001f\u0001\u001f\u0001\u001f\u0001\u001f\u0003"+
		"\u001f\u0179\b\u001f\u0001\u001f\u0001\u001f\u0001\u001f\u0003\u001f\u017e"+
		"\b\u001f\u0001 \u0001 \u0001!\u0001!\u0001!\u0001!\u0005!\u0186\b!\n!"+
		"\f!\u0189\t!\u0001\"\u0001\"\u0001\"\u0001\"\u0003\"\u018f\b\"\u0001\""+
		"\u0001\"\u0001#\u0001#\u0001#\u0005#\u0196\b#\n#\f#\u0199\t#\u0001#\u0003"+
		"#\u019c\b#\u0001#\u0003#\u019f\b#\u0001#\u0001#\u0003#\u01a3\b#\u0001"+
		"$\u0001$\u0001$\u0003$\u01a8\b$\u0001$\u0001$\u0001$\u0001$\u0001%\u0001"+
		"%\u0001%\u0001&\u0001&\u0001\'\u0001\'\u0005\'\u01b5\b\'\n\'\f\'\u01b8"+
		"\t\'\u0001(\u0001(\u0001(\u0003(\u01bd\b(\u0001)\u0001)\u0001)\u0005)"+
		"\u01c2\b)\n)\f)\u01c5\t)\u0001*\u0001*\u0001*\u0005*\u01ca\b*\n*\f*\u01cd"+
		"\t*\u0001+\u0001+\u0001+\u0005+\u01d2\b+\n+\f+\u01d5\t+\u0001,\u0001,"+
		"\u0001,\u0005,\u01da\b,\n,\f,\u01dd\t,\u0001-\u0001-\u0001-\u0005-\u01e2"+
		"\b-\n-\f-\u01e5\t-\u0001.\u0001.\u0001.\u0005.\u01ea\b.\n.\f.\u01ed\t"+
		".\u0001/\u0001/\u0001/\u0001/\u0001/\u0001/\u0003/\u01f5\b/\u00010\u0001"+
		"0\u00010\u00030\u01fa\b0\u00010\u00010\u00010\u00030\u01ff\b0\u00010\u0001"+
		"0\u00010\u00011\u00011\u00011\u00031\u0207\b1\u00011\u00011\u00011\u0003"+
		"1\u020c\b1\u00011\u00011\u00031\u0210\b1\u00011\u00011\u00012\u00012\u0001"+
		"2\u00012\u00012\u00013\u00013\u00013\u00033\u021c\b3\u00013\u00013\u0003"+
		"3\u0220\b3\u00013\u00053\u0223\b3\n3\f3\u0226\t3\u00013\u00033\u0229\b"+
		"3\u00013\u00013\u00013\u00013\u00033\u022f\b3\u00013\u00013\u00033\u0233"+
		"\b3\u00013\u00053\u0236\b3\n3\f3\u0239\t3\u00013\u00033\u023c\b3\u0003"+
		"3\u023e\b3\u00014\u00014\u00014\u00014\u00034\u0244\b4\u00014\u00014\u0001"+
		"4\u00015\u00015\u00055\u024b\b5\n5\f5\u024e\t5\u00016\u00016\u00016\u0003"+
		"6\u0253\b6\u00016\u00016\u00016\u00036\u0258\b6\u00017\u00017\u00037\u025c"+
		"\b7\u00017\u00017\u00018\u00018\u00018\u00058\u0263\b8\n8\f8\u0266\t8"+
		"\u00019\u00019\u00019\u00019\u0001:\u0001:\u0001:\u0001:\u0001:\u0001"+
		":\u0001:\u0001:\u0001:\u0001:\u0001:\u0001:\u0001:\u0001:\u0001:\u0001"+
		":\u0001:\u0001:\u0001:\u0001:\u0005:\u0280\b:\n:\f:\u0283\t:\u0001:\u0001"+
		":\u0003:\u0287\b:\u0001;\u0001;\u0001;\u0005;\u028c\b;\n;\f;\u028f\t;"+
		"\u0001<\u0001<\u0001<\u0001<\u0001<\u0001<\u0001<\u0001<\u0001<\u0001"+
		"<\u0001<\u0001<\u0001<\u0001<\u0001<\u0001<\u0001<\u0005<\u02a2\b<\n<"+
		"\f<\u02a5\t<\u0001<\u0001<\u0001<\u0001<\u0003<\u02ab\b<\u0001<\u0001"+
		"<\u0001<\u0001<\u0003<\u02b1\b<\u0001<\u0003<\u02b4\b<\u0003<\u02b6\b"+
		"<\u0001=\u0001=\u0001=\u0005=\u02bb\b=\n=\f=\u02be\t=\u0001>\u0001>\u0001"+
		">\u0005>\u02c3\b>\n>\f>\u02c6\t>\u0001?\u0001?\u0001?\u0003?\u02cb\b?"+
		"\u0001@\u0001@\u0001@\u0005@\u02d0\b@\n@\f@\u02d3\t@\u0001A\u0001A\u0001"+
		"B\u0001B\u0001C\u0004C\u02da\bC\u000bC\fC\u02db\u0001C\u0000\u0000D\u0000"+
		"\u0002\u0004\u0006\b\n\f\u000e\u0010\u0012\u0014\u0016\u0018\u001a\u001c"+
		"\u001e \"$&(*,.02468:<>@BDFHJLNPRTVXZ\\^`bdfhjlnprtvxz|~\u0080\u0082\u0084"+
		"\u0086\u0000\b\u0001\u0000\r\u000f\u0001\u0000%\'\u0002\u0000\u0002\u0004"+
		"<>\u0001\u000078\u0001\u00009;\u0002\u0000\u001e\u001e88\u0003\u0000\u0018"+
		"\u001a%\'..\u0001\u0000\u001f\"\u0308\u0000\u0089\u0001\u0000\u0000\u0000"+
		"\u0002\u0099\u0001\u0000\u0000\u0000\u0004\u00a2\u0001\u0000\u0000\u0000"+
		"\u0006\u00aa\u0001\u0000\u0000\u0000\b\u00b9\u0001\u0000\u0000\u0000\n"+
		"\u00bc\u0001\u0000\u0000\u0000\f\u00bf\u0001\u0000\u0000\u0000\u000e\u00c2"+
		"\u0001\u0000\u0000\u0000\u0010\u00cc\u0001\u0000\u0000\u0000\u0012\u00ce"+
		"\u0001\u0000\u0000\u0000\u0014\u00df\u0001\u0000\u0000\u0000\u0016\u00e1"+
		"\u0001\u0000\u0000\u0000\u0018\u00e5\u0001\u0000\u0000\u0000\u001a\u00ed"+
		"\u0001\u0000\u0000\u0000\u001c\u00f2\u0001\u0000\u0000\u0000\u001e\u00ff"+
		"\u0001\u0000\u0000\u0000 \u0104\u0001\u0000\u0000\u0000\"\u0106\u0001"+
		"\u0000\u0000\u0000$\u010c\u0001\u0000\u0000\u0000&\u0114\u0001\u0000\u0000"+
		"\u0000(\u0129\u0001\u0000\u0000\u0000*\u012b\u0001\u0000\u0000\u0000,"+
		"\u012f\u0001\u0000\u0000\u0000.\u0133\u0001\u0000\u0000\u00000\u013b\u0001"+
		"\u0000\u0000\u00002\u0143\u0001\u0000\u0000\u00004\u0152\u0001\u0000\u0000"+
		"\u00006\u015a\u0001\u0000\u0000\u00008\u015e\u0001\u0000\u0000\u0000:"+
		"\u0164\u0001\u0000\u0000\u0000<\u016c\u0001\u0000\u0000\u0000>\u017d\u0001"+
		"\u0000\u0000\u0000@\u017f\u0001\u0000\u0000\u0000B\u0181\u0001\u0000\u0000"+
		"\u0000D\u018a\u0001\u0000\u0000\u0000F\u01a2\u0001\u0000\u0000\u0000H"+
		"\u01a4\u0001\u0000\u0000\u0000J\u01ad\u0001\u0000\u0000\u0000L\u01b0\u0001"+
		"\u0000\u0000\u0000N\u01b2\u0001\u0000\u0000\u0000P\u01b9\u0001\u0000\u0000"+
		"\u0000R\u01be\u0001\u0000\u0000\u0000T\u01c6\u0001\u0000\u0000\u0000V"+
		"\u01ce\u0001\u0000\u0000\u0000X\u01d6\u0001\u0000\u0000\u0000Z\u01de\u0001"+
		"\u0000\u0000\u0000\\\u01e6\u0001\u0000\u0000\u0000^\u01f4\u0001\u0000"+
		"\u0000\u0000`\u01f6\u0001\u0000\u0000\u0000b\u0203\u0001\u0000\u0000\u0000"+
		"d\u0213\u0001\u0000\u0000\u0000f\u023d\u0001\u0000\u0000\u0000h\u023f"+
		"\u0001\u0000\u0000\u0000j\u0248\u0001\u0000\u0000\u0000l\u0257\u0001\u0000"+
		"\u0000\u0000n\u0259\u0001\u0000\u0000\u0000p\u025f\u0001\u0000\u0000\u0000"+
		"r\u0267\u0001\u0000\u0000\u0000t\u0286\u0001\u0000\u0000\u0000v\u0288"+
		"\u0001\u0000\u0000\u0000x\u02b5\u0001\u0000\u0000\u0000z\u02b7\u0001\u0000"+
		"\u0000\u0000|\u02bf\u0001\u0000\u0000\u0000~\u02c7\u0001\u0000\u0000\u0000"+
		"\u0080\u02cc\u0001\u0000\u0000\u0000\u0082\u02d4\u0001\u0000\u0000\u0000"+
		"\u0084\u02d6\u0001\u0000\u0000\u0000\u0086\u02d9\u0001\u0000\u0000\u0000"+
		"\u0088\u008a\u0003\u0086C\u0000\u0089\u0088\u0001\u0000\u0000\u0000\u0089"+
		"\u008a\u0001\u0000\u0000\u0000\u008a\u008b\u0001\u0000\u0000\u0000\u008b"+
		"\u008c\u0003\b\u0004\u0000\u008c\u008e\u0003\n\u0005\u0000\u008d\u008f"+
		"\u0003\u0086C\u0000\u008e\u008d\u0001\u0000\u0000\u0000\u008e\u008f\u0001"+
		"\u0000\u0000\u0000\u008f\u0093\u0001\u0000\u0000\u0000\u0090\u0092\u0003"+
		"\u0006\u0003\u0000\u0091\u0090\u0001\u0000\u0000\u0000\u0092\u0095\u0001"+
		"\u0000\u0000\u0000\u0093\u0091\u0001\u0000\u0000\u0000\u0093\u0094\u0001"+
		"\u0000\u0000\u0000\u0094\u0096\u0001\u0000\u0000\u0000\u0095\u0093\u0001"+
		"\u0000\u0000\u0000\u0096\u0097\u0005\u0000\u0000\u0001\u0097\u0001\u0001"+
		"\u0000\u0000\u0000\u0098\u009a\u0003\u0086C\u0000\u0099\u0098\u0001\u0000"+
		"\u0000\u0000\u0099\u009a\u0001\u0000\u0000\u0000\u009a\u009b\u0001\u0000"+
		"\u0000\u0000\u009b\u009d\u0003L&\u0000\u009c\u009e\u0003\u0086C\u0000"+
		"\u009d\u009c\u0001\u0000\u0000\u0000\u009d\u009e\u0001\u0000\u0000\u0000"+
		"\u009e\u009f\u0001\u0000\u0000\u0000\u009f\u00a0\u0005\u0000\u0000\u0001"+
		"\u00a0\u0003\u0001\u0000\u0000\u0000\u00a1\u00a3\u0003\u0086C\u0000\u00a2"+
		"\u00a1\u0001\u0000\u0000\u0000\u00a2\u00a3\u0001\u0000\u0000\u0000\u00a3"+
		"\u00a4\u0001\u0000\u0000\u0000\u00a4\u00a6\u0003 \u0010\u0000\u00a5\u00a7"+
		"\u0003\u0086C\u0000\u00a6\u00a5\u0001\u0000\u0000\u0000\u00a6\u00a7\u0001"+
		"\u0000\u0000\u0000\u00a7\u00a8\u0001\u0000\u0000\u0000\u00a8\u00a9\u0005"+
		"\u0000\u0000\u0001\u00a9\u0005\u0001\u0000\u0000\u0000\u00aa\u00ae\u0003"+
		"\b\u0004\u0000\u00ab\u00af\u0003\f\u0006\u0000\u00ac\u00af\u0003\u000e"+
		"\u0007\u0000\u00ad\u00af\u0003\u0010\b\u0000\u00ae\u00ab\u0001\u0000\u0000"+
		"\u0000\u00ae\u00ac\u0001\u0000\u0000\u0000\u00ae\u00ad\u0001\u0000\u0000"+
		"\u0000\u00af\u00b1\u0001\u0000\u0000\u0000\u00b0\u00b2\u0003\u0086C\u0000"+
		"\u00b1\u00b0\u0001\u0000\u0000\u0000\u00b1\u00b2\u0001\u0000\u0000\u0000"+
		"\u00b2\u0007\u0001\u0000\u0000\u0000\u00b3\u00b5\u0005(\u0000\u0000\u00b4"+
		"\u00b6\u0003\u0086C\u0000\u00b5\u00b4\u0001\u0000\u0000\u0000\u00b5\u00b6"+
		"\u0001\u0000\u0000\u0000\u00b6\u00b8\u0001\u0000\u0000\u0000\u00b7\u00b3"+
		"\u0001\u0000\u0000\u0000\u00b8\u00bb\u0001\u0000\u0000\u0000\u00b9\u00b7"+
		"\u0001\u0000\u0000\u0000\u00b9\u00ba\u0001\u0000\u0000\u0000\u00ba\t\u0001"+
		"\u0000\u0000\u0000\u00bb\u00b9\u0001\u0000\u0000\u0000\u00bc\u00bd\u0005"+
		"\u0005\u0000\u0000\u00bd\u00be\u0003\u0080@\u0000\u00be\u000b\u0001\u0000"+
		"\u0000\u0000\u00bf\u00c0\u0005\u0006\u0000\u0000\u00c0\u00c1\u0005)\u0000"+
		"\u0000\u00c1\r\u0001\u0000\u0000\u0000\u00c2\u00c3\u0005\u0007\u0000\u0000"+
		"\u00c3\u00c4\u0003\u0080@\u0000\u00c4\u000f\u0001\u0000\u0000\u0000\u00c5"+
		"\u00cd\u0003\u0012\t\u0000\u00c6\u00cd\u00030\u0018\u0000\u00c7\u00cd"+
		"\u00032\u0019\u0000\u00c8\u00cd\u00038\u001c\u0000\u00c9\u00cd\u0003:"+
		"\u001d\u0000\u00ca\u00cd\u0003B!\u0000\u00cb\u00cd\u0003D\"\u0000\u00cc"+
		"\u00c5\u0001\u0000\u0000\u0000\u00cc\u00c6\u0001\u0000\u0000\u0000\u00cc"+
		"\u00c7\u0001\u0000\u0000\u0000\u00cc\u00c8\u0001\u0000\u0000\u0000\u00cc"+
		"\u00c9\u0001\u0000\u0000\u0000\u00cc\u00ca\u0001\u0000\u0000\u0000\u00cc"+
		"\u00cb\u0001\u0000\u0000\u0000\u00cd\u0011\u0001\u0000\u0000\u0000\u00ce"+
		"\u00cf\u0005\b\u0000\u0000\u00cf\u00d1\u0003\u0082A\u0000\u00d0\u00d2"+
		"\u0003\u0016\u000b\u0000\u00d1\u00d0\u0001\u0000\u0000\u0000\u00d1\u00d2"+
		"\u0001\u0000\u0000\u0000\u00d2\u00d3\u0001\u0000\u0000\u0000\u00d3\u00d4"+
		"\u0005<\u0000\u0000\u00d4\u00d5\u0003\u0014\n\u0000\u00d5\u0013\u0001"+
		"\u0000\u0000\u0000\u00d6\u00e0\u0003\"\u0011\u0000\u00d7\u00dc\u0003\u001a"+
		"\r\u0000\u00d8\u00d9\u00056\u0000\u0000\u00d9\u00db\u0003\u001a\r\u0000"+
		"\u00da\u00d8\u0001\u0000\u0000\u0000\u00db\u00de\u0001\u0000\u0000\u0000"+
		"\u00dc\u00da\u0001\u0000\u0000\u0000\u00dc\u00dd\u0001\u0000\u0000\u0000"+
		"\u00dd\u00e0\u0001\u0000\u0000\u0000\u00de\u00dc\u0001\u0000\u0000\u0000"+
		"\u00df\u00d6\u0001\u0000\u0000\u0000\u00df\u00d7\u0001\u0000\u0000\u0000"+
		"\u00e0\u0015\u0001\u0000\u0000\u0000\u00e1\u00e2\u0005=\u0000\u0000\u00e2"+
		"\u00e3\u0003\u0018\f\u0000\u00e3\u00e4\u0005>\u0000\u0000\u00e4\u0017"+
		"\u0001\u0000\u0000\u0000\u00e5\u00ea\u0003\u0082A\u0000\u00e6\u00e7\u0005"+
		"3\u0000\u0000\u00e7\u00e9\u0003\u0082A\u0000\u00e8\u00e6\u0001\u0000\u0000"+
		"\u0000\u00e9\u00ec\u0001\u0000\u0000\u0000\u00ea\u00e8\u0001\u0000\u0000"+
		"\u0000\u00ea\u00eb\u0001\u0000\u0000\u0000\u00eb\u0019\u0001\u0000\u0000"+
		"\u0000\u00ec\u00ea\u0001\u0000\u0000\u0000\u00ed\u00ee\u0003\u0082A\u0000"+
		"\u00ee\u00ef\u0005/\u0000\u0000\u00ef\u00f0\u0003\u001c\u000e\u0000\u00f0"+
		"\u00f1\u00050\u0000\u0000\u00f1\u001b\u0001\u0000\u0000\u0000\u00f2\u00f7"+
		"\u0003\u001e\u000f\u0000\u00f3\u00f4\u00053\u0000\u0000\u00f4\u00f6\u0003"+
		"\u001e\u000f\u0000\u00f5\u00f3\u0001\u0000\u0000\u0000\u00f6\u00f9\u0001"+
		"\u0000\u0000\u0000\u00f7\u00f5\u0001\u0000\u0000\u0000\u00f7\u00f8\u0001"+
		"\u0000\u0000\u0000\u00f8\u001d\u0001\u0000\u0000\u0000\u00f9\u00f7\u0001"+
		"\u0000\u0000\u0000\u00fa\u00fb\u0003\u0082A\u0000\u00fb\u00fc\u00054\u0000"+
		"\u0000\u00fc\u00fd\u0003 \u0010\u0000\u00fd\u0100\u0001\u0000\u0000\u0000"+
		"\u00fe\u0100\u0003 \u0010\u0000\u00ff\u00fa\u0001\u0000\u0000\u0000\u00ff"+
		"\u00fe\u0001\u0000\u0000\u0000\u0100\u001f\u0001\u0000\u0000\u0000\u0101"+
		"\u0105\u0003\"\u0011\u0000\u0102\u0105\u0003(\u0014\u0000\u0103\u0105"+
		"\u0003*\u0015\u0000\u0104\u0101\u0001\u0000\u0000\u0000\u0104\u0102\u0001"+
		"\u0000\u0000\u0000\u0104\u0103\u0001\u0000\u0000\u0000\u0105!\u0001\u0000"+
		"\u0000\u0000\u0106\u0108\u00051\u0000\u0000\u0107\u0109\u0003$\u0012\u0000"+
		"\u0108\u0107\u0001\u0000\u0000\u0000\u0108\u0109\u0001\u0000\u0000\u0000"+
		"\u0109\u010a\u0001\u0000\u0000\u0000\u010a\u010b\u00052\u0000\u0000\u010b"+
		"#\u0001\u0000\u0000\u0000\u010c\u0111\u0003&\u0013\u0000\u010d\u010e\u0005"+
		"3\u0000\u0000\u010e\u0110\u0003&\u0013\u0000\u010f\u010d\u0001\u0000\u0000"+
		"\u0000\u0110\u0113\u0001\u0000\u0000\u0000\u0111\u010f\u0001\u0000\u0000"+
		"\u0000\u0111\u0112\u0001\u0000\u0000\u0000\u0112%\u0001\u0000\u0000\u0000"+
		"\u0113\u0111\u0001\u0000\u0000\u0000\u0114\u0115\u0003\u0082A\u0000\u0115"+
		"\u0116\u00054\u0000\u0000\u0116\u0117\u0003 \u0010\u0000\u0117\'\u0001"+
		"\u0000\u0000\u0000\u0118\u0119\u0005/\u0000\u0000\u0119\u011a\u0003 \u0010"+
		"\u0000\u011a\u011b\u00050\u0000\u0000\u011b\u012a\u0001\u0000\u0000\u0000"+
		"\u011c\u011d\u0005/\u0000\u0000\u011d\u011e\u0003 \u0010\u0000\u011e\u011f"+
		"\u00053\u0000\u0000\u011f\u0124\u0003 \u0010\u0000\u0120\u0121\u00053"+
		"\u0000\u0000\u0121\u0123\u0003 \u0010\u0000\u0122\u0120\u0001\u0000\u0000"+
		"\u0000\u0123\u0126\u0001\u0000\u0000\u0000\u0124\u0122\u0001\u0000\u0000"+
		"\u0000\u0124\u0125\u0001\u0000\u0000\u0000\u0125\u0127\u0001\u0000\u0000"+
		"\u0000\u0126\u0124\u0001\u0000\u0000\u0000\u0127\u0128\u00050\u0000\u0000"+
		"\u0128\u012a\u0001\u0000\u0000\u0000\u0129\u0118\u0001\u0000\u0000\u0000"+
		"\u0129\u011c\u0001\u0000\u0000\u0000\u012a)\u0001\u0000\u0000\u0000\u012b"+
		"\u012d\u0003\u0080@\u0000\u012c\u012e\u0003,\u0016\u0000\u012d\u012c\u0001"+
		"\u0000\u0000\u0000\u012d\u012e\u0001\u0000\u0000\u0000\u012e+\u0001\u0000"+
		"\u0000\u0000\u012f\u0130\u0005=\u0000\u0000\u0130\u0131\u0003.\u0017\u0000"+
		"\u0131\u0132\u0005>\u0000\u0000\u0132-\u0001\u0000\u0000\u0000\u0133\u0138"+
		"\u0003 \u0010\u0000\u0134\u0135\u00053\u0000\u0000\u0135\u0137\u0003 "+
		"\u0010\u0000\u0136\u0134\u0001\u0000\u0000\u0000\u0137\u013a\u0001\u0000"+
		"\u0000\u0000\u0138\u0136\u0001\u0000\u0000\u0000\u0138\u0139\u0001\u0000"+
		"\u0000\u0000\u0139/\u0001\u0000\u0000\u0000\u013a\u0138\u0001\u0000\u0000"+
		"\u0000\u013b\u013c\u0005\t\u0000\u0000\u013c\u013e\u0003\u0082A\u0000"+
		"\u013d\u013f\u0003J%\u0000\u013e\u013d\u0001\u0000\u0000\u0000\u013e\u013f"+
		"\u0001\u0000\u0000\u0000\u013f\u0140\u0001\u0000\u0000\u0000\u0140\u0141"+
		"\u0005<\u0000\u0000\u0141\u0142\u0003L&\u0000\u01421\u0001\u0000\u0000"+
		"\u0000\u0143\u0144\u0005\n\u0000\u0000\u0144\u0146\u0003\u0082A\u0000"+
		"\u0145\u0147\u0003\u0016\u000b\u0000\u0146\u0145\u0001\u0000\u0000\u0000"+
		"\u0146\u0147\u0001\u0000\u0000\u0000\u0147\u0148\u0001\u0000\u0000\u0000"+
		"\u0148\u014a\u0005/\u0000\u0000\u0149\u014b\u00034\u001a\u0000\u014a\u0149"+
		"\u0001\u0000\u0000\u0000\u014a\u014b\u0001\u0000\u0000\u0000\u014b\u014c"+
		"\u0001\u0000\u0000\u0000\u014c\u014d\u00050\u0000\u0000\u014d\u014e\u0005"+
		"\u0001\u0000\u0000\u014e\u014f\u0003 \u0010\u0000\u014f\u0150\u00054\u0000"+
		"\u0000\u0150\u0151\u0003F#\u0000\u01513\u0001\u0000\u0000\u0000\u0152"+
		"\u0157\u00036\u001b\u0000\u0153\u0154\u00053\u0000\u0000\u0154\u0156\u0003"+
		"6\u001b\u0000\u0155\u0153\u0001\u0000\u0000\u0000\u0156\u0159\u0001\u0000"+
		"\u0000\u0000\u0157\u0155\u0001\u0000\u0000\u0000\u0157\u0158\u0001\u0000"+
		"\u0000\u0000\u01585\u0001\u0000\u0000\u0000\u0159\u0157\u0001\u0000\u0000"+
		"\u0000\u015a\u015b\u0003x<\u0000\u015b\u015c\u00054\u0000\u0000\u015c"+
		"\u015d\u0003 \u0010\u0000\u015d7\u0001\u0000\u0000\u0000\u015e\u015f\u0005"+
		"\u000b\u0000\u0000\u015f\u0160\u0003\u0082A\u0000\u0160\u0161\u00054\u0000"+
		"\u0000\u0161\u0162\u0003 \u0010\u0000\u01629\u0001\u0000\u0000\u0000\u0163"+
		"\u0165\u0003<\u001e\u0000\u0164\u0163\u0001\u0000\u0000\u0000\u0164\u0165"+
		"\u0001\u0000\u0000\u0000\u0165\u0166\u0001\u0000\u0000\u0000\u0166\u0167"+
		"\u0005\f\u0000\u0000\u0167\u016a\u0003>\u001f\u0000\u0168\u0169\u0005"+
		"$\u0000\u0000\u0169\u016b\u0003L&\u0000\u016a\u0168\u0001\u0000\u0000"+
		"\u0000\u016a\u016b\u0001\u0000\u0000\u0000\u016b;\u0001\u0000\u0000\u0000"+
		"\u016c\u016d\u0007\u0000\u0000\u0000\u016d=\u0001\u0000\u0000\u0000\u016e"+
		"\u016f\u0003@ \u0000\u016f\u0170\u00054\u0000\u0000\u0170\u0171\u0003"+
		"F#\u0000\u0171\u017e\u0001\u0000\u0000\u0000\u0172\u0174\u0003@ \u0000"+
		"\u0173\u0172\u0001\u0000\u0000\u0000\u0173\u0174\u0001\u0000\u0000\u0000"+
		"\u0174\u0175\u0001\u0000\u0000\u0000\u0175\u0178\u0003\u0080@\u0000\u0176"+
		"\u0177\u0005\u0016\u0000\u0000\u0177\u0179\u0003L&\u0000\u0178\u0176\u0001"+
		"\u0000\u0000\u0000\u0178\u0179\u0001\u0000\u0000\u0000\u0179\u017a\u0001"+
		"\u0000\u0000\u0000\u017a\u017b\u00054\u0000\u0000\u017b\u017c\u0003F#"+
		"\u0000\u017c\u017e\u0001\u0000\u0000\u0000\u017d\u016e\u0001\u0000\u0000"+
		"\u0000\u017d\u0173\u0001\u0000\u0000\u0000\u017e?\u0001\u0000\u0000\u0000"+
		"\u017f\u0180\u0007\u0001\u0000\u0000\u0180A\u0001\u0000\u0000\u0000\u0181"+
		"\u0182\u0005\u0010\u0000\u0000\u0182\u0187\u0003\u0080@\u0000\u0183\u0184"+
		"\u0005>\u0000\u0000\u0184\u0186\u0003\u0080@\u0000\u0185\u0183\u0001\u0000"+
		"\u0000\u0000\u0186\u0189\u0001\u0000\u0000\u0000\u0187\u0185\u0001\u0000"+
		"\u0000\u0000\u0187\u0188\u0001\u0000\u0000\u0000\u0188C\u0001\u0000\u0000"+
		"\u0000\u0189\u0187\u0001\u0000\u0000\u0000\u018a\u018e\u0005\u0011\u0000"+
		"\u0000\u018b\u018c\u0003\u0082A\u0000\u018c\u018d\u0005<\u0000\u0000\u018d"+
		"\u018f\u0001\u0000\u0000\u0000\u018e\u018b\u0001\u0000\u0000\u0000\u018e"+
		"\u018f\u0001\u0000\u0000\u0000\u018f\u0190\u0001\u0000\u0000\u0000\u0190"+
		"\u0191\u0003L&\u0000\u0191E\u0001\u0000\u0000\u0000\u0192\u0193\u0005"+
		"?\u0000\u0000\u0193\u0197\u0005B\u0000\u0000\u0194\u0196\u0003H$\u0000"+
		"\u0195\u0194\u0001\u0000\u0000\u0000\u0196\u0199\u0001\u0000\u0000\u0000"+
		"\u0197\u0195\u0001\u0000\u0000\u0000\u0197\u0198\u0001\u0000\u0000\u0000"+
		"\u0198\u019b\u0001\u0000\u0000\u0000\u0199\u0197\u0001\u0000\u0000\u0000"+
		"\u019a\u019c\u0003L&\u0000\u019b\u019a\u0001\u0000\u0000\u0000\u019b\u019c"+
		"\u0001\u0000\u0000\u0000\u019c\u019e\u0001\u0000\u0000\u0000\u019d\u019f"+
		"\u0003\u0086C\u0000\u019e\u019d\u0001\u0000\u0000\u0000\u019e\u019f\u0001"+
		"\u0000\u0000\u0000\u019f\u01a0\u0001\u0000\u0000\u0000\u01a0\u01a3\u0005"+
		"C\u0000\u0000\u01a1\u01a3\u0003L&\u0000\u01a2\u0192\u0001\u0000\u0000"+
		"\u0000\u01a2\u01a1\u0001\u0000\u0000\u0000\u01a3G\u0001\u0000\u0000\u0000"+
		"\u01a4\u01a5\u0005\t\u0000\u0000\u01a5\u01a7\u0003x<\u0000\u01a6\u01a8"+
		"\u0003J%\u0000\u01a7\u01a6\u0001\u0000\u0000\u0000\u01a7\u01a8\u0001\u0000"+
		"\u0000\u0000\u01a8\u01a9\u0001\u0000\u0000\u0000\u01a9\u01aa\u0005<\u0000"+
		"\u0000\u01aa\u01ab\u0003L&\u0000\u01ab\u01ac\u0003\u0086C\u0000\u01ac"+
		"I\u0001\u0000\u0000\u0000\u01ad\u01ae\u00054\u0000\u0000\u01ae\u01af\u0003"+
		" \u0010\u0000\u01afK\u0001\u0000\u0000\u0000\u01b0\u01b1\u0003N\'\u0000"+
		"\u01b1M\u0001\u0000\u0000\u0000\u01b2\u01b6\u0003P(\u0000\u01b3\u01b5"+
		"\u0003\u0084B\u0000\u01b4\u01b3\u0001\u0000\u0000\u0000\u01b5\u01b8\u0001"+
		"\u0000\u0000\u0000\u01b6\u01b4\u0001\u0000\u0000\u0000\u01b6\u01b7\u0001"+
		"\u0000\u0000\u0000\u01b7O\u0001\u0000\u0000\u0000\u01b8\u01b6\u0001\u0000"+
		"\u0000\u0000\u01b9\u01bc\u0003R)\u0000\u01ba\u01bb\u0005\u001d\u0000\u0000"+
		"\u01bb\u01bd\u0003P(\u0000\u01bc\u01ba\u0001\u0000\u0000\u0000\u01bc\u01bd"+
		"\u0001\u0000\u0000\u0000\u01bdQ\u0001\u0000\u0000\u0000\u01be\u01c3\u0003"+
		"T*\u0000\u01bf\u01c0\u0005\u001c\u0000\u0000\u01c0\u01c2\u0003T*\u0000"+
		"\u01c1\u01bf\u0001\u0000\u0000\u0000\u01c2\u01c5\u0001\u0000\u0000\u0000"+
		"\u01c3\u01c1\u0001\u0000\u0000\u0000\u01c3\u01c4\u0001\u0000\u0000\u0000"+
		"\u01c4S\u0001\u0000\u0000\u0000\u01c5\u01c3\u0001\u0000\u0000\u0000\u01c6"+
		"\u01cb\u0003V+\u0000\u01c7\u01c8\u0005\u001b\u0000\u0000\u01c8\u01ca\u0003"+
		"V+\u0000\u01c9\u01c7\u0001\u0000\u0000\u0000\u01ca\u01cd\u0001\u0000\u0000"+
		"\u0000\u01cb\u01c9\u0001\u0000\u0000\u0000\u01cb\u01cc\u0001\u0000\u0000"+
		"\u0000\u01ccU\u0001\u0000\u0000\u0000\u01cd\u01cb\u0001\u0000\u0000\u0000"+
		"\u01ce\u01d3\u0003X,\u0000\u01cf\u01d0\u0005#\u0000\u0000\u01d0\u01d2"+
		"\u0003X,\u0000\u01d1\u01cf\u0001\u0000\u0000\u0000\u01d2\u01d5\u0001\u0000"+
		"\u0000\u0000\u01d3\u01d1\u0001\u0000\u0000\u0000\u01d3\u01d4\u0001\u0000"+
		"\u0000\u0000\u01d4W\u0001\u0000\u0000\u0000\u01d5\u01d3\u0001\u0000\u0000"+
		"\u0000\u01d6\u01db\u0003Z-\u0000\u01d7\u01d8\u0007\u0002\u0000\u0000\u01d8"+
		"\u01da\u0003Z-\u0000\u01d9\u01d7\u0001\u0000\u0000\u0000\u01da\u01dd\u0001"+
		"\u0000\u0000\u0000\u01db\u01d9\u0001\u0000\u0000\u0000\u01db\u01dc\u0001"+
		"\u0000\u0000\u0000\u01dcY\u0001\u0000\u0000\u0000\u01dd\u01db\u0001\u0000"+
		"\u0000\u0000\u01de\u01e3\u0003\\.\u0000\u01df\u01e0\u0007\u0003\u0000"+
		"\u0000\u01e0\u01e2\u0003\\.\u0000\u01e1\u01df\u0001\u0000\u0000\u0000"+
		"\u01e2\u01e5\u0001\u0000\u0000\u0000\u01e3\u01e1\u0001\u0000\u0000\u0000"+
		"\u01e3\u01e4\u0001\u0000\u0000\u0000\u01e4[\u0001\u0000\u0000\u0000\u01e5"+
		"\u01e3\u0001\u0000\u0000\u0000\u01e6\u01eb\u0003^/\u0000\u01e7\u01e8\u0007"+
		"\u0004\u0000\u0000\u01e8\u01ea\u0003^/\u0000\u01e9\u01e7\u0001\u0000\u0000"+
		"\u0000\u01ea\u01ed\u0001\u0000\u0000\u0000\u01eb\u01e9\u0001\u0000\u0000"+
		"\u0000\u01eb\u01ec\u0001\u0000\u0000\u0000\u01ec]\u0001\u0000\u0000\u0000"+
		"\u01ed\u01eb\u0001\u0000\u0000\u0000\u01ee\u01f5\u0003`0\u0000\u01ef\u01f5"+
		"\u0003b1\u0000\u01f0\u01f5\u0003d2\u0000\u01f1\u01f2\u0007\u0005\u0000"+
		"\u0000\u01f2\u01f5\u0003^/\u0000\u01f3\u01f5\u0003j5\u0000\u01f4\u01ee"+
		"\u0001\u0000\u0000\u0000\u01f4\u01ef\u0001\u0000\u0000\u0000\u01f4\u01f0"+
		"\u0001\u0000\u0000\u0000\u01f4\u01f1\u0001\u0000\u0000\u0000\u01f4\u01f3"+
		"\u0001\u0000\u0000\u0000\u01f5_\u0001\u0000\u0000\u0000\u01f6\u01f7\u0005"+
		"\u0012\u0000\u0000\u01f7\u01f9\u0003L&\u0000\u01f8\u01fa\u0003\u0086C"+
		"\u0000\u01f9\u01f8\u0001\u0000\u0000\u0000\u01f9\u01fa\u0001\u0000\u0000"+
		"\u0000\u01fa\u01fb\u0001\u0000\u0000\u0000\u01fb\u01fc\u0005\u0013\u0000"+
		"\u0000\u01fc\u01fe\u0003L&\u0000\u01fd\u01ff\u0003\u0086C\u0000\u01fe"+
		"\u01fd\u0001\u0000\u0000\u0000\u01fe\u01ff\u0001\u0000\u0000\u0000\u01ff"+
		"\u0200\u0001\u0000\u0000\u0000\u0200\u0201\u0005\u0014\u0000\u0000\u0201"+
		"\u0202\u0003L&\u0000\u0202a\u0001\u0000\u0000\u0000\u0203\u0204\u0005"+
		"\t\u0000\u0000\u0204\u0206\u0003x<\u0000\u0205\u0207\u0003J%\u0000\u0206"+
		"\u0205\u0001\u0000\u0000\u0000\u0206\u0207\u0001\u0000\u0000\u0000\u0207"+
		"\u0208\u0001\u0000\u0000\u0000\u0208\u0209\u0005<\u0000\u0000\u0209\u020b"+
		"\u0003L&\u0000\u020a\u020c\u0003\u0086C\u0000\u020b\u020a\u0001\u0000"+
		"\u0000\u0000\u020b\u020c\u0001\u0000\u0000\u0000\u020c\u020d\u0001\u0000"+
		"\u0000\u0000\u020d\u020f\u0005\u0017\u0000\u0000\u020e\u0210\u0003\u0086"+
		"C\u0000\u020f\u020e\u0001\u0000\u0000\u0000\u020f\u0210\u0001\u0000\u0000"+
		"\u0000\u0210\u0211\u0001\u0000\u0000\u0000\u0211\u0212\u0003L&\u0000\u0212"+
		"c\u0001\u0000\u0000\u0000\u0213\u0214\u0005\u0015\u0000\u0000\u0214\u0215"+
		"\u0003L&\u0000\u0215\u0216\u00054\u0000\u0000\u0216\u0217\u0003f3\u0000"+
		"\u0217e\u0001\u0000\u0000\u0000\u0218\u0219\u0005?\u0000\u0000\u0219\u021b"+
		"\u0005B\u0000\u0000\u021a\u021c\u0003\u0086C\u0000\u021b\u021a\u0001\u0000"+
		"\u0000\u0000\u021b\u021c\u0001\u0000\u0000\u0000\u021c\u021d\u0001\u0000"+
		"\u0000\u0000\u021d\u0224\u0003h4\u0000\u021e\u0220\u0003\u0086C\u0000"+
		"\u021f\u021e\u0001\u0000\u0000\u0000\u021f\u0220\u0001\u0000\u0000\u0000"+
		"\u0220\u0221\u0001\u0000\u0000\u0000\u0221\u0223\u0003h4\u0000\u0222\u021f"+
		"\u0001\u0000\u0000\u0000\u0223\u0226\u0001\u0000\u0000\u0000\u0224\u0222"+
		"\u0001\u0000\u0000\u0000\u0224\u0225\u0001\u0000\u0000\u0000\u0225\u0228"+
		"\u0001\u0000\u0000\u0000\u0226\u0224\u0001\u0000\u0000\u0000\u0227\u0229"+
		"\u0003\u0086C\u0000\u0228\u0227\u0001\u0000\u0000\u0000\u0228\u0229\u0001"+
		"\u0000\u0000\u0000\u0229\u022a\u0001\u0000\u0000\u0000\u022a\u022b\u0005"+
		"C\u0000\u0000\u022b\u023e\u0001\u0000\u0000\u0000\u022c\u022e\u0005?\u0000"+
		"\u0000\u022d\u022f\u0003\u0086C\u0000\u022e\u022d\u0001\u0000\u0000\u0000"+
		"\u022e\u022f\u0001\u0000\u0000\u0000\u022f\u0230\u0001\u0000\u0000\u0000"+
		"\u0230\u0237\u0003h4\u0000\u0231\u0233\u0003\u0086C\u0000\u0232\u0231"+
		"\u0001\u0000\u0000\u0000\u0232\u0233\u0001\u0000\u0000\u0000\u0233\u0234"+
		"\u0001\u0000\u0000\u0000\u0234\u0236\u0003h4\u0000\u0235\u0232\u0001\u0000"+
		"\u0000\u0000\u0236\u0239\u0001\u0000\u0000\u0000\u0237\u0235\u0001\u0000"+
		"\u0000\u0000\u0237\u0238\u0001\u0000\u0000\u0000\u0238\u023b\u0001\u0000"+
		"\u0000\u0000\u0239\u0237\u0001\u0000\u0000\u0000\u023a\u023c\u0003\u0086"+
		"C\u0000\u023b\u023a\u0001\u0000\u0000\u0000\u023b\u023c\u0001\u0000\u0000"+
		"\u0000\u023c\u023e\u0001\u0000\u0000\u0000\u023d\u0218\u0001\u0000\u0000"+
		"\u0000\u023d\u022c\u0001\u0000\u0000\u0000\u023eg\u0001\u0000\u0000\u0000"+
		"\u023f\u0240\u00056\u0000\u0000\u0240\u0243\u0003x<\u0000\u0241\u0242"+
		"\u0005\u0016\u0000\u0000\u0242\u0244\u0003L&\u0000\u0243\u0241\u0001\u0000"+
		"\u0000\u0000\u0243\u0244\u0001\u0000\u0000\u0000\u0244\u0245\u0001\u0000"+
		"\u0000\u0000\u0245\u0246\u00054\u0000\u0000\u0246\u0247\u0003F#\u0000"+
		"\u0247i\u0001\u0000\u0000\u0000\u0248\u024c\u0003t:\u0000\u0249\u024b"+
		"\u0003l6\u0000\u024a\u0249\u0001\u0000\u0000\u0000\u024b\u024e\u0001\u0000"+
		"\u0000\u0000\u024c\u024a\u0001\u0000\u0000\u0000\u024c\u024d\u0001\u0000"+
		"\u0000\u0000\u024dk\u0001\u0000\u0000\u0000\u024e\u024c\u0001\u0000\u0000"+
		"\u0000\u024f\u0258\u0003n7\u0000\u0250\u0252\u0005/\u0000\u0000\u0251"+
		"\u0253\u0003v;\u0000\u0252\u0251\u0001\u0000\u0000\u0000\u0252\u0253\u0001"+
		"\u0000\u0000\u0000\u0253\u0254\u0001\u0000\u0000\u0000\u0254\u0258\u0005"+
		"0\u0000\u0000\u0255\u0256\u00055\u0000\u0000\u0256\u0258\u0003\u0082A"+
		"\u0000\u0257\u024f\u0001\u0000\u0000\u0000\u0257\u0250\u0001\u0000\u0000"+
		"\u0000\u0257\u0255\u0001\u0000\u0000\u0000\u0258m\u0001\u0000\u0000\u0000"+
		"\u0259\u025b\u00051\u0000\u0000\u025a\u025c\u0003p8\u0000\u025b\u025a"+
		"\u0001\u0000\u0000\u0000\u025b\u025c\u0001\u0000\u0000\u0000\u025c\u025d"+
		"\u0001\u0000\u0000\u0000\u025d\u025e\u00052\u0000\u0000\u025eo\u0001\u0000"+
		"\u0000\u0000\u025f\u0264\u0003r9\u0000\u0260\u0261\u00053\u0000\u0000"+
		"\u0261\u0263\u0003r9\u0000\u0262\u0260\u0001\u0000\u0000\u0000\u0263\u0266"+
		"\u0001\u0000\u0000\u0000\u0264\u0262\u0001\u0000\u0000\u0000\u0264\u0265"+
		"\u0001\u0000\u0000\u0000\u0265q\u0001\u0000\u0000\u0000\u0266\u0264\u0001"+
		"\u0000\u0000\u0000\u0267\u0268\u0003\u0082A\u0000\u0268\u0269\u0005<\u0000"+
		"\u0000\u0269\u026a\u0003L&\u0000\u026as\u0001\u0000\u0000\u0000\u026b"+
		"\u0287\u0005)\u0000\u0000\u026c\u0287\u0005,\u0000\u0000\u026d\u0287\u0005"+
		"+\u0000\u0000\u026e\u0287\u0005*\u0000\u0000\u026f\u0287\u0005\u0018\u0000"+
		"\u0000\u0270\u0287\u0005\u0019\u0000\u0000\u0271\u0287\u0005\u001a\u0000"+
		"\u0000\u0272\u0287\u0003\u0080@\u0000\u0273\u0274\u0005/\u0000\u0000\u0274"+
		"\u0287\u00050\u0000\u0000\u0275\u0276\u0005/\u0000\u0000\u0276\u0277\u0003"+
		"L&\u0000\u0277\u0278\u00050\u0000\u0000\u0278\u0287\u0001\u0000\u0000"+
		"\u0000\u0279\u027a\u0005/\u0000\u0000\u027a\u027b\u0003L&\u0000\u027b"+
		"\u027c\u00053\u0000\u0000\u027c\u0281\u0003L&\u0000\u027d\u027e\u0005"+
		"3\u0000\u0000\u027e\u0280\u0003L&\u0000\u027f\u027d\u0001\u0000\u0000"+
		"\u0000\u0280\u0283\u0001\u0000\u0000\u0000\u0281\u027f\u0001\u0000\u0000"+
		"\u0000\u0281\u0282\u0001\u0000\u0000\u0000\u0282\u0284\u0001\u0000\u0000"+
		"\u0000\u0283\u0281\u0001\u0000\u0000\u0000\u0284\u0285\u00050\u0000\u0000"+
		"\u0285\u0287\u0001\u0000\u0000\u0000\u0286\u026b\u0001\u0000\u0000\u0000"+
		"\u0286\u026c\u0001\u0000\u0000\u0000\u0286\u026d\u0001\u0000\u0000\u0000"+
		"\u0286\u026e\u0001\u0000\u0000\u0000\u0286\u026f\u0001\u0000\u0000\u0000"+
		"\u0286\u0270\u0001\u0000\u0000\u0000\u0286\u0271\u0001\u0000\u0000\u0000"+
		"\u0286\u0272\u0001\u0000\u0000\u0000\u0286\u0273\u0001\u0000\u0000\u0000"+
		"\u0286\u0275\u0001\u0000\u0000\u0000\u0286\u0279\u0001\u0000\u0000\u0000"+
		"\u0287u\u0001\u0000\u0000\u0000\u0288\u028d\u0003L&\u0000\u0289\u028a"+
		"\u00053\u0000\u0000\u028a\u028c\u0003L&\u0000\u028b\u0289\u0001\u0000"+
		"\u0000\u0000\u028c\u028f\u0001\u0000\u0000\u0000\u028d\u028b\u0001\u0000"+
		"\u0000\u0000\u028d\u028e\u0001\u0000\u0000\u0000\u028ew\u0001\u0000\u0000"+
		"\u0000\u028f\u028d\u0001\u0000\u0000\u0000\u0290\u02b6\u0005-\u0000\u0000"+
		"\u0291\u02b6\u0005)\u0000\u0000\u0292\u02b6\u0005,\u0000\u0000\u0293\u02b6"+
		"\u0005+\u0000\u0000\u0294\u02b6\u0005*\u0000\u0000\u0295\u0296\u0005/"+
		"\u0000\u0000\u0296\u02b6\u00050\u0000\u0000\u0297\u0298\u0005/\u0000\u0000"+
		"\u0298\u0299\u0003x<\u0000\u0299\u029a\u00050\u0000\u0000\u029a\u02b6"+
		"\u0001\u0000\u0000\u0000\u029b\u029c\u0005/\u0000\u0000\u029c\u029d\u0003"+
		"x<\u0000\u029d\u029e\u00053\u0000\u0000\u029e\u02a3\u0003x<\u0000\u029f"+
		"\u02a0\u00053\u0000\u0000\u02a0\u02a2\u0003x<\u0000\u02a1\u029f\u0001"+
		"\u0000\u0000\u0000\u02a2\u02a5\u0001\u0000\u0000\u0000\u02a3\u02a1\u0001"+
		"\u0000\u0000\u0000\u02a3\u02a4\u0001\u0000\u0000\u0000\u02a4\u02a6\u0001"+
		"\u0000\u0000\u0000\u02a5\u02a3\u0001\u0000\u0000\u0000\u02a6\u02a7\u0005"+
		"0\u0000\u0000\u02a7\u02b6\u0001\u0000\u0000\u0000\u02a8\u02aa\u00051\u0000"+
		"\u0000\u02a9\u02ab\u0003|>\u0000\u02aa\u02a9\u0001\u0000\u0000\u0000\u02aa"+
		"\u02ab\u0001\u0000\u0000\u0000\u02ab\u02ac\u0001\u0000\u0000\u0000\u02ac"+
		"\u02b6\u00052\u0000\u0000\u02ad\u02b3\u0003\u0080@\u0000\u02ae\u02b0\u0005"+
		"/\u0000\u0000\u02af\u02b1\u0003z=\u0000\u02b0\u02af\u0001\u0000\u0000"+
		"\u0000\u02b0\u02b1\u0001\u0000\u0000\u0000\u02b1\u02b2\u0001\u0000\u0000"+
		"\u0000\u02b2\u02b4\u00050\u0000\u0000\u02b3\u02ae\u0001\u0000\u0000\u0000"+
		"\u02b3\u02b4\u0001\u0000\u0000\u0000\u02b4\u02b6\u0001\u0000\u0000\u0000"+
		"\u02b5\u0290\u0001\u0000\u0000\u0000\u02b5\u0291\u0001\u0000\u0000\u0000"+
		"\u02b5\u0292\u0001\u0000\u0000\u0000\u02b5\u0293\u0001\u0000\u0000\u0000"+
		"\u02b5\u0294\u0001\u0000\u0000\u0000\u02b5\u0295\u0001\u0000\u0000\u0000"+
		"\u02b5\u0297\u0001\u0000\u0000\u0000\u02b5\u029b\u0001\u0000\u0000\u0000"+
		"\u02b5\u02a8\u0001\u0000\u0000\u0000\u02b5\u02ad\u0001\u0000\u0000\u0000"+
		"\u02b6y\u0001\u0000\u0000\u0000\u02b7\u02bc\u0003x<\u0000\u02b8\u02b9"+
		"\u00053\u0000\u0000\u02b9\u02bb\u0003x<\u0000\u02ba\u02b8\u0001\u0000"+
		"\u0000\u0000\u02bb\u02be\u0001\u0000\u0000\u0000\u02bc\u02ba\u0001\u0000"+
		"\u0000\u0000\u02bc\u02bd\u0001\u0000\u0000\u0000\u02bd{\u0001\u0000\u0000"+
		"\u0000\u02be\u02bc\u0001\u0000\u0000\u0000\u02bf\u02c4\u0003~?\u0000\u02c0"+
		"\u02c1\u00053\u0000\u0000\u02c1\u02c3\u0003~?\u0000\u02c2\u02c0\u0001"+
		"\u0000\u0000\u0000\u02c3\u02c6\u0001\u0000\u0000\u0000\u02c4\u02c2\u0001"+
		"\u0000\u0000\u0000\u02c4\u02c5\u0001\u0000\u0000\u0000\u02c5}\u0001\u0000"+
		"\u0000\u0000\u02c6\u02c4\u0001\u0000\u0000\u0000\u02c7\u02ca\u0003\u0082"+
		"A\u0000\u02c8\u02c9\u0005<\u0000\u0000\u02c9\u02cb\u0003x<\u0000\u02ca"+
		"\u02c8\u0001\u0000\u0000\u0000\u02ca\u02cb\u0001\u0000\u0000\u0000\u02cb"+
		"\u007f\u0001\u0000\u0000\u0000\u02cc\u02d1\u0003\u0082A\u0000\u02cd\u02ce"+
		"\u00055\u0000\u0000\u02ce\u02d0\u0003\u0082A\u0000\u02cf\u02cd\u0001\u0000"+
		"\u0000\u0000\u02d0\u02d3\u0001\u0000\u0000\u0000\u02d1\u02cf\u0001\u0000"+
		"\u0000\u0000\u02d1\u02d2\u0001\u0000\u0000\u0000\u02d2\u0081\u0001\u0000"+
		"\u0000\u0000\u02d3\u02d1\u0001\u0000\u0000\u0000\u02d4\u02d5\u0007\u0006"+
		"\u0000\u0000\u02d5\u0083\u0001\u0000\u0000\u0000\u02d6\u02d7\u0007\u0007"+
		"\u0000\u0000\u02d7\u0085\u0001\u0000\u0000\u0000\u02d8\u02da\u0005?\u0000"+
		"\u0000\u02d9\u02d8\u0001\u0000\u0000\u0000\u02da\u02db\u0001\u0000\u0000"+
		"\u0000\u02db\u02d9\u0001\u0000\u0000\u0000\u02db\u02dc\u0001\u0000\u0000"+
		"\u0000\u02dc\u0087\u0001\u0000\u0000\u0000S\u0089\u008e\u0093\u0099\u009d"+
		"\u00a2\u00a6\u00ae\u00b1\u00b5\u00b9\u00cc\u00d1\u00dc\u00df\u00ea\u00f7"+
		"\u00ff\u0104\u0108\u0111\u0124\u0129\u012d\u0138\u013e\u0146\u014a\u0157"+
		"\u0164\u016a\u0173\u0178\u017d\u0187\u018e\u0197\u019b\u019e\u01a2\u01a7"+
		"\u01b6\u01bc\u01c3\u01cb\u01d3\u01db\u01e3\u01eb\u01f4\u01f9\u01fe\u0206"+
		"\u020b\u020f\u021b\u021f\u0224\u0228\u022e\u0232\u0237\u023b\u023d\u0243"+
		"\u024c\u0252\u0257\u025b\u0264\u0281\u0286\u028d\u02a3\u02aa\u02b0\u02b3"+
		"\u02b5\u02bc\u02c4\u02ca\u02d1\u02db";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}