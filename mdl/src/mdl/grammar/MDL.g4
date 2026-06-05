grammar MDL;

tokens {
    INDENT,
    DEDENT
}

program
    : newlines? annotations moduleDecl newlines? topItem* EOF
    ;

exprOnly
    : newlines? expr newlines? EOF
    ;

typeExprOnly
    : newlines? typeExpr newlines? EOF
    ;

topItem
    : annotations (importDecl | openDecl | declaration) newlines?
    ;

annotations
    : (ANNOT newlines?)*
    ;

moduleDecl
    : MODULE qualifiedName
    ;

importDecl
    : IMPORT STRING
    ;

openDecl
    : OPEN qualifiedName
    ;

declaration
    : typeDecl
    | valueDecl
    | funcDecl
    | entityDecl
    | eventDecl
    | ruleDecl
    | priorityDecl
    | factDecl
    | assertDecl
    | alignDecl
    ;

typeDecl
    : TYPE nameToken typeParams? EQ typeDefinition
    ;

typeDefinition
    : recordType
    | variant (BAR variant)*
    ;

typeParams
    : LT nameList GT
    ;

nameList
    : nameToken (COMMA nameToken)* COMMA?
    ;

variant
    : nameToken LPAREN variantFieldList RPAREN
    ;

variantFieldList
    : variantField (COMMA variantField)* COMMA?
    ;

variantField
    : nameToken COLON typeExpr
    | typeExpr
    ;

typeExpr
    : recordType
    | tupleOrParenType
    | typeRef
    ;

recordType
    : LBRACE typeFieldList? RBRACE
    ;

typeFieldList
    : typeField (COMMA typeField)* COMMA?
    ;

typeField
    : nameToken COLON typeExpr
    ;

tupleOrParenType
    : LPAREN typeExpr RPAREN
    | LPAREN typeExpr COMMA typeExpr (COMMA typeExpr)* RPAREN
    ;

typeRef
    : qualifiedName typeArgs?
    ;

typeArgs
    : LT typeExprList GT
    ;

typeExprList
    : typeExpr (COMMA typeExpr)*
    ;

valueDecl
    : (VAL | LET) nameToken typeAnnotation? EQ expr
    ;

funcDecl
    : FUNC nameToken typeParams? LPAREN paramList? RPAREN ARROW typeExpr COLON block
    ;

paramList
    : param (COMMA param)* COMMA?
    ;

param
    : pattern COLON typeExpr
    ;

entityDecl
    : ENTITY nameToken COLON typeExpr entityClause*
    ;

entityClause
    : WHERE expr
    ;

eventDecl
    : EVENT nameToken (LPAREN typeFieldList? RPAREN)?
    ;

ruleDecl
    : ruleStrength? RULE ruleBody (OTHERWISE expr)?
    ;

ruleStrength
    : STRICT
    | DEFEASIBLE
    | DEFEATER
    ;

ruleBody
    : deonticMod COLON expr
    | deonticMod? qualifiedName (WHEN expr)? COLON expr
    ;

deonticMod
    : O
    | P
    | F
    ;

priorityDecl
    : (PRIORITY | OVERRIDE) qualifiedName (GT qualifiedName)*
    ;

factDecl
    : FACT (nameToken EQ)? expr
    ;

assertDecl
    : ASSERT expr
    ;

alignDecl
    : ALIGN qualifiedName TO alignTarget alignKind?
    ;

alignTarget
    : qualifiedName
    | STRING
    | iriLiteral
    ;

iriLiteral
    : LT (~GT)* GT
    ;

alignKind
    : EQUIVALENT
    | BROADER
    | NARROWER
    | RELATED
    ;

block
    : NEWLINE INDENT blockLetStmt* expr? newlines? DEDENT
    | expr
    ;

blockLetStmt
    : LET pattern typeAnnotation? EQ expr newlines
    ;

typeAnnotation
    : COLON typeExpr
    ;

expr
    : temporalPostfix
    ;

temporalPostfix
    : implication temporalUnaryOp*
    ;

implication
    : orExpr
    ;

orExpr
    : andExpr (OR andExpr)*
    ;

andExpr
    : temporalBinary (AND temporalBinary)*
    ;

temporalBinary
    : comparison ((UNTIL | RELEASE | WEAK_UNTIL) comparison)*
    ;

comparison
    : additive ((EQ | EQEQ | NE | LT | LE | GT | GE) additive)*
    ;

additive
    : multiplicative ((PLUS | MINUS) multiplicative)*
    ;

multiplicative
    : unary ((STAR | SLASH | PERCENT) unary)*
    ;

unary
    : ifExpr
    | letExpr
    | matchExpr
    | (NOT | MINUS | temporalUnaryOp) unary
    | postfix
    ;

ifExpr
    : IF expr newlines? THEN expr newlines? ELSE expr
    ;

letExpr
    : LET pattern typeAnnotation? EQ expr newlines? IN expr
    ;

matchExpr
    : CASE expr COLON caseBody
    ;

caseBody
    : NEWLINE INDENT newlines? caseArm (newlines? caseArm)* newlines? DEDENT
    | NEWLINE newlines? caseArm (newlines? caseArm)* newlines?
    ;

caseArm
    : BAR pattern (WHEN expr)? COLON block
    ;

postfix
    : primary postfixSuffix*
    ;

postfixSuffix
    : recordConstructorFields
    | LPAREN exprList? RPAREN
    | DOT nameToken
    ;

recordConstructorFields
    : LBRACE recordConstructorFieldList? RBRACE
    ;

recordConstructorFieldList
    : recordConstructorField (COMMA recordConstructorField)* COMMA?
    ;

recordConstructorField
    : nameToken EQ expr
    ;

primary
    : STRING
    | INT
    | DECIMAL
    | RAT
    | TRUE
    | FALSE
    | LAST
    | qualifiedName
    | LPAREN RPAREN
    | LPAREN expr RPAREN
    | LPAREN expr COMMA expr (COMMA expr)* RPAREN
    ;

exprList
    : expr (COMMA expr)* COMMA?
    ;

pattern
    : UNDERSCORE
    | STRING
    | INT
    | DECIMAL
    | RAT
    | LPAREN RPAREN
    | LPAREN pattern RPAREN
    | LPAREN pattern COMMA pattern (COMMA pattern)* RPAREN
    | LBRACE recordPatternFieldList? RBRACE
    | qualifiedName (LPAREN patternList? RPAREN)?
    ;

patternList
    : pattern (COMMA pattern)* COMMA?
    ;

recordPatternFieldList
    : recordPatternField (COMMA recordPatternField)* COMMA?
    ;

recordPatternField
    : nameToken (EQ pattern)?
    ;

qualifiedName
    : nameToken (DOT nameToken)*
    ;

nameToken
    : IDENT
    | TRUE
    | FALSE
    | LAST
    | O
    | P
    | F
    ;

temporalUnaryOp
    : ALWAYS
    | EVENTUALLY
    | NEXT
    | WEAK_NEXT
    | NEVER
    ;

newlines
    : NEWLINE+
    ;

ARROW: '->';
LE: '<=';
GE: '>=';
NE: '!=';
EQEQ: '==';

MODULE: 'module';
IMPORT: 'import';
OPEN: 'open';
TYPE: 'type';
VAL: 'val';
LET: 'let';
FUNC: 'func';
ENTITY: 'entity';
EVENT: 'event';
RULE: 'rule';
STRICT: 'strict';
DEFEASIBLE: 'defeasible';
DEFEATER: 'defeater';
PRIORITY: 'priority';
OVERRIDE: 'override';
FACT: 'fact';
ASSERT: 'assert';
ALIGN: 'align';
TO: 'to';
EQUIVALENT: 'equivalent';
BROADER: 'broader';
NARROWER: 'narrower';
RELATED: 'related';
WHERE: 'where';
IF: 'if';
THEN: 'then';
ELSE: 'else';
CASE: 'case';
WHEN: 'when';
IN: 'in';
TRUE: 'true';
FALSE: 'false';
LAST: 'last';
AND: 'and';
OR: 'or';
NOT: 'not';
ALWAYS: 'always';
EVENTUALLY: 'eventually';
NEXT: 'next';
WEAK_NEXT: 'weak_next';
NEVER: 'never';
UNTIL: 'until';
RELEASE: 'release';
WEAK_UNTIL: 'weak_until';
OTHERWISE: 'otherwise';
O: 'O';
P: 'P';
F: 'F';

ANNOT: '@' ~[\r\n]*;
STRING: '"' ( '\\' . | ~["\\\r\n] )* '"';
RAT: [0-9]+ '/' [0-9]+;
DECIMAL: [0-9]+ '.' [0-9]+;
INT: [0-9]+;
UNDERSCORE: '_';
IDENT: [A-Za-z_] [A-Za-z0-9_']*;

LPAREN: '(';
RPAREN: ')';
LBRACE: '{';
RBRACE: '}';
COMMA: ',';
COLON: ':';
DOT: '.';
BAR: '|';
PLUS: '+';
MINUS: '-';
STAR: '*';
SLASH: '/';
PERCENT: '%';
EQ: '=';
LT: '<';
GT: '>';

NEWLINE: '\r'? '\n' | '\r';
COMMENT: '#' ~[\r\n]* -> channel(HIDDEN);
WS: [ \t]+ -> channel(HIDDEN);
