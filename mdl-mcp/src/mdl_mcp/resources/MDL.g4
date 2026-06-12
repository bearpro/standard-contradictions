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
    | ruleDecl
    | priorityDecl
    | factDecl
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
    : nameToken (COMMA nameToken)*
    ;

variant
    : nameToken LPAREN variantFieldList RPAREN
    ;

variantFieldList
    : variantField (COMMA variantField)*
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
    : typeField (COMMA typeField)*
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
    : LET nameToken typeAnnotation? EQ expr
    ;

funcDecl
    : FUNC nameToken typeParams? LPAREN paramList? RPAREN ARROW typeExpr COLON block
    ;

paramList
    : param (COMMA param)*
    ;

param
    : pattern COLON typeExpr
    ;

entityDecl
    : ENTITY nameToken COLON typeExpr
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
    : deonticMod COLON block
    | deonticMod? qualifiedName (WHEN expr)? COLON block
    ;

deonticMod
    : O
    | P
    | F
    ;

priorityDecl
    : OVERRIDE qualifiedName (GT qualifiedName)*
    ;

factDecl
    : FACT (nameToken EQ)? expr
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
    : orExpr (IMPLIES implication)?
    ;

orExpr
    : andExpr (OR andExpr)*
    ;

andExpr
    : temporalBinary (AND temporalBinary)*
    ;

temporalBinary
    : comparison (UNTIL comparison)*
    ;

comparison
    : additive ((EQ | NE | LT | LE | GT | GE) additive)*
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
    | (NOT | MINUS) unary
    | postfix
    ;

ifExpr
    : IF expr newlines? THEN expr newlines? ELSE expr
    ;

letExpr
    : LET pattern typeAnnotation? EQ expr newlines? IN newlines? expr
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
    : recordConstructorField (COMMA recordConstructorField)*
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
    | qualifiedName
    | LPAREN RPAREN
    | LPAREN expr RPAREN
    | LPAREN expr COMMA expr (COMMA expr)* RPAREN
    ;

exprList
    : expr (COMMA expr)*
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
    : pattern (COMMA pattern)*
    ;

recordPatternFieldList
    : recordPatternField (COMMA recordPatternField)*
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
    | O
    | P
    | F
    ;

temporalUnaryOp
    : ALWAYS
    | EVENTUALLY
    | NEXT
    | NOW
    ;

newlines
    : NEWLINE+
    ;

ARROW: '->';
LE: '<=';
GE: '>=';
NE: '!=';

MODULE: 'module';
IMPORT: 'import';
OPEN: 'open';
TYPE: 'type';
LET: 'let';
FUNC: 'func';
ENTITY: 'entity';
RULE: 'rule';
STRICT: 'strict';
DEFEASIBLE: 'defeasible';
DEFEATER: 'defeater';
OVERRIDE: 'override';
FACT: 'fact';
IF: 'if';
THEN: 'then';
ELSE: 'else';
CASE: 'case';
WHEN: 'when';
IN: 'in';
TRUE: 'true';
FALSE: 'false';
AND: 'and';
OR: 'or';
IMPLIES: 'implies';
NOT: 'not';
ALWAYS: 'always';
EVENTUALLY: 'eventually';
NEXT: 'next';
NOW: 'now';
UNTIL: 'until';
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
