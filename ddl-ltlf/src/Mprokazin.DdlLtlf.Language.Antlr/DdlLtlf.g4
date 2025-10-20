grammar DdlLtlf;

// ---- parser rules (lowercase by convention)
root  : (namedPredicateDefinition | deonticStatement)+ EOF ;

algebraicConstant       : (INT | RATIONAL);

parameterReference      : NAME ;

binaryAlgegraicOperation : ('+' | '-' | '%' | '/' | '*' ) ;

algebraicExpression     : algebraicConstant                             # Const
                        | parameterReference                            # Var
                        | algebraicExpression binaryAlgegraicOperation algebraicExpression # Op
                        | '(' algebraicExpression ')'                   # Parens
                        ;

algebraicPredicate      : algebraicExpression ('<' | '<=' | '=' | '>=' | '>') algebraicExpression 
                        ;

parameters : '(' NAME? (',' NAME)* ')' ;

namedPredicateCall : NAME parameters ;

predicate   : predicate 'or' predicate    # Or
            | predicate 'and' predicate   # And
            | 'not' predicate             # Not
            | BOOL                        # Bool
            | algebraicPredicate          # Algebraic
            | namedPredicateCall          # NamedPredicate
            | namedPredicateDefinition 'in' predicate # NestedPredicate
            | '(' predicate ')'           # PredicateParens
            ;

namedPredicateDefinition    : 'predicate' NAME parameters '=' predicate;

deonticModality   : 'obligated'
                  | 'permitted'
                  | 'forbidden' 
                  | 'suggested' 
                  ;

deonticStatement  : deonticModality (NAME '=')?? predicate ('when' predicate)?
                  ;

// ---- lexer rules (UPPERCASE by convention)

// Numbers
fragment DIGITS   : [0-9]+ ;
fragment EXPONENT : [eE] [+-]? DIGITS ;

RATIONAL
  : DIGITS '.' DIGITS (EXPONENT)?      // 12.34, 12.34e-2
  | '.' DIGITS (EXPONENT)?             // .5, .5e+1
  | DIGITS EXPONENT                    // 12e10
  ;

INT         : DIGITS ;

// Booleans

fragment TRUE : 'true' ;
fragment FALSE: 'false' ;

BOOL : TRUE | FALSE ;

// Other lexer tokens

NAME        : [a-zA-Z_][a-zA-Z_0-9]* ;

WS          : [ \t\r\n]+ -> skip ;

LINE_COMMENT : '#' ~[\r\n]* -> skip ;
