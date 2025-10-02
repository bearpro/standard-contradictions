grammar DdlLtlf;

// ---- parser rules (lowercase by convention)
root  : (namedPredicateDefinition | deonticStatement)+ EOF ;

algebraicConstant       : INT ;

parameterReference      : NAME ;

algebraicExpression     : algebraicConstant                             # Const
                        | parameterReference                            # Var
                        | algebraicExpression '+' algebraicExpression   # Sum
                        | algebraicExpression '%' algebraicExpression   # Mod
                        ;

algebraicPredicate      : algebraicExpression ('<' | '>' | '=') algebraicExpression 
                        ;

parameters : '(' NAME? (',' NAME)* ')' ;

namedPredicateCall : NAME parameters ;

predicate   : predicate 'or' predicate    # Or
            | predicate 'and' predicate   # And
            | 'not' predicate             # Not
            | algebraicPredicate          # Algebraic
            | namedPredicateCall          # NamedPredicate
            | namedPredicateDefinition 'in' predicate # NestedPredicate
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
NAME        : [a-zA-Z_][a-zA-Z_0-9]* ;
INT         : [0-9]+ ;
WS          : [ \t\r\n]+ -> skip ;

LINE_COMMENT : '#' ~[\r\n]* -> skip ;
