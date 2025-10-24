grammar DdlLtlf;

/* Lexer Tokens */ 

    fragment DIGITS   : [0-9]+ ;
    fragment EXPONENT : [eE] [+-]? DIGITS ;

    // Rational constant
    
    RATIONAL    : DIGITS '.' DIGITS (EXPONENT)?      // 12.34, 12.34e-2
                | '.' DIGITS (EXPONENT)?             // .5, .5e+1
                | DIGITS EXPONENT                    // 12e10
                ;


    // Integer constant
    INT         : DIGITS ;

    // Booleans
    BOOL : 'true' | 'false' ;


    // Operations
    MUL  : '*';
    DIV  : '/';
    MOD  : '%';
    ADD : '+';
    SUB: '-';

    // Logical conditions
    NOT : 'not' ;
    AND : 'and' ;
    OR  : 'or'  ;

    // Algebraic conditions
    LT   : '<'  ;
    LE   : '<=' ;
    EQ   : '='  ;
    GE   : '>=' ;
    GT   : '>'  ;
    NE   : '<>' ;

    // Keywords:
    PREDICATE   : 'predicate' ;
    TYPE        : 'type'      ;
    WHEN        : 'when'      ;
    
    INT_TN      : 'int'       ;
    RATIONAL_TN : 'rational'  ;
    BOOL_TN     : 'bool'      ;

    // Deontic modlities

    OBLIGATED   : 'obligated' ;
    PERMITTED   : 'permitted' ;
    FORBIDDEN   : 'forbidden' ;
    SUGGESTED   : 'suggested' ;


    // Other
    OF                  : 'of' ;
    IS                  : 'is' ;
    ARROW               : '->' ;
    DOT                 : '.' ;
    UNDERSCORE          : '_' ;
    NAME                : [a-zA-Z_][a-zA-Z_0-9]*;
    // NAME_REFERENCE      : NAME ('.' NAME)+ ;
    WS                  : [ \t\r\n]+ -> skip;

/* Primitives and basics */

    constant : (RATIONAL | INT | BOOL) ;
    nameReference: NAME (DOT NAME)* ;


/* Types */ 

    primitiveTypeName   : INT_TN      # Int
                        | RATIONAL_TN # Rational
                        | BOOL_TN     # Bool
                        ;

    typeReference       : primitiveTypeName | NAME ;

    sumVariant              : NAME (OF typeDescription)? ;
    sumTypeDescription      : '(' ('|')? sumVariant ('|' sumVariant)* ')' ;

    productTypeItem         : NAME (':' typeDescription)? ;
    productTypeDescription  : '(' (productTypeItem (',' productTypeItem)*)? ')' ;

    functionTypeDescription : '(' functionParameterTypes ')' ARROW typeDescription ;
    functionParameterTypes  : (typeDescription (',' typeDescription)*)? ;

    // Final type description

    typeDescription : functionTypeDescription
                    | sumTypeDescription 
                    | productTypeDescription 
                    | typeReference 
                    ;

    typeDefinition  : TYPE NAME '=' typeDescription ;


/* Predicates */ 


    // Predicate signature

    predicateParameter : NAME (':' typeDescription)? ;
    predicateParameters : '(' (predicateParameter (',' predicateParameter)*)? ')' ;

    predicateDefinition : PREDICATE NAME predicateParameters '=' predicateBody ;

    constructorInvocation : NAME '(' (expression (',' expression)*)? ')' ;
    tupleLiteral : '(' expression ',' expression (',' expression)* ')' ;

    expressionPrimary 
                    : constructorInvocation   # ConstructorExpression
                    | tupleLiteral            # TupleExpression
                    | constant                # ConstantExpression
                    | nameReference           # NameExpression
                    | '(' expression ')'      # ParenthesizedExpression
                    ;

    expression      : expressionPrimary ':' typeDescription   # AnnotatedExpression
                    | expressionPrimary                       # PlainExpression
                    ;

    predicateReferenceArguments: '(' (expression (',' expression)*)?')' ;
    predicateReference : NAME predicateReferenceArguments ;

    
    algebraicOperation      : MUL       # Mul
                            | DIV       # Div
                            | MOD       # Mod
                            | ADD       # Add
                            | SUB       # Sub
                            ;

    algebraicExpression     : expression                    # AlgebraicValue
                            | '(' algebraicExpression ')'   # AlgebraicParens
                            | algebraicExpression algebraicOperation algebraicExpression # Operation
                            ; 

    algebraicComparation    : LT  # Lt
                            | LE  # Le
                            | EQ  # Eq
                            | GE  # Ge
                            | GT  # Gt
                            | NE  # Ne
                            ;

    algebraicCondition : algebraicExpression algebraicComparation algebraicExpression ;

    pattern             : constructorPattern   # ConstructorPatternValue
                        | tuplePattern         # TuplePatternValue
                        | constant             # ConstantPatternValue
                        | UNDERSCORE           # WildcardPatternValue
                        ;

    constructorPattern  : NAME '(' (pattern (',' pattern)*)? ')' ;
    tuplePattern        : '(' pattern ',' pattern (',' pattern)* ')' ;

    // Predicate body
    predicateBody : predicateDefinition predicateBody   # WithNested
                | '(' predicateBody ')'                 # PredicateParens
                | NOT predicateBody                     # Not
                | predicateBody OR predicateBody        # Or
                | predicateBody AND predicateBody       # And
                | predicateReference                    # Reference
                | expression IS pattern                 # IsPattern
                | expression                            # Value
                | algebraicCondition                    # Algebraic
                ;

/* Deontic statements */ 

    deonticModality : OBLIGATED     # Obligated
                    | PERMITTED     # Permitted
                    | FORBIDDEN     # Forbidden
                    | SUGGESTED     # Suggested
                    ;

    deonticStatement : deonticModality (NAME '=')?? predicateBody (WHEN predicateBody)? ;

/* Program */

topLevelDefinition  : typeDefinition 
                    | predicateDefinition 
                    | deonticStatement 
                    ;


root  : (topLevelDefinition)+ EOF ;
