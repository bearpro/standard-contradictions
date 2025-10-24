namespace Mprokazin.DdlLtlf.Language.Ast

type SourceRange = { StartLine: int; StartChar: int; EndLine: int; EndChar: int }

type PrimitiveType = Int | Rational | Bool
type TypeReference = 
    | Primitive of PrimitiveType 
    | Named of string

type ProductTypeField = { Name: string; Type: TypeDescription option; Range: SourceRange }
and TypeDescription = 
    | Reference of TypeReference
    | Product of ProductTypeField list * SourceRange
    | Sum of  TypeDescription list * SourceRange

type TypeDefinition = { Name: string; Body: TypeDescription; Range: SourceRange }

type ValueReferenceValue =
    | IntConstant of int
    | RationalConstant of string
    | BooleanConstant of bool
    | Name of string list

type ValueReference = { Value: ValueReferenceValue; Type: TypeDescription option; Range: SourceRange }

type PredicateCall = {
    PredicateName: string
    Arguments: ValueReference list
    Range: SourceRange
}

type AlgebraicOperation = Mul | Div | Mod | Sum | Sub

type AlgebraicExpression =
    | Value of ValueReference
    | Operation of AlgebraicExpression * AlgebraicOperation * AlgebraicExpression * SourceRange

type AlgebraicEqualityCondition = Lt | Le | Eq | Ge | Gt | Ne

type Parameter = { Name: string; Type: TypeDescription option; Range: SourceRange }

type AlgebraicCondition = { 
    Condition: AlgebraicEqualityCondition
    LeftExpression: AlgebraicExpression
    RightExpression: AlgebraicExpression
    Range: SourceRange
}

type PredicateBodyItem = 
    | Call      of PredicateCall
    | Value     of ValueReference
    | AlgebraicCondition of AlgebraicCondition
and PredicateBody = 
    | Item  of PredicateBodyItem                    * SourceRange
    | Not   of PredicateBody                        * SourceRange
    | And   of PredicateBody * PredicateBody        * SourceRange
    | Or    of PredicateBody * PredicateBody        * SourceRange
    | Definition of PredicateDefinition * PredicateBody * SourceRange
and PredicateDefinition = { 
    Name: string 
    Parameters: Parameter list
    Body: PredicateBody
    Range: SourceRange
}

type DeonticModality = Obligated | Forbidden | Permitted | Suggeseted

type DeonticStatement = {
    Modality: DeonticModality
    Name: string option
    Body: PredicateBody
    Condition: PredicateBody option
    Range: SourceRange
}

type Definition = 
    | Type of TypeDefinition
    | Fact of unit
    | Predicate of PredicateDefinition
    | DeonticStatement of DeonticStatement

type Program = Definition list
