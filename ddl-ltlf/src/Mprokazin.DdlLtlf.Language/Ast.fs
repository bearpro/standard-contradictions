namespace Mprokazin.DdlLtlf.Language.Ast

type SourceRange = { StartLine: int; StartChar: int; EndLine: int; EndChar: int }

type PrimitiveType = Int | Rational | Bool

type TypeReference =
    | Primitive of PrimitiveType
    | Named of string

type TypeDescription =
    | Reference of TypeReference
    | Product of ProductTypeDescription
    | Sum of SumTypeDescription
    | Function of FuncTypeDescription
and ProductTypeField = { Name: string; Type: TypeDescription option; Range: SourceRange }
and ProductTypeDescription = { Fields: ProductTypeField list; Range: SourceRange }
and SumTypeVariant = { Constructor: string; Payload: TypeDescription option; Range: SourceRange }
and SumTypeDescription = { Variants: SumTypeVariant list; Range: SourceRange }
and FuncTypeDescription = { Parameters: TypeDescription list; ReturnType: TypeDescription; Range: SourceRange }

type TypeDefinition = { Name: string; Body: TypeDescription; Range: SourceRange }

type ConstantValue =
    | IntConstant of int
    | RationalConstant of string
    | BooleanConstant of bool

type Expression =
    { Kind: ExpressionKind
      Annotation: TypeDescription option
      Range: SourceRange }
and ExpressionKind =
    | Constant of ConstantValue
    | Name of string list
    | Tuple of Expression list
    | Constructor of ConstructorCall
and ConstructorCall =
    { Constructor: string
      Arguments: Expression list
      Range: SourceRange }

type Parameter = { Name: string; Type: TypeDescription option; Range: SourceRange }

type Pattern =
    | Constant of ConstantValue * SourceRange
    | Tuple of Pattern list * SourceRange
    | Constructor of ConstructorPattern
    | Wildcard of SourceRange
and ConstructorPattern =
    { Constructor: string
      Arguments: Pattern list
      Range: SourceRange }

type PredicateCall =
    { PredicateName: string
      Arguments: Expression list
      Range: SourceRange }

type AlgebraicOperation = Mul | Div | Mod | Sum | Sub

type AlgebraicExpression =
    | Expression of Expression
    | Operation of AlgebraicExpression * AlgebraicOperation * AlgebraicExpression * SourceRange

type AlgebraicEqualityCondition = Lt | Le | Eq | Ge | Gt | Ne

type AlgebraicCondition =
    { Condition: AlgebraicEqualityCondition
      LeftExpression: AlgebraicExpression
      RightExpression: AlgebraicExpression
      Range: SourceRange }

type PredicateBodyItem =
    | Call of PredicateCall
    | Expression of Expression
    | PatternMatch of Expression * Pattern
    | AlgebraicCondition of AlgebraicCondition
and PredicateBody =
    | Item of PredicateBodyItem * SourceRange
    | Not of PredicateBody * SourceRange
    | And of PredicateBody * PredicateBody * SourceRange
    | Or of PredicateBody * PredicateBody * SourceRange
    | Definition of PredicateDefinition * PredicateBody * SourceRange
and PredicateDefinition =
    { Name: string
      Parameters: Parameter list
      Body: PredicateBody
      Range: SourceRange
      PredicateType: FuncTypeDescription option }

type DeonticModality = Obligated | Forbidden | Permitted | Suggeseted

type DeonticStatement =
    { Modality: DeonticModality
      Name: string option
      Body: PredicateBody
      Condition: PredicateBody option
      Range: SourceRange
      Parameters: Parameter list
      InferredType: FuncTypeDescription option }

type Definition =
    | Type of TypeDefinition
    | Fact of unit
    | Predicate of PredicateDefinition
    | DeonticStatement of DeonticStatement

type Program = Definition list

type Model = Program
