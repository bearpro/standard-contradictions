namespace Mprokazin.DdlLtlf.Language.Ast

module Unparser =

    open System

    let joinWith (sep: string) (parts: string list) =
        parts |> List.filter (fun s -> not (String.IsNullOrWhiteSpace s)) |> String.concat sep

    // -- Type pretty-print ----------------------------------------------------

    let printPrimitiveType = function
        | PrimitiveType.Int -> "int"
        | PrimitiveType.Rational -> "rational"
        | PrimitiveType.Bool -> "bool"

    let printTypeReference = function
        | TypeReference.Primitive p -> printPrimitiveType p
        | TypeReference.Named name -> name

    let rec printTypeDescription (t: TypeDescription) : string =
        match t with
        | TypeDescription.Reference reference -> printTypeReference reference
        | TypeDescription.Product product -> printProductTypeDescription product
        | TypeDescription.Sum sumType -> printSumTypeDescription sumType
        | TypeDescription.Function func -> printFunctionTypeDescription func

    and printProductField (field: ProductTypeField) : string =
        match field.Type with
        | None -> field.Name
        | Some tp -> $"{field.Name}: {printTypeDescription tp}"

    and printProductTypeDescription (product: ProductTypeDescription) : string =
        product.Fields
        |> List.map printProductField
        |> String.concat ", "
        |> fun inner -> $"({inner})"

    and printSumVariant (variant: SumTypeVariant) : string =
        match variant.Payload with
        | None -> variant.Constructor
        | Some payload -> $"{variant.Constructor} of {printTypeDescription payload}"

    and printSumTypeDescription (sumType: SumTypeDescription) : string =
        sumType.Variants
        |> List.map printSumVariant
        |> String.concat " | "
        |> fun inner -> $"({inner})"

    and printFunctionTypeDescription (func: FuncTypeDescription) : string =
        let parameters =
            func.Parameters
            |> List.map printTypeDescription
            |> String.concat ", "
        $"({parameters}) -> {printTypeDescription func.ReturnType}"

    // -- Expressions ----------------------------------------------------------

    let printConstantValue = function
        | ConstantValue.IntConstant value -> string value
        | ConstantValue.RationalConstant value -> value
        | ConstantValue.BooleanConstant true -> "true"
        | ConstantValue.BooleanConstant false -> "false"

    let rec printExpression (expr: Expression) : string =
        let core =
            match expr.Kind with
            | ExpressionKind.Constant value -> printConstantValue value
            | ExpressionKind.Name parts -> String.concat "." parts
            | ExpressionKind.Tuple items ->
                items
                |> List.map printExpression
                |> String.concat ", "
                |> fun inner -> $"({inner})"
            | ExpressionKind.Constructor call -> printConstructorCall call

        match expr.Annotation with
        | None -> core
        | Some annotation -> $"{core}: {printTypeDescription annotation}"

    and printConstructorCall (call: ConstructorCall) : string =
        let args =
            call.Arguments
            |> List.map printExpression
            |> String.concat ", "
        $"{call.Constructor}({args})"

    // -- Patterns -------------------------------------------------------------

    let rec printPattern = function
        | Pattern.Constant (value, _) -> printConstantValue value
        | Pattern.Tuple (items, _) ->
            items
            |> List.map printPattern
            |> String.concat ", "
            |> fun inner -> $"({inner})"
        | Pattern.Constructor ctor -> printConstructorPattern ctor
        | Pattern.Wildcard _ -> "_"

    and printConstructorPattern (pattern: ConstructorPattern) : string =
        let args =
            pattern.Arguments
            |> List.map printPattern
            |> String.concat ", "
        $"{pattern.Constructor}({args})"

    // -- Algebra --------------------------------------------------------------

    let printAlgebraicOperation = function
        | AlgebraicOperation.Mul -> "*"
        | AlgebraicOperation.Div -> "/"
        | AlgebraicOperation.Mod -> "%"
        | AlgebraicOperation.Sum -> "+"
        | AlgebraicOperation.Sub -> "-"

    let rec printAlgebraicExpression (expr: AlgebraicExpression) : string =
        match expr with
        | AlgebraicExpression.Expression value -> printExpression value
        | AlgebraicExpression.Operation (left, op, right, _) ->
            let l = printAlgebraicExpression left
            let r = printAlgebraicExpression right
            $"({l} {printAlgebraicOperation op} {r})"

    let printAlgebraicEqualityCondition = function
        | AlgebraicEqualityCondition.Lt -> "<"
        | AlgebraicEqualityCondition.Le -> "<="
        | AlgebraicEqualityCondition.Eq -> "="
        | AlgebraicEqualityCondition.Ge -> ">="
        | AlgebraicEqualityCondition.Gt -> ">"
        | AlgebraicEqualityCondition.Ne -> "<>"

    let printAlgebraicCondition (condition: AlgebraicCondition) : string =
        let left = printAlgebraicExpression condition.LeftExpression
        let right = printAlgebraicExpression condition.RightExpression
        let op = printAlgebraicEqualityCondition condition.Condition
        $"{left} {op} {right}"

    // -- Predicates -----------------------------------------------------------

    let printPredicateCall (call: PredicateCall) : string =
        call.Arguments
        |> List.map printExpression
        |> String.concat ", "
        |> fun args -> $"{call.PredicateName}({args})"

    let printParameter (parameter: Parameter) : string =
        match parameter.Type with
        | None -> parameter.Name
        | Some tp -> $"{parameter.Name}: {printTypeDescription tp}"

    let printParameters (parameters: Parameter list) : string =
        parameters
        |> List.map printParameter
        |> String.concat ", "
        |> fun inner -> $"({inner})"

    let rec printPredicateBodyItem = function
        | PredicateBodyItem.Call call -> printPredicateCall call
        | PredicateBodyItem.Expression expr -> printExpression expr
        | PredicateBodyItem.PatternMatch (expr, pattern) -> $"{printExpression expr} is {printPattern pattern}"
        | PredicateBodyItem.AlgebraicCondition condition -> printAlgebraicCondition condition

    and printPredicateBody = function
        | PredicateBody.Item (item, _) -> printPredicateBodyItem item
        | PredicateBody.Not (body, _) -> $"not {printPredicateBody body}"
        | PredicateBody.And (left, right, _) -> $"({printPredicateBody left} and {printPredicateBody right})"
        | PredicateBody.Or (left, right, _) -> $"({printPredicateBody left} or {printPredicateBody right})"
        | PredicateBody.Definition (definition, body, _) ->
            let defTxt = printPredicateDefinition definition
            let bodyTxt = printPredicateBody body
            $"{defTxt} {bodyTxt}"

    and printPredicateDefinition (definition: PredicateDefinition) : string =
        let parameters = printParameters definition.Parameters
        let body = printPredicateBody definition.Body
        $"predicate {definition.Name}{parameters} = {body}"

    // -- Deontic --------------------------------------------------------------

    let printDeonticModality = function
        | DeonticModality.Obligated -> "obligated"
        | DeonticModality.Forbidden -> "forbidden"
        | DeonticModality.Permitted -> "permitted"
        | DeonticModality.Suggeseted -> "suggested"

    let printDeonticStatement (statement: DeonticStatement) : string =
        let modality = printDeonticModality statement.Modality
        let namePart =
            match statement.Name with
            | None -> ""
            | Some name -> $" {name} ="
        let body = printPredicateBody statement.Body
        let condition =
            match statement.Condition with
            | None -> ""
            | Some cond -> $" when {printPredicateBody cond}"
        $"{modality}{namePart} {body}{condition}"

    // -- Types & top-level ----------------------------------------------------

    let printTypeDefinition (definition: TypeDefinition) : string =
        $"type {definition.Name} = {printTypeDescription definition.Body}"

    let printDefinition = function
        | Definition.Type definition -> printTypeDefinition definition
        | Definition.Predicate predicate -> printPredicateDefinition predicate
        | Definition.DeonticStatement statement -> printDeonticStatement statement
        | Definition.Fact () -> "// <fact>"

    let printProgram (program: Program) : string =
        program
        |> List.map printDefinition
        |> String.concat "\n\n"

    let unparseAstNode (node: obj) : string =
        match node with
        | :? Program as program -> printProgram program
        | :? Definition as definition -> printDefinition definition
        | :? PredicateDefinition as predicate -> printPredicateDefinition predicate
        | :? DeonticStatement as statement -> printDeonticStatement statement
        | :? TypeDefinition as typeDef -> printTypeDefinition typeDef
        | :? PredicateBody as predicateBody -> printPredicateBody predicateBody
        | :? PredicateBodyItem as item -> printPredicateBodyItem item
        | :? PredicateCall as call -> printPredicateCall call
        | :? AlgebraicCondition as condition -> printAlgebraicCondition condition
        | :? AlgebraicExpression as expr -> printAlgebraicExpression expr
        | :? AlgebraicEqualityCondition as eq -> printAlgebraicEqualityCondition eq
        | :? AlgebraicOperation as op -> printAlgebraicOperation op
        | :? Expression as expr -> printExpression expr
        | :? ConstructorCall as call -> printConstructorCall call
        | :? Pattern as pattern -> printPattern pattern
        | :? ConstructorPattern as pattern -> printConstructorPattern pattern
        | :? TypeDescription as typeDesc -> printTypeDescription typeDesc
        | :? ProductTypeDescription as product -> printProductTypeDescription product
        | :? ProductTypeField as field -> printProductField field
        | :? SumTypeDescription as sumType -> printSumTypeDescription sumType
        | :? SumTypeVariant as variant -> printSumVariant variant
        | :? FuncTypeDescription as func -> printFunctionTypeDescription func
        | :? TypeReference as reference -> printTypeReference reference
        | :? PrimitiveType as primitive -> printPrimitiveType primitive
        | :? Parameter as parameter -> printParameter parameter
        | :? DeonticModality as modality -> printDeonticModality modality
        | other -> sprintf "%A" other
