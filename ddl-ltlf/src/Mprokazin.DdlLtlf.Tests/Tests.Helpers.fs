module Tests.Helpers

module RangeSanitizer =

    open Mprokazin.DdlLtlf.Language.Ast

    let unsetStatusRange = { StartLine = 0; StartChar = 0; EndLine = 0; EndChar = 0 }

    let rec sanitizeTypeDescription = function
        | TypeDescription.Reference r -> TypeDescription.Reference r
        | TypeDescription.Product { Fields = fields } ->
            let fields =
                fields
                |> List.map (fun f ->
                    { f with
                        Type = f.Type |> Option.map sanitizeTypeDescription
                        Range = unsetStatusRange })
            TypeDescription.Product { Fields = fields; Range = unsetStatusRange }
        | TypeDescription.Sum { Variants = variants } ->
            let variants =
                variants
                |> List.map (fun v ->
                    { v with
                        Payload = v.Payload |> Option.map sanitizeTypeDescription
                        Range = unsetStatusRange })
            TypeDescription.Sum { Variants = variants; Range = unsetStatusRange }
        | TypeDescription.Function { Parameters = parameters; ReturnType = returnType } ->
            let parameters = parameters |> List.map sanitizeTypeDescription
            let returnType = sanitizeTypeDescription returnType
            TypeDescription.Function { Parameters = parameters; ReturnType = returnType; Range = unsetStatusRange }

    let rec sanitizeExpression (expr: Expression) : Expression =
        let kind =
            match expr.Kind with
            | ExpressionKind.Constant value -> ExpressionKind.Constant value
            | ExpressionKind.Name parts -> ExpressionKind.Name parts
            | ExpressionKind.Tuple items ->
                items
                |> List.map sanitizeExpression
                |> ExpressionKind.Tuple
            | ExpressionKind.Constructor call ->
                ExpressionKind.Constructor (sanitizeConstructorCall call)

        { expr with
            Kind = kind
            Annotation = expr.Annotation |> Option.map sanitizeTypeDescription
            Range = unsetStatusRange }

    and sanitizeConstructorCall (call: ConstructorCall) : ConstructorCall =
        { call with
            Arguments = call.Arguments |> List.map sanitizeExpression
            Range = unsetStatusRange }

    let rec sanitizePattern = function
        | Pattern.Constant (value, _) -> Pattern.Constant (value, unsetStatusRange)
        | Pattern.Tuple (items, _) ->
            items
            |> List.map sanitizePattern
            |> fun sanitized -> Pattern.Tuple (sanitized, unsetStatusRange)
        | Pattern.Constructor ctor ->
            Pattern.Constructor (
                { ctor with
                    Arguments = ctor.Arguments |> List.map sanitizePattern
                    Range = unsetStatusRange })
        | Pattern.Wildcard _ -> Pattern.Wildcard unsetStatusRange

    let rec sanitizeAlgebraicExpression = function
        | AlgebraicExpression.Expression expr ->
            AlgebraicExpression.Expression (sanitizeExpression expr)
        | AlgebraicExpression.Operation (l, op, r, _) ->
            AlgebraicExpression.Operation (sanitizeAlgebraicExpression l, op, sanitizeAlgebraicExpression r, unsetStatusRange)

    let sanitizeAlgebraicCondition (condition: AlgebraicCondition) =
        { condition with
            LeftExpression = sanitizeAlgebraicExpression condition.LeftExpression
            RightExpression = sanitizeAlgebraicExpression condition.RightExpression
            Range = unsetStatusRange }

    let rec sanitizePredicateBody (body: PredicateBody) =
        let sr = unsetStatusRange
        match body with
        | Item (Call call, _) -> Item (Call (sanitizePredicateCall call), sr)
        | Item (Expression expr, _) -> Item (Expression (sanitizeExpression expr), sr)
        | Item (PatternMatch (expr, pattern), _) ->
            Item (PatternMatch (sanitizeExpression expr, sanitizePattern pattern), sr)
        | Item (AlgebraicCondition condition, _) ->
            Item (AlgebraicCondition (sanitizeAlgebraicCondition condition), sr)
        | Not (inner, _) -> Not (sanitizePredicateBody inner, sr)
        | And (left, right, _) -> And (sanitizePredicateBody left, sanitizePredicateBody right, sr)
        | Or (left, right, _) -> Or (sanitizePredicateBody left, sanitizePredicateBody right, sr)
        | Definition (definition, innerBody, _) ->
            Definition (sanitizePredicateDefinition definition, sanitizePredicateBody innerBody, sr)

    and sanitizePredicateCall (call: PredicateCall) =
        { call with
            Arguments = call.Arguments |> List.map sanitizeExpression
            Range = unsetStatusRange }

    and sanitizePredicateDefinition (definition: PredicateDefinition) =
        { definition with
            Parameters =
                definition.Parameters
                |> List.map (fun p ->
                    { p with
                        Type = p.Type |> Option.map sanitizeTypeDescription
                        Range = unsetStatusRange })
            Body = sanitizePredicateBody definition.Body
            Range = unsetStatusRange }

    let sanitizeTypeDefinition (definition: TypeDefinition) =
        { definition with
            Body = sanitizeTypeDescription definition.Body
            Range = unsetStatusRange }

    let sanitizeDeonticStatement (statement: DeonticStatement) =
        { statement with
            Body = sanitizePredicateBody statement.Body
            Condition = statement.Condition |> Option.map sanitizePredicateBody
            Range = unsetStatusRange
            Parameters = []
            InferredType = None }

    let sanitizeDefinition = function
        | Type definition -> Type (sanitizeTypeDefinition definition)
        | Predicate definition -> Predicate (sanitizePredicateDefinition definition)
        | DeonticStatement statement -> DeonticStatement (sanitizeDeonticStatement statement)
        | Fact () -> Fact ()

    let sanitizeProgram (program: Program) =
        program |> List.map sanitizeDefinition
