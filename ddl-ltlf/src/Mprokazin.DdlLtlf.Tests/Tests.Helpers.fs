module Tests.Helpers

module RangeSanitizer =

    open Mprokazin.DdlLtlf.Language.Ast

    let unsetStatusRange = { StartLine = 0; StartChar = 0; EndLine = 0; EndChar = 0 }

    let rec sanitizeTypeDescription = function
        | Reference r -> Reference r
        | Product { Fields = fields } ->
            let fields = 
                fields
                |> List.map (fun f -> 
                    { f with 
                        Type = f.Type |> Option.map sanitizeTypeDescription
                        Range = unsetStatusRange })
            Product { Fields = fields; Range = unsetStatusRange }
        | TypeDescription.Sum { Variants = variants } ->
            let variants = variants |> List.map sanitizeTypeDescription
            TypeDescription.Sum { Variants = variants; Range = unsetStatusRange }

    let rec sanitizeValueReference (v: ValueReference) =
        { v with 
            Type = v.Type |> Option.map sanitizeTypeDescription
            Range = unsetStatusRange }

    let rec sanitizeAlgebraicExpression = function
        | AlgebraicExpression.Value v -> AlgebraicExpression.Value (sanitizeValueReference v)
        | AlgebraicExpression.Operation (l, op, r, _) ->
            AlgebraicExpression.Operation (sanitizeAlgebraicExpression l, op, sanitizeAlgebraicExpression r, unsetStatusRange)

    let sanitizeAlgebraicCondition (c: AlgebraicCondition) =
        { c with 
            LeftExpression = sanitizeAlgebraicExpression c.LeftExpression
            RightExpression = sanitizeAlgebraicExpression c.RightExpression
            Range = unsetStatusRange }

    let rec sanitizePredicateBody (body: PredicateBody) =
        let sr = unsetStatusRange
        match body with
        | Item (Call call, _) ->
            Item (Call (sanitizePredicateCall call), sr)
        | Item (Value v, _) ->
            Item (Value (sanitizeValueReference v), sr)
        | Item (AlgebraicCondition c, _) ->
            Item (AlgebraicCondition (sanitizeAlgebraicCondition c), sr)
        | Not (b, _) ->
            Not (sanitizePredicateBody b, sr)
        | And (l, r, _) ->
            And (sanitizePredicateBody l, sanitizePredicateBody r, sr)
        | Or (l, r, _) ->
            Or (sanitizePredicateBody l, sanitizePredicateBody r, sr)
        | Definition (def, body, _) ->
            Definition (sanitizePredicateDefinition def, sanitizePredicateBody body, sr)

    and sanitizePredicateCall (call: PredicateCall) =
        { call with 
            Arguments = call.Arguments |> List.map sanitizeValueReference
            Range = unsetStatusRange }

    and sanitizePredicateDefinition (pd: PredicateDefinition) =
        { pd with
            Parameter = pd.Parameter |> fun p ->
                { p with 
                    Fields = p.Fields 
                    Range = unsetStatusRange }
            Body = sanitizePredicateBody pd.Body
            Range = unsetStatusRange }

    let sanitizeTypeDefinition (td: TypeDefinition) =
        { td with 
            Body = sanitizeTypeDescription td.Body
            Range = unsetStatusRange }

    let sanitizeDeonticStatement (d: DeonticStatement) =
        { d with
            Body = sanitizePredicateBody d.Body
            Condition = d.Condition |> Option.map sanitizePredicateBody
            Range = unsetStatusRange }

    let sanitizeDefinition = function
        | Type t -> Type (sanitizeTypeDefinition t)
        | Predicate p -> Predicate (sanitizePredicateDefinition p)
        | DeonticStatement d -> DeonticStatement (sanitizeDeonticStatement d)
        | Fact _ -> Fact ()

    let sanitizeProgram (p: Program) =
        p |> List.map sanitizeDefinition
