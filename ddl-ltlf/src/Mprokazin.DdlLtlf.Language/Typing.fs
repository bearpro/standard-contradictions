module Mprokazin.DdlLtlf.Language.Typing

open System
open System.Collections.Generic
open Mprokazin.DdlLtlf.Language.Ast

let private zeroRange =
    { StartLine = 0
      StartChar = 0
      EndLine = 0
      EndChar = 0 }

let private rangeOrDefault (range: SourceRange option) =
    range |> Option.defaultValue zeroRange

// ---------------------------------------------------------------------------
// Result helpers
// ---------------------------------------------------------------------------

type TypeResult<'a> = Result<'a, TypeError list>

and TypeErrorKind =
    | UnknownType of string
    | DuplicateType of string
    | UnknownConstructor of string
    | DuplicateConstructor of string
    | UnknownIdentifier of string
    | FieldNotFound of string
    | NotAProductType
    | PredicateNotFound of string
    | PredicateArityMismatch of expected: int * actual: int
    | ArgumentTypeMismatch of index: int * expected: string * actual: string
    | AnnotationMismatch of expected: string * actual: string
    | ExpectedBool
    | ExpectedNumeric
    | ComparisonTypeMismatch of string * string
    | ConstructorArgumentMismatch of string * expected: int * actual: int
    | PatternMismatch of string
    | UnboundParameterType of string
    | GeneralTypeError of string

and TypeError =
    { Kind: TypeErrorKind
      Message: string
      Range: SourceRange }

let private ok x : TypeResult<_> = Ok x

let private error range kind message : TypeResult<_> =
    Error
        [ { Kind = kind
            Message = message
            Range = range } ]

let private mergeResults (results: TypeResult<'a> list) : TypeResult<'a list> =
    let mutable values = []
    let mutable errors = []

    for r in results do
        match r with
        | Ok v -> values <- v :: values
        | Error es -> errors <- es @ errors

    if List.isEmpty errors then
        values |> List.rev |> Ok
    else
        Error errors

let private combine r1 r2 combiner =
    match r1, r2 with
    | Ok x, Ok y -> combiner x y
    | Error e1, Ok _ -> Error e1
    | Ok _, Error e2 -> Error e2
    | Error e1, Error e2 -> Error(e1 @ e2)

type TypeResultBuilder() =
    member _.Bind(m, f) = Result.bind f m
    member _.Return v = Ok v
    member _.ReturnFrom m = m
    member _.Zero() = Ok ()
    member _.Delay(f: unit -> TypeResult<'a>) = f
    member _.Run(f: unit -> TypeResult<'a>) = f()
let typeResult = TypeResultBuilder()

// ---------------------------------------------------------------------------
// Internal type representation
// ---------------------------------------------------------------------------

type TypeVarId = int

type TypeVar =
    { Id: TypeVarId
      mutable Binding: TypeExpr option }

and ProductFieldInfo =
    { mutable Name: string option
      Type: TypeExpr }

and ProductInfo =
    { Id: int
      mutable Fields: ProductFieldInfo list }

and TypeExpr =
    | TInt
    | TRational
    | TBool
    | TProduct of ProductInfo
    | TSum of string
    | TFunction of TypeExpr list * TypeExpr
    | TVar of TypeVar

// NOTE: Fixed compiler error here:
type TypeBuilder =
    { mutable NextTypeVar: int
      mutable NextProductId: int }

let private createBuilder () =
    { NextTypeVar = 1
      NextProductId = 1 }

let private freshTypeVar (builder: TypeBuilder) =
    let id = builder.NextTypeVar
    builder.NextTypeVar <- builder.NextTypeVar + 1
    TVar { Id = id; Binding = None }

let private freshProduct (builder: TypeBuilder) (fields: ProductFieldInfo list) =
    let id = builder.NextProductId
    builder.NextProductId <- builder.NextProductId + 1
    TProduct
        { Id = id
          Fields = fields }

let rec private prune ty =
    match ty with
    | TVar ({ Binding = Some bound } as v) ->
        let pruned = prune bound
        v.Binding <- Some pruned
        pruned
    | _ -> ty

let rec private occurs (target: TypeVarId) (ty: TypeExpr) =
    match prune ty with
    | TVar v -> v.Id = target
    | TProduct info -> info.Fields |> List.exists (fun f -> occurs target f.Type)
    | TFunction (args, ret) -> occurs target ret || args |> List.exists (occurs target)
    | _ -> false

let rec private typeExprStructuralEquals a b =
    match prune a, prune b with
    | TInt, TInt
    | TRational, TRational
    | TBool, TBool -> true
    | TSum n1, TSum n2 -> n1 = n2
    | TFunction (args1, ret1), TFunction (args2, ret2) ->
        List.length args1 = List.length args2
        && List.forall2 typeExprStructuralEquals args1 args2
        && typeExprStructuralEquals ret1 ret2
    | TProduct p1, TProduct p2 ->
        let fields1 = p1.Fields
        let fields2 = p2.Fields
        List.length fields1 = List.length fields2
        && List.forall2 (fun f1 f2 -> typeExprStructuralEquals f1.Type f2.Type) fields1 fields2
    | TVar v1, TVar v2 when obj.ReferenceEquals(v1, v2) -> true
    | TVar { Binding = Some bound }, other -> typeExprStructuralEquals bound other
    | other, TVar { Binding = Some bound } -> typeExprStructuralEquals other bound
    | _ -> false

let rec private typeExprToString ty =
    let ty = prune ty
    match ty with
    | TInt -> "int"
    | TRational -> "rational"
    | TBool -> "bool"
    | TSum name -> name
    | TVar v -> sprintf "'%d" v.Id
    | TProduct info ->
        let parts =
            info.Fields
            |> List.mapi (fun idx field ->
                let name =
                    match field.Name with
                    | Some n -> n
                    | None -> sprintf "_%d" idx
                sprintf "%s: %s" name (typeExprToString field.Type))
        sprintf "(%s)" (String.Join(", ", parts))
    | TFunction (args, ret) ->
        let args = args |> List.map typeExprToString |> Array.ofList
        let argText: string = String.Join(", ", args)
        sprintf "(%s) -> %s" argText (typeExprToString ret)

// ---------------------------------------------------------------------------
// Type description <-> TypeExpr conversion
// ---------------------------------------------------------------------------

type SumConstructorInfo =
    { Constructor: string
      SumName: string
      Payload: TypeExpr option
      Range: SourceRange }

type SumTypeInfo =
    { Name: string
      Range: SourceRange
      Variants: SumConstructorInfo list }

// NOTE: Fixed compiler error here:
type TypeEnvEntry =
    { Definition: TypeDefinition
      Placeholder: TypeExpr option }

type TypeContext =
    { TypeDefinitions: Map<string, TypeEnvEntry>
      AliasCache: Dictionary<string, TypeExpr>
      SumTypes: Dictionary<string, SumTypeInfo>
      Constructors: Dictionary<string, SumConstructorInfo>
      Builder: TypeBuilder }

let private primitiveTypeExpr =
    function
    | PrimitiveType.Int -> TInt
    | PrimitiveType.Rational -> TRational
    | PrimitiveType.Bool -> TBool

let rec private typeExprToTypeDescription (ty: TypeExpr) (range: SourceRange option) : TypeDescription =
    match prune ty with
    | TInt -> PrimitiveType.Int |> TypeReference.Primitive |> TypeDescription.Reference
    | TRational -> PrimitiveType.Rational |> TypeReference.Primitive |> TypeDescription.Reference
    | TBool -> PrimitiveType.Bool |> TypeReference.Primitive |> TypeDescription.Reference
    | TSum name -> TypeReference.Named name |> TypeDescription.Reference
    | TFunction (args, ret) ->
        let argsTd = args |> List.map (fun a -> typeExprToTypeDescription a None)
        let retTd = typeExprToTypeDescription ret None
        TypeDescription.Function
            { Parameters = argsTd
              ReturnType = retTd
              Range = rangeOrDefault range }
    | TVar v ->
        match v.Binding with
        | Some bound -> typeExprToTypeDescription bound range
        | None ->
            // Free type variables should be resolved by the time we render them.
            TypeReference.Named (sprintf "'%d" v.Id) |> TypeDescription.Reference
    | TProduct info ->
        let fields =
            info.Fields
            |> List.mapi (fun idx field ->
                { Name =
                      match field.Name with
                      | Some name -> name
                      | None -> sprintf "_%d" idx
                  Type = Some(typeExprToTypeDescription field.Type None)
                  Range = rangeOrDefault range }: ProductTypeField)

        TypeDescription.Product
            { Fields = fields
              Range = rangeOrDefault range }

let rec private resolveTypeDescription
    (ctx: TypeContext)
    (currentTypeName: string option)
    (desc: TypeDescription)
    : TypeResult<TypeExpr>
    =
    typeResult {
        let builder = ctx.Builder
        match desc with
        | TypeDescription.Reference (TypeReference.Primitive prim) ->
            return primitiveTypeExpr prim

        | TypeDescription.Reference (TypeReference.Named name) ->
            if ctx.AliasCache.ContainsKey name then
                return ctx.AliasCache[name]
            else
                match ctx.TypeDefinitions |> Map.tryFind name with
                | None ->
                    return! error
                        // NOTE: Fixed compiler error here:
                        (match desc with
                        | Reference _ -> { StartLine = -1; StartChar = 0; EndLine = -1; EndChar = 0}
                        | Product { Range = r}
                        | TypeDescription.Sum { Range = r}
                        | Function { Range = r} -> r)
                        (UnknownType name)
                        (sprintf "Type '%s' is not defined." name)
                | Some entry ->
                    let placeholder =
                        entry.Placeholder
                        |> Option.defaultWith (fun _ -> freshTypeVar builder)

                    ctx.AliasCache[name] <- placeholder

                    let! resolved = resolveTypeDescription ctx (Some name) entry.Definition.Body

                    // NOTE: Fixed complier error here:
                    do 
                        match placeholder with
                        | TVar v -> v.Binding <- Some resolved
                        | _ -> ()

                    ctx.AliasCache[name] <- resolved
                    return resolved

        | TypeDescription.Product product ->
            let! fields =
                product.Fields
                |> List.map (fun field ->
                    typeResult {
                        match field.Type with
                        | None ->
                            return! error
                                field.Range
                                (GeneralTypeError (sprintf "Field '%s' is missing a type annotation." field.Name))
                                (sprintf "Field '%s' requires an explicit type." field.Name)
                        | Some td ->
                            let! ty = resolveTypeDescription ctx None td
                            return { Name = Some field.Name; Type = ty }
                    })
                |> mergeResults

            return freshProduct builder fields

        | TypeDescription.Sum sumDesc ->
            match currentTypeName with
            | None ->
                return! error
                    sumDesc.Range
                    (GeneralTypeError "Anonymous sum types are not supported.")
                    "Anonymous sum types are not supported; declare them via 'type Name = (...)'."
            | Some typeName ->
                // NOTE: Fixed compiler error here:
                if not (ctx.SumTypes.ContainsKey typeName) then
                    let! variants = 
                        sumDesc.Variants
                        |> List.map (fun variant ->
                            typeResult {
                                let! payloadTypeOpt =
                                    match variant.Payload with
                                    | Some payloadDesc -> resolveTypeDescription ctx None payloadDesc |> Result.map Some
                                    | None -> ok None

                                let ctorInfo =
                                    { Constructor = variant.Constructor
                                      SumName = typeName
                                      Payload = payloadTypeOpt
                                      Range = variant.Range }

                                if ctx.Constructors.ContainsKey variant.Constructor then
                                    return! error
                                        variant.Range
                                        (DuplicateConstructor variant.Constructor)
                                        (sprintf "Constructor '%s' is already defined." variant.Constructor)
                                else
                                    ctx.Constructors[variant.Constructor] <- ctorInfo
                                    return ctorInfo
                            })
                        |> mergeResults

                    let info =
                        { Name = typeName
                          Range = sumDesc.Range
                          Variants = variants }
                    // NOTE: Fixed compiler error here:
                    let _ = ctx.SumTypes[typeName] <- info
                    return TSum typeName
                // NOTE: Fixed compiler error here:
                else return TSum typeName

        | TypeDescription.Function fn ->
            let! args =
                fn.Parameters
                |> List.map (resolveTypeDescription ctx None)
                |> mergeResults

            let! ret = resolveTypeDescription ctx None fn.ReturnType
            return TFunction(args, ret)
    }

// ---------------------------------------------------------------------------
// Predicate information and environments
// ---------------------------------------------------------------------------

type ParameterInfo =
    { Name: string
      TypeExpr: TypeExpr
      Range: SourceRange }

type PredicateInfo =
    { Name: string
      Parameters: ParameterInfo list
      ReturnType: TypeExpr
      Range: SourceRange }

type PredicateEnv =
    { Predicates: Dictionary<string, PredicateInfo> }

let private createPredicateEnv () =
    { Predicates = Dictionary<string, PredicateInfo>(StringComparer.Ordinal) }

let private registerPredicate (env: PredicateEnv) (info: PredicateInfo) =
    env.Predicates[info.Name] <- info

let private tryFindPredicate (env: PredicateEnv) (name: string) =
    match env.Predicates.TryGetValue name with
    | true, info -> Some info
    | _ -> None

// ---------------------------------------------------------------------------
// Unification and compatibility checks
// ---------------------------------------------------------------------------

let rec private unify range (expected: TypeExpr) (actual: TypeExpr) : TypeResult<unit> =
    let expected = prune expected
    let actual = prune actual

    match expected, actual with
    | TVar ev, _ ->
        if obj.ReferenceEquals(expected, actual) then
            ok ()
        elif occurs ev.Id actual then
            error range (GeneralTypeError "Recursive type detected.") "Recursive type detected during unification."
        else
            ev.Binding <- Some actual
            ok ()
    | _, TVar _ -> unify range actual expected
    | TInt, TInt
    | TRational, TRational
    | TBool, TBool -> ok ()
    | TSum s1, TSum s2 when s1 = s2 -> ok ()
    | TFunction (args1, ret1), TFunction (args2, ret2) ->
        if List.length args1 <> List.length args2 then
            error
                range
                (GeneralTypeError "Function arity mismatch.")
                "Functions have different arity and cannot be unified."
        else
            let argResults = List.map2 (unify range) args1 args2
            let _ = mergeResults argResults
            unify range ret1 ret2
    | TProduct p1, TProduct p2 ->
        let fields1 = p1.Fields
        let fields2 = p2.Fields
        if List.length fields1 <> List.length fields2 then
            error
                range
                (GeneralTypeError "Product arity mismatch.")
                "Product types have different number of fields."
        else
            let merged =
                List.zip fields1 fields2
                |> List.map (fun (f1, f2) ->
                    match f1.Name, f2.Name with
                    | Some n, None -> f2.Name <- Some n
                    | None, Some n -> f1.Name <- Some n
                    | _ -> ()
                    unify range f1.Type f2.Type)
            mergeResults merged |> Result.map (fun _ -> ())
    | _ ->
        let expectedText = typeExprToString expected
        let actualText = typeExprToString actual
        error
            range
            (GeneralTypeError "Type mismatch.")
            $"Type mismatch: expected {expectedText}, got {actualText}."

let private expectBool range ty =
    match prune ty with
    | TBool -> ok ()
    | TVar _ -> unify range TBool ty
    | other ->
        error range ExpectedBool $"Expected bool but got {typeExprToString other}."

let private expectNumeric range ty =
    match prune ty with
    | TInt
    | TRational -> ok ()
    | TVar _ ->
        error range ExpectedNumeric $"Unable to determine numeric type at {range.StartLine}:{range.StartChar}."
    | other ->
        error range ExpectedNumeric $"Expected numeric type but got {typeExprToString other}."

let rec private isResolvedType ty =
    match prune ty with
    | TVar { Binding = None } -> false
    | TVar { Binding = Some bound } -> isResolvedType bound
    | TProduct info -> info.Fields |> List.forall (fun f -> isResolvedType f.Type)
    | TFunction (args, ret) -> List.forall isResolvedType args && isResolvedType ret
    | _ -> true

// ---------------------------------------------------------------------------
// Value environment for expressions
// ---------------------------------------------------------------------------

type ValueEnv =
    { Parent: ValueEnv option
      Values: Dictionary<string, TypeExpr> }

let rec private lookupValue (env: ValueEnv) (name: string) =
    match env.Values.TryGetValue name with
    | true, value -> Some value
    | _ ->
        match env.Parent with
        | Some parent -> lookupValue parent name
        | None -> None

let private extendValueEnv (env: ValueEnv option) =
    { Parent = env
      Values = Dictionary<string, TypeExpr>(StringComparer.Ordinal) }

let private addValue (env: ValueEnv) (name: string) (ty: TypeExpr) =
    env.Values[name] <- ty

// ---------------------------------------------------------------------------
// Free variable collection for deontic statements
// ---------------------------------------------------------------------------

let collectFreeVariablesForDeontic
    (ctx: TypeContext)
    (predicates: PredicateEnv)
    (statement: DeonticStatement)
    : Set<string>
    =
    let knownPredicates =
        predicates.Predicates.Keys
        |> Seq.map id
        |> Set.ofSeq

    let knownConstructors =
        ctx.Constructors.Keys
        |> Seq.map id
        |> Set.ofSeq

    let rec collectExpression (env: Set<string>) (localPreds: Set<string>) (acc: Set<string>) (expr: Expression) =
        match expr.Kind with
        | ExpressionKind.Constant _ -> acc
        | ExpressionKind.Name segments ->
            match segments with
            | head :: _ ->
                if Set.contains head env
                   || Set.contains head knownPredicates
                   || Set.contains head localPreds
                   || Set.contains head knownConstructors
                then acc
                else Set.add head acc
            | [] -> acc
        | ExpressionKind.Tuple items ->
            (acc, items)
            ||> List.fold (fun state item -> collectExpression env localPreds state item)
        | ExpressionKind.Constructor call ->
            (acc, call.Arguments)
            ||> List.fold (fun state item -> collectExpression env localPreds state item)

    let rec collectAlgebraicExpression env localPreds acc algebraic =
        match algebraic with
        | AlgebraicExpression.Expression expr ->
            collectExpression env localPreds acc expr
        | AlgebraicExpression.Operation (left, _, right, _) ->
            let acc = collectAlgebraicExpression env localPreds acc left
            collectAlgebraicExpression env localPreds acc right

    let rec collectItem env localPreds acc item =
        match item with
        | PredicateBodyItem.Expression expr ->
            collectExpression env localPreds acc expr
        | PredicateBodyItem.Call call ->
            (acc, call.Arguments)
            ||> List.fold (fun state arg -> collectExpression env localPreds state arg)
        | PredicateBodyItem.PatternMatch (expr, _) ->
            collectExpression env localPreds acc expr
        | PredicateBodyItem.AlgebraicCondition cond ->
            let acc = collectAlgebraicExpression env localPreds acc cond.LeftExpression
            collectAlgebraicExpression env localPreds acc cond.RightExpression

    let rec collectBody env localPreds acc body =
        match body with
        | PredicateBody.Item (item, _) ->
            collectItem env localPreds acc item
        | PredicateBody.Not (nested, _) ->
            collectBody env localPreds acc nested
        | PredicateBody.And (left, right, _) ->
            let acc = collectBody env localPreds acc left
            collectBody env localPreds acc right
        | PredicateBody.Or (left, right, _) ->
            let acc = collectBody env localPreds acc left
            collectBody env localPreds acc right
        | PredicateBody.Definition (def, rest, _) ->
            let envWithParams =
                def.Parameters
                |> List.fold (fun e param -> Set.add param.Name e) env
            let localPreds' = Set.add def.Name localPreds
            let acc = collectBody envWithParams localPreds' acc def.Body
            collectBody envWithParams localPreds' acc rest

    let initialEnv = Set.empty
    let initialLocalPreds = Set.empty

    let afterBody = collectBody initialEnv initialLocalPreds Set.empty statement.Body

    match statement.Condition with
    | None -> afterBody
    | Some cond -> collectBody initialEnv initialLocalPreds afterBody cond

// ---------------------------------------------------------------------------
// Expression inference
// ---------------------------------------------------------------------------

let rec private inferExpression
    (ctx: TypeContext)
    (predicates: PredicateEnv)
    (env: ValueEnv)
    (expr: Expression)
    : TypeResult<TypeExpr * Expression>
    =
    typeResult {
        match expr.Kind with
        | ExpressionKind.Constant constValue ->
            let ty =
                match constValue with
                | ConstantValue.IntConstant _ -> TInt
                | ConstantValue.RationalConstant _ -> TRational
                | ConstantValue.BooleanConstant _ -> TBool

            let annotated = { expr with Annotation = Some(typeExprToTypeDescription ty (Some expr.Range)) }
            return (ty, annotated)

        | ExpressionKind.Name segments ->
            match segments with
            | [] ->
                return! error expr.Range (UnknownIdentifier "") "Empty name access."
            | head :: tail ->
                match lookupValue env head with
                | None ->
                    return!
                        error
                            expr.Range
                            (UnknownIdentifier head)
                            (sprintf "Identifier '%s' is not defined." head)
                | Some baseType ->
                    let rec resolveField (currentTy: TypeExpr) (names: string list) =
                        typeResult {
                            match names with
                            | [] -> return currentTy
                            | field :: rest ->
                                match prune currentTy with
                                | TProduct product ->
                                    match product.Fields |> List.tryFind (fun f -> f.Name = Some field) with
                                    | Some fld -> return! resolveField fld.Type rest
                                    | None ->
                                        return!
                                            error
                                                expr.Range
                                                (FieldNotFound field)
                                                (sprintf "Field '%s' not found in product type." field)
                                | other ->
                                    return!
                                        error
                                            expr.Range
                                            NotAProductType
                                            (sprintf
                                                "Cannot access field '%s' on non-product type %s."
                                                field
                                                (typeExprToString other))
                        }

                    let! finalTy = resolveField baseType tail
                    let annotated = { expr with Annotation = Some(typeExprToTypeDescription finalTy (Some expr.Range)) }
                    return (finalTy, annotated)

        | ExpressionKind.Tuple items ->
            let! inferred =
                items
                |> List.map (inferExpression ctx predicates env)
                |> mergeResults

            let elementTypes = inferred |> List.map fst
            let updatedItems = inferred |> List.map snd
            let fields =
                elementTypes
                |> List.mapi (fun idx ty ->
                    { Name = Some (sprintf "_%d" idx)
                      Type = ty })

            let productTy = freshProduct ctx.Builder fields

            let! finalTy =
                match expr.Annotation with
                | None -> ok productTy
                | Some ann ->
                    typeResult {
                        let! expected = resolveTypeDescription ctx None ann
                        let! _ = unify expr.Range expected productTy
                        return expected
                    }

            let annotated =
                { expr with
                    Kind = ExpressionKind.Tuple updatedItems
                    Annotation = Some(typeExprToTypeDescription finalTy (Some expr.Range)) }

            return (finalTy, annotated)

        | ExpressionKind.Constructor call ->
            match ctx.Constructors.TryGetValue call.Constructor with
            | false, _ ->
                return!
                    error
                        call.Range
                        (UnknownConstructor call.Constructor)
                        (sprintf "Constructor '%s' is not defined." call.Constructor)
            | true, ctorInfo ->
                let sumType = TSum ctorInfo.SumName
                match ctorInfo.Payload, call.Arguments with
                | None, [] ->
                    let annotated =
                        { expr with
                            Annotation = Some(typeExprToTypeDescription sumType (Some expr.Range)) }
                    return (sumType, annotated)

                | Some payloadTy, [ argument ] ->
                    let! argTy, updatedArg = inferExpression ctx predicates env argument
                    let! _ = unify call.Range payloadTy argTy
                    let annotatedCall = { call with Arguments = [ updatedArg ] }
                    let annotated =
                        { expr with
                            Kind = ExpressionKind.Constructor annotatedCall
                            Annotation = Some(typeExprToTypeDescription sumType (Some expr.Range)) }
                    return (sumType, annotated)

                | Some _, args ->
                    return!
                        error
                            call.Range
                            (ConstructorArgumentMismatch(call.Constructor, 1, List.length args))
                            (sprintf "Constructor '%s' expects exactly one argument." call.Constructor)

                | None, args ->
                    return!
                        error
                            call.Range
                            (ConstructorArgumentMismatch(call.Constructor, 0, List.length args))
                            (sprintf "Constructor '%s' does not accept arguments." call.Constructor)
    }

let rec private inferAlgebraicExpression
    (ctx: TypeContext)
    (predicates: PredicateEnv)
    (env: ValueEnv)
    (expr: AlgebraicExpression)
    : TypeResult<TypeExpr * AlgebraicExpression>
    =
    typeResult {
        match expr with
        | AlgebraicExpression.Expression inner ->
            let! ty, annotated = inferExpression ctx predicates env inner
            return (ty, AlgebraicExpression.Expression annotated)

        | AlgebraicExpression.Operation (left, op, right, range) ->
            let! leftTy, leftExpr = inferAlgebraicExpression ctx predicates env left
            let! rightTy, rightExpr = inferAlgebraicExpression ctx predicates env right

            let! _ = expectNumeric range leftTy
            let! _ = expectNumeric range rightTy

            let! _ =
                match prune leftTy, prune rightTy with
                | TInt, TRational
                | TRational, TInt ->
                    error
                        range
                        (ComparisonTypeMismatch("numeric", "numeric"))
                        "Numeric operands must have the same type."
                | _ -> ok ()

            let resultType =
                match op with
                | AlgebraicOperation.Div -> TRational
                | AlgebraicOperation.Mod -> TInt
                | _ -> prune leftTy

            let updated =
                AlgebraicExpression.Operation(leftExpr, op, rightExpr, range)

            return (resultType, updated)
    }

let rec private checkPattern
    (ctx: TypeContext)
    (predicates: PredicateEnv)
    (env: ValueEnv)
    (expected: TypeExpr)
    (pattern: Pattern)
    : TypeResult<unit * Pattern>
    =
    typeResult {
        match pattern with
        | Pattern.Wildcard range -> return ((), Pattern.Wildcard range)
        | Pattern.Constant (value, range) ->
            let ty =
                match value with
                | ConstantValue.IntConstant _ -> TInt
                | ConstantValue.RationalConstant _ -> TRational
                | ConstantValue.BooleanConstant _ -> TBool
            let! _ = unify range expected ty
            return ((), Pattern.Constant(value, range))

        | Pattern.Tuple (patterns, range) ->
            match prune expected with
            | TProduct product ->
                if List.length product.Fields <> List.length patterns then
                    return!
                        error
                            range
                            (PatternMismatch "Tuple arity mismatch.")
                            "Tuple pattern arity does not match the value."
                else
                    let! _ =
                        List.zip product.Fields patterns
                        |> List.map (fun (field, pat) -> checkPattern ctx predicates env field.Type pat)
                        |> mergeResults
                    let updated = Pattern.Tuple(patterns, range)
                    return ((), updated)
            | other ->
                return!
                    error
                        range
                        (PatternMismatch "Expected product type in pattern.")
                        (sprintf "Pattern expects a product type but got %s." (typeExprToString other))

        | Pattern.Constructor ctorPattern ->
            match ctx.Constructors.TryGetValue ctorPattern.Constructor with
            | false, _ ->
                return!
                    error
                        ctorPattern.Range
                        (UnknownConstructor ctorPattern.Constructor)
                        (sprintf "Constructor '%s' is not defined." ctorPattern.Constructor)
            | true, ctorInfo ->
                let expectedSum = TSum ctorInfo.SumName
                let! _ = unify ctorPattern.Range expected expectedSum
                match ctorInfo.Payload, ctorPattern.Arguments with
                | None, [] -> return ((), pattern)
                | Some payloadTy, [ pat ] ->
                    let! _ = checkPattern ctx predicates env payloadTy pat
                    return ((), pattern)
                | Some _, [] ->
                    return!
                        error
                            ctorPattern.Range
                            (PatternMismatch "Missing constructor payload pattern.")
                            (sprintf "Constructor '%s' expects a payload." ctorPattern.Constructor)
                | None, _ ->
                    return!
                        error
                            ctorPattern.Range
                            (PatternMismatch "Unexpected constructor arguments.")
                            (sprintf "Constructor '%s' does not take arguments." ctorPattern.Constructor)
                | Some _, _ ->
                    return!
                        error
                            ctorPattern.Range
                            (PatternMismatch "Multiple constructor arguments.")
                            "Constructor patterns currently support a single payload argument."
    }

// ---------------------------------------------------------------------------
// Predicate inference
// ---------------------------------------------------------------------------

let rec private inferPredicateBody
    (ctx: TypeContext)
    (predicates: PredicateEnv)
    (env: ValueEnv)
    (body: PredicateBody)
    : TypeResult<PredicateBody>
    =
    typeResult {
        match body with
        | PredicateBody.Item (item, range) ->
            match item with
            | PredicateBodyItem.Expression expr ->
                let! ty, annotated = inferExpression ctx predicates env expr
                let! _ = expectBool range ty
                return PredicateBody.Item(PredicateBodyItem.Expression annotated, range)

            | PredicateBodyItem.Call call ->
                match tryFindPredicate predicates call.PredicateName with
                | None ->
                    return!
                        error
                            call.Range
                            (PredicateNotFound call.PredicateName)
                            (sprintf "Predicate '%s' is not defined." call.PredicateName)
                | Some info ->
                    if List.length info.Parameters <> List.length call.Arguments then
                        return!
                            error
                                call.Range
                                (PredicateArityMismatch(List.length info.Parameters, List.length call.Arguments))
                                (sprintf
                                    "Predicate '%s' expects %d argument(s)."
                                    call.PredicateName
                                    (List.length info.Parameters))
                    else
                        let! updatedArgs =
                            List.zip info.Parameters call.Arguments
                            |> List.map (fun (param, arg) ->
                                typeResult {
                                    let! argTy, annotated = inferExpression ctx predicates env arg
                                    let! _ = unify arg.Range param.TypeExpr argTy
                                    return annotated
                                })
                            |> mergeResults

                        return
                            (PredicateBody.Item(
                                PredicateBodyItem.Call { call with Arguments = updatedArgs },
                                range))

            | PredicateBodyItem.PatternMatch (expr, pattern) ->
                let! exprTy, annotatedExpr = inferExpression ctx predicates env expr
                let! _ = checkPattern ctx predicates env exprTy pattern
                return
                    (PredicateBody.Item(
                        PredicateBodyItem.PatternMatch(annotatedExpr, pattern),
                        range))

            | PredicateBodyItem.AlgebraicCondition cond ->
                let! leftTy, leftExpr = inferAlgebraicExpression ctx predicates env cond.LeftExpression
                let! rightTy, rightExpr = inferAlgebraicExpression ctx predicates env cond.RightExpression
                let! _ = expectNumeric cond.Range leftTy
                let! _ = expectNumeric cond.Range rightTy
                let! _ = unify cond.Range leftTy rightTy
                return
                    (PredicateBody.Item(
                        PredicateBodyItem.AlgebraicCondition
                            { cond with
                                LeftExpression = leftExpr
                                RightExpression = rightExpr },
                        range))

        | PredicateBody.Not (nested, range) ->
            let! typedNested = inferPredicateBody ctx predicates env nested
            return PredicateBody.Not(typedNested, range)

        | PredicateBody.And (left, right, range) ->
            let! leftTyped = inferPredicateBody ctx predicates env left
            let! rightTyped = inferPredicateBody ctx predicates env right
            return PredicateBody.And(leftTyped, rightTyped, range)

        | PredicateBody.Or (left, right, range) ->
            let! leftTyped = inferPredicateBody ctx predicates env left
            let! rightTyped = inferPredicateBody ctx predicates env right
            return PredicateBody.Or(leftTyped, rightTyped, range)

        | PredicateBody.Definition (definition, rest, range) ->
            let extendedPredicates = createPredicateEnv ()
            predicates.Predicates
            |> Seq.iter (fun kv -> extendedPredicates.Predicates[kv.Key] <- kv.Value)

            let! typedDefinition, predicateInfo =
                inferPredicateDefinition ctx extendedPredicates definition

            registerPredicate extendedPredicates predicateInfo

            let! typedRest = inferPredicateBody ctx extendedPredicates env rest
            return PredicateBody.Definition(typedDefinition, typedRest, range)
    }

and private inferPredicateDefinition
    (ctx: TypeContext)
    (predicates: PredicateEnv)
    (definition: PredicateDefinition)
    : TypeResult<PredicateDefinition * PredicateInfo>
    =
    typeResult {
        let valueEnv = extendValueEnv None
        let builder = ctx.Builder

        let! parameterInfos =
            definition.Parameters
            |> List.map (fun parameter ->
                typeResult {
                    match parameter.Type with
                    | Some td ->
                        let! ty = resolveTypeDescription ctx None td
                        let info =
                            { Name = parameter.Name
                              TypeExpr = ty
                              Range = parameter.Range }
                        addValue valueEnv parameter.Name ty
                        return (info, Some td)
                    | None ->
                        let ty = freshTypeVar builder
                        addValue valueEnv parameter.Name ty
                        let info =
                            { Name = parameter.Name
                              TypeExpr = ty
                              Range = parameter.Range }
                        return (info, None)
                })
            |> mergeResults

        let predicateInfo =
            { Name = definition.Name
              Parameters = parameterInfos |> List.map fst
              ReturnType = TBool
              Range = definition.Range }

        registerPredicate predicates predicateInfo

        let! typedBody = inferPredicateBody ctx predicates valueEnv definition.Body

        let unresolvedParameters =
            predicateInfo.Parameters
            |> List.filter (fun p -> isResolvedType p.TypeExpr |> not)

        if not unresolvedParameters.IsEmpty then
            let errors =
                unresolvedParameters
                |> List.map (fun p ->
                    { Kind = UnboundParameterType p.Name
                      Message = sprintf "Could not infer type of parameter '%s'." p.Name
                      Range = p.Range })
            return! Error errors
        else
            let updatedParameters =
                (definition.Parameters, predicateInfo.Parameters)
                ||> List.map2 (fun parameter info ->
                    let typeDescription = typeExprToTypeDescription info.TypeExpr (Some parameter.Range)
                    { parameter with Type = Some typeDescription })

            let predicateType =
                { Parameters =
                      predicateInfo.Parameters
                      |> List.map (fun p -> typeExprToTypeDescription p.TypeExpr None)
                  ReturnType =
                      TypeDescription.Reference
                          (TypeReference.Primitive PrimitiveType.Bool)
                  Range = definition.Range }

            let updatedDefinition =
                { definition with
                    Parameters = updatedParameters
                    Body = typedBody
                    PredicateType = Some predicateType }

            return (updatedDefinition, predicateInfo)
    }

let private inferDeonticStatement
    (ctx: TypeContext)
    (predicates: PredicateEnv)
    (statement: DeonticStatement)
    : TypeResult<DeonticStatement * FuncTypeDescription>
    =
    typeResult {
        let freeVars = collectFreeVariablesForDeontic ctx predicates statement

        let pseudoParams =
            freeVars
            |> Set.toList
            |> List.sort
            |> List.map (fun name ->
                { Name = name
                  Type = None
                  Range = statement.Range })

        let mergedBody =
            match statement.Condition with
            | None -> statement.Body
            | Some cond -> PredicateBody.And(statement.Body, cond, statement.Range)

        let tmpPredicate =
            { Name = statement.Name |> Option.defaultValue "__deontic_tmp__"
              Parameters = pseudoParams
              Body = mergedBody
              Range = statement.Range
              PredicateType = None }

        let localPredicates = createPredicateEnv ()
        predicates.Predicates
        |> Seq.iter (fun kv -> localPredicates.Predicates[kv.Key] <- kv.Value)

        let! _, predicateInfo =
            inferPredicateDefinition ctx localPredicates tmpPredicate

        let boolType =
            TypeDescription.Reference(TypeReference.Primitive PrimitiveType.Bool)

        let funcType =
            { Parameters =
                  predicateInfo.Parameters
                  |> List.map (fun param -> typeExprToTypeDescription param.TypeExpr (Some statement.Range))
              ReturnType = boolType
              Range = statement.Range }

        let valueEnv = extendValueEnv None
        predicateInfo.Parameters
        |> List.iter (fun param -> addValue valueEnv param.Name param.TypeExpr)

        let! typedBody = inferPredicateBody ctx predicates valueEnv statement.Body

        let! typedConditionOpt =
            match statement.Condition with
            | Some cond ->
                inferPredicateBody ctx predicates valueEnv cond
                |> Result.map Some
            | None -> ok None

        let updatedStatement =
            { statement with
                Body = typedBody
                Condition = typedConditionOpt
                InferredType = Some funcType }

        return (updatedStatement, funcType)
    }

// ---------------------------------------------------------------------------
// Program-level helpers
// ---------------------------------------------------------------------------

type TypedObject =
    | TypeDefinition of Ast.TypeDefinition
    | TypeDescription of Ast.TypeDescription
    | ProductTypeField of Ast.ProductTypeField
    | Expression of Ast.Expression
    | PredicateCall of Ast.PredicateCall
    | AlgebraicExpression of Ast.AlgebraicExpression
    | Parameter of Ast.Parameter
    | AlgebraicCondition of Ast.AlgebraicCondition
    | PredicateBodyItem of Ast.PredicateBodyItem
    | PredicateBody of Ast.PredicateBody
    | PredicateDefinition of Ast.PredicateDefinition
    | DeonticStatement of Ast.DeonticStatement

type ProgramObjTypeInfo =
    { Id: int
      Object: TypedObject
      Type: TypeDescription
      Range: SourceRange }

type TypedProgram =
    { Program: Program
      TypedObjects: ProgramObjTypeInfo list }

let private collectTypeDefinitions (program: Program) =
    program
    |> List.choose (function
        | Definition.Type td -> Some td
        | _ -> None)

let private buildTypeContext (program: Program) =
    let builder = createBuilder ()

    let definitions =
        collectTypeDefinitions program
        |> List.fold
            (fun acc def ->
                if Map.containsKey def.Name acc then
                    acc
                else
                    Map.add
                        def.Name
                        { Definition = def
                          Placeholder = None }
                        acc)
            Map.empty

    { TypeDefinitions = definitions
      AliasCache = Dictionary<string, TypeExpr>(StringComparer.Ordinal)
      SumTypes = Dictionary<string, SumTypeInfo>(StringComparer.Ordinal)
      Constructors = Dictionary<string, SumConstructorInfo>(StringComparer.Ordinal)
      Builder = builder }

let private collectTypedObjects (program: Program) =
    let acc = ResizeArray<ProgramObjTypeInfo>()
    let mutable nextId = 1

    let add (obj: TypedObject) (ty: TypeDescription) (range: SourceRange) =
        acc.Add
            { Id = nextId
              Object = obj
              Type = ty
              Range = range }
        nextId <- nextId + 1

    let rec visitExpression expr =
        expr.Annotation
        |> Option.iter (fun ann -> add (TypedObject.Expression expr) ann expr.Range)

        match expr.Kind with
        | ExpressionKind.Constant _ -> ()
        | ExpressionKind.Name _ -> ()
        | ExpressionKind.Tuple items ->
            items |> List.iter visitExpression
        | ExpressionKind.Constructor call ->
            call.Arguments |> List.iter visitExpression

    let rec visitPredicateBody body =
        let range =
            match body with
            | PredicateBody.Item (_, range)
            | PredicateBody.Not (_, range)
            | PredicateBody.And (_, _, range)
            | PredicateBody.Or (_, _, range)
            | PredicateBody.Definition (_, _, range) -> range

        add
            (TypedObject.PredicateBody body)
            (TypeDescription.Reference(TypeReference.Primitive PrimitiveType.Bool))
            range

        match body with
        | PredicateBody.Item (item, _) ->
            match item with
            | PredicateBodyItem.Expression expr -> visitExpression expr
            | PredicateBodyItem.Call call ->
                let td =
                    TypeDescription.Reference(TypeReference.Primitive PrimitiveType.Bool)
                add (TypedObject.PredicateCall call) td call.Range
                call.Arguments |> List.iter visitExpression
            | PredicateBodyItem.PatternMatch (expr, _) -> visitExpression expr
            | PredicateBodyItem.AlgebraicCondition cond ->
                add
                    (TypedObject.AlgebraicCondition cond)
                    (TypeDescription.Reference(TypeReference.Primitive PrimitiveType.Bool))
                    cond.Range
        | PredicateBody.Not (nested, _) -> visitPredicateBody nested
        | PredicateBody.And (left, right, _) ->
            visitPredicateBody left
            visitPredicateBody right
        | PredicateBody.Or (left, right, _) ->
            visitPredicateBody left
            visitPredicateBody right
        | PredicateBody.Definition (def, rest, _) ->
            match def.PredicateType with
            | Some fn -> add (TypedObject.PredicateDefinition def) (TypeDescription.Function fn) def.Range
            | None -> ()
            visitPredicateDefinition def
            visitPredicateBody rest

    and visitPredicateDefinition def =
        def.Parameters
        |> List.iter (fun parameter ->
            match parameter.Type with
            | Some ty -> add (TypedObject.Parameter parameter) ty parameter.Range
            | None -> ())

        visitPredicateBody def.Body

    for definition in program do
        match definition with
        | Definition.Type td ->
            add (TypedObject.TypeDefinition td) td.Body td.Range
        | Definition.Predicate predicate ->
            visitPredicateDefinition predicate
        | Definition.DeonticStatement statement ->
            statement.InferredType
            |> Option.iter (fun fn ->
                add
                    (TypedObject.DeonticStatement statement)
                    (TypeDescription.Function fn)
                    statement.Range)
            visitPredicateBody statement.Body
            statement.Condition
            |> Option.iter visitPredicateBody
        | Definition.Fact _ -> ()

    acc |> Seq.toList

let inferTypes (program: Program) : TypeResult<TypedProgram> =
    let ctx = buildTypeContext program
    let predicates = createPredicateEnv ()

    let mutable programAcc = []
    let mutable errors = []

    for definition in program do
        match definition with
        | Definition.Type td ->
            match resolveTypeDescription ctx (Some td.Name) td.Body with
            | Ok _ -> programAcc <- Definition.Type td :: programAcc
            | Error errs -> errors <- errs @ errors

        | Definition.Predicate predicate ->
            match inferPredicateDefinition ctx predicates predicate with
            | Ok (typedDefinition, info) ->
                registerPredicate predicates info
                programAcc <- Definition.Predicate typedDefinition :: programAcc
            | Error errs -> errors <- errs @ errors

        | Definition.DeonticStatement statement ->
            match inferDeonticStatement ctx predicates statement with
            | Ok (typedStatement, _) ->
                programAcc <- Definition.DeonticStatement typedStatement :: programAcc
            | Error errs ->
                errors <- errs @ errors

        | Definition.Fact () ->
            programAcc <- definition :: programAcc

    if List.isEmpty errors then
        let program = programAcc |> List.rev
        let typedObjects = collectTypedObjects program
        Ok
            { Program = program
              TypedObjects = typedObjects }
    else
        Error errors
