module Mprokazin.DdlLtlf.Solver

open System
open System.Collections.Generic
open Microsoft.Z3
open Mprokazin.DdlLtlf.Language.Ast
open Mprokazin.DdlLtlf.Language.Typing

type PermissionSemantics =
    | Ignore
    | StrongPermission

type Conflict =
    { LeftId: string
      RightId: string
      Reason: string }

type private TypeInfo =
    | TInt
    | TRational
    | TBool
    | TProduct of (string * TypeInfo) list

type private Value =
    | IntVal of IntExpr
    | RealVal of ArithExpr
    | BoolVal of BoolExpr
    | ProductVal of Map<string, Value>

type private SharedValue =
    { Value: Value
      Constraints: BoolExpr list }

type private NumericValue =
    | IntNumeric of IntExpr
    | RealNumeric of ArithExpr

type private TypeEnvironment =
    { TypeDefinitions: Map<string, TypeDefinition>
      Cache: Dictionary<TypeDescription, TypeInfo> }

type private EvalState =
    { Context: Context
      Types: TypeEnvironment
      Predicates: Map<string, PredicateDefinition>
      FreshCounter: int ref
      Assumptions: ResizeArray<BoolExpr>
      SharedParameters: Dictionary<string * TypeDescription, SharedValue> }

// ---------------------------------------------------------------------------
// Type information helpers
// ---------------------------------------------------------------------------

let private buildTypeEnvironment (program: Program) =
    let definitions =
        program
        |> List.choose (function
            | Definition.Type td -> Some(td.Name, td)
            | _ -> None)
        |> Map.ofList
    { TypeDefinitions = definitions
      Cache = Dictionary<TypeDescription, TypeInfo>(HashIdentity.Structural) }

let rec private typeInfoOf (env: TypeEnvironment) (desc: TypeDescription) : TypeInfo =
    match env.Cache.TryGetValue desc with
    | true, info -> info
    | _ ->
        let info =
            match desc with
            | TypeDescription.Reference (TypeReference.Primitive PrimitiveType.Int) -> TInt
            | TypeDescription.Reference (TypeReference.Primitive PrimitiveType.Rational) -> TRational
            | TypeDescription.Reference (TypeReference.Primitive PrimitiveType.Bool) -> TBool
            | TypeDescription.Reference (TypeReference.Named name) ->
                match env.TypeDefinitions |> Map.tryFind name with
                | Some def -> typeInfoOf env def.Body
                | None -> failwithf "Type '%s' is not defined." name
            | TypeDescription.Product product ->
                let fields =
                    product.Fields
                    |> List.map (fun field ->
                        let fieldType =
                            match field.Type with
                            | Some td -> typeInfoOf env td
                            | None -> failwithf "Field '%s' has no type annotation." field.Name
                        field.Name, fieldType)
                TProduct fields
            | _ ->
                failwith "Solver does not support sum or function types yet."

        env.Cache[desc] <- info
        info

let private freshName (state: EvalState) (prefix: string) =
    let id = System.Threading.Interlocked.Increment state.FreshCounter
    sprintf "%s__%d" prefix id

let rec private declareValue (state: EvalState) (name: string) (info: TypeInfo) : SharedValue =
    let assumptionOffset = state.Assumptions.Count

    let value =
        match info with
        | TInt -> IntVal (state.Context.MkIntConst(freshName state name))
        | TRational -> RealVal (state.Context.MkRealConst(freshName state name) :> ArithExpr)
        | TBool -> BoolVal (state.Context.MkBoolConst(freshName state name))
        | TProduct fields ->
            fields
            |> List.map (fun (field, fieldType) ->
                let shared = declareValue state (name + "_" + field) fieldType
                field, shared.Value)
            |> Map.ofList
            |> ProductVal

    let constraints =
        if state.Assumptions.Count <= assumptionOffset then
            []
        else
            state.Assumptions
            |> Seq.skip assumptionOffset
            |> Seq.toList

    { Value = value; Constraints = constraints }
let rec private ensureBool (value: Value) =
    match value with
    | BoolVal b -> b
    | _ -> failwith "Expected boolean value."

let rec private equalValues (state: EvalState) left right =
    match left, right with
    | BoolVal a, BoolVal b -> state.Context.MkEq(a, b)
    | IntVal a, IntVal b -> state.Context.MkEq(a, b)
    | RealVal a, RealVal b -> state.Context.MkEq(a, b)
    | ProductVal a, ProductVal b ->
        a
        |> Map.toList
        |> List.map (fun (name, aValue) ->
            match b |> Map.tryFind name with
            | Some bValue -> equalValues state aValue bValue
            | None -> failwithf "Product mismatch on field '%s'." name)
        |> function
            | [] -> state.Context.MkTrue()
            | list -> state.Context.MkAnd(Array.ofList list)
    | _ -> failwith "Unsupported equality comparison."

// ---------------------------------------------------------------------------
// Expression evaluation
// ---------------------------------------------------------------------------

let rec private evaluateExpression
    (state: EvalState)
    (predicates: Map<string, PredicateDefinition>)
    (variables: Map<string, Value>)
    (expr: Expression)
    : Value =
    match expr.Kind with
    | ExpressionKind.Constant (ConstantValue.IntConstant i) ->
        IntVal (state.Context.MkInt(i))
    | ExpressionKind.Constant (ConstantValue.RationalConstant r) ->
        RealVal (state.Context.MkReal(r) :> ArithExpr)
    | ExpressionKind.Constant (ConstantValue.BooleanConstant b) ->
        BoolVal (state.Context.MkBool(b))
    | ExpressionKind.Name segments ->
        match segments with
        | [] -> failwith "Empty identifier."
        | first :: tail ->
            let headValue =
                match variables |> Map.tryFind first with
                | Some v -> v
                | None -> failwithf "Unknown identifier '%s'." first

            (headValue, tail)
            ||> List.fold (fun current field ->
                match current with
                | ProductVal fields ->
                    match fields |> Map.tryFind field with
                    | Some v -> v
                    | None -> failwithf "Field '%s' not found on '%s'." field first
                | _ ->
                    failwithf "Cannot access field '%s' on non-product value." field)
    | ExpressionKind.Tuple items ->
        items
        |> List.mapi (fun idx item ->
            let value = evaluateExpression state predicates variables item
            sprintf "_%d" idx, value)
        |> Map.ofList
        |> ProductVal
    | ExpressionKind.Constructor _ ->
        failwith "Sum constructors are not supported by solver yet."

let rec private evaluateAlgebraicExpression
    (state: EvalState)
    (predicates: Map<string, PredicateDefinition>)
    (variables: Map<string, Value>)
    (expr: AlgebraicExpression)
    : NumericValue =
    let ctx = state.Context
    match expr with
    | AlgebraicExpression.Expression e ->
        match evaluateExpression state predicates variables e with
        | IntVal i -> IntNumeric i
        | RealVal r -> RealNumeric r
        | _ -> failwith "Expected numeric expression."
    | AlgebraicExpression.Operation (left, op, right, _) ->
        let leftValue = evaluateAlgebraicExpression state predicates variables left
        let rightValue = evaluateAlgebraicExpression state predicates variables right
        let toReal =
            function
            | IntNumeric i -> ctx.MkInt2Real(i) :> ArithExpr
            | RealNumeric r -> r

        match op with
        | AlgebraicOperation.Mod ->
            match leftValue, rightValue with
            | IntNumeric l, IntNumeric r -> IntNumeric (ctx.MkMod(l, r))
            | _ -> failwith "Modulo expects integer operands."
        | AlgebraicOperation.Div ->
            RealNumeric (ctx.MkDiv(toReal leftValue, toReal rightValue))
        | AlgebraicOperation.Sum ->
            match leftValue, rightValue with
            | IntNumeric l, IntNumeric r ->
                IntNumeric (ctx.MkAdd(l :> ArithExpr, r :> ArithExpr) :?> IntExpr)
            | _ ->
                RealNumeric (ctx.MkAdd(toReal leftValue, toReal rightValue))
        | AlgebraicOperation.Sub ->
            match leftValue, rightValue with
            | IntNumeric l, IntNumeric r ->
                IntNumeric (ctx.MkSub(l :> ArithExpr, r :> ArithExpr) :?> IntExpr)
            | _ ->
                RealNumeric (ctx.MkSub(toReal leftValue, toReal rightValue))
        | AlgebraicOperation.Mul ->
            match leftValue, rightValue with
            | IntNumeric l, IntNumeric r ->
                IntNumeric (ctx.MkMul(l :> ArithExpr, r :> ArithExpr) :?> IntExpr)
            | _ ->
                RealNumeric (ctx.MkMul(toReal leftValue, toReal rightValue))

// ---------------------------------------------------------------------------
// Predicate evaluation
// ---------------------------------------------------------------------------

let rec private evaluatePredicateBody
    (state: EvalState)
    (predicates: Map<string, PredicateDefinition>)
    (variables: Map<string, Value>)
    (body: PredicateBody)
    (stack: Set<string>)
    : BoolExpr =
    let ctx = state.Context
    match body with
    | PredicateBody.Item (item, _) ->
        match item with
        | PredicateBodyItem.Expression expr ->
            evaluateExpression state predicates variables expr |> ensureBool
        | PredicateBodyItem.PatternMatch _ ->
            failwith "Pattern matching is not supported by solver yet."
        | PredicateBodyItem.AlgebraicCondition cond ->
            let leftValue = evaluateAlgebraicExpression state predicates variables cond.LeftExpression
            let rightValue = evaluateAlgebraicExpression state predicates variables cond.RightExpression
            let toReal =
                function
                | IntNumeric i -> ctx.MkInt2Real(i) :> ArithExpr
                | RealNumeric r -> r
            let leftExpr, rightExpr =
                match leftValue, rightValue with
                | IntNumeric l, IntNumeric r -> (l :> ArithExpr, r :> ArithExpr)
                | _ -> (toReal leftValue, toReal rightValue)
            match cond.Condition with
            | AlgebraicEqualityCondition.Eq -> ctx.MkEq(leftExpr, rightExpr)
            | AlgebraicEqualityCondition.Ne -> ctx.MkNot(ctx.MkEq(leftExpr, rightExpr))
            | AlgebraicEqualityCondition.Gt -> ctx.MkGt(leftExpr, rightExpr)
            | AlgebraicEqualityCondition.Ge -> ctx.MkGe(leftExpr, rightExpr)
            | AlgebraicEqualityCondition.Lt -> ctx.MkLt(leftExpr, rightExpr)
            | AlgebraicEqualityCondition.Le -> ctx.MkLe(leftExpr, rightExpr)
        | PredicateBodyItem.Call call ->
            evaluatePredicateCall state predicates variables call stack
    | PredicateBody.Not (nested, _) ->
        ctx.MkNot(evaluatePredicateBody state predicates variables nested stack)
    | PredicateBody.And (left, right, _) ->
        ctx.MkAnd
            [| evaluatePredicateBody state predicates variables left stack
               evaluatePredicateBody state predicates variables right stack |]
    | PredicateBody.Or (left, right, _) ->
        ctx.MkOr
            [| evaluatePredicateBody state predicates variables left stack
               evaluatePredicateBody state predicates variables right stack |]
    | PredicateBody.Definition (_, _, _) ->
        failwith "Local predicate definitions are not supported by solver yet."

and private evaluatePredicateCall
    (state: EvalState)
    (predicates: Map<string, PredicateDefinition>)
    (variables: Map<string, Value>)
    (call: PredicateCall)
    (stack: Set<string>)
    : BoolExpr =
    match predicates |> Map.tryFind call.PredicateName with
    | None -> failwithf "Unknown predicate '%s'." call.PredicateName
    | Some definition ->
        if stack.Contains call.PredicateName then
            failwithf "Recursive predicate '%s' is not supported." call.PredicateName

        if definition.Parameters.Length <> call.Arguments.Length then
            failwithf "Predicate '%s' arity mismatch." call.PredicateName

        let evaluatedArguments =
            (definition.Parameters, call.Arguments)
            ||> List.map2 (fun parameter argument ->
                let argumentValue = evaluateExpression state predicates variables argument
                match parameter.Type with
                | Some td ->
                    let expectedType = typeInfoOf state.Types td
                    match expectedType, argumentValue with
                    | TInt, IntVal _
                    | TRational, RealVal _
                    | TBool, BoolVal _
                    | TProduct _, ProductVal _ -> argumentValue
                    | _ ->
                        failwithf "Predicate '%s' argument type mismatch." call.PredicateName
                | None -> argumentValue)

        let localVariables =
            (definition.Parameters, evaluatedArguments)
            ||> List.fold2 (fun env param value ->
                env |> Map.add param.Name value) variables

        evaluatePredicateBody
            state
            predicates
            localVariables
            definition.Body
            (stack.Add call.PredicateName)

// ---------------------------------------------------------------------------
// Normalization
// ---------------------------------------------------------------------------

type private Normalized =
    { Name: string
      Modality: DeonticModality
      Context: BoolExpr
      Body: BoolExpr
      Source: DeonticStatement }

let private normalizeStatements (state: EvalState) semantics (program: Program) =
    let predicateMap =
        state.Predicates

    let normalize (statement: DeonticStatement) =
        match statement.Modality with
        | DeonticModality.Suggeseted -> None
        | DeonticModality.Permitted when semantics = PermissionSemantics.Ignore -> None
        | _ ->
            let assumptionOffset = state.Assumptions.Count

            let parameters =
                statement.Parameters
                |> List.map (fun param ->
                    match param.Type with
                    | Some td ->
                        let key = (param.Name, td)
                        let shared =
                            match state.SharedParameters.TryGetValue key with
                            | true, existing ->
                                for constraintExpr in existing.Constraints do
                                    state.Assumptions.Add constraintExpr
                                existing
                            | _ ->
                                let info = typeInfoOf state.Types td
                                let created = declareValue state param.Name info
                                state.SharedParameters[key] <- created
                                created
                        param.Name, shared.Value
                    | None ->
                        failwithf "Unable to infer parameter type for '%s'." (param.Name))
                |> Map.ofList

            let contextExpr =
                match statement.Condition with
                | None -> state.Context.MkTrue()
                | Some cond ->
                    evaluatePredicateBody state predicateMap parameters cond Set.empty

            let bodyExpr =
                evaluatePredicateBody state predicateMap parameters statement.Body Set.empty

            let normalizedBody =
                match statement.Modality with
                | DeonticModality.Forbidden -> state.Context.MkNot(bodyExpr)
                | _ -> bodyExpr

            let assumptionCount = state.Assumptions.Count - assumptionOffset
            let contextExpr =
                if assumptionCount <= 0 then
                    contextExpr
                else
                    let newAssumptions =
                        state.Assumptions
                        |> Seq.skip assumptionOffset
                        |> Seq.toArray
                    state.Context.MkAnd(Array.append newAssumptions [| contextExpr |])

            if assumptionCount > 0 then
                state.Assumptions.RemoveRange(assumptionOffset, assumptionCount)

            Some
                { Name = statement.Name |> Option.defaultValue "<anonymous>"
                  Modality = statement.Modality
                  Context = contextExpr
                  Body = normalizedBody
                  Source = statement }

    program
    |> List.choose (function
        | Definition.DeonticStatement ds -> normalize ds
        | _ -> None)

// ---------------------------------------------------------------------------
// Conflict analysis
// ---------------------------------------------------------------------------

let private analyzeConflicts (ctx: Context) (rules: Normalized list) =
    let sat expr =
        use solver = ctx.MkSolver()
        solver.Add [| expr |]
        solver.Check() = Status.SATISFIABLE

    let unsat expr = not (sat expr)

    let conflicts = ResizeArray<Conflict>()

    for i = 0 to rules.Length - 1 do
        for j = i + 1 to rules.Length - 1 do
            let left = rules[i]
            let right = rules[j]

            let contextsOverlap =
                sat (ctx.MkAnd(Array.ofList [ left.Context; right.Context ]))

            if contextsOverlap then
                let bodiesCompatible =
                    let conjunction =
                        ctx.MkAnd(Array.ofList [ left.Context; right.Context; left.Body; right.Body ])
                    not (unsat conjunction)

                if not bodiesCompatible then
                    conflicts.Add
                        { LeftId = left.Name
                          RightId = right.Name
                          Reason = "Bodies are incompatible under overlapping contexts." }

    conflicts |> Seq.toList

// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------

let private evaluateTypedProgram semantics (typed: TypedProgram) =
    use ctx = new Context()

    let state =
        { Context = ctx
          Types = buildTypeEnvironment typed.Program
          Predicates =
              typed.Program
              |> List.choose (function
                  | Definition.Predicate p -> Some(p.Name, p)
                  | _ -> None)
              |> Map.ofList
          FreshCounter = ref 0
          Assumptions = ResizeArray()
          SharedParameters = Dictionary<string * TypeDescription, SharedValue>(HashIdentity.Structural) }

    let normalized = normalizeStatements state semantics typed.Program
    analyzeConflicts ctx (List.ofSeq normalized)

let findConflicts semantics (program: Program) =
    match inferTypes program with
    | Ok typed -> evaluateTypedProgram semantics typed
    | Error errors ->
        let message =
            errors
            |> List.map (fun e -> e.Message)
            |> String.concat "; "
        failwithf "Typing failed: %s" message
