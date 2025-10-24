module Mprokazin.DdlLtlf.Language.Typing

open System
open Mprokazin.DdlLtlf.Language.Ast

// Все  синтаксические конструкции которые подлежат выводу типов
type TypedObject =
| TypeDefinition of Ast.TypeDefinition
| TypeDescription of Ast.TypeDescription
| ProductTypeField of Ast.ProductTypeField
| ValueReference of Ast.ValueReference
| PredicateCall of Ast.PredicateCall
| AlgebraicExpression of Ast.AlgebraicExpression
| Parameter of Ast.Parameter
| AlgebraicCondition of Ast.AlgebraicCondition
| PredicateBodyItem of Ast.PredicateBodyItem
| PredicateBody of Ast.PredicateBody
| PredicateDefinition of Ast.PredicateDefinition
| DeonticStatement of Ast.DeonticStatement


let makeSequence (ast: Definition list) : TypedObject list =
    // ---- helpers that turn AST node -> list<TypedObject> in *causal* order ----
    let rec ofTypeDescription (td: TypeDescription) : TypedObject list =
        // сама декларация типа
        let self = TypedObject.TypeDescription td
        // рекурсивно разворачиваем поля и подтипы
        let children =
            match td with
            | Reference _ ->
                []
            | Product { Fields = fields } ->
                fields
                |> List.collect ofProductField
            | TypeDescription.Sum { Variants = variants } ->
                variants
                |> List.collect ofTypeDescription
        self :: children

    and ofProductField (f: ProductTypeField) : TypedObject list =
        let self = TypedObject.ProductTypeField f
        // у поля может быть аннотированный тип
        let nested =
            match f.Type with
            | None -> []
            | Some t -> ofTypeDescription t
        self :: nested

    let ofValueReference (v: ValueReference) : TypedObject list =
        let self = TypedObject.ValueReference v
        let annotatedType =
            match v.Type with
            | None -> []
            | Some t -> ofTypeDescription t
        self :: annotatedType

    let rec ofAlgebraicExpression (expr: AlgebraicExpression) : TypedObject list =
        match expr with
        | AlgebraicExpression.Value v ->
            // сначала сам объект-выражение, потом значение
            TypedObject.AlgebraicExpression expr
            :: ofValueReference v
        | AlgebraicExpression.Operation (l, _, r, _) ->
            // порядок: текущее выражение, потом левое, потом правое
            TypedObject.AlgebraicExpression expr
            :: (ofAlgebraicExpression l @ ofAlgebraicExpression r)

    let ofAlgebraicCondition (c: AlgebraicCondition) : TypedObject list =
        // условие как узел
        let self = TypedObject.AlgebraicCondition c
        // потом обе части выражений
        self
        :: (ofAlgebraicExpression c.LeftExpression
            @ ofAlgebraicExpression c.RightExpression)

    // тело предиката (логическая формула, and/or/not/...)
    let rec ofPredicateBody (body: PredicateBody) : TypedObject list =
        // всегда кладём сам body
        let self = TypedObject.PredicateBody body

        let rest =
            match body with
            | PredicateBody.Item (item, _) ->
                ofPredicateBodyItem item
            | PredicateBody.Not (inner, _) ->
                ofPredicateBody inner
            | PredicateBody.And (l, r, _) ->
                ofPredicateBody l @ ofPredicateBody r
            | PredicateBody.Or (l, r, _) ->
                ofPredicateBody l @ ofPredicateBody r
            | PredicateBody.Definition (predDef, innerBody, _) ->
                // локальное определение предиката внутри тела:
                // сначала сама дефиниция, потом продолжение тела
                ofPredicateDefinition predDef @ ofPredicateBody innerBody

        self :: rest

    and ofPredicateBodyItem (item: PredicateBodyItem) : TypedObject list =
        let self = TypedObject.PredicateBodyItem item
        let rest =
            match item with
            | PredicateBodyItem.Call pc ->
                ofPredicateCall pc
            | PredicateBodyItem.Value v ->
                ofValueReference v
            | PredicateBodyItem.AlgebraicCondition ac ->
                ofAlgebraicCondition ac
        self :: rest

    and ofPredicateCall (pc: PredicateCall) : TypedObject list =
        // сам вызов
        let self = TypedObject.PredicateCall pc
        // плюс аргументы
        let args =
            pc.Arguments
            |> List.collect ofValueReference
        self :: args

    and ofPredicateDefinition (pd: PredicateDefinition) : TypedObject list =
        // сам предикат
        let self = TypedObject.PredicateDefinition pd
        // параметры
        let parameters = pd.Parameter |> Product |> ofTypeDescription
        // тело
        let bodyItems = ofPredicateBody pd.Body 
        self :: (parameters @ bodyItems)

    let ofDeonticStatement (ds: DeonticStatement) : TypedObject list =
        // сама деонтическая формула
        let self = TypedObject.DeonticStatement ds
        // тело (обязательная часть)
        let bodySeq = ofPredicateBody ds.Body
        // условие (если есть)
        let condSeq =
            match ds.Condition with
            | None -> []
            | Some condBody -> ofPredicateBody condBody
        self :: (bodySeq @ condSeq)

    // ---- top-level dispatcher ----
    let ofDefinition (d: Definition) : TypedObject list =
        match d with
        | Definition.Type td ->
            // объявление типа:
            // разворачиваем td.Body как TypeDefinition
            ofTypeDescription td.Body
        | Definition.Fact _ ->
            // Fact сейчас не типизируем → пусто
            []
        | Definition.Predicate pd ->
            ofPredicateDefinition pd
        | Definition.DeonticStatement ds ->
            ofDeonticStatement ds

    // ---- run over whole program ----
    ast |> List.collect ofDefinition

type InferedType = 
| Unbound   of id: int
| Bound     of TypeDescription
| Conflict  of InferedType list

type ProgramObjTypeInfo = { Object: TypedObject; Type: InferedType }

type State = ProgramObjTypeInfo list

let getSemanticBound objectTypeInfo : InferedType =
    match objectTypeInfo.Object with
    | AlgebraicCondition _
    | PredicateCall _ 
    | PredicateBody _ -> 
        PrimitiveType.Bool |> TypeReference.Primitive |> TypeDescription.Reference |> InferedType.Bound
    | AlgebraicExpression(Operation(_, AlgebraicOperation.Div, _, _)) ->
        PrimitiveType.Rational |> TypeReference.Primitive |> TypeDescription.Reference |> InferedType.Bound
    | AlgebraicExpression(Operation(_, AlgebraicOperation.Mod, _, _)) ->
        PrimitiveType.Int |> TypeReference.Primitive |> TypeDescription.Reference |> InferedType.Bound
    | _ -> objectTypeInfo.Type

let getDeclaredBound objectTypeInfo : InferedType =
    match objectTypeInfo.Object with
    | TypeDescription td -> InferedType.Bound td
    | TypeDefinition { Body = td } -> InferedType.Bound td
    | ValueReference { Type = Some td } -> InferedType.Bound td
    | Parameter { Type = Some td } -> InferedType.Bound td
    | ProductTypeField { Type = Some td } -> InferedType.Bound td
    | _ -> objectTypeInfo.Type

let unify a b =
    match a, b with
    | Bound x, Bound y ->
        if x = y then Bound x
        else Conflict [ a; b ]
    | Bound x, Unbound _ -> Bound x
    | Unbound _, Bound x -> Bound x
    | Conflict xs, Bound _ -> Conflict (a::b::xs) // или аккуратнее слить
    | Bound _, Conflict xs -> Conflict (a::b::xs)
    | Conflict xs, Conflict ys -> Conflict (xs @ ys)
    | Unbound _, Unbound _ -> a // не важно, оба свободны
    | Unbound _, Conflict _ -> b
    | Conflict _, Unbound _ -> a

let inferObject objectTypeInfo state =
    let semanticBound = getSemanticBound objectTypeInfo
    let declaredBound = getDeclaredBound objectTypeInfo
    
    unify semanticBound declaredBound

let rec inferPassForward (objects: ProgramObjTypeInfo list) (state: State) : State =
    match objects with
    | [] -> state
    | object::tail -> 
        let infered = inferObject object state
        let state: State = ({ object with Type = infered} :: state)
        inferPassForward tail state

let rec inferPassBackward state =
    state

let inferTypes (program:Program) : State =
    let programSequence = 
        program
        |> makeSequence 
        |> List.mapi (fun i x -> { Object = x; Type = InferedType.Unbound i } )
    inferPassForward programSequence []
