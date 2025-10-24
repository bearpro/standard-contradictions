module Mprokazin.DdlLtlf.Language.Typing

open System
open System.Collections.Generic
open Mprokazin.DdlLtlf.Language.Ast

// -------------------------------
// 1. Базовые сущности
// -------------------------------

type TypedObject =
| TypeDefinition      of Ast.TypeDefinition
| TypeDescription     of Ast.TypeDescription
| ProductTypeField    of Ast.ProductTypeField
| Expression          of Ast.Expression
| PredicateCall       of Ast.PredicateCall
| AlgebraicExpression of Ast.AlgebraicExpression
| Parameter           of Ast.Parameter
| AlgebraicCondition  of Ast.AlgebraicCondition
| PredicateBodyItem   of Ast.PredicateBodyItem
| PredicateBody       of Ast.PredicateBody
| PredicateDefinition of Ast.PredicateDefinition
| DeonticStatement    of Ast.DeonticStatement

type TypeVarId = int

type TypedObjectWithId =
    { Id     : TypeVarId
      Object : TypedObject }

type InferedType = 
| Unbound   of id: TypeVarId
| Bound     of TypeDescription
| Conflict  of InferedType list

type ProgramObjTypeInfo =
    { Id     : TypeVarId
      Object : TypedObject
      Type   : InferedType }

type State = ProgramObjTypeInfo list

// -------------------------------
// 2. Окружение для назначения Id
// -------------------------------

type ProductShape =
    { SelfId   : TypeVarId
      FieldIds : Map<string, TypeVarId> }

type Env =
    { mutable NextId         : int
      mutable Types          : Map<string, ProductShape>            // имя типа -> форма
      mutable ShapesBySelfId : Map<TypeVarId, ProductShape> }       // SelfId -> форма

let private freshId (env: Env) : TypeVarId =
    let id = env.NextId
    env.NextId <- env.NextId + 1
    id

let private registerShape (env:Env) (shape:ProductShape) =
    env.ShapesBySelfId <- env.ShapesBySelfId.Add(shape.SelfId, shape)

let private buildProductShape (env: Env) (fields: ProductTypeField list) : ProductShape =
    let selfId = freshId env
    let fieldIds =
        fields
        |> List.fold (fun acc f ->
            if acc |> Map.containsKey f.Name then acc
            else acc |> Map.add f.Name (freshId env)
        ) Map.empty
    let shape = { SelfId = selfId; FieldIds = fieldIds }
    registerShape env shape
    shape

// NEW: построить форму продукта с заданным SelfId (нужно для поля x: ( ... ))
let private buildProductShapeWithSelf (env: Env) (selfId: TypeVarId) (fields: ProductTypeField list) : ProductShape =
    let fieldIds =
        fields
        |> List.fold (fun acc f ->
            if acc |> Map.containsKey f.Name then acc
            else acc |> Map.add f.Name (freshId env)
        ) Map.empty
    let shape = { SelfId = selfId; FieldIds = fieldIds }
    registerShape env shape
    shape

let private ensureNamedType (env: Env) (td: Ast.TypeDefinition) : ProductShape option =
    match td.Body with
    | TypeDescription.Product p ->
        match env.Types |> Map.tryFind td.Name with
        | Some shape -> Some shape
        | None ->
            let shape = buildProductShape env p.Fields
            env.Types <- env.Types |> Map.add td.Name shape
            Some shape
    | _ -> None

// -------------------------------
// 3. makeSequence + стабильные TypeVarId
// -------------------------------

let makeSequence (definitions: Definition list) : (TypedObjectWithId list * Map<TypeVarId, ProductShape>) =
    let acc = System.Collections.Generic.List<TypedObjectWithId> ()
    let add (id:TypeVarId) (obj:TypedObject) =
        acc.Add({ Id = id; Object = obj })

    let env = { NextId = 1; Types = Map.empty; ShapesBySelfId = Map.empty }

    let tryFindCurrentConcreteType (id:TypeVarId) : TypeDescription option =
        // пробежимся с конца acc (он List<TypedObjectWithId>, но это System.Collections.Generic.List, у неё есть индекс)
        let mutable found = None
        for i = acc.Count - 1 downto 0 do
            let item = acc.[i]
            if item.Id = id then
                match item.Object with
                | TypedObject.TypeDescription td ->
                    // считаем это конкретным типом (подойдёт и Product, и Primitive, и Bool)
                    if found.IsNone then
                        found <- Some td
                | _ -> ()
        found

    let addTypeAs (targetId:TypeVarId) (td:TypeDescription) =
        // просто sugar
        add targetId (TypedObject.TypeDescription td)

    let intTd =
        TypeDescription.Reference (TypeReference.Primitive PrimitiveType.Int)

    let ratTd =
        TypeDescription.Reference (TypeReference.Primitive PrimitiveType.Rational)

    let boolTd =
        TypeDescription.Reference (TypeReference.Primitive PrimitiveType.Bool)

    // Разрешение пути полей: ["p"; "x"; "y"] -> Id последнего поля
    let resolveFieldPath (env:Env) (scope:Dictionary<string,TypeVarId>) (segments:string list) : TypeVarId =
        match segments with
        | [] -> freshId env
        | first :: rest ->
            let mutable currentId =
                match scope.TryGetValue(first) with
                | true, id -> id
                | _ ->
                    let nid = freshId env
                    scope.[first] <- nid
                    nid

            for fieldName in rest do
                match env.ShapesBySelfId |> Map.tryFind currentId with
                | Some shape ->
                    match shape.FieldIds |> Map.tryFind fieldName with
                    | Some fid -> currentId <- fid
                    | None ->
                        let newFid = freshId env
                        let newShape = { shape with FieldIds = shape.FieldIds.Add(fieldName, newFid) }
                        env.ShapesBySelfId <- env.ShapesBySelfId.Add(shape.SelfId, newShape)
                        currentId <- newFid
                | None ->
                    // текущий id становится продуктом "на лету"
                    let emptyShape = { SelfId = currentId; FieldIds = Map.empty }
                    registerShape env emptyShape
                    let newFid = freshId env
                    let updated = { emptyShape with FieldIds = emptyShape.FieldIds.Add(fieldName, newFid) }
                    env.ShapesBySelfId <- env.ShapesBySelfId.Add(currentId, updated)
                    currentId <- newFid
            currentId

    // --- Эмиссия типов / полей

    let rec emitProductField (shape: ProductShape option) (f: ProductTypeField) =
        // id поля (значение поля!)
        let fieldId =
            match shape with
            | Some sh -> sh.FieldIds |> Map.tryFind f.Name |> Option.defaultWith (fun () -> freshId env)
            | None    -> freshId env

        // сам Field
        add fieldId (TypedObject.ProductTypeField f)

        // если у поля есть явная аннотация типа
        match f.Type with
        | Some (TypeDescription.Product pdesc) ->
            // ВАЖНО: продукт-тип поля должен иметь SelfId = fieldId,
            // чтобы x: (a) позволил резолвить x.a через ShapesBySelfId[fieldId]
            let fshape = buildProductShapeWithSelf env fieldId pdesc.Fields
            ignore (emitTypeDescription (Some fshape) (TypeDescription.Product pdesc))
        | Some td ->
            ignore (emitTypeDescription None td)
        | None -> ()
    // emitTypeDescriptionWithId: принудительно эмитит данный td как TypeDescription с фиксированным selfId.
    // Для Product/Sum/Function мы всё так же хотим развернуть дочерние куски,
    // но без генерации новой "корневой" переменной типа.
    and emitTypeDescriptionWithId (selfId:TypeVarId) (td: TypeDescription) =
        // Зарегистрировать в acc сам td под уже существующим selfId
        add selfId (TypedObject.TypeDescription td)

        match td with
        | TypeDescription.Product pdesc ->
            // ВАЖНО: у продукта selfId должен стать формой продукта.
            // Если его ещё нет в ShapesBySelfId (например это сигнатура функции),
            // надо создать shape с данным selfId, чтобы дальнейшая точечная нотация работала.
            let shape =
                match env.ShapesBySelfId |> Map.tryFind selfId with
                | Some sh -> sh
                | None ->
                    let sh = buildProductShapeWithSelf env selfId pdesc.Fields
                    sh
            // поля:
            for f in pdesc.Fields do
                emitProductField (Some shape) f

        | TypeDescription.Sum sdesc ->
            for variant in sdesc.Variants do
                variant.Payload
                |> Option.iter (fun payload -> ignore (emitTypeDescription None payload))

        | TypeDescription.Function fdesc ->
            for parameter in fdesc.Parameters do
                ignore (emitTypeDescription None parameter)
            ignore (emitTypeDescription None fdesc.ReturnType)

        | TypeDescription.Reference _ ->
            ()
    and emitTypeDescription (shapeOpt: ProductShape option) (td: TypeDescription) : TypeVarId =
        match td with
        | TypeDescription.Reference _ ->
            let selfId = freshId env
            add selfId (TypedObject.TypeDescription td)
            selfId

        | TypeDescription.Sum sumDesc ->
            let selfId = freshId env
            add selfId (TypedObject.TypeDescription td)
            for variant in sumDesc.Variants do
                variant.Payload
                |> Option.iter (fun payload -> ignore (emitTypeDescription None payload))
            selfId

        | TypeDescription.Product pdesc ->
            let shape =
                match shapeOpt with
                | Some s -> s
                | None   -> buildProductShape env pdesc.Fields

            add shape.SelfId (TypedObject.TypeDescription td)
            for f in pdesc.Fields do
                emitProductField (Some shape) f
            shape.SelfId

        | TypeDescription.Function fdesc ->
            let selfId = freshId env
            add selfId (TypedObject.TypeDescription td)
            for parameter in fdesc.Parameters do
                ignore (emitTypeDescription None parameter)
            ignore (emitTypeDescription None fdesc.ReturnType)
            selfId

    // --- Выражения/значения

    let rec emitExpression (scope: Dictionary<string,TypeVarId>) (expr: Expression) : TypeVarId =
        let nodeId =
            match expr.Kind with
            | ExpressionKind.Name segments ->
                resolveFieldPath env scope segments
            | ExpressionKind.Constant _ ->
                freshId env
            | ExpressionKind.Tuple _ ->
                failwith "Typing for tuple expressions is not implemented yet"
            | ExpressionKind.Constructor _ ->
                failwith "Typing for constructor expressions is not implemented yet"

        add nodeId (TypedObject.Expression expr)

        match expr.Kind with
        | ExpressionKind.Constant (ConstantValue.IntConstant _) ->
            let intTd = TypeDescription.Reference (TypeReference.Primitive PrimitiveType.Int)
            add nodeId (TypedObject.TypeDescription intTd)
        | ExpressionKind.Constant (ConstantValue.RationalConstant _) ->
            let ratTd = TypeDescription.Reference (TypeReference.Primitive PrimitiveType.Rational)
            add nodeId (TypedObject.TypeDescription ratTd)
        | ExpressionKind.Constant (ConstantValue.BooleanConstant _) ->
            let boolTd = TypeDescription.Reference (TypeReference.Primitive PrimitiveType.Bool)
            add nodeId (TypedObject.TypeDescription boolTd)
        | _ -> ()

        expr.Annotation
        |> Option.iter (fun td -> ignore (emitTypeDescription None td))

        nodeId

    let rec emitAlgebraicExpression (scope: Dictionary<string,TypeVarId>) (expr: AlgebraicExpression) : TypeVarId =
        match expr with
        | AlgebraicExpression.Expression v ->
            let vid = emitExpression scope v
            add vid (TypedObject.AlgebraicExpression expr) // разделяем Id со значением
            vid

        | AlgebraicExpression.Operation (l, op, r, _) ->
            let lid = emitAlgebraicExpression scope l
            let rid = emitAlgebraicExpression scope r

            let selfId = freshId env
            add selfId (TypedObject.AlgebraicExpression expr)

            match op with
            | AlgebraicOperation.Mod ->
                // и операнды, и результат должны быть int
                addTypeAs lid intTd
                addTypeAs rid intTd
                // можно (но не обязательно) сказать и самому узлу, что он int;
                // сейчас мы в getSemanticBound для Mod уже это делаем через AlgebraicExpression, так что ок.

            | AlgebraicOperation.Div ->
                // деление -> результат как Rational (getSemanticBound уже это делает),
                // операнды считаем числовыми. Для простоты: тоже Rational.
                addTypeAs lid ratTd
                addTypeAs rid ratTd

            | AlgebraicOperation.Sum
            | AlgebraicOperation.Sub
            | AlgebraicOperation.Mul ->
                // операнды должны быть совместимого числового типа.
                // эвристика:
                // 1. если у кого-то уже есть явный тип (int или rational), копируем его на второго.
                // 2. иначе пока не делаем ничего, до тех пор, пока где-то не появится литерал.
                let lKnown = tryFindCurrentConcreteType lid
                let rKnown = tryFindCurrentConcreteType rid
                match lKnown, rKnown with
                | Some ltd, None ->
                    addTypeAs rid ltd
                | None, Some rtd ->
                    addTypeAs lid rtd
                | _ -> ()
            selfId

    let emitAlgebraicCondition (scope: Dictionary<string,TypeVarId>) (c: AlgebraicCondition) : unit =
        let selfId = freshId env
        add selfId (TypedObject.AlgebraicCondition c)

        let lid = emitAlgebraicExpression scope c.LeftExpression
        let rid = emitAlgebraicExpression scope c.RightExpression

        // правило по видам сравнений
        // тебе нужно посмотреть что у тебя за дизъюнкт в AST:
        //   Eq / Neq / Lt / Gt / Le / Ge ... — я буду называть их условно
        //
        match c.Condition with
        | Eq
        | Ne ->
            // x = y, x != y
            // если кто-то уже конкретно типизирован (int, bool, product и т.п.),
            // протягиваем этот тип на другого
            let lKnown = tryFindCurrentConcreteType lid
            let rKnown = tryFindCurrentConcreteType rid
            match lKnown, rKnown with
            | Some ltd, None ->
                addTypeAs rid ltd
            | None, Some rtd ->
                addTypeAs lid rtd
            | _ -> ()

        | Lt
        | Gt
        | Le
        | Ge ->
            // числовые сравнения — операнды должны быть числовыми.
            // эвристика: если один из операндов уже Int или Rational, копируем на другой.
            let lKnown = tryFindCurrentConcreteType lid
            let rKnown = tryFindCurrentConcreteType rid

            // helper: является ли td числовым (int или rational)?
            let isNumeric td =
                match td with
                | TypeDescription.Reference (TypeReference.Primitive PrimitiveType.Int) -> true
                | TypeDescription.Reference (TypeReference.Primitive PrimitiveType.Rational) -> true
                | _ -> false

            match lKnown, rKnown with
            | Some ltd, None when isNumeric ltd ->
                addTypeAs rid ltd
            | None, Some rtd when isNumeric rtd ->
                addTypeAs lid rtd
            | _ -> ()

        // если появятся другие операции — добавь сюда

    let rec emitPredicateBody (scope: Dictionary<string,TypeVarId>) (body: PredicateBody) : unit =
        let selfId = freshId env
        add selfId (TypedObject.PredicateBody body)
        match body with
        | PredicateBody.Item (item, _) ->
            emitPredicateBodyItem scope item
        | PredicateBody.Not (inner, _) ->
            emitPredicateBody scope inner
        | PredicateBody.And (l, r, _) ->
            emitPredicateBody scope l
            emitPredicateBody scope r
        | PredicateBody.Or (l, r, _) ->
            emitPredicateBody scope l
            emitPredicateBody scope r
        | PredicateBody.Definition (innerPd, innerBody, _) ->
            emitPredicateDefinition innerPd
            emitPredicateBody scope innerBody

    and emitPredicateBodyItem (scope: Dictionary<string,TypeVarId>) (item: PredicateBodyItem) : unit =
        let selfId = freshId env
        add selfId (TypedObject.PredicateBodyItem item)
        match item with
        | PredicateBodyItem.Call pc ->
            let callId = freshId env
            add callId (TypedObject.PredicateCall pc)
            for arg in pc.Arguments do
                ignore (emitExpression scope arg)
        | PredicateBodyItem.Expression v ->
            ignore (emitExpression scope v)
        | PredicateBodyItem.PatternMatch _ ->
            failwith "Typing for pattern matching is not implemented yet"
        | PredicateBodyItem.AlgebraicCondition ac ->
            emitAlgebraicCondition scope ac

    // --- Предикат

    and emitPredicateDefinition (pd: PredicateDefinition) : unit =
        let paramFields : ProductTypeField list =
            pd.Parameters
            |> List.map (fun p -> { Name = p.Name; Type = p.Type; Range = p.Range })
        let paramProduct = { Fields = paramFields; Range = pd.Range }
        let paramFieldNames = paramFields |> List.map (fun f -> f.Name) |> Set.ofList

        let reusedShapeOpt =
            env.Types
            |> Map.tryPick (fun _name shape ->
                let shapeFieldNames = shape.FieldIds |> Map.toList |> List.map fst |> Set.ofList
                if shapeFieldNames = paramFieldNames then Some shape else None)

        let argShape =
            match reusedShapeOpt with
            | Some shape -> shape
            | None       -> buildProductShape env paramProduct.Fields

        // scope: каждое верхнеуровневое поле параметра -> свой FieldId
        let scope = Dictionary<string,TypeVarId>()
        for f in paramProduct.Fields do
            match argShape.FieldIds |> Map.tryFind f.Name with
            | Some fid -> scope.[f.Name] <- fid
            | None     -> ()

        // эмитим сам product-параметр, деля Id с формой
        let paramTypeDesc = TypeDescription.Product paramProduct
        ignore (emitTypeDescription (Some argShape) paramTypeDesc)

        // тело
        emitPredicateBody scope pd.Body

        // тип предиката: (param) -> bool
        let boolTypeDesc =
            PrimitiveType.Bool
            |> TypeReference.Primitive
            |> TypeDescription.Reference

        let predFuncTypeDesc : TypeDescription =
            TypeDescription.Function {
                Parameters = [ paramTypeDesc ]
                ReturnType = boolTypeDesc
                Range = pd.Range
            }

        // один и тот же Id для предиката как объекта и его функционального типа
        let predId = freshId env
        add predId (TypedObject.PredicateDefinition pd)

        // привязываем сигнатуру к predId, а не создаём новый freshId
        emitTypeDescriptionWithId predId predFuncTypeDesc

    // --- Деонтическое высказывание

    let emitDeonticStatement (ds: DeonticStatement) =
        let selfId = freshId env
        add selfId (TypedObject.DeonticStatement ds)
        let scope = Dictionary<string,TypeVarId>()
        emitPredicateBody scope ds.Body
        match ds.Condition with
        | Some condBody -> emitPredicateBody scope condBody
        | None -> ()

    // --- Два прохода по определениям

    for d in definitions do
        match d with
        | Definition.Type td -> ignore (ensureNamedType env td)
        | _ -> ()

    for d in definitions do
        match d with
        | Definition.Type td ->
            let shapeOpt = env.Types |> Map.tryFind td.Name
            match td.Body with
            | TypeDescription.Product pdesc ->
                let shape =
                    match shapeOpt with
                    | Some s -> s
                    | None   -> buildProductShape env pdesc.Fields
                let tdProduct = TypeDescription.Product pdesc
                ignore (emitTypeDescription (Some shape) tdProduct)
            | otherTd ->
                ignore (emitTypeDescription None otherTd)

            let defId =
                match shapeOpt with
                | Some s -> s.SelfId
                | None   -> freshId env
            add defId (TypedObject.TypeDefinition td)

        | Definition.Predicate pd ->
            emitPredicateDefinition pd

        | Definition.DeonticStatement ds ->
            emitDeonticStatement ds

        | Definition.Fact _ -> ()

    (List.ofSeq acc, env.ShapesBySelfId)


// -------------------------------
// 4. Инференс типов
// -------------------------------

let getSemanticBound (objectInfo: ProgramObjTypeInfo) : InferedType =
    match objectInfo.Object with
    | AlgebraicCondition _
    | PredicateCall _ 
    | PredicateBody _ 
    | PredicateBodyItem _ ->
        PrimitiveType.Bool |> TypeReference.Primitive |> TypeDescription.Reference |> InferedType.Bound

    | AlgebraicExpression (AlgebraicExpression.Operation(_, AlgebraicOperation.Div, _, _)) ->
        PrimitiveType.Rational |> TypeReference.Primitive |> TypeDescription.Reference |> InferedType.Bound

    | AlgebraicExpression (AlgebraicExpression.Operation(_, AlgebraicOperation.Mod, _, _)) ->
        PrimitiveType.Int |> TypeReference.Primitive |> TypeDescription.Reference |> InferedType.Bound

    // литералы: сразу известен тип
    | Expression { Kind = ExpressionKind.Constant (ConstantValue.IntConstant _) } ->
        PrimitiveType.Int |> TypeReference.Primitive |> TypeDescription.Reference |> InferedType.Bound
    | Expression { Kind = ExpressionKind.Constant (ConstantValue.RationalConstant _) } ->
        PrimitiveType.Rational |> TypeReference.Primitive |> TypeDescription.Reference |> InferedType.Bound
    | Expression { Kind = ExpressionKind.Constant (ConstantValue.BooleanConstant _) } ->
        PrimitiveType.Bool |> TypeReference.Primitive |> TypeDescription.Reference |> InferedType.Bound

    | _ -> objectInfo.Type

let getDeclaredBound (objectInfo: ProgramObjTypeInfo) : InferedType =
    match objectInfo.Object with
    | TypeDescription td -> InferedType.Bound td
    | TypeDefinition { Body = td } -> InferedType.Bound td
    | Expression { Annotation = Some td } -> InferedType.Bound td
    | Parameter { Type = Some td } -> InferedType.Bound td
    | ProductTypeField { Type = Some td } -> InferedType.Bound td
    | PredicateDefinition { PredicateType = Some ftd } ->
        InferedType.Bound (TypeDescription.Function ftd)
    | _ -> objectInfo.Type

let unify a b =
    match a, b with
    | Bound x, Bound y ->
        if x = y then Bound x else Conflict [ a; b ]
    | Bound x, Unbound _ -> Bound x
    | Unbound _, Bound x -> Bound x
    | Conflict xs, Bound _ -> Conflict (a :: b :: xs)
    | Bound _, Conflict xs -> Conflict (a :: b :: xs)
    | Conflict xs, Conflict ys -> Conflict (xs @ ys)
    | Unbound _, Unbound _ -> a
    | Unbound _, Conflict _ -> b
    | Conflict _, Unbound _ -> a

let inferObject (objectInfo: ProgramObjTypeInfo) (_state:State) : InferedType =
    let semanticBound = getSemanticBound objectInfo
    let declaredBound = getDeclaredBound objectInfo // keep style consistent
    unify semanticBound declaredBound

let rec inferPassForward (objects: ProgramObjTypeInfo list) (acc: State) : State =
    match objects with
    | [] -> acc
    | obj::tail ->
        let inferred = inferObject obj acc
        let updated = { obj with Type = inferred }
        inferPassForward tail (updated :: acc)

// NEW: рекурсивный апгрейд product-типов с учётом типов полей (вложенно)
let patchProducts (shapesBySelfId: Map<TypeVarId, ProductShape>) (state: State) : State =
    // быстрый доступ: Id -> Bound TypeDescription (если есть)
    let finalBound : Dictionary<TypeVarId, TypeDescription> =
        let d = Dictionary<TypeVarId, TypeDescription>()
        for o in state do
            match o.Type with
            | Bound td -> d[o.Id] <- td
            | _ -> ()
        d

    let rec upgrade (selfId: TypeVarId) (td: TypeDescription) : TypeDescription =
        match td with
        | TypeDescription.Product pdesc ->
            match shapesBySelfId |> Map.tryFind selfId with
            | None -> td
            | Some shape ->
                let newFields =
                    pdesc.Fields
                    |> List.map (fun f ->
                        match shape.FieldIds |> Map.tryFind f.Name with
                        | None -> f
                        | Some fid ->
                            // если знаем тип поля — улучшаем его (вложенно, если это продукт)
                            match finalBound.TryGetValue(fid) with
                            | true, ftd ->
                                { f with Type = Some (upgrade fid ftd) }
                            | _ -> f)
                TypeDescription.Product { pdesc with Fields = newFields }

        | TypeDescription.Sum sdesc ->
            let newVars =
                sdesc.Variants
                |> List.mapi (fun i variant ->
                    let upgradedPayload =
                        variant.Payload
                        |> Option.map (fun payload -> upgrade (selfId + i + 1000000) payload)
                    { variant with Payload = upgradedPayload })
            TypeDescription.Sum { sdesc with Variants = newVars }

        | other -> other

    state
    |> List.map (fun o ->
        match o.Type with
        | Bound td ->
            let improved = upgrade o.Id td
            { o with Type = Bound improved }
        | _ -> o)

let inferPassBackward (forwardState: State) : State =
    let byId = forwardState |> List.groupBy (fun o -> o.Id)
    let groupResult =
        byId
        |> List.map (fun (id, objs) ->
            let merged =
                match objs with
                | []    -> Unbound id
                | h::ts -> ts |> List.fold (fun acc o -> unify acc o.Type) h.Type
            id, merged)
        |> Map.ofList

    forwardState
    |> List.map (fun o ->
        let finalType = defaultArg (groupResult.TryFind o.Id) o.Type
        { o with Type = finalType })

let inferTypes (program: Program) : State =
    // 1) построение + снимок форм
    let (seqWithIds, shapesBySelfId) = makeSequence program

    // 2) начальное состояние
    let initialState : ProgramObjTypeInfo list =
        seqWithIds
        |> List.map (fun x ->
            { Id     = x.Id
              Object = x.Object
              Type   = Unbound x.Id })

    // 3) семантика / явные типы
    let afterForward = inferPassForward initialState [] |> List.rev

    // 4) унификация по Id
    let afterBackward = inferPassBackward afterForward

    // 5) поднять типы полей внутрь продуктов (вложенно)
    let afterPatch = patchProducts shapesBySelfId afterBackward

    afterPatch
