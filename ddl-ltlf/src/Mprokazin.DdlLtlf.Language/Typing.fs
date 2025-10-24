module Mprokazin.DdlLtlf.Language.Typing

open System
open System.Collections.Generic
open Mprokazin.DdlLtlf.Language.Ast

// -------------------------------
// 1. Базовые сущности
// -------------------------------

// Все синтаксические конструкции которые подлежат выводу типов
type TypedObject =
| TypeDefinition      of Ast.TypeDefinition
| TypeDescription     of Ast.TypeDescription
| ProductTypeField    of Ast.ProductTypeField
| ValueReference      of Ast.ValueReference
| PredicateCall       of Ast.PredicateCall
| AlgebraicExpression of Ast.AlgebraicExpression
| Parameter           of Ast.Parameter
| AlgebraicCondition  of Ast.AlgebraicCondition
| PredicateBodyItem   of Ast.PredicateBodyItem
| PredicateBody       of Ast.PredicateBody
| PredicateDefinition of Ast.PredicateDefinition
| DeonticStatement    of Ast.DeonticStatement

// Каждой типизируемой сущности нужен стабильный type-var id.
// ОДИН id должен использоваться:
//   - в определении product-типа
//   - в параметре предиката, если он тот же shape
//   - в обращениях к полям этого product'a
//
// Т.е. это не "индекс в списке", а именно "кто я как типовая переменная".
type TypeVarId = int

type TypedObjectWithId =
    { Id     : TypeVarId
      Object : TypedObject }

// Тип после вывода
type InferedType = 
| Unbound   of id: TypeVarId
| Bound     of TypeDescription
| Conflict  of InferedType list

// Состояние о типе конкретного узла программы
type ProgramObjTypeInfo =
    { Id     : TypeVarId
      Object : TypedObject
      Type   : InferedType }

type State = ProgramObjTypeInfo list


// -------------------------------
// 2. Вспомогательное окружение для назначения Id
// -------------------------------
//
// Нам нужно присваивать одинаковый Id повторяющимся логическим сущностям.
// Примеры сущностей:
//  - сам именованный product-тип (например, A)
//  - каждое поле product-типа (A.x)
//  - предикат как функция (pred)
//  - параметр предиката (который структурно product)
//  - поля параметра предиката (которые могут совпадать с полями типа)
//
// Мы будем держать окружение, где:
//  * для каждого объявленного product-типа мы помним:
//      SelfId  -> TypeVarId для самого этого product-типа
//      FieldIds["x"] -> TypeVarId для поля "x"
//  * у freshId() счётчик идёт глобально.

type ProductShape =
    { SelfId   : TypeVarId
      FieldIds : Map<string, TypeVarId> }

type Env =
    { mutable NextId : int
      // Имя типа -> его shape (Id для самого типа + Id для его полей)
      mutable Types  : Map<string, ProductShape>
      mutable ShapesBySelfId : Map<TypeVarId, ProductShape> }

let private freshId (env: Env) : TypeVarId =
    let id = env.NextId
    env.NextId <- env.NextId + 1
    id

let private registerShape (env:Env) (shape:ProductShape) =
    env.ShapesBySelfId <- env.ShapesBySelfId.Add(shape.SelfId, shape)

// Создать "форму" для product-типа по списку полей.
// Каждый field получает собственный TypeVarId.
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

// Добавить именованный тип в окружение, если его там ещё нет.
// Вернуть его shape.
let private ensureNamedType (env: Env) (td: Ast.TypeDefinition) : ProductShape option =
    // сейчас мы поддерживаем только product-типы и sum-типы.
    // Sum-типы → это варианты, на них можно навесить отдельный id как на сам sum.
    // Для унификации предикат-параметр ↔ именованный тип нам нужен именно product.
    match td.Body with
    | TypeDescription.Product p ->
        match env.Types |> Map.tryFind td.Name with
        | Some shape -> Some shape
        | None ->
            let shape = buildProductShape env p.Fields
            env.Types <- env.Types |> Map.add td.Name shape
            Some shape
    | _ ->
        // не product — не сохраняем форму для маппинга аргумента предиката
        None


// -------------------------------
// 3. Построение плоской последовательности (makeSequence)
//    + назначение стабильных TypeVarId
// -------------------------------
//
// Возвращаем НЕ просто TypedObject list, а TypedObjectWithId list.
// Каждый элемент уже помечен Id, который будет использоваться как Unbound id.

let makeSequence (definitions: Definition list) : TypedObjectWithId list =

    // Внутренний аккумулятор результата
    let acc = List<TypedObjectWithId> ()
    let add (id:TypeVarId) (obj:TypedObject) =
        acc.Add({ Id = id; Object = obj })

    let env = { NextId = 1; Types = Map.empty; ShapesBySelfId = Map.empty }

    let resolveFieldPath (env:Env) (scope:Dictionary<string,TypeVarId>) (segments:string list) : TypeVarId =
        match segments with
        | [] ->
            // странно, но вернём fresh
            freshId env
        | first :: rest ->
            // найдём id первого сегмента (переменной верхнего уровня)
            let mutable currentId =
                match scope.TryGetValue(first) with
                | true, id -> id
                | _        -> freshId env

            // теперь спускаемся по остальным сегментам как по полям продукта
            for fieldName in rest do
                // пытаемся узнать shape текущего значения
                match env.ShapesBySelfId |> Map.tryFind currentId with
                | Some shape ->
                    match shape.FieldIds |> Map.tryFind fieldName with
                    | Some fieldId ->
                        currentId <- fieldId
                    | None ->
                        // Поля такого нет. Создаём новое поле-id на лету.
                        let newId = freshId env
                        // Обновим shape.FieldIds для текущего продукта
                        let newFieldIds = shape.FieldIds.Add(fieldName, newId)
                        let newShape =
                            { shape with FieldIds = newFieldIds }
                        env.ShapesBySelfId <- env.ShapesBySelfId.Add(shape.SelfId, newShape)
                        currentId <- newId
                | None ->
                    // Текущий объект не зарегистрирован как product,
                    // но мы пытаемся взять у него поле.
                    // Значит он "внезапно стал product'ом".
                    // Создадим новый shape для него прямо сейчас.
                    let newShape =
                        { SelfId   = currentId
                          FieldIds = Map.empty }
                    env.ShapesBySelfId <- env.ShapesBySelfId.Add(currentId, newShape)
                    // повторим lookup ещё раз руками:
                    let newFieldId = freshId env
                    let newerShape =
                        { newShape with FieldIds = newShape.FieldIds.Add(fieldName, newFieldId) }
                    env.ShapesBySelfId <- env.ShapesBySelfId.Add(currentId, newerShape)
                    currentId <- newFieldId
            // финальный id поля/цепочки
            currentId

    // Локальная функция: сопоставить Bound-тип полю, если он был явно аннотирован,
    // и гарантировать что у этого поля есть стабильный id
    //
    // Нам нужен ещё один слой: когда мы говорим про поле ProductTypeField f,
    // мы должны иметь возможность получить его TypeVarId из shape.FieldIds[f.Name].
    //
    // Поэтому почти все обходы ниже повсюду прокидывают:
    //   - shape для текущего product (SelfId + FieldIds)
    //   - локальные мапы имен к TypeVarId (например, для параметров предиката)
    //
    // Это то самое "scope": имя -> TypeVarId
    //
    //   Пример:
    //      predicate pred(x) = x % 2 = 0
    //   Здесь параметр "x" логически означает "поле 'x' некоего product-а".
    //
    //   Значит в scope будет:
    //      "x" -> <TypeVarId поля 'x' в аргументном product-е>

    // ------------------
    // Рекурсивный обход ProductTypeDescription / TypeDescription
    // c назначением id из известного shape.
    // ------------------

    // Разворачивает один ProductTypeField, выбирая корректный id
    // из shape.FieldIds по имени поля.
    let rec emitProductField (shape: ProductShape option) (f: ProductTypeField) =
        // если shape есть, то id поля известен (shape.FieldIds.[f.Name])
        // иначе создаём новый id для этого поля
        let fieldId =
            match shape with
            | Some sh ->
                sh.FieldIds
                |> Map.tryFind f.Name
                |> Option.defaultWith (fun () -> freshId env)
            | None -> freshId env

        // сам Field
        add fieldId (TypedObject.ProductTypeField f)

        // если у поля есть явная аннотация типа - разворачиваем её
        match f.Type with
        | Some td -> emitTypeDescription None td  // None: внутри поля нет предопределённого shape
        | None -> ()

    // Разворачивает TypeDescription и эмитит TypeDescription-узел
    // Для product-типа пытается использовать существующий shape (если есть)
    and emitTypeDescription (shapeOpt: ProductShape option) (td: TypeDescription) =
        match td with
        | TypeDescription.Reference _ ->
            // просто ссылка на примитив/по имени.
            let selfId = freshId env
            add selfId (TypedObject.TypeDescription td)

        | TypeDescription.Sum sumDesc ->
            // у Sum сейчас единый selfId.
            // подтипы внутри Sum не обязаны делить с ним id.
            let selfId = freshId env
            add selfId (TypedObject.TypeDescription td)
            for v in sumDesc.Variants do
                emitTypeDescription None v

        | TypeDescription.Product pdesc ->
            // У product-типа есть "shape": если shapeOpt дан, используем его SelfId/FieldIds.
            // Если нет → создаём новый shape.
            let shape =
                match shapeOpt with
                | Some s -> s
                | None ->
                    // buildProductShape присвоит freshId всем полям и SelfId
                    buildProductShape env pdesc.Fields

            // сам product целиком
            add shape.SelfId (TypedObject.TypeDescription td)

            // и поля
            for f in pdesc.Fields do
                emitProductField (Some shape) f

        | TypeDescription.Function fdesc ->
            // Функция — новое понятие, это не из исходного кода,
            // но нужно для типа предиката. Ей даём freshId для "функции как объекта".
            let selfId = freshId env
            add selfId (TypedObject.TypeDescription td)

            // параметр функции — это SumTypeDescription по твоему дизайну.
            // (у тебя параметр сейчас объявлен как SumTypeDescription, но семантически в предикате будет один Product внутри)
            for variant in fdesc.Parameter.Variants do
                emitTypeDescription None variant

            emitTypeDescription None fdesc.Value

    // ------------------
    // Предикат, тело, выражения
    // ------------------

    // Здесь scope: Dictionary<string,TypeVarId> - сопоставляет имя локальной "переменной" с полем аргумента.
    //
    // В простейшем случае предикат pred(x) = ...
    //   у параметра product есть поле "x" -> id_x
    //   в scope кладём scope.["x"] = id_x
    //
    // Дальше любые ValueReference(Name "x") должны получить этот же id_x.

    let rec emitValueReference 
        (scope: System.Collections.Generic.Dictionary<string,TypeVarId>) 
        (v: ValueReference) =
    
        let nodeId : TypeVarId =
            match v.Value with
            | ValueReferenceValue.Name segments ->
                // вот тут мы используем цепочку сегментов
                resolveFieldPath env scope segments

            | ValueReferenceValue.IntConstant _
            | ValueReferenceValue.RationalConstant _
            | ValueReferenceValue.BooleanConstant _ ->
                freshId env

        add nodeId (TypedObject.ValueReference v)

        match v.Type with
        | Some td -> emitTypeDescription None td
        | None -> ()

    let rec emitAlgebraicExpression (scope: Dictionary<string,TypeVarId>) (expr: AlgebraicExpression) =
        let selfId = freshId env

        add selfId (TypedObject.AlgebraicExpression expr)

        match expr with
        | AlgebraicExpression.Value v ->
            emitValueReference scope v
        | AlgebraicExpression.Operation (l, _, r, _) ->
            emitAlgebraicExpression scope l
            emitAlgebraicExpression scope r

    let emitAlgebraicCondition (scope: Dictionary<string,TypeVarId>) (c: AlgebraicCondition) =
        let selfId = freshId env
        add selfId (TypedObject.AlgebraicCondition c)

        emitAlgebraicExpression scope c.LeftExpression
        emitAlgebraicExpression scope c.RightExpression

    let rec emitPredicateBody (scope: Dictionary<string,TypeVarId>) (body: PredicateBody) =
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
            // локальное определение предиката в теле
            emitPredicateDefinition innerPd
            emitPredicateBody scope innerBody

    and emitPredicateBodyItem (scope: Dictionary<string,TypeVarId>) (item: PredicateBodyItem) =
        let selfId = freshId env
        add selfId (TypedObject.PredicateBodyItem item)

        match item with
        | PredicateBodyItem.Call pc ->
            // Вызов предиката, сам по себе булев.
            let callId = freshId env
            add callId (TypedObject.PredicateCall pc)
            for arg in pc.Arguments do
                emitValueReference scope arg
        | PredicateBodyItem.Value v ->
            emitValueReference scope v
        | PredicateBodyItem.AlgebraicCondition ac ->
            emitAlgebraicCondition scope ac

    // ------------------
    // Предикат целиком
    // ------------------
    and emitPredicateDefinition (pd: PredicateDefinition) =
        //
        // 1. Пытаемся сопоставить параметр предиката с каким-то уже объявленным именованным product-типом.
        //    В твоей модели у предиката один параметр, и это ProductTypeDescription.
        //
        //    Если есть тип A = (x, ...), и здесь Parameter = (x, ...) с тем же набором полей,
        //    то мы хотим реиспользовать Shape из A (одни и те же TypeVarId для полей).
        //
        //    Если подходящего типа нет, создаём новый shape.
        //

        // Соберём поля параметра предиката.
        let paramProduct = pd.Parameter
        let paramFieldNames = paramProduct.Fields |> List.map (fun f -> f.Name) |> Set.ofList

        // heuristics: ищем среди env.Types такой product, у которого та же сигнатура полей (по именам).
        // если находим → берём его Shape
        let reusedShapeOpt =
            env.Types
            |> Map.tryPick (fun _name shape ->
                let shapeFieldNames = shape.FieldIds |> Map.toList |> List.map fst |> Set.ofList
                if shapeFieldNames = paramFieldNames then Some shape else None
            )

        // если не нашли - создаём новый shape для аргумента предиката
        let argShape =
            match reusedShapeOpt with
            | Some shape -> shape
            | None ->
                buildProductShape env paramProduct.Fields

        // создаём scope для тела предиката:
        // каждое поле параметра (например "x") мапим на уже выделенный для поля id из argShape
        let scope = Dictionary<string,TypeVarId>()
        for f in paramProduct.Fields do
            match argShape.FieldIds |> Map.tryFind f.Name with
            | Some fid -> scope.[f.Name] <- fid
            | None -> () // shouldn't really happen, но ладно

        // 2. Сам параметр предиката — это product. 
        //    Мы хотим эмитить:
        //      - каждый ProductTypeField (x, ...) с его fieldId
        //      - сам product как TypeDescription(Product ...) с argShape.SelfId
        //
        //    Обёртка "Parameter" (твой Ast.Parameter) у тебя в AST была как отдельный тип?
        //    Сейчас у PredicateDefinition.Parameter = ProductTypeDescription,
        //    нет явных Ast.Parameter дочерних узлов с именем (как было раньше в обычном многопараметрическом стиле).
        //
        //    Значит TypedObject.Parameter из твоего union сейчас почти не используется напрямую,
        //    но я его оставлю; если у тебя всё ещё есть Ast.Parameter где-то (например при локальных let-подобных вещах),
        //    можно будет сюда встроить.

        // эмитим сам product-параметр
        let paramTypeDesc = TypeDescription.Product paramProduct
        emitTypeDescription (Some argShape) paramTypeDesc

        // 3. Эмитим тело предиката
        emitPredicateBody scope pd.Body

        // 4. Тип предиката — это Function(argType, Bool)
        //
        //    argType: мы можем представить как:
        //      - если reusedShapeOpt был Some shape, и shape вообще соответствует именованному типу A,
        //        можно сделать TypeDescription.Reference (Named "A").
        //      - иначе надо зафиксировать конкретный Product(...) с его полями.
        //
        //    Чтобы не врать, пока используем конкретный product (pd.Parameter) прямо.
        //    Результат -> Bool.
        //
        let boolTypeDesc =
            PrimitiveType.Bool
            |> TypeReference.Primitive
            |> TypeDescription.Reference

        let predFuncTypeDesc : TypeDescription =
            TypeDescription.Function {
                Parameter = { Variants = [ paramTypeDesc ]; Range = pd.Range }
                Value     = boolTypeDesc
                Range     = pd.Range
            }

        // Добавим сам объект предиката: TypedObject.PredicateDefinition pd
        // и повесим на него новый Id для функции.
        let predId = freshId env
        add predId (TypedObject.PredicateDefinition pd)

        // Также полезно явно эмитить TypeDescription для функции-предиката,
        // чтобы унификация могла его подхватить
        emitTypeDescription None predFuncTypeDesc


    // ------------------
    // Деонтическое высказывание
    // ------------------
    let emitDeonticStatement (ds: DeonticStatement) =
        let selfId = freshId env
        add selfId (TypedObject.DeonticStatement ds)

        let scope = Dictionary<string,TypeVarId>()
        emitPredicateBody scope ds.Body

        match ds.Condition with
        | Some condBody -> emitPredicateBody scope condBody
        | None -> ()


    // ------------------
    // Сам проход по верхнеуровневым Definition
    // ------------------

    // Нам нужно сначала прогреть env.Types известными именованными типами,
    // чтобы потом предикаты могли reuse их shape.

    // Первый проход — зарегать product-типы по именам.
    for d in definitions do
        match d with
        | Definition.Type td ->
            // если это product type, сохраним его shape в env
            match ensureNamedType env td with
            | Some _ -> ()
            | None -> ()
        | _ -> ()

    // Второй проход — собирать плоскую последовательность уже с ID
    for d in definitions do
        match d with
        | Definition.Type td ->
            // у типа td может быть shape, если это product
            let shapeOpt = env.Types |> Map.tryFind td.Name

            // 1) Развернуть тело типа
            match td.Body with
            | TypeDescription.Product pdesc ->
                let shape =
                    match shapeOpt with
                    | Some s -> s
                    | None -> buildProductShape env pdesc.Fields

                // сам product typedesc
                let tdProduct = TypeDescription.Product pdesc
                emitTypeDescription (Some shape) tdProduct

            | otherTd ->
                // Sum / Reference / Function (функций "из кода" не бывает, но вдруг)
                emitTypeDescription None otherTd

            // 2) И саму декларацию типа
            //    TypeDefinition должен делить SelfId с телом, если это product.
            let defId =
                match shapeOpt with
                | Some s -> s.SelfId
                | None -> freshId env

            add defId (TypedObject.TypeDefinition td)

        | Definition.Predicate pd ->
            emitPredicateDefinition pd

        | Definition.DeonticStatement ds ->
            emitDeonticStatement ds

        | Definition.Fact _ ->
            // Факты пока не типизируем
            ()

    // конец makeSequence
    List.ofSeq acc



// -------------------------------
// 4. Инференс типов
// -------------------------------

// getSemanticBound: семантические "жёсткие" типы по языку
// - логические выражения → Bool
// - вызов предиката → Bool
// - AlgebraicCondition → Bool
// - деление → Rational
// - mod → Int
let getSemanticBound (objectInfo: ProgramObjTypeInfo) : InferedType =
    match objectInfo.Object with
    | AlgebraicCondition _
    | PredicateCall _ 
    | PredicateBody _ -> 
        PrimitiveType.Bool 
        |> TypeReference.Primitive 
        |> TypeDescription.Reference 
        |> InferedType.Bound

    | AlgebraicExpression (AlgebraicExpression.Operation(_, AlgebraicOperation.Div, _, _)) ->
        PrimitiveType.Rational 
        |> TypeReference.Primitive 
        |> TypeDescription.Reference 
        |> InferedType.Bound

    | AlgebraicExpression (AlgebraicExpression.Operation(_, AlgebraicOperation.Mod, _, _)) ->
        PrimitiveType.Int 
        |> TypeReference.Primitive 
        |> TypeDescription.Reference 
        |> InferedType.Bound

    | _ -> objectInfo.Type


// getDeclaredBound: то, что явно описано в коде аннотациями
let getDeclaredBound (objectInfo: ProgramObjTypeInfo) : InferedType =
    match objectInfo.Object with
    | TypeDescription td -> InferedType.Bound td
    | TypeDefinition { Body = td } -> InferedType.Bound td
    | ValueReference { Type = Some td } -> InferedType.Bound td
    // Parameter сейчас фактически не используется (см. emitPredicateDefinition),
    // но если он где-то появится с Type=Some td, тоже учтём.
    | Parameter { Type = Some td } -> InferedType.Bound td
    | ProductTypeField { Type = Some td } -> InferedType.Bound td
    | PredicateDefinition { PredicateType = Some ftd } ->
        // Если ты в будущем заполнишь PredicateType во время парсинга,
        // это эксплицитная сигнатура предиката.
        InferedType.Bound (TypeDescription.Function ftd)
    | _ -> objectInfo.Type


// Унификация: склеиваем две догадки о типе в одну
let unify a b =
    match a, b with
    | Bound x, Bound y ->
        if x = y then Bound x
        else Conflict [ a; b ]

    | Bound x, Unbound _ -> Bound x
    | Unbound _, Bound x -> Bound x

    | Conflict xs, Bound _ -> Conflict (a :: b :: xs)
    | Bound _, Conflict xs -> Conflict (a :: b :: xs)
    | Conflict xs, Conflict ys -> Conflict (xs @ ys)

    | Unbound _, Unbound _ -> a // оба не знают лучше
    | Unbound _, Conflict _ -> b
    | Conflict _, Unbound _ -> a


// Для одного узла делаем попытку уточнить его тип:
// семантика языка + явно объявленный тип
let inferObject (objectInfo: ProgramObjTypeInfo) (state:State) : InferedType =
    let semanticBound = getSemanticBound objectInfo
    let declaredBound = getDeclaredBound objectInfo
    unify semanticBound declaredBound


// Один прямой проход по всем объектам в порядке их появления.
// Важно: мы пока НЕ делаем обратного проталкивания по общему Id.
// Это сделаем следующим шагом.
let rec inferPassForward (objects: ProgramObjTypeInfo list) (acc: State) : State =
    match objects with
    | [] -> acc
    | obj::tail ->
        let inferred = inferObject obj acc
        let updated = { obj with Type = inferred }
        inferPassForward tail (updated :: acc)


// Обратный проход для согласования одинаковых TypeVarId.
// Идея:
//  - после forward у нас есть список ProgramObjTypeInfo с их Type
//  - сгруппируем по Id
//  - внутри каждой группы унифицируем все типы
//  - и потом раздадим этот "итог по группе" всем членам группы
//
// Это как "closure": если поле 'x' выяснилось Int в одном месте, то и в объявлении типа A поле 'x' тоже Int.
let inferPassBackward (forwardState: State) : State =

    // Группируем по Id
    let byId =
        forwardState
        |> List.groupBy (fun o -> o.Id)

    // Для каждой группы считаем итоговый тип объединением всех наблюдений
    let groupResult =
        byId
        |> List.map (fun (id, objs) ->
            let mergedType =
                objs
                |> List.map (fun o -> o.Type)
                |> function
                    | [] -> Unbound id
                    | h::t -> List.fold unify h t
            (id, mergedType))

        |> Map.ofList

    // Теперь обновляем каждый объект его "финальным" типом
    forwardState
    |> List.map (fun o ->
        let finalType =
            match groupResult.TryFind o.Id with
            | Some t -> t
            | None -> o.Type
        { o with Type = finalType })


// Главная функция вывода типов по всей программе
let inferTypes (program: Program) : State =
    // 1. строим последовательность + назначаем стабильные Id
    let seqWithIds : TypedObjectWithId list = makeSequence program

    // 2. инициализируем состояние: каждому объекту даём Unbound с его собственным Id
    let initialState : ProgramObjTypeInfo list =
        seqWithIds
        |> List.map (fun x ->
            { Id     = x.Id
              Object = x.Object
              Type   = Unbound x.Id })

    // 3. прямой проход (семантика / объявленные типы)
    let afterForward =
        inferPassForward initialState []
        |> List.rev

    // 4. обратный проход (согласование по Id)
    let afterBackward =
        inferPassBackward afterForward

    afterBackward
