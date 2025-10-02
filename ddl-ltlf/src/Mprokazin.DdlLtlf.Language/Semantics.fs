module Mprokazin.DdlLtlf.Language.Semantics

open Mprokazin.DdlLtlf.Language.Ast
open System.Collections.Generic

type SemanticError = {
    Kind: string
    Name: string
    Path: string list
    Message: string
}

/// Удобный конструктор ошибки
let private undefined name path =
    { Kind = "UndefinedPredicate"
      Name = name
      Path = List.rev path
      Message = $"Undefined predicate '{name}'" }

/// Рекурсивный обход PredicateExpression с учётом локальных определений
let rec private checkPredicate (env: Set<string>) (path: string list) (errs: ResizeArray<SemanticError>) (p: PredicateExpression) =
    match p with
    | PredicateExpression.NamedPredicateCall(name, _params) ->
        if not (env.Contains name) then errs.Add (undefined name path)

    | PredicateExpression.And (a, b) ->
        checkPredicate env ("and-R"::path) errs b
        checkPredicate env ("and-L"::path) errs a

    | PredicateExpression.Or (a, b) ->
        checkPredicate env ("or-R"::path) errs b
        checkPredicate env ("or-L"::path) errs a

    | PredicateExpression.Not x ->
        checkPredicate env ("not"::path) errs x

    | PredicateExpression.AlgebraicPredicate _ ->
        () // алгебраические — не влияют на окружение предикатов

    | PredicateExpression.NotImplementedExpression _ ->
        () // пропускаем

    | PredicateExpression.NestedPredicate (def, body) ->
        // Имя локального предиката доступно и в его теле (разрешаем рекурсию),
        // и в последующем выражении после 'in'.
        let env' = env.Add def.Name
        // Проверяем тело определения (с already-extended env', чтобы разрешить рекурсию)
        checkPredicate env' ($"def {def.Name}"::path) errs def.Predicate
        // Проверяем выражение после 'in' с тем же окружением
        checkPredicate env' ($"in ({def.Name})"::path) errs body

/// Проверка одного деонтического утверждения
let private checkStatement (globalEnv: Set<string>) (errs: ResizeArray<SemanticError>) (s: DeonticStatement) =
    let basePath = [ $"statement {s.Name}" ]
    checkPredicate globalEnv ("body"::basePath) errs s.Body
    match s.Context with
    | Some ctx -> checkPredicate globalEnv ("context"::basePath) errs ctx
    | None -> ()

/// Проверка тела глобально объявленных предикатов (разрешаем взаимные ссылки)
let private checkGlobalDefs (globalEnv: Set<string>) (errs: ResizeArray<SemanticError>) (defs: NamedPredicateDefinition list) =
    for d in defs do
        checkPredicate globalEnv [ $"def {d.Name}" ] errs d.Predicate

/// Главная функция анализа.
/// Возвращает список ошибок; если он пуст — обращений к неопределённым предикатам нет.
let analyzeUndefinedPredicates (model: Model) : SemanticError list =
    // Глобальное окружение: все имена предикатов, определённых на верхнем уровне.
    let globalEnv =
        model.NamedPredicates
        |> List.map (fun d -> d.Name)
        |> Set.ofList

    let errs = ResizeArray<SemanticError>()

    // 1) Проверяем тела глобальных определений (взаимные ссылки допустимы).
    checkGlobalDefs globalEnv errs model.NamedPredicates

    // 2) Проверяем все деонтические утверждения.
    for st in model.DeonticStatements do
        checkStatement globalEnv errs st

    errs |> Seq.toList

/// Вспомогательная обёртка, если удобнее получать Result
let validate (model: Model) =
    match analyzeUndefinedPredicates model with
    | [] -> Ok ()
    | xs -> Error xs
