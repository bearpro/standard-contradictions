module Mprokazin.DdlLtlf.Solver

open System
open Microsoft.Z3
open Mprokazin.DdlLtlf.Language.Ast

// ---- Конфиг семантики ----
type PermissionSemantics =
  | Ignore            // не учитывать Permitted в конфликтах
  | StrongPermission  // Permitted φ конфликтует с Obligated ¬φ

// ---- Внутренние типы ----
type Normalized =
  { Id       : string
    Context  : PredicateExpression   // уже без None (true)
    Body     : PredicateExpression   // то, что "обязано"
    Source   : DeonticStatement }

type Conflict =
  { LeftId    : string
    RightId   : string
    Reason    : string }

// ==================== Н О Р М А Л И З А Ц И Я ====================

let private truePred = AlgebraicPredicate(AlgebraicExpression.Constant 1,
                                          AlgebraicExpression.Constant 1,
                                          AlgebraicEqualityCondition.Eq)

let private negate (p: PredicateExpression) =
  match p with
  | Not q -> q                // чуть-чуть уплощаем ¬(¬q)
  | q     -> Not q

let private normalize (sem: PermissionSemantics) (ds: DeonticStatement) : Normalized option =
  // приводим к паре (Context, Body) только для того, что реально может конфликтовать
  match ds.Modality with
  | DeonticModality.Obligated ->
      Some { Id = ds.Name; Context = defaultArg ds.Context truePred; Body = ds.Body; Source = ds }
  | DeonticModality.Forbidden ->
      Some { Id = ds.Name; Context = defaultArg ds.Context truePred; Body = negate ds.Body; Source = ds }
  | DeonticModality.Permitted ->
      match sem with
      | Ignore -> None
      | StrongPermission ->
          // представим как «анти-запрет»: это пригодится в детекции конфликта с обязательным ¬φ
          // технически: отметим специальной формой — здесь оставим как есть, а в парном матчинге учтём
          Some { Id = ds.Name; Context = defaultArg ds.Context truePred; Body = ds.Body; Source = ds }
  | DeonticModality.Suggested ->
      None

// ==================== С Б О Р   П Е Р Е М Е Н Н Ы Х ====================

let rec private varsA = function
  | AlgebraicExpression.Constant _      -> Set.empty
  | AlgebraicExpression.Variable v      -> Set.singleton v
  | AlgebraicExpression.Op(a,_,b)        -> Set.union (varsA a) (varsA b)

let rec private varsP = function
  | And(a,b) | Or(a,b)                  -> Set.union (varsP a) (varsP b)
  | Not p                               -> varsP p
  | AlgebraicPredicate(a, b, _)         -> Set.union (varsA a) (varsA b)
  | NamedPredicateCall(_, pars)         -> Set.ofArray pars
  | NestedPredicate(defined, p)         -> Set.union (Set.ofArray defined.Parameters) (varsP defined.Predicate) |> Set.union (varsP p)
  | NotImplementedExpression _          -> Set.empty

let private collectVars (norm: Normalized list) =
  norm
  |> List.collect (fun r -> [ varsP r.Context; varsP r.Body ])
  |> List.fold Set.union Set.empty
  |> Set.toList

// ==================== Т Р А Н С Л Я Ц И Я   В   Z 3 ====================

type private Z = {
  Ctx  : Context
  Env  : Map<string, Expr>     // имена → IntConst
}

let private mkZ (vars: string list) =
  let ctx = new Context()
  let env: Map<string, Expr> =
    vars |> List.fold (fun m v -> m.Add(v, (ctx.MkIntConst v) :> Expr)) Map.empty
  { Ctx = ctx; Env = env }

let private aexpr (z:Z) =
  let rec go = function
    | AlgebraicExpression.Constant i   -> z.Ctx.MkInt(i) :> ArithExpr
    | AlgebraicExpression.Variable v   -> z.Env[v] :?> ArithExpr
    | AlgebraicExpression.Op(a,op,b)     -> 
      match op with
      | Sum -> z.Ctx.MkAdd(go a, go b)
      | Mod -> z.Ctx.MkMod(go a :?> IntExpr, go b :?> IntExpr)
      | Mul -> z.Ctx.MkMul(go a, go b)
      | Sub -> z.Ctx.MkSub(go a, go b)
      | Dev -> z.Ctx.MkDiv(go a, go b)
      
        
  go

let private pred (z:Z) =
  let rec go = function
    | And(a,b) ->
        z.Ctx.MkAnd(go a, go b)
    | Or(a,b)  ->
        z.Ctx.MkOr(go a, go b)
    | Not p    ->
        z.Ctx.MkNot(go p)
    | AlgebraicPredicate(a,b,cond) ->
        let aa, bb = aexpr z a, aexpr z b
        match cond with
        | AlgebraicEqualityCondition.Eq -> z.Ctx.MkEq(aa,bb)
        | AlgebraicEqualityCondition.Gt -> z.Ctx.MkGt(aa,bb)
        | AlgebraicEqualityCondition.Lt -> z.Ctx.MkLt(aa,bb)
        | AlgebraicEqualityCondition.Ge -> z.Ctx.MkGe(aa,bb)
        | AlgebraicEqualityCondition.Le -> z.Ctx.MkLe(aa,bb)
    | NamedPredicateCall(name, pars) ->
        // на всякий случай: если вдруг осталось — считаем как неинтерпретируемый булев предикат
        let args = pars |> Array.map (fun p -> z.Env[p])
        let sorts = args |> Array.map (fun e -> e.Sort)
        let f = z.Ctx.MkFuncDecl(name, sorts, z.Ctx.BoolSort :> Sort)
        z.Ctx.MkApp(f, args) :?> BoolExpr
    | NestedPredicate(_, p) ->
        // по условию вы уже инлайнили; если что-то осталось — просто раскрываем тело
        go p
    | NotImplementedExpression s ->
        failwithf "NotImplementedExpression in solver: %s" s
  go

let private sat (z:Z) (phi: BoolExpr) =
  use s = z.Ctx.MkSolver()
  s.Add phi
  match s.Check() with
  | Status.SATISFIABLE -> true
  | _ -> false

let private unsat z phi = not (sat z phi)

// ==================== П О И С К   К О Н Ф Л И К Т О В ====================

/// Главная функция: возвращает список конфликтующих пар
let findConflicts
    (permSemantics: PermissionSemantics)
    (model: Model)
    : Conflict list =

  // 1) нормализация
  let rules =
    model.DeonticStatements
    |> List.choose (normalize permSemantics)

  // 2) переменные
  let vars =
    collectVars rules

  // 3) Z3 окружение
  let z = mkZ vars
  let asBool = pred z

  // вспомогательный тест «контексты пересекаются?»
  let ctxMeet c1 c2 = sat z (z.Ctx.MkAnd(asBool c1, asBool c2))

  // тело — просто конъюнкция
  let both c1 b1 c2 b2 =
    z.Ctx.MkAnd(asBool c1, asBool c2, asBool b1, asBool b2)

  // определим, конфликтуют ли два нормализованных правила
  let isConflict (l:Normalized) (r:Normalized) =
    if not (ctxMeet l.Context r.Context) then
      None
    else
      // базовый случай: оба как обязательства (у Forbidden уже стоит ¬φ)
      let hardHard =
        unsat z (both l.Context l.Body r.Context r.Body)

      // опционально учитываем "сильное разрешение":
      // Permitted φ мы нормализовали как "φ" (но не отличили от Obligated),
      // поэтому допусловие: конфликтовать с обязательным ¬φ должен только Permitted vs Forbidden/Obligated¬φ.
      // Проще: если один из src.Modality = Permitted, другой = Forbidden|Obligated и формулы противны.
      let permVsObl =
        match l.Source.Modality, r.Source.Modality with
        | DeonticModality.Permitted, (DeonticModality.Forbidden | DeonticModality.Obligated)
        | (DeonticModality.Forbidden | DeonticModality.Obligated), DeonticModality.Permitted ->
            // тот же тест совместной неразрешимости
            unsat z (both l.Context l.Body r.Context r.Body)
        | _ -> false

      if hardHard || permVsObl then
        Some { LeftId = l.Id; RightId = r.Id; Reason = "Bodies incompatible on overlapping contexts" }
      else None

  // 4) попарная проверка
  [|
    for i in 0 .. rules.Length - 1 do
      for j in i+1 .. rules.Length - 1 do
        match isConflict rules[i] rules[j] with
        | Some c -> yield c
        | None -> ()
  |] |> Array.toList
