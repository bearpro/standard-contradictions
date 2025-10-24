module Mprokazin.DdlLtlf.Language.Typing

open System
open Mprokazin.DdlLtlf.Language.Ast

// ---------- 1) ВНУТРЕННЕЕ ПРЕДСТАВЛЕНИЕ ТИПОВ (HM) ----------

type Ty =
  | TVar of int
  | TInt
  | TBool
  | TRational
  | TFun of Ty * Ty
  | TProduct of (string * Ty) list          // именованный product ≈ запись; без row-полиморфизма
  | TSum of Ty list
  | TCon of string * Ty list                // именованные параметризованные типы (Option α и т.п.)

type Scheme = Forall of Set<int> * Ty       // ∀vars. τ

type Subst = Map<int, Ty>
type VarEnv = Map<string, Scheme>           // значения/параметры/переменные
type PredEnv = Map<string, Scheme>          // имена предикатов -> схема (τ1 -> ... -> Bool)
type TypeEnv = Map<string, int>             // реестр объявленных имен типов (арность)

type Env =
  { Vars : VarEnv
    Preds: PredEnv
    Types: TypeEnv }

let emptyEnv = { Vars = Map.empty; Preds = Map.empty; Types = Map.empty }

// ---------- 2) УТИЛИТЫ: fresh, apply, ftv, compose ----------

module private G =
  let mutable seed = 0
  let fresh() = seed <- seed + 1; seed

let rec apply (s:Subst) (t:Ty) : Ty =
  match t with
  | TVar a when s.ContainsKey a -> apply s s.[a]
  | TVar _ | TInt | TBool | TRational -> t
  | TFun (a,b) -> TFun (apply s a, apply s b)
  | TProduct fields -> fields |> List.map (fun (n,ty) -> n, apply s ty) |> TProduct
  | TSum xs -> xs |> List.map (apply s) |> TSum
  | TCon (n,args) -> TCon (n, args |> List.map (apply s))

let applyScheme s (Forall (qs, t)) =
  // не трогаем квантифицированные переменные
  let s' = s |> Map.filter (fun k _ -> not (Set.contains k qs))
  Forall (qs, apply s' t)

let applyVarEnv s (ve:VarEnv) = ve |> Map.map (fun _ sc -> applyScheme s sc)
let applyPredEnv s (pe:PredEnv) = pe |> Map.map (fun _ sc -> applyScheme s sc)
let applyEnv s (e:Env) = { e with Vars = applyVarEnv s e.Vars; Preds = applyPredEnv s e.Preds }

let compose (s2:Subst) (s1:Subst) : Subst =
  // s2 ∘ s1 (сначала s1, потом s2)
  let s1' = s1 |> Map.map (fun _ t -> apply s2 t)
  s1' |> Map.fold (fun acc k v -> acc.Add(k, v)) s2

let rec ftv (t:Ty) : Set<int> =
  match t with
  | TVar a -> Set.singleton a
  | TInt | TBool | TRational -> Set.empty
  | TFun (a,b) -> Set.union (ftv a) (ftv b)
  | TProduct fields -> fields |> List.collect (snd >> ftv >> Set.toList) |> Set.ofList
  | TSum xs -> xs |> List.collect (ftv >> Set.toList) |> Set.ofList
  | TCon (_,args) -> args |> List.collect (ftv >> Set.toList) |> Set.ofList

let ftvScheme (Forall (qs, t)) = Set.difference (ftv t) qs
let ftvVarEnv (ve:VarEnv) = ve |> Seq.collect (fun kv -> ftvScheme kv.Value) |> Set.ofSeq

let generalize (ve:VarEnv) (t:Ty) =
  let qs = Set.difference (ftv t) (ftvVarEnv ve)
  Forall (qs, t)

let instantiate (Forall (qs, t)) =
  if Set.isEmpty qs then t else
  let sub =
    qs
    |> Seq.map (fun a -> a, TVar (G.fresh()))
    |> Map.ofSeq
  apply sub t

// ---------- 3) УНИФИКАЦИЯ (с occurs-check) ----------

exception TypeError of string * SourceRange

let rec occurs (a:int) (t:Ty) =
  match t with
  | TVar b -> a = b
  | TInt | TBool | TRational -> false
  | TFun (x,y) -> occurs a x || occurs a y
  | TProduct fields -> fields |> List.exists (snd >> occurs a)
  | TSum xs -> xs |> List.exists (occurs a)
  | TCon (_,args) -> args |> List.exists (occurs a)

let bind (a:int) (t:Ty) : Subst =
  if t = TVar a then Map.empty
  elif occurs a t then failwith $"occurs-check: α{a} in {t}"
  else Map.ofList [a, t]

let unify (rng:SourceRange) (t1:Ty) (t2:Ty) : Subst =
  let rec go t1 t2 =
    match t1, t2 with
    | a, b when a = b -> Map.empty
    | TVar a, t | t, TVar a -> bind a t
    | TFun (a1,b1), TFun (a2,b2) ->
        let s1 = go a1 a2
        let s2 = go (apply s1 b1) (apply s1 b2)
        compose s2 s1
    | TProduct f1, TProduct f2 ->
        if List.map fst f1 <> List.map fst f2 then
          let f1 = String.concat "," (List.map fst f1)
          let f2 = String.concat "," (List.map fst f2)
          raise (TypeError ($"record fields mismatch: {f1} vs {f2}", rng))
        (f1, f2)
        ||> List.zip
        |> List.fold (fun s ((_,x),(_,y)) ->
            let s' = go (apply s x) (apply s y)
            compose s' s) Map.empty
    | TSum xs, TSum ys ->
        if xs.Length <> ys.Length then raise (TypeError ("sum arity mismatch", rng))
        (xs, ys)
        ||> List.zip
        |> List.fold (fun s (x,y) ->
            let s' = go (apply s x) (apply s y)
            compose s' s) Map.empty
    | TCon (n1,a1), TCon (n2,a2) when n1 = n2 && a1.Length = a2.Length ->
        (a1, a2)
        ||> List.zip
        |> List.fold (fun s (x,y) ->
            let s' = go (apply s x) (apply s y)
            compose s' s) Map.empty
    | _ ->
        raise (TypeError ($"cannot unify {t1} with {t2}", rng))
  go t1 t2

// ---------- 4) ПРОЕКЦИЯ TypeDescription -> Ty (для аннотаций/объявлений) ----------

let primitiveToTy = function
  | PrimitiveType.Int -> TInt
  | PrimitiveType.Bool -> TBool
  | PrimitiveType.Rational -> TRational

let rec tyOfTypeDescription (types:TypeEnv) (td:TypeDescription) : Ty =
  match td with
  | Reference (Primitive p) -> primitiveToTy p
  | Reference (Named name)  ->
      match types.TryFind name with
      | Some k when k = 0 -> TCon (name, [])
      | Some k when k > 0 ->
          let args = List.init k (fun _ -> TVar (G.fresh()))
          TCon (name, args)
      | _ ->
          failwith $"Unknown type name '{name}'. Did you declare it?"
  | Product (fields, _) ->
      fields
      |> List.map (fun f ->
          let t =
            match f.Type with
            | Some tdesc -> tyOfTypeDescription types tdesc
            | None -> TVar (G.fresh())
          f.Name, t)
      |> TProduct
  | TypeDescription.Sum (alts, _) ->
      alts |> List.map (tyOfTypeDescription types) |> TSum

// ---------- 5) ЧИСЛОВЫЕ ОПЕРАЦИИ: простая проверка "должен быть Int или Rational" ----------

type NumericConstraint = Ty * SourceRange  // "этот тип должен стать Int или Rational"
type ConstraintSet = NumericConstraint list

let requireNumeric (rng:SourceRange) (t:Ty) (cs:ConstraintSet) : ConstraintSet =
  (t, rng) :: cs

let checkNumericConstraints (cs:ConstraintSet) (tfinalize:Ty->Ty) =
  // после унификации смотрим, кем стали переменные; допускаем только Int|Rational
  for (t, rng) in cs do
    match tfinalize t with
    | TInt | TRational -> ()
    | TVar _ -> raise (TypeError ("ambiguous numeric type; annotate with Int or Rational", rng))
    | other -> raise (TypeError ($"numeric operation requires Int or Rational, got {other}", rng))

// ---------- 6) ВЫВОД ТИПОВ ДЛЯ ВЫРАЖЕНИЙ: AlgebraicExpression, PredicateBody ----------

type InferState =
  { Env : Env
    Sub : Subst
    NumC: ConstraintSet }

let withSub s st = { st with Env = applyEnv s st.Env; Sub = compose s st.Sub }

let freshT() = TVar (G.fresh())

let inferLiteral (v:ValueReferenceValue) : Ty =
  match v with
  | IntConstant _ -> TInt
  | BooleanConstant _ -> TBool
  | RationalConstant _ -> TRational
  | Name _ -> freshT()   // имя — это переменная (ниже мы попробуем прочитать её из окружения)

let tryLookupVar (st:InferState) (namePath:string list) : Ty option =
  match namePath with
  | [single] ->
      match st.Env.Vars.TryFind single with
      | Some sc -> Some (instantiate sc)
      | None -> None
  | _ ->
      // TODO: поддержка object.field1.field2 через row-polymorphism.
      None

let inferValueReference (st:InferState) (vr:ValueReference) : InferState * Ty =
  // 1) базовый тип по литералу/имени
  let baseTy =
    match vr.Value with
    | Name path ->
        match tryLookupVar st path with
        | Some t -> t
        | None ->
            match vr.Type with
            | Some ann -> tyOfTypeDescription st.Env.Types ann
            | None -> 
                let name = String.concat "." path
                raise (TypeError ($"unbound variable: {name}", vr.Range))
    | _ -> inferLiteral vr.Value
  // 2) если есть аннотация у Value — унифицируем
  let st =
    match vr.Type with
    | None -> st
    | Some ann ->
        let annTy = tyOfTypeDescription st.Env.Types ann
        let s = unify vr.Range baseTy annTy
        withSub s st
  st, apply st.Sub baseTy

let rec inferAExpr (st:InferState) (e:AlgebraicExpression) : InferState * Ty =
  match e with
  | AlgebraicExpression.Value vr ->
      inferValueReference st vr
  | AlgebraicExpression.Operation (l, op, r, rng) ->
      let (st1, tl) = inferAExpr st l
      let (st2, tr) = inferAExpr st1 r

      match op with
      | AlgebraicOperation.Mod ->
          // x % y : Int, с жёсткой унификацией
          let s1 = unify rng tl TInt
          let st' = withSub s1 st2
          let s2 = unify rng (apply st'.Sub tr) TInt
          let st'' = withSub s2 st'
          st'', TInt
      | AlgebraicOperation.Div ->
          // / : Num t -> Num t -> Rational  (или сделай "тот же тип", если хочешь)
          let sSame = unify rng tl tr
          let st' = withSub sSame st2
          let st'' = { st' with NumC = requireNumeric rng (apply st'.Sub tl) st'.NumC }
          st'', TRational
      | AlgebraicOperation.Sum
      | AlgebraicOperation.Sub
      | AlgebraicOperation.Mul ->
          // + - * : Num t -> Num t -> Num t  (оба операнда одного типа, результат того же)
          let sSame = unify rng tl tr
          let st' = withSub sSame st2
          let tres = apply st'.Sub tl
          let st'' = { st' with NumC = requireNumeric rng tres st'.NumC }
          st'', tres

let rec inferPredicateBody (st:InferState) (pb:PredicateBody) : InferState * Ty =
  match pb with
  | PredicateBody.Item (item, rng) ->
      match item with
      | PredicateBodyItem.Value vr ->
          let (st1, t) = inferValueReference st vr
          let s = unify vr.Range t TBool
          withSub s st1, TBool
      | PredicateBodyItem.AlgebraicCondition cond ->
          let (st1, tl) = inferAExpr st cond.LeftExpression
          let (st2, tr) = inferAExpr st1 cond.RightExpression
          // сравнения требуют совместимости типов, и сами дают Bool
          let s = unify cond.Range tl tr
          let st3 = withSub s st2
          // для Eq/Ne можно разрешать Bool, но оставим просто унификацию сторон
          st3, TBool
      | PredicateBodyItem.Call call ->
          // предикат — функция τ1 -> ... -> Bool
          match st.Env.Preds.TryFind call.PredicateName with
          | None -> raise (TypeError ($"unknown predicate {call.PredicateName}", call.Range))
          | Some sc ->
              let tPred = instantiate sc
              // свернём вызов как аппликации
              let rec applyArgs (st:InferState) (tFun:Ty) (args:ValueReference list) =
                match args with
                | [] ->
                    // финальный тип должен быть Bool
                    let s = unify call.Range tFun TBool
                    withSub s st, TBool
                | a::rest ->
                    let (stA, ta) = inferValueReference st a
                    let b = freshT()
                    let s = unify a.Range tFun (TFun(ta, b))
                    let st' = withSub s stA
                    applyArgs st' (apply st'.Sub b) rest
              applyArgs st tPred call.Arguments

  | PredicateBody.Not (inner, rng) ->
      let (st1, t) = inferPredicateBody st inner
      let s = unify rng t TBool
      withSub s st1, TBool

  | PredicateBody.And (l, r, rng)
  | PredicateBody.Or  (l, r, rng) ->
      let (st1, tl) = inferPredicateBody st l
      let s1 = unify rng tl TBool
      let st1 = withSub s1 st1
      let (st2, tr) = inferPredicateBody st1 r
      let s2 = unify rng tr TBool
      withSub s2 st2, TBool

  | PredicateBody.Definition (pd, body, _rng) ->
      // локальное определение предиката доступно в body
      // 1) полноценно выводим тип локального предиката (с проверкой его тела)
      let (stPd, sc) = inferPredicateDefinition st pd
      // 2) добавляем его схему в Preds для последующей части тела
      let stWithPred =
        { stPd with Env = { stPd.Env with Preds = stPd.Env.Preds.Add(pd.Name, sc) } }
      // 3) типизируем остаток body в расширенном окружении
      inferPredicateBody stWithPred body

// ---------- 7) ВЫВОД ТИПА ПРЕДИКАТА (сигнатура и тело) ----------

and inferPredicateDefinitionHeader (st:InferState) (pd:PredicateDefinition) : Scheme * (string * Ty) list =
  let paramTypes =
    pd.Parameters
    |> List.map (fun p ->
        let tp =
          match p.Type with
          | Some ann -> tyOfTypeDescription st.Env.Types ann
          | None -> TVar (G.fresh())
        (p.Name, tp))

  let funTy =
    List.foldBack (fun (_n, tp) acc -> TFun (tp, acc)) paramTypes TBool

  // схема полиморфна по тем TVar, что не связаны именами (по глобальному VarEnv)
  let varsEnv = st.Env.Vars
  let sc = generalize varsEnv funTy
  sc, paramTypes

// и сам вывод предиката — с «push/pop» параметров:
and inferPredicateDefinition (st:InferState) (pd:PredicateDefinition) : InferState * Scheme =
  let (scHeader, paramTypes) = inferPredicateDefinitionHeader st pd

  // push параметров как МОНОтипов
  let savedVars = st.Env.Vars
  let varsPushed =
    (savedVars, paramTypes) ||> List.fold (fun acc (name, tp) -> acc.Add(name, Forall (Set.empty, tp)))
  let st1 = { st with Env = { st.Env with Vars = varsPushed } }

  // проверяем тело : Bool
  let (st2, tBody) = inferPredicateBody st1 pd.Body
  let sBool = unify pd.Range tBody TBool
  let st3 = withSub sBool st2

  // pop: возвращаем исходный Vars (НО под действием найденной подстановки)
  let st3 =
    let popped = applyVarEnv st3.Sub savedVars
    { st3 with Env = { st3.Env with Vars = popped } }

  // финальная схема предиката: берём заголовок, инстанцируем и применяем найденную подстановку,
  // затем делаем generalize по ГЛОБАЛЬНОМУ (уже возвращённому) окружению.
  //let funTy' = scHeader |> instantiate |> apply st3.Sub
  let funTy' =
    paramTypes
    |> List.map (snd >> apply st3.Sub)
    |> (fun t -> List.foldBack (fun tp acc -> TFun (tp, acc)) t TBool)

  let scFinal = generalize st3.Env.Vars funTy'

  st3, scFinal

// ---------- 8) ДЕОНТИЧЕСКИЕ ВЫСКАЗЫВАНИЯ ----------

let inferDeontic (st:InferState) (ds:DeonticStatement) : InferState =
  // Body : Bool, Condition? : Bool
  let (st1, tb) = inferPredicateBody st ds.Body
  let s1 = unify ds.Range tb TBool
  let st1 = withSub s1 st1
  let st1 =
    match ds.Condition with
    | None -> st1
    | Some cond ->
        let (stc, tc) = inferPredicateBody st1 cond
        let s = unify ds.Range tc TBool
        withSub s stc
  st1

// ---------- 9) ВЕРХНЕУРОВНЕВЫЕ ОБЪЯВЛЕНИЯ ТИПОВ И ПРОГРАММЫ ----------


let rec collectNamedRefs (td:TypeDescription) (acc:Set<string>) : Set<string> =
  match td with
  | Reference (Named n) -> acc.Add n
  | Reference (Primitive _) -> acc
  | Product (fields, _) ->
      (acc, fields)
      ||> List.fold (fun a f ->
            match f.Type with
            | Some t -> collectNamedRefs t a
            | None -> a)
  | TypeDescription.Sum (xs, _) ->
      (acc, xs) ||> List.fold (fun a t -> collectNamedRefs t a)

  //// у тебя нет явной параметризации именованных типов в AST, поэтому считаем арность = 0
  //// Если добавишь параметризацию (например, TypeDefinition with Parameters), тут заполнишь арность
  //types.Add(td.Name, 0)
let registerTypeDefinition (types:TypeEnv) (td:TypeDefinition) : TypeEnv =
  // регистрируем само имя типа
  let types = types.Add(td.Name, 0)
  // и все Named, встреченные внутри тела, как арность 0
  let alts = collectNamedRefs td.Body Set.empty
  (types, alts) ||> Set.fold (fun m n -> if m.ContainsKey n then m else m.Add(n, 0))

let inferProgram (prog:Program) : Env =
  // 1) сначала регистрируем имена типов (арность), чтобы аннотации могли ссылаться
  let types =
    (Map.empty, prog)
    ||> List.fold (fun acc def ->
      match def with
      | Definition.Type td -> registerTypeDefinition acc td
      | _ -> acc)

  // 2) построим начальное окружение (включая примитивные предикаты/операции, если они есть)
  let baseEnv =
    { emptyEnv with
        Types = types
        Vars  = Map.empty
        Preds = Map.empty }

  // 3) проходим определения: предикаты добавляем по одному, проверяя тело; Deontic — проверяем Bool
  let st0 = { Env = baseEnv; Sub = Map.empty; NumC = [] }

  let stFinal =
    (st0, prog)
    ||> List.fold (fun st def ->
      match def with
      | Definition.Type _ -> st // типы уже зарегистрировали
      | Definition.Fact _ -> st // факты типами тут не заведуем
      | Definition.Predicate pd ->
          let (st', sc) = inferPredicateDefinition st pd
          let preds' = st'.Env.Preds.Add(pd.Name, sc)
          { st' with Env = { st'.Env with Preds = preds' } }
      | Definition.DeonticStatement ds ->
          inferDeontic st ds)

  // 4) проверка числовых ограничений
  let tfinal t = apply stFinal.Sub t
  checkNumericConstraints stFinal.NumC tfinal

  stFinal.Env
