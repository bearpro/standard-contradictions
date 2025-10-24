namespace Mprokazin.DdlLtlf.Language.Ast

open Mprokazin.DdlLtlf.Language.Ast.Visitors
open Mprokazin.DdlLtlf.Language.Antlr

module Unparser =

    open Mprokazin.DdlLtlf.Language.Ast
    open System.Text

    // ----- helpers -----

    let joinWith (sep:string) (parts:string list) =
        parts |> List.filter (fun s -> s <> "") |> String.concat sep

    // -- Type pretty-print ----------------------------------------------------

    let rec printPrimitiveType = function
        | PrimitiveType.Int      -> "int"
        | PrimitiveType.Rational -> "rational"
        | PrimitiveType.Bool     -> "bool"

    let rec printTypeReference = function
        | TypeReference.Primitive p -> printPrimitiveType p
        | TypeReference.Named name  -> name

    let rec printTypeDescription (t: TypeDescription) : string =
        match t with
        | TypeDescription.Reference r ->
            printTypeReference r

        | TypeDescription.Product ptd ->
            printProductTypeDescription ptd

        | TypeDescription.Sum sumTd ->
            printSumTypeDescription sumTd

        | TypeDescription.Function ftd ->
            // нет в исходном языке, делаем читаемо для отладки
            $"({printSumTypeDescription ftd.Parameter} -> {printTypeDescription ftd.Value})"

    and printProductField (f: ProductTypeField) : string =
        match f.Type with
        | None      -> f.Name
        | Some tp   -> $"{f.Name}: {printTypeDescription tp}"

    and printProductTypeDescription (p: ProductTypeDescription) : string =
        // () если пусто
        let inner =
            p.Fields
            |> List.map printProductField
            |> String.concat ", "
        $"({inner})"

    and printSumTypeDescription (s: SumTypeDescription) : string =
        // (A | B | C)
        let inner =
            s.Variants
            |> List.map printTypeDescription
            |> String.concat " | "
        $"({inner})"

    // -- Values / expressions -------------------------------------------------

    let printValueReferenceValue = function
        | IntConstant i           -> string i
        | RationalConstant r      -> r
        | BooleanConstant b       -> if b then "true" else "false"
        | Name parts              -> String.concat "." parts

    let printValueReference (v: ValueReference) : string =
        let core = printValueReferenceValue v.Value
        match v.Type with
        | None -> core
        | Some t -> $"{core}: {printTypeDescription t}"

    let printAlgebraicOperation = function
        | AlgebraicOperation.Mul -> "*"
        | AlgebraicOperation.Div -> "/"
        | AlgebraicOperation.Mod -> "%"
        | AlgebraicOperation.Sum -> "+"
        | AlgebraicOperation.Sub -> "-"

    // We'll parenthesize sub-expressions in Operation to be safe
    let rec printAlgebraicExpression (e: AlgebraicExpression) : string =
        match e with
        | AlgebraicExpression.Value v ->
            printValueReference v
        | AlgebraicExpression.Operation (left, op, right, _range) ->
            let l = printAlgebraicExpression left
            let r = printAlgebraicExpression right
            $"({l} {printAlgebraicOperation op} {r})"

    let printAlgebraicEqualityCondition = function
        | AlgebraicEqualityCondition.Lt -> "<"
        | AlgebraicEqualityCondition.Le -> "<="
        | AlgebraicEqualityCondition.Eq -> "="
        | AlgebraicEqualityCondition.Ge -> ">="
        | AlgebraicEqualityCondition.Gt -> ">"
        | AlgebraicEqualityCondition.Ne -> "<>"

    let printAlgebraicCondition (c: AlgebraicCondition) : string =
        let l = printAlgebraicExpression c.LeftExpression
        let r = printAlgebraicExpression c.RightExpression
        let cond = printAlgebraicEqualityCondition c.Condition
        $"{l} {cond} {r}"

    // -- Predicate calls / params --------------------------------------------

    let printPredicateCall (pc: PredicateCall) : string =
        let args =
            pc.Arguments
            |> List.map printValueReference
            |> String.concat ", "
        $"{pc.PredicateName}({args})"

    // ProductTypeDescription is also used for predicate params in AST
    // so we reuse printProductTypeDescription.

    // -- Predicate body -------------------------------------------------------

    let rec printPredicateBodyItem = function
        | PredicateBodyItem.Call c ->
            printPredicateCall c
        | PredicateBodyItem.Value v ->
            printValueReference v
        | PredicateBodyItem.AlgebraicCondition ac ->
            printAlgebraicCondition ac

    // We wrap binary ops and definitions in parens for safety.
    and printPredicateBody (pb: PredicateBody) : string =
        match pb with
        | PredicateBody.Item (item, _r) ->
            printPredicateBodyItem item

        | PredicateBody.Not (inner, _r) ->
            // "not X"
            let innerPrinted = printPredicateBody inner
            $"not {innerPrinted}"

        | PredicateBody.And (l, r, _r) ->
            let lp = printPredicateBody l
            let rp = printPredicateBody r
            $"({lp} and {rp})"

        | PredicateBody.Or (l, r, _r) ->
            let lp = printPredicateBody l
            let rp = printPredicateBody r
            $"({lp} or {rp})"

        | PredicateBody.Definition (def, rest, _r) ->
            // "predicate ... = ... <rest>"
            // грамматика даёт это как `predicateDefinition predicateBody`
            let defTxt = printPredicateDefinition def
            let restTxt = printPredicateBody rest
            $"{defTxt} {restTxt}"

    // -- Predicate definition -------------------------------------------------

    and printPredicateDefinition (pd: PredicateDefinition) : string =
        // predicate <Name>(<params>) = <Body>
        // params come from pd.Parameter : ProductTypeDescription
        let paramTxt = printProductTypeDescription pd.Parameter
        let bodyTxt = printPredicateBody pd.Body
        $"predicate {pd.Name}{paramTxt} = {bodyTxt}"

    // -- Deontic statement ----------------------------------------------------

    let printDeonticModality = function
        | DeonticModality.Obligated -> "obligated"
        | DeonticModality.Forbidden -> "forbidden"
        | DeonticModality.Permitted -> "permitted"
        | DeonticModality.Suggeseted -> "suggested"

    let printDeonticStatement (ds: DeonticStatement) : string =
        // <modality> [Name '='] <Body> ['when' Condition]
        let modality = printDeonticModality ds.Modality
        let namePart =
            match ds.Name with
            | None -> ""
            | Some n -> $" {n} ="
        let bodyTxt = printPredicateBody ds.Body
        let whenTxt =
            match ds.Condition with
            | None -> ""
            | Some cond -> $" when {printPredicateBody cond}"

        $"{modality}{namePart} {bodyTxt}{whenTxt}"

    // -- Type definition ------------------------------------------------------

    let printTypeDefinition (td: TypeDefinition) : string =
        // type Name = <Body>
        $"type {td.Name} = {printTypeDescription td.Body}"

    // -- Top-level ------------------------------------------------------------

    let printDefinition (d: Definition) : string =
        match d with
        | Definition.Type tdef ->
            printTypeDefinition tdef
        | Definition.Predicate pdef ->
            printPredicateDefinition pdef
        | Definition.DeonticStatement ds ->
            printDeonticStatement ds
        | Definition.Fact () ->
            // фактов в исходной грамматике нет.
            "// <fact>"

    let printProgram (prog: Program) : string =
        prog
        |> List.map printDefinition
        |> String.concat "\n\n"

    let unparseAstNode (node: obj) : string =
        match node with
        // самые "крупные" узлы
        | :? Program as p ->
            printProgram p
        | :? Definition as d ->
            printDefinition d
        | :? PredicateDefinition as pd ->
            printPredicateDefinition pd
        | :? DeonticStatement as ds ->
            printDeonticStatement ds
        | :? TypeDefinition as td ->
            printTypeDefinition td

        // predicate internals
        | :? PredicateBody as pb ->
            printPredicateBody pb
        | :? PredicateBodyItem as pbi ->
            printPredicateBodyItem pbi
        | :? PredicateCall as pc ->
            printPredicateCall pc

        // algebra
        | :? AlgebraicCondition as ac ->
            printAlgebraicCondition ac
        | :? AlgebraicExpression as ae ->
            printAlgebraicExpression ae
        | :? AlgebraicEqualityCondition as aeq ->
            printAlgebraicEqualityCondition aeq
        | :? AlgebraicOperation as aop ->
            printAlgebraicOperation aop

        // values
        | :? ValueReference as vr ->
            printValueReference vr
        | :? ValueReferenceValue as vrv ->
            printValueReferenceValue vrv

        // types
        | :? TypeDescription as tdsc ->
            printTypeDescription tdsc
        | :? ProductTypeDescription as ptd ->
            printProductTypeDescription ptd
        | :? ProductTypeField as pf ->
            printProductField pf
        | :? SumTypeDescription as std ->
            printSumTypeDescription std
        | :? FuncTypeDescription as ftd ->
            // не в языке, но мы fallback'аемся тем же способом
            $"({printSumTypeDescription ftd.Parameter} -> {printTypeDescription ftd.Value})"
        | :? TypeReference as tr ->
            printTypeReference tr
        | :? PrimitiveType as pt ->
            printPrimitiveType pt

        // deontic enums etc.
        | :? DeonticModality as dm ->
            printDeonticModality dm

        // если ничего не подошло
        | other ->
            // отладочный fallback
            sprintf "%A" other