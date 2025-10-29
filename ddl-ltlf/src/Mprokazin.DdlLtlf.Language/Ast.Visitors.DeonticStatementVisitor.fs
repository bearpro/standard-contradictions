namespace Mprokazin.DdlLtlf.Language.Ast.Visitors

open Mprokazin.DdlLtlf.Language.Ast
open Mprokazin.DdlLtlf.Language.Ast.Visitors.Helpers
open Mprokazin.DdlLtlf.Language.Antlr

type DeonticStatementVisitor() =
    inherit DdlLtlfBaseVisitor<DeonticStatement>()

    let modalityOf (ctx: DdlLtlfParser.DeonticModalityContext) =
        match ctx with
        | :? DdlLtlfParser.ObligatedContext -> Obligated
        | :? DdlLtlfParser.PermittedContext -> Permitted
        | :? DdlLtlfParser.ForbiddenContext -> Forbidden
        | :? DdlLtlfParser.SuggestedContext -> Suggeseted
        | _ -> failwith "Unexpected deoontic modality"

    override _.VisitDeonticStatement (ctx: DdlLtlfParser.DeonticStatementContext): DeonticStatement = 
        let modality = ctx.deonticModality() |> modalityOf
        let name = 
            ctx.NAME()
            |> Option.ofObj
            |> Option.map(_.GetText())
        let body = ctx.predicateBody(0) |> PredicateBodyVisitor().Visit
        let range = rangeOfCtx ctx
        let condition = 
            ctx.predicateBody(1) 
            |> Option.ofObj
            |> Option.map (PredicateBodyVisitor().Visit)


        { Modality = modality
          Name = name
          Body = body
          Condition = condition
          Range = range
          Parameters = []
          InferredType = None }
