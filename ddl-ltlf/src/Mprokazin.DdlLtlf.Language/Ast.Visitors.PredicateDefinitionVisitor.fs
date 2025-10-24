namespace Mprokazin.DdlLtlf.Language.Ast.Visitors

open Mprokazin.DdlLtlf.Language.Ast
open Mprokazin.DdlLtlf.Language.Ast.Visitors.Helpers
open Mprokazin.DdlLtlf.Language.Antlr

type PredicateBodyVisitor() =
    inherit DdlLtlfBaseVisitor<PredicateBody>()

    let predicateReferenceOf (ctx : DdlLtlfParser.PredicateReferenceContext) : PredicateCall =
        let name = ctx.NAME().GetText()
        let range = rangeOfCtx ctx
        let arguments = 
            ctx.predicateReferenceArguments().valueReference()
            |> Seq.map (ValueReferenceVisitor().Visit)
            |> List.ofSeq
        { PredicateName = name; Range = range; Arguments = arguments }

    override this.VisitPredicateParens ctx = ctx.predicateBody() |> this.Visit

    override this.VisitNot ctx = 
        let body = ctx.predicateBody() |> this.Visit 
        let range = rangeOfCtx ctx
        PredicateBody.Not(body, range)

    override this.VisitOr ctx = 
        match ctx.predicateBody() with
        | [| a; b |] -> 
            let range = rangeOfCtx ctx
            let a = this.Visit a
            let b = this.Visit b
            PredicateBody.Or(a, b, range)
        | _ -> failwith "Unexpected count of predicates int NOT. Grammar probably changed."

    override this.VisitAnd ctx = 
        match ctx.predicateBody() with
        | [| a; b |] -> 
            let range = rangeOfCtx ctx
            let a = this.Visit a
            let b = this.Visit b
            PredicateBody.Or(a, b, range)
        | _ -> failwith "Unexpected count of predicates int NOT. Grammar probably changed."

    override _.VisitReference (ctx: DdlLtlfParser.ReferenceContext): PredicateBody = 
        let call = ctx.predicateReference() |> predicateReferenceOf |> PredicateBodyItem.Call
        let range = rangeOfCtx ctx
        PredicateBody.Item (call, range)

    override _.VisitValue (ctx: DdlLtlfParser.ValueContext): PredicateBody = 
        let value = ctx.valueReference() |> ValueReferenceVisitor().Visit |> PredicateBodyItem.Value
        let range = rangeOfCtx ctx
        PredicateBody.Item (value, range)

    override _.VisitAlgebraic (ctx: DdlLtlfParser.AlgebraicContext): PredicateBody = 
        let range = rangeOfCtx ctx
        let value = 
            ctx.algebraicCondition() 
            |> AlgebraicConditionVisitor().Visit 
            |> PredicateBodyItem.AlgebraicCondition

        PredicateBody.Item (value, range)

    override this.VisitWithNested (ctx: DdlLtlfParser.WithNestedContext): PredicateBody = 
        let defined = ctx.predicateDefinition() |> PredicateDefinitionVisitor().Visit
        let main = ctx.predicateBody() |> this.Visit
        let range = rangeOfCtx ctx

        PredicateBody.Definition (defined, main, range)

and PredicateDefinitionVisitor() =
    inherit DdlLtlfBaseVisitor<PredicateDefinition>()

    let predicateParameterOf (ctx: DdlLtlfParser.PredicateParameterContext) = 
        let parameterType = 
            ctx.typeDescription() 
            |> Option.ofObj
            |> Option.map (TypeDescritptionVisitor().Visit)
        
        let name = ctx.NAME().GetText()
        let range = rangeOfCtx ctx

        { Name = name; Type = parameterType; Range = range }
        

    let predicateParametersOf (ctx: DdlLtlfParser.PredicateParametersContext) : Parameter list =
        ctx.predicateParameter()
        |> Seq.map predicateParameterOf
        |> List.ofSeq

    let predicateBodyOf ctx : PredicateBody = 
        PredicateBodyVisitor().Visit ctx

    override _.VisitPredicateDefinition (ctx: DdlLtlfParser.PredicateDefinitionContext): PredicateDefinition = 
        let name = ctx.NAME().GetText()
        let parameters = ctx.predicateParameters()
        let body = ctx.predicateBody()
        let range = rangeOfCtx ctx

        { Name = name
          Parameters = predicateParametersOf parameters
          Body = predicateBodyOf ctx
          Range = range }