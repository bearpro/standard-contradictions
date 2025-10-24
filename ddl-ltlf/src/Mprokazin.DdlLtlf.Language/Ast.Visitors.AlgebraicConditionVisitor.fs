namespace Mprokazin.DdlLtlf.Language.Ast.Visitors

open Mprokazin.DdlLtlf.Language.Ast
open Mprokazin.DdlLtlf.Language.Ast.Visitors.Helpers
open Mprokazin.DdlLtlf.Language.Antlr

type AlgebraicConditionVisitor() =
    inherit DdlLtlfBaseVisitor<AlgebraicCondition>()

    let operationOf (ctx: DdlLtlfParser.AlgebraicOperationContext) =
        match ctx with
        | :? DdlLtlfParser.MulContext   -> Mul
        | :? DdlLtlfParser.DivContext   -> Div
        | :? DdlLtlfParser.ModContext   -> Mod
        | :? DdlLtlfParser.AddContext   -> Sum
        | :? DdlLtlfParser.SubContext   -> Sub
        | _ -> failwith "Unexpected algebraic operation"

    let rec algebraicExpressionOf (ctx: DdlLtlfParser.AlgebraicExpressionContext) =
        match ctx with
        | :? DdlLtlfParser.AlgebraicParensContext as ctx -> 
            ctx.algebraicExpression() 
            |> algebraicExpressionOf 
        | :? DdlLtlfParser.AlgebraicValueContext as ctx -> 
            ctx.expression()
            |> ExpressionVisitor().Visit
            |> AlgebraicExpression.Expression
        | :? DdlLtlfParser.OperationContext as ctx -> 
            let range = rangeOfCtx ctx
            let op = ctx.algebraicOperation() |> operationOf
            let (left, right) = 
                match ctx.algebraicExpression() with
                | [| left; right |] -> 
                    let left = algebraicExpressionOf left
                    let right = algebraicExpressionOf right
                    (left, right)
                | _ -> failwith "Unexpected amount of operands in algebraic expression"
            AlgebraicExpression.Operation (left, op, right, range)
        | _ -> failwith "Unexpected algebraic expression"


    let conditionOf (ctx: DdlLtlfParser.AlgebraicComparationContext) =
        match ctx with
        | :? DdlLtlfParser.LtContext -> Lt
        | :? DdlLtlfParser.LeContext -> Le
        | :? DdlLtlfParser.EqContext -> Eq
        | :? DdlLtlfParser.GeContext -> Ge
        | :? DdlLtlfParser.GtContext -> Gt
        | :? DdlLtlfParser.NeContext -> Ne
        | _ -> failwith "Unexpected algebraic condition"

    override _.VisitAlgebraicCondition (ctx: DdlLtlfParser.AlgebraicConditionContext): AlgebraicCondition = 
        let range = rangeOfCtx ctx
        let condition = ctx.algebraicComparation() |> conditionOf
        let (left, right) =
            match ctx.algebraicExpression() with
            | [| left; right |] -> 
                let left = algebraicExpressionOf left
                let right = algebraicExpressionOf right
                (left, right)
            | _ -> failwith "Unexpected amount of expressions in algebraic condition"
        { Condition = condition; LeftExpression = left; RightExpression = right; Range = range}

