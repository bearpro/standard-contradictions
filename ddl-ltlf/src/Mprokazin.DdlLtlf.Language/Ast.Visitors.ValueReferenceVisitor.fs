namespace Mprokazin.DdlLtlf.Language.Ast.Visitors

open Mprokazin.DdlLtlf.Language.Ast
open Mprokazin.DdlLtlf.Language.Ast.Visitors.Helpers
open Mprokazin.DdlLtlf.Language.Antlr

type ValueReferenceVisitor() =
    inherit DdlLtlfBaseVisitor<ValueReference>()

    let constantValueOf (ctx: DdlLtlfParser.ConstantContext) =
        let (|Rational|_|) _ = ctx.RATIONAL() |> Option.ofObj
        let (|Int|_|) _ = ctx.INT() |> Option.ofObj
        let (|Bool|_|) _ = ctx.BOOL() |> Option.ofObj

        match ctx with
        | Rational x -> x.GetText() |> RationalConstant
        | Int x -> x.GetText() |> int |> IntConstant
        | Bool x -> x.GetText() |> bool.Parse |> BooleanConstant
        | _ -> failwith "Unexpected contant value"

    let valueReferenceValueOf (ctx: DdlLtlfParser.ValueReferenceContext) =
        let (|Constant|_|) _ = ctx.constant() |> Option.ofObj
        let (|NameReference|_|) _ = ctx.nameReference() |> Option.ofObj

        match ctx with
        | Constant x -> constantValueOf x
        | NameReference x -> 
            x.NAME() 
            |> Seq.map (_.GetText())
            |> Seq.toList 
            |> ValueReferenceValue.Name
        | _ -> failwith "Unexpected form of value reference"

    let valueReferenceOf (ctx: DdlLtlfParser.ValueReferenceContext): ValueReference =
        let range = rangeOfCtx ctx
        let value = valueReferenceValueOf ctx
        let t = 
            ctx.typeDescription() 
            |> Option.ofObj
            |> Option.map (TypeDescritptionVisitor().Visit)

        { Value = value; Type = t; Range = range }

    override _.VisitValueReference (ctx: DdlLtlfParser.ValueReferenceContext): ValueReference = 
        valueReferenceOf ctx