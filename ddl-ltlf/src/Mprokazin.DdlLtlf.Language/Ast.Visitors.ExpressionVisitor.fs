namespace Mprokazin.DdlLtlf.Language.Ast.Visitors

open Mprokazin.DdlLtlf.Language.Ast
open Mprokazin.DdlLtlf.Language.Ast.Visitors.Helpers
open Mprokazin.DdlLtlf.Language.Antlr

type ExpressionVisitor() =
    inherit DdlLtlfBaseVisitor<Expression>()

    let constantValueOf (ctx: DdlLtlfParser.ConstantContext) =
        let (|Rational|_|) _ = ctx.RATIONAL() |> Option.ofObj
        let (|Int|_|) _ = ctx.INT() |> Option.ofObj
        let (|Bool|_|) _ = ctx.BOOL() |> Option.ofObj

        match ctx with
        | Rational x -> x.GetText() |> ConstantValue.RationalConstant
        | Int x -> x.GetText() |> int |> ConstantValue.IntConstant
        | Bool x -> x.GetText() |> bool.Parse |> ConstantValue.BooleanConstant
        | _ -> failwith "Unexpected constant value"

    let nameSegmentsOf (ctx: DdlLtlfParser.NameReferenceContext) =
        ctx.NAME()
        |> Seq.map (fun token -> token.GetText())
        |> List.ofSeq

    let rec expressionPrimaryOf (visitor: ExpressionVisitor) (ctx: DdlLtlfParser.ExpressionPrimaryContext) : Expression =
        let make kind range =
            { Kind = kind
              Annotation = None
              Range = range }

        match ctx with
        | :? DdlLtlfParser.ConstructorExpressionContext as ctorCtx ->
            let invocation = ctorCtx.constructorInvocation()
            let arguments : Expression list =
                invocation.expression()
                |> Seq.map visitor.Visit
                |> List.ofSeq
            let ctor : ConstructorCall =
                { Constructor = invocation.NAME().GetText()
                  Arguments = arguments
                  Range = rangeOfCtx invocation }
            make (ExpressionKind.Constructor ctor) (rangeOfCtx ctorCtx)
        | :? DdlLtlfParser.TupleExpressionContext as tupleCtx ->
            let tupleLiteral = tupleCtx.tupleLiteral()
            let items : Expression list =
                tupleLiteral.expression()
                |> Seq.map visitor.Visit
                |> List.ofSeq
            make (ExpressionKind.Tuple items) (rangeOfCtx tupleLiteral)
        | :? DdlLtlfParser.ConstantExpressionContext as constCtx ->
            constCtx.constant()
            |> constantValueOf
            |> ExpressionKind.Constant
            |> fun kind -> make kind (rangeOfCtx constCtx)
        | :? DdlLtlfParser.NameExpressionContext as nameCtx ->
            nameCtx.nameReference()
            |> nameSegmentsOf
            |> ExpressionKind.Name
            |> fun kind -> make kind (rangeOfCtx nameCtx)
        | :? DdlLtlfParser.ParenthesizedExpressionContext as parenCtx ->
            let inner = parenCtx.expression() |> visitor.Visit
            { inner with Range = rangeOfCtx parenCtx }
        | _ -> failwith "Unexpected form of expression"

    override this.VisitAnnotatedExpression (ctx: DdlLtlfParser.AnnotatedExpressionContext) : Expression =
        let primary = ctx.expressionPrimary() |> expressionPrimaryOf this
        let annotation =
            ctx.typeDescription()
            |> TypeDescritptionVisitor().Visit
        { primary with Annotation = Some annotation; Range = rangeOfCtx ctx }

    override this.VisitPlainExpression (ctx: DdlLtlfParser.PlainExpressionContext) : Expression =
        ctx.expressionPrimary()
        |> expressionPrimaryOf this
        |> fun expr -> { expr with Range = rangeOfCtx ctx }

type PatternVisitor() =
    inherit DdlLtlfBaseVisitor<Pattern>()

    let constantValueOf (ctx: DdlLtlfParser.ConstantContext) =
        let (|Rational|_|) _ = ctx.RATIONAL() |> Option.ofObj
        let (|Int|_|) _ = ctx.INT() |> Option.ofObj
        let (|Bool|_|) _ = ctx.BOOL() |> Option.ofObj

        match ctx with
        | Rational x -> x.GetText() |> ConstantValue.RationalConstant
        | Int x -> x.GetText() |> int |> ConstantValue.IntConstant
        | Bool x -> x.GetText() |> bool.Parse |> ConstantValue.BooleanConstant
        | _ -> failwith "Unexpected constant value"

    override this.VisitConstructorPatternValue (ctx: DdlLtlfParser.ConstructorPatternValueContext) : Pattern =
        let ctor = ctx.constructorPattern()
        let args =
            ctor.pattern()
            |> Seq.map this.Visit
            |> List.ofSeq
        let constructor =
            { Constructor = ctor.NAME().GetText()
              Arguments = args
              Range = rangeOfCtx ctor }
        Pattern.Constructor constructor

    override this.VisitTuplePatternValue (ctx: DdlLtlfParser.TuplePatternValueContext) : Pattern =
        let tuple = ctx.tuplePattern()
        let items =
            tuple.pattern()
            |> Seq.map this.Visit
            |> List.ofSeq
        Pattern.Tuple (items, rangeOfCtx tuple)

    override this.VisitConstantPatternValue (ctx: DdlLtlfParser.ConstantPatternValueContext) : Pattern =
        ctx.constant()
        |> constantValueOf
        |> fun value -> Pattern.Constant (value, rangeOfCtx ctx)

    override this.VisitWildcardPatternValue (ctx: DdlLtlfParser.WildcardPatternValueContext) : Pattern =
        Pattern.Wildcard (rangeOfCtx ctx)
