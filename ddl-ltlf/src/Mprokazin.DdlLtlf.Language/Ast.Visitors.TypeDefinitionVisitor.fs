namespace Mprokazin.DdlLtlf.Language.Ast.Visitors

open Mprokazin.DdlLtlf.Language.Ast
open Mprokazin.DdlLtlf.Language.Ast.Visitors.Helpers
open Mprokazin.DdlLtlf.Language.Antlr

type TypeDefinitionVisitor() =
    inherit DdlLtlfBaseVisitor<TypeDefinition>()

    override _.VisitTypeDefinition (ctx: DdlLtlfParser.TypeDefinitionContext) : TypeDefinition =
        let name = ctx.NAME().GetText()
        let body = ctx.typeDescription() |> TypeDescritptionVisitor().Visit
        let range = rangeOfCtx ctx
        { Name = name; Body = body; Range = range }