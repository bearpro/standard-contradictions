module internal Mprokazin.DdlLtlf.Language.Ast.Visitors.Helpers

open Antlr4.Runtime
open Mprokazin.DdlLtlf.Language.Ast

let rangeOfCtx (ctx: ParserRuleContext) : SourceRange =
    { StartLine = ctx.Start.Line
      StartChar = ctx.Start.Column
      EndLine   = ctx.Stop.Line
      EndChar   = ctx.Stop.Column }

