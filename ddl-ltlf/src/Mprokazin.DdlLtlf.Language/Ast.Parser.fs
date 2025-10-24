namespace Mprokazin.DdlLtlf.Language.Ast

open Mprokazin.DdlLtlf.Language.Ast.Visitors
open Mprokazin.DdlLtlf.Language.Antlr

module Parser =
    let visit (tree: DdlLtlfParser.RootContext) = ProgramVisitor().Visit tree

    let parse (input: string) = 
        let stream = Antlr4.Runtime.AntlrInputStream input
        let lexer = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfLexer(stream)
        let tokens = Antlr4.Runtime.CommonTokenStream(lexer)
        let parser = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfParser(tokens)
    
        let tree = parser.root()
    
        let result = Visitors.ProgramVisitor().Visit tree
        result
