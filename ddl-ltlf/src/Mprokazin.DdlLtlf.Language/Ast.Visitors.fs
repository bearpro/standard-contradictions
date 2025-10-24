namespace Mprokazin.DdlLtlf.Language.Ast.Visitors

open Mprokazin.DdlLtlf.Language.Ast
open Mprokazin.DdlLtlf.Language.Antlr

type TopLevelDefinitionVisitor() =
    inherit DdlLtlfBaseVisitor<Definition>()

    override this.VisitTopLevelDefinition ctx: Definition = 
        let (|Type|_|) _ = ctx.typeDefinition() |> Option.ofObj
        let (|Predicate|_|) _ = ctx.predicateDefinition() |> Option.ofObj
        let (|DeonticStatement|_|) _ = ctx.deonticStatement() |> Option.ofObj

        match ctx with 
        | Type t -> 
            t 
            |> TypeDefinitionVisitor().Visit 
            |> Definition.Type
        | Predicate p -> 
            p 
            |> PredicateDefinitionVisitor().Visit 
            |> Definition.Predicate
        | DeonticStatement d -> 
            d
            |> DeonticStatementVisitor().Visit
            |>Definition.DeonticStatement 
        | _ -> failwith "Unexpected top level definition"


type ProgramVisitor() =
    inherit DdlLtlfBaseVisitor<Program>()

    override _.VisitRoot (ctx: DdlLtlfParser.RootContext): Program = 
        ctx.topLevelDefinition()
        |> Seq.map (TopLevelDefinitionVisitor().Visit)
        |> List.ofSeq
