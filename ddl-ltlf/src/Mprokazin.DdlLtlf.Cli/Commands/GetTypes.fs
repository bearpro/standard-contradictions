module Mprokazin.DdlLtlf.Cli.Commands.GetTypes

open System.IO
open Mprokazin.DdlLtlf.Cli.Parameters
open Mprokazin.DdlLtlf.Language.Typing

type GetTypesInput = { 
    Source: string
    Reader: TextReader
}

let createInputs (paramters: GetTypesParameters) = 
    match paramters with
    | { Input = Files list } -> 
        list
        |> List.where (fun path -> 
            if File.Exists path 
            then true
            else failwithf "File does not exists: %s" path)
        |> List.map (fun path -> 

            { Source = path
              Reader = new StreamReader(path) })
    | { Input = StdIn } -> [
        { Source = "STDIN" 
          Reader = System.Console.In }
    ]

let processInput (input: GetTypesInput) = 
    try
        let stream = Antlr4.Runtime.AntlrInputStream input.Reader
        let lexer = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfLexer(stream)
        let tokens = Antlr4.Runtime.CommonTokenStream(lexer)
        let parser = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfParser(tokens)
            
        let tree = parser.root()
        let ast = Mprokazin.DdlLtlf.Language.Ast.Parser.visit tree
        let types = Mprokazin.DdlLtlf.Language.Typing.inferTypes ast

        Ok (input.Source, types)
    with
        | e -> Error (input.Source, e)

let printModel (model: Mprokazin.DdlLtlf.Language.Typing.ProgramObjTypeInfo list) format =
    match format with
    | FSharp -> 
        model
        |> List.map (_.Type)
        |> printfn "%A" 
    | Shell ->
        model
        |> List.map (fun x -> 
            let source = 
                match (x.Object: Mprokazin.DdlLtlf.Language.Typing.TypedObject) with
                | TypeDefinition      x -> Mprokazin.DdlLtlf.Language.Ast.Unparser.unparseAstNode x
                | TypeDescription     x -> Mprokazin.DdlLtlf.Language.Ast.Unparser.unparseAstNode x
                | ProductTypeField    x -> Mprokazin.DdlLtlf.Language.Ast.Unparser.unparseAstNode x
                | Expression          x -> Mprokazin.DdlLtlf.Language.Ast.Unparser.unparseAstNode x
                | PredicateCall       x -> Mprokazin.DdlLtlf.Language.Ast.Unparser.unparseAstNode x
                | AlgebraicExpression x -> Mprokazin.DdlLtlf.Language.Ast.Unparser.unparseAstNode x
                | Parameter           x -> Mprokazin.DdlLtlf.Language.Ast.Unparser.unparseAstNode x
                | AlgebraicCondition  x -> Mprokazin.DdlLtlf.Language.Ast.Unparser.unparseAstNode x
                | PredicateBodyItem   x -> Mprokazin.DdlLtlf.Language.Ast.Unparser.unparseAstNode x
                | PredicateBody       x -> Mprokazin.DdlLtlf.Language.Ast.Unparser.unparseAstNode x
                | PredicateDefinition x -> Mprokazin.DdlLtlf.Language.Ast.Unparser.unparseAstNode x
                | DeonticStatement    x -> Mprokazin.DdlLtlf.Language.Ast.Unparser.unparseAstNode x
            let t = 
                match x.Type with
                | Bound td ->   Mprokazin.DdlLtlf.Language.Ast.Unparser.printTypeDescription td
                | Conflict s -> 
                    sprintf "conflicts: %A" s
                | Unbound x -> $"Unbound {x}"
            printfn "%3d | %-40s : %s" x.Id source t
        )
        |> ignore
    | x -> printfn "Format %A not implemented" x

let run (parameters: GetTypesParameters) = 
    let inputs = 
        try
            createInputs parameters
        with
            | e -> 
                reraise()

    let results = 
        try
            inputs |> List.map processInput
        finally
            for i in inputs do
                i.Reader.Dispose()
        
    let mutable code = 0

    for r in results do
        match r with
        | Ok (source, model) -> 
            printfn "Types of %s:" source
            printModel model parameters.OutputFormat
        | Error (source, e) -> 
            printfn "Error in %s:" source
            printfn "%A" e
            code <- 1
    
    code
