module Mprokazin.DdlLtlf.Cli.Commands.GetTypes

open System
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
        match Mprokazin.DdlLtlf.Language.Typing.inferTypes ast with
        | Ok typed -> Ok (input.Source, typed)
        | Error errors ->
            let message =
                errors
                |> List.map (fun err ->
                    let range =
                        sprintf "%d:%d-%d:%d" err.Range.StartLine err.Range.StartChar err.Range.EndLine err.Range.EndChar
                    sprintf "%s (%A): %s" range err.Kind err.Message)
                |> String.concat Environment.NewLine
            Error (input.Source, message)
    with
        | e -> Error (input.Source, e.Message)

let printModel (model: Mprokazin.DdlLtlf.Language.Typing.TypedProgram) format =
    match format with
    | FSharp -> 
        model.TypedObjects
        |> List.map (fun info -> info.Id, info.Type)
        |> printfn "%A"
    | Shell ->
        model.TypedObjects
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
                Mprokazin.DdlLtlf.Language.Ast.Unparser.printTypeDescription x.Type
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
        | Error (source, message) -> 
            printfn "Error in %s:" source
            printfn "%s" message
            code <- 1
    
    code
