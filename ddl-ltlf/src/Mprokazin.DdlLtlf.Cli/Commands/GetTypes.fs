module Mprokazin.DdlLtlf.Cli.Commands.GetTypes

open System.IO
open Mprokazin.DdlLtlf.Cli.Parameters

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
