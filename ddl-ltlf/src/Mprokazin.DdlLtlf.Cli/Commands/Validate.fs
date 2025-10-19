module Mprokazin.DdlLtlf.Cli.Commands.Validate

open System.IO
open Mprokazin.DdlLtlf.Cli.Parameters

type ValidationInput = { 
    Source: string
    Reader: TextReader
}

let createInputs (paramters: ValidateParameters) = 
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

type ValidationError =
    | SemanticError of Mprokazin.DdlLtlf.Language.Semantics.SemanticError
    | Exception of exn

type ValidationResult = { 
    Source: string
    Errors: ValidationError list
    Model: obj option
}

let processInput (input: ValidationInput) = 
    let result = {
        Source = input.Source 
        Errors = [] 
        Model = None 
    }

    let result = 
        try
            let stream = Antlr4.Runtime.AntlrInputStream input.Reader
            let lexer = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfLexer(stream)
            let tokens = Antlr4.Runtime.CommonTokenStream(lexer)
            let parser = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfParser(tokens)
            
            let tree = parser.root()

            let ast = Mprokazin.DdlLtlf.Language.Ast.Visitors.visit tree
            let semanticValidationResult = Mprokazin.DdlLtlf.Language.Semantics.validate ast
            
            match semanticValidationResult with
            | Ok () -> { result with Model = Some ast }
            | Error errors -> { result with Errors = result.Errors @ List.map SemanticError errors }
        with
            | e -> { result with Errors = Exception e :: result.Errors }
    result


let run (parameters: ValidateParameters) = 
    if parameters.Verbose then 
        do printfn "Parameters: %A" parameters

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
        
    for r in results do
        if not (List.isEmpty r.Errors) then do
            printfn "Errors in %s:" r.Source
            for e in r.Errors do
                match e with
                | SemanticError e -> 
                    let location = 
                        e.Path
                        |> List.fold (fun s x -> $"{s}->{x}") r.Source
                    printfn "%s in %s: %s" e.Kind location e.Message
                | Exception e ->
                    printfn "Exception in %s: %s" r.Source e.Message
            failwith "Validation failed"

    if parameters.PrintModel then 
        for r in results do
            if List.isEmpty r.Errors then do
                printfn "Model in %s:\n%A" r.Source r.Model.Value
    
    0
