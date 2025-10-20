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

type AntlrError = {
    Line: int
    Char: int
    Message: string
}

type ValidationError =
    | SemanticError of Mprokazin.DdlLtlf.Language.Semantics.SemanticError
    | LexerError of AntlrError
    | ParserError of AntlrError
    | Exception of exn

type CollectingErrorListener() =
    inherit Antlr4.Runtime.BaseErrorListener()

    let mutable errors: AntlrError list = []

    member _.Errors = errors |> List.rev

    override _.SyntaxError(output, recognizer, offendingSymbol, line, charPositionInLine, msg, e) =
        let e = { 
            Line = line
            Char = charPositionInLine
            Message = msg }
        errors <- e :: errors

    interface Antlr4.Runtime.IAntlrErrorListener<int> with 
        member this.SyntaxError (
            output: TextWriter, 
            recognizer: Antlr4.Runtime.IRecognizer, 
            offendingSymbol: int, 
            line: int, 
            charPositionInLine: int, 
            msg: string, 
            e: Antlr4.Runtime.RecognitionException): unit = 
                this.SyntaxError(output, recognizer, null, line, charPositionInLine, msg, e)

type ValidationResult = { 
    Source: string
    Errors: ValidationError list
    Model: Mprokazin.DdlLtlf.Language.Ast.Model option
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
            let lexerErrors = CollectingErrorListener()
            lexer.RemoveErrorListeners()
            lexer.AddErrorListener lexerErrors

            let tokens = Antlr4.Runtime.CommonTokenStream(lexer)
            let parser = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfParser(tokens)
            let parserErrors = CollectingErrorListener()
            parser.RemoveErrorListeners()
            parser.AddErrorListener parserErrors
            
            let tree = parser.root()
            let ast = Mprokazin.DdlLtlf.Language.Ast.Visitors.visit tree

            let parserErrors = 
                parserErrors.Errors 
                |> List.map ParserError

            let lexerErrors = 
                lexerErrors.Errors
                |> List.map LexerError

            let result = 
                { result with 
                    Errors = result.Errors @ lexerErrors @ parserErrors
                    Model = Some ast }

            result
        with
            | e -> { result with Errors = Exception e :: result.Errors }

    let result = 
        if Option.isSome result.Model then 
            try 
                let semanticValidationResult = Mprokazin.DdlLtlf.Language.Semantics.validate result.Model.Value
            
                match semanticValidationResult with
                | Ok () -> result
                | Error errors -> { result with Errors = result.Errors @ List.map SemanticError errors }
            with 
                | e -> { result with Errors = Exception e :: result.Errors }
        else result
        
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
        
    let mutable code = 0

    for r in results do
        if not (List.isEmpty r.Errors) then do
            printfn "Errors in %s:" r.Source
            for e in r.Errors do
                match e with
                | SemanticError e -> 
                    let location = String.concat "->" e.Path
                    printfn "Semantic error in %s (%s): %s" r.Source location e.Message
                | LexerError e ->
                    printfn "Lexer error in %s:line %d:%d: %s" r.Source e.Line e.Char e.Message
                | ParserError e ->
                    printfn "Parser error in %s:line %d:%d: %s" r.Source e.Line e.Char e.Message
                | Exception e ->
                    printfn "Exception in %s: %s\n%s" r.Source e.Message e.StackTrace
            code <- 1

    if parameters.PrintModel then 
        for r in results do
            if List.isEmpty r.Errors then do
                printfn "Model in %s:\n%A" r.Source r.Model.Value
    
    code
