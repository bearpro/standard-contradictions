module Mprokazin.DdlLtlf.Cli.Commands.Solve

open System
open System.IO
open System.Text.Json
open Mprokazin.DdlLtlf.Cli.Parameters

type private SolveInput = {
    Source: string
    Reader: TextReader
}

type private SolveError =
    | Exception of exn

type private SolveInputResult = {
    Source: string
    Errors: SolveError list
    Model: Mprokazin.DdlLtlf.Language.Ast.Program option
}

let private createInputs (parameters: SolveParameters) : SolveInput list =
    match parameters.Input with
    | Files list ->
        list
        |> List.where (fun path ->
            if File.Exists path then true
            else failwithf "File does not exists: %s" path)
        |> List.map (fun path ->
            { Source = path
              Reader = new StreamReader(path) :> TextReader })
    | StdIn ->
        [ { Source = "STDIN"
            Reader = Console.In } ]

let private parseInput (input: SolveInput) : SolveInputResult =
    let initial = {
        Source = input.Source
        Errors = []
        Model = None
    }

    try
        let stream = Antlr4.Runtime.AntlrInputStream input.Reader
        let lexer = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfLexer(stream)
        let tokens = Antlr4.Runtime.CommonTokenStream(lexer)
        let parser = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfParser(tokens)
        let tree = parser.root()
        let model = Mprokazin.DdlLtlf.Language.Ast.Parser.visit tree

        { initial with Model = Some model }
    with ex ->
        { initial with Errors = [ Exception ex ] }

let private disposeInput (input: SolveInput) =
    if not (obj.ReferenceEquals(input.Reader, Console.In)) then
        input.Reader.Dispose()

let private combineModels (models: Mprokazin.DdlLtlf.Language.Ast.Program list) : Mprokazin.DdlLtlf.Language.Ast.Program =
    models |> List.collect id

// let private printSemanticErrors (errors: Mprokazin.DdlLtlf.Language.Semantics.SemanticError list) =
//     printfn "Semantic validation failed:"
//     for error in errors do
//         let location =
//             error.Path
//             |> List.fold (fun acc part -> $"{acc}->{part}") "model"
//         printfn "%s in %s: %s" error.Kind location error.Message

let private printInputErrors (results: SolveInputResult list) =
    for result in results do
        if not (List.isEmpty result.Errors) then
            printfn "Errors in %s:" result.Source
            for err in result.Errors do
                match err with
                | Exception ex ->
                    printfn "Exception in %s: %s" result.Source ex.Message

let private writeShellOutput (conflicts: Mprokazin.DdlLtlf.Solver.Conflict list) =
    if List.isEmpty conflicts then
        printfn "No conflicts found"
    else
        printfn "Conflicts found (%d):" conflicts.Length
        for conflict in conflicts do
            printfn "- %s vs %s: %s" conflict.LeftId conflict.RightId conflict.Reason

let private writeJsonOutput (conflicts: Mprokazin.DdlLtlf.Solver.Conflict list) =
    let payload =
        {| conflicts = conflicts
           summary = {| count = conflicts.Length |} |}

    let options = JsonSerializerOptions(WriteIndented = true)
    let json = JsonSerializer.Serialize(payload, options)
    printfn "%s" json

let run (parameters: SolveParameters) =
    if parameters.Verbose then
        printfn "Parameters: %A" parameters

    let inputs =
        try
            createInputs parameters
        with
        | ex ->
            if parameters.Verbose then
                printfn "Failed to create inputs: %s" ex.Message
            reraise()

    let results =
        try
            inputs |> List.map parseInput
        finally
            for input in inputs do
                disposeInput input

    if parameters.Verbose then
        printfn "Parsed %d input(s)" results.Length

    let hasErrors =
        results
        |> List.exists (fun r -> not (List.isEmpty r.Errors))

    if hasErrors then
        printInputErrors results
        1
    else
        let models =
            results
            |> List.choose (fun r -> r.Model)

        let combined = combineModels models

        let conflicts =
            combined
            |> Mprokazin.DdlLtlf.Solver.findConflicts Mprokazin.DdlLtlf.Solver.PermissionSemantics.StrongPermission

        if parameters.Verbose then
            printfn "Conflicts detected: %d" conflicts.Length

        match parameters.OutputFormat with
        | OutputFormat.Shell -> writeShellOutput conflicts
        | OutputFormat.Json -> writeJsonOutput conflicts
        | OutputFormat.FSharp -> printfn "%A" conflicts

        if List.isEmpty conflicts then 0 else 1
