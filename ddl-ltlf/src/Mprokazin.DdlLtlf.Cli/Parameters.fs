module Mprokazin.DdlLtlf.Cli.Parameters

type InputSource =
    | StdIn
    | Files of string list

type OutputFormat =
    | Json
    | Shell
    | FSharp

type SolveParameters = {
    Input: InputSource
    OutputFormat: OutputFormat
    Verbose: bool
}
    with static member init: SolveParameters = {
            Input = Files []
            OutputFormat = Shell
            Verbose = false
        }

type ValidateParameters = {
    Input: InputSource
    OutputFormat: OutputFormat
    Verbose: bool
}
    with static member init: ValidateParameters = { 
            OutputFormat = Shell;
            Input = Files [];
            Verbose = false;
        }

type GetAstParameters = {
    Input: InputSource
    OutputFormat: OutputFormat
}
    with static member init: GetAstParameters = {
            Input = Files []
            OutputFormat = FSharp;
        }

type GetTypesParameters = {
    Input: InputSource
    OutputFormat: OutputFormat
}
    with static member init: GetTypesParameters = {
            Input = Files []
            OutputFormat = FSharp;
        }

let rec processSolveArgs args state : SolveParameters =
    match args with
    | [] -> state
    | Args.SolveArgs.Files ["--"] :: tail ->
        processSolveArgs tail { state with Input = StdIn }
    | Args.SolveArgs.Files list :: tail ->
        processSolveArgs tail { state with Input = Files list }
    | Args.SolveArgs.Output_Format Args.OutputFormat.Json :: tail ->
        processSolveArgs tail { state with OutputFormat = Json }
    | Args.SolveArgs.Output_Format Args.OutputFormat.Shell :: tail ->
        processSolveArgs tail { state with OutputFormat = Shell }
    | Args.SolveArgs.Output_Format Args.OutputFormat.FSharp :: tail ->
        processSolveArgs tail { state with OutputFormat = FSharp }
    | Args.SolveArgs.Verbose :: tail ->
        processSolveArgs tail { state with Verbose = true }
    | _::tail -> processSolveArgs tail state

let rec processValidateArgs args state : ValidateParameters =
    match args with
    | [] -> state
    | Args.ValidateArgs.Files ["--"] :: tail -> 
        processValidateArgs tail { state with Input = StdIn }
    | Args.ValidateArgs.Files list :: tail -> 
        processValidateArgs tail { state with Input = Files list }
    | Args.ValidateArgs.Output_Format Args.OutputFormat.Json :: tail-> 
        processValidateArgs tail { state with OutputFormat = Json }
    | Args.ValidateArgs.Output_Format Args.OutputFormat.Shell :: tail-> 
        processValidateArgs tail { state with OutputFormat = Shell }
    | Args.ValidateArgs.Output_Format Args.OutputFormat.FSharp :: tail-> 
        processValidateArgs tail { state with OutputFormat = FSharp }
    | Args.ValidateArgs.Verbose :: tail -> 
        processValidateArgs tail { state with Verbose = true }
    | _::tail -> processValidateArgs tail state // Unknown parameter

let rec processGetAstArgs args state : GetAstParameters =
    match args with
    | [] -> state
    | Args.GetAstArgs.Files ["--"] :: tail -> 
        processGetAstArgs tail { state with Input = StdIn }
    | Args.GetAstArgs.Files list :: tail -> 
        processGetAstArgs tail { state with Input = Files list }
    | Args.GetAstArgs.Output_Format Args.OutputFormat.Json :: tail-> 
        processGetAstArgs tail { state with OutputFormat = Json }
    | Args.GetAstArgs.Output_Format Args.OutputFormat.Shell :: tail-> 
        processGetAstArgs tail { state with OutputFormat = Shell }
    | Args.GetAstArgs.Output_Format Args.OutputFormat.FSharp :: tail-> 
        processGetAstArgs tail { state with OutputFormat = FSharp }
    | _::tail -> processGetAstArgs tail state // Unknown parameter

let rec processGetTypesArgs args state : GetTypesParameters =
    match args with
    | [] -> state
    | Args.GetTypesArgs.Files ["--"] :: tail -> 
        processGetTypesArgs tail { state with Input = StdIn }
    | Args.GetTypesArgs.Files list :: tail -> 
        processGetTypesArgs tail { state with Input = Files list }
    | Args.GetTypesArgs.Output_Format Args.OutputFormat.Json :: tail-> 
        processGetTypesArgs tail { state with OutputFormat = Json }
    | Args.GetTypesArgs.Output_Format Args.OutputFormat.Shell :: tail-> 
        processGetTypesArgs tail { state with OutputFormat = Shell }
    | Args.GetTypesArgs.Output_Format Args.OutputFormat.FSharp :: tail-> 
        processGetTypesArgs tail { state with OutputFormat = FSharp }
    | _::tail -> processGetTypesArgs tail state // Unknown parameter
