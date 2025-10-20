module Mprokazin.DdlLtlf.Cli.Parameters

type InputSource =
    | StdIn
    | Files of string list

type OutputFormat =
    | Json
    | Shell

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
    PrintModel: bool
    Verbose: bool
}
    with static member init: ValidateParameters = { 
            OutputFormat = Shell;
            Input = Files [];
            PrintModel = false;
            Verbose = false;
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
    | Args.ValidateArgs.PrintModel :: tail -> 
        processValidateArgs tail { state with PrintModel = true }
    | Args.ValidateArgs.Verbose :: tail -> 
        processValidateArgs tail { state with Verbose = true }
    | _::tail -> processValidateArgs tail state // Unknown parameter
