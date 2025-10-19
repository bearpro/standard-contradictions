module Mprokazin.DdlLtlf.Cli.Parameters

type InputSource =
    | StdIn
    | Files of string list

type OutputFormat =
    | Json
    | Shell

type SolveParameters = {
    Input: InputSource
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

let processSolveArgs args state : SolveParameters =
    match args with
    | Args.SolveArgs.Files ["--"] -> { state with Input = StdIn }
    | Args.SolveArgs.Files list -> { state with Input = Files list }
    | _ -> state

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
