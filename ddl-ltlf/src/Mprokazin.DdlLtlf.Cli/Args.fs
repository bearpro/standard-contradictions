module Mprokazin.DdlLtlf.Cli.Args
open System
open Argu

type OutputFormat =
    | Json = 0
    | Shell = 1
    | FSharp = 2

type SolveArgs =
    | [<AltCommandLine("-v")>] Verbose
    | [<AltCommandLine("-f")>] Output_Format of OutputFormat
    | [<MainCommand; ExactlyOnce; Last>] Files of files: string list
with
    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | Verbose -> "Set verbose output"
            | Output_Format _ -> "Output format"
            | Files _ -> "Source files to validate"

type ValidateArgs =
    | [<AltCommandLine("-f")>] Output_Format of OutputFormat
    | [<AltCommandLine("-v")>] Verbose
    | [<MainCommand; ExactlyOnce; Last>] Files of files: string list
with
    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | Output_Format _ -> "Output format"
            | Verbose -> "Verbose output"
            | Files _ -> "Source files to validate"

type GetAstArgs =
    | [<AltCommandLineAttribute("f")>] Output_Format of OutputFormat
    | [<MainCommand; ExactlyOnce; Last>] Files of files: string list
with 
    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | Output_Format _ -> "Output format"
            | Files _ -> "Source files to validate"

type GetTypesArgs =
    | [<AltCommandLineAttribute("f")>] Output_Format of OutputFormat
    | [<MainCommand; ExactlyOnce; Last>] Files of files: string list
with 
    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | Output_Format _ -> "Output format"
            | Files _ -> "Source files to validate"

type Verbs =
    | [<CliPrefix(CliPrefix.None)>] Get_Ast of ParseResults<GetAstArgs>
    | [<CliPrefix(CliPrefix.None)>] Get_Types of ParseResults<GetTypesArgs>
    | [<CliPrefix(CliPrefix.None)>] Solve of ParseResults<SolveArgs>
    | [<CliPrefix(CliPrefix.None)>] Validate of ParseResults<ValidateArgs>
with
    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | Validate _    -> "Validate syntax and semantics of input files."
            | Get_Ast _     -> "Builds and prints syntax tree"
            | Get_Types _   -> "Infers types and prints inferred type information"
            | Solve _       -> "Solve the combined set of assertions from input files."
