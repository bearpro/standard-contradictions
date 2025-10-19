module Mprokazin.DdlLtlf.Cli.Args
open System
open Argu

type OutputFormat =
    | Json = 0
    | Shell = 1

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
    | [<AltCommandLine("-p")>] PrintModel
    | [<MainCommand; ExactlyOnce; Last>] Files of files: string list
with
    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | PrintModel -> "If no error, prints semantic model"
            | Output_Format _ -> "Output format"
            | Verbose -> "Verbose output"
            | Files _ -> "Source files to validate"

type Verbs =
    | [<CliPrefix(CliPrefix.None)>] Solve of ParseResults<SolveArgs>
    | [<CliPrefix(CliPrefix.None)>] Validate of ParseResults<ValidateArgs>
with
    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | Solve _    -> "Solve the combined set of assertions from input files."
            | Validate _ -> "Validate syntax and semantics of input files."
