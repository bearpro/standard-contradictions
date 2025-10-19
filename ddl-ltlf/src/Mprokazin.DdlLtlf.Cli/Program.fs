namespace Mprokazin.DdlLtlf.Cli

open System
open Argu

module Main =
    let solve () = ()
    let validate () = ()


    [<EntryPoint>]
    let main argv =
        let parser = ArgumentParser.Create<Args.Verbs>(programName = "ddl-ltlf")
        try
            let parsedArgRoot = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
            let verbs = parsedArgRoot.GetAllResults ParseSource.CommandLine

            let result: int =
                match verbs with
                | [Args.Verbs.Validate args] -> 
                    let parameters = Parameters.processValidateArgs (args.GetAllResults()) Parameters.ValidateParameters.init
                    Commands.Validate.run parameters

                | [Args.Verbs.Solve args] -> 0
                | _ -> failwith "Unexpected arguments"
            
            result
        with
        | :? ArguParseException as ex ->
            // Argu already prints caret’d, friendly messages. You can customize:
            eprintfn "%s" (ex.Message.TrimEnd())
            1
