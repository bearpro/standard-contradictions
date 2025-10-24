namespace Mprokazin.DdlLtlf.Cli

open System
open Argu

module Main =

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

                | [Args.Verbs.Solve args] ->
                    let parameters = Parameters.processSolveArgs (args.GetAllResults()) Parameters.SolveParameters.init
                    Commands.Solve.run parameters

                | [Args.Verbs.Get_Ast args] ->
                    let parameters = Parameters.processGetAstArgs (args.GetAllResults()) Parameters.GetAstParameters.init
                    Commands.GetAst.run parameters

                | [Args.Verbs.Get_Types args] ->
                    let parameters = Parameters.processGetTypesArgs (args.GetAllResults()) Parameters.GetTypesParameters.init
                    Commands.GetTypes.run parameters

                | _ -> failwith "Unexpected arguments"

            result
        with
        | :? ArguParseException as ex ->
            // Argu already prints caret’d, friendly messages. You can customize:
            eprintfn "%s" (ex.Message.TrimEnd())
            1
