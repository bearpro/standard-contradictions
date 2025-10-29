module Tests.Language.Typing

open System
open Xunit

open Mprokazin.DdlLtlf.Language

#nowarn "FS0020"
#nowarn "FS0025"

[<Fact>]
let ``Simple types valdidated`` () =
    let input = 
        """
        predicate run(x: int) = x > 0
        """

    let ast = 
        Ast.Parser.parse input 
        |> Tests.Helpers.RangeSanitizer.sanitizeProgram

    match Typing.inferTypes ast with
    | Ok typed ->
        Assert.NotEmpty typed.TypedObjects
    | Error errors ->
        let message =
            errors
            |> List.map (fun e -> e.Message)
            |> String.concat "; "
        failwithf "Typing failed: %s" message
