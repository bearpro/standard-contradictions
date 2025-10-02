module Tests.Language.Semantics

open Mprokazin.DdlLtlf.Language
open Xunit

[<Fact>]
let ``Undefined predicate fails`` () =
    let input = "obligated run(x) when runner(x)"
    let ast = Ast.parse input
    match Semantics.validate ast with
    | Ok () -> Assert.Fail("Unexpectedly Ok")
    | Error e -> Assert.NotEmpty e