module Tests.Solver

open Mprokazin.DdlLtlf.Language
open Mprokazin.DdlLtlf
open Xunit

[<Fact>]
let ``No conflicts on single simple algebraic statement`` () =
    let input = "obligated x % 3 = 0"
    let ast = Ast.Parser.parse input
    let conflicts = 
        ast 
        |> Solver.findConflicts Solver.PermissionSemantics.Ignore
    Assert.Empty conflicts

[<Fact>]
let ``Obveous algebraic conflicts found`` () =
    let input = """
        obligated x % 2 = 1
        obligated x = 2
    """
    let ast = Ast.Parser.parse input
    let conflicts = 
        ast 
        |> Solver.findConflicts Solver.PermissionSemantics.Ignore
    Assert.NotEmpty conflicts

[<Fact>]
let ``No conflicts when contexts not overlap`` () =
    let input = """
        permitted x = 5 when x > 10
        forbidden x = 5 when x < 10
    """
    let ast = Ast.Parser.parse input
    let conflicts = 
        ast 
        |> Solver.findConflicts Solver.PermissionSemantics.StrongPermission
    Assert.Empty conflicts
