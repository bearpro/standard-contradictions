module Tests.Language.Ast

open System
open Xunit

open Mprokazin.DdlLtlf.Language.Ast

#nowarn "FS0020"
#nowarn "FS0025"

[<Fact>]
let ``Simple obligation`` () =
    let input = 
        """
        obligated wipe(x) when shit(x)
        """
    let stream = Antlr4.Runtime.AntlrInputStream input
    let lexer = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfLexer(stream)
    let tokens = Antlr4.Runtime.CommonTokenStream(lexer)
    let parser = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfParser(tokens)
    
    let tree = parser.root()
    
    let result = Mprokazin.DdlLtlf.Language.Ast.Parser.visit tree

    let deonticStatements = 
        result 
        |> List.where (function DeonticStatement _ -> true | _ -> false)
    
    do Assert.Single(deonticStatements)
    
    let statement::[] = 
        deonticStatements
        |> List.map (function DeonticStatement x -> x)

    match statement.Body with
    | Item(Call ({ PredicateName = n; Arguments = [ { Value = Name p } ] }), _) -> 
        Assert.Equal("wipe", n)
        Assert.Equal<string>(["x"], p)
    | _ -> Assert.Fail()
    
    match statement.Condition.Value with
    | Item(Call ({ PredicateName = n; Arguments = [ { Value = Name p } ] }), _) -> 
        Assert.Equal("shit", n)
        Assert.Equal<string>(["x"], p)
    | _ -> Assert.Fail()

[<Fact>]
let ``Simple obligation with named predicate definition`` () =
    let input = 
        """
        predicate wipe(x) = 
            predicate take_papper(x) = 1 = 1
            predicate use_papper(x) = 1 = 1

            take_papper(x) and use_papper(x_ass)
        
        predicate shit(x) = free_up_as(x)
        
        obligated wipe(x) when shit(x)
        """
    let stream = Antlr4.Runtime.AntlrInputStream input
    let lexer = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfLexer(stream)
    let tokens = Antlr4.Runtime.CommonTokenStream(lexer)
    let parser = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfParser(tokens)
    
    let tree = parser.root()
    
    let result = Mprokazin.DdlLtlf.Language.Ast.Parser.visit tree

    let namedPredicates = 
        result
        |> List.choose (function Predicate p -> Some p | _ -> None)

    let deonticStatements =
        result
        |> List.choose (function DeonticStatement p -> Some p | _ -> None)
    
    do Assert.Equal(2, namedPredicates.Length)
    do Assert.Single(deonticStatements)


[<Fact>]
let ``Algebraic obligation`` () =
    let input = 
        """
        obligated x % 3 = 0 when x > 10
        """
    let stream = Antlr4.Runtime.AntlrInputStream input
    let lexer = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfLexer(stream)
    let tokens = Antlr4.Runtime.CommonTokenStream(lexer)
    let parser = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfParser(tokens)
    
    let tree = parser.root()
    
    let result = Mprokazin.DdlLtlf.Language.Ast.Parser.visit tree

    let namedPredicates = 
        result
        |> List.choose (function Predicate p -> Some p | _ -> None)

    let deonticStatements =
        result
        |> List.choose (function DeonticStatement p -> Some p | _ -> None)
    
    do Assert.Empty(namedPredicates)
    do Assert.Single(deonticStatements)
    
    let statement::[] =  deonticStatements

    match statement.Body with
    | Item(
        AlgebraicCondition({
            Condition = Eq
            LeftExpression = Operation(
                AlgebraicExpression.Value ({ Value = ValueReferenceValue.Name(["x"]) }),
                Mod,
                AlgebraicExpression.Value ({ Value = ValueReferenceValue.IntConstant(3)}),
                _
                )
            RightExpression = AlgebraicExpression.Value ({ Value = ValueReferenceValue.IntConstant(0) })
            }), _) -> Assert.Equal(1, 1)
    | _ -> Assert.Fail()

    match statement.Condition.Value with
    | Item(
        AlgebraicCondition({
            Condition = Gt
            LeftExpression = AlgebraicExpression.Value ({ Value = ValueReferenceValue.Name(["x"]) })
            RightExpression = AlgebraicExpression.Value({ Value = ValueReferenceValue.IntConstant(10) })
            }), _) -> Assert.Equal(1, 1)
    | _ -> Assert.Fail()


[<Fact>]
let ``Boolean and real literal parsed`` () =
    let input = 
        """
        obligated (true)
        obligated (false)
        obligated 0 = 0.2
        """
    let stream = Antlr4.Runtime.AntlrInputStream input
    let lexer = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfLexer(stream)
    let tokens = Antlr4.Runtime.CommonTokenStream(lexer)
    let parser = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfParser(tokens)
    
    let tree = parser.root()
    
    let result = Mprokazin.DdlLtlf.Language.Ast.Parser.visit tree

    let namedPredicates = 
        result
        |> List.choose (function Predicate p -> Some p | _ -> None)

    let deonticStatements =
        result
        |> List.choose (function DeonticStatement p -> Some p | _ -> None)
    
    let (trueSt, falseSt, ratSt) =
        match deonticStatements with
        | [ trueSt; falseSt; ratSt ] -> (trueSt, falseSt, ratSt)
        | _ -> 
            Assert.Fail("Expected to parse 3 obligations")
            failwith ""

    match trueSt.Body with
    | Item(Value({ Value = BooleanConstant (true) }), _) -> Assert.Equal(1, 1)
    | _ -> Assert.Fail()
    
    match falseSt.Body with
    | Item(Value({ Value = BooleanConstant (false) }), _) -> Assert.Equal(1, 1)
    | _ -> Assert.Fail()

    match ratSt.Body with
    | Item(
        AlgebraicCondition({ 
            Condition = Eq
            LeftExpression = AlgebraicExpression.Value ({ Value = ValueReferenceValue.IntConstant(0) })
            RightExpression = AlgebraicExpression.Value({ Value = ValueReferenceValue.RationalConstant("0.2") })
            }),
        _) 
        -> Assert.Equal(1, 1)
    | _ -> Assert.Fail()

