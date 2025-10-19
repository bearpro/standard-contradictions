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
    
    let result = Mprokazin.DdlLtlf.Language.Ast.Visitors.visit tree
    
    do Assert.Empty(result.NamedPredicates)
    do Assert.Single(result.DeonticStatements)
    
    let statement::[] =  result.DeonticStatements

    match statement.Body with
    | NamedPredicateCall(name, parameters) ->
        Assert.Equal("wipe", name)
        Assert.Equal<string>(["x"], parameters)
    | _ -> Assert.Fail()
    match statement.Context.Value with
    | NamedPredicateCall(name, parameters) ->
        Assert.Equal("shit", name)
        Assert.Equal<string>(["x"], parameters)
    | _ -> Assert.Fail()

[<Fact>]
let ``Simple obligation with named predicate definition`` () =
    let input = 
        """
        predicate wipe(x) = 
            predicate take_papper(x) = 1 = 1 in
            predicate use_papper(x) = 1 = 1 in

            take_papper(x) and use_papper(x_ass)
        
        predicate shit(x) = free_up_as(x)
        
        obligated wipe(x) when shit(x)
        """
    let stream = Antlr4.Runtime.AntlrInputStream input
    let lexer = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfLexer(stream)
    let tokens = Antlr4.Runtime.CommonTokenStream(lexer)
    let parser = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfParser(tokens)
    
    let tree = parser.root()
    
    let result = Mprokazin.DdlLtlf.Language.Ast.Visitors.visit tree
    
    do Assert.Equal(2, result.NamedPredicates.Length)
    do Assert.Single(result.DeonticStatements)
    
    let statement::[] =  result.DeonticStatements

    match statement.Body with
    | NamedPredicateCall(name, parameters) ->
        Assert.Equal("wipe", name)
        Assert.Equal<string>(["x"], parameters)
    | _ -> Assert.Fail()
    match statement.Context.Value with
    | NamedPredicateCall(name, parameters) ->
        Assert.Equal("shit", name)
        Assert.Equal<string>(["x"], parameters)
    | _ -> Assert.Fail()

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
    
    let result = Mprokazin.DdlLtlf.Language.Ast.Visitors.visit tree
    
    do Assert.Empty(result.NamedPredicates)
    do Assert.Single(result.DeonticStatements)
    
    let statement::[] =  result.DeonticStatements

    match statement.Body with
    | AlgebraicPredicate(a, b, condition) ->
        Assert.Equal(
            AlgebraicExpression.Op(
                AlgebraicExpression.Variable("x"),
                AlgebraicOperation.Mod,
                AlgebraicExpression.Constant(3)),
            a)
        Assert.Equal(
            AlgebraicExpression.Constant(0),
            b)
        Assert.True(condition.IsEq)
    | _ -> Assert.Fail()
    match statement.Context.Value with
    | AlgebraicPredicate(a, b, condition) ->
        Assert.Equal(AlgebraicExpression.Variable "x", a)
        Assert.Equal(AlgebraicExpression.Constant 10, b)
        Assert.Equal(AlgebraicEqualityCondition.Gt, condition)
    | _ -> Assert.Fail()
