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
    | Item(Call ({ PredicateName = n; Arguments = [ { Kind = ExpressionKind.Name segments } ] }), _) -> 
        Assert.Equal("wipe", n)
        Assert.Equal<string list>(["x"], segments)
    | _ -> Assert.Fail()
    
    match statement.Condition.Value with
    | Item(Call ({ PredicateName = n; Arguments = [ { Kind = ExpressionKind.Name segments } ] }), _) -> 
        Assert.Equal("shit", n)
        Assert.Equal<string list>(["x"], segments)
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
    | Item(AlgebraicCondition condition, _) ->
        match condition.LeftExpression with
        | Operation(
            AlgebraicExpression.Expression { Kind = ExpressionKind.Name segments },
            Mod,
            AlgebraicExpression.Expression { Kind = ExpressionKind.Constant (ConstantValue.IntConstant 3) },
            _) ->
            Assert.Equal<string list>(["x"], segments)
        | _ -> Assert.Fail()
        match condition.RightExpression with
        | AlgebraicExpression.Expression { Kind = ExpressionKind.Constant (ConstantValue.IntConstant 0) } -> ()
        | _ -> Assert.Fail()
    | _ -> Assert.Fail()

    match statement.Condition.Value with
    | Item(AlgebraicCondition condition, _) ->
        match condition.LeftExpression with
        | AlgebraicExpression.Expression { Kind = ExpressionKind.Name segments } ->
            Assert.Equal<string list>(["x"], segments)
        | _ -> Assert.Fail()
        match condition.RightExpression with
        | AlgebraicExpression.Expression { Kind = ExpressionKind.Constant (ConstantValue.IntConstant 10) } -> ()
        | _ -> Assert.Fail()
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
    
    let trueSt, falseSt, ratSt =
        match deonticStatements with
        | [ trueSt; falseSt; ratSt ] -> trueSt, falseSt, ratSt
        | _ -> 
            Assert.Fail("Expected to parse 3 obligations")
            failwith ""

    match trueSt.Body with
    | Item(Expression { Kind = ExpressionKind.Constant (ConstantValue.BooleanConstant true) }, _) -> Assert.Equal(1, 1)
    | _ -> Assert.Fail()
    
    match falseSt.Body with
    | Item(Expression { Kind = ExpressionKind.Constant (ConstantValue.BooleanConstant false) }, _) -> Assert.Equal(1, 1)
    | _ -> Assert.Fail()

    match ratSt.Body with
    | Item(
        AlgebraicCondition({ 
            Condition = Eq
            LeftExpression = AlgebraicExpression.Expression { Kind = ExpressionKind.Constant (ConstantValue.IntConstant 0) }
            RightExpression = AlgebraicExpression.Expression { Kind = ExpressionKind.Constant (ConstantValue.RationalConstant "0.2") }
            }),
        _) 
        -> Assert.Equal(1, 1)
    | _ -> Assert.Fail()

[<Fact>]
let ``Zero-arity sum type pattern matching parsed``() = 
    let input = 
        """
        type T = (A|B)
        predicate p(t) = t is A()
        """
    
    let stream = Antlr4.Runtime.AntlrInputStream input
    let lexer = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfLexer(stream)
    let tokens = Antlr4.Runtime.CommonTokenStream(lexer)
    let parser = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfParser(tokens)
    
    let tree = parser.root()
    
    let result = Mprokazin.DdlLtlf.Language.Ast.Parser.visit tree

    let p = 
        result
        |> List.choose (function Predicate p -> Some p | _ -> None)
        |> List.find (fun p -> p.Name = "p")

    match p.Body with
    | Item(PatternMatch(x, p), _) -> 
        match x with
        | { Kind = Name ["t"] } -> ()
        | x -> Assert.Fail(message=sprintf "%A" x)
        match p with
        | Constructor { Constructor = "A" } -> ()
        | x -> Assert.Fail(message=sprintf "%A" x)
    | x -> Assert.Fail(message=sprintf "%A" x)


[<Fact>]
let ``N-arity sum type pattern matching parsed``() = 
    let input = 
        """
        type T = (A of int|B of rational)
        predicate p(t) = t is A(1)
        """
    
    let stream = Antlr4.Runtime.AntlrInputStream input
    let lexer = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfLexer(stream)
    let tokens = Antlr4.Runtime.CommonTokenStream(lexer)
    let parser = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfParser(tokens)
    
    let tree = parser.root()
    
    let result = Mprokazin.DdlLtlf.Language.Ast.Parser.visit tree

    let t = 
        result
        |> List.choose (function Type t -> Some t | _ -> None)
        |> List.find (fun t -> t.Name = "T")

    let tVariants = 
        match t.Body with
        | TypeDescription.Sum { Variants = x } -> x
        | _ -> failwith "T was not sum type"

    let a, b = 
        match tVariants with
        | [a; b] -> a, b
        | _ -> failwith "Variants A and B not parsed"

    let ap = 
        match a with
        | { Constructor = "A"; Payload = Some ap } -> ap
        | _ -> failwith "Constructor or payload A not parsed"

    let bp = 
        match b with
        | { Constructor = "B"; Payload = Some bp } -> bp
        | _ -> failwith "Constructor or payload B not parsed"

    match ap with
    | Reference(Primitive Int) -> ()
    | x -> failwithf "Variant A payload was %A, not 'int'." x

    match bp with
    | Reference(Primitive Rational) -> ()
    | x -> failwithf "Variant B payload was %A, not 'rational'." x

    let p = 
        result
        |> List.choose (function Predicate p -> Some p | _ -> None)
        |> List.find (fun p -> p.Name = "p")

    match p.Body with
    | Item(PatternMatch(x, p), _) -> 
        match x with
        | { Kind = Name ["t"] } -> ()
        | x -> Assert.Fail(message=sprintf "%A" x)
        match p with
        | Constructor { Constructor = "A" } -> ()
        | x -> Assert.Fail(message=sprintf "%A" x)
    | x -> Assert.Fail(message=sprintf "%A" x)


