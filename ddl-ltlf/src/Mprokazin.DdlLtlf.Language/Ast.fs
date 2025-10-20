
module Mprokazin.DdlLtlf.Language.Ast

type AlgebraicEqualityCondition =
    | Gt
    | Ge
    | Eq
    | Lt
    | Le

type AlgebraicOperation = Sum | Mod | Mul | Sub | Dev

type AlgebraicExpression = 
    | IntegerConstant of int64
    | RealConstant of string // No need to parse it to float and loose percision
    | Variable of string
    | Op of A: AlgebraicExpression * Op: AlgebraicOperation * B: AlgebraicExpression

type PredicateExpression =
    | And of A: PredicateExpression * B: PredicateExpression
    | Or of A: PredicateExpression * B: PredicateExpression
    | Not of PredicateExpression
    | NamedPredicateCall of Name: string * Parameters: string array
    | Bool of bool
    | NestedPredicate of 
        Defined: NamedPredicateDefinition * 
        Predicate: PredicateExpression
    | AlgebraicPredicate of 
        A: AlgebraicExpression * 
        B: AlgebraicExpression * 
        Condition: AlgebraicEqualityCondition
    | NotImplementedExpression of string

and NamedPredicateDefinition = { 
    Name: string
    Parameters: string array
    Predicate: PredicateExpression
}

type DeonticModality = 
    | Obligated 
    | Permitted
    | Forbidden
    | Suggested

type DeonticStatement = {
    Name: string
    Modality: DeonticModality
    Body: PredicateExpression
    Context: PredicateExpression option
}

type Model = { 
    DeonticStatements: DeonticStatement list
    NamedPredicates: NamedPredicateDefinition list
}

module Visitors =

    open Mprokazin.DdlLtlf.Language.Antlr

    type ModalityVisitor() =
        inherit DdlLtlfBaseVisitor<DeonticModality>()

        override _.VisitDeonticModality ctx =
            match ctx.GetText() with
            | "obligated" -> DeonticModality.Obligated
            | "permitted" -> DeonticModality.Permitted
            | "forbidden" -> DeonticModality.Forbidden
            | "suggested" -> DeonticModality.Suggested
            | x -> failwithf "Unknown modality: %s" x

    type ParametersVisitor() =
        inherit DdlLtlfBaseVisitor<string list>()
        override _.VisitParameters ctx = 
            ctx.NAME()
            |> Array.map (fun x -> x.GetText())
            |> List.ofArray

    type AlgebraicExpressionVisitor() =
        inherit DdlLtlfBaseVisitor<AlgebraicExpression>()

        override this.VisitConst ctx =
            if ctx.algebraicConstant().INT() <> null then
                ctx.algebraicConstant().INT().GetText() 
                |> int64 
                |> AlgebraicExpression.IntegerConstant
            else if ctx.algebraicConstant().RATIONAL() <> null then
                ctx.algebraicConstant().RATIONAL().GetText() 
                |> AlgebraicExpression.RealConstant
            else failwithf "Unexpected algebraic constant %s" (ctx.algebraicConstant().GetText())

        override this.VisitVar ctx =
            let paramName = ctx.parameterReference().GetText()
            AlgebraicExpression.Variable paramName

        override this.VisitOp ctx =
            let a = this.Visit (ctx.algebraicExpression(0))
            let b = this.Visit (ctx.algebraicExpression(1))
            match ctx.binaryAlgegraicOperation().GetText() with
            | "/" -> AlgebraicExpression.Op(a, Dev, b)
            | "*" -> AlgebraicExpression.Op(a, Mul, b)
            | "+" -> AlgebraicExpression.Op(a, Sum, b)
            | "-" -> AlgebraicExpression.Op(a, Sub, b)
            | "%" -> AlgebraicExpression.Op(a, Mod, b)
            | x -> failwithf "Unexpected algebraic operation %s" x

        override this.VisitParens ctx: AlgebraicExpression = 
            this.Visit (ctx.algebraicExpression())

    type PredicateExpressionVisitor() =
        inherit DdlLtlfBaseVisitor<PredicateExpression>()

        override this.VisitOr ctx =
            let a = this.Visit(ctx.predicate(0))
            let b = this.Visit(ctx.predicate(1))
            PredicateExpression.Or(A = a, B = b)

        override this.VisitAnd ctx =
            let a = this.Visit(ctx.predicate(0))
            let b = this.Visit(ctx.predicate(1))
            PredicateExpression.And(A = a, B = b)

        override this.VisitNot ctx =
            PredicateExpression.Not (this.Visit (ctx.predicate()))

        override this.VisitPredicateParens ctx =
            this.Visit(ctx.predicate())

        override this.VisitBool ctx =
            match ctx.GetText() with
            | "true" -> Bool true
            | "false" -> Bool false
            | x -> failwithf "Unexpected boolean constant %s" x

        override this.VisitAlgebraic ctx =
            let v = AlgebraicExpressionVisitor()
            
            let a = ctx.algebraicPredicate().algebraicExpression(0)
            let a = v.Visit a
            
            let b = ctx.algebraicPredicate().algebraicExpression(1)
            let b = v.Visit b
            
            let condition = 
                match ctx.algebraicPredicate().GetChild(1).GetText() with
                | "<" -> AlgebraicEqualityCondition.Lt
                | "<=" -> AlgebraicEqualityCondition.Le
                | "=" -> AlgebraicEqualityCondition.Eq
                | ">=" -> AlgebraicEqualityCondition.Ge
                | ">" -> AlgebraicEqualityCondition.Gt
                | x -> failwithf "Unknown equality condition: %s" x

            PredicateExpression.AlgebraicPredicate(a, b, condition)

        override _.VisitNamedPredicateCall ctx =
            let name = ctx.NAME().GetText()
            let ps =
                if isNull (ctx.parameters()) then []
                else ParametersVisitor().VisitParameters(ctx.parameters())
            PredicateExpression.NamedPredicateCall(name, Array.ofList ps)

        override this.VisitNestedPredicate ctx =
            let nestedPredicate = 
                NamedPredicateDefinitionVisitor().Visit(ctx.namedPredicateDefinition())
            let predicate = this.Visit(ctx.predicate())
            PredicateExpression.NestedPredicate(nestedPredicate, predicate)

    and NamedPredicateDefinitionVisitor() =
        inherit DdlLtlfBaseVisitor<NamedPredicateDefinition>()

        override this.VisitNamedPredicateDefinition ctx =
            let name = ctx.NAME().GetText()
            let ps =
                if isNull (ctx.parameters()) then []
                else ParametersVisitor().VisitParameters(ctx.parameters())
            let predicate = PredicateExpressionVisitor().Visit(ctx.predicate())
            
            { Name = name 
              Parameters = Array.ofList ps
              Predicate = predicate }


    type DeonticStatementVisitor() =
        inherit DdlLtlfBaseVisitor<DeonticStatement>()

        override this.VisitDeonticStatement ctx =
            let modality = ctx.deonticModality().Accept(ModalityVisitor())
            
            let nameOpt =
                match ctx.NAME() with
                | null -> $"(anonymous statement at {ctx.Start.Line}:{ctx.Start.Column})"
                | name -> name.GetText()

            let body = ctx.predicate(0).Accept(PredicateExpressionVisitor())

            let contextOpt =
                if ctx.predicate().Length > 1 then
                    Some (ctx.predicate(1).Accept(PredicateExpressionVisitor()))
                else None

            {
                Name = nameOpt
                Modality = modality
                Body = body
                Context = contextOpt
            }

    type RootVisitor() =
        inherit DdlLtlfBaseVisitor<Model>()

        override this.VisitRoot (context: DdlLtlfParser.RootContext): Model = 
            let deonticStatements = 
                context.deonticStatement() 
                |> Array.map (DeonticStatementVisitor().Visit)
                |> List.ofArray

            let namedPredicates =
                context.namedPredicateDefinition()
                |> Array.map (NamedPredicateDefinitionVisitor().Visit)
                |> List.ofArray

            { DeonticStatements = deonticStatements
              NamedPredicates = namedPredicates }
                
    let visit x = RootVisitor().Visit x

let parse (input: string) =

    let stream = Antlr4.Runtime.AntlrInputStream input
    let lexer = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfLexer(stream)
    let tokens = Antlr4.Runtime.CommonTokenStream(lexer)
    let parser = Mprokazin.DdlLtlf.Language.Antlr.DdlLtlfParser(tokens)
    
    let tree = parser.root()
    
    let result = Visitors.visit tree
    result