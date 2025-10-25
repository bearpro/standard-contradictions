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

    let types = Typing.inferTypes ast 
    let expctedTypes = [ 
        Typing.ProgramObjTypeInfo.Predicate(
            { 
                Name = "run"
                Parameters = [] 
                Body = Ast.PredicateBody.Item(
                    Ast.PredicateBodyItem.AlgebraicCondition(
                        ({
                            Condition = Ast.AlgebraicEqualityCondition.Gt
                            LeftExpression = Ast.AlgebraicExpression.Value(
                                { Value = Ast.ValueReferenceValue.Name(["x"]) 
                                  Type = None
                                  Range = Tests.Helpers.RangeSanitizer.unsetStatusRange
                                }: Ast.ValueReference)
                            RightExpression = Ast.AlgebraicExpression.Value(
                                { Value = Ast.ValueReferenceValue.IntConstant(3)
                                  Type = None
                                  Range = Tests.Helpers.RangeSanitizer.unsetStatusRange
                                }: Ast.ValueReference)
                            Range = Tests.Helpers.RangeSanitizer.unsetStatusRange
                         }: Ast.AlgebraicCondition)
                    ),
                    Tests.Helpers.RangeSanitizer.unsetStatusRange
                )
                Range = Tests.Helpers.RangeSanitizer.unsetStatusRange
            },
            Typing.InferedType.Bound(
                Ast.TypeDescription.Product(
                    ([ 
                        { Name = "x"
                          Type = Some <| Ast.TypeDescription.Reference(
                            Ast.TypeReference.Primitive(Ast.PrimitiveType.Int)
                          )
                          Range = Tests.Helpers.RangeSanitizer.unsetStatusRange
                        };
                    ] : Ast.ProductTypeField list),
                    Tests.Helpers.RangeSanitizer.unsetStatusRange
                ))
        ) 
    ]

    ()
