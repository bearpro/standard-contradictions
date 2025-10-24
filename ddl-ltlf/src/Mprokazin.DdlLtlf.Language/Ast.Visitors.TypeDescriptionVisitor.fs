namespace Mprokazin.DdlLtlf.Language.Ast.Visitors

open Mprokazin.DdlLtlf.Language.Ast
open Mprokazin.DdlLtlf.Language.Ast.Visitors.Helpers
open Mprokazin.DdlLtlf.Language.Antlr

type TypeDescritptionVisitor() =
    inherit DdlLtlfBaseVisitor<TypeDescription>()

    let primitiveOf (ctx: DdlLtlfParser.PrimitiveTypeNameContext) : PrimitiveType =
        match ctx with
        | :? DdlLtlfParser.RationalContext  -> PrimitiveType.Rational
        | :? DdlLtlfParser.IntContext       -> PrimitiveType.Int
        | :? DdlLtlfParser.BoolContext      -> PrimitiveType.Bool
        | _ -> failwith "Unknown primitiveTypeName"

    let typeReferenceOf (ctx: DdlLtlfParser.TypeReferenceContext) : TypeReference =
        let (|Primitive|_|) _ = ctx.primitiveTypeName() |> Option.ofObj
        let (|TypeName|_|) _ = ctx.NAME() |> Option.ofObj
        
        match ctx with
        | Primitive p -> primitiveOf p |> TypeReference.Primitive
        | TypeName t -> t.GetText() |> TypeReference.Named
        | _ -> failwith "Unexpected type reference"

    let rec typeDescriptionOf (ctx: DdlLtlfParser.TypeDescriptionContext) : TypeDescription =
        let (|Reference|_|) _ = ctx.typeReference() |> Option.ofObj
        let (|Sum|_|) _ = ctx.sumTypeDescription() |> Option.ofObj
        let (|Product|_|) _ = ctx.productTypeDescription() |> Option.ofObj

        match ctx with
        | Reference r -> TypeDescription.Reference (typeReferenceOf r)
        | Sum s -> 
            let cases =
                s.typeDescription()
                |> Seq.map typeDescriptionOf
                |> Seq.toList
            TypeDescription.Sum { Variants = cases; Range = rangeOfCtx s }
        | Product p -> 
            let fields : ProductTypeField list =
                p.productTypeItem()
                |> Seq.map (fun item ->
                    let name = item.NAME().GetText()
                    let typOpt =
                        item.typeDescription()
                        |> Option.ofObj
                        |> Option.map typeDescriptionOf

                    { Name  = name
                      Type  = typOpt
                      Range = rangeOfCtx item }: ProductTypeField)
                |> Seq.toList
            TypeDescription.Product { Fields = fields; Range = rangeOfCtx p }
        | _ -> 
            failwith "Unreachable: typeDescription must match one of the three alternatives"

    override _.VisitTypeDescription (ctx: DdlLtlfParser.TypeDescriptionContext): TypeDescription = 
        typeDescriptionOf ctx
