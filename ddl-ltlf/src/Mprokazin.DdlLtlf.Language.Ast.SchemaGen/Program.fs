open System
open System.Text
open System.Text.Json
open System.Text.Json.Serialization
open NJsonSchema
open NJsonSchema.Generation
open NJsonSchema.Generation.TypeMappers

open FSharp.SystemTextJson

open Mprokazin.DdlLtlf

type SchemaRoot = Language.Ast.Model

let postprocess (schema: JsonSchema) =
    for def in schema.Definitions do
        def.Value.EnumerationNames <- null

[<EntryPoint>]
let main _ =
    let schema = FSharp.Data.JsonSchema.Generator.Create("ddl-ltlf") typeof<SchemaRoot>
    schema.Title <- "DDL-LTLf AST"
    schema.Description <- "JSON Schema for DDL-LTLf's language AST"
    schema.SchemaVersion <- "https://json-schema.org/draft/2020-12/schema"

    do postprocess schema

    Console.OutputEncoding <- Encoding.UTF8
    let json = schema.ToJson Newtonsoft.Json.Formatting.Indented
    Console.WriteLine(json)
    0
