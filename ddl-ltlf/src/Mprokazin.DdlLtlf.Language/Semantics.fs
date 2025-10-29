module Mprokazin.DdlLtlf.Language.Semantics

open Mprokazin.DdlLtlf.Language.Ast
open System.Collections.Generic

type SemanticError =
    { Kind: string
      Name: string
      Path: string list
      Message: string }

let validate (_model: Program) : Result<unit, SemanticError list> =
    Ok ()
