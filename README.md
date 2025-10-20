# Standard contradiction analysis

Set of tool to check for contradictions and conflicts inside and between normative documents 

# Repository overview

- `ddl-ltlf/` - DSL that can represent normative statements as set of predicates and [deontic logic](https://en.wikipedia.org/wiki/Deontic_logic) formulas.  
  This folder contains both grammar, parser, semantic analyzer, runtime (solver) and shell tool for this language.
- `ddl-ltlf-from-text/` - Tool for generation `.mdl` (ddl-ltlf files) based on natural text, by using LLMs.
- `semantic-segmenter/` - Tool for splitting large text into semantically complete chunks.
- `embedding-clusters/` - my early attempt to find contradictions, based on the idea of somewhat similarity between contradictive statements.

# Components

- Language: mostly F#, also Python for some relatevely light tools
- Parsing: ANTLR4
- LLM integration: OpenAI API
- Solver backend: Z3 (via Microsoft.Z3)

# Status

This is early prototype - expect breaking changes.

## Running tests

- `dotnet test ddl-ltlf/src/Mprokazin.DdlLtlf.Tests` – parser and semantics tests that do not require the native Z3 library and can run in any environment.
- `dotnet test ddl-ltlf/src/Mprokazin.DdlLtlf.Tests.Solver` – solver integration tests that require Microsoft Z3 (native) to be available on the machine.
