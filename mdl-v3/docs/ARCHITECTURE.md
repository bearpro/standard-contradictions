# Architecture

The toolkit is deliberately split into layers that mirror the language design.

```text
source .mdl
  -> lexer/parser
  -> public Python AST (`mdl.ast` dataclasses)
  -> formatter / linter / runtime / aligner
  -> DDL-LTLf core JSON
```

## Python-first model inference

The parser is not the only way to obtain a model. Any external process may create `mdl.ast.Module` directly or use `mdl.builder.ModelBuilder`. This supports pipelines where an LLM or a Python extraction script infers a normative model as Python objects first.

The canonical text syntax is produced by `mdl.printer.format_module`.

## Single binary policy

The project exposes one executable entry point: `mdl`.

Subcommands:

- `parse`: AST JSON;
- `format`: canonical MDL source;
- `lint`: diagnostics;
- `translate`: backend-agnostic DDL-LTLf core JSON;
- `run`: point-wise runtime for pure term expressions and facts;
- `align`: explicit and heuristic semantic alignments;
- `lsp`: stdio language server.

## Runtime scope

The runtime is point-wise and deterministic. It evaluates ordinary pure functions, records, lists, pattern matching and facts. It intentionally does not model full temporal semantics; temporal formulas are translated to the core layer and should be consumed by a dedicated DDL-LTLf/model-checking backend.
