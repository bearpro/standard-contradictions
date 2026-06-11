# Architecture

The toolkit is deliberately split into layers that mirror the language design.

```text
source .mdl
  -> lexer/parser
  -> public Python AST (`mdl.ast` dataclasses)
  -> formatter / linter / runtime / aligner
  -> bounded Z3 solver
```

## Python-first model inference

The parser is not the only way to obtain a model. Any external process may
create `mdl.ast.Module` directly or use `mdl.builder.ModelBuilder`. This
supports pipelines where an LLM or a Python extraction script infers a normative
model as Python objects first.

The canonical text syntax is produced by `mdl.printer.format_module`.

## Single binary policy

The project exposes one executable entry point: `mdl`.

Subcommands:

- `parse`: AST JSON;
- `format`: canonical MDL source;
- `lint`: diagnostics;
- `run`: point-wise runtime for pure term expressions and facts;
- `align`: heuristic semantic alignment module generation;
- `solve`: bounded DDL-LTLf solve via Z3, returning JSON conflicts or a witness
  trace;
- `lsp`: stdio language server.

## Runtime scope

The runtime is point-wise and deterministic. It evaluates ordinary pure
functions, records, tuples, stdlib collection constructors, pattern matching and
facts. It intentionally does not model full temporal semantics; temporal
formulas are consumed by the bounded solver / model-checking backend.

## Solver scope

The solver encodes a finite trace horizon into Z3. It accepts multiple modules,
resolves imports among the input modules, and treats alignment modules as
ordinary rules. Use `--horizon K` for one bounded check or `--max-horizon N` to
search `1..N`.
