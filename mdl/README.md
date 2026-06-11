# MDL / DDL-LTLf Toolkit

`mdl` is a Python implementation scaffold for an ML-inspired language used as an
intermediate representation for normative provisions. The language separates
ordinary pure computations, LTLf temporal formulas, and defeasible deontic
rules.

The repository contains:

- one command-line utility: `mdl`;
- a parser and typed AST dataclasses;
- a pretty-printer / formatter;
- a bounded Z3 solver for DDL-LTLf consistency checks;
- a small deterministic runtime for evaluating pure functions and facts;
- a linter;
- a minimal stdio LSP server with diagnostics and completions for keywords,
  visible names, type names and record fields;
- a semantic aligner;
- tests for the language constructs.

## Install locally

```bash
python -m pip install -e .
```

For a system-style command from this checkout:

```bash
make install
make uninstall
```

By default this runs `UV_PYTHON=3.12 uv tool install --editable .`, which
installs the `mdl` console script into uv's tool directory while keeping this
checkout editable. Override `UV_PYTHON` if needed:

```bash
make install UV_PYTHON=3.11
```

## CLI

```bash
mdl parse examples/email.mdl
mdl parse examples/pipe.mdl.py
mdl format examples/email.mdl
mdl lint examples/pipe.mdl.py
mdl run examples/email.mdl --expr 'email_is_correct(email)'
mdl align examples/email.mdl examples/pipe.mdl
mdl solve examples/pipe.mdl examples/tube.mdl examples/alignment.mdl --horizon 1
mdl lsp
```

## Python-side model construction

The recommended Python front-end is a static Python-syntax DSL. It lets
LLM-assisted and external inference pipelines emit ordinary-looking Python with
decorators while still lowering to the canonical MDL AST before formatting,
linting, running or solving.

```python
from mdl.dsl import *

module("pipe_spec")

@record
class Pipe:
    length: Rat
    radius: Rat


pipe = entity(Pipe)


@rule(O)
def pipe_length_positive():
    return always(pipe.length > 0)

fact(pipe == Pipe(length=10, radius=2))
```

Compile this source without executing arbitrary Python:

```python
from mdl.dsl import compile_file
from mdl.printer import format_module

model = compile_file("examples/pipe.mdl.py")
print(format_module(model))
```

The generated MDL is still the canonical syntax:

```plain
module pipe_spec

type Pipe = { length: rat, radius: rat }

entity pipe: Pipe

rule O pipe_length_positive: pipe.length > 0 always

fact pipe = Pipe { length = 10, radius = 2 }
```

The lower-level `mdl.builder` API remains available for callers that already
construct `mdl.ast` objects directly.

## Editor Integrations

Editor integration assets are kept outside the Python package:

- VS Code extension: `../editor-support/vscode-extension/`
- Tree-sitter grammar scaffold: `../editor-support/tree-sitter-grammar/`

The Tree-sitter grammar is a scaffold suitable for editor integration and
incremental parsing. The authoritative parser in this repository is the Python
parser under `src/mdl/parser.py`.

## Current status

This is a deliberately compact but working project skeleton. It is suitable for
evolving the language design, writing tests, and wiring external DDL-LTLf
backends. The parser supports the main constructs used in the draft examples:
modules, imports, annotations, ADTs, record types, functions, `let`, `if`,
`case`, entities, `rule O name: ...` declarations, priorities, facts, temporal
operators and deontic modalities. Names are checked in declaration order, with
self-recursive function bodies supported.

### Note on python `mdl.dsl` supported features

| feature                          | `.mdl` status | `.mdl.py` status                  | Note                                                                                                                                                                                                                                                                                     |
| -------------------------------- | ------------- | --------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Modules                          | full support  | full support                      | `.mdl.py` uses `module("name")`.                                                                                                                                                                                                                                                         |
| Module/declaration annotations   | full support  | partial support                   | `.mdl` supports annotations before modules/imports/opens/declarations. `.mdl.py` exposes module annotations via `module(..., annotations=[...])` and some decorator metadata, but not a general annotation surface for every declaration.                                                |
| Imports                          | full support  | partial support                   | `.mdl.py` can emit imports via `import_("...")`, but normal Python `import`/`from` statements are ignored by the DSL compiler. Cross-front-end import resolution is not first-class yet; compile/import modules consistently or compile Python-syntax modules to canonical `.mdl` first. |
| Opens / namespace imports        | full support  | full support                      | `.mdl.py` supports `open_("module.name")`; external module resolution follows the same caveats as imports.                                                                                                                                                                               |
| Primitive types                  | full support  | full support                      | `Bool`, `Int`, `Rat`, `Decimal`, `String`, and `Unit` are exposed in the Python DSL.                                                                                                                                                                                                     |
| Primitive literals               | full support  | partial support                   | Python literals cover unit/`None`, bool, int, decimal/float, and string. Exact `.mdl` rational literal syntax such as `1/2` has no direct Python literal equivalent and is lowered as a division expression if written as `1 / 2`.                                                       |
| Records                          | full support  | full support                      | `.mdl.py` uses `@record class ...` with annotated fields. Field defaults and methods are intentionally rejected.                                                                                                                                                                         |
| Record construction              | full support  | full support                      | `.mdl.py` uses `TypeName(field=value, ...)`. Keyword calls are reserved for record constructors.                                                                                                                                                                                         |
| Field access                     | full support  | full support                      | `.mdl.py` uses normal attribute syntax, e.g. `pipe.kind`.                                                                                                                                                                                                                                |
| Sum types / ADTs                 | full support  | no support                        | No Python DSL syntax/builder exists for MDL variants or `SumType` declarations yet.                                                                                                                                                                                                      |
| Generic type declarations        | full support  | no support                        | `.mdl` can declare generic types such as `type Box<T> = ...`; `.mdl.py` records/type declarations cannot declare type parameters.                                                                                                                                                        |
| Generic type references          | full support  | partial support                   | `.mdl.py` can reference generic types with subscript syntax, e.g. `List[String]`, but cannot declare new generic types/functions.                                                                                                                                                        |
| Tuple types and tuple literals   | full support  | full support                      | Unit and 2+ tuples are supported. Single-item tuples are intentionally rejected.                                                                                                                                                                                                         |
| Top-level values                 | full support  | full support                      | `.mdl` uses `let`; `.mdl.py` uses assignment, annotated assignment, or `value(...)`.                                                                                                                                                                                                     |
| Entities                         | full support  | full support                      | `.mdl.py` supports assignment-style entities and decorator-style entities.                                                                                                                                                                                                               |
| Functions                        | full support  | full support for simple functions | `.mdl.py` supports `@function def f(x: T) -> U: ...` with typed positional parameters.                                                                                                                                                                                                   |
| Predicates                       | full support  | full support                      | `.mdl.py` supports `@predicate`; the return type defaults to `bool`.                                                                                                                                                                                                                     |
| Polymorphic functions            | full support  | no support                        | `.mdl.py` cannot declare function type parameters.                                                                                                                                                                                                                                       |
| Pattern parameters               | full support  | no support                        | `.mdl` function parameters are patterns. `.mdl.py` only supports simple variable parameters.                                                                                                                                                                                             |
| Local `let` / function blocks    | full support  | partial support                   | `.mdl.py` lowers local assignments to `let`, but only simple name targets and a restricted statement subset are supported.                                                                                                                                                               |
| `if` expressions                 | full support  | partial support                   | `.mdl.py` supports Python ternary expressions and restricted `if` statements whose branches return expressions.                                                                                                                                                                          |
| `case` / match expressions       | full support  | no support                        | Python `match` is not lowered to MDL `case`.                                                                                                                                                                                                                                             |
| Patterns and guards              | full support  | no support                        | `.mdl` supports wildcard, literal, tuple, record, and constructor patterns plus `when` guards. `.mdl.py` has no pattern matching surface yet.                                                                                                                                            |
| Function calls                   | full support  | full support                      | Positional calls lower directly. Keyword calls are unsupported except for record constructors.                                                                                                                                                                                           |
| Arithmetic operators             | full support  | full support                      | `+`, `-`, `*`, `/`, and `%` are supported.                                                                                                                                                                                                                                               |
| Comparisons                      | full support  | full support                      | Python `==` lowers to MDL `=`, `!=` lowers to `!=`, and chained comparisons lower through `and`.                                                                                                                                                                                         |
| Boolean logic                    | full support  | full support                      | `.mdl.py` supports `and`, `or`, and `not`.                                                                                                                                                                                                                                               |
| Implication                      | full support  | full support                      | `.mdl.py` uses `implies(a, b)`.                                                                                                                                                                                                                                                          |
| Temporal operators               | full support  | full support                      | `.mdl.py` uses `always(...)`, `eventually(...)`, `initially(...)`, `next_(...)`, and `until(a, b)`.                                                                                                                                                                                      |
| Deontic modalities               | full support  | full support                      | `.mdl.py` rules use `@rule(O)`, `@rule(P)`, `@rule(F)`, or `modality=...`.                                                                                                                                                                                                               |
| Rule strength                    | full support  | full support                      | `.mdl.py` supports `strength="strict"`, `strength="defeasible"`, and `strength="defeater"`; default is defeasible.                                                                                                                                                                       |
| Rule antecedent / otherwise      | full support  | full support                      | `.mdl.py` supports `when=...` / `antecedent=...` and `otherwise=...` on `@rule`.                                                                                                                                                                                                         |
| Anonymous rules                  | full support  | no support                        | `.mdl.py` rules are Python functions, so they always have a Python function/declaration name.                                                                                                                                                                                            |
| Rule priorities / override       | full support  | no support                        | No Python DSL surface exists for `override a > b`.                                                                                                                                                                                                                                       |
| Facts                            | full support  | full support                      | `.mdl.py` supports `fact(expr)` and `fact(target="name", value=expr)`.                                                                                                                                                                                                                   |
| Comments                         | full support  | full support                      | `.mdl` supports `#` comments. `.mdl.py` uses normal Python comments; docstrings are ignored by the static compiler.                                                                                                                                                                      |
| Direct CLI/lint/solve file input | full support  | no support                        | Current CLI/solver paths parse files as canonical `.mdl`. `.mdl.py` should be compiled to canonical MDL/AST first.                                                                                                                                                                       |
