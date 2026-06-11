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
mdl format examples/email.mdl
mdl lint examples/email.mdl
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

model = compile_file("examples/pipe.py")
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
