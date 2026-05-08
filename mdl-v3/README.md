# MDL / DDL-LTLf Toolkit

`mdl` is a Python implementation scaffold for an ML-inspired language used as an intermediate representation for normative provisions. The language separates ordinary pure computations, LTLf temporal formulas, and defeasible deontic rules.

The repository contains:

- one command-line utility: `mdl`;
- a parser and typed AST dataclasses;
- a pretty-printer / formatter;
- a translator to a JSON-like DDL-LTLf core;
- a bounded Z3 solver for DDL-LTLf consistency checks;
- a small deterministic runtime for evaluating pure functions and facts;
- a linter;
- a minimal stdio LSP server;
- a semantic aligner;
- VS Code TextMate grammar and language configuration;
- Tree-sitter grammar scaffold;
- tests for the core language constructs.

## Install locally

```bash
python -m pip install -e .
```

## CLI

```bash
mdl parse examples/email.mdl
mdl format examples/email.mdl
mdl lint examples/email.mdl
mdl translate examples/email.mdl
mdl run examples/email.mdl --expr 'email_is_correct(email)'
mdl align examples/email.mdl examples/pipe.mdl
mdl solve examples/pipe.mdl examples/tube.mdl examples/alignment.mdl --horizon 1
mdl lsp
```

## Python-side model construction

The internal AST is intentionally public. A model can be built directly in Python and then printed back to MDL syntax.

```python
from mdl.builder import ModelBuilder, ref, call, always

m = ModelBuilder("email")
m.entity("email", "string")
m.rule(
    name="email_addr_spec_correct",
    modality="O",
    body=always(call("email_is_correct", ref("email"))),
)

print(m.to_source())
```

This path is meant for LLM-assisted or external inference pipelines where the model is first represented as Python objects and only then converted to the canonical textual syntax.

## Grammar locations

- VS Code grammar: `grammars/vscode/`
- Tree-sitter grammar: `grammars/tree-sitter-mdl/`

The Tree-sitter grammar is a scaffold suitable for editor integration and incremental parsing. The authoritative parser in this repository is the Python parser under `src/mdl/parser.py`.

## Current status

This is a deliberately compact but working project skeleton. It is suitable for evolving the language design, writing tests, and wiring external DDL-LTLf backends. The parser supports the main constructs used in the draft examples: modules, imports, annotations, ADTs, record types, functions, `let`, `if`, `case`, entities, events, rules, priorities, facts, assertions, alignments, temporal operators and deontic modalities.
