# MDL / DDL-LTLf Toolkit

This repository contains the Python implementation of `mdl`, an ML-inspired
language used as an intermediate representation for normative provisions. The
language separates ordinary pure computations, LTLf temporal formulas, and
defeasible deontic rules.

The project lives in [`mdl/`](mdl/):

- command-line utility: `mdl`;
- parser and typed AST dataclasses;
- pretty-printer / formatter;
- translator to a JSON-like DDL-LTLf core;
- bounded Z3 solver for consistency checks;
- deterministic runtime for evaluating pure functions and facts;
- linter and stdio LSP server;
- semantic aligner;
- tests for the core language constructs.

The MCP adapter lives in [`mdl-mcp/`](mdl-mcp/). It exposes `mdl-mcp` for
LLM-assisted verification without adding MCP server dependencies to the core
`mdl` package.

Editor integrations live under [`editor-support/`](editor-support/):

- [`editor-support/vscode-extension/`](editor-support/vscode-extension/) - VS Code extension with TextMate highlighting and LSP wiring;
- [`editor-support/tree-sitter-grammar/`](editor-support/tree-sitter-grammar/) - Tree-sitter grammar scaffold.

## Development

```bash
nix develop
cd mdl
python -m pip install -e .
pytest
```

Build the VS Code extension from its own directory:

```bash
cd editor-support/vscode-extension
make build
```

Useful CLI examples:

```bash
cd mdl
mdl parse examples/email.mdl
mdl format examples/email.mdl
mdl lint examples/email.mdl
mdl translate examples/email.mdl
mdl run examples/email.mdl --expr 'email_is_correct(email)'
mdl align examples/email.mdl examples/pipe.mdl
mdl solve examples/pipe.mdl examples/tube.mdl examples/alignment.mdl --horizon 1
mdl lsp
```
