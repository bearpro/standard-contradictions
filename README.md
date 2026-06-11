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

## Install from TestPyPI

`mdl` development builds are published to TestPyPI after successful `master`
builds. Install the latest test build with:

```bash
python -m pip install --extra-index-url https://test.pypi.org/simple/ mprokazin-mdl
mdl --version
```

Use `--extra-index-url` instead of replacing the main index so third-party
dependencies can still be resolved from PyPI.

## Install the VS Code extension

After a successful `master` build, GitHub Actions publishes a VSIX artifact from
the `VS Code VSIX` workflow. Download the `mdl-language-support-vsix-*` artifact
from the workflow run, unzip it, and install the contained `.vsix` file:

```bash
code --install-extension mdl-language-support-0.1.1-<commit>.vsix
```

The extension starts `mdl lsp` by default, so install the Python package first
and make sure the `mdl` command is available in the VS Code environment.

## Development

```bash
nix develop
cd mdl
python -m pip install -e '.[dev]'
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
mdl parse examples/pipe.mdl.py
mdl format examples/email.mdl
mdl lint examples/pipe.mdl.py
mdl translate examples/email.mdl
mdl run examples/pipe.mdl.py --expr 'pipe.length > 0'
mdl align examples/pipe.mdl.py examples/tube.mdl
mdl solve examples/pipe.mdl examples/tube.mdl examples/alignment.mdl --horizon 1
mdl lsp
```
