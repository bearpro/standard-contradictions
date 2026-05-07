# MDL / DDL-LTLf Language Support

VS Code extension assets for MDL normative models.

## Features

- TextMate syntax highlighting for `.mdl` files.
- MDL language configuration for comments, brackets, pairs and indentation.
- LSP diagnostics through the `mdl lsp` stdio server.

## LSP setup

The extension starts `mdl lsp` by default. Install the Python package so the `mdl`
command is available in the VS Code environment:

```bash
python -m pip install -e /path/to/mdl-v3
```

Override `mdl.lsp.command`, `mdl.lsp.args` or `mdl.lsp.cwd` in VS Code settings if
the server must be started from a specific virtual environment.
