# ddl-ltlf-v2-lang demo

This directory contains a small demo stack for the MDL syntax sketch:

- `grammar.js` is a minimal Tree-sitter grammar for `sample.mdl`.
- `queries/mdl/highlights.scm` provides Tree-sitter highlighting in Neovim.
- `lsp/server.py` is a tiny stdio LSP that publishes one demo warning.
- `.lazy.lua` wires the parser and LSP into your normal lazy.nvim config.

The `@ ...` lines are parsed and highlighted as source trace comments.

## Run the demo

From the repository root:

```sh
nix develop -c ddl-ltlf-v2-lang/scripts/open-demo.sh
```

The script builds the parser and opens:

```sh
ddl-ltlf-v2-lang/sample.mdl
```

from inside:

```sh
ddl-ltlf-v2-lang/
```

The current working directory matters: lazy.nvim loads `.lazy.lua` from the
project directory, so `open-demo.sh` changes into `ddl-ltlf-v2-lang` before
starting Neovim.

This expects lazy.nvim local specs to be enabled. In lazy.nvim this is the
default `local_spec = true` setting.

Neovim 0.12 treats `.lazy.lua` as local project code and requires trust before
loading it. `open-demo.sh` runs a headless `:trust` for this file before opening
the sample.

## Build only the parser

```sh
nix develop -c ddl-ltlf-v2-lang/scripts/build-parser.sh
```

If `node`, `tree-sitter`, or `cc` are missing from `PATH`, the script
re-executes itself through the repository root `nix develop`.

This generates ignored build output under:

```text
ddl-ltlf-v2-lang/src/
ddl-ltlf-v2-lang/build/
ddl-ltlf-v2-lang/parser/mdl.so
```

## LSP warning

The demo LSP reports warning `NL18` when a predicate used in a rule body does
not start with `satisfies`.

For the sample rule:

```mdl
rule O email_is_correct = email_is_correct(email) always
```

the LSP highlights the body call `email_is_correct` and reports:

```text
NL18 Predicates used in rule bodies must start with the word satisfies
```

## Headless check

```sh
nix develop -c bash -lc '
nvim --clean --headless "$PWD/ddl-ltlf-v2-lang/.lazy.lua" "+silent! trust" "+qa!"
cd ddl-ltlf-v2-lang
nvim --headless sample.mdl \
  "+sleep 800m" \
  "+lua print(\"filetype=\" .. vim.bo.filetype)" \
  "+lua local q=vim.treesitter.query.get(\"mdl\", \"highlights\"); print(\"highlight_query=\" .. tostring(q ~= nil))" \
  "+lua print(\"lsp_clients=\" .. #vim.lsp.get_clients({bufnr=0}))" \
  "+lua print(\"diagnostics=\" .. #vim.diagnostic.get(0))" \
  "+qa!"
'
```

Expected output includes:

```text
filetype=mdl
highlight_query=true
lsp_clients=1
diagnostics=1
```
