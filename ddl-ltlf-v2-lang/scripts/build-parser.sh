#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
REPO_ROOT="$(cd "$ROOT/.." && pwd)"

if ! command -v node >/dev/null 2>&1 \
  || ! command -v tree-sitter >/dev/null 2>&1 \
  || ! command -v cc >/dev/null 2>&1; then
  if [[ "${MDL_LANG_IN_NIX:-}" != "1" ]]; then
    exec env MDL_LANG_IN_NIX=1 nix develop "path:$REPO_ROOT" -c "$0" "$@"
  fi

  echo "Missing node, tree-sitter, or cc even inside nix develop." >&2
  exit 1
fi

cd "$ROOT"
mkdir -p build parser
tree-sitter generate
cc -fPIC -I"$ROOT/src" -c "$ROOT/src/parser.c" -o "$ROOT/build/parser.o"
cc -shared "$ROOT/build/parser.o" -o "$ROOT/parser/mdl.so"
