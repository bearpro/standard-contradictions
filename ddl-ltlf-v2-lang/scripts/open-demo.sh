#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

"$ROOT/scripts/build-parser.sh"
nvim --clean --headless "$ROOT/.lazy.lua" '+silent! trust' '+qa!'
cd "$ROOT"
nvim sample.mdl
