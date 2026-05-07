from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import Any

from . import __version__, ast as A
from .aligner import suggest_alignments
from .core import to_json, translate
from .diagnostics import MDLError, ParseError
from .linter import lint_source
from .lsp import run_stdio
from .parser import parse
from .printer import format_module
from .runtime import Runtime


def read_file(path: str) -> str:
    return Path(path).read_text(encoding="utf-8")


def write_or_print(text: str, output: str | None) -> None:
    if output:
        Path(output).write_text(text, encoding="utf-8")
    else:
        print(text, end="")


def cmd_parse(args: argparse.Namespace) -> int:
    module = parse(read_file(args.file))
    print(json.dumps(A.node_to_dict(module), ensure_ascii=False, indent=2, default=str))
    return 0


def cmd_format(args: argparse.Namespace) -> int:
    module = parse(read_file(args.file))
    text = format_module(module)
    write_or_print(text, args.output)
    return 0


def cmd_lint(args: argparse.Namespace) -> int:
    diagnostics = lint_source(read_file(args.file), path=args.file)
    if args.json:
        print(json.dumps([d.to_dict() for d in diagnostics], ensure_ascii=False, indent=2))
    else:
        for d in diagnostics:
            location = f"{d.path or args.file}:{d.line}:{d.column}"
            code = f" [{d.code}]" if d.code else ""
            print(f"{location}: {d.severity}{code}: {d.message}")
    return 1 if any(d.severity == "error" for d in diagnostics) else 0


def cmd_translate(args: argparse.Namespace) -> int:
    module = parse(read_file(args.file))
    text = to_json(module, indent=2)
    write_or_print(text + "\n", args.output)
    return 0


def json_default(value: Any) -> Any:
    try:
        return str(value)
    except Exception:  # pragma: no cover
        return repr(value)


def cmd_run(args: argparse.Namespace) -> int:
    module = parse(read_file(args.file))
    runtime = Runtime(module)
    if args.expr:
        value = runtime.eval_source_expr(args.expr)
        print(json.dumps(value, ensure_ascii=False, indent=2, default=json_default))
    else:
        print(json.dumps({"values": runtime.values, "facts": runtime.facts}, ensure_ascii=False, indent=2, default=json_default))
    return 0


def cmd_align(args: argparse.Namespace) -> int:
    modules = [parse(read_file(path)) for path in args.files]
    report = suggest_alignments(modules, threshold=args.threshold)
    if args.json:
        print(json.dumps(report.to_dict(), ensure_ascii=False, indent=2))
    else:
        if report.explicit:
            print("Explicit alignments:")
            for row in report.explicit:
                print(f"  {row['module']}: {row['subject']} -> {row['target']} ({row['kind']})")
        if report.suggestions:
            print("Suggestions:")
            for suggestion in report.suggestions:
                print(f"  {suggestion.left.qualified} ~= {suggestion.right.qualified}  score={suggestion.score:.3f}  {suggestion.reason}")
        if not report.explicit and not report.suggestions:
            print("No alignments found.")
    return 0


def cmd_lsp(args: argparse.Namespace) -> int:
    return run_stdio()


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(prog="mdl", description="MDL / DDL-LTLf language toolkit")
    parser.add_argument("--version", action="version", version=f"mdl {__version__}")
    sub = parser.add_subparsers(dest="command", required=True)

    p = sub.add_parser("parse", help="parse a file and dump AST JSON")
    p.add_argument("file")
    p.set_defaults(func=cmd_parse)

    p = sub.add_parser("format", help="format a file")
    p.add_argument("file")
    p.add_argument("-o", "--output")
    p.set_defaults(func=cmd_format)

    p = sub.add_parser("lint", help="lint a file")
    p.add_argument("file")
    p.add_argument("--json", action="store_true")
    p.set_defaults(func=cmd_lint)

    p = sub.add_parser("translate", help="translate a file to JSON-like DDL-LTLf core")
    p.add_argument("file")
    p.add_argument("-o", "--output")
    p.set_defaults(func=cmd_translate)

    p = sub.add_parser("run", help="evaluate facts or a point-wise expression")
    p.add_argument("file")
    p.add_argument("--expr", help="expression to evaluate after applying facts")
    p.set_defaults(func=cmd_run)

    p = sub.add_parser("align", help="print explicit alignments and heuristic suggestions")
    p.add_argument("files", nargs="+")
    p.add_argument("--threshold", type=float, default=0.78)
    p.add_argument("--json", action="store_true")
    p.set_defaults(func=cmd_align)

    p = sub.add_parser("lsp", help="run stdio language server")
    p.set_defaults(func=cmd_lsp)
    return parser


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    try:
        return args.func(args)
    except ParseError as exc:
        print(str(exc), file=sys.stderr)
        return 2
    except MDLError as exc:
        print(str(exc), file=sys.stderr)
        return 1


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
