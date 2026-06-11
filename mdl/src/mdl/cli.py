from __future__ import annotations

import argparse
import json
import os
import sys
from pathlib import Path
from typing import Any

from . import __version__, ast as A
from .aligner import AlignmentOptions, align_modules, render_alignment_module
from .diagnostics import MDLError, ParseError
from .linter import STDLIB_ENV, lint_source
from .lsp import run_stdio
from .parser import parse
from .printer import format_module
from .runtime import Runtime
from .solver import SolveOptions, solve_paths


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
    diagnostics = lint_source(read_file(args.file), path=args.file, stdlib_path=args.stdlib_path)
    if args.json:
        print(json.dumps([d.to_dict() for d in diagnostics], ensure_ascii=False, indent=2))
    else:
        for d in diagnostics:
            location = f"{d.path or args.file}:{d.line}:{d.column}"
            code = f" [{d.code}]" if d.code else ""
            print(f"{location}: {d.severity}{code}: {d.message}")
    return 1 if any(d.severity == "error" for d in diagnostics) else 0


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
    left, right = [parse(read_file(path)) for path in args.files]
    accept_threshold = args.threshold if args.threshold is not None else args.accept_threshold
    candidate_threshold = min(args.candidate_threshold, accept_threshold)
    options = AlignmentOptions(
        matcher=args.matcher,
        candidate_threshold=candidate_threshold,
        accept_threshold=accept_threshold,
    )
    report = align_modules(left, right, options)
    alignment_module = render_alignment_module(
        report,
        module_name=args.module_name,
    )
    source = format_module(alignment_module)
    payload = json.dumps(report.to_dict(), ensure_ascii=False, indent=2)

    if args.output:
        write_or_print(source, args.output)
    if args.report:
        write_or_print(payload + "\n", args.report)

    if args.json:
        print(payload)
    elif not args.output:
        print(source, end="")
    return 0


def cmd_solve(args: argparse.Namespace) -> int:
    payload = solve_paths(
        args.files,
        SolveOptions(
            horizon=args.horizon,
            max_horizon=args.max_horizon,
            permission=args.permission,
            max_conflicts=args.max_conflicts,
            stdlib_path=args.stdlib_path,
        ),
    )
    print(json.dumps(payload, ensure_ascii=False, indent=2, default=json_default))
    if payload["status"] == "sat":
        return 0
    if payload["status"] == "unsat":
        return 1
    return 2


def cmd_lsp(args: argparse.Namespace) -> int:
    if args.stdlib_path:
        os.environ[STDLIB_ENV] = args.stdlib_path
    return run_stdio()


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(prog="mdl", description="MDL / DDL-LTLf language toolkit")
    parser.add_argument("--version", action="version", version=f"mdl {__version__}")
    parser.add_argument("--stdlib", dest="stdlib_path", help="directory containing MDL stdlib modules")
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

    p = sub.add_parser("run", help="evaluate facts or a point-wise expression")
    p.add_argument("file")
    p.add_argument("--expr", help="expression to evaluate after applying facts")
    p.set_defaults(func=cmd_run)

    p = sub.add_parser("align", help="align two MDL modules and render an alignment module")
    p.add_argument("files", nargs=2)
    p.add_argument("--matcher", choices=["auto", "builtin", "bdikit:coma"], default="auto")
    p.add_argument("--candidate-threshold", type=float, default=0.55)
    p.add_argument("--accept-threshold", type=float, default=0.75)
    p.add_argument("--threshold", type=float, help=argparse.SUPPRESS)
    p.add_argument("--module-name")
    p.add_argument("-o", "--output")
    p.add_argument("--report")
    p.add_argument("--json", action="store_true", help="print the machine-readable alignment report")
    p.set_defaults(func=cmd_align)

    p = sub.add_parser("solve", help="solve DDL-LTLf constraints with Z3")
    p.add_argument("files", nargs="+")
    p.add_argument("--horizon", type=int, help="check exactly one finite trace horizon")
    p.add_argument("--max-horizon", type=int, default=3, help="search horizons 1..N when --horizon is not set")
    p.add_argument("--permission", choices=["strong", "ignore"], default="strong")
    p.add_argument("--max-conflicts", type=int, default=20)
    p.set_defaults(func=cmd_solve)

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
