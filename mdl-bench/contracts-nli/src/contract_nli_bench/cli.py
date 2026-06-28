from __future__ import annotations

import argparse
import json
import os
import sys
from pathlib import Path

from dotenv import load_dotenv

from .cases import resolve_cases
from .constants import RUN_SCOPES
from .download import download_dataset
from .evaluation import evaluate_cases, validate_file
from .paths import (
    BENCH_ROOT,
    DEFAULT_DATA_ROOT,
    DEFAULT_SYSTEM_PROMPT,
    DEFAULT_USER_TEMPLATE,
    artifact_paths,
)
from .prompting import read_prompt_file
from .runner import InferenceConfig, OpenAIResponsesGenerator, infer_cases


def cmd_download(args: argparse.Namespace) -> int:
    archive = download_dataset(Path(args.data_root), accept_terms=args.accept_terms)
    print(str(archive))
    return 0


def cmd_infer(args: argparse.Namespace) -> int:
    if args.mdl_tool_max_attempts < 1:
        raise ValueError("--mdl-tool-max-attempts must be at least 1")

    data_root = Path(args.data_root)
    cases = _resolve_args_cases(args, data_root)
    if args.dry_run:
        print(json.dumps({"cases": len(cases)}, indent=2))
        return 0

    system_prompt_path = Path(args.system_prompt)
    user_template_path = Path(args.user_template)
    config = InferenceConfig(
        data_root=data_root,
        model=args.model,
        scenario=args.scenario,
        scope=args.scope,
        system_prompt=read_prompt_file(system_prompt_path),
        system_prompt_name=str(system_prompt_path),
        user_prompt_template=read_prompt_file(user_template_path),
        user_prompt_template_name=str(user_template_path),
        base_url=_base_url(args),
        max_output_tokens=args.max_output_tokens,
        temperature=args.temperature,
        force=args.force,
        resume=args.resume,
        progress=args.progress,
        mdl_tools=args.mdl_tools,
        mdl_tool_max_attempts=args.mdl_tool_max_attempts,
    )
    generator = OpenAIResponsesGenerator(
        model=args.model,
        api_key_env=args.api_key_env,
        base_url=_base_url(args),
        max_output_tokens=args.max_output_tokens,
        temperature=args.temperature,
        mdl_tools=args.mdl_tools,
        mdl_tool_max_attempts=args.mdl_tool_max_attempts,
    )
    records = infer_cases(cases, config, generator)
    print(
        json.dumps(
            {
                "cases": len(records),
                "skipped_existing": sum(1 for item in records if item.skipped_existing),
            },
            indent=2,
        )
    )
    return 0


def cmd_validate(args: argparse.Namespace) -> int:
    data_root = Path(args.data_root)
    cases = _resolve_args_cases(args, data_root)
    records = []
    for case in cases:
        paths = artifact_paths(
            data_root,
            args.scenario,
            case.doc_id,
            case.hypothesis_id,
        )
        parse_ok, lint_error_count, error = validate_file(paths.mdl)
        records.append(
            {
                "case_id": case.case_id,
                "artifact": str(paths.mdl.relative_to(data_root)),
                "parse_ok": parse_ok,
                "lint_error_count": lint_error_count,
                "error": error,
            }
        )
    summary = {
        "cases": len(records),
        "parse_errors": sum(1 for item in records if not item["parse_ok"]),
        "lint_error_cases": sum(
            1 for item in records if item["lint_error_count"] > 0
        ),
    }
    print(json.dumps(summary, ensure_ascii=False, indent=2, sort_keys=True))
    return 1 if summary["parse_errors"] or summary["lint_error_cases"] else 0


def cmd_evaluate(args: argparse.Namespace) -> int:
    data_root = Path(args.data_root)
    cases = _resolve_args_cases(args, data_root)
    _, summary = evaluate_cases(
        cases,
        data_root=data_root,
        scenario=args.scenario,
        horizon=args.horizon,
        write=True,
    )
    print(json.dumps(summary, ensure_ascii=False, indent=2, sort_keys=True))
    return 0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="contract-nli-bench",
        description="Contract-NLI benchmark for prompt-driven MDL generation.",
    )
    parser.add_argument("--data-root", default=str(DEFAULT_DATA_ROOT))
    sub = parser.add_subparsers(dest="command", required=True)

    p = sub.add_parser("download", help="download the upstream Contract-NLI archive")
    p.add_argument("--accept-terms", action="store_true")
    p.set_defaults(func=cmd_download)

    p = sub.add_parser("infer", help="generate MDL artifacts through an LLM API")
    _add_scope_arg(p)
    _add_run_args(p)
    p.add_argument("--api-key-env", default="OPENAI_API_KEY")
    p.add_argument("--base-url")
    p.add_argument("--max-output-tokens", type=int, default=9000)
    p.add_argument("--temperature", type=float)
    p.add_argument("--system-prompt", default=str(DEFAULT_SYSTEM_PROMPT))
    p.add_argument("--user-template", default=str(DEFAULT_USER_TEMPLATE))
    p.add_argument("--force", action="store_true")
    p.add_argument("--no-resume", action="store_false", dest="resume")
    p.add_argument("--no-progress", action="store_false", dest="progress")
    p.add_argument("--mdl-tools", action="store_true")
    p.add_argument("--mdl-tool-max-attempts", type=int, default=4)
    p.add_argument("--dry-run", action="store_true")
    p.set_defaults(func=cmd_infer, resume=True, progress=True)

    p = sub.add_parser("validate", help="parse and lint persisted MDL artifacts")
    _add_scope_arg(p)
    p.add_argument("--scenario", required=True)
    p.set_defaults(func=cmd_validate)

    p = sub.add_parser("evaluate", help="solve persisted MDL artifacts")
    _add_scope_arg(p)
    p.add_argument("--scenario", required=True)
    p.add_argument("--horizon", type=int, default=1)
    p.set_defaults(func=cmd_evaluate)
    return parser


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    try:
        load_dotenv(BENCH_ROOT.parents[1] / ".env", override=False)
        load_dotenv(BENCH_ROOT / ".env", override=False)
        return args.func(args)
    except Exception as exc:
        print(str(exc), file=sys.stderr)
        return 2


def _add_run_args(parser: argparse.ArgumentParser) -> None:
    parser.add_argument("--model", required=True)
    parser.add_argument("--scenario", required=True)


def _add_scope_arg(parser: argparse.ArgumentParser) -> None:
    parser.add_argument("scope", choices=RUN_SCOPES)
    parser.add_argument("--case-id")


def _resolve_args_cases(args: argparse.Namespace, data_root: Path):
    return resolve_cases(
        data_root=data_root,
        scope=args.scope,
        case_id=args.case_id,
    )


def _base_url(args: argparse.Namespace) -> str | None:
    return args.base_url or os.environ.get("OPENAI_BASE_URL") or None


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
