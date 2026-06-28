from __future__ import annotations

import fnmatch
import hashlib
import json
import os
import shutil
import sys
import time
from dataclasses import dataclass
from datetime import UTC, datetime
from pathlib import Path
from typing import Any, Callable

from .cases import ContractNliCase
from .constants import BENCHMARK_NAME
from .mdl_tools import (
    MDL_VERIFY_TOOL,
    MDL_VERIFY_TOOL_NAME,
    call_mdl_verify_tool,
    is_successful_mdl_verify,
)
from .paths import artifact_paths, model_run_slug, scenario_run_slug
from .prompting import extract_mdl_source, render_prompt


@dataclass(frozen=True)
class GenerationResult:
    output_text: str
    response: dict[str, Any]
    verified_source: str | None = None
    tool_trace: tuple[dict[str, Any], ...] = ()
    tool_documents: tuple[dict[str, Any], ...] = ()


Generator = Callable[[str, str, dict[str, str]], GenerationResult]


TEXT_TOOL_OUTPUT_MESSAGE_MODEL_PATTERNS = [
    "openai/*",
    "anthropic/*",
    "minimax/*",
    "deepseek/*",
]


@dataclass(frozen=True)
class FunctionCall:
    name: str
    call_id: str
    arguments: str


@dataclass(frozen=True)
class InferenceConfig:
    data_root: Path
    model: str
    scenario: str
    scope: str
    system_prompt: str
    system_prompt_name: str
    user_prompt_template: str
    user_prompt_template_name: str
    base_url: str | None = None
    max_output_tokens: int = 9000
    temperature: float | None = None
    force: bool = False
    resume: bool = True
    progress: bool = False
    mdl_tools: bool = False
    mdl_tool_max_attempts: int = 4


@dataclass(frozen=True)
class ArtifactRecord:
    case_id: str
    mdl_path: Path
    raw_path: Path
    meta_path: Path
    skipped_existing: bool = False


class OpenAIResponsesGenerator:
    def __init__(
        self,
        model: str,
        api_key_env: str,
        base_url: str | None,
        max_output_tokens: int,
        temperature: float | None,
        mdl_tools: bool = False,
        mdl_tool_max_attempts: int = 4,
        client: Any | None = None,
    ):
        self.model = model
        self.api_key_env = api_key_env
        self.base_url = base_url
        self.max_output_tokens = max_output_tokens
        self.temperature = temperature
        self.mdl_tools = mdl_tools
        self.mdl_tool_max_attempts = mdl_tool_max_attempts
        self.client = client

    def __call__(
        self,
        system_prompt: str,
        user_prompt: str,
        metadata: dict[str, str],
    ) -> GenerationResult:
        client = self._client()
        kwargs: dict[str, Any] = {
            "model": self.model,
            "input": [
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt},
            ],
            "max_output_tokens": self.max_output_tokens,
            "metadata": metadata,
        }
        if self.temperature is not None:
            kwargs["temperature"] = self.temperature
        if self.mdl_tools:
            return self._call_with_mdl_tools(client, kwargs)

        response = client.responses.create(**kwargs)
        payload = _response_dump(response)
        return GenerationResult(
            output_text=_response_output_text(response, payload),
            response=payload,
        )

    def _client(self) -> Any:
        if self.client is not None:
            return self.client

        from openai import OpenAI

        api_key = os.environ.get(self.api_key_env)
        if not api_key:
            raise RuntimeError(f"missing API key in ${self.api_key_env}")
        return OpenAI(api_key=api_key, base_url=self.base_url)

    def _call_with_mdl_tools(
        self,
        client: Any,
        kwargs: dict[str, Any],
    ) -> GenerationResult:
        trace: list[dict[str, Any]] = []
        tool_documents: list[dict[str, Any]] = []
        responses: list[dict[str, Any]] = []
        last_text: str | None = None
        last_verified_source: str | None = None

        kwargs = {
            **kwargs,
            "input": [
                *kwargs["input"],
                {
                    "role": "user",
                    "content": (
                        "Before your final answer, call mdl_verify with the complete "
                        "MDL source. If diagnostics report errors, revise the source "
                        "and call mdl_verify again."
                    ),
                },
            ],
            "tools": [MDL_VERIFY_TOOL],
            "tool_choice": {"type": "function", "name": MDL_VERIFY_TOOL_NAME},
        }

        for attempt in range(1, max(1, self.mdl_tool_max_attempts) + 1):
            response = client.responses.create(**kwargs)
            payload = _response_dump(response)
            responses.append(payload)
            last_text = _response_output_text_or_none(response, payload) or last_text
            calls = _response_function_calls(payload)

            if not calls:
                trace.append(
                    {
                        "attempt": attempt,
                        "tool": None,
                        "ok": False,
                        "phase": "missing-tool-call",
                    }
                )
                kwargs = _next_tool_retry_kwargs(
                    kwargs,
                    payload,
                    "No mdl_verify call was returned. Call mdl_verify with source.",
                )
                continue

            call = calls[0]
            candidate = _tool_call_source(call.arguments)
            if candidate is not None:
                last_text = f"```mdl\n{candidate.strip()}\n```"
                tool_documents.append(
                    {
                        "attempt": attempt,
                        "tool": call.name,
                        "call_id": call.call_id,
                        "source": candidate,
                    }
                )
            if call.name == MDL_VERIFY_TOOL_NAME:
                tool_payload = call_mdl_verify_tool(call.arguments)
            else:
                tool_payload = _wrong_tool_payload(call.name)
            trace.append(
                {
                    "attempt": attempt,
                    "tool": call.name,
                    "call_id": call.call_id,
                    "ok": tool_payload.get("ok"),
                    "phase": tool_payload.get("phase"),
                    "summary": tool_payload.get("summary"),
                    "diagnostics": tool_payload.get("diagnostics", []),
                }
            )
            if is_successful_mdl_verify(tool_payload):
                formatted = tool_payload.get("formatted_source")
                if isinstance(formatted, str) and formatted.strip():
                    last_verified_source = formatted
                    last_text = f"```mdl\n{formatted.strip()}\n```"
                break

            kwargs = _next_tool_output_kwargs(kwargs, payload, call.call_id, tool_payload)

        if last_text is None:
            raise RuntimeError("model response did not contain output text")
        return GenerationResult(
            output_text=last_text,
            response={"responses": responses},
            verified_source=last_verified_source,
            tool_trace=tuple(trace),
            tool_documents=tuple(tool_documents),
        )


def infer_cases(
    cases: list[ContractNliCase],
    config: InferenceConfig,
    generator: Generator,
) -> list[ArtifactRecord]:
    records: list[ArtifactRecord] = []
    started_at = time.monotonic()
    started_at_iso = datetime.now(UTC).isoformat()
    total = len(cases)
    skipped_count = 0
    generated_count = 0
    system_prompt_sha = hashlib.sha256(config.system_prompt.encode("utf-8")).hexdigest()
    user_prompt_template_sha = hashlib.sha256(
        config.user_prompt_template.encode("utf-8")
    ).hexdigest()
    run_meta = _run_metadata(
        config,
        cases,
        started_at_iso=started_at_iso,
        completed_at=None,
        generated_count=0,
        skipped_existing=0,
        system_prompt_sha=system_prompt_sha,
        user_prompt_template_sha=user_prompt_template_sha,
    )
    run_root = _run_root(config.data_root, config.scenario)
    run_root.mkdir(parents=True, exist_ok=True)
    _write_json(run_root / "meta.json", run_meta)
    if config.progress:
        _print_infer_progress(0, total, skipped_count, generated_count, started_at)

    for index, case in enumerate(cases, start=1):
        paths = artifact_paths(
            config.data_root,
            config.scenario,
            case.doc_id,
            case.hypothesis_id,
        )
        if paths.mdl.exists() and config.resume and not config.force:
            records.append(
                ArtifactRecord(
                    case_id=case.case_id,
                    mdl_path=paths.mdl,
                    raw_path=paths.raw,
                    meta_path=paths.meta,
                    skipped_existing=True,
                )
            )
            skipped_count += 1
            if config.progress:
                _print_infer_progress(
                    index,
                    total,
                    skipped_count,
                    generated_count,
                    started_at,
                    status="skipped",
                    case_id=case.case_id,
                )
            continue

        user_prompt = render_prompt(config.user_prompt_template, case)
        user_prompt_sha = hashlib.sha256(user_prompt.encode("utf-8")).hexdigest()
        metadata = {
            "benchmark": BENCHMARK_NAME,
            "case_id": case.case_id,
            "scenario": scenario_run_slug(config.scenario),
        }
        result = generator(config.system_prompt, user_prompt, metadata)
        mdl_source = result.verified_source or extract_mdl_source(result.output_text)

        paths.case_root.mkdir(parents=True, exist_ok=True)
        paths.mdl.write_text(mdl_source, encoding="utf-8")
        paths.raw.write_text(result.output_text, encoding="utf-8")
        paths.user_prompt.write_text(user_prompt, encoding="utf-8")
        if result.tool_documents:
            _write_tool_documents(paths.case_root, result.tool_documents)
        if result.tool_trace:
            _write_json(
                paths.case_root / "tool_trace.json",
                {"trace": list(result.tool_trace)},
            )
        _write_json(
            paths.case_root / "meta.json",
            _case_metadata(case, user_prompt_sha, result),
        )
        records.append(
            ArtifactRecord(
                case_id=case.case_id,
                mdl_path=paths.mdl,
                raw_path=paths.raw,
                meta_path=paths.meta,
            )
        )
        generated_count += 1
        if config.progress:
            _print_infer_progress(
                index,
                total,
                skipped_count,
                generated_count,
                started_at,
                status="generated",
                case_id=case.case_id,
            )
    _write_json(
        run_root / "meta.json",
        _run_metadata(
            config,
            cases,
            started_at_iso=started_at_iso,
            completed_at=datetime.now(UTC).isoformat(),
            generated_count=generated_count,
            skipped_existing=skipped_count,
            system_prompt_sha=system_prompt_sha,
            user_prompt_template_sha=user_prompt_template_sha,
        ),
    )
    return records


def _run_root(data_root: Path, scenario: str) -> Path:
    return data_root / "generated" / scenario_run_slug(scenario)


def _run_metadata(
    config: InferenceConfig,
    cases: list[ContractNliCase],
    *,
    started_at_iso: str,
    completed_at: str | None,
    generated_count: int,
    skipped_existing: int,
    system_prompt_sha: str,
    user_prompt_template_sha: str,
) -> dict[str, Any]:
    splits = sorted({case.split for case in cases})
    return {
        "benchmark": BENCHMARK_NAME,
        "started_at": started_at_iso,
        "completed_at": completed_at,
        "model": config.model,
        "model_slug": model_run_slug(config.model),
        "scope": config.scope,
        "splits": splits,
        "case_count": len(cases),
        "generated_count": generated_count,
        "skipped_existing": skipped_existing,
        "scenario": config.scenario,
        "scenario_slug": scenario_run_slug(config.scenario),
        "system_prompt": config.system_prompt_name,
        "system_prompt_sha256": system_prompt_sha,
        "user_prompt_template": config.user_prompt_template_name,
        "user_prompt_template_sha256": user_prompt_template_sha,
        "base_url": config.base_url,
        "max_output_tokens": config.max_output_tokens,
        "temperature": config.temperature,
        "force": config.force,
        "resume": config.resume,
        "mdl_tools": config.mdl_tools,
        "mdl_tool_max_attempts": config.mdl_tool_max_attempts,
    }


def _case_metadata(
    case: ContractNliCase,
    user_prompt_sha: str,
    result: GenerationResult | None = None,
) -> dict[str, Any]:
    metadata = {
        "case_id": case.case_id,
        "split": case.split,
        "doc_id": case.doc_id,
        "hypothesis_id": case.hypothesis_id,
        "module_name": case.module_name,
        "user_prompt_sha256": user_prompt_sha,
    }
    if result is None or not result.tool_trace:
        return metadata

    final = result.tool_trace[-1]
    metadata.update(
        {
            "mdl_tools": True,
            "mdl_tool_attempts": len(result.tool_trace),
            "mdl_tool_final_ok": final.get("ok"),
            "mdl_tool_final_phase": final.get("phase"),
        }
    )
    return metadata


def _write_tool_documents(
    case_root: Path,
    tool_documents: tuple[dict[str, Any], ...],
) -> None:
    output_root = case_root / "tool_inputs"
    output_root.mkdir(parents=True, exist_ok=True)
    for index, document in enumerate(tool_documents, start=1):
        source = document.get("source")
        if not isinstance(source, str):
            continue
        attempt = document.get("attempt")
        attempt_number = attempt if isinstance(attempt, int) and attempt > 0 else index
        tool = document.get("tool")
        tool_name = tool if isinstance(tool, str) and tool else "tool"
        filename = f"attempt-{attempt_number:03d}-{_safe_artifact_name(tool_name)}.mdl"
        (output_root / filename).write_text(source, encoding="utf-8")


def _safe_artifact_name(value: str) -> str:
    safe = "".join(char if char.isalnum() or char in "._-" else "-" for char in value)
    return safe.strip("-._") or "tool"


def _print_infer_progress(
    completed: int,
    total: int,
    skipped: int,
    generated: int,
    started_at: float,
    status: str | None = None,
    case_id: str | None = None,
) -> None:
    elapsed = time.monotonic() - started_at
    eta = _eta_seconds(elapsed, completed, total)
    message = _render_infer_progress(
        completed=completed,
        total=total,
        skipped=skipped,
        generated=generated,
        elapsed=elapsed,
        eta=eta,
        status=status,
        case_id=case_id,
        width=_progress_bar_width(),
    )
    if sys.stderr.isatty():
        end = "\n" if completed >= total else ""
        print(f"\r{message}", file=sys.stderr, end=end, flush=True)
        return

    print(message, file=sys.stderr, flush=True)


def _render_infer_progress(
    completed: int,
    total: int,
    skipped: int,
    generated: int,
    elapsed: float,
    eta: float | None,
    status: str | None = None,
    case_id: str | None = None,
    width: int = 28,
) -> str:
    ratio = 1.0 if total <= 0 else min(1.0, max(0.0, completed / total))
    filled = round(ratio * width)
    bar = "#" * filled + "-" * (width - filled)
    percent = round(ratio * 100)
    action = _progress_action(status, case_id)
    return (
        f"infer [{bar}] {percent:3d}%"
        f" {completed}/{total}"
        f" | gen {generated}"
        f" | skip {skipped}"
        f" | elapsed {_format_duration(elapsed)}"
        f" | eta {_format_duration(eta) if eta is not None else '--:--'}"
        f"{action}"
    )


def _eta_seconds(elapsed: float, completed: int, total: int) -> float | None:
    if completed <= 0 or total <= completed:
        return 0.0 if total <= completed else None
    return elapsed / completed * (total - completed)


def _format_duration(seconds: float) -> str:
    rounded = int(seconds)
    hours, remainder = divmod(rounded, 3600)
    minutes, secs = divmod(remainder, 60)
    if hours:
        return f"{hours:d}:{minutes:02d}:{secs:02d}"
    return f"{minutes:d}:{secs:02d}"


def _progress_action(status: str | None, case_id: str | None) -> str:
    if status is None and case_id is None:
        return ""
    if status is None:
        return f" | {case_id}"
    if case_id is None:
        return f" | {status}"
    return f" | {status} {case_id}"


def _progress_bar_width() -> int:
    columns = shutil.get_terminal_size(fallback=(100, 24)).columns
    return max(16, min(36, columns // 4))


def _write_json(path: Path, payload: dict[str, Any]) -> None:
    path.write_text(
        json.dumps(payload, ensure_ascii=False, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )


def _response_dump(response: Any) -> dict[str, Any]:
    if hasattr(response, "model_dump"):
        return response.model_dump(mode="json", exclude_none=True)
    if isinstance(response, dict):
        return response
    return {"repr": repr(response)}


def _response_output_text(response: Any, payload: dict[str, Any]) -> str:
    text = _response_output_text_or_none(response, payload)
    if text is not None:
        return text
    raise RuntimeError("model response did not contain output text")


def _response_output_text_or_none(response: Any, payload: dict[str, Any]) -> str | None:
    output_text = getattr(response, "output_text", None)
    if isinstance(output_text, str) and output_text.strip():
        return output_text

    parts: list[str] = []
    for item in payload.get("output", []):
        if not isinstance(item, dict):
            continue
        for content in item.get("content", []):
            if isinstance(content, dict) and content.get("type") == "output_text":
                text = content.get("text")
                if isinstance(text, str):
                    parts.append(text)
    if parts:
        return "".join(parts)
    return None


def _response_function_calls(payload: dict[str, Any]) -> list[FunctionCall]:
    calls: list[FunctionCall] = []
    for item in payload.get("output", []):
        if not isinstance(item, dict):
            continue
        call = _function_call_from_item(item)
        if call is not None:
            calls.append(call)
    return calls


def _function_call_from_item(item: dict[str, Any]) -> FunctionCall | None:
    if item.get("type") != "function_call":
        return None
    name = item.get("name")
    call_id = item.get("call_id") or item.get("id")
    arguments = item.get("arguments", "{}")
    if not isinstance(name, str) or not isinstance(call_id, str):
        return None
    if not isinstance(arguments, str):
        arguments = json.dumps(arguments, ensure_ascii=False)
    return FunctionCall(name=name, call_id=call_id, arguments=arguments)


def _tool_call_source(arguments: str) -> str | None:
    try:
        payload = json.loads(arguments)
    except json.JSONDecodeError:
        return None
    source = payload.get("source")
    return source if isinstance(source, str) and source.strip() else None


def _next_tool_output_kwargs(
    kwargs: dict[str, Any],
    response_payload: dict[str, Any],
    call_id: str,
    tool_payload: dict[str, Any],
) -> dict[str, Any]:
    output = json.dumps(tool_payload, ensure_ascii=False)
    input_items = [
        {
            "type": "function_call_output",
            "call_id": call_id,
            "output": output,
        }
    ]
    if _needs_text_tool_output_message(str(kwargs.get("model", ""))):
        input_items.append({"role": "user", "content": output})

    return _next_response_kwargs(
        kwargs,
        response_payload,
        input_items,
    )


def _next_tool_retry_kwargs(
    kwargs: dict[str, Any],
    response_payload: dict[str, Any],
    message: str,
) -> dict[str, Any]:
    return _next_response_kwargs(
        kwargs,
        response_payload,
        [{"role": "user", "content": message}],
    )


def _next_response_kwargs(
    kwargs: dict[str, Any],
    response_payload: dict[str, Any],
    input_items: list[dict[str, Any]],
) -> dict[str, Any]:
    keep = {
        "model",
        "max_output_tokens",
        "metadata",
        "temperature",
        "tools",
        "tool_choice",
    }
    next_kwargs = {key: value for key, value in kwargs.items() if key in keep}
    previous_response_id = response_payload.get("id")
    if isinstance(previous_response_id, str) and previous_response_id:
        next_kwargs["previous_response_id"] = previous_response_id
    next_kwargs["input"] = input_items
    return next_kwargs


def _needs_text_tool_output_message(model: str) -> bool:
    normalized = model.lower()
    return any(
        fnmatch.fnmatchcase(normalized, pattern)
        for pattern in TEXT_TOOL_OUTPUT_MESSAGE_MODEL_PATTERNS
    )


def _wrong_tool_payload(name: str) -> dict[str, Any]:
    return {
        "ok": False,
        "input_kind": "mdl",
        "phase": "input",
        "module": None,
        "diagnostics": [
            {
                "message": f"unsupported tool call {name!r}; call {MDL_VERIFY_TOOL_NAME}",
                "line": 1,
                "column": 1,
                "severity": "error",
                "code": "unsupported-tool",
                "path": None,
            }
        ],
        "formatted_source": None,
        "generated_source": None,
        "solver": None,
        "summary": {"errors": 1, "warnings": 0, "solver_status": None, "conflicts": 0},
    }
