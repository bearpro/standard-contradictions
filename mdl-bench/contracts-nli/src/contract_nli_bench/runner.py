from __future__ import annotations

import hashlib
import json
import os
from dataclasses import dataclass
from datetime import UTC, datetime
from pathlib import Path
from typing import Any, Callable

from .cases import ContractNliCase
from .constants import BENCHMARK_NAME
from .paths import artifact_paths, model_run_slug, scenario_run_slug
from .prompting import extract_mdl_source, render_prompt


@dataclass(frozen=True)
class GenerationResult:
    output_text: str
    response: dict[str, Any]


Generator = Callable[[str, str, dict[str, str]], GenerationResult]


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
    ):
        self.model = model
        self.api_key_env = api_key_env
        self.base_url = base_url
        self.max_output_tokens = max_output_tokens
        self.temperature = temperature

    def __call__(
        self,
        system_prompt: str,
        user_prompt: str,
        metadata: dict[str, str],
    ) -> GenerationResult:
        from openai import OpenAI

        api_key = os.environ.get(self.api_key_env)
        if not api_key:
            raise RuntimeError(f"missing API key in ${self.api_key_env}")

        client = OpenAI(api_key=api_key, base_url=self.base_url)
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
        response = client.responses.create(**kwargs)
        payload = _response_dump(response)
        return GenerationResult(
            output_text=_response_output_text(response, payload),
            response=payload,
        )


def infer_cases(
    cases: list[ContractNliCase],
    config: InferenceConfig,
    generator: Generator,
) -> list[ArtifactRecord]:
    records: list[ArtifactRecord] = []
    for case in cases:
        paths = artifact_paths(
            config.data_root,
            config.model,
            config.scenario,
            config.scope,
            case.split,
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
            continue

        user_prompt = render_prompt(config.user_prompt_template, case)
        system_prompt_sha = hashlib.sha256(
            config.system_prompt.encode("utf-8")
        ).hexdigest()
        user_prompt_sha = hashlib.sha256(user_prompt.encode("utf-8")).hexdigest()
        metadata = {
            "benchmark": BENCHMARK_NAME,
            "case_id": case.case_id,
            "scenario": scenario_run_slug(config.scenario),
        }
        result = generator(config.system_prompt, user_prompt, metadata)
        mdl_source = extract_mdl_source(result.output_text)

        paths.case_root.mkdir(parents=True, exist_ok=True)
        paths.mdl.write_text(mdl_source, encoding="utf-8")
        paths.raw.write_text(result.output_text, encoding="utf-8")
        _write_json(
            paths.meta,
            {
                "benchmark": BENCHMARK_NAME,
                "case_id": case.case_id,
                "split": case.split,
                "doc_id": case.doc_id,
                "hypothesis_id": case.hypothesis_id,
                "module_name": case.module_name,
                "model": config.model,
                "model_slug": model_run_slug(config.model),
                "scope": config.scope,
                "scenario": config.scenario,
                "scenario_slug": scenario_run_slug(config.scenario),
                "system_prompt": config.system_prompt_name,
                "system_prompt_sha256": system_prompt_sha,
                "user_prompt_template": config.user_prompt_template_name,
                "user_prompt_sha256": user_prompt_sha,
                "created_at": datetime.now(UTC).isoformat(),
                "base_url": config.base_url,
                "max_output_tokens": config.max_output_tokens,
                "temperature": config.temperature,
                "response": result.response,
            },
        )
        records.append(
            ArtifactRecord(
                case_id=case.case_id,
                mdl_path=paths.mdl,
                raw_path=paths.raw,
                meta_path=paths.meta,
            )
        )
    return records


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
    raise RuntimeError("model response did not contain output text")
