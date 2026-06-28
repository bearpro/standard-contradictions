from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path

BENCH_ROOT = Path(__file__).resolve().parents[2]
DEFAULT_DATA_ROOT = BENCH_ROOT / "data"
DEFAULT_SYSTEM_PROMPT = BENCH_ROOT / "prompts" / "no_align_v1.system.md"
DEFAULT_USER_TEMPLATE = BENCH_ROOT / "prompts" / "no_align_v1.user.md"

_SLUG_RE = re.compile(r"[^A-Za-z0-9._-]+")


def slugify(value: str) -> str:
    slug = _SLUG_RE.sub("-", value.strip()).strip("-._").lower()
    slug = re.sub(r"-{2,}", "-", slug)
    if not slug or slug in {".", ".."} or "/" in slug or "\\" in slug:
        raise ValueError(f"invalid slug value: {value!r}")
    return slug


def model_run_slug(model: str) -> str:
    model_slug = slugify(model)
    if model_slug.endswith("-no-align"):
        return model_slug
    return f"{model_slug}-no-align"


def scenario_run_slug(scenario: str) -> str:
    return slugify(scenario)


@dataclass(frozen=True)
class ArtifactPaths:
    run_root: Path
    case_root: Path
    mdl: Path
    raw: Path
    meta: Path


def artifact_paths(
    data_root: Path,
    scenario: str,
    doc_id: int | str,
    hypothesis_id: str,
) -> ArtifactPaths:
    run_root = data_root / "generated" / scenario_run_slug(scenario)
    case_root = run_root / str(doc_id) / hypothesis_id
    return ArtifactPaths(
        run_root=run_root,
        case_root=case_root,
        mdl=case_root / "generated.mdl",
        raw=case_root / "raw.txt",
        meta=run_root / "meta.json",
    )
