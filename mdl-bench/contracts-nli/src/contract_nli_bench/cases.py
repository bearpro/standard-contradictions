from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from .constants import (
    RUN_SCOPES,
    SPLITS,
)


@dataclass(frozen=True)
class ContractNliCase:
    split: str
    doc_id: int
    file_name: str
    document_type: str
    url: str
    text: str
    spans: tuple[tuple[int, int], ...]
    hypothesis_id: str
    short_description: str
    hypothesis: str
    choice: str
    evidence_span_indices: tuple[int, ...]

    @property
    def case_id(self) -> str:
        return f"{self.split}-{self.doc_id}-{self.hypothesis_id}"

    @property
    def module_name(self) -> str:
        safe_hypothesis = self.hypothesis_id.replace("-", "_")
        return f"contract_nli_no_align_{self.split}_{self.doc_id}_{safe_hypothesis}"


def load_split(data_root: Path, split: str) -> list[ContractNliCase]:
    if split not in SPLITS:
        raise ValueError(f"unknown split {split!r}; expected one of {SPLITS}")
    payload = _load_split_payload(data_root, split)
    labels = payload["labels"]
    cases: list[ContractNliCase] = []
    for doc in payload["documents"]:
        annotations = doc["annotation_sets"][0]["annotations"]
        spans = tuple(tuple(span) for span in doc["spans"])
        for hypothesis_id in sorted(annotations):
            annotation = annotations[hypothesis_id]
            label = labels[hypothesis_id]
            cases.append(
                ContractNliCase(
                    split=split,
                    doc_id=int(doc["id"]),
                    file_name=str(doc.get("file_name", "")),
                    document_type=str(doc.get("document_type", "")),
                    url=str(doc.get("url", "")),
                    text=str(doc["text"]),
                    spans=spans,
                    hypothesis_id=hypothesis_id,
                    short_description=str(label.get("short_description", "")),
                    hypothesis=str(label["hypothesis"]),
                    choice=str(annotation["choice"]),
                    evidence_span_indices=tuple(int(i) for i in annotation["spans"]),
                )
            )
    return sorted(cases, key=_case_sort_key)


def resolve_cases(
    data_root: Path,
    scope: str,
    case_id: str | None = None,
) -> list[ContractNliCase]:
    if scope not in RUN_SCOPES:
        raise ValueError(f"unknown run scope {scope!r}; expected one of {RUN_SCOPES}")

    selected_splits = ("dev",) if scope == "dev" else SPLITS
    cases = [
        case
        for split in selected_splits
        for case in load_split(data_root, split)
    ]

    if case_id is not None:
        cases = [
            case
            for case in cases
            if case.case_id == case_id
            or f"{case.split}/{case.doc_id}/{case.hypothesis_id}" == case_id
        ]
        if not cases:
            raise ValueError(f"case not found: {case_id}")
    return cases


def _load_split_payload(data_root: Path, split: str) -> dict[str, Any]:
    path = data_root / "raw" / "contract-nli" / f"{split}.json"
    if path.exists():
        return json.loads(path.read_text(encoding="utf-8"))

    raise FileNotFoundError(
        f"missing Contract-NLI {split}.json; run `contract-nli-bench download`"
    )


def _case_sort_key(case: ContractNliCase) -> tuple[str, int, str]:
    return (case.split, case.doc_id, case.hypothesis_id)
