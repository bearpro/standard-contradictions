from __future__ import annotations

import re
from pathlib import Path

from .cases import ContractNliCase

_FENCE_RE = re.compile(r"```(?P<lang>[A-Za-z0-9_-]*)\s*\n(?P<body>.*?)```", re.S)


def read_prompt_file(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def render_prompt(template: str, case: ContractNliCase) -> str:
    values = {
        "module_name": case.module_name,
        "document_id": str(case.doc_id),
        "file_name": case.file_name,
        "hypothesis_id": case.hypothesis_id,
        "hypothesis": case.hypothesis,
        "document_text": case.text,
    }
    rendered = template
    for name, value in values.items():
        rendered = rendered.replace("{{" + name + "}}", value)
    return rendered


def extract_mdl_source(text: str) -> str:
    matches = list(_FENCE_RE.finditer(text))
    if not matches:
        return text.strip() + "\n"

    mdl_match = next(
        (match for match in matches if match.group("lang").lower() == "mdl"),
        matches[0],
    )
    return mdl_match.group("body").strip() + "\n"
