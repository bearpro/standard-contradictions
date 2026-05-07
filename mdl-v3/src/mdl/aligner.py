from __future__ import annotations

import re
from dataclasses import dataclass, field
from difflib import SequenceMatcher
from typing import Iterable

from . import ast as A
from .parser import parse


@dataclass(frozen=True)
class Symbol:
    module: str
    name: str
    kind: str
    line: int = 0
    column: int = 0

    @property
    def qualified(self) -> str:
        return f"{self.module}.{self.name}"


@dataclass
class AlignmentSuggestion:
    left: Symbol
    right: Symbol
    score: float
    reason: str

    def to_dict(self) -> dict:
        return {
            "left": self.left.qualified,
            "right": self.right.qualified,
            "score": round(self.score, 3),
            "reason": self.reason,
        }


@dataclass
class AlignmentReport:
    explicit: list[dict] = field(default_factory=list)
    suggestions: list[AlignmentSuggestion] = field(default_factory=list)

    def to_dict(self) -> dict:
        return {
            "explicit": self.explicit,
            "suggestions": [s.to_dict() for s in self.suggestions],
        }


_WORD_RE = re.compile(r"[a-z0-9]+")


def normalize(name: str) -> str:
    parts = _WORD_RE.findall(name.replace(".", "_").replace("-", "_").lower())
    return "_".join(parts)


def extract_symbols(module: A.Module) -> list[Symbol]:
    symbols: list[Symbol] = []
    for decl in module.declarations:
        if isinstance(decl, A.TypeDecl):
            symbols.append(Symbol(module.name, decl.name, "type", decl.line, decl.column))
        elif isinstance(decl, A.FuncDecl):
            symbols.append(Symbol(module.name, decl.name, "func", decl.line, decl.column))
        elif isinstance(decl, A.EntityDecl):
            symbols.append(Symbol(module.name, decl.name, "entity", decl.line, decl.column))
        elif isinstance(decl, A.EventDecl):
            symbols.append(Symbol(module.name, decl.name, "event", decl.line, decl.column))
        elif isinstance(decl, A.RuleDecl):
            symbols.append(Symbol(module.name, decl.name, "rule", decl.line, decl.column))
    return symbols


def explicit_alignments(module: A.Module) -> list[dict]:
    rows: list[dict] = []
    for decl in module.declarations:
        if isinstance(decl, A.AlignDecl):
            rows.append({
                "module": module.name,
                "subject": decl.subject,
                "target": decl.target,
                "kind": decl.kind,
                "source_span": {"line": decl.line, "column": decl.column},
            })
    return rows


def suggest_alignments(modules: Iterable[A.Module], threshold: float = 0.78) -> AlignmentReport:
    mods = list(modules)
    report = AlignmentReport(explicit=[row for m in mods for row in explicit_alignments(m)])
    symbols = [s for m in mods for s in extract_symbols(m)]
    for i, left in enumerate(symbols):
        for right in symbols[i + 1:]:
            if left.module == right.module:
                continue
            if left.kind != right.kind and {left.kind, right.kind} != {"entity", "type"}:
                continue
            ln = normalize(left.name)
            rn = normalize(right.name)
            if not ln or not rn:
                continue
            score = SequenceMatcher(None, ln, rn).ratio()
            if ln == rn:
                score = 1.0
                reason = "same normalized name"
            else:
                reason = "similar normalized names"
            if score >= threshold:
                report.suggestions.append(AlignmentSuggestion(left, right, score, reason))
    report.suggestions.sort(key=lambda s: (-s.score, s.left.qualified, s.right.qualified))
    return report


def align_sources(sources: Iterable[str], threshold: float = 0.78) -> AlignmentReport:
    return suggest_alignments([parse(source) for source in sources], threshold=threshold)
