from __future__ import annotations

from dataclasses import dataclass
from typing import Protocol

from mdl.lib.model import Module


@dataclass(frozen=True)
class Alignment:
    left_name: str
    right_name: str
    score: float


class AlignmentTool(Protocol):
    def align(self, left: Module, right: Module) -> tuple[Alignment, ...]:
        ...


class ExactNameMatcher:
    def align(self, left: Module, right: Module) -> tuple[Alignment, ...]:
        shared_names = sorted(set(left.objects) & set(right.objects))
        return tuple(Alignment(name, name, 1.0) for name in shared_names)


def align(
    left: Module,
    right: Module,
    tool: AlignmentTool | None = None,
) -> tuple[Alignment, ...]:
    matcher = tool or ExactNameMatcher()
    return matcher.align(left, right)


__all__ = ["Alignment", "AlignmentTool", "ExactNameMatcher", "align"]
