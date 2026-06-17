from __future__ import annotations

import re
from dataclasses import dataclass, field
from difflib import SequenceMatcher
from typing import Any, Iterable, Sequence

from . import ast as A
from .parser import parse
from .printer import PrettyPrinter


@dataclass(frozen=True)
class AlignmentOptions:
    matcher: str = "auto"
    candidate_threshold: float = 0.55
    accept_threshold: float = 0.75


@dataclass(frozen=True)
class AlignmentElement:
    kind: str
    module: str
    path: str
    name: str
    parent: str | None = None
    type: str | None = None
    metadata: dict[str, Any] = field(default_factory=dict)
    line: int = 0
    column: int = 0

    @property
    def qualified(self) -> str:
        return f"{self.module}.{self.path}"

    def to_dict(self) -> dict[str, Any]:
        data: dict[str, Any] = {
            "kind": self.kind,
            "module": self.module,
            "path": self.path,
            "qualified": self.qualified,
            "name": self.name,
        }
        if self.parent:
            data["parent"] = self.parent
        if self.type:
            data["type"] = self.type
        if self.metadata:
            data["metadata"] = self.metadata
        if self.line or self.column:
            data["source_span"] = {"line": self.line, "column": self.column}
        return data


@dataclass
class AlignmentCandidate:
    left: AlignmentElement
    right: AlignmentElement
    score: float
    matcher: str
    reason: str
    accepted: bool = False

    @property
    def kind(self) -> str:
        return self.left.kind if self.left.kind == self.right.kind else "mixed"

    def to_dict(self) -> dict[str, Any]:
        return {
            "kind": self.kind,
            "left": self.left.qualified,
            "right": self.right.qualified,
            "score": round(self.score, 3),
            "matcher": self.matcher,
            "reason": self.reason,
            "accepted": self.accepted,
            "left_element": self.left.to_dict(),
            "right_element": self.right.to_dict(),
        }


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
class AlignmentReport:
    left_module: str | None = None
    right_module: str | None = None
    accepted: list[AlignmentCandidate] = field(default_factory=list)
    evidence: list[AlignmentCandidate] = field(default_factory=list)
    candidates: list[AlignmentCandidate] = field(default_factory=list)
    diagnostics: list[str] = field(default_factory=list)

    @property
    def suggestions(self) -> list[AlignmentCandidate]:
        """Backward-compatible alias for accepted inferred alignments."""
        return self.accepted

    def to_dict(self) -> dict[str, Any]:
        return {
            "left_module": self.left_module,
            "right_module": self.right_module,
            "accepted": [candidate.to_dict() for candidate in self.accepted],
            "evidence": [candidate.to_dict() for candidate in self.evidence],
            "candidates": [candidate.to_dict() for candidate in self.candidates],
            "diagnostics": self.diagnostics,
            "suggestions": [candidate.to_dict() for candidate in self.suggestions],
        }


_WORD_RE = re.compile(r"[a-z0-9]+")
_CAMEL_RE = re.compile(r"([a-z0-9])([A-Z])")
_GENERIC_RE = re.compile(r"[<>{}(),:\[\]]+")
_PRIMITIVE_TYPES = {
    "bool": "bool",
    "boolean": "bool",
    "int": "number",
    "integer": "number",
    "decimal": "number",
    "float": "number",
    "double": "number",
    "number": "number",
    "rat": "number",
    "rational": "number",
    "str": "string",
    "string": "string",
}
_COLLECTION_TYPES = {"list", "set", "seq", "array", "vector", "collection"}
_UNIT_TYPES = {"unit"}
_ABBREVIATIONS = {
    "addr": "address",
    "amt": "amount",
    "qty": "quantity",
    "lat": "latitude",
    "lon": "longitude",
    "lng": "longitude",
    "r": "radius",
}


def normalize(name: str) -> str:
    parts = split_words(name)
    return "_".join(parts)


def split_words(text: str | None) -> list[str]:
    if not text:
        return []
    expanded = _CAMEL_RE.sub(r"\1 \2", text)
    expanded = _GENERIC_RE.sub(" ", expanded)
    expanded = expanded.replace(".", " ").replace("-", " ").replace("_", " ")
    return _WORD_RE.findall(expanded.lower())


def token_similarity(left: Sequence[str], right: Sequence[str]) -> float:
    if not left or not right:
        return 0.0
    left_set = set(left)
    right_set = set(right)
    jaccard = len(left_set & right_set) / len(left_set | right_set)
    sequence = SequenceMatcher(None, " ".join(left), " ".join(right)).ratio()
    semantic = semantic_token_similarity(left, right)
    return max((0.65 * jaccard) + (0.35 * sequence), semantic)


def semantic_token_similarity(left: Sequence[str], right: Sequence[str]) -> float:
    matches: list[float] = []
    used_right: set[int] = set()
    for left_token in left:
        best_index = -1
        best_score = 0.0
        for index, right_token in enumerate(right):
            if index in used_right:
                continue
            score = token_match_score(left_token, right_token)
            if score > best_score:
                best_score = score
                best_index = index
        if best_index >= 0:
            used_right.add(best_index)
        matches.append(best_score)
    if not matches:
        return 0.0
    coverage = sum(matches) / max(len(left), len(right))
    order = SequenceMatcher(None, list(left), list(right)).ratio()
    return (0.75 * coverage) + (0.25 * order)


def token_match_score(left: str, right: str) -> float:
    if left == right:
        return 1.0
    if _ABBREVIATIONS.get(left) == right or _ABBREVIATIONS.get(right) == left:
        return 1.0
    shorter, longer = sorted((left, right), key=len)
    if len(shorter) >= 3 and longer.startswith(shorter):
        return 0.92
    if len(shorter) == 1 and longer.startswith(shorter):
        return 0.82
    return SequenceMatcher(None, left, right).ratio() * 0.45


def text_similarity(left: str | None, right: str | None) -> float:
    return token_similarity(split_words(left), split_words(right))


def type_signature(
    typ: A.TypeExpr | A.SumType | None, printer: PrettyPrinter
) -> str | None:
    if typ is None:
        return None
    if isinstance(typ, A.SumType):
        return "sum " + " ".join(variant.name for variant in typ.variants)
    return printer.type_expr(typ)


def structural_type_signature(
    typ: A.TypeExpr | A.SumType | None,
    type_env: dict[str, A.TypeExpr | A.SumType],
    printer: PrettyPrinter,
    *,
    seen: frozenset[str] = frozenset(),
) -> str | None:
    resolved = resolve_type(typ, type_env, seen)
    if isinstance(resolved, A.RecordType):
        fields = [
            f"{name}:{structural_type_signature(field_type, type_env, printer, seen=seen) or type_signature(field_type, printer)}"
            for name, field_type in resolved.fields
        ]
        return "record " + " ".join(fields)
    if isinstance(resolved, A.SumType):
        variants = []
        for variant in resolved.variants:
            fields = [
                structural_type_signature(field_type, type_env, printer, seen=seen)
                or type_signature(field_type, printer)
                or "unit"
                for _, field_type in variant.fields
            ]
            variants.append(f"{variant.name}({','.join(fields)})")
        return "sum " + " ".join(variants)
    if isinstance(resolved, A.TupleType):
        return "tuple " + " ".join(
            structural_type_signature(item, type_env, printer, seen=seen)
            or type_signature(item, printer)
            or "unit"
            for item in resolved.items
        )
    return type_signature(typ, printer)


def normalized_type(typ: str | None) -> str | None:
    if not typ:
        return None
    words = split_words(typ)
    if not words:
        return None
    if words[0] in _COLLECTION_TYPES and len(words) > 1:
        inner = normalized_type(" ".join(words[1:]))
        return f"many {inner or 'unknown'}"
    if len(words) == 1:
        return _PRIMITIVE_TYPES.get(words[0], words[0])
    mapped = [_PRIMITIVE_TYPES.get(word, word) for word in words]
    return " ".join(mapped)


def terminal_type_key(
    typ: A.TypeExpr | None,
    type_env: dict[str, A.TypeExpr | A.SumType],
    printer: PrettyPrinter,
) -> str | None:
    resolved = resolve_type(typ, type_env, frozenset())
    if isinstance(resolved, A.RecordType):
        return None
    if isinstance(resolved, A.SumType):
        return None
    if isinstance(resolved, A.TupleType):
        return None
    signature = type_signature(typ, printer)
    return normalized_type(signature)


def is_terminal_type(
    typ: A.TypeExpr | None,
    type_env: dict[str, A.TypeExpr | A.SumType],
    printer: PrettyPrinter,
) -> bool:
    return terminal_type_key(typ, type_env, printer) is not None


def extract_symbols(module: A.Module) -> list[Symbol]:
    symbols: list[Symbol] = []
    for decl in module.declarations:
        if isinstance(decl, A.TypeDecl):
            symbols.append(
                Symbol(module.name, decl.name, "type", decl.line, decl.column)
            )
        elif isinstance(decl, A.FuncDecl):
            symbols.append(
                Symbol(module.name, decl.name, "func", decl.line, decl.column)
            )
        elif isinstance(decl, A.EntityDecl):
            symbols.append(
                Symbol(module.name, decl.name, "entity", decl.line, decl.column)
            )
        elif isinstance(decl, A.RuleDecl):
            symbols.append(
                Symbol(module.name, decl.name, "rule", decl.line, decl.column)
            )
    return symbols


def project_module(module: A.Module) -> list[AlignmentElement]:
    printer = PrettyPrinter()
    type_env: dict[str, A.TypeExpr | A.SumType] = {
        decl.name: decl.definition
        for decl in module.declarations
        if isinstance(decl, A.TypeDecl) and decl.definition is not None
    }
    elements: list[AlignmentElement] = []

    for decl in module.declarations:
        if not isinstance(decl, A.EntityDecl):
            continue
        entity_type = structural_type_signature(decl.type_annotation, type_env, printer)
        fields = list(fields_for_type(decl.type_annotation, type_env, printer))
        variants = list(
            variant_fields_for_type(decl.type_annotation, type_env, printer)
        )
        elements.append(
            AlignmentElement(
                kind="entity",
                module=module.name,
                path=decl.name,
                name=decl.name,
                type=entity_type,
                metadata={
                    "tokens": split_words(decl.name),
                    "type_tokens": split_words(entity_type),
                    "field_names": [field_name for field_name, _, _ in fields],
                    "field_tokens": [
                        token
                        for field_name, _, _ in fields
                        for token in split_words(field_name)
                    ]
                    + [
                        token
                        for variant_name, field_name, _, _, _ in variants
                        for token in [
                            *split_words(variant_name),
                            *split_words(field_name),
                        ]
                    ],
                },
                line=decl.line,
                column=decl.column,
            )
        )
        if is_terminal_type(decl.type_annotation, type_env, printer):
            entity_type_text = type_signature(decl.type_annotation, printer)
            elements.append(
                AlignmentElement(
                    kind="field",
                    module=module.name,
                    path=decl.name,
                    name=decl.name,
                    type=entity_type_text,
                    metadata={
                        "tokens": split_words(decl.name),
                        "path_tokens": split_words(decl.name),
                        "type_tokens": split_words(entity_type_text),
                        "normalized_type": normalized_type(entity_type_text),
                        "cardinality": cardinality(decl.type_annotation),
                        "terminal": True,
                        "source_kind": "entity",
                    },
                    line=decl.line,
                    column=decl.column,
                )
            )
        for relative_path, field_name, field_type in fields:
            full_path = f"{decl.name}.{relative_path}"
            parent = ".".join(full_path.split(".")[:-1]) or decl.name
            field_type_text = type_signature(field_type, printer)
            terminal = is_terminal_type(field_type, type_env, printer)
            elements.append(
                AlignmentElement(
                    kind="field",
                    module=module.name,
                    path=full_path,
                    name=field_name,
                    parent=parent,
                    type=field_type_text,
                    metadata={
                        "tokens": split_words(field_name),
                        "path_tokens": split_words(full_path),
                        "parent_tokens": split_words(parent),
                        "type_tokens": split_words(field_type_text),
                        "normalized_type": normalized_type(field_type_text),
                        "cardinality": cardinality(field_type),
                        "relation_target": relation_target(field_type),
                        "terminal": terminal,
                    },
                )
            )
        for (
            variant_name,
            field_name,
            field_index,
            payload_count,
            field_type,
        ) in variants:
            if not is_terminal_type(field_type, type_env, printer):
                continue
            field_type_text = type_signature(field_type, printer)
            type_name = local_type_name(decl.type_annotation)
            path = f"{decl.name}.{variant_name}.{field_name}"
            parent = f"{decl.name}.{variant_name}"
            elements.append(
                AlignmentElement(
                    kind="field",
                    module=module.name,
                    path=path,
                    name=field_name,
                    parent=parent,
                    type=field_type_text,
                    metadata={
                        "tokens": split_words(field_name),
                        "path_tokens": split_words(path),
                        "parent_tokens": split_words(parent),
                        "type_tokens": split_words(field_type_text),
                        "normalized_type": normalized_type(field_type_text),
                        "cardinality": cardinality(field_type),
                        "terminal": True,
                        "render": "case",
                        "subject_path": decl.name,
                        "variant": variant_name,
                        "type_name": type_name,
                        "field_index": field_index,
                        "payload_count": payload_count,
                    },
                )
            )
    return elements


def fields_for_type(
    typ: A.TypeExpr | None,
    type_env: dict[str, A.TypeExpr | A.SumType],
    printer: PrettyPrinter,
    *,
    prefix: str = "",
    seen: frozenset[str] = frozenset(),
) -> Iterable[tuple[str, str, A.TypeExpr]]:
    current_seen = seen
    type_name = local_type_name(typ)
    if type_name and type_name in type_env:
        if type_name in seen:
            return
        current_seen = seen | {type_name}

    resolved = resolve_type(typ, type_env, seen)
    if isinstance(resolved, A.RecordType):
        for field_name, field_type in resolved.fields:
            path = f"{prefix}.{field_name}" if prefix else field_name
            yield path, field_name, field_type
            nested = resolve_type(field_type, type_env, current_seen)
            if isinstance(nested, A.RecordType):
                yield from fields_for_type(
                    field_type,
                    type_env,
                    printer,
                    prefix=path,
                    seen=current_seen,
                )


def variant_fields_for_type(
    typ: A.TypeExpr | None,
    type_env: dict[str, A.TypeExpr | A.SumType],
    printer: PrettyPrinter,
) -> Iterable[tuple[str, str, int, int, A.TypeExpr]]:
    resolved = resolve_type(typ, type_env, frozenset())
    if not isinstance(resolved, A.SumType):
        return
    for variant in resolved.variants:
        payload_count = len(variant.fields)
        for index, (label, field_type) in enumerate(variant.fields):
            if is_unit_type(field_type):
                continue
            yield (
                variant.name,
                label or f"value_{index}",
                index,
                payload_count,
                field_type,
            )


def resolve_type(
    typ: A.TypeExpr | A.SumType | None,
    type_env: dict[str, A.TypeExpr | A.SumType],
    seen: frozenset[str],
) -> A.TypeExpr | A.SumType | None:
    if isinstance(typ, A.TypeRef):
        type_name = typ.name.split(".")[-1]
        if type_name in type_env and type_name not in seen:
            return resolve_type(type_env[type_name], type_env, seen | {type_name})
    return typ


def local_type_name(typ: A.TypeExpr | None) -> str | None:
    if isinstance(typ, A.TypeRef):
        return typ.name.split(".")[-1]
    return None


def cardinality(typ: A.TypeExpr | None) -> str:
    if isinstance(typ, A.TypeRef):
        type_name = typ.name.split(".")[-1].lower()
        if type_name in _COLLECTION_TYPES:
            return "many"
    return "single"


def relation_target(typ: A.TypeExpr | None) -> str | None:
    if isinstance(typ, A.TypeRef):
        if typ.args:
            first = typ.args[0]
            if isinstance(first, A.TypeRef):
                return first.name
        type_name = typ.name.split(".")[-1]
        if type_name.lower() not in _PRIMITIVE_TYPES:
            return typ.name
    return None


def is_unit_type(typ: A.TypeExpr | None) -> bool:
    if isinstance(typ, A.TypeRef):
        return typ.name.split(".")[-1].lower() in _UNIT_TYPES
    return False


def builtin_candidates(
    left_elements: list[AlignmentElement],
    right_elements: list[AlignmentElement],
    *,
    options: AlignmentOptions,
) -> list[AlignmentCandidate]:
    candidates: list[AlignmentCandidate] = []

    entity_candidates = score_element_pairs(
        [element for element in left_elements if element.kind == "entity"],
        [element for element in right_elements if element.kind == "entity"],
        matcher="builtin",
        options=options,
        accepted_entities={},
    )
    accepted_entities = accepted_parent_map(entity_candidates, options.accept_threshold)

    field_candidates = score_element_pairs(
        [element for element in left_elements if element.kind == "field"],
        [element for element in right_elements if element.kind == "field"],
        matcher="builtin",
        options=options,
        accepted_entities=accepted_entities,
    )

    candidates.extend(entity_candidates)
    candidates.extend(field_candidates)
    mark_accepted(candidates, options.accept_threshold)
    return sorted(candidates, key=candidate_sort_key)


def score_element_pairs(
    left_items: list[AlignmentElement],
    right_items: list[AlignmentElement],
    *,
    matcher: str,
    options: AlignmentOptions,
    accepted_entities: dict[str, str],
) -> list[AlignmentCandidate]:
    candidates: list[AlignmentCandidate] = []
    for left in left_items:
        for right in right_items:
            score, reason = score_pair(left, right, accepted_entities)
            if left.kind == "field" and not compatible_terminal_pair(left, right):
                reason += "; incompatible terminal types"
                score = min(score, options.accept_threshold - 0.001)
            if score >= options.candidate_threshold:
                candidates.append(
                    AlignmentCandidate(left, right, score, matcher, reason)
                )
    return candidates


def score_pair(
    left: AlignmentElement,
    right: AlignmentElement,
    accepted_entities: dict[str, str],
) -> tuple[float, str]:
    name_score = text_similarity(left.name, right.name)
    path_score = text_similarity(left.path, right.path)
    type_score = type_similarity(left.type, right.type)

    if left.kind == "entity":
        fields_score = token_similarity(
            left.metadata.get("field_tokens", []),
            right.metadata.get("field_tokens", []),
        )
        score = (
            (0.45 * name_score)
            + (0.25 * fields_score)
            + (0.2 * type_score)
            + (0.1 * path_score)
        )
        reason = (
            f"name={name_score:.2f}; fields={fields_score:.2f}; "
            f"type={type_score:.2f}; path={path_score:.2f}"
        )
        return min(score, 1.0), reason

    parent_score = text_similarity(left.parent, right.parent)
    if (
        left.parent in accepted_entities
        and accepted_entities[left.parent] == right.parent
    ):
        parent_score = max(parent_score, 1.0)
    score = (
        (0.4 * name_score)
        + (0.25 * type_score)
        + (0.2 * parent_score)
        + (0.15 * path_score)
    )
    if compatible_terminal_pair(left, right) and terminal_name_match(left, right):
        exact_leaf = normalize(left.name) == normalize(right.name) and path_score == 1.0
        score = max(
            score,
            1.0 if type_score == 1.0 and (parent_score == 1.0 or exact_leaf) else 0.8,
        )
    reason = (
        f"name={name_score:.2f}; type={type_score:.2f}; "
        f"parent={parent_score:.2f}; path={path_score:.2f}"
    )
    return min(score, 1.0), reason


def type_similarity(left: str | None, right: str | None) -> float:
    left_norm = normalized_type(left)
    right_norm = normalized_type(right)
    if left_norm and right_norm and left_norm == right_norm:
        return 1.0
    if not left_norm or not right_norm:
        return 0.0
    return text_similarity(left_norm, right_norm)


def compatible_terminal_pair(left: AlignmentElement, right: AlignmentElement) -> bool:
    if left.kind != "field" or right.kind != "field":
        return False
    if not left.metadata.get("terminal", True) or not right.metadata.get(
        "terminal", True
    ):
        return False
    left_type = left.metadata.get("normalized_type") or normalized_type(left.type)
    right_type = right.metadata.get("normalized_type") or normalized_type(right.type)
    if not left_type or not right_type or left_type != right_type:
        return False
    if left.metadata.get("cardinality") != right.metadata.get("cardinality"):
        return False
    if left.metadata.get("render") != right.metadata.get("render"):
        return False
    if left.metadata.get("render") == "case":
        return left.metadata.get("variant") == right.metadata.get(
            "variant"
        ) or terminal_name_match(left, right)
    return True


def terminal_name_match(left: AlignmentElement, right: AlignmentElement) -> bool:
    if normalize(left.name) == normalize(right.name) and normalize(left.name):
        return True
    return token_similarity(split_words(left.name), split_words(right.name)) >= 0.75


def accepted_parent_map(
    candidates: list[AlignmentCandidate],
    accept_threshold: float,
) -> dict[str, str]:
    selected = select_one_to_one(candidates, accept_threshold)
    return {candidate.left.path: candidate.right.path for candidate in selected}


def mark_accepted(
    candidates: list[AlignmentCandidate], accept_threshold: float
) -> None:
    selected = set()
    for candidate in select_one_to_one(
        [
            item
            for item in candidates
            if item.kind == "field" and compatible_terminal_pair(item.left, item.right)
        ],
        accept_threshold,
    ):
        selected.add((candidate.left.qualified, candidate.right.qualified))
    for candidate in candidates:
        candidate.accepted = (
            candidate.left.qualified,
            candidate.right.qualified,
        ) in selected


def select_one_to_one(
    candidates: list[AlignmentCandidate],
    accept_threshold: float,
) -> list[AlignmentCandidate]:
    viable = [
        candidate for candidate in candidates if candidate.score >= accept_threshold
    ]
    if len(viable) <= 18:
        return select_one_to_one_optimal(viable)
    selected: list[AlignmentCandidate] = []
    used_left: set[str] = set()
    used_right: set[str] = set()
    for candidate in sorted(candidates, key=candidate_sort_key):
        if candidate.score < accept_threshold:
            continue
        if (
            candidate.left.qualified in used_left
            or candidate.right.qualified in used_right
        ):
            continue
        selected.append(candidate)
        used_left.add(candidate.left.qualified)
        used_right.add(candidate.right.qualified)
    return selected


def select_one_to_one_optimal(
    candidates: list[AlignmentCandidate],
) -> list[AlignmentCandidate]:
    by_left: dict[str, list[AlignmentCandidate]] = {}
    right_keys = sorted({candidate.right.qualified for candidate in candidates})
    right_index = {qualified: index for index, qualified in enumerate(right_keys)}
    for candidate in candidates:
        by_left.setdefault(candidate.left.qualified, []).append(candidate)
    left_keys = sorted(by_left)
    memo: dict[tuple[int, int], tuple[float, tuple[AlignmentCandidate, ...]]] = {}

    def search(
        left_pos: int, used_mask: int
    ) -> tuple[float, tuple[AlignmentCandidate, ...]]:
        key = (left_pos, used_mask)
        if key in memo:
            return memo[key]
        if left_pos >= len(left_keys):
            return 0.0, ()
        best_score, best_items = search(left_pos + 1, used_mask)
        for candidate in sorted(by_left[left_keys[left_pos]], key=candidate_sort_key):
            bit = 1 << right_index[candidate.right.qualified]
            if used_mask & bit:
                continue
            rest_score, rest_items = search(left_pos + 1, used_mask | bit)
            total = candidate.score + rest_score
            items = (candidate, *rest_items)
            if (
                total > best_score
                or total == best_score
                and tuple(candidate_sort_key(item) for item in items)
                < tuple(candidate_sort_key(item) for item in best_items)
            ):
                best_score, best_items = total, items
        memo[key] = best_score, best_items
        return memo[key]

    return list(search(0, 0)[1])


def candidate_sort_key(candidate: AlignmentCandidate) -> tuple[float, str, str]:
    return (-candidate.score, candidate.left.qualified, candidate.right.qualified)


def external_candidates(
    left_elements: list[AlignmentElement],
    right_elements: list[AlignmentElement],
    *,
    options: AlignmentOptions,
) -> tuple[list[AlignmentCandidate] | None, list[str]]:
    matcher = options.matcher
    if matcher == "auto":
        result, diagnostics = run_external_matcher(
            left_elements,
            right_elements,
            options=AlignmentOptions(
                matcher="bdikit:coma",
                candidate_threshold=0.0,
                accept_threshold=options.accept_threshold,
            ),
        )
        if result is not None:
            return calibrate_external_candidates(
                result,
                options=options,
            ), diagnostics
        return None, [
            "optional align dependencies are unavailable; using builtin matcher"
        ]
    if matcher == "bdikit:coma":
        return run_external_matcher(left_elements, right_elements, options=options)
    return None, []


def run_external_matcher(
    left_elements: list[AlignmentElement],
    right_elements: list[AlignmentElement],
    *,
    options: AlignmentOptions,
) -> tuple[list[AlignmentCandidate] | None, list[str]]:
    try:
        import pandas as pd  # type: ignore
    except Exception as exc:
        return None, [f"{options.matcher} unavailable: pandas import failed ({exc})"]

    diagnostics: list[str] = []
    rows: list[AlignmentCandidate] = []
    for kind in ["entity", "field"]:
        left_kind = [element for element in left_elements if element.kind == kind]
        right_kind = [element for element in right_elements if element.kind == kind]
        if not left_kind or not right_kind:
            continue

        source_data, source_map = dataframe_data_for_elements(left_kind)
        target_data, target_map = dataframe_data_for_elements(right_kind)
        source_df = pd.DataFrame(source_data)
        target_df = pd.DataFrame(target_data)

        try:
            raw_matches = execute_external_matcher(
                source_df, target_df, options.matcher
            )
        except Exception as exc:
            return None, [f"{options.matcher} failed: {exc}"]

        for source_column, target_column, score in raw_matches:
            left = source_map.get(source_column)
            right = target_map.get(target_column)
            if left is None or right is None:
                continue
            if score >= options.candidate_threshold:
                rows.append(
                    AlignmentCandidate(
                        left=left,
                        right=right,
                        score=score,
                        matcher=options.matcher,
                        reason="external COMA-style matcher",
                    )
                )

    mark_accepted(rows, options.accept_threshold)
    return sorted(rows, key=candidate_sort_key), diagnostics


def calibrate_external_candidates(
    candidates: list[AlignmentCandidate],
    *,
    options: AlignmentOptions,
) -> list[AlignmentCandidate]:
    entity_candidates = [
        candidate for candidate in candidates if candidate.kind == "entity"
    ]
    accepted_entities = accepted_parent_map(entity_candidates, options.accept_threshold)
    calibrated: list[AlignmentCandidate] = []
    for candidate in candidates:
        score, reason = score_pair(candidate.left, candidate.right, accepted_entities)
        score = max(candidate.score, score)
        if candidate.kind == "field" and not compatible_terminal_pair(
            candidate.left, candidate.right
        ):
            score = min(score, options.accept_threshold - 0.001)
            reason += "; incompatible terminal types"
        if (
            score >= options.candidate_threshold
            or candidate.score >= options.candidate_threshold
        ):
            calibrated.append(
                AlignmentCandidate(
                    left=candidate.left,
                    right=candidate.right,
                    score=score,
                    matcher=candidate.matcher,
                    reason=f"{candidate.reason}; calibrated {reason}",
                )
            )
    mark_accepted(calibrated, options.accept_threshold)
    return sorted(calibrated, key=candidate_sort_key)


def dataframe_data_for_elements(
    elements: list[AlignmentElement],
) -> tuple[dict[str, list[str]], dict[str, AlignmentElement]]:
    values_by_column: dict[str, list[str]] = {}
    column_map: dict[str, AlignmentElement] = {}
    for index, element in enumerate(elements, start=1):
        column = dedupe_column_name(element.path, values_by_column, index)
        column_map[column] = element
        values = [
            f"kind {element.kind}",
            f"name {' '.join(split_words(element.name))}",
            f"path {' '.join(split_words(element.path))}",
            f"parent {' '.join(split_words(element.parent))}",
            f"type {' '.join(split_words(element.type))}",
        ]
        for key in ["normalized_type", "cardinality", "relation_target"]:
            if element.metadata.get(key):
                values.append(
                    f"{key} {' '.join(split_words(str(element.metadata[key])))}"
                )
        if element.kind == "entity":
            field_names = element.metadata.get("field_names", [])
            values.append("fields " + " ".join(split_words(" ".join(field_names))))
        values_by_column[column] = values

    max_len = max((len(values) for values in values_by_column.values()), default=0)
    data = {
        column: values + [""] * (max_len - len(values))
        for column, values in values_by_column.items()
    }
    return data, column_map


def dedupe_column_name(base: str, existing: dict[str, Any], index: int) -> str:
    if base not in existing:
        return base
    return f"{base}__dup_{index:04d}"


def execute_external_matcher(
    source_df: Any, target_df: Any, matcher: str
) -> list[tuple[str, str, float]]:
    if matcher == "bdikit:coma":
        from bdikit.schema_matching.valentine import Coma  # type: ignore

        results = Coma().match_schema(source_df, target_df)
        return normalize_match_rows(results)

    raise ValueError(f"unsupported external matcher {matcher!r}")


def normalize_match_rows(matches: Any) -> list[tuple[str, str, float]]:
    rows: list[tuple[str, str, float]] = []
    if hasattr(matches, "to_dict") and hasattr(matches, "columns"):
        iterable = matches.to_dict(orient="records")
    else:
        iterable = matches.items() if hasattr(matches, "items") else matches
    for item in iterable:
        if isinstance(item, dict):
            source_column = (
                item.get("source_column")
                or item.get("source_attribute")
                or item.get("source")
            )
            target_column = (
                item.get("target_column")
                or item.get("target_attribute")
                or item.get("target")
            )
            score = item.get("similarity") or item.get("score") or 1.0
        elif hasattr(item, "source_column"):
            source_column = item.source_column
            target_column = item.target_column
            score = item.similarity
        elif len(item) == 2 and not isinstance(item[0], str):
            pair, score = item
            if hasattr(pair, "source_column"):
                source_column = pair.source_column
                target_column = pair.target_column
            else:
                source_column = pair[0][1] if isinstance(pair[0], tuple) else pair[0]
                target_column = pair[1][1] if isinstance(pair[1], tuple) else pair[1]
        else:
            source_column = item[0]
            target_column = item[1]
            score = item[2] if len(item) > 2 else 1.0
        rows.append((str(source_column), str(target_column), float(score)))
    return rows


def align_modules(
    left: A.Module,
    right: A.Module,
    options: AlignmentOptions | None = None,
) -> AlignmentReport:
    options = options or AlignmentOptions()
    left_elements = project_module(left)
    right_elements = project_module(right)

    diagnostics: list[str] = []
    candidates: list[AlignmentCandidate] | None = None
    if options.matcher != "builtin":
        candidates, diagnostics = external_candidates(
            left_elements, right_elements, options=options
        )
    if candidates is None:
        candidates = builtin_candidates(left_elements, right_elements, options=options)
        matcher_note = "builtin matcher used"
        if matcher_note not in diagnostics and options.matcher != "builtin":
            diagnostics.append(matcher_note)

    accepted = [candidate for candidate in candidates if candidate.accepted]
    evidence = [
        candidate
        for candidate in candidates
        if candidate.kind == "entity" and candidate.score >= options.accept_threshold
    ]
    return AlignmentReport(
        left_module=left.name,
        right_module=right.name,
        accepted=accepted,
        evidence=evidence,
        candidates=candidates,
        diagnostics=diagnostics,
    )


def suggest_alignments(
    modules: Iterable[A.Module],
    threshold: float = 0.78,
) -> AlignmentReport:
    mods = list(modules)
    if len(mods) < 2:
        return AlignmentReport()
    if len(mods) == 2:
        return align_modules(
            mods[0],
            mods[1],
            AlignmentOptions(
                candidate_threshold=min(0.55, threshold),
                accept_threshold=threshold,
                matcher="builtin",
            ),
        )

    merged = AlignmentReport()
    for index, left in enumerate(mods):
        for right in mods[index + 1 :]:
            report = align_modules(
                left,
                right,
                AlignmentOptions(
                    candidate_threshold=min(0.55, threshold),
                    accept_threshold=threshold,
                    matcher="builtin",
                ),
            )
            merged.candidates.extend(report.candidates)
            merged.accepted.extend(report.accepted)
            merged.evidence.extend(report.evidence)
            merged.diagnostics.extend(report.diagnostics)
    merged.candidates.sort(key=candidate_sort_key)
    merged.accepted.sort(key=candidate_sort_key)
    merged.evidence.sort(key=candidate_sort_key)
    return merged


def align_sources(
    sources: Iterable[str],
    threshold: float = 0.78,
    *,
    options: AlignmentOptions | None = None,
) -> AlignmentReport:
    modules = [parse(source) for source in sources]
    if options is not None and len(modules) == 2:
        return align_modules(modules[0], modules[1], options)
    return suggest_alignments(modules, threshold=threshold)


def render_alignment_module(
    report: AlignmentReport,
    module_name: str | None = None,
) -> A.Module:
    left_module = report.left_module or "left"
    right_module = report.right_module or "right"
    name = (
        module_name
        or f"alignment_{safe_identifier(left_module)}_{safe_identifier(right_module)}"
    )
    module = A.Module(name=name)
    module.imports.append(A.ImportDecl(path=f"{left_module}.mdl"))
    module.imports.append(A.ImportDecl(path=f"{right_module}.mdl"))

    for index, candidate in enumerate(report.accepted, start=1):
        equality = expression_for_candidate(candidate, left_module, right_module, index)
        body = A.TemporalUnary(
            op="always",
            operand=equality,
            position="postfix",
        )
        annotation = (
            f"# align kind={candidate.kind} score={candidate.score:.3f} "
            f"matcher={candidate.matcher}"
        )
        module.declarations.append(
            A.RuleDecl(
                name=f"alignment_{index:03d}",
                modality="O",
                body=body,
                annotations=[annotation],
            )
        )
    return module


def expression_for_candidate(
    candidate: AlignmentCandidate,
    left_module: str,
    right_module: str,
    index: int,
) -> A.Expr:
    if (
        candidate.left.metadata.get("render") == "case"
        and candidate.right.metadata.get("render") == "case"
    ):
        return case_expression_for_candidate(
            candidate, left_module, right_module, index
        )
    left_expr = expression_for_path(left_module, candidate.left.path)
    right_expr = expression_for_path(right_module, candidate.right.path)
    return A.BinaryOp(op="=", left=left_expr, right=right_expr)


def case_expression_for_candidate(
    candidate: AlignmentCandidate,
    left_module: str,
    right_module: str,
    index: int,
) -> A.Expr:
    left_var = f"left_{index}"
    right_var = f"right_{index}"
    equality = A.BinaryOp(
        op="=",
        left=A.Name(name=left_var),
        right=A.Name(name=right_var),
    )
    right_match = A.MatchExpr(
        subject=expression_for_path(
            right_module, str(candidate.right.metadata["subject_path"])
        ),
        arms=[
            A.MatchArm(
                pattern=case_pattern(right_module, candidate.right, right_var),
                body=A.Block(result=equality),
            ),
            A.MatchArm(
                pattern=A.WildcardPattern(),
                body=A.Block(result=A.Literal(value=True, kind="bool")),
            ),
        ],
    )
    return A.MatchExpr(
        subject=expression_for_path(
            left_module, str(candidate.left.metadata["subject_path"])
        ),
        arms=[
            A.MatchArm(
                pattern=case_pattern(left_module, candidate.left, left_var),
                body=A.Block(result=right_match),
            ),
            A.MatchArm(
                pattern=A.WildcardPattern(),
                body=A.Block(result=A.Literal(value=True, kind="bool")),
            ),
        ],
    )


def case_pattern(
    module_name: str, element: AlignmentElement, binding: str
) -> A.ConstructorPattern:
    payload_count = int(element.metadata["payload_count"])
    field_index = int(element.metadata["field_index"])
    args: list[A.Pattern] = [A.WildcardPattern() for _ in range(payload_count)]
    args[field_index] = A.VarPattern(name=binding)
    type_name = element.metadata.get("type_name")
    variant = str(element.metadata["variant"])
    constructor = (
        f"{module_name}.{type_name}.{variant}"
        if type_name
        else f"{module_name}.{variant}"
    )
    return A.ConstructorPattern(name=constructor, args=args)


def expression_for_path(module_name: str, path: str) -> A.Expr:
    parts = [module_name, *path.split(".")]
    expr: A.Expr = A.Name(name=parts[0])
    for part in parts[1:]:
        expr = A.FieldAccess(target=expr, field=part)
    return expr


def safe_identifier(value: str) -> str:
    text = normalize(value)
    if not text:
        return "module"
    if text[0].isdigit():
        return f"m_{text}"
    return text
