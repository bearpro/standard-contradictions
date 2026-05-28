from __future__ import annotations


def split_qualified(name: str) -> list[str]:
    return [part for part in name.split(".") if part]


def root_name(name: str) -> str:
    parts = split_qualified(name)
    return parts[0] if parts else ""


def local_name(name: str) -> str:
    parts = split_qualified(name)
    return parts[-1] if parts else ""


def is_qualified(name: str) -> bool:
    return "." in name
