from __future__ import annotations

from pathlib import Path
from urllib.parse import unquote, urlparse
from urllib.request import url2pathname

from . import ast as A
from .dsl import compile_source
from .parser import parse


def is_python_dsl_path(path: str | Path | None) -> bool:
    if path is None:
        return False
    return _path_text(path).replace("\\", "/").endswith(".mdl.py")


def default_module_name_for_path(path: str | Path | None) -> str:
    if path is None:
        return "model"
    text = _path_text(path).replace("\\", "/")
    name = text.rsplit("/", 1)[-1]
    if name.endswith(".mdl.py"):
        return name[:-len(".mdl.py")] or "model"
    if name.endswith(".mdl"):
        return name[:-len(".mdl")] or "model"
    return Path(name).stem or "model"


def parse_document(source: str, path: str | Path | None = None) -> A.Module:
    if is_python_dsl_path(path):
        filename = _path_text(path)
        return compile_source(source, filename=filename, default_module_name=default_module_name_for_path(path))
    return parse(source)


def load_module(path: str | Path) -> A.Module:
    file_path = Path(path)
    return parse_document(file_path.read_text(encoding="utf-8"), path=file_path)


def _path_text(path: str | Path) -> str:
    text = str(path)
    parsed = urlparse(text)
    if parsed.scheme == "file":
        return url2pathname(unquote(parsed.path))
    return text
