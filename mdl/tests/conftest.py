from __future__ import annotations

from pathlib import Path

import pytest


@pytest.fixture(autouse=True)
def mdl_stdlib_path(monkeypatch: pytest.MonkeyPatch) -> None:
    stdlib = Path(__file__).resolve().parents[1] / "src" / "mdl" / "stdlib"
    monkeypatch.setenv("MDL_STDLIB_PATH", str(stdlib))
