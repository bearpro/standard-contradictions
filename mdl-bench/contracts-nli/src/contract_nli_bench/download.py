from __future__ import annotations

import hashlib
import urllib.request
import zipfile
from pathlib import Path

from .constants import ARCHIVE_NAME, DATASET_SHA256, DATASET_URL


def download_dataset(data_root: Path, accept_terms: bool) -> Path:
    if not accept_terms:
        raise ValueError("pass --accept-terms after reviewing upstream terms")
    raw_root = data_root / "raw"
    raw_root.mkdir(parents=True, exist_ok=True)
    archive = raw_root / ARCHIVE_NAME
    if not archive.exists():
        with urllib.request.urlopen(DATASET_URL) as response:
            archive.write_bytes(response.read())
    digest = sha256_file(archive)
    if digest != DATASET_SHA256:
        raise ValueError(
            f"unexpected SHA256 for {archive}: {digest}; expected {DATASET_SHA256}"
        )
    extract_dataset(archive, raw_root)
    return archive


def extract_dataset(archive: Path, raw_root: Path) -> Path:
    output_root = raw_root / "contract-nli"
    with zipfile.ZipFile(archive) as zf:
        zf.extractall(raw_root)
    return output_root


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as fp:
        for chunk in iter(lambda: fp.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()
