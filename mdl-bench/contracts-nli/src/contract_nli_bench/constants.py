from __future__ import annotations

DATASET_URL = "https://stanfordnlp.github.io/contract-nli/resources/contract-nli.zip"
DATASET_SHA256 = "e03fc77bbf8b53e2976a250e81d8a294bc3d5e5fb014521e477dee9340d6287b"
ARCHIVE_NAME = "contract-nli.zip"

SPLITS = ("train", "dev", "test")
RUN_SCOPES = ("dev", "full")
CHOICE_ENTAILMENT = "Entailment"
CHOICE_CONTRADICTION = "Contradiction"
CHOICE_NOT_MENTIONED = "NotMentioned"
EXPECTED_SOLVE_STATUS = {
    CHOICE_ENTAILMENT: "sat",
    CHOICE_CONTRADICTION: "unsat",
}

BENCHMARK_NAME = "contracts-nli-no-align"
