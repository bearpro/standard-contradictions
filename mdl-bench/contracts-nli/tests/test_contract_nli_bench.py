from __future__ import annotations

import json
import zipfile
from pathlib import Path

from contract_nli_bench.cases import (
    ContractNliCase,
    resolve_cases,
)
from contract_nli_bench.download import extract_dataset
from contract_nli_bench.evaluation import evaluate_cases
from contract_nli_bench.paths import artifact_paths
from contract_nli_bench.prompting import extract_mdl_source
from contract_nli_bench.runner import (
    GenerationResult,
    InferenceConfig,
    infer_cases,
)


def test_resolve_cases_supports_dev_and_full_scopes(tmp_path: Path) -> None:
    write_split(tmp_path, "train", [make_doc(1, "Entailment", [])])
    write_split(tmp_path, "dev", [make_doc(2, "Contradiction", [0])])
    write_split(tmp_path, "test", [make_doc(3, "NotMentioned", [])])

    assert [case.case_id for case in resolve_cases(tmp_path, scope="dev")] == [
        "dev-2-nda-1"
    ]
    assert [case.case_id for case in resolve_cases(tmp_path, scope="full")] == [
        "train-1-nda-1",
        "dev-2-nda-1",
        "test-3-nda-1",
    ]


def test_artifact_path_uses_model_and_scenario_slugs(tmp_path: Path) -> None:
    paths = artifact_paths(
        tmp_path,
        "openai/gpt-5.2",
        "Rules Only V2",
        "full",
        "dev",
        3,
        "nda-11",
    )

    assert paths.mdl == (
        tmp_path
        / "generated"
        / "openai-gpt-5.2-no-align"
        / "rules-only-v2"
        / "full"
        / "dev"
        / "3"
        / "nda-11"
        / "nda-11.mdl"
    )


def test_extract_dataset_unpacks_contract_nli_directory(tmp_path: Path) -> None:
    archive = tmp_path / "contract-nli.zip"
    with zipfile.ZipFile(archive, "w") as zf:
        zf.writestr("contract-nli/dev.json", '{"labels": {}, "documents": []}')

    output_root = extract_dataset(archive, tmp_path / "raw")

    assert output_root == tmp_path / "raw" / "contract-nli"
    assert (output_root / "dev.json").exists()


def test_extract_mdl_source_prefers_mdl_fence() -> None:
    text = """ignore
```text
not mdl
```
```mdl
module generated
fact true
```
"""

    assert extract_mdl_source(text) == "module generated\nfact true\n"


def test_infer_cases_writes_artifacts_without_gold_metadata(tmp_path: Path) -> None:
    case = make_case(choice="Contradiction")
    generator_calls = 0

    def fake_generator(
        system_prompt: str,
        user_prompt: str,
        metadata: dict[str, str],
    ) -> GenerationResult:
        nonlocal generator_calls
        generator_calls += 1
        assert system_prompt
        assert user_prompt
        assert metadata["case_id"] == case.case_id
        return GenerationResult(
            output_text="```mdl\nmodule generated\nfact true\n```",
            response={"id": "resp_test", "usage": {"total_tokens": 10}},
        )

    records = infer_cases(
        [case],
        InferenceConfig(
            data_root=tmp_path,
            model="gpt-5-mini",
            scenario="baseline-v1",
            scope="dev",
            system_prompt="Generate MDL.",
            system_prompt_name="inline-system",
            user_prompt_template="{{module_name}}\n{{document_text}}\n{{hypothesis}}\n",
            user_prompt_template_name="inline-user",
        ),
        fake_generator,
    )

    assert len(records) == 1
    assert records[0].mdl_path.read_text(encoding="utf-8") == (
        "module generated\nfact true\n"
    )
    metadata = json.loads(records[0].meta_path.read_text(encoding="utf-8"))
    assert metadata["scenario_slug"] == "baseline-v1"
    assert metadata["system_prompt"] == "inline-system"
    assert metadata["user_prompt_template"] == "inline-user"
    assert metadata["system_prompt_sha256"]
    assert metadata["user_prompt_sha256"]
    assert "choice" not in metadata
    assert "evidence" not in json.dumps(metadata)
    assert generator_calls == 1

    skipped = infer_cases(
        [case],
        InferenceConfig(
            data_root=tmp_path,
            model="gpt-5-mini",
            scenario="baseline-v1",
            scope="dev",
            system_prompt="Generate MDL.",
            system_prompt_name="inline-system",
            user_prompt_template="{{document_text}}",
            user_prompt_template_name="inline-user",
        ),
        fake_generator,
    )
    assert skipped[0].skipped_existing is True
    assert generator_calls == 1


def test_evaluate_cases_maps_entailment_to_sat_and_contradiction_to_unsat(
    tmp_path: Path,
) -> None:
    entailment = make_case(doc_id=1, choice="Entailment", hypothesis_id="nda-1")
    contradiction = make_case(
        doc_id=2,
        choice="Contradiction",
        hypothesis_id="nda-2",
    )
    model = "gpt-5-mini"
    scenario = "baseline-v1"

    write_artifact(
        tmp_path,
        model,
        scenario,
        entailment,
        f"""module {entailment.module_name}
entity x: bool
fact x = true
""",
    )
    write_artifact(
        tmp_path,
        model,
        scenario,
        contradiction,
        f"""module {contradiction.module_name}
entity x: bool
fact x = true
fact x = false
""",
    )

    _, summary = evaluate_cases(
        [entailment, contradiction],
        data_root=tmp_path,
        model=model,
        scenario=scenario,
        scope="dev",
        horizon=1,
        write=True,
    )

    assert summary["evaluated_cases"] == 2
    assert summary["correct_evaluated"] == 2
    assert summary["accuracy_evaluated"] == 1.0
    assert (
        tmp_path
        / "generated"
        / "gpt-5-mini-no-align"
        / "baseline-v1"
        / "dev"
        / "results.jsonl"
    ).exists()


def write_artifact(
    data_root: Path,
    model: str,
    scenario: str,
    case: ContractNliCase,
    source: str,
) -> None:
    paths = artifact_paths(
        data_root,
        model,
        scenario,
        "dev",
        case.split,
        case.doc_id,
        case.hypothesis_id,
    )
    paths.case_root.mkdir(parents=True, exist_ok=True)
    paths.mdl.write_text(source, encoding="utf-8")


def write_split(tmp_path: Path, split: str, docs: list[dict[str, object]]) -> None:
    payload = {
        "labels": {
            "nda-1": {
                "short_description": "Limited use",
                "hypothesis": "Receiving Party shall use information only as agreed.",
            }
        },
        "documents": docs,
    }
    split_path = tmp_path / "raw" / "contract-nli" / f"{split}.json"
    split_path.parent.mkdir(parents=True, exist_ok=True)
    split_path.write_text(
        json.dumps(payload),
        encoding="utf-8",
    )


def make_doc(doc_id: int, choice: str, evidence: list[int]) -> dict[str, object]:
    return {
        "id": doc_id,
        "file_name": f"doc-{doc_id}.txt",
        "document_type": "sec-text",
        "url": "https://example.com",
        "text": f"Contract text {doc_id}.",
        "spans": [[0, 10]],
        "annotation_sets": [
            {
                "annotations": {
                    "nda-1": {
                        "choice": choice,
                        "spans": evidence,
                    }
                }
            }
        ],
    }


def make_case(
    doc_id: int = 1,
    choice: str = "Entailment",
    hypothesis_id: str = "nda-1",
) -> ContractNliCase:
    return ContractNliCase(
        split="dev",
        doc_id=doc_id,
        file_name=f"doc-{doc_id}.txt",
        document_type="sec-text",
        url="https://example.com",
        text=f"Contract text {doc_id}.",
        spans=((0, 10),),
        hypothesis_id=hypothesis_id,
        short_description="Limited use",
        hypothesis="Receiving Party shall use information only as agreed.",
        choice=choice,
        evidence_span_indices=(0,),
    )
