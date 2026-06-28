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
from contract_nli_bench.mdl_tools import call_mdl_verify_tool
from contract_nli_bench.paths import artifact_paths
from contract_nli_bench.prompting import extract_mdl_source
from contract_nli_bench.runner import (
    GenerationResult,
    InferenceConfig,
    OpenAIResponsesGenerator,
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


def test_artifact_path_uses_scenario_slug_only(tmp_path: Path) -> None:
    paths = artifact_paths(
        tmp_path,
        "Rules Only V2",
        3,
        "nda-11",
    )

    assert paths.mdl == (
        tmp_path
        / "generated"
        / "rules-only-v2"
        / "3"
        / "nda-11"
        / "generated.mdl"
    )
    assert paths.raw == (
        tmp_path / "generated" / "rules-only-v2" / "3" / "nda-11" / "raw.txt"
    )
    assert paths.user_prompt == (
        tmp_path
        / "generated"
        / "rules-only-v2"
        / "3"
        / "nda-11"
        / "user_prompt.txt"
    )
    assert paths.meta == tmp_path / "generated" / "rules-only-v2" / "meta.json"


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
    assert (records[0].mdl_path.parent / "user_prompt.txt").read_text(
        encoding="utf-8"
    ) == (
        f"{case.module_name}\n{case.text}\n{case.hypothesis}\n"
    )
    run_metadata = json.loads(records[0].meta_path.read_text(encoding="utf-8"))
    assert run_metadata["scenario_slug"] == "baseline-v1"
    assert run_metadata["model"] == "gpt-5-mini"
    assert run_metadata["scope"] == "dev"
    assert run_metadata["splits"] == ["dev"]
    assert run_metadata["completed_at"]
    assert run_metadata["system_prompt"] == "inline-system"
    assert run_metadata["user_prompt_template"] == "inline-user"
    assert run_metadata["system_prompt_sha256"]
    assert run_metadata["user_prompt_template_sha256"]
    assert "choice" not in run_metadata
    assert "evidence" not in json.dumps(run_metadata)
    case_metadata = json.loads(
        (records[0].mdl_path.parent / "meta.json").read_text(encoding="utf-8")
    )
    assert case_metadata["case_id"] == case.case_id
    assert case_metadata["user_prompt_sha256"]
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


def test_mdl_verify_tool_reports_parse_and_lint_status() -> None:
    parse_payload = call_mdl_verify_tool(
        json.dumps({"source": "module broken\nfunc x( -> bool: true\n"})
    )
    assert parse_payload["ok"] is False
    assert parse_payload["phase"] == "parse"

    lint_payload = call_mdl_verify_tool(
        json.dumps({"source": "module bad\nrule O missing: later always\n"})
    )
    assert lint_payload["ok"] is False
    assert lint_payload["phase"] == "lint"

    ok_payload = call_mdl_verify_tool(
        json.dumps({"source": "module generated\nfact true\n"})
    )
    assert ok_payload["ok"] is True
    assert ok_payload["phase"] == "complete"
    assert "module generated" in ok_payload["formatted_source"]


def test_openrouter_mdl_tool_loop_repairs_until_verified_source() -> None:
    client = FakeOpenAIClient(
        [
            {
                "id": "resp_1",
                "output": [
                    {
                        "type": "function_call",
                        "name": "mdl_verify",
                        "call_id": "call_1",
                        "arguments": json.dumps(
                            {"source": "module broken\nfunc x( -> bool: true\n"}
                        ),
                    }
                ],
            },
            {
                "id": "resp_2",
                "output": [
                    {
                        "type": "function_call",
                        "name": "mdl_verify",
                        "call_id": "call_2",
                        "arguments": json.dumps(
                            {"source": "module generated\nfact true\n"}
                        ),
                    }
                ],
            },
        ]
    )
    generator = OpenAIResponsesGenerator(
        model="openai/o4-mini",
        api_key_env="OPENAI_API_KEY",
        base_url="https://openrouter.ai/api/v1",
        max_output_tokens=9000,
        temperature=None,
        mdl_tools=True,
        mdl_tool_max_attempts=4,
        client=client,
    )

    result = generator("system", "user", {"case_id": "dev-1-nda-1"})

    assert len(client.responses.calls) == 2
    assert client.responses.calls[0]["tool_choice"] == {
        "type": "function",
        "name": "mdl_verify",
    }
    second_input = client.responses.calls[1]["input"]
    assert client.responses.calls[1]["previous_response_id"] == "resp_1"
    assert len(second_input) == 2
    assert second_input[0]["type"] == "function_call_output"
    assert second_input[0]["call_id"] == "call_1"
    assert second_input[1]["role"] == "user"
    assert second_input[1]["content"] == second_input[0]["output"]
    assert result.verified_source is not None
    assert "module generated" in result.verified_source
    assert [item["phase"] for item in result.tool_trace] == ["parse", "complete"]
    assert [item["source"] for item in result.tool_documents] == [
        "module broken\nfunc x( -> bool: true\n",
        "module generated\nfact true\n",
    ]


def test_mdl_tool_loop_does_not_add_text_output_message_for_other_models() -> None:
    client = FakeOpenAIClient(
        [
            {
                "id": "resp_1",
                "output": [
                    {
                        "type": "function_call",
                        "name": "mdl_verify",
                        "call_id": "call_1",
                        "arguments": json.dumps(
                            {"source": "module broken\nfunc x( -> bool: true\n"}
                        ),
                    }
                ],
            },
            {
                "id": "resp_2",
                "output": [
                    {
                        "type": "function_call",
                        "name": "mdl_verify",
                        "call_id": "call_2",
                        "arguments": json.dumps(
                            {"source": "module generated\nfact true\n"}
                        ),
                    }
                ],
            },
        ]
    )
    generator = OpenAIResponsesGenerator(
        model="google/gemini-2.5-pro",
        api_key_env="OPENAI_API_KEY",
        base_url="https://openrouter.ai/api/v1",
        max_output_tokens=9000,
        temperature=None,
        mdl_tools=True,
        mdl_tool_max_attempts=4,
        client=client,
    )

    result = generator("system", "user", {"case_id": "dev-1-nda-1"})

    second_input = client.responses.calls[1]["input"]
    assert client.responses.calls[1]["previous_response_id"] == "resp_1"
    assert len(second_input) == 1
    assert second_input[0]["type"] == "function_call_output"
    assert result.verified_source is not None


def test_infer_cases_writes_mdl_tool_trace_and_metadata(tmp_path: Path) -> None:
    case = make_case(choice="Contradiction")

    def fake_generator(
        system_prompt: str,
        user_prompt: str,
        metadata: dict[str, str],
    ) -> GenerationResult:
        return GenerationResult(
            output_text="raw model text",
            response={"id": "resp_test"},
            verified_source="module generated\nfact true\n",
            tool_trace=(
                {
                    "attempt": 1,
                    "tool": "mdl_verify",
                    "call_id": "call_1",
                    "ok": True,
                    "phase": "complete",
                    "summary": {
                        "errors": 0,
                        "warnings": 0,
                        "solver_status": None,
                        "conflicts": 0,
                    },
                    "diagnostics": [],
                },
            ),
            tool_documents=(
                {
                    "attempt": 1,
                    "tool": "mdl_verify",
                    "call_id": "call_1",
                    "source": "module generated\nfact true\n",
                },
            ),
        )

    records = infer_cases(
        [case],
        InferenceConfig(
            data_root=tmp_path,
            model="openai/o4-mini",
            scenario="tools-v1",
            scope="dev",
            system_prompt="Generate MDL.",
            system_prompt_name="inline-system",
            user_prompt_template="{{document_text}}",
            user_prompt_template_name="inline-user",
            mdl_tools=True,
            mdl_tool_max_attempts=4,
        ),
        fake_generator,
    )

    assert records[0].mdl_path.read_text(encoding="utf-8") == (
        "module generated\nfact true\n"
    )
    trace = json.loads(
        (records[0].mdl_path.parent / "tool_trace.json").read_text(encoding="utf-8")
    )
    assert trace["trace"][0]["phase"] == "complete"
    tool_input = records[0].mdl_path.parent / "tool_inputs" / "attempt-001-mdl_verify.mdl"
    assert tool_input.read_text(encoding="utf-8") == "module generated\nfact true\n"
    case_metadata = json.loads(
        (records[0].mdl_path.parent / "meta.json").read_text(encoding="utf-8")
    )
    assert case_metadata["mdl_tools"] is True
    assert case_metadata["mdl_tool_attempts"] == 1
    assert case_metadata["mdl_tool_final_ok"] is True
    run_metadata = json.loads(records[0].meta_path.read_text(encoding="utf-8"))
    assert run_metadata["mdl_tools"] is True
    assert run_metadata["mdl_tool_max_attempts"] == 4


def test_evaluate_cases_maps_entailment_to_sat_and_contradiction_to_unsat(
    tmp_path: Path,
) -> None:
    entailment = make_case(doc_id=1, choice="Entailment", hypothesis_id="nda-1")
    contradiction = make_case(
        doc_id=2,
        choice="Contradiction",
        hypothesis_id="nda-2",
    )
    scenario = "baseline-v1"

    write_artifact(
        tmp_path,
        scenario,
        entailment,
        f"""module {entailment.module_name}
entity x: bool
fact x = true
""",
    )
    write_artifact(
        tmp_path,
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
        scenario=scenario,
        horizon=1,
        write=True,
    )

    assert summary["evaluated_cases"] == 2
    assert summary["correct_evaluated"] == 2
    assert summary["accuracy_evaluated"] == 1.0
    assert (tmp_path / "generated" / "baseline-v1" / "results.jsonl").exists()


def write_artifact(
    data_root: Path,
    scenario: str,
    case: ContractNliCase,
    source: str,
) -> None:
    paths = artifact_paths(
        data_root,
        scenario,
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


class FakeOpenAIClient:
    def __init__(self, payloads: list[dict[str, object]]) -> None:
        self.responses = FakeResponses(payloads)


class FakeResponses:
    def __init__(self, payloads: list[dict[str, object]]) -> None:
        self.payloads = list(payloads)
        self.calls: list[dict[str, object]] = []

    def create(self, **kwargs: object) -> dict[str, object]:
        self.calls.append(kwargs)
        assert self.payloads
        return self.payloads.pop(0)
