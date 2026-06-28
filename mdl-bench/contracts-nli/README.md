# Contract-NLI MDL Benchmark

This benchmark evaluates a generation scenario, not just a model. A scenario is
the pair of a system prompt and a user data template, and a run is stored under
a short scenario slug:

```text
data/generated/{scenario_slug}/{doc_id}/{hypothesis_id}/generated.mdl
```

The model receives a generic MDL system prompt plus a user prompt containing
only the contract text and one hypothesis. It does not receive the Contract-NLI
gold label or evidence spans. The generated artifact is a single MDL module
containing both the document model and the hypothesis model. The benchmark then
runs `mdl solve`: `Contradiction` is expected to be `unsat`, `Entailment` is
expected to be `sat`, and `NotMentioned` is reported but skipped from the main
v1 accuracy.

## Layout

- `src/contract_nli_bench/`: benchmark code and CLI.
- `prompts/`: prompt scenario templates.
- `data/raw/`: downloaded and extracted Contract-NLI dataset, ignored by git.
- `data/generated/`: paid generation artifacts, intentionally allowed by git.

Each generated case directory stores:

- `generated.mdl`: extracted MDL source used by validation and evaluation.
- `raw.txt`: raw model text returned by the API.
- `user_prompt.txt`: rendered user prompt sent to the model for this case.
- `meta.json`: case metadata without the gold label or evidence spans.

The scenario run directory also stores `meta.json` with the inference
configuration: start/end timestamps, model, scope, splits, prompt file names and
hashes, generation counts, and API settings.

## Setup

Download the dataset after reviewing the upstream terms:

```bash
uv run contract-nli-bench download --accept-terms
```

This stores the original archive as `data/raw/contract-nli.zip` and extracts it
to `data/raw/contract-nli/`. Inference, validation, and evaluation read the
extracted JSON files only.

For OpenAI-compatible inference, set an API key:

```bash
cp mdl-bench/contracts-nli/.env.sample mdl-bench/contracts-nli/.env
$EDITOR mdl-bench/contracts-nli/.env
```

The CLI loads `.env` from the repository root and from `mdl-bench/contracts-nli/`.
Values already exported in the shell take precedence.

For OpenRouter, pass its OpenAI-compatible Responses API base URL and use an
OpenRouter model slug:

```bash
uv run contract-nli-bench infer \
  dev \
  --model openai/o4-mini \
  --scenario baseline-v1
```

Set `OPENAI_BASE_URL=https://openrouter.ai/api/v1` in `.env`, or pass
`--base-url https://openrouter.ai/api/v1`. Keep the provider key in
`OPENAI_API_KEY`.

To let OpenRouter models check generated MDL before the benchmark accepts it,
enable the MDL verifier tool loop:

```bash
uv run contract-nli-bench infer \
  dev \
  --model openai/o4-mini \
  --scenario mcp-v1 \
  --mdl-tools
```

`--mdl-tools` exposes the existing `mdl_verify` verifier as a Responses API
function tool. The infer step accepts a candidate only after MDL parse and lint
checks pass; it does not require `mdl solve` to return `sat`, because
`Contradiction` cases are expected to evaluate to `unsat`. Use
`--mdl-tool-max-attempts` to change the default limit of 4 verification attempts
per case. Tool-enabled runs write `tool_trace.json` in each generated case
directory, plus `tool_inputs/attempt-*.mdl` with every MDL source passed to the
verifier.

## Iterating On Prompts

Use the normal Contract-NLI `dev` split before spending money on `full`:

```bash
uv run contract-nli-bench infer \
  dev \
  --model gpt-5-mini \
  --scenario baseline-v1

uv run contract-nli-bench evaluate \
  dev \
  --scenario baseline-v1
```

`dev` writes and reads artifacts under `data/generated/{scenario_slug}/`.
`full` covers train, dev, and test in the same scenario directory, so use a
different `--scenario` slug when you want to preserve separate runs. Inference
prints progress, elapsed time, and ETA to stderr; pass `--no-progress` to
silence it.

To add a new prompt scenario:

1. Add system/user prompt files under `prompts/`, or pass `--system-prompt` and
   `--user-template`.
2. Choose a short ASCII slug such as `rules-only-v2`.
3. Run inference with `--scenario rules-only-v2` and either `dev` or `full`.
4. Commit the resulting `data/generated/rules-only-v2/` artifacts if the run
   should be preserved.

## Full Runs

Run all splits:

```bash
uv run contract-nli-bench infer \
  full \
  --model gpt-5-mini \
  --scenario baseline-v1

uv run contract-nli-bench validate \
  full \
  --scenario baseline-v1

uv run contract-nli-bench evaluate \
  full \
  --scenario baseline-v1 \
  --horizon 1
```

Evaluation writes `results.jsonl` and `summary.json` under the scenario run
directory.
