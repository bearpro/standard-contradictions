# Contract-NLI MDL Benchmark

This benchmark evaluates a generation scenario, not just a model. A run is
identified by both the model slug and a short prompt scenario slug:

```text
data/generated/{model_slug}-no-align/{scenario_slug}/{scope}/{split}/{doc_id}/{hypothesis_id}.mdl
```

The model receives only the contract text and one hypothesis. It does not receive
the Contract-NLI gold label or evidence spans. The generated artifact is a single
MDL module containing both the document model and the hypothesis model. The
benchmark then runs `mdl solve`: `Contradiction` is expected to be `unsat`,
`Entailment` is expected to be `sat`, and `NotMentioned` is reported but skipped
from the main v1 accuracy.

## Layout

- `src/contract_nli_bench/`: benchmark code and CLI.
- `prompts/`: prompt scenario templates.
- `data/raw/`: downloaded and extracted Contract-NLI dataset, ignored by git.
- `data/generated/`: paid generation artifacts, intentionally allowed by git.

Each generated case stores:

- `.mdl`: extracted MDL source used by validation and evaluation.
- `.raw.txt`: raw model text returned by the API.
- `.meta.json`: run metadata, prompt hash, model, scenario, usage, and response
  metadata. It intentionally does not store the gold label or evidence spans.

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

## Iterating On Prompts

Use the normal Contract-NLI `dev` split before spending money on `full`:

```bash
uv run contract-nli-bench infer \
  dev \
  --model gpt-5-mini \
  --scenario baseline-v1

uv run contract-nli-bench evaluate \
  dev \
  --model gpt-5-mini \
  --scenario baseline-v1
```

`dev` writes and reads artifacts under the `.../{scenario_slug}/dev/` run
directory. `full` covers train, dev, and test and uses the separate
`.../{scenario_slug}/full/` directory.

To add a new prompt scenario:

1. Add a template under `prompts/`, or pass `--prompt-template`.
2. Choose a short ASCII slug such as `rules-only-v2`.
3. Run inference with `--scenario rules-only-v2` and either `dev` or `full`.
4. Commit the resulting `data/generated/.../rules-only-v2/{scope}/...`
   artifacts if the run should be preserved.

## Full Runs

Run all splits:

```bash
uv run contract-nli-bench infer \
  full \
  --model gpt-5-mini \
  --scenario baseline-v1

uv run contract-nli-bench validate \
  full \
  --model gpt-5-mini \
  --scenario baseline-v1

uv run contract-nli-bench evaluate \
  full \
  --model gpt-5-mini \
  --scenario baseline-v1 \
  --horizon 1
```

Evaluation writes `results.jsonl` and `summary.json` under the model/scenario
run directory.
