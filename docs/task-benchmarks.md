# Task-suite benchmarks

`monad-bench` runs a *suite* of tasks against a *matrix* of agent
configurations, several times each, grades every answer, and reports success
rate, score, and latency per configuration — so you can compare model /
strategy combinations automatically.

A suite is one YAML file. Each run makes real provider requests and may incur
usage charges.

## Run

```console
cabal build agent-cli:exe:monad-cli agent-cli:exe:monad-bench

monad-bench \
  --suite tasks.yaml \
  --agent-bin "$(cabal list-bin agent-cli:exe:monad-cli)" \
  --results-dir bench-results
```

- `--suite FILE` — the YAML suite (required).
- `--agent-bin PATH` — the `monad-cli` executable (required).
- `--results-dir DIR` — output directory (default `bench-results`).

Output: `results.json` (one object per trial) and `summary.md` (per-task
success/score/latency table plus a configuration ranking).

## Schema

```yaml
name: example                              # suite name (shown in summary.md)
runs:                                      # optional
  trials: 3                                # repeats per (configuration, task)
  timeout_seconds: 180                     # per-trial wall-clock budget
  max_concurrency: 4                       # parallel trials

matrix:                                    # configurations to compare
  - id: deepseek-flash                     # unique id (shown in the report)
    provider: deepseek                     # optional; omit to auto-detect
    model: deepseek-v4-flash               # optional; omit for the default
    connection: deepseek                   # optional
    effort: medium                         # optional reasoning effort
    shell: default                         # default | ghci | bash | none

tasks:                                     # the work to grade
  - id: hello                              # unique id
    prompt: "Reply with exactly: ok"       # the task given to the agent
    validator:
      type: contains
      needle: ok
```

### `runs`

| field | default | meaning |
|---|---|---|
| `trials` | 1 | how many times each (configuration, task) runs |
| `timeout_seconds` | 180 | per-trial wall-clock budget; a slow run counts as a failure |
| `max_concurrency` | 4 | how many trials run at once |

### `matrix`

Each entry is one agent configuration. The only required field is `id`;
everything else is optional and passed to `monad-cli` when present.

- `provider`, `model`, `connection`, `effort` map to the corresponding
  `monad-cli` flags.
- `shell` selects the tool surface: `default` (provider default),
  `ghci` (`--ghci --no-bash`), `bash` (`--bash --no-ghci`), or `none`
  (`--no-bash --no-ghci`).

To compare subagent strategies, point different matrix entries at different
`~/.haskell-agent/subagents.json` files (future: a first-class
`strategy.subagents` axis).

### `tasks`

Each task is `id` + `prompt` + `validator`. The agent runs as a one-shot
session with the prompt; its final stdout answer is handed to the validator.

### `validator`

One of:

| type | fields | pass when |
|---|---|---|
| `exact` | `expect: <text>` | the trimmed answer equals `expect` |
| `contains` | `needle: <text>` | the answer contains `needle` |
| `script` | `command: [cmd, arg, …]` | the command exits 0 |
| `judge` | `rubric: <text>`, `judge_model` (optional), `scale` (default 5) | an LLM judge scores ≥ `scale/2 + 1` |

```yaml
validator:
  type: judge
  rubric: "Names the tradeoff and gives two concrete levers; no factual errors."
  judge_model: gpt-5.6-sol     # optional; defaults to the combo's model
  scale: 5
```

`script` validators run in the suite's working directory; give them an
absolute path or make the fixture files available alongside the suite.

## Example

See [`docs/examples/task-benchmark-example.yaml`](examples/task-benchmark-example.yaml).

## Notes

- The suite is validated at load: duplicate matrix/task ids, unknown
  validator types, empty prompts, and out-of-range numbers are rejected with
  a named error.
- Latency is wall-clock and includes provider cold-start; treat it as a
  rough signal, not a micro-benchmark.
- `judge` costs one extra model call per trial.
