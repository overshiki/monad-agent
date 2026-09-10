---
name: task-benchmark
description: Write and run task-suite benchmarks that compare model and agent-strategy combinations across a set of graded tasks, using the monad-bench YAML suite format.
when-to-use: Use when the user asks to compare models or agent strategies, set up an eval or benchmark of agent tasks, measure success rate on a set of prompts, or write a tasks.yaml suite for monad-bench.
argument-hint: "<task suite file, or a set of tasks to benchmark>"
---
# Task-suite benchmarks with monad-bench

`monad-bench` runs every (configuration × task) cell of a YAML suite several
times through one-shot `monad-cli` sessions, grades each answer, and writes
`results.json` and `summary.md` with success rate, score, and latency per
configuration plus a ranking. Use it to answer "which model / tool mode /
effort does best on my tasks", automatically.

## Schema essentials

A suite is one YAML file:

```yaml
name: my-comparison
runs: { trials: 3, timeout_seconds: 180, max_concurrency: 4 }
matrix:                     # configurations to compare
  - { id: a, model: deepseek-v4-flash }
  - { id: b, model: deepseek-v4-flash, shell: ghci }
tasks:                      # work to grade
  - id: hello
    prompt: "Reply with exactly: ok"
    validator: { type: exact, expect: ok }
```

- `matrix` entries need only an `id`; `provider` / `model` / `connection` /
  `effort` / `shell` are optional flags. `shell` is `default`, `ghci`,
  `bash`, or `none` (maps to `--ghci`/`--no-bash` etc.).
- `tasks` entries need `id`, `prompt`, and `validator`.
- `validator` types: `exact` (`expect`), `contains` (`needle`), `script`
  (`command: [cmd, …]`, pass on exit 0), `judge` (`rubric`, optional
  `judge_model`, optional `scale` default 5; pass when score ≥ scale/2+1).

Full reference: `docs/task-benchmarks.md`. A runnable example:
`docs/examples/task-benchmark-example.yaml`.

## Run

```console
cabal build agent-cli:exe:monad-cli agent-cli:exe:monad-bench
monad-bench --suite tasks.yaml \
  --agent-bin "$(cabal list-bin agent-cli:exe:monad-cli)" \
  --results-dir bench-results
```

## Writing a good suite

1. Prefer deterministic validators (`exact`/`contains`/`script`) where a task
   has a checkable answer; use `judge` only for open-ended ones, and give it
   a precise rubric and a small `scale` (3) so grading is repeatable.
2. Use `trials: 3` (or more) before comparing; single trials are noise.
   Keep `timeout_seconds` generous enough that slow-but-correct answers
   aren't counted as failures.
3. Change one axis at a time across matrix entries (model, tool mode, effort)
   so the ranking explains itself.
4. A `script` validator runs in the suite's directory — use absolute paths
   or ship fixtures next to the suite.
5. Each trial is a real provider request and may incur charges; `judge`
   doubles the calls.

## Reporting

After a run, `summary.md` has a per-task table and a `## Ranking` section.
Read `results.json` for per-trial detail (`output`, `error`, `latency_ms`,
`score`). Report the ranking, note the trial count, and flag any task whose
success differs sharply between configurations as needing more trials.
