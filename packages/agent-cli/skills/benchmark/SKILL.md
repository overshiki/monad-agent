---
name: benchmark
description: Run and validate performance benchmarks in Haskell/cabal projects from monad-cli, and write the project benchmark description that lets agents run them.
when-to-use: Use when the user asks to benchmark code, validate a performance claim or improvement, find or run a project's benchmark suite, or when a project wants to document its benchmarks so monad-cli can run them.
argument-hint: "<benchmark name or workload, or a performance change to validate>"
---
# Running benchmarks from monad-cli

Performance work is only accepted when backed by a benchmark that proves the
claimed improvement. This skill covers (a) finding and running benchmarks in
Haskell/cabal projects, (b) measuring rigorously, and (c) the description a
project writes so its benchmarks can be run from monad-cli at all.

## Where benchmark descriptions come from

The agent discovers how to benchmark a project from three sources, in order:

1. **Project instructions** — the files the project ships for the agent
   (`AGENTS.md`, `AGENTS.override.md`, `Claude.md`, `CLAUDE.md`,
   `.claude/CLAUDE.md`), discovered per directory by the project-instruction
   loader at session start. A `## Performance` / `## Benchmarks` section in
   `AGENTS.md` is the canonical way for a project to describe its
   benchmarks. See "How a project describes its benchmarks" below.
2. **Skills** — this benchmark skill, plus any project- or user-scoped
   learned skills, loaded from the skills catalog.
3. **The code itself** — when neither source names a benchmark, discover one.

## Finding a project's benchmarks

Haskell/cabal projects declare benchmarks as components:

```console
grep -n '^benchmark ' **/*.cabal
```

A component named `foo-bench` in package `bar` is built and located with:

```console
cabal build bar:bench:foo-bench
cabal list-bin bar:bench:foo-bench
```

Then run the located binary directly — do not benchmark through GHCi or a
debug build.

### This repository's benchmarks

In `haskell-agent`, the benchmark protocol lives in the repo's `AGENTS.md`
(`## performance`, `## streaming text benchmark`). Components:

- `agent-core` benchmarks (allocation-focused, built `-O2`, `+RTS -T`):
  `grep-streaming-bench`, `output-artifact-bench`, `streaming-text-bench`,
  `read-file-streaming-bench`, `tool-scheduling-bench`, `loop-events-bench`,
  `concurrent-discovery-bench`, `code-mode-pool-bench`.
- `agent-cli` benchmarks (interactive-latency and retention): 
  `subagent-retention-bench`, `conversation-retention-bench`,
  `history-retention-bench`, `concurrency-bench`,
  `image-preview-latency-bench`, `clipboard-latency-bench`,
  `composer-paste-latency-bench`, `transcript-scrolling-bench`.

Example:

```console
cabal build --offline agent-core:bench:streaming-text-bench
bin=$(cabal list-bin agent-core:bench:streaming-text-bench)
"$bin" old-accumulate 10000 16 7 +RTS -T
"$bin" new-accumulate 10000 16 7 +RTS -T
```

## Measurement discipline

Performance claims must be supported by a benchmark that proves them:

- Benchmark the old and the new implementation with equivalent,
  representative workloads. Keep a baseline workload in the benchmark so
  future changes can be compared against the behavior being replaced.
- Use an optimized build rather than interpreted/GHCi timings. Run against
  an optimized (`-O2`) build, force the result so GHC cannot optimize the
  work away, and enable RTS statistics when measuring allocation.
- Measure both elapsed/CPU time and allocated bytes when the change is
  intended to reduce allocation or GC pressure. A speedup that shifts work
  or allocation to another stage is not sufficient.
- Run multiple samples and report a robust aggregate such as the median.
  Test multiple input sizes to distinguish constant/linear behavior from
  quadratic behavior, and repeat at least one representative case to check
  measurement stability.
- Record the benchmark command, GHC/build settings, workload dimensions,
  and before/after results in the PR. Keep the benchmark in the repository
  when it is useful for detecting regressions.
- If benchmarking shows that part of a proposed optimization regresses, do
  not ship that part. Revert it or redesign it, and document the remaining
  performance boundary.

## How a project describes its benchmarks

For monad-cli to run a project's benchmarks, add a section to the project's
`AGENTS.md` (or a `BENCHMARKS.md` linked from it). Keep it concrete and
executable — the agent will run these exact commands:

```markdown
## performance

Performance improvements must be supported by a benchmark that proves the
claimed improvement before opening a PR. [measurement rules…]

## benchmarks

The benchmark suite is `<pkg>:bench:<name>` etc.

```console
cabal build <pkg>:bench:<name>
bin=$(cabal list-bin <pkg>:bench:<name>)
"$bin" <workload-args> +RTS -T
```

Each workload argument is positional: <describe each>. A successful run
prints <expected output>. Run N samples and report the median CPU time and
allocated bytes; keep the baseline case for regression comparison.
```

Notes for the author:

- Name components consistently (`*-bench`) and set `-O2 -rtsopts` in each
  benchmark stanza's `ghc-options` so timing is meaningful and `+RTS`
  works.
- Document positional arguments and expected output: a benchmark nobody can
  interpret is as useless as none.
- Keep a baseline workload (the behavior being replaced) inside the
  benchmark so future changes compare against it.
- Alternative reusable route: ship the methodology as a project-scoped
  skill or extend this skill when the pattern generalizes.

## Reporting

Finish any benchmark run by reporting: the command(s) run, build settings,
workload dimensions, samples and median for before/after (or current alone
when no baseline exists), and the verdict (improved / regressed / no
change), pointing to where the benchmark lives.
