# Subagent catalog

The harness ships three built-in subagent types — `general-purpose`, `explore`,
and `plan` — and a JSON catalog that lets you add custom types and bind agent
types to backend models. The catalog is how you get, for example, a main loop
on `kimi-k3` with exploration subagents on `deepseek`.

## Files

- Shipped defaults: `packages/agent-cli/config/subagents.default.json`
  (installed with the executable, empty extras by default — zero behavior
  change out of the box).
- User overlay: `~/.haskell-agent/subagents.json`. An entry with the same
  `id` replaces the shipped entry wholesale (entry-level replacement, same
  semantics as `models.json`); new ids are appended in file order.

```json
{
  "subagents": [
    {
      "id": "explore-deepseek",
      "description": "Fast, read-only codebase exploration on DeepSeek.",
      "model": "deepseek-v4-flash",
      "system_prompt": "Prefer grep and targeted reads over whole files."
    }
  ]
}
```

## Entry fields

| field | required | meaning |
| --- | --- | --- |
| `id` | yes | The `subagent_type` value. Non-empty. |
| `description` | yes | Shown to the parent agent in the `spawn_subagent` tool menu so it can select the right type. |
| `model` | no | Default backend model id — a model id from `models.json` / `~/.haskell-agent/models.json` (e.g. `deepseek-v4-flash`). |
| `system_prompt` | no | Text appended to the child system prompt. |
| `group` | no | Grouping heading in the task tool's custom agent type menu. |
| `rank` | no | Menu sort order within/across groups (must be `>= 1`); unranked entries sort last, then by id. |
| `use_when` | no | "Use when…" selection guidance shown to the parent agent next to the entry in the menu. |
| `kind` | no | Reuse a built-in tool filter preset: `"general-purpose"`, `"explore"`, `"plan"`, or `"runtime-bounded"`. Ignored when `tools` is set. |
| `tools` | no | Explicit child tool-name allowlist (e.g. `["read_file", "grep", "bash"]`). Wins over `kind`. |

Validation happens at load time: duplicate ids (the error names the dup),
empty ids, `rank < 1`, an unknown `kind` (the error names the entry), and an
empty `tools` list or a blank tool name in `tools` (the error names the
entry) are rejected. Entries whose `model` is not present
in the loaded model catalog are dropped from the effective catalog with a
warning on stderr; an unparsable overlay is ignored with a warning and the
defaults are kept. Loading problems never abort startup.

## Semantics

- **Selection.** Custom types appear in the `spawn_subagent` description under
  "Custom agent types (from subagents catalog)" and in the `subagent_type`
  property description, so the parent model can pick them. With an empty
  catalog the tool description is byte-identical to the historical text.
  Entries are sorted by `rank` (unranked last) then id; when at least one
  entry has a `group`, entries render under `### <group>` headings with
  ungrouped entries under `### default`. Each line is
  `- **<id>** (default model: <m>): <description> Use when: <text>`, with the
  model suffix only when the entry sets `model` and the use-when suffix only
  when it sets `use_when`.
- **Tool filtering.** A custom type's child tool surface comes from its
  `tools` allowlist if set, else from its `kind` preset (`"explore"` is
  read-only `read_file`/`list_dir`/`grep`, `"plan"` adds the planning tools,
  `"general-purpose"` is the default coding surface minus root-only tools,
  `"runtime-bounded"` additionally hides `task`), else from the agent type
  name as before — so catalog-less types and built-ins behave exactly as
  they did before the catalog existed.
- **Root-only tools and explicit allowlists.** The built-in filters hide the
  root-only tools (`scheduler_*`, `update_goal`, `workflow`, and `task` for
  `runtime-bounded`) from children. An explicit `tools` allowlist bypasses
  that hiding: it keeps exactly the named tools. Do not list
  `scheduler_*` / `update_goal` / `workflow` / `task` in a custom allowlist
  unless you mean to expose them to the child.
- **Model default.** When a spawn omits `model`, the catalog default for the
  spawn's `subagent_type` becomes the child's model override, flowing through
  the same path as an explicitly requested model (child session resolution,
  dialect recomputation, effort defaults). An explicit `model` always wins.
  Whenever any catalog entry sets a model, the `model` property description
  and the custom agent type section gain a note that an omitted `model` uses
  the type's default when set, otherwise the parent model; likewise the
  `spawn_agent` model guidance gains a pointer to the catalog defaults.
- **Built-in shadowing.** A catalog entry whose `id` is a built-in type
  (`explore`, `plan`, `general-purpose`) does not add a type; it changes that
  type's default model and/or appends a system-prompt suffix.
- **System-prompt suffix.** `system_prompt` is appended to the child system
  prompt for every spawn whose `subagent_type` matches the entry.
- **Resume.** `resume_from` reuses the source agent's recorded spec; catalog
  defaults are not re-applied, so a resumed agent keeps its original model
  and type.

## Worked example: explore on DeepSeek

`~/.haskell-agent/subagents.json`:

```json
{
  "subagents": [
    {
      "id": "explore-deepseek",
      "description": "Read-only exploration; cheaper and fast. Use for codebase searches.",
      "model": "deepseek-v4-flash"
    },
    {
      "id": "explore",
      "description": "Built-in explore, but routed to DeepSeek by default.",
      "model": "deepseek-v4-flash"
    }
  ]
}
```

The first entry adds a new type; the parent can spawn
`subagent_type: "explore-deepseek"` and the child runs on `deepseek-v4-flash`
unless the spawn explicitly names a `model`. The second entry shadows the
built-in `explore` so even plain `explore` spawns default to DeepSeek.

## Limitations

- Catalog-driven children keep the host runtime's tool surface; the catalog
  only narrows it (`kind`/`tools` can remove tools, never add ones the host
  does not provide).

## Outcome tracking & experience

The harness records what happened so the parent model can learn which
subagent types and models work — as observable hints only. It never changes
routing policy by itself: no spawn is ever re-routed, no catalog entry is
ever edited automatically. "Learning" is the parent model reading outcome
summaries and making its own type/model choices. Hint effectiveness can be
compared with monad-bench.

### What is recorded

When a `task` spawn reaches a terminal state, one outcome is recorded:

```
(subagent_type, model, spawn description, status, timestamp, project cwd)
```

- `model` is the explicit request or the catalog default, if any.
- `status` is one of `completed`, `errored`, `interrupted`, `shutdown`,
  `unknown`.
- `project` is the session cwd at record time (the sink stamps it).

Background spawns you collect later via `get_task_output` are not recorded
(only the foreground wait completion path records).

### Where it lives

- In-session: a session-scoped log (IORef). All tool surfaces built during
  the session — the root's and every nested child's — share the same sink,
  because children build their tools from the same `SubagentRuntime`, so a
  child's `task` records into the root session's log.
- Cross-session: each outcome is appended as one JSON line to
  `~/.haskell-agent/subagent-outcomes.jsonl`. The file is append-only; the
  harness never rewrites or trims it. On load it reads at most the last
  2000 lines and silently skips corrupt lines.

Recording is best-effort: the file is created lazily on the first outcome
and every IO failure is ignored — outcome tracking never crashes or
delays a run.

### How hints surface

1. **Completion-message rollup.** After a foreground `task` completes, the
   message the parent reads gains a final line, e.g.
   `recent outcomes: explore-deepseek 3 runs (3 ok) · reviewer 2 runs (1 ok, 1 errored)`.
   Grouped by type, most-recent-first order, at most 5 types.
2. **History section in the tool menu.** At session startup, matching
   entries (recorded cwd is a prefix of the session cwd) are rendered into
   the `task` tool description:

   ```
   Historical outcomes (this project):
   - explore-deepseek: 12 runs, 10 ok, 2 errored
   - reviewer: 4 runs, 4 ok
   ```

   Sorted by run count, at most 8 lines. Both surfaces are absent entirely
   when there is nothing to show.

Note: the tool surface (including the history section) is built once per
session at startup, not per turn — outcomes recorded during the session
appear in the rollup immediately, but the description's history section
updates only on the next session.

### Resetting

Delete `~/.haskell-agent/subagent-outcomes.jsonl` to clear all history.
