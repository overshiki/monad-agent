# Session history as a quest tree

Today a session's history is a linear, append-only transcript: turns with an
`append` / `replace` / `reset` effect, where the "active transcript" is a
derived view over the log (everything after the last reset, with compaction
checkpoints re-applied). Forks copy that active prefix into a new, independent
session; there is no link back. Context for the model is assembled as an
immutable prefix (instructions + tools + generated context) followed by that
linear transcript, managed by occupancy tracking and automatic compaction.

This document proposes making the history an explicit **tree** — each "quest"
is a branch, and a fork is a larger branch — and defines context generation
as a function from `(tree, focus)` to a token budget. It is a design only;
no schema or code changes are implied by this file.

## Status

- **P1 implemented** (migration v106): `session_turns.parent_turn_id` and
  `sessions.focus_turn_id` exist, and the store maintains append-log lineage —
  each appended turn's parent is the previous head, `focus_turn_id` advances to
  the new turn, and a `NULL` parent marks a quest root. Lineage lives on
  `StoredTurn` (read) and `TurnInsert` (write), not on `SessionTurn`, so the
  CLI is unchanged. Fork snapshots chain their copied turns the same way.
- **P2–P5 pending**: no context assembler, relevance, hierarchical compaction,
  or tree UX yet; `parent_turn_id` is currently informational.

## Goals and non-goals

**Goals**

- Store conversation history as an append-only DAG of turns, with explicit
  lineage (`parent_turn_id`) and a notion of the *current quest* (`focus`).
- Generate model context through one pure, budget-driven assembler rather
  than a fixed linear concatenation.
- Make merge a first-class tree operation (join two branches), folding the
  earlier fork/merge question into the same model.
- Preserve today's durability guarantees: turns are never rewritten; a crash
  between commits never loses a turn.

**Non-goals (for now)**

- Embedding-based retrieval is **deferred** (see "Relevance" below); we start
  with the PostgreSQL full-text search already present.
- No change to how a single turn's items are stored or streamed.
- No change to the wire format of `ResponseCreateParams` / `ResponseItem`.

## Current model (reference)

- `SessionTurn` (`packages/agent-cli/src/Agent/CLI/Session/Types.hs`) has
  `turnAt`, `turnUserText`, `turnAssistantText`, `turnError`,
  `turnResponseId`, `turnEffect`, `turnItems`, `turnUsage`,
  `turnProviderTelemetry`. `TranscriptEffect` is `append` | `replace` |
  `reset`.
- `harness.session_turns` already carries `turn_index`, `event_sequence`,
  `transcript_effect`, and a full-text `search_vector`.
- The active transcript is reconstructed in `activeTranscriptTurns`
  (`Session.hs`) as the suffix after the last `TranscriptReset`, with
  `TranscriptReplace` checkpoints re-applied.
- The live context is a `LiveConversation` `IORef`
  (`Agent.CLI.Session.History`); `readLiveTranscript` feeds the per-turn
  request. Compaction (`Agent.CLI.Compaction`) produces a
  `CompactOutcome` and installs it as a `TranscriptReplace` checkpoint.

## Core model

A **quest tree** is a DAG whose nodes are turns and whose edges are parent
pointers. A *quest* is the set of turns reachable from a designated **root**
turn without crossing another quest's root; a **focus** is the current
position (quest + turn) from which context is generated.

- Roots: the session's first turn, plus every fork/rewind/quest-create point.
- Branching happens only at explicit points: fork, rewind, `/quest`, merge
  join. Ordinary turns extend the current quest linearly.
- The tree is **derived** from `parent_turn_id` + existing effect markers.
  Storage stays append-only; nothing is rewritten.

A linear session is the degenerate single-path tree, so the migration is
backward compatible.

## Schema (migration v106, draft)

`harness.session_turns`:

```sql
ALTER TABLE harness.session_turns
  ADD COLUMN IF NOT EXISTS parent_turn_id uuid
    REFERENCES harness.session_turns (turn_id);
CREATE INDEX IF NOT EXISTS session_turns_parent_turn_id_idx
  ON harness.session_turns (session_id, parent_turn_id);
```

`harness.sessions`:

```sql
ALTER TABLE harness.sessions
  ADD COLUMN IF NOT EXISTS focus_turn_id uuid;
-- quest roots are recorded as turns whose parent_turn_id IS NULL.
```

- `parent_turn_id IS NULL` marks a root (a quest boundary).
- `focus_turn_id` records the head of the current quest; `/switch` and
  `/quest` move it. It defaults to the newest turn for old sessions.
- The existing `TranscriptReset` marker can be reused as the fork/rewind
  branch point, with the retained prefix's first turn getting
  `parent_turn_id = <the turn that was branched from>` — no new effect kind
  is required. (Alternatively introduce `TranscriptFork`, decided at
  implementation time; reusing reset keeps `activeTranscriptTurns`
  semantics unchanged.)

## Context assembly

One pure entry point:

```haskell
assembleContext
  :: QuestTree          -- turns indexed by id, with parent pointers
  -> Focus              -- current quest + turn
  -> ContextBudget      -- token limits per segment
  -> IO [ContextSegment]
```

Segments, in fill order:

| # | Segment | Content | Deterministic |
|---|---|---|---|
| 0 | prefix | instructions, tools, skills, generated context (existing snapshot) | yes |
| 1 | lineage | ancestors of `focus` within the quest, newest first, up to a recency window | yes |
| 2 | relevant | retrieval hits over the whole tree, deduped against segment 1 | FTS: yes; embeddings: no |
| 3 | reference | per-quest summaries + citation ids for everything else | text is model-generated |

- Each segment has a hard budget; the assembler fills in priority order and
  stops at budget.
- Segment 0 + 1 alone reproduce today's behavior when the window is
  unbounded and retrieval is disabled, so the assembler can replace the
  current linear path with **no behavioral change** first, then add segments.
- A `Focus`/`QuestTree` snapshot is recorded with each durable turn so a
  request is reproducible (`--replay`) even with retrieval enabled.

### Relevance

- **Phase A (deterministic):** reuse `search_vector` + the existing
  `conversation_search` path, scoring by recency + keyword overlap. No new
  infrastructure, fully reproducible for a fixed query.
- **Phase B (deferred):** embeddings + a vector index (pgvector or a side
  store). Requires a retrieval evaluation harness before it lands; sessions
  become non-reproducible unless snapshots are stored (see above).

## Compaction becomes hierarchical

Today compaction summarizes the *whole* active transcript. In the tree it
summarizes a **subtree**: a quest (or a stale suffix of one) is collapsed
into a single `TranscriptReplace` node with a `compactSummary`, and its
children remain addressable as references. Auto-compaction triggers target
the *focused quest's* occupancy rather than a global window. The existing
`summarizePortableLocalAttempt` / `CompactOutcome` machinery carries over
with a per-quest transcript as input.

## Merge as a branch join

Merging a fork back becomes a tree operation: create a join turn in the main
quest whose `parent_turn_id` points at the main head (and optionally record
the fork's head as a second parent), with a summary segment carrying the
fork's delta. Git still performs the code merge; the tree performs the
history merge. This subsumes the `/merge` design discussed previously.

## Commands and UX

- `/quest <name>` — create a new quest (branch) at the current turn.
- `/switch <quest|turn|session>` — move `focus_turn_id` (the `git checkout`
  of a node).
- `/merge <branch-or-session>` — join a fork into the current quest (tree +
  git, with `--continue` for conflict resolution).
- `/fork` — unchanged surface; now also records lineage (parent + base turn),
  which closes the gap noted in the fork/merge work.
- TUI: a tree view (breadcrumb of the current quest + collapsible branches)
  alongside the linear scrollback. This is the largest UX chunk and is staged
  last.

## Phased plan

| Phase | Deliverable | Acceptance |
|---|---|---|
| P0 | This design doc | agreed |
| P1 | Lineage + focus: migration v106, `parent_turn_id`, `focus_turn_id`, fork writes lineage | old sessions load unchanged; fork records parent/base |
| P2 | `ContextAssembler` with segments 0+1 only, wired behind the existing linear path | byte-identical context for linear sessions; benchmark proves no regression |
| P3 | Relevance Phase A (FTS) behind a flag | deterministic; recall/precision spot-checked |
| P4 | Hierarchical compaction + reference segments | per-quest summaries; auto-compaction targets the focus |
| P5 | `/quest`, `/switch`, `/merge`, TUI tree view | end-to-end tree workflows |

## Risks and mitigations

- **Non-reproducibility (relevance/embeddings):** store a context snapshot
  with each turn; keep retrieval optional and pinned.
- **Performance:** the assembler is a hot path; measure with an `-O2`
  benchmark before enabling retrieval (repo policy).
- **TUI complexity:** stage the tree view last; keep the linear scrollback as
  the default until P5.
- **Durability:** append-only turns only; the tree is derived, never a
  rewrite. Rollback keeps today's `onException`-style cleanup guarantees.
