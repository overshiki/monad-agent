# Reasoning-replay tests (DeepSeek thinking mode)

DeepSeek's Responses endpoint is **stateless**: every turn replays the entire
local transcript, and its thinking mode requires every replayed reasoning item
to carry plaintext `reasoning_text` content. If any reasoning item reaches the
wire without non-empty plaintext text — or carrying an OpenAI-style
`encrypted_content` blob — the API rejects the request with:

```
The `reasoning_text` in the thinking mode must be passed back to the API.
```

That single constraint regressed **five times** (commits `a4431175`,
`b0a3d52e`, `11b13b36`, `707e1c32`, `77d88d12`), each time at a different link
of the decode → assemble → persist → project chain, and each fix was verified
by unit tests only. This document describes the dedicated contract spec that
now guards the whole chain so the bug cannot silently return.

## Run it

After **any** change to the reasoning capture, replay, or request-projection
path:

```console
cabal test agent-deepseek --offline --test-options='--match thinking'
```

The full DeepSeek suite (54 examples) is the broader gate:

```console
cabal test agent-deepseek --offline
```

## What it asserts

`packages/agent-deepseek/test/Agent/DeepSeek/ThinkingModeContractSpec.hs`
projects a realistic follow-up request through the real pipeline — synthetic
DeepSeek SSE → `StreamAssembly.buildResponse` → `DeepSeek.Request.buildRequest`
→ wire JSON — and checks one invariant (`assertValidThinkingReplay`):

- every `reasoning` item in `input` has a non-empty `content` array whose
  parts are all `reasoning_text` with non-empty `text`;
- no `reasoning` item carries `encrypted_content`;
- `include` never lists `reasoning.encrypted_content`.

Unlike the older per-link tests, this asserts DeepSeek's actual rule on the
bytes that would be sent, so a regression in *any* link fails loudly here even
if the intermediate unit tests still pass.

## Test inventory

Each test is named for the commit whose regression it guards. Do not delete or
weaken a case; if a change breaks one, the change is wrong.

**Capture** — SSE decoding and stream assembly must yield a reasoning item
that survives to the wire:

| Test | Guards |
|---|---|
| `delta then a sparse output_item.done materializes buffered text` | buffered `reasoning_text.delta`s are materialized when a sparse `output_item.done` arrives (`b0a3d52e` machinery) |
| `reasoning_text.done then a sparse output_item.done keeps captured text` | `707e1c32` — a sparse `output_item.done` no longer clobbers already-captured content |
| `fully-populated reasoning replays untouched` | healthy path, no false positives |
| `a turn that emitted no reasoning still replays with a non-empty marker` | empty reasoning is echoed with a placeholder, not dropped |

**Projection** — `buildRequest` must satisfy the contract for every historical
edge case:

| Test | Guards |
|---|---|
| `encrypted blob with empty content becomes plaintext, blob dropped` | `77d88d12` — the echo fires regardless of `encrypted_content`, and the blob is dropped |
| `encrypted blob alongside valid content is dropped, plaintext kept` | always drop `encrypted_content`, even when content is present |
| `empty content and empty summary yield a non-empty placeholder` | `b0a3d52e` — `"(thinking unavailable)"`, since DeepSeek rejects an empty part |
| `absent content echoes the summary text` | `a4431175` — summary re-emitted as `reasoning_text` |
| `the reasoning.encrypted_content include is dropped` | `11b13b36` — never ask for encrypted reasoning |

**Replay shape** — the sibling ways DeepSeek rejects a stateless replay
(distinct error text, same root cause):

| Test | Guards |
|---|---|
| `a tool output is reordered to follow its call` | `dd4b2a9d` — `orderToolPairs` |
| `duplicate call_ids are uniquified with outputs still paired` | `eda750db` — `uniquifyRequestCallIds` |
| `a trailing reasoning item is followed by an empty assistant message` | `ensureReasoningHasFollowingItem` |

## Related tests

- **Capture mechanics** live in
  `packages/agent-responses/test/Agent/Responses/StreamAssemblySpec.hs` (run
  with `--match buildStreamResponse`, 30 examples). The reasoning machinery is
  shared by DeepSeek, Kimi, OpenRouter, and generic stateless Responses hosts,
  so those tests guard every provider that replays transcripts.
- **Persistence** is covered by a property test in
  `packages/agent-cli/test/Agent/CLI/SessionSpec.hs`
  (`storedResponseItemRoundTrip`), which round-trips reasoning items through
  the session store.

## Known gaps

- All of these are **unit tests against synthetic SSE**; none has been verified
  against a live DeepSeek key. If the error resurfaces, capture the real
  request bytes (dump `Aeson.encode (buildRequest opts request)` in
  `performResponsesRequest`) rather than adding another unit-only patch.
- `agent-responses`'s full suite has 6 **pre-existing, environment-dependent
  failures** (local HTTP server timeouts, oversized-body cap, and three
  others). They are not regressions — use the targeted `--match` invocation
  above.
- The newer `agent-kimi-code` Anthropic Messages transport has the analogous
  thinking-block replay requirement but no equivalent contract spec yet.
