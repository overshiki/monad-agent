# Stateless replay contract tests (DeepSeek thinking mode + tool pairing)

DeepSeek's Responses endpoint is **stateless**: every turn replays the entire
local transcript, so the replayed `input` must satisfy the provider's contracts
or the whole request is rejected. Two contract families are guarded here, each
by a dedicated spec that asserts the rule on the *projected wire request*:

1. **Thinking mode** — every reasoning item must carry plaintext
   `reasoning_text` content.
2. **Tool pairing** — every tool call must be answered by a tool output with
   the same `call_id`.

## Thinking-mode contract

DeepSeek's thinking mode requires every replayed reasoning item to carry
plaintext `reasoning_text` content. If any reasoning item reaches the wire
without non-empty plaintext text — or carrying an OpenAI-style
`encrypted_content` blob — the API rejects the request with:

```
The `reasoning_text` in the thinking mode must be passed back to the API.
```

That single constraint regressed **five times** (commits `a4431175`,
`b0a3d52e`, `11b13b36`, `707e1c32`, `77d88d12`), each time at a different link
of the decode → assemble → persist → project chain, and each fix was verified
by unit tests only. The contract spec guards the whole chain so the bug cannot
silently return.

## Tool-pairing contract

Every stateless Responses host requires each tool call in the replayed
transcript to be answered by a tool output with the same `call_id`:

```
an assistant message with 'tool_calls' must be followed by tool messages
responding to each 'tool_call_id'. The following tool_call_ids did not have
response messages: ...
```

A tool call can reach the wire without its result when a turn is
interrupted/retried, a result is lost, or call-id dedup mis-pairs. The fix is a
universal `closeDanglingToolCalls` pass in `Agent.Responses.LoopBackend.withRequestInput`
that synthesizes a "not executed" output for any bare call, plus a fix for an
orphan-output double-emission in `orderToolPairs`.

## Run them

After **any** change to the reasoning capture, replay, or request-projection
path:

```console
cabal test agent-deepseek  --offline --test-options='--match thinking'
cabal test agent-responses --offline --test-options='--match pairing'
```

The full suites are the broader gates:

```console
cabal test agent-deepseek --offline    # 54 examples
cabal test agent-responses --offline   # 97 examples, 4 known env failures
```

## What the thinking-mode spec asserts

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

## Thinking-mode test inventory

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

## What the tool-pairing spec asserts

`packages/agent-responses/test/Agent/Responses/ToolPairingContractSpec.hs`
projects an item list through `withRequestInput` → wire JSON and checks one
invariant (`assertValidToolPairing`): every `function_call`/`custom_tool_call`
is **immediately followed by** an output with the same `call_id`, and call ids
are unique.

| Test | Guards |
|---|---|
| `a tool call with no result is closed with a not-executed output` | the universal `closeDanglingToolCalls` guard |
| `a custom tool call with no result is closed too` | custom calls are also closed |
| `an output before its call is reordered after it` | `dd4b2a9d` — `orderToolPairs` |
| `duplicate call ids are uniquified with outputs still paired` | `eda750db` — `uniquifyRequestCallIds` |
| `a duplicate call missing one result still leaves no bare call` | dedup + close interaction |
| `an orphan output is emitted exactly once` | the `orderToolPairs` double-emission fix |
| `a healthy call/result turn passes unchanged` | no false positives |

## Related tests

- **Capture mechanics** live in
  `packages/agent-responses/test/Agent/Responses/StreamAssemblySpec.hs` (run
  with `--match buildStreamResponse`, 30 examples). The reasoning machinery is
  shared by DeepSeek, Kimi, OpenRouter, and generic stateless Responses hosts,
  so those tests guard every provider that replays transcripts.
- **Request assembly** is covered by
  `packages/agent-responses/test/Agent/Responses/LoopBackendSpec.hs`
  (prefix preservation, tool-pair ordering, call-id dedup, web-search mirroring).
- **Persistence** is covered by a property test in
  `packages/agent-cli/test/Agent/CLI/SessionSpec.hs`
  (`storedResponseItemRoundTrip`), which round-trips reasoning items through
  the session store.

## Known gaps

- All of these are **unit tests against synthetic input**; none has been
  verified against a live key. If an error resurfaces, capture the real request
  bytes (dump `Aeson.encode (buildRequest opts request)` in
  `performResponsesRequest`) rather than adding another unit-only patch.
- `agent-responses`'s full suite has 4 **pre-existing, environment-dependent
  failures** (local-HTTP-server `NoResponseDataReceived` ×2, oversized-body cap
  ×2). They are not regressions — use the targeted `--match` invocation above.
- The newer `agent-kimi-code` Anthropic Messages transport has the analogous
  thinking-block replay requirement but no equivalent contract spec yet.

