# Liquid Haskell verification

Selected pure `agent-core` modules carry
[Liquid Haskell](https://ucsd-progsys.github.io/liquidhaskell/) refinement
annotations in `{-@ ... @-}` comments. The normal build (GHC 9.6/9.10) treats
them as comments and is completely unaffected.

## Running the verification

Verification is a separate, opt-in build that pins the GHC version supported
by the current Liquid Haskell release and never enters the default project
build or the GHCi development loop:

```console
cabal build --project-file=cabal.liquid.project all
```

Requirements:

- **GHC 9.14.1** — `liquidhaskell-0.9.14.1.1` supports exactly this compiler
  (the project file sets `with-compiler` accordingly). Each Liquid Haskell
  release supports exactly one GHC series; when Liquid Haskell moves to a
  newer GHC, bump the release in `agent-core-liquid.cabal` and the compiler
  in `cabal.liquid.project` together.
- **Z3 on `PATH`**. The Liquid Haskell README asks for >= 4.8.17; in
  practice Ubuntu's `z3` 4.8.12 also completed this module's verification.
  If solver problems appear, unpack a newer binary anywhere and prepend it
  to `PATH`, e.g.
  `PATH=/opt/z3-4.8.17/bin:$PATH cabal build --project-file=cabal.liquid.project all`.

## Layout

- `packages/agent-core-liquid/` / `packages/agent-tui-liquid/` /
  `packages/agent-cli-liquid/` — the verification packages. Each one's
  `src` is a symlink to the corresponding production `src` (agent-cli-liquid
  has `src`/`tui-src`/`core-src` for all three source packages), so it
  compiles the *same* sources as the production libraries; dependencies
  stay minimal, keeping the solver hermetic and fast.
- `cabal.liquid.project` — builds only those packages (plus the small
  local `agent-json` dependency), with the pinned compiler. Deliberately
  not referenced from `cabal.project`.

## What is verified and what is trusted

Liquid Haskell proves properties of the *pure* code. `Text` is opaque to the
verifier, so the following are the **trusted boundary** (axioms), documented
at the top of each annotated module:

- `Data.Text.length` / `Data.Text.null` / `Data.Text.concat` behave as their
  names suggest with respect to a logical length measure;
- `reverse` preserves the sum of chunk lengths (it is a permutation).

Everything else — the buffer invariant and every exported function's
contract — is discharged by the SMT solver on every verification build.

### `Agent.TextBuffer`

- **Data invariant** (checked at every construction, assumed at every
  pattern match): `count` equals the number of retained chunks and `size`
  equals their total length in code units.
- `appendTextBuffer` grows `size` by exactly the chunk length and `count` by
  one for non-empty chunks; `textBufferFromText` is exact;
  `textBufferToText` produces text of exactly the buffered length;
  `compactTextBuffer` preserves the buffered length and never increases the
  chunk count; `textBufferNull` is equivalent to zero buffered length.

### `Agent.TUI.TextWidth`

- `charCellWidth` / `displayCharCellWidth` always return 0, 1, or 2 — the
  width classification is total.
- `graphemeCellWidth` is non-negative.
- **Boundary invariant**: `graphemeBoundaries` is non-empty, starts at 0,
  is non-negative, and never exceeds the text length. This is the property
  every cursor and viewport computation relies on. The upper bound is a
  structural induction (`boundaryOffsetsLe`, written as an equational
  proof).
- `clampGraphemeCursor` lands within the clamped request range;
  `previousGraphemeBoundary` never moves the cursor right of the request;
  `nextGraphemeBoundary` never moves it left of the request and never runs
  past the end of the text.

The upper bound of `nextGraphemeBoundary` additionally relies on the
**partition axiom** (trusted, documented in the module): `graphemeClusters`
preserves every character exactly once, so cluster lengths sum to the input
length. The implementation partitions by construction; a full in-logic
proof of the segmentation recursion is future work.

Notes for this module:

- Termination checking is disabled module-wide (`--no-termination`): the
  segmentation recursion shrinks an implicit suffix the checker cannot see,
  and the verified properties are partial-correctness contracts.
- `graphemeBoundaries` and the cursor helpers are written as explicit
  recursions (`boundaryOffsets`, `lastLe`, `firstGt`, `lastLt`) that are
  provably equal to the `scanl`/`takeWhile`/`dropWhile` idioms they
  replaced; `firstGt`'s `hi` parameter is a runtime-ignored ghost naming
  the upper bound in its contract.

### `Agent.TUI.FencedCode`

- **Marker invariant** — the module's raison d'être ("block numbering and
  fence matching cannot drift apart"): every `FencedBlock` the parser
  produces carries a valid marker (backtick or tilde, length >= 3). Checked
  at the construction site inside `fenceChunks` and propagated through
  `ContextualFence`; consumers of `fencedBlocks` may rely on it.
- `fenceOpener` only succeeds with a valid marker.
- Indent arithmetic: `stripFenceIndent` never reports more than 3 leading
  spaces, `dropSpaceIndent` shortens by exactly the indent,
  `stripBodyIndent` never lengthens, `leadingSpaceCount` is bounded by the
  line length, and the list-container indent chain is non-negative.
- `sourceLines` preserves the input exactly (in length): no character is
  dropped or duplicated when splitting into lines. Currently an **axiom**
  (trusted): the in-logic induction is blocked by an LH panic when
  reflecting the function (ucsd-progsys/liquidhaskell#2629, base's
  UNPACKed `SrcLoc` constructor).

Notes for this module:

- Record-dot selection (`OverloadedRecordDot` via `HasField/getField`)
  crashes liquid-fixpoint's SMT encoding when the selector appears as a
  dictionary value, so verified modules use pattern matching for field
  access. Higher-order `assume`s (function-typed arguments) hit the same
  encoding; first-order wrapper functions (`spanSpace`, `takeWhileSpace`)
  are used instead.
- The verification package passes `-fomit-interface-pragmas` (base's
  UNPACKed constructors trigger the worker/wrapper panic) and
  `--prune-unsorted` (fixpoint rejects constraints with free variables
  from unrefined parameters).

### `Agent.CLI.Input.Display`

The first verification spanning packages: `agent-cli-liquid` compiles
`Agent.CLI.Input.Display` and `Agent.CLI.Input.Types` from `agent-cli`'s
source **and** `Agent.TUI.TextWidth` from `agent-tui`'s source (plus a few
leaf `agent-core` modules), so TextWidth's verified contracts (e.g.
`graphemeCellWidth` non-negative) are available to Display's proofs.

- `DisplayCell` carries a non-negative width and source length (data
  invariant, checked at construction).
- `terminalTextWidth` / `textColumns` / `terminalCharWidth` are
  non-negative.
- **Column budget**: `takeColumns` and `takeSuffixColumns` provably never
  return cells whose total width exceeds the budget — the property every
  overlay and viewport relies on. A mutation weakening the guard
  (`used > width`) fails the build at the construction site.

`truncateDisplayText`'s budget property is future work: it needs axioms
about re-clustering (`terminalTextWidth` of concatenated text), which are
deliberately not assumed yet.

This module required two **verification-enablement refactors** (both
independently justified):

- `Agent.CLI.Input.Types` now imports `SlashCatalog` from
  `Agent.CLI.Command.Types` instead of the `Agent.CLI.Command` umbrella,
  removing an 18-import IO closure from its dependency cone.
- The attachment types (`ImageAttachment`, `FileAttachment`,
  `TurnAttachment`) moved from `Agent.Loop.Internal` to a new leaf module
  `Agent.Loop.Attachments` (re-exported through `Agent.Loop.Internal` and
  `Agent.Loop`, so the public API is unchanged). This avoids linking the
  full agent loop — and its `tls` dependency — into the verification
  build. Note that building all of `agent-core` under GHC 9.14.1 is
  currently blocked anyway: `tls >= 2` (which the WebSocket transport
  needs for `Error_TCP_Terminate`) depends on `serialise`/`cborg`, whose
  `base`/`containers`/`bytestring`/`time` bounds are not yet revised for
  the 9.14 boot libraries.

### `Agent.CLI.Input.EditOps`

The pure editing core of the inline composer (moved out of
`Agent.CLI.Input.Editor` in another verification-enablement refactor —
the editor's state-transition reducer depends on the slash-menu engine,
whose import closure is far too heavy for the verification build; the
pure operations now live in their own leaf module, re-exported through
`Agent.CLI.Input.Editor` for API compatibility).

- **`CursorOk` invariant**: after `insertText`, `backspace`,
  `deleteAtCursor`, `killStart`, `killEnd`, `killWord`, `historyMove`,
  `moveMenuSelection`, `normalizeSelection`, and at
  `initialEditorState`, the cursor is always inside the text
  (`0 <= cursor <= length text`). The proofs use TextWidth's verified
  grapheme-boundary contracts directly. Mutation (backspace forgetting
  to move the cursor back after deletion) fails the build at the
  function's spec.
- `selectedSuggestion`: a non-empty menu always yields a suggestion
  (the index is reduced into range via a checked `mod` wrapper).

`reduceEditorKey` and `acceptSuggestion` remain unverified: their
invariant needs the replace-range contract of `slashMenuForCatalog`,
which lives in the `Agent.CLI.Command` umbrella — a future refactor
candidate.

### `Agent.CLI.TextLayout`

The selection-safety trio for fixed-size overlays (built on Display's
verified budget contracts):

- `clampSelectionIndex`: the result is non-negative, below the item count
  whenever there are items, and 0 for an empty list — a selection can
  never point outside the item list. Mutation (`= count` instead of
  `count - 1`) fails the build.
- `selectionWindow`: never returns more rows than its budget.
- `atMay`: in-range indices always yield a value.

`renderSplitPaneFrame`'s record-dot field access was converted to a
`SplitPaneFrame{..}` wildcard pattern (record-dot selection crashes
liquid-fixpoint; see the FencedCode notes). `fitTextCell`'s exact-width
contract is future work — it needs the same re-clustering axioms as
`truncateDisplayText`. This module also required the `TextLayout` import
to move from the `Agent.CLI.Input` umbrella to `Agent.CLI.Input.Display`
(another de-umbrelling, API unchanged).

### `Agent.TUI.Model.Edit`

The composer cursor/edit operations are straight-line code (no recursion),
so every contract is proved directly from the `Text` assume boundary with
no inductions:

- `deleteWordBefore` / `deleteWordAfter` / `deleteToLineStart` /
  `deleteToLineEnd`: the result is never longer than the input, and the
  returned cursor always lands inside the resulting text — an out-of-bounds
  composer cursor is impossible by construction.
- `lineStartCursor` / `moveWordLeft` never move right of the request and
  stay non-negative; `lineEndCursor` / `moveWordRight` never move left of
  the request and never run past the end of the text.

The `size` contract is the property the streaming backpressure accounting
relies on; previously it was maintained by convention only.

### `Agent.InterAgentMessage`

The first confidentiality (non-disclosure) property in the series: the
log-safe `show` of `EncryptedInterAgentContent` is **provably constant** —
it is exactly the `redacted` string, so the encrypted payload cannot leak
through `Show` into logs or status output. No axioms: the property is
structural. A mutation that prints the payload
(`"EncryptedInterAgentContent " <> show text`) fails the build at the
branch. `interAgentMessagePayload` deliberately returns the token (the
OpenAI adapter needs it); the contract covers only the `Show` path.

Note: the PLE `--interpreter` engine crashes on string literals
(`unpackCString#` elaboration), so `agent-core-liquid` uses classic PLE
(with `--prune-unsorted`); `agent-tui-liquid` still uses the interpreter
engine, which its modules' proofs were developed against.

### `Agent.OsPath`

The second security property, with `OsPath` fully opaque to the verifier:

- `directoryChain root cwd`: the result is non-empty, **always ends at
  `cwd`**, and is either exactly `[cwd]` (the containment fallback that
  prevents discovering files from unrelated filesystem ancestors) or
  **starts at `root`**. A mutation dropping the `reverse` (a plausible
  "simplification") fails the build.
- `isInsideRelative` (the workspace-escape guard in
  `relativeDisplayPath`): an accepted path is provably never absolute and
  never starts with `".."`.

**Trusted boundary** (documented in the module): the shape of
`directoryChainWalk` (true by construction; an in-logic proof is blocked
because `OsPath` equality cannot be reflected), list reverse/last facts,
and trivial wrapper specs giving `isAbsolute`/`isDotDotHead` a logic
presence. The *composition* logic is proved from those facts. The full
parent-linkage of the chain (every consecutive pair is a
`takeDirectory` step) is future work — it needs reverse-pair reasoning
through the opaque path functions.

Notes:

- The verification-only helpers (`sumT`, `reverseSumT`, `withProof`) live in
  the production module because reflected functions must be top-level
  Haskell definitions. `sumT` therefore triggers one expected
  `-Wunused-top-binds` warning in the normal build.
- The verification component builds with `-O0`: with optimisation GHC
  splits the strict `TextBuffer` constructor into worker/wrapper, which
  liquidhaskell-boot-0.9.14.1.1 cannot merge
  ([liquidhaskell#2629](https://github.com/ucsd-progsys/liquidhaskell/issues/2629)).
- Verification was mutation-tested: dropping the chunk length from the
  `size` accumulation in `appendTextBuffer` makes the build fail with
  `LIQUID: UNSAFE`, pointing at the construction site; returning
  `upperBound + 1` from `nextGraphemeBoundary`'s fallback makes the
  TextWidth build fail at that line.

## Adding another module

1. Add refinement annotations (`{-@ ... @-}`) to the module in
   `packages/agent-core/src`. Keep production code unchanged where possible.
2. Add the module to `exposed-modules` in
   `packages/agent-core-liquid/agent-core-liquid.cabal`, plus any new
   `build-depends` the module imports.
3. Run the verification build. Prefer proofs over new `assume` axioms; when
   an axiom is unavoidable (unrefined dependency code), document it in the
   module header as part of the trusted boundary.
