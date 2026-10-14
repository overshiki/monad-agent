# Code-density program

Haskell's density comes from pushing work into types/deriving, higher-order
reuse, combinators, and compiler inference — not from cleverness. This document
is the working checklist for keeping the codebase short while functionality
stays identical. Baseline measurements live in [`code-size.tsv`](code-size.tsv);
update that file on every density PR.

## Hard guardrails (never trade away)

- **Totality stays compiler-enforced**: keep `-Werror=incomplete-patterns`
  and friends; no partial matches/`head`/`fromJust` introduced for brevity.
- **Wire formats are frozen by tests**: after any change,
  `cabal test agent-deepseek --offline --test-options='--match thinking'` and
  `cabal test agent-responses --offline --test-options='--match pairing'` stay
  green.
- **Perf-sensitive loops need the benchmark first** (AGENTS.md): `StreamAssembly`,
  SSE decoders, `TextBuffer`/render paths.
- **No point-free golf, no lens dependency.** Density must read as "the obvious
  thing, shorter."

## Record updates: use `makeSetters` (implemented)

`NoFieldSelectors` + `DuplicateRecordFields` ban record-update syntax (GHC
cannot resolve `r { f = x }` without `RebindableSyntax`, which we do not
enable). Instead of hand-writing "clone with one field changed" rebuilds, use
the compile-time splice `Agent.Json.TH.makeSetters`:

```haskell
makeSetters ''ReasoningItem   -- in a module importing the type
-- generates setReasoningItemContent, setReasoningItemStatus, ...
```

Generated setters are total, pure positional rebuilds — identical semantics to
the hand-written helpers they replace. Pre-generated for the wire types in
`Agent.Responses.Types.Setters`; add `makeSetters ''T` beside new records.
Delete hand-written single-field helpers in favor of the generated names.

## Idiomatic sweep checklist (apply when touching code)

Mechanical, behavior-preserving transformations, highest yield first:

1. Redundant brackets/`$`; `fmap f . fmap g` → `fmap (f . g)`.
2. `case b of True -> x; False -> y` → `Data.Bool.bool y x b`.
3. Case chains on the same scrutinee → guards or `MultiWayIf` (enable
   per-module).
4. Hand-rolled second-projection on `Either`/pairs → `Data.Bifunctor`
   (`first`/`second`/`bimap`).
5. Explicit cons/fold loops → `foldMap`/`fold`/`traverse`+`for` where the
   accumulation is plainly a monoid/traversal.
6. `maybe [] pure` / `[x | Just x <- xs]` → `maybeToList`/`mapMaybe`/`catMaybes`.
7. Single-field record rebuilds → generated setters (above).

## Structural opportunities (larger, do as dedicated PRs)

- **Provider-hook tables**: `Login.hs` `connect*` family and the
  `Providers.hs` per-provider branches are near-identical; abstract over a
  record of hooks so a provider is data, not a 100-line copy.
- **JSON layer stays manual** on purpose (intentional wire naming); do not
  "densify" `ToJSON` instances or Hermes decoders.

## Verification for any density PR

```console
cabal test agent-responses --offline --test-options='--match pairing'
cabal test agent-deepseek  --offline --test-options='--match thinking'
cabal test <touched package> --offline
```

plus the tmux smoke check for any CLI-visible change (AGENTS.md).
