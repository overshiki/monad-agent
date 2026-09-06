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

- `packages/agent-core-liquid/` / `packages/agent-tui-liquid/` — the
  verification packages. Each one's `src` is a symlink to the corresponding
  production `src`, so it compiles the *same* sources as the production
  library; dependencies stay minimal (`base`, `text`, `liquidhaskell`, plus
  `vty` for the TUI module), keeping the solver hermetic and fast.
- `cabal.liquid.project` — builds only those packages, with the pinned
  compiler. Deliberately not referenced from `cabal.project`.

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

The `size` contract is the property the streaming backpressure accounting
relies on; previously it was maintained by convention only.

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
