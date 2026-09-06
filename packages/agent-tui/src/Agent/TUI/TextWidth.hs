-- | Terminal-cell width classification shared by line editors and renderers.
module Agent.TUI.TextWidth
    ( charCellWidth
    , displayCharCellWidth
    , displayTerminalChar
    , displayTerminalText
    , graphemeCellWidth
    , graphemeClusters
    , splitTerminalGraphemeSuffix
    , terminalTextImage
    , clampGraphemeCursor
    , nextGraphemeBoundary
    , previousGraphemeBoundary
    , isWideCharacter
    ) where

import Data.Char
    ( GeneralCategory(..)
    , chr
    , generalCategory
    , ord
    )
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Graphics.Vty as V

-- Liquid Haskell verification (see docs/liquid-haskell.md).
--
-- The annotations below are comments for the normal build. The
-- @agent-tui-liquid@ package (cabal.liquid.project, GHC 9.14.1) compiles
-- this same source with @-fplugin=LiquidHaskell@, which checks them.
--
-- 'Text' is abstract to the verifier; the only tracked property is its
-- length, via the uninterpreted measure 'tlen'. The @assume@ specs for
-- "Data.Text" functions and the 'graphemeClusters' partition axiom are the
-- trusted boundary of the verification. Everything else is proved by the
-- SMT solver.

{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}

-- Termination checking is disabled module-wide: the grapheme segmentation
-- recursion ('go', 'takeEmojiJoins') shrinks an implicit suffix the checker
-- cannot see, and the verified properties here are partial-correctness
-- contracts, not totality proofs (the module's totality is covered by the
-- agent-tui test suite).
{-@ LIQUID "--no-termination" @-}

-- | Logical length of a 'Text' value (uninterpreted; 'Text' is opaque).
{-@ measure tlen :: Text -> Int @-}

{-@ assume Text.length :: t:Data.Text.Text -> {v:Nat | v == tlen t} @-}

-- | Every element of the list is at least the bound.
{-@ reflect allGe @-}
allGe :: [Int] -> Int -> Bool
allGe [] _ = True
allGe (x : xs) bound = x >= bound && allGe xs bound

-- | Every element of the list is at most the bound.
{-@ reflect allLe @-}
allLe :: [Int] -> Int -> Bool
allLe [] _ = True
allLe (x : xs) bound = x <= bound && allLe xs bound

-- | Total length of a list of clusters. Reflected so it can appear in
-- specifications; PLE unfolds it at list constructors.
{-@ reflect sumClusterLengths @-}
{-@ sumClusterLengths :: [Text] -> Nat @-}
sumClusterLengths :: [Text] -> Int
sumClusterLengths [] = 0
sumClusterLengths (cluster : clusters) =
    Text.length cluster + sumClusterLengths clusters

-- | Trusted: 'graphemeClusters' partitions its input, so the clusters'
-- lengths sum to the input length. The implementation is a conservative
-- left-to-right partition by construction (every character is emitted in
-- exactly one cluster, in order); a full in-logic proof requires reflecting
-- the segmentation recursion and is left as future work.
{-@ assume graphemeClusters :: t:Data.Text.Text
      -> {v:[Data.Text.Text] | sumClusterLengths v == tlen t} @-}

-- | Clamp to the range [0, hi]. Equal to @max 0 (min hi x)@ when @hi >= 0@,
-- written out so the verifier can reason about it.
{-@ reflect clampLo @-}
{-@ clampLo :: hi:Nat -> x:Int
      -> {v:Int | 0 <= v && v <= hi && (0 <= x && x <= hi => v == x)} @-}
clampLo :: Int -> Int -> Int
clampLo hi x = if x < 0 then 0 else if x > hi then hi else x

-- | Sum of 'charCellWidth' over a character list. Equal to
-- @sum (map charCellWidth xs)@, written directly so the verifier can track
-- non-negativity.
{-@ reflect cellWidthSum @-}
{-@ cellWidthSum :: [Char] -> Nat @-}
cellWidthSum :: [Char] -> Int
cellWidthSum [] = 0
cellWidthSum (char : rest) = charCellWidth char + cellWidthSum rest

-- | Cumulative cluster offsets: @boundaryOffsets o cs@ is the list of
-- cluster start positions when the first cluster starts at @o@. Equal to
-- @scanl (\offset c -> offset + Text.length c) o cs@.
--
-- The contract is the module's central invariant: the boundary list is
-- non-empty, starts at @o@, and stays non-negative. The upper bound needs a
-- structural induction and lives in 'boundaryOffsetsLe' below; callers rely
-- on both through 'graphemeBoundaries'.
{-@ reflect boundaryOffsets @-}
{-@ boundaryOffsets :: o:Nat -> clusters:[Text]
      -> {v:[Int] | len v >= 1 && head v == o && allGe v 0} @-}
boundaryOffsets :: Int -> [Text] -> [Int]
boundaryOffsets offset [] = [offset]
boundaryOffsets offset (cluster : clusters) =
    offset : boundaryOffsets (offset + Text.length cluster) clusters

-- | Induction: no boundary exceeds the total cluster length. Written as an
-- equational proof: each chain element is a literal constructor term, which
-- the PLE evaluator unfolds eagerly (the direct recursive style leaves the
-- goal quantified over the scrutinee and does not go through).
{-@ boundaryOffsetsLe :: o:Nat -> clusters:[Text]
      -> {v:Proof | allLe (boundaryOffsets o clusters) (o + sumClusterLengths clusters)} @-}
boundaryOffsetsLe :: Int -> [Text] -> Proof
boundaryOffsetsLe _ [] = ()
boundaryOffsetsLe o (cluster : clusters) =
        allLe
            (boundaryOffsets o (cluster : clusters))
            (o + sumClusterLengths (cluster : clusters))
        -- the inductive hypothesis is in scope for every step below
        ? boundaryOffsetsLe (o + Text.length cluster) clusters
    === allLe
            (boundaryOffsets (o + Text.length cluster) clusters)
            ((o + Text.length cluster) + sumClusterLengths clusters)
    === True
    *** QED

-- | Attach a lemma's postcondition to an expression (ProofCombinators
-- style): @withProof x (lemma args)@ evaluates to @x@, and the lemma's
-- contract is visible to the verifier at that point. Zero cost after
-- inlining.
withProof :: a -> b -> a
withProof x _ = x
{-# INLINE withProof #-}

-- Minimal local proof combinators (the liquid-prelude idiom).
type Proof = ()

data QED = QED

infixl 3 ***
infixl 3 ===
infixl 3 ?

{-@ (***) :: a -> QED -> Proof @-}
(***) :: a -> QED -> Proof
_ *** _ = ()

{-@ (===) :: x:a -> y:{a | y == x} -> {v:a | v == x && v == y} @-}
(===) :: a -> a -> a
_ === y = y

{-@ (?) :: x:a -> b:b -> {v:a | v == x} @-}
(?) :: a -> b -> a
x ? _ = x

-- | Last element of @takeWhile (<= b) xs@, for non-empty @xs@ whose head is
-- at most @b@ (both always hold for boundary lists and clamped bounds).
{-@ lastLe :: b:Int -> {xs:[Int] | len xs >= 1 && head xs <= b && allGe xs 0}
      -> {v:Int | 0 <= v && v <= b} @-}
lastLe :: Int -> [Int] -> Int
lastLe _ [] = 0
lastLe _ [x] = x
lastLe b (x : y : rest) = if y <= b then lastLe b (y : rest) else x

-- | First element greater than @b@. The @hi@ parameter is a ghost: it only
-- names the upper bound the contract refers to and is passed through
-- unchanged.
{-@ firstGt :: b:Int -> hi:Nat -> {xs:[Int] | allGe xs 0 && allLe xs hi}
      -> Maybe {v:Int | b < v && 0 <= v && v <= hi} @-}
firstGt :: Int -> Int -> [Int] -> Maybe Int
firstGt _ _ [] = Nothing
firstGt b hi (x : xs) = if x <= b then firstGt b hi xs else Just x

-- | Last element of @takeWhile (< c) xs@, or 0 if that prefix is empty.
{-@ lastLt :: {c:Int | 0 <= c} -> {xs:[Int] | allGe xs 0}
      -> {v:Int | 0 <= v && v <= c} @-}
lastLt :: Int -> [Int] -> Int
lastLt _ [] = 0
lastLt c (x : rest)
    | x >= c = 0
    | otherwise = case rest of
        [] -> x
        y : _
            | y < c -> lastLt c rest
            | otherwise -> x

-- | Width of a Unicode character before any control-character visualization.
--
-- Combining marks occupy no additional cells. Control and unassigned
-- characters are also zero-width; callers that render them as replacement
-- glyphs should measure the replacement instead.
{-@ charCellWidth :: Char -> {v:Nat | v <= 2} @-}
charCellWidth :: Char -> Int
charCellWidth char
    | category `elem` [NonSpacingMark, SpacingCombiningMark, EnclosingMark] = 0
    | category `elem` [Control, Format, Surrogate, NotAssigned] = 0
    | isWideCharacter char = 2
    | otherwise = 1
  where
    category = generalCategory char

-- | Width used when a renderer displays terminal controls as visible
-- one-cell placeholders rather than treating them as zero-width input.
{-@ displayCharCellWidth :: Char -> {v:Nat | v <= 2} @-}
displayCharCellWidth :: Char -> Int
displayCharCellWidth char
    | char == '\n' || char == '\r' || char == '\t' = 1
    | code <= 0x1f || code == 0x7f = 1
    | code >= 0x80 && code <= 0x9f = 1
    | generalCategory char == Format = 1
    | otherwise = charCellWidth char
  where
    code = ord char

-- | Replace terminal control characters with visible, inert glyphs.
--
-- Newlines remain structural so width-aware renderers can split on them.
-- Every other replacement has the width reported by 'displayCharCellWidth'.
displayTerminalChar :: Char -> Text
displayTerminalChar char
    | char == '\n' = "\n"
    | char == '\r' = "↵"
    | char == '\t' = "⇥"
    | isVariationSelector char = ""
    | code >= 0 && code <= 0x1f =
        Text.singleton (chr (0x2400 + code))
    | code == 0x7f = "␡"
    | code >= 0x80 && code <= 0x9f = "�"
    | generalCategory char == Format = "�"
    | otherwise = Text.singleton char
  where
    code = ord char

displayTerminalText :: Text -> Text
displayTerminalText =
    Text.concat . map displayTerminalCluster . graphemeClusters
  where
    -- ZWJ and emoji tag characters are terminal formatting controls when
    -- they occur on their own, but are required source characters inside a
    -- recognized emoji grapheme. Other format characters remain inert
    -- replacement glyphs.
    displayTerminalCluster cluster
        | needsVtyCompatibleWidth cluster
        , V.safeWctwidth preserved /= graphemeCellWidth cluster =
            compatibleFallback cluster
        | otherwise = preserved
      where
        preserved
            | '\x20e3' `Text.elem` cluster
            , not (isKeycapCluster (Text.unpack cluster)) =
                Text.concatMap displayInvalidKeycapChar cluster
            | Text.length cluster > 1
            , Text.any isEmojiCandidate cluster =
                Text.concatMap displayEmojiClusterChar cluster
            | otherwise =
                Text.concatMap displayOrdinaryClusterChar cluster

    displayEmojiClusterChar character
        | character == '\x200d'
            || isEmojiTag character
            || isVariationSelector character =
            Text.singleton character
        | otherwise = displayTerminalChar character

    displayInvalidKeycapChar character
        | character == '\x20e3' = "�"
        | isVariationSelector character = ""
        | otherwise = displayTerminalChar character

    -- Variation selectors only have meaning when attached to a compatible
    -- base character. Dropping a dangling selector prevents adjacent Vty
    -- text spans from accidentally forming a wider emoji grapheme after
    -- Brick has already split the source into separate widgets.
    displayOrdinaryClusterChar character
        | isVariationSelector character = ""
        | otherwise = displayTerminalChar character

    -- Vty 6 measures some modern terminal graphemes differently from the
    -- terminal itself (notably long family ZWJ sequences and keycaps). Such
    -- text cannot safely be placed in a Vty image: its span accounting and
    -- the terminal cursor would disagree. Preserve a representative glyph
    -- with the same target width instead.
    compatibleFallback cluster =
        case Text.find
            (\character ->
                V.safeWcwidth character == targetWidth
                    && displayTerminalChar character
                        == Text.singleton character)
            cluster of
            Just character -> Text.singleton character
            Nothing ->
                case fullWidthAscii cluster of
                    Just character
                        | V.safeWcwidth character == targetWidth ->
                            Text.singleton character
                    _
                        | targetWidth == 2 -> "？"
                        | targetWidth == 0 -> ""
                        | otherwise -> "�"
      where
        targetWidth = graphemeCellWidth cluster

    fullWidthAscii =
        fmap (chr . (+ 0xfee0) . ord)
            . Text.find
                (\character ->
                    character >= '!' && character <= '~')

-- | Split text into the terminal grapheme units that must not be wrapped or
-- truncated independently. This covers combining sequences and the emoji
-- constructions commonly emitted by terminals: ZWJ sequences, regional
-- indicator flags, skin-tone modifiers, keycaps, and emoji tag sequences.
graphemeClusters :: Text -> [Text]
graphemeClusters = map Text.pack . go . Text.unpack
  where
    go [] = []
    go input =
        let (cluster, rest) = takeCluster input
        in cluster : go rest

    takeCluster [] = ([], [])
    takeCluster (character : rest)
        | isRegionalIndicator character =
            let (extensions, afterExtensions) =
                    takeClusterExtensions True rest
            in case afterExtensions of
                next : remaining
                    | isRegionalIndicator next ->
                        let (nextExtensions, afterNext) =
                                takeClusterExtensions True remaining
                        in ( character
                                : extensions
                                <> (next : nextExtensions)
                           , afterNext
                           )
                _ -> (character : extensions, afterExtensions)
        | otherwise =
            let (extensions, afterExtensions) =
                    takeClusterExtensions
                        (isEmojiCandidate character)
                        rest
                initial = character : extensions
            in takeEmojiJoins initial afterExtensions

    takeEmojiJoins cluster ('\x200d' : next : rest)
        | any isEmojiCandidate cluster
        , isEmojiCandidate next =
            let (extensions, remaining) =
                    takeClusterExtensions True rest
            in takeEmojiJoins
                (cluster <> ('\x200d' : next : extensions))
                remaining
    takeEmojiJoins cluster rest = (cluster, rest)

    takeClusterExtensions allowEmoji = goExtensions
      where
        goExtensions (character : rest)
            | isCombiningMark character =
                let (extensions, remaining) = goExtensions rest
                in (character : extensions, remaining)
            | allowEmoji
            , isEmojiModifier character || isEmojiTag character =
                let (extensions, remaining) = goExtensions rest
                in (character : extensions, remaining)
        goExtensions remaining = ([], remaining)

-- | Hold a trailing cluster that may combine with the next streamed chunk.
--
-- This is intentionally narrower than general Unicode grapheme segmentation:
-- ordinary combining marks can still combine across terminal SGR sequences,
-- while emoji, flags, keycaps, and trailing ZWJ sequences must be sanitized as
-- a whole or they can be irreversibly replaced before the next chunk arrives.
splitTerminalGraphemeSuffix :: Text -> (Text, Text)
splitTerminalGraphemeSuffix text =
    case reverse (graphemeClusters text) of
        [] -> ("", "")
        lastCluster : previous
            | lastCluster == Text.singleton '\x200d'
            , previousCluster : _ <- previous ->
                splitAtSuffix (previousCluster <> lastCluster)
            | clusterMayExtend lastCluster ->
                splitAtSuffix lastCluster
            | otherwise -> (text, "")
  where
    splitAtSuffix suffix =
        (Text.dropEnd (Text.length suffix) text, suffix)

    clusterMayExtend cluster =
        Text.any isEmojiCandidate cluster
            || Text.any isKeycapBase cluster

-- | Width of one cluster as displayed by a modern terminal.
{-@ graphemeCellWidth :: Text -> Nat @-}
graphemeCellWidth :: Text -> Int
graphemeCellWidth cluster
    | Text.null cluster = 0
    | isRegionalIndicatorPair characters = 2
    | isKeycapCluster characters = 2
    | any isEmojiModifier characters = 2
    | hasEmojiTagSequence characters = 2
    | '\xfe0f' `elem` characters
    , any isEmojiCandidate characters = 2
    | '\x200d' `elem` characters
    , any isEmojiCandidate characters = 2
    | otherwise = cellWidthSum characters
  where
    characters = Text.unpack cluster

-- | Build a single-line Vty image after replacing any grapheme whose terminal
-- width disagrees with Vty's width model. This keeps Vty's span accounting,
-- our layout model, and the terminal cursor in agreement.
--
-- The input is sanitized with 'displayTerminalText' before it reaches Vty.
-- Callers should split structural newlines before constructing an image.
terminalTextImage :: V.Attr -> Text -> V.Image
terminalTextImage attr =
    V.text' attr . displayTerminalText

-- | The clamped cursor always lands on a real boundary inside the text.
{-@ clampGraphemeCursor :: t:Text -> req:Int
      -> {v:Int | 0 <= v && v <= clampLo (tlen t) req} @-}
clampGraphemeCursor :: Text -> Int -> Int
clampGraphemeCursor text requested =
    lastLe bounded (graphemeBoundaries text)
  where
    bounded = clampLo (Text.length text) requested

-- | The previous boundary never moves the cursor right.
{-@ previousGraphemeBoundary :: t:Text -> req:Int
      -> {v:Int | 0 <= v && v <= clampLo (tlen t) req} @-}
previousGraphemeBoundary :: Text -> Int -> Int
previousGraphemeBoundary text requested =
    lastLt cursor (graphemeBoundaries text)
  where
    cursor = clampGraphemeCursor text requested

-- | The next boundary never moves the cursor left of the request and never
-- runs past the end of the text.
{-@ nextGraphemeBoundary :: t:Text -> req:Int
      -> {v:Int | clampLo (tlen t) req <= v && v <= tlen t} @-}
nextGraphemeBoundary :: Text -> Int -> Int
nextGraphemeBoundary text requested =
    case firstGt bounded upperBound (graphemeBoundaries text) of
        Just boundary -> boundary
        Nothing -> upperBound
  where
    upperBound = Text.length text
    bounded = clampLo upperBound requested

-- | Cluster start offsets: non-empty, starts at 0, non-negative, and
-- bounded by the text length (via the 'graphemeClusters' partition axiom).
{-@ graphemeBoundaries :: t:Text
      -> {v:[Int] | len v >= 1 && head v == 0 && allGe v 0 && allLe v (tlen t)} @-}
graphemeBoundaries :: Text -> [Int]
graphemeBoundaries text =
    boundaryOffsets 0 clusters
        `withProof` boundaryOffsetsLe 0 clusters
  where
    clusters = graphemeClusters text

isCombiningMark :: Char -> Bool
isCombiningMark character =
    generalCategory character
        `elem` [NonSpacingMark, SpacingCombiningMark, EnclosingMark]

isEmojiCandidate :: Char -> Bool
isEmojiCandidate character =
    let code = ord character
    in (code >= 0x1f000 && code <= 0x1faff)
        || (code >= 0x2600 && code <= 0x27ff)
        || code == 0x00a9
        || code == 0x00ae
        || code == 0x3030
        || code == 0x303d
        || code == 0x3297
        || code == 0x3299

isRegionalIndicator :: Char -> Bool
isRegionalIndicator character =
    let code = ord character
    in code >= 0x1f1e6 && code <= 0x1f1ff

isRegionalIndicatorPair :: [Char] -> Bool
isRegionalIndicatorPair characters =
    length (filter isRegionalIndicator characters) == 2

needsVtyCompatibleWidth :: Text -> Bool
needsVtyCompatibleWidth cluster =
    any isEmojiCandidate characters
        || isKeycapCluster characters
  where
    characters = Text.unpack cluster

isKeycapCluster :: [Char] -> Bool
isKeycapCluster characters =
    '\x20e3' `elem` characters
        && any isKeycapBase characters

isKeycapBase :: Char -> Bool
isKeycapBase character =
    (character >= '0' && character <= '9')
        || character == '#'
        || character == '*'

hasEmojiTagSequence :: [Char] -> Bool
hasEmojiTagSequence characters =
    any isEmojiTag characters
        && any isEmojiCandidate characters

isEmojiModifier :: Char -> Bool
isEmojiModifier character =
    let code = ord character
    in code >= 0x1f3fb && code <= 0x1f3ff

isEmojiTag :: Char -> Bool
isEmojiTag character =
    let code = ord character
    in code >= 0xe0020 && code <= 0xe007f

-- Variation selectors are meaningful only when they remain attached to the
-- character they modify. Keeping a standalone selector lets Vty merge it with
-- text from an adjacent widget, potentially changing that glyph's terminal
-- width after layout has already been calculated.
isVariationSelector :: Char -> Bool
isVariationSelector character =
    let code = ord character
    in (code >= 0xfe00 && code <= 0xfe0f)
        || (code >= 0xe0100 && code <= 0xe01ef)

-- | Report whether Vty's terminal-width table treats a character as wide.
--
-- Vty intentionally treats many standalone emoji code points as one cell,
-- while the harness has historically reserved two cells for the emoji block.
-- Keep that UI policy as a narrow override and delegate the rest of Unicode
-- width classification to the renderer's own table.
isWideCharacter :: Char -> Bool
isWideCharacter char =
    V.safeWcwidth char == 2
        || code >= 0x1f300 && code <= 0x1faff
  where
    code = ord char
