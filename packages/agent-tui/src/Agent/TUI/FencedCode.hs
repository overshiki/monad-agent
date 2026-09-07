-- | Shared Markdown fenced-code parsing.
--
-- The fullscreen renderer and copy-code commands use this module so their
-- block numbering and fence matching cannot drift apart.
module Agent.TUI.FencedCode
    ( FenceMarker(..)
    , FencedBlock(..)
    , FenceChunk(..)
    , fenceOpener
    , isFenceCloser
    , fenceChunks
    , fencedBlocks
    ) where

import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text

-- Liquid Haskell verification (see docs/liquid-haskell.md).
--
-- The annotations below are comments for the normal build. The
-- @agent-tui-liquid@ package (cabal.liquid.project, GHC 9.14.1) compiles
-- this same source with @-fplugin=LiquidHaskell@, which checks them.
--
-- 'Text' is abstract to the verifier; the only tracked property is its
-- length, via the uninterpreted measure 'tlen'. The @assume@ specs for
-- "Data.Text" functions and the 'newlineLen' fact are the trusted boundary
-- of the verification. Everything else is proved by the SMT solver.
--
-- Termination checking is disabled: 'fenceChunks' and 'sourceLines' recurse
-- over computed suffixes the checker cannot see shrinking, and the
-- contracts verified here are partial-correctness properties (totality is
-- covered by the agent-tui test suite).

{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}
{-@ LIQUID "--no-termination" @-}

-- | Logical length of a 'Text' value (uninterpreted; 'Text' is opaque).
{-@ measure tlen :: Text -> Int @-}

{-@ assume Text.length :: t:Data.Text.Text -> {v:Nat | v == tlen t} @-}
{-@ assume Text.null :: t:Data.Text.Text -> {v:Bool | v <=> (tlen t == 0)} @-}

-- | Dropping @n <= tlen t@ characters shortens by exactly @n@.
{-@ assume Text.drop :: n:Nat -> t:Data.Text.Text
      -> {v:Data.Text.Text | tlen v <= tlen t && (n <= tlen t => tlen v == tlen t - n)} @-}

-- | 'Text.span' at the space predicate splits into prefix and remainder.
-- (First-order wrapper: higher-order assumes crash liquid-fixpoint's SMT
-- encoding, so the production call sites below use these wrappers.)
{-@ assume spanSpace :: t:Data.Text.Text
      -> {v:(Data.Text.Text, Data.Text.Text) | tlen (fst v) + tlen (snd v) == tlen t
                                                && tlen (fst v) >= 0
                                                && tlen (snd v) >= 0} @-}
spanSpace :: Text -> (Text, Text)
spanSpace = Text.span (== ' ')

-- | A 'Text.takeWhile' space prefix is never longer than the input.
{-@ assume takeWhileSpace :: t:Data.Text.Text
      -> {v:Data.Text.Text | tlen v <= tlen t} @-}
takeWhileSpace :: Text -> Text
takeWhileSpace = Text.takeWhile (== ' ')



-- | A valid fence marker: backtick or tilde, at least three characters.
-- This is the invariant that keeps renderer and copy-code fence matching
-- from drifting apart: every 'FencedBlock' the parser produces carries it.
{-@ reflect markerOk @-}
markerOk :: FenceMarker -> Bool
markerOk (FenceMarker character len) =
    (character == '`' || character == '~') && len >= 3

{-@ data FenceMarker = FenceMarker
      { fenceCharacter :: Char
      , fenceLength :: Int
      }
@-}

data FenceMarker = FenceMarker
    { fenceCharacter :: !Char
    , fenceLength :: !Int
    }
    deriving (Eq, Show)

-- | Markdown source split into prose and fenced-code chunks in source order.
-- Delimiter lines are represented by the metadata on 'FenceBlock', rather than
-- repeated in the surrounding prose.
data FenceChunk
    = FenceText !Text
    | FenceBlock !FencedBlock
    deriving (Eq, Show)

-- | Every block the parser produces carries a valid marker. Checked at the
-- construction site in 'fenceChunks'; consumers of 'fencedBlocks' may rely
-- on it.
{-@ data FencedBlock = FencedBlock
      { fencedIndex :: Int
      , fencedMarker :: {v:FenceMarker | markerOk v}
      , fencedInfo :: Text
      , fencedBody :: Text
      , fencedClosed :: Bool
      }
@-}

data FencedBlock = FencedBlock
    { fencedIndex :: !Int
    , fencedMarker :: !FenceMarker
    , fencedInfo :: !Text
    , fencedBody :: !Text
    , fencedClosed :: !Bool
    }
    deriving (Eq, Show)

-- | Parse a fence opener, allowing the zero-to-three leading spaces permitted
-- by Markdown. Backtick fence info strings may not themselves contain a
-- backtick.
{-@ fenceOpener :: Text -> Maybe {v:(FenceMarker, Text) | markerOk (fst v)} @-}
fenceOpener :: Text -> Maybe (FenceMarker, Text)
fenceOpener line = do
    (_, stripped) <- stripFenceIndent line
    character <- Text.uncons stripped >>= \(first, _) ->
        if first == '`' || first == '~'
            then Just first
            else Nothing
    let
        (markerText, suffix) = Text.span (== character) stripped
        markerLength = Text.length markerText
        info = Text.strip suffix
    if markerLength < 3
        || (character == '`' && Text.any (== '`') info)
        then Nothing
        else Just (FenceMarker character markerLength, info)

-- | Whether a line closes a particular opener. A closer must use the same
-- marker character, be at least as long, and contain only trailing whitespace.
isFenceCloser :: FenceMarker -> Text -> Bool
isFenceCloser (FenceMarker character len) line =
    case stripFenceIndent line of
        Nothing -> False
        Just (_, stripped) ->
            let (markerText, suffix) =
                    Text.span (== character) stripped
            in Text.length markerText >= len
                && Text.all isSpace suffix

-- | Split Markdown into prose and fenced blocks in source order. Unterminated
-- blocks are included with 'fencedClosed' set to 'False'. Prose and body text
-- retain their original line endings.
fenceChunks :: Text -> [FenceChunk]
fenceChunks = go 1 [] [] . sourceLines
  where
    go _ prose _ [] = proseChunk prose
    go index prose previous (line@(SourceLine lineText _) : rest) =
        case fenceOpenerInContext previous lineText of
            Nothing -> go index (line : prose) (line : previous) rest
            Just opener@(ContextualFence marker info _ indent) ->
                let
                    (bodyLines, closingAndRest) =
                        break
                            (\(SourceLine text _) ->
                                isContextualFenceCloser opener text)
                            rest
                    (closed, closingLine, remaining) =
                        case closingAndRest of
                            [] -> (False, [], [])
                            closing : after -> (True, [closing], after)
                    block = FencedBlock
                        { fencedIndex = index
                        , fencedMarker = marker
                        , fencedInfo = info
                        , fencedBody =
                            foldMap
                                (\(SourceLine bodyText bodyEnding) ->
                                    stripBodyIndent indent bodyText
                                        <> bodyEnding)
                                bodyLines
                        , fencedClosed = closed
                        }
                    consumed = line : bodyLines <> closingLine
                in proseChunk prose
                    <> [FenceBlock block]
                    <> go
                        (index + 1)
                        []
                        (reverse consumed <> previous)
                        remaining

    proseChunk [] = []
    proseChunk reversedLines =
        [FenceText (foldMap sourceLineText (reverse reversedLines))]

{-@ data ContextualFence = ContextualFence
      { openMarker :: {v:FenceMarker | markerOk v}
      , openInfo :: Text
      , openContainerIndent :: Nat
      , openIndent :: Nat
      }
@-}

data ContextualFence = ContextualFence
    { openMarker :: !FenceMarker
    , openInfo :: !Text
    , openContainerIndent :: !Int
    , openIndent :: !Int
    }

fenceOpenerInContext :: [SourceLine] -> Text -> Maybe ContextualFence
fenceOpenerInContext previous line =
    if not (startsWithFenceMarker line)
        then Nothing
        else
            let lineIndent = leadingSpaceCount line
            in (do
                    containerIndent <-
                        listContainerIndent lineIndent previous
                    contextualFence containerIndent line)
                <|> contextualFence 0 line

{-@ contextualFence :: Nat -> Text -> Maybe ContextualFence @-}
contextualFence :: Int -> Text -> Maybe ContextualFence
contextualFence containerIndent line = do
    insideContainer <- dropSpaceIndent containerIndent line
    (fenceIndent, stripped) <- stripFenceIndent insideContainer
    (marker, info) <- fenceOpener stripped
    pure ContextualFence
        { openMarker = marker
        , openInfo = info
        , openContainerIndent = containerIndent
        , openIndent = containerIndent + fenceIndent
        }

isContextualFenceCloser :: ContextualFence -> Text -> Bool
isContextualFenceCloser (ContextualFence marker _ containerIndent _) line =
    maybe False
        (isFenceCloser marker)
        (dropSpaceIndent containerIndent line)

-- | Find the nearest surrounding list item whose continuation indentation
-- contains the prospective fence. Intervening non-blank lines must remain
-- within that item, which prevents an old list from making an unrelated
-- top-level four-space-indented line look like a fence.
{-@ listContainerIndent :: Int -> [SourceLine] -> Maybe Nat @-}
listContainerIndent :: Int -> [SourceLine] -> Maybe Int
listContainerIndent lineIndent = go maxBound
  where
    -- The sentinel-based minimum stays a plain Int; only the result is
    -- non-negative.
    {-@ go :: Int -> [SourceLine] -> Maybe Nat @-}
    go _ [] = Nothing
    go minimumIndent (SourceLine text _ : rest)
        | Text.null (Text.strip text) =
            go minimumIndent rest
        | Just contentIndent <- listItemContentIndent text
        , contentIndent <= lineIndent
        , contentIndent <= minimumIndent =
            Just contentIndent
        | otherwise =
            let minimumIndent' =
                    min minimumIndent (leadingSpaceCount text)
            in if minimumIndent' == 0
                then Nothing
                else go minimumIndent' rest

{-@ listItemContentIndent :: Text -> Maybe Nat @-}
listItemContentIndent :: Text -> Maybe Int
listItemContentIndent line =
    bulletIndent <|> orderedIndent
  where
    leading = leadingSpaceCount line
    stripped = Text.drop leading line
    bulletIndent = do
        (marker, afterMarker) <- Text.uncons stripped
        if marker `elem` ['-', '+', '*']
            then contentIndentAfterMarker leading 1 afterMarker
            else Nothing
    orderedIndent = do
        let (digits, afterDigits) = Text.span isAsciiDigit stripped
        if Text.null digits || Text.length digits > 9
            then Nothing
            else do
                (marker, afterMarker) <- Text.uncons afterDigits
                if marker == '.' || marker == ')'
                    then
                        contentIndentAfterMarker
                            leading
                            (Text.length digits + 1)
                            afterMarker
                    else Nothing

{-@ contentIndentAfterMarker :: Nat -> Nat -> Text -> Maybe Nat @-}
contentIndentAfterMarker :: Int -> Int -> Text -> Maybe Int
contentIndentAfterMarker leading markerWidth afterMarker =
    let spaces = leadingSpaceCount afterMarker
    in if spaces >= 1 && spaces <= 4
        then Just (leading + markerWidth + spaces)
        else Nothing

isAsciiDigit :: Char -> Bool
isAsciiDigit character = character >= '0' && character <= '9'

{-@ leadingSpaceCount :: line:Text -> {v:Nat | v <= tlen line} @-}
leadingSpaceCount :: Text -> Int
leadingSpaceCount = Text.length . takeWhileSpace

startsWithFenceMarker :: Text -> Bool
startsWithFenceMarker line =
    case Text.uncons (Text.dropWhile (== ' ') line) of
        Just (marker, _) -> marker == '`' || marker == '~'
        Nothing -> False

{-@ dropSpaceIndent :: count:Nat -> line:Text
      -> Maybe {v:Text | tlen v == tlen line - count} @-}
dropSpaceIndent :: Int -> Text -> Maybe Text
dropSpaceIndent count line =
    let (spaces, _) = spanSpace line
    in if Text.length spaces >= count
        then Just (Text.drop count line)
        else Nothing

{-@ stripBodyIndent :: Nat -> line:Text -> {v:Text | tlen v <= tlen line} @-}
stripBodyIndent :: Int -> Text -> Text
stripBodyIndent count line =
    let available = leadingSpaceCount line
    in Text.drop (min count available) line

-- | Extract all fenced blocks in source order.
fencedBlocks :: Text -> [FencedBlock]
fencedBlocks =
    foldr
        (\chunk rest ->
            case chunk of
                FenceText _ -> rest
                FenceBlock block -> block : rest)
        []
        . fenceChunks

{-@ data SourceLine = SourceLine
      { lineText :: Text
      , lineEnding :: Text
      }
@-}

data SourceLine = SourceLine
    { lineText :: !Text
    , lineEnding :: !Text
    }

-- | Total length of a list of source lines, line endings included.
-- Reflected so it can appear in specifications.
{-@ reflect sourceLinesLen @-}
{-@ sourceLinesLen :: [SourceLine] -> Nat @-}
sourceLinesLen :: [SourceLine] -> Int
sourceLinesLen [] = 0
sourceLinesLen (SourceLine text ending : rest) =
    Text.length text + Text.length ending + sourceLinesLen rest

-- | Trusted: 'sourceLines' preserves the input exactly (in length) — no
-- character is dropped or duplicated when splitting into lines. True by
-- construction (every line keeps its ending). An in-logic induction proof
-- is currently blocked: reflecting this function makes liquidhaskell-boot
-- 0.9.14.1.1 panic on the worker/wrapper split of base's UNPACKed 'SrcLoc'
-- constructor (ucsd-progsys/liquidhaskell#2629).
{-@ assume sourceLines :: input:Data.Text.Text
      -> {v:[SourceLine] | sourceLinesLen v == tlen input} @-}

sourceLines :: Text -> [SourceLine]
sourceLines input
    | Text.null input = []
    | otherwise =
        let (line, suffix) = Text.breakOn "\n" input
        in if Text.null suffix
            then [SourceLine line ""]
            else SourceLine line "\n" : sourceLines (Text.drop 1 suffix)

sourceLineText :: SourceLine -> Text
sourceLineText (SourceLine text ending) = text <> ending

{-@ stripFenceIndent :: line:Text -> Maybe {v:(Nat, Text) | fst v <= 3} @-}
stripFenceIndent :: Text -> Maybe (Int, Text)
stripFenceIndent line =
    let (spaces, stripped) = spanSpace line
    in if Text.length spaces <= 3
        then Just (Text.length spaces, stripped)
        else Nothing
