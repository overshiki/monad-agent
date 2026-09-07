-- | Width-aware rendering helpers for inline terminal input.
module Agent.CLI.Input.Display
    ( terminalTextWidth
    , terminalCharWidth
    , displayCells
    , displayCell
    , safeDisplayChar
    , textColumns
    , charColumns
    , cellsWidth
    , renderCells
    , takeColumns
    , takeSuffixColumns
    , visibleEditorText
    , displayEditorText
    , truncateDisplayText
    ) where

import Agent.CLI.Input.Types (DisplayCell(..))
import Agent.TUI.TextWidth
    ( charCellWidth
    , displayTerminalText
    , graphemeCellWidth
    , graphemeClusters
    )
import Data.Text (Text)
import qualified Data.Text as Text

-- Liquid Haskell verification (see docs/liquid-haskell.md).
--
-- The annotations below are comments for the normal build. The
-- @agent-cli-liquid@ package (cabal.liquid.project, GHC 9.14.1) compiles
-- this same source with @-fplugin=LiquidHaskell@, which checks them.
-- "Agent.TUI.TextWidth" is compiled in the same package, so its verified
-- specs (e.g. 'graphemeCellWidth' being non-negative) are available here.
--
-- The central contract is the column budget: 'takeColumns' and
-- 'takeSuffixColumns' provably never return cells wider than the budget.
--
-- Trusted boundary: the 'tlen' length measure for 'Text', the reverse
-- facts, and the two documented cell-list axioms below.

{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}

-- | Logical length of a 'Text' value (uninterpreted; 'Text' is opaque).
{-@ measure tlen :: Text -> Int @-}

{-@ assume Text.length :: t:Data.Text.Text -> {v:Nat | v == tlen t} @-}

-- | Logical image of 'reverse'.
{-@ measure lrev :: [a] -> [a] @-}

{-@ assume reverse :: xs:[a] -> {v:[a] | v == lrev xs} @-}

-- | Trusted: reversing a cell list preserves its total width (widths are
-- non-negative, so the sum is order-independent).
{-@ assume cellsWidthReverse :: cells:[DisplayCell]
      -> {v:() | cellsWidth (lrev cells) == cellsWidth cells} @-}
cellsWidthReverse :: [DisplayCell] -> ()
cellsWidthReverse _ = ()

-- | Trusted: dropping cells cannot increase the total width (widths are
-- non-negative).
{-@ assume dropZeroWidth :: cells:[DisplayCell]
      -> {v:[DisplayCell] | cellsWidth v <= cellsWidth cells} @-}
dropZeroWidth :: [DisplayCell] -> [DisplayCell]
dropZeroWidth = dropWhile (\(DisplayCell _ width _) -> width == 0)

-- | Attach a lemma's postcondition to an expression (ProofCombinators
-- style).
withProof :: a -> b -> a
withProof x _ = x
{-# INLINE withProof #-}

{-@ terminalTextWidth :: Text -> Nat @-}
terminalTextWidth :: Text -> Int
terminalTextWidth = cellsWidth . displayCells

{-@ terminalCharWidth :: Char -> Nat @-}
terminalCharWidth :: Char -> Int
terminalCharWidth char = case displayCell char of
    DisplayCell _ width _ -> width

displayCells :: Text -> [DisplayCell]
displayCells = map displayCluster . graphemeClusters

displayCell :: Char -> DisplayCell
displayCell = displayCluster . Text.singleton

displayCluster :: Text -> DisplayCell
displayCluster cluster =
    let lineSafe =
            Text.concatMap
                (\character ->
                    if character == '\n'
                        then "↵"
                        else Text.singleton character)
                cluster
        shown = displayTerminalText lineSafe
    in DisplayCell
        { displayCellText = shown
        , displayCellWidth = textColumns shown
        , displayCellSourceLength = Text.length cluster
        }

safeDisplayChar :: Char -> Text
safeDisplayChar char
    | char == '\n' = "↵"
    | otherwise = displayTerminalText (Text.singleton char)

-- | Total display width of a text. Direct recursion (rather than
-- @sum . map graphemeCellWidth@) so the verifier can track non-negativity.
{-@ textColumns :: Text -> Nat @-}
textColumns :: Text -> Int
textColumns = go . graphemeClusters
  where
    {-@ go :: [Text] -> Nat @-}
    go [] = 0
    go (cluster : rest) = graphemeCellWidth cluster + go rest

charColumns :: Char -> Int
charColumns = charCellWidth

-- | Total display width of a cell list. Direct recursion (rather than
-- @sum . map (.displayCellWidth)@) so it can appear in specifications.
{-@ reflect cellsWidth @-}
{-@ cellsWidth :: [DisplayCell] -> Nat @-}
cellsWidth :: [DisplayCell] -> Int
cellsWidth [] = 0
cellsWidth (DisplayCell _ width _ : rest) = width + cellsWidth rest

renderCells :: [DisplayCell] -> Text
renderCells = Text.concat . map (\(DisplayCell text _ _) -> text)

-- | Take the longest prefix of cells fitting the column budget.
-- Verified: the result's total width never exceeds the budget.
{-@ takeColumns :: width:Nat -> [DisplayCell]
      -> {v:[DisplayCell] | cellsWidth v <= width} @-}
takeColumns :: Int -> [DisplayCell] -> [DisplayCell]
takeColumns width = go 0
  where
    {-@ go :: {used:Int | 0 <= used && used <= width} -> [DisplayCell]
          -> {v:[DisplayCell] | used + cellsWidth v <= width} @-}
    go _ [] = []
    go used (DisplayCell text cellWidth sourceLength : rest)
        | used + cellWidth > width = []
        | otherwise =
            DisplayCell text cellWidth sourceLength
                : go (used + cellWidth) rest

-- | Take the longest suffix of cells fitting the column budget (dropping
-- zero-width cells at the cut). Verified: same budget guarantee as
-- 'takeColumns'.
{-@ takeSuffixColumns :: width:Nat -> cells:[DisplayCell]
      -> {v:[DisplayCell] | cellsWidth v <= width} @-}
takeSuffixColumns width cells =
    dropZeroWidth (reverse taken)
        `withProof` cellsWidthReverse taken
  where
    taken = takeColumns width (reverse cells)

visibleEditorText :: Int -> Text -> Int -> (Text, Int)
visibleEditorText available raw requestedCursor
    | available <= 0 = ("", 0)
    | otherwise =
        let cells = fitCells False (displayCells raw)
            cursor = max 0 (min (Text.length raw) requestedCursor)
            before = cellsBeforeCursor cursor cells
        in if cellsWidth cells <= available
            then (renderCells cells, cellsWidth before)
            else
                let leftRoom = max 1 (available * 2 `div` 3)
                    preferredBefore = takeSuffixColumns leftRoom before
                    -- A wide glyph may not fit in the preferred left-hand
                    -- budget. If it fits in the full viewport, keep it
                    -- rather than dropping all text before the cursor.
                    visibleBefore =
                        if null preferredBefore
                            then takeSuffixColumns available before
                            else preferredBefore
                    start = length before - length visibleBefore
                    shownCells = takeColumns available (drop start cells)
                in (renderCells shownCells, cellsWidth visibleBefore)
  where
    -- A single terminal glyph can occupy two cells, so it cannot be shown
    -- verbatim in a one-cell viewport. Keep a visible placeholder and,
    -- crucially, preserve the cursor's cell position.
    fitCells _ [] = []
    fitCells suppressCombining (cell@(DisplayCell _ cellWidth _) : rest)
        | cellWidth > available =
            cell
                { displayCellText = "…"
                , displayCellWidth = 1
                }
                : fitCells True rest
        | suppressCombining
        , cellWidth == 0 =
            cell { displayCellText = "" } : fitCells True rest
        | otherwise =
            cell : fitCells False rest

    cellsBeforeCursor cursor = go 0
      where
        go _ [] = []
        go offset (cell@(DisplayCell _ _ sourceLength) : rest)
            | offset + sourceLength <= cursor =
                cell : go (offset + sourceLength) rest
            | otherwise = []

displayEditorText :: Text -> Text
displayEditorText = renderCells . displayCells

truncateDisplayText :: Int -> Text -> Text
truncateDisplayText width text
    | width <= 0 = ""
    | cellsWidth cells <= width = renderCells cells
    | width == 1 = "…"
    | otherwise = renderCells (takeColumns (width - 1) cells) <> "…"
  where
    cells = displayCells text
