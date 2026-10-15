-- | Text layout helpers shared by fixed-size CLI overlays.
module Agent.CLI.Util.TextLayout
    ( SplitPaneFrame(..)
    , clampSelectionIndex
    , fitTextCell
    , renderSplitPaneFrame
    , selectionWindow
    , transcriptPreviewRows
    ) where

import Agent.CLI.Input.Display
    ( displayCells
    , displayEditorText
    , terminalTextWidth
    , truncateDisplayText
    )
import Agent.CLI.Input.Types (DisplayCell(..))
import Data.Text (Text)
import qualified Data.Text as Text

-- Liquid Haskell verification (see docs/liquid-haskell.md).
--
-- The annotations below are comments for the normal build. The
-- @agent-cli-liquid@ package (cabal.liquid.project, GHC 9.14.1) compiles
-- this same source with @-fplugin=LiquidHaskell@, which checks them.
--
-- Verified: a clamped selection index is always in range, the selection
-- window never exceeds its row budget, and 'atMay' always succeeds for
-- in-range indices. Trusted boundary: the documented @assume@ specs for
-- Prelude list functions below (all true).

{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}

-- Termination checking is disabled: 'hardWrapText' terminates via a
-- lexicographic argument (retrying the same cell only with an empty line
-- accumulator, which then makes progress) that the checker cannot see.
-- Totality is covered by the agent-cli test suite.
{-@ LIQUID "--no-termination" @-}

{-@ reflect isJustM @-}
isJustM :: Maybe a -> Bool
isJustM (Just _) = True
isJustM Nothing = False

-- | 'take' never returns more than the count or the input.
{-@ assume take :: n:Nat -> xs:[a]
      -> {v:[a] | len v <= n && len v <= len xs} @-}

-- | 'drop' removes exactly @n@ elements when @n@ fits.
{-@ assume drop :: n:Nat -> xs:[a]
      -> {v:[a] | len v <= len xs && (n <= len xs => len v == len xs - n)} @-}

-- | 'zip' never returns more than either input.
{-@ assume zip :: xs:[a] -> ys:[b] -> {v:[(a, b)] | len v <= len ys} @-}

-- | List length. (Wrapper: 'length' is a Foldable class method and cannot
-- be named in an assume.)
{-@ assume lengthL :: xs:[a] -> {v:Nat | v == len xs} @-}
lengthL :: [a] -> Int
lengthL = length

-- | Presentation inputs for a fixed-height, two-column selection overlay.
--
-- The renderer owns the shared layout, selection-window, padding, and
-- transcript-tail behavior. Callers supply domain labels and styling.
data SplitPaneFrame item = SplitPaneFrame
    { splitPaneMinColumns :: !Int
    , splitPaneColumns :: !Int
    , splitPaneBodyRows :: !Int
    , splitPaneLeftMinWidth :: !Int
    , splitPaneLeftMaxWidth :: !Int
    , splitPaneDivider :: !Text
    , splitPaneTitle :: !Text
    , splitPaneHeaderDetail :: !(Int -> Text)
    , splitPaneLeftHeading :: !Text
    , splitPaneRightHeading :: !(Maybe item -> Text)
    , splitPaneItems :: ![item]
    , splitPaneSelectedIndex :: !Int
    , splitPaneLeftLabel :: !(Int -> item -> Text)
    , splitPaneTranscript :: !(item -> [Text])
    , splitPaneEmptyTranscript :: !Text
    , splitPaneFooter :: !Text
    , splitPanePromptStyle :: !(Text -> Text)
    , splitPaneMutedStyle :: !(Text -> Text)
    , splitPaneSelectedStyle :: !(Text -> Text)
    }

renderSplitPaneFrame :: SplitPaneFrame item -> Text
renderSplitPaneFrame SplitPaneFrame{..} =
    Text.intercalate "\n" (header : headings : body <> [footer])
  where
    cols = max 0 (max splitPaneMinColumns splitPaneColumns)
    bodyRows = max 0 splitPaneBodyRows
    dividerText = truncateDisplayText cols splitPaneDivider
    dividerWidth = terminalTextWidth dividerText
    divider = splitPaneMutedStyle dividerText
    paneWidth = max 0 (cols - dividerWidth)
    desiredLeftWidth =
        max splitPaneLeftMinWidth $
            min splitPaneLeftMaxWidth
                (paneWidth * 2 `div` 5)
    leftWidth = max 0 (min paneWidth desiredLeftWidth)
    rightWidth = paneWidth - leftWidth
    items = splitPaneItems
    itemCount = length items
    selectedIndex =
        clampSelectionIndex itemCount splitPaneSelectedIndex
    shown = selectionWindow bodyRows selectedIndex items
    selected = atMay selectedIndex items
    fittedTitle = truncateDisplayText cols splitPaneTitle
    header =
        splitPanePromptStyle fittedTitle
            <> splitPaneMutedStyle
                (fitTextCell
                    (max 0 (cols - terminalTextWidth fittedTitle))
                    (" · " <> splitPaneHeaderDetail itemCount))
    headings =
        splitPanePromptStyle
            (fitTextCell leftWidth splitPaneLeftHeading)
            <> divider
            <> splitPanePromptStyle
                (fitTextCell rightWidth
                    (splitPaneRightHeading selected))
    leftRows =
        map
            (\(absoluteIndex, item) ->
                let prefix =
                        if absoluteIndex == selectedIndex then "› " else "  "
                    text =
                        fitTextCell leftWidth
                            (prefix
                                <> splitPaneLeftLabel absoluteIndex item)
                in if absoluteIndex == selectedIndex
                    then splitPaneSelectedStyle text
                    else text)
            shown
            <> repeat (Text.replicate leftWidth " ")
    rightRows = case selected of
        Nothing ->
            splitPaneMutedStyle
                (fitTextCell rightWidth splitPaneEmptyTranscript)
                : repeat (Text.replicate rightWidth " ")
        Just item ->
            map (fitTextCell rightWidth)
                (transcriptPreviewRows
                    rightWidth bodyRows (splitPaneTranscript item))
                <> repeat (Text.replicate rightWidth " ")
    body =
        take bodyRows $
            zipWith (\left right -> left <> divider <> right) leftRows rightRows
    footer =
        splitPaneMutedStyle $
            fitTextCell cols splitPaneFooter

-- | In-range indices always yield a value.
{-@ atMay :: index:Int -> values:[a]
      -> {v:Maybe a | (0 <= index && index < len values) => isJustM v} @-}
atMay :: Int -> [a] -> Maybe a
atMay index values
    | index < 0 = Nothing
    | otherwise = case drop index values of
        value : _ -> Just value
        [] -> Nothing

-- | A clamped selection index is always in range: non-negative, and below
-- the item count whenever there are items.
{-@ clampSelectionIndex :: count:Int -> index:Int
      -> {v:Int | 0 <= v && (count >= 1 => v < count) && (count <= 0 => v == 0)} @-}
clampSelectionIndex :: Int -> Int -> Int
clampSelectionIndex count index
    | count <= 0 = 0
    | index < 0 = 0
    | index >= count = count - 1
    | otherwise = index

-- | The selection window never exceeds its row budget.
{-@ selectionWindow :: count:Nat -> selected:Int -> values:[a]
      -> {v:[(Int, a)] | len v <= count} @-}
selectionWindow :: Int -> Int -> [a] -> [(Int, a)]
selectionWindow count selected values =
    let total = lengthL values
        start = max 0 (min selected (total - count))
    in zip [start ..] (take count (drop start values))

transcriptPreviewRows :: Int -> Int -> [Text] -> [Text]
transcriptPreviewRows width count logicalLines =
    let wrapped = concatMap (hardWrapText width) logicalLines
        rows
            | null wrapped = ["(empty transcript)"]
            | otherwise = wrapped
    in drop (max 0 (lengthL rows - count)) rows

hardWrapText :: Int -> Text -> [Text]
hardWrapText width raw
    | Text.null raw = [""]
    | otherwise = go [] 0 (displayCells raw)
  where
    width' = max 1 width
    go currentRev _ [] =
        [Text.concat (reverse currentRev) | not (null currentRev)]
    go currentRev used input@(cell : rest)
        | cellWidth cell > width' =
            let replacement =
                    truncateDisplayText width' (cellText cell)
                prefix =
                    [ Text.concat (reverse currentRev)
                    | not (null currentRev)
                    ]
            in prefix <> (replacement : go [] 0 rest)
        | cellWidth cell > 0
            && used > 0
            && used + cellWidth cell > width' =
                Text.concat (reverse currentRev) : go [] 0 input
        | otherwise =
            go
                (cellText cell : currentRev)
                (used + cellWidth cell)
                rest
    cellWidth (DisplayCell _ widthOfCell _) = widthOfCell
    cellText (DisplayCell textOfCell _ _) = textOfCell

fitTextCell :: Int -> Text -> Text
fitTextCell width raw
    | width <= 0 = ""
    | otherwise =
        fitted
            <> Text.replicate
                (max 0 (width - terminalTextWidth fitted))
                " "
  where
    clean =
        displayEditorText $
            Text.map
                (\c -> if c == '\t' || c == '\r' || c == '\n' then ' ' else c)
                raw
    fitted = truncateDisplayText width clean
