-- | Pure cursor/text editing operations for the inline editor.
--
-- These operations sit in their own leaf module (depending only on
-- "Agent.CLI.Input.Types", "Agent.CLI.Command.Types", "Agent.TUI.TextWidth",
-- and "Data.Text") so the pure editing core can be built — and verified —
-- without the slash-command menu engine or the terminal input loop.
--
-- The central contract ('CursorOk', verified with Liquid Haskell — see
-- docs/liquid-haskell.md): after every editing operation the cursor stays
-- inside the text.
module Agent.CLI.Input.EditOps
    ( backspace
    , deleteAtCursor
    , exactCommandSelected
    , historyMove
    , initialEditorState
    , insertText
    , killEnd
    , killStart
    , killWord
    , moveMenuSelection
    , selectedSuggestion
    ) where

import Agent.CLI.Command.Types (SlashCatalog, SlashMenu(..), SlashSuggestion(..))
import Agent.CLI.Input.Types (EditorState(..))
import Agent.TUI.TextWidth
    ( nextGraphemeBoundary
    , previousGraphemeBoundary
    )
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

-- Liquid Haskell verification (see docs/liquid-haskell.md).
--
-- The annotations below are comments for the normal build. The
-- @agent-cli-liquid@ package (cabal.liquid.project, GHC 9.14.1) compiles
-- this same source with @-fplugin=LiquidHaskell@, which checks them.
-- Trusted boundary: the documented @assume@ specs for "Data.Text"
-- functions and wrappers below (all true).

{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}

-- The logical 'tlen' length measure and the 'Text.length' spec come from
-- "Agent.TUI.TextWidth", compiled in the same package and imported below;
-- sharing one measure keeps the grapheme-boundary contracts compatible
-- with the 'CursorOk' invariant.

-- | 'splitAt' splits at the count when it fits; the pieces sum to the input.
{-@ assume Text.splitAt :: n:Nat -> t:Data.Text.Text
      -> {v:(Data.Text.Text, Data.Text.Text) | tlen (fst v) <= n
                                                && tlen (fst v) <= tlen t
                                                && (n <= tlen t => tlen (fst v) == n)
                                                && tlen (fst v) + tlen (snd v) == tlen t
                                                && tlen (snd v) >= 0} @-}

-- | A 'take' prefix is bounded by both the count and the input.
{-@ assume Text.take :: n:Nat -> t:Data.Text.Text
      -> {v:Data.Text.Text | tlen v >= 0
                             && tlen v <= n
                             && tlen v <= tlen t
                             && (n <= tlen t => tlen v == n)} @-}

-- | Dropping @n <= tlen t@ characters shortens by exactly @n@.
{-@ assume Text.drop :: n:Nat -> t:Data.Text.Text
      -> {v:Data.Text.Text | tlen v >= 0
                             && tlen v <= tlen t
                             && (n <= tlen t => tlen v == tlen t - n)} @-}

-- | Dropping @n <= tlen t@ trailing characters shortens by exactly @n@.
{-@ assume Text.dropEnd :: n:Nat -> t:Data.Text.Text
      -> {v:Data.Text.Text | tlen v >= 0
                             && tlen v <= tlen t
                             && (n <= tlen t => tlen v == tlen t - n)} @-}

-- | A 'takeWhileEnd' suffix is never longer than the input.
{-@ assume takeWhileEndSpace :: t:Data.Text.Text
      -> {v:Data.Text.Text | tlen v >= 0 && tlen v <= tlen t} @-}
takeWhileEndSpace :: Text -> Text
takeWhileEndSpace = Text.takeWhileEnd isSpace

{-@ assume takeWhileEndNotSpace :: t:Data.Text.Text
      -> {v:Data.Text.Text | tlen v >= 0 && tlen v <= tlen t} @-}
takeWhileEndNotSpace :: Text -> Text
takeWhileEndNotSpace = Text.takeWhileEnd (not . isSpace)

-- | Appending adds lengths. (Wrapper: '(<>)' is a class method.)
{-@ assume appendText :: a:Data.Text.Text -> b:Data.Text.Text
      -> {v:Data.Text.Text | tlen v == tlen a + tlen b} @-}
appendText :: Text -> Text -> Text
appendText = (<>)

-- | List length. (Wrapper: 'length' is a Foldable class method.)
{-@ assume lengthL :: xs:[a] -> {v:Nat | v == len xs} @-}
lengthL :: [a] -> Int
lengthL = length

-- | 'null' decides emptiness. (Wrapper: 'null' is a Foldable class method.)
{-@ assume nullL :: xs:[a] -> {v:Bool | v <=> (len xs == 0)} @-}
nullL :: [a] -> Bool
nullL = null

-- | 'mod' with a positive modulus lands in range. (Wrapper: 'mod' is an
-- Integral class method; the contract is true for every dividend.)
{-@ assume modPos :: dividend:Int -> {divisor:Int | divisor >= 1}
      -> {v:Nat | v < divisor} @-}
-- The body is trusted (a 'mod' wrapper); GHC's 'mod' carries a /= 0 premise
-- that only the assumed contract supplies.
{-@ ignore modPos @-}
modPos :: Int -> Int -> Int
modPos dividend divisor = mod dividend divisor

-- | The central editor invariant: the cursor is inside the text.
{-@ predicate CursorOk S = 0 <= editorCursor S && editorCursor S <= tlen (editorText S) @-}

{-@ reflect isJustM @-}
isJustM :: Maybe a -> Bool
isJustM (Just _) = True
isJustM Nothing = False

-- | Whether the menu exists and has selectable suggestions.
{-@ reflect menuHasSuggestions @-}
menuHasSuggestions :: Maybe SlashMenu -> Bool
menuHasSuggestions (Just SlashMenu{slashMenuSuggestions}) =
    not (nullL slashMenuSuggestions)
menuHasSuggestions Nothing = False

{-@ initialEditorState :: SlashCatalog -> Bool -> initial:Text
      -> {v:EditorState | CursorOk v} @-}
initialEditorState :: SlashCatalog -> Bool -> Text -> EditorState
initialEditorState catalog slashEnabled initial =
    EditorState
        { editorText = initial
        , editorCursor = Text.length initial
        , editorSelected = 0
        , editorHistoryIndex = Nothing
        , editorHistoryDraft = initial
        , editorKillBuffer = ""
        , editorPasted = False
        , editorSlashEnabled = slashEnabled
        , editorSlashDismissed = False
        , editorSlashCatalog = catalog
        }

{-@ moveMenuSelection :: Int -> Maybe SlashMenu
      -> {state:EditorState | CursorOk state}
      -> {v:EditorState | CursorOk v} @-}
moveMenuSelection :: Int -> Maybe SlashMenu -> EditorState -> EditorState
moveMenuSelection delta menu state@EditorState{..} =
    case menu of
        Nothing -> state
        Just SlashMenu{slashMenuSuggestions}
            | nullL slashMenuSuggestions -> state
            | otherwise ->
                let count = length slashMenuSuggestions
                    selected = (editorSelected + delta) `mod` count
                in state { editorSelected = selected }

-- | A non-empty menu always yields a suggestion (the selection index is
-- reduced into range first).
{-@ selectedSuggestion :: state:EditorState -> menu:Maybe SlashMenu
      -> {v:Maybe SlashSuggestion | menuHasSuggestions menu => isJustM v} @-}
selectedSuggestion
    :: EditorState
    -> Maybe SlashMenu
    -> Maybe SlashSuggestion
selectedSuggestion state@EditorState{..} menu =
    case menu of
        Nothing -> Nothing
        Just SlashMenu{slashMenuSuggestions}
            | nullL slashMenuSuggestions -> Nothing
            | otherwise ->
                Just $
                    slashMenuSuggestions
                        !! modPos editorSelected (lengthL slashMenuSuggestions)

exactCommandSelected :: EditorState -> SlashSuggestion -> Bool
exactCommandSelected state@EditorState{..} suggestion =
    Text.strip editorText == suggestion.slashSuggestionDisplay

{-@ insertText :: inserted:Text -> {state:EditorState | CursorOk state}
      -> {v:EditorState | CursorOk v} @-}
insertText :: Text -> EditorState -> EditorState
insertText inserted state@EditorState{..} =
    let (before, after) =
            Text.splitAt editorCursor editorText
        text = appendText before (appendText inserted after)
    in state
        { editorText = text
        , editorCursor = editorCursor + Text.length inserted
        , editorSelected = 0
        , editorHistoryIndex = Nothing
        , editorHistoryDraft = text
        , editorSlashDismissed = False
        }

{-@ backspace :: {state:EditorState | CursorOk state}
      -> {v:EditorState | CursorOk v} @-}
backspace :: EditorState -> EditorState
backspace state@EditorState{..}
    | editorCursor <= 0 = state
    | otherwise =
        let start =
                previousGraphemeBoundary
                    editorText
                    editorCursor
            (before, rest) = Text.splitAt start editorText
            (_, after) =
                Text.splitAt (editorCursor - start) rest
            text = appendText before after
        in state
            { editorText = text
            , editorCursor = start
            , editorSelected = 0
            , editorHistoryIndex = Nothing
            , editorHistoryDraft = text
            , editorSlashDismissed = False
            }

{-@ deleteAtCursor :: {state:EditorState | CursorOk state}
      -> {v:EditorState | CursorOk v} @-}
deleteAtCursor :: EditorState -> EditorState
deleteAtCursor state@EditorState{..}
    | editorCursor >= Text.length editorText = state
    | otherwise =
        let (before, rest) =
                Text.splitAt editorCursor editorText
            end =
                nextGraphemeBoundary
                    editorText
                    editorCursor
            (_, after) =
                Text.splitAt (end - editorCursor) rest
            text = appendText before after
        in state
            { editorText = text
            , editorSelected = 0
            , editorHistoryIndex = Nothing
            , editorHistoryDraft = text
            , editorSlashDismissed = False
            }

{-@ killStart :: {state:EditorState | CursorOk state}
      -> {v:EditorState | CursorOk v} @-}
killStart :: EditorState -> EditorState
killStart state@EditorState{..} =
    let (killed, after) =
            Text.splitAt editorCursor editorText
    in state
        { editorText = after
        , editorCursor = 0
        , editorSelected = 0
        , editorHistoryIndex = Nothing
        , editorHistoryDraft = after
        , editorKillBuffer = killed
        , editorSlashDismissed = False
        }

{-@ killEnd :: {state:EditorState | CursorOk state}
      -> {v:EditorState | CursorOk v} @-}
killEnd :: EditorState -> EditorState
killEnd state@EditorState{..} =
    let (before, killed) =
            Text.splitAt editorCursor editorText
    in state
        { editorText = before
        , editorSelected = 0
        , editorHistoryIndex = Nothing
        , editorHistoryDraft = before
        , editorKillBuffer = killed
        , editorSlashDismissed = False
        }

{-@ killWord :: {state:EditorState | CursorOk state}
      -> {v:EditorState | CursorOk v} @-}
killWord :: EditorState -> EditorState
killWord state@EditorState{..}
    | editorCursor <= 0 = state
    | otherwise =
        let before = Text.take editorCursor editorText
            trailingSpaces =
                Text.length (takeWhileEndSpace before)
            withoutSpaces = Text.dropEnd trailingSpaces before
            wordLength =
                Text.length (takeWhileEndNotSpace withoutSpaces)
            start = editorCursor - trailingSpaces - wordLength
            killed =
                Text.take
                    (editorCursor - start)
                    (Text.drop start editorText)
            text =
                appendText
                    (Text.take start editorText)
                    (Text.drop editorCursor editorText)
        in state
            { editorText = text
            , editorCursor = start
            , editorSelected = 0
            , editorHistoryIndex = Nothing
            , editorHistoryDraft = text
            , editorKillBuffer = killed
            , editorSlashDismissed = False
            }

{-@ historyMove :: Int -> [Text] -> {state:EditorState | CursorOk state}
      -> {v:EditorState | CursorOk v} @-}
historyMove :: Int -> [Text] -> EditorState -> EditorState
historyMove delta entries state@EditorState{..}
    | nullL entries = state
    | otherwise =
        let current = fromMaybe (-1) editorHistoryIndex
            next = current - delta
        in if next < 0
            then state
                { editorText = editorHistoryDraft
                , editorCursor = Text.length editorHistoryDraft
                , editorHistoryIndex = Nothing
                , editorSelected = 0
                , editorSlashDismissed = False
                }
            else if next >= lengthL entries
                then state
                else
                    let text = entries !! next
                        draft =
                            case editorHistoryIndex of
                                Nothing -> editorText
                                Just _ -> editorHistoryDraft
                    in state
                        { editorText = text
                        , editorCursor = Text.length text
                        , editorHistoryIndex = Just next
                        , editorHistoryDraft = draft
                        , editorSelected = 0
                        , editorSlashDismissed = False
                        }
