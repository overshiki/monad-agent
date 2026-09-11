-- | Pure state transitions for the first-party inline editor.
--
-- Terminal reads, redraws, dictation, interrupts, and history persistence stay
-- in "Agent.CLI.Input"; this module decides what the next editor state and
-- effect should be for each decoded key.
module Agent.CLI.Input.Editor
    ( EditorEffect(..)
    , EditorStep(..)
    , currentMenu
    , initialEditorState
    , reduceEditorKey
    ) where

import Agent.CLI.Command
    ( SlashCatalog
    , SlashMenu(..)
    , SlashSuggestion(..)
    , slashMenuForCatalog
    )
import Agent.CLI.Input.EditOps
import Agent.CLI.Input.Types
    ( EditorKey(..)
    , EditorState(..)
    , ReplLine(..)
    )
import Agent.TUI.TextWidth
    ( nextGraphemeBoundary
    , previousGraphemeBoundary
    )
import Data.Char (isSpace)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Decide the next editor state and requested effect for a decoded key.
--
-- History is passed newest-first, matching Haskeline's 'historyLines'.
reduceEditorKey :: [Text] -> EditorState -> EditorKey -> EditorStep
reduceEditorKey entries state@EditorState{..} key =
    case key of
        EditorEnter ->
            case selectedSuggestion state menu of
                Just suggestion
                    | not (exactCommandSelected state suggestion) ->
                        let accepted = acceptSuggestion state menu suggestion
                        in if suggestion.slashSuggestionTakesArguments
                            then redraw accepted
                            else effect accepted SubmitEditor
                _ -> effect state SubmitEditor
        EditorCycleMode ->
            effect state (ReturnEditor (ReplCycleMode editorText))
        EditorClipboardPaste images ->
            effect state $
                ReturnEditor (ReplClipboardPaste editorText images)
        EditorInterrupt ->
            effect state CheckEditorInterrupt
        EditorEof
            | Text.null editorText ->
                effect state (ReturnEditor ReplEof)
            | otherwise ->
                redraw (deleteAtCursor state)
        EditorUp
            | isJust editorHistoryIndex ->
                redraw (historyMove (-1) entries state)
            | menuHasRows menu ->
                redraw (moveMenuSelection (-1) menu state)
            | otherwise ->
                redraw (historyMove (-1) entries state)
        EditorDown
            | isJust editorHistoryIndex ->
                redraw (historyMove 1 entries state)
            | menuHasRows menu ->
                redraw (moveMenuSelection 1 menu state)
            | otherwise ->
                redraw (historyMove 1 entries state)
        EditorTab ->
            case selectedSuggestion state menu of
                Nothing -> effect state IgnoreEditorInput
                Just suggestion ->
                    redraw (acceptSuggestion state menu suggestion)
        EditorEscape
            | menuHasRows menu ->
                redraw state
                    { editorSelected = 0
                    , editorSlashDismissed = True
                    }
            | otherwise ->
                effect state IgnoreEditorInput
        EditorBackspace -> redraw (backspace state)
        EditorDelete -> redraw (deleteAtCursor state)
        EditorLeft -> redraw state
            { editorCursor =
                previousGraphemeBoundary
                    editorText
                    editorCursor
            , editorSelected = 0
            , editorSlashDismissed = False
            }
        EditorRight -> redraw state
            { editorCursor =
                nextGraphemeBoundary
                    editorText
                    editorCursor
            , editorSelected = 0
            , editorSlashDismissed = False
            }
        EditorHome -> redraw state
            { editorCursor = 0
            , editorSelected = 0
            , editorSlashDismissed = False
            }
        EditorEnd -> redraw state
            { editorCursor = Text.length editorText
            , editorSelected = 0
            , editorSlashDismissed = False
            }
        EditorKillStart -> redraw (killStart state)
        EditorKillEnd -> redraw (killEnd state)
        EditorKillWord -> redraw (killWord state)
        EditorYank -> redraw (insertText editorKillBuffer state)
        EditorClearScreen -> effect state ClearEditorScreen
        EditorDictate -> effect state DictateIntoEditor
        EditorPaste pasted ->
            let pastedState@EditorState{editorText = pastedText} =
                    (insertText pasted state) { editorPasted = True }
            in effect state $
                ReturnEditor
                    (ReplClipboardPasteOrText
                        editorText
                        pasted
                        pastedText)
        EditorInputError message ->
            effect state (ReportEditorError message)
        EditorChar char ->
            redraw (insertText (Text.singleton char) state)
        EditorIgnore ->
            effect state IgnoreEditorInput
  where
    menu = currentMenu state
    effect next requested = EditorStep
        { editorStepState = next
        , editorStepEffect = requested
        }
    redraw = flip effect RedrawEditor . normalizeSelection

menuHasRows :: Maybe SlashMenu -> Bool
menuHasRows = maybe False (not . null . (.slashMenuSuggestions))

currentMenu :: EditorState -> Maybe SlashMenu
currentMenu EditorState{..}
    | not editorSlashEnabled = Nothing
    | editorSlashDismissed = Nothing
    | otherwise =
        slashMenuForCatalog
            editorSlashCatalog
            editorText
            editorCursor

-- | The side effect requested by one pure editor transition.
data EditorEffect
    = RedrawEditor
    | SubmitEditor
    | ReturnEditor !ReplLine
    | CheckEditorInterrupt
    | ClearEditorScreen
    | DictateIntoEditor
    | ReportEditorError !Text
    | IgnoreEditorInput
    deriving (Eq, Show)

-- | The state and effect produced by reducing one decoded key.
data EditorStep = EditorStep
    { editorStepState :: !EditorState
    , editorStepEffect :: !EditorEffect
    }
    deriving (Eq, Show)

{-@ normalizeSelection :: {state:EditorState | CursorOk state}
      -> {v:EditorState | CursorOk v} @-}
normalizeSelection :: EditorState -> EditorState
normalizeSelection state@EditorState{..} =
    case currentMenu state of
        Nothing -> state { editorSelected = 0 }
        Just menu ->
            let count = length menu.slashMenuSuggestions
            in state
                { editorSelected =
                    if count == 0 then 0 else editorSelected `mod` count
                }

acceptSuggestion
    :: EditorState
    -> Maybe SlashMenu
    -> SlashSuggestion
    -> EditorState
acceptSuggestion state@EditorState{..} menu suggestion =
    case menu of
        Nothing -> state
        Just SlashMenu{slashMenuReplaceStart, slashMenuReplaceEnd} ->
            let (before, rest) =
                    Text.splitAt slashMenuReplaceStart editorText
                (_, after) =
                    Text.splitAt
                        (slashMenuReplaceEnd - slashMenuReplaceStart)
                        rest
                replacement = suggestion.slashSuggestionReplacement
                text = before <> replacement <> after
            in state
                { editorText = text
                , editorCursor =
                    slashMenuReplaceStart + Text.length replacement
                , editorSelected = 0
                , editorHistoryIndex = Nothing
                , editorHistoryDraft = text
                , editorSlashDismissed = False
                }
