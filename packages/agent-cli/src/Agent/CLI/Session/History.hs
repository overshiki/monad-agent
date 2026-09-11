-- | Restore and reset persisted conversation history.
module Agent.CLI.Session.History
    ( detectGitBranch
    , foldSessionItems
    , hydrateUiHistory
    , LiveConversation
    , TranscriptCheckpoint(..)
    , TranscriptGeneration
    , currentLiveTranscriptGeneration
    , commitLiveBackendState
    , durableTranscriptCheckpoint
    , evictLiveTranscript
    , replaceLiveConversation
    , withLiveTranscript
    , withLiveBackendState
    , readLiveAttachments
    , readLivePreviousResponseId
    , readLiveTranscript
    , retargetLiveTranscript
    , modifyLiveAttachments
    , resetLiveConversationState
    , resetLiveConversation
    , resetLiveConversationWith
    , writeLivePreviousResponseId
    , writeLiveTranscript
    ) where

import Agent.CLI.Git (detectGitBranch)
import Agent.CLI.Session
    ( SessionTurn(..)
    , TranscriptEffect(..)
    , loadActiveSession
    )
import Agent.CLI.Session.ConversationStore
    ( ConversationStore
    , TranscriptCheckpoint(..)
    , TranscriptGeneration
    )
import qualified Agent.CLI.Session.ConversationStore as ConversationStore
import Agent.Loop (BackendSnapshot, ImageAttachment)
import Agent.OpenAI.Compaction
    ( isCompactSessionTurn
    , isTranscriptResetTurn
    )
import Agent.Responses.Types (ResponseItem)
import Agent.Tools.PlanMode
    ( PlanModeEnv
    , deactivatePlanMode
    )
import Agent.TUI.Model
    ( UiEvent(..)
    , UiState
    , initialUiState
    , reduceUi
    )
import Control.Exception.Safe
    ( throwString
    )
import Control.Monad (void)
import Data.Foldable (foldl')
import Data.IORef
    ( IORef
    , readIORef
    )
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath (OsPath)
import Agent.Store.Postgres.Connection (StorePool)

-- | The pieces of conversation state that must change together when a live
-- session is reset.
--
-- Keeping them in one value makes resets and multi-field updates atomic and
-- prevents callers from observing mismatched response IDs, transcripts, and
-- attachments.
type LiveConversation = ConversationStore

-- | Reset the live conversation atomically.
resetLiveConversationState :: LiveConversation -> IO ()
resetLiveConversationState = ConversationStore.resetConversationStore

readLivePreviousResponseId :: IORef LiveConversation -> IO (Maybe Text)
readLivePreviousResponseId ref =
    readIORef ref >>= ConversationStore.readConversationPreviousResponseId

readLiveTranscript :: IORef LiveConversation -> IO [ResponseItem]
readLiveTranscript ref =
    withLiveTranscript ref pure

withLiveTranscript
    :: IORef LiveConversation
    -> ([ResponseItem] -> IO a)
    -> IO a
withLiveTranscript ref action =
    readIORef ref >>= \store ->
        ConversationStore.withConversationTranscript store action

withLiveBackendState
    :: IORef LiveConversation
    -> (BackendSnapshot -> IO a)
    -> IO a
withLiveBackendState ref action =
    readIORef ref >>= \store ->
        ConversationStore.withConversationBackendState store action

commitLiveBackendState
    :: IORef LiveConversation
    -> BackendSnapshot
    -> IO BackendSnapshot
commitLiveBackendState ref snapshot =
    readIORef ref >>= \store ->
        ConversationStore.commitConversationBackendState store snapshot

readLiveAttachments :: IORef LiveConversation -> IO [ImageAttachment]
readLiveAttachments ref =
    readIORef ref >>= ConversationStore.readConversationAttachments

modifyLiveAttachments
    :: IORef LiveConversation
    -> ([ImageAttachment] -> ([ImageAttachment], a))
    -> IO a
modifyLiveAttachments ref update =
    readIORef ref >>= \store ->
        ConversationStore.modifyConversationAttachments store update

writeLivePreviousResponseId
    :: IORef LiveConversation
    -> Maybe Text
    -> IO ()
writeLivePreviousResponseId ref value =
    readIORef ref >>= \store ->
        ConversationStore.writeConversationPreviousResponseId store value

writeLiveTranscript
    :: IORef LiveConversation
    -> [ResponseItem]
    -> IO ()
writeLiveTranscript ref value =
    readIORef ref >>= \store ->
        void (ConversationStore.commitConversationTranscript store value)

replaceLiveConversation
    :: IORef LiveConversation
    -> Maybe Text
    -> [ResponseItem]
    -> IO TranscriptGeneration
replaceLiveConversation ref previousResponseId transcript =
    readIORef ref >>= \store ->
        ConversationStore.replaceConversationTranscript
            store previousResponseId transcript

currentLiveTranscriptGeneration
    :: IORef LiveConversation
    -> IO TranscriptGeneration
currentLiveTranscriptGeneration ref =
    readIORef ref >>= ConversationStore.currentTranscriptGeneration

evictLiveTranscript
    :: IORef LiveConversation
    -> TranscriptGeneration
    -> TranscriptCheckpoint
    -> IO Bool
evictLiveTranscript ref generation checkpoint =
    readIORef ref >>= \store ->
        ConversationStore.evictConversationTranscript
            store generation checkpoint

-- | Retarget an unchanged transcript to an equivalent durable session
-- snapshot without hydrating it.
retargetLiveTranscript
    :: IORef LiveConversation
    -> TranscriptCheckpoint
    -> IO ()
retargetLiveTranscript ref checkpoint =
    readIORef ref >>= \store ->
        ConversationStore.retargetConversationCheckpoint store checkpoint

-- | Reconstruct the exact root transcript from durable session turns.
--
-- The returned closure captures only durable identity, never the transcript
-- it is intended to release.
durableTranscriptCheckpoint
    :: StorePool
    -> OsPath
    -> Text
    -> TranscriptCheckpoint
durableTranscriptCheckpoint pool root sessionId =
    TranscriptCheckpoint
        { checkpointDescription = "session:" <> sessionId
        , checkpointLoad =
            -- Load only the checkpoint-bounded active suffix. Loading the
            -- complete immutable history would briefly rematerialize every
            -- superseded transcript before 'foldSessionItems' discarded it.
            loadActiveSession pool root sessionId >>= \case
                Left err ->
                    throwString
                        ("could not hydrate durable conversation: "
                            <> Text.unpack err)
                Right (_, turns) ->
                    pure (foldSessionItems turns)
        }

-- | Drop live conversation state without touching persisted session files.
resetLiveConversation
    :: IORef LiveConversation
    -> PlanModeEnv
    -> IO ()
resetLiveConversation =
    resetLiveConversationWith (pure ())

resetLiveConversationWith
    :: IO ()
    -> IORef LiveConversation
    -> PlanModeEnv
    -> IO ()
resetLiveConversationWith resetBackend conversationRef planMode = do
    resetBackend
    readIORef conversationRef >>= resetLiveConversationState
    deactivatePlanMode planMode

-- | Apply compact turns as full transcript replacements when resuming.
foldSessionItems :: [SessionTurn] -> [ResponseItem]
foldSessionItems =
    concat . reverse . foldl' addTurn []
  where
    addTurn chunks turn = case turn.turnEffect of
        TranscriptAppend -> turn.turnItems : chunks
        TranscriptReplace -> [turn.turnItems]
        TranscriptReset -> [turn.turnItems]

hydrateUiHistory :: [SessionTurn] -> UiState
hydrateUiHistory = foldl' addTurn initialUiState
  where
    addTurn state turn
        | isCompactSessionTurn turn.turnUserText =
            addCompactTurn state turn
        | isTranscriptResetTurn turn.turnUserText =
            addResetTurn state turn
        | otherwise =
            addRegularTurn state turn

    -- Compaction replaces the model's inference context, not the transcript
    -- presented to the user. Keep earlier blocks scrollable and append the
    -- compaction summary as the live UI does.
    addCompactTurn state turn =
        case turn.turnAssistantText of
            Nothing -> state
            Just text -> reduceUi (UiSystemMessage text) state

    addResetTurn state turn =
        let cleared = reduceUi UiConversationCleared state
        in case turn.turnAssistantText of
            Nothing -> cleared
            Just text -> reduceUi (UiHistory text) cleared

    addRegularTurn state turn =
        let withUser =
                if Text.null (Text.strip turn.turnUserText)
                    then state
                    else reduceUi
                        (UiUserSubmitted turn.turnUserText)
                        state
            withAssistant = case turn.turnAssistantText of
                Nothing -> withUser
                Just text ->
                    reduceUi (UiAssistantHistory text) withUser
        in case turn.turnError of
            Nothing -> withAssistant
            Just err -> reduceUi (UiErrorMessage err) withAssistant
