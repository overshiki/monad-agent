-- | Session interaction helpers shared by turn handling and the REPL.
module Agent.CLI.Session.Interaction
    ( buildPromptState
    , runBtwQuestion
    , runModelTest
    , setSessionEffort
    , setSessionEffortText
    , syncFullscreenPrompt
    ) where

import Agent.CLI.Turn.Btw
    ( formatBtwError
    , runBtwWithCancel
    )
import Agent.CLI.Turn.CancelWatch (withEscCancel)
import Agent.CLI.Command
    ( currentEffort
    , currentModel
    , setReasoningEffort
    )
import Agent.CLI.Turn.Interrupt (withTurnCancel)
import Agent.CLI.Model.ModelTest
    ( ProbeOutcome(..)
    , catalogTargets
    , classifyProbe
    , defaultProbeTimeoutMicros
    , formatProbeLine
    , matchesFilters
    , modelTestQuestion
    , targetConn
    , targetId
    , targetProvider
    )
import Agent.CLI.Subagents.Runtime (loadForeignHttpBackend)
import Agent.Concurrent (mapConcurrentlyBounded)
import Agent.CLI.Options
    ( ApprovalPolicy
    , normalizeReasoningEffortForDialect
    , reasoningEffortsForDialect
    )
import Agent.CLI.Render
    ( RenderConfig(..)
    , putTextLn
    , renderAssistantText
    )
import Agent.CLI.Provider.Request (setRequestModel)
import Agent.CLI.TUI.ReplMode
    ( replModeFromState
    , replModeLabel
    )
import Agent.CLI.Session
    ( Persistence(..)
    , PersistenceState(..)
    , SessionHandle(..)
    , SessionMeta(..)
    , SessionCreate(..)
    , writeSessionMeta
    )
import Agent.CLI.Session.SessionEnv (SessionEnv(..))
import Agent.CLI.Session.History
    ( readLiveAttachments
    , readLiveTranscript
    )
import Agent.CLI.Util.Style (roleError)
import Agent.CLI.Util.Terminal (resolveColor)
import Agent.CLI.TUI.App
    ( emitUiEvent
    )
import Agent.Loop (TokenUsage)
import Agent.Provider (Provider(..))
import Agent.Dialect (DialectId, dialectId)
import Agent.ReasoningEffort
    ( ReasoningEffort
    , parseReasoningEffort
    , reasoningEffortText
    )
import Agent.Responses.Types
    ( ResponseCreateParams
    , ResponseItem
    )
import Agent.Tools.PlanMode
    ( PlanModeEnv(..)
    , PlanModeState
    )
import Agent.TUI.Model
    ( PromptState(..)
    , UiEvent(..)
    , progressNotice
    )
import Control.Monad (forM_)
import Data.IORef
    ( modifyIORef'
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Timeout (timeout)

-- | Publish the current session prompt metadata to a retained fullscreen
-- runtime before replaying a pending turn after a provider rebuild.
syncFullscreenPrompt :: SessionEnv -> IO ()
syncFullscreenPrompt env =
    forM_ env.sessionFullscreen \runtime -> do
        planState <- readIORef env.sessionPlanMode.planStateRef
        params <- readIORef env.sessionParams
        policy <- readIORef env.sessionPolicy
        account <- readIORef env.sessionAccount
        usage <- readIORef env.sessionUsage
        attachments <- readLiveAttachments env.sessionConversation
        emitUiEvent runtime $ UiSetPrompt $
            buildPromptState
                (dialectId env.sessionDialect)
                params
                planState
                policy
                account
                (isJust env.sessionSelectAccount)
                usage
                (length attachments)

buildPromptState
    :: DialectId
    -> ResponseCreateParams
    -> PlanModeState
    -> ApprovalPolicy
    -> Text
    -> Bool
    -> TokenUsage
    -> Int
    -> PromptState
buildPromptState activeDialect params planState policy account accountSelectable usage attachments =
    PromptState
        { promptModel = currentModel params
        , promptEffort =
            reasoningEffortText $
                normalizeReasoningEffortForDialect
                    activeDialect
                    (currentEffort params)
        , promptEffortOptions =
            map reasoningEffortText
                (reasoningEffortsForDialect activeDialect)
        , promptMode =
            replModeLabel (replModeFromState planState policy)
        , promptAccount = account
        , promptAccountSelectable = accountSelectable
        , promptUsage = usage
        , promptLimitStatus = Nothing
        , promptAttachments = attachments
        }

setSessionEffort :: SessionEnv -> ReasoningEffort -> IO ()
setSessionEffort env level = do
    modifyIORef' env.sessionParams (setReasoningEffort level)
    let levelText = reasoningEffortText level
    case env.sessionFullscreen of
        Just runtime ->
            emitUiEvent runtime (UiSetPromptEffort levelText)
        Nothing -> pure ()
    case env.sessionPersist of
        PersistenceDisabled -> pure ()
        PersistenceEnabled slotRef -> do
            slot <- readIORef slotRef
            case slot of
                PersistencePending pending sessionId tempDir ->
                    writeIORef slotRef
                        (PersistencePending
                            pending { createEffort = levelText }
                            sessionId
                            tempDir)
                PersistenceActive handle -> do
                    let meta =
                            handle.sessionMeta { metaEffort = levelText }
                    writeSessionMeta
                        handle.sessionPool
                        handle.sessionMetaPath
                        meta
                    writeIORef slotRef
                        (PersistenceActive handle { sessionMeta = meta })

setSessionEffortText :: SessionEnv -> Text -> IO ()
setSessionEffortText env level =
    case parseReasoningEffort level of
        Left err -> ioError (userError (Text.unpack err))
        Right effort -> setSessionEffort env effort

runBtwQuestion :: Bool -> SessionEnv -> Text -> IO ()
runBtwQuestion registerCancel env question = do
    let fullscreen = env.sessionFullscreen
        stdoutHandle = env.sessionRender.renderStdout
        stderrHandle = env.sessionRender.renderStderr
    color <- resolveColor stdoutHandle
    transcriptRef <- newIORef =<< readLiveTranscript env.sessionConversation
    forM_ fullscreen \runtime ->
        emitUiEvent runtime
            (UiSetNotice (Just (progressNotice "btw · asking…")))
    result <-
        runBtwWithCancel
            (\cancel action ->
                if registerCancel
                    then
                        withTurnCancel env.sessionInterrupt cancel $
                            case fullscreen of
                                Nothing
                                    | env.sessionBackground -> action
                                    | otherwise ->
                                        withEscCancel
                                            cancel
                                            env.sessionEscPaused
                                            action
                                Just _ -> action
                    else action)
            env.sessionBtwBackend
            env.sessionParams
            transcriptRef
            question
    forM_ fullscreen \runtime ->
        emitUiEvent runtime (UiSetNotice Nothing)
    case result of
        Left err -> do
            errorColor <- resolveColor stderrHandle
            let message = formatBtwError err
            case fullscreen of
                Just runtime ->
                    emitUiEvent runtime (UiErrorMessage message)
                Nothing ->
                    putTextLn stderrHandle (roleError errorColor message)
        Right answer ->
            case fullscreen of
                Just runtime ->
                    emitUiEvent runtime (UiAssistantHistory answer)
                Nothing ->
                    putTextLn stdoutHandle (renderAssistantText color answer)

-- | Probe the models a subagent can be dispatched to — every catalog model
-- with an available credential, across providers — and report which respond
-- right now. Out-of-band like @/btw@: nothing enters the conversation, no
-- tools are offered, and every probe is one fresh request with a small fixed
-- question. The session's own provider uses the session backend; foreign
-- providers use a lazily loaded one-shot backend (shared with cross-provider
-- subagent dispatch); models whose provider has no credential are skipped.
runModelTest :: SessionEnv -> [Text] -> IO ()
runModelTest env filters = do
    let stdoutHandle = env.sessionRender.renderStdout
        stderrHandle = env.sessionRender.renderStderr
        fullscreen = env.sessionFullscreen
        targets =
            filter (matchesFilters filters)
                (catalogTargets env.sessionModelCatalog)
    if null targets
        then do
            color <- resolveColor stderrHandle
            let suffix =
                    if null filters
                        then ""
                        else
                            " matching "
                                <> Text.intercalate ", " filters
                message =
                    "no configured models to test"
                        <> suffix
                        <> " in the model catalog"
            case fullscreen of
                Just runtime -> emitUiEvent runtime (UiErrorMessage message)
                Nothing ->
                    putTextLn stderrHandle (roleError color message)
        else do
            color <- resolveColor stdoutHandle
            baseParams <- readIORef env.sessionParams
            forM_ fullscreen \runtime ->
                emitUiEvent runtime
                    (UiSetNotice (Just (progressNotice "model-test · probing…")))
            results <-
                mapConcurrentlyBounded 4 (probeTarget baseParams) targets
            forM_ fullscreen \runtime ->
                emitUiEvent runtime (UiSetNotice Nothing)
            let okCount = length [() | (_, _, ProbeOk) <- results]
                summary =
                    "model-test: "
                        <> Text.pack (show okCount)
                        <> "/"
                        <> Text.pack (show (length results))
                        <> " models responded"
                emitLine line =
                    case fullscreen of
                        Just runtime -> emitUiEvent runtime (UiSystemMessage line)
                        Nothing -> putTextLn stdoutHandle line
            mapM_ (emitLine . summarize) results
            emitLine summary
  where
    probeTarget baseParams target
        | targetProvider target == env.sessionProvider
        , targetConn target == env.sessionConnection =
            runProbe baseParams env.sessionBtwBackend target
        | otherwise = do
            backendFactory <- loadForeignHttpBackend (targetProvider target)
            case backendFactory of
                Nothing ->
                    pure
                        ( target
                        , Nothing
                        , ProbeSkipped
                            ("no credential for "
                                <> providerName (targetProvider target))
                        )
                Just mkBackend ->
                    runProbe baseParams mkBackend target

    runProbe baseParams mkBackend target = do
        probeParamsRef <-
            newIORef
                (setRequestModel
                    (targetProvider target)
                    (targetId target)
                    baseParams)
        transcriptRef <- newIORef []
        started <- getCurrentTime
        result <-
            timeout defaultProbeTimeoutMicros $
                runBtwWithCancel
                    cancelScope
                    mkBackend
                    probeParamsRef
                    transcriptRef
                    modelTestQuestion
        finished <- getCurrentTime
        let latencyMs =
                floor
                    (realToFrac (diffUTCTime finished started) * 1000)
                    :: Int
            outcome =
                case result of
                    Nothing -> ProbeTimeout
                    Just probeResult -> classifyProbe probeResult
        pure (target, Just latencyMs, outcome)

    providerName = \case
        DeepSeekProvider -> "deepseek"
        KimiProvider -> "kimi"
        KimiCodeProvider -> "kimi-code"
        OpenRouterProvider -> "openrouter"
        OpenAIProvider -> "openai"
        XAIProvider -> "xai"
        GeminiProvider -> "gemini"
        ClaudeCodeProvider -> "claude"

    cancelScope cancel action =
        withTurnCancel env.sessionInterrupt cancel $
            case env.sessionFullscreen of
                Nothing
                    | env.sessionBackground -> action
                    | otherwise ->
                        withEscCancel cancel env.sessionEscPaused action
                Just _ -> action

    summarize (target, latencyMs, outcome) =
        formatProbeLine target latencyMs outcome
