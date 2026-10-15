module Agent.CLI.Runtime.Orchestration.Flow (runAgentWithRuntime, withRestoredCurrentDirectory) where

import Agent.CLI.Auth.AccountPicker ()
import Agent.CLI.Auth.AccountSelection ()
import Agent.CLI.Session.Afk ()
import Agent.CLI.AgentSessions ( signalManagedSessionReady )
import Agent.CLI.AgentViewport ( AgentTarget(AgentRoot) )
import Agent.CLI.Approval ()
import Agent.CLI.Turn.Artifact ()
import Agent.CLI.Auth ()
import Agent.CLI.Clipboard ()
import Agent.CLI.CodeModeRuntime ()
import Agent.CLI.Command ()
import Agent.CLI.Compaction ()
import Agent.CLI.Config ()
import Agent.CLI.Provider.Connectivity ()
import Agent.CLI.Database ()
import Agent.CLI.Database.Store ()
import Agent.CLI.Dialects ()
import Agent.CLI.Util.Error ( formatApiErrorAt )
import Agent.CLI.Provider.GatewayBridge ()
import Agent.CLI.Input ()
import Agent.CLI.Turn.Interrupt
    ( newInterruptState,
      noteFullscreenCtrlC,
      CtrlCDecision(ForceExit) )
import Agent.CLI.LearnedSkills ()
import Agent.CLI.LearnedSkills.Store ()
import Agent.CLI.Login ( runLoginManager )
import Agent.CLI.Lsp ()
import Agent.CLI.Turn.ManagedTurn ()
import Agent.CLI.Mcp.McpManager ()
import Agent.CLI.Mcp.McpStatus ()
import Agent.CLI.Model.ModelConfig ( loadModelCatalogAt )
import Agent.CLI.Model.Models
    ( defaultModelOptionFor,
      resolveConfiguredModel,
      ModelOption(modelTarget),
      ModelTarget(targetDialect, targetConnectionId, targetProvider,
                  targetModelId) )
import Agent.CLI.Options
    ( isOneShot,
      CliOptions(optMotionMode, optManagedTurnFile, optScreenMode,
                 optProvider, optModel, optWorktree, optEffort, optPrompt,
                 optPromptFile, optResume, optCwd, optCodeMode),
      ScreenMode(ScreenMinimal) )
import Agent.CLI.Turn.PendingInputs ()
import Agent.CLI.Turn.Plan ()
import Agent.CLI.Project
    ( loadUserSettings
    , ProjectSettings(settingsMouseCapture)
    )
import Agent.CLI.Prompt.Prompt ()
import Agent.CLI.Prompt.PromptHooks ()
import Agent.CLI.Provider.OpenAI ()
import Agent.CLI.Provider.Switch
    ( continueAutomaticFallback, reportProviderUnavailable )
import Agent.CLI.Provider.ProviderAvailability ()
import Agent.CLI.Provider.ProviderFallback ()
import Agent.CLI.Provider.ProviderTransition
    ( applyProviderTransition,
      ProviderTransition(ProviderTransition, transitionCause,
                         transitionUnavailableProviders, transitionPendingTurn,
                         transitionTarget, transitionAccountSelectionId,
                         transitionAccountId, transitionAutomaticBilling,
                         transitionSessionId),
      TransitionCause(AutomaticFallback, ManualTransition) )
import Agent.CLI.Session.Recap ()
import Agent.CLI.Render ( putTextLn )
import Agent.CLI.TUI.ReplMode ()
import Agent.CLI.Provider.Request ()
import Agent.CLI.Session.Resume (latestSessionForCwd)
import Agent.CLI.Runtime.HistorySource
    ( loadFullscreenHistoryPage, sessionUiPageSize )
import Agent.CLI.Runtime.Orchestration.Background ()
import Agent.CLI.Runtime.Orchestration.Concurrent ()
import Agent.CLI.Runtime.Orchestration.Initialized
    ( PreparedStartupAuth
    , prepareStartupAuth
    , runAgentInitialized
    )
import Agent.CLI.Runtime.Orchestration.Restart
    ( RestartCallbacks(..), runFullscreenRestartLoop )
import Agent.CLI.Runtime.Orchestration.Startup
    ( clearNativeProgress, setNativeProgress )
import Agent.CLI.Runtime.Orchestration.Types
    ( AgentProcessRuntime,
      AgentRunMode(runInBackground, runStdout, runStderr, runCwdHint) )
import Agent.CLI.Runtime.Persistence ()
import Agent.CLI.Runtime.Recap ()
import Agent.CLI.Runtime.Repl ()
import Agent.CLI.Runtime.Types
    ( DevResult(..), PreparedAgent(..), RunResult(..) )
import Agent.CLI.Util.Secret ()
import Agent.CLI.Session
    ( deleteSession,
      listSessions,
      loadActiveSession,
      loadRecentSessionTurns,
      sessionDirForId,
      sessionsRoot,
      SessionMeta(metaId, metaCwd) )
import Agent.CLI.Session.Attachments ()
import Agent.CLI.Session.Choices ( modelChoice )
import Agent.CLI.Session.History ()
import Agent.CLI.Session.Lifecycle ()
import Agent.CLI.Session.Runtime.Types
    ( StartupCancelled(..),
      StartupFailure(..),
      StartupRuntime(startupSessionState, StartupRuntime, startupToolEnv,
                     startupDatabaseStore, startupInterrupt, startupEscPaused,
                     startupUiRuntimeRef, startupFullscreen, startupTerminal,
                     startupStdout, startupStderr, startupBackground, startupUseColor,
                     startupStderrTty, startupStdinTty, startupStdoutTty,
                     startupFullscreenReused, startupAgentSnapshot, startupAgentSelect,
                     startupRestartEffort, startupStartedAt, startupTimings,
                     startupSyntaxLoadDuration, startupFinished) )
import Agent.CLI.Session.Selection ()
import Agent.CLI.Session.SessionAdmin ( managedPostgresConfigForHome )
import Agent.CLI.Session.SessionEnv ()
import Agent.CLI.Session.SessionLock
    ( acquireSessionLock, releaseSessionLock, SessionLock )
import Agent.CLI.Session.SessionState ( SessionState(..), newSessionState )
import Agent.CLI.Session.SessionTitle ()
import Agent.CLI.Prompt.Skills ()
import Agent.CLI.Startup.Auth
    ( recordStartupTiming, setStartupNotice )
import Agent.CLI.Startup.Context ()
import Agent.CLI.Util.Style
    ( glyphOk, glyphSession, roleError, roleMuted, setCliWindowTitle )
import Agent.CLI.Subagents.Runtime ()
import Agent.CLI.TUI.App
    ( FullscreenInputBuffer,
      FullscreenRuntime,
      clearFullscreenHistorySource,
      emitUiEvent,
      newFullscreenInputBuffer,
      newFullscreenRuntime,
      queuedFullscreenInputDisplays,
      runFullscreen,
      setFullscreenHistorySource,
      setFullscreenSessionActions )
import Agent.CLI.TUI.History
    ( HistoryDirection(HistoryNewer), HistoryGeneration(..) )
import Agent.CLI.TUI.SessionHistory ( sessionHistoryPage )
import Agent.CLI.Util.Terminal
    ( copyTerminalClipboard,
      detectTerminalCapabilities,
      reportTerminalCwd,
      resolveColor,
      TerminalCapabilities(terminalNativeProgress) )
import Agent.CLI.Tools ()
import Agent.CLI.Turn.Turn ()
import Agent.CLI.Provider.Usage ()
import Agent.CLI.Provider.WebFetch ()
import Agent.CLI.Git.Worktree
    ( createManagedWorktreeWithProgress, worktreeProgressMessage )
import Agent.Cancel ( requestCancel )
import Agent.Claude ()
import Agent.Dialect ()
import Agent.Error ()
import Agent.GrokBuild.Dialect.Goal ()
import Agent.GrokBuild.Dialect.Runtime ()
import Agent.GrokBuild.Dialect.Task ()
import Agent.GrokBuild.Dialect.Workflow ()
import Agent.Loop ()
import Agent.OpenAI.Compaction ()
import Agent.OpenAI.Usage ()
import Agent.OpenAI.WebSocketClient ()
import Agent.OpenRouter.LoopBackend ()
import Agent.OsPath ( toText )
import Agent.Provider ( Provider(OpenAIProvider) )
import Agent.ReasoningEffort ()
import Agent.Responses.GenericBackend ()
import Agent.Responses.GenericClient ()
import Agent.Responses.Types ()
import Agent.Skills ()
import Agent.Store.Postgres
    ( Store, closeStore, openStore, trustedPool )
import Agent.Store.Types ( renderStoreError )
import Agent.Subagents ()
import Agent.Subagents.TaskPath ()
import Agent.TUI.Model
    ( initialUiState,
      progressNotice,
      reduceUi,
      warningNotice,
      UiEvent(UiSystemMessage, UiSetRepository, UiSetNotice),
      UiState(uiQueuedInputs) )
import Agent.TUI.Motion ( nativeProgressAnimationEnabled )
import Agent.Tools.MultiAgents ()
import Agent.Tools.PlanMode ()
import Agent.Tools.Secret ()
import Agent.Tools.Types ( defaultToolEnv, ToolEnv(toolCancel) )
import Agent.XAI.LoopBackend ()
import Control.Applicative ( (<|>) )
import Control.Concurrent.Async ( Async, withAsync )
import Control.Concurrent.Chan ()
import Control.Concurrent.MVar
    ( newEmptyMVar, newMVar, readMVar, tryPutMVar )
import Control.Concurrent.STM ()
import Control.Exception ()
import Control.Exception.Safe
    ( displayException, finally, onException, throwIO, try, tryAny )
import Control.Monad ( when, forM_, void, unless )
import Data.Functor ()
import Data.IORef
    ( IORef, atomicModifyIORef', newIORef, readIORef, writeIORef )
import Data.List ()
import Data.Maybe ( isJust, isNothing )
import Data.Text ( Text )
import Data.Time.Clock ( getCurrentTime )
import System.Console.ANSI ()
import System.Console.ANSI.Codes ()
import System.Directory.OsPath
    ( doesDirectoryExist,
      getCurrentDirectory,
      getHomeDirectory,
      makeAbsolute,
      setCurrentDirectory )
import System.Environment ()
import System.Exit ( die )
import System.IO ( hIsTerminalDevice, stderr, stdin )
import System.OsPath ( decodeFS, takeDirectory, takeFileName )
import qualified Data.ByteString as BS ()
import qualified Agent.Responses.GenericClient as GenericResponses
    ()
import qualified Agent.MCP as MCP ()
import qualified Data.Map.Strict as Map ()
import qualified Agent.OpenAI.Auth as OpenAI ()
import qualified Agent.OpenRouter as OpenRouter ()
import qualified Agent.OpenRouter.Usage as OpenRouterUsage ()
import qualified Agent.Provider as Provider ()
import qualified Agent.CLI.Session.Lifecycle as SessionLifecycle ()
import qualified Agent.CLI.Session.Runner as SessionRunner ()
import qualified Data.Set as Set ( empty )
import qualified Data.Text as Text ( pack, unpack )
import qualified Data.Text.IO as Text ( hPutStr )
import qualified Agent.XAI.Options as XAI ()
import qualified Agent.XAI.Client as XAIClient ()
import qualified Agent.XAI.Request as XAIRequest ()
import qualified Agent.XAI.Usage as XAIUsage ()

runAgentWithRuntime
    :: AgentProcessRuntime
    -> AgentRunMode
    -> CliOptions
    -> IO DevResult
runAgentWithRuntime processRuntime runMode options = do
    fullscreenInputs <- newFullscreenInputBuffer
    sessionState <- newSessionState
    go fullscreenInputs sessionState options Nothing
  where
    go fullscreenInputs sessionState current transition =
        runAgent
            processRuntime
            runMode
            fullscreenInputs
            sessionState
            current
            transition >>= \case
            RunResumeSession sessionId ->
                newSessionState >>= \nextState ->
                    go fullscreenInputs nextState
                        current
                            { optProvider = Nothing
                            , optModel = Nothing
                            , optCwd = Nothing
                            , optWorktree = False
                            , optEffort = Nothing
                            , optPrompt = Nothing
                            , optPromptFile = Nothing
                            , optResume = Just (Just sessionId)
                            }
                        Nothing
            RunForkSession sessionId directive ->
                newSessionState >>= \nextState -> do
                    writeIORef nextState.sessionInitialPrompt directive
                    go fullscreenInputs nextState
                        current
                            { optProvider = Nothing
                            , optModel = Nothing
                            , optCwd = Nothing
                            , optWorktree = False
                            , optEffort = Nothing
                            , optPrompt = Nothing
                            , optPromptFile = Nothing
                            , optResume = Just (Just sessionId)
                            }
                        Nothing
            RunCheckoutBranch branch maybeSessionId -> do
                color <- resolveColor runMode.runStderr
                putTextLn runMode.runStderr
                    (roleMuted color
                        (glyphSession
                            <> "switching to branch '"
                            <> branch
                            <> "'…"))
                newSessionState >>= \nextState ->
                    case maybeSessionId of
                        Just sessionId ->
                            go fullscreenInputs nextState
                                current
                                    { optProvider = Nothing
                                    , optModel = Nothing
                                    , optCwd = Nothing
                                    , optWorktree = False
                                    , optEffort = Nothing
                                    , optPrompt = Nothing
                                    , optPromptFile = Nothing
                                    , optResume = Just (Just sessionId)
                                    }
                                Nothing
                        Nothing -> do
                            putTextLn runMode.runStderr
                                (roleMuted color
                                    ("no chat linked to branch '"
                                        <> branch
                                        <> "' — starting a fresh session"))
                            go fullscreenInputs nextState
                                current
                                    { optCwd = Nothing
                                    , optWorktree = False
                                    , optPrompt = Nothing
                                    , optPromptFile = Nothing
                                    , optManagedTurnFile = Nothing
                                    , optResume = Nothing
                                    }
                                Nothing
            RunDeleteSession sessionId cwd -> do
                home <- getHomeDirectory
                config <- managedPostgresConfigForHome home
                deletion <-
                    openStore config >>= \case
                        Left err ->
                            pure (Left (renderStoreError err))
                        Right store ->
                            deleteSession
                                (trustedPool store)
                                (sessionsRoot home)
                                sessionId
                                `finally` closeStore store
                color <- resolveColor runMode.runStderr
                case deletion of
                    Left err -> do
                        putTextLn runMode.runStderr
                            (roleError color
                                ("could not delete session "
                                    <> sessionId
                                    <> ": "
                                    <> err))
                        pure DevQuit
                    Right () -> do
                        putTextLn runMode.runStderr
                            (roleMuted color
                                (glyphOk
                                    <> "deleted session "
                                    <> sessionId))
                        newSessionState >>= \nextState ->
                            go fullscreenInputs nextState
                                current
                                    { optCwd = Just cwd
                                    , optWorktree = False
                                    , optPrompt = Nothing
                                    , optPromptFile = Nothing
                                    , optManagedTurnFile = Nothing
                                    , optResume = Nothing
                                    }
                                Nothing
            RunSwitchWorktree path provider model effort ->
                newSessionState >>= \nextState ->
                    go fullscreenInputs nextState
                        current
                            { optProvider = Just provider
                            , optModel = Just model
                            , optCwd = Just path
                            , optWorktree = False
                            , optEffort = Just effort
                            , optPrompt = Nothing
                            , optPromptFile = Nothing
                            , optResume = Nothing
                            }
                        Nothing
            RunSwitchProvider next ->
                go fullscreenInputs sessionState
                    (applyProviderTransition current next)
                    (Just next)
            RunRestart sessionId ->
                go fullscreenInputs sessionState
                    (restartSessionOptions current sessionId)
                    Nothing
            RunUpdateAndRestart sessionId -> do
                color <- resolveColor runMode.runStderr
                putTextLn runMode.runStderr
                    (roleMuted color
                        (glyphSession <> "installing the latest Haskell Agent…"))
                tryAny (updateAndResume sessionId) >>= \case
                    Left err -> do
                        putTextLn runMode.runStderr
                            (roleError color
                                ("update failed: "
                                    <> Text.pack (displayException err)))
                        go fullscreenInputs sessionState
                            (restartSessionOptions current sessionId)
                            Nothing
                    Right result -> pure result
            RunEnableCodeMode sessionId ->
                let nextOptions =
                        (restartSessionOptions current sessionId)
                            { optCodeMode = True }
                in go fullscreenInputs sessionState nextOptions Nothing
            RunProviderStartFailed apiError ->
                case transition of
                    Just failed
                        | failed.transitionCause == AutomaticFallback ->
                            continueAutomaticFallback
                                runMode.runCwdHint
                                runMode.runStderr
                                Nothing
                                failed
                                apiError >>= \case
                                Just next ->
                                    go fullscreenInputs sessionState
                                        (applyProviderTransition current next)
                                        (Just next)
                                Nothing
                                    | runMode.runInBackground -> do
                                        now <- getCurrentTime
                                        throwIO $
                                            StartupFailure
                                                (Text.unpack
                                                    (formatApiErrorAt
                                                        now
                                                        apiError))
                                    | otherwise -> do
                                        reportProviderUnavailable Nothing apiError
                                        pure DevQuit
                    _
                        | runMode.runInBackground -> do
                            now <- getCurrentTime
                            throwIO $
                                StartupFailure
                                    (Text.unpack
                                        (formatApiErrorAt now apiError))
                        | otherwise -> pure DevQuit
            RunQuit -> pure DevQuit
            RunReload sessionId -> pure (DevReload sessionId)

    -- Self-update used to reinstall the Nix profile from the upstream flake.
    -- This fork ships no Nix packaging, so automatic updates are unavailable;
    -- fail with upgrade guidance and let the caller restart the session with
    -- the current binary.
    updateAndResume _sessionId =
        ioError
            (userError
                "automatic updates are unavailable for this build; upgrade from your source checkout with `cabal install agent-cli:exe:monad-cli` and restart")

-- | Restore the process cwd after an action succeeds or throws. Cabal gives
-- GHCi relative source paths, so returning from an agent session in its cwd
-- would make the following @:reload@ lose local modules.
withRestoredCurrentDirectory :: IO a -> IO a
withRestoredCurrentDirectory action = do
    originalCwd <- getCurrentDirectory
    action `finally` setCurrentDirectory originalCwd

runAgent
    :: AgentProcessRuntime
    -> AgentRunMode
    -> FullscreenInputBuffer
    -> SessionState
    -> CliOptions
    -> Maybe ProviderTransition
    -> IO RunResult
runAgent
        processRuntime runMode fullscreenInputs sessionState options transition = do
    prepared <-
        prepareAgentIteration
            processRuntime
            runMode
            fullscreenInputs
            sessionState
            Nothing
            options
            transition
    let runPrepared = case prepared.preparedFullscreen of
            Nothing -> prepared.preparedRun
            Just runtime ->
                let chooseRecoveryModel nextOptions nextTransition = do
                        home <- getHomeDirectory
                        cwd <- case nextOptions.optCwd <|> runMode.runCwdHint of
                            Nothing -> getCurrentDirectory
                            Just path -> makeAbsolute path
                        loadModelCatalogAt home cwd >>= \case
                            Left err -> pure (Left err)
                            Right catalog -> do
                                color <- resolveColor runMode.runStderr
                                let currentTarget =
                                        ((.transitionTarget) <$> nextTransition)
                                            <|> ( (.modelTarget)
                                                    <$> (nextOptions.optModel
                                                        >>= resolveConfiguredModel
                                                            catalog)
                                                )
                                            <|> ( (.modelTarget)
                                                    <$> (nextOptions.optProvider
                                                        >>= defaultModelOptionFor
                                                            catalog)
                                                )
                                            <|> ( (.modelTarget)
                                                    <$> defaultModelOptionFor
                                                        catalog
                                                        OpenAIProvider
                                                )
                                case currentTarget of
                                    Nothing ->
                                        pure
                                            (Left
                                                "No configured models are available.")
                                    Just current ->
                                        modelChoice
                                            catalog
                                            (Just runtime)
                                            color
                                            current.targetConnectionId
                                            current.targetProvider
                                            current.targetModelId
                                            current.targetDialect >>= \case
                                                Nothing ->
                                                    pure (Right Nothing)
                                                Just choice ->
                                                    pure $ Right $ Just $
                                                        recoveryModelTransition
                                                            nextOptions
                                                            nextTransition
                                                            choice.modelTarget
                    recoveryModelTransition nextOptions nextTransition target =
                        case nextTransition of
                            Just active ->
                                active
                                    { transitionTarget = target
                                    , transitionAccountSelectionId = Nothing
                                    , transitionAccountId = Nothing
                                    , transitionUnavailableProviders = Set.empty
                                    , transitionCause = ManualTransition
                                    , transitionAutomaticBilling = Nothing
                                    }
                            Nothing ->
                                ProviderTransition
                                    { transitionTarget = target
                                    , transitionAccountSelectionId = Nothing
                                    , transitionAccountId = Nothing
                                    , transitionSessionId = case nextOptions.optResume of
                                Just (Just sessionId) -> Just sessionId
                                _ -> Nothing
                                    , transitionPendingTurn = Nothing
                                    , transitionUnavailableProviders = Set.empty
                                    , transitionCause = ManualTransition
                                    , transitionAutomaticBilling = Nothing
                                    }
                    callbacks = RestartCallbacks
                        { restartPrepare =
                            \nextOptions nextTransition ->
                                prepareAgentIteration
                                    processRuntime
                                    runMode
                                    fullscreenInputs
                                    sessionState
                                    (Just runtime)
                                    nextOptions
                                    nextTransition
                        , restartFallback =
                            \failed apiError ->
                                continueAutomaticFallback
                                    runMode.runCwdHint
                                    runMode.runStderr
                                    (Just runtime)
                                    failed
                                    apiError
                        , restartFormatFailure = \apiError -> do
                            now <- getCurrentTime
                            pure (formatApiErrorAt now apiError)
                        , restartOptions = restartSessionOptions
                        , restartApplyTransition = applyProviderTransition
                        , restartManageAccounts = do
                            color <- resolveColor stderr
                            runLoginManager color
                        , restartChooseModel = chooseRecoveryModel
                        }
                in
                runFullscreen runtime $
                    runFullscreenRestartLoop
                        callbacks
                        runtime
                        options
                        transition
                        prepared.preparedRun
    outcome <- try @_ @StartupCancelled (try @_ @StartupFailure runPrepared)
    result <- case outcome of
        Left StartupCancelled -> pure RunQuit
        Right startupOutcome ->
            either
                (\failure@(StartupFailure message) ->
                    if runMode.runInBackground
                        then throwIO failure
                        else die message)
                pure
                startupOutcome
    case (prepared.preparedFullscreen, result) of
        -- The retained screen has been restored before this persistent final
        -- diagnostic is printed.
        (Just _, RunProviderStartFailed apiError) -> do
            reportProviderUnavailable Nothing apiError
            pure RunQuit
        _ -> pure result

-- | Prepare one provider-specific backend. The outer Brick worker loops over
-- these prepared actions while reusing @activeFullscreen@, so Vty stays in the
-- alternate screen until the whole provider-restart chain finishes. Session
-- resumes still return to 'runAgentWithRestarts' and start a fresh UI.
prepareAgentIteration
    :: AgentProcessRuntime
    -> AgentRunMode
    -> FullscreenInputBuffer
    -> SessionState
    -> Maybe FullscreenRuntime
    -> CliOptions
    -> Maybe ProviderTransition
    -> IO PreparedAgent
prepareAgentIteration
        processRuntime runMode
        fullscreenInputs sessionState activeFullscreen options transition = do
    resumeLockRef <- newIORef (Nothing :: Maybe SessionLock)
    databaseStoreRef <- newIORef (Nothing :: Maybe Store)
    prepareAgentIterationTracked
        resumeLockRef
        databaseStoreRef
        processRuntime
        runMode
        fullscreenInputs
        sessionState
        activeFullscreen
        options
        transition
        `onException`
            releasePreparationResources resumeLockRef databaseStoreRef

prepareAgentIterationTracked
    :: IORef (Maybe SessionLock)
    -> IORef (Maybe Store)
    -> AgentProcessRuntime
    -> AgentRunMode
    -> FullscreenInputBuffer
    -> SessionState
    -> Maybe FullscreenRuntime
    -> CliOptions
    -> Maybe ProviderTransition
    -> IO PreparedAgent
prepareAgentIterationTracked
        resumeLockRef databaseStoreRef
        processRuntime runMode
        fullscreenInputs sessionState activeFullscreen options transition = do
    forM_ activeFullscreen resetFullscreenSessionActions
    let stdoutHandle = runMode.runStdout
        stderrHandle = runMode.runStderr
        background = runMode.runInBackground
        signalReady result =
            unless background (signalManagedSessionReady result)
        failPreparation message =
            releasePreparationResources resumeLockRef databaseStoreRef >>
                case activeFullscreen of
                    Nothing
                        | background -> throwIO (StartupFailure message)
                        | otherwise -> die message
                    Just _ -> throwIO (StartupFailure message)
    startedAt <- getCurrentTime
    startupTimingsRef <- newIORef []
    syntaxLoadDurationRef <- newIORef Nothing
    startupFinishedRef <- newIORef False
    home <- getHomeDirectory
    let root = sessionsRoot home
    databaseConfig <- managedPostgresConfigForHome home
    databaseStore <- openStore databaseConfig >>= \case
        Left err -> failPreparation (Text.unpack (renderStoreError err))
        Right store -> writeIORef databaseStoreRef (Just store) >> pure store
    let sessionPool = trustedPool databaseStore
    resumed <- case options.optResume of
        Nothing -> pure Nothing
        Just maybeSessionId -> do
            sessionId <- case maybeSessionId of
                Just sessionId -> pure sessionId
                Nothing -> do
                    -- Mirror the target-cwd computation below so bare
                    -- --resume resolves against the same directory the run
                    -- would have used without --resume.
                    targetCwd <- case options.optCwd of
                        Just requestedCwd -> makeAbsolute requestedCwd
                        Nothing ->
                            maybe
                                getCurrentDirectory
                                makeAbsolute
                                runMode.runCwdHint
                    (metas, warnings) <- listSessions sessionPool root
                    forM_ warnings \warning ->
                        putTextLn stderrHandle ("warning: " <> warning)
                    case latestSessionForCwd targetCwd metas of
                        Just latestId -> pure latestId
                        Nothing -> do
                            let err =
                                    "no persisted session for "
                                        <> toText targetCwd
                                        <> " — start one with `monad-cli`,"
                                        <> " or list sessions with"
                                        <> " `monad-cli sessions list`"
                            signalReady (Left err)
                            failPreparation (Text.unpack err)
            dir <- either
                (\err -> do
                    signalReady (Left err)
                    failPreparation (Text.unpack err))
                pure
                (sessionDirForId root sessionId)
            exists <- doesDirectoryExist dir
            when (not exists) do
                let err = "session not found: " <> sessionId
                signalReady (Left err)
                failPreparation (Text.unpack err)
            acquireSessionLock dir sessionId >>= \case
                Left err -> do
                    signalReady (Left err)
                    failPreparation (Text.unpack err)
                Right lock -> do
                    writeIORef resumeLockRef (Just lock)
                    loadActiveSession sessionPool root sessionId >>= \case
                        Left err -> do
                            signalReady (Left err)
                            failPreparation (Text.unpack err)
                        Right loaded -> do
                            signalReady (Right ())
                            pure (Just loaded)

    source <- case options.optCwd of
        Just requestedCwd -> makeAbsolute requestedCwd
        Nothing -> case resumed of
            Just (meta, _) -> makeAbsolute meta.metaCwd
            Nothing ->
                maybe getCurrentDirectory makeAbsolute runMode.runCwdHint
    let initialCwd = source
    uiRuntimeRef <- newIORef Nothing
    cancelToolRef <- newIORef (pure ())
    interrupt <- newInterruptState \msg -> do
        readIORef uiRuntimeRef >>= \case
            Just runtime ->
                emitUiEvent runtime
                    (UiSetNotice (Just (warningNotice msg)))
            Nothing -> do
                -- Drop an in-place "Thinking…" status so the hint is its own line.
                Text.hPutStr stderrHandle "\r\ESC[K"
                clearNativeProgress stderrHandle
                color <- resolveColor stderrHandle
                putTextLn stderrHandle (roleMuted color msg)
    -- Shared with Esc cancel and plan prompts so arrow-key pickers own stdin.
    escPaused <- newIORef False
    stderrTty <-
        if background then pure False else hIsTerminalDevice stderrHandle
    stdinTty <- if background then pure False else hIsTerminalDevice stdin
    stdoutTty <-
        if background then pure False else hIsTerminalDevice stdoutHandle
    terminal <- detectTerminalCapabilities stdoutHandle
    useColor <- if background then pure False else resolveColor stdoutHandle
    agentSnapshotRef <- newIORef (pure (AgentRoot, []))
    agentSelectRef <- newIORef (\_ -> pure ())
    restartEffortActionRef <- newIORef (\_ -> pure ())
    queuedInputDisplays <- queuedFullscreenInputDisplays fullscreenInputs
    let fullscreenEnabled =
            stdinTty
                && stdoutTty
                && not (isOneShot options)
                && options.optScreenMode /= ScreenMinimal
        initialFullscreenState =
            (reduceUi
                (UiSetNotice
                    (Just (progressNotice
                        (if options.optWorktree
                            then "Creating worktree…"
                            else "Loading project…"))))
                (reduceUi
                    (UiSetRepository
                        ""
                        (toText (takeFileName (takeDirectory initialCwd))
                            <> "/"
                            <> toText (takeFileName initialCwd))
                        (toText initialCwd))
                    initialUiState))
                        { uiQueuedInputs = queuedInputDisplays }
    firstFrameReady <-
        if isJust activeFullscreen || not fullscreenEnabled
            then newMVar ()
            else newEmptyMVar
    fullscreen <- case activeFullscreen of
        Just runtime -> pure (Just runtime)
        Nothing
            | fullscreenEnabled -> do
                userSettings <- loadUserSettings home
                Just <$> newFullscreenRuntime
                    fullscreenInputs
                    (readIORef cancelToolRef >>= id)
                    (\level ->
                        readIORef restartEffortActionRef >>= ($ level))
                    (noteFullscreenCtrlC interrupt)
                    (copyTerminalClipboard terminal stdoutHandle)
                    (setCliWindowTitle stdoutTty stdoutHandle)
                    (\active ->
                        when
                            (terminal.terminalNativeProgress
                                && nativeProgressAnimationEnabled
                                    options.optMotionMode) $
                            setNativeProgress stderrHandle active)
                    (readIORef agentSnapshotRef >>= id)
                    (\target -> readIORef agentSelectRef >>= ($ target))
                    (do
                        recordStartupTiming
                            startedAt startupTimingsRef "first frame"
                        void (tryPutMVar firstFrameReady ()))
                    (writeIORef syntaxLoadDurationRef . Just)
                    options.optMotionMode
                    useColor
                    initialFullscreenState
                    userSettings.settingsMouseCapture
            | otherwise -> pure Nothing
    forM_ fullscreen \runtime ->
        case resumed of
            Nothing ->
                clearFullscreenHistorySource runtime
            Just (meta, _) ->
                loadRecentSessionTurns
                    sessionPool
                    root
                    meta.metaId
                    sessionUiPageSize >>= \case
                        Left err ->
                            failPreparation (Text.unpack err)
                        Right page ->
                            setFullscreenHistorySource
                                runtime
                                meta.metaId
                                (loadFullscreenHistoryPage
                                    sessionPool root meta.metaId)
                                (sessionHistoryPage
                                    (HistoryGeneration 0)
                                    HistoryNewer
                                    page)
    writeIORef uiRuntimeRef fullscreen
    resumeLock <- readIORef resumeLockRef
    let runAction
            :: Maybe (Async PreparedStartupAuth)
            -> IO RunResult
        runAction preparedAuth =
            do
                cwd <- case resumed of
                    Just _ -> pure initialCwd
                    Nothing
                        | options.optWorktree -> do
                            readMVar firstFrameReady
                            let reportWorktreeProgress progress = do
                                    let message =
                                            worktreeProgressMessage progress
                                    case fullscreen of
                                        Nothing ->
                                            putTextLn stderrHandle message
                                        Just runtime ->
                                            emitUiEvent runtime
                                                (UiSetNotice
                                                    (Just
                                                        (progressNotice
                                                            message)))
                            createManagedWorktreeWithProgress
                                reportWorktreeProgress
                                home
                                source
                                >>= either
                                    (\err -> do
                                        mapM_ releaseSessionLock resumeLock
                                        case fullscreen of
                                            Nothing -> die (Text.unpack err)
                                            Just _ ->
                                                throwIO
                                                    (StartupFailure
                                                        (Text.unpack err)))
                                    (\path -> do
                                        color <- resolveColor stderrHandle
                                        case fullscreen of
                                            Nothing ->
                                                putTextLn stderrHandle
                                                    (roleMuted color
                                                        (glyphSession
                                                            <> "worktree: "
                                                            <> toText path))
                                            Just runtime ->
                                                emitUiEvent runtime
                                                    (UiSystemMessage
                                                        (glyphSession
                                                            <> "worktree: "
                                                            <> toText path))
                                        setStartupNotice fullscreen
                                            "Loading project…"
                                        pure path)
                        | otherwise -> pure initialCwd
                unless background (setCurrentDirectory cwd)
                terminalCwd <- decodeFS cwd
                reportTerminalCwd terminal stdoutHandle terminalCwd
                toolEnv <- defaultToolEnv cwd
                writeIORef cancelToolRef (requestCancel toolEnv.toolCancel)
                forM_ fullscreen \runtime ->
                    setFullscreenSessionActions
                        runtime
                        Nothing
                        (requestCancel toolEnv.toolCancel)
                        (const (pure (Right ())))
                        (const (pure ()))
                        (pure ())
                        (\level ->
                            readIORef restartEffortActionRef >>= ($ level))
                        (noteFullscreenCtrlC interrupt)
                        (readIORef agentSnapshotRef >>= id)
                        (\target -> readIORef agentSelectRef >>= ($ target))
                let startup = StartupRuntime
                        { startupToolEnv = toolEnv
                        , startupDatabaseStore = databaseStore
                        , startupInterrupt = interrupt
                        , startupEscPaused = escPaused
                        , startupUiRuntimeRef = uiRuntimeRef
                        , startupFullscreen = fullscreen
                        , startupTerminal = terminal
                        , startupStdout = stdoutHandle
                        , startupStderr = stderrHandle
                        , startupBackground = background
                        , startupUseColor = useColor
                        , startupStderrTty = stderrTty
                        , startupStdinTty = stdinTty
                        , startupStdoutTty = stdoutTty
                        , startupFullscreenReused = isJust activeFullscreen
                        , startupAgentSnapshot = agentSnapshotRef
                        , startupAgentSelect = agentSelectRef
                        , startupRestartEffort = restartEffortActionRef
                        , startupStartedAt = startedAt
                        , startupTimings = startupTimingsRef
                        , startupSyntaxLoadDuration = syntaxLoadDurationRef
                        , startupFinished = startupFinishedRef
                        , startupSessionState = sessionState
                        }
                runAgentInitialized
                    (runAgentWithRuntime processRuntime)
                    processRuntime
                    options
                    transition
                    home
                    root
                    resumed
                    resumeLock
                    cwd
                    startup
                    preparedAuth
        action
            | options.optWorktree
            , isNothing resumed
            , isNothing transition =
                let prepareAccountUsage =
                        isNothing fullscreen
                            || isJust options.optProvider
                            || isJust options.optModel
                in withAsync
                    (prepareStartupAuth
                        prepareAccountUsage
                        options.optProvider) $
                    runAction . Just
            | otherwise = runAction Nothing
        cleanup = do
            writeIORef uiRuntimeRef Nothing
            writeIORef cancelToolRef (pure ())
            forM_ fullscreen resetFullscreenSessionActions
            closeStore databaseStore
    pure PreparedAgent
        { preparedFullscreen = fullscreen
        , preparedRun = action `finally` cleanup
        }

releasePreparationResources
    :: IORef (Maybe SessionLock)
    -> IORef (Maybe Store)
    -> IO ()
releasePreparationResources resumeLockRef databaseStoreRef = do
    atomicModifyIORef' resumeLockRef (\current -> (Nothing, current))
        >>= mapM_ releaseSessionLock
    atomicModifyIORef' databaseStoreRef (\current -> (Nothing, current))
        >>= mapM_ closeStore

resetFullscreenSessionActions :: FullscreenRuntime -> IO ()
resetFullscreenSessionActions runtime =
    setFullscreenSessionActions
        runtime
        Nothing
        (pure ())
        (const (pure (Right ())))
        (const (pure ()))
        (pure ())
        (const (pure ()))
        -- No session-local interrupt state is alive between providers. A
        -- transition must remain escapable even if auth probing blocks.
        (pure ForceExit)
        (pure (AgentRoot, []))
        (const (pure ()))


restartSessionOptions :: CliOptions -> Text -> CliOptions
restartSessionOptions options sessionId =
    options
        { optProvider = Nothing
        , optModel = Nothing
        , optCwd = Nothing
        , optWorktree = False
        , optEffort = Nothing
        , optPrompt = Nothing
        , optPromptFile = Nothing
        , optManagedTurnFile = Nothing
        , optResume = Just (Just sessionId)
        }
