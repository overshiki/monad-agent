-- | Map the provider-neutral loop onto the Kimi Code Anthropic Messages
-- transport.
--
-- Kimi Code is stateless: the complete local transcript is replayed on every
-- request as Anthropic @messages@. This backend keeps a local item list so
-- tool follow-ups can resend the conversation the loop only supplies as
-- 'CompletedTool' items. The loop threads that history explicitly so a
-- resumed session can seed it and the CLI can persist it.
module Agent.KimiCode.LoopBackend
    ( kimiCodeBackend
    , kimiCodeBackendWith
    ) where

import Agent.Error (ApiError)
import Agent.KimiCode.Client (createResponseWithEvents)
import Agent.KimiCode.Options (ClientOptions)
import Agent.Loop
    ( Backend(..)
    , BackendResult(..)
    , BackendSnapshot(..)
    , LoopEvent
    , advanceBackendSnapshot
    )
import Agent.Provider (TokenProvider, runWithTokenProvider)
import Agent.Responses.LoopBackend
    ( responseToTurnOutput
    , turnInputsToItems
    )
import Agent.Responses.Types

-- | Close over Kimi Code options, a token provider, and the request fields
-- the loop does not own (model, instructions, tools, reasoning). Credentials
-- stay cached; an auth rejection triggers one provider reload and retry.
-- Params are re-read each turn so the REPL can change reasoning effort
-- without dropping the local transcript.
kimiCodeBackend
    :: ClientOptions
    -> TokenProvider
    -> IO ResponseCreateParams
    -> Backend
kimiCodeBackend options provider getParams =
    Backend \snapshot _legacyPreviousResponseId inputs onEvent -> do
        baseParams <- getParams
        let newItems = turnInputsToItems inputs
            requestItems = snapshot.backendItems <> newItems
            request = withInput requestItems baseParams
        result <-
            runWithTokenProvider provider \credential ->
                createResponseWithEvents options credential request onEvent
        case result of
            Left err -> pure (Left err)
            Right response ->
                pure $ Right BackendResult
                    { backendOutput = responseToTurnOutput response
                    , backendState =
                        advanceBackendSnapshot snapshot
                            (requestItems <> response.output)
                            Nothing
                    }

-- | Same mapping as 'kimiCodeBackend', with an injectable transport for tests
-- and downstream integrations.
kimiCodeBackendWith
    :: (ResponseCreateParams
        -> (LoopEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
kimiCodeBackendWith send getParams =
    Backend \snapshot _legacyPreviousResponseId inputs onEvent -> do
        baseParams <- getParams
        let newItems = turnInputsToItems inputs
            requestItems = snapshot.backendItems <> newItems
            request = withInput requestItems baseParams
        result <- send request onEvent
        case result of
            Left err -> pure (Left err)
            Right response ->
                pure $ Right BackendResult
                    { backendOutput = responseToTurnOutput response
                    , backendState =
                        advanceBackendSnapshot snapshot
                            (requestItems <> response.output)
                            Nothing
                    }

-- | Rebuild request parameters with a fresh input item list. 'input' is also
-- a field on other records, so a record update is ambiguous; rebuild from the
-- constructor instead.
withInput :: [ResponseItem] -> ResponseCreateParams -> ResponseCreateParams
withInput items params =
    case params of
        ResponseCreateParams{..} ->
            ResponseCreateParams { input = Just (ResponseInputItems items), .. }
