-- | Map the provider-neutral loop onto the Kimi Responses transport.
--
-- Kimi does not store transcripts ('store = false', no
-- @previous_response_id@). This backend keeps a local item list so tool
-- follow-ups can resend the conversation the loop only supplies as
-- 'CompletedTool' items. The loop threads that history explicitly so a
-- resumed session can seed it and the CLI can persist it.
module Agent.Kimi.LoopBackend
    ( kimiBackend
    , kimiBackendWith
    ) where

import Agent.Error (ApiError)
import Agent.Loop (Backend)
import Agent.Responses.LoopBackend
    ( statelessResponsesBackend
    , tokenProviderStatelessResponsesBackend
    )
import Agent.Responses.Types
import Agent.Provider (TokenProvider)
import Agent.Kimi.Client (createResponseWithEvents)
import Agent.Kimi.Options (ClientOptions)

-- | Close over Kimi options, a token provider, and the request fields
-- the loop does not own (model, instructions, tools, reasoning). Credentials
-- stay cached; an auth rejection triggers one provider reload and retry.
-- Params are re-read each turn so the REPL can change reasoning effort
-- without dropping the local transcript.
kimiBackend
    :: ClientOptions
    -> TokenProvider
    -> IO ResponseCreateParams
    -> Backend
kimiBackend options provider =
    tokenProviderStatelessResponsesBackend provider
        (createResponseWithEvents options)

-- | Same mapping as 'kimiBackend', with an injectable transport for tests
-- and downstream integrations.
kimiBackendWith
    :: (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
kimiBackendWith = statelessResponsesBackend
