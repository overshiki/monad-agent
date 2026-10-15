-- | Map the provider-neutral loop onto the DeepSeek Responses transport.
--
-- DeepSeek does not store transcripts ('store = false', no
-- @previous_response_id@). This backend keeps a local item list so tool
-- follow-ups can resend the conversation the loop only supplies as
-- 'CompletedTool' items. The loop threads that history explicitly so a
-- resumed session can seed it and the CLI can persist it.
module Agent.DeepSeek.LoopBackend
    ( deepSeekBackend
    , deepSeekBackendWith
    ) where

import Agent.Error (ApiError)
import Agent.Loop (Backend)
import Agent.Responses.LoopBackend
    ( statelessResponsesBackend
    , tokenProviderStatelessResponsesBackend
    )
import Agent.Responses.Types
import Agent.Provider (TokenProvider)
import Agent.DeepSeek.Client (createResponseWithEvents)
import Agent.DeepSeek.Options (ClientOptions)

-- | Close over DeepSeek options, a token provider, and the request fields
-- the loop does not own (model, instructions, tools, reasoning). Credentials
-- stay cached; an auth rejection triggers one provider reload and retry.
-- Params are re-read each turn so the REPL can change reasoning effort
-- without dropping the local transcript.
deepSeekBackend
    :: ClientOptions
    -> TokenProvider
    -> IO ResponseCreateParams
    -> Backend
deepSeekBackend options provider =
    tokenProviderStatelessResponsesBackend provider
        (createResponseWithEvents options)

-- | Same mapping as 'deepSeekBackend', with an injectable transport for tests
-- and downstream integrations.
deepSeekBackendWith
    :: (ResponseCreateParams
        -> (ResponseStreamEvent -> IO ())
        -> IO (Either ApiError Response))
    -> IO ResponseCreateParams
    -> Backend
deepSeekBackendWith = statelessResponsesBackend
