-- | Typed decoding and terminal-response assembly for DeepSeek Responses SSE.
module Agent.DeepSeek.Stream
    ( SseDecoder
    , newSseDecoder
    , feedSseDecoder
    , finishSseDecoder
    , parseSseEvents
    , streamAssemblyConfig
    , buildResponse
    ) where

import Agent.Error (ApiError(..))
import Agent.Responses.SSE
    ( SseDecoder
    , feedSseDecoder
    , finishSseDecoder
    , newSseDecoder
    , parseSseEvents
    )
import Agent.Responses.StreamAssembly
    ( StreamAssemblyConfig(..)
    , buildStreamResponse
    , failedStreamResponseMessage
    )
import Agent.Responses.Types
import Agent.DeepSeek.Error (classifyStreamError)

-- | Merge streamed output-item events into the terminal completed response.
buildResponse :: [ResponseStreamEvent] -> Either ApiError Response
buildResponse = buildStreamResponse streamAssemblyConfig

streamAssemblyConfig :: StreamAssemblyConfig
streamAssemblyConfig = StreamAssemblyConfig
    { missingCompletionMessage =
        "No terminal response event found in DeepSeek SSE stream"
    , classifyStreamError
    , classifyFailedResponse =
        ConnectionError . failedStreamResponseMessage
    , incompleteAsFailure = False
    }
