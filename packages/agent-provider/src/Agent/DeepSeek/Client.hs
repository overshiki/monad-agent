-- | HTTP client for the DeepSeek Responses endpoint.
module Agent.DeepSeek.Client
    ( StreamEventCallback
    , createResponse
    , createResponseWith
    , createResponseWithEvents
    , createResponseWithEventsPolicy
    , retryTransientDeepSeekResultWithPolicy
    , deepSeekProviderConfig
    ) where

import Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    , isInlineRetryableProviderError
    )
import Agent.Responses.GenericClient
    ( ProviderClientConfig(..)
    , createResponseWithProviderPolicy
    , retryTransientResultWithPolicy
    )
import qualified Agent.Responses.HttpSSE as HttpSSE
import Agent.Responses.Types
import Agent.Provider (Credential(..), Provider(..))
import Agent.DeepSeek.Error (classifyFailure)
import Agent.DeepSeek.Options
import Agent.DeepSeek.Request (buildRequest)
import Agent.DeepSeek.Stream (streamAssemblyConfig)
import Control.Retry
    ( RetryPolicyM
    , exponentialBackoff
    , limitRetries
    )
import qualified Data.Text.Encoding as Text
import Network.HTTP.Simple hiding (Response)

type StreamEventCallback = HttpSSE.StreamEventCallback

-- | Send one request using environment-derived client options.
createResponse :: Credential -> ResponseCreateParams -> IO (Either ApiError Response)
createResponse credential request = do
    options <- clientOptionsFromEnv
    createResponseWith options credential request

-- | Send one request using explicit client options.
createResponseWith
    :: ClientOptions
    -> Credential
    -> ResponseCreateParams
    -> IO (Either ApiError Response)
createResponseWith options credential request =
    createResponseWithEvents options credential request (const (pure ()))

-- | Send one request and deliver decoded typed Responses events incrementally
-- in wire order before returning the assembled terminal response.
createResponseWithEvents
    :: ClientOptions
    -> Credential
    -> ResponseCreateParams
    -> StreamEventCallback
    -> IO (Either ApiError Response)
createResponseWithEvents =
    createResponseWithEventsPolicy transientResultPolicy

-- | Like 'createResponseWithEvents', with an injectable retry policy for
-- deterministic tests. Transient failures retry only before the first stream
-- callback, so callers never observe replayed output.
createResponseWithEventsPolicy
    :: RetryPolicyM IO
    -> ClientOptions
    -> Credential
    -> ResponseCreateParams
    -> StreamEventCallback
    -> IO (Either ApiError Response)
createResponseWithEventsPolicy policy options credential request onEvent
    | credential.provider /= DeepSeekProvider = pure $ Left $ ProviderError ApiErrorType
        "agent-deepseek requires a DeepSeek credential"
        Nothing
    | otherwise =
        createResponseWithProviderPolicy
            policy
            (deepSeekProviderConfig options credential)
            request
            (Just onEvent)

-- | Retry transient failures while replay is safe. The callback marker is
-- written before user code runs, so callback exceptions are never retried.
retryTransientDeepSeekResultWithPolicy
    :: RetryPolicyM IO
    -> ((event -> IO ()) -> IO (Either ApiError value))
    -> (event -> IO ())
    -> IO (Either ApiError value)
retryTransientDeepSeekResultWithPolicy = retryTransientResultWithPolicy

transientResultPolicy :: RetryPolicyM IO
transientResultPolicy = exponentialBackoff 1_000_000 <> limitRetries 3

deepSeekProviderConfig
    :: ClientOptions
    -> Credential
    -> ProviderClientConfig
deepSeekProviderConfig options credential = ProviderClientConfig
    { providerExceptionPrefix = "DeepSeek request failed"
    , providerBaseUrl = options.baseUrl
    , providerRequestTimeoutSeconds = options.requestTimeoutSeconds
    , providerBuildRequest = buildRequest options
    , providerConfigureRequest =
        setRequestHeader "Authorization"
            ["Bearer " <> Text.encodeUtf8 credential.accessToken]
            . setRequestHeader "User-Agent" ["haskell-agent"]
    , providerClassifyFailure = classifyFailure
    , providerAssemblyConfig = streamAssemblyConfig
    , providerRetryableFailure = isInlineRetryableProviderError
    }
