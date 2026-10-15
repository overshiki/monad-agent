-- | HTTP client for the Kimi Code Anthropic Messages endpoint.
module Agent.KimiCode.Client
    ( StreamEventCallback
    , createResponse
    , createResponseWith
    , createResponseWithEvents
    , createResponseWithEventsPolicy
    , retryTransientKimiCodeResultWithPolicy
    ) where

import Agent.Error
    ( ApiError(..)
    , ErrorType(..)
    , isInlineRetryableProviderError
    )
import Agent.Http.Header (parseRetryAfterSeconds)
import Agent.Http.Url (trimTrailingSlash)
import Agent.KimiCode.Error (classifyFailure)
import Agent.KimiCode.Options (ClientOptions(..), clientOptionsFromEnv)
import Agent.KimiCode.Request (buildRequestBody)
import Agent.KimiCode.Stream
    ( Assembler
    , StreamStep(..)
    , emptyAssembler
    , parseSseBlock
    , stepStream
    )
import Agent.Loop (LoopEvent(..))
import Agent.Provider (Credential(..), Provider(..))
import Agent.Responses.Client (retryStreamingResultWithPolicy)
import Agent.Responses.GenericClient (retryTransientResultWithPolicy)
import Agent.Responses.Types
import Control.Exception.Safe (Exception, throwIO, tryAny)
import Control.Retry (RetryPolicyM, exponentialBackoff, limitRetries)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text (lenientDecode)
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.TLS as HttpTls
import Network.HTTP.Simple hiding (Response)
import qualified System.Timeout as Timeout

type StreamEventCallback = LoopEvent -> IO ()

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

-- | Send one request and deliver decoded loop events incrementally in wire
-- order before returning the assembled terminal response.
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
    | credential.provider /= KimiCodeProvider = pure $ Left $ ProviderError ApiErrorType
        "agent-kimi-code requires a Kimi Code credential"
        Nothing
    | otherwise =
        retryStreamingResultWithPolicy
            policy
            isInlineRetryableProviderError
            (\emit -> performRequest options credential request emit)
            (Just onEvent)

-- | Retry transient failures while replay is safe. The callback marker is
-- written before user code runs, so callback exceptions are never retried.
retryTransientKimiCodeResultWithPolicy
    :: RetryPolicyM IO
    -> ((event -> IO ()) -> IO (Either ApiError value))
    -> (event -> IO ())
    -> IO (Either ApiError value)
retryTransientKimiCodeResultWithPolicy = retryTransientResultWithPolicy

transientResultPolicy :: RetryPolicyM IO
transientResultPolicy = exponentialBackoff 1_000_000 <> limitRetries 3

-- | Raised when a streaming body read stalls past the configured timeout.
newtype StreamStalled = StreamStalled Int

instance Show StreamStalled where
    show (StreamStalled seconds) =
        "streaming response stalled: no data received for "
            <> show seconds <> "s"

instance Exception StreamStalled

performRequest
    :: ClientOptions
    -> Credential
    -> ResponseCreateParams
    -> StreamEventCallback
    -> IO (Either ApiError Response)
performRequest options credential request emit =
    tryAny (run options credential request emit) >>= \case
        Left exception -> pure $ Left $ ConnectionError
            ("Kimi Code request failed: " <> Text.pack (show exception))
        Right result -> pure result

run
    :: ClientOptions
    -> Credential
    -> ResponseCreateParams
    -> StreamEventCallback
    -> IO (Either ApiError Response)
run options credential request emit = do
    baseRequest <- parseRequest
        ("POST " <> trimTrailingSlash options.baseUrl <> "/messages")
    manager <- HttpTls.getGlobalManager
    HttpClient.withResponse
        ( setRequestBodyLBS (buildRequestBody options request)
        $ setRequestHeader "Authorization"
            ["Bearer " <> Text.encodeUtf8 credential.accessToken]
        $ setRequestHeader "User-Agent" ["haskell-agent"]
        $ setRequestHeader "Content-Type" ["application/json"]
        $ setRequestHeader "Accept" ["text/event-stream"]
        $ withTimeout baseRequest
        )
        manager
        (handleResponse options request emit)
  where
    withTimeout req = req
        { HttpClient.responseTimeout =
            HttpClient.responseTimeoutMicro (options.requestTimeoutSeconds * 1_000_000)
        }

handleResponse
    :: ClientOptions
    -> ResponseCreateParams
    -> StreamEventCallback
    -> HttpClient.Response HttpClient.BodyReader
    -> IO (Either ApiError Response)
handleResponse options request emit response = do
    let status = getResponseStatusCode response
    if status >= 200 && status < 300
        then consumeSse options request emit (HttpClient.responseBody response)
        else do
            (body, truncated) <-
                consumeBodyBounded (HttpClient.responseBody response)
            let decoded = Text.decodeUtf8With Text.lenientDecode (LBS.toStrict body)
                classified =
                    classifyFailure status
                        (parseRetryAfterSeconds
                            (getResponseHeader "Retry-After" response))
                        decoded
            pure $ Left $
                if truncated then appendBodyTruncatedMessage classified else classified

consumeSse
    :: ClientOptions
    -> ResponseCreateParams
    -> StreamEventCallback
    -> HttpClient.BodyReader
    -> IO (Either ApiError Response)
consumeSse options request emit body = go emptyAssembler ""
  where
    modelHint = request.model
    readChunkWithin = timeoutReader options

    go assembler leftover = do
        chunk <- readChunkWithin body
        if BS.null chunk
            then finalize assembler leftover
            else stepText assembler (leftover <> Text.decodeUtf8With Text.lenientDecode chunk)

    stepText assembler text =
        case Text.breakOn "\n\n" text of
            (block, rest)
                | Text.null rest -> go assembler block
                | otherwise -> do
                    outcome <- applyBlock assembler block
                    case outcome of
                        StreamContinue next events -> do
                            mapM_ emit events
                            stepText next (Text.drop 2 rest)
                        StreamFinished result events -> do
                            mapM_ emit events
                            pure result

    applyBlock assembler block =
        case parseSseBlock block of
            Nothing -> pure (StreamContinue assembler [])
            Just event -> pure (stepStream modelHint assembler event)

    finalize assembler leftover =
        case parseSseBlock leftover of
            Just event -> case stepStream modelHint assembler event of
                StreamFinished result events ->
                    mapM_ emit events >> pure result
                StreamContinue next events ->
                    mapM_ emit events
                        >> pure (Left (ConnectionError missingTerminalMessage))
            Nothing -> pure (Left (ConnectionError missingTerminalMessage))

    missingTerminalMessage =
        "Kimi Code stream ended without a message_stop event"

timeoutReader :: ClientOptions -> HttpClient.BodyReader -> IO BS.ByteString
timeoutReader options body
    | options.requestTimeoutSeconds <= 0 = HttpClient.brRead body
    | otherwise =
        Timeout.timeout (options.requestTimeoutSeconds * 1_000_000)
            (HttpClient.brRead body) >>= \case
                Just chunk -> pure chunk
                Nothing -> throwIO (StreamStalled options.requestTimeoutSeconds)

consumeBodyBounded
    :: HttpClient.BodyReader
    -> IO (LBS.ByteString, Bool)
consumeBodyBounded body = readChunks [] 0
  where
    readChunks reversedChunks total = do
        chunk <- HttpClient.brRead body
        if BS.null chunk
            then pure (LBS.fromChunks (reverse reversedChunks), False)
            else
                let remaining = maxErrorBodyBytes - total
                in if BS.length chunk > remaining
                    then pure
                        ( LBS.fromChunks
                            (reverse (BS.take remaining chunk : reversedChunks))
                        , True
                        )
                    else if BS.length chunk == remaining
                        then do
                            next <- HttpClient.brRead body
                            pure
                                ( LBS.fromChunks
                                    (reverse (chunk : reversedChunks))
                                , not (BS.null next)
                                )
                        else readChunks
                            (chunk : reversedChunks)
                            (total + BS.length chunk)

maxErrorBodyBytes :: Int
maxErrorBodyBytes = 1024 * 1024

appendBodyTruncatedMessage :: ApiError -> ApiError
appendBodyTruncatedMessage apiError =
    let suffix =
            "\n[response body truncated after "
                <> Text.pack (show maxErrorBodyBytes)
                <> " bytes]"
    in case apiError of
        HttpError status message -> HttpError status (message <> suffix)
        JsonDecodeError message body ->
            JsonDecodeError (message <> suffix) body
        ProviderError errorType message retryAfter ->
            ProviderError errorType (message <> suffix) retryAfter
        ConnectionError message -> ConnectionError (message <> suffix)
        CredentialError message -> CredentialError (message <> suffix)
        CredentialsExhausted{} -> apiError
