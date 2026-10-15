-- | DeepSeek-specific normalization of HTTP and streaming failures.
module Agent.DeepSeek.Error
    ( classifyFailure
    , classifyStreamError
    ) where

import Agent.Error (ApiError(..), ErrorType(..), errorTypeFromText)
import Agent.Responses.Error (classifyHttpFailure, decodeOpenAIError, mkOpenAIError)
import Agent.Responses.Types (ResponseStreamError(..))
import Data.Text (Text)
import qualified Data.Text as Text

-- | Classify a non-success response from DeepSeek.
--
-- DeepSeek returns plain OpenAI-shaped error envelopes, so the decoded error
-- type is trusted except that auth, billing, and rate-limit statuses are
-- pinned to their canonical error types.
classifyFailure :: Int -> Maybe Int -> Text -> ApiError
classifyFailure status retryAfterHeader body = case decodeOpenAIError body of
    Right (ProviderError _ message retryAfter)
        | status == 401 || status == 403 ->
            ProviderError AuthenticationError message (retryAfter `orElse` retryAfterHeader)
        | status == 402 ->
            ProviderError BillingError message (retryAfter `orElse` retryAfterHeader)
        | status == 429 ->
            ProviderError RateLimitError message (retryAfter `orElse` retryAfterHeader)
    Right other -> other
    Left _ -> case status of
        401 -> ProviderError AuthenticationError (preview body) retryAfterHeader
        403 -> ProviderError AuthenticationError (preview body) retryAfterHeader
        402 -> ProviderError BillingError (preview body) retryAfterHeader
        429 -> ProviderError RateLimitError (preview body) retryAfterHeader
        _ -> classifyHttpFailure status body

-- | Convert a typed Responses streaming error into the shared error channel.
classifyStreamError :: ResponseStreamError -> ApiError
classifyStreamError streamError
    | Just errorType <- streamError.errorType =
        mkOpenAIError
            (errorTypeFromText errorType)
            streamError.message
            streamError.code
            streamError.retryAfter
    | otherwise = ConnectionError
        ("DeepSeek stream error: " <> streamError.message)

preview :: Text -> Text
preview = Text.take 500

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just value) _ = Just value
orElse Nothing fallback = fallback
