-- | Kimi Code-specific normalization of HTTP and streaming failures.
module Agent.KimiCode.Error
    ( classifyFailure
    , classifyStreamError
    ) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Responses.Error (classifyHttpFailure)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- | Classify a non-success response from Kimi Code.
--
-- Kimi Code returns Anthropic-shaped error envelopes
-- (@{"type":"error","error":{"type":...,"message":...}}@). The decoded error
-- type is trusted except that auth, billing, and rate-limit statuses are
-- pinned to their canonical error types.
classifyFailure :: Int -> Maybe Int -> Text -> ApiError
classifyFailure status retryAfterHeader body = case decodeError body of
    Just (errorType, message)
        | status == 401 || status == 403 ->
            ProviderError AuthenticationError message retryAfterHeader
        | status == 402 || errorType == "permission_error" ->
            ProviderError BillingError message retryAfterHeader
        | status == 429 || errorType == "rate_limit_error" ->
            ProviderError RateLimitError message retryAfterHeader
        | errorType == "authentication_error" ->
            ProviderError AuthenticationError message retryAfterHeader
        | otherwise ->
            classifyHttpFailure status body
    Nothing -> classifyHttpFailure status body

-- | Convert an Anthropic streaming error event into the shared error channel.
classifyStreamError :: Text -> Text -> ApiError
classifyStreamError errorType message = case errorType of
    "overloaded_error" ->
        ProviderError OverloadedError message Nothing
    "rate_limit_error" ->
        ProviderError RateLimitError message Nothing
    "authentication_error" ->
        ProviderError AuthenticationError message Nothing
    "permission_error" ->
        ProviderError BillingError message Nothing
    _ -> ConnectionError
        ("Kimi Code stream error: " <> message)

-- | Pull the Anthropic error envelope's @type@ and @message@ when present.
decodeError :: Text -> Maybe (Text, Text)
decodeError body = do
    value <- Aeson.decodeStrict' (Text.encodeUtf8 body)
    object <- valueObject value
    inner <- valueObject =<< KeyMap.lookup "error" object
    errorType <- textField =<< KeyMap.lookup "type" inner
    let message = maybe "" id (textField =<< KeyMap.lookup "message" inner)
    pure (errorType, message)

valueObject :: Aeson.Value -> Maybe Aeson.Object
valueObject = \case
    Aeson.Object object -> Just object
    _ -> Nothing

textField :: Aeson.Value -> Maybe Text
textField = \case
    Aeson.String value -> Just value
    _ -> Nothing
