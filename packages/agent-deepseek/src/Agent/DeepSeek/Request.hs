-- | Pure projection from canonical Responses API parameters to the dialect
-- accepted by DeepSeek's stateless Responses endpoint.
module Agent.DeepSeek.Request
    ( mapModel
    , buildRequest
    ) where

import Agent.Responses.Request
    ( forceStatelessStreaming
    , mapResponseTools
    , setResponseModel
    )
import Agent.Responses.Types
import Agent.DeepSeek.Options (ClientOptions(..))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Use the configured default for an absent or empty model; DeepSeek model
-- slugs pass through unchanged.
mapModel :: ClientOptions -> Text -> Text
mapModel options model
    | Text.null (Text.strip model) = options.defaultModel
    | otherwise = model

-- | Build the typed Responses request sent to DeepSeek.
--
-- DeepSeek is a drop-in Responses host except that it is stateless:
-- @store@ must be false and @previous_response_id@ is unsupported. ChatGPT-only
-- computer-use tools are dropped; function tools and @web_search@ pass through.
-- DeepSeek silently ignores unknown tools, so anything else passes through too.
buildRequest :: ClientOptions -> ResponseCreateParams -> ResponseCreateParams
buildRequest options request =
    mapResponseTools deepSeekTool $
        setResponseModel (mapModel options (fromMaybe "" request.model)) $
            echoReasoningContent $
                excludeEncryptedReasoningInclude $
                    forceStatelessStreaming request

-- | DeepSeek's thinking mode requires the assistant's reasoning to be passed
-- back on tool follow-ups ("The reasoning_text in the thinking mode must be
-- passed back to the API"). The stream assembly stores that text either as the
-- reasoning item's 'summary' or as raw @reasoning_text@ 'content'. When a
-- replay is missing plaintext content (including an empty content list, which
-- DeepSeek sends for turns with no reasoning and still requires to be echoed),
-- re-emit it from the summary so the wire encoder carries the reasoning text.
-- A reasoning item with no summary text becomes a placeholder @reasoning_text@
-- part (DeepSeek rejects empty text as "not passed back"), and any OpenAI-style
-- encrypted payload is dropped because DeepSeek only accepts plaintext.
echoReasoningContent :: ResponseCreateParams -> ResponseCreateParams
echoReasoningContent ResponseCreateParams{..} =
    ResponseCreateParams { input = echoInput <$> input, .. }
  where
    echoInput = \case
        ResponseInputItems items -> ResponseInputItems (map echoItem items)
        other -> other
    echoItem (ReasoningItemValue reasoning)
        | hasNoContent reasoning.content =
            ReasoningItemValue
                (setReasoningContent
                    (Just (echoedContent reasoning.summary))
                    reasoning { encryptedContent = Nothing })
    echoItem item = item
    echoedContent summary =
        case mapMaybe summaryText summary of
            [] -> [ReasoningTextPart unavailableReasoningPlaceholder]
            parts -> parts
    summaryText (ReasoningSummaryPart _ text) =
        ReasoningTextPart <$> text

-- | DeepSeek's thinking-mode contract accepts only plaintext @reasoning_text@
-- content — not OpenAI-style @encrypted_content@ blobs, which it rejects even
-- as a leftover. A reasoning item replayed without plaintext content therefore
-- needs content echoed from its summary (or a placeholder when no summary text
-- exists) and its encrypted payload dropped so the follow-up carries echoable
-- text. (Codex workaround for the same 400: inject a placeholder text part.)
unavailableReasoningPlaceholder :: Text
unavailableReasoningPlaceholder = "(thinking unavailable)"

-- | Nothing or an empty list both mean "no reasoning_text carried yet".
hasNoContent :: Maybe [ResponseContentPart] -> Bool
hasNoContent = maybe True null

setReasoningContent :: Maybe [ResponseContentPart] -> ReasoningItem -> ReasoningItem
setReasoningContent newContent reasoning =
    ReasoningItem
        { itemId = reasoning.itemId
        , summary = reasoning.summary
        , content = newContent
        , encryptedContent = reasoning.encryptedContent
        , status = reasoning.status
        }

-- | DeepSeek exposes thinking-mode reasoning as plain @reasoning_text@ content,
-- not as OpenAI-style encrypted reasoning. Asking for
-- @reasoning.encrypted_content@ can coax the provider into returning an
-- encrypted blob alongside (or instead of) the text we need to echo back on
-- tool follow-ups, which then triggers the "reasoning_text must be passed
-- back" error. Drop that include so the response carries echoable text.
excludeEncryptedReasoningInclude :: ResponseCreateParams -> ResponseCreateParams
excludeEncryptedReasoningInclude ResponseCreateParams{..} =
    ResponseCreateParams
        { include = filter (not . isEncryptedReasoningInclude) <$> include
        , ..
        }
  where
    isEncryptedReasoningInclude (ResponseInclude value) =
        value == "reasoning.encrypted_content"

deepSeekTool :: ResponseTool -> Maybe ResponseTool
deepSeekTool tool = case tool of
    FunctionToolValue {} -> Just tool
    KnownResponseTool ToolWebSearch -> Just tool
    KnownResponseTool ToolComputer -> Nothing
    KnownResponseTool ToolComputerUsePreview -> Nothing
    _ -> Just tool
