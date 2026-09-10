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
                forceStatelessStreaming request

-- | DeepSeek's thinking mode requires the assistant's reasoning to be passed
-- back on tool follow-ups ("The reasoning_text in the thinking mode must be
-- passed back to the API"). The stream assembly stores that text as the
-- reasoning item's 'summary'; re-emit it as the item's 'content' so the wire
-- encoder carries the full text rather than only the summary shape.
echoReasoningContent :: ResponseCreateParams -> ResponseCreateParams
echoReasoningContent ResponseCreateParams{..} =
    ResponseCreateParams { input = echoInput <$> input, .. }
  where
    echoInput = \case
        ResponseInputItems items -> ResponseInputItems (map echoItem items)
        other -> other
    echoItem (ReasoningItemValue reasoning)
        | reasoning.content == Nothing
        , not (null reasoning.summary) =
            ReasoningItemValue reasoning
                { content = Just (mapMaybe summaryText reasoning.summary)
                }
    echoItem item = item
    summaryText (ReasoningSummaryPart _ text) =
        OutputTextPart <$> text <*> Just Nothing <*> Just Nothing

deepSeekTool :: ResponseTool -> Maybe ResponseTool
deepSeekTool tool = case tool of
    FunctionToolValue {} -> Just tool
    KnownResponseTool ToolWebSearch -> Just tool
    KnownResponseTool ToolComputer -> Nothing
    KnownResponseTool ToolComputerUsePreview -> Nothing
    _ -> Just tool
