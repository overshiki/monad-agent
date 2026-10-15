-- | Pure projection from canonical Responses API parameters to the dialect
-- accepted by Kimi's stateless Responses endpoint.
module Agent.Kimi.Request
    ( mapModel
    , buildRequest
    ) where

import Agent.Responses.Request
    ( forceStatelessStreaming
    , mapResponseTools
    , setRequestModel
    )
import Agent.Responses.Types
import Agent.Kimi.Options (ClientOptions(..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Use the configured default for an absent or empty model; Kimi model
-- slugs pass through unchanged.
mapModel :: ClientOptions -> Text -> Text
mapModel options model
    | Text.null (Text.strip model) = options.defaultModel
    | otherwise = model

-- | Build the typed Responses request sent to Kimi.
--
-- Kimi is a drop-in Responses host except that it is stateless:
-- @store@ must be false and @previous_response_id@ is unsupported. ChatGPT-only
-- computer-use tools are dropped; function tools and @web_search@ pass through.
-- Kimi silently ignores unknown tools, so anything else passes through too.
-- @tool_choice: \"none\"@ is rejected by Kimi, so it is dropped (the callers
-- that set it also send an empty tool list, so no tool calls are possible).
buildRequest :: ClientOptions -> ResponseCreateParams -> ResponseCreateParams
buildRequest options request =
    mapResponseTools kimiTool $
        setRequestModel (mapModel options (fromMaybe "" request.model)) $
            forceStatelessStreaming $
                dropUnsupportedToolChoice request

dropUnsupportedToolChoice :: ResponseCreateParams -> ResponseCreateParams
dropUnsupportedToolChoice request = case request.toolChoice of
    Just (ToolChoiceMode ToolChoiceNone) -> request { toolChoice = Nothing }
    _ -> request

kimiTool :: ResponseTool -> Maybe ResponseTool
kimiTool tool = case tool of
    FunctionToolValue {} -> Just tool
    KnownResponseTool ToolWebSearch -> Just tool
    KnownResponseTool ToolComputer -> Nothing
    KnownResponseTool ToolComputerUsePreview -> Nothing
    _ -> Just tool
