-- | Generated record setters for the wire types.
--
-- Under @NoFieldSelectors@ + @DuplicateRecordFields@ every "clone with one
-- field changed" becomes a hand-written positional rebuild. 'makeSetters'
-- generates those setters at compile time; see 'Agent.Json.TH'. Import this
-- module wherever such an update is needed instead of writing the rebuild by
-- hand. Names are @set\<Record\>\<Field\>@.
{-# LANGUAGE TemplateHaskell #-}
module Agent.Responses.Types.Setters where

import Agent.Json.TH (makeSetters)
import Agent.Responses.Types.Common
import Agent.Responses.Types.Content
import Agent.Responses.Types.Items
import Agent.Responses.Types.Items.Known
import Agent.Responses.Types.Request
import Agent.Responses.Types.Response
import Agent.Responses.Types.Tools

makeSetters ''ReasoningItem
makeSetters ''ReasoningSummaryPart
makeSetters ''ResponseMessage
makeSetters ''FunctionCall
makeSetters ''FunctionCallOutput
makeSetters ''CustomToolCall
makeSetters ''CustomToolCallOutput
makeSetters ''WebSearchCall
makeSetters ''LocalShellCall
makeSetters ''ToolSearchCall
makeSetters ''ToolSearchOutput
makeSetters ''ImageGenerationCall
makeSetters ''Response
makeSetters ''ResponseUsage
makeSetters ''ReasoningConfig
makeSetters ''ResponseTextConfig
makeSetters ''ResponseCreateParams
