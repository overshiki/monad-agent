-- | Pure projection from canonical Responses parameters to the Anthropic
-- Messages dialect accepted by Kimi Code's coding endpoint.
--
-- Kimi Code is stateless: the complete local transcript is replayed on every
-- request as Anthropic @messages@, with prior assistant reasoning replayed as
-- @thinking@ blocks (Anthropic's multi-turn contract mirrors the DeepSeek
-- @reasoning_text@ echo).
module Agent.KimiCode.Request
    ( mapModel
    , buildRequest
    , buildRequestBody
    ) where

import Agent.Json (RawJson, rawJsonBytes)
import Agent.KimiCode.Options (ClientOptions(..))
import Agent.Responses.Types
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- | Use the configured default for an absent or empty model; Kimi Code model
-- slugs pass through unchanged.
mapModel :: ClientOptions -> Text -> Text
mapModel options model
    | Text.null (Text.strip model) = options.defaultModel
    | otherwise = model

-- | Build the Anthropic Messages request body sent to Kimi Code.
buildRequest :: ClientOptions -> ResponseCreateParams -> Aeson.Value
buildRequest options request =
    Aeson.object $
        catMaybes
            [ Just ("model" .= mapModel options (fromMaybe "" request.model))
            , ("system" .=) <$> nonBlank (buildSystem request)
            , Just ("messages" .= buildMessages (inputItems request.input))
            , ("tools" .=) <$> nonEmptyList (buildTools request.tools)
            , Just ("max_tokens" .= fromMaybe defaultMaxTokens request.maxOutputTokens)
            , Just ("stream" .= (True :: Bool))
            , ("thinking" .=) <$> buildThinking (request.reasoning >>= (.effort))
            , ("temperature" .=) <$> request.temperature
            , ("tool_choice" .=) <$> buildToolChoice request.toolChoice
            ]

-- | Encoded request body for direct HTTP use.
buildRequestBody :: ClientOptions -> ResponseCreateParams -> LBS.ByteString
buildRequestBody options = Aeson.encode . buildRequest options

-- | Anthropic Messages requires @max_tokens@; use a generous default when the
-- canonical request leaves it unset.
defaultMaxTokens :: Int
defaultMaxTokens = 32768

-- | Placeholder used when prior reasoning must be replayed but no plaintext
-- reasoning was captured. Anthropic-compatible hosts reject a missing
-- @thinking@ block on multi-turn the same way DeepSeek rejects a missing
-- @reasoning_text@.
unavailableReasoningPlaceholder :: Text
unavailableReasoningPlaceholder = "(thinking unavailable)"

inputItems :: Maybe ResponseInput -> [ResponseItem]
inputItems = \case
    Just (ResponseInputItems items) -> items
    Just (ResponseInputText text) ->
        [ MessageItem ResponseMessage
            { messageId = Nothing
            , content = MessageContentParts [InputTextPart text Nothing]
            , role = RoleUser
            , status = Nothing
            , phase = Nothing
            , passthrough = Nothing
            }
        ]
    Nothing -> []

-- | The top-level @system@ prompt combines the canonical @instructions@ with
-- any developer/system-role messages already present in the input.
buildSystem :: ResponseCreateParams -> Text
buildSystem request =
    Text.intercalate "\n\n" $
        maybeToList (nonBlank =<< request.instructions) <> developerTexts
  where
    developerTexts =
        [ text
        | MessageItem message <- inputItems request.input
        , message.role == RoleDeveloper || message.role == RoleSystem
        , text <- messageText message
        ]

messageText :: ResponseMessage -> [Text]
messageText message = case message.content of
    MessageContentText text -> [text]
    MessageContentParts parts ->
        [ text
        | InputTextPart { text } <- parts
        ]
            <> [text | OutputTextPart { text } <- parts]

-- | Group canonical input items into alternating Anthropic @user@/@assistant@
-- messages. Contiguous assistant items (thinking, text, tool calls) collapse
-- into a single message, matching Anthropic's extended-thinking grouping.
buildMessages :: [ResponseItem] -> [Aeson.Value]
buildMessages items =
    [ Aeson.object ["role" .= role, "content" .= blocks]
    | (role, blocks) <- groupMessages (mapMaybe itemMessage items)
    ]

groupMessages :: [(Text, [Aeson.Value])] -> [(Text, [Aeson.Value])]
groupMessages = finish . foldl step (Nothing, [])
  where
    step (current, acc) (role, blocks) = case current of
        Nothing -> (Just (role, blocks), acc)
        Just (currentRole, currentBlocks)
            | currentRole == role ->
                (Just (role, currentBlocks <> blocks), acc)
            | otherwise ->
                (Just (role, blocks), (currentRole, currentBlocks) : acc)
    finish (Nothing, acc) = reverse acc
    finish (Just message, acc) = reverse (message : acc)

itemMessage :: ResponseItem -> Maybe (Text, [Aeson.Value])
itemMessage = \case
    MessageItem message
        | message.role == RoleUser ->
            Just ("user", userBlocks message.content)
        | message.role == RoleAssistant ->
            Just ("assistant", assistantBlocks message.content)
        | otherwise -> Nothing
    ReasoningItemValue reasoning ->
        Just ("assistant", [thinkingBlock reasoning])
    FunctionCallItem call ->
        Just ("assistant", [toolUseBlock call.callId call.name (parseJsonObject call.arguments)])
    CustomToolCallItem call ->
        Just ("assistant", [toolUseBlock call.callId call.name (parseJsonObject call.input)])
    FunctionCallOutputItem output ->
        Just ("user", [toolResultBlock output.callId output.output])
    CustomToolCallOutputItem output ->
        Just ("user", [toolResultBlock output.callId output.output])
    _ -> Nothing

userBlocks :: MessageContent -> [Aeson.Value]
userBlocks = \case
    MessageContentText text -> [textBlock text]
    MessageContentParts parts -> mapMaybe userPartBlock parts

assistantBlocks :: MessageContent -> [Aeson.Value]
assistantBlocks = \case
    MessageContentText text -> [textBlock text]
    MessageContentParts parts -> mapMaybe assistantPartBlock parts

userPartBlock :: ResponseContentPart -> Maybe Aeson.Value
userPartBlock = \case
    InputTextPart { text } -> Just (textBlock text)
    InputImagePart { imageUrl = Just url } -> Just (imageBlock url)
    _ -> Nothing

assistantPartBlock :: ResponseContentPart -> Maybe Aeson.Value
assistantPartBlock = \case
    OutputTextPart { text } -> Just (textBlock text)
    _ -> Nothing

textBlock :: Text -> Aeson.Value
textBlock text =
    Aeson.object ["type" .= ("text" :: Text), "text" .= text]

-- | Emit an Anthropic image source. Data URLs produced by the harness are
-- converted to base64 sources; anything else is passed through as a URL.
imageBlock :: Text -> Aeson.Value
imageBlock url = case parseDataUrl url of
    Just (mediaType, base64) ->
        Aeson.object
            [ "type" .= ("image" :: Text)
            , "source" .= Aeson.object
                [ "type" .= ("base64" :: Text)
                , "media_type" .= mediaType
                , "data" .= base64
                ]
            ]
    Nothing ->
        Aeson.object
            [ "type" .= ("image" :: Text)
            , "source" .= Aeson.object
                [ "type" .= ("url" :: Text)
                , "url" .= url
                ]
            ]

-- | Parse @data:<mime>;base64,<payload>@ into its media type and payload.
parseDataUrl :: Text -> Maybe (Text, Text)
parseDataUrl url = do
    rest <- Text.stripPrefix "data:" url
    (header, comma) <- Just (Text.breakOn "," rest)
    let payload = Text.drop 1 comma
    case Text.breakOn ";" header of
        (mediaType, semi)
            | not (Text.null semi)
            , Text.stripPrefix ";base64" semi == Just "" ->
                Just (mediaType, payload)
        _ -> Nothing

thinkingBlock :: ReasoningItem -> Aeson.Value
thinkingBlock reasoning =
    Aeson.object ["type" .= ("thinking" :: Text), "thinking" .= thinkingText reasoning]

thinkingText :: ReasoningItem -> Text
thinkingText reasoning =
    fromMaybe unavailableReasoningPlaceholder $
        firstNonBlank
            (nonBlank contentText)
            (nonBlank summaryText)
  where
    contentText =
        Text.intercalate "\n"
            [ text
            | ReasoningTextPart { text } <- fromMaybe [] reasoning.content
            ]
    summaryText =
        Text.intercalate "\n"
            [ text
            | ReasoningSummaryPart _ (Just text) <- reasoning.summary
            ]

toolUseBlock :: Text -> Text -> Aeson.Value -> Aeson.Value
toolUseBlock callId name input =
    Aeson.object
        [ "type" .= ("tool_use" :: Text)
        , "id" .= callId
        , "name" .= name
        , "input" .= input
        ]

toolResultBlock :: Text -> RawJson -> Aeson.Value
toolResultBlock callId output =
    Aeson.object
        [ "type" .= ("tool_result" :: Text)
        , "tool_use_id" .= callId
        , "content" .= Text.decodeUtf8 (rawJsonBytes output)
        ]

-- | Decode a JSON-encoded argument string into an object, defaulting to an
-- empty object when the payload is absent or not an object.
parseJsonObject :: Text -> Aeson.Value
parseJsonObject text = case Aeson.decodeStrict' (Text.encodeUtf8 text) of
    Just (Aeson.Object object) -> Aeson.Object object
    _ -> Aeson.object []

-- | Anthropic tool schemas for function and custom tools. Namespace tools are
-- flattened; hosted tools have no Messages equivalent and are dropped.
buildTools :: Maybe [ResponseTool] -> [Aeson.Value]
buildTools = maybe [] (mapMaybe toolSchema . flattenTools)

flattenTools :: [ResponseTool] -> [ResponseTool]
flattenTools = concatMap \case
    NamespaceToolValue namespace -> flattenTools namespace.tools
    tool -> [tool]

toolSchema :: ResponseTool -> Maybe Aeson.Value
toolSchema = \case
    FunctionToolValue tool ->
        Just (schemaObject tool.name tool.description tool.parameters)
    CustomToolValue tool ->
        Just (schemaObject tool.name tool.description tool.format)
    _ -> Nothing

schemaObject :: Text -> Maybe Text -> Maybe RawJson -> Aeson.Value
schemaObject name description parameters =
    Aeson.object $
        catMaybes
            [ Just ("name" .= name)
            , ("description" .=) <$> description
            , Just ("input_schema" .= inputSchemaOf parameters)
            ]

inputSchemaOf :: Maybe RawJson -> Aeson.Value
inputSchemaOf parameters =
    fromMaybe emptySchema (parameters >>= decodeRaw)

decodeRaw :: RawJson -> Maybe Aeson.Value
decodeRaw = Aeson.decodeStrict' . rawJsonBytes

emptySchema :: Aeson.Value
emptySchema =
    Aeson.object
        [ "type" .= ("object" :: Text)
        , "properties" .= Aeson.object []
        ]

-- | Map reasoning effort onto Anthropic's @thinking@ block budget.
buildThinking :: Maybe Text -> Maybe Aeson.Value
buildThinking effort = case Text.toLower (Text.strip (fromMaybe "" effort)) of
    "none" -> Just (disabledThinking ())
    "off" -> Just (disabledThinking ())
    "minimal" -> Just (disabledThinking ())
    "low" -> Just (enabledThinking 2048)
    "high" -> Just (enabledThinking 8192)
    "max" -> Just (enabledThinking 16384)
    _ -> Nothing
  where
    enabledThinking :: Int -> Aeson.Value
    enabledThinking budget =
        Aeson.object
            [ "type" .= ("enabled" :: Text)
            , "budget_tokens" .= budget
            ]
    disabledThinking () =
        Aeson.object ["type" .= ("disabled" :: Text)]

buildToolChoice :: Maybe ToolChoice -> Maybe Aeson.Value
buildToolChoice = \case
    Just (ToolChoiceMode ToolChoiceNone) ->
        Just (Aeson.object ["type" .= ("none" :: Text)])
    Just (ToolChoiceMode ToolChoiceRequired) ->
        Just (Aeson.object ["type" .= ("any" :: Text)])
    _ -> Nothing

nonEmptyList :: [a] -> Maybe [a]
nonEmptyList [] = Nothing
nonEmptyList values = Just values

nonBlank :: Text -> Maybe Text
nonBlank text
    | Text.null (Text.strip text) = Nothing
    | otherwise = Just text

firstNonBlank :: Maybe a -> Maybe a -> Maybe a
firstNonBlank first second = case first of
    Just value -> Just value
    Nothing -> second
