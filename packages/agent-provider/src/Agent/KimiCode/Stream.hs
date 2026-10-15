-- | Typed decoding and terminal-response assembly for Kimi Code's Anthropic
-- Messages SSE stream.
module Agent.KimiCode.Stream
    ( KimiCodeStreamEvent(..)
    , KcBlock(..)
    , KcDelta(..)
    , parseStreamEvent
    , parseSseBlock
    , Assembler
    , emptyAssembler
    , StreamStep(..)
    , stepStream
    ) where

import Agent.Error (ApiError(..))
import Agent.KimiCode.Error (classifyStreamError)
import Agent.Loop (LoopEvent(..))
import Agent.Responses.Types
import Agent.ToolDispatch (ToolCall(..), ToolCallKind(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

-- | One Anthropic Messages SSE event.
data KimiCodeStreamEvent
    = KcMessageStart
        { kcMessageId :: !Text
        , kcModel :: !Text
        , kcInputTokens :: !Int
        , kcOutputTokens :: !Int
        }
    | KcContentBlockStart { kcIndex :: !Int, kcBlock :: !KcBlock }
    | KcContentBlockDelta { kcIndex :: !Int, kcDelta :: !KcDelta }
    | KcContentBlockStop { kcIndex :: !Int }
    | KcMessageDelta
        { kcStopReason :: !(Maybe Text)
        , kcStopSequence :: !(Maybe Text)
        , kcDeltaOutputTokens :: !(Maybe Int)
        }
    | KcMessageStop
    | KcError { kcErrorType :: !Text, kcErrorMessage :: !Text }
    | KcPing
    deriving (Eq, Show)

data KcBlock
    = KcTextBlock
    | KcThinkingBlock
    | KcToolUseBlock { kcToolUseId :: !Text, kcToolUseName :: !Text }
    deriving (Eq, Show)

data KcDelta
    = KcTextDelta !Text
    | KcThinkingDelta !Text
    | KcInputJsonDelta !Text
    | KcSignatureDelta !Text
    | KcUnknownDelta
    deriving (Eq, Show)

-- | Decode one @data:@ payload into a typed event. Unknown or malformed
-- payloads yield 'Nothing' so a healthy stream can skip them.
parseStreamEvent :: Text -> Maybe KimiCodeStreamEvent
parseStreamEvent payload = do
    value <- Aeson.decodeStrict' (Text.encodeUtf8 payload)
    object <- valueObject value
    eventType <- textField =<< KeyMap.lookup "type" object
    decodeByType eventType object

-- | Decode one complete SSE block (its @event:@/@data:@ lines) by extracting
-- the @data:@ payload. Blocks without a data payload (e.g. @[DONE]@) yield
-- 'Nothing'.
parseSseBlock :: Text -> Maybe KimiCodeStreamEvent
parseSseBlock block =
    case dataLines of
        [] -> Nothing
        _ -> parseStreamEvent (Text.intercalate "\n" dataLines)
  where
    dataLines =
        [ Text.strip (Text.drop 5 stripped)
        | line <- Text.lines block
        , let stripped = Text.stripStart line
        , "data:" `Text.isPrefixOf` stripped
        ]

decodeByType :: Text -> Aeson.Object -> Maybe KimiCodeStreamEvent
decodeByType eventType object = case eventType of
    "message_start" -> do
        message <- valueObject =<< KeyMap.lookup "message" object
        messageId <- textField =<< KeyMap.lookup "id" message
        model <- textField =<< KeyMap.lookup "model" message
        let usage = valueObject =<< KeyMap.lookup "usage" message
            inputTokens = maybe 0 id (usage >>= intFieldAt "input_tokens")
            outputTokens = maybe 0 id (usage >>= intFieldAt "output_tokens")
        pure KcMessageStart { kcMessageId = messageId, kcModel = model, kcInputTokens = inputTokens, kcOutputTokens = outputTokens }
    "content_block_start" -> do
        index <- intField =<< KeyMap.lookup "index" object
        block <- valueObject =<< KeyMap.lookup "content_block" object
        KcContentBlockStart index <$> decodeBlock block
    "content_block_delta" -> do
        index <- intField =<< KeyMap.lookup "index" object
        delta <- valueObject =<< KeyMap.lookup "delta" object
        KcContentBlockDelta index <$> decodeDelta delta
    "content_block_stop" ->
        KcContentBlockStop <$> (intField =<< KeyMap.lookup "index" object)
    "message_delta" -> do
        delta <- valueObject =<< KeyMap.lookup "delta" object
        let usage = valueObject =<< KeyMap.lookup "usage" object
        pure KcMessageDelta
            { kcStopReason = textField =<< KeyMap.lookup "stop_reason" delta
            , kcStopSequence = textField =<< KeyMap.lookup "stop_sequence" delta
            , kcDeltaOutputTokens = usage >>= intFieldAt "output_tokens"
            }
    "message_stop" -> Just KcMessageStop
    "error" -> do
        inner <- valueObject =<< KeyMap.lookup "error" object
        let errorType = maybe "" id (textField =<< KeyMap.lookup "type" inner)
            message = maybe "" id (textField =<< KeyMap.lookup "message" inner)
        pure (KcError errorType message)
    "ping" -> Just KcPing
    _ -> Nothing

decodeBlock :: Aeson.Object -> Maybe KcBlock
decodeBlock block = do
    blockType <- textField =<< KeyMap.lookup "type" block
    case blockType of
        "text" -> Just KcTextBlock
        "thinking" -> Just KcThinkingBlock
        "tool_use" -> do
            toolId <- textField =<< KeyMap.lookup "id" block
            name <- textField =<< KeyMap.lookup "name" block
            Just KcToolUseBlock { kcToolUseId = toolId, kcToolUseName = name }
        _ -> Nothing

decodeDelta :: Aeson.Object -> Maybe KcDelta
decodeDelta delta = do
    deltaType <- textField =<< KeyMap.lookup "type" delta
    pure $ case deltaType of
        "text_delta" -> KcTextDelta (atDefault "text")
        "thinking_delta" -> KcThinkingDelta (atDefault "thinking")
        "input_json_delta" -> KcInputJsonDelta (atDefault "partial_json")
        "signature_delta" -> KcSignatureDelta (atDefault "signature")
        _ -> KcUnknownDelta
  where
    atDefault key =
        maybe "" id (textField =<< KeyMap.lookup key delta)

-- | Per-block accumulation buffer.
data BlockState = BlockState
    { blockKind :: !KcBlock
    , blockChunksRev :: ![Text]
    }
    deriving (Eq, Show)

-- | Incremental assembly state for one streamed message.
data Assembler = Assembler
    { assemblerMessageId :: !(Maybe Text)
    , assemblerModel :: !Text
    , assemblerInputTokens :: !Int
    , assemblerOutputTokens :: !Int
    , assemblerStopReason :: !(Maybe Text)
    , assemblerBlocks :: !(IntMap BlockState)
    }
    deriving (Eq, Show)

emptyAssembler :: Assembler
emptyAssembler = Assembler
    { assemblerMessageId = Nothing
    , assemblerModel = ""
    , assemblerInputTokens = 0
    , assemblerOutputTokens = 0
    , assemblerStopReason = Nothing
    , assemblerBlocks = IntMap.empty
    }

data StreamStep
    = StreamContinue !Assembler ![LoopEvent]
    | StreamFinished !(Either ApiError Response) ![LoopEvent]

-- | Fold one event into the assembler, producing live loop events and,
-- on the terminal event, the assembled 'Response'.
stepStream :: Maybe Text -> Assembler -> KimiCodeStreamEvent -> StreamStep
stepStream modelHint assembler = \case
    KcMessageStart { kcMessageId, kcModel, kcInputTokens, kcOutputTokens } ->
        StreamContinue assembler
            { assemblerMessageId = Just kcMessageId
            , assemblerModel = kcModel
            , assemblerInputTokens = kcInputTokens
            , assemblerOutputTokens = kcOutputTokens
            }
            []
    KcContentBlockStart { kcIndex, kcBlock } ->
        StreamContinue
            (insertBlock kcIndex kcBlock assembler)
            (toolStarted kcIndex kcBlock)
    KcContentBlockDelta { kcIndex, kcDelta } ->
        StreamContinue
            (appendDelta kcIndex kcDelta assembler)
            (deltaEvent kcDelta)
    KcContentBlockStop { kcIndex } ->
        StreamContinue assembler (toolFinished kcIndex assembler)
    KcMessageDelta { kcStopReason, kcDeltaOutputTokens } ->
        StreamContinue assembler
            { assemblerStopReason = kcStopReason <|> assembler.assemblerStopReason
            , assemblerOutputTokens =
                maybe assembler.assemblerOutputTokens id kcDeltaOutputTokens
            }
            []
    KcMessageStop ->
        StreamFinished (finishStream modelHint assembler) []
    KcError { kcErrorType, kcErrorMessage } ->
        StreamFinished
            (Left (classifyStreamError kcErrorType kcErrorMessage))
            []
    KcPing -> StreamContinue assembler []

(<|>) :: Maybe a -> Maybe a -> Maybe a
Just value <|> _ = Just value
Nothing <|> fallback = fallback

insertBlock :: Int -> KcBlock -> Assembler -> Assembler
insertBlock index block assembler =
    assembler
        { assemblerBlocks =
            IntMap.insert index (BlockState block []) assembler.assemblerBlocks
        }

appendDelta :: Int -> KcDelta -> Assembler -> Assembler
appendDelta index delta assembler =
    case IntMap.lookup index assembler.assemblerBlocks of
        Nothing -> assembler
        Just block ->
            let chunk = deltaText delta
            in assembler
                { assemblerBlocks = IntMap.insert index
                    (block { blockChunksRev = chunk : block.blockChunksRev })
                    assembler.assemblerBlocks
                }

deltaText :: KcDelta -> Text
deltaText = \case
    KcTextDelta text -> text
    KcThinkingDelta text -> text
    KcInputJsonDelta text -> text
    KcSignatureDelta _ -> ""
    KcUnknownDelta -> ""

deltaEvent :: KcDelta -> [LoopEvent]
deltaEvent = \case
    KcTextDelta text
        | not (Text.null text) -> [TextDelta text]
    KcThinkingDelta text
        | not (Text.null text) -> [ReasoningDelta text]
    _ -> []

toolStarted :: Int -> KcBlock -> [LoopEvent]
toolStarted _index = \case
    KcToolUseBlock { kcToolUseId, kcToolUseName } ->
        [ToolStarted (emptyToolCall kcToolUseId kcToolUseName)]
    _ -> []

toolFinished :: Int -> Assembler -> [LoopEvent]
toolFinished index assembler =
    case IntMap.lookup index assembler.assemblerBlocks of
        Just block -> case block.blockKind of
            KcToolUseBlock { kcToolUseId, kcToolUseName } ->
                [ ToolUpdated
                    ( ToolCall
                        { callId = kcToolUseId
                        , name = kcToolUseName
                        , arguments = Text.concat (reverse block.blockChunksRev)
                        , callKind = FunctionCallKind
                        , argumentsEncrypted = False
                        }
                    )
                ]
            _ -> []
        Nothing -> []

emptyToolCall :: Text -> Text -> ToolCall
emptyToolCall callId name =
    ToolCall
        { callId
        , name
        , arguments = ""
        , callKind = FunctionCallKind
        , argumentsEncrypted = False
        }

finishStream :: Maybe Text -> Assembler -> Either ApiError Response
finishStream modelHint assembler =
    case assembler.assemblerMessageId of
        Nothing ->
            Left (JsonDecodeError "Kimi Code stream ended without a message" "")
        Just messageId ->
            Right (mkResponse modelHint messageId assembler (assembledItems assembler))

assembledItems :: Assembler -> [ResponseItem]
assembledItems assembler =
    map (blockItem . snd) (IntMap.toAscList assembler.assemblerBlocks)

blockItem :: BlockState -> ResponseItem
blockItem block = case block.blockKind of
    KcTextBlock ->
        MessageItem ResponseMessage
            { messageId = Nothing
            , content = MessageContentParts [OutputTextPart text Nothing Nothing]
            , role = RoleAssistant
            , status = Nothing
            , phase = Nothing
            , passthrough = Nothing
            }
    KcThinkingBlock ->
        ReasoningItemValue ReasoningItem
            { itemId = Nothing
            , summary = []
            , content = Just [ReasoningTextPart text]
            , encryptedContent = Nothing
            , status = Nothing
            }
    KcToolUseBlock { kcToolUseId, kcToolUseName } ->
        FunctionCallItem FunctionCall
            { itemId = Just kcToolUseId
            , callId = kcToolUseId
            , name = kcToolUseName
            , namespace = Nothing
            , provider = Nothing
            , arguments = text
            , encryptedFunctionArgs = Nothing
            , status = Nothing
            }
  where
    text = Text.concat (reverse block.blockChunksRev)

mkResponse
    :: Maybe Text
    -> Text
    -> Assembler
    -> [ResponseItem]
    -> Response
mkResponse modelHint messageId assembler output =
    Response
        { responseId = messageId
        , createdAt = 0
        , error = Nothing
        , incompleteDetails =
            case assembler.assemblerStopReason of
                Just "max_tokens" -> Just (IncompleteDetails "max_output_tokens")
                _ -> Nothing
        , instructions = Nothing
        , metadata = Nothing
        , model =
            if Text.null assembler.assemblerModel
                then fromMaybe "" modelHint
                else assembler.assemblerModel
        , object = "response"
        , output
        , parallelToolCalls = Nothing
        , temperature = Nothing
        , toolChoice = Nothing
        , tools = Nothing
        , topP = Nothing
        , background = Nothing
        , completedAt = Nothing
        , conversation = Nothing
        , maxOutputTokens = Nothing
        , maxToolCalls = Nothing
        , moderation = Nothing
        , previousResponseId = Nothing
        , prompt = Nothing
        , promptCacheKey = Nothing
        , promptCacheOptions = Nothing
        , promptCacheRetention = Nothing
        , reasoning = Nothing
        , safetyIdentifier = Nothing
        , serviceTier = Nothing
        , status = responseStatusFor assembler.assemblerStopReason
        , text = Nothing
        , topLogprobs = Nothing
        , truncation = Nothing
        , usage = Just ResponseUsage
            { inputTokens = assembler.assemblerInputTokens
            , inputTokensDetails = Nothing
            , outputTokens = assembler.assemblerOutputTokens
            , outputTokensDetails = Nothing
            , totalTokens =
                assembler.assemblerInputTokens + assembler.assemblerOutputTokens
            }
        , user = Nothing
        }

responseStatusFor :: Maybe Text -> ResponseStatus
responseStatusFor = \case
    Just "max_tokens" -> ResponseIncomplete
    _ -> ResponseCompleted

valueObject :: Aeson.Value -> Maybe Aeson.Object
valueObject = \case
    Aeson.Object object -> Just object
    _ -> Nothing

textField :: Aeson.Value -> Maybe Text
textField = \case
    Aeson.String value -> Just value
    _ -> Nothing

intField :: Aeson.Value -> Maybe Int
intField = \case
    Aeson.Number number -> Just (round number :: Int)
    _ -> Nothing

intFieldAt :: Text -> Aeson.Object -> Maybe Int
intFieldAt key object =
    intField =<< KeyMap.lookup (Key.fromText key) object
