module Agent.KimiCode.RequestSpec (spec) where

import Agent.Json (rawJsonFromEncoding)
import Agent.KimiCode.Options
import Agent.KimiCode.Request
import Agent.Responses.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "mapModel" do
        it "uses the configured default for empty models and passes slugs through" do
            let options = defaultClientOptions { defaultModel = "kimi-for-coding" }
            mapModel options "" `shouldBe` "kimi-for-coding"
            mapModel options "   " `shouldBe` "kimi-for-coding"
            mapModel options "kimi-for-coding" `shouldBe` "kimi-for-coding"

    describe "buildRequest" do
        it "sets model, max_tokens, and stream" do
            object <- expectObject (requestValue sampleRequest)
            KeyMap.lookup "model" object `shouldBe` Just (Aeson.String "kimi-for-coding")
            KeyMap.lookup "stream" object `shouldBe` Just (Aeson.Bool True)
            case KeyMap.lookup "max_tokens" object of
                Just (Aeson.Number _) -> pure ()
                other -> expectationFailure ("expected max_tokens number, got " <> show other)

        it "puts instructions into the top-level system field" do
            object <- expectObject (requestValue sampleRequest)
            KeyMap.lookup "system" object
                `shouldBe` Just (Aeson.String "You are a bookkeeping agent.")

        it "maps user, assistant, reasoning, and tool items into messages" do
            let items =
                    [ userItem "hello"
                    , reasoningItem [ReasoningTextPart "thinking hard"]
                    , functionCall "call-1" "echo" "{}"
                    , functionOutput "call-1" "{}"
                    ]
                request = withInput items sampleRequest
            object <- expectObject (requestValue request)
            messages <- expectArray (KeyMap.lookup "messages" object)
            length messages `shouldBe` 3

            user <- expectObject (messages !! 0)
            KeyMap.lookup "role" user `shouldBe` Just (Aeson.String "user")

            assistant <- expectObject (messages !! 1)
            KeyMap.lookup "role" assistant `shouldBe` Just (Aeson.String "assistant")
            blocks <- expectArray (KeyMap.lookup "content" assistant)
            case blocks of
                [thinking, toolUse] -> do
                    thinkingObject <- expectObject thinking
                    KeyMap.lookup "type" thinkingObject `shouldBe` Just (Aeson.String "thinking")
                    toolUseObject <- expectObject toolUse
                    KeyMap.lookup "type" toolUseObject `shouldBe` Just (Aeson.String "tool_use")
                    KeyMap.lookup "id" toolUseObject `shouldBe` Just (Aeson.String "call-1")
                other -> expectationFailure ("expected [thinking, tool_use], got " <> show other)

            toolResult <- expectObject (messages !! 2)
            KeyMap.lookup "role" toolResult `shouldBe` Just (Aeson.String "user")
            resultBlocks <- expectArray (KeyMap.lookup "content" toolResult)
            case resultBlocks of
                [result] -> do
                    resultObject <- expectObject result
                    KeyMap.lookup "type" resultObject `shouldBe` Just (Aeson.String "tool_result")
                    KeyMap.lookup "tool_use_id" resultObject `shouldBe` Just (Aeson.String "call-1")
                other -> expectationFailure ("expected [tool_result], got " <> show other)

        it "maps function tools into Anthropic tool schemas" do
            object <- expectObject (requestValue sampleRequest)
            tools <- expectArray (KeyMap.lookup "tools" object)
            length tools `shouldBe` 1
            tool <- expectObject (head tools)
            KeyMap.lookup "name" tool `shouldBe` Just (Aeson.String "echo_text")
            KeyMap.lookup "description" tool
                `shouldBe` Just (Aeson.String "Echo the text back")
            case KeyMap.lookup "input_schema" tool of
                Just (Aeson.Object _) -> pure ()
                other -> expectationFailure ("expected input_schema object, got " <> show other)

        it "maps reasoning effort to a thinking budget" do
            let request = withReasoning
                    ReasoningConfig
                        { context = Nothing
                        , effort = Just "high"
                        , generateSummary = Nothing
                        , reasoningMode = Nothing
                        , summary = Nothing
                        }
                    sampleRequest
            object <- expectObject (requestValue request)
            thinking <- expectField "thinking" object
            KeyMap.lookup "type" thinking `shouldBe` Just (Aeson.String "enabled")

        it "drops hosted tools and keeps function tools" do
            let request = withTools
                    [ FunctionToolValue FunctionTool
                        { name = "echo_text"
                        , description = Nothing
                        , parameters = Nothing
                        , strict = Nothing
                        }
                    , KnownResponseTool ToolWebSearch
                    ]
                    sampleRequest
            object <- expectObject (requestValue request)
            tools <- expectArray (KeyMap.lookup "tools" object)
            length tools `shouldBe` 1

sampleRequest :: ResponseCreateParams
sampleRequest = defaultResponseCreateParams
    { model = Just "kimi-for-coding"
    , instructions = Just "You are a bookkeeping agent."
    , input = Just (ResponseInputItems [userItem "hello"])
    , tools = Just
        [ FunctionToolValue FunctionTool
            { name = "echo_text"
            , description = Just "Echo the text back"
            , parameters = Just $
                rawJsonFromEncoding (Aeson.toEncoding (Aeson.object []))
            , strict = Nothing
            }
        ]
    }

withInput :: [ResponseItem] -> ResponseCreateParams -> ResponseCreateParams
withInput items params =
    case params of
        ResponseCreateParams{..} ->
            ResponseCreateParams { input = Just (ResponseInputItems items), .. }

withTools :: [ResponseTool] -> ResponseCreateParams -> ResponseCreateParams
withTools nextTools params =
    case params of
        ResponseCreateParams{..} ->
            ResponseCreateParams { tools = Just nextTools, .. }

withReasoning :: ReasoningConfig -> ResponseCreateParams -> ResponseCreateParams
withReasoning nextReasoning params =
    case params of
        ResponseCreateParams{..} ->
            ResponseCreateParams { reasoning = Just nextReasoning, .. }

userItem :: Text -> ResponseItem
userItem text = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts [InputTextPart text Nothing]
    , role = RoleUser
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing
    }

reasoningItem :: [ResponseContentPart] -> ResponseItem
reasoningItem content = ReasoningItemValue ReasoningItem
    { itemId = Just "rs-1"
    , summary = []
    , content = Just content
    , encryptedContent = Nothing
    , status = Just ItemCompleted
    }

functionCall :: Text -> Text -> Text -> ResponseItem
functionCall callId name arguments = FunctionCallItem FunctionCall
    { itemId = Just callId
    , callId
    , name
    , namespace = Nothing
    , provider = Nothing
    , arguments
    , encryptedFunctionArgs = Nothing
    , status = Just ItemCompleted
    }

functionOutput :: Text -> Text -> ResponseItem
functionOutput callId output = FunctionCallOutputItem FunctionCallOutput
    { itemId = Nothing
    , callId
    , name = Nothing
    , namespace = Nothing
    , provider = Nothing
    , output = rawJsonFromEncoding (Aeson.toEncoding (Aeson.String output))
    , status = Nothing
    }

requestValue :: ResponseCreateParams -> Aeson.Value
requestValue = buildRequest defaultClientOptions

expectObject :: Aeson.Value -> IO Aeson.Object
expectObject = \case
    Aeson.Object object -> pure object
    other -> expectationFailure ("expected object, got " <> show other) >> fail "unreachable"

expectField :: Text -> Aeson.Object -> IO Aeson.Object
expectField key object = do
    value <- case KeyMap.lookup (Key.fromText key) object of
        Just value -> pure value
        Nothing -> expectationFailure ("missing field " <> Text.unpack key) >> fail "unreachable"
    expectObject value

expectArray :: Maybe Aeson.Value -> IO [Aeson.Value]
expectArray = \case
    Just (Aeson.Array values) -> pure (foldr (:) [] values)
    other -> expectationFailure ("expected array, got " <> show other) >> fail "unreachable"
