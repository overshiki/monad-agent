module Agent.DeepSeek.RequestSpec (spec) where

import Agent.Error (ApiError(..))
import Agent.Json (rawJsonFromEncoding)
import Agent.Responses.Types
import Agent.DeepSeek.Options
import Agent.DeepSeek.Request
import Agent.DeepSeek.Stream
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "mapModel" do
        it "uses the configured default for empty models and passes slugs through" do
            let options = defaultClientOptions { defaultModel = "deepseek-v4-pro" }
            mapModel options "" `shouldBe` "deepseek-v4-pro"
            mapModel options "   " `shouldBe` "deepseek-v4-pro"
            mapModel options "deepseek-v4-flash" `shouldBe` "deepseek-v4-flash"

    describe "buildRequest" do
        it "forces a stateless streaming Responses request" do
            let value = requestValue defaultClientOptions sampleRequest
            object <- expectObject value

            KeyMap.lookup "model" object `shouldBe` Just (Aeson.String "deepseek-v4-flash")
            KeyMap.lookup "store" object `shouldBe` Just (Aeson.Bool False)
            KeyMap.lookup "stream" object `shouldBe` Just (Aeson.Bool True)
            KeyMap.lookup "previous_response_id" object `shouldBe` Nothing
            KeyMap.lookup "instructions" object
                `shouldBe` Just (Aeson.String "You are a bookkeeping agent.")
            KeyMap.lookup "prompt_cache_key" object `shouldBe` Just (Aeson.String "cache-1")

        it "keeps instructions as a Responses field rather than a system item" do
            let value = requestValue defaultClientOptions sampleRequest
            object <- expectObject value
            input <- expectArray (KeyMap.lookup "input" object)
            firstItem <- case input of
                (item : _) -> expectObject item
                [] -> expectationFailure "input is empty" >> fail "unreachable"
            KeyMap.lookup "role" firstItem `shouldBe` Just (Aeson.String "user")
            length input `shouldBe` 1

        it "maps web_search, keeps function tools, and drops the computer tool" do
            let value = requestValue defaultClientOptions sampleRequest
            object <- expectObject value
            tools <- expectArray (KeyMap.lookup "tools" object)
            toolObjects <- traverse expectObject tools
            map (KeyMap.lookup "type") toolObjects `shouldBe`
                [ Just (Aeson.String "function")
                , Just (Aeson.String "web_search")
                ]

        it "preserves Codex custom and namespace tools" do
            let codexTools =
                    [ CustomToolValue CustomTool
                        { name = "grammar"
                        , description = Nothing
                        , format = Nothing
                        }
                    , NamespaceToolValue NamespaceTool
                        { name = "tools"
                        , description = Nothing
                        , tools = []
                        }
                    ]
                request = withTools (Just codexTools) sampleRequest
            object <- expectObject
                (requestValue defaultClientOptions request)
            tools <- expectArray (KeyMap.lookup "tools" object)
            toolObjects <- traverse expectObject tools
            map (KeyMap.lookup "type") toolObjects `shouldBe`
                [ Just (Aeson.String "custom")
                , Just (Aeson.String "namespace")
                ]

        it "uses the configured default when the request has no model" do
            let value = requestValue defaultClientOptions
                    (withModel Nothing sampleRequest)
            object <- expectObject value
            KeyMap.lookup "model" object `shouldBe` Just (Aeson.String "deepseek-v4-flash")

        it "preserves reasoning as supplied" do
            let value = requestValue defaultClientOptions sampleRequest
            object <- expectObject value
            KeyMap.lookup "reasoning" object `shouldBe` Just (Aeson.object
                [ "effort" .= ("high" :: Text)
                ])

    describe "echoReasoningContent" do
        let reasoningItem content summary =
                reasoningItemWithEncrypted content summary Nothing
            reasoningItemWithEncrypted content summary encrypted =
                ReasoningItemValue ReasoningItem
                    { itemId = Just "rs-1"
                    , summary = summary
                    , content = content
                    , encryptedContent = encrypted
                    , status = Just ItemCompleted
                    }
            summaryPart text =
                ReasoningSummaryPart
                    { partType = "summary_text"
                    , text = Just text
                    }
            reasoningTextPart :: Text -> Aeson.Value
            reasoningTextPart text = Aeson.object
                [ "type" .= ("reasoning_text" :: Text)
                , "text" .= text
                ]

        it "re-emits summary text as reasoning_text when content is absent" do
            let request = withInput
                    [reasoningItem Nothing [summaryPart "Checked repo"]]
                    sampleRequest
            object <- expectObject (requestValue defaultClientOptions request)
            content <- reasoningContentOf object
            content `shouldBe` [reasoningTextPart "Checked repo"]

        it "re-emits summary text when content is an empty list" do
            let request = withInput
                    [reasoningItem (Just []) [summaryPart "Checked repo"]]
                    sampleRequest
            object <- expectObject (requestValue defaultClientOptions request)
            content <- reasoningContentOf object
            content `shouldBe` [reasoningTextPart "Checked repo"]

        it "emits a placeholder reasoning_text marker when both are empty" do
            let request = withInput
                    [reasoningItem (Just []) []]
                    sampleRequest
            object <- expectObject (requestValue defaultClientOptions request)
            content <- reasoningContentOf object
            content `shouldBe` [reasoningTextPart "(thinking unavailable)"]

        it "leaves existing reasoning_text content untouched" do
            let request = withInput
                    [reasoningItem (Just [ReasoningTextPart "raw"]) [summaryPart "Checked repo"]]
                    sampleRequest
            object <- expectObject (requestValue defaultClientOptions request)
            content <- reasoningContentOf object
            content `shouldBe` [reasoningTextPart "raw"]

        it "synthesizes reasoning_text over an encrypted payload and drops it" do
            let request = withInput
                    [ reasoningItemWithEncrypted
                        (Just [])
                        []
                        (Just "encrypted-payload")
                    ]
                    sampleRequest
            object <- expectObject (requestValue defaultClientOptions request)
            -- DeepSeek rejects OpenAI-style encrypted reasoning blobs, so the
            -- replay must carry echoable plaintext and drop the payload.
            input <- expectArray (KeyMap.lookup "input" object)
            firstItem <- case input of (item : _) -> expectObject item; [] -> expectationFailure "input is empty" >> fail "unreachable"
            content <- expectArray (KeyMap.lookup "content" firstItem)
            content `shouldBe` [reasoningTextPart "(thinking unavailable)"]
            KeyMap.lookup "encrypted_content" firstItem `shouldBe` Nothing

        it "preserves plaintext content and always drops the encrypted payload" do
            let request = withInput
                    [ reasoningItemWithEncrypted
                        (Just [ReasoningTextPart "raw"])
                        [summaryPart "Checked repo"]
                        (Just "encrypted-payload")
                    ]
                    sampleRequest
            object <- expectObject (requestValue defaultClientOptions request)
            content <- reasoningContentOf object
            content `shouldBe` [reasoningTextPart "raw"]
            input <- expectArray (KeyMap.lookup "input" object)
            firstItem <- case input of (item : _) -> expectObject item; [] -> expectationFailure "input is empty" >> fail "unreachable"
            KeyMap.lookup "encrypted_content" firstItem `shouldBe` Nothing

        it "drops reasoning.encrypted_content from the include list" do
            let request = sampleRequest
                    { include = Just
                        [ ResponseInclude "reasoning.encrypted_content"
                        , ResponseInclude "foo.bar"
                        ]
                    }
            object <- expectObject (requestValue defaultClientOptions request)
            includes <- expectArray (KeyMap.lookup "include" object)
            includes `shouldBe`
                [ Aeson.String "foo.bar"
                ]

    describe "SSE assembly" do
        it "decodes typed event constructors and builds the merged final response" do
            let sse = Text.intercalate ""
                    [ sseBlock "response.output_item.done"
                        "{\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"function_call\",\"call_id\":\"call-1\",\"name\":\"echo\",\"arguments\":\"{}\"}}"
                    , sseBlock "response.completed"
                        "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp-1\",\"created_at\":0,\"model\":\"deepseek-v4-flash\",\"status\":\"completed\",\"output\":[],\"usage\":{\"input_tokens\":10,\"output_tokens\":5,\"total_tokens\":15}}}"
                    ]
            events <- expectRight (parseSseEvents sse)
            map responseStreamEventType events
                `shouldBe` [EventOutputItemDone, EventResponseCompleted]
            response <- expectRight (buildResponse events)
            response.responseId `shouldBe` "resp-1"
            fmap (.inputTokens) response.usage `shouldBe` Just 10
            [name | FunctionCallItem FunctionCall { name } <- response.output]
                `shouldBe` ["echo"]

        it "maps response.failed and missing completion to transport-level errors" do
            failedEvents <- expectRight $ parseSseEvents $ sseBlock "response.failed"
                "{\"type\":\"response.failed\",\"response\":{\"id\":\"resp-f\",\"created_at\":0,\"model\":\"deepseek-v4-flash\",\"status\":\"failed\",\"incomplete_details\":{\"reason\":\"overloaded\"}}}"
            case buildResponse failedEvents of
                Left (ConnectionError message) ->
                    message `shouldSatisfy` Text.isInfixOf "overloaded"
                other -> expectationFailure ("expected ConnectionError, got " <> show other)

            case buildResponse [] of
                Left (JsonDecodeError message _) ->
                    message `shouldSatisfy` Text.isInfixOf "terminal response"
                other -> expectationFailure ("expected JsonDecodeError, got " <> show other)

sseBlock :: Text -> Text -> Text
sseBlock eventType dataText =
    "event: " <> eventType <> "\ndata: " <> dataText <> "\n\n"

requestValue :: ClientOptions -> ResponseCreateParams -> Aeson.Value
requestValue options = Aeson.toJSON . buildRequest options

sampleRequest :: ResponseCreateParams
sampleRequest = defaultResponseCreateParams
    { model = Just "deepseek-v4-flash"
    , instructions = Just "You are a bookkeeping agent."
    , previousResponseId = Just "resp-should-drop"
    , store = Just True
    , input = Just (ResponseInputItems
        [ MessageItem ResponseMessage
            { messageId = Nothing
            , role = RoleUser
            , content = MessageContentParts [InputTextPart "hello" Nothing]
            , status = Nothing
            , phase = Nothing
            , passthrough = Nothing
            }
        ])
    , tools = Just
        [ FunctionToolValue FunctionTool
            { name = "echo_text"
            , description = Just "Echo the text back"
            , parameters = Just $
                rawJsonFromEncoding (Aeson.toEncoding (Aeson.object []))
            , strict = Nothing
            }
        , KnownResponseTool ToolWebSearch
        , KnownResponseTool ToolComputer
        ]
    , reasoning = Just ReasoningConfig
        { context = Nothing
        , effort = Just "high"
        , generateSummary = Nothing
        , reasoningMode = Nothing
        , summary = Nothing
        }
    , include = Just [ResponseInclude "reasoning.encrypted_content"]
    , promptCacheKey = Just "cache-1"
    }

withModel :: Maybe Text -> ResponseCreateParams -> ResponseCreateParams
withModel nextModel ResponseCreateParams { model = _, .. } =
    ResponseCreateParams { model = nextModel, .. }

withTools :: Maybe [ResponseTool] -> ResponseCreateParams -> ResponseCreateParams
withTools nextTools ResponseCreateParams { tools = _, .. } =
    ResponseCreateParams { tools = nextTools, .. }

withInput :: [ResponseItem] -> ResponseCreateParams -> ResponseCreateParams
withInput items ResponseCreateParams { input = _, .. } =
    ResponseCreateParams { input = Just (ResponseInputItems items), .. }

-- | Pull the @content@ array out of the first (reasoning) input item.
reasoningContentOf :: Aeson.Object -> IO [Aeson.Value]
reasoningContentOf object = do
    input <- expectArray (KeyMap.lookup "input" object)
    firstItem <- case input of
        (item : _) -> expectObject item
        [] -> expectationFailure "input is empty" >> fail "unreachable"
    case KeyMap.lookup "content" firstItem of
        Just value -> expectArray (Just value)
        Nothing -> pure []

expectObject :: Aeson.Value -> IO Aeson.Object
expectObject = \case
    Aeson.Object object -> pure object
    other -> expectationFailure ("expected object, got " <> show other) >> fail "unreachable"

expectArray :: Maybe Aeson.Value -> IO [Aeson.Value]
expectArray = \case
    Just (Aeson.Array values) -> pure (foldr (:) [] values)
    other -> expectationFailure ("expected array, got " <> show other) >> fail "unreachable"

expectRight :: Show e => Either e a -> IO a
expectRight = \case
    Left err -> expectationFailure ("expected Right, got Left " <> show err) >> fail "unreachable"
    Right value -> pure value
