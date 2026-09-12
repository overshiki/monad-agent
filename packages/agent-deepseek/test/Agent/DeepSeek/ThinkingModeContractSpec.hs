-- | Systematic guard for DeepSeek's stateless thinking-mode replay contract.
--
-- DeepSeek rejects a follow-up request with
-- @\"The reasoning_text in the thinking mode must be passed back to the API\"@
-- whenever a replayed reasoning item reaches the wire without non-empty
-- plaintext @reasoning_text@ content, or carrying an OpenAI-style
-- @encrypted_content@ blob. That single constraint has regressed five times
-- (a4431175, b0a3d52e, 11b13b36, 707e1c32, 77d88d12), each time at a different
-- link of the decode -> assemble -> persist -> project pipeline.
--
-- This spec encodes the contract directly and asserts it on the *projected
-- wire request*, so any future modification that breaks any link fails here.
-- Each test names the commit whose regression it guards. Run it after any
-- change to the reasoning/replay path:
--
-- @
-- cabal test agent-deepseek --offline --test-options='--match thinking'
-- @
module Agent.DeepSeek.ThinkingModeContractSpec (spec) where

import Agent.DeepSeek.Options (defaultClientOptions)
import Agent.DeepSeek.Request (buildRequest)
import Agent.DeepSeek.Stream (buildResponse)
import Agent.Json (rawJsonFromEncoding)
import Agent.Responses.LoopBackend (withRequestInput)
import Agent.Responses.SSE (parseSseEvents)
import Agent.Responses.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = do
    describe "DeepSeek thinking-mode replay contract" do
        captureGroup
        projectionGroup
        shapeGroup

-- | Every reasoning item that reaches the wire must carry non-empty plaintext
-- @reasoning_text@ content and no @encrypted_content@; the request must not
-- ask for encrypted reasoning.
assertValidThinkingReplay :: Aeson.Value -> IO ()
assertValidThinkingReplay request = do
    object <- expectObject request
    case KeyMap.lookup "include" object of
        Nothing -> pure ()
        Just (Aeson.Array includes) ->
            includes `shouldSatisfy`
                (not . any isEncryptedReasoningInclude)
        _ -> pure ()
    input <- expectArray (KeyMap.lookup "input" object)
    mapM_ checkItem input
  where
    isEncryptedReasoningInclude = \case
        Aeson.String value -> value == "reasoning.encrypted_content"
        _ -> False
    checkItem item = do
        object <- expectObject item
        case KeyMap.lookup "type" object of
            Just (Aeson.String "reasoning") -> do
                content <- expectArray (KeyMap.lookup "content" object)
                content `shouldSatisfy` (not . null)
                mapM_ checkReasoningTextPart content
                KeyMap.lookup "encrypted_content" object `shouldBe` Nothing
            _ -> pure ()
    checkReasoningTextPart part = do
        object <- expectObject part
        KeyMap.lookup "type" object
            `shouldBe` Just (Aeson.String "reasoning_text")
        case KeyMap.lookup "text" object of
            Just (Aeson.String text) ->
                text `shouldSatisfy` (not . Text.null)
            _ -> expectationFailure "reasoning_text part is missing text"

-- | Project a follow-up request from an already-assembled transcript:
-- the response output followed by a fresh user turn, exactly what a stateless
-- tool follow-up replays.
replayRequest :: [ResponseItem] -> Aeson.Value
replayRequest transcript =
    Aeson.toJSON $
        buildRequest defaultClientOptions
            (defaultResponseCreateParams :: ResponseCreateParams)
                { model = Just "deepseek-v4-pro"
                , input = Just (ResponseInputItems (transcript <> [userTurn]))
                }

userTurn :: ResponseItem
userTurn = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts [InputTextPart "continue" Nothing]
    , role = RoleUser
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing
    }

-- | Decode + assemble one DeepSeek SSE stream into the canonical response.
assemble :: Text -> IO Response
assemble sse = do
    events <- expectRight (parseSseEvents sse)
    expectRight (buildResponse events)

-- | Run a captured transcript through the real request projection and assert
-- the wire request satisfies the thinking-mode contract.
replaysValid :: [ResponseItem] -> IO ()
replaysValid = assertValidThinkingReplay . replayRequest

-- Capture-side regressions: SSE -> assembly must yield a reasoning item that
-- survives to the wire (guards the decode/assembly links).
captureGroup :: Spec
captureGroup = do
    it "delta then a sparse output_item.done materializes buffered text (assemble link)" do
        response <- assemble
            ( sseStream "resp-a" "rs-a"
                [ sseBlock "response.output_item.added"
                    "{\"type\":\"response.output_item.added\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs-a\",\"summary\":[],\"content\":[]}}"
                , sseBlock "response.reasoning_text.delta"
                    "{\"type\":\"response.reasoning_text.delta\",\"item_id\":\"rs-a\",\"output_index\":0,\"delta\":\"checking the \"}"
                , sseBlock "response.reasoning_text.delta"
                    "{\"type\":\"response.reasoning_text.delta\",\"item_id\":\"rs-a\",\"output_index\":0,\"delta\":\"implementation\"}"
                , sseBlock "response.output_item.done"
                    "{\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs-a\",\"summary\":[],\"content\":[]}}"
                ]
            )
        reasoningContent response `shouldBe` Just "checking the implementation"
        replaysValid response.output

    it "reasoning_text.done then a sparse output_item.done keeps captured text (707e1c32)" do
        response <- assemble
            ( sseStream "resp-b" "rs-b"
                [ sseBlock "response.output_item.added"
                    "{\"type\":\"response.output_item.added\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs-b\",\"summary\":[],\"content\":[]}}"
                , sseBlock "response.reasoning_text.delta"
                    "{\"type\":\"response.reasoning_text.delta\",\"item_id\":\"rs-b\",\"output_index\":0,\"delta\":\"Let me think.\"}"
                , sseBlock "response.reasoning_text.done"
                    "{\"type\":\"response.reasoning_text.done\",\"item_id\":\"rs-b\",\"output_index\":0,\"text\":\"Let me think.\"}"
                , sseBlock "response.output_item.done"
                    "{\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs-b\",\"summary\":[],\"content\":[]}}"
                ]
            )
        reasoningContent response `shouldBe` Just "Let me think."
        replaysValid response.output

    it "fully-populated reasoning replays untouched (healthy path)" do
        response <- assemble
            ( sseStream "resp-c" "rs-c"
                [ sseBlock "response.output_item.added"
                    "{\"type\":\"response.output_item.added\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs-c\",\"summary\":[],\"content\":[]}}"
                , sseBlock "response.reasoning_text.done"
                    "{\"type\":\"response.reasoning_text.done\",\"item_id\":\"rs-c\",\"output_index\":0,\"text\":\"Thinking.\"}"
                , sseBlock "response.output_item.done"
                    "{\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs-c\",\"summary\":[],\"content\":[{\"type\":\"reasoning_text\",\"text\":\"Thinking.\"}]}}"
                ]
            )
        reasoningContent response `shouldBe` Just "Thinking."
        replaysValid response.output

    it "a turn that emitted no reasoning still replays with a non-empty marker" do
        response <- assemble
            ( sseStream "resp-d" "rs-d"
                [ sseBlock "response.output_item.added"
                    "{\"type\":\"response.output_item.added\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs-d\",\"summary\":[],\"content\":[]}}"
                , sseBlock "response.output_item.done"
                    "{\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs-d\",\"summary\":[],\"content\":[]}}"
                ]
            )
        replaysValid response.output

-- Projection-side regressions: buildRequest must satisfy the contract for
-- every historical edge case (guards the echo/include links).
projectionGroup :: Spec
projectionGroup = do
    it "encrypted blob with empty content becomes plaintext, blob dropped (77d88d12)" do
        assertValidThinkingReplay (replayRequest
            [ reasoningStored Nothing [] (Just "encrypted-payload") ])

    it "encrypted blob alongside valid content is dropped, plaintext kept" do
        assertValidThinkingReplay (replayRequest
            [ reasoningStored
                (Just [ReasoningTextPart "raw"]) [] (Just "encrypted-payload") ])

    it "empty content and empty summary yield a non-empty placeholder (b0a3d52e)" do
        assertValidThinkingReplay (replayRequest
            [ reasoningStored (Just []) [] Nothing ])

    it "absent content echoes the summary text (a4431175)" do
        response <- assemble
            ( sseStream "resp-e" "rs-e"
                [ sseBlock "response.output_item.added"
                    "{\"type\":\"response.output_item.added\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs-e\",\"summary\":[],\"content\":[]}}"
                , sseBlock "response.reasoning_summary_part.added"
                    "{\"type\":\"response.reasoning_summary_part.added\",\"item_id\":\"rs-e\",\"output_index\":0,\"summary_index\":0,\"part\":{\"type\":\"summary_text\",\"text\":\"\"}}"
                , sseBlock "response.reasoning_summary_text.done"
                    "{\"type\":\"response.reasoning_summary_text.done\",\"item_id\":\"rs-e\",\"output_index\":0,\"summary_index\":0,\"text\":\"Checked the repo.\"}"
                , sseBlock "response.output_item.done"
                    "{\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":{\"type\":\"reasoning\",\"id\":\"rs-e\",\"summary\":[],\"content\":[]}}"
                ]
            )
        assertValidThinkingReplay (replayRequest response.output)

    it "the reasoning.encrypted_content include is dropped (11b13b36)" do
        let request = (defaultResponseCreateParams :: ResponseCreateParams)
                { model = Just "deepseek-v4-pro"
                , include = Just
                    [ ResponseInclude "reasoning.encrypted_content"
                    , ResponseInclude "foo.bar"
                    ]
                , input = Just (ResponseInputItems [userTurn])
                }
        object <- expectObject
            (Aeson.toJSON (buildRequest defaultClientOptions request))
        case KeyMap.lookup "include" object of
            Nothing -> pure ()
            Just (Aeson.Array includes) ->
                foldr (:) [] includes `shouldBe` [Aeson.String "foo.bar"]
            _ -> expectationFailure "include is not an array"

-- Shape-side regressions: the sibling ways DeepSeek rejects a stateless replay
-- (guards orderToolPairs / uniquifyRequestCallIds / ensureReasoningHasFollowingItem).
shapeGroup :: Spec
shapeGroup = do
    it "a tool output is reordered to follow its call (dd4b2a9d)" do
        items <- wireInput (withRequestInput defaultResponseCreateParams
            [ functionOutput "call-1" , functionCall "call-1" "echo" ])
        let callIndex = itemIndex items (\o -> strField "type" o == Just "function_call")
            outputIndex = itemIndex items (\o -> strField "type" o == Just "function_call_output")
        callIndex `shouldSatisfy` (< outputIndex)

    it "duplicate call_ids are uniquified with outputs still paired (eda750db)" do
        items <- wireInput (withRequestInput defaultResponseCreateParams
            [ functionCall "cid" "echo"
            , functionCall "cid" "echo"
            , functionOutput "cid"
            , functionOutput "cid"
            ])
        let callIds =
                [ cid
                | item <- items
                , Just (Aeson.Object o) <- [Just item]
                , strField "type" o == Just "function_call"
                , Just (Aeson.String cid) <- [KeyMap.lookup "call_id" o]
                ]
            outputIds =
                [ cid
                | item <- items
                , Just (Aeson.Object o) <- [Just item]
                , strField "type" o == Just "function_call_output"
                , Just (Aeson.String cid) <- [KeyMap.lookup "call_id" o]
                ]
        length callIds `shouldBe` 2
        length (dedup callIds) `shouldBe` 2
        -- every output answers a call that exists in the request
        mapM_
            (\cid -> cid `shouldSatisfy` (`elem` callIds))
            outputIds

    it "a trailing reasoning item is followed by an empty assistant message" do
        items <- wireInput (withRequestInput defaultResponseCreateParams
            [ reasoningStored (Just []) [] Nothing ])
        case reverse items of
            lastItem : _ -> do
                object <- expectObject lastItem
                strField "type" object `shouldBe` Just "message"
                strField "role" object `shouldBe` Just "assistant"
            [] -> expectationFailure "expected a trailing assistant message"

-- Helpers

reasoningContent :: Response -> Maybe Text
reasoningContent response = case response.output of
    [ReasoningItemValue reasoning] ->
        case reasoning.content of
            Just [ReasoningTextPart text] -> Just text
            _ -> Nothing
    _ -> Nothing

reasoningStored
    :: Maybe [ResponseContentPart]
    -> [ReasoningSummaryPart]
    -> Maybe Text
    -> ResponseItem
reasoningStored content summary encrypted = ReasoningItemValue ReasoningItem
    { itemId = Just "rs-1"
    , summary
    , content
    , encryptedContent = encrypted
    , status = Just ItemCompleted
    }

functionCall :: Text -> Text -> ResponseItem
functionCall callId name = FunctionCallItem FunctionCall
    { itemId = Just callId
    , callId
    , name
    , namespace = Nothing
    , provider = Nothing
    , arguments = "{}"
    , encryptedFunctionArgs = Nothing
    , status = Nothing
    }

functionOutput :: Text -> ResponseItem
functionOutput callId = FunctionCallOutputItem FunctionCallOutput
    { itemId = Nothing
    , callId
    , name = Nothing
    , namespace = Nothing
    , provider = Nothing
    , output = rawJsonFromEncoding (Aeson.toEncoding (Aeson.object []))
    , status = Nothing
    }

-- | Build one DeepSeek SSE stream: created -> blocks -> completed, with the
-- terminal response echoing the reasoning item at output index 0.
sseStream :: Text -> Text -> [Text] -> Text
sseStream responseId reasoningId blocks = Text.concat
    ( sseBlock "response.created"
        ("{\"type\":\"response.created\",\"response\":{\"id\":\""
            <> responseId <> "\",\"created_at\":0,\"model\":\"deepseek-v4-pro\",\"status\":\"in_progress\"}}")
        : blocks
        <> [ sseBlock "response.completed"
            ("{\"type\":\"response.completed\",\"response\":{\"id\":\""
                <> responseId
                <> "\",\"created_at\":0,\"model\":\"deepseek-v4-pro\",\"status\":\"completed\",\"output\":[{\"type\":\"reasoning\",\"id\":\""
                <> reasoningId
                <> "\",\"summary\":[],\"content\":[]}],\"usage\":{\"input_tokens\":10,\"output_tokens\":5,\"total_tokens\":15}}}")
           ]
    )

sseBlock :: Text -> Text -> Text
sseBlock eventType dataText =
    "event: " <> eventType <> "\ndata: " <> dataText <> "\n\n"

-- | Project params through buildRequest and return the wire input array.
wireInput :: ResponseCreateParams -> IO [Aeson.Value]
wireInput params = do
    object <- expectObject (Aeson.toJSON (buildRequest defaultClientOptions params))
    expectArray (KeyMap.lookup "input" object)

strField :: Text -> Aeson.Object -> Maybe Text
strField key object = case KeyMap.lookup (keyToKey key) object of
    Just (Aeson.String value) -> Just value
    _ -> Nothing

keyToKey :: Text -> Key.Key
keyToKey = Key.fromText

itemIndex :: [Aeson.Value] -> (Aeson.Object -> Bool) -> Int
itemIndex items predicate = go 0 items
  where
    go _ [] = -1
    go index (item : rest) = case item of
        Aeson.Object object
            | predicate object -> index
        _ -> go (index + 1) rest

dedup :: Eq a => [a] -> [a]
dedup = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

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
    Right value -> pure value
    Left err -> expectationFailure ("expected Right, got Left " <> show err) >> fail "unreachable"
