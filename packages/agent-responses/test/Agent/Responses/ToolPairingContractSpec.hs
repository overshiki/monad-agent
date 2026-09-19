-- | Systematic guard for the stateless tool call/output pairing contract.
--
-- Every stateless Responses host (DeepSeek, Kimi, OpenRouter, generic)
-- requires each tool call in the replayed transcript to be answered by a tool
-- output with the same @call_id@, and rejects the whole request otherwise:
--
-- @
-- an assistant message with 'tool_calls' must be followed by tool messages
-- responding to each 'tool_call_id'. The following tool_call_ids did not
-- have response messages: ...
-- @
--
-- A tool call can reach the wire without its result when a turn is
-- interrupted/retried, a result is lost, or call-id dedup mis-pairs. This
-- spec asserts the contract on the *projected wire request* so any future
-- modification that reintroduces a dangling call fails here. Each test names
-- the regression it guards. Run after any change to the replay path:
--
-- @
-- cabal test agent-responses --offline --test-options='--match pairing'
-- @
module Agent.Responses.ToolPairingContractSpec (spec) where

import Agent.Json (RawJson, rawJsonFromEncoding)
import Agent.Responses.LoopBackend (withRequestInput)
import Agent.Responses.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec =
    describe "stateless tool call/output pairing contract" do
        it "a tool call with no result is closed with a not-executed output (universal guard)" do
            input <- project [functionCall "run_terminal_cmd:5" "run_terminal_cmd"]
            assertValidToolPairing input

        it "a custom tool call with no result is closed too" do
            input <- project [customCall "lookup_1" "lookup"]
            assertValidToolPairing input

        it "an output before its call is reordered after it (dd4b2a9d)" do
            input <- project
                [ functionOutput "call-1"
                , functionCall "call-1" "echo"
                ]
            assertValidToolPairing input

        it "duplicate call ids are uniquified with outputs still paired (eda750db)" do
            input <- project
                [ functionCall "cid" "echo"
                , functionCall "cid" "echo"
                , functionOutput "cid"
                , functionOutput "cid"
                ]
            assertValidToolPairing input

        it "a duplicate call missing one result still leaves no bare call" do
            input <- project
                [ functionCall "cid" "echo"
                , functionCall "cid" "echo"
                , functionOutput "cid"
                ]
            assertValidToolPairing input

        it "an orphan output is emitted exactly once (double-emission fix)" do
            input <- project
                [ functionCall "call-1" "echo"
                , functionOutput "call-1"
                , functionOutput "orphan-9"
                ]
            assertValidToolPairing input
            countOutputs "orphan-9" input `shouldBe` 1

        it "a healthy call/result turn passes unchanged" do
            input <- project
                [ userMessage "run the tests"
                , functionCall "run_terminal_cmd:5" "run_terminal_cmd"
                , functionOutput "run_terminal_cmd:5"
                , assistantText "done"
                ]
            assertValidToolPairing input
            length input `shouldBe` 4

-- | Project an item list through the real request assembly and return the
-- wire input array.
project :: [ResponseItem] -> IO [Aeson.Value]
project items = do
    object <- expectObject
        (Aeson.toJSON (withRequestInput defaultResponseCreateParams items))
    expectArray (KeyMap.lookup "input" object)

-- | Every tool call must be immediately followed by a tool output with the
-- same @call_id@, and call ids must be unique. (An orphan output — one whose
-- call id matches no call — is tolerated at most once by design and is
-- checked separately.)
assertValidToolPairing :: [Aeson.Value] -> IO ()
assertValidToolPairing input = do
    let callEntries =
            [ (index, cid)
            | (index, item) <- zip [0 :: Int ..] input
            , Just cid <- [callIdOf item]
            ]
        cids = map snd callEntries
    length cids `shouldBe` length (dedup cids)
    mapM_ (assertAnswered input) callEntries
  where
    assertAnswered items (index, cid) =
        case drop (index + 1) items of
            next : _ ->
                case outputIdOf next of
                    Just outCid | outCid == cid -> pure ()
                    _ -> expectationFailure
                        ("call " <> Text.unpack cid
                            <> " is not followed by its output")
            [] -> expectationFailure
                ("call " <> Text.unpack cid <> " has no following output")

countOutputs :: Text -> [Aeson.Value] -> Int
countOutputs cid items =
    length [ () | item <- items, outputIdOf item == Just cid ]

callIdOf :: Aeson.Value -> Maybe Text
callIdOf item = do
    object <- valueObject item
    case textAt "type" object of
        Just t | t == "function_call" || t == "custom_tool_call" ->
            textAt "call_id" object
        _ -> Nothing

outputIdOf :: Aeson.Value -> Maybe Text
outputIdOf item = do
    object <- valueObject item
    case textAt "type" object of
        Just t | t == "function_call_output" || t == "custom_tool_call_output" ->
            textAt "call_id" object
        _ -> Nothing

-- Item builders

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
    , output = rawOutput "ok"
    , status = Nothing
    }

customCall :: Text -> Text -> ResponseItem
customCall callId name = CustomToolCallItem CustomToolCall
    { itemId = Nothing
    , callId
    , name
    , namespace = Nothing
    , input = "{}"
    , status = Nothing
    }

userMessage :: Text -> ResponseItem
userMessage text = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts [InputTextPart text Nothing]
    , role = RoleUser
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing
    }

assistantText :: Text -> ResponseItem
assistantText text = MessageItem ResponseMessage
    { messageId = Nothing
    , content = MessageContentParts [OutputTextPart text Nothing Nothing]
    , role = RoleAssistant
    , status = Nothing
    , phase = Nothing
    , passthrough = Nothing
    }

rawOutput :: Text -> RawJson
rawOutput = rawJsonFromEncoding . Aeson.toEncoding

-- JSON helpers

valueObject :: Aeson.Value -> Maybe Aeson.Object
valueObject = \case
    Aeson.Object object -> Just object
    _ -> Nothing

textAt :: Text -> Aeson.Object -> Maybe Text
textAt key object = case KeyMap.lookup (Key.fromText key) object of
    Just (Aeson.String value) -> Just value
    _ -> Nothing

expectObject :: Aeson.Value -> IO Aeson.Object
expectObject = \case
    Aeson.Object object -> pure object
    other -> expectationFailure ("expected object, got " <> show other) >> fail "unreachable"

expectArray :: Maybe Aeson.Value -> IO [Aeson.Value]
expectArray = \case
    Just (Aeson.Array values) -> pure (foldr (:) [] values)
    other -> expectationFailure ("expected array, got " <> show other) >> fail "unreachable"

dedup :: Eq a => [a] -> [a]
dedup = foldr (\x acc -> if x `elem` acc then acc else x : acc) []
