module Agent.KimiCode.StreamSpec (spec) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.KimiCode.Stream
import Agent.Loop (LoopEvent(..))
import Agent.Responses.Types
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = do
    describe "parseStreamEvent" do
        it "decodes message_start" do
            parseStreamEvent
                "{\"type\":\"message_start\",\"message\":{\"id\":\"msg-1\",\"model\":\"kimi-for-coding\",\"usage\":{\"input_tokens\":10,\"output_tokens\":0}}}"
                `shouldBe` Just KcMessageStart
                    { kcMessageId = "msg-1"
                    , kcModel = "kimi-for-coding"
                    , kcInputTokens = 10
                    , kcOutputTokens = 0
                    }

        it "decodes content_block_start with a tool_use block" do
            parseStreamEvent
                "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"call-1\",\"name\":\"echo\",\"input\":{}}}"
                `shouldBe` Just KcContentBlockStart
                    { kcIndex = 0
                    , kcBlock = KcToolUseBlock
                        { kcToolUseId = "call-1"
                        , kcToolUseName = "echo"
                        }
                    }

        it "decodes message_delta with a stop reason" do
            parseStreamEvent
                "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":5}}"
                `shouldBe` Just KcMessageDelta
                    { kcStopReason = Just "end_turn"
                    , kcStopSequence = Nothing
                    , kcDeltaOutputTokens = Just 5
                    }

        it "ignores unknown event types" do
            parseStreamEvent "{\"type\":\"ping\"}" `shouldBe` Just KcPing
            parseStreamEvent "{\"type\":\"something_new\"}" `shouldBe` Nothing

    describe "parseSseBlock" do
        it "extracts the data payload from an SSE block" do
            parseSseBlock
                "event: message_stop\ndata: {\"type\":\"message_stop\"}\n"
                `shouldBe` Just KcMessageStop

        it "skips blocks without a data payload" do
            parseSseBlock "event: ping\n" `shouldBe` Nothing

    describe "stepStream" do
        it "assembles text, reasoning, and a tool call into a response" do
            let events =
                    [ KcMessageStart
                        { kcMessageId = "msg-1"
                        , kcModel = "kimi-for-coding"
                        , kcInputTokens = 10
                        , kcOutputTokens = 0
                        }
                    , KcContentBlockStart 0 KcThinkingBlock
                    , KcContentBlockDelta 0 (KcThinkingDelta "Let me think.")
                    , KcContentBlockStop 0
                    , KcContentBlockStart 1 KcTextBlock
                    , KcContentBlockDelta 1 (KcTextDelta "Hello!")
                    , KcContentBlockStop 1
                    , KcMessageDelta (Just "end_turn") Nothing (Just 7)
                    , KcMessageStop
                    ]
            response <- expectResponse (foldEvents (Just "kimi-for-coding") events)
            response.responseId `shouldBe` "msg-1"
            case response.output of
                [ ReasoningItemValue reasoning, MessageItem message ] -> do
                    reasoning.content
                        `shouldBe` Just [ReasoningTextPart "Let me think."]
                    message.role `shouldBe` RoleAssistant
                other -> expectationFailure ("expected reasoning + message, got " <> show other)
            fmap (.outputTokens) response.usage `shouldBe` Just 7

        it "assembles a tool_use stop into a function call with completed status" do
            let events =
                    [ KcMessageStart
                        { kcMessageId = "msg-2"
                        , kcModel = "kimi-for-coding"
                        , kcInputTokens = 4
                        , kcOutputTokens = 0
                        }
                    , KcContentBlockStart 0 (KcToolUseBlock "call-1" "echo")
                    , KcContentBlockDelta 0 (KcInputJsonDelta "{\"text\":")
                    , KcContentBlockDelta 0 (KcInputJsonDelta "\"hi\"}")
                    , KcContentBlockStop 0
                    , KcMessageDelta (Just "tool_use") Nothing (Just 3)
                    , KcMessageStop
                    ]
            response <- expectResponse (foldEvents (Just "kimi-for-coding") events)
            case response.output of
                [FunctionCallItem call] -> do
                    call.callId `shouldBe` "call-1"
                    call.name `shouldBe` "echo"
                    call.arguments `shouldBe` "{\"text\":\"hi\"}"
                other -> expectationFailure ("expected a function call, got " <> show other)

        it "maps a max_tokens stop to an incomplete response" do
            let events =
                    [ KcMessageStart
                        { kcMessageId = "msg-3"
                        , kcModel = "kimi-for-coding"
                        , kcInputTokens = 2
                        , kcOutputTokens = 0
                        }
                    , KcContentBlockStart 0 KcTextBlock
                    , KcContentBlockDelta 0 (KcTextDelta "trunc")
                    , KcContentBlockStop 0
                    , KcMessageDelta (Just "max_tokens") Nothing (Just 1)
                    , KcMessageStop
                    ]
            response <- expectResponse (foldEvents (Just "kimi-for-coding") events)
            response.status `shouldBe` ResponseIncomplete
            fmap (.reason) response.incompleteDetails `shouldBe` Just "max_output_tokens"

        it "turns an error event into a transport failure" do
            let events =
                    [ KcMessageStart
                        { kcMessageId = "msg-4"
                        , kcModel = "kimi-for-coding"
                        , kcInputTokens = 1
                        , kcOutputTokens = 0
                        }
                    , KcError "overloaded_error" "try again"
                    ]
            case foldEvents (Just "kimi-for-coding") events of
                Left err -> err `shouldSatisfy` isOverloaded
                Right _ -> expectationFailure "expected a transport failure"

        it "emits text and reasoning deltas as loop events" do
            let events =
                    [ KcContentBlockStart 0 KcThinkingBlock
                    , KcContentBlockDelta 0 (KcThinkingDelta "thinking...")
                    , KcContentBlockStart 1 KcTextBlock
                    , KcContentBlockDelta 1 (KcTextDelta "hello")
                    ]
            collected <- collectEvents events
            collected `shouldBe` [ReasoningDelta "thinking...", TextDelta "hello"]

isOverloaded :: ApiError -> Bool
isOverloaded = \case
    ProviderError OverloadedError _ _ -> True
    _ -> False

-- | Fold events, returning the terminal response.
foldEvents :: Maybe Text -> [KimiCodeStreamEvent] -> Either ApiError Response
foldEvents modelHint = go emptyAssembler
  where
    go _ [] = Left (ConnectionError "stream ended without a terminal event")
    go assembler (event : rest) =
        case stepStream modelHint assembler event of
            StreamContinue next _ -> go next rest
            StreamFinished result _ -> result

-- | Fold events, collecting the emitted loop events.
collectEvents :: [KimiCodeStreamEvent] -> IO [LoopEvent]
collectEvents = go emptyAssembler
  where
    go _ [] = pure []
    go assembler (event : rest) =
        case stepStream (Just "kimi-for-coding") assembler event of
            StreamContinue next emitted -> (emitted <>) <$> go next rest
            StreamFinished _ emitted -> pure emitted

expectResponse :: Either ApiError Response -> IO Response
expectResponse = \case
    Right response -> pure response
    Left err -> expectationFailure ("expected response, got " <> show err) >> fail "unreachable"
