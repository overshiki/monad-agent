module Agent.Kimi.ErrorSpec (spec) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.Kimi.Error
import Agent.Kimi.Stream
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = describe "classifyFailure" do
    it "types a bare 429 and honours the Retry-After header" do
        classifyFailure 429 (Just 90) "too many requests"
            `shouldBe` ProviderError RateLimitError "too many requests" (Just 90)

    it "keeps a typed OpenAI envelope and only fills a missing retry interval" do
        let bare = "{\"error\":{\"type\":\"rate_limit_error\",\"message\":\"limited\"}}"
        classifyFailure 429 (Just 90) bare
            `shouldBe` ProviderError RateLimitError "limited" (Just 90)
        let envelope = "{\"error\":{\"type\":\"rate_limit_error\",\"message\":\"limited\",\"retry_after\":300}}"
        classifyFailure 429 Nothing envelope
            `shouldBe` ProviderError RateLimitError "limited" (Just 300)

    it "maps 401 and 403 to authentication errors from status" do
        case classifyFailure 401 Nothing "nope" of
            ProviderError AuthenticationError _ _ -> pure ()
            other -> expectationFailure ("expected AuthenticationError, got " <> show other)
        case classifyFailure 403 Nothing "forbidden" of
            ProviderError AuthenticationError _ _ -> pure ()
            other -> expectationFailure ("expected AuthenticationError, got " <> show other)

    it "maps 402 to a billing error, keeping a decoded message" do
        case classifyFailure 402 Nothing "pay up" of
            ProviderError BillingError _ _ -> pure ()
            other -> expectationFailure ("expected BillingError, got " <> show other)
        let body = "{\"error\":{\"type\":\"insufficient_quota\",\"message\":\"out of quota\"}}"
        classifyFailure 402 Nothing body
            `shouldBe` ProviderError BillingError "out of quota" Nothing

    it "pins decoded errors on auth, billing, and rate-limit statuses" do
        let body = "{\"error\":{\"type\":\"server_error\",\"message\":\"bad key\"}}"
        case classifyFailure 401 Nothing body of
            ProviderError AuthenticationError "bad key" _ -> pure ()
            other -> expectationFailure ("expected AuthenticationError, got " <> show other)

    it "surfaces typed stream errors" do
        events <- expectRight $ parseSseEvents $ sseBlock "error"
            "{\"type\":\"error\",\"error\":{\"type\":\"rate_limit_error\",\"message\":\"limited\",\"resets_in_seconds\":120}}"
        buildResponse events
            `shouldBe` Left (ProviderError RateLimitError "limited" (Just 120))

    it "leaves other statuses as plain HTTP errors" do
        classifyFailure 503 Nothing "unavailable"
            `shouldBe` HttpError 503 "unavailable"

sseBlock :: Text -> Text -> Text
sseBlock eventType dataText =
    "event: " <> eventType <> "\ndata: " <> dataText <> "\n\n"

expectRight :: Show e => Either e a -> IO a
expectRight = \case
    Left err -> expectationFailure ("expected Right, got Left " <> show err) >> fail "unreachable"
    Right value -> pure value
