module Agent.KimiCode.ErrorSpec (spec) where

import Agent.Error (ApiError(..), ErrorType(..))
import Agent.KimiCode.Error
import Test.Hspec

spec :: Spec
spec = do
    describe "classifyFailure" do
        it "maps a 401 Anthropic envelope to an authentication error" do
            classifyFailure 401 Nothing
                "{\"type\":\"error\",\"error\":{\"type\":\"authentication_error\",\"message\":\"invalid x-api-key\"}}"
                `shouldBe` ProviderError AuthenticationError
                    "invalid x-api-key"
                    Nothing

        it "maps a 429 to a rate-limit error" do
            classifyFailure 429 Nothing
                "{\"type\":\"error\",\"error\":{\"type\":\"rate_limit_error\",\"message\":\"slow down\"}}"
                `shouldBe` ProviderError RateLimitError
                    "slow down"
                    Nothing

        it "maps a 402 to a billing error" do
            classifyFailure 402 Nothing
                "{\"type\":\"error\",\"error\":{\"type\":\"permission_error\",\"message\":\"quota exhausted\"}}"
                `shouldBe` ProviderError BillingError
                    "quota exhausted"
                    Nothing

        it "falls back to a generic HTTP error without an envelope" do
            case classifyFailure 500 Nothing "boom" of
                HttpError status _ -> status `shouldBe` 500
                other -> expectationFailure ("expected HttpError, got " <> show other)

    describe "classifyStreamError" do
        it "maps overloaded errors to a retryable overload error" do
            classifyStreamError "overloaded_error" "try again"
                `shouldBe` ProviderError OverloadedError "try again" Nothing

        it "maps unknown stream errors to a connection error" do
            case classifyStreamError "weird_error" "something broke" of
                ConnectionError message ->
                    message `shouldSatisfy` (/= "")
                other -> expectationFailure ("expected ConnectionError, got " <> show other)
