module Agent.DeepSeek.CredentialSpec (spec) where

import Agent.Error
    ( ApiError(..)
    , CredentialExhaustionReason(..)
    , ErrorType(..)
    )
import Agent.DeepSeek.Credential
import Agent.Provider
import Data.Time.Clock (addUTCTime, getCurrentTime)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

spec :: Spec
spec = do
    describe "staticApiKeyProvider" do
        it "returns the same bearer without an account id" do
            let provider = staticApiKeyProvider "ds-key"
            tokenProviderBillingMode provider `shouldBe` ApiBilled
            result <- getNextToken provider Nothing
            case result of
                Right credential -> do
                    credential.accessToken `shouldBe` "ds-key"
                    credential.accountId `shouldBe` ""
                    credential.leaseId `shouldBe` Nothing
                    credential.provider `shouldBe` DeepSeekProvider
                Left err -> expectationFailure ("expected credential, got " <> show err)

        it "surfaces rate limits as CredentialsExhausted instead of cycling the same key" do
            now <- getCurrentTime
            let provider = staticApiKeyProvider "ds-key"
            first <- expectCredential =<< getNextToken provider Nothing
            exhausted <- getNextToken provider
                (Just FailedCredential
                    { credential = first
                    , failure = AccountRateLimited (Just 90)
                    , failureReason = ExhaustedByRateLimit
                        { exhaustionErrorType = Just RateLimitError
                        , exhaustionStatusCode = Just 429
                        , exhaustionRetryAfter = Just 90
                        }
                    })
            case exhausted of
                Left CredentialsExhausted { retryAt } ->
                    retryAt `shouldSatisfy` (> addUTCTime 80 now)
                other -> expectationFailure ("expected CredentialsExhausted, got " <> show other)

        it "does not treat a rejected static key as recoverable" do
            let provider = staticApiKeyProvider "ds-key"
            first <- expectCredential =<< getNextToken provider Nothing
            rejected <- getNextToken provider
                (Just FailedCredential
                    { credential = first
                    , failure = AccountAuthenticationRejected
                    , failureReason = ExhaustedByAuthentication
                        { exhaustionErrorType = Just AuthenticationError
                        , exhaustionStatusCode = Just 401
                        }
                    })
            case rejected of
                Left (ProviderError AuthenticationError _ _) -> pure ()
                other -> expectationFailure
                    ("expected AuthenticationError, got " <> show other)

        it "builds a DeepSeek credential from an API key" do
            let credential = credentialFromApiKey "sk-ds-test"
            credential.provider `shouldBe` DeepSeekProvider
            credential.accessToken `shouldBe` "sk-ds-test"

    describe "credentialFromEnv" do
        it "returns Nothing when DEEPSEEK_API_KEY is unset" do
            withEnv "DEEPSEEK_API_KEY" Nothing do
                result <- credentialFromEnv
                result `shouldBe` Nothing

        it "returns Nothing when DEEPSEEK_API_KEY is empty" do
            withEnv "DEEPSEEK_API_KEY" (Just "") do
                result <- credentialFromEnv
                result `shouldBe` Nothing

        it "returns a DeepSeek credential when DEEPSEEK_API_KEY is set" do
            withEnv "DEEPSEEK_API_KEY" (Just "sk-ds-env") do
                result <- credentialFromEnv
                fmap (.accessToken) result `shouldBe` Just "sk-ds-env"
                fmap (.provider) result `shouldBe` Just DeepSeekProvider
                fmap (.accountId) result `shouldBe` Just ""

expectCredential :: Either ApiError Credential -> IO Credential
expectCredential = \case
    Right credential -> pure credential
    Left err -> expectationFailure ("expected credential, got " <> show err) >> fail "unreachable"

withEnv :: String -> Maybe String -> IO a -> IO a
withEnv name value action = do
    old <- lookupEnv name
    set value
    result <- action
    set old
    pure result
  where
    set (Just value) = setEnv name value
    set Nothing = unsetEnv name
