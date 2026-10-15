module Agent.Kimi.CredentialSpec (spec) where

import Agent.Error
    ( ApiError(..)
    , CredentialExhaustionReason(..)
    , ErrorType(..)
    )
import Agent.Kimi.Credential
import Agent.Provider
import Data.Time.Clock (addUTCTime, getCurrentTime)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

spec :: Spec
spec = do
    describe "staticApiKeyProvider" do
        it "returns the same bearer without an account id" do
            let provider = staticApiKeyProvider "kimi-key"
            tokenProviderBillingMode provider `shouldBe` ApiBilled
            result <- getNextToken provider Nothing
            case result of
                Right credential -> do
                    credential.accessToken `shouldBe` "kimi-key"
                    credential.accountId `shouldBe` ""
                    credential.leaseId `shouldBe` Nothing
                    credential.provider `shouldBe` KimiProvider
                Left err -> expectationFailure ("expected credential, got " <> show err)

        it "surfaces rate limits as CredentialsExhausted instead of cycling the same key" do
            now <- getCurrentTime
            let provider = staticApiKeyProvider "kimi-key"
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
            let provider = staticApiKeyProvider "kimi-key"
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

        it "builds a Kimi credential from an API key" do
            let credential = credentialFromApiKey "sk-kimi-test"
            credential.provider `shouldBe` KimiProvider
            credential.accessToken `shouldBe` "sk-kimi-test"

    describe "credentialFromEnv" do
        it "returns Nothing when both env vars are unset" do
            withEnv "MOONSHOT_API_KEY" Nothing do
                withEnv "KIMI_API_KEY" Nothing do
                    result <- credentialFromEnv
                    result `shouldBe` Nothing

        it "returns Nothing when MOONSHOT_API_KEY is empty" do
            withEnv "MOONSHOT_API_KEY" (Just "") do
                result <- credentialFromEnv
                result `shouldBe` Nothing

        it "returns a Kimi credential when MOONSHOT_API_KEY is set" do
            withEnv "MOONSHOT_API_KEY" (Just "sk-kimi-env") do
                result <- credentialFromEnv
                fmap (.accessToken) result `shouldBe` Just "sk-kimi-env"
                fmap (.provider) result `shouldBe` Just KimiProvider
                fmap (.accountId) result `shouldBe` Just ""

        it "falls back to KIMI_API_KEY when MOONSHOT_API_KEY is unset" do
            withEnv "MOONSHOT_API_KEY" Nothing do
                withEnv "KIMI_API_KEY" (Just "sk-kimi-fallback") do
                    result <- credentialFromEnv
                    fmap (.accessToken) result `shouldBe` Just "sk-kimi-fallback"
                    fmap (.provider) result `shouldBe` Just KimiProvider

        it "prefers MOONSHOT_API_KEY over KIMI_API_KEY" do
            withEnv "MOONSHOT_API_KEY" (Just "sk-moonshot") do
                withEnv "KIMI_API_KEY" (Just "sk-kimi-fallback") do
                    result <- credentialFromEnv
                    fmap (.accessToken) result `shouldBe` Just "sk-moonshot"

        it "ignores an empty KIMI_API_KEY fallback" do
            withEnv "MOONSHOT_API_KEY" Nothing do
                withEnv "KIMI_API_KEY" (Just "") do
                    result <- credentialFromEnv
                    result `shouldBe` Nothing

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
