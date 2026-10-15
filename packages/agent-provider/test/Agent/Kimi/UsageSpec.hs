-- | Functional tests for Kimi key validation against an in-process HTTP mock.
module Agent.Kimi.UsageSpec (spec) where

import Agent.Kimi.Usage
import Control.Exception (finally)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

spec :: Spec
spec = do
    describe "fetchKimiUsageFromBase" do
        it "accepts a 2xx /models response and reports no account details" do
            let handler _request = pure $ Wai.responseLBS HTTP.status200
                    [("Content-Type", "application/json")]
                    "{\"object\":\"list\",\"data\":[]}"
            withMockKimiUsage handler \baseUrl recorded -> do
                result <- fetchKimiUsageFromBase baseUrl "token-a"
                result `shouldBe` Right KimiUsage { keyLabel = Nothing }

                [request] <- readIORef recorded
                request.path `shouldBe` "/models"
                lookup "Authorization" request.headers `shouldBe` Just "Bearer token-a"

        it "rejects a 401 /models response" do
            let handler _request = pure $ Wai.responseLBS HTTP.status401
                    [("Content-Type", "application/json")]
                    "{\"error\":{\"type\":\"authentication_error\",\"message\":\"bad key\"}}"
            withMockKimiUsage handler \baseUrl _recorded -> do
                result <- fetchKimiUsageFromBase baseUrl "bad"
                result `shouldBe` Left "Kimi usage returned HTTP 401"

        it "rejects any non-2xx /models response with its status" do
            let handler _request = pure $ Wai.responseLBS HTTP.status500
                    [("Content-Type", "text/plain")]
                    "server error"
            withMockKimiUsage handler \baseUrl _recorded -> do
                result <- fetchKimiUsageFromBase baseUrl "token-a"
                result `shouldBe` Left "Kimi usage returned HTTP 500"

        it "reports connection failures without exposing internals" do
            result <- fetchKimiUsageFromBase "http://127.0.0.1:1" "token-a"
            result `shouldBe` Left
                "Could not load Kimi usage. Check your connection and retry."

    describe "fetchKimiUsage" do
        it "validates against the KIMI_BASE_URL override when set" do
            let handler _request = pure $ Wai.responseLBS HTTP.status200
                    [("Content-Type", "application/json")]
                    "{\"object\":\"list\",\"data\":[]}"
            withMockKimiUsage handler \baseUrl recorded ->
                withEnv "KIMI_BASE_URL" baseUrl do
                    result <- fetchKimiUsage "token-a"
                    result `shouldBe` Right KimiUsage { keyLabel = Nothing }

                    [request] <- readIORef recorded
                    request.path `shouldBe` "/models"
                    lookup "Authorization" request.headers
                        `shouldBe` Just "Bearer token-a"

--------------------------------------------------------------------------------
-- Mock server
--------------------------------------------------------------------------------

data RecordedRequest = RecordedRequest
    { path :: !Text
    , headers :: ![(Text, Text)]
    }

-- | Run an action against a mock Kimi account endpoint and give it the mock
-- base URL plus the request recorder.
withMockKimiUsage
    :: (RecordedRequest -> IO Wai.Response)
    -> (String -> IORef [RecordedRequest] -> IO a)
    -> IO a
withMockKimiUsage handler action = do
    recorded <- newIORef []
    Warp.testWithApplication (pure (app recorded)) \port ->
        action ("http://127.0.0.1:" <> show port) recorded
  where
    app recordedRef waiRequest respond = do
        _requestBody <- Wai.strictRequestBody waiRequest
        let request = RecordedRequest
                { path = "/" <> Text.intercalate "/" (Wai.pathInfo waiRequest)
                , headers =
                    [ (Text.decodeUtf8 (CI.original name), Text.decodeUtf8 value)
                    | (name, value) <- Wai.requestHeaders waiRequest
                    ]
                }
        atomicModifyIORef' recordedRef \requests -> (requests <> [request], ())
        respond =<< handler request

-- | Run an action with an environment variable set, restoring the previous
-- value (or unset state) afterwards.
withEnv :: String -> String -> IO a -> IO a
withEnv name value action = do
    old <- lookupEnv name
    setEnv name value
    action `finally` case old of
        Just previous -> setEnv name previous
        Nothing -> unsetEnv name
