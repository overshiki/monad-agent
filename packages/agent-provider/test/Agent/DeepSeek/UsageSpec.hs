module Agent.DeepSeek.UsageSpec (spec) where

import Agent.DeepSeek.Usage
import Control.Exception (finally)
import Data.IORef
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

spec :: Spec
spec = do
    describe "fetchDeepSeekUsage" do
        it "validates and reads balance from the DEEPSEEK_BASE_URL override" do
            let handler :: RecordedRequest -> IO Wai.Response
                handler request
                    | request.path == "/models" =
                        pure $ Wai.responseLBS HTTP.status200
                            [("Content-Type", "application/json")]
                            "{\"object\":\"list\",\"data\":[]}"
                    | otherwise =
                        pure $ Wai.responseLBS HTTP.status200
                            [("Content-Type", "application/json")]
                            "{\"balance_infos\":[{\"currency\":\"USD\",\"total_balance\":\"100.00\",\"granted_balance\":\"60.00\",\"topped_up_balance\":\"40.00\"}]}"
            withMockDeepSeekUsage handler \baseUrl recorded ->
                withEnv "DEEPSEEK_BASE_URL" baseUrl do
                    result <- fetchDeepSeekUsage "token-a"
                    result `shouldBe` Right DeepSeekUsage
                        { keyLabel = Nothing
                        , currency = Just "USD"
                        , totalBalance = Just "100.00"
                        , grantedBalance = Just "60.00"
                        , toppedUpBalance = Just "40.00"
                        }

                    paths <- map (.path) <$> readIORef recorded
                    paths `shouldBe` ["/models", "/user/balance"]

    describe "decodeBalances" do
        it "decodes string amounts and currency from balance_infos" do
            decodeBalances
                (LBS.pack "{\"balance_infos\":[{\"currency\":\"USD\",\"total_balance\":\"100.00\",\"granted_balance\":\"60.00\",\"topped_up_balance\":\"40.00\"}]}")
                `shouldBe` Right
                    [ BalanceInfo
                        { currency = Just "USD"
                        , totalBalance = Just "100.00"
                        , grantedBalance = Just "60.00"
                        , toppedUpBalance = Just "40.00"
                        }
                    ]

        it "treats every field as optional" do
            decodeBalances (LBS.pack "{\"balance_infos\":[{}]}")
                `shouldBe` Right
                    [ BalanceInfo
                        { currency = Nothing
                        , totalBalance = Nothing
                        , grantedBalance = Nothing
                        , toppedUpBalance = Nothing
                        }
                    ]

        it "hides parser internals for unreadable responses" do
            decodeBalances "not json"
                `shouldBe`
                    Left "DeepSeek returned an unreadable balance response."

--------------------------------------------------------------------------------
-- Mock server
--------------------------------------------------------------------------------

data RecordedRequest = RecordedRequest
    { path :: !Text
    }

-- | Run an action against a mock DeepSeek account endpoint and give it the
-- mock base URL plus the request recorder.
withMockDeepSeekUsage
    :: (RecordedRequest -> IO Wai.Response)
    -> (String -> IORef [RecordedRequest] -> IO a)
    -> IO a
withMockDeepSeekUsage handler action = do
    recorded <- newIORef []
    Warp.testWithApplication (pure (app recorded)) \port ->
        action ("http://127.0.0.1:" <> show port) recorded
  where
    app recordedRef waiRequest respond = do
        _requestBody <- Wai.strictRequestBody waiRequest
        let request = RecordedRequest
                { path = "/" <> Text.intercalate "/" (Wai.pathInfo waiRequest)
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
