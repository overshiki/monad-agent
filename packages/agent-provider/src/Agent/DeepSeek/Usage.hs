-- | Key validation and balance inspection for a DeepSeek API key.
module Agent.DeepSeek.Usage
    ( DeepSeekUsage(..)
    , BalanceInfo(..)
    , decodeBalances
    , fetchDeepSeekUsage
    ) where

import Control.Exception.Safe (tryAny)
import qualified Agent.Json.Decode as Json
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Client as HttpClient
import Network.HTTP.Simple

import Agent.DeepSeek.Options (ClientOptions(..), clientOptionsFromEnv)

-- | Account balance reported for a validated DeepSeek API key. All fields are
-- optional: a valid key whose balance cannot be read still yields a 'Right'.
data DeepSeekUsage = DeepSeekUsage
    { keyLabel :: !(Maybe Text)
    , currency :: !(Maybe Text)
    , totalBalance :: !(Maybe Text)
    , grantedBalance :: !(Maybe Text)
    , toppedUpBalance :: !(Maybe Text)
    }
    deriving (Eq, Show)

-- | One entry of DeepSeek's @balance_infos@ array. Amounts are JSON strings.
data BalanceInfo = BalanceInfo
    { currency :: !(Maybe Text)
    , totalBalance :: !(Maybe Text)
    , grantedBalance :: !(Maybe Text)
    , toppedUpBalance :: !(Maybe Text)
    }
    deriving (Eq, Show)

balancesDecoder :: Json.Decoder [BalanceInfo]
balancesDecoder = Json.object $
    Json.atKey "balance_infos" (Json.list balanceInfoDecoder)

balanceInfoDecoder :: Json.Decoder BalanceInfo
balanceInfoDecoder = Json.object $
    BalanceInfo
        <$> Json.optionalKey "currency" Json.text
        <*> Json.optionalKey "total_balance" Json.text
        <*> Json.optionalKey "granted_balance" Json.text
        <*> Json.optionalKey "topped_up_balance" Json.text

-- | Decode a successful @GET /user/balance@ response.
decodeBalances :: LBS.ByteString -> Either Text [BalanceInfo]
decodeBalances body = case Json.decodeEither balancesDecoder (LBS.toStrict body) of
    Left _ ->
        Left "DeepSeek returned an unreadable balance response."
    Right balances -> Right balances

-- | Validate an API key and fetch its account balance.
--
-- The key is validated against @GET /models@; the balance is then read from
-- @GET /user/balance@. A key that validates but has an unreadable balance
-- still reports success with empty balance fields. Both requests use the
-- configured base URL (@DEEPSEEK_BASE_URL@ when set, otherwise the default).
fetchDeepSeekUsage :: Text -> IO (Either Text DeepSeekUsage)
fetchDeepSeekUsage apiKey = do
    options <- clientOptionsFromEnv
    keyResult <- fetch options.baseUrl "/models" (Right . const ())
    case keyResult of
        Left err -> pure (Left err)
        Right () -> do
            balanceResult <- fetch options.baseUrl "/user/balance" decodeBalances
            pure $ Right $ usageFromBalances $ either (const []) id balanceResult
  where
    fetch baseUrl path decode = do
        result <- tryAny do
            request <- parseRequest (baseUrl <> path)
            httpLBS
                $ setRequestHeader
                    "Authorization"
                    ["Bearer " <> Text.encodeUtf8 apiKey]
                $ setRequestHeader "Accept" ["application/json"]
                $ setRequestResponseTimeout
                    (HttpClient.responseTimeoutMicro (30 * 1_000_000))
                    request
        pure case result of
            Left _ ->
                Left
                    "Could not load DeepSeek usage. Check your connection and retry."
            Right response
                | let status = getResponseStatusCode response
                , status >= 200
                , status < 300 ->
                    decode (getResponseBody response)
                | otherwise ->
                    Left
                        ("DeepSeek usage returned HTTP "
                            <> Text.pack (show (getResponseStatusCode response)))

usageFromBalances :: [BalanceInfo] -> DeepSeekUsage
usageFromBalances balances = DeepSeekUsage
    { keyLabel = Nothing
    , currency = firstOf (.currency)
    , totalBalance = firstOf (.totalBalance)
    , grantedBalance = firstOf (.grantedBalance)
    , toppedUpBalance = firstOf (.toppedUpBalance)
    }
  where
    firstOf :: (BalanceInfo -> Maybe Text) -> Maybe Text
    firstOf field = case balances of
        (balance : _) -> field balance
        [] -> Nothing
