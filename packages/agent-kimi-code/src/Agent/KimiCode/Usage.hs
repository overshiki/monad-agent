-- | Key validation for a Kimi Code API key.
module Agent.KimiCode.Usage
    ( KimiCodeUsage(..)
    , fetchKimiCodeUsage
    , fetchKimiCodeUsageFromBase
    ) where

import Agent.KimiCode.Options (ClientOptions(..), clientOptionsFromEnv)
import Control.Exception.Safe (tryAny)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Client as HttpClient
import Network.HTTP.Simple

-- | Result of validating a Kimi Code API key. Kimi Code has no public balance
-- endpoint, so only the key check is performed and no account details are
-- reported.
data KimiCodeUsage = KimiCodeUsage
    { keyLabel :: !(Maybe Text)
    }
    deriving (Eq, Show)

-- | Validate an API key against @GET /models@ using the configured base URL
-- (@KIMI_CODE_BASE_URL@ when set, otherwise the default).
fetchKimiCodeUsage :: Text -> IO (Either Text KimiCodeUsage)
fetchKimiCodeUsage apiKey = do
    options <- clientOptionsFromEnv
    fetchKimiCodeUsageFromBase options.baseUrl apiKey

-- | Validate an API key against @GET /models@ on an explicit base URL.
-- A non-2xx status rejects the key; a 2xx status reports success with no
-- account details.
fetchKimiCodeUsageFromBase :: String -> Text -> IO (Either Text KimiCodeUsage)
fetchKimiCodeUsageFromBase baseUrl apiKey = do
    result <- tryAny do
        request <- parseRequest (baseUrl <> "/models")
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
                "Could not load Kimi Code usage. Check your connection and retry."
        Right response
            | let status = getResponseStatusCode response
            , status >= 200
            , status < 300 ->
                Right KimiCodeUsage { keyLabel = Nothing }
            | otherwise ->
                Left
                    ("Kimi Code usage returned HTTP "
                        <> Text.pack (show (getResponseStatusCode response)))
