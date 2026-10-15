-- | Key validation for a Kimi API key.
module Agent.Kimi.Usage
    ( KimiUsage(..)
    , fetchKimiUsage
    , fetchKimiUsageFromBase
    ) where

import Control.Exception.Safe (tryAny)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Client as HttpClient
import Network.HTTP.Simple

import Agent.Kimi.Options (ClientOptions(..), clientOptionsFromEnv)

-- | Result of validating a Kimi API key. Kimi has no balance endpoint, so
-- only the key check is performed and no account details are reported.
data KimiUsage = KimiUsage
    { keyLabel :: !(Maybe Text)
    }
    deriving (Eq, Show)

-- | Validate an API key against @GET /models@ using the configured base URL
-- (@KIMI_BASE_URL@ when set, e.g. the China endpoint, otherwise the default).
fetchKimiUsage :: Text -> IO (Either Text KimiUsage)
fetchKimiUsage apiKey = do
    options <- clientOptionsFromEnv
    fetchKimiUsageFromBase options.baseUrl apiKey

-- | Validate an API key against @GET /models@ on an explicit base URL.
-- A non-2xx status rejects the key; a 2xx status reports success with no
-- account details.
fetchKimiUsageFromBase :: String -> Text -> IO (Either Text KimiUsage)
fetchKimiUsageFromBase baseUrl apiKey = do
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
                "Could not load Kimi usage. Check your connection and retry."
        Right response
            | let status = getResponseStatusCode response
            , status >= 200
            , status < 300 ->
                Right KimiUsage { keyLabel = Nothing }
            | otherwise ->
                Left
                    ("Kimi usage returned HTTP "
                        <> Text.pack (show (getResponseStatusCode response)))
