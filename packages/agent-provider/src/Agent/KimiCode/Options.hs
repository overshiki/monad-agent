-- | Configuration for the Kimi Code Anthropic Messages transport.
module Agent.KimiCode.Options
    ( ClientOptions(..)
    , defaultClientOptions
    , clientOptionsFromEnv
    ) where

import Agent.Provider.Options
    ( lookupIntEnv
    , lookupNonEmptyEnv
    )
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text

data ClientOptions = ClientOptions
    { baseUrl :: !String
      -- ^ Kimi Code API prefix, including the @/v1@ version segment.
    , defaultModel :: !Text
      -- ^ Target when the request model is absent or empty.
    , requestTimeoutSeconds :: !Int
      -- ^ Full-response timeout. Reasoning turns can stream for minutes.
    } deriving (Eq, Show)

defaultClientOptions :: ClientOptions
defaultClientOptions = ClientOptions
    { baseUrl = "https://api.kimi.com/coding/v1"
    , defaultModel = "kimi-for-coding"
    , requestTimeoutSeconds = 600
    }

-- | Load optional transport overrides from the environment.
clientOptionsFromEnv :: IO ClientOptions
clientOptionsFromEnv = do
    baseUrl <- lookupNonEmptyEnv "KIMI_CODE_BASE_URL"
    defaultModel <- lookupNonEmptyEnv "KIMI_CODE_DEFAULT_MODEL"
    timeoutSeconds <- lookupIntEnv "KIMI_CODE_TIMEOUT_SECONDS"
    pure ClientOptions
        { baseUrl = Maybe.fromMaybe defaultClientOptions.baseUrl baseUrl
        , defaultModel = maybe defaultClientOptions.defaultModel Text.pack defaultModel
        , requestTimeoutSeconds = Maybe.fromMaybe defaultClientOptions.requestTimeoutSeconds
            timeoutSeconds
        }
