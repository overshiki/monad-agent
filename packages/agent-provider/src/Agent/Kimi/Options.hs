-- | Configuration for the Kimi Responses transport.
module Agent.Kimi.Options
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
      -- ^ Kimi API prefix, including the version segment.
    , defaultModel :: !Text
      -- ^ Target when the request model is absent or empty.
    , requestTimeoutSeconds :: !Int
      -- ^ Full-response timeout. Reasoning turns can stream for minutes.
    } deriving (Eq, Show)

defaultClientOptions :: ClientOptions
defaultClientOptions = ClientOptions
    { baseUrl = "https://api.moonshot.ai/v1"
    , defaultModel = "kimi-k3"
    , requestTimeoutSeconds = 600
    }

-- | Load optional transport overrides from the environment.
clientOptionsFromEnv :: IO ClientOptions
clientOptionsFromEnv = do
    baseUrl <- lookupNonEmptyEnv "KIMI_BASE_URL"
    defaultModel <- lookupNonEmptyEnv "KIMI_DEFAULT_MODEL"
    timeoutSeconds <- lookupIntEnv "KIMI_TIMEOUT_SECONDS"
    pure ClientOptions
        { baseUrl = Maybe.fromMaybe defaultClientOptions.baseUrl baseUrl
        , defaultModel = maybe defaultClientOptions.defaultModel Text.pack defaultModel
        , requestTimeoutSeconds = Maybe.fromMaybe defaultClientOptions.requestTimeoutSeconds
            timeoutSeconds
        }
