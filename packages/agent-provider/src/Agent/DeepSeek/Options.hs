-- | Configuration for the DeepSeek Responses transport.
module Agent.DeepSeek.Options
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
      -- ^ DeepSeek API prefix, without a version segment.
    , defaultModel :: !Text
      -- ^ Target when the request model is absent or empty.
    , requestTimeoutSeconds :: !Int
      -- ^ Full-response timeout. Reasoning turns can stream for minutes.
    } deriving (Eq, Show)

defaultClientOptions :: ClientOptions
defaultClientOptions = ClientOptions
    { baseUrl = "https://api.deepseek.com"
    , defaultModel = "deepseek-v4-flash"
    , requestTimeoutSeconds = 600
    }

-- | Load optional transport overrides from the environment.
clientOptionsFromEnv :: IO ClientOptions
clientOptionsFromEnv = do
    baseUrl <- lookupNonEmptyEnv "DEEPSEEK_BASE_URL"
    defaultModel <- lookupNonEmptyEnv "DEEPSEEK_DEFAULT_MODEL"
    timeoutSeconds <- lookupIntEnv "DEEPSEEK_TIMEOUT_SECONDS"
    pure ClientOptions
        { baseUrl = Maybe.fromMaybe defaultClientOptions.baseUrl baseUrl
        , defaultModel = maybe defaultClientOptions.defaultModel Text.pack defaultModel
        , requestTimeoutSeconds = Maybe.fromMaybe defaultClientOptions.requestTimeoutSeconds
            timeoutSeconds
        }
