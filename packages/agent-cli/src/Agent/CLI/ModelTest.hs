-- | @/model-test@: probe the session's configured models — across every
-- connection with an available credential — and report which respond now.
--
-- A probe is one real, minimal model request (no tools, no inherited
-- transcript) through the same out-of-band backend path @/btw@ uses, so
-- "responds" means the actual request path worked, not just that an account
-- usage endpoint answered. The pure parts (target selection, outcome
-- classification, line rendering) live here so they can be unit-tested;
-- @Session/Interaction.hs@ owns the I/O (timeouts, concurrency, cancel).
module Agent.CLI.ModelTest
    ( ModelTestTarget(..)
    , catalogTargets
    , matchesFilters
    , ProbeOutcome(..)
    , classifyProbe
    , probeStatusText
    , formatProbeLine
    , modelTestQuestion
    , defaultProbeTimeoutMicros
    , targetId
    , targetConn
    , targetProvider
    ) where

import Agent.CLI.Btw (BtwError(..))
import Agent.CLI.Models
    ( ModelOption(..)
    , ModelTarget(..)
    , modelCatalog
    )
import Agent.CLI.ModelConfig (ModelCatalog)
import Agent.CLI.Error (formatApiErrorInline)
import Agent.Provider (Provider)
import Data.List (nubBy, sortBy)
import Data.Text (Text)
import qualified Data.Text as Text

-- | A resolved probe target: the model and the provider/connection that own
-- it.
data ModelTestTarget = ModelTestTarget
    { probeModelId :: !Text
    , probeConnectionId :: !Text
    , probeProvider :: !Provider
    }
    deriving (Eq, Show)

-- Field selectors are suppressed by NoFieldSelectors; these accessors keep
-- the module record-dot free.
targetId :: ModelTestTarget -> Text
targetId (ModelTestTarget modelId _ _) = modelId

targetConn :: ModelTestTarget -> Text
targetConn (ModelTestTarget _ connectionId _) = connectionId

targetProvider :: ModelTestTarget -> Provider
targetProvider (ModelTestTarget _ _ provider) = provider

-- | Every catalog model (deduplicated by model id and sorted), regardless of
-- provider/connection. These are the models a subagent could be dispatched
-- to; /model-test probes them all.
catalogTargets :: ModelCatalog -> [ModelTestTarget]
catalogTargets catalog =
    [ ModelTestTarget id' conn provider'
    | ModelOption{ modelTarget = target } <- modelCatalog catalog
    , let id' = target.targetModelId
    , let conn = target.targetConnectionId
    , let provider' = target.targetProvider
    ]
    |> nubBy (\left right -> targetId left == targetId right)
    |> sortBy (\left right -> compare (targetId left) (targetId right))
  where
    (|>) value function = function value

-- | Keep targets whose model id or connection id contains any filter
-- (case-insensitive). No filters means keep everything.
matchesFilters :: [Text] -> ModelTestTarget -> Bool
matchesFilters filters target =
    null filters
        || any
            (\filterText ->
                filterText
                    `Text.isInfixOf` Text.toLower (targetId target)
                    || filterText
                        `Text.isInfixOf` Text.toLower (targetConn target))
            (map Text.toLower filters)

-- | One probe's verdict. Transport keeps a short human-readable detail.
data ProbeOutcome
    = ProbeOk
    | ProbeTimeout
    | ProbeCancelled
    | ProbeEmpty
    | ProbeToolCall
    | ProbeInvalid
    | ProbeSkipped !Text
    | ProbeTransport !Text
    deriving (Eq, Show)

-- | Classify a @/btw@-style probe result into a verdict.
classifyProbe :: Either BtwError Text -> ProbeOutcome
classifyProbe = \case
    Right _ -> ProbeOk
    Left err -> case err of
        BtwTransport apiError -> ProbeTransport (formatApiErrorInline apiError)
        BtwCancelled -> ProbeCancelled
        BtwEmptyResponse -> ProbeEmpty
        BtwUnexpectedToolCall -> ProbeToolCall
        BtwInvalidResponse -> ProbeInvalid

-- | Short status word used in the table.
probeStatusText :: ProbeOutcome -> Text
probeStatusText = \case
    ProbeOk -> "ok"
    ProbeTimeout -> "timeout"
    ProbeCancelled -> "cancelled"
    ProbeEmpty -> "empty"
    ProbeToolCall -> "tool call"
    ProbeInvalid -> "invalid"
    ProbeSkipped _ -> "skipped"
    ProbeTransport _ -> "failed"

-- | The minimal question each probe sends. Kept tiny: the reply only needs
-- to prove the request path works.
modelTestQuestion :: Text
modelTestQuestion = "Reply with exactly: ok"

-- | Per-probe wall-clock budget.
defaultProbeTimeoutMicros :: Int
defaultProbeTimeoutMicros = 20_000_000

-- | One fixed-width table row: model, connection, status, latency, detail.
formatProbeLine
    :: ModelTestTarget
    -> Maybe Int
    -- ^ latency in milliseconds, when measured
    -> ProbeOutcome
    -> Text
formatProbeLine target latencyMs outcome =
    Text.stripEnd $
        pad 28 (targetId target)
            <> pad 16 (targetConn target)
            <> pad 10 (probeStatusText outcome)
            <> maybe (pad 8 "") (\ms -> pad 8 (Text.pack (show ms <> " ms"))) latencyMs
            <> detail
  where
    detail = case outcome of
        ProbeTransport text -> text
        ProbeSkipped text -> text
        ProbeEmpty -> "model returned no text"
        ProbeToolCall -> "model tried to call a tool; no tools were run"
        ProbeInvalid -> "model returned an invalid response"
        _ -> ""
    pad width value =
        value <> Text.replicate (max 0 (width - Text.length value)) " "
