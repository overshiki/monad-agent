-- | Resolve a concrete child model id to the provider/connection that must
-- serve it, and classify it against the session's own target.
--
-- Phase 2 of cross-provider subagent execution: today every child is forced
-- onto the session's provider (see @prepareChild@ in
-- "Agent.CLI.Subagents.Runtime", which stamps the parent's provider on the
-- child session, and @resolveChildModelAndEffort@, which sends the catalog
-- model id over the parent transport with no provider validation). This
-- module is the pure resolution layer the future dispatcher consumes: given
-- the effective model id a spawn settled on, it says which provider +
-- connection + dialect own that model, and whether that differs from the
-- session's target. Pure + unit-testable; nothing here changes runtime
-- behavior yet.
module Agent.CLI.Subagents.TargetResolution
    ( ChildTarget(..)
    , resolveChildTarget
    , isSameProviderTarget
    , isSameConnectionTarget
    , foreignTargetMessage
    , childTargetProvider
    , childTargetConnection
    ) where

import Agent.CLI.Model.ModelConfig (ModelCatalog)
import Agent.CLI.Model.Models (ModelOption(..), ModelTarget(..), resolveConfiguredModel)
import Agent.Dialect (DialectId, dialectSlug)
import Agent.Provider (Provider, providerSlug)
import Data.Text (Text)
import qualified Data.Text as Text

-- | The provider/connection/dialect that owns a resolved child model.
data ChildTarget = ChildTarget
    { childProvider :: !Provider
    , childConnectionId :: !Text
    , childModelId :: !Text
    -- ^ catalog id of the model (also the wire model on built-in
    -- connections; on custom connections 'childWireModel' may differ)
    , childWireModel :: !Text
    , childDialect :: !DialectId
    }
    deriving (Eq, Show)

-- | Resolve a model id (an explicit override or a catalog/type default)
-- to the target that must serve it. Fails with a descriptive error when the
-- id is not in the loaded model catalog.
resolveChildTarget :: ModelCatalog -> Text -> Either Text ChildTarget
resolveChildTarget catalog modelId =
    case resolveConfiguredModel catalog modelId of
        Nothing ->
            Left
                ( "cannot resolve subagent model "
                    <> Text.pack (show modelId)
                    <> ": not present in the loaded model catalog"
                )
        Just ModelOption{ modelTarget = target } ->
            Right ChildTarget
                { childProvider = target.targetProvider
                , childConnectionId = target.targetConnectionId
                , childModelId = target.targetModelId
                , childWireModel = target.targetWireModelId
                , childDialect = target.targetDialect
                }

-- | Provider that owns the resolved model (accessor; field selectors are
-- suppressed by NoFieldSelectors).
childTargetProvider :: ChildTarget -> Provider
childTargetProvider (ChildTarget p _ _ _ _) = p

-- | Connection id that owns the resolved model.
childTargetConnection :: ChildTarget -> Text
childTargetConnection (ChildTarget _ c _ _ _) = c

-- | Whether the child target shares the session's provider.
isSameProviderTarget :: Provider -> ChildTarget -> Bool
isSameProviderTarget sessionProvider target =
    target.childProvider == sessionProvider

-- | Whether the child target shares the session's exact connection.
isSameConnectionTarget :: Provider -> Text -> ChildTarget -> Bool
isSameConnectionTarget sessionProvider sessionConnection target =
    isSameProviderTarget sessionProvider target
        && target.childConnectionId == sessionConnection

-- | A clear spawn-time error for a model that resolves to a different
-- provider than the session's (today it would silently go over the parent
-- transport and likely fail with a provider error; cross-provider dispatch
-- will make it work instead of erroring).
foreignTargetMessage :: Provider -> Text -> ChildTarget -> Text
foreignTargetMessage sessionProvider sessionConnection target =
    "subagent model "
        <> target.childModelId
        <> " resolves to the "
        <> providerSlug target.childProvider
        <> " transport (connection "
        <> target.childConnectionId
        <> "), but this session runs on "
        <> providerSlug sessionProvider
        <> " (connection "
        <> sessionConnection
        <> "); cross-provider subagent dispatch is not implemented yet. "
        <> "Move the subagent model to this session's connection "
        <> "(dialect "
        <> dialectSlug target.childDialect
        <> " on "
        <> target.childWireModel
        <> ") or start the session on the "
        <> providerSlug target.childProvider
        <> " connection."
