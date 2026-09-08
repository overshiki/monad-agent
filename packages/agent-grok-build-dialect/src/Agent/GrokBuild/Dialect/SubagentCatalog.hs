-- | Config-driven catalog of subagent types.
--
-- The harness ships three built-in subagent types (see 'builtinSubagentTypes').
-- A catalog — loaded from @subagents.default.json@ plus the user overlay
-- @~/.haskell-agent/subagents.json@ — can add custom types and/or shadow a
-- built-in id to set its default model or system-prompt suffix.
--
-- Merge semantics are entry-level replacement, matching @models.json@: a user
-- entry with the same id replaces the whole default entry; new ids are
-- appended in file order.
module Agent.GrokBuild.Dialect.SubagentCatalog
    ( CatalogEntry(..)
    , SubagentCatalog(..)
    , builtinSubagentTypes
    , emptyCatalog
    , catalogLookup
    , catalogExtraTypes
    , catalogModelFor
    , catalogHasModelDefaults
    , catalogKnownTypes
    , mergeSubagentCatalogs
    ) where

import Data.Aeson
    ( FromJSON(..)
    , (.:)
    , (.:?)
    , withObject
    )
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

-- | Subagent types provided by the harness itself. A catalog entry with one
-- of these ids does not add a type; it shadows the built-in to set defaults.
builtinSubagentTypes :: [Text]
builtinSubagentTypes = ["general-purpose", "explore", "plan"]

-- | Valid @kind@ values: the built-in filter presets a custom type can reuse.
catalogKinds :: [Text]
catalogKinds = "runtime-bounded" : builtinSubagentTypes

data CatalogEntry = CatalogEntry
    { entryId :: !Text
    -- ^ @subagent_type@ id; also the key for merge replacement.
    , entryDescription :: !Text
    -- ^ Shown to the parent agent so it can select the right type.
    , entryGroup :: !(Maybe Text)
    -- ^ Optional grouping hint for future pickers.
    , entryRank :: !(Maybe Int)
    -- ^ Optional ordering hint (must be >= 1) for future pickers.
    , entryModel :: !(Maybe Text)
    -- ^ Default backend model id (a models.json catalog id) used when a
    -- spawn omits the @model@ argument.
    , entrySystemPrompt :: !(Maybe Text)
    -- ^ Appended to the child system prompt when present.
    , entryUseWhen :: !(Maybe Text)
    -- ^ "Use when…" selection guidance shown to the parent agent next to
    -- the entry in the task tool's custom agent type list.
    , entryKind :: !(Maybe Text)
    -- ^ One of the built-in filter presets (@general-purpose@, @explore@,
    -- @plan@, @runtime-bounded@); lets a custom type reuse that preset's
    -- child tool filter. Ignored when 'entryTools' is set.
    , entryTools :: !(Maybe [Text])
    -- ^ Explicit child tool-name allowlist. Wins over 'entryKind'.
    }
    deriving (Eq, Show)

-- | Ordered list of entries. Lookup is by 'entryId'; order follows merge
-- semantics (defaults in file order, overlay replacements in place, new
-- overlay ids appended in overlay file order).
newtype SubagentCatalog = SubagentCatalog [CatalogEntry]
    deriving (Eq, Show)

instance Semigroup SubagentCatalog where
    (<>) = mergeSubagentCatalogs

instance Monoid SubagentCatalog where
    mempty = emptyCatalog

instance FromJSON CatalogEntry where
    parseJSON = withObject "subagent catalog entry" \object -> do
        rawId <- object .: "id"
        description <- object .: "description"
        group <- object .:? "group"
        rank <- object .:? "rank"
        model <- object .:? "model"
        systemPrompt <- object .:? "system_prompt"
        useWhen <- object .:? "use_when"
        kind <- object .:? "kind"
        rawTools <- object .:? "tools"
        let entryId = Text.strip rawId
        if Text.null entryId
            then fail "subagent catalog entry id must not be empty"
            else pure ()
        traverse_
            (\value -> if value >= 1
                then pure ()
                else fail ("subagent catalog entry " <> Text.unpack entryId
                    <> " has rank " <> show value <> "; rank must be >= 1"))
            rank
        traverse_
            (\value -> if value `elem` catalogKinds
                then pure ()
                else fail ("subagent catalog entry " <> Text.unpack entryId
                    <> " has unknown kind " <> show value
                    <> "; expected one of: "
                    <> Text.unpack (Text.intercalate ", " catalogKinds)))
            kind
        tools <- case rawTools of
            Nothing -> pure Nothing
            Just names
                | null names ->
                    fail ("subagent catalog entry " <> Text.unpack entryId
                        <> " has an empty tools list")
                | any (Text.null . Text.strip) names ->
                    fail ("subagent catalog entry " <> Text.unpack entryId
                        <> " has a blank tool name in tools")
                | otherwise -> pure (Just (map Text.strip names))
        pure CatalogEntry
            { entryId
            , entryDescription = description
            , entryGroup = nonEmptyText =<< group
            , entryRank = rank
            , entryModel = nonEmptyText =<< model
            , entrySystemPrompt = nonEmptyText =<< systemPrompt
            , entryUseWhen = nonEmptyText =<< useWhen
            , entryKind = kind
            , entryTools = tools
            }

instance FromJSON SubagentCatalog where
    parseJSON = withObject "subagent catalog" \object -> do
        rawEntries <- object .: "subagents"
        let entries = Vector.toList rawEntries
        case duplicatedIds entries of
            [] -> pure ()
            duplicates -> fail
                ( "duplicate subagent id"
                    <> (if length duplicates == 1 then "" else "s")
                    <> ": "
                    <> Text.unpack (Text.intercalate ", " duplicates)
                )
        pure (SubagentCatalog entries)

duplicatedIds :: [CatalogEntry] -> [Text]
duplicatedIds entries =
    Map.keys (Map.filter (> 1) counts)
  where
    counts :: Map Text Int
    counts = Map.fromListWith (+)
        [ (entry.entryId, 1 :: Int)
        | entry <- entries
        ]

-- | Per-id overlay: entries from the right catalog replace left entries with
-- the same id wholesale; ids only present on the right are appended in the
-- right catalog's file order.
mergeSubagentCatalogs :: SubagentCatalog -> SubagentCatalog -> SubagentCatalog
mergeSubagentCatalogs (SubagentCatalog base) (SubagentCatalog overlay) =
    SubagentCatalog (replaced <> appended)
  where
    baseIds = map (.entryId) base
    overlayById = Map.fromList [(entry.entryId, entry) | entry <- overlay]
    replaced =
        [ Map.findWithDefault entry entry.entryId overlayById
        | entry <- base
        ]
    appended = [entry | entry <- overlay, entry.entryId `notElem` baseIds]

emptyCatalog :: SubagentCatalog
emptyCatalog = SubagentCatalog []

catalogLookup :: Text -> SubagentCatalog -> Maybe CatalogEntry
catalogLookup wanted (SubagentCatalog entries) =
    case [entry | entry <- entries, entry.entryId == wanted] of
        entry : _ -> Just entry
        [] -> Nothing

-- | Catalog entries whose id is not one of the built-in types.
catalogExtraTypes :: SubagentCatalog -> [CatalogEntry]
catalogExtraTypes (SubagentCatalog entries) =
    [entry | entry <- entries, entry.entryId `notElem` builtinSubagentTypes]

-- | Default model bound to an agent type. Works for built-in ids too: a
-- catalog entry shadowing a built-in id overrides its default model.
catalogModelFor :: Text -> SubagentCatalog -> Maybe Text
catalogModelFor agentType catalog =
    (.entryModel) =<< catalogLookup agentType catalog

-- | True when any entry (including one shadowing a built-in id) binds a
-- default model to its agent type.
catalogHasModelDefaults :: SubagentCatalog -> Bool
catalogHasModelDefaults (SubagentCatalog entries) =
    any (isJust . (.entryModel)) entries

-- | Valid subagent types for validation: built-ins first, then catalog ids
-- not already built in, in catalog order.
catalogKnownTypes :: SubagentCatalog -> [Text]
catalogKnownTypes catalog =
    builtinSubagentTypes
        <> map (.entryId) (catalogExtraTypes catalog)

nonEmptyText :: Text -> Maybe Text
nonEmptyText value
    | Text.null stripped = Nothing
    | otherwise = Just stripped
  where
    stripped = Text.strip value
