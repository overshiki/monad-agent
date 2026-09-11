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
    , ToJSON(..)
    , (.:)
    , (.:?)
    , (.=)
    , object
    , withObject
    )
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

-- Liquid Haskell verification (see docs/liquid-haskell.md).
--
-- The annotations below are comments for the normal build. The
-- @agent-cli-liquid@ package (cabal.liquid.project, GHC 9.14.1) compiles
-- this same source with @-fplugin=LiquidHaskell@, which checks them.
--
-- The central contract is the lookup-composition law for the overlay merge
-- ('mergeLookupLaw'): looking up an id in a merged catalog is the same as
-- trying the overlay first and falling back to the defaults — entry-level
-- replacement with overlay precedence, exactly the @models.json@ semantics
-- the module documents.
--
-- Trusted boundary: the 'textEquals' wrapper spec (SMT equality on 'Text'),
-- the two "Data.Map" presence assumes, and the 'findWithDefaultId' axiom
-- (a fromList-built map over unique ids agrees with a first-match list
-- lookup). Everything else is proved.

{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}

-- | 'Text' equality as genuine SMT equality (transitivity etc. come free).
{-@ assume textEquals :: a:Data.Text.Text -> b:Data.Text.Text
      -> {v:Bool | v <=> (a == b)} @-}
textEquals :: Text -> Text -> Bool
textEquals a b = a == b

-- | Attach a lemma's postcondition to an expression (ProofCombinators
-- style): @withProof x (lemma args)@ evaluates to @x@, and the lemma's
-- contract is visible to the verifier at that point. Zero cost after
-- inlining.
withProof :: a -> b -> a
withProof x _ = x
{-# INLINE withProof #-}

{-@ reflect isJustM @-}
isJustM :: Maybe a -> Bool
isJustM (Just _) = True
isJustM Nothing = False

-- | Maybe's left-biased choice, named for specs.
{-@ reflect maybeAlt @-}
maybeAlt :: Maybe a -> Maybe a -> Maybe a
maybeAlt (Just x) _ = Just x
maybeAlt Nothing y = y

-- | List append, named for specs ('(++)' is a class method).
{-@ reflect appendL @-}
appendL :: [a] -> [a] -> [a]
appendL [] ys = ys
appendL (x : xs) ys = x : appendL xs ys

-- | First entry with the given id, if any. The result's id matches the
-- query, and lookup fails only when the id is absent.
{-@ reflect lookupId @-}
{-@ lookupId :: wanted:Text -> entries:[CatalogEntry]
      -> {v:Maybe {e:CatalogEntry | entryId e == wanted}
          | not (memberId wanted entries) => not (isJustM v)} @-}
lookupId :: Text -> [CatalogEntry] -> Maybe CatalogEntry
lookupId _ [] = Nothing
lookupId wanted (entry : rest) =
    if textEquals (entryIdOf entry) wanted then Just entry else lookupId wanted rest

-- | Id membership in an entry list.
{-@ reflect memberId @-}
memberId :: Text -> [CatalogEntry] -> Bool
memberId _ [] = False
memberId wanted (entry : rest) =
    textEquals (entryIdOf entry) wanted || memberId wanted rest

-- | Id membership in an id list.
{-@ reflect memberIdList @-}
memberIdList :: Text -> [Text] -> Bool
memberIdList _ [] = False
memberIdList wanted (eid : rest) = textEquals eid wanted || memberIdList wanted rest

-- | fromMaybe specialised to entries (named for specs).
{-@ reflect fromMaybeEntry @-}
{-@ fromMaybeEntry :: d:CatalogEntry
      -> Maybe {e:CatalogEntry | entryId e == entryId d}
      -> {v:CatalogEntry | entryId v == entryId d} @-}
fromMaybeEntry :: CatalogEntry -> Maybe CatalogEntry -> CatalogEntry
fromMaybeEntry _ (Just entry) = entry
fromMaybeEntry fallback Nothing = fallback

-- | The ids of an entry list, in order.
{-@ reflect baseIdsOf @-}
baseIdsOf :: [CatalogEntry] -> [Text]
baseIdsOf [] = []
baseIdsOf (entry : rest) = entryIdOf entry : baseIdsOf rest

-- | Overlay entries whose id is not present in the defaults, in order.
{-@ reflect filterNotBase @-}
filterNotBase :: [Text] -> [CatalogEntry] -> [CatalogEntry]
filterNotBase _ [] = []
filterNotBase baseIds (entry : rest) =
    if memberIdList (entryIdOf entry) baseIds
        then filterNotBase baseIds rest
        else entry : filterNotBase baseIds rest

-- | Replace a defaults entry by the overlay entry of the same id, if any.
-- (A first-match list lookup; the previous Map-based lookup agreed with it
-- on exactly the duplicate-free catalogs the loader enforces, and catalog
-- sizes never exceed a few dozen entries.) The replacement (or kept) entry
-- always has the same id as the input.
{-@ replaceFromOverlay :: overlay:[CatalogEntry] -> entry:CatalogEntry
      -> {v:CatalogEntry | entryId v == entryId entry
                          && v == fromMaybeEntry entry (lookupId (entryIdOf entry) overlay)} @-}
replaceFromOverlay :: [CatalogEntry] -> CatalogEntry -> CatalogEntry
replaceFromOverlay overlay entry =
    fromMaybeEntry entry (lookupId (entryIdOf entry) overlay)

-- | 'replaceFromOverlay' over a whole defaults list.
{-@ reflect mapReplaceEntry @-}
mapReplaceEntry :: [CatalogEntry] -> [CatalogEntry] -> [CatalogEntry]
mapReplaceEntry _ [] = []
mapReplaceEntry overlay (entry : rest) =
    replaceFromOverlay overlay entry : mapReplaceEntry overlay rest

-- | Subagent types provided by the harness itself. A catalog entry with one
-- of these ids does not add a type; it shadows the built-in to set defaults.
builtinSubagentTypes :: [Text]
builtinSubagentTypes = ["general-purpose", "explore", "plan"]

-- | Valid @kind@ values: the built-in filter presets a custom type can reuse.
catalogKinds :: [Text]
catalogKinds = "runtime-bounded" : builtinSubagentTypes

-- Liquid Haskell: selectors made available to specifications.
{-@ data CatalogEntry = CatalogEntry
      { entryId :: Text
      , entryDescription :: Text
      , entryGroup :: Maybe Text
      , entryRank :: Maybe Int
      , entryModel :: Maybe Text
      , entrySystemPrompt :: Maybe Text
      , entryUseWhen :: Maybe Text
      , entryKind :: Maybe Text
      , entryTools :: Maybe [Text]
      }
@-}

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
        [ (entryIdOf entry, 1 :: Int)
        | entry <- entries
        ]

-- | Encode an entry back to the @subagents.json@ shape. Optional fields are
-- omitted, matching the decoder's @.:?@ expectations. This is the inverse of
-- 'parseJSON' so a catalog can be persisted (e.g. by @/subagents-config@).
instance ToJSON CatalogEntry where
    toJSON (CatalogEntry eid description group rank model systemPrompt useWhen kind tools) =
        object
            [ "id" .= eid
            , "description" .= description
            , "group" .= group
            , "rank" .= rank
            , "model" .= model
            , "system_prompt" .= systemPrompt
            , "use_when" .= useWhen
            , "kind" .= kind
            , "tools" .= tools
            ]

-- | Encode a whole catalog as the @{ "subagents": [...] }@ wrapper.
instance ToJSON SubagentCatalog where
    toJSON (SubagentCatalog entries) =
        object ["subagents" .= entries]

-- | Per-id overlay: entries from the right catalog replace left entries with
-- the same id wholesale; ids only present on the right are appended in the
-- right catalog's file order.
--
-- Verified by 'mergeLookupLaw' below: looking up an id in the merged catalog
-- tries the overlay first and falls back to the defaults. (The law is stated
-- for duplicate-free overlays, which the loader guarantees.)
{-@ reflect mergeSubagentCatalogs @-}
mergeSubagentCatalogs :: SubagentCatalog -> SubagentCatalog -> SubagentCatalog
mergeSubagentCatalogs (SubagentCatalog base) (SubagentCatalog overlay) =
    SubagentCatalog (appendL replaced appended)
  where
    baseIds = baseIdsOf base
    replaced = mapReplaceEntry overlay base
    appended = filterNotBase baseIds overlay

emptyCatalog :: SubagentCatalog
emptyCatalog = SubagentCatalog []

{-@ reflect catalogLookup @-}
catalogLookup :: Text -> SubagentCatalog -> Maybe CatalogEntry
catalogLookup wanted (SubagentCatalog entries) = lookupId wanted entries

-- | Catalog entries whose id is not one of the built-in types.
catalogExtraTypes :: SubagentCatalog -> [CatalogEntry]
catalogExtraTypes (SubagentCatalog entries) =
    [entry | entry <- entries, entryIdOf entry `notElem` builtinSubagentTypes]

-- | Default model bound to an agent type. Works for built-in ids too: a
-- catalog entry shadowing a built-in id overrides its default model.
--
-- Verified: a model is returned only for an id present in the catalog.
{-@ catalogModelFor :: agentType:Text -> catalog:SubagentCatalog
      -> {v:Maybe Text | isJustM v => isJustM (catalogLookup agentType catalog)} @-}
catalogModelFor :: Text -> SubagentCatalog -> Maybe Text
catalogModelFor agentType catalog =
    case catalogLookup agentType catalog of
        Just entry -> entryModelOf entry
        Nothing -> Nothing

-- | True when any entry (including one shadowing a built-in id) binds a
-- default model to its agent type.
catalogHasModelDefaults :: SubagentCatalog -> Bool
catalogHasModelDefaults (SubagentCatalog entries) =
    any (\entry -> isJust (entryModelOf entry)) entries

-- | Valid subagent types for validation: built-ins first, then catalog ids
-- not already built in, in catalog order.
catalogKnownTypes :: SubagentCatalog -> [Text]
catalogKnownTypes catalog =
    builtinSubagentTypes
        <> baseIdsOf (catalogExtraTypes catalog)

{-@ reflect entryIdOf @-}
entryIdOf :: CatalogEntry -> Text
entryIdOf (CatalogEntry eid _ _ _ _ _ _ _ _) = eid

entryModelOf :: CatalogEntry -> Maybe Text
entryModelOf (CatalogEntry _ _ _ _ model _ _ _ _) = model

nonEmptyText :: Text -> Maybe Text
nonEmptyText value
    | Text.null stripped = Nothing
    | otherwise = Just stripped
  where
    stripped = Text.strip value

-- ---------------------------------------------------------------------
-- Liquid Haskell proofs

-- | Ids named by 'baseIdsOf' are exactly the ids present in the list.
{-@ memberBaseIds :: wanted:Text -> entries:[CatalogEntry]
      -> {v:() | memberIdList wanted (baseIdsOf entries) <=> memberId wanted entries} @-}
memberBaseIds :: Text -> [CatalogEntry] -> ()
memberBaseIds _ [] = ()
memberBaseIds wanted (CatalogEntry eid _ _ _ _ _ _ _ _ : rest) =
    memberBaseIds wanted rest

-- | Lookup succeeds when the id is present.
{-@ lookupIdMember :: wanted:Text -> entries:[CatalogEntry]
      -> {v:() | memberId wanted entries => isJustM (lookupId wanted entries)} @-}
lookupIdMember :: Text -> [CatalogEntry] -> ()
lookupIdMember _ [] = ()
lookupIdMember wanted (CatalogEntry eid _ _ _ _ _ _ _ _ : rest)
    | textEquals eid wanted = ()
    | otherwise = lookupIdMember wanted rest

-- | Lookup distributes over append, overlay-first.
{-@ lookupIdAppend :: wanted:Text -> xs:[CatalogEntry] -> ys:[CatalogEntry]
      -> {v:() | lookupId wanted (appendL xs ys)
                 == maybeAlt (lookupId wanted xs) (lookupId wanted ys)} @-}
lookupIdAppend :: Text -> [CatalogEntry] -> [CatalogEntry] -> ()
lookupIdAppend _ [] _ = ()
lookupIdAppend wanted (CatalogEntry _ _ _ _ _ _ _ _ _ : rest) ys =
    lookupIdAppend wanted rest ys

-- | Looking up in the replaced defaults finds the replacement at the same
-- position as the defaults lookup. Written as an equational proof (the
-- direct recursive style leaves the goal quantified over the scrutinee).
{-@ lookupReplaced :: wanted:Text -> overlay:[CatalogEntry] -> base:[CatalogEntry]
      -> {v:Proof | lookupId wanted (mapReplaceEntry overlay base)
                 == maybeMapReplace (lookupId wanted base) overlay} @-}
lookupReplaced :: Text -> [CatalogEntry] -> [CatalogEntry] -> Proof
lookupReplaced _ _ [] = ()
lookupReplaced wanted overlay (entry@(CatalogEntry _ _ _ _ _ _ _ _ _) : rest) =
        lookupId wanted (mapReplaceEntry overlay (entry : rest))
    === lookupId wanted
            (replaceFromOverlay overlay entry : mapReplaceEntry overlay rest)
    === (if textEquals (entryIdOf (replaceFromOverlay overlay entry)) wanted
            then Just (replaceFromOverlay overlay entry)
            else lookupId wanted (mapReplaceEntry overlay rest))
        ? lookupReplaced wanted overlay rest
    === (if textEquals (entryIdOf entry) wanted
            then Just (replaceFromOverlay overlay entry)
            else maybeMapReplace (lookupId wanted rest) overlay)
    === (if textEquals (entryIdOf entry) wanted
            then Just (fromMaybeEntry entry (lookupId (entryIdOf entry) overlay))
            else maybeMapReplace (lookupId wanted rest) overlay)
    === (if textEquals (entryIdOf entry) wanted
            then maybeMapReplace (Just entry) overlay
            else maybeMapReplace (lookupId wanted rest) overlay)
    === maybeMapReplace (lookupId wanted (entry : rest)) overlay
    *** QED

-- Minimal local proof combinators (the liquid-prelude idiom).
type Proof = ()

data QED = QED

infixl 3 ***
infixl 3 ===
infixl 3 ?

{-@ (***) :: a -> QED -> Proof @-}
(***) :: a -> QED -> Proof
_ *** _ = ()

{-@ (===) :: x:a -> y:{a | y == x} -> {v:a | v == x && v == y} @-}
(===) :: a -> a -> a
_ === y = y

{-@ (?) :: x:a -> b:b -> {v:a | v == x} @-}
(?) :: a -> b -> a
x ? _ = x

-- | Map 'replaceFromOverlay' over a lookup result.
{-@ reflect maybeMapReplace @-}
maybeMapReplace :: Maybe CatalogEntry -> [CatalogEntry] -> Maybe CatalogEntry
maybeMapReplace Nothing _ = Nothing
maybeMapReplace (Just entry) overlay =
    Just (replaceFromOverlay overlay entry)

-- | A present left alternative wins over anything.
{-@ maybeAltJustNothing :: x:(Maybe CatalogEntry)
      -> {v:() | isJustM x => maybeAlt x Nothing == x} @-}
maybeAltJustNothing :: Maybe CatalogEntry -> ()
maybeAltJustNothing (Just _) = ()
maybeAltJustNothing Nothing = ()

-- | 'maybeMapReplace' on a found entry is the overlay-first choice.
{-@ maybeMapReplaceIsAlt :: wanted:Text -> entry:CatalogEntry -> overlay:[CatalogEntry]
      -> {v:() | entryIdOf entry == wanted
                 => maybeMapReplace (Just entry) overlay
                    == maybeAlt (lookupId wanted overlay) (Just entry)} @-}
maybeMapReplaceIsAlt :: Text -> CatalogEntry -> [CatalogEntry] -> ()
maybeMapReplaceIsAlt _ entry overlay =
        maybeMapReplace (Just entry) overlay
    === Just (replaceFromOverlay overlay entry)
    === Just (fromMaybeEntry entry (lookupId (entryIdOf entry) overlay))
    === maybeAlt (lookupId (entryIdOf entry) overlay) (Just entry)
    *** QED

-- | 'maybeMapReplaceIsAlt' at the defaults lookup result (when present).
{-@ maybeMapReplaceIsAltHelper :: wanted:Text -> base:[CatalogEntry] -> overlay:[CatalogEntry]
      -> {v:() | isJustM (lookupId wanted base)
                 => maybeMapReplace (lookupId wanted base) overlay
                    == maybeAlt (lookupId wanted overlay) (lookupId wanted base)} @-}
maybeMapReplaceIsAltHelper :: Text -> [CatalogEntry] -> [CatalogEntry] -> ()
maybeMapReplaceIsAltHelper wanted base overlay =
    case lookupId wanted base of
        Nothing -> ()
        Just entry -> () `withProof` maybeMapReplaceIsAlt wanted entry overlay

-- | The appended entries never contain an id that is present in the
-- defaults.
{-@ lookupAppendedNothing :: wanted:Text -> {base:[CatalogEntry] | memberIdList wanted (baseIdsOf base)}
      -> overlay:[CatalogEntry]
      -> {v:() | not (isJustM (lookupId wanted (filterNotBase (baseIdsOf base) overlay)))} @-}
lookupAppendedNothing :: Text -> [CatalogEntry] -> [CatalogEntry] -> ()
lookupAppendedNothing _ _ [] = ()
lookupAppendedNothing wanted base (entry@(CatalogEntry _ _ _ _ _ _ _ _ _) : rest) =
    case memberIdList (entryIdOf entry) (baseIdsOf base) of
        -- the entry is dropped, the lookup falls through to the IH
        True ->
            lookupId wanted (filterNotBase (baseIdsOf base) (entry : rest))
            === lookupId wanted (filterNotBase (baseIdsOf base) rest)
                ? lookupAppendedNothing wanted base rest
            === lookupId wanted (filterNotBase (baseIdsOf base) rest)
            *** QED
        -- the entry is kept, but it cannot match: its id is not in the
        -- defaults, while the query id is
        False ->
            lookupId wanted (filterNotBase (baseIdsOf base) (entry : rest))
            === lookupId wanted (entry : filterNotBase (baseIdsOf base) rest)
            === (if textEquals (entryIdOf entry) wanted
                    then Just entry
                    else lookupId wanted (filterNotBase (baseIdsOf base) rest))
                ? lookupAppendedNothing wanted base rest
            === lookupId wanted (filterNotBase (baseIdsOf base) rest)
            *** QED

-- | For an id absent from the defaults, the appended entries answer lookups
-- exactly like the overlay.
{-@ lookupAppendedKept :: wanted:Text -> {base:[CatalogEntry] | not (memberIdList wanted (baseIdsOf base))}
      -> overlay:[CatalogEntry]
      -> {v:() | lookupId wanted (filterNotBase (baseIdsOf base) overlay)
                 == lookupId wanted overlay} @-}
lookupAppendedKept :: Text -> [CatalogEntry] -> [CatalogEntry] -> ()
lookupAppendedKept _ _ [] = ()
lookupAppendedKept wanted base (entry@(CatalogEntry _ _ _ _ _ _ _ _ _) : rest) =
    case memberIdList (entryIdOf entry) (baseIdsOf base) of
        -- the entry is dropped; it cannot be the match anyway since the
        -- query id is absent from the defaults
        True ->
            lookupId wanted (filterNotBase (baseIdsOf base) (entry : rest))
            === lookupId wanted (filterNotBase (baseIdsOf base) rest)
                ? lookupAppendedKept wanted base rest
            === (if textEquals (entryIdOf entry) wanted
                    then Just entry
                    else lookupId wanted rest)
            === lookupId wanted (entry : rest)
            *** QED
        -- the entry is kept; both sides match or skip it identically
        False ->
            lookupId wanted (filterNotBase (baseIdsOf base) (entry : rest))
            === lookupId wanted (entry : filterNotBase (baseIdsOf base) rest)
            === (if textEquals (entryIdOf entry) wanted
                    then Just entry
                    else lookupId wanted (filterNotBase (baseIdsOf base) rest))
                ? lookupAppendedKept wanted base rest
            === (if textEquals (entryIdOf entry) wanted
                    then Just entry
                    else lookupId wanted rest)
            === lookupId wanted (entry : rest)
            *** QED

-- | The lookup-composition law: the merged catalog answers every lookup by
-- trying the overlay first and falling back to the defaults. This is the
-- entry-level replacement semantics the module documents.
{-@ mergeLookupLaw :: base:SubagentCatalog -> overlay:SubagentCatalog -> wanted:Text
      -> {v:() | catalogLookup wanted (mergeSubagentCatalogs base overlay)
                 == maybeAlt (catalogLookup wanted overlay) (catalogLookup wanted base)} @-}
mergeLookupLaw :: SubagentCatalog -> SubagentCatalog -> Text -> ()
mergeLookupLaw (SubagentCatalog base) (SubagentCatalog overlay) wanted =
    case memberIdList wanted (baseIdsOf base) of
        -- the defaults know the id: the replacement (overlay entry, if any)
        -- answers, and the appended entries are irrelevant
        True ->
                catalogLookup wanted
                    (mergeSubagentCatalogs (SubagentCatalog base) (SubagentCatalog overlay))
                === catalogLookup wanted
                        (SubagentCatalog (appendL replaced appended))
                === lookupId wanted (appendL replaced appended)
                    ? lookupIdAppend wanted replaced appended
                    ? lookupReplaced wanted overlay base
                    ? lookupAppendedNothing wanted base overlay
                    ? lookupIdMember wanted base
                    ? memberBaseIds wanted base
                === maybeAlt (lookupId wanted replaced) (lookupId wanted appended)
                === maybeAlt (maybeMapReplace (lookupId wanted base) overlay)
                             (lookupId wanted appended)
                    ? maybeAltJustNothing (maybeMapReplace (lookupId wanted base) overlay)
                === maybeMapReplace (lookupId wanted base) overlay
                    ? maybeMapReplaceIsAltHelper wanted base overlay
                === maybeAlt (lookupId wanted overlay) (lookupId wanted base)
                === maybeAlt (catalogLookup wanted (SubagentCatalog overlay))
                             (catalogLookup wanted (SubagentCatalog base))
                *** QED
        -- the defaults do not know the id: the appended overlay entries
        -- answer exactly like the overlay
        False ->
                catalogLookup wanted
                    (mergeSubagentCatalogs (SubagentCatalog base) (SubagentCatalog overlay))
                === catalogLookup wanted
                        (SubagentCatalog (appendL replaced appended))
                === lookupId wanted (appendL replaced appended)
                    ? lookupIdAppend wanted replaced appended
                    ? lookupReplaced wanted overlay base
                    ? lookupAppendedKept wanted base overlay
                    ? memberBaseIds wanted base
                === maybeAlt (lookupId wanted replaced) (lookupId wanted appended)
                === maybeAlt (maybeMapReplace (lookupId wanted base) overlay)
                             (lookupId wanted appended)
                === maybeAlt Nothing (lookupId wanted overlay)
                === lookupId wanted overlay
                === maybeAlt (lookupId wanted overlay) Nothing
                === maybeAlt (catalogLookup wanted (SubagentCatalog overlay))
                             (catalogLookup wanted (SubagentCatalog base))
                *** QED
  where
    baseIds = baseIdsOf base
    replaced = mapReplaceEntry overlay base
    appended = filterNotBase baseIds overlay

-- | Unwrap the catalog newtype (named for specs).
{-@ reflect catalogEntriesOf @-}
catalogEntriesOf :: SubagentCatalog -> [CatalogEntry]
catalogEntriesOf (SubagentCatalog entries) = entries
