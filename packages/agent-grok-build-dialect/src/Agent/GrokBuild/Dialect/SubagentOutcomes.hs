-- | Subagent outcome tracking: data types and pure rendering for the
-- in-session "experience" hints.
--
-- When a subagent reaches a terminal state the host records an outcome
-- through 'SubagentOutcomeSink'. The completion message the parent agent
-- reads gains a one-line rollup of recent outcomes ('outcomeRollupLine'),
-- and the host may render historical outcomes into the @task@ tool
-- description so past experience informs type/model selection.
--
-- The dialect stays free of filesystem policy: the sink and the
-- pre-rendered history section are injected by the host. When the sink is
-- 'Nothing', every user-visible string is byte-identical to the
-- pre-tracking behavior.
module Agent.GrokBuild.Dialect.SubagentOutcomes
    ( SubagentOutcome(..)
    , SubagentOutcomeSink(..)
    , SubagentExperience(..)
    , noSubagentExperience
    , outcomeStatusName
    , outcomeRollupLine
    ) where

import Agent.Subagents (SubagentStatus(..))
import Data.Aeson
    ( FromJSON(..)
    , ToJSON(..)
    , (.:)
    , (.:?)
    , (.=)
    , object
    , withObject
    )
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as Text

-- | One terminal subagent outcome.
data SubagentOutcome = SubagentOutcome
    { outcomeType :: !Text
    -- ^ The spawn's @subagent_type@.
    , outcomeModel :: !(Maybe Text)
    -- ^ Explicitly requested or catalog-default model, if any.
    , outcomeDescription :: !Text
    -- ^ The spawn's short description.
    , outcomeStatus :: !Text
    -- ^ @"completed"@, @"errored"@, @"interrupted"@, @"shutdown"@, or
    -- @"unknown"@ (see 'outcomeStatusName').
    , outcomeAt :: !Text
    -- ^ ISO timestamp, rendered by the host clock ('nowIso') so the
    -- dialect keeps no time dependency of its own.
    , outcomeProject :: !(Maybe Text)
    -- ^ The cwd the spawn ran with (filled by the host at record time);
    -- used to scope historical outcomes to the current project.
    }
    deriving (Eq, Show)

instance ToJSON SubagentOutcome where
    toJSON outcome = object
        [ "type" .= outcome.outcomeType
        , "model" .= outcome.outcomeModel
        , "description" .= outcome.outcomeDescription
        , "status" .= outcome.outcomeStatus
        , "at" .= outcome.outcomeAt
        , "project" .= outcome.outcomeProject
        ]

-- | Tolerates extra fields so older/newer JSONL lines keep parsing.
instance FromJSON SubagentOutcome where
    parseJSON = withObject "subagent outcome" \object -> do
        outcomeType <- object .: "type"
        outcomeModel <- object .:? "model"
        outcomeDescription <- object .: "description"
        outcomeStatus <- object .: "status"
        outcomeAt <- object .: "at"
        outcomeProject <- object .:? "project"
        pure SubagentOutcome
            { outcomeType
            , outcomeModel
            , outcomeDescription
            , outcomeStatus
            , outcomeAt
            , outcomeProject
            }

-- | Injected by the host (agent-cli); the dialect stays free of
-- filesystem policy.
data SubagentOutcomeSink = SubagentOutcomeSink
    { recordOutcome :: SubagentOutcome -> IO ()
    -- ^ Record one terminal outcome. Fire-and-forget: must not throw.
    , recentOutcomes :: Int -> IO [SubagentOutcome]
    -- ^ The @n@ most recent outcomes, most recent first.
    , nowIso :: IO Text
    -- ^ Host clock as an ISO timestamp for 'outcomeAt'.
    }

-- | Bundle passed to the @task@ tool. 'Nothing' sink means no tracking
-- and zero behavior change.
data SubagentExperience = SubagentExperience
    { experienceSink :: Maybe SubagentOutcomeSink
    , experienceHistory :: Maybe Text
    -- ^ Pre-rendered historical outcomes section (host-rendered), appended
    -- to the tool description when non-empty.
    }

-- | The Stage-2 bundle: no sink, no history — every description and
-- completion message is byte-identical to the pre-tracking behavior.
noSubagentExperience :: SubagentExperience
noSubagentExperience = SubagentExperience Nothing Nothing

-- | Map a terminal 'SubagentStatus' onto the outcome status vocabulary.
outcomeStatusName :: Maybe SubagentStatus -> Text
outcomeStatusName = \case
    Just (Completed _) -> "completed"
    Just (Errored _) -> "errored"
    Just Interrupted -> "interrupted"
    Just Closed -> "shutdown"
    _ -> "unknown"

-- | One-line rollup of recent outcomes for the completion message the
-- parent reads, e.g.
--
-- > recent outcomes: explore-deepseek 3 runs (3 ok) · reviewer 2 runs (1 ok, 1 errored)
--
-- Groups preserve most-recent-first type order and are capped at 5 types;
-- @"ok"@ counts @status == "completed"@, everything else is listed by its
-- status name. 'Nothing' for empty input.
outcomeRollupLine :: [SubagentOutcome] -> Maybe Text
outcomeRollupLine [] = Nothing
outcomeRollupLine outcomes =
    Just ("recent outcomes: " <> Text.intercalate " · " (map renderGroup groups))
  where
    groups = take 5 (foldl' insert [] outcomes)
    insert :: [(Text, [Text])] -> SubagentOutcome -> [(Text, [Text])]
    insert acc outcome = case break ((== outcome.outcomeType) . fst) acc of
        (before, (agentType, statuses) : after) ->
            before <> [(agentType, statuses <> [outcome.outcomeStatus])] <> after
        _ -> acc <> [(outcome.outcomeType, [outcome.outcomeStatus])]
    renderGroup (agentType, statuses) =
        agentType
            <> " "
            <> countText (length statuses)
            <> " ("
            <> Text.intercalate ", " parts
            <> ")"
      where
        okCount = length (filter (== "completed") statuses)
        nonOkCounts =
            [ (status, length (filter (== status) statuses))
            | status <- foldl' uniqueSnoc [] statuses
            , status /= "completed"
            ]
        uniqueSnoc acc status
            | status `elem` acc = acc
            | otherwise = acc <> [status]
        parts =
            [ Text.pack (show okCount) <> " ok" | okCount > 0 ]
                <> [ Text.pack (show count) <> " " <> status
                   | (status, count) <- nonOkCounts
                   ]
    countText 1 = "1 run"
    countText n = Text.pack (show n) <> " runs"
