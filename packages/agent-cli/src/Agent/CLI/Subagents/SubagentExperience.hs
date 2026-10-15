-- | Session-scoped subagent outcome tracking for the CLI host.
--
-- Every terminal subagent spawn appends one JSON line to
-- @~/.haskell-agent/subagent-outcomes.jsonl@ (best-effort: the file is
-- created lazily on the first outcome and every IO failure is ignored, so
-- tracking never crashes or delays a run). The same outcomes are kept in a
-- session IORef so the completion message rollup sees them without disk IO.
--
-- At session startup the last 2000 lines are loaded, corrupt lines are
-- skipped, and entries are filtered to the current project (the recorded
-- cwd is a prefix of the session cwd). The survivors are rendered into a
-- "Historical outcomes" section that the dialect appends to the @task@
-- tool description.
--
-- The harness never changes routing policy by itself: these are hints the
-- parent model observes. Delete the JSONL file to reset.
module Agent.CLI.Subagents.SubagentExperience
    ( newSubagentExperience
    , loadOutcomeHistory
    , subagentOutcomesPath
    ) where

import Agent.GrokBuild.Dialect.SubagentOutcomes
    ( SubagentExperience(..)
    , SubagentOutcome(..)
    , SubagentOutcomeSink(..)
    )
import Agent.OsPath (toText, unsafeToFilePath)
import Control.Concurrent.MVar (modifyMVar_, newMVar)
import Control.Exception.Safe (SomeException, try)
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS.Char8
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (foldl', sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (Down(..), comparing)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.OsPath (OsPath)

-- | @<home>/.haskell-agent/subagent-outcomes.jsonl@.
subagentOutcomesPath :: FilePath -> FilePath
subagentOutcomesPath home =
    home </> ".haskell-agent" </> "subagent-outcomes.jsonl"

-- | Build the experience bundle for one session: a sink recording into a
-- session IORef plus the append-only JSONL, and the pre-rendered history
-- section for the session cwd.
newSubagentExperience :: OsPath -> OsPath -> IO SubagentExperience
newSubagentExperience home cwd = do
    logRef <- newIORef []
    writeLock <- newMVar ()
    let jsonlPath = subagentOutcomesPath (unsafeToFilePath home)
        sessionProject = toText cwd
        sink = SubagentOutcomeSink
            { recordOutcome = \outcome -> do
                let stamped = outcome { outcomeProject = Just sessionProject }
                atomicModifyIORef' logRef (\entries -> (stamped : entries, ()))
                _ <- try @_ @SomeException $ modifyMVar_ writeLock \() -> do
                    createDirectoryIfMissing True (takeDirectory jsonlPath)
                    LBS.appendFile jsonlPath (encode stamped <> "\n")
                pure ()
            , recentOutcomes = \n -> take n <$> readIORef logRef
            , nowIso =
                Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
                    <$> getCurrentTime
            }
    history <- loadOutcomeHistory (unsafeToFilePath home) (unsafeToFilePath cwd)
    pure SubagentExperience
        { experienceSink = Just sink
        , experienceHistory = history
        }

-- | Read the last 2000 JSONL lines, skip corrupt ones, keep entries whose
-- recorded project cwd is a prefix of the session cwd, and render the
-- project-scoped history section ('Nothing' when no entry matches).
loadOutcomeHistory :: FilePath -> FilePath -> IO (Maybe Text)
loadOutcomeHistory home sessionCwd = do
    contents <- try @_ @SomeException (LBS.readFile (subagentOutcomesPath home))
    case contents of
        Left _ -> pure Nothing
        Right bytes -> do
            let recentLines =
                    reverse (take 2000 (reverse (LBS.Char8.lines bytes)))
                parsed =
                    mapMaybe (either (const Nothing) Just . eitherDecode)
                        recentLines
                sessionProject = Text.pack sessionCwd
                matching =
                    [ outcome
                    | outcome <- parsed
                    , Just project <- [outcome.outcomeProject]
                    , project `Text.isPrefixOf` sessionProject
                    ]
            pure (renderHistory matching)

-- | > Historical outcomes (this project):
--   > - explore-deepseek: 12 runs, 10 ok, 2 errored
--   > - reviewer: 4 runs, 4 ok
--
-- Sorted by run count desc (type asc on ties), capped at 8 lines.
renderHistory :: [SubagentOutcome] -> Maybe Text
renderHistory [] = Nothing
renderHistory outcomes =
    Just
        ( "Historical outcomes (this project):\n"
            <> Text.intercalate "\n" (map renderLine (take 8 sortedGroups))
        )
  where
    sortedGroups =
        sortBy
            (comparing (Down . length . snd) <> comparing fst)
            (foldl' insert [] outcomes)
    insert :: [(Text, [SubagentOutcome])] -> SubagentOutcome -> [(Text, [SubagentOutcome])]
    insert acc outcome = case break ((== outcome.outcomeType) . fst) acc of
        (before, (agentType, entries) : after) ->
            before <> [(agentType, entries <> [outcome])] <> after
        _ -> acc <> [(outcome.outcomeType, [outcome])]
    renderLine (agentType, entries) =
        "- "
            <> agentType
            <> ": "
            <> countText (length entries)
            <> Text.concat (map (", " <>) parts)
      where
        statuses = map (.outcomeStatus) entries
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
