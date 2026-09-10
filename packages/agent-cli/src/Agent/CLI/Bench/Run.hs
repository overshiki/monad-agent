{-# LANGUAGE FieldSelectors #-}
-- | Runner for a task-suite benchmark: spawn a one-shot @monad-cli@ session
-- per (combo, task, trial), validate the answer, and collect pass/score,
-- latency, and the raw answer. Reuses the same subprocess pattern as the
-- existing @eval-ghci-vs-bash@ harness.
module Agent.CLI.Bench.Run
    ( BenchResult(..)
    , runSuite
    , comboFlags
    ) where

import Agent.CLI.Bench.Suite
import Agent.Concurrent (mapConcurrentlyBounded)
import Control.Exception.Safe (tryIO)
import Control.Monad (forM_, when)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory
    ( copyFile
    , createDirectoryIfMissing
    , doesFileExist
    )
import System.Exit (ExitCode(..))
import System.FilePath (takeFileName, (</>))
import System.Process
    ( CreateProcess(..)
    , proc
    , readCreateProcessWithExitCode
    )
import System.Timeout (timeout)
import Data.Time.Clock (diffUTCTime, getCurrentTime)

-- | One measured run: a (combo, task, trial) cell.
data BenchResult = BenchResult
    { resultCombo :: !Text
    , resultTask :: !Text
    , resultTrial :: !Int
    , resultPass :: !Bool
    , resultScore :: !Int
    , resultLatencyMs :: !Int
    , resultOutput :: !Text
    , resultError :: !Text
    }
    deriving (Eq, Show)

instance ToJSON BenchResult where
    toJSON r = object
        [ "combo" .= resultCombo r
        , "task" .= resultTask r
        , "trial" .= resultTrial r
        , "pass" .= resultPass r
        , "score" .= resultScore r
        , "latency_ms" .= resultLatencyMs r
        , "output" .= resultOutput r
        , "error" .= resultError r
        ]

-- | Run every (combo, task) cell @trials@ times with bounded concurrency.
runSuite :: FilePath -> FilePath -> FilePath -> Suite -> IO [BenchResult]
runSuite agentBin suiteDir resultsDir suite = do
    resultsRef <- newIORef []
    let cells =
            [ (combo, task, trial)
            | combo <- suiteMatrix suite
            , task <- suiteTasks suite
            , trial <- [1 .. runTrials (suiteRuns suite)]
            ]
    mapConcurrentlyBounded
        (runMaxConcurrency (suiteRuns suite))
        (runCell agentBin suiteDir resultsDir suite resultsRef)
        cells
    reverse <$> readIORef resultsRef

runCell
    :: FilePath
    -> FilePath
    -> FilePath
    -> Suite
    -> IORef [BenchResult]
    -> (Combo, Task, Int)
    -> IO ()
runCell agentBin suiteDir resultsDir suite resultsRef (combo, task, trial) = do
    workspace <- prepareWorkspace resultsDir suiteDir task combo trial
    let args = ["-p", Text.unpack (taskPrompt task)] <> comboFlags combo
        agentProc = (proc agentBin args) { cwd = Just workspace }
    started <- getCurrentTime
    outcome <-
        timeout
            (runTimeoutSeconds (suiteRuns suite) * 1_000_000)
            (tryIO (readCreateProcessWithExitCode agentProc ""))
    finished <- getCurrentTime
    let latencyMs = floor (realToFrac (diffUTCTime finished started) * 1000) :: Int
    (output, pass, score, errText) <-
        case outcome of
            Nothing ->
                pure ("", False, 0, "timeout after "
                    <> Text.pack (show (runTimeoutSeconds (suiteRuns suite))) <> "s")
            Just (Left exception) ->
                pure ("", False, 0, Text.pack (show exception))
            Just (Right (exitCode, stdoutText, stderrText))
                | exitCode /= ExitSuccess ->
                    pure ("", False, 0, Text.pack (trim stdoutText <> trim stderrText))
                | otherwise -> do
                    let answer = Text.pack (trim stdoutText)
                    (pass', score') <- validateAnswer agentBin workspace combo task answer
                    pure (answer, pass', score', "")
    let result = BenchResult
            { resultCombo = comboId combo
            , resultTask = taskId task
            , resultTrial = trial
            , resultPass = pass
            , resultScore = score
            , resultLatencyMs = latencyMs
            , resultOutput = output
            , resultError = errText
            }
    modifyIORef resultsRef (result :)

-- | A fresh per-(task, combo, trial) workspace, seeded with the task's
-- fixture files, in which the agent runs and the grader checks the result.
prepareWorkspace
    :: FilePath
    -> FilePath
    -> Task
    -> Combo
    -> Int
    -> IO FilePath
prepareWorkspace resultsDir suiteDir task combo trial = do
    let workspace =
            resultsDir
                </> "tasks"
                </> Text.unpack (taskId task)
                </> Text.unpack (comboId combo)
                </> show trial
    createDirectoryIfMissing True workspace
    forM_ (taskFiles task) \fixture -> do
        let source = suiteDir </> fixture
        exists <- doesFileExist source
        when exists $
            copyFile source (workspace </> takeFileName fixture)
    pure workspace

comboFlags :: Combo -> [String]
comboFlags combo =
    concat
        [ optFlag "--provider" (comboProvider combo)
        , optFlag "--model" (comboModel combo)
        , optFlag "--effort" (comboEffort combo)
        , shellFlags (comboShell combo)
        ]
  where
    optFlag name = maybe [] (\value -> [name, Text.unpack value])
    shellFlags = \case
        ShellDefault -> []
        ShellGhci -> ["--ghci", "--no-bash"]
        ShellBash -> ["--bash", "--no-ghci"]
        ShellNone -> ["--no-bash", "--no-ghci"]

validateAnswer
    :: FilePath
    -> FilePath
    -> Combo
    -> Task
    -> Text
    -> IO (Bool, Int)
validateAnswer agentBin workspace combo task answer =
    case taskValidator task of
        ValidatorExact expected ->
            let matches = Text.strip answer == Text.strip expected
            in pure (matches, if matches then 1 else 0)
        ValidatorContains needle ->
            let matches = needle `Text.isInfixOf` answer
            in pure (matches, if matches then 1 else 0)
        ValidatorScript command -> do
            exitResult <-
                tryIO
                    (readCreateProcessWithExitCode
                        (proc
                            (Text.unpack (head command))
                            (map Text.unpack (tail command)))
                            { cwd = Just workspace }
                        "")
            let passed = case exitResult of
                    Right (ExitSuccess, _, _) -> True
                    _ -> False
            pure (passed, if passed then 1 else 0)
        ValidatorJudge rubric judgeModel scale -> do
            let judgePrompt =
                    Text.unlines
                        [ "Grade the following answer against the rubric."
                        , "Reply with a single integer score from 0 to "
                            <> Text.pack (show scale) <> "."
                        , "Rubric: " <> rubric
                        , "Answer: " <> answer
                        ]
                judgeArgs =
                    ["-p", Text.unpack judgePrompt]
                        <> maybe []
                            (\model -> ["--model", Text.unpack model])
                            (maybe (comboModel combo) Just judgeModel)
            judgeResult <-
                tryIO (readCreateProcessWithExitCode (proc agentBin judgeArgs) "")
            let score = case judgeResult of
                    Right (ExitSuccess, out, _) ->
                        readScore scale (Text.pack (trim out))
                    _ -> 0
                threshold = scale `div` 2 + 1
            pure (score >= threshold, score)

readScore :: Int -> Text -> Int
readScore scale text =
    case reads (Text.unpack text) :: [(Int, String)] of
        (n, _) : _ | n >= 0 && n <= scale -> n
        _ -> 0

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
  where
    isSpace = (== ' ')
    dropWhileEnd p = reverse . dropWhile p . reverse

modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef ref function = do
    value <- readIORef ref
    writeIORef ref (function value)
