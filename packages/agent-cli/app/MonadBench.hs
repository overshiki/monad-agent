-- | Task-suite benchmark runner: load a YAML suite, run every
-- (configuration, task) cell the requested number of times through one-shot
-- @monad-cli@ sessions, validate each answer, and write @results.json@ and
-- @summary.md@.
module Main (main) where

import Agent.CLI.Bench.Report (writeReport)
import Agent.CLI.Bench.Run (runSuite)
import Agent.CLI.Bench.Suite (loadSuite)
import Control.Monad (unless)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Left err -> putStrLn err >> exitFailure
        Right (suitePath, agentBin, resultsDir) -> do
            exists <- doesFileExist suitePath
            unless exists $ putStrLn ("suite file not found: " <> suitePath)
            if not exists
                then exitFailure
                else do
                    suite <- loadSuite suitePath >>= either
                        (\err -> putStrLn ("invalid suite: " <> show err) >> exitFailure)
                        pure
                    results <- runSuite agentBin resultsDir suite
                    writeReport resultsDir suite results
                    putStrLn
                        ( "benchmark complete: "
                            <> show (length results)
                            <> " runs -> "
                            <> resultsDir
                        )

parseArgs :: [String] -> Either String (FilePath, FilePath, FilePath)
parseArgs = go Nothing Nothing Nothing
  where
    go suite agentBin resultsDir = \case
        [] -> case (suite, agentBin) of
            (Just s, Just b) ->
                Right (s, b, maybe "bench-results" id resultsDir)
            _ ->
                Left "usage: monad-bench --suite FILE --agent-bin PATH [--results-dir DIR]"
        "--suite" : value : rest -> go (Just value) agentBin resultsDir rest
        "--agent-bin" : value : rest -> go suite (Just value) resultsDir rest
        "--results-dir" : value : rest -> go suite agentBin (Just value) rest
        _ : rest -> go suite agentBin resultsDir rest
