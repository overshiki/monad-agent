{-# LANGUAGE FieldSelectors #-}
-- | Reporting for a task-suite benchmark: a machine-readable @results.json@
-- and a human-readable @summary.md@ with per-task success rates and a combo
-- ranking.
module Agent.CLI.Bench.Report
    ( writeReport
    ) where

import Agent.CLI.Bench.Run (BenchResult(..))
import Agent.CLI.Bench.Suite (Suite(..), suiteMatrix, suiteTasks, taskId, comboId)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortBy)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

writeReport :: FilePath -> Suite -> [BenchResult] -> IO ()
writeReport resultsDir suite results = do
    createDirectoryIfMissing True resultsDir
    LBS.writeFile (resultsDir </> "results.json") (encode results)
    TextIO.writeFile (resultsDir </> "summary.md") (renderSummary suite results)

renderSummary :: Suite -> [BenchResult] -> Text
renderSummary suite results =
    Text.unlines $
        [ "# " <> suiteName suite
        , ""
        , "## By task"
        , ""
        ]
            <> concatMap renderTask (suiteTasks suite)
            <> [ "## Ranking"
               , ""
               , "| rank | configuration | overall success | mean score |"
               , "|---|---|---|---|"
               ]
            <> [ "| " <> Text.pack (show rank)
                     <> " | " <> comboId combo
                     <> " | " <> Text.pack (show passes <> "/" <> show trials)
                     <> " | " <> Text.pack (show (roundScore meanScore))
                     <> " |"
               | (rank, combo) <- zip [1 :: Int ..] ranked
               , let trials = cellTrials results (comboId combo)
                     passes = cellPasses results (comboId combo)
                     meanScore = cellMeanScore results (comboId combo)
               ]
  where
    ranked =
        sortBy (\a b -> compare (Down (overallPass b)) (Down (overallPass a)))
            (suiteMatrix suite)
    overallPass combo = cellPasses results (comboId combo)

    renderTask task =
        [ "### " <> taskId task
        , ""
        , "| configuration | success | score | mean latency |"
        , "|---|---|---|---|"
        ]
            <> [ "| " <> comboId combo
                     <> " | " <> Text.pack (show passes <> "/" <> show trials)
                     <> " | " <> Text.pack (show (roundScore meanScore))
                     <> " | " <> Text.pack (show meanLatencyMs <> " ms")
                     <> " |"
               | combo <- suiteMatrix suite
               , let trials = cellTrials results (comboId combo)
                     passes = cellPasses results (comboId combo)
                     meanScore = cellMeanScore results (comboId combo)
                     meanLatencyMs = cellMeanLatency results (comboId combo)
               ]
            <> [ "" ]

cellTrials :: [BenchResult] -> Text -> Int
cellTrials results comboId' =
    length [ () | r <- results, resultCombo r == comboId' ]

cellPasses :: [BenchResult] -> Text -> Int
cellPasses results comboId' =
    length [ () | r <- results, resultCombo r == comboId', resultPass r ]

cellMeanScore :: [BenchResult] -> Text -> Double
cellMeanScore results comboId' =
    case [ resultScore r | r <- results, resultCombo r == comboId' ] of
        [] -> 0
        scores -> fromIntegral (sum scores) / fromIntegral (length scores)

cellMeanLatency :: [BenchResult] -> Text -> Int
cellMeanLatency results comboId' =
    case [ resultLatencyMs r | r <- results, resultCombo r == comboId' ] of
        [] -> 0
        latencies -> sum latencies `div` length latencies

roundScore :: Double -> Double
roundScore = fromIntegral . (round :: Double -> Int)
