{-# LANGUAGE FieldSelectors #-}
-- | Task-suite benchmark definition: a YAML file describing the agent
-- configurations to compare (the matrix) and the tasks to run, each with a
-- validator that grades the agent's answer. Loaded and validated by
-- @monad-bench@.
module Agent.CLI.Bench.Suite
    ( Suite(..)
    , RunConfig(..)
    , Combo(..)
    , ShellMode(..)
    , Task(..)
    , Validator(..)
    , loadSuite
    ) where

import Data.Aeson
    ( FromJSON(..)
    , withObject
    , withText
    , (.:)
    , (.:?)
    , (.!=)
    )
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml

data Suite = Suite
    { suiteName :: !Text
    , suiteRuns :: !RunConfig
    , suiteMatrix :: ![Combo]
    , suiteTasks :: ![Task]
    }
    deriving (Eq, Show)

data RunConfig = RunConfig
    { runTrials :: !Int
    , runTimeoutSeconds :: !Int
    , runMaxConcurrency :: !Int
    }
    deriving (Eq, Show)

data ShellMode = ShellDefault | ShellGhci | ShellBash | ShellNone
    deriving (Eq, Show)

data Combo = Combo
    { comboId :: !Text
    , comboProvider :: !(Maybe Text)
    , comboModel :: !(Maybe Text)
    , comboConnection :: !(Maybe Text)
    , comboEffort :: !(Maybe Text)
    , comboShell :: !ShellMode
    }
    deriving (Eq, Show)

data Task = Task
    { taskId :: !Text
    , taskPrompt :: !Text
    , taskValidator :: !Validator
    }
    deriving (Eq, Show)

data Validator
    = ValidatorExact !Text
    | ValidatorContains !Text
    | ValidatorScript ![Text]
    | ValidatorJudge !Text !(Maybe Text) !Int
    deriving (Eq, Show)

-- | Parse and validate a suite file. Errors are human-readable and name the
-- offending entry.
loadSuite :: FilePath -> IO (Either Text Suite)
loadSuite path = do
    raw <- Yaml.decodeFileEither path
    pure $ case raw of
        Left err -> Left (Text.pack (Yaml.prettyPrintParseException err))
        Right value -> first Text.pack (Yaml.parseEither parseJSON value)

first :: (a -> b) -> Either a c -> Either b c
first function = \case
    Left value -> Left (function value)
    Right value -> Right value

instance FromJSON Suite where
    parseJSON = withObject "benchmark suite" \object -> do
        name <- object .: "name"
        runs <- object .:? "runs" .!= RunConfig 1 180 4
        matrix <- object .: "matrix"
        tasks <- object .: "tasks"
        let comboIds = [ cid | Combo{comboId = cid} <- matrix ]
            taskIds = [ tid | Task{taskId = tid} <- tasks ]
        case duplicates comboIds of
            [] -> pure ()
            dups -> fail ("duplicate matrix id: " <> Text.unpack (Text.intercalate ", " dups))
        case duplicates taskIds of
            [] -> pure ()
            dups -> fail ("duplicate task id: " <> Text.unpack (Text.intercalate ", " dups))
        if null matrix
            then fail "matrix must list at least one agent configuration"
            else pure ()
        if null tasks
            then fail "tasks must list at least one task"
            else pure ()
        pure Suite
            { suiteName = name
            , suiteRuns = runs
            , suiteMatrix = matrix
            , suiteTasks = tasks
            }
      where
        duplicates ids = [ x | x <- nub ids, length (filter (== x) ids) > 1 ]

instance FromJSON RunConfig where
    parseJSON = withObject "runs" \object -> do
        trials <- object .:? "trials" .!= 1
        timeoutSeconds <- object .:? "timeout_seconds" .!= 180
        concurrency <- object .:? "max_concurrency" .!= 4
        if trials < 1 then fail "runs.trials must be >= 1" else pure ()
        if timeoutSeconds < 1 then fail "runs.timeout_seconds must be >= 1" else pure ()
        if concurrency < 1 then fail "runs.max_concurrency must be >= 1" else pure ()
        pure (RunConfig trials timeoutSeconds concurrency)

instance FromJSON Combo where
    parseJSON = withObject "matrix entry" \object -> do
        identifier <- object .: "id"
        if Text.null (Text.strip identifier)
            then fail "matrix entry id must not be empty"
            else pure ()
        provider <- object .:? "provider"
        model <- object .:? "model"
        connection <- object .:? "connection"
        effort <- object .:? "effort"
        shell <- object .:? "shell" .!= ShellDefault
        pure Combo
            { comboId = identifier
            , comboProvider = provider
            , comboModel = model
            , comboConnection = connection
            , comboEffort = effort
            , comboShell = shell
            }

instance FromJSON ShellMode where
    parseJSON = withText "shell" \case
        "default" -> pure ShellDefault
        "ghci" -> pure ShellGhci
        "bash" -> pure ShellBash
        "none" -> pure ShellNone
        other -> fail ("unknown shell mode: " <> Text.unpack other)

instance FromJSON Task where
    parseJSON = withObject "task" \object -> do
        identifier <- object .: "id"
        if Text.null (Text.strip identifier)
            then fail "task id must not be empty"
            else pure ()
        prompt <- object .: "prompt"
        if Text.null (Text.strip prompt)
            then fail ("task " <> Text.unpack identifier <> " has an empty prompt")
            else pure ()
        validator <- object .: "validator"
        pure Task
            { taskId = identifier
            , taskPrompt = prompt
            , taskValidator = validator
            }

instance FromJSON Validator where
    parseJSON = withObject "validator" \object -> do
        validatorType <- object .: "type"
        case (validatorType :: Text) of
            "exact" -> ValidatorExact <$> object .: "expect"
            "contains" -> ValidatorContains <$> object .: "needle"
            "script" -> do
                command <- object .: "command"
                if null command
                    then fail "script validator command must not be empty"
                    else pure (ValidatorScript command)
            "judge" -> do
                rubric <- object .: "rubric"
                judgeModel <- object .:? "judge_model"
                scale <- object .:? "scale" .!= 5
                if scale < 1 then fail "judge scale must be >= 1" else pure ()
                pure (ValidatorJudge rubric judgeModel scale)
            other -> fail ("unknown validator type: " <> Text.unpack other)
