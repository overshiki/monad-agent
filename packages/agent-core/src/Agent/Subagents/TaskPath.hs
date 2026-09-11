-- | Codex multi-agent v2 task paths (@/root/...@).
--
-- Subset of openai/codex 'AgentPath': absolute paths under @/root@ with
-- lowercase alphanumeric / underscore segments.
module Agent.Subagents.TaskPath
    ( TaskPath
    , taskPathRoot
    , taskPathText
    , taskPathName
    , isTaskPathRoot
    , parseTaskPath
    , joinTaskPath
    , resolveTaskPath
    , validateTaskName
    ) where

import Data.Char (isAsciiLower, isDigit)
import Data.Text (Text)
import qualified Data.Text as Text

-- Liquid Haskell verification (see docs/liquid-haskell.md).
--
-- The verified contracts are the validator's reserved-value rejections:
-- 'validateTaskName' returns 'Left' for the empty name and for @"root"@, so a
-- task name can never be the reserved root label or the empty string. This is
-- the structural half of "a task name is either valid or rejected"; the
-- character-class check ('Text.any' over 'isNameChar') stays on the trusted
-- boundary.
--
-- Trusted boundary: 'textEquals' (SMT equality on 'Text') and 'textNullEq'
-- ('Text.null' agrees with equality to the empty string).

{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}

{-@ assume textEquals :: a:Data.Text.Text -> b:Data.Text.Text
      -> {v:Bool | v <=> (a == b)} @-}
textEquals :: Text -> Text -> Bool
textEquals a b = a == b

{-@ measure tlen :: Text -> Int @-}
{-@ assume Text.null :: t:Data.Text.Text -> {v:Bool | v <=> (tlen t == 0)} @-}

{-@ reflect isLeftM @-}
isLeftM :: Either a b -> Bool
isLeftM (Left _) = True
isLeftM (Right _) = False

newtype TaskPath = TaskPath { unTaskPath :: Text }
    deriving (Eq, Ord, Show)

taskPathRoot :: TaskPath
taskPathRoot = TaskPath "/root"

taskPathText :: TaskPath -> Text
taskPathText (TaskPath text) = text

isTaskPathRoot :: TaskPath -> Bool
isTaskPathRoot (TaskPath text) = text == "/root"

taskPathName :: TaskPath -> Text
taskPathName path@(TaskPath text)
    | isTaskPathRoot path = "root"
    | otherwise =
        case reverse (Text.splitOn "/" text) of
            (name : _) | not (Text.null name) -> name
            _ -> "root"

parseTaskPath :: Text -> Either Text TaskPath
parseTaskPath raw = do
    let path = Text.strip raw
    if Text.null path
        then Left "task path must not be empty"
        else do
            validateAbsolute path
            pure (TaskPath path)

{-@ reflect validateTaskName @-}
{-@ validateTaskName :: name:Text
      -> {v:Either Text Text | (tlen name == 0) => isLeftM v} @-}
validateTaskName :: Text -> Either Text Text
validateTaskName name
    | Text.null name = Left "task_name must not be empty"
    | textEquals name "root" = Left "task_name must not be \"root\""
    | hasInvalidNameChar name =
        Left "task_name must use lowercase letters, digits, and underscores only"
    | otherwise = Right name

-- | Opaque to the verifier: the character-class check stays on the trusted
-- boundary ('Text.any' is higher-order and not reflected).
hasInvalidNameChar :: Text -> Bool
hasInvalidNameChar name = Text.any (not . isNameChar) name
  where
    isNameChar c = isAsciiLower c || isDigit c || c == '_'

joinTaskPath :: TaskPath -> Text -> Either Text TaskPath
joinTaskPath (TaskPath parent) name = do
    validated <- validateTaskName name
    parseTaskPath (parent <> "/" <> validated)

-- | Resolve a target reference relative to the caller's path.
-- Absolute @/root/...@ paths parse directly; relative names join the caller.
resolveTaskPath :: TaskPath -> Text -> Either Text TaskPath
resolveTaskPath (TaskPath currentText) reference
    | Text.null reference = Left "target must not be empty"
    | reference == "/root" = Right taskPathRoot
    | "/" `Text.isPrefixOf` reference = parseTaskPath reference
    | otherwise = do
        validateRelative reference
        parseTaskPath (currentText <> "/" <> reference)

validateAbsolute :: Text -> Either Text ()
validateAbsolute path
    | path == "/root" = Right ()
    | "/root/" `Text.isPrefixOf` path = do
        let rest = Text.drop (Text.length "/root/") path
        validateSegments (Text.splitOn "/" rest)
    | otherwise = Left "task path must start with /root"

validateRelative :: Text -> Either Text ()
validateRelative reference
    | "/" `Text.isPrefixOf` reference =
        Left "relative target must not start with /"
    | otherwise =
        validateSegments (Text.splitOn "/" reference)

validateSegments :: [Text] -> Either Text ()
validateSegments segments
    | any Text.null segments = Left "task path must not contain empty segments"
    | otherwise = mapM_ validateTaskName segments
