-- | Small git utilities shared by session persistence and the REPL.
module Agent.CLI.Git
    ( detectGitBranch
    ) where

import Agent.OsPath (unsafeToFilePath)
import Control.Exception.Safe (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (ExitCode(..))
import System.OsPath (OsPath)
import System.Process (readProcessWithExitCode)

-- | The current branch of the repository at @cwd@. Returns @"detached"@ for a
-- detached HEAD and @""@ when the directory is not a git repository or git
-- fails.
detectGitBranch :: OsPath -> IO Text
detectGitBranch cwd = do
    result <-
        (try $
            readProcessWithExitCode
                "git"
                ["-C", unsafeToFilePath cwd, "rev-parse", "--abbrev-ref", "HEAD"]
                "")
            :: IO (Either SomeException (ExitCode, String, String))
    pure $ case result of
        Right (ExitSuccess, output, _) ->
            let branch = Text.strip (Text.pack output)
            in if Text.null branch
                then ""
                else if branch == "HEAD" then "detached" else branch
        _ -> ""
