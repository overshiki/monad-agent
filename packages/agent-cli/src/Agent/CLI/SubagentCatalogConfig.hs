-- | Versioned subagent catalog configuration.
--
-- The application ships a default catalog (@config/subagents.default.json@)
-- and merges an optional user overlay from @~/.haskell-agent/subagents.json@.
-- Entry ids in the overlay replace shipped entries with the same id; new
-- entries are appended in file order. Loading never fails the session:
-- problems are reported through a caller-supplied warning sink and the
-- effective catalog falls back to what could be loaded.
module Agent.CLI.SubagentCatalogConfig
    ( decodeSubagentCatalog
    , loadSubagentCatalogAt
    , mergeSubagentCatalogFiles
    , packagedSubagentCatalogPath
    , subagentCatalogUserPath
    ) where

import Agent.CLI.ModelConfig (ModelCatalog)
import Agent.CLI.Models (resolveConfiguredModel)
import Agent.FileRetry (retryOnFileBusy)
import Agent.GrokBuild.Dialect.SubagentCatalog
    ( CatalogEntry(..)
    , SubagentCatalog(..)
    , emptyCatalog
    , mergeSubagentCatalogs
    )
import Agent.OsPath (toText, unsafeEncodeUtf, unsafeToFilePath)
import Control.Exception.Safe (tryIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (eitherDecode)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Paths_agent_cli (getDataFileName)
import qualified System.Directory as Directory
import System.Directory.OsPath (doesFileExist)
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import System.OsPath (OsPath, (</>))

subagentCatalogUserPath :: OsPath -> OsPath
subagentCatalogUserPath home =
    home </> unsafeEncodeUtf ".haskell-agent" </> unsafeEncodeUtf "subagents.json"

packagedSubagentCatalogPath :: IO FilePath
packagedSubagentCatalogPath = do
    cwd <- unsafeEncodeUtf <$> Directory.getCurrentDirectory
    packagedSubagentCatalogPathAt cwd

packagedSubagentCatalogPathAt :: OsPath -> IO FilePath
packagedSubagentCatalogPathAt cwd = do
    installed <- getDataFileName "config/subagents.default.json"
    executable <- Environment.getExecutablePath
    let roots =
            take 16 (iterate FilePath.takeDirectory executable)
                <> take 8
                    (iterate FilePath.takeDirectory (unsafeToFilePath cwd))
        sourceCandidates =
            [ root FilePath.</> "packages/agent-cli/config/subagents.default.json"
            | root <- roots
            ]
    firstExisting
        ( [installed, "config/subagents.default.json"]
            <> sourceCandidates
        ) >>= \case
            Just path -> pure path
            Nothing -> pure installed
  where
    firstExisting = \case
        [] -> pure Nothing
        path : rest ->
            Directory.doesFileExist path >>= \case
                True -> pure (Just path)
                False -> firstExisting rest

-- | Decode and validate one standalone catalog file.
decodeSubagentCatalog :: Text -> LBS.ByteString -> Either Text SubagentCatalog
decodeSubagentCatalog source bytes =
    case eitherDecode bytes of
        Left err ->
            Left ("invalid subagent catalog " <> source <> ": " <> Text.pack err)
        Right catalog -> Right catalog

-- | Merge already-decoded catalog files using the public overlay semantics.
mergeSubagentCatalogFiles
    :: (Text, LBS.ByteString)
    -> Maybe (Text, LBS.ByteString)
    -> Either Text SubagentCatalog
mergeSubagentCatalogFiles (defaultSource, defaultBytes) user = do
    defaults <- decodeSubagentCatalog defaultSource defaultBytes
    overlay <- traverse (uncurry decodeSubagentCatalog) user
    pure case overlay of
        Nothing -> defaults
        Just userCatalog -> mergeSubagentCatalogs defaults userCatalog

-- | Load the shipped default catalog and the user overlay, then drop entries
-- whose @model@ is not present in the loaded model catalog. Warnings go to
-- the caller's sink; failures degrade to the defaults (or an empty catalog)
-- rather than aborting startup.
loadSubagentCatalogAt
    :: (Text -> IO ())
    -- ^ warning sink
    -> ModelCatalog
    -- ^ loaded model catalog, used to validate entry model ids
    -> OsPath
    -- ^ home
    -> OsPath
    -- ^ cwd
    -> IO SubagentCatalog
loadSubagentCatalogAt warn modelCatalog home cwd = do
    defaultPath <- packagedSubagentCatalogPathAt cwd
    defaults <- readCatalogFile (Text.pack defaultPath) defaultPath >>= \case
        Left err -> do
            warn (err <> "; using an empty subagent catalog")
            pure emptyCatalog
        Right catalog -> pure catalog
    let userPath = subagentCatalogUserPath home
    userExists <- doesFileExist userPath
    merged <- if not userExists
        then pure defaults
        else readCatalogFile (toText userPath) (unsafeToFilePath userPath) >>= \case
            Left err -> do
                warn (err <> "; keeping the default subagent catalog")
                pure defaults
            Right overlay ->
                pure (mergeSubagentCatalogs defaults overlay)
    validateEntryModels warn modelCatalog merged

readCatalogFile :: Text -> FilePath -> IO (Either Text SubagentCatalog)
readCatalogFile source path =
    tryIO (retryOnFileBusy (LBS.readFile path)) >>= \case
        Left exception ->
            pure $ Left
                ("could not read subagent catalog " <> source <> ": "
                    <> Text.pack (show exception))
        Right bytes -> pure (decodeSubagentCatalog source bytes)

-- | Entries with a @model@ that does not resolve in the model catalog are
-- dropped from the effective catalog, with one warning per dropped entry.
validateEntryModels
    :: (Text -> IO ())
    -> ModelCatalog
    -> SubagentCatalog
    -> IO SubagentCatalog
validateEntryModels warn modelCatalog (SubagentCatalog entries) = do
    kept <- traverse keepEntry entries
    pure (SubagentCatalog (mapMaybe id kept))
  where
    keepEntry :: CatalogEntry -> IO (Maybe CatalogEntry)
    keepEntry entry = case entry.entryModel of
        Nothing -> pure (Just entry)
        Just modelId
            | Just _ <- resolveConfiguredModel modelCatalog modelId ->
                pure (Just entry)
            | otherwise -> do
                warn
                    ( "subagent catalog entry '" <> entry.entryId
                        <> "' references unknown model '" <> modelId
                        <> "'; dropping the entry"
                    )
                pure Nothing
