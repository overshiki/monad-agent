module Agent.CLI.SubagentCatalogConfigSpec (spec) where

import Agent.CLI.SubagentCatalogConfig
    ( decodeSubagentCatalog
    , setSubagentTypeModel
    , subagentCatalogUserPath
    , writeSubagentCatalog
    )
import Agent.GrokBuild.Dialect.SubagentCatalog
    ( CatalogEntry(..)
    , SubagentCatalog(..)
    , catalogModelFor
    )
import Agent.OsPath (unsafeEncodeUtf, unsafeToFilePath)
import Control.Exception.Safe (bracket)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.Directory.OsPath (doesFileExist)
import qualified System.FilePath as FilePath
import System.OsPath (OsPath)
import System.Posix.Temp (mkdtemp)
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.SubagentCatalogConfig" do
    describe "setSubagentTypeModel" do
        it "updates the model on an existing entry" do
            let catalog = SubagentCatalog [entry "explore" (Just "kimi-k3")]
                updated = setSubagentTypeModel catalog "explore" (Just "deepseek-v4-flash")
            catalogModelFor "explore" updated `shouldBe` Just "deepseek-v4-flash"

        it "adds a shadow entry for a type with no entry" do
            let catalog = SubagentCatalog []
                updated = setSubagentTypeModel catalog "general-purpose" (Just "deepseek-v4-pro")
            catalogModelFor "general-purpose" updated `shouldBe` Just "deepseek-v4-pro"

        it "clears to inherit for an existing entry" do
            let catalog = SubagentCatalog [entry "explore" (Just "kimi-k3")]
                updated = setSubagentTypeModel catalog "explore" Nothing
            catalogModelFor "explore" updated `shouldBe` Nothing

        it "leaves a missing type untouched when clearing" do
            let catalog = SubagentCatalog []
                updated = setSubagentTypeModel catalog "general-purpose" Nothing
            updated `shouldBe` catalog

    describe "round-trip" do
        it "encodes and decodes a catalog losslessly" do
            let catalog =
                    SubagentCatalog
                        [ entry "explore" (Just "deepseek-v4-flash")
                        , fullEntry
                        ]
                encoded = Aeson.encode catalog
            decodeSubagentCatalog "test" encoded `shouldBe` Right catalog

        it "writes the user overlay and reads it back" $
            withTempDir "subagents-config-spec-" \home -> do
                let catalog = SubagentCatalog [entry "coder" (Just "deepseek-v4-pro")]
                result <- writeSubagentCatalog home catalog
                result `shouldBe` Right ()
                let path = subagentCatalogUserPath home
                exists <- doesFileExist path
                exists `shouldBe` True
                bytes <- LBS.readFile (unsafeToFilePath path)
                decodeSubagentCatalog "roundtrip" bytes `shouldBe` Right catalog

entry :: Text -> Maybe Text -> CatalogEntry
entry i m =
    CatalogEntry i ("type " <> i) Nothing Nothing m Nothing Nothing Nothing Nothing

fullEntry :: CatalogEntry
fullEntry =
    CatalogEntry
        "reviewer"
        "Strict code reviewer on Kimi K3 (read-only)."
        (Just "quality")
        (Just 1)
        (Just "kimi-k3")
        (Just "You are a strict code reviewer.")
        (Just "reviewing diffs and PRs")
        (Just "explore")
        (Just ["read_file", "grep"])

withTempDir :: String -> (OsPath -> IO a) -> IO a
withTempDir prefix action = do
    root <- getTemporaryDirectory
    bracket
        (unsafeEncodeUtf <$> mkdtemp (root FilePath.</> prefix <> "XXXXXX"))
        (removePathForcibly . unsafeToFilePath)
        action
