module Agent.CLI.QuestTreeSpec (spec) where

import Agent.CLI.QuestTree
    ( QuestNode(..)
    , buildQuestTree
    , defaultFocus
    , lineageToRoot
    , questRoots
    )
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.QuestTree" do
    it "walks a linear lineage from focus to root" do
        let tree = buildTree [node "a" Nothing 0, node "b" (Just "a") 1, node "c" (Just "b") 2]
        lineageToRoot tree "c" `shouldBe` Right ["a", "b", "c"]
        lineageToRoot tree "b" `shouldBe` Right ["a", "b"]

    it "treats a parentless node as its own root lineage" do
        let tree = buildTree [node "a" Nothing 0]
        lineageToRoot tree "a" `shouldBe` Right ["a"]

    it "lists quest roots" do
        let tree = buildTree [node "a" Nothing 0, node "b" (Just "a") 1, node "root2" Nothing 0]
        questRoots tree `shouldBe` ["a", "root2"]

    it "picks the newest turn as the default focus" do
        let tree = buildTree [node "a" Nothing 0, node "b" (Just "a") 1, node "c" (Just "b") 2]
        defaultFocus tree `shouldBe` Just "c"

    it "defaults to no focus on an empty tree" do
        defaultFocus (buildTree []) `shouldBe` Nothing

    it "rejects an unknown parent" do
        buildQuestTree [node "a" (Just "ghost") 0]
            `shouldBe` Left "quest tree references unknown parent turn ghost"

    it "rejects duplicate ids" do
        buildQuestTree [node "a" Nothing 0, node "a" Nothing 1]
            `shouldBe` Left "quest tree contains duplicate turn ids"

    it "rejects an unknown focus" do
        let tree = buildTree [node "a" Nothing 0]
        lineageToRoot tree "ghost" `shouldBe` Left "unknown turn id ghost"

    it "detects a parent cycle" do
        let tree = buildTree [node "a" (Just "b") 0, node "b" (Just "a") 1]
        lineageToRoot tree "a" `shouldBe` Left "quest tree lineage contains a cycle"
  where
    node :: Text -> Maybe Text -> Int64 -> QuestNode
    node id_ parent index =
        QuestNode
            { questNodeId = id_
            , questNodeParentId = parent
            , questNodeIndex = index
            }

    buildTree = either (error . ("buildQuestTree failed: " <>) . Text.unpack) id . buildQuestTree
