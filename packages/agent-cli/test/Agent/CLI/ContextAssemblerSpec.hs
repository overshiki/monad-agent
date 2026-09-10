module Agent.CLI.ContextAssemblerSpec (spec) where

import Agent.CLI.Context.Assembler
    ( ContextBudget(..)
    , assembleLineage
    , assembleLineageFromTree
    , unboundedBudget
    )
import Agent.CLI.QuestTree (QuestNode(..), buildQuestTree)
import Agent.OpenAI.Compaction (estimateItemsTokens, userTextItem)
import Agent.Responses.Types (ResponseItem)
import Control.Monad (when)
import Data.Int (Int64)
import Data.List (isSuffixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec

spec :: Spec
spec = describe "Agent.CLI.Context.Assembler" do
    it "reproduces the transcript byte-for-byte with an unbounded budget" do
        let items = mkItems 50
        assembleLineage unboundedBudget items `shouldBe` items

    it "trims to a bounded budget, preserving the nearest turns" do
        let items = mkItems 50
            limit = estimateItemsTokens items `div` 2
            budget = ContextBudget { budgetLineageTokens = Just limit }
            result = assembleLineage budget items
        result `shouldSatisfy` (not . null)
        result `shouldSatisfy` (`isSuffixOf` items)
        last result `shouldBe` last items
        -- when more than one item survives, the total fits the budget
        when (length result > 1) $
            estimateItemsTokens result `shouldSatisfy` (<= limit)

    it "never drops the nearest item even at a zero budget" do
        let items = mkItems 10
            result =
                assembleLineage
                    (ContextBudget { budgetLineageTokens = Just 0 })
                    items
        result `shouldBe` [last items]

    describe "assembleLineageFromTree" do
        let items =
                Map.fromList
                    [ ("a", [userTextItem "a0"])
                    , ("b", [userTextItem "b0", userTextItem "b1"])
                    , ("c", [userTextItem "c0"])
                    , ("d", [userTextItem "d0"])
                    ]
            tree =
                either (error . Text.unpack) id $
                    buildQuestTree
                        [ node "a" Nothing 0
                        , node "b" (Just "a") 1
                        , node "c" (Just "b") 2
                        , node "d" (Just "a") 1
                        ]

        it "walks the full lineage of a linear branch" do
            assembleLineageFromTree unboundedBudget tree items (Just "c")
                `shouldBe` Right
                    [userTextItem "a0", userTextItem "b0", userTextItem "b1", userTextItem "c0"]

        it "walks only the focused branch's ancestors" do
            assembleLineageFromTree unboundedBudget tree items (Just "d")
                `shouldBe` Right [userTextItem "a0", userTextItem "d0"]

        it "defaults the focus to the newest turn" do
            assembleLineageFromTree unboundedBudget tree items Nothing
                `shouldBe` Right
                    [userTextItem "a0", userTextItem "b0", userTextItem "b1", userTextItem "c0"]

        it "still trims to a bounded budget" do
            let result =
                    assembleLineageFromTree
                        (ContextBudget { budgetLineageTokens = Just 0 })
                        tree
                        items
                        (Just "c")
            result `shouldBe` Right [userTextItem "c0"]

        it "reports a missing item map entry" do
            assembleLineageFromTree unboundedBudget tree (Map.delete "b" items) (Just "c")
                `shouldBe` Left "no items for turn b"

        it "reports an empty tree" do
            assembleLineageFromTree unboundedBudget (either (error . Text.unpack) id (buildQuestTree [])) items Nothing
                `shouldBe` Left "cannot assemble context from an empty quest tree"
  where
    mkItems :: Int -> [ResponseItem]
    mkItems n = [userTextItem (Text.pack ("turn " <> show i)) | i <- [1 .. n]]

    node :: Text -> Maybe Text -> Int64 -> QuestNode
    node id_ parent index =
        QuestNode
            { questNodeId = id_
            , questNodeParentId = parent
            , questNodeIndex = index
            }
