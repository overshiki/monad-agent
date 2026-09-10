module Agent.CLI.ContextAssemblerSpec (spec) where

import Agent.CLI.Context.Assembler
    ( ContextBudget(..)
    , assembleLineage
    , unboundedBudget
    )
import Agent.OpenAI.Compaction (estimateItemsTokens, userTextItem)
import Agent.Responses.Types (ResponseItem)
import Control.Monad (when)
import Data.List (isSuffixOf)
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
  where
    mkItems :: Int -> [ResponseItem]
    mkItems n = [userTextItem (Text.pack ("turn " <> show i)) | i <- [1 .. n]]
