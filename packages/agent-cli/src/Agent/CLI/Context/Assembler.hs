-- | Pure context assembly (quest-tree P2, see docs/session-tree.md).
--
-- Context is a function of @(history, budget)@, not a stored sequence. This
-- module defines the segment model and the deterministic segment-1 (nearest
-- history / lineage) selection. Segment 0 (the immutable request prefix:
-- instructions, tools, skills, generated context) is supplied by the caller;
-- segments 2 (relevant) and 3 (reference/compact) are future work.
--
-- 'assembleLineage' operates on an already-linearized transcript;
-- 'assembleLineageFromTree' walks a quest tree from a focus. The budget
-- contract is shared and is independent of how the lineage is produced.
module Agent.CLI.Context.Assembler
    ( ContextBudget(..)
    , ContextSegmentKind(..)
    , ContextSegment(..)
    , unboundedBudget
    , assembleLineage
    , assembleLineageFromTree
    ) where

import Agent.CLI.Git.QuestTree (QuestTree, defaultFocus, lineageToRoot)
import Agent.OpenAI.Compaction (estimateItemsTokens)
import Agent.Responses.Types (ResponseItem)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | Where a segment's items sit in the assembled context.
data ContextSegmentKind
    = SegmentPrefix
    -- ^ Immutable request prefix (instructions, tools, skills).
    | SegmentLineage
    -- ^ Nearest history: the focused quest's ancestors, newest first.
    | SegmentReference
    -- ^ Relevant / compacted references (future).
    deriving (Eq, Show)

-- | One slice of the assembled context.
data ContextSegment = ContextSegment
    { contextSegmentKind :: !ContextSegmentKind
    , contextSegmentItems :: ![ResponseItem]
    }
    deriving (Eq, Show)

-- | Per-segment token budgets. 'Nothing' means unbounded.
data ContextBudget = ContextBudget
    { budgetLineageTokens :: !(Maybe Int)
    }
    deriving (Eq, Show)

-- | The budget that reproduces today's context exactly.
unboundedBudget :: ContextBudget
unboundedBudget = ContextBudget { budgetLineageTokens = Nothing }

-- | Segment 1: nearest history. For a linear single-path session the input is
-- the full active transcript, so 'unboundedBudget' reproduces the current
-- context byte-for-byte. A bounded budget trims the OLDEST items first, always
-- preserving the nearest turn.
assembleLineage :: ContextBudget -> [ResponseItem] -> [ResponseItem]
assembleLineage budget items =
    case budget.budgetLineageTokens of
        Nothing -> items
        Just limit -> keepNewestToFit limit items

-- | Segment 1 from a quest tree: resolve the focus, walk its lineage to the
-- root, concatenate each turn's items, then trim to the budget.
--
-- The items map keys are turn ids; the focus defaults to the newest turn.
-- This walks append-log lineage, so an unbounded budget over a linear session
-- (no reset/replace markers) reproduces the transcript byte-for-byte;
-- effect-aware reconstruction of rewound or compacted branches stays with the
-- store's active-transcript load.
assembleLineageFromTree
    :: ContextBudget
    -> QuestTree
    -> Map Text [ResponseItem]
    -> Maybe Text
    -> Either Text [ResponseItem]
assembleLineageFromTree budget tree itemsAt focus = do
    focusId <-
        case focus of
            Just fid -> Right fid
            Nothing ->
                maybe
                    (Left "cannot assemble context from an empty quest tree")
                    Right
                    (defaultFocus tree)
    ids <- lineageToRoot tree focusId
    turns <- traverse (turnItems itemsAt) ids
    pure (assembleLineage budget (concat turns))
  where
    turnItems m tid =
        maybe
            (Left ("no items for turn " <> tid))
            Right
            (Map.lookup tid m)

-- | Keep the newest suffix of @items@ whose estimated token count fits within
-- @limit@. The newest item is kept even if it alone exceeds the limit, so the
-- nearest turn is never lost.
keepNewestToFit :: Int -> [ResponseItem] -> [ResponseItem]
keepNewestToFit limit = go [] 0 . reverse
  where
    go kept tokens items =
        case items of
            [] -> kept
            item : rest
                | null kept ->
                    go [item] (estimateItemsTokens [item]) rest
                | otherwise ->
                    let itemTokens = estimateItemsTokens [item]
                    in if tokens + itemTokens <= limit
                        then go (item : kept) (tokens + itemTokens) rest
                        else kept
