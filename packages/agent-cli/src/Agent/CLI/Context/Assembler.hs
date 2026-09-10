-- | Pure context assembly (quest-tree P2, see docs/session-tree.md).
--
-- Context is a function of @(history, budget)@, not a stored sequence. This
-- module defines the segment model and the deterministic segment-1 (nearest
-- history / lineage) selection. Segment 0 (the immutable request prefix:
-- instructions, tools, skills, generated context) is supplied by the caller;
-- segments 2 (relevant) and 3 (reference/compact) are future work.
--
-- Today the input is the already-linearized active transcript, so an unbounded
-- budget reproduces the current context byte-for-byte. Walking parent pointers
-- to linearize a non-linear quest tree arrives with the focus/navigation work
-- (P5); the budget contract below is deliberately independent of how the
-- lineage is produced.
module Agent.CLI.Context.Assembler
    ( ContextBudget(..)
    , ContextSegmentKind(..)
    , ContextSegment(..)
    , unboundedBudget
    , assembleLineage
    ) where

import Agent.OpenAI.Compaction (estimateItemsTokens)
import Agent.Responses.Types (ResponseItem)

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
