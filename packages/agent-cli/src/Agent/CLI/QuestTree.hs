-- | Quest-tree reconstruction from stored turn lineage (quest-tree P1/P2, see
-- docs/session-tree.md).
--
-- The store keeps every turn's 'parent_turn_id'. This module turns those
-- pointers into a queryable tree: lineage walking (ancestors of a focus),
-- quest roots, and the default focus (the newest turn). It is pure and has no
-- store dependency, so the assembler can consume it directly.
module Agent.CLI.QuestTree
    ( QuestNode(..)
    , QuestTree
    , buildQuestTree
    , lineageToRoot
    , questRoots
    , defaultFocus
    , findByIndex
    ) where

import Data.Int (Int64)
import Data.List (maximumBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)

-- | One turn in the lineage: its id, its parent's id ('Nothing' marks a quest
-- root), and its turn index (used to order roots and pick the default focus).
data QuestNode = QuestNode
    { questNodeId :: !Text
    , questNodeParentId :: !(Maybe Text)
    , questNodeIndex :: !Int64
    }
    deriving (Eq, Show)

-- | Id -> node, plus the reverse parent -> children index.
data QuestTree = QuestTree
    { treeNodes :: !(Map Text QuestNode)
    , treeChildren :: !(Map Text [Text])
    }
    deriving (Eq, Show)

-- | Build a quest tree from its nodes.
--
-- Fails on a duplicate id or a parent pointer that references a missing node.
-- Parent pointers are append-log chains (acyclic by construction), but a cycle
-- would be caught by 'lineageToRoot' rather than here.
buildQuestTree :: [QuestNode] -> Either Text QuestTree
buildQuestTree nodes = do
    let byId = Map.fromList [(n.questNodeId, n) | n <- nodes]
        duplicate =
            length nodes /= Map.size byId
    if duplicate
        then Left "quest tree contains duplicate turn ids"
        else case
            [ parent
            | n <- nodes
            , Just parent <- [n.questNodeParentId]
            , Map.notMember parent byId
            ]
          of
            [] ->
                Right QuestTree
                    { treeNodes = byId
                    , treeChildren = foldr insertChild Map.empty nodes
                    }
            missing ->
                Left
                    ( "quest tree references unknown parent turn "
                        <> head missing
                    )
  where
    insertChild node =
        case node.questNodeParentId of
            Nothing -> id
            Just parent ->
                Map.insertWith
                    (<>)
                    parent
                    [node.questNodeId]

-- | The ancestors of @focus@ from the root to the focus, inclusive, walking
-- parent pointers. The root-most id comes first. Fails on an unknown focus or
-- a cycle (a parent chain longer than the tree).
lineageToRoot :: QuestTree -> Text -> Either Text [Text]
lineageToRoot tree focus = go [] focus
  where
    size = Map.size tree.treeNodes
    go acc current
        | length acc >= size =
            Left "quest tree lineage contains a cycle"
        | otherwise =
            case Map.lookup current tree.treeNodes of
                Nothing -> Left ("unknown turn id " <> current)
                Just node ->
                    case node.questNodeParentId of
                        Nothing -> Right (current : acc)
                        Just parent -> go (current : acc) parent

-- | The ids of every quest root (nodes with no parent), oldest-first.
questRoots :: QuestTree -> [Text]
questRoots tree =
    [ n.questNodeId
    | n <- Map.elems tree.treeNodes
    , case n.questNodeParentId of
        Nothing -> True
        Just _ -> False
    ]

-- | The default focus: the newest turn by turn index. For linear and
-- single-branch sessions this is the current head; multi-branch focus arrives
-- with navigation (P5).
defaultFocus :: QuestTree -> Maybe Text
defaultFocus tree =
    case Map.elems tree.treeNodes of
        [] -> Nothing
        nodes ->
            let newest =
                    maximumBy
                        (\a b -> compare a.questNodeIndex b.questNodeIndex)
                        nodes
            in Just newest.questNodeId

-- | The turn id at the given turn index, if any turn has it.
findByIndex :: QuestTree -> Int64 -> Maybe Text
findByIndex tree index =
    listToMaybe
        [ n.questNodeId
        | n <- Map.elems tree.treeNodes
        , n.questNodeIndex == index
        ]
