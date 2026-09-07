-- | Pure composer cursor and deletion operations.
module Agent.TUI.Model.Edit
    ( deleteToLineStart
    , deleteToLineEnd
    , deleteWordAfter
    , deleteWordBefore
    , lineEndCursor
    , lineStartCursor
    , moveWordLeft
    , moveWordRight
    ) where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text

-- Liquid Haskell verification (see docs/liquid-haskell.md).
--
-- The annotations below are comments for the normal build. The
-- @agent-tui-liquid@ package (cabal.liquid.project, GHC 9.14.1) compiles
-- this same source with @-fplugin=LiquidHaskell@, which checks them.
--
-- 'Text' is abstract to the verifier; the only tracked property is its
-- length, via the uninterpreted measure 'tlen'. The @assume@ specs for
-- "Data.Text" functions (and the first-order wrappers around predicate
-- taking functions, whose higher-order assumes crash liquid-fixpoint) are
-- the trusted boundary of the verification. Every contract here is proved
-- by the SMT solver from those facts.

{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}

-- | Logical length of a 'Text' value (uninterpreted; 'Text' is opaque).
{-@ measure tlen :: Text -> Int @-}

{-@ assume Text.length :: t:Data.Text.Text -> {v:Nat | v == tlen t} @-}

-- | A 'take' prefix is bounded by both the count and the input, and is
-- exactly the count when the count fits.
{-@ assume Text.take :: n:Nat -> t:Data.Text.Text
      -> {v:Data.Text.Text | tlen v >= 0
                             && tlen v <= n
                             && tlen v <= tlen t
                             && (n <= tlen t => tlen v == n)} @-}

-- | Dropping @n <= tlen t@ characters shortens by exactly @n@.
{-@ assume Text.drop :: n:Nat -> t:Data.Text.Text
      -> {v:Data.Text.Text | tlen v >= 0
                             && tlen v <= tlen t
                             && (n <= tlen t => tlen v == tlen t - n)} @-}

{-@ assume Text.reverse :: t:Data.Text.Text -> {v:Data.Text.Text | tlen v == tlen t} @-}

-- | 'breakOnEnd' splits at the last occurrence: the pieces sum to the input.
{-@ assume Text.breakOnEnd :: pat:Data.Text.Text -> t:Data.Text.Text
      -> {v:(Data.Text.Text, Data.Text.Text) | tlen (fst v) + tlen (snd v) == tlen t
                                                && tlen (fst v) >= 0
                                                && tlen (snd v) >= 0} @-}

-- | Appending adds lengths.
{-@ assume appendText :: a:Data.Text.Text -> b:Data.Text.Text
      -> {v:Data.Text.Text | tlen v == tlen a + tlen b} @-}
appendText :: Text -> Text -> Text
appendText = (<>)

-- | Dropping a prefix of spaces never lengthens.
{-@ assume dropWhileSpace :: t:Data.Text.Text
      -> {v:Data.Text.Text | tlen v >= 0 && tlen v <= tlen t} @-}
dropWhileSpace :: Text -> Text
dropWhileSpace = Text.dropWhile isSpace

-- | Dropping a prefix of non-spaces never lengthens.
{-@ assume dropWhileNotSpace :: t:Data.Text.Text
      -> {v:Data.Text.Text | tlen v >= 0 && tlen v <= tlen t} @-}
dropWhileNotSpace :: Text -> Text
dropWhileNotSpace = Text.dropWhile (not . isSpace)

-- | 'break' at a newline splits into line and remainder.
{-@ assume breakNewline :: t:Data.Text.Text
      -> {v:(Data.Text.Text, Data.Text.Text) | tlen (fst v) + tlen (snd v) == tlen t
                                                && tlen (fst v) >= 0
                                                && tlen (snd v) >= 0} @-}
breakNewline :: Text -> (Text, Text)
breakNewline = Text.break (== '\n')

-- | Clamp to the range [0, hi]. Equal to @max 0 (min hi x)@ when @hi >= 0@,
-- written out so the verifier can reason about it.
{-@ reflect clampInt @-}
{-@ clampInt :: hi:Nat -> x:Int
      -> {v:Int | 0 <= v && v <= hi && (0 <= x && x <= hi => v == x)} @-}
clampInt :: Int -> Int -> Int
clampInt hi x = if x < 0 then 0 else if x > hi then hi else x

-- | Delete whitespace and the previous non-whitespace word before the cursor.
-- The result is shorter than the input and the cursor stays inside it.
{-@ deleteWordBefore :: text:Text -> cursor:Int
      -> {v:(Text, Int) | tlen (fst v) <= tlen text
                          && 0 <= snd v
                          && snd v <= tlen (fst v)} @-}
deleteWordBefore :: Text -> Int -> (Text, Int)
deleteWordBefore text cursor =
    let cursor' = clampInt (Text.length text) cursor
        before = Text.take cursor' text
        after = Text.drop cursor' text
        reversed = Text.reverse before
        withoutSpace = dropWhileSpace reversed
        remaining = dropWhileNotSpace withoutSpace
        kept = Text.reverse remaining
        after'
            | Text.isSuffixOf " " kept
            , Text.isPrefixOf " " after =
                Text.drop 1 after
            | otherwise = after
    in (appendText kept after', Text.length kept)

-- | Delete whitespace and the next non-whitespace word after the cursor.
-- The result is shorter than the input and the cursor stays inside it.
{-@ deleteWordAfter :: text:Text -> cursor:Int
      -> {v:(Text, Int) | tlen (fst v) <= tlen text
                          && 0 <= snd v
                          && snd v <= tlen (fst v)} @-}
deleteWordAfter :: Text -> Int -> (Text, Int)
deleteWordAfter text cursor =
    let cursor' = clampInt (Text.length text) cursor
        before = Text.take cursor' text
        after = Text.drop cursor' text
        withoutSpace = dropWhileSpace after
        remaining = dropWhileNotSpace withoutSpace
    in (appendText before remaining, cursor')

-- | Delete from the cursor to the beginning of its logical line.
-- The result is shorter than the input and the cursor stays inside it.
{-@ deleteToLineStart :: text:Text -> cursor:Int
      -> {v:(Text, Int) | tlen (fst v) <= tlen text
                          && 0 <= snd v
                          && snd v <= tlen (fst v)} @-}
deleteToLineStart :: Text -> Int -> (Text, Int)
deleteToLineStart text cursor =
    let cursor' = clampInt (Text.length text) cursor
        before = Text.take cursor' text
        after = Text.drop cursor' text
        kept = case Text.breakOnEnd "\n" before of
            ("", _) -> ""
            (prefix, _) -> prefix
    in (appendText kept after, Text.length kept)

-- | Delete from the cursor to the end of its logical line.
-- The result is shorter than the input and the cursor stays inside it.
{-@ deleteToLineEnd :: text:Text -> cursor:Int
      -> {v:(Text, Int) | tlen (fst v) <= tlen text
                          && 0 <= snd v
                          && snd v <= tlen (fst v)} @-}
deleteToLineEnd :: Text -> Int -> (Text, Int)
deleteToLineEnd text cursor =
    let cursor' = clampInt (Text.length text) cursor
        before = Text.take cursor' text
        after = Text.drop cursor' text
        keptAfter = case breakNewline after of
            (_, rest) -> rest
    in (appendText before keptAfter, cursor')

-- | The cursor at the start of its logical line: in bounds, never right of
-- the requested position.
{-@ lineStartCursor :: text:Text -> cursor:Int
      -> {v:Nat | v <= clampInt (tlen text) cursor} @-}
lineStartCursor :: Text -> Int -> Int
lineStartCursor text cursor =
    let cursor' = clampInt (Text.length text) cursor
        before = Text.take cursor' text
    in Text.length (fst (Text.breakOnEnd "\n" before))

-- | The cursor at the end of its logical line: in bounds, never left of the
-- requested position.
{-@ lineEndCursor :: text:Text -> cursor:Int
      -> {v:Nat | v <= tlen text && v >= clampInt (tlen text) cursor} @-}
lineEndCursor :: Text -> Int -> Int
lineEndCursor text cursor =
    let cursor' = clampInt (Text.length text) cursor
        after = Text.drop cursor' text
        (line, _) = breakNewline after
    in cursor' + Text.length line

-- | Word-left motion never overshoots the start of the text.
{-@ moveWordLeft :: text:Text -> cursor:Int
      -> {v:Nat | v <= clampInt (tlen text) cursor} @-}
moveWordLeft :: Text -> Int -> Int
moveWordLeft text cursor =
    let cursor' = clampInt (Text.length text) cursor
        reversed = Text.reverse (Text.take cursor' text)
        withoutSpace = dropWhileSpace reversed
        remaining = dropWhileNotSpace withoutSpace
    in Text.length remaining

-- | Word-right motion stays in bounds and never moves left.
{-@ moveWordRight :: text:Text -> cursor:Int
      -> {v:Nat | v <= tlen text && v >= clampInt (tlen text) cursor} @-}
moveWordRight :: Text -> Int -> Int
moveWordRight text cursor =
    let cursor' = clampInt (Text.length text) cursor
        after = Text.drop cursor' text
        withoutWord = dropWhileNotSpace after
        remaining = dropWhileSpace withoutWord
    in Text.length text - Text.length remaining
