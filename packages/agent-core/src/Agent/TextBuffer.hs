-- | Append-friendly text accumulation.
--
-- Chunks are retained in reverse arrival order so appending does not copy the
-- accumulated prefix. Convert to strict 'Text' only at an output boundary.
-- The streaming benchmark also compares this representation with
-- @text-builder@ under the same 'IORef' update pattern.
module Agent.TextBuffer
    ( TextBuffer
    , appendTextBuffer
    , compactTextBuffer
    , emptyTextBuffer
    , textBufferChunkCount
    , textBufferFromText
    , textBufferLength
    , textBufferNull
    , textBufferToText
    ) where

import Data.Text (Text)
import qualified Data.Text as Text

-- Liquid Haskell verification (see docs/liquid-haskell.md).
--
-- The annotations below are comments for the normal build. The
-- @agent-core-liquid@ package (cabal.liquid.project, GHC 9.14.1) compiles
-- this same source with @-fplugin=LiquidHaskell@, which checks them.
--
-- 'Text' is abstract to the verifier; the only tracked property is its
-- length, via the uninterpreted measure 'tlen'. The @assume@ specs for
-- "Data.Text" functions and the 'reverseSumT' permutation axiom are the
-- trusted boundary of the verification. Everything else — the buffer
-- invariant and every exported function's contract — is proved by the SMT
-- solver.

{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}

-- | Logical length of a 'Text' value (uninterpreted; 'Text' is opaque).
{-@ measure tlen :: Text -> Int @-}

{-@ assume Text.length :: t:Data.Text.Text -> {v:Nat | v == tlen t} @-}
{-@ assume Text.null :: t:Data.Text.Text -> {v:Bool | v <=> (tlen t == 0)} @-}
{-@ assume Text.concat :: ts:[Data.Text.Text] -> {v:Data.Text.Text | tlen v == sumT ts} @-}

-- | Sum of the lengths of a list of chunks. Reflected into the logic so it
-- can appear in specifications; PLE unfolds it at list constructors.
{-@ reflect sumT @-}
{-@ sumT :: [Text] -> Nat @-}
sumT :: [Text] -> Int
sumT [] = 0
sumT (t : ts) = Text.length t + sumT ts

-- | Logical image of 'reverse' ('Prelude.reverse' itself is not structural
-- and cannot be reflected).
{-@ measure lrev :: [a] -> [a] @-}

{-@ assume reverse :: xs:[a] -> {v:[a] | v == lrev xs} @-}

-- | Trusted: 'reverse' is a permutation and 'sumT' is permutation-invariant.
-- A proof inside LH would need induction over the foldl-based
-- 'GHC.List.reverse', which the bundled specs do not expose.
{-@ assume reverseSumT :: chunks:[Text] -> {v:() | sumT (lrev chunks) == sumT chunks} @-}
reverseSumT :: [Text] -> ()
reverseSumT _ = ()

-- | Attach a lemma's postcondition to an expression (ProofCombinators
-- style): @withProof x (lemma args)@ evaluates to @x@, and the lemma's
-- contract is visible to the verifier at that point. Zero cost after
-- inlining.
withProof :: a -> b -> a
withProof x _ = x
{-# INLINE withProof #-}

-- | The buffer invariant: @count@ is the number of retained chunks and
-- @size@ is their total length in code units. Every construction site in
-- this module is checked against it; every pattern match may rely on it.
{-@ data TextBuffer = TextBuffer
      { tbSize   :: Nat
      , tbCount  :: Nat
      , tbChunks :: {v:[Text] | len v == tbCount && sumT v == tbSize}
      }
@-}

data TextBuffer = TextBuffer !Int !Int ![Text]

instance Eq TextBuffer where
    left == right = textBufferToText left == textBufferToText right

instance Show TextBuffer where
    showsPrec precedence = showsPrec precedence . textBufferToText

{-@ emptyTextBuffer :: {v:TextBuffer | textBufferLength v == 0 && textBufferChunkCount v == 0} @-}
emptyTextBuffer :: TextBuffer
emptyTextBuffer = TextBuffer 0 0 []

{-@ textBufferFromText :: t:Text
      -> {v:TextBuffer | textBufferLength v == tlen t
                         && textBufferChunkCount v == (if tlen t == 0 then 0 else 1)} @-}
textBufferFromText :: Text -> TextBuffer
textBufferFromText text
    | Text.null text = emptyTextBuffer
    | otherwise = TextBuffer (Text.length text) 1 [text]

{-@ appendTextBuffer :: chunk:Text -> buffer:TextBuffer
      -> {v:TextBuffer | textBufferLength v == textBufferLength buffer + tlen chunk
                         && textBufferChunkCount v
                              == (if tlen chunk == 0
                                    then textBufferChunkCount buffer
                                    else textBufferChunkCount buffer + 1)} @-}
appendTextBuffer :: Text -> TextBuffer -> TextBuffer
appendTextBuffer chunk buffer
    | Text.null chunk = buffer
appendTextBuffer chunk (TextBuffer size count chunks) =
    TextBuffer
        (size + Text.length chunk)
        (count + 1)
        (chunk : chunks)

{-@ textBufferToText :: b:TextBuffer -> {v:Text | tlen v == textBufferLength b} @-}
textBufferToText :: TextBuffer -> Text
textBufferToText (TextBuffer _ _ chunks) =
    Text.concat (reverse chunks) `withProof` reverseSumT chunks

{-@ textBufferNull :: b:TextBuffer -> {v:Bool | v <=> (textBufferLength b == 0)} @-}
textBufferNull :: TextBuffer -> Bool
textBufferNull (TextBuffer size _ _) =
    size == 0

-- | Number of 'Text' code units retained by the buffer. This is constant time
-- and is intended for queue/backpressure accounting.
{-@ measure textBufferLength @-}
{-@ textBufferLength :: TextBuffer -> Nat @-}
textBufferLength :: TextBuffer -> Int
textBufferLength (TextBuffer size _ _) = size

-- | Number of append chunks retained by the buffer.
{-@ measure textBufferChunkCount @-}
{-@ textBufferChunkCount :: TextBuffer -> Nat @-}
textBufferChunkCount :: TextBuffer -> Int
textBufferChunkCount (TextBuffer _ count _) = count

-- | Collapse accumulated chunks after streaming completes.
{-@ compactTextBuffer :: b:TextBuffer
      -> {v:TextBuffer | textBufferLength v == textBufferLength b
                         && textBufferChunkCount v <= textBufferChunkCount b} @-}
compactTextBuffer :: TextBuffer -> TextBuffer
compactTextBuffer buffer@(TextBuffer _ _ chunks) =
    case chunks of
        [] -> buffer
        [_] -> buffer
        _ -> textBufferFromText (textBufferToText buffer)

