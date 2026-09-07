-- | Conversions at 'Text'-oriented API boundaries.
--
-- Import 'OsPath' and the standard encoding functions directly from
-- "System.OsPath".
module Agent.OsPath
    ( decodeUtf
    , directoryChain
    , fromText
    , normalizeLexically
    , relativeDisplayPath
    , toText
    , unsafeEncodeUtf
    , unsafeToFilePath
    ) where

import Control.Exception.Safe (impureThrow)
import Data.Foldable (foldl')
import Data.Text (Text)
import qualified Data.Text as Text
import System.OsPath
    ( OsPath
    , decodeUtf
    , dropTrailingPathSeparator
    , encodeUtf
    , isAbsolute
    , joinPath
    , makeRelative
    , normalise
    , splitDirectories
    , takeDirectory
    , (</>)
    )

-- Liquid Haskell verification (see docs/liquid-haskell.md).
--
-- The annotations below are comments for the normal build. The
-- @agent-core-liquid@ package (cabal.liquid.project, GHC 9.14.1) compiles
-- this same source with @-fplugin=LiquidHaskell@, which checks them.
--
-- 'OsPath' is fully opaque to the verifier. The trusted boundary is the
-- small set of documented @assume@ axioms below: the shape of
-- 'directoryChainWalk' (true by construction), list reverse/last facts, and
-- trivial wrapper specs giving library functions a logic presence. The
-- composition logic of 'directoryChain' and the guard structure of
-- 'isInsideRelative' are proved from those facts.
--
-- Termination checking is disabled: 'directoryChainWalk' recurses via the
-- opaque 'takeDirectory', whose progress the checker cannot see.

{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}
{-@ LIQUID "--no-termination" @-}

-- | Logical image of 'reverse' (the Prelude function is not structural and
-- cannot be reflected).
{-@ measure lrev :: [a] -> [a] @-}

-- | Logical last element of a non-empty list.
{-@ measure lastM :: [a] -> a @-}

{-@ assume reverse :: xs:[a] -> {v:[a] | len v == len xs && v == lrev xs} @-}
{-@ assume last :: {xs:[a] | len xs >= 1} -> {v:a | v == lastM xs} @-}

-- | Trusted list facts (all true for non-empty lists).
{-@ assume headReverse :: {xs:[a] | len xs >= 1} -> {v:() | head (lrev xs) == lastM xs} @-}
headReverse :: [a] -> ()
headReverse _ = ()

{-@ assume lastReverse :: {xs:[a] | len xs >= 1} -> {v:() | lastM (lrev xs) == head xs} @-}
lastReverse :: [a] -> ()
lastReverse _ = ()

{-@ assume lastSingleton :: x:a -> {v:() | lastM [x] == x} @-}
lastSingleton :: a -> ()
lastSingleton _ = ()

-- | Opaque logical predicates for path properties the verifier cannot see
-- through. The wrappers below connect code to them at call sites; their
-- definitions are the trusted boundary.
{-@ measure isAbsoluteB :: OsPath -> Bool @-}
{-@ measure isDotDotHeadB :: OsPath -> Bool @-}

-- | Whether a path is absolute. Trusted: matches 'System.OsPath.isAbsolute'.
{-@ assume isAbsolutePath :: p:OsPath -> {v:Bool | v <=> isAbsoluteB p} @-}
isAbsolutePath :: OsPath -> Bool
isAbsolutePath = isAbsolute

-- | Whether the first path component is exactly @".."@. Trusted: matches
-- the case analysis in the body.
{-@ assume isDotDotHead :: rel:OsPath -> {v:Bool | v <=> isDotDotHeadB rel} @-}
isDotDotHead :: OsPath -> Bool
isDotDotHead rel = case splitDirectories rel of
    first : _ -> first == dotdot
    _ -> False

-- | Attach a lemma's postcondition to an expression (ProofCombinators
-- style): @withProof x (lemma args)@ evaluates to @x@, and the lemma's
-- contract is visible to the verifier at that point. Zero cost after
-- inlining.
withProof :: a -> b -> a
withProof x _ = x
{-# INLINE withProof #-}

-- | Directories from @root@ through @cwd@, inclusive.
--
-- If @cwd@ is not contained by @root@, return only @cwd@. Callers use this
-- fallback to avoid discovering files from unrelated filesystem ancestors.
-- Inputs are expected to be absolute and normalized by the caller.
--
-- Verified: the result is non-empty, always ends at @cwd@, and is either
-- exactly @[cwd]@ (the containment fallback) or starts at @root@.
{-@ directoryChain :: root:OsPath -> cwd:OsPath
      -> {v:[OsPath] | len v >= 1
                       && lastM v == cwd
                       && (head v == root || v == [cwd])} @-}
directoryChain :: OsPath -> OsPath -> [OsPath]
directoryChain root cwd =
    case directoryChainWalk root cwd of
        Nothing -> [cwd] `withProof` lastSingleton cwd
        Just dirs ->
            reverse dirs
                `withProof` headReverse dirs
                `withProof` lastReverse dirs

-- | Trusted: the walk either fails or returns a non-empty chain from @dir@
-- down to @root@. True by construction: each step prepends the current
-- directory and recurses to its parent, stopping at @root@ (the base case
-- requires @dir == root@). An in-logic proof is currently blocked because
-- 'OsPath' equality cannot be reflected.
{-@ assume directoryChainWalk :: root:OsPath -> dir:OsPath
      -> Maybe {v:[OsPath] | len v >= 1 && head v == dir && lastM v == root} @-}
directoryChainWalk :: OsPath -> OsPath -> Maybe [OsPath]
directoryChainWalk root = walk
  where
    walk dir
        | dir == root = Just [dir]
        | parent == dir = Nothing
        | otherwise = (dir :) <$> walk parent
      where
        parent = takeDirectory dir

-- | Resolve @.@ and @..@ without touching the filesystem.
--
-- Use only for lexical display or containment. Symlinks can make a normalized
-- spelling name a different target than the OS would resolve.
normalizeLexically :: OsPath -> OsPath
normalizeLexically path =
    case foldl' step [] (splitDirectories (normalise path)) of
        [] -> dot
        components -> dropTrailingPathSeparator (joinPath components)
  where
    step acc component
        | component == dot = acc
        | component == dotdot =
            case acc of
                [] -> [dotdot]
                [root] | isAbsolute root -> acc
                prefix
                    | last prefix == dotdot -> prefix <> [dotdot]
                    | otherwise -> init prefix
        | otherwise = acc <> [component]

-- | Present @path@ relative to @workspace@ when it is inside that tree.
-- Already-relative paths are rewritten through the workspace so @src/../a@
-- becomes @a@. Paths outside the workspace stay absolute after normalization.
relativeDisplayPath :: OsPath -> OsPath -> Text
relativeDisplayPath workspace path
    | Text.null (toText path) = ""
    | Text.null (toText workspace) = toText (normalizeLexically path)
    | otherwise =
        let root = dropTrailingPathSeparator (normalizeLexically workspace)
            candidate = normalizeLexically path
            absolute =
                normalizeLexically $
                    if isAbsolute candidate then candidate else root </> candidate
            relative = makeRelative root absolute
        in if isInsideRelative relative
            then
                if isDot relative
                    then "."
                    else toText relative
            else toText absolute
  where
    isDot rel = rel == dot || Text.null (toText rel)

    -- | Verified: an accepted relative path is never absolute and never
    -- starts with @".."@ — the two ways a display path could escape the
    -- workspace.
    {-@ isInsideRelative :: rel:OsPath
          -> {v:Bool | v => (not (isAbsoluteB rel) && not (isDotDotHeadB rel))} @-}
    isInsideRelative rel =
        not (isAbsolutePath rel) && not (isDotDotHead rel)

dot :: OsPath
dot = fromText "."

dotdot :: OsPath
dotdot = fromText ".."

-- | Pure UTF encoding for path values that originate as Unicode text.
fromText :: Text -> OsPath
fromText = either impureThrow id . encodeUtf . Text.unpack

-- | Compatibility alias for code written against older @filepath@ versions
-- that exported @unsafeEncodeUtf@ from "System.OsPath".
unsafeEncodeUtf :: String -> OsPath
unsafeEncodeUtf = either impureThrow id . encodeUtf

-- | Render a path for human-readable output.
--
-- An undecodable path is represented with its escaped 'Show' form. This
-- fallback is suitable for diagnostics, never filesystem access.
toText :: OsPath -> Text
toText path =
    either (const (Text.pack (show path))) Text.pack (decodeUtf path)

-- | Decode a UTF-encoded path for APIs that still require 'FilePath'.
--
-- Use only for paths known to have originated from UTF text. Invalid encoding
-- is treated as a programming error.
unsafeToFilePath :: OsPath -> FilePath
unsafeToFilePath = either impureThrow id . decodeUtf
