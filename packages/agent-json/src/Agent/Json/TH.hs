-- | Compile-time record setters.
--
-- The project compiles with @NoFieldSelectors@ + @DuplicateRecordFields@, under
-- which GHC cannot resolve record-update syntax (@r { f = x }@ requires
-- @RebindableSyntax@ for the non-static path, which we do not enable). The
-- result is hundreds of hand-written "clone with one field changed" helpers
-- that rebuild a record field by field.
--
-- 'makeSetters' generates those helpers mechanically: one total, pure setter
-- per field, implemented as a positional rebuild — exactly the code a human
-- writes, generated at compile time. Uses only the boot @template-haskell@
-- library; no runtime dependency and no @unsafeCoerce@.
--
-- @
-- makeSetters ''ReasoningItem
--   -- generates: setReasoningItemItemId, setReasoningItemSummary,
--   --              setReasoningItemContent, setReasoningItemEncryptedContent,
--   --              setReasoningItemStatus
-- @
module Agent.Json.TH (makeSetters) where

import Control.Monad (forM)
import Data.Char (toUpper)
import Language.Haskell.TH

makeSetters :: Name -> Q [Dec]
makeSetters typeName = do
    info <- reify typeName
    (conName, fields) <- case info of
        TyConI (DataD _ _ _ _ [RecC con fields] _) ->
            pure (con, fields)
        TyConI (NewtypeD _ _ _ _ (RecC con fields) _) ->
            pure (con, fields)
        _ ->
            fail "makeSetters: expected a single-constructor record or newtype"
    let typeBase = nameBase typeName
    fmap concat $ forM (zip [0 ..] fields) $ \(i, (fieldName, _, fieldType)) -> do
        recVar <- newName "rec"
        newVar <- newName "new"
        vars <- mapM (const (newName "x")) fields
        let funName = mkName ("set" ++ typeBase ++ upperFirst (nameBase fieldName))
            sigTy =
                AppT (AppT ArrowT fieldType)
                    (AppT (AppT ArrowT (ConT typeName)) (ConT typeName))
            exprs =
                [ if j == i then VarE newVar else VarE v
                | (j, v) <- zip [0 ..] vars
                ]
            pat =
                ConP conName []
                    [ if j == i then WildP else VarP v
                    | (j, v) <- zip [0 ..] vars
                    ]
            body =
                CaseE (VarE recVar)
                    [ Match pat
                        (NormalB (foldl AppE (ConE conName) exprs))
                        []
                    ]
        pure
            [ SigD funName sigTy
            , FunD funName [Clause [VarP newVar, VarP recVar] (NormalB body) []]
            ]

upperFirst :: String -> String
upperFirst [] = []
upperFirst (c : rest) = toUpper c : rest
