-- | @/subagents-config@: tune each subagent type's default model.
--
-- Loads the user overlay (@~/.haskell-agent/subagents.json@), lets the user
-- pick a type and then a model (or "inherit parent"), and persists the
-- result. The change takes effect on the next session: the live tool surface
-- and child runtime bake the catalog in at startup.
module Agent.CLI.Runtime.Repl.SubagentsConfig
    ( handleSubagentsConfig
    ) where

import Agent.CLI.Model.ModelConfig
    ( CatalogModel(..)
    , ModelCatalog(..)
    , connectionBuiltinProvider
    )
import Agent.CLI.TUI.Picker (PickerKey(..), runOverlay)
import Agent.CLI.Runtime.Types (RunResult)
import Agent.CLI.Session.SessionEnv (SessionEnv(..))
import Agent.CLI.Util.Style
    ( glyphOk
    , roleError
    , roleMuted
    , rolePrompt
    , roleSuccess
    , roleWarn
    )
import Agent.CLI.Subagents.SubagentCatalogConfig
    ( decodeSubagentCatalog
    , setSubagentTypeModel
    , subagentCatalogUserPath
    , writeSubagentCatalog
    )
import Agent.CLI.Util.Terminal (resolveColor)
import Agent.CLI.TUI.App (FullscreenRuntime, requestFullscreenFilterChoice)
import Agent.FileRetry (retryOnFileBusy)
import Agent.GrokBuild.Dialect.SubagentCatalog
    ( SubagentCatalog
    , catalogKnownTypes
    , catalogModelFor
    , emptyCatalog
    )
import Agent.OsPath (toText, unsafeToFilePath)
import Agent.Provider (Provider(..))
import Control.Exception.Safe (tryIO)
import qualified Data.ByteString.Lazy as LBS
import Data.List (elemIndex)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory.OsPath (doesFileExist)
import System.IO (hFlush, hIsTerminalDevice, stderr, stdin)
import System.OsPath (OsPath)

-- | Providers whose models can actually serve a child agent. xAI is
-- temporarily banned; meta and claude-code have no child runner.
runnableProviders :: [Provider]
runnableProviders =
    [ OpenAIProvider
    , GeminiProvider
    , OpenRouterProvider
    , DeepSeekProvider
    , KimiProvider
    , KimiCodeProvider
    ]

-- | Model ids the panel offers, in catalog order, excluding providers that
-- cannot serve a child agent.
cyclableModels :: ModelCatalog -> [Text]
cyclableModels catalog =
    [ model.catalogModelId
    | model <- catalog.catalogModels
    , Just provider <- [modelProvider catalog model]
    , provider `elem` runnableProviders
    ]

modelProvider :: ModelCatalog -> CatalogModel -> Maybe Provider
modelProvider catalog model =
    case Map.lookup model.catalogModelConnectionId catalog.catalogConnections of
        Just connection -> connectionBuiltinProvider connection
        Nothing -> Nothing

-- | Read the user overlay, defaulting to an empty catalog when the file is
-- missing or unreadable (matching startup's "defaults kept" behavior).
loadRawOverlay :: OsPath -> IO SubagentCatalog
loadRawOverlay home = do
    let path = subagentCatalogUserPath home
    exists <- doesFileExist path
    if not exists
        then pure emptyCatalog
        else do
            result <-
                tryIO (retryOnFileBusy (LBS.readFile (unsafeToFilePath path)))
            pure $ case result of
                Left _ -> emptyCatalog
                Right bytes ->
                    either
                        (const emptyCatalog)
                        id
                        (decodeSubagentCatalog (toText path) bytes)

handleSubagentsConfig :: SessionEnv -> IO RunResult -> IO RunResult
handleSubagentsConfig env continue = do
    let home = env.sessionHome
        catalog = env.sessionModelCatalog
    rawOverlay <- loadRawOverlay home
    color <- resolveColor stderr
    let types = catalogKnownTypes rawOverlay
        modelIds = cyclableModels catalog
    if null types
        then do
            displayError color "subagents-config: no subagent types configured"
            continue
        else do
            picked <- pickType color env.sessionFullscreen types rawOverlay
            case picked of
                Nothing -> continue
                Just typeId -> do
                    let current = catalogModelFor typeId rawOverlay
                    chosen <- pickModel color env.sessionFullscreen typeId current modelIds
                    case chosen of
                        Nothing -> continue
                        Just modelChoice -> do
                            let next = setSubagentTypeModel rawOverlay typeId modelChoice
                            result <- writeSubagentCatalog home next
                            case result of
                                Left err -> do
                                    displayError color err
                                    continue
                                Right () -> do
                                    displayInfo color
                                        ( "subagent '"
                                            <> typeId
                                            <> "' default model: "
                                            <> maybe "inherit parent" id modelChoice
                                            <> " (applies next session)"
                                        )
                                    continue

-- | @Nothing@ means "inherit parent" (no default model bound).

--------------------------------------------------------------------------------
-- Type picker

data TypeState = TypeState
    { typeList :: [Text]
    , typeSelected :: Int
    }

data TypePick = TypeCancelled | TypeChosen Text

pickType
    :: Bool
    -> Maybe FullscreenRuntime
    -> [Text]
    -> SubagentCatalog
    -> IO (Maybe Text)
pickType color fullscreen types catalog =
    case fullscreen of
        Just runtime -> do
            index <-
                requestFullscreenFilterChoice
                    runtime
                    "subagents"
                    0
                    [ (t, maybe "inherit parent" id (catalogModelFor t catalog))
                    | t <- types
                    ]
            pure (index >>= \i -> atMayIndex i types)
        Nothing -> do
            isTty <- hIsTerminalDevice stdin
            if not isTty
                then do
                    Text.hPutStrLn stderr (renderTypeListing color types catalog)
                    hFlush stderr
                    pure Nothing
                else do
                    result <-
                        runOverlay
                            (renderTypeList color catalog)
                            stepType
                            (TypeState types 0)
                    pure $ case result of
                        Just (TypeChosen t) -> Just t
                        _ -> Nothing

stepType :: PickerKey -> TypeState -> Either TypePick TypeState
stepType key state = case key of
    PickerKeyUp ->
        Right state { typeSelected = clampIndex (length state.typeList) (state.typeSelected - 1) }
    PickerKeyDown ->
        Right state { typeSelected = clampIndex (length state.typeList) (state.typeSelected + 1) }
    PickerKeyConfirm ->
        case atMayIndex state.typeSelected state.typeList of
            Just t -> Left (TypeChosen t)
            Nothing -> Right state
    PickerKeyCancel -> Left TypeCancelled
    _ -> Right state

renderTypeList :: Bool -> SubagentCatalog -> TypeState -> Text
renderTypeList color catalog state =
    Text.intercalate "\n" $
        [ rolePrompt color "subagents"
            <> roleMuted color " · pick a type to tune"
        ]
            <> zipWith renderTypeRow [0 ..] state.typeList
            <> [roleMuted color "↑↓ select · enter pick · esc cancel"]
  where
    renderTypeRow i t =
        let current = maybe "inherit parent" id (catalogModelFor t catalog)
            selected = i == state.typeSelected
        in (if selected then roleWarn color "› " else "  ")
            <> (if selected then roleSuccess color t else t)
            <> roleMuted color ("  " <> current)

renderTypeListing :: Bool -> [Text] -> SubagentCatalog -> Text
renderTypeListing _color types catalog =
    Text.intercalate "\n" $
        ["subagents:"]
            <> [ "  "
                    <> t
                    <> " -> "
                    <> maybe "inherit parent" id (catalogModelFor t catalog)
               | t <- types
               ]

--------------------------------------------------------------------------------
-- Model picker

data ModelState = ModelState
    { modelList :: [Maybe Text]
    , modelSelected :: Int
    }

data ModelPick = ModelCancelled | ModelChosen (Maybe Text)

-- | Outer 'Nothing' means cancelled; inner 'Nothing' means "inherit parent".
pickModel
    :: Bool
    -> Maybe FullscreenRuntime
    -> Text
    -> Maybe Text
    -> [Text]
    -> IO (Maybe (Maybe Text))
pickModel color fullscreen typeId current modelIds =
    case fullscreen of
        Just runtime -> do
            index <-
                requestFullscreenFilterChoice
                    runtime
                    (typeId <> " default model")
                    (currentModelIndex current modelIds)
                    ([("inherit parent", "")] <> [(m, "") | m <- modelIds])
            pure $ case index of
                Just 0 -> Just Nothing
                Just i -> Just (atMayIndex (i - 1) modelIds)
                Nothing -> Nothing
        Nothing -> do
            isTty <- hIsTerminalDevice stdin
            if not isTty
                then pure Nothing
                else do
                    let choices = Nothing : map Just modelIds
                    result <-
                        runOverlay
                            (renderModelList color typeId current)
                            stepModel
                            (ModelState choices 0)
                    pure $ case result of
                        Just (ModelChosen m) -> Just m
                        _ -> Nothing

currentModelIndex :: Maybe Text -> [Text] -> Int
currentModelIndex current modelIds = case current of
    Nothing -> 0
    Just m -> case elemIndex m modelIds of
        Just i -> i + 1
        Nothing -> 0

stepModel :: PickerKey -> ModelState -> Either ModelPick ModelState
stepModel key state = case key of
    PickerKeyUp ->
        Right state { modelSelected = clampIndex (length state.modelList) (state.modelSelected - 1) }
    PickerKeyDown ->
        Right state { modelSelected = clampIndex (length state.modelList) (state.modelSelected + 1) }
    PickerKeyConfirm ->
        case atMayIndex state.modelSelected state.modelList of
            Just m -> Left (ModelChosen m)
            Nothing -> Right state
    PickerKeyCancel -> Left ModelCancelled
    _ -> Right state

renderModelList :: Bool -> Text -> Maybe Text -> ModelState -> Text
renderModelList color typeId current state =
    Text.intercalate "\n" $
        [rolePrompt color ("default model for " <> typeId)]
            <> zipWith renderModelRow [0 ..] state.modelList
            <> [roleMuted color "↑↓ select · enter set · esc cancel"]
  where
    renderModelRow i m =
        let label = maybe "inherit parent" id m
            selected = i == state.modelSelected
            mark = if m == current then roleSuccess color " ✓" else ""
        in (if selected then roleWarn color "› " else "  ")
            <> (if selected then roleSuccess color label else label)
            <> mark

--------------------------------------------------------------------------------
-- Shared helpers

atMayIndex :: Int -> [a] -> Maybe a
atMayIndex i xs
    | i < 0 = Nothing
    | otherwise = case drop i xs of
        x : _ -> Just x
        [] -> Nothing

clampIndex :: Int -> Int -> Int
clampIndex n i
    | n == 0 = 0
    | otherwise = max 0 (min (n - 1) i)

displayError :: Bool -> Text -> IO ()
displayError color message =
    Text.hPutStrLn stderr (roleError color message)

displayInfo :: Bool -> Text -> IO ()
displayInfo color message =
    Text.putStrLn (roleMuted color (glyphOk <> message))
