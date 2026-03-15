module CLI.UI
  ( selectFromList
  , inputForm
  , withSpinner
  ) where

import Relude

import Brick qualified as B
import Brick.AttrMap qualified as BA
import Brick.Widgets.Border qualified as BB
import Brick.Widgets.Center qualified as BC
import Brick.Widgets.Edit qualified as BE
import Brick.Widgets.List qualified as BL
import Data.Text qualified as T
import Data.Vector qualified as V
import Graphics.Vty qualified as Vty
import UnliftIO.Async (withAsync)
import UnliftIO.Concurrent (threadDelay)

-- Arrow-key list picker. Returns selected value or Nothing on Esc.
selectFromList :: Bool -> Text -> [(Text, Text)] -> IO (Maybe Text)
selectFromList interactive title items
  | not interactive = selectFallback title items
  | otherwise = do
      let initial = BL.list ListName (V.fromList items) 1
      result <- B.defaultMain (listApp title) initial
      pure $ fst . snd <$> BL.listSelectedElement result

selectFallback :: Text -> [(Text, Text)] -> IO (Maybe Text)
selectFallback title items = do
  putTextLn $ "\n" <> title <> ":"
  forM_ (zip [1 :: Int ..] items) $ \(i, (_, label)) ->
    putTextLn $ "  " <> show i <> ". " <> label
  putStr ("Select [1]: " :: String)
  hFlush stdout
  input <- toText <$> getLine
  let idx = fromMaybe 1 (readMaybe @Int (toString input)) - 1
  pure $ fst <$> viaNonEmpty head (drop idx items)

data ListName = ListName
  deriving stock (Eq, Ord, Show)

listApp :: Text -> B.App (BL.List ListName (Text, Text)) () ListName
listApp title =
  B.App
    { B.appDraw = drawList title
    , B.appChooseCursor = B.neverShowCursor
    , B.appHandleEvent = handleListEvent
    , B.appStartEvent = pure ()
    , B.appAttrMap = const listAttrMap
    }

drawList :: Text -> BL.List ListName (Text, Text) -> [B.Widget ListName]
drawList title l =
  [ BC.center $
      BB.borderWithLabel (B.txt $ " " <> title <> " ") $
        B.hLimit 60 $
          B.vLimit (min 20 (V.length (BL.listElements l) + 2)) $
            BL.renderList (\sel (_, label) -> B.txt $ (if sel then "▸ " else "  ") <> label) True l
  ]

cancelList :: B.EventM ListName (BL.List ListName (Text, Text)) ()
cancelList = B.put (BL.list ListName V.empty 1) >> B.halt

handleListEvent :: B.BrickEvent ListName () -> B.EventM ListName (BL.List ListName (Text, Text)) ()
handleListEvent = \case
  B.VtyEvent (Vty.EvKey Vty.KEnter []) -> B.halt
  B.VtyEvent (Vty.EvKey k []) | k == Vty.KEsc || k == Vty.KChar 'q' -> cancelList
  B.VtyEvent ev -> BL.handleListEvent ev
  _ -> pure ()

listAttrMap :: BA.AttrMap
listAttrMap =
  BA.attrMap
    Vty.defAttr
    [(BL.listSelectedAttr, Vty.withStyle (Vty.withForeColor Vty.defAttr Vty.cyan) Vty.bold)]

-- Multi-field form. Returns map of key -> entered value.
inputForm :: Bool -> Text -> [(Text, Text, Text)] -> IO (Map Text Text)
inputForm interactive title fields
  | not interactive = formFallback fields
  | otherwise = do
      let initial = FormState (map (\(k, l, d) -> FormField k l (BE.editorText (FormEditor k) (Just 1) d)) fields) 0
      result <- B.defaultMain (formApp title) initial
      pure $ fromList [(f.key, currentValue f.editor) | f <- result.fields]

formFallback :: [(Text, Text, Text)] -> IO (Map Text Text)
formFallback fields = do
  results <- forM fields $ \(key, label, def) -> do
    let prompt = if T.null def then label <> ": " else label <> " [" <> def <> "]: "
    putStr (toString prompt :: String)
    hFlush stdout
    input <- toText <$> getLine
    pure (key, if T.null input then def else input)
  pure $ fromList results

data FormEditorName = FormEditor Text
  deriving stock (Eq, Ord, Show)

data FormField = FormField {key :: Text, label :: Text, editor :: BE.Editor Text FormEditorName}
data FormState = FormState {fields :: [FormField], focusIdx :: Int}

currentValue :: BE.Editor Text FormEditorName -> Text
currentValue = T.strip . T.unlines . BE.getEditContents

formApp :: Text -> B.App FormState () FormEditorName
formApp title =
  B.App
    { B.appDraw = drawForm title
    , B.appChooseCursor = B.showFirstCursor
    , B.appHandleEvent = handleFormEvent
    , B.appStartEvent = pure ()
    , B.appAttrMap = const $ BA.attrMap Vty.defAttr [(BE.editFocusedAttr, Vty.withStyle Vty.defAttr Vty.underline)]
    }

drawForm :: Text -> FormState -> [B.Widget FormEditorName]
drawForm title st =
  [ BC.center $
      BB.borderWithLabel (B.txt $ " " <> title <> " ") $
        B.hLimit 60 $
          B.vBox $
            [drawField (i == st.focusIdx) f | (i, f) <- zip [0 ..] st.fields]
              <> [B.txt " ", B.txt "  Tab: next field  Enter: submit  Esc: cancel"]
  ]

drawField :: Bool -> FormField -> B.Widget FormEditorName
drawField focused f =
  B.padLeftRight 1 $
    B.vBox
      [ B.txt $ f.label <> ":"
      , (if focused then B.visible else id) $ BE.renderEditor (B.txt . T.unlines) focused f.editor
      ]

handleFormEvent :: B.BrickEvent FormEditorName () -> B.EventM FormEditorName FormState ()
handleFormEvent = \case
  B.VtyEvent (Vty.EvKey Vty.KEnter []) -> B.halt
  B.VtyEvent (Vty.EvKey Vty.KEsc []) -> do
    B.modify $ \s -> s{fields = map (\f -> f{editor = BE.editorText (FormEditor f.key) (Just 1) ""}) s.fields}
    B.halt
  B.VtyEvent (Vty.EvKey (Vty.KChar '\t') []) ->
    B.modify $ \s -> s{focusIdx = (s.focusIdx + 1) `mod` length s.fields}
  B.VtyEvent (Vty.EvKey Vty.KBackTab []) ->
    B.modify $ \s -> s{focusIdx = (s.focusIdx - 1) `mod` length s.fields}
  B.VtyEvent ev -> do
    st <- B.get
    let idx = st.focusIdx
    whenJust (viaNonEmpty head $ drop idx st.fields) $ \field -> do
      newEditor <- B.nestEventM' field.editor $ BE.handleEditorEvent (B.VtyEvent ev)
      let updated = zipWith (\i f -> if i == idx then f{editor = newEditor} else f) [0 ..] st.fields
      B.put st{fields = updated}
  _ -> pure ()

-- Animated spinner shown while an IO action runs. Cleans up terminal on completion.
withSpinner :: Bool -> Text -> IO a -> IO a
withSpinner interactive msg action
  | not interactive = putTextLn msg >> action
  | otherwise = withAsync spinLoop \_ -> action
 where
  frames = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"] :: [Text]
  spinLoop = go $ cycle frames
  go [] = pure ()
  go (f : fs) = do
    putStr $ "\r" <> toString f <> " " <> toString msg
    hFlush stdout
    threadDelay 80_000
    go fs
