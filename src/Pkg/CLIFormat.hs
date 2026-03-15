module Pkg.CLIFormat (
  colWidths,
  formatRow,
  AgentEnvelope (..),
  AgentMeta (..),
  wrapOk,
  wrapError,
  extractTextArray,
  extractRows,
  extractNumericRows,
  extractInt,
  valToText,
  evalCond,
  sparklineBar,
  renderSummaryItems,
) where

import Relude

import Data.Aeson qualified as AE
import Data.Text qualified as T
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE


-- | Pad text to a fixed width.
--
-- >>> padRight 10 "hi"
-- "hi        "
-- >>> padRight 2 "hello"
-- "hello"
padRight :: Int -> Text -> Text
padRight n t = t <> T.replicate (max 0 (n - T.length t)) " "


-- | Render summary items from `field;style⇒value` format into ANSI-colored text.
--
-- >>> renderSummaryItems ["kind;badge-neutral⇒internal", "span_name;badge-neutral⇒runBg"]
-- "\ESC[2minternal\ESC[0m \ESC[2mrunBg\ESC[0m"
-- >>> renderSummaryItems ["status;badge-2xx⇒200", "method;badge-GET⇒GET"]
-- "\ESC[32m200\ESC[0m \ESC[32mGET\ESC[0m"
-- >>> renderSummaryItems ["attributes;text-textWeak⇒{\"foo\":1}"]
-- ""
-- >>> renderSummaryItems ["plain text without separator"]
-- "plain text without separator"
renderSummaryItems :: [Text] -> Text
renderSummaryItems = T.intercalate " " . mapMaybe renderItem
  where
    renderItem item = case T.splitOn ";" item of
      [field, rest] -> case T.splitOn "⇒" rest of
        [style, value]
          | field == "attributes" -> Nothing
          | "text-textWeak" `T.isPrefixOf` style -> Nothing
          | "text-weak" `T.isPrefixOf` style -> Nothing
          | otherwise -> Just $ applyAnsi (stripRight style) value
        _ -> Just item
      _ -> Just item
    stripRight s = fromMaybe s $ T.stripPrefix "right-" s
    applyAnsi style val = case styleToAnsi style of
      "" -> val
      code -> code <> val <> "\ESC[0m"
    styleToAnsi s
      | s `elem` ["badge-2xx", "badge-success", "badge-GET"] = "\ESC[32m"
      | s `elem` ["badge-3xx", "badge-warning", "badge-PUT", "badge-PATCH"] = "\ESC[33m"
      | s `elem` ["badge-4xx", "badge-error", "badge-fatal", "badge-DELETE"] = "\ESC[31m"
      | s == "badge-5xx" = "\ESC[1;31m"
      | s `elem` ["badge-info", "badge-POST"] = "\ESC[36m"
      | s == "badge-neutral" = "\ESC[2m"
      | otherwise = ""


-- | Compute column widths (capped at 60) from rows of text.
--
-- >>> colWidths [["ab", "cdef"], ["x", "y"]]
-- [2,4]
-- >>> colWidths []
-- []
colWidths :: [[Text]] -> [Int]
colWidths rows =
  let ncols = foldl' (\acc r -> max acc (length r)) 0 rows
      getCol i = map (\r -> maybe 0 T.length (viaNonEmpty head $ drop i r)) rows
   in [min 60 (foldl' max 0 (getCol i)) | i <- [0 .. ncols - 1]]


-- | Format a row with given column widths.
--
-- >>> formatRow [5, 3] ["hi", "ok"]
-- "hi     ok "
formatRow :: [Int] -> [Text] -> Text
formatRow widths cols =
  T.intercalate "  " $ zipWith padRight widths (cols <> repeat "")


data AgentEnvelope = AgentEnvelope
  { status :: Text
  , agentData :: AE.Value
  , meta :: AgentMeta
  }
  deriving stock (Generic)
  deriving (AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.StripPrefix "agent", DAE.CamelToSnake]] AgentEnvelope


data AgentMeta = AgentMeta
  { hints :: [Text]
  , nextCommands :: [Text]
  }
  deriving stock (Generic)
  deriving (AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] AgentMeta


-- | Wrap a successful result in an agent envelope.
--
-- >>> import Data.Aeson (encode)
-- >>> encode (wrapOk ("hello" :: Text) ["h1"] ["c1"])
-- "{\"status\":\"ok\",\"data\":\"hello\",\"meta\":{\"hints\":[\"h1\"],\"next_commands\":[\"c1\"]}}"
wrapOk :: AE.ToJSON a => a -> [Text] -> [Text] -> AgentEnvelope
wrapOk val hints' cmds =
  AgentEnvelope
    { status = "ok"
    , agentData = AE.toJSON val
    , meta = AgentMeta{hints = hints', nextCommands = cmds}
    }


-- | Wrap an error message in an agent envelope.
--
-- >>> import Data.Aeson (encode)
-- >>> encode (wrapError "fail" ["check logs"])
-- "{\"status\":\"error\",\"data\":{\"message\":\"fail\"},\"meta\":{\"hints\":[\"check logs\"],\"next_commands\":[]}}"
wrapError :: Text -> [Text] -> AgentEnvelope
wrapError msg hints' =
  AgentEnvelope
    { status = "error"
    , agentData = AE.object ["message" AE..= msg]
    , meta = AgentMeta{hints = hints', nextCommands = []}
    }


-- | Extract text values from a JSON array.
--
-- >>> extractTextArray (Just (AE.Array (V.fromList [AE.String "a", AE.String "b"])))
-- ["a","b"]
-- >>> extractTextArray Nothing
-- []
extractTextArray :: Maybe AE.Value -> [Text]
extractTextArray (Just (AE.Array arr)) = mapMaybe (\case AE.String s -> Just s; _ -> Nothing) (V.toList arr)
extractTextArray _ = []


-- | Convert a JSON value to text for table display.
--
-- >>> valToText (AE.String "hello")
-- "hello"
-- >>> valToText AE.Null
-- ""
-- >>> valToText (AE.Bool True)
-- "True"
valToText :: AE.Value -> Text
valToText (AE.String s) = s
valToText AE.Null = ""
valToText (AE.Number n) = show n
valToText (AE.Bool b) = show b
valToText v = decodeUtf8 $ AE.encode v


-- | Extract rows of text from a JSON array of arrays.
--
-- >>> extractRows (Just (AE.Array (V.fromList [AE.Array (V.fromList [AE.String "a", AE.Number 1])])))
-- [["a","1.0"]]
-- >>> extractRows Nothing
-- []
extractRows :: Maybe AE.Value -> [[Text]]
extractRows (Just (AE.Array arr)) = map extractRow (V.toList arr)
  where
    extractRow (AE.Array row) = map valToText (V.toList row)
    extractRow _ = []
extractRows _ = []


-- | Extract numeric rows from a JSON array of arrays.
--
-- >>> extractNumericRows (Just (AE.Array (V.fromList [AE.Array (V.fromList [AE.Number 1.0, AE.Null])])))
-- [[Just 1.0,Nothing]]
-- >>> extractNumericRows Nothing
-- []
extractNumericRows :: Maybe AE.Value -> [[Maybe Double]]
extractNumericRows (Just (AE.Array arr)) = map extractRow (V.toList arr)
  where
    extractRow (AE.Array row) = map (\case AE.Number n -> Just (realToFrac n); _ -> Nothing) (V.toList row)
    extractRow _ = []
extractNumericRows _ = []


-- | Extract an integer from a JSON number value.
--
-- >>> extractInt (Just (AE.Number 42))
-- 42
-- >>> extractInt Nothing
-- 0
extractInt :: Maybe AE.Value -> Int
extractInt (Just (AE.Number n)) = round n
extractInt _ = 0


-- | Evaluate a threshold condition against a value.
--
-- >>> evalCond 0.5 "< 1.0"
-- True
-- >>> evalCond 2.0 "< 1.0"
-- False
-- >>> evalCond 5.0 "> 3.0"
-- True
-- >>> evalCond 5.0 "<= 5.0"
-- True
-- >>> evalCond 5.0 ">= 6.0"
-- False
evalCond :: Double -> Text -> Bool
evalCond v cond
  | "< " `T.isPrefixOf` cond = maybe False (v <) (readMaybe $ toString $ T.drop 2 cond)
  | "> " `T.isPrefixOf` cond = maybe False (v >) (readMaybe $ toString $ T.drop 2 cond)
  | "<= " `T.isPrefixOf` cond = maybe False (v <=) (readMaybe $ toString $ T.drop 3 cond)
  | ">= " `T.isPrefixOf` cond = maybe False (v >=) (readMaybe $ toString $ T.drop 3 cond)
  | otherwise = True


-- | Convert a normalized value (0-1) to a sparkline bar character.
--
-- >>> sparklineBar (Just 0.0)
-- "\9601"
-- >>> sparklineBar (Just 1.0)
-- "\9608"
-- >>> sparklineBar Nothing
-- " "
sparklineBar :: Maybe Double -> Text
sparklineBar Nothing = " "
sparklineBar (Just v) =
  let blocks = "▁▂▃▄▅▆▇█" :: Text
      idx = min 7 $ max 0 $ round (v * fromIntegral (T.length blocks - 1))
   in T.singleton $ T.index blocks idx
