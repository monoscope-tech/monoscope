module Pkg.CLIFormat (
  colWidths,
  formatRow,
  extractTextArray,
  extractRows,
  extractInt,
  valToText,
  evalCond,
  sparklineBar,
  renderSummaryItems,
) where

import Relude

import Data.Aeson qualified as AE
import Data.Scientific (FPFormat (..), Scientific, formatScientific, isInteger)
import Data.Text qualified as T
import Data.Vector qualified as V


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
colWidths [] = []
colWidths rows = map (min 60 . foldl' (\acc t -> max acc (T.length t)) 0) $ transpose padded
  where
    ncols = foldl' (\acc r -> max acc (length r)) 0 rows
    padded = map (\r -> r <> replicate (ncols - length r) "") rows


-- | Format a row with given column widths (cols shorter than widths are padded with empty strings).
--
-- >>> formatRow [5, 3] ["hi", "ok"]
-- "hi     ok "
formatRow :: [Int] -> [Text] -> Text
formatRow widths cols =
  T.intercalate "  " $ zipWith padRight widths (cols <> repeat "")


-- | Apply a function over JSON array contents, defaulting to empty list.
withArray :: (V.Vector AE.Value -> [a]) -> Maybe AE.Value -> [a]
withArray f (Just (AE.Array arr)) = f arr
withArray _ _ = []


-- | Extract text values from a JSON array.
--
-- >>> extractTextArray (Just (AE.Array (V.fromList [AE.String "a", AE.String "b"])))
-- ["a","b"]
-- >>> extractTextArray Nothing
-- []
extractTextArray :: Maybe AE.Value -> [Text]
extractTextArray = withArray $ mapMaybe (\case AE.String s -> Just s; _ -> Nothing) . V.toList


-- | Convert a JSON value to text for table display.
--
-- >>> valToText (AE.String "hello")
-- "hello"
-- >>> valToText AE.Null
-- ""
-- >>> valToText (AE.Bool True)
-- "true"
-- >>> valToText (AE.Bool False)
-- "false"
-- >>> valToText (AE.Number 100)
-- "100"
-- >>> valToText (AE.Number 1.5)
-- "1.5"
valToText :: AE.Value -> Text
valToText (AE.String s) = s
valToText AE.Null = ""
valToText (AE.Number n) = showScientific n
valToText (AE.Bool True) = "true"
valToText (AE.Bool False) = "false"
valToText v = decodeUtf8 $ AE.encode v


-- | Extract rows of text from a JSON array of arrays.
--
-- >>> extractRows (Just (AE.Array (V.fromList [AE.Array (V.fromList [AE.String "a", AE.Number 1])])))
-- [["a","1"]]
-- >>> extractRows Nothing
-- []
extractRows :: Maybe AE.Value -> [[Text]]
extractRows = withArray $ map extractRow . V.toList
  where
    extractRow (AE.Array row) = map valToText (V.toList row)
    extractRow _ = []


-- | Extract an integer from a JSON number value.
--
-- >>> extractInt (Just (AE.Number 42))
-- 42
-- >>> extractInt Nothing
-- 0
extractInt :: Maybe AE.Value -> Int
extractInt (Just (AE.Number n)) = round n
extractInt _ = 0


-- | Evaluate a threshold condition against a value. Returns False for unparseable conditions.
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
-- >>> evalCond 5.0 "bogus"
-- False
evalCond :: Double -> Text -> Bool
evalCond v cond
  | "<= " `T.isPrefixOf` cond = maybe False (v <=) (readMaybe $ toString $ T.drop 3 cond)
  | ">= " `T.isPrefixOf` cond = maybe False (v >=) (readMaybe $ toString $ T.drop 3 cond)
  | "< " `T.isPrefixOf` cond = maybe False (v <) (readMaybe $ toString $ T.drop 2 cond)
  | "> " `T.isPrefixOf` cond = maybe False (v >) (readMaybe $ toString $ T.drop 2 cond)
  | otherwise = False


-- | Display-friendly formatting: integers without decimal, floats with minimal precision.
showScientific :: Scientific -> Text
showScientific n
  | isInteger n = show (round n :: Integer)
  | otherwise = toText $ formatScientific Fixed Nothing n


sparklineBlocks :: Text
sparklineBlocks = "▁▂▃▄▅▆▇█"


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
  let maxIdx = T.length sparklineBlocks - 1
      idx = min maxIdx $ max 0 $ round (v * fromIntegral maxIdx)
   in one $ T.index sparklineBlocks idx
