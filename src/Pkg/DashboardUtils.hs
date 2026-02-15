module Pkg.DashboardUtils (replacePlaceholders, variablePresets, variablePresetsKQL, constantToSQLList, constantToKQLList) where

import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)
import Pkg.Parser (calculateAutoBinWidth)
import Relude
import Utils qualified


-- | Replace all occurrences of {{key}} in the input text using the provided mapping.
-- Unknown placeholders are left as-is (useful for debugging).
--
-- >>> replacePlaceholders (Map.fromList [("name", "world")]) "Hello {{name}}!"
-- "Hello world!"
-- >>> replacePlaceholders (Map.fromList [("a", "1"), ("b", "2")]) "{{a}} + {{b}}"
-- "1 + 2"
-- >>> replacePlaceholders Map.empty "{{missing}}"
-- "{{missing}}"
replacePlaceholders :: Map.Map Text Text -> Text -> Text
replacePlaceholders mappng = Map.foldlWithKey' (\t k v -> T.replace ("{{" <> k <> "}}") v t) ?? mappng


variablePresets :: Text -> Maybe UTCTime -> Maybe UTCTime -> [(Text, Maybe Text)] -> UTCTime -> Map Text Text
variablePresets pid mf mt allParams currentTime =
  let fmt = maybe "" Utils.formatUTC
      andPrefix = (" AND " <>)
      clause field = case (mf, mt) of
        (Nothing, Nothing) -> ""
        (Just a, Nothing) -> andPrefix $ "(" <> field <> " >= '" <> Utils.formatUTC a <> "')"
        (Nothing, Just b) -> andPrefix $ "(" <> field <> " <= '" <> Utils.formatUTC b <> "')"
        (Just a, Just b) -> andPrefix $ "(" <> field <> " >= '" <> Utils.formatUTC a <> "' AND " <> field <> " <= '" <> Utils.formatUTC b <> "')"
      allParams' = allParams <&> second maybeToMonoid
      rollupInterval = calculateAutoBinWidth (mf, mt) currentTime
   in Map.fromList
        $ [ ("project_id", pid)
          , ("from", andPrefix $ fmt mf)
          , ("to", andPrefix $ fmt mt)
          , ("time_filter", clause "timestamp")
          , ("time_filter_sql_created_at", clause "created_at")
          , ("rollup_interval", rollupInterval)
          ]
        <> allParams'


-- | Like variablePresets but uses KQL format for constants.
-- Automatically maps {{const-*}} placeholders to their KQL-formatted values.
-- Users just use {{const-foo}} in both SQL and KQL - the system handles formatting.
-- Filters out -kql suffix entries from the output.
variablePresetsKQL :: Text -> Maybe UTCTime -> Maybe UTCTime -> [(Text, Maybe Text)] -> UTCTime -> Map.Map Text Text
variablePresetsKQL pid mf mt allParams currentTime =
  let basePresets = variablePresets pid mf mt allParams currentTime
      paramsMap = Map.fromList [(k, fromMaybe "" v) | (k, v) <- allParams]
      kqlRemapping = Map.fromList [(k, v) | (k, _) <- allParams, "const-" `T.isPrefixOf` k, not ("-kql" `T.isSuffixOf` k), Just v <- [Map.lookup (k <> "-kql") paramsMap]]
      filteredBase = Map.filterWithKey (\k _ -> not ("-kql" `T.isSuffixOf` k)) basePresets
   in Map.union kqlRemapping filteredBase


-- >>> constantToSQLList [["api/users"], ["api/orders"]]
-- "('api/users', 'api/orders')"
-- >>> constantToSQLList [["foo'bar"]]
-- "('foo''bar')"
-- >>> constantToSQLList []
-- "(SELECT NULL::text WHERE FALSE)"
constantToSQLList :: [[Text]] -> Text
constantToSQLList = \case
  [] -> "(SELECT NULL::text WHERE FALSE)"
  rows -> "(" <> T.intercalate ", " [escapeQuote v | (v : _) <- rows] <> ")"
  where
    escapeQuote v = "'" <> T.replace "'" "''" v <> "'"


-- >>> constantToKQLList [["api/users"], ["api/orders"]]
-- "(\"api/users\", \"api/orders\")"
-- >>> constantToKQLList [["foo\"bar"]]
-- "(\"foo\\\"bar\")"
-- >>> constantToKQLList [["back\\slash"]]
-- "(\"back\\\\slash\")"
-- >>> constantToKQLList []
-- "(\"__EMPTY_CONST__\")"
constantToKQLList :: [[Text]] -> Text
constantToKQLList = \case
  [] -> "(\"__EMPTY_CONST__\")" -- Sentinel value - valid syntax but won't match real data
  rows -> "(" <> T.intercalate ", " [escapeDoubleQuote v | (v : _) <- rows] <> ")"
  where
    escapeDoubleQuote v = "\"" <> T.replace "\"" "\\\"" (T.replace "\\" "\\\\" v) <> "\""
