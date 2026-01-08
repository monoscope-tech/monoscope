module Pkg.DashboardUtils (replacePlaceholders, variablePresets, variablePresetsKQL, constantToSQLList, constantToKQLList) where

import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)
import Pkg.Parser (calculateAutoBinWidth)
import Relude
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()
import Utils qualified


-- | Replace all occurrences of {key} in the input text using the provided mapping.
replacePlaceholders :: Map.Map Text Text -> Text -> Text
replacePlaceholders mappng = go
  where
    regex = "\\{\\{([^}]+)\\}\\}" :: Text
    go txt =
      case toString txt =~ toString regex :: (String, String, String, [String]) of
        (before, match, after, [key])
          | not (null match) ->
              let replacement = Map.findWithDefault "" (toText key) mappng
               in toText before <> replacement <> go (toText after)
        _ -> txt


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
-- For {{const-*}} placeholders, looks up the const-*-kql value which has KQL-compatible formatting.
variablePresetsKQL :: Text -> Maybe UTCTime -> Maybe UTCTime -> [(Text, Maybe Text)] -> UTCTime -> Map.Map Text Text
variablePresetsKQL pid mf mt allParams currentTime =
  let basePresets = variablePresets pid mf mt allParams currentTime
      paramsMap = Map.fromList [(k, fromMaybe "" v) | (k, v) <- allParams]
      -- For each const-* key, use the const-*-kql value instead
      kqlRemapping = Map.fromList [(k, v) | (k, _) <- allParams, "const-" `T.isPrefixOf` k, not ("-kql" `T.isSuffixOf` k), let kqlKey = k <> "-kql", v <- maybeToList (Map.lookup kqlKey paramsMap)]
   in Map.union kqlRemapping basePresets


-- | Convert constant results to a SQL list format suitable for IN clauses.
-- For single-column results [["api/users"], ["api/orders"]],
-- this generates: "('api/users', 'api/orders')".
-- For multi-column results, only the first column is used (empty inner lists are skipped).
-- For empty results, generates an empty subquery "(SELECT NULL::text WHERE FALSE)"
-- to avoid SQL three-valued logic issues with NULL in IN clauses.
constantToSQLList :: [[Text]] -> Text
constantToSQLList = \case
  [] -> "(SELECT NULL::text WHERE FALSE)"
  rows -> "(" <> T.intercalate ", " [escapeQuote v | (v : _) <- rows] <> ")"
  where
    escapeQuote v = "'" <> T.replace "'" "''" v <> "'"


-- | Convert constant results to a KQL list format suitable for IN expressions.
-- For single-column results [["api/users"], ["api/orders"]],
-- this generates: ("api/users", "api/orders").
-- KQL uses double quotes for strings with backslash escaping.
-- For empty results, generates a sentinel value that will never match real data.
constantToKQLList :: [[Text]] -> Text
constantToKQLList = \case
  [] -> "(\"__EMPTY_CONST__\")" -- Sentinel value - valid syntax but won't match real data
  rows -> "(" <> T.intercalate ", " [escapeDoubleQuote v | (v : _) <- rows] <> ")"
  where
    escapeDoubleQuote v = "\"" <> T.replace "\"" "\\\"" (T.replace "\\" "\\\\" v) <> "\""
