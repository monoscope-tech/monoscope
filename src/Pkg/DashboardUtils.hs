module Pkg.DashboardUtils (replacePlaceholders, variablePresets, constantToSQLList, constantsToMap) where

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


-- | Convert constant results to a SQL list format suitable for IN clauses.
-- For single-column results [["api/users"], ["api/orders"]],
-- this generates: "('api/users', 'api/orders')".
-- For multi-column results, only the first column is used (empty inner lists are skipped).
-- For empty results, generates an empty subquery "(SELECT NULL::text WHERE FALSE)"
-- to avoid SQL three-valued logic issues with NULL in IN clauses.
constantToSQLList :: [[Text]] -> Text
constantToSQLList = \case
  [] -> "(SELECT NULL::text WHERE FALSE)"
  rows ->
    let escapeValue v = "'" <> T.replace "'" "''" v <> "'"
     in "(" <> T.intercalate ", " [escapeValue v | (v : _) <- rows] <> ")"


-- | Convert a list of processed constants (with results) to a map for placeholder substitution.
-- Each constant with key "foo" becomes available as "const-foo" placeholder.
constantsToMap :: [(Text, [[Text]])] -> Map Text Text
constantsToMap = Map.fromList . map (bimap ("const-" <>) constantToSQLList)
