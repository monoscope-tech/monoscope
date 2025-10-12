module Pkg.DashboardUtils (replacePlaceholders, variablePresets) where

import Data.Map qualified as Map
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
