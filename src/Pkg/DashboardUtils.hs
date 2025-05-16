module Pkg.DashboardUtils (replacePlaceholders, variablePresets) where

import Data.Map qualified as M
import Data.Text qualified as T
import Data.Time (UTCTime)
import Relude
import Text.Regex.TDFA ((=~))
import Utils qualified


-- | Replace all occurrences of {key} in the input text using the provided mapping.
replacePlaceholders :: M.Map Text Text -> Text -> Text
replacePlaceholders mappng input = go input
  where
    regex = "\\{\\{([^}]+)\\}\\}" :: Text
    go txt =
      case T.unpack txt =~ T.unpack regex :: (String, String, String, [String]) of
        (before, match, after, [key])
          | not (null match) ->
              let replacement = M.findWithDefault (T.pack match) (T.pack key) mappng
               in T.pack before <> replacement <> go (T.pack after)
        _ -> txt


variablePresets :: Text -> Maybe UTCTime -> Maybe UTCTime -> [(Text, Maybe Text)] -> Map Text Text
variablePresets pid mf mt allParams =
  let fmt = maybe "" Utils.formatUTC
      andPrefix = (" AND " <>)
      clause field = case (mf, mt) of
        (Nothing, Nothing) -> ""
        (Just a, Nothing) -> andPrefix $ "(" <> field <> " >= '" <> Utils.formatUTC a <> "')"
        (Nothing, Just b) -> andPrefix $ "(" <> field <> " <= '" <> Utils.formatUTC b <> "')"
        (Just a, Just b) -> andPrefix $ "(" <> field <> " >= '" <> Utils.formatUTC a <> "' AND " <> field <> " <= '" <> Utils.formatUTC b <> "')"
      allParams' = allParams <&> \(a, bm) -> (a, maybeToMonoid bm)
   in M.fromList
        $ [ ("project_id", pid)
          , ("from", andPrefix $ fmt mf)
          , ("to", andPrefix $ fmt mt)
          , ("time_filter", clause "timestamp")
          , ("time_filter_sql_created_at", clause "created_at")
          ]
        <> allParams'
