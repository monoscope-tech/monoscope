module Pkg.Rum (AudienceFilters (..), audienceFilters, audienceParams, audienceSessionsSql, audienceKql) where

import Data.Aeson qualified as AE
import Data.List (lookup)
import Data.Text qualified as T
import Pkg.Parser.Expr (sqlStringLit)
import Relude
import Utils (kqlQuoted, nonEmptyT)
import Web.FormUrlEncoded (FromForm)


data AudienceFilters = AudienceFilters
  { app :: Maybe Text
  , browser :: Maybe Text
  , os :: Maybe Text
  , device :: Maybe Text
  , errored :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, FromForm)


audienceFilters :: [(Text, Maybe Text)] -> AudienceFilters
audienceFilters params = AudienceFilters (param "app") (param "browser") (param "os") (param "device") (param "errors" == Just "true")
  where
    param key = nonEmptyT $ join $ lookup ("var-" <> key) params


audienceParams :: AudienceFilters -> [(Text, Maybe Text)]
audienceParams f = [("var-app", f.app), ("var-browser", f.browser), ("var-os", f.os), ("var-device", f.device), ("var-errors", Just $ if f.errored then "true" else "false")]


-- Ordered rules: embedded browser/OS names must not win over their host family.
data AgentRule = Contains (NonEmpty Text) | Tablet | AnyAgent


families :: [(Text, [(Text, AgentRule)])]
families =
  [ ("browser", [("Edge", Contains ("Edg/" :| ["Edge/", "EdgiOS/", "EdgA/"])), ("Opera", Contains ("OPR/" :| ["Opera", "OPiOS/"])), ("Samsung Internet", Contains ("SamsungBrowser" :| [])), ("Firefox", Contains ("Firefox/" :| ["FxiOS/"])), ("Chrome", Contains ("Chrome/" :| ["CriOS/"])), ("Safari", Contains ("Safari/" :| []))])
  , ("os", [("Windows", Contains ("Windows" :| [])), ("iOS", Contains ("iPhone" :| ["iPad", "iPod"])), ("macOS", Contains ("Mac OS X" :| ["Macintosh"])), ("Android", Contains ("Android" :| [])), ("ChromeOS", Contains ("CrOS" :| [])), ("Linux", Contains ("Linux" :| ["X11"]))])
  , ("device", [("Tablet", Tablet), ("Mobile", Contains ("Mobile" :| ["iPhone", "iPod"])), ("Desktop", AnyAgent)])
  ]


audienceSessionsSql :: Text -> Text -> AudienceFilters -> Text
audienceSessionsSql browserSql errorSql f =
  "WITH rum_session_events AS (SELECT attributes___session___id AS session, "
    <> "MAX(COALESCE(NULLIF(attributes___user_agent___original, ''), NULLIF(resource___user_agent___original, ''), '')) AS ua, "
    <> "MAX(NULLIF(attributes___user___id, '')) AS user_id, MAX(NULLIF(attributes___user___email, '')) AS email, MAX(NULLIF(attributes___user___full_name, '')) AS full_name, "
    <> "MAX(timestamp) AS last_seen, MAX(CASE WHEN "
    <> errorSql
    <> " THEN 1 ELSE 0 END) AS errored "
    <> "FROM otel_logs_and_spans WHERE {{query_ast_filters}} AND "
    <> browserSql
    <> " AND NULLIF(attributes___session___id, '') IS NOT NULL GROUP BY attributes___session___id), "
    <> "rum_sessions AS (SELECT *, "
    <> T.intercalate ", " [classify rules <> " AS " <> key | (key, rules) <- families]
    <> " FROM rum_session_events), "
    <> "audience_sessions AS (SELECT * FROM rum_sessions WHERE "
    <> T.intercalate " AND " ("TRUE" : [key <> " = " <> sqlStringLit val | (key, Just val) <- [("browser", f.browser), ("os", f.os), ("device", f.device)]] <> ["errored = 1" | f.errored])
    <> ") "
  where
    classify rules = "CASE WHEN ua = '' THEN 'Unknown' " <> T.concat ["WHEN " <> match rule <> " THEN " <> sqlStringLit label <> " " | (label, rule) <- rules] <> "ELSE 'Other' END"
    match Tablet = "(ua ILIKE '%iPad%' OR (ua ILIKE '%Android%' AND ua NOT ILIKE '%Mobile%'))"
    match AnyAgent = "TRUE"
    match (Contains tokens) = "(" <> T.intercalate " OR " ["ua ILIKE " <> sqlStringLit ("%" <> token <> "%") | token <- toList tokens] <> ")"


-- The same ordered family rules drive session Explorer links.
audienceKql :: AudienceFilters -> Text
audienceKql f =
  T.intercalate " and "
    $ ["resource.service.name == " <> kqlQuoted value | value <- toList f.app]
    <> [predicate rules value | (key, rules) <- families, Just value <- [join $ lookup key [("browser", f.browser), ("os", f.os), ("device", f.device)]]]
    <> ["(status_code == \"ERROR\" or level matches /^error$/ or attributes.exception.type != null)" | f.errored]
  where
    attribute = "attributes.user_agent.original"
    resource = "resource.user_agent.original"
    predicate rules value
      | value == "Unknown" = "(isempty(" <> attribute <> ") and isempty(" <> resource <> "))"
      | value /= "Other" && isNothing (lookup value rules) = "1 == 0"
      | otherwise = "((isnotempty(" <> attribute <> ") and " <> select attribute <> ") or (isempty(" <> attribute <> ") and isnotempty(" <> resource <> ") and " <> select resource <> "))"
      where
        (before, rest) = break ((== value) . fst) rules
        select field = "(" <> T.intercalate " and " ([test False field rule | (_, rule) <- before] <> [maybe "1 == 1" (test True field . snd) $ listToMaybe rest]) <> ")"
    test positive field (Contains tokens) = "(" <> T.intercalate (if positive then " or " else " and ") [field <> (if positive then " contains " else " !contains ") <> kqlQuoted token | token <- toList tokens] <> ")"
    test positive _ AnyAgent = if positive then "1 == 1" else "1 == 0"
    test positive field Tablet =
      let has token = field <> " contains " <> kqlQuoted token
          lacks token = field <> " !contains " <> kqlQuoted token
       in if positive
            then "(" <> has "iPad" <> " or (" <> has "Android" <> " and " <> lacks "Mobile" <> "))"
            else "(" <> lacks "iPad" <> " and (" <> lacks "Android" <> " or " <> has "Mobile" <> "))"
