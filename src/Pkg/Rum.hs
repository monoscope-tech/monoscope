module Pkg.Rum (AudienceFilters (..), audienceFilters, audienceSessionsSql, audienceKql) where

import Data.List (lookup)
import Data.Text qualified as T
import Pkg.Parser.Expr (sqlStringLit)
import Relude
import Utils (kqlQuoted, nonEmptyT)


data AudienceFilters = AudienceFilters
  { app :: Maybe Text
  , browser :: Maybe Text
  , os :: Maybe Text
  , device :: Maybe Text
  , errored :: Bool
  }
  deriving stock (Eq, Generic, Show)


-- | Parse audience filter query params; empty-string values collapse to absent.
--
-- >>> audienceFilters [("var-browser", Just "Chrome"), ("var-os", Just ""), ("var-errors", Just "true")]
-- AudienceFilters {app = Nothing, browser = Just "Chrome", os = Nothing, device = Nothing, errored = True}
audienceFilters :: [(Text, Maybe Text)] -> AudienceFilters
audienceFilters params = AudienceFilters (param "app") (param "browser") (param "os") (param "device") (param "errors" == Just "true")
  where
    param key = nonEmptyT $ join $ lookup ("var-" <> key) params


-- Ordered rules: embedded browser/OS names must not win over their host family.
data AgentRule = Contains (NonEmpty Text) | Tablet | AnyAgent


families :: [(Text, [(Text, AgentRule)])]
families =
  [ ("browser", [("Edge", Contains ("Edg/" :| ["Edge/", "EdgiOS/", "EdgA/"])), ("Opera", Contains ("OPR/" :| ["Opera", "OPiOS/"])), ("Samsung Internet", Contains (one "SamsungBrowser")), ("Firefox", Contains ("Firefox/" :| ["FxiOS/"])), ("Chrome", Contains ("Chrome/" :| ["CriOS/"])), ("Safari", Contains (one "Safari/"))])
  , ("os", [("Windows", Contains (one "Windows")), ("iOS", Contains ("iPhone" :| ["iPad", "iPod"])), ("macOS", Contains ("Mac OS X" :| ["Macintosh"])), ("Android", Contains (one "Android")), ("ChromeOS", Contains (one "CrOS")), ("Linux", Contains ("Linux" :| ["X11"]))])
  , ("device", [("Tablet", Tablet), ("Mobile", Contains ("Mobile" :| ["iPhone", "iPod"])), ("Desktop", AnyAgent)])
  ]


selections :: AudienceFilters -> [(Text, Text)]
selections f = [(key, value) | (key, Just value) <- [("browser", f.browser), ("os", f.os), ("device", f.device)]]


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
    <> T.intercalate " AND " ("TRUE" : [key <> " = " <> sqlStringLit val | (key, val) <- selections f] <> ["errored = 1" | f.errored])
    <> ") "
  where
    classify rules = "CASE WHEN ua = '' THEN 'Unknown' " <> T.concat ["WHEN " <> match rule <> " THEN " <> sqlStringLit label <> " " | (label, rule) <- rules] <> "ELSE 'Other' END"
    match Tablet = "(ua ILIKE '%iPad%' OR (ua ILIKE '%Android%' AND ua NOT ILIKE '%Mobile%'))"
    match AnyAgent = "TRUE"
    match (Contains tokens) = "(" <> T.intercalate " OR " ["ua ILIKE " <> sqlStringLit ("%" <> token <> "%") | token <- toList tokens] <> ")"


-- | The same ordered family rules drive session Explorer links.
--
-- >>> audienceKql (AudienceFilters Nothing (Just "Unknown") Nothing Nothing False)
-- "(isempty(attributes.user_agent.original) and isempty(resource.user_agent.original))"
-- >>> audienceKql (AudienceFilters Nothing (Just "Netscape") Nothing Nothing False)
-- "1 == 0"
-- >>> audienceKql (audienceFilters [("var-errors", Just "true")])
-- "(status_code == \"ERROR\" or level matches /^error$/ or attributes.exception.type != null)"
audienceKql :: AudienceFilters -> Text
audienceKql f =
  T.intercalate " and "
    $ ["resource.service.name == " <> kqlQuoted value | value <- toList f.app]
    <> [predicate rules value | (key, rules) <- families, Just value <- [lookup key (selections f)]]
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
    -- Negation flips each token test and connective (De Morgan).
    test p field (Contains tokens) = "(" <> T.intercalate (anyOf p) (has p field <$> toList tokens) <> ")"
    test p _ AnyAgent = bool "1 == 0" "1 == 1" p
    test p field Tablet = "(" <> has p field "iPad" <> anyOf p <> "(" <> has p field "Android" <> anyOf (not p) <> has (not p) field "Mobile" <> "))"
    has p field token = field <> bool " !contains " " contains " p <> kqlQuoted token
    anyOf = bool " and " " or "
