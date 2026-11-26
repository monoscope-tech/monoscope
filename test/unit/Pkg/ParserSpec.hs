module Pkg.ParserSpec (spec) where

import Data.Either.Extra (fromRight')
import Data.Text qualified as T
import Data.Time (addUTCTime)
import NeatInterpolation (text)
import Pkg.Parser (
  PageCursor (..),
  PageDirection (..),
  QueryComponents (finalAlertQuery, finalSummarizeQuery, percentilesInfo),
  ScopedQuery (..),
  SqlQueryCfg (..),
  applyScopedQuery,
  defPid,
  defSqlQueryCfg,
  fixedUTCTime,
  parseQueryToAST,
  parseQueryToComponents,
 )
import Pkg.Parser.Expr (errorFlagSql)
import Pkg.Parser.Stats (Sources (..))
import Relude
import System.Directory (doesFileExist)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldNotSatisfy, shouldSatisfy)


goldenText :: FilePath -> Text -> Expectation
goldenText name actual = do
  let path = "./test/golden/" <> name
  update <- (== Just "true") <$> lookupEnv "UPDATE_SNAPSHOTS"
  exists <- doesFileExist path
  if
    | update -> writeFileText path actual
    | exists -> readFileBS path >>= (actual `shouldBe`) . decodeUtf8
    | otherwise -> expectationFailure $ "missing golden " <> path <> "; rerun with UPDATE_SNAPSHOTS=true"


-- Normalize text by removing newlines, carriage returns, tabs, and extra spaces
normT :: Text -> Text
normT = unwords . words . T.filter (`notElem` ['\n', '\r', '\t'])


-- Check that parse result is Left with expected error message
shouldFailWith :: Either Text a -> Text -> IO ()
shouldFailWith (Left err) expected = normT err `shouldBe` normT expected
shouldFailWith (Right _) _ = fail "Expected parse error"


spec :: Spec
spec = do
  describe "shared query scope" do
    it "overrides only the investigation boundary" do
      let fromTime = addUTCTime (-3600) fixedUTCTime
          scope = ScopedQuery defPid (Just fromTime, Just fixedUTCTime) (Just "production") (Just "checkout") (Just "trace-1")
          cfg = (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing){alertLookbackMins = 15, cursorM = Just $ PageCursor PageNewer fixedUTCTime}
          scoped = applyScopedQuery scope cfg
      (scoped.pid, scoped.dateRange, scoped.environment, scoped.service) `shouldBe` (defPid, (Just fromTime, Just fixedUTCTime), Just "production", Just "checkout")
      (scoped.alertLookbackMins, scoped.cursorM, scope.traceId) `shouldBe` (15, Just (PageCursor PageNewer fixedUTCTime), Just "trace-1")

  describe "incomplete comparison guidance" do
    forM_ ["==", "!=", ">=", "<=", ">", "<"] \op ->
      it ("explains a missing value after " <> toString op) do
        parseQueryToAST ("level " <> op) `shouldSatisfy` either (T.isInfixOf $ "Add a value after " <> op) (const False)
    it "does not suggest a value for an unclosed string" do
      parseQueryToAST "level == \"==" `shouldSatisfy` either (not . T.isInfixOf "Add a value") (const False)
  describe "parseQueryToSQL" do
    -- Each case compiles to the SQL pinned in test/golden/kql-to-sql.txt (UPDATE_SNAPSHOTS=true rewrites it).
    it "compiles KQL to its pinned SQL" do
      let compile = fromRight' . parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing)
          rows = fst . compile
          summary = fromMaybe "" . (.finalSummarizeQuery) . snd . compile
          cases :: [(Text, Text -> Text, Text)]
          cases =
            [ ("rows", rows, "timestamp >= ago(7d)")
            , ("rows", rows, "timestamp == now()")
            , ("rows", rows, "name==\"GET\"")
            , -- Lowercase `desc`: this branch now builds its ORDER BY with buildOrderBy
              -- like every other path, rather than hand-rolling an uppercase one.
              ("summary", summary, "name==\"GET\"")
            , ("summary", summary, "name==\"GET\" | summarize count(*) by bin(timestamp, 1d)")
            , -- `bin(timestamp, 60)` is 60 SECONDS. These two previously asserted
              -- `5 minutes`, which was the silent fallback for any width the parser could
              -- not read — the bug, encoded as the expectation.
              ("rows", rows, "name==\"GET\" | summarize sum(attributes.client) by attributes.client, bin(timestamp, 60)")
            , -- ORDER BY uses GROUP BY column (kind) since timestamp is not in GROUP BY
              ("rows", rows, "timestamp >= ago(7d) | summarize TotalCount = count() by kind")
            , ("rows", rows, "name==\"GET\" | summarize sum(attributes.client) by attributes.client, bin(timestamp, 60) | sort by parent_id asc | take 1000")
            , ("rows", rows, "| summarize count(*) by bin_auto(timestamp)")
            , ("rows", rows, "| summarize count() / 5.0 by bin_auto(timestamp)")
            , ("rows", rows, "metrics | where metric_name == \"app_recommendations_counter\" | summarize count(*) by bin_auto(timestamp),attributes")
            , ("rows", rows, "resource.service.name == \"cart\" | where duration != null | summarize percentiles(duration, 50, 90) by bin(timestamp, 1h)")
            , ("rows", rows, "| extend error_cat = case(attributes.http.response.status_code >= 500, \"5xx\", \"ok\")")
            , ("rows", rows, "| extend error_rate = round(countif(attributes.http.response.status_code >= 400), 3)")
            , ("rows", rows, "| project total_duration = toint(sum(duration))")
            , ("rows", rows, "| project service = resource.service.name")
            , ("rows", rows, "coalesce(status_code, 0) >= 400")
            , ("rows", rows, "isnull(status_message)")
            , ("rows", rows, "isnotnull(trace_id)")
            , ("rows", rows, "toint(status_code) >= 400")
            , ("rows", rows, "toint(coalesce(status_code, 0)) >= 400")
            , ("rows", rows, "name == \"O'Brien\"")
            ]
      goldenText "kql-to-sql.txt" $ T.intercalate "\n" [kql <> "\n" <> label <> ": " <> normT (render kql) <> "\n" | (label, render, kql) <- cases]

    it "newer pagination scans forward from its cursor and returns the adjacent page" do
      let cfg = (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing){cursorM = Just (PageCursor PageNewer fixedUTCTime), dateRange = (Nothing, Just $ addUTCTime 60 fixedUTCTime)}
          (query, _) = fromRight' $ parseQueryToComponents cfg "level == \"ERROR\""
      query `shouldSatisfy` T.isInfixOf "timestamp BETWEEN"
      query `shouldSatisfy` T.isInfixOf "ORDER BY timestamp asc"
      query `shouldNotSatisfy` T.isInfixOf "ORDER BY timestamp desc"

    it "scalar summarize keeps its aggregate numeric (no ::text cast)" do
      -- Regression: the grouped shape casts aggregates to text for the DTText
      -- decoder, but a `by`-less summarize is read as a float. Casting it broke
      -- `metrics query 'summarize count()' --assert` and the TF metric read.
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "summarize count(*)"
      fromMaybe "" c.finalSummarizeQuery `shouldNotSatisfy` T.isInfixOf "::text"

    it "table summaries return aggregate columns rather than default event fields" do
      let parse q = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) q
          (scalarSql, scalar) = parse "metrics | summarize peak=max(value)"
          (groupedSql, grouped) = parse "metrics | summarize peak=max(value) by metric_name"
      scalarSql `shouldSatisfy` T.isInfixOf "max((value)::float)"
      scalarSql `shouldNotSatisfy` T.isInfixOf "context___trace_id"
      scalarSql `shouldNotSatisfy` T.isInfixOf "ORDER BY timestamp"
      scalar.toColNames `shouldBe` ["peak"]
      groupedSql `shouldSatisfy` T.isInfixOf "jsonb_build_array(metric_name,max((value)::float)"
      grouped.toColNames `shouldBe` ["metric_name", "peak"]

    it "grouped summarize casts to text and projects the group column" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "summarize count(*) by name"
          sql = fromMaybe "" c.finalSummarizeQuery
      sql `shouldSatisfy` T.isInfixOf "COALESCE((name)::text, 'null')"
      sql `shouldSatisfy` T.isInfixOf "::text"

    it "keeps non-time groups in binned table results" do
      let (query, components) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize count() by bin_auto(timestamp), level"
      query `shouldSatisfy` T.isInfixOf "jsonb_build_array(extract(epoch from time_bucket('2 hours', timestamp))::integer, level,count(*)::float"
      query `shouldSatisfy` T.isInfixOf "GROUP BY time_bucket('2 hours', timestamp), level"
      components.toColNames `shouldBe` ["timestamp", "level", "count_"]

    -- The ladder is bin_auto's business alone: an explicit bin() is the user's own
    -- choice of resolution and must survive any time range, in both directions.
    it "an explicit bin() is never rewritten by the auto ladder" do
      let atRange secs = (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing){dateRange = (Just (addUTCTime (negate secs) fixedUTCTime), Just fixedUTCTime)}
          binOf secs q = normT $ fromMaybe "" $ snd (fromRight' $ parseQueryToComponents (atRange secs) q) & (.finalSummarizeQuery)
          fiveMins = 300
          thirtyDays = 2592000
      -- over five minutes the ladder picks seconds; the pinned hour does not budge
      binOf fiveMins "| summarize count(*) by bin_auto(timestamp)" `shouldSatisfy` T.isInfixOf "time_bucket('2 seconds'"
      binOf fiveMins "| summarize count(*) by bin(timestamp, 1h)" `shouldSatisfy` T.isInfixOf "time_bucket('1 hours'"
      -- over thirty days it coarsens to hours; the pinned hour still does not budge
      binOf thirtyDays "| summarize count(*) by bin_auto(timestamp)" `shouldSatisfy` T.isInfixOf "time_bucket('6 hours'"
      binOf thirtyDays "| summarize count(*) by bin(timestamp, 1h)" `shouldSatisfy` T.isInfixOf "time_bucket('1 hours'"

    -- An alert reads its whole configured window, not a bucket of it. This used to
    -- bucket by bin_auto's width (the 14-day fallback, since a monitor has no viewer
    -- time range) and then take the newest bucket — an epoch-aligned fraction of the
    -- window, so the same monitor read a different amount of data depending on the
    -- wall clock.
    it "an alert query reads its lookback window, not a time bucket of it" do
      let cfg = (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing){alertLookbackMins = 60}
          alertSql q = normT $ fromMaybe "" $ snd (fromRight' $ parseQueryToComponents cfg q) & (.finalAlertQuery)
      alertSql "| summarize count(*) by bin_auto(timestamp)" `shouldSatisfy` T.isInfixOf "timestamp >= NOW() - INTERVAL '60 minutes'"
      alertSql "| summarize count(*) by bin_auto(timestamp)" `shouldNotSatisfy` T.isInfixOf "time_bucket"
      -- a non-time grouping survives; the largest group is the one evaluated
      alertSql "| summarize count(*) by bin_auto(timestamp), kind" `shouldSatisfy` T.isInfixOf "GROUP BY kind ORDER BY GREATEST(count(*)::float)::float8 DESC LIMIT 1"

    it "alert queries evaluate requested aggregates instead of counting samples" do
      let cfg = defSqlQueryCfg defPid fixedUTCTime Nothing Nothing
          alertSql q = T.toLower $ normT $ fromMaybe "" $ snd (fromRight' $ parseQueryToComponents cfg q) & (.finalAlertQuery)
      forM_ ["max(value)", "peak=max(value)", "max(value) by resource.service.name"] \aggregation -> do
        let sql = alertSql $ "metrics | summarize " <> aggregation
        sql `shouldSatisfy` T.isInfixOf "max("
        sql `shouldNotSatisfy` T.isInfixOf "count(*)"
        sql `shouldNotSatisfy` T.isInfixOf " as "
      let havingSql = snd $ T.breakOn "having" $ alertSql "metrics | summarize peak=max(value) | where peak > 80"
      havingSql `shouldSatisfy` T.isInfixOf "max((value)::float)"
      havingSql `shouldSatisfy` T.isInfixOf "> 80"
      havingSql `shouldNotSatisfy` T.isInfixOf "peak"

    it "alert queries project post-summarize extend ratios without raw helper aggregates" do
      let cfg = defSqlQueryCfg defPid fixedUTCTime (Just SMetrics) Nothing
          alertSql q = T.toLower $ normT $ fromMaybe "" $ snd (fromRight' $ parseQueryToComponents cfg q) & (.finalAlertQuery)
          sql = alertSql "metrics | summarize hits = count(), total = sum(value) by bin_auto(timestamp) | extend hit_pct = 100.0 * hits / total"
      -- Must evaluate only hit_pct inside GREATEST(...), not GREATEST(hit_pct, hits, total)
      sql `shouldSatisfy` T.isInfixOf "greatest((((100.0 * count(*)::float) / nullif(sum((value)::float), 0)))::float)::float8"

    it "alert queries evaluate the extreme quantile for percentiles in the alert direction" do
      let aboveCfg = defSqlQueryCfg defPid fixedUTCTime Nothing Nothing
          belowCfg = aboveCfg{alertBelow = True}
          alertSql c kql = T.toLower $ normT $ fromMaybe "" $ snd (fromRight' $ parseQueryToComponents c kql) & (.finalAlertQuery)
          q = "| summarize percentiles(duration, 50, 75, 90, 95, 99) by bin_auto(timestamp)"
      alertSql aboveCfg q `shouldSatisfy` T.isInfixOf "approx_percentile(0.99"
      alertSql belowCfg q `shouldSatisfy` T.isInfixOf "approx_percentile(0.5"

    it "PostgreSQL metrics source uses native JSONB dimensions" do
      let cfg = defSqlQueryCfg defPid fixedUTCTime (Just SMetrics) Nothing
          components query = fromRight' $ parseQueryToComponents cfg ("metrics | " <> query)
      let (serviceQuery, _) = components "where service == \"accounting\" and metric_name == \"k8s.container.cpu_request\""
      serviceQuery `shouldSatisfy` T.isInfixOf "resource___service___name = 'accounting'"
      let (attributeQuery, _) = components "where attributes.system.device == \"disk0\""
      attributeQuery `shouldSatisfy` T.isInfixOf "attributes->'system'->>'device' = 'disk0'"
      let (resourceQuery, _) = components "where resource.kind.name == \"node-1\""
      resourceQuery `shouldSatisfy` T.isInfixOf "resource->'kind'->>'name' = 'node-1'"
      -- Grouping dimensions live on the chart query (finalSummarizeQuery), not the
      -- flat data query; assert the flattened resource path reaches its GROUP BY.
      let (_, groupComps) = components "summarize avg(value) by bin_auto(timestamp), resource.kind.name"
      fromMaybe "" groupComps.finalSummarizeQuery `shouldSatisfy` T.isInfixOf "GROUP BY time_bucket('2 hours', timestamp), COALESCE((resource->'kind'->>'name')::text, 'null')"

    it "TimeFusion metrics source converts variant dimensions to JSON" do
      let cfg = (defSqlQueryCfg defPid fixedUTCTime (Just SMetrics) Nothing){targetsTimefusion = True}
          components query = fromRight' $ parseQueryToComponents cfg ("metrics | " <> query)
      let (attributeQuery, _) = components "where attributes.system.device == \"disk0\""
      attributeQuery `shouldSatisfy` T.isInfixOf "variant_to_json(attributes)->'system'->>'device' = 'disk0'"
      let (resourceQuery, _) = components "where resource.kind.name == \"node-1\""
      resourceQuery `shouldSatisfy` T.isInfixOf "variant_to_json(resource)->'kind'->>'name' = 'node-1'"
      -- Grouping dimensions live on the chart query (finalSummarizeQuery), not the
      -- flat data query; assert the flattened resource path reaches its GROUP BY.
      let (_, groupComps) = components "summarize avg(value) by bin_auto(timestamp), resource.kind.name"
      fromMaybe "" groupComps.finalSummarizeQuery `shouldSatisfy` T.isInfixOf "GROUP BY time_bucket('2 hours', timestamp), COALESCE((variant_to_json(resource)->'kind'->>'name')::text, 'null')"

    it "spans source leaves service filter as a flat column" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| where service == \"accounting\""
      query `shouldSatisfy` T.isInfixOf "resource___service___name = 'accounting'"

  describe "percentile lowering" do
    it "lowers p95 to a portable aggregate and coalesces empty buckets" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize p95(duration) by bin(timestamp, 1h)"
      query `shouldSatisfy` T.isInfixOf "COALESCE(approx_percentile(0.95, percentile_agg(CAST(duration AS DOUBLE PRECISION))), 0)::float"

    it "extracts percentilesInfo from AST" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize percentiles(duration, 50, 75, 90, 95) by bin(timestamp, 1h)"
      case c.percentilesInfo of
        Just (fieldExpr, pcts) -> do
          fieldExpr `shouldBe` "duration"
          pcts `shouldBe` [50.0, 75.0, 90.0, 95.0]
        Nothing -> fail "Expected percentilesInfo to be extracted"

    it "extracts percentilesInfo with division from AST" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize percentile(duration / 1e6, 95) by bin(timestamp, 1h)"
      case c.percentilesInfo of
        Just (fieldExpr, pcts) -> do
          fieldExpr `shouldBe` "(duration / 1000000.0)"
          pcts `shouldBe` [95.0]
        Nothing -> fail "Expected percentilesInfo to be extracted"

    it "generates one digest scan for percentiles with bin" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize percentiles(duration, 50, 90) by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      let expected =
            [text|
WITH bucket_digests AS (SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer AS timeB, percentile_agg(CAST(duration AS DOUBLE PRECISION)) AS digest FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY timeB HAVING COUNT(*) > 0) SELECT b.timeB, q.quantile, COALESCE(approx_percentile(q.percentile, b.digest), 0)::float AS value FROM bucket_digests b CROSS JOIN (VALUES (0.5, 'p50'), (0.9, 'p90')) AS q(percentile, quantile) ORDER BY b.timeB DESC, q.quantile
            |]
      normT sql `shouldBe` normT expected

  describe "summarize SQL lowering" do
    it "generates COUNT FILTER SQL for countif" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize countif(status_code == \"ERROR\") by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, 'value', COUNT(*) FILTER (WHERE status_code = 'ERROR')::float AS countif_ FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC
            |]
      normT sql `shouldBe` normT expected

    it "lowers dcount to a sketch, the spelling both backends share" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize dcount(attributes.user.id) by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, 'value', distinct_count(approx_count_distinct(attributes___user___id))::float AS dcount_attributes_user_id FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC
            |]
      normT sql `shouldBe` normT expected

    it "lowers a dotted-path dcount to a sketch too" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize dcount(resource.service.name) by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, 'value', distinct_count(approx_count_distinct(resource___service___name))::float AS dcount_resource_service_name FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC
            |]
      normT sql `shouldBe` normT expected

    it "generates COALESCE SQL" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize coalesce(name, \"unknown\") by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, 'value', COALESCE(name, 'unknown') AS coalesce_ FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC
            |]
      normT sql `shouldBe` normT expected

    it "generates CONCAT SQL for strcat" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize strcat(name, \" \", url_path) by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, 'value', CONCAT(name, ' ', attributes___url___path) AS strcat_ FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC
            |]
      normT sql `shouldBe` normT expected

    it "generates CASE WHEN SQL for iff" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize iff(status_code == \"ERROR\", \"error\", \"ok\") by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, 'value', CASE WHEN status_code = 'ERROR' THEN 'error' ELSE 'ok' END AS iff_ FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC
            |]
      normT sql `shouldBe` normT expected

    it "generates multi-branch CASE SQL" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize case(attributes.http.response.status_code >= 500, \"5xx\", attributes.http.response.status_code >= 400, \"4xx\", \"ok\") by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, 'value', CASE WHEN attributes___http___response___status_code >= 500 THEN '5xx' WHEN attributes___http___response___status_code >= 400 THEN '4xx' ELSE 'ok' END AS case_ FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC
            |]
      normT sql `shouldBe` normT expected

  describe "endpoint analytics query forms" do
    it "keeps a named average available to order by" do
      let (sql, query) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "name != null and duration != null | summarize avg_latency=avg(duration) by name | order by avg_latency desc"
      query.toColNames `shouldBe` ["name", "avg_latency"]
      sql `shouldSatisfy` T.isInfixOf "ORDER BY avg((duration)::float) desc"
      sql `shouldNotSatisfy` T.isInfixOf "ORDER BY avg_"

    it "compiles browser error predicates and outcome grouping" do
      let browserContext = "hashes[*]==\"807c56e1\" AND (resource.telemetry.sdk.language in (\"webjs\", \"javascript\", \"js\") or resource.user_agent.original != \"\" or name in (\"documentLoad\", \"documentFetch\") or name startswith \"Pageview \")"
          errorPredicate = "status_code == \"ERROR\" or level in (\"ERROR\", \"FATAL\", \"error\", \"fatal\") or attributes.exception.type != null"
          parse q = fst $ fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) q
          errorSql = parse $ browserContext <> " AND (" <> errorPredicate <> ") | summarize count() by bin_auto(timestamp)"
          outcomeSql = parse $ browserContext <> " | summarize requests=count(), errors=countif(" <> errorPredicate <> ") by bin_auto(timestamp)"
          sessionSql = parse $ browserContext <> " AND attributes.session.id != null | summarize dcount(attributes.session.id) by bin_auto(timestamp)"
          p75Sql = parse $ browserContext <> " AND duration != null | summarize p75(duration) / 1000000 by bin_auto(timestamp)"
      errorSql `shouldSatisfy` T.isInfixOf "status_code = 'ERROR'"
      outcomeSql `shouldSatisfy` T.isInfixOf "FILTER (WHERE"
      sessionSql `shouldSatisfy` T.isInfixOf "distinct_count(approx_count_distinct(attributes___session___id))"
      p75Sql `shouldSatisfy` T.isInfixOf "approx_percentile(0.75"

  describe "validation errors" do
    it "rejects dcount with accuracy > 4" do
      parseQueryToAST "| summarize dcount(attributes.user.id, 5) by bin_auto(timestamp)" `shouldFailWith` "Syntax error at column 42: dcount accuracy must be 0-4 per KQL spec"

    it "rejects coalesce with single argument" do
      parseQueryToAST "| summarize coalesce(x) by bin_auto(timestamp)" `shouldFailWith` "Syntax error at column 24: coalesce requires at least 2 arguments"

    it "rejects round with non-numeric decimals" do
      parseQueryToAST "| summarize round(duration, \"two\") by bin_auto(timestamp)" `shouldFailWith` "Syntax error at column 34: round() decimals must be numeric"

    it "rejects project with no columns" do
      parseQueryToAST "| project" `shouldFailWith` "Syntax error at column 3: unexpected 'p', expecting end of input or white space"

    it "rejects decimals > 15" do
      parseQueryToAST "| summarize round(duration, 16) by bin_auto(timestamp)" `shouldFailWith` "Syntax error at column 31: round() decimals must be 0-15 per PostgreSQL limit"

    it "rejects negative decimals" do
      parseQueryToAST "| summarize round(duration, -1) by bin_auto(timestamp)" `shouldFailWith` "Syntax error at column 31: round() decimals must be 0-15 per PostgreSQL limit"

  describe "round and type casting SQL" do
    it "generates ROUND SQL" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize round(duration, 2) by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      -- round produces ROUND(value::numeric, decimals)::float
      normT sql `shouldBe` normT "SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, 'value', ROUND((duration)::numeric, 2)::float AS round_ FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC"

    it "generates proper SQL for tofloat" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize tofloat(duration) by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      -- tofloat produces ::float which is correct for numeric aggregation
      normT sql `shouldBe` normT "SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, 'value', (duration)::float AS tofloat_ FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC"

    it "generates proper SQL for toint" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize toint(duration) by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      -- toint produces ::integer
      normT sql `shouldBe` normT "SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, 'value', (duration)::integer AS toint_ FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC"

    it "generates proper SQL for tostring" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize tostring(status_code) by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      -- tostring produces ::text (not ::float, as text cannot be cast to float)
      normT sql `shouldBe` normT "SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, 'value', (status_code)::text AS tostring_ FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC"

  describe "nested aggregations" do
    it "generates SQL for round with nested countif" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize round(countif(attributes.http.response.status_code >= 400), 2) by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, 'value', ROUND((COUNT(*) FILTER (WHERE attributes___http___response___status_code >= 400)::float)::numeric, 2)::float AS round_ FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC
            |]
      normT sql `shouldBe` normT expected

    it "generates SQL for tofloat with nested sum" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize tofloat(sum(duration)) by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, 'value', (sum((duration)::float))::float AS tofloat_ FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC
            |]
      normT sql `shouldBe` normT expected

    it "generates SQL for toint with nested count" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize toint(count()) by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, 'value', (count(*)::float)::integer AS toint_ FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC
            |]
      normT sql `shouldBe` normT expected

    it "generates SQL for named nested aggregation" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize error_pct = round(countif(attributes.http.response.status_code >= 500), 2) by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      let expected =
            [text|
      SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, 'value', ROUND((COUNT(*) FILTER (WHERE attributes___http___response___status_code >= 500)::float)::numeric, 2)::float AS error_pct FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC
            |]
      normT sql `shouldBe` normT expected

  describe "scalar functions in filter expressions" do
    -- The list projects `errors` as a computed boolean, so `errors == true` is what a reader
    -- writes next. It used to reach the raw payload column and fail the whole query with
    -- "Cannot infer common argument type for comparison operation Utf8View = Boolean" on
    -- TimeFusion. The filter must be the same predicate the projection is.
    it "filters on the projected errors flag rather than the payload column" do
      -- Asserted on the WHERE clause rather than the whole statement: the projection contains
      -- the same predicate, and how deeply the filter happens to be parenthesised is
      -- incidental. 'errorFlagSql' is imported rather than retyped — its text is pinned by
      -- the doctest on 'Pkg.Parser.Expr.displayExprHelper'; what belongs here is the wiring.
      let whereOf q = snd $ T.breakOn "WHERE" $ fst $ fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) q
      whereOf "errors == true" `shouldSatisfy` T.isInfixOf errorFlagSql
      whereOf "errors == true" `shouldNotSatisfy` T.isInfixOf ("NOT " <> errorFlagSql)
      whereOf "errors == false" `shouldSatisfy` T.isInfixOf ("NOT " <> errorFlagSql)
      whereOf "errors != true" `shouldBe` whereOf "errors == false"
      -- Nothing that already worked moves: the column itself stays addressable.
      whereOf "errors != null" `shouldSatisfy` T.isInfixOf "errors IS NOT NULL"

  describe "output field aliases" do
    -- These three names appear in query results and the schema endpoint, so
    -- users naturally try to filter by them. The alias map rewrites them to
    -- the underlying DB columns so the WHERE clause hits a real column.
    it "rewrites span_name → name in WHERE" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "span_name==\"foo\""
      query `shouldSatisfy` T.isInfixOf "name = 'foo'"
      query `shouldSatisfy` not . T.isInfixOf "span_name = "

    it "rewrites service → resource___service___name in WHERE" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "service==\"api\""
      query `shouldSatisfy` T.isInfixOf "resource___service___name = 'api'"

    it "rewrites trace_id → context___trace_id in WHERE" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "trace_id==\"abc\""
      query `shouldSatisfy` T.isInfixOf "context___trace_id = 'abc'"

  describe "exception COALESCE rewriting" do
    -- OTel SDKs (notably hs-opentelemetry) record exceptions as span events
    -- (event_name=\"exception\") rather than flat attributes, so a query that
    -- only hits the flat column misses real exceptions. The rewrite COALESCEs
    -- both sources so either path returns a match.
    it "rewrites attributes.exception.message to COALESCE over events" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "attributes.exception.message==\"boom\""
      query `shouldSatisfy` T.isInfixOf "COALESCE(attributes___exception___message"
      query `shouldSatisfy` T.isInfixOf "jsonb_path_query_first(events"
      query `shouldSatisfy` T.isInfixOf "@.event_name == \"exception\""

    it "rewrites attributes.exception.type with COALESCE" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "attributes.exception.type==\"E\""
      query `shouldSatisfy` T.isInfixOf "COALESCE(attributes___exception___type"

    it "rewrites attributes.exception.stacktrace with COALESCE" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "attributes.exception.stacktrace==\"x\""
      query `shouldSatisfy` T.isInfixOf "COALESCE(attributes___exception___stacktrace"

    it "leaves other flattened OTel attributes as plain triple-underscore (no COALESCE)" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "attributes.http.request.method==\"GET\""
      query `shouldSatisfy` T.isInfixOf "attributes___http___request___method = 'GET'"
      query `shouldSatisfy` not . T.isInfixOf "COALESCE(attributes___http___request___method"
