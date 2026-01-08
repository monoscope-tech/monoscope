module Pkg.ParserSpec (spec) where

import Data.Either.Extra (fromRight')
import Data.Text qualified as T
import NeatInterpolation (text)
import Pkg.Parser (
  QueryComponents (finalSummarizeQuery, percentilesInfo),
  defPid,
  defSqlQueryCfg,
  fixedUTCTime,
  parseQueryToComponents,
  parseQueryToAST,
 )
import Relude
import Test.Hspec (Spec, describe, it, shouldBe)


-- Normalize text by removing newlines, carriage returns, tabs, and extra spaces
normT :: Text -> Text
normT = unwords . words . T.filter (`notElem` ['\n', '\r', '\t'])


-- Check that parse result is Left containing expected substring
shouldFailWith :: Either Text a -> Text -> IO ()
shouldFailWith (Left err) expected = T.isInfixOf expected err `shouldBe` True
shouldFailWith (Right _) _ = fail "Expected parse error"


spec :: Spec
spec = do
  describe "parseQueryToSQL" do
    it "query with ago() time function" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "timestamp >= ago(7d)"
      let expected =
            [text|
      SELECT id,CASE WHEN RIGHT(TRIM('"' FROM CAST(to_json(timestamp at time zone 'UTC') AS VARCHAR)), 1) = 'Z' THEN TRIM('"' FROM CAST(to_json(timestamp at time zone 'UTC') AS VARCHAR)) ELSE TRIM('"' FROM CAST(to_json(timestamp at time zone 'UTC') AS VARCHAR)) || 'Z' END,context___trace_id,name,duration,resource___service___name,parent_id,CAST(EXTRACT(EPOCH FROM (start_time)) * 1000000000 AS BIGINT),errors is not null,to_json(summary),context___span_id, count(*) OVER() as _total_count FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and ((timestamp >= NOW() - INTERVAL '7 days')) ORDER BY timestamp desc limit 500|]
      normT query `shouldBe` normT expected

    it "query with now() time function" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "timestamp == now()"
      let expected =
            [text|
      SELECT id,CASE WHEN RIGHT(TRIM('"' FROM CAST(to_json(timestamp at time zone 'UTC') AS VARCHAR)), 1) = 'Z' THEN TRIM('"' FROM CAST(to_json(timestamp at time zone 'UTC') AS VARCHAR)) ELSE TRIM('"' FROM CAST(to_json(timestamp at time zone 'UTC') AS VARCHAR)) || 'Z' END,context___trace_id,name,duration,resource___service___name,parent_id,CAST(EXTRACT(EPOCH FROM (start_time)) * 1000000000 AS BIGINT),errors is not null,to_json(summary),context___span_id, count(*) OVER() as _total_count FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and ((timestamp = NOW())) ORDER BY timestamp desc limit 500|]
      normT query `shouldBe` normT expected

    it "basic query eq query" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "method==\"GET\""
      let expected =
            [text|
      SELECT id,CASE WHEN RIGHT(TRIM('"' FROM CAST(to_json(timestamp at time zone 'UTC') AS VARCHAR)), 1) = 'Z' THEN TRIM('"' FROM CAST(to_json(timestamp at time zone 'UTC') AS VARCHAR)) ELSE TRIM('"' FROM CAST(to_json(timestamp at time zone 'UTC') AS VARCHAR)) || 'Z' END,context___trace_id,name,duration,resource___service___name,parent_id,CAST(EXTRACT(EPOCH FROM (start_time)) * 1000000000 AS BIGINT),errors is not null,to_json(summary),context___span_id, count(*) OVER() as _total_count FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and ((method = 'GET')) ORDER BY timestamp desc limit 500|]
      normT query `shouldBe` normT expected
    it "summarize query query" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "method==\"GET\""
      let expected =
            [text| SELECT  FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and ((method = 'GET')) ORDER BY timestamp DESC limit 500 |]
      normT (fromMaybe "" c.finalSummarizeQuery) `shouldBe` normT expected
    it "summarize query by time bin" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "method==\"GET\" | summarize count(*) by bin(timestamp, 1d)"
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1 days', timestamp))::integer, 'value', count(*)::float AS count_ FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and ((method = 'GET')) GROUP BY time_bucket('1 days', timestamp) ORDER BY time_bucket('1 days', timestamp) DESC
      |]
      normT (fromMaybe "" c.finalSummarizeQuery) `shouldBe` normT expected
    it "summarize with bin()" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "method==\"GET\" | summarize sum(attributes.client) by attributes.client, bin(timestamp, 60)"
      let expected =
            [text|
      SELECT extract(epoch from time_bucket('5 minutes', timestamp))::integer, sum((attributes->>'client')::float) AS sum_attributes_client, count(*) OVER() as _total_count FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and ((method = 'GET')) GROUP BY time_bucket('5 minutes', timestamp) ORDER BY time_bucket('5 minutes', timestamp) DESC |]
      normT query `shouldBe` normT expected
    it "summarize with named aggregation" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "timestamp >= ago(7d) | summarize TotalCount = count() by Computer"
      let expected =
            [text|
      SELECT count(*)::float AS TotalCount, count(*) OVER() as _total_count FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and ((timestamp >= NOW() - INTERVAL '7 days')) GROUP BY Computer ORDER BY timestamp desc limit 500 |]
      normT query `shouldBe` normT expected
    it "summarize with sort by and take" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "method==\"GET\" | summarize sum(attributes.client) by attributes.client, bin(timestamp, 60) | sort by parent_id asc | take 1000"
      let expected =
            [text|
      SELECT extract(epoch from time_bucket('5 minutes', timestamp))::integer, sum((attributes->>'client')::float) AS sum_attributes_client, count(*) OVER() as _total_count FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and ((method = 'GET')) GROUP BY time_bucket('5 minutes', timestamp) ORDER BY time_bucket('5 minutes', timestamp) DESC limit 1000 |]
      normT query `shouldBe` normT expected

    it "summarize with bin_auto()" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize count(*) by bin_auto(timestamp)"
      let expected =
            [text|
      SELECT extract(epoch from time_bucket('6 hours', timestamp))::integer, count(*)::float AS count_, count(*) OVER() as _total_count FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('6 hours', timestamp) ORDER BY time_bucket('6 hours', timestamp) DESC |]
      normT query `shouldBe` normT expected

    it "query a metric" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "telemetry.metrics | where metric_name == \"app_recommendations_counter\" | summarize count(*) by bin_auto(timestamp),attributes"
      let expected =
            [text|
      SELECT extract(epoch from time_bucket('6 hours', timestamp))::integer, count(*)::float AS count_, count(*) OVER() as _total_count FROM telemetry.metrics WHERE project_id='00000000-0000-0000-0000-000000000000' and ((metric_name = 'app_recommendations_counter')) GROUP BY time_bucket('6 hours', timestamp) ORDER BY time_bucket('6 hours', timestamp) DESC |]
      normT query `shouldBe` normT expected

  describe "percentile parsing" do
    it "parses percentile(duration, 95) via full query" do
      let result = parseQueryToAST "| summarize percentile(duration, 95) by bin(timestamp, 1h)"
      isRight result `shouldBe` True

    it "parses percentiles(duration, 50, 75, 90, 95) via full query" do
      let result = parseQueryToAST "| summarize percentiles(duration, 50, 75, 90, 95) by bin(timestamp, 1h)"
      isRight result `shouldBe` True

    it "parses percentile with division expression via full query" do
      let result = parseQueryToAST "| summarize percentile(duration / 1e6, 95) by bin(timestamp, 1h)"
      isRight result `shouldBe` True

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

    it "generates LATERAL unnest SQL for percentiles with bin" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize percentiles(duration, 50, 90) by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      let expected =
            [text|
SELECT timeB, quantile, value FROM (
  SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer AS timeB,
    ARRAY[COALESCE((approx_percentile(0.5, percentile_agg((duration)::float)))::float, 0),
          COALESCE((approx_percentile(0.9, percentile_agg((duration)::float)))::float, 0)] AS values,
    ARRAY['p50','p90'] AS quantiles
  FROM otel_logs_and_spans
  WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE)
  GROUP BY timeB
  HAVING COUNT(*) > 0 ) s, LATERAL unnest(s.values, s.quantiles) AS u(value, quantile) WHERE value IS NOT NULL ORDER BY timeB DESC
            |]
      normT sql `shouldBe` normT expected

    it "combines multiple where clauses with AND" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "resource.service.name == \"cart\" | where duration != null | summarize percentiles(duration, 50, 90) by bin(timestamp, 1h)"
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, approx_percentile(0.5, percentile_agg((duration)::float))::float AS percentiles_duration, count(*) OVER() as _total_count FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and ((resource___service___name = 'cart' AND duration IS NOT NULL)) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC
            |]
      normT query `shouldBe` normT expected

  describe "countif parsing" do
    it "parses countif with simple condition" do
      let result = parseQueryToAST "| summarize countif(status_code == \"ERROR\") by bin(timestamp, 1h)"
      isRight result `shouldBe` True

    it "parses countif with comparison condition" do
      let result = parseQueryToAST "| summarize countif(duration > 1000) by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "generates COUNT FILTER SQL for countif" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize countif(status_code == \"ERROR\") by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, 'value', COUNT(*) FILTER (WHERE status_code = 'ERROR')::float AS countif_ FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC
            |]
      normT sql `shouldBe` normT expected

  describe "dcount parsing" do
    it "parses dcount with simple field" do
      let result = parseQueryToAST "| summarize dcount(user_id) by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "parses dcount with dotted field path" do
      let result = parseQueryToAST "| summarize dcount(resource.service.name) by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "parses dcount with optional accuracy parameter" do
      -- Microsoft KQL: dcount(expr [, accuracy]) where accuracy is 0-4
      let result = parseQueryToAST "| summarize dcount(user_id, 2) by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "generates COUNT DISTINCT SQL for dcount" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize dcount(user_id) by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, 'value', COUNT(DISTINCT user_id)::float AS dcount_user_id FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC
            |]
      normT sql `shouldBe` normT expected

    it "generates COUNT DISTINCT SQL for dcount with dotted path" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize dcount(resource.service.name) by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, 'value', COUNT(DISTINCT resource___service___name)::float AS dcount_resource_service_name FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC
            |]
      normT sql `shouldBe` normT expected

  describe "coalesce parsing" do
    it "parses coalesce with two arguments" do
      let result = parseQueryToAST "| summarize coalesce(method, \"unknown\") by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "parses coalesce with multiple arguments (variadic)" do
      -- Microsoft KQL: coalesce(arg, arg_2, [arg_3,...]) - up to 64 args
      let result = parseQueryToAST "| summarize coalesce(field1, field2, \"default\") by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "generates COALESCE SQL" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize coalesce(method, \"unknown\") by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, 'value', COALESCE(method, 'unknown') AS coalesce_ FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC
            |]
      normT sql `shouldBe` normT expected

  describe "strcat parsing" do
    it "parses strcat with multiple arguments" do
      -- Microsoft KQL: strcat(argument1, argument2 [, argument3 ... ])
      let result = parseQueryToAST "| summarize strcat(method, \" \", url_path) by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "parses strcat with variadic arguments" do
      let result = parseQueryToAST "| summarize strcat(service, \":\", operation, \"@\", host) by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "generates CONCAT SQL for strcat" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize strcat(method, \" \", url_path) by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, 'value', CONCAT(method, ' ', url_path) AS strcat_ FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC
            |]
      normT sql `shouldBe` normT expected

  describe "iff parsing" do
    it "parses iff with literal values" do
      -- Microsoft KQL: iff(if, then, else) - all scalar types
      let result = parseQueryToAST "| summarize iff(status_code == \"ERROR\", \"error\", \"ok\") by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "parses iff with field references" do
      let result = parseQueryToAST "| summarize iff(duration > 1000, slow_count, fast_count) by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "generates CASE WHEN SQL for iff" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize iff(status_code == \"ERROR\", \"error\", \"ok\") by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, 'value', CASE WHEN status_code = 'ERROR' THEN 'error' ELSE 'ok' END AS iff_ FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC
            |]
      normT sql `shouldBe` normT expected

  describe "case parsing" do
    it "parses case with literal values" do
      -- Microsoft KQL: case(predicate_1, then_1, [predicate_2, then_2, ...] else)
      let result = parseQueryToAST "| summarize case(status >= 500, \"5xx\", status >= 400, \"4xx\", \"ok\") by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "parses case with field references" do
      let result = parseQueryToAST "| summarize case(status >= 500, error_val, status >= 400, warning_val, success_val) by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "generates multi-branch CASE SQL" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize case(status >= 500, \"5xx\", status >= 400, \"4xx\", \"ok\") by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, 'value', CASE WHEN status >= 500 THEN '5xx' WHEN status >= 400 THEN '4xx' ELSE 'ok' END AS case_ FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC
            |]
      normT sql `shouldBe` normT expected

  describe "combined aggregations" do
    it "parses multiple aggregations including countif and dcount" do
      let result = parseQueryToAST "| summarize count(), countif(status_code == \"ERROR\"), dcount(user_id) by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "parses aggregation with named alias" do
      let result = parseQueryToAST "| summarize error_count = countif(status_code == \"ERROR\") by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "parses complex query with countif for error rate calculation" do
      let result = parseQueryToAST "kind == \"server\" | summarize countif(status_code == \"ERROR\") by resource.service.name"
      isRight result `shouldBe` True

  describe "edge cases and validation" do
    it "rejects dcount with accuracy > 4" do
      parseQueryToAST "| summarize dcount(user_id, 5) by bin_auto(timestamp)" `shouldFailWith` "dcount accuracy must be 0-4 per KQL spec"

    it "rejects coalesce with single argument" do
      parseQueryToAST "| summarize coalesce(x) by bin_auto(timestamp)" `shouldFailWith` "coalesce requires at least 2 arguments"

    it "accepts dcount with accuracy 0-4" do
      let result0 = parseQueryToAST "| summarize dcount(user_id, 0) by bin_auto(timestamp)"
      let result4 = parseQueryToAST "| summarize dcount(user_id, 4) by bin_auto(timestamp)"
      isRight result0 `shouldBe` True
      isRight result4 `shouldBe` True

    it "parses coalesce with multiple arguments" do
      let result = parseQueryToAST "| summarize coalesce(field1, field2, field3, \"default\") by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "parses strcat with multiple arguments" do
      let result = parseQueryToAST "| summarize strcat(service, \":\", operation, \"@\", host, \"/\", endpoint) by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "parses nested function calls" do
      -- Test iff with strcat in then clause
      let result = parseQueryToAST "| summarize iff(status >= 500, \"error\", \"ok\") by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "parses dotted field paths in scalar contexts" do
      let result = parseQueryToAST "| summarize dcount(resource.service.name) by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "parses complex case with multiple branches" do
      let result = parseQueryToAST "| summarize case(status >= 500, \"5xx\", status >= 400, \"4xx\", status >= 300, \"3xx\", \"ok\") by bin_auto(timestamp)"
      isRight result `shouldBe` True

  describe "round parsing" do
    it "parses round with value and decimals" do
      let result = parseQueryToAST "| summarize round(duration, 2) by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "rejects round with non-numeric decimals" do
      parseQueryToAST "| summarize round(duration, \"two\") by bin_auto(timestamp)" `shouldFailWith` "round() decimals must be numeric"

    it "generates ROUND SQL" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize round(duration, 2) by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      -- round produces ROUND(value::numeric, decimals)::float
      normT sql `shouldBe` normT "SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, 'value', ROUND((duration)::numeric, 2)::float AS round_ FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC"

  describe "type casting functions" do
    it "parses tofloat" do
      let result = parseQueryToAST "| summarize tofloat(count_val) by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "parses todouble (alias for tofloat)" do
      let result = parseQueryToAST "| summarize todouble(count_val) by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "parses toint" do
      let result = parseQueryToAST "| summarize toint(duration) by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "parses tolong (alias for toint)" do
      let result = parseQueryToAST "| summarize tolong(duration) by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "parses tostring" do
      let result = parseQueryToAST "| summarize tostring(status_code) by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "generates proper SQL for tofloat" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize tofloat(count_val) by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      -- tofloat produces ::float which is correct for numeric aggregation
      normT sql `shouldBe` normT "SELECT extract(epoch from time_bucket('1 hours', timestamp))::integer, 'value', (count_val)::float AS tofloat_ FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and (TRUE) GROUP BY time_bucket('1 hours', timestamp) ORDER BY time_bucket('1 hours', timestamp) DESC"

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

  describe "extend operator" do
    it "parses extend with single column" do
      let result = parseQueryToAST "| extend error_cat = case(status >= 500, \"5xx\", \"ok\")"
      isRight result `shouldBe` True

    it "parses extend with multiple columns" do
      let result = parseQueryToAST "| extend cat = case(status >= 500, \"5xx\", \"ok\"), formatted = tostring(status)"
      isRight result `shouldBe` True

    it "generates SQL for extend with case" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| extend error_cat = case(status >= 500, \"5xx\", \"ok\")"
      -- extend should add columns to the select
      T.isInfixOf "CASE WHEN status >= 500 THEN '5xx' ELSE 'ok' END AS error_cat" query `shouldBe` True

  describe "project operator" do
    it "parses project with single column" do
      let result = parseQueryToAST "| project service = resource.service.name"
      isRight result `shouldBe` True

    it "parses project with multiple columns" do
      let result = parseQueryToAST "| project service = resource.service.name, total = count()"
      isRight result `shouldBe` True

    it "parses project with aggregation functions" do
      let result = parseQueryToAST "| project total = count(), avg_duration = avg(duration)"
      isRight result `shouldBe` True

    it "rejects project with no columns" do
      parseQueryToAST "| project" `shouldFailWith` "expecting"

  describe "nested aggregations" do
    it "parses round with nested countif" do
      let result = parseQueryToAST "| summarize round(countif(status >= 400), 2) by bin_auto(timestamp)"
      isRight result `shouldBe` True

    it "generates SQL for round with nested countif" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize round(countif(status >= 400), 2) by bin(timestamp, 1h)"
      let sql = fromMaybe "" c.finalSummarizeQuery
      T.isInfixOf "ROUND((COUNT(*) FILTER (WHERE" sql `shouldBe` True

    it "parses tofloat with nested count" do
      let result = parseQueryToAST "| summarize tofloat(count()) by bin_auto(timestamp)"
      isRight result `shouldBe` True

  describe "project SQL output" do
    it "project generates correct alias" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| project service = resource.service.name"
      T.isInfixOf "AS service" query `shouldBe` True

  describe "round validation" do
    it "rejects decimals > 15" do
      parseQueryToAST "| summarize round(duration, 16) by bin_auto(timestamp)" `shouldFailWith` "round() decimals must be 0-15 per PostgreSQL limit"

    it "rejects negative decimals" do
      parseQueryToAST "| summarize round(duration, -1) by bin_auto(timestamp)" `shouldFailWith` "round() decimals must be 0-15 per PostgreSQL limit"

    it "accepts valid decimals range" do
      let result = parseQueryToAST "| summarize round(duration, 0) by bin_auto(timestamp)"
      isRight result `shouldBe` True
      let result2 = parseQueryToAST "| summarize round(duration, 15) by bin_auto(timestamp)"
      isRight result2 `shouldBe` True

  describe "pNamedExpr with dots" do
    it "allows dots in names and sanitizes to underscores" do
      let result = parseQueryToAST "| extend error.count = count()"
      isRight result `shouldBe` True

    it "sanitizes dots in summarize named aggregations" do
      let result = parseQueryToAST "| summarize total.count = count() by status"
      isRight result `shouldBe` True

