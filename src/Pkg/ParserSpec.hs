module Pkg.ParserSpec (spec) where

import Data.Either.Extra (fromRight')
import Data.Text qualified as T
import NeatInterpolation (text)
import Pkg.Parser (
  QueryComponents (finalTimechartQuery),
  defPid,
  defSqlQueryCfg,
  fixedUTCTime,
  parseQueryToComponents,
 )
import Relude
import Test.Hspec (Spec, describe, it, shouldBe)


-- Normalize text by removing newlines, carriage returns, tabs, and extra spaces
normT :: Text -> Text
normT = unwords . words . T.filter (`notElem` ['\n', '\r', '\t'])


spec :: Spec
spec = do
  describe "parseQueryToSQL" do
    it "basic query eq query" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "method==\"GET\""
      let expected =
            [text|
      SELECT json_build_array(id,to_char(timestamp AT TIME ZONE 'UTC', 'YYYY-MM-DD"T"HH24:MI:SS.US"Z"'),context___trace_id,kind,status_message,name,duration,body,level,resource___service___name,parent_id,CAST(EXTRACT(EPOCH FROM (start_time)) * 1_000_000_000 AS BIGINT),EXISTS(SELECT 1 FROM jsonb_array_elements(events) elem WHERE elem->>'event_name' = 'exception'),jsonb_build_object( 'method', COALESCE(attributes->'http'->>'method', attributes___http___request___method), 'url', COALESCE(attributes->'http'->>'route', attributes->'url'->>'path', attributes->'http'->>'target', attributes->'http'->>'url'), 'status_code', COALESCE(attributes->'http'->>'status_code', attributes->'http'->'response'->>'status_code') ),jsonb_build_object('system', attributes->'db'->'system','statement', coalesce(attributes->'db'->'query'->'text', attributes->'db'->'statement')),json_build_object('system', attributes->'rpc'->'system', 'method', attributes->'rpc'->'method'),LEFT( CONCAT( COALESCE(attributes::text, '') ), 500 ),context___span_id) FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and ( timestamp > NOW() - interval '14 days' AND (method='GET') ) ORDER BY timestamp desc limit 150|]
      normT query `shouldBe` normT expected
    it "timechart query query" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "method==\"GET\""
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1h', timestamp))::integer as timeB, count(*)::integer as count, 'Throughput' FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and ( timestamp > NOW() - interval '14 days' AND (method='GET') ) GROUP BY timeB
      |]
      normT (fromMaybe "" c.finalTimechartQuery) `shouldBe` normT expected
    it "timechart query query 1d" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "method==\"GET\" | timechart count(*) [1d]"
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1d', timestamp))::integer as timeB, count(*)::integer as count, 'Throughput' FROM otel_logs_and_spans WHERE project_id='00000000-0000-0000-0000-000000000000' and ( timestamp > NOW() - interval '14 days' AND (method='GET') ) GROUP BY timeB
      |]
      normT (fromMaybe "" c.finalTimechartQuery) `shouldBe` normT expected
