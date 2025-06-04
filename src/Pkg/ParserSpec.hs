module Pkg.ParserSpec (spec) where

import Data.Either.Extra (fromRight')
import Data.Text qualified as T
import NeatInterpolation (text)
import Pkg.Parser (
  QueryComponents (finalSummarizeQuery),
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
    it "query with ago() time function" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "timestamp >= ago(7d)"
      let expected =
            [text|
      SELECT json_build_array(id::text,
      to_char(created_at AT TIME ZONE 'UTC', 'YYYY-MM-DD"T"HH24:MI:SS.US"Z"'),
      request_type,host,status_code,method,url_path,JSONB_ARRAY_LENGTH(errors),
      LEFT( CONCAT(
        'url=', COALESCE(raw_url, 'null'),
        ' response_body=', COALESCE(response_body, 'null'),
        ' request_body=', COALESCE(request_body, 'null')
      ), 255
      )) FROM apis.request_dumps
      WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid
      and ( created_at > NOW() - interval '14 days' AND (timestamp >= NOW() - INTERVAL '7 days') )
      ORDER BY created_at desc limit 200|]
      normT query `shouldBe` normT expected
      
    it "query with now() time function" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "timestamp == now()"
      let expected =
            [text|
      SELECT json_build_array(id::text,
      to_char(created_at AT TIME ZONE 'UTC', 'YYYY-MM-DD"T"HH24:MI:SS.US"Z"'),
      request_type,host,status_code,method,url_path,JSONB_ARRAY_LENGTH(errors),
      LEFT( CONCAT(
        'url=', COALESCE(raw_url, 'null'),
        ' response_body=', COALESCE(response_body, 'null'),
        ' request_body=', COALESCE(request_body, 'null')
      ), 255
      )) FROM apis.request_dumps
      WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid
      and ( created_at > NOW() - interval '14 days' AND (timestamp = NOW()) )
      ORDER BY created_at desc limit 200|]
      normT query `shouldBe` normT expected
    
    it "basic query eq query" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "method==\"GET\""
      let expected =
            [text|
      SELECT json_build_array(id::text,
      to_char(created_at AT TIME ZONE 'UTC', 'YYYY-MM-DD"T"HH24:MI:SS.US"Z"'),
      request_type,host,status_code,method,url_path,JSONB_ARRAY_LENGTH(errors),
      LEFT( CONCAT(
        'url=', COALESCE(raw_url, 'null'),
        ' response_body=', COALESCE(response_body, 'null'),
        ' request_body=', COALESCE(request_body, 'null')
      ), 255
      )) FROM apis.request_dumps
      WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid
      and ( created_at > NOW() - interval '14 days' AND (method='GET') )
      ORDER BY created_at desc limit 200|]
      normT query `shouldBe` normT expected
    it "summarize query query" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "method==\"GET\""
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1h', created_at))::integer as timeB, count(*)::integer as count, 'Throughput'
FROM apis.request_dumps WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid
and ( created_at > NOW() - interval '14 days' AND (method='GET') ) GROUP BY timeB
      |]
      normT (fromMaybe "" c.finalSummarizeQuery) `shouldBe` normT expected
    it "summarize query by time bin" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "method==\"GET\" | summarize count(*) by bin(timestamp, 1d)"
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1d', created_at))::integer as timeB, count(*)::integer as count, 'Throughput'
FROM apis.request_dumps WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid
and ( created_at > NOW() - interval '14 days' AND (method='GET') ) GROUP BY timeB
      |]
      normT (fromMaybe "" c.finalSummarizeQuery) `shouldBe` normT expected
    it "summarize with bin()" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "method==\"GET\" | summarize sum(attributes.client) by attributes.client, bin(timestamp, 60)"
      let expected =
            [text|
      SELECT json_build_array(id::text,
      to_char(created_at AT TIME ZONE 'UTC', 'YYYY-MM-DD"T"HH24:MI:SS.US"Z"'),
      request_type,host,status_code,method,url_path,JSONB_ARRAY_LENGTH(errors),
      LEFT( CONCAT(
        'url=', COALESCE(raw_url, 'null'),
        ' response_body=', COALESCE(response_body, 'null'),
        ' request_body=', COALESCE(request_body, 'null')
      ), 255
      ),sum(attributes.client)) FROM apis.request_dumps
      WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid
      and ( created_at > NOW() - interval '14 days' AND (method='GET') )
      GROUP BY COALESCE(attributes.client::text, '')::text,time_bucket('60 seconds', timestamp) ORDER BY created_at desc limit 200|]
      normT query `shouldBe` normT expected
    it "summarize with named aggregation" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "timestamp >= ago(7d) | summarize TotalCount = count() by Computer"
      let expected =
            [text|
      SELECT json_build_array(id::text,
      to_char(created_at AT TIME ZONE 'UTC', 'YYYY-MM-DD"T"HH24:MI:SS.US"Z"'),
      request_type,host,status_code,method,url_path,JSONB_ARRAY_LENGTH(errors),
      LEFT( CONCAT(
        'url=', COALESCE(raw_url, 'null'),
        ' response_body=', COALESCE(response_body, 'null'),
        ' request_body=', COALESCE(request_body, 'null')
      ), 255
      ),count(*)) FROM apis.request_dumps
      WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid
      and ( created_at > NOW() - interval '14 days' AND (timestamp >= ago(7d)) )
      GROUP BY COALESCE(Computer::text, '')::text ORDER BY created_at desc limit 200|]
      normT query `shouldBe` normT expected
    it "summarize with sort by and take" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "method==\"GET\" | summarize sum(attributes.client) by attributes.client, bin(timestamp, 60) | sort by parent_id asc | take 1000"
      let expected =
            [text|
      SELECT json_build_array(id::text,
      to_char(created_at AT TIME ZONE 'UTC', 'YYYY-MM-DD"T"HH24:MI:SS.US"Z"'),
      request_type,host,status_code,method,url_path,JSONB_ARRAY_LENGTH(errors),
      LEFT( CONCAT(
        'url=', COALESCE(raw_url, 'null'),
        ' response_body=', COALESCE(response_body, 'null'),
        ' request_body=', COALESCE(request_body, 'null')
      ), 255
      ),sum(attributes.client)) FROM apis.request_dumps
      WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid
      and ( created_at > NOW() - interval '14 days' AND (method='GET') )
      GROUP BY COALESCE(attributes.client::text, '')::text,time_bucket('60 seconds', timestamp) ORDER BY parent_id asc limit 1000|]
      normT query `shouldBe` normT expected
      
    it "summarize with bin_auto()" do
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime Nothing Nothing) "| summarize count(*) by bin_auto(timestamp)"
      let expected =
            [text|
      SELECT json_build_array(id::text,
      to_char(created_at AT TIME ZONE 'UTC', 'YYYY-MM-DD"T"HH24:MI:SS.US"Z"'),
      request_type,host,status_code,method,url_path,JSONB_ARRAY_LENGTH(errors),
      LEFT( CONCAT(
        'url=', COALESCE(raw_url, 'null'),
        ' response_body=', COALESCE(response_body, 'null'),
        ' request_body=', COALESCE(request_body, 'null')
      ), 255
      ),count(*)) FROM apis.request_dumps
      WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid
      and ( created_at > NOW() - interval '14 days' )
      GROUP BY time_bucket('5 minutes', timestamp) ORDER BY created_at desc limit 200|]
      normT query `shouldBe` normT expected
