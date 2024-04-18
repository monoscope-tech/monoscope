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
      let (query, _) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime) "method==\"GET\""
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
    it "timechart query query" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime) "method==\"GET\""
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1h', created_at))::integer as timeB, count(*)::integer as count, 'Throughput' 
FROM apis.request_dumps WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid 
and ( created_at > NOW() - interval '14 days' AND (method='GET') ) GROUP BY timeB
      |]
      normT (fromMaybe "" c.finalTimechartQuery) `shouldBe` normT expected
    it "timechart query query 1d" do
      let (_, c) = fromRight' $ parseQueryToComponents (defSqlQueryCfg defPid fixedUTCTime) "method==\"GET\" | timechart count(*) [1d]"
      let expected =
            [text|
SELECT extract(epoch from time_bucket('1d', created_at))::integer as timeB, count(*)::integer as count, 'Throughput' 
FROM apis.request_dumps WHERE project_id='00000000-0000-0000-0000-000000000000'::uuid 
and ( created_at > NOW() - interval '14 days' AND (method='GET') ) GROUP BY timeB
      |]
      normT (fromMaybe "" c.finalTimechartQuery) `shouldBe` normT expected
