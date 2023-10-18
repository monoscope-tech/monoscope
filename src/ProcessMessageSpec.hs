module ProcessMessageSpec (spec) where

import Colog (logStringStdout)
import Config qualified
import Data.Aeson (Result (Error, Success), Value, fromJSON)
import Data.Aeson.QQ (aesonQQ)
import Data.Cache
import Data.Default (Default (..))
import Data.UUID qualified as UUID
import Database.PostgreSQL.Entity.DBT (QueryNature (Insert), execute, withPool)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Pkg.TmpPg qualified as TmpPg
import ProcessMessage (processMessages')
import Relude
import Relude.Unsafe qualified as Unsafe
import RequestMessages qualified
import System.Clock
import Test.Hspec


-- msg = PubsubMessage
--         { attributes = Nothing
--         , data' = Just
--             ( Base64
--                 { fromBase64 = "{"duration":476434,"host":"172.31.29.11","method":"GET","path_params":{},"project_id":"e36dd90e-ddcf-4d3b-a986-4de0bddcfcbb","proto_minor":1,"proto_major":1,"query_params":{},"raw_url":"/","referer":"","request_body":"e30=","request_headers":{"connection":["upgrade"],"host":["172.31.29.11"],"x-real-ip":["172.31.81.1"],"x-forwarded-for":["172.31.81.1"],"user-agent":["ELB-HealthChecker/2.0"],"accept-encoding":["gzip, compressed"]},"response_body":"V2VsY29tZSB0byBSZXRhaWxsb29w","response_headers":{"x-powered-by":["Express"],"vary":["Origin"],"access-control-allow-credentials":["true"],"content-type":["text/html; charset=utf-8"],"content-length":["21"],"etag":["W/\"15-2rFUmgZR2gmQik/+S8kDb7KSIZk\""]},"sdk_type":"JsExpress","status_code":200,"timestamp":"2023-10-16T19:34:31.513Z","url_path":"/","errors":[],"tags":[],"msg_id":"d3475081-0794-4700-a498-eccd623ae41f"}" }
--             )
--         , messageId = Just "8886167500690442"
--         , orderingKey = Nothing
--         , publishTime = Just
--             ( DateTime
--                 { unDateTime = 2023-10-16 19:34:31.543 UTC }
--             )
--         }

msg1 :: Value
msg1 =
  [aesonQQ|{"duration":476434,
                "host":"172.31.29.11",
                "method":"GET",
                "path_params":{},
                "project_id":"00000000-0000-0000-0000-000000000000",
                "proto_minor":1,
                "proto_major":1,"query_params":{},
                "raw_url":"/","referer":"","request_body":"e30=",
                "request_headers":{
                  "connection":["upgrade"],"host":["172.31.29.11"],
                  "x-real-ip":["172.31.81.1"],"x-forwarded-for":["172.31.81.1"],
                  "user-agent":["ELB-HealthChecker/2.0"],"accept-encoding":["gzip, compressed"]},
                  "response_body":"V2VsY29tZSB0byBSZXRhaWxsb29w","response_headers":{"x-powered-by":["Express"],
                  "vary":["Origin"],"access-control-allow-credentials":["true"],"content-type":["text/html; charset=utf-8"],
                  "content-length":["21"],"etag":["W/\"15-2rFUmgZR2gmQik/+S8kDb7KSIZk\""]
                },
                "sdk_type":"JsExpress",
                "status_code":200,
                "timestamp":"2023-10-16T19:34:31.513Z",
                "url_path":"/","errors":[],"tags":[],
                "msg_id":"d3475081-0794-4700-a498-eccd623ae41f"} 
      |]


msg2 :: Value
msg2 =
  [aesonQQ|{"timestamp":"2023-10-16T21:47:41.45628582Z",
            "request_headers":{
                "Accept":["application/json, text/plain, */*"],
                "Accept-Encoding":["gzip, deflate, br"],
                "Accept-Language":["en-US,en;q=0.9"],"Access-Control-Allow-Headers":["Content-Type"],
                "Access-Control-Allow-Origin":["*"],"Authorization":["Bearer null"],"Content-Length":["62"],
                "Content-Type":["application/json"],"Forwarded":["for=\"[2a01:4b00:f65f:0:59b2:efe6:d68b:691c]\";proto=https"],
                "Origin":["https://grovepay-admin-git-selectbox-daemon-team.vercel.app"],
                "Referer":["https://grovepay-admin-git-selectbox-daemon-team.vercel.app/"],
                "Sec-Ch-Ua":["\"Google Chrome\";v=\"117\", \"Not;A=Brand\";v=\"8\", \"Chromium\";v=\"117\""],
                "Sec-Ch-Ua-Mobile":["?0"],"Sec-Ch-Ua-Platform":["\"macOS\""],"Sec-Fetch-Dest":["empty"],
                "Sec-Fetch-Mode":["cors"],"Sec-Fetch-Site":["cross-site"],
                "Traceparent":["00-064f45d39dadb50679db4755b295ff84-e79ae1ef6a4e42c0-00"],
                "User-Agent":["Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36"],
                "X-Cloud-Trace-Context":["064f45d39dadb50679db4755b295ff84/16688899787571741376"],
                "X-Forwarded-For":["2a01:4b00:f65f:0:59b2:efe6:d68b:691c"],"X-Forwarded-Proto":["https"]},
                "query_params":{},"path_params":{},"response_headers":{"Access-Control-Allow-Credentials":["true"],
                "Access-Control-Allow-Origin":["*"],
                "Access-Control-Expose-Headers":["Content-Length,Authorization,X-Access-Token,X-Refresh-Token,Content-Type"],
                "Content-Type":["application/json; charset=utf-8"]},"method":"POST","sdk_type":"GoGin","host":"api.grovepay.co.uk",
                "raw_url":"/api/v1/user/login","referer":"https://grovepay-admin-git-selectbox-daemon-team.vercel.app/",
            "project_id":"00000000-0000-0000-0000-000000000000","url_path":"/api/v1/user/login",
            "response_body":"eyJlcnJvcnMiOiJjcnlwdG8vYmNyeXB0OiBoYXNoZWRQYXNzd29yZCBpcyBub3QgdGhlIGhhc2ggb2YgdGhlIGdpdmVuIHBhc3N3b3JkIiwibWVzc2FnZSI6ImludmFsaWQgY3JlZGVudGlhbHMiLCJzdGF0dXMiOiJVbnByb2Nlc3NhYmxlIEVudGl0eSIsInRpbWVzdGFtcCI6Ik1vbmRheSwgMTYtT2N0LTIzIDIxOjQ3OjQxIFVUQyJ9",
            "request_body":"eyJwYXNzd29yZCI6IltDTElFTlRfUkVEQUNURURdIiwidXNlcm5hbWUiOiJhZG1pbkBncm92ZXBheS5jby51ayJ9",
            "proto_minor":1,
            "status_code":422,"proto_major":1,"duration":103636077} 
        |]


pid :: Projects.ProjectId
pid = Projects.ProjectId UUID.nil


spec :: Spec
spec = aroundAll TmpPg.withSetup do
  describe "process request to db" do
    it "should save the request" \pool -> do
      let reqMsg1 = Unsafe.fromJust $ convert msg1
      let reqMsg2 = Unsafe.fromJust $ convert msg2
      let msgs =
            [ Right (Just "m1", reqMsg1)
            , Right (Just "m2", reqMsg2)
            ]
      projectCache <- newCache (Just $ TimeSpec (60 * 60) 0) :: IO (Cache Projects.ProjectId Projects.ProjectCache) -- 60*60secs or 1 hour TTL
      resp <- processMessages' logStringStdout (def :: Config.EnvConfig) pool msgs projectCache
      resp `shouldBe` [Just "m1", Just "m2"]

    it "should be able to query request dumps that include the added requests" \pool -> do
      (reqs, count) <- withPool pool $ RequestDumps.selectRequestDumpByProject pid "" Nothing
      count `shouldBe` 2 -- Since 2 were saved above.
      count `shouldBe` length reqs

    it "We should expect 2 endpoints, albeit unacknowleged." \pool -> do
      _ <-
        withPool pool
          $ execute
            Insert
            [sql|
            REFRESH MATERIALIZED VIEW CONCURRENTLY apis.endpoint_request_stats;
            REFRESH MATERIALIZED VIEW CONCURRENTLY apis.project_request_stats;
            REFRESH MATERIALIZED VIEW CONCURRENTLY apis.target_hash_agg_14days;
            REFRESH MATERIALIZED VIEW CONCURRENTLY apis.target_hash_agg_24hrs;
        |]
            ()
      endpoints <- withPool pool $ Endpoints.endpointRequestStatsByProject pid False False Nothing
      length endpoints `shouldBe` 2 -- Two new endpoints from the last 2 requests
      forM_ endpoints \enp -> do
        ["/", "/api/v1/user/login"] `shouldContain` [enp.urlPath]
      pass


convert :: Value -> Maybe RequestMessages.RequestMessage
convert val = case fromJSON val of
  Success p -> Just p
  Error _ -> Nothing
