module ProcessMessageSpec (spec) where

import Data.Aeson (Result (Error, Success), Value, fromJSON)
import Data.Aeson.QQ (aesonQQ)
import Data.Cache
import Data.Default (Default (..))
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Entity.DBT (withPool)
import Log.Backend.StandardOutput.Bulk qualified as LogBulk
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Pkg.TmpPg qualified as TmpPg
import ProcessMessage (processMessages, processMessages')
import Relude
import Relude.Unsafe qualified as Unsafe
import RequestMessages qualified
import System.Clock
import System.Config qualified as Config
import System.Types
import Test.Hspec


msg1 :: Text -> Value
msg1 timestamp =
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
            "timestamp": #{timestamp},
            "url_path":"/","errors":[],"tags":[],
            "msg_id":"d3475081-0794-4700-a498-eccd623ae41f"} 
      |]


msg2 :: Text -> Value
msg2 timestamp =
  [aesonQQ|{"timestamp": #{timestamp},
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
    it "test processing raw request message string" \pool -> do
      currentTime <- getCurrentTime
      projectCache <- newCache (Just $ TimeSpec (60 * 60) 0) :: IO (Cache Projects.ProjectId Projects.ProjectCache) -- 60*60secs or 1 hour TTL
      let config = (def :: Config.EnvConfig)
      let authContext = Config.AuthContext{env = def :: Config.EnvConfig, pool = pool, jobsPool = pool, projectCache = projectCache, config}

      let jsonMsg = "{\"duration\":737639,\"host\":\"3.228.92.161\",\"method\":\"POST\",\"path_params\":{\"0\":\"/service/extension/backup/mboximport\"},\"project_id\":\"00000000-0000-0000-0000-000000000000\",\"proto_minor\":1,\"proto_major\":1,\"query_params\":{\"account-name\":[\"admin\"],\"account-status\":[\"1\"],\"ow\":[\"cmd\"]},\"raw_url\":\"/service/extension/backup/mboximport?account-name=admin&account-status=1&ow=cmd\",\"referer\":\"\",\"request_body\":\"eyJQS1x1MDAwM1x1MDAwNFx1MDAxNFx1MDAwMFxiXHUwMDAwXGJcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDAiOiJcdTAwMDBcdTAwMDBcdTAwMDAuLi8uLi8uLi8uLi9tYWlsYm94ZC93ZWJhcHBzL3ppbWJyYUFkbWluLzBNVnpBZTZwZ3dlNWdvMUQuanNwXHUwMDFjyL1cbu+/vTBcdTAwMTBcdTAwMDDvv71PUVx1MDAwMu+/vVx1MDAwNCEo77+9VVxc77+9W++/vVx1MDAxNjvvv710O++/vUNT77+9JO+/vUvvv73vv71F77+9b1VkLu+/ve+/ve+/vVgiLCIg77+9ae+/ve+/ve+/vTvvv71cdTAwMDRt77+977+9PkQ+77+9Re+/vXrvv73vv70/77+9XHUwMDA11Lvvv73vv71QZWbvv71P77+9QFx1MDAxYu+/ve+/ve+/vVDvv73vv71kXHUwMDA2YOOsvlwi77+9XHUwMDEx77+9XHUwMDA277+9yYDPiC/vv71Z77+9IVx1MDAxMe+/vVJ6REJG77+9yqxYf1x1MDAwM1x1MDAwMFx1MDAwMO+/ve+/vVBLXHUwMDA3XGJcdTAwMDI/77+9Xe+/vVx1MDAwMFx1MDAwMFx1MDAwMO+/vVx1MDAwMFx1MDAwMFx1MDAwMFBLXHUwMDAzXHUwMDA0XHUwMDE0XHUwMDAwXGJcdTAwMDBcYlx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMCI6Ilx1MDAwMFx1MDAwMFx1MDAwMC4uLy4uLy4uLy4uL21haWxib3hkL3dlYmFwcHMvemltYnJhQWRtaW4vME1WekFlNnBnd2U1Z28xRC5qc3BcdTAwMWPIvVxu77+9MFx1MDAxMFx1MDAwMO+/vU9RXHUwMDAy77+9XHUwMDA0ISjvv71VXFzvv71b77+9XHUwMDE2O++/vXQ777+9Q1Pvv70k77+9S++/ve+/vUXvv71vVWQu77+977+977+9WCIsIiDvv71p77+977+977+9O++/vVx1MDAwNG3vv73vv70+RD7vv71F77+9eu+/ve+/vT/vv71cdTAwMDXUu++/ve+/vVBlZu+/vU/vv71AXHUwMDFi77+977+977+9UO+/ve+/vWRcdTAwMDZg46y+XCLvv71cdTAwMTHvv71cdTAwMDbvv73JgM+IL++/vVnvv70hXHUwMDEx77+9UnpEQkbvv73KrFh/XHUwMDAzXHUwMDAwXHUwMDAw77+977+9UEtcdTAwMDdcYlx1MDAwMj/vv71d77+9XHUwMDAwXHUwMDAwXHUwMDAw77+9XHUwMDAwXHUwMDAwXHUwMDAwUEtcdTAwMDFcdTAwMDJcdTAwMTRcdTAwMDBcdTAwMTRcdTAwMDBcYlx1MDAwMFxiXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAyP++/vV3vv71cdTAwMDBcdTAwMDBcdTAwMDDvv71cdTAwMDBcdTAwMDBcdTAwMDAiOiJcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDAuLi8uLi8uLi8uLi9tYWlsYm94ZC93ZWJhcHBzL3ppbWJyYUFkbWluLzBNVnpBZTZwZ3dlNWdvMUQuanNwUEtcdTAwMDFcdTAwMDJcdTAwMTRcdTAwMDBcdTAwMTRcdTAwMDBcYlx1MDAwMFxiXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAyP++/vV3vv71cdTAwMDBcdTAwMDBcdTAwMDDvv71cdTAwMDBcdTAwMDBcdTAwMDA9XHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAw77+9XHUwMDAwXHUwMDAwXHUwMDAwLi4vLi4vLi4vLi4vbWFpbGJveGQvd2ViYXBwcy96aW1icmFBZG1pbi8wTVZ6QWU2cGd3ZTVnbzFELmpzcFBLXHUwMDA1XHUwMDA2XHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAyXHUwMDAwXHUwMDAyXHUwMDAw77+9XHUwMDAwXHUwMDAwXHUwMDAw77+9XHUwMDAxXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwIn0=\",\"request_headers\":{\"connection\":[\"upgrade\"],\"host\":[\"3.228.92.161\"],\"x-real-ip\":[\"172.31.5.55\"],\"x-forwarded-for\":[\"138.199.34.206, 172.31.5.55\"],\"content-length\":[\"716\"],\"x-forwarded-proto\":[\"http\"],\"x-forwarded-port\":[\"80\"],\"x-amzn-trace-id\":[\"Root=1-65e01ff0-22108e1659ac458930d6e9b0\"],\"user-agent\":[\"Mozilla/5.0 (Windows NT 6.3; Win64; x64; rv:92.0) Gecko/20100101 Firefox/92.0\"],\"accept-encoding\":[\"gzip, deflate\"],\"content-type\":[\"application/x-www-form-urlencoded\"]},\"response_body\":\"Tm90IEZvdW5k\",\"response_headers\":{\"x-powered-by\":[\"Express\"],\"vary\":[\"Origin\"],\"access-control-allow-credentials\":[\"true\"],\"content-type\":[\"text/html; charset=utf-8\"],\"content-length\":[\"9\"],\"etag\":[\"W/\\\"9-0gXL1ngzMqISxa6S1zx3F4wtLyg\\\"\"]},\"sdk_type\":\"JsExpress\",\"status_code\":404,\"timestamp\":\"2024-02-29T06:10:56.870Z\",\"url_path\":\"/service/extension/backup/mboximport\",\"errors\":[],\"tags\":[],\"msg_id\":\"fdadc75d-5710-49cd-adfd-2d7547c9ab17\"}"
      let msgs = [("m1", jsonMsg)]

      resp <- LogBulk.withBulkStdOutLogger \logger ->
        runBackground logger authContext
          $ processMessages config msgs projectCache
      resp `shouldBe` ["m1"]

    it "should save the request" \pool -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ msg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ msg2 nowTxt
      let msgs =
            [ ("m1", reqMsg1)
            , ("m2", reqMsg2)
            ]
      projectCache <- newCache (Just $ TimeSpec (60 * 60) 0) :: IO (Cache Projects.ProjectId Projects.ProjectCache) -- 60*60secs or 1 hour TTL
      let config = (def :: Config.EnvConfig)
      let authContext = Config.AuthContext{env = def :: Config.EnvConfig, pool = pool, jobsPool = pool, projectCache = projectCache, config}
      resp <- LogBulk.withBulkStdOutLogger \logger -> runBackground logger authContext $ processMessages' config msgs projectCache
      resp `shouldBe` ["m1", "m2"]

    it "should be able to query request dumps that include the added requests" \pool -> do
      (reqs, count) <- withPool pool $ RequestDumps.selectRequestDumpByProject pid "" Nothing Nothing Nothing
      count `shouldBe` 2 -- Since 2 were saved above.
      count `shouldBe` length reqs


-- it "We should expect 2 endpoints, albeit unacknowleged." \pool -> do
--   _ <-
--     withPool pool
--       $ execute
--         Insert
--         [sql|
--         REFRESH MATERIALIZED VIEW CONCURRENTLY apis.endpoint_request_stats;
--         REFRESH MATERIALIZED VIEW CONCURRENTLY apis.project_request_stats;
--         REFRESH MATERIALIZED VIEW CONCURRENTLY apis.target_hash_agg_14days;
--         REFRESH MATERIALIZED VIEW CONCURRENTLY apis.target_hash_agg_24hrs;
--     |]
--         ()
--   endpoints <- withPool pool $ Endpoints.endpointRequestStatsByProject pid False False Nothing
--   length endpoints `shouldBe` 2 -- Two new endpoints from the last 2 requests
--   forM_ endpoints \enp -> do
--     ["/", "/api/v1/user/login"] `shouldContain` [enp.urlPath]
--   pass

convert :: Value -> Maybe RequestMessages.RequestMessage
convert val = case fromJSON val of
  Success p -> Just p
  Error _ -> Nothing
