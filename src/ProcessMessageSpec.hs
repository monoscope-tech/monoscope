module ProcessMessageSpec (spec, testAuthContext) where

import Data.Cache (Cache, newCache)
import Data.Default (Default (..))
import Data.Pool (Pool)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), execute, withPool)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Pkg.TestUtils qualified as TestUtils
import ProcessMessage (processMessages, processRequestMessages)
import Relude
import Relude.Unsafe qualified as Unsafe
import System.Clock (TimeSpec (TimeSpec))
import System.Config qualified as Config
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldContain)
import qualified Data.HashMap.Strict as HashMap


pid :: Projects.ProjectId
pid = Projects.ProjectId UUID.nil


testAuthContext :: Pool Connection -> IO (Config.AuthContext)
testAuthContext pool = do
  projectCache <- newCache (Just $ TimeSpec (60 * 60) 0) :: IO (Cache Projects.ProjectId Projects.ProjectCache) -- 60*60secs or 1 hour TTL
  let config = (def :: Config.EnvConfig){Config.enableBackgroundJobs = True}
  pure $ Config.AuthContext{env = def :: Config.EnvConfig, pool = pool, jobsPool = pool, projectCache = projectCache, config}


spec :: Spec
spec = aroundAll TestUtils.withSetup do
  describe "process request to db" do
    it "test processing raw request message string" \pool -> do
      let jsonMsg = "{\"duration\":737639,\"host\":\"3.228.92.161\",\"method\":\"POST\",\"path_params\":{\"0\":\"/service/extension/backup/mboximport\"},\"project_id\":\"00000000-0000-0000-0000-000000000000\",\"proto_minor\":1,\"proto_major\":1,\"query_params\":{\"account-name\":[\"admin\"],\"account-status\":[\"1\"],\"ow\":[\"cmd\"]},\"raw_url\":\"/service/extension/backup/mboximport?account-name=admin&account-status=1&ow=cmd\",\"referer\":\"\",\"request_body\":\"eyJQS1x1MDAwM1x1MDAwNFx1MDAxNFx1MDAwMFxiXHUwMDAwXGJcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDAiOiJcdTAwMDBcdTAwMDBcdTAwMDAuLi8uLi8uLi8uLi9tYWlsYm94ZC93ZWJhcHBzL3ppbWJyYUFkbWluLzBNVnpBZTZwZ3dlNWdvMUQuanNwXHUwMDFjyL1cbu+/vTBcdTAwMTBcdTAwMDDvv71PUVx1MDAwMu+/vVx1MDAwNCEo77+9VVxc77+9W++/vVx1MDAxNjvvv710O++/vUNT77+9JO+/vUvvv73vv71F77+9b1VkLu+/ve+/ve+/vVgiLCIg77+9ae+/ve+/ve+/vTvvv71cdTAwMDRt77+977+9PkQ+77+9Re+/vXrvv73vv70/77+9XHUwMDA11Lvvv73vv71QZWbvv71P77+9QFx1MDAxYu+/ve+/ve+/vVDvv73vv71kXHUwMDA2YOOsvlwi77+9XHUwMDEx77+9XHUwMDA277+9yYDPiC/vv71Z77+9IVx1MDAxMe+/vVJ6REJG77+9yqxYf1x1MDAwM1x1MDAwMFx1MDAwMO+/ve+/vVBLXHUwMDA3XGJcdTAwMDI/77+9Xe+/vVx1MDAwMFx1MDAwMFx1MDAwMO+/vVx1MDAwMFx1MDAwMFx1MDAwMFBLXHUwMDAzXHUwMDA0XHUwMDE0XHUwMDAwXGJcdTAwMDBcYlx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMFx1MDAwMCI6Ilx1MDAwMFx1MDAwMFx1MDAwMC4uLy4uLy4uLy4uL21haWxib3hkL3dlYmFwcHMvemltYnJhQWRtaW4vME1WekFlNnBnd2U1Z28xRC5qc3BcdTAwMWPIvVxu77+9MFx1MDAxMFx1MDAwMO+/vU9RXHUwMDAy77+9XHUwMDA0ISjvv71VXFzvv71b77+9XHUwMDE2O++/vXQ777+9Q1Pvv70k77+9S++/ve+/vUXvv71vVWQu77+977+977+9WCIsIiDvv71p77+977+977+9O++/vVx1MDAwNG3vv73vv70+RD7vv71F77+9eu+/ve+/vT/vv71cdTAwMDXUu++/ve+/vVBlZu+/vU/vv71AXHUwMDFi77+977+977+9UO+/ve+/vWRcdTAwMDZg46y+XCLvv71cdTAwMTHvv71cdTAwMDbvv73JgM+IL++/vVnvv70hXHUwMDEx77+9UnpEQkbvv73KrFh/XHUwMDAzXHUwMDAwXHUwMDAw77+977+9UEtcdTAwMDdcYlx1MDAwMj/vv71d77+9XHUwMDAwXHUwMDAwXHUwMDAw77+9XHUwMDAwXHUwMDAwXHUwMDAwUEtcdTAwMDFcdTAwMDJcdTAwMTRcdTAwMDBcdTAwMTRcdTAwMDBcYlx1MDAwMFxiXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAyP++/vV3vv71cdTAwMDBcdTAwMDBcdTAwMDDvv71cdTAwMDBcdTAwMDBcdTAwMDAiOiJcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDBcdTAwMDAuLi8uLi8uLi8uLi9tYWlsYm94ZC93ZWJhcHBzL3ppbWJyYUFkbWluLzBNVnpBZTZwZ3dlNWdvMUQuanNwUEtcdTAwMDFcdTAwMDJcdTAwMTRcdTAwMDBcdTAwMTRcdTAwMDBcYlx1MDAwMFxiXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAyP++/vV3vv71cdTAwMDBcdTAwMDBcdTAwMDDvv71cdTAwMDBcdTAwMDBcdTAwMDA9XHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAw77+9XHUwMDAwXHUwMDAwXHUwMDAwLi4vLi4vLi4vLi4vbWFpbGJveGQvd2ViYXBwcy96aW1icmFBZG1pbi8wTVZ6QWU2cGd3ZTVnbzFELmpzcFBLXHUwMDA1XHUwMDA2XHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAyXHUwMDAwXHUwMDAyXHUwMDAw77+9XHUwMDAwXHUwMDAwXHUwMDAw77+9XHUwMDAxXHUwMDAwXHUwMDAwXHUwMDAwXHUwMDAwIn0=\",\"request_headers\":{\"connection\":[\"upgrade\"],\"host\":[\"3.228.92.161\"],\"x-real-ip\":[\"172.31.5.55\"],\"x-forwarded-for\":[\"138.199.34.206, 172.31.5.55\"],\"content-length\":[\"716\"],\"x-forwarded-proto\":[\"http\"],\"x-forwarded-port\":[\"80\"],\"x-amzn-trace-id\":[\"Root=1-65e01ff0-22108e1659ac458930d6e9b0\"],\"user-agent\":[\"Mozilla/5.0 (Windows NT 6.3; Win64; x64; rv:92.0) Gecko/20100101 Firefox/92.0\"],\"accept-encoding\":[\"gzip, deflate\"],\"content-type\":[\"application/x-www-form-urlencoded\"]},\"response_body\":\"Tm90IEZvdW5k\",\"response_headers\":{\"x-powered-by\":[\"Express\"],\"vary\":[\"Origin\"],\"access-control-allow-credentials\":[\"true\"],\"content-type\":[\"text/html; charset=utf-8\"],\"content-length\":[\"9\"],\"etag\":[\"W/\\\"9-0gXL1ngzMqISxa6S1zx3F4wtLyg\\\"\"]},\"sdk_type\":\"JsExpress\",\"status_code\":404,\"timestamp\":\"2024-02-29T06:10:56.870Z\",\"url_path\":\"/service/extension/backup/mboximport\",\"errors\":[],\"tags\":[],\"msg_id\":\"fdadc75d-5710-49cd-adfd-2d7547c9ab17\"}"
      let msgs = [("m1", jsonMsg)]
      authCtx <- testAuthContext pool
      resp <- TestUtils.runTestBackground authCtx $ processMessages msgs HashMap.empty
      resp `shouldBe` ["m1"]

    it "should save the request" \pool -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ TestUtils.convert $ TestUtils.testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ TestUtils.convert $ TestUtils.testRequestMsgs.reqMsg2 nowTxt
      let msgs =
            [ ("m1", reqMsg1)
            , ("m2", reqMsg2)
            ]
      authCtx <- testAuthContext pool
      resp <- TestUtils.runTestBackground authCtx $ processRequestMessages msgs
      resp `shouldBe` ["m1", "m2"]

    it "should be able to query request dumps that include the added requests" \pool -> do
      (reqs, count) <- withPool pool $ RequestDumps.selectRequestDumpByProject pid "" Nothing Nothing Nothing
      count `shouldBe` 2 -- Since 2 were saved above.
      count `shouldBe` length reqs

    it "We should expect 2 endpoints, albeit unacknowleged." \pool -> do
      _ <- withPool pool $ execute Select [sql|CALL apis.refresh_request_dump_views_every_5mins(0, '{}')|] ()
      endpoints <- withPool pool $ Endpoints.endpointRequestStatsByProject pid False False Nothing Nothing Nothing 0
      length endpoints `shouldBe` 2 -- Two new endpoints from the last 2 requests
      forM_ endpoints \enp -> do
        ["/", "/api/v1/user/login"] `shouldContain` [enp.urlPath]
      pass
