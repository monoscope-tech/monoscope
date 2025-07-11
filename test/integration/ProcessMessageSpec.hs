module ProcessMessageSpec (spec, testAuthContext) where

import Data.Aeson qualified as AE
import Data.Cache (Cache, newCache)
import Data.Default (Default (..))
import Data.HashMap.Strict qualified as HashMap
import Data.Pool (Pool)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime, addUTCTime)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Entity.DBT (execute, withPool)
import Database.PostgreSQL.Transact qualified as PGT
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Pkg.TestUtils qualified as TestUtils
import ProcessMessage (processMessages)
import BackgroundJobs (processFiveMinuteSpans)
import Relude
import Relude.Unsafe qualified as Unsafe
import System.Clock (TimeSpec (TimeSpec))
import System.Config qualified as Config
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldContain)


pid :: Projects.ProjectId
pid = Projects.ProjectId UUID.nil


testAuthContext :: Pool Connection -> IO Config.AuthContext
testAuthContext pool = do
  projectCache <- newCache (Just $ TimeSpec (60 * 60) 0) :: IO (Cache Projects.ProjectId Projects.ProjectCache) -- 60*60secs or 1 hour TTL
  projectKeyCache <- newCache Nothing
  let config = (def :: Config.EnvConfig){Config.enableBackgroundJobs = True}
  pure $ Config.AuthContext{env = def :: Config.EnvConfig, pool = pool, jobsPool = pool, timefusionPgPool = pool, projectCache = projectCache, projectKeyCache, config}


spec :: Spec
spec = aroundAll TestUtils.withSetup do
  describe "process request to db" do
    it "test processing raw request message string" \pool -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      -- Use a simple test message without problematic Unicode escape sequences
      let jsonMsg = "{\"duration\":737639,\"host\":\"3.228.92.161\",\"method\":\"POST\",\"path_params\":{\"0\":\"/service/extension/backup/mboximport\"},\"project_id\":\"00000000-0000-0000-0000-000000000000\",\"proto_minor\":1,\"proto_major\":1,\"query_params\":{\"account-name\":[\"admin\"],\"account-status\":[\"1\"],\"ow\":[\"cmd\"]},\"raw_url\":\"/service/extension/backup/mboximport?account-name=admin&account-status=1&ow=cmd\",\"referer\":\"\",\"request_body\":\"e30=\",\"request_headers\":{\"connection\":[\"upgrade\"],\"host\":[\"3.228.92.161\"],\"x-real-ip\":[\"172.31.5.55\"],\"x-forwarded-for\":[\"138.199.34.206, 172.31.5.55\"],\"content-length\":[\"716\"],\"x-forwarded-proto\":[\"http\"],\"x-forwarded-port\":[\"80\"],\"x-amzn-trace-id\":[\"Root=1-65e01ff0-22108e1659ac458930d6e9b0\"],\"user-agent\":[\"Mozilla/5.0 (Windows NT 6.3; Win64; x64; rv:92.0) Gecko/20100101 Firefox/92.0\"],\"accept-encoding\":[\"gzip, deflate\"],\"content-type\":[\"application/x-www-form-urlencoded\"]},\"response_body\":\"Tm90IEZvdW5k\",\"response_headers\":{\"x-powered-by\":[\"Express\"],\"vary\":[\"Origin\"],\"access-control-allow-credentials\":[\"true\"],\"content-type\":[\"text/html; charset=utf-8\"],\"content-length\":[\"9\"],\"etag\":[\"W/\\\"9-0gXL1ngzMqISxa6S1zx3F4wtLyg\\\"\"]},\"sdk_type\":\"JsExpress\",\"status_code\":404,\"timestamp\":\"" <> toString nowTxt <> "\",\"url_path\":\"/service/extension/backup/mboximport\",\"errors\":[],\"tags\":[],\"msg_id\":\"fdadc75d-5710-49cd-adfd-2d7547c9ab17\"}"
      let msgs = [("m1", encodeUtf8 jsonMsg)]
      authCtx <- testAuthContext pool
      resp <- TestUtils.runTestBackground authCtx $ processMessages msgs HashMap.empty
      resp `shouldBe` ["m1"]

    it "should save the request" \pool -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ TestUtils.convert $ TestUtils.testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ TestUtils.convert $ TestUtils.testRequestMsgs.reqMsg2 nowTxt
      let msgs =
            [ ("m1", toStrict $ AE.encode reqMsg1)
            , ("m2", toStrict $ AE.encode reqMsg2)
            ]
      authCtx <- testAuthContext pool
      resp <- TestUtils.runTestBackground authCtx $ processMessages msgs HashMap.empty
      resp `shouldBe` ["m1", "m2"]

    it "We should expect 2 endpoints, albeit unacknowleged." \pool -> do
      -- First, create the messages
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ TestUtils.convert $ TestUtils.testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ TestUtils.convert $ TestUtils.testRequestMsgs.reqMsg2 nowTxt
      let msgs =
            [ ("m1", toStrict $ AE.encode reqMsg1)
            , ("m2", toStrict $ AE.encode reqMsg2)
            ]
      authCtx <- testAuthContext pool
      _ <- TestUtils.runTestBackground authCtx $ processMessages msgs HashMap.empty
      _ <- TestUtils.runTestBackground authCtx $ processFiveMinuteSpans currentTime
      _ <- TestUtils.runAllBackgroundJobs authCtx
      -- Now refresh the materialized view to see the results
      _ <- withPool pool $ TestUtils.refreshMaterializedView "apis.endpoint_request_stats"
      endpoints <- withPool pool $ Endpoints.endpointRequestStatsByProject pid False False Nothing Nothing Nothing 0 "Incoming"
      length endpoints `shouldBe` 3 -- Two new endpoints from the last 2 requests
      forM_ endpoints \enp -> do
        ["/", "/api/v1/user/login", "/service/extension/backup/mboximport"] `shouldContain` [enp.urlPath]
      pass
