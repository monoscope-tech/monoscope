module ProcessMessageSpec (spec) where

import Data.Aeson qualified as AE
import Data.HashMap.Strict qualified as HashMap
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Entity.DBT (withPool)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Pkg.TestUtils
import ProcessMessage (processMessages)
import BackgroundJobs (processFiveMinuteSpans)
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldContain)


pid :: Projects.ProjectId
pid = Projects.ProjectId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "process request to db" do
    it "test processing raw request message string" $ \tr -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let jsonMsg = "{\"duration\":737639,\"host\":\"3.228.92.161\",\"method\":\"POST\",\"path_params\":{\"0\":\"/service/extension/backup/mboximport\"},\"project_id\":\"00000000-0000-0000-0000-000000000000\",\"proto_minor\":1,\"proto_major\":1,\"query_params\":{\"account-name\":[\"admin\"],\"account-status\":[\"1\"],\"ow\":[\"cmd\"]},\"raw_url\":\"/service/extension/backup/mboximport?account-name=admin&account-status=1&ow=cmd\",\"referer\":\"\",\"request_body\":\"e30=\",\"request_headers\":{\"connection\":[\"upgrade\"],\"host\":[\"3.228.92.161\"],\"x-real-ip\":[\"172.31.5.55\"],\"x-forwarded-for\":[\"138.199.34.206, 172.31.5.55\"],\"content-length\":[\"716\"],\"x-forwarded-proto\":[\"http\"],\"x-forwarded-port\":[\"80\"],\"x-amzn-trace-id\":[\"Root=1-65e01ff0-22108e1659ac458930d6e9b0\"],\"user-agent\":[\"Mozilla/5.0 (Windows NT 6.3; Win64; x64; rv:92.0) Gecko/20100101 Firefox/92.0\"],\"accept-encoding\":[\"gzip, deflate\"],\"content-type\":[\"application/x-www-form-urlencoded\"]},\"response_body\":\"Tm90IEZvdW5k\",\"response_headers\":{\"x-powered-by\":[\"Express\"],\"vary\":[\"Origin\"],\"access-control-allow-credentials\":[\"true\"],\"content-type\":[\"text/html; charset=utf-8\"],\"content-length\":[\"9\"],\"etag\":[\"W/\\\"9-0gXL1ngzMqISxa6S1zx3F4wtLyg\\\"\"]},\"sdk_type\":\"JsExpress\",\"status_code\":404,\"timestamp\":\"" <> toString nowTxt <> "\",\"url_path\":\"/service/extension/backup/mboximport\",\"errors\":[],\"tags\":[],\"msg_id\":\"fdadc75d-5710-49cd-adfd-2d7547c9ab17\"}"
      let msgs = [("m1", encodeUtf8 jsonMsg)]
      resp <- runTestBg tr $ processMessages msgs HashMap.empty
      resp `shouldBe` ["m1"]

    it "should save the request" $ \tr -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt
      let msgs =
            [ ("m1", toStrict $ AE.encode reqMsg1)
            , ("m2", toStrict $ AE.encode reqMsg2)
            ]
      resp <- runTestBg tr $ processMessages msgs HashMap.empty
      resp `shouldBe` ["m1", "m2"]

    it "We should expect 2 endpoints, albeit unacknowleged." $ \tr@TestResources{..} -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt
      let msgs =
            [ ("m1", toStrict $ AE.encode reqMsg1)
            , ("m2", toStrict $ AE.encode reqMsg2)
            ]
      _ <- runTestBg tr $ processMessages msgs HashMap.empty
      _ <- runTestBg tr $ processFiveMinuteSpans currentTime pid

      pendingJobs <- getPendingBackgroundJobs trATCtx
      logBackgroundJobsInfo trLogger pendingJobs

      _ <- runAllBackgroundJobs trATCtx
      endpoints <- withPool trPool $ Endpoints.endpointRequestStatsByProject pid False False Nothing Nothing Nothing 0 "Incoming"
      length endpoints `shouldBe` 3
      forM_ endpoints \enp ->
        ["/", "/api/v1/user/login", "/service/extension/backup/mboximport"] `shouldContain` [enp.urlPath]
