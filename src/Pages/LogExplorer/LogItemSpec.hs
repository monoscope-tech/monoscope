module Pages.LogExplorer.LogItemSpec (spec) where

import Control.Lens ((^?))
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Aeson.QQ (aesonQQ)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Models.Projects.Projects qualified as Projects
import Pages.LogExplorer.LogItem qualified as LogItem
import Pkg.TestUtils
import ProcessMessage (processRequestMessages)
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec


testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Log Item" do
    it "should return an empty list" \TestResources{..} -> do
      currentTime <- getCurrentTime
      logId <- UUID.nextRandom
      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger $ LogItem.apiLogItemH testPid logId currentTime Nothing
      case pg of
        LogItem.ApiLogItemNotFound msg -> do
          msg `shouldBe` "Invalid request log ID"
        _ -> error "Unexpected response"
    it "should return a log item" \TestResources{..} -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ msg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt
      let msgs =
            concat $
              replicate
                5
                [ ("m1", reqMsg1)
                , ("m2", reqMsg2)
                ]
      _ <- runTestBackground trATCtx $ processRequestMessages msgs
      _ <- runAllBackgroundJobs trATCtx

      let logId = Unsafe.fromJust $ UUID.fromText "00000000-0000-0000-0000-000000000000"
      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger $ LogItem.apiLogItemH testPid logId currentTime Nothing

      case pg of
        LogItem.ApiLogItem pid lId item urlPath source -> do
          item ^? key "urlPath" . _String `shouldBe` Just "/hello"
          item ^? key "rawUrl" . _String `shouldBe` Just "/hello?hi=byebye"
          item ^? key "method" . _String `shouldBe` Just "GET"
          item ^? key "statusCode" . _Number `shouldBe` Just 200
          item ^? key "errorsCount" . _Number `shouldBe` Just 0
        _ -> error "Unexpected response"


msg1 :: Text -> Value
msg1 timestamp =
  [aesonQQ|{"duration":476434,
            "host":"172.31.29.11",
            "method":"GET",
            "path_params":{},
            "project_id":"00000000-0000-0000-0000-000000000000",
            "proto_minor":1,
            "proto_major":1,"query_params":{},
            "raw_url":"/hello?hi=byebye","referer":"","request_body":"e30=",
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
            "msg_id": "00000000-0000-0000-0000-000000000000",
            "timestamp": #{timestamp},
            "url_path":"/hello","errors":[],"tags":[]}
      |]
