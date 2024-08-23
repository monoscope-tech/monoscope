module Pages.ShareSpec (spec) where

import Data.Aeson (Value)
import Data.Aeson.QQ (aesonQQ)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Models.Apis.RequestDumps
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (PageCtx (..))
import Pkg.TestUtils
import ProcessMessage (processRequestMessages)
import Relude
import Relude.Unsafe qualified as Unsafe
import Servant.Server qualified as ServantS
import System.Types (effToServantHandlerTest)
import Test.Hspec

import Pages.Share qualified as Share


testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Share Request" do
    it "should create share link" \TestResources{..} -> do
      currentTime <- getCurrentTime
      let shareForm =
            Share.ReqForm
              { expiresIn = "1 hour"
              , reqId = Unsafe.fromJust $ UUID.fromText "00000000-0000-0000-0000-000000000000"
              , reqCreatedAt = currentTime
              }
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ msg1 nowTxt
      let msgs = [("m1", reqMsg1)]

      _ <- runTestBackground trATCtx $ processRequestMessages msgs

      res <- toServantResponse trATCtx trSessAndHeader trLogger $ Share.shareLinkPostH testPid shareForm
      case res of
        Share.ShareLinkPost shareId -> do
          let share_id = Unsafe.fromJust $ UUID.fromText shareId
          Share.ShareLinkGet (PageCtx _ reqM) <-
            Share.shareLinkGetH share_id
              & effToServantHandlerTest trATCtx trLogger
              & ServantS.runHandler
              <&> fromRightShow
          isJust reqM `shouldBe` True
          let req = Unsafe.fromJust reqM
          req.id `shouldBe` Unsafe.fromJust (UUID.fromText "00000000-0000-0000-0000-000000000000")
          req.projectId `shouldBe` testPid
          req.urlPath `shouldBe` "/"
        _ -> error "Unexpected response"

    it "should NOT create share link" \TestResources{..} -> do
      currentTime <- getCurrentTime
      let shareForm =
            Share.ReqForm
              { expiresIn = "55 hour"
              , reqId = Unsafe.fromJust $ UUID.fromText "00000000-0000-0000-0000-000000000000"
              , reqCreatedAt = currentTime
              }
      res <- toServantResponse trATCtx trSessAndHeader trLogger $ Share.shareLinkPostH testPid shareForm
      case res of
        Share.ShareLinkPostError -> do
          pass
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
            "msg_id": "00000000-0000-0000-0000-000000000000",
            "timestamp": #{timestamp},
            "url_path":"/","errors":[],"tags":[]} 
      |]
