module Pages.Endpoints.OutgoingSpec (spec) where

import Data.Aeson (Value)
import Data.Aeson.QQ (aesonQQ)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Models.Apis.Endpoints
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (PageCtx (..))
import Pages.Endpoints.Outgoing qualified as Outgoing
import Pkg.Components.ItemsList qualified as ItemsList
import Pkg.TestUtils
import ProcessMessage (processRequestMessages)
import Relude
import Relude.Unsafe qualified as Unsafe
import Servant qualified
import Servant.Server qualified as ServantS
import System.Types (atAuthToBase, effToServantHandlerTest)
import Test.Hspec


testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Outgoing Integrations" do
    it "should return outgoing integrations" \TestResources{..} -> do
      let reqMsg1 = Unsafe.fromJust $ convert $ msg1
      let reqMsg2 = Unsafe.fromJust $ convert $ msg2
      let msgs = concat $ replicate 100 $ [("m1", reqMsg1), ("m2", reqMsg2)]
      _ <- runTestBackground trATCtx $ processRequestMessages msgs

      PageCtx _ (ItemsList.ItemsPage _ (he)) <-
        Outgoing.outgoingGetH testPid Nothing
          & atAuthToBase trSessAndHeader
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse

      length he `shouldBe` 2
      let githubM = V.find (\(Outgoing.HostEventsVM _ host) -> host.host == "github.com") he
      let aptM = V.find (\(Outgoing.HostEventsVM pid h) -> h.host == "api.apitoolkit.io") he
      isJust githubM `shouldBe` True
      isJust aptM `shouldBe` True
      let github = (\(Outgoing.HostEventsVM _ h) -> h) <$> Unsafe.fromJust $ githubM
      let apt = (\(Outgoing.HostEventsVM _ h) -> h) <$> Unsafe.fromJust $ aptM

      github.eventCount `shouldBe` 100
      apt.eventCount `shouldBe` 100

      1 `shouldBe` 1


msg1 :: Value
msg1 =
  [aesonQQ|{"duration":476434,
            "host":"github.com",
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
            "sdk_type":"JsAxiosOutgoing",
            "status_code":200,
            "timestamp":"2024-07-05T13:06:26.620094239Z",
            "url_path":"/","errors":[],"tags":[]} 
      |]


msg2 :: Value
msg2 =
  [aesonQQ|{"timestamp": "2024-07-05T13:06:26.620094239Z",
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
                "Content-Type":["application/json; charset=utf-8"]},"method":"POST","host":"api.apitoolkit.io",
                "raw_url":"/api/v1/user/login","referer":"https://grovepay-admin-git-selectbox-daemon-team.vercel.app/",
            "project_id":"00000000-0000-0000-0000-000000000000","url_path":"/api/v1/user/login",
            "response_body":"eyJlcnJvcnMiOiJjcnlwdG8vYmNyeXB0OiBoYXNoZWRQYXNzd29yZCBpcyBub3QgdGhlIGhhc2ggb2YgdGhlIGdpdmVuIHBhc3N3b3JkIiwibWVzc2FnZSI6ImludmFsaWQgY3JlZGVudGlhbHMiLCJzdGF0dXMiOiJVbnByb2Nlc3NhYmxlIEVudGl0eSIsInRpbWVzdGFtcCI6Ik1vbmRheSwgMTYtT2N0LTIzIDIxOjQ3OjQxIFVUQyJ9",
            "request_body":"eyJwYXNzd29yZCI6IltDTElFTlRfUkVEQUNURURdIiwidXNlcm5hbWUiOiJhZG1pbkBncm92ZXBheS5jby51ayJ9",
            "proto_minor":1,
            "sdk_type": "JsAxiosOutgoing",
            "status_code":422,"proto_major":1,"duration":103636077} 
        |]
