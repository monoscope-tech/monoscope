module Pages.LogExplorer.LogItemSpec (spec) where

import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Models.Projects.Projects qualified as Projects
import Pages.LogExplorer.LogItem qualified as LogItem
import Pkg.TestUtils
import ProcessMessage (processRequestMessages)
import Test.Hspec

import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Relude
import Servant qualified
import Servant.Server qualified as ServantS
import System.Types (atAuthToBase, effToServantHandlerTest)

import Models.Apis.RequestDumps (RequestDumpLogItem (..))

import Relude.Unsafe qualified as Unsafe


testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Log Item" do
    it "should return an empty list" \TestResources{..} -> do
      currentTime <- getCurrentTime
      logId <- UUID.nextRandom
      pg <-
        LogItem.apiLogItemH testPid logId currentTime
          & atAuthToBase trSessAndHeader
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse
      case pg of
        LogItem.ApiLogItemNotFound msg -> do
          msg `shouldBe` "Invalid request log ID"
        _ -> error "Unexpected response"
    it "should return a log item" \TestResources{..} -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt
      let msgs =
            concat
              $ replicate 5
              $ [ ("m1", reqMsg1)
                , ("m2", reqMsg2)
                ]
      _ <- runTestBackground trATCtx $ processRequestMessages msgs
      _ <- runAllBackgroundJobs trATCtx

      let logId = Unsafe.fromJust $ UUID.fromText "00000000-0000-0000-0000-000000000000"
      pg <-
        LogItem.apiLogItemH testPid logId currentTime
          & atAuthToBase trSessAndHeader
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse

      case pg of
        LogItem.ApiLogItem (item, urlPath) -> do
          item.urlPath `shouldBe` "/"
          item.method `shouldBe` "GET"
          item.statusCode `shouldBe` 200
          item.errorsCount `shouldBe` 0
        _ -> error "Unexpected response"
