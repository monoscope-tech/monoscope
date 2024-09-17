module Pages.LogSpec (spec) where

import Data.Time (defaultTimeLocale, formatTime, getCurrentTime, parseTimeM)
import Data.Time.Clock (UTCTime, addUTCTime)
import Data.UUID qualified as UUID
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (PageCtx (..))
import Pages.Log qualified as Log
import Pkg.TestUtils
import ProcessMessage (processRequestMessages)
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec


testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Log Page" do
    it "should return an empty list" \TestResources{..} -> do
      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Log.apiLogH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

      case pg of
        Log.LogPage (PageCtx _ content) -> do
          content.pid `shouldBe` testPid
          content.resultCount `shouldBe` 0
          content.cols `shouldBe` ["id", "created_at", "rest"]
        _ -> error "Unexpected response"

    it "should return log items" \TestResources{..} -> do
      currentTime <- getCurrentTime
      let yesterdayTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-86400) currentTime
      let twoDaysAgoTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-172800) currentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt

      -- new requests otherwise cursor for load more will be the same
      -- and hence loadmore request will return 0 items
      let reqMsg3 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 yesterdayTxt
      let reqMsg4 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 twoDaysAgoTxt

      let msgs = concat (replicate 100 [("m1", reqMsg1), ("m2", reqMsg2)]) ++ [("m3", reqMsg3), ("m4", reqMsg4)]
      res <- runTestBackground trATCtx $ processRequestMessages msgs
      length res `shouldBe` 202
      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Log.apiLogH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

      case pg of
        Log.LogPage (PageCtx _ content) -> do
          content.pid `shouldBe` testPid
          content.query `shouldBe` Nothing
          content.cols `shouldBe` ["id", "created_at", "rest"]
          length content.requestVecs `shouldBe` 200

          let cur = textToUTCTime $ fromMaybe "" content.cursor
          pg2 <-
            toServantResponse trATCtx trSessAndHeader trLogger $ Log.apiLogH testPid Nothing Nothing cur Nothing Nothing Nothing (Just "loadmore") Nothing Nothing (Just "true") Nothing
          case pg2 of
            Log.LogsGetRows pid requestVecs curatedColNames colIdxMap nextLogsURL source -> do
              pid `shouldBe` testPid
              length requestVecs `shouldBe` 2
            _ -> error "Unexpected response"
          content.resultCount `shouldBe` 202
        _ -> error "Unexpected response"


textToUTCTime :: Text -> Maybe UTCTime
textToUTCTime t = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (toString t)
