module Pages.LogExplorer.LogSpec (spec) where

import Data.Aeson qualified as AE
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HashMap
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.Time.Clock (addUTCTime)
import Data.UUID qualified as UUID
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (PageCtx (..))
import Pages.LogExplorer.Log qualified as Log
import Pkg.TestUtils
import ProcessMessage (processMessages)
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
        toServantResponse trATCtx trSessAndHeader trLogger $ Log.apiLogH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing

      case pg of
        Log.LogsGetJson vecs colors nextLogsURL resetLogsURL recentLogsURL cols colIdxMap count -> do
          count `shouldBe` 0
          cols `shouldBe` ["id", "timestamp", "service", "summary", "latency_breakdown"]
          length vecs `shouldBe` 0
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

      let msgs = concat (replicate 100 [("m1", BL.toStrict $ AE.encode reqMsg1), ("m2", BL.toStrict $ AE.encode reqMsg2)]) ++ [("m3", BL.toStrict $ AE.encode reqMsg3), ("m4", BL.toStrict $ AE.encode reqMsg4)]
      processMessagesAndBackgroundJobs TestResources{..} msgs
      -- res <- runTestBackground trATCtx $ processMessages msgs HashMap.empty
      -- length res `shouldBe` 202
      
      -- Get time range that includes all messages (3 days ago to now)
      let threeDaysAgo = addUTCTime (-259200) currentTime  -- 3 days in seconds
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" threeDaysAgo
      let toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      
      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Log.apiLogH testPid Nothing Nothing Nothing Nothing fromTime toTime Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing

      case pg of
        Log.LogsGetJson vecs colors nextLogsURL resetLogsURL recentLogsURL cols colIdxMap count -> do
          cols `shouldBe` ["id", "timestamp", "service", "summary", "latency_breakdown"]
          length vecs `shouldBe` 150  -- API limits to 150 results per page
          count `shouldSatisfy` (>= 202)  -- Should have at least 202 (100*2 + 2)
        _ -> error "Unexpected response"
