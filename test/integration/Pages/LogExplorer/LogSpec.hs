module Pages.LogExplorer.LogSpec (spec) where

import Data.Aeson qualified as AE
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HashMap
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.Time.Clock (addUTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
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
        Log.LogsGetJson requestVecs serviceColors nextUrl resetUrl recentUrl cols colIdxMap resultCount -> do
          -- For empty case
          V.length requestVecs `shouldBe` 0
          resultCount `shouldBe` 0
          cols `shouldBe` ["id", "timestamp", "service", "summary", "latency_breakdown"]
        _ -> error "Expected JSON response but got something else"

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
      res <- runTestBackground trATCtx $ processMessages msgs HashMap.empty
      length res `shouldBe` 202
      
      -- Get time range that includes all messages (3 days ago to 1 day from now)
      let threeDaysAgo = addUTCTime (-259200) currentTime  -- 3 days in seconds
      let oneDayFuture = addUTCTime 86400 currentTime  -- 1 day in the future
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" threeDaysAgo
      let toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" oneDayFuture
      
      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Log.apiLogH testPid Nothing Nothing Nothing Nothing fromTime toTime Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing

      case pg of
        Log.LogsGetJson requestVecs serviceColors nextUrl resetUrl recentUrl cols colIdxMap resultCount -> do
          -- Verify we got results
          V.length requestVecs `shouldBe` 150  -- API limits to 150 results per page
          resultCount `shouldSatisfy` (>= 202)  -- At least our 202 test messages (might include data from other tests)
          
          -- Verify column structure
          cols `shouldBe` ["id", "timestamp", "service", "summary", "latency_breakdown"]
          
          -- Verify URLs are generated correctly
          nextUrl `shouldNotBe` ""
          resetUrl `shouldNotBe` ""
          recentUrl `shouldNotBe` ""
        _ -> error "Expected JSON response but got something else"

    it "should handle query filters correctly" \TestResources{..} -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt

      -- Process some test messages
      let msgs = [("m1", BL.toStrict $ AE.encode reqMsg1), ("m2", BL.toStrict $ AE.encode reqMsg2)]
      res <- runTestBackground trATCtx $ processMessages msgs HashMap.empty
      length res `shouldBe` 2
      
      -- Get time range that includes the messages we just processed
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) currentTime
      let toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 currentTime

      -- Test with a query filter (using proper string comparison for JSONB field)
      let query = "status_code == \"200\""
      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Log.apiLogH testPid (Just query) Nothing Nothing Nothing fromTime toTime Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing

      case pg of
        Log.LogsGetJson requestVecs _ _ _ _ _ _ resultCount -> do
          -- Should only return entries matching the query
          resultCount `shouldSatisfy` (> 0)
          V.length requestVecs `shouldSatisfy` (> 0)
        _ -> error "Expected JSON response but got something else"

    it "should paginate results correctly" \TestResources{..} -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt

      -- Create many messages to test pagination
      let msgs = take 200 $ zip (map (\i -> "m" <> show i) [1..]) (repeat $ BL.toStrict $ AE.encode reqMsg)
      res <- runTestBackground trATCtx $ processMessages msgs HashMap.empty
      length res `shouldBe` 200
      
      -- Get time range that includes the messages we just processed
      let fromTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime (-60) currentTime
      let toTime = Just $ toText $ formatTime defaultTimeLocale "%FT%T%QZ" $ addUTCTime 60 currentTime

      -- First page
      pg1 <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Log.apiLogH testPid Nothing Nothing Nothing Nothing fromTime toTime Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just "true") Nothing

      case pg1 of
        Log.LogsGetJson requestVecs1 _ nextUrl1 _ _ _ _ resultCount1 -> do
          V.length requestVecs1 `shouldBe` 150  -- API limits to 150 per page
          resultCount1 `shouldSatisfy` (>= 200)  -- At least our 200 test messages
          nextUrl1 `shouldNotBe` ""
        _ -> error "Expected JSON response but got something else"
