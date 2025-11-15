module MonitoringSpec (spec) where

import Data.Aeson qualified as AE
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HashMap
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Transact qualified as PGT
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Projects qualified as Projects
import Pages.Monitors.Alerts (AlertUpsertForm (..), convertToQueryMonitor)
import Pkg.TestUtils
import ProcessMessage (processMessages)
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec


spec :: Spec
spec = aroundAll withTestResources do
  describe "Query Log Monitors" do
    it "should create monitor with no triggers" $ \tr@TestResources{..} -> do
      currentTime <- getCurrentTime
      let queryMonitor =
            convertToQueryMonitor (Projects.ProjectId UUID.nil) currentTime (Monitors.QueryMonitorId UUID.nil)
              $ AlertUpsertForm
                { alertId = Nothing
                , warningThreshold = Just "3"
                , alertThreshold = 4
                , recipientEmails = ["test@monoscope.tech"]
                , recipientSlacks = ["default"]
                , recipientEmailAll = Just True
                , direction = "above"
                , title = "Test Query Monitor"
                , severity = "Warning"
                , subject = "Test Query Subject"
                , message = "Test Query Message"
                , query = "status_code==200"
                , since = "7d"
                , from = ""
                , to = ""
                , frequency = Nothing
                , timeWindow = Nothing
                , conditionType = Nothing
                , source = Nothing
                }
      respC <- withPool trPool $ Monitors.queryMonitorUpsert queryMonitor
      respC `shouldBe` 1
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt
      let msgs =
            [ ("m1", BL.toStrict $ AE.encode reqMsg1)
            , ("m2", BL.toStrict $ AE.encode reqMsg1)
            , ("m4", BL.toStrict $ AE.encode reqMsg1)
            , ("m5", BL.toStrict $ AE.encode reqMsg1)
            , ("m5", BL.toStrict $ AE.encode reqMsg2)
            ]
      r <- runTestBg tr $ processMessages msgs HashMap.empty
      r `shouldBe` ["m1", "m2", "m4", "m5", "m5"]

      _ <- withPool trPool $ PGT.execute [sql|CALL monitors.check_triggered_query_monitors(0, '{}'::jsonb)|] ()

      pendingJobs <- getPendingBackgroundJobs trATCtx
      logBackgroundJobsInfo trLogger pendingJobs

      void $ runAllBackgroundJobs trATCtx
