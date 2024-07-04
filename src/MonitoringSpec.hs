module MonitoringSpec (spec) where

import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), execute, withPool)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Projects qualified as Projects
import Pages.Monitors.Alerts (AlertUpsertForm (..), convertToQueryMonitor)
import Pkg.TestUtils qualified as TestUtils
import ProcessMessage (processRequestMessages)
import ProcessMessageSpec (runTestBackground, testAuthContext)
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec (Spec, aroundAll, describe, it)


spec :: Spec
spec = aroundAll TestUtils.withSetup do
  describe "Query Log Monitors" do
    it "should create monitor with no triggers" \pool -> do
      currentTime <- getCurrentTime
      authCtx <- testAuthContext pool
      let queryMonitor =
            convertToQueryMonitor (Projects.ProjectId UUID.nil) currentTime (Monitors.QueryMonitorId UUID.nil)
              $ AlertUpsertForm
                { alertId = Nothing
                , warningThreshold = Just "3" -- Text due to servant parsing limitations
                , alertThreshold = 4
                , recipientEmails = ["test@apitoolkit.io"]
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
                }
      _ <- withPool pool $ Monitors.queryMonitorUpsert queryMonitor
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ TestUtils.convert $ TestUtils.testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ TestUtils.convert $ TestUtils.testRequestMsgs.reqMsg2 nowTxt
      let msgs =
            [ ("m1", reqMsg1)
            , ("m2", reqMsg1)
            , ("m4", reqMsg1)
            , ("m5", reqMsg1)
            , ("m5", reqMsg2)
            ]
      _ <- runTestBackground authCtx $ processRequestMessages msgs
      _ <- withPool pool $ execute Select [sql|CALL monitors.check_triggered_query_monitors(0, '{}')|] ()
      _ <- TestUtils.runAllBackgroundJobs authCtx
      -- TODO:
      -- Introduce a .env.test
      -- - Configure email sending into the test context
      pass
