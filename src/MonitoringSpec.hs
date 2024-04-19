module MonitoringSpec (spec) where

import BackgroundJobs (jobsRunner)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), execute, withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT)
import Log.Backend.StandardOutput.Bulk qualified as LogBulk
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Projects qualified as Projects
import OddJobs.Job (Job)
import Pages.Monitors.Alerts (AlertUpsertForm (..), convertToQueryMonitor)
import Pkg.TmpPg qualified as TmpPg
import ProcessMessage (processMessages')
import ProcessMessageSpec (convert, msg1, msg2, runTestBackground, testAuthContext)
import Relude
import Relude.Unsafe qualified as Unsafe
import System.Config (AuthContext (config, pool, projectCache))
import Test.Hspec (Spec, aroundAll, describe, it)


spec :: Spec
spec = aroundAll TmpPg.withSetup do
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
      let reqMsg1 = Unsafe.fromJust $ convert $ msg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ msg2 nowTxt
      let msgs =
            [ ("m1", reqMsg1)
            , ("m2", reqMsg1)
            , ("m4", reqMsg1)
            , ("m5", reqMsg1)
            , ("m5", reqMsg2)
            ]
      _ <- runTestBackground authCtx $ processMessages' authCtx.config msgs authCtx.projectCache
      _ <- withPool pool $ execute Select [sql|CALL monitors.check_triggered_query_monitors(0, '{}')|] ()
      _ <- runAllBackgroundJobs authCtx
      -- TODO:
      -- Introduce a .env.test
      -- - Configure email sending into the test context
      pass


runAllBackgroundJobs :: AuthContext -> IO (V.Vector Job)
runAllBackgroundJobs authCtx = do
  jobs <- withPool authCtx.pool $ getBackgroundJobs
  LogBulk.withBulkStdOutLogger \logger ->
    V.forM_ jobs \job -> jobsRunner logger authCtx job
  pure jobs


getBackgroundJobs :: DBT IO (V.Vector Job)
getBackgroundJobs = DBT.query Select q ()
  where
    q = [sql|SELECT id, created_at, updated_at, run_at, status, payload,last_error, attempts, locked_at, locked_by FROM background_jobs|]
