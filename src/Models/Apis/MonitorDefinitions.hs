module Models.Apis.MonitorDefinitions (MonitorInput (..), monitorFromInput, monitorToInput, compileAlertSql) where

import Data.Aeson qualified as AE
import Data.CaseInsensitive qualified as CI
import Data.Default (Default)
import Data.OpenApi (ToSchema)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Deriving.Aeson.Stock qualified as DAE
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.ProjectMembers qualified as PM
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (SnakeSchema (..))
import Pkg.Parser qualified as Parser
import Relude


-- | Input for creating or replacing (PUT) a monitor. Maps onto `QueryMonitor`
-- fields without surfacing db-internal lifecycle columns.
data MonitorInput = MonitorInput
  { unit :: Maybe Text
  , title :: Text
  , query :: Text
  , severity :: Maybe Text
  , subject :: Maybe Text
  , message :: Maybe Text
  , alertThreshold :: Double
  , warningThreshold :: Maybe Double
  , triggerLessThan :: Bool
  , checkIntervalMins :: Int
  , timeWindowMins :: Int
  , thresholdSustainedForMins :: Maybe Int
  , notifyAfterMins :: Maybe Int
  , stopAfterCount :: Maybe Int
  , emails :: [Text]
  , emailAll :: Maybe Bool
  , slackChannels :: [Text]
  , teams :: [PM.TeamId]
  , visualizationType :: Maybe Text
  , alertRecoveryThreshold :: Maybe Double
  , warningRecoveryThreshold :: Maybe Double
  , active :: Maybe Bool
  , environment :: Maybe Text
  , service :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake MonitorInput
  deriving (ToSchema) via SnakeSchema MonitorInput


-- | The compiled form of a monitor's KQL, which is what alert evaluation actually
-- runs. Shared by the create/PUT path and by PATCH: PATCH used to build its merged
-- monitor from the stored row, so it updated @log_query@ and inherited the previous
-- @log_query_as_sql@ — the monitor kept alerting on its old query. Falls back to
-- @prev@ when the query doesn't parse, so a bad edit cannot blank the compiled SQL.
compileAlertSql :: Projects.ProjectId -> Int -> Maybe Text -> Maybe Text -> Text -> Text -> Text
compileAlertSql pid windowMins environment service q prev =
  let scope = Parser.mkScopedQuery pid (Nothing, Nothing) environment service
      cfg = (Parser.applyScopedQuery scope $ Parser.defSqlQueryCfg pid Parser.fixedUTCTime Nothing Nothing){Parser.alertLookbackMins = windowMins}
   in fromMaybe prev $ (.finalAlertQuery) . snd =<< rightToMaybe (Parser.parseQueryToComponents cfg q)


-- | Build a 'QueryMonitor' from a 'MonitorInput'. Shared by create/update.
monitorFromInput :: Projects.ProjectId -> UTCTime -> Monitors.QueryMonitorId -> Maybe Monitors.QueryMonitor -> MonitorInput -> Monitors.QueryMonitor
monitorFromInput pid now mid existingM inp =
  Monitors.QueryMonitor
    { id = mid
    , projectId = pid
    , createdAt = maybe now (.createdAt) existingM
    , updatedAt = now
    , checkIntervalMins = inp.checkIntervalMins
    , alertThreshold = inp.alertThreshold
    , warningThreshold = inp.warningThreshold
    , logQuery = inp.query
    , logQueryAsSql = compileAlertSql pid inp.timeWindowMins inp.environment inp.service inp.query (foldMap (.logQueryAsSql) existingM)
    , lastEvaluated = Just now
    , warningLastTriggered = existingM >>= (.warningLastTriggered)
    , alertLastTriggered = existingM >>= (.alertLastTriggered)
    , triggerLessThan = inp.triggerLessThan
    , thresholdSustainedForMins = fromMaybe 0 inp.thresholdSustainedForMins
    , alertConfig =
        Monitors.MonitorAlertConfig
          { unit = mfilter (not . T.null) $ T.strip <$> inp.unit
          , title = inp.title
          , severity = fromMaybe "error" inp.severity
          , subject = fromMaybe inp.title inp.subject
          , message = fromMaybe "" inp.message
          , emails = V.fromList $ CI.mk <$> inp.emails
          , emailAll = fromMaybe False inp.emailAll
          , slackChannels = V.fromList inp.slackChannels
          }
    , deactivatedAt = bool (Just now) Nothing (fromMaybe True inp.active)
    , deletedAt = Nothing
    , visualizationType = fromMaybe "timeseries" inp.visualizationType
    , teams = V.fromList inp.teams
    , widgetId = existingM >>= (.widgetId)
    , dashboardId = existingM >>= (.dashboardId)
    , alertRecoveryThreshold = inp.alertRecoveryThreshold
    , warningRecoveryThreshold = inp.warningRecoveryThreshold
    , currentStatus = maybe Monitors.MSNormal (.currentStatus) existingM
    , currentValue = maybe 0 (.currentValue) existingM
    , mutedUntil = existingM >>= (.mutedUntil)
    , renotifyIntervalMins = inp.notifyAfterMins
    , stopAfterCount = inp.stopAfterCount
    , notificationCount = maybe 0 (.notificationCount) existingM
    , timeWindowMins = inp.timeWindowMins
    , environment = inp.environment
    , service = inp.service
    }


monitorToInput :: Monitors.QueryMonitor -> MonitorInput
monitorToInput m =
  MonitorInput
    { unit = m.alertConfig.unit
    , title = m.alertConfig.title
    , query = m.logQuery
    , severity = Just m.alertConfig.severity
    , subject = Just m.alertConfig.subject
    , message = Just m.alertConfig.message
    , alertThreshold = m.alertThreshold
    , warningThreshold = m.warningThreshold
    , triggerLessThan = m.triggerLessThan
    , checkIntervalMins = m.checkIntervalMins
    , timeWindowMins = m.timeWindowMins
    , thresholdSustainedForMins = Just m.thresholdSustainedForMins
    , notifyAfterMins = m.renotifyIntervalMins
    , stopAfterCount = m.stopAfterCount
    , emails = V.toList (CI.original <$> m.alertConfig.emails)
    , emailAll = Just m.alertConfig.emailAll
    , slackChannels = V.toList m.alertConfig.slackChannels
    , teams = V.toList m.teams
    , visualizationType = Just m.visualizationType
    , alertRecoveryThreshold = m.alertRecoveryThreshold
    , warningRecoveryThreshold = m.warningRecoveryThreshold
    , active = Just (isNothing m.deactivatedAt)
    , environment = m.environment
    , service = m.service
    }
