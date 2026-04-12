module Models.Apis.Monitors (
  queryMonitorsAll,
  queryMonitorById,
  queryMonitorUpsert,
  monitorToggleActiveById,
  monitorDeactivateByIds,
  monitorReactivateByIds,
  monitorMuteByIds,
  monitorUnmuteByIds,
  monitorResolveByIds,
  monitorSoftDeleteByIds,
  QueryMonitor (..),
  MonitorAlertConfig (..),
  QueryMonitorId (..),
  MonitorStatus (..),
  getAlertsByTeamHandle,
  monitorRemoveTeam,
  getActiveQueryMonitors,
  updateLastEvaluatedAt,
  -- Widget alert functions
  queryMonitorByWidgetId,
  deleteMonitorsByWidgetIds,
  WidgetAlertStatus (..),
  getWidgetAlertStatuses,
) where

import Data.Aeson qualified as AE
import Data.CaseInsensitive qualified as CI
import Data.Default (Default (..))
import Data.Effectful.Hasql qualified as Hasql
import Data.OpenApi (ToSchema (..))
import Data.Text.Display (Display)
import Data.Time.Calendar (Day (..))
import Data.Time.Clock (UTCTime (..), addUTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Deriving.Aeson qualified as DAE
import Effectful (Eff, type (:>))
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import GHC.Records (HasField (getField))
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (SnakeSchema (..), WrappedEnumSC (..))
import Relude
import Servant (FromHttpApiData)
import System.Types (DB)


newtype QueryMonitorId = QueryMonitorId {unQueryMonitorId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON, Default, Eq, FromField, FromHttpApiData, HI.DecodeValue, HI.EncodeValue, NFData, Ord, ToField, ToSchema)
  deriving anyclass (HI.DecodeRow)


instance HasField "toText" QueryMonitorId Text where
  getField = UUID.toText . unQueryMonitorId


data MonitorStatus = MSNormal | MSWarning | MSAlerting
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving anyclass (Default, NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, HI.DecodeValue, HI.EncodeValue, ToField, ToSchema) via WrappedEnumSC "MS" MonitorStatus


instance HI.DecodeRow MonitorStatus where
  decodeRow = HI.getOneColumn <$> HI.decodeRow


instance ToSchema (CI.CI Text) where declareNamedSchema _ = declareNamedSchema (Proxy @Text)


data MonitorAlertConfig = MonitorAlertConfig
  { title :: Text
  , severity :: Text
  , subject :: Text
  , message :: Text
  , emails :: V.Vector (CI.CI Text)
  , emailAll :: Bool
  , slackChannels :: V.Vector Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, NFData)
  deriving (FromField, ToField) via Aeson MonitorAlertConfig
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] MonitorAlertConfig
  deriving (ToSchema) via SnakeSchema MonitorAlertConfig


deriving via Aeson MonitorAlertConfig instance HI.DecodeValue MonitorAlertConfig
deriving via Aeson MonitorAlertConfig instance HI.EncodeValue MonitorAlertConfig


data QueryMonitor = QueryMonitor
  { id :: QueryMonitorId
  , projectId :: Projects.ProjectId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , checkIntervalMins :: Int
  , alertThreshold :: Double
  , warningThreshold :: Maybe Double
  , logQuery :: Text
  , logQueryAsSql :: Text
  , lastEvaluated :: UTCTime
  , warningLastTriggered :: Maybe UTCTime
  , alertLastTriggered :: Maybe UTCTime
  , triggerLessThan :: Bool
  , thresholdSustainedForMins :: Int
  , alertConfig :: MonitorAlertConfig
  , deactivatedAt :: Maybe UTCTime
  , deletedAt :: Maybe UTCTime
  , visualizationType :: Text
  , teams :: V.Vector UUID.UUID
  , widgetId :: Maybe Text
  , dashboardId :: Maybe UUID.UUID
  , alertRecoveryThreshold :: Maybe Double
  , warningRecoveryThreshold :: Maybe Double
  , currentStatus :: MonitorStatus
  , currentValue :: Double
  , mutedUntil :: Maybe UTCTime
  , renotifyIntervalMins :: Maybe Int
  , stopAfterCount :: Maybe Int
  , notificationCount :: Int
  , timeWindowMins :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, HI.DecodeRow, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] QueryMonitor
  deriving (ToSchema) via SnakeSchema QueryMonitor


queryMonitorUpsert :: DB es => QueryMonitor -> Eff es Int64
queryMonitorUpsert qm = do
  let (qId, qPid, qAT, qWT, qLQ, qLQS, qLE, qWLT, qALT, qTLT) =
        (qm.id, qm.projectId, qm.alertThreshold, qm.warningThreshold, qm.logQuery, qm.logQueryAsSql, qm.lastEvaluated, qm.warningLastTriggered, qm.alertLastTriggered, qm.triggerLessThan)
      (qTSFM, qAC, qCIM, qVT, qTeams, qWId, qDId, qART, qWRT, qRIM, qSAC, qTWM) =
        (qm.thresholdSustainedForMins, qm.alertConfig, qm.checkIntervalMins, qm.visualizationType, qm.teams, qm.widgetId, qm.dashboardId, qm.alertRecoveryThreshold, qm.warningRecoveryThreshold, qm.renotifyIntervalMins, qm.stopAfterCount, qm.timeWindowMins)
  Hasql.interpExecute
    [HI.sql|
    INSERT INTO monitors.query_monitors (id, project_id, alert_threshold, warning_threshold, log_query,
                  log_query_as_sql, last_evaluated, warning_last_triggered, alert_last_triggered, trigger_less_than,
                  threshold_sustained_for_mins, alert_config, check_interval_mins, visualization_type, teams,
                  widget_id, dashboard_id, alert_recovery_threshold, warning_recovery_threshold,
                  renotify_interval_mins, stop_after_count, time_window_mins)
    VALUES (#{qId},#{qPid},#{qAT},#{qWT},#{qLQ},#{qLQS},#{qLE},#{qWLT},#{qALT},#{qTLT},#{qTSFM},#{qAC},#{qCIM},#{qVT},#{qTeams}::uuid[],#{qWId},#{qDId},#{qART},#{qWRT},#{qRIM},#{qSAC},#{qTWM})
    ON CONFLICT (id) DO UPDATE SET
                  alert_threshold=EXCLUDED.alert_threshold,
                  warning_threshold=EXCLUDED.warning_threshold,
                  log_query=EXCLUDED.log_query,
                  log_query_as_sql=EXCLUDED.log_query_as_sql,
                  last_evaluated=EXCLUDED.last_evaluated,
                  warning_last_triggered=EXCLUDED.warning_last_triggered,
                  alert_last_triggered=EXCLUDED.alert_last_triggered,
                  trigger_less_than=EXCLUDED.trigger_less_than,
                  threshold_sustained_for_mins=EXCLUDED.threshold_sustained_for_mins,
                  alert_config=EXCLUDED.alert_config,
                  check_interval_mins=EXCLUDED.check_interval_mins,
                  visualization_type=EXCLUDED.visualization_type,
                  teams=EXCLUDED.teams,
                  widget_id=EXCLUDED.widget_id,
                  dashboard_id=EXCLUDED.dashboard_id,
                  alert_recovery_threshold=EXCLUDED.alert_recovery_threshold,
                  warning_recovery_threshold=EXCLUDED.warning_recovery_threshold,
                  renotify_interval_mins=EXCLUDED.renotify_interval_mins,
                  stop_after_count=EXCLUDED.stop_after_count,
                  time_window_mins=EXCLUDED.time_window_mins
    |]


queryMonitorById :: DB es => QueryMonitorId -> Eff es (Maybe QueryMonitor)
queryMonitorById mid = Hasql.interpOne [HI.sql| SELECT * FROM monitors.query_monitors WHERE id = #{mid} |]


monitorToggleActiveById :: (DB es, Time :> es) => QueryMonitorId -> Eff es Int64
monitorToggleActiveById mid = do
  now <- Time.currentTime
  Hasql.interpExecute
    [HI.sql|
        UPDATE monitors.query_monitors SET deactivated_at=CASE
            WHEN deactivated_at IS NOT NULL THEN NULL
            ELSE #{now}::timestamptz
        END
        where id=#{mid}|]


monitorDeactivateByIds :: (DB es, Time :> es) => [QueryMonitorId] -> Eff es Int64
monitorDeactivateByIds ids = do
  now <- Time.currentTime
  let vIds = V.fromList ids
  Hasql.interpExecute [HI.sql| UPDATE monitors.query_monitors SET deactivated_at = #{now} WHERE id = ANY(#{vIds}::uuid[]) AND deactivated_at IS NULL |]


monitorReactivateByIds :: DB es => [QueryMonitorId] -> Eff es Int64
monitorReactivateByIds ids = do
  let vIds = V.fromList ids
  Hasql.interpExecute [HI.sql| UPDATE monitors.query_monitors SET deactivated_at = NULL WHERE id = ANY(#{vIds}::uuid[]) AND deactivated_at IS NOT NULL |]


-- | Mute monitors until a future time. Pass Nothing for indefinite mute.
monitorMuteByIds :: (DB es, Time :> es) => Maybe Int -> [QueryMonitorId] -> Eff es Int64
monitorMuteByIds durationMinsM ids = do
  now <- Time.currentTime
  let mutedUntil = maybe (UTCTime (ModifiedJulianDay 100000) 0) (\mins -> addUTCTime (fromIntegral mins * 60) now) durationMinsM
      vIds = V.fromList ids
  Hasql.interpExecute [HI.sql| UPDATE monitors.query_monitors SET muted_until = #{mutedUntil} WHERE id = ANY(#{vIds}::uuid[]) |]


monitorUnmuteByIds :: DB es => [QueryMonitorId] -> Eff es Int64
monitorUnmuteByIds ids = do
  let vIds = V.fromList ids
  Hasql.interpExecute [HI.sql| UPDATE monitors.query_monitors SET muted_until = NULL WHERE id = ANY(#{vIds}::uuid[]) AND muted_until IS NOT NULL |]


monitorResolveByIds :: DB es => [QueryMonitorId] -> Eff es Int64
monitorResolveByIds ids = do
  let vIds = V.fromList ids
  Hasql.interpExecute [HI.sql| UPDATE monitors.query_monitors SET current_status = 'normal', alert_last_triggered = NULL, warning_last_triggered = NULL, notification_count = 0 WHERE id = ANY(#{vIds}::uuid[]) AND current_status != 'normal' |]


monitorSoftDeleteByIds :: (DB es, Time :> es) => [QueryMonitorId] -> Eff es Int64
monitorSoftDeleteByIds ids = do
  now <- Time.currentTime
  let vIds = V.fromList ids
  Hasql.interpExecute [HI.sql| UPDATE monitors.query_monitors SET deleted_at = #{now} WHERE id = ANY(#{vIds}::uuid[]) AND deleted_at IS NULL |]


queryMonitorsAll :: DB es => Projects.ProjectId -> Eff es [QueryMonitor]
queryMonitorsAll pid = Hasql.interp [HI.sql| SELECT * FROM monitors.query_monitors WHERE project_id = #{pid} AND deleted_at IS NULL |]


getAlertsByTeamHandle :: DB es => Projects.ProjectId -> UUID.UUID -> Eff es [QueryMonitor]
getAlertsByTeamHandle pid teamId = Hasql.interp [HI.sql| SELECT * FROM monitors.query_monitors WHERE project_id = #{pid} AND #{teamId} = ANY(teams) AND deleted_at IS NULL |]


monitorRemoveTeam :: DB es => Projects.ProjectId -> QueryMonitorId -> UUID.UUID -> Eff es Int64
monitorRemoveTeam pid monitorId teamId =
  Hasql.interpExecute
    [HI.sql|
    UPDATE monitors.query_monitors
    SET teams = array_remove(teams, #{teamId}::uuid)
    WHERE project_id = #{pid} AND id = #{monitorId}
    |]


getActiveQueryMonitors :: DB es => Eff es [QueryMonitor]
getActiveQueryMonitors = Hasql.interp [HI.sql| SELECT * FROM monitors.query_monitors WHERE deactivated_at IS NULL AND deleted_at IS NULL AND log_query_as_sql IS NOT NULL AND log_query_as_sql != '' |]


updateLastEvaluatedAt :: DB es => QueryMonitorId -> UTCTime -> Eff es Int64
updateLastEvaluatedAt qmId time = Hasql.interpExecute [HI.sql|UPDATE monitors.query_monitors SET last_evaluated=#{time} where id=#{qmId}|]


queryMonitorByWidgetId :: DB es => Text -> Eff es (Maybe QueryMonitor)
queryMonitorByWidgetId wId = Hasql.interpOne [HI.sql| SELECT * FROM monitors.query_monitors WHERE widget_id = #{wId} AND deleted_at IS NULL |]


deleteMonitorsByWidgetIds :: DB es => [Text] -> Eff es Int64
deleteMonitorsByWidgetIds widgetIds = do
  let vIds = V.fromList widgetIds
  Hasql.interpExecute [HI.sql|DELETE FROM monitors.query_monitors WHERE widget_id = ANY(#{vIds}::text[])|]


data WidgetAlertStatus = WidgetAlertStatus
  { widgetId :: Text
  , monitorId :: QueryMonitorId
  , alertStatus :: MonitorStatus
  , currentValue :: Maybe Double
  , alertThreshold :: Double
  , warningThreshold :: Maybe Double
  , lastTriggeredAt :: Maybe UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, HI.DecodeRow, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] WidgetAlertStatus


getWidgetAlertStatuses :: DB es => V.Vector Text -> Eff es [WidgetAlertStatus]
getWidgetAlertStatuses widgetIds
  | V.null widgetIds = pure []
  | otherwise =
      Hasql.interp
        [HI.sql|
      SELECT
        widget_id,
        id,
        current_status,
        current_value,
        alert_threshold,
        warning_threshold,
        GREATEST(alert_last_triggered, warning_last_triggered)
      FROM monitors.query_monitors
      WHERE widget_id = ANY(#{widgetIds}::text[])
        AND deleted_at IS NULL
        AND deactivated_at IS NULL
    |]
