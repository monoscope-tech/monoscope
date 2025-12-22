module Models.Apis.Monitors (
  queryMonitorsAll,
  queryMonitorById,
  queryMonitorsById,
  queryMonitorUpsert,
  monitorToggleActiveById,
  QueryMonitor (..),
  QueryMonitorEvaled (..),
  MonitorAlertConfig (..),
  QueryMonitorId (..),
  updateQMonitorTriggeredState,
  getAlertsByTeamHandle,
) where

import Data.Aeson qualified as AE
import Data.CaseInsensitive qualified as CI
import Data.Default (Default)
import Data.Time.Clock (UTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity (_selectWhere)
import Database.PostgreSQL.Entity.Types (
  CamelToSnake,
  Entity,
  FieldModifiers,
  GenericEntity,
  PrimaryKey,
  Schema,
  TableName,
 )
import Database.PostgreSQL.Entity.Types qualified as DAT
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Deriving.Aeson qualified as DAE
import Effectful (Eff, type (:>))
import Effectful.PostgreSQL qualified as PG
import GHC.Records (HasField (getField))
import Models.Projects.Projects qualified as Projects
import Relude
import Servant (FromHttpApiData)
import System.Types (DB)


newtype QueryMonitorId = QueryMonitorId {unQueryMonitorId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON, Default, Eq, FromField, FromHttpApiData, NFData, Ord, ToField)


instance HasField "toText" QueryMonitorId Text where
  getField = UUID.toText . unQueryMonitorId


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


data QueryMonitor = QueryMonitor
  { id :: QueryMonitorId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , projectId :: Projects.ProjectId
  , checkIntervalMins :: Int
  , alertThreshold :: Int
  , warningThreshold :: Maybe Int
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
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "monitors", TableName "query_monitors", PrimaryKey "id", FieldModifiers '[CamelToSnake]] QueryMonitor)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] QueryMonitor


data QueryMonitorEvaled = QueryMonitorEvaled
  { id :: QueryMonitorId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , projectId :: Projects.ProjectId
  , checkIntervalMins :: Int
  , alertThreshold :: Int
  , warningThreshold :: Maybe Int
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
  , evalResult :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] QueryMonitorEvaled


queryMonitorUpsert :: DB es => QueryMonitor -> Eff es Int64
queryMonitorUpsert qm =
  PG.execute
    q
    ( qm.id
    , qm.projectId
    , qm.alertThreshold
    , qm.warningThreshold
    , qm.logQuery
    , qm.logQueryAsSql
    , qm.lastEvaluated
    , qm.warningLastTriggered
    , qm.alertLastTriggered
    , qm.triggerLessThan
    , qm.thresholdSustainedForMins
    , qm.alertConfig
    , qm.checkIntervalMins
    , qm.visualizationType
    , qm.teams
    )
  where
    q =
      [sql|
    INSERT INTO monitors.query_monitors (id, project_id, alert_threshold, warning_threshold, log_query,
                  log_query_as_sql, last_evaluated, warning_last_triggered, alert_last_triggered, trigger_less_than,
                  threshold_sustained_for_mins, alert_config, check_interval_mins, visualization_type, teams) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?::uuid[])
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
                  teams=EXCLUDED.teams
    |]


queryMonitorById :: DB es => QueryMonitorId -> Eff es (Maybe QueryMonitor)
queryMonitorById id' = listToMaybe <$> PG.query (_selectWhere @QueryMonitor [[DAT.field| id |]]) (Only id')


queryMonitorsById :: DB es => V.Vector QueryMonitorId -> Eff es [QueryMonitorEvaled]
queryMonitorsById ids
  | V.null ids = pure []
  | otherwise = PG.query q (Only ids)
  where
    q =
      [sql|
    SELECT id, created_at, updated_at, project_id, check_interval_mins, alert_threshold, warning_threshold,
        log_query, log_query_as_sql, last_evaluated, warning_last_triggered, alert_last_triggered, trigger_less_than,
        threshold_sustained_for_mins, alert_config, deactivated_at, deleted_at, visualization_type, teams, eval(log_query_as_sql)
      FROM monitors.query_monitors where id=ANY(?::UUID[])
    |]


updateQMonitorTriggeredState :: DB es => QueryMonitorId -> Bool -> Eff es Int64
updateQMonitorTriggeredState qmId isAlert = PG.execute q (Only qmId)
  where
    q =
      if isAlert
        then [sql|UPDATE monitors.query_monitors SET alert_last_triggered=NOW() where id=?|]
        else [sql|UPDATE monitors.query_monitors SET warning_last_triggered=NOW() where id=?|]


monitorToggleActiveById :: DB es => QueryMonitorId -> Eff es Int64
monitorToggleActiveById id' = PG.execute q (Only id')
  where
    q =
      [sql| 
        UPDATE monitors.query_monitors SET deactivated_at=CASE
            WHEN deactivated_at IS NOT NULL THEN NULL
            ELSE NOW()
        END
        where id=?|]


queryMonitorsAll :: DB es => Projects.ProjectId -> Eff es [QueryMonitor]
queryMonitorsAll pid = PG.query (_selectWhere @QueryMonitor [[DAT.field| project_id |]]) (Only pid)


getAlertsByTeamHandle :: DB es => Projects.ProjectId -> UUID.UUID -> Eff es [QueryMonitor]
getAlertsByTeamHandle pid teamId = PG.query q (pid, teamId)
  where
    q =
      [sql|
      SELECT qm.id, qm.created_at, qm.updated_at, qm.project_id, qm.check_interval_mins, qm.alert_threshold, qm.warning_threshold,
        qm.log_query, qm.log_query_as_sql, qm.last_evaluated, qm.warning_last_triggered, qm.alert_last_triggered, qm.trigger_less_than,
        qm.threshold_sustained_for_mins, qm.alert_config, qm.deactivated_at, qm.deleted_at, qm.visualization_type, qm.teams
      FROM monitors.query_monitors qm
      WHERE qm.project_id = ? AND ? = ANY(qm.teams)
    |]
