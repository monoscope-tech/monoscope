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
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.CaseInsensitive qualified as CI
import Data.Default (Default)
import Data.Time.Clock (UTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity (Entity, selectById, selectManyByField)
import Database.PostgreSQL.Entity.DBT (
  QueryNature (..),
  execute,
  query,
 )
import Database.PostgreSQL.Entity.Types (
  CamelToSnake,
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
import Database.PostgreSQL.Transact (DBT)
import Deriving.Aeson qualified as DAE
import GHC.Records (HasField (getField))
import Models.Projects.Projects qualified as Projects
import Relude (
  Bool,
  Eq,
  Generic,
  IO,
  Int,
  Int64,
  Maybe,
  NFData,
  Ord,
  Show,
  Text,
  (.),
 )
import Servant (FromHttpApiData)


newtype QueryMonitorId = QueryMonitorId {unQueryMonitorId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (ToJSON, FromJSON, Eq, Ord, FromField, ToField, FromHttpApiData, NFData, Default)


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
  deriving anyclass (NFData, Default)
  deriving (FromJSON, ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] MonitorAlertConfig
  deriving (FromField, ToField) via Aeson MonitorAlertConfig


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
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData, Default)
  deriving (FromJSON, ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] QueryMonitor
  deriving (Entity) via (GenericEntity '[Schema "monitors", TableName "query_monitors", PrimaryKey "id", FieldModifiers '[CamelToSnake]] QueryMonitor)


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
  , evalResult :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData, Default)
  deriving (FromJSON, ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] QueryMonitorEvaled


queryMonitorUpsert :: QueryMonitor -> DBT IO Int64
queryMonitorUpsert qm =
  execute
    Insert
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
    )
  where
    q =
      [sql|
    INSERT INTO monitors.query_monitors (id, project_id, alert_threshold, warning_threshold, log_query, 
                  log_query_as_sql, last_evaluated, warning_last_triggered, alert_last_triggered, trigger_less_than, 
                  threshold_sustained_for_mins, alert_config, check_interval_mins  ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)
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
                  check_interval_mins=EXCLUDED.check_interval_mins;
    |]


queryMonitorById :: QueryMonitorId -> DBT IO (Maybe QueryMonitor)
queryMonitorById id' = selectById @QueryMonitor (Only id')


queryMonitorsById :: V.Vector QueryMonitorId -> DBT IO (V.Vector QueryMonitorEvaled)
queryMonitorsById ids = query Select q (Only ids)
  where
    q =
      [sql|
    SELECT id, created_at, updated_at, project_id, check_interval_mins, alert_threshold, warning_threshold, 
        log_query, log_query_as_sql, last_evaluated, warning_last_triggered, alert_last_triggered, trigger_less_than, 
        threshold_sustained_for_mins, alert_config, deactivated_at, deleted_at, eval(log_query_as_sql)
      FROM monitors.query_monitors where id=ANY(?::UUID[]) 
    |]


updateQMonitorTriggeredState :: QueryMonitorId -> Bool -> DBT IO Int64
updateQMonitorTriggeredState qmId isAlert = execute Update q (Only qmId)
  where
    q =
      if isAlert
        then [sql|UPDATE monitors.query_monitors SET alert_last_triggered=NOW() where id=?|]
        else [sql|UPDATE monitors.query_monitors SET warning_last_triggered=NOW() where id=?|]


monitorToggleActiveById :: QueryMonitorId -> DBT IO Int64
monitorToggleActiveById id' = execute Update q (Only id')
  where
    q =
      [sql| 
        UPDATE monitors.query_monitors SET deactivated_at=CASE
            WHEN deactivated_at IS NOT NULL THEN NULL
            ELSE NOW()
        END
        where id=?|]


queryMonitorsAll :: Projects.ProjectId -> DBT IO (V.Vector QueryMonitor)
queryMonitorsAll pid = selectManyByField @QueryMonitor [DAT.field| project_id |] pid
