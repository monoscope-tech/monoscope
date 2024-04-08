module Models.Apis.Monitors (
  queryMonitorsAll,
  queryMonitorById,
  queryMonitorUpsert,
  QueryMonitor (..),
  MonitorAlertConfig (..),
  QueryMonitorId (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.CaseInsensitive qualified as CI
import Data.Time.Clock (UTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity (Entity, insert, selectById, selectManyByField)
import Database.PostgreSQL.Entity.DBT (
  QueryNature (..),
  execute,
  query,
  queryOne,
 )
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
import Database.PostgreSQL.Transact (DBT)
import Deriving.Aeson qualified as DAE
import Models.Projects.Projects qualified as Projects
import Relude
import Servant (FromHttpApiData)


newtype QueryMonitorId = QueryMonitorId {unQueryMonitorId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (ToJSON, FromJSON, Eq, Ord, FromField, ToField, FromHttpApiData, NFData)


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
  deriving anyclass (NFData)
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
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving (FromJSON, ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] QueryMonitor
  deriving (Entity) via (GenericEntity '[Schema "monitors", TableName "query_monitors", PrimaryKey "id", FieldModifiers '[CamelToSnake]] QueryMonitor)


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


queryMonitorsAll :: Projects.ProjectId -> DBT IO (V.Vector QueryMonitor)
queryMonitorsAll pid = selectManyByField @QueryMonitor [DAT.field| project_id |] pid
