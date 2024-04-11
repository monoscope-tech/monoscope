{-# LANGUAGE DuplicateRecordFields #-}

module Models.Apis.Testing (
  Collection (..),
  CollectionId (..),
  CollectionListItem (..),
  CollectionStepId (..),
  CollectionStep (..),
  addCollection,
  addCollectionStep,
  getCollections,
  updateCollection,
  insertSteps,
  updateCollectionConfig,
  updateCollectionStep,
  getCollectionSteps,
  scheduleInsertScheduleInBackgroundJobs,
  getCollectionById,
  deleteCollectionSteps,
  deleteCollectionStep,
  updateSchedule,
  getCollectionsId,
  getCollectionStepById,
)
where

import Data.Aeson as Aeson
import Data.Aeson qualified as AE
import Data.Default (Default)
import Data.Default.Instances ()
import Data.Time (UTCTime, ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity (insert, selectById)
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, query, queryOne)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple hiding (execute, executeMany, query)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Transact (DBT, executeMany)
import GHC.Records (HasField (getField))
import Models.Projects.Projects qualified as Projects
import Relude
import Web.HttpApiData (FromHttpApiData)


newtype CollectionId = CollectionId {collectionId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving
    (Eq, Ord, ToJSON, FromJSON, FromField, ToField, FromHttpApiData, Default, NFData)
    via UUID.UUID
  deriving anyclass (FromRow, ToRow)


instance HasField "toText" CollectionId Text where
  getField colid = UUID.toText colid.collectionId


newtype CollectionStepId = CollectionStepId {collectionStepId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving
    (Eq, Ord, ToJSON, FromJSON, FromField, ToField, FromHttpApiData, Default, NFData)
    via UUID.UUID


instance HasField "toText" CollectionStepId Text where
  getField = UUID.toText . collectionStepId


data CollectionStep = CollectionStep
  { id :: CollectionStepId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , lastRun :: Maybe ZonedTime
  , projectId :: Projects.ProjectId
  , collectionId :: CollectionId
  , stepData :: Value
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, ToJSON, FromJSON, NFData)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "test_steps", PrimaryKey "id", FieldModifiers '[CamelToSnake]] CollectionStep)


data Collection = Collection
  { id :: CollectionId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , lastRun :: Maybe ZonedTime
  , projectId :: Projects.ProjectId
  , title :: Text
  , description :: Text
  , config :: Value
  , schedule :: Maybe Text
  , isScheduled :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, ToJSON, FromJSON, NFData)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "testing", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Collection)


data CollectionListItem = ReportListItem
  { id :: CollectionId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , projectId :: Projects.ProjectId
  , lastRun :: Maybe ZonedTime
  , title :: Text
  , description :: Text
  , stepsCount :: Int
  , schedule :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "testing", PrimaryKey "id", FieldModifiers '[CamelToSnake]] CollectionListItem)


addCollection :: Collection -> DBT IO ()
addCollection = insert @Collection


addCollectionStep :: CollectionStep -> DBT IO ()
addCollectionStep = insert @CollectionStep


insertSteps :: Projects.ProjectId -> CollectionId -> [CollectionStep] -> DBT IO Int64
insertSteps pid cid steps = do
  let q =
        [sql| 
        INSERT INTO apis.test_steps
        (project_id, collection_id, step_data)
        VALUES (?,?,?) ON CONFLICT DO NOTHING;
      |]
  let params = map getStepParams steps
  executeMany q params
  where
    getStepParams :: CollectionStep -> (Projects.ProjectId, CollectionId, AE.Value)
    getStepParams step =
      ( pid
      , cid
      , step.stepData
      )


updateStep :: CollectionId -> CollectionStepId -> AE.Value -> DBT IO Int64
updateStep cid sid step_data = do
  let q =
        [sql| UPDATE apis.test_steps SET step_data=? WHERE collection_id = ? AND id=?  |]
  execute Update q (step_data, cid, sid)


updateCollection :: Projects.ProjectId -> Text -> Text -> Text -> DBT IO Int64
updateCollection pid cid title description = do
  let q =
        [sql| UPDATE apis.testing SET title=?, description=? WHERE project_id=? AND id=? |]
  execute Update q (title, description, pid, cid)


getCollectionById :: CollectionId -> DBT IO (Maybe Collection)
getCollectionById id' = selectById (Only id')


getCollections :: Projects.ProjectId -> DBT IO (Vector CollectionListItem)
getCollections pid = query Select q (Only pid)
  where
    q =
      [sql| SELECT t.id id , t.created_at created_at , t.updated_at updated_at, t.project_id project_id, t.last_run last_run, 
                  t.title title, t.description description, COUNT(ts.id) as steps_count, t.schedule schedule
                  FROM  apis.testing t
                  LEFT JOIN apis.test_steps ts ON t.id = ts.collection_id
                  WHERE t.project_id = ?
                  GROUP BY t.id
                  ORDER BY t.updated_at DESC;
  |]


getCollectionsId :: DBT IO (Vector CollectionId)
getCollectionsId = query Select q ()
  where
    q =
      [sql|   
        SELECT id FROM apis.testing where deleted_at IS NULL AND schedule IS NOT NULL;
    
    |]


getCollectionSteps :: CollectionId -> DBT IO (Vector CollectionStep)
getCollectionSteps cid = query Select q (Only cid)
  where
    q =
      [sql| SELECT id, created_at, updated_at, last_run, project_id, collection_id, step_data 
                  FROM apis.test_steps 
                  WHERE collection_id =? AND deleted_at IS NULL 
                |]


updateCollectionConfig :: CollectionId -> Value -> DBT IO Int64
updateCollectionConfig cid config = do
  let q =
        [sql| UPDATE apis.testing SET config=? WHERE id=? |]
  execute Update q (config, cid)


updateCollectionStep :: CollectionStepId -> Value -> DBT IO Int64
updateCollectionStep csid val = do
  let q =
        [sql| UPDATE apis.test_steps SET step_data=? WHERE id=? |]
  execute Update q (val, csid)


deleteCollectionSteps :: Vector Text -> DBT IO Int64
deleteCollectionSteps csid = do
  let q = [sql| UPDATE apis.test_steps SET deleted_at=now() WHERE id=ANY(array_remove(?, '')::uuid[]) |]
  execute Update q (Only csid)


deleteCollectionStep :: CollectionStepId -> DBT IO Int64
deleteCollectionStep csid = do
  let q = [sql| UPDATE apis.test_steps SET deleted_at=now() WHERE id=? |]
  execute Update q (Only csid)


updateSchedule :: CollectionId -> Maybe Text -> Bool -> DBT IO Int64
updateSchedule cid schedule isScheduled = do
  let q =
        [sql| UPDATE apis.testing SET schedule=?, is_scheduled=? WHERE id=? |]
  execute Update q (schedule, isScheduled, cid)


getCollectionStepById :: CollectionId -> CollectionStepId -> DBT IO (Maybe CollectionStep)
getCollectionStepById col_id step_id = queryOne Select q (col_id, step_id)
  where
    q = [sql|SELECT * FROM apis.test_step where collection_id = ? AND id = ? |]


scheduleInsertScheduleInBackgroundJobs :: [(UTCTime, Text, AE.Value)] -> DBT IO Int64
scheduleInsertScheduleInBackgroundJobs schedules = do
  let q =
        [sql| 
        INSERT INTO background_jobs
        (run_at, status, payload)
        VALUES (?,?,?);
      |]
  executeMany q schedules
