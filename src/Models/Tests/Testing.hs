{-# LANGUAGE DuplicateRecordFields #-}

module Models.Tests.Testing (
  Collection (..),
  CollectionId (..),
  CollectionListItem (..),
  CollectionStepId (..),
  CollectionStep (..),
  CollectionStepData (..),
  CollectionSteps (..),
  stepDataMethod,
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
  deleteSchedulesFromBackgroundJobs,
)
where

import Data.Aeson as Aeson
import Data.Aeson qualified as AE
import Data.Default (Default)
import Data.Default.Instances ()
import Data.Time (UTCTime, ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity (insert, selectById)
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, query, queryOne)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple hiding (execute, executeMany, query)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Transact (DBT, executeMany)
import GHC.Records (HasField (getField))
import Models.Projects.Projects qualified as Projects
import Relude hiding (get, put)
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


data CollectionStepData = CollectionStepData
  { title :: Maybe Text
  , post :: Maybe Text
  , get :: Maybe Text
  , update :: Maybe Text
  , delete :: Maybe Text
  , patch :: Maybe Text
  , put :: Maybe Text
  , params :: Maybe (Map Text Text)
  , headers :: Maybe (Map Text Text)
  , json :: Maybe (Value)
  , asserts :: Maybe (V.Vector (Map Text Value))
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData, Default)
  deriving (FromField) via Aeson CollectionStepData
  deriving (ToField) via Aeson CollectionStepData


stepDataMethod :: CollectionStepData -> Maybe (Text, Text)
stepDataMethod stepData =
  asum
    . map (\(method, url) -> fmap (\u -> (method, u)) url)
    $ [ ("GET", stepData.get)
      , ("POST", stepData.post)
      , ("UPDATE", stepData.update)
      , ("DELETE", stepData.delete)
      , ("PATCH", stepData.patch)
      , ("PUT", stepData.put)
      ]


instance ToJSON CollectionStepData where
  toJSON csd =
    object
      $ [ "title" .= csd.title
        , "POST" .= csd.post -- Change the key to "POST" here for the output JSON
        , "GET" .= csd.get
        , "UPDATE" .= csd.update
        , "DELETE" .= csd.delete
        , "PATCH" .= csd.patch
        , "PUT" .= csd.put
        , "params" .= csd.params
        , "headers" .= csd.headers
        , "json" .= csd.json
        , "asserts" .= csd.asserts
        ]


instance FromJSON CollectionStepData where
  parseJSON = withObject "CollectionStepData" $ \v -> do
    title <- v .:? "title"
    get <- v .:? "GET"
    post <- v .:? "POST" -- Map from "POST" back to the `post` field
    update <- v .:? "UPDATE"
    delete <- v .:? "DELETE"
    patch <- v .:? "PATCH"
    put <- v .:? "PUT"
    params <- v .:? "params"
    headers <- v .:? "headers"
    json <- v .:? "json"
    asserts <- v .:? "asserts"
    return CollectionStepData{..}


-- TODO: delete table
data CollectionStep = CollectionStep
  { id :: CollectionStepId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , lastRun :: Maybe ZonedTime
  , projectId :: Projects.ProjectId
  , collectionId :: CollectionId
  , stepData :: CollectionStepData
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, ToJSON, FromJSON, NFData, Default)
  deriving
    (Entity)
    via (GenericEntity '[Schema "tests", TableName "collection_steps", PrimaryKey "id", FieldModifiers '[CamelToSnake]] CollectionStep)

newtype CollectionSteps = CollectionSteps  (V.Vector CollectionStepData)
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData, Default)
  deriving (FromField, ToField) via Aeson (CollectionSteps)


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
  , collectionSteps :: CollectionSteps
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, ToJSON, FromJSON, NFData, Default)
  deriving
    (Entity)
    via (GenericEntity '[Schema "tests", TableName "collections", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Collection)


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
    via (GenericEntity '[Schema "tests", TableName "collections", PrimaryKey "id", FieldModifiers '[CamelToSnake]] CollectionListItem)


addCollection :: Collection -> DBT IO ()
addCollection = insert @Collection


addCollectionStep :: CollectionStep -> DBT IO ()
addCollectionStep = insert @CollectionStep


insertSteps :: Projects.ProjectId -> CollectionId -> [CollectionStep] -> DBT IO Int64
insertSteps pid cid steps = do
  let q =
        [sql| 
        INSERT INTO tests.collection_steps
        (project_id, collection_id, step_data)
        VALUES (?,?,?) ON CONFLICT DO NOTHING;
      |]
  let params = map getStepParams steps
  executeMany q params
  where
    getStepParams :: CollectionStep -> (Projects.ProjectId, CollectionId, CollectionStepData)
    getStepParams step =
      ( pid
      , cid
      , step.stepData
      )


updateCollection :: Projects.ProjectId -> Text -> Text -> Text -> DBT IO Int64
updateCollection pid cid title description = do
  let q =
        [sql| UPDATE tests.collections SET title=?, description=? WHERE project_id=? AND id=? |]
  execute Update q (title, description, pid, cid)


getCollectionById :: CollectionId -> DBT IO (Maybe Collection)
getCollectionById id' = selectById (Only id')


getCollections :: Projects.ProjectId -> DBT IO (V.Vector CollectionListItem)
getCollections pid = query Select q (Only pid)
  where
    q =
      [sql| SELECT t.id id , t.created_at created_at , t.updated_at updated_at, t.project_id project_id, t.last_run last_run, 
                  t.title title, t.description description, COUNT(ts.id) as steps_count, t.schedule schedule
                  FROM  tests.collections t
                  LEFT JOIN tests.collection_steps ts ON t.id = ts.collection_id
                  WHERE t.project_id = ?
                  GROUP BY t.id
                  ORDER BY t.updated_at DESC;
  |]


getCollectionsId :: DBT IO (V.Vector CollectionId)
getCollectionsId = query Select q ()
  where
    q =
      [sql|   
        SELECT id FROM tests.collections where deleted_at IS NULL AND schedule IS NOT NULL;
    
    |]


getCollectionSteps :: CollectionId -> DBT IO (V.Vector CollectionStep)
getCollectionSteps cid = query Select q (Only cid)
  where
    q =
      [sql| SELECT id, created_at, updated_at, last_run, project_id, collection_id, step_data 
                  FROM tests.collection_steps 
                  WHERE collection_id =? AND deleted_at IS NULL 
                |]


updateCollectionConfig :: CollectionId -> Value -> DBT IO Int64
updateCollectionConfig cid config = do
  let q =
        [sql| UPDATE tests.collections SET config=? WHERE id=? |]
  execute Update q (config, cid)


updateCollectionStep :: CollectionStepId -> CollectionStepData -> DBT IO Int64
updateCollectionStep csid val = do
  let q =
        [sql| UPDATE tests.collection_steps SET step_data=? WHERE id=? |]
  execute Update q (val, csid)


deleteCollectionSteps :: V.Vector Text -> DBT IO Int64
deleteCollectionSteps csid = do
  let q = [sql| UPDATE tests.collection_steps SET deleted_at=now() WHERE id=ANY(array_remove(?, '')::uuid[]) |]
  execute Update q (Only csid)


deleteCollectionStep :: CollectionStepId -> DBT IO Int64
deleteCollectionStep csid = do
  let q = [sql| UPDATE tests.collection_steps SET deleted_at=now() WHERE id=? |]
  execute Update q (Only csid)


updateSchedule :: CollectionId -> Maybe Text -> Bool -> DBT IO Int64
updateSchedule cid schedule isScheduled = do
  let q =
        [sql| UPDATE tests.collections SET schedule=?, is_scheduled=? WHERE id=? |]
  execute Update q (schedule, isScheduled, cid)


getCollectionStepById :: CollectionId -> CollectionStepId -> DBT IO (Maybe CollectionStep)
getCollectionStepById col_id step_id = queryOne Select q (col_id, step_id)
  where
    q =
      [sql|SELECT id, created_at, updated_at, last_run, project_id, collection_id, step_data 
               FROM tests.collection_steps where collection_id = ? AND id = ? |]


scheduleInsertScheduleInBackgroundJobs :: [(UTCTime, Text, AE.Value)] -> DBT IO Int64
scheduleInsertScheduleInBackgroundJobs schedules = do
  let q =
        [sql| 
        INSERT INTO background_jobs
        (run_at, status, payload)
        VALUES (?,?,?);
      |]
  executeMany q schedules


deleteSchedulesFromBackgroundJobs :: CollectionId -> DBT IO Int64
deleteSchedulesFromBackgroundJobs cid = do
  let q = [sql| DELETE FROM background_jobs where payload->>'tag' = 'RunCollectionTests' and payload->>'contents' = ? |]
  execute Delete q cid
