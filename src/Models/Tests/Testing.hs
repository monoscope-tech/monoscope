{-# LANGUAGE DuplicateRecordFields #-}

module Models.Tests.Testing (
  Collection (..),
  StepResult (..),
  StepRequest (..),
  StepResponse (..),
  CollectionId (..),
  CollectionListItem (..),
  CollectionStepId (..),
  CollectionStep (..),
  CollectionStepData (..),
  CollectionSteps (..),
  stepDataMethod,
  addCollection,
  updateCollectionSteps,
  getCollections,
  updateCollection,
  updateCollectionConfig,
  scheduleInsertScheduleInBackgroundJobs,
  getCollectionById,
  updateSchedule,
  getCollectionsId,
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
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, query)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple hiding (execute, executeMany, query)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Transact (DBT, executeMany)
import Deriving.Aeson qualified as DAE
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
  , exports :: Maybe (Map Text Text)
  , json :: Maybe (Value)
  , raw :: Maybe (Text)
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
      $ catMaybes
        [ Just $ "title" .= csd.title
        , fmap ("POST" .=) csd.post -- Change the key to "POST" here for the output JSON
        , fmap ("GET" .=) csd.get
        , fmap ("UPDATE" .=) csd.update
        , fmap ("DELETE" .=) csd.delete
        , fmap ("PATCH" .=) csd.patch
        , fmap ("PUT" .=) csd.put
        , fmap ("params" .=) csd.params
        , fmap ("headers" .=) csd.headers
        , fmap ("exports" .=) csd.exports
        , fmap ("json" .=) csd.json
        , fmap ("raw" .=) csd.raw
        , fmap ("asserts" .=) csd.asserts
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
    exports <- v .:? "exports"
    json <- v .:? "json"
    raw <- v .:? "raw"
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


newtype CollectionSteps = CollectionSteps (V.Vector CollectionStepData)
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


data StepResponse = StepResponse
  { status :: Int
  , headers :: Map Text [Text]
  , raw :: Text
  , json :: Value
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] StepResponse


data StepRequest = StepRequest
  { req :: CollectionStepData
  , resp :: StepResponse
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] StepRequest


data AssertResult = AssertResult
  {}
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] AssertResult


data StepResult = StepResult
  { stepName :: Text
  , stepIndex :: Int
  , assertResults :: [AssertResult]
  , request :: StepRequest
  , stepLog :: Text
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] StepResult


addCollection :: Collection -> DBT IO ()
addCollection = insert @Collection


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


updateCollectionConfig :: CollectionId -> Value -> DBT IO Int64
updateCollectionConfig cid config = do
  let q =
        [sql| UPDATE tests.collections SET config=? WHERE id=? |]
  execute Update q (config, cid)


updateCollectionSteps :: CollectionId -> V.Vector CollectionStepData -> DBT IO Int64
updateCollectionSteps cid steps = do
  let q =
        [sql| UPDATE tests.collections SET collection_steps=? WHERE id=? |]
  execute Update q (CollectionSteps steps, cid)


updateSchedule :: CollectionId -> Maybe Text -> Bool -> DBT IO Int64
updateSchedule cid schedule isScheduled = do
  let q =
        [sql| UPDATE tests.collections SET schedule=?, is_scheduled=? WHERE id=? |]
  execute Update q (schedule, isScheduled, cid)


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
