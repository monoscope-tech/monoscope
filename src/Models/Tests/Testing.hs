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
  updateCollection,
  addCollection,
  getCollections,
  getCollectionById,
  getCollectionsId,
)
where

import Data.Aeson (KeyValue ((.=)), (.:?))
import Data.Aeson qualified as AE
import Data.Default (Default)
import Data.Default.Instances ()
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity (insert)
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, query, queryOne)
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple hiding (execute, executeMany, query)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Transact (DBT)
import Deriving.Aeson qualified as DAE
import GHC.Records (HasField (getField))
import Models.Projects.Projects qualified as Projects
import Relude hiding (get, put)
import Web.HttpApiData (FromHttpApiData)


newtype CollectionId = CollectionId {collectionId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving
    (Eq, Ord, AE.ToJSON, AE.FromJSON, FromField, ToField, FromHttpApiData, Default, NFData)
    via UUID.UUID
  deriving anyclass (FromRow, ToRow)


instance HasField "toText" CollectionId Text where
  getField colid = UUID.toText colid.collectionId


newtype CollectionStepId = CollectionStepId {collectionStepId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving
    (Eq, Ord, AE.ToJSON, AE.FromJSON, FromField, ToField, FromHttpApiData, Default, NFData)
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
  , json :: Maybe (AE.Value)
  , raw :: Maybe (Text)
  , asserts :: Maybe (V.Vector (Map Text AE.Value))
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


instance AE.ToJSON CollectionStepData where
  toJSON csd =
    AE.object
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


instance AE.FromJSON CollectionStepData where
  parseJSON = AE.withObject "CollectionStepData" $ \v -> do
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
  deriving anyclass (FromRow, ToRow, AE.ToJSON, AE.FromJSON, NFData, Default)
  deriving
    (Entity)
    via (GenericEntity '[Schema "tests", TableName "collection_steps", PrimaryKey "id", FieldModifiers '[CamelToSnake]] CollectionStep)


newtype CollectionSteps = CollectionSteps (V.Vector CollectionStepData)
  deriving stock (Show, Generic)
  deriving anyclass (AE.ToJSON, AE.FromJSON, NFData, Default)
  deriving (FromField, ToField) via Aeson (CollectionSteps)


data Collection = Collection
  { id :: CollectionId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , lastRun :: Maybe ZonedTime
  , projectId :: Projects.ProjectId
  , title :: Text
  , description :: Text
  , config :: AE.Value
  , schedule :: Text
  , isScheduled :: Bool
  , collectionSteps :: CollectionSteps
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, AE.ToJSON, AE.FromJSON, NFData, Default)
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
  , schedule :: Text
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
  , json :: AE.Value
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


updateCollection :: Projects.ProjectId -> CollectionId -> Text -> Text -> Bool -> Text -> V.Vector CollectionStepData -> DBT IO Int64
updateCollection pid cid title description scheduled scheduleInterval collectionSteps = execute Update q params
  where
    params = (title, description, scheduleInterval, scheduled, CollectionSteps collectionSteps, pid, cid)
    q = [sql| UPDATE tests.collections SET title=?, description=?, schedule=?, is_scheduled=?,  collection_steps=? WHERE project_id=? AND id=? |]


getCollectionById :: CollectionId -> DBT IO (Maybe Collection)
getCollectionById id' = queryOne Select q (Only id')
  where
    q =
      [sql| SELECT id, created_at, updated_at, last_run, project_id, title, description, config, 
                  CASE
                      WHEN EXTRACT(DAY FROM schedule) > 0 THEN CONCAT(EXTRACT(DAY FROM schedule)::TEXT, ' days')
                      WHEN EXTRACT(HOUR FROM schedule) > 0 THEN CONCAT(EXTRACT(HOUR FROM schedule)::TEXT, ' hours')
                      ELSE CONCAT(EXTRACT(MINUTE FROM schedule)::TEXT, ' minutes')
                  END as schedule, is_scheduled, collection_steps 
                  FROM tests.collections t WHERE id=?|]


-- TODO: delete or remove the collect_steps join
getCollections :: Projects.ProjectId -> DBT IO (V.Vector CollectionListItem)
getCollections pid = query Select q (Only pid)
  where
    q =
      [sql| SELECT t.id id , t.created_at created_at , t.updated_at updated_at, t.project_id project_id, t.last_run last_run, 
                  t.title title, t.description description, COUNT(ts.id) as steps_count,CASE
                      WHEN EXTRACT(DAY FROM t.schedule) > 0 THEN CONCAT(EXTRACT(DAY FROM t.schedule)::TEXT, ' days')
                      WHEN EXTRACT(HOUR FROM t.schedule) > 0 THEN CONCAT(EXTRACT(HOUR FROM t.schedule)::TEXT, ' hours')
                      ELSE CONCAT(EXTRACT(MINUTE FROM t.schedule)::TEXT, ' minutes')
                  END as  schedule
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
      [sql|SELECT id FROM tests.collections where deleted_at IS NULL AND schedule IS NOT NULL;|]
