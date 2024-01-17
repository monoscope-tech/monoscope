{-# LANGUAGE DuplicateRecordFields #-}

module Models.Apis.Testing (
    Collection (..),
    CollectionId (..),
    CollectionListItem (..),
    addCollection,
    getCollections,
    updateCollection,
    updateCollectionSteps,
    updateCollectionConfig,
    getCollectionById,
    updateSchedule,
) where

import Data.Default (Default)
import Data.Default.Instances ()
import Data.Time (ZonedTime)

import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, query)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Records (HasField (getField))
import Models.Projects.Projects qualified as Projects
import Relude
import Web.HttpApiData (FromHttpApiData)

import Data.Aeson as Aeson
import Database.PostgreSQL.Entity (insert, selectById, update)

import Database.PostgreSQL.Simple hiding (execute, query)

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Transact (DBT)


newtype CollectionId = CollectionId {collectionId :: UUID.UUID}
    deriving stock (Generic, Show)
    deriving
        (Eq, Ord, ToJSON, FromJSON, FromField, ToField, FromHttpApiData, Default)
        via UUID.UUID


instance HasField "toText" CollectionId Text where
    getField = UUID.toText . collectionId


data Collection = Collection
    { id :: CollectionId
    , createdAt :: ZonedTime
    , updatedAt :: ZonedTime
    , lastRun :: Maybe ZonedTime
    , projectId :: Projects.ProjectId
    , title :: Text
    , description :: Text
    , steps :: Value
    , config :: Value
    , schedule :: Maybe Text
    , isScheduled :: Bool
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromRow, ToRow, ToJSON, FromJSON)
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
    deriving anyclass (FromRow, ToRow)
    deriving
        (Entity)
        via (GenericEntity '[Schema "apis", TableName "testing", PrimaryKey "id", FieldModifiers '[CamelToSnake]] CollectionListItem)


addCollection :: Collection -> DBT IO ()
addCollection = insert @Collection


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
            [sql| SELECT id, created_at, updated_at, project_id, last_run, title, description, jsonb_array_length(steps) as steps_count, schedule FROM apis.testing
    WHERE project_id = ? 
    ORDER BY created_at DESC;
  |]


updateCollectionSteps :: CollectionId -> Value -> DBT IO Int64
updateCollectionSteps cid steps = do
    let q =
            [sql| UPDATE apis.testing SET steps=? WHERE id=? |]
    execute Update q (steps, cid)


updateCollectionConfig :: CollectionId -> Value -> DBT IO Int64
updateCollectionConfig cid config = do
    let q =
            [sql| UPDATE apis.testing SET config=? WHERE id=? |]
    execute Update q (config, cid)


updateSchedule :: CollectionId -> Maybe Text -> Bool -> DBT IO Int64
updateSchedule cid schedule isScheduled = do
    let q =
            [sql| UPDATE apis.testing SET schedule=?, is_scheduled=? WHERE id=? |]
    execute Update q (schedule, isScheduled, cid)
