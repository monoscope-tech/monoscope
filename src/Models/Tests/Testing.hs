module Models.Tests.Testing (
  Collection (..),
  StepResult (..),
  StepRequest (..),
  AssertResult (..),
  StepResponse (..),
  CollectionId (..),
  CollectionListItem (..),
  CollectionStepId (..),
  CollectionStepData (..),
  CollectionSteps (..),
  CollectionVariablesItem (..),
  CollectionVariables (..),
  CollectionStepUpdateForm (..),
  stepDataMethod,
  updateCollection,
  addCollection,
  getCollections,
  updateCollectionLastRun,
  getCollectionById,
  getCollectionsId,
  TabStatus (..),
  updateCollectionVariables,
  getCollectionRunStatus,
  inactiveCollectionsCount,
)
where

import Data.Aeson (KeyValue ((.=)), (.:?))
import Data.Aeson qualified as AE
import Data.Aeson.Types
import Data.Default (Default)
import Data.Default.Instances ()
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity (insert)
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, query, queryOne)
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple hiding (execute, executeMany, query, query_)
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
  , json :: Maybe AE.Value
  , raw :: Maybe Text
  , requestBody :: Maybe (Map Text Text)
  , asserts :: Maybe (V.Vector (Map Text AE.Value))
  , httpVersion :: Maybe Text
  , timeout :: Maybe Int
  , followRedirects :: Maybe Bool
  , allowRedirects :: Maybe Bool
  , ignoreSSLErrors :: Maybe Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData, Default)
  deriving (FromField) via Aeson CollectionStepData
  deriving (ToField) via Aeson CollectionStepData


data CollectionStepConfig = CollectionStepConfig
  {
  }
  deriving stock (Show, Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON, NFData, Default)
  deriving (FromField) via Aeson CollectionStepConfig
  deriving (ToField) via Aeson CollectionStepConfig


stepDataMethod :: CollectionStepData -> Maybe (Text, Text)
stepDataMethod stepData =
  asum
    . map (\(method, url) -> fmap (method,) url)
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
        , fmap ("httpVersion" .=) csd.httpVersion
        , fmap ("timeout" .=) csd.timeout
        , fmap ("followRedirects" .=) csd.followRedirects
        , fmap ("allowRedirects" .=) csd.allowRedirects
        , fmap ("ignoreSSLErrors" .=) csd.ignoreSSLErrors
        , fmap ("requestBody" .=) csd.requestBody
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
    httpVersion <- v .:? "httpVersion"
    timeout <- v .:? "timeout"
    followRedirects <- v .:? "followRedirects"
    allowRedirects <- v .:? "allowRedirects"
    ignoreSSLErrors <- v .:? "ignoreSSLErrors"
    requestBody <- v .:? "requestBody"
    return CollectionStepData{..}


data CollectionVariablesItem = CollectionVariablesItem
  { variableName :: Text
  , variableValue :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON, NFData, Default)
  deriving (FromField) via Aeson CollectionVariablesItem
  deriving (ToField) via Aeson CollectionVariablesItem


newtype CollectionSteps = CollectionSteps (V.Vector CollectionStepData)
  deriving stock (Show, Generic)
  deriving anyclass (AE.ToJSON, AE.FromJSON, NFData, Default)
  deriving (FromField, ToField) via Aeson CollectionSteps


newtype CollectionVariables = CollectionVariables (V.Vector CollectionVariablesItem)
  deriving stock (Show, Generic)
  deriving anyclass (AE.ToJSON, AE.FromJSON, NFData, Default)
  deriving (FromField, ToField) via Aeson CollectionVariables


data Collection = Collection
  { id :: CollectionId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , lastRun :: Maybe UTCTime
  , projectId :: Projects.ProjectId
  , title :: Text
  , description :: Text
  , config :: AE.Value
  , schedule :: Text
  , isScheduled :: Bool
  , collectionSteps :: CollectionSteps
  , lastRunResponse :: Maybe AE.Value
  , lastRunPassed :: Int
  , lastRunFailed :: Int
  , tags :: V.Vector Text
  , collectionVariables :: CollectionVariables
  , alertSeverity :: Text
  , alertMessage :: Text
  , alertSubject :: Text
  , notifyAfter :: Text
  , notifyAfterCheck :: Bool
  , stopAfter :: Text
  , stopAfterCheck :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, AE.ToJSON, AE.FromJSON, NFData, Default)
  deriving
    (Entity)
    via (GenericEntity '[Schema "tests", TableName "collections", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Collection)


data CollectionListItem = CollectionListItem
  { id :: CollectionId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , projectId :: Projects.ProjectId
  , lastRun :: Maybe UTCTime
  , title :: Text
  , description :: Text
  , stepsCount :: Int
  , schedule :: Text
  , isScheduled :: Bool
  , passed :: Int
  , failed :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving
    (Entity)
    via (GenericEntity '[Schema "tests", TableName "collections", PrimaryKey "id", FieldModifiers '[CamelToSnake]] CollectionListItem)


data StepResponse = StepResponse
  { status :: Int
  , headers :: Maybe (Map Text [Text])
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


newtype AssertError = AssertError {advice :: Maybe Text}
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] AssertError
data AssertResult = AssertResult
  { ok :: Maybe Bool
  , err :: Maybe AssertError -- Assuming AssertError is a String for simplicity
  }
  deriving stock (Show, Generic)
  deriving (AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] AssertResult


-- Custom FromJSON instance
instance AE.FromJSON AssertResult where
  parseJSON = AE.withObject "AssertResult" $ \obj -> do
    okValue <- obj AE..:? "Ok"
    errValue <- obj AE..:? "Err"
    return
      AssertResult
        { ok = okValue
        , err = errValue
        }


data StepResult = StepResult
  { stepName :: Maybe Text
  , stepIndex :: Int
  , assertResults :: [AssertResult]
  , request :: StepRequest
  , stepLog :: Text
  , stepError :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] StepResult


data TabStatus = Active | Inactive


data CollectionStepUpdateForm = CollectionStepUpdateForm
  { stepsData :: V.Vector CollectionStepData
  , collectionId :: Maybe CollectionId
  , title :: Maybe Text
  , description :: Maybe Text
  , tags :: Maybe (V.Vector Text)
  , scheduled :: Maybe Text
  , scheduleNumber :: Maybe Text
  , scheduleNumberUnit :: Maybe Text
  , alertSeverity :: Maybe Text
  , alertMessage :: Maybe Text
  , alertSubject :: Maybe Text
  , notifyAfter :: Maybe Text
  , notifyAfterCheck :: Maybe Text
  , stopAfter :: Maybe Text
  , stopAfterCheck :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields] CollectionStepUpdateForm


getCollectionRunStatus :: V.Vector StepResult -> (Int, Int)
getCollectionRunStatus steps = (passed, failed)
  where
    passed = V.length $ V.filter (\x -> hasPassed x.assertResults) steps
    failed = V.length $ V.filter (\x -> not $ hasPassed x.assertResults) steps
    hasPassed :: [AssertResult] -> Bool
    hasPassed res = length res == length (filter (\x -> x.ok == Just True) res)


addCollection :: Collection -> DBT IO ()
addCollection = insert @Collection


updateCollectionLastRun :: CollectionId -> Maybe AE.Value -> Int -> Int -> DBT IO Int64
updateCollectionLastRun id' lastRunResponse' passed failed = execute Update q params
  where
    params = (lastRunResponse', passed, failed, id')
    q = [sql| UPDATE tests.collections SET last_run=NOW(), last_run_response=?, last_run_passed=?, last_run_failed=? WHERE id=? |]


updateCollection :: Projects.ProjectId -> CollectionId -> CollectionStepUpdateForm -> DBT IO Int64
updateCollection pid cid colF = execute Update q params
  where
    scheduled = colF.scheduled == Just "on"
    stopAfterCheck = colF.stopAfterCheck == Just "on"
    notifyAfterCheck = colF.notifyAfterCheck == Just "on"
    notifyAfter = fromMaybe "6hours" colF.notifyAfter
    stopAfter = fromMaybe "0" colF.stopAfter
    scheduleTxt = fromMaybe "" colF.scheduleNumber <> " " <> fromMaybe "" colF.scheduleNumberUnit
    title = colF.title
    description = fromMaybe "" colF.description
    scheduleInterval = scheduleTxt
    sv = colF.alertSeverity
    msg = colF.alertMessage
    sub = colF.alertSubject
    tags = colF.tags
    collectionSteps = colF.stepsData
    params =
      ( title
      , description
      , scheduleInterval
      , scheduled
      , sv
      , msg
      , sub
      , notifyAfter
      , notifyAfterCheck
      , stopAfter
      , stopAfterCheck
      , tags
      , AE.toJSON collectionSteps
      , pid
      , cid
      )
    q =
      [sql| UPDATE tests.collections SET title=?, description=?, schedule=?,
              is_scheduled=?, alert_severity=?, alert_message=?, alert_subject=?,
              notify_after=?, notify_after_check=?, stop_after=?, stop_after_check=?,
              tags=?, collection_steps=? WHERE project_id=? AND id=? |]


getCollectionById :: CollectionId -> DBT IO (Maybe Collection)
getCollectionById id' = queryOne Select q (Only id')
  where
    q =
      [sql| SELECT id, created_at, updated_at, last_run, project_id, title, description, config,
                  CASE
                      WHEN EXTRACT(DAY FROM schedule) > 0 THEN CONCAT(EXTRACT(DAY FROM schedule)::TEXT, ' days')
                      WHEN EXTRACT(HOUR FROM schedule) > 0 THEN CONCAT(EXTRACT(HOUR FROM schedule)::TEXT, ' hours')
                      ELSE CONCAT(EXTRACT(MINUTE FROM schedule)::TEXT, ' minutes')
                  END as schedule, is_scheduled, collection_steps, last_run_response, last_run_passed, last_run_failed, tags,
                  collection_variables, alert_severity, alert_message, alert_subject, notify_after,
                  notify_after_check, stop_after, stop_after_check
                  FROM tests.collections t WHERE id=?|]


getCollections :: Projects.ProjectId -> TabStatus -> DBT IO (V.Vector CollectionListItem)
getCollections pid tabStatus = query Select q (pid, statusValue)
  where
    statusValue = case tabStatus of
      Active -> True
      Inactive -> False

    q =
      [sql|
           SELECT t.id, t.created_at, t.updated_at, t.project_id, t.last_run,
                  t.title, t.description, jsonb_array_length(t.collection_steps),
                  CASE
                    WHEN EXTRACT(DAY FROM t.schedule) > 0 THEN CONCAT(EXTRACT(DAY FROM t.schedule)::TEXT, ' days')
                    WHEN EXTRACT(HOUR FROM t.schedule) > 0 THEN CONCAT(EXTRACT(HOUR FROM t.schedule)::TEXT, ' hours')
                    ELSE CONCAT(EXTRACT(MINUTE FROM t.schedule)::TEXT, ' minutes')
                  END as schedule,
                  t.is_scheduled,
                  t.last_run_passed as passed,
                  t.last_run_failed as failed
           FROM tests.collections t
           WHERE t.project_id = ? AND t.is_scheduled = ?
           ORDER BY t.updated_at DESC;
    |]


updateCollectionVariables :: Projects.ProjectId -> CollectionId -> CollectionVariables -> DBT IO Int64
updateCollectionVariables pid cid variables = execute Update q params
  where
    params = (variables, pid, cid)
    q = [sql| UPDATE tests.collections SET collection_variables = ? WHERE project_id = ? AND id = ? |]


inactiveCollectionsCount :: Projects.ProjectId -> DBT IO Int
inactiveCollectionsCount pid = do
  result <- query Select q pid
  case result of
    [Only countt] -> return countt
    v -> return $ length v
  where
    q =
      [sql|SELECT COUNT(*)
           FROM tests.collections
           WHERE project_id = ? and is_scheduled = false;
      |]


getCollectionsId :: DBT IO (V.Vector CollectionId)
getCollectionsId = query Select q ()
  where
    q =
      [sql|SELECT id FROM tests.collections where deleted_at IS NULL AND schedule IS NOT NULL;|]
