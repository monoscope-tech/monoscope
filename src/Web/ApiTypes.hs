-- | JSON request/response types for the public REST API (Plan A).
--
-- These types are the wire format for /api/v1/{monitors,dashboards,api-keys,...}.
-- Domain logic lives in Models.* and Pages.* — handlers in Web.ApiHandlers adapt
-- these types to the underlying representations used by the HTML handlers.
module Web.ApiTypes (
  -- Monitor types
  MonitorInput (..),
  MonitorPatch (..),
  -- Dashboard types
  DashboardInput (..),
  DashboardPatch (..),
  DashboardSummary (..),
  DashboardFull (..),
  DashboardYAMLDoc (..),
  WidgetPosition (..),
  -- API key types
  ApiKeyCreate (..),
  ApiKeyCreated (..),
  ApiKeySummary (..),
  -- Events query (body variant)
  EventsQuery (..),
  -- Generic
  BulkAction (..),
  BulkResult (..),
) where

import Data.Aeson qualified as AE
import Data.Default (Default)
import Data.OpenApi (ToSchema (..))
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (JsonValueSchema (..), SnakeSchema (..))
import Relude


-- Orphan: Widget's nested types don't have ToSchema — emit an open-value schema.
deriving via JsonValueSchema Widget.Widget instance ToSchema Widget.Widget


-- | Input for creating or replacing (PUT) a monitor. Maps onto `QueryMonitor`
-- fields without surfacing db-internal lifecycle columns.
data MonitorInput = MonitorInput
  { title :: Text
  , query :: Text
  , severity :: Maybe Text
  , subject :: Maybe Text
  , message :: Maybe Text
  , alertThreshold :: Double
  , warningThreshold :: Maybe Double
  , triggerLessThan :: Bool
  , checkIntervalMins :: Int
  , timeWindowMins :: Int
  , thresholdSustainedForMins :: Maybe Int
  , notifyAfterMins :: Maybe Int
  , stopAfterCount :: Maybe Int
  , emails :: [Text]
  , emailAll :: Maybe Bool
  , slackChannels :: [Text]
  , teams :: [UUID.UUID]
  , visualizationType :: Maybe Text
  , alertRecoveryThreshold :: Maybe Double
  , warningRecoveryThreshold :: Maybe Double
  , active :: Maybe Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] MonitorInput
  deriving (ToSchema) via SnakeSchema MonitorInput


-- | Patch: all fields optional; only provided fields are updated.
data MonitorPatch = MonitorPatch
  { title :: Maybe Text
  , query :: Maybe Text
  , alertThreshold :: Maybe Double
  , warningThreshold :: Maybe Double
  , triggerLessThan :: Maybe Bool
  , checkIntervalMins :: Maybe Int
  , timeWindowMins :: Maybe Int
  , thresholdSustainedForMins :: Maybe Int
  , notifyAfterMins :: Maybe Int
  , stopAfterCount :: Maybe Int
  , emails :: Maybe [Text]
  , emailAll :: Maybe Bool
  , slackChannels :: Maybe [Text]
  , teams :: Maybe [UUID.UUID]
  , visualizationType :: Maybe Text
  , alertRecoveryThreshold :: Maybe Double
  , warningRecoveryThreshold :: Maybe Double
  , active :: Maybe Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] MonitorPatch
  deriving (ToSchema) via SnakeSchema MonitorPatch


-- | Compact summary for list endpoints.
data DashboardSummary = DashboardSummary
  { id :: Dashboards.DashboardId
  , title :: Text
  , tags :: V.Vector Text
  , teams :: V.Vector UUID.UUID
  , starred :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , filePath :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] DashboardSummary
  deriving (ToSchema) via SnakeSchema DashboardSummary


-- | Full dashboard view including schema body.
data DashboardFull = DashboardFull
  { id :: Dashboards.DashboardId
  , title :: Text
  , tags :: V.Vector Text
  , teams :: V.Vector UUID.UUID
  , starred :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , filePath :: Maybe Text
  , fileSha :: Maybe Text
  , schema :: Maybe Dashboards.Dashboard
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] DashboardFull
  deriving (ToSchema) via JsonValueSchema DashboardFull


-- | Input for create/replace.
data DashboardInput = DashboardInput
  { title :: Text
  , tags :: Maybe [Text]
  , teams :: Maybe [UUID.UUID]
  , filePath :: Maybe Text
  , schema :: Maybe Dashboards.Dashboard
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] DashboardInput
  deriving (ToSchema) via JsonValueSchema DashboardInput


-- | PATCH — all fields optional.
data DashboardPatch = DashboardPatch
  { title :: Maybe Text
  , tags :: Maybe [Text]
  , teams :: Maybe [UUID.UUID]
  , filePath :: Maybe Text
  , schema :: Maybe Dashboards.Dashboard
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] DashboardPatch
  deriving (ToSchema) via JsonValueSchema DashboardPatch


-- | Upsert-by-file-path payload for `monoscope dashboards apply`.
data DashboardYAMLDoc = DashboardYAMLDoc
  { filePath :: Text
  , title :: Maybe Text
  , tags :: Maybe [Text]
  , teams :: Maybe [UUID.UUID]
  , schema :: Dashboards.Dashboard
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] DashboardYAMLDoc
  deriving (ToSchema) via JsonValueSchema DashboardYAMLDoc


-- | x/y/w/h used by widget reorder endpoint.
data WidgetPosition = WidgetPosition
  { x :: Int
  , y :: Int
  , w :: Int
  , h :: Int
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields] WidgetPosition
  deriving (ToSchema) via SnakeSchema WidgetPosition


data ApiKeySummary = ApiKeySummary
  { id :: ProjectApiKeys.ProjectApiKeyId
  , title :: Text
  , keyPrefix :: Text
  , active :: Bool
  , createdAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] ApiKeySummary
  deriving (ToSchema) via SnakeSchema ApiKeySummary


newtype ApiKeyCreate = ApiKeyCreate {title :: Text}
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] ApiKeyCreate
  deriving (ToSchema) via SnakeSchema ApiKeyCreate


-- | Encrypted+base64 API-key token — same value stored as @key_prefix@. Returned
-- once on creation (the DB never stores the token in recoverable plaintext).
data ApiKeyCreated = ApiKeyCreated
  { summary :: ApiKeySummary
  , key :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] ApiKeyCreated
  deriving (ToSchema) via SnakeSchema ApiKeyCreated


data EventsQuery = EventsQuery
  { query :: Maybe Text
  , since :: Maybe Text
  , from :: Maybe Text
  , to :: Maybe Text
  , source :: Maybe Text
  , limit :: Maybe Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] EventsQuery
  deriving (ToSchema) via SnakeSchema EventsQuery


data BulkAction = BulkAction
  { action :: Text
  , ids :: [UUID.UUID]
  , durationMinutes :: Maybe Int
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] BulkAction
  deriving (ToSchema) via SnakeSchema BulkAction


data BulkResult = BulkResult
  { succeeded :: [UUID.UUID]
  , failed :: [(UUID.UUID, Text)]
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] BulkResult
  deriving (ToSchema) via SnakeSchema BulkResult
