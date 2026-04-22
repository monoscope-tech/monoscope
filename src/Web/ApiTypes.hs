-- | JSON request/response types for the public REST API.
--
-- Wire format for /api/v1/{monitors,dashboards,api-keys,...}. Domain logic lives
-- in Models.* — handlers in Web.ApiHandlers adapt these types to the underlying
-- representations used by the HTML handlers.
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
  -- Generic bulk
  BulkAction (..),
  BulkFailure (..),
  BulkResult (..),
  -- Shared
  Paged (..),
  UserRef (..),
  ProjectSummary (..),
  ProjectFull (..),
  ProjectPatch (..),
  MeResponse (..),
  -- Endpoints
  EndpointSummary (..),
  EndpointFull (..),
  -- Log patterns
  LogPatternSummary (..),
  LogPatternFull (..),
  -- Issues
  IssueApiSummary (..),
  IssueApiFull (..),
  IssueStatus (..),
  -- Teams
  TeamId,
  TeamSummary (..),
  TeamFull (..),
  TeamInput (..),
  TeamPatch (..),
  -- Members
  MemberSummary (..),
  MemberAdd (..),
  MemberPatch (..),
) where

import Data.Aeson qualified as AE
import Data.Default (Default)
import Data.OpenApi (ToParamSchema, ToSchema (..))
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogPatterns qualified as LogPatterns
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.ProjectMembers qualified as PM
import Models.Projects.Projects qualified as Projects
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (JsonValueSchema (..), SnakeSchema (..), UUIDId, WrappedEnumSC (..))
import Relude
import Servant (FromHttpApiData)


-- | Phantom-tagged team id.
type TeamId = UUIDId "team"


-- | Default JSON encoding for wire types: snake_case fields, omit @Nothing@.
type SnakeJSON a = DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] a


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
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON MonitorInput
  deriving (ToSchema) via SnakeSchema MonitorInput


-- | Patch: all fields optional; only provided fields are updated.
data MonitorPatch = MonitorPatch
  { title :: Maybe Text
  , query :: Maybe Text
  , severity :: Maybe Text
  , subject :: Maybe Text
  , message :: Maybe Text
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
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON MonitorPatch
  deriving (ToSchema) via SnakeSchema MonitorPatch


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
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON DashboardSummary
  deriving (ToSchema) via SnakeSchema DashboardSummary


-- | Full dashboard view: summary + schema body/sha.
data DashboardFull = DashboardFull
  { summary :: DashboardSummary
  , fileSha :: Maybe Text
  , schema :: Maybe Dashboards.Dashboard
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON DashboardFull
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
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON DashboardInput
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
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON DashboardPatch
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
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON DashboardYAMLDoc
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
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON ApiKeySummary
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
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON EventsQuery
  deriving (ToSchema) via SnakeSchema EventsQuery


-- | Generic bulk envelope parameterised by id type (UUIDs for most resources,
-- Int64 for log patterns).
data BulkAction a = BulkAction
  { action :: Text
  , ids :: [a]
  , durationMinutes :: Maybe Int
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON (BulkAction a)


instance ToSchema a => ToSchema (BulkAction a) where
  declareNamedSchema _ = declareNamedSchema (Proxy @AE.Value)


-- | One failed id within a 'BulkResult'. Serialises as @{id, error}@ so SDK
-- consumers don't have to destructure a 2-element array.
data BulkFailure a = BulkFailure
  { id :: a
  , error :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] (BulkFailure a)


instance ToSchema a => ToSchema (BulkFailure a) where
  declareNamedSchema _ = declareNamedSchema (Proxy @AE.Value)


data BulkResult a = BulkResult
  { succeeded :: [a]
  , failed :: [BulkFailure a]
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] (BulkResult a)


instance ToSchema a => ToSchema (BulkResult a) where
  declareNamedSchema _ = declareNamedSchema (Proxy @AE.Value)


-- =============================================================================
-- Shared paginated envelope + lightweight user reference
-- =============================================================================

data Paged a = Paged
  { items :: [a]
  , totalCount :: Int
  , page :: Int
  , perPage :: Int
  , hasMore :: Bool
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON (Paged a)


instance ToSchema a => ToSchema (Paged a) where
  declareNamedSchema _ = declareNamedSchema (Proxy @AE.Value)


data UserRef = UserRef
  { id :: UUID.UUID
  , email :: Text
  , name :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON UserRef
  deriving (ToSchema) via SnakeSchema UserRef


-- =============================================================================
-- Project (singular)
-- =============================================================================

data ProjectSummary = ProjectSummary
  { id :: Projects.ProjectId
  , title :: Text
  , description :: Text
  , paymentPlan :: Text
  , timeZone :: Text
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON ProjectSummary
  deriving (ToSchema) via SnakeSchema ProjectSummary


data ProjectFull = ProjectFull
  { summary :: ProjectSummary
  , dailyNotif :: Bool
  , weeklyNotif :: Bool
  , notifyEmails :: [Text]
  , endpointAlerts :: Bool
  , errorAlerts :: Bool
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON ProjectFull
  deriving (ToSchema) via SnakeSchema ProjectFull


data ProjectPatch = ProjectPatch
  { title :: Maybe Text
  , description :: Maybe Text
  , timeZone :: Maybe Text
  , dailyNotif :: Maybe Bool
  , weeklyNotif :: Maybe Bool
  , endpointAlerts :: Maybe Bool
  , errorAlerts :: Maybe Bool
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON ProjectPatch
  deriving (ToSchema) via SnakeSchema ProjectPatch


-- =============================================================================
-- /me
-- =============================================================================

data MeResponse = MeResponse
  { projectId :: Projects.ProjectId
  , project :: ProjectSummary
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON MeResponse
  deriving (ToSchema) via SnakeSchema MeResponse


-- =============================================================================
-- Endpoints — catalog surfacing
-- =============================================================================

data EndpointSummary = EndpointSummary
  { id :: Endpoints.EndpointId
  , projectId :: Projects.ProjectId
  , urlPath :: Text
  , method :: Text
  , host :: Text
  , hash :: Text
  , outgoing :: Bool
  , description :: Text
  , serviceName :: Maybe Text
  , environment :: Maybe Text
  , totalRequests :: Maybe Int
  , lastSeen :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON EndpointSummary
  deriving (ToSchema) via SnakeSchema EndpointSummary


data EndpointFull = EndpointFull
  { summary :: EndpointSummary
  , urlParams :: AE.Value
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON EndpointFull
  deriving (ToSchema) via JsonValueSchema EndpointFull


-- =============================================================================
-- Log patterns
-- =============================================================================

data LogPatternSummary = LogPatternSummary
  { id :: Int64
  , projectId :: Projects.ProjectId
  , patternHash :: Text
  , sourceField :: Text
  , serviceName :: Maybe Text
  , logLevel :: Maybe Text
  , state :: LogPatterns.LogPatternState
  , occurrenceCount :: Int64
  , firstSeenAt :: UTCTime
  , lastSeenAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON LogPatternSummary
  deriving (ToSchema) via SnakeSchema LogPatternSummary


data LogPatternFull = LogPatternFull
  { summary :: LogPatternSummary
  , logPattern :: Text
  , sampleMessage :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON LogPatternFull
  deriving (ToSchema) via SnakeSchema LogPatternFull


-- =============================================================================
-- Issues — triage surface
-- =============================================================================

-- | Query-param filter for /issues. Derives 'FromHttpApiData' via @WrappedEnumSC@
-- so the server rejects unknown statuses rather than silently falling through.
data IssueStatus
  = ISOpen
  | ISAcknowledged
  | ISArchived
  | ISAll
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON, FromHttpApiData, ToParamSchema, ToSchema) via WrappedEnumSC "IS" IssueStatus


data IssueApiSummary = IssueApiSummary
  { id :: Issues.IssueId
  , projectId :: Projects.ProjectId
  , issueType :: Issues.IssueType
  , title :: Text
  , severity :: Text
  , critical :: Bool
  , service :: Maybe Text
  , affectedRequests :: Int
  , affectedClients :: Int
  , acknowledged :: Bool
  , archived :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON IssueApiSummary
  deriving (ToSchema) via SnakeSchema IssueApiSummary


data IssueApiFull = IssueApiFull
  { summary :: IssueApiSummary
  , recommendedAction :: Text
  , migrationComplexity :: Text
  , issueData :: AE.Value
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON IssueApiFull
  deriving (ToSchema) via JsonValueSchema IssueApiFull


-- =============================================================================
-- Teams + Members
-- =============================================================================

data TeamSummary = TeamSummary
  { id :: TeamId
  , name :: Text
  , handle :: Text
  , description :: Text
  , isEveryone :: Bool
  , memberCount :: Int
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON TeamSummary
  deriving (ToSchema) via SnakeSchema TeamSummary


data TeamFull = TeamFull
  { summary :: TeamSummary
  , members :: [UserRef]
  , notifyEmails :: [Text]
  , slackChannels :: [Text]
  , discordChannels :: [Text]
  , phoneNumbers :: [Text]
  , pagerdutyServices :: [Text]
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON TeamFull
  deriving (ToSchema) via SnakeSchema TeamFull


data TeamInput = TeamInput
  { name :: Text
  , handle :: Text
  , description :: Maybe Text
  , members :: Maybe [UUID.UUID]
  , notifyEmails :: Maybe [Text]
  , slackChannels :: Maybe [Text]
  , discordChannels :: Maybe [Text]
  , phoneNumbers :: Maybe [Text]
  , pagerdutyServices :: Maybe [Text]
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON TeamInput
  deriving (ToSchema) via SnakeSchema TeamInput


data TeamPatch = TeamPatch
  { name :: Maybe Text
  , handle :: Maybe Text
  , description :: Maybe Text
  , members :: Maybe [UUID.UUID]
  , notifyEmails :: Maybe [Text]
  , slackChannels :: Maybe [Text]
  , discordChannels :: Maybe [Text]
  , phoneNumbers :: Maybe [Text]
  , pagerdutyServices :: Maybe [Text]
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON TeamPatch
  deriving (ToSchema) via SnakeSchema TeamPatch


data MemberSummary = MemberSummary
  { id :: UUID.UUID
  , userId :: UUID.UUID
  , email :: Text
  , firstName :: Text
  , lastName :: Text
  , permission :: PM.Permissions
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON MemberSummary
  deriving (ToSchema) via SnakeSchema MemberSummary


-- | Add a project member. Exactly one of @email@ (looked up or created) or
-- @user_id@ (must refer to an existing user) must be provided. @permission@
-- defaults to @view@ when omitted.
data MemberAdd = MemberAdd
  { email :: Maybe Text
  , userId :: Maybe UUID.UUID
  , permission :: Maybe PM.Permissions
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via SnakeJSON MemberAdd
  deriving (ToSchema) via SnakeSchema MemberAdd


newtype MemberPatch = MemberPatch {permission :: PM.Permissions}
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] MemberPatch
  deriving (ToSchema) via SnakeSchema MemberPatch
