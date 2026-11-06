-- | JSON request/response types for the public REST API.
--
-- Wire format for /api/v1/{monitors,dashboards,api-keys,...}. Most handlers
-- live in Web.ApiHandlers; Pages.Dashboards renders dashboard data.
module Web.ApiTypes (
  ApiActor (..),
  principalUser,
  ApiProblem (..),
  ApiProblemBody (..),
  ProblemCode (..),
  problem,
  ApiError (..),
  Resource (..),
  refusal,
  refuse,
  -- Monitor types
  MonitorInput (..),
  MonitorPatch (..),
  -- Dashboard types
  DashboardInput (..),
  DashboardPatch (..),
  DashboardSummary (..),
  DashboardFull (..),
  DashboardYAMLDoc (..),
  DashboardData (..),
  RenderedWidget (..),
  WidgetPosition (..),
  -- API key types
  ApiKeyCreate (..),
  ApiKeyCreated (..),
  ApiKeySummary (..),
  -- Events query (body variant)
  EventsQuery (..),
  LogResult (..),
  TraceTreeEntry (..),
  -- Generic bulk
  BulkAction (..),
  BulkFailure (..),
  BulkResult (..),
  -- Shared
  Paged (..),
  UserRef (..),
  ProjectSummary (..),
  ProjectFull (..),
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
  IssueApiActivity (..),
  IssueStatus (..),
  -- Incidents
  IncidentSummary (..),
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
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as Map
import Data.OpenApi (ToParamSchema, ToSchema)
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAE
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Incidents qualified as Incidents
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogPatterns qualified as LogPatterns
import Models.Apis.MonitorDefinitions (MonitorInput (..))
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.ProjectMembers (TeamId)
import Models.Projects.ProjectMembers qualified as PM
import Models.Projects.Projects qualified as Projects
import Pages.Charts.Types qualified as Charts
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (CamelSchema (..), JsonValueSchema (..), SnakeJSON (..), SnakeSchema (..), WrappedEnumSC (..))
import Relude
import Servant (FromHttpApiData, ServerError (..), err400, err401, err403, err404)
import Web.Wire (ApiProblem (..), ApiProblemBody (..), Paged (..), ProblemCode (..))


data ApiActor = ApiKeyPrincipal | MemberPrincipal Projects.UserId
  deriving stock (Eq, Show)


principalUser :: ApiActor -> Maybe Projects.UserId
principalUser ApiKeyPrincipal = Nothing
principalUser (MemberPrincipal userId) = Just userId


-- | The one error body JSON endpoints refuse with. @code@ is the stable snake_case
-- identifier clients branch on; @details@ carries raw backend text for debugging.
problem :: ServerError -> ApiProblem -> ServerError
problem status p = status{errBody = AE.encode (ApiProblemBody p), errHeaders = [("Content-Type", "application/json")]}


data Resource = RMonitor | RDashboard | RApiKey | REvent | RProject | REndpoint | RLogPattern | RIncident | RIssue | RTeam | RMember | RUser
  deriving stock (Eq, Show)


-- | Every refusal @/api/v1@ answers with; 'refusal' fixes each one's status, 'ProblemCode' and message.
data ApiError
  = NotFound Resource
  | TabNotFound Text
  | NoRoute
  | NoActiveIngestionKey
  | UserNotCreated
  | UnknownBulkAction Text
  | NonPositiveTimeWindow
  | Required Text
  | EveryoneTeamNotUpdatable
  | EveryoneTeamNotDeletable
  | ReservedHandle
  | HandleInUse Text
  | MemberIdentityMissing
  | AuthenticationRequired
  | MissingAuthorization
  | InvalidCredentials
  | ProjectInactive
  | InvalidRequest Text
  deriving stock (Eq, Show)


refusal :: ApiError -> ServerError
refusal e = problem status ApiProblem{code = c, message = msg, field = fieldM, suggestion = Nothing, details = Nothing}
  where
    (status, c, msg, fieldM) = case e of
      NotFound r -> (err404, PNotFound, resource r <> " not found", Nothing)
      TabNotFound t -> (err404, PNotFound, "Tab not found: " <> t, Nothing)
      NoRoute -> (err404, PNotFound, "No route matches this path", Nothing)
      NoActiveIngestionKey -> (err404, PNoActiveIngestionKey, "No active ingestion key; create one in project settings", Nothing)
      UserNotCreated -> (err404, PUserNotCreated, "Could not create user", Nothing)
      UnknownBulkAction a -> (err400, PUnknownBulkAction, "Unknown bulk action: " <> a, Just "action")
      NonPositiveTimeWindow -> (err400, PNonPositiveTimeWindow, "time_window_mins must be positive", Just "time_window_mins")
      Required f -> (err400, PRequired, f <> " is required", Just f)
      EveryoneTeamNotUpdatable -> (err400, PEveryoneTeamNotUpdatable, "The everyone team cannot be updated via this endpoint", Nothing)
      EveryoneTeamNotDeletable -> (err400, PEveryoneTeamNotDeletable, "The everyone team cannot be deleted", Nothing)
      ReservedHandle -> (err400, PReservedHandle, "Handle \"everyone\" is reserved", Just "handle")
      HandleInUse h -> (err400, PHandleInUse, "Handle \"" <> h <> "\" is already in use", Just "handle")
      MemberIdentityMissing -> (err400, PMemberIdentityMissing, "Either email or user_id is required", Nothing)
      AuthenticationRequired -> (err401, PAuthenticationRequired, "Authentication required to retrieve an ingestion key", Nothing)
      MissingAuthorization -> (err401, PUnauthorized, "Missing Authorization header", Nothing)
      InvalidCredentials -> (err401, PUnauthorized, "Invalid API key, session, or project access", Nothing)
      ProjectInactive -> (err403, PProjectInactive, "Project is deactivated or deleted", Nothing)
      InvalidRequest m -> (err400, PInvalidRequest, m, Nothing)
    resource = \case
      RMonitor -> "Monitor"
      RDashboard -> "Dashboard"
      RApiKey -> "API key"
      REvent -> "event"
      RProject -> "Project"
      REndpoint -> "Endpoint"
      RLogPattern -> "Log pattern"
      RIncident -> "Incident"
      RIssue -> "Issue"
      RTeam -> "Team"
      RMember -> "Member"
      RUser -> "User"


refuse :: Error ServerError :> es => ApiError -> Eff es a
refuse = throwError . refusal


-- | Patch: all fields optional; only provided fields are updated.
data MonitorPatch = MonitorPatch
  { unit :: Maybe Text
  , title :: Maybe Text
  , query :: Maybe Text
  , environment :: Maybe Text
  , service :: Maybe Text
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
  , teams :: Maybe [PM.TeamId]
  , visualizationType :: Maybe Text
  , alertRecoveryThreshold :: Maybe Double
  , warningRecoveryThreshold :: Maybe Double
  , active :: Maybe Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON SnakeSchema MonitorPatch


data DashboardSummary = DashboardSummary
  { id :: Dashboards.DashboardId
  , title :: Text
  , tags :: V.Vector Text
  , teams :: V.Vector PM.TeamId
  , starred :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , filePath :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON SnakeSchema DashboardSummary


-- | A dashboard with every widget's data already resolved server-side, for
-- clients that render outside the browser (the CLI's terminal dashboards, and
-- anything else that can't run the widget JS). One request instead of one
-- @/chart_data@ round trip per widget, and the constant/variable resolution and
-- query defaulting stay in one place rather than being re-implemented per client.
data DashboardData = DashboardData
  { id :: Dashboards.DashboardId
  , title :: Text
  , tab :: Maybe Text
  -- ^ Name of the tab actually rendered (Nothing for a single-tab dashboard).
  , tabs :: [Text]
  , since :: Maybe Text
  , from :: Maybe Int
  , to :: Maybe Int
  -- ^ Resolved window in epoch milliseconds, matching the x-axis of every widget.
  , widgets :: [RenderedWidget]
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON JsonValueSchema DashboardData


-- | One widget, flattened to what a non-browser renderer needs: what to draw,
-- where it sits in the 12-column grid, and the data itself.
data RenderedWidget = RenderedWidget
  { id :: Maybe Text
  , title :: Maybe Text
  , subtitle :: Maybe Text
  , wType :: Widget.WidgetType
  , unit :: Maybe Text
  , query :: Maybe Text
  , layout :: Maybe Widget.Layout
  , summarizeBy :: Maybe Widget.SummarizeBy
  , value :: Maybe Double
  -- ^ Scalar for stat widgets.
  , headers :: [Text]
  , rows :: [[Maybe Double]]
  , textRows :: [[Text]]
  -- ^ Populated for table/log widgets, where the columns aren't numeric.
  , stats :: Maybe Charts.MetricsStats
  , error :: Maybe Text
  , columns :: [Text]
  , children :: [RenderedWidget]
  -- ^ Group widgets nest a sub-grid; child layouts are relative to the parent.
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON JsonValueSchema RenderedWidget


-- | Full dashboard view: summary + schema body/sha.
data DashboardFull = DashboardFull
  { summary :: DashboardSummary
  , fileSha :: Maybe Text
  , schema :: Maybe Dashboards.Dashboard
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON JsonValueSchema DashboardFull


data DashboardInput = DashboardInput
  { title :: Text
  , tags :: Maybe [Text]
  , teams :: Maybe [PM.TeamId]
  , filePath :: Maybe Text
  , schema :: Maybe Dashboards.Dashboard
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON JsonValueSchema DashboardInput


data DashboardPatch = DashboardPatch
  { title :: Maybe Text
  , tags :: Maybe [Text]
  , teams :: Maybe [PM.TeamId]
  , filePath :: Maybe Text
  , schema :: Maybe Dashboards.Dashboard
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON JsonValueSchema DashboardPatch


-- | Upsert-by-file-path payload for `monoscope dashboards apply`.
data DashboardYAMLDoc = DashboardYAMLDoc
  { filePath :: Text
  , title :: Maybe Text
  , tags :: Maybe [Text]
  , teams :: Maybe [PM.TeamId]
  , schema :: Dashboards.Dashboard
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON JsonValueSchema DashboardYAMLDoc


-- | x/y/w/h used by widget reorder endpoint.
data WidgetPosition = WidgetPosition
  { x :: Int
  , y :: Int
  , w :: Int
  , h :: Int
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON SnakeSchema WidgetPosition


data ApiKeySummary = ApiKeySummary
  { id :: ProjectApiKeys.ProjectApiKeyId
  , title :: Text
  , keyPrefix :: Text
  , active :: Bool
  , createdAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON SnakeSchema ApiKeySummary


newtype ApiKeyCreate = ApiKeyCreate {title :: Text}
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON SnakeSchema ApiKeyCreate


-- | Encrypted+base64 API-key token — same value stored as @key_prefix@. Returned
-- once on creation (the DB never stores the token in recoverable plaintext).
data ApiKeyCreated = ApiKeyCreated
  { summary :: ApiKeySummary
  , key :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON SnakeSchema ApiKeyCreated


data EventsQuery = EventsQuery
  { query :: Maybe Text
  , since :: Maybe Text
  , from :: Maybe Text
  , to :: Maybe Text
  , source :: Maybe Text
  , environment :: Maybe Text
  -- ^ Optional deployment environment. Absent deliberately means all environments,
  -- matching the API-key (non-session) GET endpoints.
  , service :: Maybe Text
  -- ^ Optional service. Like environment, this is compiled into the generated SQL.
  , limit :: Maybe Int
  , withChildren :: Maybe Bool
  -- ^ When true, also return descendants of each matched span (the
  -- sub-tree). Default (Nothing/false) returns only rows that satisfy the
  -- predicate — no sibling/parent spans bleed in.
  , includeAttributes :: Maybe Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON SnakeSchema EventsQuery


data TraceTreeEntry = TraceTreeEntry
  { traceId :: Text
  , startTime :: Int64
  , duration :: Int64
  , traceStartTime :: Maybe Text
  , root :: Text
  , children :: Map.Map Text [Text]
  }
  deriving stock (Eq, Generic, Show)
  deriving (AE.ToJSON) via DAE.Snake TraceTreeEntry
  deriving (ToSchema) via SnakeSchema TraceTreeEntry


data LogResult = LogResult
  { logsData :: V.Vector (V.Vector AE.Value)
  , cols :: [Text]
  , colIdxMap :: HM.HashMap Text Int
  , cursor :: Maybe Text
  , nextUrl, resetLogsUrl, recentUrl :: Text
  , serviceColors :: HM.HashMap Text Text
  , queryResultCount, count :: Int
  , hasMore :: Bool
  , traces :: [TraceTreeEntry]
  , error :: Maybe Text
  -- ^ Sanitized backend-failure message. When set, the web client renders an
  -- error state (inline on first load, toast on refresh) instead of the
  -- misleading empty "no events" list. Raw detail stays in the OTEL span + log.
  }
  deriving stock (Generic)
  deriving (ToSchema) via CamelSchema LogResult
  deriving (AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields] LogResult


-- | Generic bulk envelope parameterised by id type (UUIDs for most resources,
-- Int64 for log patterns).
data BulkAction a = BulkAction
  { action :: Text
  , ids :: [a]
  , durationMinutes :: Maybe Int
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON JsonValueSchema (BulkAction a)


-- | One failed id within a 'BulkResult'. Serialises as @{id, error}@ so SDK
-- consumers don't have to destructure a 2-element array.
data BulkFailure a = BulkFailure
  { id :: a
  , error :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON JsonValueSchema (BulkFailure a)


data BulkResult a = BulkResult
  { succeeded :: [a]
  , failed :: [BulkFailure a]
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON JsonValueSchema (BulkResult a)


data UserRef = UserRef
  { id :: UUID.UUID
  , email :: Text
  , name :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON SnakeSchema UserRef


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
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON SnakeSchema ProjectSummary


data ProjectFull = ProjectFull
  { summary :: ProjectSummary
  , dailyNotif :: Bool
  , weeklyNotif :: Bool
  , notifyEmails :: [Text]
  , endpointAlerts :: Bool
  , errorAlerts :: Bool
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON SnakeSchema ProjectFull


data MeResponse = MeResponse
  { projectId :: Projects.ProjectId
  , project :: ProjectSummary
  , hostUrl :: Text
  -- ^ Where this deployment's web UI lives. Clients can't derive it: the API
  -- may sit on a different hostname, and self-hosted installs put it anywhere.
  -- @monoscope open@ builds its deep links from this.
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON SnakeSchema MeResponse


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
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON SnakeSchema EndpointSummary


data EndpointFull = EndpointFull
  { summary :: EndpointSummary
  , urlParams :: AE.Value
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON JsonValueSchema EndpointFull


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
  , isError :: Bool
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON SnakeSchema LogPatternSummary


data LogPatternFull = LogPatternFull
  { summary :: LogPatternSummary
  , logPattern :: Text
  , sampleMessage :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON SnakeSchema LogPatternFull


-- | Query-param filter for /issues. Derives 'FromHttpApiData' via @WrappedEnumSC@
-- so the server rejects unknown statuses rather than silently falling through.
data IssueStatus
  = ISOpen
  | ISAcknowledged
  | ISArchived
  | ISAll
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON, FromHttpApiData, ToParamSchema, ToSchema) via WrappedEnumSC 'Nothing "IS" IssueStatus


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
  , acknowledgedUntil :: Maybe UTCTime
  -- ^ End of the acknowledgement window; far-future when acknowledged indefinitely.
  , archived :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON SnakeSchema IssueApiSummary


data IssueApiActivity = IssueApiActivity
  { event :: Text
  , createdBy :: Maybe Projects.UserId
  , createdAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON SnakeSchema IssueApiActivity


data IssueApiFull = IssueApiFull
  { summary :: IssueApiSummary
  , recommendedAction :: Text
  , migrationComplexity :: Text
  , issueData :: AE.Value
  , activity :: [IssueApiActivity]
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON JsonValueSchema IssueApiFull


data IncidentSummary = IncidentSummary
  { id :: Incidents.EpisodeId
  , projectId :: Projects.ProjectId
  , issueId :: Maybe Issues.IssueId
  , phase :: Incidents.EpisodePhase
  , startedAt :: UTCTime
  , lastEventAt :: UTCTime
  , closedAt :: Maybe UTCTime
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON SnakeSchema IncidentSummary


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
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON SnakeSchema TeamSummary


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
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON SnakeSchema TeamFull


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
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON SnakeSchema TeamInput


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
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON SnakeSchema TeamPatch


data MemberSummary = MemberSummary
  { id :: PM.ProjectMemberId
  , userId :: Projects.UserId
  , email :: Text
  , firstName :: Text
  , lastName :: Text
  , permission :: PM.Permissions
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON SnakeSchema MemberSummary


-- | Add a project member. Exactly one of @email@ (looked up or created) or
-- @user_id@ (must refer to an existing user) must be provided. @permission@
-- defaults to @view@ when omitted.
data MemberAdd = MemberAdd
  { email :: Maybe Text
  , userId :: Maybe Projects.UserId
  , permission :: Maybe PM.Permissions
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON SnakeSchema MemberAdd


newtype MemberPatch = MemberPatch {permission :: PM.Permissions}
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON, ToSchema) via SnakeJSON SnakeSchema MemberPatch
