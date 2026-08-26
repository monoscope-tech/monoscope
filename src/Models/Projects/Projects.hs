module Models.Projects.Projects (
  -- Users
  User (..),
  UserId (..),
  createUser,
  userIdByEmail,
  createUserId,
  insertUser,
  userById,
  userByEmail,
  createEmptyUser,
  -- Projects
  Project (..),
  ProjectListItem (..),
  ProjectId,
  CreateProject (..),
  ProjectS3Bucket (..),
  insertProject,
  projectIdFromText,
  usersByProjectId,
  usersByIds,
  selectProjectsForUser,
  getProjectByPhoneNumber,
  activeProjects,
  activeNonOnboardingProjectIds,
  recentlyActiveProjectIds,
  newProjectsSince,
  updateProject,
  patchProjectSettings,
  ProjectPatch (..),
  deleteProject,
  updateProjectPricing,
  updateProjectBilling,
  projectById,
  activeProjectById,
  projectByOrderId,
  projectByCustomerId,
  UserBilling (..),
  userBilling,
  projectBySubId,
  updateSubItemIdBySubId,
  projectCacheById,
  projectCacheByIdIO,
  updateProjectReportNotif,
  ProjectCache (..),
  defaultProjectCache,
  updateProjectS3Bucket,
  QueryLibItemId,
  QueryLibType (..),
  QueryLibItem (..),
  queryLibHistoryForUser,
  queryLibInsert,
  queryLibTitleEdit,
  queryLibItemDelete,
  -- Billing
  BillingProvider (..),
  billingProviderFromSubId,
  projectProvider,
  Plan (..),
  parsePlan,
  isPaidPlan,
  isFreeTier,
  isOnboarding,
  LemonSub (..),
  LemonSubId (..),
  addSubscription,
  getTotalUsage,
  getDailyUsageBreakdown,
  -- Usage report submissions (chunked provider submissions)
  UsageSubmission (..),
  ChunkQuantity,
  mkChunkQuantity,
  chunkQuantityInt,
  UsageChunk (..),
  splitUsageIntoChunks,
  pendingUsageSubmissions,
  recordUsageWindow,
  UsageTotals (..),
  -- Metered billing dimensions
  MeterKind (..),
  meterQuantity,
  stripeMeterEventName,
  MeterTarget (..),
  DormantReason (..),
  ProjectMeterConfig (..),
  projectMeterConfig,
  resolveMeterTarget,
  meterIsDormant,
  meterSubItemIds,
  setMeterSubItemId,
  markUsageSubmissionSucceeded,
  markUsageSubmissionFailed,
  upgradeToPaid,
  downgradeToFree,
  downgradeToFreeBySubId,
  setPlanBySubId,
  updateStripeProjectBilling,
  -- Sessions
  PersistentSessionId (..),
  PersistentSession (..),
  Session (..),
  sessionAndProject,
  craftSessionCookie,
  SessionData (..),
  PSUser (..),
  PSProjects (..),
  addCookie,
  emptySessionCookie,
  getSession,
  insertSession,
  getPersistentSession,
  newPersistentSessionId,
  -- Audit Log
  AuditEvent (..),
  logAudit,
  logAuditS,
)
where

import Data.Aeson qualified as AE
import Data.CaseInsensitive qualified as CI
import Data.Char (isDigit)
import Data.Default
import Data.Effectful.Hasql qualified as EHasql
import Data.Effectful.UUID (UUIDEff, genUUID)
import Data.Map.Strict qualified as Map
import Data.OpenApi (ToSchema)
import Data.Text qualified as T
import Data.Text.Display
import Data.Time (Day, UTCTime, ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes
import Database.PostgreSQL.Simple.ToField (ToField)
import Deriving.Aeson.Stock qualified as DAE
import Effectful
import Effectful.Error.Static qualified as EffError
import Effectful.Reader.Static qualified as EffReader
import Effectful.Time (Time, currentTime, runTime)
import GHC.Records (HasField (getField))
import Hasql.Interpolate qualified as HI
import Hasql.Statement (Statement)
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as TxS
import OpenTelemetry.Instrumentation.Hasql qualified as OHasql
import Pkg.DeriveUtils (DB, SnakeSchema (..), UUIDId (..), WrappedEnumSC (..), idFromText, selectFrom)
import Pkg.Parser.Stats (Section)
import Relude
import Servant (FromHttpApiData, Header, Headers, ServerError, addHeader, err302, errHeaders, getResponse)
import System.Envy (Var)
import Web.Cookie (SetCookie (setCookieHttpOnly, setCookieMaxAge, setCookieName, setCookiePath, setCookieSameSite, setCookieSecure, setCookieValue), defaultSetCookie, sameSiteLax)
import Web.FormUrlEncoded (FromForm)
import Web.HttpApiData (ToHttpApiData)
import Web.I18n qualified


instance AE.FromJSON (CI.CI Text) where
  parseJSON = fmap CI.mk . AE.parseJSON


instance AE.ToJSON (CI.CI Text) where
  toJSON = AE.toJSON . CI.original


newtype UserId = UserId {getUserId :: UUID.UUID}
  deriving stock (Eq, Generic, Show)
  deriving newtype (NFData)
  deriving anyclass (FromRow, HI.DecodeRow, ToRow)
  deriving
    (AE.FromJSON, AE.ToJSON, Default, FromField, FromHttpApiData, HI.DecodeValue, HI.EncodeValue, Ord, ToField)
    via UUID.UUID


instance HasField "toText" UserId Text where
  getField u = UUID.toText u.getUserId


data User = User
  { id :: UserId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , deletedAt :: Maybe UTCTime
  , active :: Bool
  , firstName :: Text
  , lastName :: Text
  , displayImageUrl :: Text
  , email :: CI.CI Text
  , isSudo :: Bool
  , phoneNumber :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, HI.DecodeRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "users", TableName "users", PrimaryKey "id", FieldModifiers '[CamelToSnake]] User)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake User


createUserId :: UUIDEff :> es => Eff es UserId
createUserId = UserId <$> genUUID


createUser :: (Time :> es, UUIDEff :> es) => Text -> Text -> Text -> Text -> Eff es User
createUser firstName lastName picture email = do
  uid <- createUserId
  now <- currentTime
  pure
    $ User
      { id = uid
      , createdAt = now
      , updatedAt = now
      , deletedAt = Nothing
      , active = True
      , firstName = firstName
      , lastName = lastName
      , displayImageUrl = picture
      , email = CI.mk email
      , phoneNumber = Nothing
      , isSudo = False
      }


insertUser :: DB es => User -> Eff es ()
insertUser u = EHasql.interpExecute_ [HI.sql| INSERT INTO users.users (id, created_at, updated_at, deleted_at, active, first_name, last_name, display_image_url, email, phone_number, is_sudo) VALUES (#{u.id}, #{u.createdAt}, #{u.updatedAt}, #{u.deletedAt}, #{u.active}, #{u.firstName}, #{u.lastName}, #{u.displayImageUrl}, #{u.email}, #{u.phoneNumber}, #{u.isSudo}) |]


userById :: DB es => UserId -> Eff es (Maybe User)
userById uid = EHasql.interpOne (selectFrom @User <> [HI.sql| WHERE id = #{uid} |])


userByEmail :: DB es => Text -> Eff es (Maybe User)
userByEmail email = EHasql.interpOne (selectFrom @User <> [HI.sql| WHERE email = #{email} |])


userIdByEmail :: DB es => Text -> Eff es (Maybe UserId)
userIdByEmail email = EHasql.interpOne [HI.sql|select id from users.users where email=#{email}|]


createEmptyUser :: DB es => Text -> Eff es (Maybe UserId)
createEmptyUser email = EHasql.interpOne [HI.sql| insert into users.users (email, active) values (#{email}, TRUE) on conflict do nothing returning id |]


---------------------------------
type ProjectId = UUIDId "project"


projectIdFromText :: Text -> Maybe ProjectId
projectIdFromText = idFromText


data Project = Project
  { id :: ProjectId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , deletedAt :: Maybe UTCTime
  , active :: Bool
  , title :: Text
  , description :: Text
  , -- NOTE: We used to have hosts under project, but now hosts should be gotten from the endpoints.
    -- NOTE: If there's heavy need and usage, we caould create a view. Otherwise, the project cache is best, if it meets our needs.
    paymentPlan :: Text
  , questions :: Maybe AE.Value
  , dailyNotif :: Bool
  , weeklyNotif :: Bool
  , timeZone :: Text
  , subId :: Maybe Text
  , firstSubItemId :: Maybe Text
  , orderId :: Maybe Text
  , usageLastReported :: UTCTime
  , billingDay :: Maybe UTCTime
  , onboardingStepsCompleted :: V.Vector Text
  , s3Bucket :: Maybe ProjectS3Bucket
  , endpointAlerts :: Bool
  , errorAlerts :: Bool
  , customerId :: Maybe Text
  , -- Positional decode: must stay LAST to match the trailing @billing_provider@ column added in migration 0106.
    billingProvider :: BillingProvider
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, NFData)
  deriving
    (Entity)
    via (GenericEntity '[Schema "projects", TableName "projects", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Project)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake Project


-- | Project-list projection: every 'Project' column PLUS two columns computed by the join
-- in 'selectProjectsForUser' (@has_integrated@, @users_display_images@). It is NOT a
-- duplicate of 'Project' to merge away — those columns are only available via that aggregate
-- query, so folding them into 'Project' would force every plain project lookup to carry the
-- joins. Kept as a dedicated projection for the project-list page.
data ProjectListItem = ProjectListItem
  { id :: ProjectId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , deletedAt :: Maybe UTCTime
  , active :: Bool
  , title :: Text
  , description :: Text
  , paymentPlan :: Text
  , questions :: Maybe AE.Value
  , dailyNotif :: Bool
  , weeklyNotif :: Bool
  , timeZone :: Text
  , subId :: Maybe Text
  , firstSubItemId :: Maybe Text
  , orderId :: Maybe Text
  , usageLastReported :: UTCTime
  , billingDay :: Maybe UTCTime
  , onboardingStepsCompleted :: V.Vector Text
  , s3Bucket :: Maybe ProjectS3Bucket
  , endpointAlerts :: Bool
  , errorAlerts :: Bool
  , customerId :: Maybe Text
  , -- Positional: matches @pp.*@ (billing_provider is the last projects column), before the joined columns below.
    billingProvider :: BillingProvider
  , hasIntegrated :: Bool
  , usersDisplayImages :: V.Vector Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, HI.DecodeRow, NFData)


data ProjectS3Bucket = ProjectS3Bucket
  { accessKey :: Text
  , secretKey :: Text
  , region :: Text
  , bucket :: Text
  , endpointUrl :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, FromForm, NFData)
  deriving (FromField, HI.DecodeValue, HI.EncodeValue, ToField) via Aeson ProjectS3Bucket


data ProjectCache = ProjectCache
  { -- We need this hosts to mirror all the hosts in the endpoints table, and could use this for validation purposes to skip inserting endpoints just because of hosts
    -- if endpoint exists but host is not in this list, then we have a query specifically for inserting hosts.
    hosts :: V.Vector Text
  , -- maybe we don't need this? See the next point.
    endpointHashes :: V.Vector Text
  , -- Daily count of events from otel_logs_and_spans table for the last 24 hours
    dailyEventCount :: Int
  , -- Daily count of metrics for the last 24 hours
    dailyMetricCount :: Int
  , paymentPlan :: Text
  , -- Canonical URL path templates for matching at ingestion: "method|host|template_path"
    canonicalPaths :: V.Vector Text
  , -- Literal prefixes this project's ids are known to start with, promoted
    -- from confirmed LLM verdicts once proven not to collide with any segment
    -- the project routes on. Lets ingest recognise the format on a path the
    -- learner has never seen, with no population to wait for.
    idRulePrefixes :: V.Vector Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, HI.DecodeRow, NFData)


defaultProjectCache :: ProjectCache
defaultProjectCache = def{paymentPlan = "Free"}


data CreateProject = CreateProject
  { id :: ProjectId
  , title :: Text
  , description :: Text
  , paymentPlan :: Text
  , timeZone :: Text
  , subId :: Maybe Text
  , firstSubItemId :: Maybe Text
  , orderId :: Maybe Text
  , dailyNotif :: Bool
  , weeklyNotif :: Bool
  , endpointAlerts :: Bool
  , errorAlerts :: Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "projects", TableName "projects", PrimaryKey "id", FieldModifiers '[CamelToSnake]] CreateProject)


-- FIXME: We currently return an object with empty vectors when nothing was found.
projectCacheById :: (DB es, Time :> es) => ProjectId -> Eff es (Maybe ProjectCache)
projectCacheById pid = do
  now <- currentTime
  EHasql.interpOne
    [HI.sql|
    select  coalesce(ARRAY_AGG(DISTINCT hosts ORDER BY hosts ASC),'{}') hosts,
            coalesce(ARRAY_AGG(DISTINCT endpoint_hashes ORDER BY endpoint_hashes ASC),'{}') endpoint_hashes,
            ( SELECT count(*)::bigint FROM otel_logs_and_spans
             WHERE project_id=#{pid.toText} AND timestamp > #{now}::timestamptz - INTERVAL '1' DAY
            ) daily_event_count,
            ( SELECT count(*)::bigint FROM otel_metrics
             WHERE project_id=#{pid.toText} AND timestamp > #{now}::timestamptz - INTERVAL '1' DAY
            ) daily_metric_count,
            (SELECT COALESCE((SELECT payment_plan FROM projects.projects WHERE id = #{pid}),'Free')) payment_plan,
            (SELECT COALESCE(ARRAY_AGG(DISTINCT method || '|' || host || '|' || canonical_path), '{}')
             FROM apis.endpoints WHERE project_id = #{pid} AND canonical_path IS NOT NULL
            ) canonical_paths,
            (SELECT COALESCE(ARRAY_AGG(prefix), '{}')
             FROM apis.learned_id_rules WHERE project_id = #{pid} AND disabled_at IS NULL
            ) id_rule_prefixes
    from
      (select e.host hosts, e.hash endpoint_hashes
         from apis.endpoints e
         where e.project_id = #{pid}
       ) enp; |]


projectCacheByIdIO :: OHasql.TracedPool -> ProjectId -> IO (Maybe ProjectCache)
projectCacheByIdIO hpool pid = runEff $ EHasql.runHasqlPool hpool $ runTime $ projectCacheById pid


insertProject :: DB es => CreateProject -> Eff es ()
insertProject p = EHasql.interpExecute_ [HI.sql| INSERT INTO projects.projects (id, title, description, payment_plan, time_zone, sub_id, first_sub_item_id, order_id, daily_notif, weekly_notif, endpoint_alerts, error_alerts) VALUES (#{p.id}, #{p.title}, #{p.description}, #{p.paymentPlan}, #{p.timeZone}, #{p.subId}, #{p.firstSubItemId}, #{p.orderId}, #{p.dailyNotif}, #{p.weeklyNotif}, #{p.endpointAlerts}, #{p.errorAlerts}) |]


-- | The project row whatever its state — including deactivated and soft-deleted.
--
-- Correct only where the caller must see a project that is going away: the
-- deletion and cancellation jobs, and anything reporting on a project's own
-- teardown. Everywhere else, acting on a row this returns means alerting,
-- billing or serving data for a project that is not supposed to exist —
-- 'activeProjectById' is what those want.
projectById :: DB es => ProjectId -> Eff es (Maybe Project)
projectById pid = EHasql.interpOne [HI.sql| select p.* from projects.projects p where id=#{pid}|]


-- | The project only if it is live. A deactivated or soft-deleted project reads
-- as absent, which is the right answer for delivery (alerts, reports), for
-- unauthenticated surfaces (share links) and for authenticating a key against
-- the project that owns it.
activeProjectById :: DB es => ProjectId -> Eff es (Maybe Project)
activeProjectById pid = EHasql.interpOne [HI.sql| SELECT p.* FROM projects.projects p WHERE id=#{pid} AND active=TRUE AND deleted_at IS NULL |]


projectByOrderId :: DB es => Text -> Eff es (Maybe Project)
projectByOrderId oid = EHasql.interpOne [HI.sql| select p.* from projects.projects p where order_id=#{oid}|]


projectByCustomerId :: DB es => Text -> Eff es (Maybe Project)
projectByCustomerId cid = EHasql.interpOne [HI.sql| select p.* from projects.projects p where customer_id=#{cid}|]


-- | A user's billing footprint across every project they administer. Both fields
-- answer a checkout question that must be asked of the person, not the project:
-- which Stripe customer to reuse (one human is one customer, however many projects
-- they run) and whether they have subscribed before (a second project is not a
-- second free trial). Deleted projects count — a deleted project's subscription is
-- precisely the history we must not lose sight of.
data UserBilling = UserBilling
  { stripeCustomerId :: Maybe Text
  , hasSubscribedBefore :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Default, FromRow, HI.DecodeRow, NFData)


-- | Scoped to @admin@ membership: an invited viewer on someone else's paid project
-- has not bought anything, and must keep their own trial.
userBilling :: DB es => UserId -> Eff es UserBilling
userBilling uid =
  fromMaybe def
    <$> EHasql.interpOne
      [HI.sql|
        SELECT (ARRAY_AGG(p.customer_id ORDER BY p.created_at DESC)
                 FILTER (WHERE p.customer_id IS NOT NULL AND p.customer_id <> ''))[1] AS stripe_customer_id,
               COALESCE(BOOL_OR(p.sub_id IS NOT NULL AND p.sub_id <> ''), FALSE) AS has_subscribed_before
        FROM projects.projects p
        JOIN projects.project_members pm ON pm.project_id = p.id
        WHERE pm.user_id = #{uid} AND pm.deleted_at IS NULL AND pm.permission = 'admin'
      |]


projectBySubId :: DB es => Text -> Eff es (Maybe Project)
projectBySubId subId = EHasql.interpOne [HI.sql| select p.* from projects.projects p where sub_id=#{subId}|]


updateSubItemIdBySubId :: DB es => Text -> Text -> Eff es Int64
updateSubItemIdBySubId newItemId subId = EHasql.interpExecute [HI.sql| update projects.projects set first_sub_item_id=#{newItemId} where sub_id=#{subId}|]


getProjectByPhoneNumber :: DB es => Text -> Eff es (Maybe Project)
getProjectByPhoneNumber number =
  EHasql.interpOne
    [HI.sql| SELECT p.* FROM projects.projects p
             JOIN projects.teams t ON t.project_id = p.id
             WHERE t.is_everyone = TRUE AND t.deleted_at IS NULL
               AND #{number} = ANY(t.phone_numbers)
             LIMIT 1 |]


activeProjects :: DB es => Eff es [Project]
activeProjects = EHasql.interp [HI.sql|SELECT p.* FROM projects.projects p WHERE p.active = TRUE AND p.deleted_at IS NULL|]


activeNonOnboardingProjectIds :: DB es => Eff es (V.Vector ProjectId)
activeNonOnboardingProjectIds =
  V.fromList <$> EHasql.interp [HI.sql|SELECT DISTINCT p.id FROM projects.projects p WHERE p.active = TRUE AND p.deleted_at IS NULL AND p.payment_plan != 'ONBOARDING'|]


-- | Projects with telemetry in the last day — the set every per-project
-- background job is seeded for.
--
-- Activity is read from the derived tables, not from @otel_logs_and_spans@.
-- Telemetry moved to TimeFusion and the Postgres table is empty by design, so
-- the join this used to do matched nothing and returned no projects at all —
-- which silently stopped every per-project job from being scheduled: endpoint
-- discovery, log-pattern processing, pattern merge, the span-derivation safety
-- net and usage reporting. Nothing failed; they were simply never created.
--
-- @schema_catalog@ is the primary signal because the observation flush touches
-- it for any span at all. Anomalies are a second source for a project whose
-- catalog rows are all settled.
recentlyActiveProjectIds :: DB es => UTCTime -> Eff es [ProjectId]
recentlyActiveProjectIds since =
  EHasql.interp
    [HI.sql|SELECT p.id FROM projects.projects p
           WHERE p.active = TRUE AND p.deleted_at IS NULL AND p.payment_plan != 'ONBOARDING'
             AND (EXISTS (SELECT 1 FROM apis.schema_catalog c
                           WHERE c.project_id = p.id AND c.updated_at > #{since}::timestamptz - interval '24 hours')
               OR EXISTS (SELECT 1 FROM apis.anomalies a
                           WHERE a.project_id = p.id AND a.created_at > #{since}::timestamptz - interval '24 hours'))|]


newProjectsSince :: DB es => UTCTime -> Eff es [Project]
newProjectsSince since = EHasql.interp [HI.sql|SELECT p.* FROM projects.projects p WHERE p.created_at >= #{since}::timestamptz AND p.deleted_at IS NULL ORDER BY p.created_at DESC|]


selectProjectsForUser :: (DB es, Time :> es) => UserId -> Eff es [ProjectListItem]
selectProjectsForUser uid = do
  now <- currentTime
  EHasql.interp
    [HI.sql|
        SELECT pp.*,
               EXISTS (
                    SELECT 1 FROM otel_logs_and_spans ols
                    WHERE ols.project_id = pp.id::text
                    AND ols.timestamp >= #{now}::timestamptz - INTERVAL '30 days'
                    LIMIT 1
                ) as has_integrated,
               ARRAY_AGG('/api/avatar/' || us.id::text) OVER (PARTITION BY pp.id)
        FROM projects.projects AS pp
        JOIN projects.project_members AS ppm ON (pp.id = ppm.project_id)
        JOIN users.users AS us ON (us.id = ppm.user_id)
        WHERE ppm.user_id = #{uid} AND pp.deleted_at IS NULL AND ppm.active = TRUE
        ORDER BY updated_at DESC
      |]


usersByProjectId :: DB es => ProjectId -> Eff es [User]
usersByProjectId pid =
  EHasql.interp
    [HI.sql| select u.id, u.created_at, u.updated_at, u.deleted_at, u.active, u.first_name, u.last_name, u.display_image_url, u.email, u.is_sudo, u.phone_number
                from users.users u join projects.project_members pm on (pm.user_id=u.id) where project_id=#{pid} and u.active IS True and pm.active = TRUE;|]


usersByIds :: DB es => V.Vector UUID.UUID -> Eff es [User]
usersByIds uids
  | V.null uids = pure []
  | otherwise = EHasql.interp (selectFrom @User <> [HI.sql| WHERE id = ANY(#{uids}::uuid[]) |])


updateProject :: DB es => CreateProject -> Eff es Int64
updateProject cp =
  EHasql.interpExecute
    [HI.sql|
       UPDATE projects.projects SET title=#{cp.title}, description=#{cp.description},
        payment_plan=#{cp.paymentPlan}, sub_id=#{cp.subId}, first_sub_item_id=#{cp.firstSubItemId}, order_id=#{cp.orderId},
        time_zone=#{cp.timeZone}, weekly_notif=#{cp.weeklyNotif}, daily_notif=#{cp.dailyNotif}, endpoint_alerts=#{cp.endpointAlerts}, error_alerts=#{cp.errorAlerts} where id=#{cp.id};
        |]


-- | Partial project update. Unspecified fields keep their current value (via COALESCE).
-- Returns the number of rows affected (0 ⇒ project not found).
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
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake ProjectPatch
  deriving (ToSchema) via SnakeSchema ProjectPatch


patchProjectSettings :: DB es => ProjectId -> ProjectPatch -> UTCTime -> Eff es Int64
patchProjectSettings pid p now =
  EHasql.interpExecute
    [HI.sql|
      UPDATE projects.projects SET
        title = COALESCE(#{p.title}, title),
        description = COALESCE(#{p.description}, description),
        time_zone = COALESCE(#{p.timeZone}, time_zone),
        daily_notif = COALESCE(#{p.dailyNotif}, daily_notif),
        weekly_notif = COALESCE(#{p.weeklyNotif}, weekly_notif),
        endpoint_alerts = COALESCE(#{p.endpointAlerts}, endpoint_alerts),
        error_alerts = COALESCE(#{p.errorAlerts}, error_alerts),
        updated_at = #{now}
      WHERE id = #{pid}
    |]


updateProjectPricing :: DB es => ProjectId -> Text -> Text -> Text -> Text -> V.Vector Text -> Eff es Int64
-- billing_provider inferred from the sub_id shape (same source as the webhooks/backfill):
-- this shared onboarding path carries a real LemonSqueezy sub_id (numeric) for paid plans and "" for Free/Open Source.
updateProjectPricing pid paymentPlan subId firstSubItemId orderId stepsCompleted =
  EHasql.interpExecute [HI.sql| UPDATE projects.projects SET payment_plan=#{paymentPlan}, sub_id=#{subId}, first_sub_item_id=#{firstSubItemId}, order_id=#{orderId}, onboarding_steps_completed=#{stepsCompleted}, billing_provider=#{billingProviderFromSubId (Just subId)} where id=#{pid};|]


updateProjectBilling :: DB es => ProjectId -> Text -> Text -> Text -> Text -> Eff es Int64
updateProjectBilling pid paymentPlan subId firstSubItemId orderId =
  EHasql.interpExecute [HI.sql| UPDATE projects.projects SET payment_plan=#{paymentPlan}, sub_id=#{subId}, first_sub_item_id=#{firstSubItemId}, order_id=#{orderId}, billing_provider=#{LemonSqueezyProvider} WHERE id=#{pid} AND (first_sub_item_id IS NULL OR first_sub_item_id = '');|]


updateProjectReportNotif :: DB es => ProjectId -> Text -> Eff es Int64
updateProjectReportNotif pid reportType =
  EHasql.interpExecute
    if reportType == "daily"
      then [HI.sql| UPDATE projects.projects SET daily_notif=(not daily_notif) WHERE id=#{pid};|]
      else [HI.sql| UPDATE projects.projects SET weekly_notif=(not weekly_notif) WHERE id=#{pid};|]


deleteProject :: (DB es, Time :> es) => ProjectId -> Eff es Int64
deleteProject pid = do
  now <- currentTime
  EHasql.interpExecute [HI.sql| UPDATE projects.projects SET deleted_at=#{now}, active=False where id=#{pid};|]


updateProjectS3Bucket :: DB es => ProjectId -> Maybe ProjectS3Bucket -> Eff es Int64
updateProjectS3Bucket pid bucket = EHasql.interpExecute [HI.sql| UPDATE projects.projects SET s3_bucket=#{bucket} WHERE id=#{pid}|]


---------------------------------
type QueryLibItemId = UUIDId "querylib"


data QueryLibType = QLTHistory | QLTSaved
  deriving (Eq, Generic, NFData, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC ('Just "projects.query_library_kind") "QLT" QueryLibType


data QueryLibItem = QueryLibItem
  { id :: QueryLibItemId
  , projectId :: ProjectId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , userId :: UserId
  , queryType :: QueryLibType
  , queryText :: Text
  , queryAst :: AE.Value
  , title :: Maybe Text
  , byMe :: Bool
  }
  deriving (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, FromRow, HI.DecodeRow, NFData, ToRow)


queryLibHistoryForUser :: DB es => ProjectId -> UserId -> Eff es [QueryLibItem]
queryLibHistoryForUser pid uid =
  EHasql.interp
    [HI.sql|
      (
        SELECT id, project_id, created_at, updated_at, user_id, query_type, query_text, query_ast, title,  user_id=#{uid}::uuid as byMe
        FROM projects.query_library
        WHERE user_id = #{uid}::uuid AND project_id = #{pid}::uuid AND query_type = 'history'
        ORDER BY created_at DESC
        LIMIT 50
      )
      UNION ALL
      (
        SELECT id, project_id, created_at, updated_at, user_id, query_type, query_text, query_ast, title, user_id=#{uid}::uuid as byMe
        FROM projects.query_library
        WHERE user_id = #{uid}::uuid AND project_id = #{pid}::uuid AND query_type = 'saved'
        ORDER BY created_at DESC
        LIMIT 50
      )
      UNION ALL
      (
        SELECT id, project_id, created_at, updated_at, user_id, query_type, query_text, query_ast,title, user_id=#{uid}::uuid as byMe
        FROM projects.query_library
        WHERE project_id = #{pid}::uuid AND user_id != #{uid}::uuid AND query_type = 'saved'
        ORDER BY created_at DESC
        LIMIT 50
      );
    |]


queryLibInsert :: DB es => QueryLibType -> ProjectId -> UserId -> Text -> [Section] -> Maybe Text -> Eff es ()
queryLibInsert qKind pid uid qt qast title =
  EHasql.interpExecute_
    [HI.sql|
      WITH removed_old AS (
        DELETE FROM projects.query_library
        WHERE id IN (
          SELECT id
          FROM projects.query_library
          WHERE project_id = #{pid} AND user_id = #{uid} AND query_type = #{qKind}::projects.query_library_kind
          ORDER BY created_at ASC
          OFFSET 49
        )
      )
      INSERT INTO projects.query_library (project_id, user_id, query_type, query_text, query_ast, title)
      SELECT #{pid}, #{uid}, #{qKind}::projects.query_library_kind, #{qt}, #{HI.AsJsonb qast}, #{title}
      WHERE NOT EXISTS (
        SELECT 1
        FROM projects.query_library
        WHERE project_id = #{pid} AND user_id = #{uid} AND query_type = #{qKind}::projects.query_library_kind
        AND query_text = #{qt}
        ORDER BY created_at DESC
        LIMIT 1
      )
      ON CONFLICT DO NOTHING;
    |]


queryLibTitleEdit :: DB es => ProjectId -> UserId -> Text -> Text -> Eff es ()
queryLibTitleEdit pid uid qId title = EHasql.interpExecute_ [HI.sql|UPDATE projects.query_library SET title=#{title} where project_id=#{pid} AND user_id=#{uid} AND id=#{qId}::uuid|]


queryLibItemDelete :: DB es => ProjectId -> UserId -> Text -> Eff es ()
queryLibItemDelete pid uid qId = EHasql.interpExecute_ [HI.sql|DELETE from projects.query_library where project_id=#{pid} AND user_id=#{uid} AND id=#{qId}::uuid|]


---------------------------------
-- LemonSqueezy subscription management
newtype LemonSubId = LemonSubId {lemonSubId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON, Default, Eq, FromField, FromHttpApiData, HI.DecodeValue, HI.EncodeValue, NFData, Ord, ToField)


instance HasField "toText" LemonSubId Text where
  getField l = UUID.toText l.lemonSubId


data LemonSub = LemonSub
  { id :: LemonSubId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , projectId :: Text
  , subscriptionId :: Int
  , orderId :: Int
  , firstSubId :: Int
  , productName :: Text
  , userEmail :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "subscriptions", PrimaryKey "id", FieldModifiers '[CamelToSnake]] LemonSub)


addSubscription :: DB es => LemonSub -> Eff es ()
addSubscription s =
  EHasql.interpExecute_
    [HI.sql|
      INSERT INTO apis.subscriptions (id, created_at, updated_at, project_id, subscription_id, order_id, first_sub_id, product_name, user_email)
      VALUES (#{s.id}, #{s.createdAt}, #{s.updatedAt}, #{s.projectId}, #{s.subscriptionId}, #{s.orderId}, #{s.firstSubId}, #{s.productName}, #{s.userEmail})
      ON CONFLICT (subscription_id) DO UPDATE SET
        updated_at = now(),
        order_id = EXCLUDED.order_id,
        first_sub_id = EXCLUDED.first_sub_id,
        product_name = EXCLUDED.product_name,
        user_email = EXCLUDED.user_email,
        project_id = COALESCE(NULLIF(EXCLUDED.project_id, ''), apis.subscriptions.project_id)
    |]


-- | (totalRequests, totalBytes) since `start`, using window_start (when events
-- occurred) rather than created_at. Pre-migration rows without window_start
-- are excluded from billing calculations.
getTotalUsage :: DB es => ProjectId -> UTCTime -> Eff es (Int64, Int64)
getTotalUsage pid start =
  fromMaybe (0, 0)
    <$> EHasql.interpOne
      [HI.sql|
    SELECT COALESCE(SUM(total_requests), 0)::bigint,
           COALESCE(SUM(total_event_bytes + total_metric_bytes), 0)::bigint
    FROM apis.daily_usage
    WHERE project_id = #{pid} AND window_start >= #{start}
  |]


-- | Per-day usage breakdown since `start`, grouped by the day the events
-- occurred (window_start), newest first. Returns
-- (day, events, metrics, eventBytes, metricBytes).
getDailyUsageBreakdown :: DB es => ProjectId -> UTCTime -> Eff es [(Day, Int64, Int64, Int64, Int64)]
getDailyUsageBreakdown pid start =
  EHasql.interp
    [HI.sql|
      SELECT (window_start AT TIME ZONE 'UTC')::date AS day,
             SUM(total_requests)::bigint,
             SUM(total_metrics)::bigint,
             SUM(total_event_bytes)::bigint,
             SUM(total_metric_bytes)::bigint
      FROM apis.daily_usage
      WHERE project_id = #{pid} AND window_start >= #{start}
      GROUP BY day
      ORDER BY day DESC
    |]


-- | Quantity of events in one submission chunk. Invariant: 0 < n <= 900_000
-- (Lemon Squeezy rejects quantities > 1,000,000; 900k leaves headroom). The
-- smart constructor is the only public way in; splitUsageIntoChunks is the
-- only producer in the codebase.
newtype ChunkQuantity = ChunkQuantity {chunkQuantityInt :: Int}
  deriving stock (Eq, Generic)
  deriving newtype (FromField, HI.DecodeValue, HI.EncodeValue, NFData, Show, ToField)
  deriving anyclass (HI.DecodeRow)


-- | >>> mkChunkQuantity 0
-- Nothing
-- >>> mkChunkQuantity (-1)
-- Nothing
-- >>> mkChunkQuantity 1
-- Just 1
-- >>> mkChunkQuantity 900000
-- Just 900000
-- >>> mkChunkQuantity 900001
-- Nothing
mkChunkQuantity :: Int -> Maybe ChunkQuantity
mkChunkQuantity = fmap ChunkQuantity . guarded (\n -> n > 0 && n <= chunkCap)


-- | Largest quantity a single billing-provider usage record may carry (Lemon Squeezy limit).
chunkCap :: Int
chunkCap = 900_000


-- | Per-chunk record of a usage submission to a billing provider. One window
-- may produce several rows. Bookkeeping (apis.daily_usage + usage_last_reported)
-- is committed atomically with 'Pending' rows BEFORE any provider HTTP call;
-- the outcome is then updated per submission. Non-'Submitted' rows are retried
-- on the next daily ReportUsage tick for the same project.
data UsageSubmission = UsageSubmission
  { id :: UUID.UUID
  , projectId :: ProjectId
  , windowStart :: UTCTime
  , windowEnd :: UTCTime
  , quantity :: ChunkQuantity
  , meter :: MeterKind
  , status :: Text
  , submittedAt :: Maybe UTCTime
  , lastError :: Maybe Text
  , createdAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow, NFData)


-- | One provider submission: which meter it bills against and how much. The
-- 900k cap is a Lemon Squeezy POST limit, not a pricing one, so it applies
-- identically to all three meters — the per-unit rate lives provider-side.
data UsageChunk = UsageChunk
  { meter :: MeterKind
  , quantity :: ChunkQuantity
  }
  deriving stock (Eq, Generic, Show)


-- | >>> splitUsageIntoChunks Events 0
-- []
-- >>> splitUsageIntoChunks Events 500
-- [UsageChunk {meter = Events, quantity = 500}]
-- >>> map (.quantity) (splitUsageIntoChunks SessionReplays 900001)
-- [900000,1]
-- >>> map (.quantity) (splitUsageIntoChunks MetricDatapoints 2700000)
-- [900000,900000,900000]
-- >>> map (.meter) (splitUsageIntoChunks MetricDatapoints 2700001)
-- [MetricDatapoints,MetricDatapoints,MetricDatapoints,MetricDatapoints]
splitUsageIntoChunks :: MeterKind -> Int -> [UsageChunk]
splitUsageIntoChunks meter total = UsageChunk meter <$> mapMaybe mkChunkQuantity (replicate fulls chunkCap <> [rem_])
  where
    (fulls, rem_) = max 0 total `divMod` chunkCap


pendingUsageSubmissions :: DB es => ProjectId -> Eff es [UsageSubmission]
pendingUsageSubmissions pid =
  EHasql.interp
    [HI.sql|
      SELECT id, project_id, window_start, window_end, quantity::int8, meter_kind, status, submitted_at, last_error, created_at
      FROM projects.usage_report_submissions
      WHERE project_id = #{pid} AND status <> 'submitted'
      ORDER BY created_at ASC
    |]


-- | Per-window usage totals: row counts split between events (logs+spans) and
-- metrics, plus the encoded-protobuf payload-byte sum for each. `total_requests`
-- (= events + metrics) drives existing pricing/chunking; the per-bucket fields
-- are visibility-only.
data UsageTotals = UsageTotals
  { events :: Int
  , eventBytes :: Int64
  , metrics :: Int
  , metricBytes :: Int64
  , replays :: Int
  }
  deriving stock (Eq, Generic, Show)


instance Default UsageTotals where
  def = UsageTotals 0 0 0 0 0


-- | The billable count for one dimension. Single mapping from meter to number,
-- so a dimension cannot silently read the wrong column at one of several sites.
--
-- >>> map (`meterQuantity` def{events = 3, metrics = 7, replays = 2}) [minBound .. maxBound]
-- [3,7,2]
meterQuantity :: MeterKind -> UsageTotals -> Int
meterQuantity = \case
  Events -> (.events)
  MetricDatapoints -> (.metrics)
  SessionReplays -> (.replays)


recordUsageWindow
  :: (DB es, UUIDEff :> es)
  => ProjectId -> UTCTime -> UTCTime -> UsageTotals -> [UsageChunk] -> Eff es ()
recordUsageWindow pid wStart wEnd totals chunks = do
  chunkRows <- forM chunks \c -> (,c) <$> genUUID
  let exec :: HI.Sql -> Tx.Transaction ()
      exec s = void $ Tx.statement () (HI.interp True s :: Statement () HI.RowsAffected)
      -- total_requests historically = events + metrics (drives getTotalUsage and
      -- the Settings billing estimate). Preserve that invariant; new columns are
      -- additive. Replays are deliberately NOT folded in — they are priced 1000x
      -- differently, so adding them would corrupt that estimate.
      totalUsage = totals.events + totals.metrics
      -- Guard on ANY metered dimension, not just total_requests: a replay-only
      -- window (no spans, no metrics, some replays) would otherwise skip both the
      -- daily_usage row and its chunks, and go silently unbilled.
      anyUsage = any (\k -> meterQuantity k totals > 0) [minBound .. maxBound]
  EHasql.transaction TxS.ReadCommitted TxS.Write do
    -- usage_last_reported always advances (even on zero-usage days); otherwise
    -- the next tick re-scans an ever-growing window, which is the failure mode
    -- that produced the original 15-month poison loop.
    exec [HI.sql| UPDATE projects.projects SET usage_last_reported = #{wEnd} WHERE id = #{pid} |]
    when anyUsage do
      exec
        [HI.sql| INSERT INTO apis.daily_usage (project_id, total_requests, total_metrics, total_replays, total_event_bytes, total_metric_bytes, window_start, window_end)
                       VALUES (#{pid}, #{totalUsage}, #{totals.metrics}, #{totals.replays}, #{totals.eventBytes}, #{totals.metricBytes}, #{wStart}, #{wEnd}) |]
      for_ chunkRows \(cid, UsageChunk meter (ChunkQuantity qty)) ->
        exec
          [HI.sql| INSERT INTO projects.usage_report_submissions (id, project_id, window_start, window_end, quantity, meter_kind)
                      VALUES (#{cid}, #{pid}, #{wStart}, #{wEnd}, #{qty}, #{meter}) |]


markUsageSubmissionSucceeded :: DB es => UUID.UUID -> Eff es Int64
markUsageSubmissionSucceeded sid =
  EHasql.interpExecute
    [HI.sql|
      UPDATE projects.usage_report_submissions
      SET status = 'submitted', submitted_at = now(), last_error = NULL
      WHERE id = #{sid}
    |]


markUsageSubmissionFailed :: DB es => UUID.UUID -> Text -> Eff es Int64
markUsageSubmissionFailed sid err =
  EHasql.interpExecute
    [HI.sql|
      UPDATE projects.usage_report_submissions
      SET status = 'failed', last_error = #{err}
      WHERE id = #{sid}
    |]


-- Keep sub_id/order_id/first_sub_item_id intact on downgrade so resume/payment_success can re-upgrade without a new checkout.
downgradeToFree :: DB es => Int -> Eff es Int64
-- @projects.projects.order_id@ is TEXT but callers (LemonSqueezy webhooks) pass a
-- numeric id; cast the bind parameter so PG doesn't reject @text = bigint@.
downgradeToFree orderId = EHasql.interpExecute [HI.sql|UPDATE projects.projects SET payment_plan = 'Free' WHERE order_id = #{orderId}::text|]


-- Match on sub_id when available (stable across plan/order changes), else fall back to order_id.
-- Using OR with both risks matching a stale preserved row on a different project after downgrade.
upgradeToPaid :: DB es => Int -> Int -> Int -> Text -> Eff es Int64
upgradeToPaid orderId subId subItemId plan =
  EHasql.interpExecute
    [HI.sql|
      UPDATE projects.projects
         SET payment_plan = #{plan}, sub_id = #{subId}::text, first_sub_item_id = #{subItemId}::text, billing_provider = #{LemonSqueezyProvider}
       WHERE sub_id = #{subId}::text
          OR (sub_id IS NULL AND order_id = #{orderId}::text) |]


-- | The free-tier predicate. `payment_plan` is an open-valued text column (paid tiers
-- carry arbitrary names like "Startup"/"Bring your own storage"), so we can't model it as
-- a closed enum — but the free-tier check must live in exactly ONE place. Comparing the
-- literal inline let two call sites drift ("Free" vs "FREE"), silently disabling the
-- free-tier gate. Case-insensitive so historical "FREE" rows still match.
--
-- >>> map isFreeTier ["Free", "FREE", "free", "Startup", "ONBOARDING"]
-- [True,True,True,False,False]
isFreeTier :: Text -> Bool
isFreeTier = (== "free") . T.toLower


-- | >>> map isOnboarding ["ONBOARDING", "onboarding", "Free"]
-- [True,True,False]
isOnboarding :: Text -> Bool
isOnboarding = (== "onboarding") . T.toLower


-- | Payment provider for a project. Stored authoritatively in the
-- @billing_provider@ column (written by the webhook that knows which provider
-- fired) rather than guessed at every read from the shape of @sub_id@.
data BillingProvider = StripeProvider | LemonSqueezyProvider | NoBillingProvider
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC 'Nothing "" BillingProvider


-- Semantic default is NoBillingProvider; generic Default would pick the first constructor.
instance Default BillingProvider where
  def = NoBillingProvider


-- | Provider inferred from the shape of @sub_id@. Only used to backfill the
-- stored column (migration 0106) and as the read fallback in 'projectProvider'
-- for legacy rows whose @billing_provider@ was never written.
--
-- >>> billingProviderFromSubId (Just "sub_abc123")
-- StripeProvider
-- >>> billingProviderFromSubId (Just "12345")
-- LemonSqueezyProvider
-- >>> billingProviderFromSubId Nothing
-- NoBillingProvider
-- >>> billingProviderFromSubId (Just "")
-- NoBillingProvider
billingProviderFromSubId :: Maybe Text -> BillingProvider
billingProviderFromSubId = \case
  Just sid
    | "sub_" `T.isPrefixOf` sid -> StripeProvider
    | not (T.null sid) && T.all isDigit sid -> LemonSqueezyProvider
  _ -> NoBillingProvider


-- | Authoritative provider: the stored column, falling back to shape inference
-- only when unset (legacy rows / paths that never wrote it).
projectProvider :: Project -> BillingProvider
projectProvider p = case p.billingProvider of
  NoBillingProvider -> billingProviderFromSubId p.subId
  StripeProvider -> StripeProvider
  LemonSqueezyProvider -> LemonSqueezyProvider


-- | A separately-metered billing dimension. Constructors are the single source
-- of truth for the meter's spelling everywhere (JSON, the @meter_kind@ column,
-- the @CHECK@ constraint) — @WrappedEnumSC@ derives them all from the constructor
-- name, so they cannot drift apart.
--
-- Rates are provider-side; we only ever submit raw counts:
-- 'Events' $1/1M, 'MetricDatapoints' $1/10M, 'SessionReplays' $1/1k.
data MeterKind = Events | MetricDatapoints | SessionReplays
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, FromField, HI.DecodeValue, HI.EncodeValue, ToField, Var) via WrappedEnumSC 'Nothing "" MeterKind


-- | Stripe addresses a meter by @event_name@ against the customer, so a meter
-- name is derivable and needs no config entry. Both of these names are the ones
-- already live in the Stripe account, not ones we chose:
--
--   * @events_usage@ carries the existing overage price and is on every subscription.
--   * @metrics_usage@ predates this work and already has a \"Metrics\" product priced
--     at 0.00001 cents/unit — exactly the \$1 per 10M we want — attached to no
--     subscription yet. Reused rather than duplicated under a tidier name; a second
--     meter would have meant a second product and a second price saying the same thing.
--
-- >>> map stripeMeterEventName [minBound .. maxBound]
-- ["events_usage","metrics_usage","session_replays_usage"]
stripeMeterEventName :: MeterKind -> Text
stripeMeterEventName = \case
  Events -> "events_usage"
  MetricDatapoints -> "metrics_usage"
  SessionReplays -> "session_replays_usage"


-- | Where one meter's usage is submitted. The two providers address a meter
-- completely differently: Stripe by customer + meter event name, Lemon Squeezy
-- by subscription item. Carrying the resolved address in the type means the
-- submit path cannot be reached without one.
data MeterTarget
  = StripeMeter {customerId :: Text, eventName :: Text}
  | LemonSqueezyMeter {subItemId :: Text}
  deriving stock (Eq, Generic, Show)


-- | Why a meter is not submitting. Every reason is logged, so "why is this not
-- being billed" is answerable from logs alone rather than by reading this file.
data DormantReason
  = -- | Stripe project with neither @customer_id@ nor @order_id@ — a misconfig.
    NoStripeCustomer
  | -- | Lemon Squeezy project with no subscription item for this meter. LS
    -- usage records are addressed only by subscription item, so a second and
    -- third metered variant must exist on the subscription before its metrics
    -- or replays can be billed.
    NoSubscriptionItem
  | -- | Paid plan whose provider could not be determined.
    ProviderUnusable
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


-- | Does this reason mean "this dimension does not bill for this project yet",
-- or "we are owed this but cannot send it"?
--
-- The distinction decides whether a window cuts submission chunks at all. Only a
-- misconfig cuts them (so the drain leaves an auditable, retriable @failed@ row);
-- a dormant meter records its totals in @apis.daily_usage@ and nothing else.
--
-- 'NoSubscriptionItem' is dormancy, not misconfig: a Lemon Squeezy customer whose
-- subscription has no metered variant for this dimension never agreed to that
-- price. Cutting chunks for them would accrue failed rows forever against a
-- product that does not exist. 'NoStripeCustomer' is the opposite — the project
-- is on the plan and we simply cannot address it.
--
-- >>> map meterIsDormant [NoSubscriptionItem, NoStripeCustomer, ProviderUnusable]
-- [True,False,False]
meterIsDormant :: DormantReason -> Bool
meterIsDormant = \case
  NoSubscriptionItem -> True
  NoStripeCustomer -> False
  ProviderUnusable -> False


-- | Everything 'resolveMeterTarget' needs about a project, lifted out of
-- 'Project' so the decision stays a pure, totally-testable function.
data ProjectMeterConfig = ProjectMeterConfig
  { provider :: BillingProvider
  , stripeCustomerId :: Maybe Text
  , eventsSubItemId :: Maybe Text
  -- ^ The @Events@ subscription item, from @projects.first_sub_item_id@.
  , subItemIds :: Map MeterKind Text
  -- ^ Per-meter subscription items from @projects.billing_meter_items@.
  }
  deriving stock (Generic, Show)


projectMeterConfig :: Project -> Map MeterKind Text -> ProjectMeterConfig
projectMeterConfig p subItemIds =
  ProjectMeterConfig
    { provider = projectProvider p
    , stripeCustomerId = mfilter (not . T.null) (p.customerId <|> p.orderId)
    , eventsSubItemId = mfilter (not . T.null) p.firstSubItemId
    , subItemIds
    }


-- | The single decision point for "may this meter submit, and to where".
--
-- There is no enable list. Every dimension submits wherever it is addressable,
-- and addressability is the only gate — which puts the off-switch provider-side,
-- where it belongs: a Stripe meter with no price attached bills nothing however
-- many events it receives.
--
-- >>> let ls n = ProjectMeterConfig LemonSqueezyProvider Nothing (Just "si_1") n
-- >>> resolveMeterTarget (ls mempty) Events
-- Right (LemonSqueezyMeter {subItemId = "si_1"})
--
-- Metric datapoints ride the events item on Lemon Squeezy, where a subscription
-- cannot carry a second one; replays do not, because the events rate would
-- undercharge them a thousandfold.
--
-- >>> resolveMeterTarget (ls mempty) MetricDatapoints
-- Right (LemonSqueezyMeter {subItemId = "si_1"})
-- >>> resolveMeterTarget (ls mempty) SessionReplays
-- Left NoSubscriptionItem
-- >>> resolveMeterTarget (ls (fromList [(SessionReplays, "si_9")])) SessionReplays
-- Right (LemonSqueezyMeter {subItemId = "si_9"})
-- >>> resolveMeterTarget (ProjectMeterConfig StripeProvider (Just "cus_1") Nothing mempty) MetricDatapoints
-- Right (StripeMeter {customerId = "cus_1", eventName = "metrics_usage"})
-- >>> resolveMeterTarget (ProjectMeterConfig StripeProvider Nothing Nothing mempty) Events
-- Left NoStripeCustomer
-- >>> resolveMeterTarget (ProjectMeterConfig NoBillingProvider Nothing Nothing mempty) Events
-- Left ProviderUnusable
resolveMeterTarget :: ProjectMeterConfig -> MeterKind -> Either DormantReason MeterTarget
resolveMeterTarget cfg kind = case cfg.provider of
  NoBillingProvider -> Left ProviderUnusable
  -- Stripe meter events key off customer + event_name; the subscription item
  -- id is not part of the call, so no per-meter item is needed.
  StripeProvider -> maybe (Left NoStripeCustomer) (Right . (`StripeMeter` stripeMeterEventName kind)) cfg.stripeCustomerId
  -- LS usage records are addressed ONLY by subscription item, and an LS
  -- subscription carries exactly one — the API cannot add a second. So a
  -- per-dimension price is not expressible there the way it is on Stripe.
  --
  -- Metric datapoints therefore ride the events item, billing at the events
  -- rate. That is a deliberate pricing decision, not a fallback: it charges an
  -- LS customer ~10x per datapoint what a Stripe customer pays (\$1/1M rather
  -- than \$1/10M), and it is the only way to bill them for metrics at all.
  --
  -- Session replays do NOT ride it. The error is asymmetric: replays are priced
  -- \$1/1,000, so billing them at the events rate would undercharge by a factor
  -- of a thousand. They stay dormant until a per-meter item exists.
  LemonSqueezyProvider -> case Map.lookup kind cfg.subItemIds <|> (guard (kind `elem` [Events, MetricDatapoints]) >> cfg.eventsSubItemId) of
    Just sid -> Right (LemonSqueezyMeter sid)
    Nothing -> Left NoSubscriptionItem


-- | Per-meter Lemon Squeezy subscription items. Empty for every project until
-- the metered variants exist provider-side and are recorded here.
meterSubItemIds :: DB es => ProjectId -> Eff es (Map MeterKind Text)
meterSubItemIds pid =
  Map.fromList
    <$> EHasql.interp
      [HI.sql| SELECT meter_kind, sub_item_id FROM projects.billing_meter_items WHERE project_id = #{pid} |]


setMeterSubItemId :: DB es => ProjectId -> MeterKind -> Text -> Eff es Int64
setMeterSubItemId pid kind sid =
  EHasql.interpExecute
    [HI.sql|
      INSERT INTO projects.billing_meter_items (project_id, meter_kind, sub_item_id)
      VALUES (#{pid}, #{kind}, #{sid})
      ON CONFLICT (project_id, meter_kind) DO UPDATE SET sub_item_id = EXCLUDED.sub_item_id, updated_at = now()
    |]


-- | Typed view over the open-valued @payment_plan@ column. Storage stays 'Text'
-- (paid tiers carry arbitrary names, still on @payment_plan@); this is the single
-- parse point folding the onboarding/free/paid distinction so gating code matches
-- exhaustively instead of scattering case-insensitive string compares.
--
-- Phased rollout: the usage-reporting gate uses this today; the remaining
-- 'isFreeTier'/'isOnboarding' call sites (ingestion hot path, several Pages) can
-- migrate onto 'Plan' incrementally. See docs/design-notes/billing-plan-types.md.
--
-- >>> map parsePlan ["ONBOARDING", "onboarding", "Free", "FREE", "Startup"]
-- [Onboarding,Onboarding,Free,Free,Paid]
data Plan = Onboarding | Free | Paid
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


parsePlan :: Text -> Plan
parsePlan t
  | isOnboarding t = Onboarding
  | isFreeTier t = Free
  | otherwise = Paid


-- | >>> map (isPaidPlan . parsePlan) ["Startup", "Free", "ONBOARDING"]
-- [True,False,False]
isPaidPlan :: Plan -> Bool
isPaidPlan = \case
  Paid -> True
  Free -> False
  Onboarding -> False


downgradeToFreeBySubId :: DB es => Text -> Eff es Int64
downgradeToFreeBySubId sid =
  EHasql.interpExecute [HI.sql|UPDATE projects.projects SET payment_plan = 'Free' WHERE sub_id = #{sid}|]


-- Re-enable a previously-downgraded subscription (paused → active, past_due → active).
-- IDs were preserved by downgradeToFree* so we match by sub_id.
setPlanBySubId :: DB es => Text -> Text -> Text -> Eff es Int64
setPlanBySubId plan firstSubItemId sid =
  EHasql.interpExecute [HI.sql|UPDATE projects.projects SET payment_plan = #{plan}, first_sub_item_id = #{firstSubItemId} WHERE sub_id = #{sid}|]


updateStripeProjectBilling :: DB es => ProjectId -> Text -> Text -> Text -> Text -> Eff es Int64
updateStripeProjectBilling pid plan subId firstSubItemId customerId =
  -- Clear order_id so a late LemonSqueezy cancel webhook (from a prior LS→Stripe
  -- switch) can't rematch this project by order_id and downgrade to Free.
  EHasql.interpExecute [HI.sql|UPDATE projects.projects SET payment_plan = #{plan}, sub_id = #{subId}, first_sub_item_id = #{firstSubItemId}, customer_id = #{customerId}, order_id = NULL, billing_provider = #{StripeProvider} WHERE id = #{pid}|]


-- Sessions

newtype PersistentSessionId = PersistentSessionId {getPersistentSessionId :: UUID.UUID}
  deriving newtype (NFData)
  deriving (Display) via ShowInstance UUID.UUID
  deriving
    (Default, Eq, FromField, FromHttpApiData, HI.DecodeValue, HI.EncodeValue, Show, ToField, ToHttpApiData)
    via UUID.UUID


newtype SessionData = SessionData {getSessionData :: Map Text Text}
  deriving stock (Eq, Generic, Show)
  deriving newtype (NFData)
  deriving anyclass (Default)
  deriving
    (FromField, HI.DecodeValue, HI.EncodeValue, ToField)
    via Aeson (Map Text Text)


newtype PSUser = PSUser {getUser :: User}
  deriving stock (Generic, Show)
  deriving newtype (NFData)
  deriving anyclass (Default)
  deriving
    (FromField, HI.DecodeValue, HI.EncodeValue, ToField)
    via Aeson User


newtype PSProjects = PSProjects {getProjects :: V.Vector Project}
  deriving stock (Generic, Show)
  deriving newtype (NFData)
  deriving anyclass (Default)
  deriving
    (FromField, HI.DecodeValue, HI.EncodeValue, ToField)
    via Aeson (V.Vector Project)


data PersistentSession = PersistentSession
  { id :: PersistentSessionId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , userId :: UserId
  , sessionData :: SessionData
  , user :: PSUser
  , isSudo :: Bool
  , projects :: PSProjects
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, HI.DecodeRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "users", TableName "persistent_sessions", PrimaryKey "id"] PersistentSession)


newPersistentSessionId :: UUIDEff :> es => Eff es PersistentSessionId
newPersistentSessionId = PersistentSessionId <$> genUUID


insertSession :: DB es => PersistentSessionId -> UserId -> SessionData -> Eff es ()
insertSession psId uid sd = EHasql.interpExecute_ [HI.sql| insert into users.persistent_sessions(id, user_id, session_data) VALUES (#{psId}, #{uid}, #{sd}) |]


getPersistentSession :: DB es => PersistentSessionId -> Eff es (Maybe PersistentSession)
getPersistentSession sessionId =
  EHasql.interpOne
    [HI.sql| select ps.id, ps.created_at, ps.updated_at, ps.user_id, ps.session_data, to_jsonb(u) as user, u.is_sudo,
        COALESCE(jsonb_agg(to_jsonb(pp.*) ORDER BY pp.updated_at DESC) FILTER (WHERE pp.id is not NULL AND pp.deleted_at IS NULL),'[]') as projects
        from users.persistent_sessions as ps
        left join users.users u on (u.id=ps.user_id)
        left join projects.project_members ppm on (ps.user_id=ppm.user_id AND ppm.active = TRUE)
        left join projects.projects pp on (pp.id=ppm.project_id)
        where ps.id=#{sessionId}
        GROUP BY ps.created_at, ps.updated_at, ps.id, ps.user_id, ps.session_data, u.* ,u.is_sudo; |]


getSession :: EffReader.Reader (Headers '[Header "Set-Cookie" SetCookie] Session) :> es => Eff es Session
getSession = EffReader.asks (getResponse @'[Header "Set-Cookie" SetCookie])


craftSessionCookie :: PersistentSessionId -> Bool -> SetCookie
craftSessionCookie (PersistentSessionId content) rememberSession =
  defaultSetCookie
    { setCookieValue = UUID.toASCIIBytes content
    , setCookieName = "monoscope_session"
    , setCookiePath = Just "/"
    , setCookieHttpOnly = True
    , setCookieSameSite = Just sameSiteLax
    , setCookieMaxAge = if rememberSession then Just 604800 else Nothing
    , setCookieSecure = True
    }


emptySessionCookie :: SetCookie
emptySessionCookie =
  defaultSetCookie
    { setCookieName = "monoscope_session"
    , setCookieValue = ""
    , setCookieMaxAge = Just 0
    }


addCookie :: SetCookie -> a -> Headers '[Header "Set-Cookie" SetCookie] a
addCookie = addHeader


data Session = Session
  { sessionId :: PersistentSessionId
  , persistentSession :: PersistentSession
  , user :: User
  , requestID :: Text
  , isSidebarClosed :: Bool
  , theme :: Text
  , lang :: Web.I18n.Language
  -- ^ User-selected UI language, read from the @lang@ cookie at auth time
  -- and threaded down to templates so 'Web.I18n.t' calls can render the
  -- right language without extra context plumbing.
  , environment :: Maybe Text
  -- ^ The selected deployment environment (@prod@, @staging@, …), read from the @env@
  -- cookie at auth time so the choice is sticky across pages, reloads and tabs — the way
  -- Datadog's Env selector behaves. 'Nothing' is every environment, and it is the default:
  -- rows predating the promoted column report no environment, so choosing one for the user
  -- would silently hide their history.
  }
  deriving stock (Generic, Show)


sessionAndProject
  :: (DB es, EffError.Error ServerError :> es, EffReader.Reader (Headers '[Header "Set-Cookie" SetCookie] Session) :> es)
  => ProjectId
  -> Eff es (Session, Project)
sessionAndProject pid = do
  sess <- getSession
  let redirect = EffError.throwError $ err302{errHeaders = [("Location", "/?missingProjectPermission")]}
      fetch = projectById pid >>= maybe redirect (pure . (sess,))
  case V.find ((== pid) . (.id)) sess.persistentSession.projects.getProjects of
    -- Onboarding projects in the session cache are stale; re-read them.
    Just p | not (isOnboarding p.paymentPlan) -> pure (sess, p)
    Just _ -> fetch
    Nothing
      | pid == UUIDId UUID.nil || sess.user.isSudo -> fetch
      | otherwise -> redirect


----------------------------------------------------------------------
-- Audit Log
----------------------------------------------------------------------

data AuditEvent
  = AEProjectDeleted
  | AEProjectCreated
  | AEProjectUpdated
  | AEMemberAdded
  | AEMemberRemoved
  | AEMemberPermissionChanged
  | AEApiKeyCreated
  | AEApiKeyRevoked
  | AEApiKeyActivated
  | AEMonitorDeleted
  | AES3Configured
  | AES3Removed
  | AEIntegrationConnected
  | AEIntegrationDisconnected
  | AEPlanChanged
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (Display, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC 'Nothing "AE" AuditEvent


logAudit :: DB es => ProjectId -> AuditEvent -> Maybe UserId -> Maybe Text -> Maybe AE.Value -> Eff es ()
logAudit pid event actorId actorEmail metadataM =
  EHasql.interpExecute_ [HI.sql| INSERT INTO projects.audit_log (project_id, event, actor_id, actor_email, metadata) VALUES (#{pid}, #{event}, #{actorId}, #{actorEmail}, #{HI.AsJsonb <$> metadataM}) |]


logAuditS :: DB es => ProjectId -> AuditEvent -> Session -> Maybe AE.Value -> Eff es ()
logAuditS pid event sess = logAudit pid event (Just sess.user.id) (Just $ CI.original sess.user.email)
