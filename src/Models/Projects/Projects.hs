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
  Project' (..),
  ProjectId,
  CreateProject (..),
  NotificationChannel (..),
  OnboardingStep (..),
  ProjectS3Bucket (..),
  insertProject,
  projectIdFromText,
  usersByProjectId,
  selectProjectsForUser,
  getProjectByPhoneNumber,
  activeProjects,
  activeNonOnboardingProjectIds,
  recentlyActiveProjectIds,
  newProjectsSince,
  updateProject,
  deleteProject,
  updateProjectPricing,
  updateProjectBilling,
  projectById,
  projectByOrderId,
  projectBySubId,
  updateSubItemIdBySubId,
  projectCacheById,
  projectCacheByIdIO,
  updateProjectReportNotif,
  ProjectCache (..),
  defaultProjectCache,
  updateNotificationsChannel,
  enableNotificationChannel,
  updateUsageLastReported,
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
  billingProvider,
  LemonSub (..),
  LemonSubId (..),
  addSubscription,
  getTotalUsage,
  addDailyUsageReport,
  upgradeToPaid,
  downgradeToFree,
  downgradeToFreeBySubId,
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
import Hasql.Interpolate qualified as HI
import Data.Effectful.UUID (UUIDEff, genUUID)
import Data.Effectful.UUID qualified as UUID
import Hasql.Pool qualified as HPool
import Data.Text qualified as T
import Data.Text.Display
import Data.Time (UTCTime, ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes
import Database.PostgreSQL.Simple.ToField (ToField)
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.Error.Static (throwError)
import Effectful.Error.Static qualified as EffError
import Effectful.Reader.Static qualified as EffReader
import Effectful.Time (Time, currentTime, runTime)
import GHC.Records (HasField (getField))
import Pkg.DeriveUtils (DB, UUIDId (..), WrappedEnumSC (..), idFromText)
import Pkg.Parser.Stats (Section)
import Relude
import Servant (FromHttpApiData, Header, Headers, ServerError, addHeader, err302, errHeaders, getResponse)
import Web.Cookie (SetCookie (setCookieHttpOnly, setCookieMaxAge, setCookieName, setCookiePath, setCookieSameSite, setCookieSecure, setCookieValue), defaultSetCookie, sameSiteLax)
import Web.FormUrlEncoded (FromForm)
import Web.HttpApiData (ToHttpApiData)


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
  getField = UUID.toText . getUserId


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
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] User


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
insertUser u = do
  let (uId, uCr, uUp, uDel, uAct, uFn, uLn, uImg, uEm, uPh, uSudo) =
        (u.id, u.createdAt, u.updatedAt, u.deletedAt, u.active, u.firstName, u.lastName, u.displayImageUrl, u.email, u.phoneNumber, u.isSudo)
  EHasql.interpExecute_ [HI.sql| INSERT INTO users.users (id, created_at, updated_at, deleted_at, active, first_name, last_name, display_image_url, email, phone_number, is_sudo) VALUES (#{uId}, #{uCr}, #{uUp}, #{uDel}, #{uAct}, #{uFn}, #{uLn}, #{uImg}, #{uEm}, #{uPh}, #{uSudo}) |]


userById :: DB es => UserId -> Eff es (Maybe User)
userById uid = EHasql.interpOne [HI.sql| SELECT * FROM users.users WHERE id = #{uid} |]


userByEmail :: DB es => Text -> Eff es (Maybe User)
userByEmail email = EHasql.interpOne [HI.sql| SELECT * FROM users.users WHERE email = #{email} |]


userIdByEmail :: DB es => Text -> Eff es (Maybe UserId)
userIdByEmail email = EHasql.interpOne [HI.sql|select id from users.users where email=#{email}|]


createEmptyUser :: DB es => Text -> Eff es (Maybe UserId)
createEmptyUser email = EHasql.interpOne [HI.sql| insert into users.users (email, active) values (#{email}, TRUE) on conflict do nothing returning id |]


---------------------------------
type ProjectId = UUIDId "project"


projectIdFromText :: Text -> Maybe ProjectId
projectIdFromText = idFromText


data NotificationChannel
  = NEmail
  | NSlack
  | NDiscord
  | NPhone
  | NPagerduty
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC "N" NotificationChannel


instance HasField "toText" NotificationChannel Text where
  getField nc = case AE.toJSON nc of
    AE.String t -> t
    _ -> error "NotificationChannel should serialize to String"


data OnboardingStep = Info | Survey | CreateMonitor | NotifChannel | Integration | Pricing | Complete
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON, FromField, NFData, ToField) via OnboardingStep


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
  , notificationsChannel :: V.Vector NotificationChannel
  , subId :: Maybe Text
  , firstSubItemId :: Maybe Text
  , orderId :: Maybe Text
  , usageLastReported :: UTCTime
  , discordUrl :: Maybe Text
  , billingDay :: Maybe UTCTime
  , onboardingStepsCompleted :: V.Vector Text
  , notifyPhoneNumber :: Maybe Text
  , notifyEmails :: V.Vector Text
  , whatsappNumbers :: V.Vector Text
  , s3Bucket :: Maybe ProjectS3Bucket
  , endpointAlerts :: Bool
  , errorAlerts :: Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, NFData)
  deriving
    (Entity)
    via (GenericEntity '[Schema "projects", TableName "projects", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Project)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Project


-- FIXME: Why was this record created? And not the regular projects record?
data Project' = Project'
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
  , notificationsChannel :: V.Vector NotificationChannel
  , subId :: Maybe Text
  , firstSubItemId :: Maybe Text
  , orderId :: Maybe Text
  , usageLastReported :: UTCTime
  , discordUrl :: Maybe Text
  , billingDay :: Maybe UTCTime
  , onboardingStepsCompleted :: V.Vector Text
  , notifyPhoneNumber :: Maybe Text
  , notifyEmails :: V.Vector Text
  , whatsappNumbers :: V.Vector Text
  , s3Bucket :: Maybe ProjectS3Bucket
  , endpointAlerts :: Bool
  , errorAlerts :: Bool
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
  deriving (FromField, ToField) via Aeson ProjectS3Bucket

deriving via Aeson ProjectS3Bucket instance HI.DecodeValue ProjectS3Bucket
deriving via Aeson ProjectS3Bucket instance HI.EncodeValue ProjectS3Bucket


data ProjectCache = ProjectCache
  { -- We need this hosts to mirror all the hosts in the endpoints table, and could use this for validation purposes to skip inserting endpoints just because of hosts
    -- if endpoint exists but host is not in this list, then we have a query specifically for inserting hosts.
    hosts :: V.Vector Text
  , -- maybe we don't need this? See the next point.
    endpointHashes :: V.Vector Text
  , -- Since shapes always have the endpoints hash prepended to them, maybe we don't need to store the hash of endpoints,
    -- since we can derive that from the shapes.
    shapeHashes :: V.Vector Text
  , -- We check if every request is part of the redact list, so it's better if we don't need to  hit the db for them with each request.
    -- Since we have a need to redact fields by endpoint, we can simply have the fields paths be prepended by the endpoint hash.
    -- [endpointHash]<>[field_category eg requestBody]<>[field_key_path]
    -- Those redact fields that don't have endpoint or field_category attached, would be aplied to every endpoint and field category.
    redactFieldslist :: V.Vector Text
  , -- Daily count of events from otel_logs_and_spans table for the last 24 hours
    dailyEventCount :: Int
  , -- Daily count of metrics for the last 24 hours
    dailyMetricCount :: Int
  , paymentPlan :: Text
  , -- Canonical URL path templates for matching at ingestion: "method|host|template_path"
    canonicalPaths :: V.Vector Text
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
  let pidText = pid.toText
  EHasql.interpOne [HI.sql|
    select  coalesce(ARRAY_AGG(DISTINCT hosts ORDER BY hosts ASC),'{}') hosts,
            coalesce(ARRAY_AGG(DISTINCT endpoint_hashes ORDER BY endpoint_hashes ASC),'{}') endpoint_hashes,
            coalesce(ARRAY_AGG(DISTINCT shape_hashes ORDER BY shape_hashes ASC),'{}'::text[]) shape_hashes,
            coalesce(ARRAY_AGG(DISTINCT paths ORDER BY paths ASC),'{}') redacted_fields,
            ( SELECT count(*)::int FROM otel_logs_and_spans
             WHERE project_id=#{pidText} AND timestamp > #{now}::timestamptz - INTERVAL '1' DAY
            ) daily_event_count,
            ( SELECT count(*)::int FROM telemetry.metrics
             WHERE project_id=#{pid} AND timestamp > #{now}::timestamptz - INTERVAL '1' DAY
            ) daily_metric_count,
            (SELECT COALESCE((SELECT payment_plan FROM projects.projects WHERE id = #{pid}),'Free')) payment_plan,
            (SELECT COALESCE(ARRAY_AGG(DISTINCT method || '|' || host || '|' || canonical_path), '{}')
             FROM apis.endpoints WHERE project_id = #{pid} AND canonical_path IS NOT NULL
            ) canonical_paths
    from
      (select e.host hosts, e.hash endpoint_hashes, sh.hash shape_hashes, concat(rf.endpoint_hash,'<>', rf.field_category,'<>', rf.path) paths
        from apis.endpoints e
        left join apis.shapes sh ON sh.endpoint_hash = e.hash
        left join projects.redacted_fields rf ON rf.project_id = e.project_id
        where e.project_id = #{pid} AND sh.hash IS NOT null
       ) enp; |]


projectCacheByIdIO :: HPool.Pool -> ProjectId -> IO (Maybe ProjectCache)
projectCacheByIdIO hpool pid = runEff $ EHasql.runHasqlPool hpool $ runTime $ projectCacheById pid


insertProject :: DB es => CreateProject -> Eff es ()
insertProject p = do
  let (pId, pT, pD, pPP, pTZ, pSub, pFSI, pOrd, pDN, pWN, pEA, pErA) =
        (p.id, p.title, p.description, p.paymentPlan, p.timeZone, p.subId, p.firstSubItemId, p.orderId, p.dailyNotif, p.weeklyNotif, p.endpointAlerts, p.errorAlerts)
  EHasql.interpExecute_ [HI.sql| INSERT INTO projects.projects (id, title, description, payment_plan, time_zone, sub_id, first_sub_item_id, order_id, daily_notif, weekly_notif, endpoint_alerts, error_alerts) VALUES (#{pId}, #{pT}, #{pD}, #{pPP}, #{pTZ}, #{pSub}, #{pFSI}, #{pOrd}, #{pDN}, #{pWN}, #{pEA}, #{pErA}) |]


projectById :: DB es => ProjectId -> Eff es (Maybe Project)
projectById pid = EHasql.interpOne [HI.sql| select p.* from projects.projects p where id=#{pid}|]


projectByOrderId :: DB es => Text -> Eff es (Maybe Project)
projectByOrderId oid = EHasql.interpOne [HI.sql| select p.* from projects.projects p where order_id=#{oid}|]


projectBySubId :: DB es => Text -> Eff es (Maybe Project)
projectBySubId subId = EHasql.interpOne [HI.sql| select p.* from projects.projects p where sub_id=#{subId}|]


updateSubItemIdBySubId :: DB es => Text -> Text -> Eff es Int64
updateSubItemIdBySubId newItemId subId =
  EHasql.interpExecute [HI.sql| update projects.projects set first_sub_item_id=#{newItemId} where sub_id=#{subId}|]


getProjectByPhoneNumber :: DB es => Text -> Eff es (Maybe Project)
getProjectByPhoneNumber number = EHasql.interpOne [HI.sql| select p.* from projects.projects p where #{number}=Any(p.whatsapp_numbers) |]


activeProjects :: DB es => Eff es [Project]
activeProjects = EHasql.interp [HI.sql|SELECT p.* FROM projects.projects p WHERE p.active = TRUE AND p.deleted_at IS NULL|]


activeNonOnboardingProjectIds :: DB es => Eff es (V.Vector ProjectId)
activeNonOnboardingProjectIds =
  V.fromList <$> EHasql.interp [HI.sql|SELECT DISTINCT p.id FROM projects.projects p WHERE p.active = TRUE AND p.deleted_at IS NULL AND p.payment_plan != 'ONBOARDING'|]


recentlyActiveProjectIds :: DB es => UTCTime -> Eff es [ProjectId]
recentlyActiveProjectIds since =
  EHasql.interp [HI.sql|SELECT DISTINCT p.id FROM projects.projects p JOIN otel_logs_and_spans o ON o.project_id = p.id::text
           WHERE p.active = TRUE AND p.deleted_at IS NULL AND p.payment_plan != 'ONBOARDING' AND o.timestamp > #{since}::timestamptz - interval '24 hours'|]


newProjectsSince :: DB es => UTCTime -> Eff es [Project]
newProjectsSince since =
  EHasql.interp [HI.sql|SELECT p.* FROM projects.projects p WHERE p.created_at >= #{since}::timestamptz AND p.deleted_at IS NULL ORDER BY p.created_at DESC|]


selectProjectsForUser :: (DB es, Time :> es) => UserId -> Eff es [Project']
selectProjectsForUser uid = do
  now <- currentTime
  EHasql.interp [HI.sql|
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


updateProject :: DB es => CreateProject -> Eff es Int64
updateProject cp = do
  let (cT, cD, cPP, cSub, cFSI, cOrd, cTZ, cWN, cDN, cEA, cErA, cId) =
        (cp.title, cp.description, cp.paymentPlan, cp.subId, cp.firstSubItemId, cp.orderId, cp.timeZone, cp.weeklyNotif, cp.dailyNotif, cp.endpointAlerts, cp.errorAlerts, cp.id)
  EHasql.interpExecute
    [HI.sql|
       UPDATE projects.projects SET title=#{cT}, description=#{cD},
        payment_plan=#{cPP}, sub_id=#{cSub}, first_sub_item_id=#{cFSI}, order_id=#{cOrd},
        time_zone=#{cTZ}, weekly_notif=#{cWN}, daily_notif=#{cDN}, endpoint_alerts=#{cEA}, error_alerts=#{cErA} where id=#{cId};
        |]


updateProjectPricing :: DB es => ProjectId -> Text -> Text -> Text -> Text -> V.Vector Text -> Eff es Int64
updateProjectPricing pid paymentPlan subId firstSubItemId orderId stepsCompleted =
  EHasql.interpExecute [HI.sql| UPDATE projects.projects SET payment_plan=#{paymentPlan}, sub_id=#{subId}, first_sub_item_id=#{firstSubItemId}, order_id=#{orderId}, onboarding_steps_completed=#{stepsCompleted} where id=#{pid};|]


updateProjectBilling :: DB es => ProjectId -> Text -> Text -> Text -> Text -> Eff es Int64
updateProjectBilling pid paymentPlan subId firstSubItemId orderId =
  EHasql.interpExecute [HI.sql| UPDATE projects.projects SET payment_plan=#{paymentPlan}, sub_id=#{subId}, first_sub_item_id=#{firstSubItemId}, order_id=#{orderId} WHERE id=#{pid} AND (first_sub_item_id IS NULL OR first_sub_item_id = '');|]


updateProjectReportNotif :: DB es => ProjectId -> Text -> Eff es Int64
updateProjectReportNotif pid report_type =
  if report_type == "daily"
    then EHasql.interpExecute [HI.sql| UPDATE projects.projects SET daily_notif=(not daily_notif) WHERE id=#{pid};|]
    else EHasql.interpExecute [HI.sql| UPDATE projects.projects SET weekly_notif=(not weekly_notif) WHERE id=#{pid};|]


deleteProject :: (DB es, Time :> es) => ProjectId -> Eff es Int64
deleteProject pid = do
  now <- currentTime
  EHasql.interpExecute [HI.sql| UPDATE projects.projects SET deleted_at=#{now}, active=False where id=#{pid};|]


updateNotificationsChannel :: DB es => ProjectId -> [Text] -> [Text] -> [Text] -> Eff es Int64
updateNotificationsChannel pid channels phones emails = do
  let list = V.fromList channels
      vPhones = V.fromList phones
      vEmails = V.fromList emails
  EHasql.interpExecute [HI.sql| UPDATE projects.projects SET notifications_channel=#{list}::notification_channel_enum[], whatsapp_numbers=#{vPhones}, notify_emails=#{vEmails} WHERE id=#{pid};|]


-- | Idempotently append a channel (e.g. NSlack) to projects.notifications_channel.
enableNotificationChannel :: DB es => ProjectId -> NotificationChannel -> Eff es Int64
enableNotificationChannel pid ch =
  EHasql.interpExecute
    [HI.sql| UPDATE projects.projects
            SET notifications_channel = notifications_channel || ARRAY[#{ch}::notification_channel_enum]
            WHERE id = #{pid} AND NOT (#{ch}::notification_channel_enum = ANY(notifications_channel)); |]


updateUsageLastReported :: DB es => ProjectId -> ZonedTime -> Eff es Int64
updateUsageLastReported pid lastReported =
  EHasql.interpExecute [HI.sql| UPDATE projects.projects SET usage_last_reported=#{lastReported} WHERE id=#{pid};|]


updateProjectS3Bucket :: DB es => ProjectId -> Maybe ProjectS3Bucket -> Eff es Int64
updateProjectS3Bucket pid bucket =
  EHasql.interpExecute [HI.sql| UPDATE projects.projects SET s3_bucket=#{bucket} WHERE id=#{pid}|]


---------------------------------
type QueryLibItemId = UUIDId "querylib"


data QueryLibType = QLTHistory | QLTSaved
  deriving (Eq, Generic, NFData, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC "QLT" QueryLibType


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
queryLibInsert qKind pid uid qt qast title = do
  let qastJson = HI.AsJsonb qast
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
SELECT #{pid}, #{uid}, #{qKind}::projects.query_library_kind, #{qt}, #{qastJson}, #{title}
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
  getField = UUID.toText . lemonSubId


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
addSubscription s = do
  let (sId, sCr, sUp, sPid, sSub, sOrd, sFSub, sProd, sEmail) =
        (s.id, s.createdAt, s.updatedAt, s.projectId, s.subscriptionId, s.orderId, s.firstSubId, s.productName, s.userEmail)
  EHasql.interpExecute_ [HI.sql| INSERT INTO apis.subscriptions (id, created_at, updated_at, project_id, subscription_id, order_id, first_sub_id, product_name, user_email) VALUES (#{sId}, #{sCr}, #{sUp}, #{sPid}, #{sSub}, #{sOrd}, #{sFSub}, #{sProd}, #{sEmail}) |]


addDailyUsageReport :: DB es => ProjectId -> Int -> Eff es Int64
addDailyUsageReport pid total_requests =
  EHasql.interpExecute [HI.sql|INSERT INTO apis.daily_usage (project_id, total_requests) VALUES (#{pid}, #{total_requests}) |]


getTotalUsage :: DB es => ProjectId -> UTCTime -> Eff es Int64
getTotalUsage pid start = do
  results :: [Int64] <- EHasql.interp [HI.sql|SELECT COALESCE(SUM(total_requests), 0) FROM apis.daily_usage WHERE project_id = #{pid} AND created_at >= #{start}|]
  case results of
    [count] -> pure count
    _ -> pure 0


downgradeToFree :: DB es => Int -> Int -> Int -> Eff es Int64
downgradeToFree orderId' _subId _subItemId = do
  let oid = show orderId' :: Text
  EHasql.interpExecute [HI.sql|UPDATE projects.projects SET payment_plan = 'Free', first_sub_item_id = NULL, sub_id = NULL, order_id = NULL WHERE order_id = #{oid}|]


upgradeToPaid :: DB es => Int -> Int -> Int -> Eff es Int64
upgradeToPaid orderId' subId subItemId = do
  let (sId, sItemId, oId) = (show subId :: Text, show subItemId :: Text, show orderId' :: Text)
  EHasql.interpExecute [HI.sql|UPDATE projects.projects SET payment_plan = 'GraduatedPricing', sub_id = #{sId}, first_sub_item_id = #{sItemId} WHERE order_id = #{oId}|]


-- | >>> billingProvider (Just "sub_abc123")
-- StripeProvider
-- >>> billingProvider (Just "12345")
-- LemonSqueezyProvider
-- >>> billingProvider Nothing
-- NoBillingProvider
-- >>> billingProvider (Just "")
-- NoBillingProvider
data BillingProvider = StripeProvider | LemonSqueezyProvider | NoBillingProvider
  deriving stock (Eq, Show)


billingProvider :: Maybe Text -> BillingProvider
billingProvider = \case
  Just sid
    | "sub_" `T.isPrefixOf` sid -> StripeProvider
    | not (T.null sid) && T.all isDigit sid -> LemonSqueezyProvider
  _ -> NoBillingProvider


downgradeToFreeBySubId :: DB es => Text -> Eff es Int64
downgradeToFreeBySubId sid =
  EHasql.interpExecute [HI.sql|UPDATE projects.projects SET payment_plan = 'Free', first_sub_item_id = NULL, sub_id = NULL, order_id = NULL WHERE sub_id = #{sid}|]


-- order_id stores Stripe customer_id for Stripe users (semantic mismatch, TODO: add customer_id column)
updateStripeProjectBilling :: DB es => ProjectId -> Text -> Text -> Text -> Text -> Eff es Int64
updateStripeProjectBilling pid plan subId firstSubItemId customerId =
  EHasql.interpExecute [HI.sql|UPDATE projects.projects SET payment_plan = #{plan}, sub_id = #{subId}, first_sub_item_id = #{firstSubItemId}, order_id = #{customerId} WHERE id = #{pid}|]


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
    (FromField, ToField)
    via Aeson (Map Text Text)

deriving via Aeson (Map Text Text) instance HI.DecodeValue SessionData
deriving via Aeson (Map Text Text) instance HI.EncodeValue SessionData


newtype PSUser = PSUser {getUser :: User}
  deriving stock (Generic, Show)
  deriving newtype (NFData)
  deriving anyclass (Default)
  deriving
    (FromField, ToField)
    via Aeson User

deriving via Aeson User instance HI.DecodeValue PSUser
deriving via Aeson User instance HI.EncodeValue PSUser


newtype PSProjects = PSProjects {getProjects :: V.Vector Project}
  deriving stock (Generic, Show)
  deriving newtype (NFData)
  deriving anyclass (Default)
  deriving
    (FromField, ToField)
    via Aeson (V.Vector Project)

deriving via Aeson (V.Vector Project) instance HI.DecodeValue PSProjects
deriving via Aeson (V.Vector Project) instance HI.EncodeValue PSProjects


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
newPersistentSessionId = PersistentSessionId <$> UUID.genUUID


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


getSession
  :: EffReader.Reader (Headers '[Header "Set-Cookie" SetCookie] Session) :> es
  => Eff es Session
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
  }
  deriving stock (Generic, Show)


sessionAndProject
  :: (DB es, EffError.Error ServerError :> es, EffReader.Reader (Headers '[Header "Set-Cookie" SetCookie] Session) :> es)
  => ProjectId
  -> Eff es (Session, Project)
sessionAndProject pid = do
  sess <- getSession
  let projects = sess.persistentSession.projects.getProjects
  case V.find (\v -> v.id == pid) projects of
    Just p | p.paymentPlan /= "ONBOARDING" -> pure (sess, p)
    Just _ ->
      projectById pid >>= \case
        Just p -> pure (sess, p)
        Nothing -> throwError $ err302{errHeaders = [("Location", "/?missingProjectPermission")]}
    _
      | pid == UUIDId UUID.nil || sess.user.isSudo ->
          projectById pid >>= \case
            Just p -> pure (sess, p)
            Nothing -> throwError $ err302{errHeaders = [("Location", "/?missingProjectPermission")]}
    _ -> throwError $ err302{errHeaders = [("Location", "/?missingProjectPermission")]}


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
  deriving (Display, FromField, HI.DecodeValue, HI.EncodeValue, ToField) via WrappedEnumSC "AE" AuditEvent


logAudit :: DB es => ProjectId -> AuditEvent -> Maybe UserId -> Maybe Text -> Maybe AE.Value -> Eff es ()
logAudit pid event actorId actorEmail metadataM = do
  let metaJson = HI.AsJsonb <$> metadataM
  EHasql.interpExecute_ [HI.sql| INSERT INTO projects.audit_log (project_id, event, actor_id, actor_email, metadata) VALUES (#{pid}, #{event}, #{actorId}, #{actorEmail}, #{metaJson}) |]


logAuditS :: DB es => ProjectId -> AuditEvent -> Session -> Maybe AE.Value -> Eff es ()
logAuditS pid event sess = logAudit pid event (Just sess.user.id) (Just $ CI.original sess.user.email)
