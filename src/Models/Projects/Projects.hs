module Models.Projects.Projects (
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
  updateProject,
  deleteProject,
  updateProjectPricing,
  projectById,
  projectCacheById,
  projectCacheByIdIO,
  updateProjectReportNotif,
  ProjectCache (..),
  updateNotificationsChannel,
  updateUsageLastReported,
  updateProjectS3Bucket,
  QueryLibItemId,
  QueryLibType (..),
  QueryLibItem (..),
  queryLibHistoryForUser,
  queryLibInsert,
  queryLibTitleEdit,
  queryLibItemDelete,
  -- LemonSqueezy
  LemonSub (..),
  LemonSubId (..),
  addSubscription,
  getTotalUsage,
  addDailyUsageReport,
  upgradeToPaid,
  downgradeToFree,
)
where

import Data.Aeson qualified as AE
import Data.Default
import Data.Pool (Pool)
import Data.Time (UTCTime, ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity (_insert)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Connection, FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField, fromField, fromJSONField)
import Database.PostgreSQL.Simple.Newtypes
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField, toField, toJSONField)
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.PostgreSQL qualified as PG
import GHC.Records (HasField (getField))
import Models.Users.Users qualified as Users
import Pkg.DBUtils (WrappedEnumSC (..))
import Pkg.DeriveUtils (UUIDId (..), idFromText)
import Pkg.Parser.Stats (Section)
import Relude
import Servant (FromHttpApiData)
import System.DB (DB)
import Web.FormUrlEncoded (FromForm)


type ProjectId = UUIDId "project"


projectIdFromText :: Text -> Maybe ProjectId
projectIdFromText = idFromText


data NotificationChannel
  = NEmail
  | NSlack
  | NDiscord
  | NPhone
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.StripPrefix "N", DAE.CamelToSnake]] NotificationChannel
  deriving (FromField, ToField) via WrappedEnumSC "N" NotificationChannel


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
  deriving anyclass (FromRow, NFData)
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
  deriving anyclass (Default, FromRow, NFData)


data ProjectS3Bucket = ProjectS3Bucket
  { accessKey :: Text
  , secretKey :: Text
  , region :: Text
  , bucket :: Text
  , endpointUrl :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, FromForm, NFData)


instance FromField ProjectS3Bucket where
  fromField = fromJSONField


instance ToField ProjectS3Bucket where
  toField = toJSONField


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
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData)


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
projectCacheById :: DB es => ProjectId -> Eff es (Maybe ProjectCache)
projectCacheById pid = listToMaybe <$> PG.query q (pid, pid, pid, pid)
  where
    q =
      [sql| select  coalesce(ARRAY_AGG(DISTINCT hosts ORDER BY hosts ASC),'{}') hosts,
                    coalesce(ARRAY_AGG(DISTINCT endpoint_hashes ORDER BY endpoint_hashes ASC),'{}') endpoint_hashes,
                    coalesce(ARRAY_AGG(DISTINCT shape_hashes ORDER BY shape_hashes ASC),'{}'::text[]) shape_hashes,
                    coalesce(ARRAY_AGG(DISTINCT paths ORDER BY paths ASC),'{}') redacted_fields,
                    ( SELECT count(*) FROM otel_logs_and_spans
                     WHERE project_id=? AND timestamp > NOW() - INTERVAL '1' DAY
                    ) daily_event_count,
                    ( SELECT count(*) FROM telemetry.metrics
                     WHERE project_id=? AND timestamp > NOW() - INTERVAL '1' DAY
                    ) daily_metric_count,
                    (SELECT COALESCE((SELECT payment_plan FROM projects.projects WHERE id = ?),'Free')) payment_plan
            from
              (select e.host hosts, e.hash endpoint_hashes, sh.hash shape_hashes, concat(rf.endpoint_hash,'<>', rf.field_category,'<>', rf.path) paths
                from apis.endpoints e
                left join apis.shapes sh ON sh.endpoint_hash = e.hash
                left join projects.redacted_fields rf ON rf.project_id = e.project_id
                where e.project_id = ? AND sh.hash IS NOT null
               ) enp; |]


projectCacheByIdIO :: Pool Connection -> ProjectId -> IO (Maybe ProjectCache)
projectCacheByIdIO pool pid = runEff $ PG.runWithConnectionPool pool $ projectCacheById pid


insertProject :: DB es => CreateProject -> Eff es ()
insertProject p = void $ PG.execute (_insert @CreateProject) p


projectById :: DB es => ProjectId -> Eff es (Maybe Project)
projectById pid = listToMaybe <$> PG.query q (Only pid)
  where
    q = [sql| select p.* from projects.projects p where id=?|]


getProjectByPhoneNumber :: DB es => Text -> Eff es (Maybe Project)
getProjectByPhoneNumber number = listToMaybe <$> PG.query q (Only number)
  where
    q = [sql| select p.* from projects.projects p where ?=Any(p.whatsapp_numbers) |]


selectProjectsForUser :: DB es => Users.UserId -> Eff es [Project']
selectProjectsForUser uid = PG.query q (Only uid)
  where
    q =
      [sql|
        SELECT pp.*,
               EXISTS (
                    SELECT 1 FROM otel_logs_and_spans ols
                    WHERE ols.project_id = pp.id::text
                    AND ols.timestamp >= CURRENT_DATE - INTERVAL '30 days'
                    LIMIT 1
                ) as has_integrated,
               ARRAY_AGG('/api/avatar/' || us.id::text) OVER (PARTITION BY pp.id)
        FROM projects.projects AS pp
        JOIN projects.project_members AS ppm ON (pp.id = ppm.project_id)
        JOIN users.users AS us ON (us.id = ppm.user_id)
        WHERE ppm.user_id = ? AND pp.deleted_at IS NULL
        ORDER BY updated_at DESC
      |]


usersByProjectId :: DB es => ProjectId -> Eff es [Users.User]
usersByProjectId pid = PG.query q (Only pid)
  where
    q =
      [sql| select u.id, u.created_at, u.updated_at, u.deleted_at, u.active, u.first_name, u.last_name, u.display_image_url, u.email, u.phone_number, u.is_sudo
                from users.users u join projects.project_members pm on (pm.user_id=u.id) where project_id=? and u.active IS True;|]


updateProject :: DB es => CreateProject -> Eff es Int64
updateProject cp = PG.execute q (cp.title, cp.description, cp.paymentPlan, cp.subId, cp.firstSubItemId, cp.orderId, cp.timeZone, cp.weeklyNotif, cp.dailyNotif, cp.endpointAlerts, cp.errorAlerts, cp.id)
  where
    q =
      [sql|
       UPDATE projects.projects SET title=?, description=?,
        payment_plan=?, sub_id=?, first_sub_item_id=?, order_id=?,
        time_zone=?, weekly_notif=?, daily_notif=?, endpoint_alerts=?, error_alerts=? where id=?;
        |]


updateProjectPricing :: DB es => ProjectId -> Text -> Text -> Text -> Text -> V.Vector Text -> Eff es Int64
updateProjectPricing pid paymentPlan subId firstSubItemId orderId stepsCompleted =
  PG.execute q (paymentPlan, subId, firstSubItemId, orderId, stepsCompleted, pid)
  where
    q = [sql| UPDATE projects.projects SET payment_plan=?, sub_id=?, first_sub_item_id=?, order_id=?, onboarding_steps_completed=? where id=?;|]


updateProjectReportNotif :: DB es => ProjectId -> Text -> Eff es Int64
updateProjectReportNotif pid report_type = PG.execute q (Only pid)
  where
    q =
      if report_type == "daily"
        then [sql| UPDATE projects.projects SET daily_notif=(not daily_notif) WHERE id=?;|]
        else [sql| UPDATE projects.projects SET weekly_notif=(not weekly_notif) WHERE id=?;|]


deleteProject :: DB es => ProjectId -> Eff es Int64
deleteProject pid = PG.execute q (Only pid)
  where
    q =
      [sql| UPDATE projects.projects SET deleted_at=NOW(), active=False where id=?;|]


updateNotificationsChannel :: DB es => ProjectId -> [Text] -> [Text] -> Eff es Int64
updateNotificationsChannel pid channels phones = PG.execute q (list, V.fromList phones, pid)
  where
    list = V.fromList channels
    q = [sql| UPDATE projects.projects SET notifications_channel=?::notification_channel_enum[], whatsapp_numbers=? WHERE id=?;|]


updateUsageLastReported :: DB es => ProjectId -> ZonedTime -> Eff es Int64
updateUsageLastReported pid lastReported = PG.execute q (lastReported, pid)
  where
    q = [sql| UPDATE projects.projects SET usage_last_reported=? WHERE id=?;|]


updateProjectS3Bucket :: DB es => ProjectId -> Maybe ProjectS3Bucket -> Eff es Int64
updateProjectS3Bucket pid bucket = PG.execute q (bucket, pid)
  where
    q = [sql| UPDATE projects.projects SET s3_bucket=? WHERE id=?|]


---------------------------------
type QueryLibItemId = UUIDId "querylib"


data QueryLibType = QLTHistory | QLTSaved
  deriving (Eq, Generic, NFData, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.StripPrefix "QLT", DAE.CamelToSnake]] QueryLibType
  deriving (FromField, ToField) via WrappedEnumSC "QLT" QueryLibType


data QueryLibItem = QueryLibItem
  { id :: QueryLibItemId
  , projectId :: ProjectId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , userId :: Users.UserId
  , queryType :: QueryLibType
  , queryText :: Text
  , queryAst :: AE.Value
  , title :: Maybe Text
  , byMe :: Bool
  }
  deriving (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, FromRow, NFData, ToRow)


queryLibHistoryForUser :: DB es => ProjectId -> Users.UserId -> Eff es [QueryLibItem]
queryLibHistoryForUser pid uid = PG.query q (uid, uid, pid, uid, uid, pid, uid, pid, uid)
  where
    q =
      [sql|
(
  SELECT id, project_id, created_at, updated_at, user_id, query_type, query_text, query_ast, title,  user_id=?::uuid as byMe
  FROM projects.query_library
  WHERE user_id = ?::uuid AND project_id = ?::uuid AND query_type = 'history'
  ORDER BY created_at DESC
  LIMIT 50
)
UNION ALL
(
  SELECT id, project_id, created_at, updated_at, user_id, query_type, query_text, query_ast, title, user_id=?::uuid as byMe
  FROM projects.query_library
  WHERE user_id = ?::uuid AND project_id = ?::uuid AND query_type = 'saved'
  ORDER BY created_at DESC
  LIMIT 50
)
UNION ALL
(
  SELECT id, project_id, created_at, updated_at, user_id, query_type, query_text, query_ast,title, user_id=?::uuid as byMe
  FROM projects.query_library
  WHERE project_id = ?::uuid AND user_id != ?::uuid AND query_type = 'saved'
  ORDER BY created_at DESC
  LIMIT 50
);
    |]


queryLibInsert :: DB es => QueryLibType -> ProjectId -> Users.UserId -> Text -> [Section] -> Maybe Text -> Eff es ()
queryLibInsert qKind pid uid qt qast title = void $ PG.execute q (pid, uid, qKind, pid, uid, qKind, qt, Aeson qast, title, pid, uid, qKind, qt)
  where
    q =
      [sql|
WITH removed_old AS (
  DELETE FROM projects.query_library
  WHERE id IN (
    SELECT id
    FROM projects.query_library
    WHERE project_id = ? AND user_id = ? AND query_type = ?
    ORDER BY created_at ASC
    OFFSET 49
  )
)
INSERT INTO projects.query_library (project_id, user_id, query_type, query_text, query_ast, title)
SELECT ?, ?, ?, ?, ?, ?
WHERE NOT EXISTS (
  SELECT 1
  FROM projects.query_library
  WHERE project_id = ? AND user_id = ? AND query_type = ?
  AND query_text = ?
  ORDER BY created_at DESC
  LIMIT 1
)
ON CONFLICT DO NOTHING;
    |]


queryLibTitleEdit :: DB es => ProjectId -> Users.UserId -> Text -> Text -> Eff es ()
queryLibTitleEdit pid uid qId title = void $ PG.execute q (title, pid, uid, qId)
  where
    q = [sql|UPDATE projects.query_library SET title=? where project_id=? AND user_id=? AND id=?::uuid|]


queryLibItemDelete :: DB es => ProjectId -> Users.UserId -> Text -> Eff es ()
queryLibItemDelete pid uid qId = void $ PG.execute q (pid, uid, qId)
  where
    q = [sql|DELETE from projects.query_library where project_id=? AND user_id=? AND id=?::uuid|]


---------------------------------
-- LemonSqueezy subscription management
newtype LemonSubId = LemonSubId {lemonSubId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON, Default, Eq, FromField, FromHttpApiData, NFData, Ord, ToField)


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
addSubscription sub = void $ PG.execute (_insert @LemonSub) sub


addDailyUsageReport :: DB es => ProjectId -> Int -> Eff es Int64
addDailyUsageReport pid total_requests = PG.execute q (pid, total_requests)
  where
    q = [sql|INSERT INTO apis.daily_usage (project_id, total_requests) VALUES (?, ?) |]


getTotalUsage :: DB es => ProjectId -> UTCTime -> Eff es Int64
getTotalUsage pid start = do
  results <- PG.query q (pid, start)
  case results of
    [Only count] -> pure count
    _ -> pure 0
  where
    q = [sql|SELECT COALESCE(SUM(total_requests), 0) FROM apis.daily_usage WHERE project_id = ? AND created_at >= ?|]


downgradeToFree :: DB es => Int -> Int -> Int -> Eff es Int64
downgradeToFree orderId' subId subItemId = PG.execute q (show orderId', show subId, show subItemId)
  where
    q = [sql|UPDATE projects.projects SET payment_plan = 'FREE' WHERE order_id = ? AND sub_id = ? AND first_sub_item_id = ?|]


upgradeToPaid :: DB es => Int -> Int -> Int -> Eff es Int64
upgradeToPaid orderId' subId subItemId = PG.execute q (show orderId', show subId, show subItemId)
  where
    q = [sql|UPDATE projects.projects SET payment_plan = 'GraduatedPricing' WHERE order_id = ? AND sub_id = ? AND first_sub_item_id = ?|]
