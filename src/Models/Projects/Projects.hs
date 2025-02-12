module Models.Projects.Projects (
  Project (..),
  Project' (..),
  ProjectId (..),
  CreateProject (..),
  ProjectRequestStats (..),
  NotificationChannel (..),
  parseNotifChannel,
  OnboardingStep (..),
  insertProject,
  projectIdFromText,
  usersByProjectId,
  userByProjectId,
  selectProjectsForUser,
  projectRequestStatsByProject,
  updateOnboardingStepsCompleted,
  updateProject,
  deleteProject,
  updateProjectPricing,
  projectById,
  projectCacheById,
  updateProjectReportNotif,
  ProjectCache (..),
  updateNotificationsChannel,
  updateUsageLastReported,
  QueryLibItemId (..),
  QueryLibType (..),
  QueryLibItem (..),
  queryLibHistoryForUser,
  queryLibInsert,
  queryLibTitleEdit,
  queryLibItemDelete,
)
where

import Data.Aeson qualified as AE
import Data.Default
import Data.Time (UTCTime, ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, query, queryOne)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, Only (Only), ResultError (..), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField (fromField), returnError)
import Database.PostgreSQL.Simple.Newtypes
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (Action (..), ToField (toField))
import Database.PostgreSQL.Transact (DBT)
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import GHC.Records (HasField (getField))
import Language.Haskell.TH.Syntax qualified as THS
import Models.Users.Users qualified as Users
import Pkg.DBUtils (WrappedEnumSC (..))
import Pkg.Parser.Stats (Section)
import Relude
import Web.HttpApiData


newtype ProjectId = ProjectId {unProjectId :: UUID.UUID}
  deriving stock (Generic, Show, Read, THS.Lift)
  deriving newtype (Eq, Ord, AE.ToJSON, AE.FromJSON, FromField, ToField, FromHttpApiData, Default, Hashable, NFData)
  deriving anyclass (FromRow, ToRow)


instance HasField "unwrap" ProjectId UUID.UUID where
  getField = coerce


instance HasField "toText" ProjectId Text where
  getField = UUID.toText . unProjectId


projectIdFromText :: Text -> Maybe ProjectId
projectIdFromText pid = ProjectId <$> UUID.fromText pid


data NotificationChannel
  = NEmail
  | NSlack
  | NDiscord
  | NPhone
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


instance AE.ToJSON NotificationChannel where
  toJSON NEmail = AE.String "email"
  toJSON NSlack = AE.String "slack"
  toJSON NDiscord = AE.String "discord"
  toJSON NPhone = AE.String "phone"


instance AE.FromJSON NotificationChannel where
  parseJSON (AE.String "email") = pure NEmail
  parseJSON (AE.String "slack") = pure NSlack
  parseJSON (AE.String "discord") = pure NDiscord
  parseJSON (AE.String "phone") = pure NPhone
  parseJSON _ = fail "Invalid NotificationChannel value"


instance ToField NotificationChannel where
  toField NEmail = Escape "email"
  toField NSlack = Escape "slack"
  toField NDiscord = Escape "discord"
  toField NPhone = Escape "phone"


parsePermissions :: (Eq s, IsString s) => s -> NotificationChannel
parsePermissions "slack" = NSlack
parsePermissions _ = NEmail


parseNotifChannel :: NotificationChannel -> Text
parseNotifChannel NSlack = "slack"
parseNotifChannel NDiscord = "discord"
parseNotifChannel NEmail = "email"
parseNotifChannel NPhone = "phone"


data OnboardingStep = Info | Survey | CreateMonitor | NotifChannel | Integration | Pricing | Complete
  deriving stock (Eq, Generic, Show, Read)
  deriving (AE.FromJSON, AE.ToJSON, NFData, ToField, FromField) via OnboardingStep


instance FromField NotificationChannel where
  fromField f mdata =
    case mdata of
      Nothing -> returnError UnexpectedNull f ""
      Just bs -> pure $ parsePermissions bs


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
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, NFData)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Project
  deriving
    (Entity)
    via (GenericEntity '[Schema "projects", TableName "projects", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Project)


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
  , hasIntegrated :: Bool
  , usersDisplayImages :: V.Vector Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, Default, NFData)


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
  , weeklyRequestCount :: Int
  , paymentPlan :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow)


data CreateProject = CreateProject
  { id :: ProjectId
  , title :: Text
  , description :: Text
  , paymentPlan :: Text
  , timeZone :: Text
  , subId :: Maybe Text
  , firstSubItemId :: Maybe Text
  , orderId :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "projects", TableName "projects", PrimaryKey "id", FieldModifiers '[CamelToSnake]] CreateProject)


-- FIXME: We currently return an object with empty vectors when nothing was found.
projectCacheById :: ProjectId -> DBT IO (Maybe ProjectCache)
projectCacheById pid = queryOne Select q (pid, pid, pid)
  where
    q =
      [sql| select  coalesce(ARRAY_AGG(DISTINCT hosts ORDER BY hosts ASC),'{}') hosts,
                    coalesce(ARRAY_AGG(DISTINCT endpoint_hashes ORDER BY endpoint_hashes ASC),'{}') endpoint_hashes,
                    coalesce(ARRAY_AGG(DISTINCT shape_hashes ORDER BY shape_hashes ASC),'{}'::text[]) shape_hashes,
                    coalesce(ARRAY_AGG(DISTINCT paths ORDER BY paths ASC),'{}') redacted_fields,
                    ( SELECT count(*) FROM apis.request_dumps
                     WHERE project_id=? AND created_at > NOW() - INTERVAL '7' DAY
                    ) weekly_request_count,
                    (SELECT COALESCE((SELECT payment_plan FROM projects.projects WHERE id = ?),'Free')) payment_plan
            from
              (select e.host hosts, e.hash endpoint_hashes, sh.hash shape_hashes, concat(rf.endpoint_hash,'<>', rf.field_category,'<>', rf.path) paths
                from apis.endpoints e
                left join apis.shapes sh ON sh.endpoint_hash = e.hash
                left join projects.redacted_fields rf ON rf.project_id = e.project_id
                where e.project_id = ? AND sh.hash IS NOT null
               ) enp; |]


insertProject :: CreateProject -> DBT IO ()
insertProject = insert @CreateProject


projectById :: ProjectId -> DBT IO (Maybe Project)
projectById = queryOne Select q
  where
    q = [sql| select p.* from projects.projects p where id=?|]


selectProjectsForUser :: Users.UserId -> DBT IO (V.Vector Project')
selectProjectsForUser = query Select q
  where
    q =
      [sql|
        SELECT pp.*,
               (SELECT COUNT(*) > 0 FROM apis.request_dumps rd
                WHERE rd.project_id = pp.id) has_integrated,
               ARRAY_AGG(us.display_image_url) OVER (PARTITION BY pp.id)
        FROM projects.projects AS pp
        JOIN projects.project_members AS ppm ON (pp.id = ppm.project_id)
        JOIN users.users AS us ON (us.id = ppm.user_id)
        WHERE ppm.user_id = ? AND pp.deleted_at IS NULL
        ORDER BY updated_at DESC
      |]


usersByProjectId :: ProjectId -> DBT IO (V.Vector Users.User)
usersByProjectId pid = query Select q (Only pid)
  where
    q =
      [sql| select u.id, u.created_at, u.updated_at, u.deleted_at, u.active, u.first_name, u.last_name, u.display_image_url, u.email, u.phone_number, u.is_sudo
                from users.users u join projects.project_members pm on (pm.user_id=u.id) where project_id=? and u.active IS True;|]


userByProjectId :: ProjectId -> Users.UserId -> DBT IO (V.Vector Users.User)
userByProjectId pid user_id = query Select q (user_id, pid)
  where
    q =
      [sql| select u.id, u.created_at, u.updated_at, u.deleted_at, u.active, u.first_name, u.last_name, u.display_image_url, u.email, u.phone_number,  u.is_sudo
                from users.users u join projects.project_members pm on (pm.user_id= ?) where project_id=? and u.active IS True;|]


updateProject :: CreateProject -> DBT IO Int64
updateProject cp = do
  execute Update q (cp.title, cp.description, cp.paymentPlan, cp.subId, cp.firstSubItemId, cp.orderId, cp.timeZone, cp.id)
  where
    q =
      [sql| UPDATE projects.projects SET title=?,  description=?, payment_plan=?, sub_id=?, first_sub_item_id=?, order_id=?, time_zone=? where id=?;|]


updateProjectPricing :: ProjectId -> Text -> Text -> Text -> Text -> V.Vector Text -> DBT IO Int64
updateProjectPricing pid paymentPlan subId firstSubItemId orderId stepsCompleted = do
  execute Update q (paymentPlan, subId, firstSubItemId, orderId, stepsCompleted, pid)
  where
    q = [sql| UPDATE projects.projects SET payment_plan=?, sub_id=?, first_sub_item_id=?, order_id=?, onboarding_steps_completed=? where id=?;|]


updateProjectReportNotif :: ProjectId -> Text -> DBT IO Int64
updateProjectReportNotif pid report_type = do
  execute Update q (Only pid)
  where
    q =
      if report_type == "daily"
        then [sql| UPDATE projects.projects SET daily_notif=(not daily_notif) WHERE id=?;|]
        else [sql| UPDATE projects.projects SET weekly_notif=(not weekly_notif) WHERE id=?;|]


deleteProject :: ProjectId -> DBT IO Int64
deleteProject pid = do
  execute Update q pid
  where
    q =
      [sql| UPDATE projects.projects SET deleted_at=NOW(), active=False where id=?;|]


data ProjectRequestStats = ProjectRequestStats
  { projectId :: ProjectId
  , min :: Double
  , p50 :: Double
  , p75 :: Double
  , p90 :: Double
  , p95 :: Double
  , p99 :: Double
  , max :: Double
  , totalTime :: Double
  , totalRequests :: Int
  , totalEndpoints :: Int
  , totalEndpointsLastWeek :: Int
  , totalShapes :: Int
  , totalShapesLastWeek :: Int
  , totalAnomalies :: Int
  , totalAnomaliesLastWeek :: Int
  , requestsPerMin :: Int
  , requestsPerMinLastWeek :: Int
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromRow, ToRow, Default, NFData)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "project_request_stats", PrimaryKey "project_id", FieldModifiers '[CamelToSnake]] ProjectRequestStats)


projectRequestStatsByProject :: ProjectId -> DBT IO (Maybe ProjectRequestStats)
projectRequestStatsByProject = selectById @ProjectRequestStats


updateNotificationsChannel :: ProjectId -> [Text] -> Maybe Text -> DBT IO Int64
updateNotificationsChannel pid channels discordUrl = execute Update q (list, discordUrl, pid)
  where
    list = V.fromList channels
    q = [sql| UPDATE projects.projects SET notifications_channel=?::notification_channel_enum[], discord_url=? WHERE id=?;|]


updateOnboardingStepsCompleted :: ProjectId -> V.Vector Text -> DBT IO Int64
updateOnboardingStepsCompleted pid steps = execute Update q (steps, pid)
  where
    q = [sql| UPDATE projects.projects SET onboarding_steps_completed=? WHERE id=?;|]


updateUsageLastReported :: ProjectId -> ZonedTime -> DBT IO Int64
updateUsageLastReported pid lastReported = execute Update q (lastReported, pid)
  where
    q = [sql| UPDATE projects.projects SET usage_last_reported=? WHERE id=?;|]


---------------------------------
newtype QueryLibItemId = QueryLibItemId {unQueryLibItemId :: UUID.UUID}
  deriving stock (Generic, Show, Read)
  deriving newtype (Eq, Ord, FromField, ToField, FromHttpApiData, Default, Hashable, NFData)


instance HasField "unwrap" QueryLibItemId UUID.UUID where
  getField = coerce


instance HasField "toText" QueryLibItemId Text where
  getField = UUID.toText . unQueryLibItemId


data QueryLibType = QLTHistory | QLTSaved
  deriving (Eq, Generic, Show, Read, NFData)
  deriving (ToField, FromField) via WrappedEnumSC "QLT" QueryLibType


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
  deriving (Show, Generic, Eq)
  deriving anyclass (FromRow, ToRow, NFData)


queryLibHistoryForUser :: DB :> es => ProjectId -> Users.UserId -> Eff es (V.Vector QueryLibItem)
queryLibHistoryForUser pid uid = dbtToEff $ query Select q (uid, uid, pid, uid, uid, pid, uid, pid, uid)
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


queryLibInsert :: DB :> es => QueryLibType -> ProjectId -> Users.UserId -> Text -> [Section] -> Maybe Text -> Eff es ()
queryLibInsert qKind pid uid qt qast title = void $ dbtToEff $ execute Insert q (pid, uid, qKind, pid, uid, qKind, qt, Aeson qast, title, pid, uid, qKind, qt)
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


queryLibTitleEdit :: DB :> es => ProjectId -> Users.UserId -> Text -> Text -> Eff es ()
queryLibTitleEdit pid uid qId title = void $ dbtToEff $ execute Update q (title, pid, uid, qId)
  where
    q = [sql|UPDATE projects.query_library SET title=? where project_id=? AND user_id=? AND id=?::uuid|]


queryLibItemDelete :: DB :> es => ProjectId -> Users.UserId -> Text -> Eff es ()
queryLibItemDelete pid uid qId = void $ dbtToEff $ execute Delete q (pid, uid, qId)
  where
    q = [sql|DELETE from projects.query_library where project_id=? AND user_id=? AND id=?::uuid|]
