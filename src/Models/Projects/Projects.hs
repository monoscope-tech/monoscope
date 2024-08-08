module Models.Projects.Projects (
  Project (..),
  Project' (..),
  ProjectId (..),
  CreateProject (..),
  ProjectRequestStats (..),
  NotificationChannel (..),
  insertProject,
  projectIdFromText,
  usersByProjectId,
  userByProjectId,
  selectProjectsForUser,
  projectRequestStatsByProject,
  updateProject,
  deleteProject,
  projectById,
  projectCacheById,
  updateProjectReportNotif,
  ProjectCache (..),
  updateNotificationsChannel,
  updateUsageLastReported,
)
where

import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (String))
import Data.Default
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, query, queryOne)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, Only (Only), ResultError (..), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField (fromField), returnError)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (Action (..), ToField (toField))
import Database.PostgreSQL.Transact (DBT)
import Deriving.Aeson qualified as DAE
import GHC.Records (HasField (getField))
import Models.Users.Users qualified as Users
import Relude
import Web.HttpApiData


newtype ProjectId = ProjectId {unProjectId :: UUID.UUID}
  deriving stock (Generic, Show, Read)
  deriving newtype (Eq, Ord, ToJSON, FromJSON, FromField, ToField, FromHttpApiData, Default, Hashable, NFData)
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
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


instance ToJSON NotificationChannel where
  toJSON NEmail = String "email"
  toJSON NSlack = String "slack"
  toJSON NDiscord = String "discord"


instance FromJSON NotificationChannel where
  parseJSON (String "email") = pure NEmail
  parseJSON (String "slack") = pure NSlack
  parseJSON (String "discord") = pure NDiscord
  parseJSON _ = fail "Invalid NotificationChannel value"


instance ToField NotificationChannel where
  toField NEmail = Escape "email"
  toField NSlack = Escape "slack"
  toField NDiscord = Escape "discord"


parsePermissions :: (Eq s, IsString s) => s -> NotificationChannel
parsePermissions "slack" = NSlack
parsePermissions _ = NEmail


instance FromField NotificationChannel where
  fromField f mdata =
    case mdata of
      Nothing -> returnError UnexpectedNull f ""
      Just bs -> pure $ parsePermissions bs


data Project = Project
  { id :: ProjectId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , deletedAt :: Maybe ZonedTime
  , active :: Bool
  , title :: Text
  , description :: Text
  , -- NOTE: We used to have hosts under project, but now hosts should be gotten from the endpoints.
    -- NOTE: If there's heavy need and usage, we caould create a view. Otherwise, the project cache is best, if it meets our needs.
    paymentPlan :: Text
  , questions :: Maybe Value
  , dailyNotif :: Bool
  , weeklyNotif :: Bool
  , timeZone :: Text
  , notificationsChannel :: Vector NotificationChannel
  , subId :: Maybe Text
  , firstSubItemId :: Maybe Text
  , orderId :: Maybe Text
  , usageLastReported :: ZonedTime
  , discordUrl :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, NFData)
  deriving
    (FromJSON, ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Project
  deriving
    (Entity)
    via (GenericEntity '[Schema "projects", TableName "projects", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Project)


-- FIXME: Why was this record created? And not the regular projects record?
data Project' = Project'
  { id :: ProjectId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , deletedAt :: Maybe ZonedTime
  , active :: Bool
  , title :: Text
  , description :: Text
  , -- NOTE: We used to have hosts under project, but now hosts should be gotten from the endpoints.
    -- NOTE: If there's heavy need and usage, we caould create a view. Otherwise, the project cache is best, if it meets our needs.
    paymentPlan :: Text
  , questions :: Maybe Value
  , dailyNotif :: Bool
  , weeklyNotif :: Bool
  , timeZone :: Text
  , notificationsChannel :: Vector NotificationChannel
  , subId :: Maybe Text
  , firstSubItemId :: Maybe Text
  , orderId :: Maybe Text
  , usageLastReported :: ZonedTime
  , discordUrl :: Maybe Text
  , hasIntegrated :: Bool
  , usersDisplayImages :: Vector Text
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


usersByProjectId :: ProjectId -> DBT IO (Vector Users.User)
usersByProjectId pid = query Select q (Only pid)
  where
    q =
      [sql| select u.id, u.created_at, u.updated_at, u.deleted_at, u.active, u.first_name, u.last_name, u.display_image_url, u.email, u.phone_number, u.is_sudo
                from users.users u join projects.project_members pm on (pm.user_id=u.id) where project_id=? and u.active IS True;|]


userByProjectId :: ProjectId -> Users.UserId -> DBT IO (Vector Users.User)
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


updateUsageLastReported :: ProjectId -> ZonedTime -> DBT IO Int64
updateUsageLastReported pid lastReported = execute Update q (lastReported, pid)
  where
    q = [sql| UPDATE projects.projects SET usage_last_reported=? WHERE id=?;|]
