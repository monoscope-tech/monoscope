module Models.Projects.ProjectMembers (
  ProjectMembers (..),
  insertProjectMembers,
  CreateProjectMembers (..),
  ProjectMemberVM (..),
  ProjectMemberWithStatusVM (..),
  Permissions (..),
  selectActiveProjectMembers,
  getActiveProjectMemberByUserId,
  selectAllProjectMembers,
  getUserPermission,
  updateProjectMembersPermissons,
  softDeleteProjectMembers,
  deactivateNonOwnerMembers,
  activateAllMembers,
  TeamDetails (..),
  createTeam,
  createEveryoneTeam,
  updateTeam,
  getTeams,
  getTeamByHandle,
  getTeamsByHandles,
  getTeamsVM,
  getEveryoneTeam,
  TeamVM (..),
  deleteTeams,
  getTeamsById,
  getTeamById,
  Team (..),
  TeamMemberVM (..),
  teamToDetails,
  addSlackChannelToEveryoneTeam,
  addDiscordChannelToEveryoneTeam,
  addPagerdutyServiceToEveryoneTeam,
  removeSlackChannelsFromEveryoneTeam,
  removeDiscordChannelsFromEveryoneTeam,
  removePagerdutyServicesFromEveryoneTeam,
  setEveryoneTeamEmails,
  setEveryoneTeamPhones,
  setEveryoneTeamDisabledChannels,
  isChannelEnabled,
  isEveryoneChannelEnabled,
  teamHasAnyEnabledChannel,
  resolveTeamEmails,
) where

import Data.Aeson qualified as AE
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.Effectful.Hasql qualified as Hasql
import Data.OpenApi (ToSchema)
import Data.Text.Display (Display)
import Data.Time (UTCTime, ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types (
  CamelToSnake,
  Entity,
  FieldModifiers,
  GenericEntity,
  PrimaryKey,
  Schema,
  TableName,
 )
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (ToField)
import Effectful (Eff, type (:>))
import Effectful.Log (Log)
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (WrappedEnumSC (..), selectFrom)
import Relude
import Servant (FromHttpApiData)
import System.Logging qualified as Log
import System.Types (DB)


data Permissions
  = PView
  | PEdit
  | PAdmin
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Read, Show)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON, Display, FromField, FromHttpApiData, ToField, ToSchema) via WrappedEnumSC "P" Permissions
  deriving (HI.DecodeValue, HI.EncodeValue) via WrappedEnumSC "P" Permissions


instance HI.DecodeRow Permissions where
  decodeRow = HI.getOneColumn <$> HI.decodeRow


data ProjectMembers = ProjectMembers
  { id :: UUID.UUID
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , deletedAt :: Maybe ZonedTime
  , active :: Bool
  , projectId :: Projects.ProjectId
  , userId :: Projects.UserId
  , permission :: Permissions
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "projects", TableName "project_members", PrimaryKey "id", FieldModifiers '[CamelToSnake]] ProjectMembers)


data CreateProjectMembers = CreateProjectMembers
  { projectId :: Projects.ProjectId
  , userId :: Projects.UserId
  , permission :: Permissions
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "projects", TableName "project_members", PrimaryKey "id", FieldModifiers '[CamelToSnake]] CreateProjectMembers)


insertProjectMembers :: DB es => [CreateProjectMembers] -> Eff es Int64
insertProjectMembers [] = pure 0
insertProjectMembers members =
  Hasql.interpExecute
    [HI.sql| INSERT INTO projects.project_members(project_id, user_id, permission)
           SELECT * FROM unnest(#{pidsV}::uuid[], #{uidsV}::uuid[], #{permsV}::projects.project_permissions[])
           ON CONFLICT (project_id, user_id) DO UPDATE SET active = TRUE, deleted_at = NULL |]
  where
    v = V.fromList members
    pidsV = V.map (.projectId) v
    uidsV = V.map (.userId) v
    permsV = V.map (.permission) v


data ProjectMemberVM = ProjectMemberVM
  { id :: UUID.UUID
  , userId :: Projects.UserId
  , permission :: Permissions
  , email :: CI Text
  , first_name :: Text
  , last_name :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, NFData)


selectActiveProjectMembers :: DB es => Projects.ProjectId -> Eff es [ProjectMemberVM]
selectActiveProjectMembers pid =
  Hasql.interp
    [HI.sql| SELECT pm.id, pm.user_id, pm.permission, us.email, us.first_name, us.last_name FROM projects.project_members pm
           JOIN users.users us ON (pm.user_id=us.id)
           WHERE pm.project_id=#{pid}::uuid AND pm.active=TRUE
           ORDER BY pm.created_at ASC |]


getActiveProjectMemberByUserId :: DB es => Projects.ProjectId -> UUID.UUID -> Eff es (Maybe ProjectMemberVM)
getActiveProjectMemberByUserId pid uid =
  Hasql.interpOne
    [HI.sql|
      SELECT pm.id, pm.user_id, pm.permission, us.email, us.first_name, us.last_name
      FROM projects.project_members pm JOIN users.users us ON (pm.user_id = us.id)
      WHERE pm.project_id = #{pid} AND pm.user_id = #{uid} AND pm.active = TRUE |]


getUserPermission :: DB es => Projects.ProjectId -> Projects.UserId -> Eff es (Maybe Permissions)
getUserPermission pid uid =
  Hasql.interp
    [HI.sql| SELECT permission FROM projects.project_members
           WHERE project_id = #{pid} AND user_id = #{uid} AND active = TRUE |]


updateProjectMembersPermissons :: DB es => [(UUID.UUID, Permissions)] -> Eff es ()
updateProjectMembersPermissons [] = pass
updateProjectMembersPermissons vals =
  Hasql.interpExecute_
    [HI.sql| UPDATE projects.project_members pm
           SET permission = u.perm::projects.project_permissions
           FROM unnest(#{idsV}::uuid[], #{permsV}::text[]) AS u(id, perm)
           WHERE pm.id = u.id |]
  where
    (ids, perms) = unzip vals
    (idsV, permsV) = (V.fromList ids, V.fromList perms)


softDeleteProjectMembers :: (DB es, Time :> es) => NonEmpty UUID.UUID -> Eff es ()
softDeleteProjectMembers ids = do
  now <- Time.currentTime
  let idsVec = V.fromList $ toList ids
  Hasql.interpExecute_
    [HI.sql| UPDATE projects.project_members SET active = FALSE, deleted_at = #{now} WHERE id = ANY(#{idsVec}::uuid[]) |]


deactivateNonOwnerMembers :: DB es => Projects.ProjectId -> Eff es Int64
deactivateNonOwnerMembers pid =
  Hasql.interpExecute
    [HI.sql| UPDATE projects.project_members SET active = FALSE WHERE project_id = #{pid}
           AND user_id != (SELECT user_id FROM projects.project_members WHERE project_id = #{pid} ORDER BY created_at LIMIT 1) |]


activateAllMembers :: DB es => Projects.ProjectId -> Eff es Int64
activateAllMembers pid =
  Hasql.interpExecute
    [HI.sql| UPDATE projects.project_members SET active = TRUE WHERE project_id = #{pid} |]


data ProjectMemberWithStatusVM = ProjectMemberWithStatusVM
  { id :: UUID.UUID
  , userId :: Projects.UserId
  , permission :: Permissions
  , email :: CI Text
  , first_name :: Text
  , last_name :: Text
  , active :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, NFData)


selectAllProjectMembers :: DB es => Projects.ProjectId -> Eff es [ProjectMemberWithStatusVM]
selectAllProjectMembers pid =
  Hasql.interp
    [HI.sql| SELECT pm.id, pm.user_id, pm.permission, us.email, us.first_name, us.last_name, pm.active
           FROM projects.project_members pm JOIN users.users us ON (pm.user_id=us.id)
           WHERE pm.project_id=#{pid}::uuid AND pm.deleted_at IS NULL AND pm.active = TRUE ORDER BY pm.created_at ASC |]


data TeamDetails = TeamDetails
  { name :: Text
  , description :: Text
  , handle :: Text
  , members :: V.Vector Projects.UserId
  , notifyEmails :: V.Vector Text
  , slackChannels :: V.Vector Text
  , discordChannels :: V.Vector Text
  , phoneNumbers :: V.Vector Text
  , pagerdutyServices :: V.Vector Text
  , disabledChannels :: V.Vector Text
  }
  deriving stock (Eq, Generic, Show)


createTeam :: DB es => Projects.ProjectId -> Maybe Projects.UserId -> TeamDetails -> Eff es (Maybe UUID.UUID)
createTeam pid uidM TeamDetails{name, description, handle, members, notifyEmails, slackChannels, discordChannels, phoneNumbers, pagerdutyServices, disabledChannels}
  | handle == "everyone" = pure Nothing -- Prevent creating team with reserved handle
  | otherwise =
      Hasql.interpOne
        [HI.sql| INSERT INTO projects.teams
               (project_id, created_by, name, description, handle, members, notify_emails, slack_channels, discord_channels, phone_numbers, pagerduty_services, disabled_channels)
               VALUES (#{pid}, #{uidM}, #{name}, #{description}, #{handle}, #{members}::uuid[], #{notifyEmails}, #{slackChannels}, #{discordChannels}, #{phoneNumbers}, #{pagerdutyServices}, #{disabledChannels})
               ON CONFLICT (project_id, handle) DO NOTHING
               RETURNING id |]


createEveryoneTeam :: DB es => Projects.ProjectId -> Projects.UserId -> Eff es Int64
createEveryoneTeam pid uid =
  Hasql.interpExecute
    [HI.sql| INSERT INTO projects.teams
           (project_id, created_by, name, description, handle, is_everyone, members, notify_emails, slack_channels, discord_channels, phone_numbers, pagerduty_services, disabled_channels)
           VALUES (#{pid}, #{uid}, 'Everyone', 'All project members and configured integrations', 'everyone', TRUE,
                   '{}'::uuid[], '{}'::text[], '{}'::text[], '{}'::text[], '{}'::text[], '{}'::text[], '{}'::text[])
           ON CONFLICT (project_id, handle) DO NOTHING |]


getEveryoneTeam :: DB es => Projects.ProjectId -> Eff es (Maybe Team)
getEveryoneTeam pid =
  Hasql.interp
    (selectFrom @Team <> [HI.sql| WHERE project_id = #{pid} AND is_everyone = TRUE AND deleted_at IS NULL |])


updateTeam :: DB es => Projects.ProjectId -> UUID.UUID -> TeamDetails -> Eff es Int64
updateTeam pid tid TeamDetails{name, description, handle, members, notifyEmails, slackChannels, discordChannels, phoneNumbers, pagerdutyServices, disabledChannels} =
  Hasql.interpExecute
    [HI.sql| UPDATE projects.teams
           SET name = #{name}, description = #{description}, handle = #{handle}, members = #{members}::uuid[], notify_emails = #{notifyEmails}, slack_channels = #{slackChannels}, discord_channels = #{discordChannels}, phone_numbers = #{phoneNumbers}, pagerduty_services = #{pagerdutyServices}, disabled_channels = #{disabledChannels}
           WHERE project_id = #{pid} AND id = #{tid} |]


data Team = Team
  { id :: UUID.UUID
  , name :: Text
  , description :: Text
  , handle :: Text
  , members :: V.Vector Projects.UserId
  , created_by :: Maybe Projects.UserId
  , created_at :: UTCTime
  , updated_at :: UTCTime
  , notify_emails :: V.Vector Text
  , slack_channels :: V.Vector Text
  , discord_channels :: V.Vector Text
  , phone_numbers :: V.Vector Text
  , pagerduty_services :: V.Vector Text
  , disabled_channels :: V.Vector Text
  , is_everyone :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, NFData)
  deriving (Entity) via (GenericEntity '[Schema "projects", TableName "teams", PrimaryKey "id"] Team)


getTeams :: DB es => Projects.ProjectId -> Eff es [Team]
getTeams pid =
  Hasql.interp
    (selectFrom @Team <> [HI.sql| WHERE project_id = #{pid} AND deleted_at IS NULL |])


getTeamsVM :: DB es => Projects.ProjectId -> Eff es [TeamVM]
getTeamsVM pid =
  Hasql.interp
    [HI.sql|
      SELECT
        t.id,
        t.created_at,
        t.updated_at,
        t.created_by,
        t.name,
        t.handle,
        t.description,
        t.notify_emails,
        t.slack_channels,
        t.discord_channels,
        t.phone_numbers,
        t.pagerduty_services,
        t.disabled_channels,
        t.is_everyone,
        COALESCE(
          array_agg(
            jsonb_build_object(
              'memberId', u.id,
              'memberName', concat_ws(' ', u.first_name, u.last_name),
              'memberEmail', u.email,
              'memberAvatar', '/api/avatar/' || u.id::text
            ) ORDER BY u.first_name, u.last_name
          ) FILTER (WHERE u.id IS NOT NULL),
          '{}'
        ) AS members
      FROM projects.teams t
      LEFT JOIN unnest(t.members) AS mid ON true
      LEFT JOIN users.users u ON u.id = mid
      WHERE t.project_id = #{pid} AND t.deleted_at IS NULL
      GROUP BY t.id, t.created_at, t.updated_at, t.created_by, t.name, t.handle,
               t.description, t.notify_emails, t.slack_channels, t.discord_channels, t.phone_numbers, t.pagerduty_services, t.disabled_channels, t.is_everyone
      ORDER BY t.is_everyone DESC, t.created_at ASC
    |]


data TeamVM = TeamVM
  { id :: UUID.UUID
  , created_at :: UTCTime
  , updated_at :: UTCTime
  , created_by :: Maybe Projects.UserId
  , name :: Text
  , handle :: Text
  , description :: Text
  , notify_emails :: V.Vector Text
  , slack_channels :: V.Vector Text
  , discord_channels :: V.Vector Text
  , phone_numbers :: V.Vector Text
  , pagerduty_services :: V.Vector Text
  , disabled_channels :: V.Vector Text
  , is_everyone :: Bool
  , members :: V.Vector TeamMemberVM
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, HI.DecodeRow, NFData)


data TeamMemberVM = TeamMemberVM
  { memberId :: UUID.UUID
  , memberEmail :: Text
  , memberName :: Text
  , memberAvatar :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, NFData)
  deriving (FromField) via Aeson TeamMemberVM
  deriving (HI.DecodeValue) via HI.AsJsonb TeamMemberVM


getTeamByHandle :: DB es => Projects.ProjectId -> Text -> Eff es (Maybe TeamVM)
getTeamByHandle pid handle = listToMaybe <$> getTeamsByHandles pid [handle]


deleteTeams :: (DB es, Time :> es) => Projects.ProjectId -> V.Vector UUID.UUID -> Eff es ()
deleteTeams pid tids
  | V.null tids = pass
  | otherwise = do
      now <- Time.currentTime
      Hasql.interpExecute_
        [HI.sql| UPDATE projects.teams SET deleted_at = #{now} WHERE project_id = #{pid} AND id = ANY(#{tids}::uuid[]) AND is_everyone = FALSE |]


getTeamsById :: DB es => Projects.ProjectId -> V.Vector UUID.UUID -> Eff es [Team]
getTeamsById pid tids =
  if V.null tids
    then pure []
    else
      Hasql.interp
        (selectFrom @Team <> [HI.sql| WHERE project_id = #{pid} AND id = ANY(#{tids}::uuid[]) AND deleted_at IS NULL |])


getTeamById :: DB es => Projects.ProjectId -> UUID.UUID -> Eff es (Maybe Team)
getTeamById pid tid =
  Hasql.interpOne
    (selectFrom @Team <> [HI.sql| WHERE project_id = #{pid} AND id = #{tid} AND deleted_at IS NULL |])


-- | Bulk fetch teams by handles - more efficient than mapping getTeamByHandle
getTeamsByHandles :: DB es => Projects.ProjectId -> [Text] -> Eff es [TeamVM]
getTeamsByHandles _ [] = pure []
getTeamsByHandles pid handles =
  Hasql.interp
    $ let handlesVec = V.fromList handles
       in [HI.sql|
      SELECT
        t.id,
        t.created_at,
        t.updated_at,
        t.created_by,
        t.name,
        t.handle,
        t.description,
        t.notify_emails,
        t.slack_channels,
        t.discord_channels,
        t.phone_numbers,
        t.pagerduty_services,
        t.disabled_channels,
        t.is_everyone,
        COALESCE(
          array_agg(
            jsonb_build_object(
              'memberId', u.id,
              'memberName', concat_ws(' ', u.first_name, u.last_name),
              'memberEmail', u.email,
              'memberAvatar', '/api/avatar/' || u.id::text
            ) ORDER BY u.first_name, u.last_name
          ) FILTER (WHERE u.id IS NOT NULL),
          '{}'
        ) AS members
      FROM projects.teams t
      LEFT JOIN unnest(t.members) AS mid ON true
      LEFT JOIN users.users u ON u.id = mid
      WHERE t.project_id = #{pid} AND t.handle = ANY(#{handlesVec}) AND t.deleted_at IS NULL
      GROUP BY t.id, t.created_at, t.updated_at, t.created_by, t.name, t.handle,
               t.description, t.notify_emails, t.slack_channels, t.discord_channels, t.phone_numbers, t.pagerduty_services, t.disabled_channels, t.is_everyone
    |]


-- | Convert Team to TeamDetails for updates
teamToDetails :: Team -> TeamDetails
teamToDetails t = TeamDetails{name = t.name, description = t.description, handle = t.handle, members = t.members, notifyEmails = t.notify_emails, slackChannels = t.slack_channels, discordChannels = t.discord_channels, phoneNumbers = t.phone_numbers, pagerdutyServices = t.pagerduty_services, disabledChannels = t.disabled_channels}


-- | Run a TeamDetails update against the @everyone team. When the team is
-- missing we log attention rather than silently no-opping — callers of the
-- dependents (disconnect handlers, onboarding, integrations save) assume the
-- write succeeded and surface a success toast; silence here hides real bugs.
modifyEveryoneTeamDetails :: (DB es, Log :> es) => Projects.ProjectId -> (TeamDetails -> TeamDetails) -> Eff es ()
modifyEveryoneTeamDetails pid f =
  getEveryoneTeam pid >>= \case
    Nothing -> Log.logAttention "modifyEveryoneTeamDetails: no @everyone team; write ignored" (AE.object ["project_id" AE..= pid])
    Just team -> void $ updateTeam pid team.id $ f (teamToDetails team)


-- | Race-free idempotent append to one of @everyone's text[] fields. Returns True
-- if the value was actually added, False if it was already present (or there was
-- no team). Single UPDATE, no read-modify-write window.
addSlackChannelToEveryoneTeam
  , addDiscordChannelToEveryoneTeam
  , addPagerdutyServiceToEveryoneTeam
    :: DB es => Projects.ProjectId -> Text -> Eff es Bool
addSlackChannelToEveryoneTeam pid val =
  (> 0)
    <$> Hasql.interpExecute
      [HI.sql|
    UPDATE projects.teams SET slack_channels = array_append(slack_channels, #{val})
    WHERE project_id = #{pid} AND is_everyone = TRUE AND deleted_at IS NULL
      AND NOT (#{val} = ANY(slack_channels))|]
addDiscordChannelToEveryoneTeam pid val =
  (> 0)
    <$> Hasql.interpExecute
      [HI.sql|
    UPDATE projects.teams SET discord_channels = array_append(discord_channels, #{val})
    WHERE project_id = #{pid} AND is_everyone = TRUE AND deleted_at IS NULL
      AND NOT (#{val} = ANY(discord_channels))|]
addPagerdutyServiceToEveryoneTeam pid val =
  (> 0)
    <$> Hasql.interpExecute
      [HI.sql|
    UPDATE projects.teams SET pagerduty_services = array_append(pagerduty_services, #{val})
    WHERE project_id = #{pid} AND is_everyone = TRUE AND deleted_at IS NULL
      AND NOT (#{val} = ANY(pagerduty_services))|]


removeSlackChannelsFromEveryoneTeam
  , removeDiscordChannelsFromEveryoneTeam
  , removePagerdutyServicesFromEveryoneTeam
    :: (DB es, Log :> es) => Projects.ProjectId -> Eff es ()
removeSlackChannelsFromEveryoneTeam pid = modifyEveryoneTeamDetails pid \d -> d{slackChannels = mempty}
removeDiscordChannelsFromEveryoneTeam pid = modifyEveryoneTeamDetails pid \d -> d{discordChannels = mempty}
removePagerdutyServicesFromEveryoneTeam pid = modifyEveryoneTeamDetails pid \d -> d{pagerdutyServices = mempty}


setEveryoneTeamEmails
  , setEveryoneTeamPhones
  , setEveryoneTeamDisabledChannels
    :: (DB es, Log :> es) => Projects.ProjectId -> V.Vector Text -> Eff es ()
setEveryoneTeamEmails pid v = modifyEveryoneTeamDetails pid \d -> d{notifyEmails = v}
setEveryoneTeamPhones pid v = modifyEveryoneTeamDetails pid \d -> d{phoneNumbers = v}
setEveryoneTeamDisabledChannels pid v = modifyEveryoneTeamDetails pid \d -> d{disabledChannels = v}


resolveTeamEmails :: DB es => Projects.ProjectId -> Team -> Eff es [CI Text]
resolveTeamEmails projectId team =
  if team.is_everyone
    then do
      memberEmails <- fmap (.email) <$> selectActiveProjectMembers projectId
      pure $ memberEmails <> V.toList (fmap CI.mk team.notify_emails)
    else pure $ V.toList $ fmap CI.mk team.notify_emails


-- | A channel type ("slack" | "discord" | "email" | "phone" | "pagerduty") is
-- enabled for a team iff it's not listed in disabled_channels. Absence of
-- targets on the team is a separate question — this only answers "has the
-- user muted this channel type?"
isChannelEnabled :: Text -> Team -> Bool
isChannelEnabled ch t = not $ V.elem ch t.disabled_channels


-- | Fetch @everyone and ask whether a channel type is enabled there. Defaults to False
-- when the team doesn't exist — the caller is about to dispatch, and silently dispatching
-- with no team would route nowhere.
isEveryoneChannelEnabled :: DB es => Text -> Projects.ProjectId -> Eff es Bool
isEveryoneChannelEnabled ch pid = maybe False (isChannelEnabled ch) <$> getEveryoneTeam pid


-- | True iff at least one channel could deliver. Email is special — project
-- members are always addressable, so "email enabled" is sufficient regardless
-- of notify_emails (extra addresses on top of members). The other channels
-- need a non-empty target list since they have no implicit recipients.
teamHasAnyEnabledChannel :: Team -> Bool
teamHasAnyEnabledChannel t =
  any populatedAndEnabled [("slack" :: Text, t.slack_channels), ("discord", t.discord_channels), ("phone", t.phone_numbers), ("pagerduty", t.pagerduty_services)]
    || isChannelEnabled "email" t
  where
    populatedAndEnabled (ch, xs) = not (V.null xs) && isChannelEnabled ch t
