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


-- | Insert members AND sync their emails into each project's @everyone notify_emails
-- atomically. @everyone.notify_emails is the authoritative alert audience (see
-- resolveTeamEmails) — inserting without this sync would leave the new member silently
-- unreachable. Single CTE so the insert and sync succeed or fail together.
-- Emails are stored lower-cased to match the soft-delete/deactivate paths and the
-- backfill migration, avoiding case-duplicate entries.
insertProjectMembers :: (DB es, Log :> es) => [CreateProjectMembers] -> Eff es Int64
insertProjectMembers [] = pure 0
insertProjectMembers members = do
  n <-
    Hasql.interpExecute
      [HI.sql|
        WITH ins AS (
          INSERT INTO projects.project_members(project_id, user_id, permission)
          SELECT * FROM unnest(#{pidsV}::uuid[], #{uidsV}::uuid[], #{permsV}::projects.project_permissions[])
          ON CONFLICT (project_id, user_id) DO UPDATE SET active = TRUE, deleted_at = NULL
          RETURNING project_id, user_id
        ),
        added AS (
          SELECT DISTINCT ins.project_id, lower(u.email::text) AS email
          FROM ins JOIN users.users u ON u.id = ins.user_id
        )
        UPDATE projects.teams t SET notify_emails = ARRAY(
          SELECT DISTINCT e FROM unnest(
            t.notify_emails || ARRAY(SELECT email FROM added WHERE project_id = t.project_id)
          ) e
        )
        WHERE t.is_everyone = TRUE AND t.deleted_at IS NULL
          AND t.project_id IN (SELECT project_id FROM added) |]
  -- Surface projects without an @everyone row so members don't silently become unreachable.
  let distinctPids = ordNub (V.toList pidsV)
  when (n < fromIntegral (length distinctPids))
    $ Log.logAttention "insertProjectMembers: @everyone sync updated fewer teams than projects — some members may be unreachable"
    $ AE.object ["project_ids" AE..= distinctPids, "teams_updated" AE..= n]
  pure n
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


-- | Soft-delete members and strip their emails from the matching @everyone
-- notify_emails — the list is the authoritative audience, so leaving stale
-- addresses would keep paging ex-members.
softDeleteProjectMembers :: (DB es, Time :> es) => NonEmpty UUID.UUID -> Eff es ()
softDeleteProjectMembers ids = do
  now <- Time.currentTime
  let idsVec = V.fromList $ toList ids
  Hasql.interpExecute_
    [HI.sql|
      WITH del AS (
        UPDATE projects.project_members SET active = FALSE, deleted_at = #{now}
        WHERE id = ANY(#{idsVec}::uuid[])
        RETURNING project_id, user_id
      ),
      affected AS (
        SELECT del.project_id, lower(u.email::text) AS email
        FROM del JOIN users.users u ON u.id = del.user_id
      )
      UPDATE projects.teams t SET notify_emails = ARRAY(
        SELECT e FROM unnest(t.notify_emails) e
        WHERE lower(e) <> ALL(SELECT email FROM affected WHERE project_id = t.project_id)
      )
      WHERE t.is_everyone = TRUE AND t.deleted_at IS NULL
        AND t.project_id IN (SELECT project_id FROM affected) |]


-- | Deactivate all non-owner members (Free-plan downgrade) and strip their
-- emails from @everyone in one atomic CTE. Externally-added emails (not tied
-- to any project member) are preserved.
deactivateNonOwnerMembers :: DB es => Projects.ProjectId -> Eff es ()
deactivateNonOwnerMembers pid =
  Hasql.interpExecute_
    [HI.sql|
      WITH del AS (
        UPDATE projects.project_members SET active = FALSE
        WHERE project_id = #{pid}
          AND user_id != (SELECT user_id FROM projects.project_members WHERE project_id = #{pid} ORDER BY created_at LIMIT 1)
        RETURNING user_id
      ),
      removed AS (SELECT lower(u.email::text) AS email FROM del JOIN users.users u ON u.id = del.user_id)
      UPDATE projects.teams t SET notify_emails = ARRAY(
        SELECT e FROM unnest(t.notify_emails) e WHERE lower(e) <> ALL(SELECT email FROM removed)
      )
      WHERE t.is_everyone = TRUE AND t.deleted_at IS NULL AND t.project_id = #{pid} |]


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


-- | The notify_emails column is now the single source of truth for who gets
-- paged — @everyone syncs member emails in/out at add/remove time, so no
-- implicit expansion is needed here.
resolveTeamEmails :: Team -> [CI Text]
resolveTeamEmails team = V.toList $ fmap CI.mk team.notify_emails


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


-- | True iff at least one channel has a populated, enabled target list.
-- notify_emails is the authoritative email audience (kept in sync with
-- project members), so it's treated like any other channel — empty means
-- undeliverable.
teamHasAnyEnabledChannel :: Team -> Bool
teamHasAnyEnabledChannel t =
  any populatedAndEnabled
    [ ("email" :: Text, t.notify_emails)
    , ("slack", t.slack_channels)
    , ("discord", t.discord_channels)
    , ("phone", t.phone_numbers)
    , ("pagerduty", t.pagerduty_services)
    ]
  where
    populatedAndEnabled (ch, xs) = not (V.null xs) && isChannelEnabled ch t
