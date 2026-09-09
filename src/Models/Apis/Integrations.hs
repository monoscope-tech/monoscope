module Models.Apis.Integrations (
  SlackData (..),
  slackAgentScopesGranted,
  DiscordData (..),
  insertAccessToken,
  getSlackDataByTeamId,
  insertDiscordData,
  getDiscordDataByProjectId,
  getProjectSlackData,
  updateSlackDefaultChannel,
  getDashboardsForDiscord,
  getDashboardsForSlack,
  getDashboardsForWhatsapp,
  getDiscordData,
  deleteSlackData,
  SlackInstall (..),
  createSlackInstall,
  consumeSlackInstall,
  SlackLink (..),
  SlackLinkProject (..),
  SlackPrincipal (..),
  createSlackLink,
  getSlackLink,
  slackLinkProjects,
  completeSlackLink,
  resolveSlackPrincipal,
  slackThreadProject,
  bindSlackInvestigation,
  recordSlackSessionEvent,
  slackInvestigationStopped,
) where

import Data.Effectful.Hasql qualified as Hasql
import Data.Vector qualified as V
import Deriving.Aeson qualified as AE
import Deriving.Aeson.Stock qualified as DAE
import Effectful
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId)
import Relude
import System.Types (DB)


data SlackLink = SlackLink {id :: UUIDId "slack_link", teamId :: Text, slackUserId :: Text, channelId :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data SlackLinkProject = SlackLinkProject {projectId :: Projects.ProjectId, title :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data SlackPrincipal = SlackPrincipal {userId :: Projects.UserId, projectId :: Projects.ProjectId}
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


-- | The identity comes from the signed receipt, never from a link URL or form.
-- A retry reuses the same private link instead of minting another credential.
createSlackLink :: DB es => UUIDId "slack_link" -> UUIDId "slack_event" -> Eff es (Maybe SlackLink)
createSlackLink linkId receiptId =
  Hasql.interpOne
    [HI.sql|WITH request AS (
    INSERT INTO apis.slack_identity_requests (id, receipt_id)
    SELECT #{linkId}, id FROM apis.slack_events
    WHERE id = #{receiptId} AND payload #>> '{event,type}' IN ('message', 'app_mention')
      AND COALESCE(payload #>> '{event,user}', '') <> ''
      AND COALESCE(payload #>> '{event,channel}', '') <> ''
      AND payload #>> '{event,bot_id}' IS NULL AND payload #>> '{event,subtype}' IS NULL
    ON CONFLICT (receipt_id) DO UPDATE SET receipt_id = EXCLUDED.receipt_id
    RETURNING id, receipt_id, expires_at, consumed_at
  ) SELECT request.id, event.team_id, event.payload #>> '{event,user}', event.payload #>> '{event,channel}'
    FROM request JOIN apis.slack_events event ON event.id = request.receipt_id
    WHERE request.expires_at > now() AND request.consumed_at IS NULL|]


getSlackLink :: DB es => UUIDId "slack_link" -> Eff es (Maybe SlackLink)
getSlackLink linkId =
  Hasql.interpOne
    [HI.sql|SELECT request.id, event.team_id, event.payload #>> '{event,user}', event.payload #>> '{event,channel}'
    FROM apis.slack_identity_requests request JOIN apis.slack_events event ON event.id = request.receipt_id
    WHERE request.id = #{linkId} AND request.expires_at > now() AND request.consumed_at IS NULL|]


slackLinkProjects :: DB es => Text -> Projects.UserId -> Eff es [SlackLinkProject]
slackLinkProjects teamId userId =
  Hasql.interp
    [HI.sql|SELECT project.id, project.title FROM projects.projects project
    JOIN projects.project_members member ON member.project_id = project.id
    JOIN users.users account ON account.id = member.user_id
    JOIN apis.slack installation ON installation.project_id = project.id
    WHERE installation.team_id = #{teamId} AND member.user_id = #{userId}
      AND member.active AND member.deleted_at IS NULL AND account.active AND account.deleted_at IS NULL AND project.active AND project.deleted_at IS NULL
    ORDER BY project.title, project.id|]


-- | Lock the request so concurrent form submissions cannot bind it twice.
-- Existing identities cannot be reassigned to a different Monoscope account.
completeSlackLink :: DB es => UUIDId "slack_link" -> Projects.UserId -> Projects.ProjectId -> Eff es Bool
completeSlackLink linkId userId projectId = do
  affected <-
    Hasql.interpExecute
      [HI.sql|WITH request AS (
      SELECT request.id, event.team_id, event.payload #>> '{event,user}' AS slack_user_id
      FROM apis.slack_identity_requests request JOIN apis.slack_events event ON event.id = request.receipt_id
      JOIN apis.slack installation ON installation.team_id = event.team_id AND installation.project_id = #{projectId}
      JOIN projects.project_members member ON member.project_id = installation.project_id AND member.user_id = #{userId}
      JOIN users.users account ON account.id = member.user_id
      JOIN projects.projects project ON project.id = member.project_id
      WHERE request.id = #{linkId} AND request.expires_at > now() AND request.consumed_at IS NULL
        AND member.active AND member.deleted_at IS NULL AND account.active AND account.deleted_at IS NULL AND project.active AND project.deleted_at IS NULL
      FOR UPDATE OF request
    ), identity AS (
      INSERT INTO apis.slack_identities (team_id, slack_user_id, user_id, project_id)
      SELECT team_id, slack_user_id, #{userId}, #{projectId} FROM request
      ON CONFLICT (team_id, slack_user_id) DO UPDATE SET project_id = EXCLUDED.project_id
      WHERE slack_identities.user_id = EXCLUDED.user_id
      RETURNING team_id, slack_user_id
    ) UPDATE apis.slack_identity_requests target SET consumed_at = now()
      FROM request JOIN identity USING (team_id, slack_user_id) WHERE target.id = request.id|]
  pure $ affected == 1


-- | Revalidate membership and the installation on every incoming investigation.
resolveSlackPrincipal :: DB es => Text -> Text -> Maybe Projects.ProjectId -> Eff es (Maybe SlackPrincipal)
resolveSlackPrincipal teamId slackUserId threadProject =
  Hasql.interpOne
    [HI.sql|SELECT identity.user_id, member.project_id FROM apis.slack_identities identity
    JOIN projects.project_members member ON member.project_id = COALESCE(#{threadProject}, identity.project_id) AND member.user_id = identity.user_id
    JOIN users.users account ON account.id = identity.user_id
    JOIN projects.projects project ON project.id = member.project_id
    JOIN apis.slack installation ON installation.project_id = member.project_id AND installation.team_id = identity.team_id
    WHERE identity.team_id = #{teamId} AND identity.slack_user_id = #{slackUserId}
      AND member.active AND member.deleted_at IS NULL AND account.active AND account.deleted_at IS NULL AND project.active AND project.deleted_at IS NULL|]


slackThreadProject :: DB es => Text -> Text -> Text -> Eff es (Maybe Projects.ProjectId)
slackThreadProject teamId channelId threadTs =
  fmap HI.getOneColumn
    <$> Hasql.interpOne
      [HI.sql|SELECT project_id FROM (
    SELECT episode.project_id, 0 AS priority FROM apis.slack_incident_roots root
    JOIN apis.incident_episodes episode ON episode.id = root.episode_id
    WHERE root.team_id = #{teamId} AND root.channel_id = #{channelId} AND root.message_ts = #{threadTs}
    UNION ALL
    SELECT project_id, 1 AS priority FROM apis.slack_investigation_threads
    WHERE team_id = #{teamId} AND channel_id = #{channelId} AND thread_ts = #{threadTs}
  ) binding ORDER BY priority LIMIT 1|]


bindSlackInvestigation :: DB es => SlackPrincipal -> Text -> Text -> Text -> Eff es Bool
bindSlackInvestigation principal teamId channelId threadTs = do
  result <-
    Hasql.interpOne @(HI.OneColumn Bool)
      [HI.sql|INSERT INTO apis.slack_investigation_threads (team_id, channel_id, thread_ts, project_id)
      SELECT #{teamId}, #{channelId}, #{threadTs}, member.project_id FROM projects.project_members member
      JOIN projects.projects project ON project.id = member.project_id
      JOIN users.users account ON account.id = member.user_id
      JOIN apis.slack installation ON installation.project_id = project.id AND installation.team_id = #{teamId}
      WHERE member.project_id = #{principal.projectId} AND member.user_id = #{principal.userId}
        AND member.active AND member.deleted_at IS NULL AND account.active AND account.deleted_at IS NULL AND project.active AND project.deleted_at IS NULL
      ON CONFLICT (team_id, channel_id, thread_ts) DO UPDATE SET thread_ts = EXCLUDED.thread_ts
      WHERE slack_investigation_threads.project_id = EXCLUDED.project_id RETURNING TRUE|]
  pure $ isJust result


-- | Only an authenticated, linked project member can change an existing thread.
-- Source identity and coordinates come from the stored signed receipt.
recordSlackSessionEvent :: DB es => Text -> Text -> Eff es ()
recordSlackSessionEvent teamId eventId =
  Hasql.interpExecute_
    [HI.sql|UPDATE apis.slack_investigation_threads thread
    SET stopped_through = CASE WHEN receipt.payload->'event'->>'type' = 'agent_session_stopped'
          THEN GREATEST(thread.stopped_through, (receipt.payload->'event'->>'event_ts')::numeric) ELSE thread.stopped_through END,
        title = CASE WHEN receipt.payload->'event'->>'type' = 'agent_session_title_changed'
          AND (thread.title_event_ts IS NULL OR thread.title_event_ts < (receipt.payload->'event'->>'event_ts')::numeric)
          THEN receipt.payload->'event'->>'title' ELSE thread.title END,
        title_event_ts = CASE WHEN receipt.payload->'event'->>'type' = 'agent_session_title_changed'
          THEN GREATEST(thread.title_event_ts, (receipt.payload->'event'->>'event_ts')::numeric) ELSE thread.title_event_ts END
    FROM apis.slack_events receipt
    JOIN apis.slack_identities identity ON identity.team_id = receipt.team_id AND identity.slack_user_id = receipt.payload->'event'->>'user'
    JOIN projects.project_members member ON member.user_id = identity.user_id
    JOIN users.users account ON account.id = member.user_id
    JOIN projects.projects project ON project.id = member.project_id
    JOIN apis.slack installation ON installation.project_id = member.project_id AND installation.team_id = receipt.team_id
    WHERE receipt.team_id = #{teamId} AND receipt.event_id = #{eventId}
      AND receipt.payload->'event'->>'type' IN ('agent_session_stopped', 'agent_session_title_changed')
      AND (receipt.payload->'event'->>'type' <> 'agent_session_title_changed' OR receipt.payload->'event'->>'team_id' = receipt.team_id)
      AND thread.team_id = receipt.team_id AND thread.project_id = member.project_id
      AND thread.channel_id = receipt.payload->'event'->>'channel'
      AND thread.thread_ts = receipt.payload->'event'->>'thread_ts'
      AND member.active AND member.deleted_at IS NULL AND account.active AND account.deleted_at IS NULL
      AND project.active AND project.deleted_at IS NULL|]


slackInvestigationStopped :: DB es => Projects.ProjectId -> Text -> Text -> Text -> Text -> Eff es Bool
slackInvestigationStopped pid teamId channelId threadTs messageTs = do
  stopped <-
    Hasql.interpOne @(HI.OneColumn Bool)
      [HI.sql|SELECT TRUE FROM apis.slack_investigation_threads
      WHERE project_id = #{pid} AND team_id = #{teamId} AND channel_id = #{channelId}
        AND thread_ts = #{threadTs} AND stopped_through >= #{messageTs}::text::numeric|]
  pure $ isJust stopped


data SlackInstall = SlackInstall {projectId :: Projects.ProjectId, onboarding :: Bool}
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


-- | Installation modifies credentials shared by the whole project. Check current
-- admin membership, rather than the browser session's cached project list.
createSlackInstall :: DB es => UUIDId "slack_install" -> Projects.UserId -> Projects.ProjectId -> Bool -> Eff es Bool
createSlackInstall stateId userId projectId onboarding = do
  affected <-
    Hasql.interpExecute
      [HI.sql|INSERT INTO apis.slack_install_requests (id, user_id, project_id, onboarding)
      SELECT #{stateId}, #{userId}, #{projectId}, #{onboarding}
      FROM projects.project_members
      WHERE project_id = #{projectId} AND user_id = #{userId}
        AND active AND deleted_at IS NULL AND permission = 'admin'|]
  pure $ affected == 1


-- | Consume before contacting Slack. A failed exchange requires a fresh request.
-- The callback must have the same authenticated Monoscope user as the initiator.
consumeSlackInstall :: DB es => UUIDId "slack_install" -> Projects.UserId -> Eff es (Maybe SlackInstall)
consumeSlackInstall stateId userId =
  Hasql.interpOne
    [HI.sql|DELETE FROM apis.slack_install_requests request
    USING projects.project_members member
    WHERE request.id = #{stateId} AND request.user_id = #{userId} AND request.expires_at > now()
      AND member.project_id = request.project_id AND member.user_id = request.user_id
      AND member.active AND member.deleted_at IS NULL AND member.permission = 'admin'
    RETURNING request.project_id, request.onboarding|]


-- | OAuth-time Slack credentials + the channel the app was installed to.
--
-- @channelId@ and @channelName@ record the channel the user picked during
-- OAuth. @webhookUrl@ is the channel-bound incoming webhook Slack issues at
-- install time — POSTing to it delivers to that exact channel without
-- requiring the bot user to be a member. That's the only path that works for
-- private channels unless the user manually @/invite@s the bot.
--
-- Alert routing to this channel uses @webhookUrl@; routing to any additional
-- channels the user adds (via /here or the dropdown) uses chat.postMessage
-- with @botToken@ and does require bot membership. Both transports accept
-- thread_ts; only chat.postMessage returns the timestamp for a new root.
data SlackData = SlackData
  { projectId :: Projects.ProjectId
  , teamId :: Text
  , teamName :: Maybe Text
  , botToken :: Text
  , channelId :: Text
  , channelName :: Maybe Text
  , webhookUrl :: Maybe Text
  , scopes :: Maybe (V.Vector Text)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (HI.DecodeRow, NFData)
  deriving (AE.FromJSON) via DAE.Snake SlackData


slackAgentScopesGranted :: SlackData -> Bool
slackAgentScopesGranted installation = maybe False (\grants -> all (`elem` grants) (["assistant:write", "chat:write"] :: [Text])) installation.scopes


insertAccessToken :: DB es => Projects.ProjectId -> Text -> Text -> Text -> Text -> Text -> Text -> Maybe (V.Vector Text) -> Eff es Int64
insertAccessToken pid teamId channelId teamName botToken channelName webhookUrl scopes =
  Hasql.interpExecute
    [HI.sql|INSERT INTO apis.slack
               (project_id, team_id, channel_id, team_name, bot_token, channel_name, webhook_url, scopes)
               VALUES (#{pid},#{teamId},#{channelId},#{teamName},#{botToken},#{channelName},#{webhookUrl},#{scopes})
               ON CONFLICT (project_id)
               DO UPDATE SET team_id = EXCLUDED.team_id, channel_id = EXCLUDED.channel_id, team_name = EXCLUDED.team_name, bot_token = EXCLUDED.bot_token, channel_name = EXCLUDED.channel_name, webhook_url = EXCLUDED.webhook_url, scopes = EXCLUDED.scopes |]


-- | Column order must match 'SlackData''s field order — 'HI.DecodeRow' is positional.
selectSlack :: HI.Sql
selectSlack = [HI.sql|SELECT project_id, team_id, team_name, bot_token, channel_id, channel_name, webhook_url, scopes FROM apis.slack |]


getProjectSlackData :: DB es => Projects.ProjectId -> Eff es (Maybe SlackData)
getProjectSlackData pid = Hasql.interpOne (selectSlack <> [HI.sql|WHERE project_id = #{pid}|])


getSlackDataByTeamId :: DB es => Text -> Eff es (Maybe SlackData)
getSlackDataByTeamId teamId = Hasql.interpOne (selectSlack <> [HI.sql|WHERE team_id = #{teamId}|])


-- | Change one project's default channel. An incoming webhook is tied to its
-- installation channel, so moving the default clears it and uses the bot API.
updateSlackDefaultChannel :: DB es => Projects.ProjectId -> Text -> Maybe Text -> Eff es Int64
updateSlackDefaultChannel pid channelId channelName =
  Hasql.interpExecute
    [HI.sql|UPDATE apis.slack SET channel_id = #{channelId}, channel_name = #{channelName},
      webhook_url = CASE WHEN channel_id = #{channelId} THEN webhook_url ELSE NULL END
      WHERE project_id = #{pid}|]


data DiscordData = DiscordData
  { projectId :: Projects.ProjectId
  , guildId :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (HI.DecodeRow, NFData)
  deriving (AE.FromJSON) via DAE.Snake DiscordData


insertDiscordData :: DB es => Projects.ProjectId -> Text -> Eff es Int64
insertDiscordData pid guildId =
  Hasql.interpExecute
    [HI.sql|INSERT INTO apis.discord
               (project_id, guild_id)
               VALUES (#{pid},#{guildId})
               ON CONFLICT (project_id)
               DO UPDATE SET guild_id = EXCLUDED.guild_id |]


selectDiscord :: HI.Sql
selectDiscord = [HI.sql|SELECT project_id, guild_id FROM apis.discord |]


getDiscordData :: DB es => Text -> Eff es (Maybe DiscordData)
getDiscordData guildId = Hasql.interpOne (selectDiscord <> [HI.sql|WHERE guild_id = #{guildId}|])


getDiscordDataByProjectId :: DB es => Projects.ProjectId -> Eff es (Maybe DiscordData)
getDiscordDataByProjectId pid = Hasql.interpOne (selectDiscord <> [HI.sql|WHERE project_id = #{pid}|])


-- | Shared (d.title, d.id::text) dashboard lookup; each caller supplies the
-- platform-specific JOIN + WHERE fragment (Slack/Discord share the same shape).
dashboardsByProjectJoin :: DB es => HI.Sql -> Eff es [(Text, Text)]
dashboardsByProjectJoin joinWhere = Hasql.interp ([HI.sql|SELECT d.title, d.id::text FROM projects.dashboards d |] <> joinWhere)


getDashboardsForSlack :: DB es => Projects.ProjectId -> Eff es [(Text, Text)]
getDashboardsForSlack pid = dashboardsByProjectJoin [HI.sql|WHERE d.project_id = #{pid}|]


getDashboardsForWhatsapp :: DB es => Text -> Eff es [(Text, Text)]
getDashboardsForWhatsapp number =
  dashboardsByProjectJoin
    [HI.sql|JOIN projects.teams t ON t.project_id = d.project_id
            WHERE t.is_everyone = TRUE AND t.deleted_at IS NULL
              AND #{number} = ANY(t.phone_numbers)|]


getDashboardsForDiscord :: DB es => Text -> Eff es [(Text, Text)]
getDashboardsForDiscord guildId = dashboardsByProjectJoin [HI.sql|JOIN apis.discord dd ON d.project_id = dd.project_id WHERE dd.guild_id = #{guildId}|]


deleteSlackData :: DB es => Projects.ProjectId -> Eff es Int64
deleteSlackData pid = Hasql.interpExecute [HI.sql|DELETE FROM apis.slack WHERE project_id = #{pid}|]
