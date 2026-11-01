module Models.Apis.Integrations (
  activeMembership,
  SlackChannel (teamId, channelId),
  SlackChannelError (..),
  slackChannel,
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
  SlackAuthorization (..),
  SlackEvidenceConsent (projectId, userId, teamId, slackUserId, verifier),
  SlackPKCE,
  newSlackPKCE,
  slackPKCEVerifier,
  slackPKCEChallenge,
  createSlackEvidenceConsent,
  consumeSlackAuthorization,
  SlackUserGrant (..),
  SlackTokenLifetime (..),
  slackEvidenceScopes,
  saveSlackEvidenceGrant,
  getSlackEvidenceGrant,
  SlackEvidenceSubject (..),
  memberSlackEvidenceSubject,
  SlackCredentialFailure (..),
  resolveSlackEvidenceGrant,
  revokeSlackEvidence,
  recordSlackTokenRevocation,
  SlackLink (..),
  SlackLinkProject (..),
  SlackPrincipal (..),
  createSlackLink,
  hasDeliveredSlackLink,
  markSlackLinkDelivered,
  getSlackLink,
  slackLinkProjects,
  completeSlackLink,
  resolveSlackPrincipal,
  resolveSlackPrincipalTx,
  slackChannelMember,
  slackDirectMessageOwner,
  slackThreadProject,
  bindSlackInvestigation,
  SlackSessionTitle,
  mkSlackSessionTitle,
  slackSessionTitleText,
  SlackTitleChange (..),
  recordSlackSessionEventTx,
  slackThreadTitle,
  slackInvestigationStopped,
) where

import Control.Lens ((.~), (^.))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Base64.Types (extractBase64)
import Data.ByteArray qualified as BA
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.UUID qualified as UUID
import Data.Effectful.Wreq qualified as HTTP
import Data.List (lookup)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Deriving.Aeson qualified as AE
import Deriving.Aeson.Stock qualified as DAE
import Effectful
import Effectful.Exception qualified as Exception
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as TxS
import Models.Projects.ProjectApiKeys qualified as Secrets
import Models.Projects.ProjectMembers (activeMembership)
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Network.Wreq qualified as Wreq
import Pkg.DeriveUtils (UUIDId, enumLit, refineText)
import Pkg.SlackOAuth (SlackTokenLifetime (..), SlackUserGrant (..), slackEvidenceScopes)
import Pkg.SlackOAuth qualified as OAuth
import Pkg.SlackRateLimit qualified as RateLimit
import Relude
import System.Types (DB)
import "base64" Data.ByteString.Base64.URL qualified as B64URL
import "cryptonite" Crypto.Hash (Digest, SHA256, hash)


data SlackChannel = SlackChannel {teamId :: Text, channelId :: Text}
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Aeson.ToJSON)


data SlackChannelError = WorkspaceRequired | ChannelRequired
  deriving stock (Eq, Show)


-- | A channel identity shared by destinations and evidence audiences.
--
-- >>> fmap (\channel -> (channel.teamId, channel.channelId)) (slackChannel "T1" "C123") == Right ("T1", "C123")
-- True
-- >>> slackChannel "T1" "D123" == Left ChannelRequired
-- True
-- >>> slackChannel "" "C123" == Left WorkspaceRequired
-- True
slackChannel :: Text -> Text -> Either SlackChannelError SlackChannel
slackChannel team channel
  | T.null (T.strip team) = Left WorkspaceRequired
  | T.length channel < 2 || not (T.isPrefixOf "C" channel || T.isPrefixOf "G" channel) || not (T.all (\c -> isAsciiUpper c || isDigit c) channel) = Left ChannelRequired
  | otherwise = Right $ SlackChannel team channel


instance Aeson.FromJSON SlackChannel where
  parseJSON = Aeson.withObject "Slack channel" \value -> do
    team <- value Aeson..: "teamId"
    channel <- value Aeson..: "channelId"
    either (fail . show) pure $ slackChannel team channel


data SlackLink = SlackLink {id :: UUIDId "slack_link", teamId :: Text, slackUserId :: Text, channelId :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data SlackLinkProject = SlackLinkProject {projectId :: Projects.ProjectId, title :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data SlackPrincipal = SlackPrincipal {userId :: Projects.UserId, projectId :: Projects.ProjectId, permission :: ProjectMembers.Permissions}
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
    WHERE id = #{receiptId} AND (payload #>> '{event,type}' IN ('message', 'app_mention', 'monoscope_message_shortcut')
      OR (payload #>> '{event,type}' = 'app_home_opened' AND payload #>> '{event,tab}' = 'messages'))
      AND COALESCE(payload #>> '{event,user}', '') <> ''
      AND COALESCE(payload #>> '{event,channel}', '') <> ''
      AND payload #>> '{event,bot_id}' IS NULL AND payload #>> '{event,subtype}' IS NULL
    ON CONFLICT (receipt_id) DO UPDATE SET receipt_id = EXCLUDED.receipt_id
    RETURNING id, receipt_id, expires_at, consumed_at
  ) SELECT request.id, event.team_id, event.payload #>> '{event,user}', event.payload #>> '{event,channel}'
    FROM request JOIN apis.slack_events event ON event.id = request.receipt_id
    WHERE request.expires_at > now() AND request.consumed_at IS NULL|]


markSlackLinkDelivered :: DB es => UUIDId "slack_link" -> Eff es ()
markSlackLinkDelivered linkId =
  Hasql.interpExecute_
    [HI.sql|UPDATE apis.slack_identity_requests SET delivered_at = COALESCE(delivered_at, now()) WHERE id = #{linkId}|]


hasDeliveredSlackLink :: DB es => Text -> Text -> Text -> Eff es Bool
hasDeliveredSlackLink teamId channelId slackUserId = do
  request <-
    Hasql.interpOne @(HI.OneColumn Bool)
      [HI.sql|SELECT TRUE FROM apis.slack_identity_requests request
      JOIN apis.slack_events receipt ON receipt.id = request.receipt_id
      WHERE receipt.team_id = #{teamId} AND receipt.payload #>> '{event,channel}' = #{channelId}
        AND receipt.payload #>> '{event,user}' = #{slackUserId} AND request.delivered_at IS NOT NULL
        AND request.expires_at > now() AND request.consumed_at IS NULL LIMIT 1|]
  pure $ isJust request


getSlackLink :: DB es => UUIDId "slack_link" -> Eff es (Maybe SlackLink)
getSlackLink linkId =
  Hasql.interpOne
    [HI.sql|SELECT request.id, event.team_id, event.payload #>> '{event,user}', event.payload #>> '{event,channel}'
    FROM apis.slack_identity_requests request JOIN apis.slack_events event ON event.id = request.receipt_id
    WHERE request.id = #{linkId} AND request.expires_at > now() AND request.consumed_at IS NULL|]


slackLinkProjects :: DB es => Text -> Projects.UserId -> Eff es [SlackLinkProject]
slackLinkProjects teamId userId =
  Hasql.interp
    $ [HI.sql|SELECT project.id, project.title FROM projects.projects project
    JOIN projects.project_members member ON member.project_id = project.id
    JOIN users.users account ON account.id = member.user_id
    JOIN apis.slack installation ON installation.project_id = project.id
    WHERE installation.team_id = #{teamId} AND member.user_id = #{userId}|]
    <> ProjectMembers.activeMembership
    <> [HI.sql|ORDER BY project.title, project.id|]


-- | Lock the request so concurrent form submissions cannot bind it twice.
-- Existing identities cannot be reassigned to a different Monoscope account.
completeSlackLink :: DB es => UUIDId "slack_link" -> Projects.UserId -> Projects.ProjectId -> Eff es Bool
completeSlackLink linkId userId projectId = do
  affected <-
    Hasql.interpExecute
      $ [HI.sql|WITH request AS (
      SELECT request.id, event.team_id, event.payload #>> '{event,user}' AS slack_user_id
      FROM apis.slack_identity_requests request JOIN apis.slack_events event ON event.id = request.receipt_id
      JOIN apis.slack installation ON installation.team_id = event.team_id AND installation.project_id = #{projectId}
      JOIN projects.project_members member ON member.project_id = installation.project_id AND member.user_id = #{userId}
      JOIN users.users account ON account.id = member.user_id
      JOIN projects.projects project ON project.id = member.project_id
      WHERE request.id = #{linkId} AND request.expires_at > now() AND request.consumed_at IS NULL|]
      <> ProjectMembers.activeMembership
      <> [HI.sql|FOR UPDATE OF request
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
resolveSlackPrincipal teamId slackUserId threadProject = Hasql.interpOne $ slackPrincipalSql teamId slackUserId threadProject


resolveSlackPrincipalTx :: Text -> Text -> Maybe Projects.ProjectId -> Tx.Transaction (Maybe SlackPrincipal)
resolveSlackPrincipalTx teamId slackUserId threadProject = Hasql.queryTx $ slackPrincipalSql teamId slackUserId threadProject


slackPrincipalSql :: Text -> Text -> Maybe Projects.ProjectId -> HI.Sql
slackPrincipalSql teamId slackUserId threadProject =
  [HI.sql|SELECT identity.user_id, member.project_id, member.permission FROM apis.slack_identities identity
    JOIN projects.project_members member ON member.project_id = COALESCE(#{threadProject}, identity.project_id) AND member.user_id = identity.user_id
    JOIN users.users account ON account.id = identity.user_id
    JOIN projects.projects project ON project.id = member.project_id
    JOIN apis.slack installation ON installation.project_id = member.project_id AND installation.team_id = identity.team_id
    WHERE identity.team_id = #{teamId} AND identity.slack_user_id = #{slackUserId}|]
    <> ProjectMembers.activeMembership


-- | Proof comes from Slack's current channel membership, bound to a linked account.
slackChannelMember :: (DB es, HTTP.HTTP :> es) => Projects.ProjectId -> Projects.UserId -> SlackChannel -> Eff es Bool
slackChannelMember project user channel = do
  identities <- slackAudienceIdentities project user channel.teamId
  case nonEmpty identities of
    Nothing -> pure False
    Just ((_, token) :| _) -> members token (map fst identities) S.empty ""
  where
    members token users seen cursor = do
      response <-
        HTTP.getWith
          ( HTTP.defaults & Wreq.header "Authorization"
              .~ [encodeUtf8 $ "Bearer " <> token]
                & Wreq.param "channel"
              .~ [channel.channelId] & Wreq.param "limit"
              .~ ["200"] & Wreq.param "cursor"
              .~ [cursor]
          )
          "https://slack.com/api/conversations.members"
      case Aeson.eitherDecode (response ^. HTTP.responseBody) >>= Aeson.parseEither parsePage of
        Left _ -> pure False
        Right (current, next)
          | any (`elem` current) users -> pure True
          | T.null next || next `S.member` seen -> pure False
          | otherwise -> members token users (S.insert cursor seen) next
    parsePage = Aeson.withObject "Slack channel members" \value -> do
      ok <- value Aeson..: "ok"
      unless ok $ fail "Channel membership unavailable"
      users <- value Aeson..: "members"
      metadata <- value Aeson..:? "response_metadata"
      cursor <- maybe (pure "") (\object -> object Aeson..:? "next_cursor" Aeson..!= "") metadata
      pure (users :: [Text], T.strip cursor)


slackDirectMessageOwner :: (DB es, HTTP.HTTP :> es) => Projects.ProjectId -> Projects.UserId -> Text -> Text -> Text -> Eff es Bool
slackDirectMessageOwner project member team user channel = do
  identities <- slackAudienceIdentities project member team
  case lookup user identities of
    Nothing -> pure False
    Just token -> do
      response <-
        HTTP.getWith
          (HTTP.defaults & Wreq.header "Authorization" .~ [encodeUtf8 $ "Bearer " <> token] & Wreq.param "channel" .~ [channel])
          "https://slack.com/api/conversations.info"
      pure
        $ fromRight False
        $ Aeson.eitherDecode (response ^. HTTP.responseBody)
        >>= Aeson.parseEither
          ( Aeson.withObject "Slack direct message" \value -> do
              ok <- value Aeson..: "ok"
              info <- value Aeson..: "channel"
              direct <- info Aeson..: "is_im"
              peer <- info Aeson..: "user"
              identifier <- info Aeson..: "id"
              pure $ ok && direct && peer == user && identifier == channel
          )


slackAudienceIdentities :: DB es => Projects.ProjectId -> Projects.UserId -> Text -> Eff es [(Text, Text)]
slackAudienceIdentities project user team =
  Hasql.interp @[(Text, Text)]
    $ [HI.sql|SELECT identity.slack_user_id, installation.bot_token FROM apis.slack_identities identity
      JOIN projects.project_members member ON member.user_id = identity.user_id AND member.project_id = #{project}
      JOIN users.users account ON account.id = identity.user_id
      JOIN projects.projects project ON project.id = member.project_id
      JOIN apis.slack installation ON installation.project_id = member.project_id AND installation.team_id = identity.team_id
      WHERE identity.team_id = #{team} AND identity.user_id = #{user}|]
    <> activeMembership


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
      $ [HI.sql|INSERT INTO apis.slack_investigation_threads (team_id, channel_id, thread_ts, project_id)
      SELECT #{teamId}, #{channelId}, #{threadTs}, member.project_id FROM projects.project_members member
      JOIN projects.projects project ON project.id = member.project_id
      JOIN users.users account ON account.id = member.user_id
      JOIN apis.slack installation ON installation.project_id = project.id AND installation.team_id = #{teamId}
      WHERE member.project_id = #{principal.projectId} AND member.user_id = #{principal.userId}|]
      <> ProjectMembers.activeMembership
      <> [HI.sql|ON CONFLICT (team_id, channel_id, thread_ts) DO UPDATE SET thread_ts = EXCLUDED.thread_ts
      WHERE slack_investigation_threads.project_id = EXCLUDED.project_id RETURNING TRUE|]
  pure $ isJust result


newtype SlackSessionTitle = SlackSessionTitle Text
  deriving stock (Eq, Show)
  deriving newtype (AE.ToJSON)


-- $setup
-- >>> import Data.Aeson qualified as Aeson
-- >>> import Data.Text qualified as T
-- >>> import Relude


-- | Slack session titles contain 1–200 characters and no null bytes.
-- >>> let title :: Maybe SlackSessionTitle; title = mkSlackSessionTitle "Checkout"
-- >>> fmap slackSessionTitleText title
-- Just "Checkout"
-- >>> map (isNothing . mkSlackSessionTitle) ["", " ", T.replicate 201 "x", "a\NULb"]
-- [True,True,True,True]
-- >>> (Aeson.decode . Aeson.encode =<< title) == title
-- True
mkSlackSessionTitle :: Text -> Maybe SlackSessionTitle
mkSlackSessionTitle value = SlackSessionTitle value <$ guard (not (T.null $ T.strip value) && T.length value <= 200 && not (T.any (== '\NUL') value))


slackSessionTitleText :: SlackSessionTitle -> Text
slackSessionTitleText (SlackSessionTitle value) = value


instance Aeson.FromJSON SlackSessionTitle where
  parseJSON = Aeson.withText "Slack session title" $ maybe (fail "Enter 1–200 characters without null bytes") pure . mkSlackSessionTitle


instance HI.DecodeValue SlackSessionTitle where
  decodeValue = refineText "Slack session title" mkSlackSessionTitle


data SlackTitleChange = SlackTitleChange {projectId :: Projects.ProjectId, channelId :: Text, threadTs :: Text, title :: SlackSessionTitle}
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


-- | Only a linked current member can update a thread; return newly accepted titles.
recordSlackSessionEventTx :: Text -> Text -> Tx.Transaction [SlackTitleChange]
recordSlackSessionEventTx teamId eventId =
  Hasql.queryTx
    $ [HI.sql|WITH changed AS (UPDATE apis.slack_investigation_threads thread
    SET stopped_through = CASE WHEN receipt.payload->'event'->>'type' = 'agent_session_stopped'
          THEN GREATEST(thread.stopped_through, (receipt.payload->'event'->>'event_ts')::numeric) ELSE thread.stopped_through END,
        title = CASE WHEN receipt.payload->'event'->>'type' = 'agent_session_title_changed'
          THEN receipt.payload->'event'->>'title' ELSE thread.title END,
        title_event_ts = CASE WHEN receipt.payload->'event'->>'type' = 'agent_session_title_changed'
          THEN (receipt.payload->'event'->>'event_ts')::numeric ELSE thread.title_event_ts END
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
      AND thread.thread_ts = receipt.payload->'event'->>'thread_ts'|]
    <> ProjectMembers.activeMembership
    <> [HI.sql| AND (receipt.payload->'event'->>'type' <> 'agent_session_title_changed'
      OR thread.title_event_ts IS NULL OR thread.title_event_ts < (receipt.payload->'event'->>'event_ts')::numeric)
      RETURNING thread.project_id, thread.channel_id, thread.thread_ts, thread.title,
        receipt.payload->'event'->>'type' AS kind)
      SELECT project_id, channel_id, thread_ts, title FROM changed WHERE kind = 'agent_session_title_changed'|]


slackThreadTitle :: DB es => Projects.ProjectId -> Text -> Text -> Text -> Eff es (Maybe SlackSessionTitle)
slackThreadTitle pid team channel thread =
  (HI.getOneColumn =<<)
    <$> Hasql.interpOne @(HI.OneColumn (Maybe SlackSessionTitle))
      [HI.sql|SELECT title FROM apis.slack_investigation_threads
      WHERE project_id = #{pid} AND team_id = #{team} AND channel_id = #{channel} AND thread_ts = #{thread}|]


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
createSlackInstall stateId userId projectId onboarding = createSlackAuthorization stateId userId projectId $ WorkspaceConsent onboarding


createSlackEvidenceConsent :: DB es => UUIDId "slack_install" -> Projects.UserId -> Projects.ProjectId -> Text -> SlackPKCE -> Eff es Bool
createSlackEvidenceConsent stateId userId projectId slackUserId verifier = createSlackAuthorization stateId userId projectId $ PersonalConsent slackUserId verifier


data SlackConsentPurpose = WorkspaceConsent Bool | PersonalConsent Text SlackPKCE


createSlackAuthorization :: DB es => UUIDId "slack_install" -> Projects.UserId -> Projects.ProjectId -> SlackConsentPurpose -> Eff es Bool
createSlackAuthorization stateId userId projectId purpose = do
  let (onboarding, slackUser, verifier) = case purpose of
        WorkspaceConsent requestedOnboarding -> (requestedOnboarding, Nothing, Nothing)
        PersonalConsent requestedUser pkce -> (False, Just requestedUser, Just $ slackPKCEVerifier pkce)
  affected <-
    withSlackCredentialLock projectId userId
      . fmap HI.getRowsAffected
      . Hasql.queryTx
      $ [HI.sql|WITH expired AS (DELETE FROM apis.slack_install_requests WHERE project_id = #{projectId} AND user_id = #{userId} AND expires_at <= now())
      INSERT INTO apis.slack_install_requests (id, user_id, project_id, onboarding, team_id, slack_user_id, code_verifier)
      SELECT #{stateId}, #{userId}, #{projectId}, #{onboarding}, identity.team_id, identity.slack_user_id, #{verifier}
      FROM projects.project_members member
      JOIN projects.projects project ON project.id = member.project_id
      JOIN users.users account ON account.id = member.user_id
      LEFT JOIN apis.slack installation ON installation.project_id = member.project_id
      LEFT JOIN apis.slack_identities identity ON identity.team_id = installation.team_id
        AND identity.slack_user_id = #{slackUser} AND identity.user_id = member.user_id
      WHERE member.project_id = #{projectId} AND member.user_id = #{userId}
        AND ((#{slackUser}::text IS NULL AND member.permission = ^{enumLit ProjectMembers.PAdmin})
          OR (#{slackUser}::text IS NOT NULL AND identity.slack_user_id IS NOT NULL))|]
      <> ProjectMembers.activeMembership
  pure $ affected == 1


data SlackAuthorization = WorkspaceAuthorization SlackInstall | EvidenceAuthorization SlackEvidenceConsent


data SlackEvidenceConsent = SlackEvidenceConsent
  { id :: UUIDId "slack_install"
  , projectId :: Projects.ProjectId
  , userId :: Projects.UserId
  , teamId :: Text
  , slackUserId :: Text
  , verifier :: SlackPKCE
  }


newtype SlackPKCE = SlackPKCE Text
  deriving newtype (HI.EncodeValue)


instance HI.DecodeValue SlackPKCE where
  decodeValue = refineText "OAuth PKCE verifier" \text -> SlackPKCE text <$ guard (T.length text >= 43 && T.length text <= 128 && T.all (\c -> isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("-._~" :: String)) text)


newSlackPKCE :: UUID.UUIDEff :> es => Eff es SlackPKCE
newSlackPKCE = SlackPKCE . foldMap UUID.toText <$> replicateM 2 UUID.genUUID


slackPKCEVerifier :: SlackPKCE -> Text
slackPKCEVerifier (SlackPKCE text) = text


slackPKCEChallenge :: SlackPKCE -> Text
slackPKCEChallenge verifier = T.dropWhileEnd (== '=') $ extractBase64 $ B64URL.encodeBase64 $ BA.convert (hash (encodeUtf8 (slackPKCEVerifier verifier) :: ByteString) :: Digest SHA256)


-- | Claim once before contacting Slack. Keep personal receipts until the grant is
-- saved so a disconnect during exchange can invalidate the in-flight consent.
consumeSlackAuthorization :: DB es => UUIDId "slack_install" -> Projects.UserId -> Eff es (Maybe SlackAuthorization)
consumeSlackAuthorization stateId userId = do
  request <-
    Hasql.interpOne @(Projects.ProjectId, Bool, Maybe Text, Maybe Text, Maybe SlackPKCE)
      $ [HI.sql|UPDATE apis.slack_install_requests request SET consumed_at = now()
    FROM projects.project_members member, projects.projects project, users.users account
    WHERE request.id = #{stateId} AND request.user_id = #{userId} AND request.expires_at > now()
      AND request.consumed_at IS NULL
      AND member.project_id = request.project_id AND member.user_id = request.user_id
      AND project.id = member.project_id AND account.id = member.user_id
      AND ((request.team_id IS NULL AND member.permission = ^{enumLit ProjectMembers.PAdmin})
        OR EXISTS (SELECT 1 FROM apis.slack_identities identity JOIN apis.slack installation
          ON installation.team_id = identity.team_id AND installation.project_id = request.project_id
          WHERE identity.team_id = request.team_id AND identity.slack_user_id = request.slack_user_id AND identity.user_id = request.user_id))|]
      <> ProjectMembers.activeMembership
      <> [HI.sql| RETURNING request.project_id, request.onboarding, request.team_id, request.slack_user_id, request.code_verifier |]
  pure $ request >>= \case
    (pid, onboarding, Nothing, Nothing, Nothing) -> Just $ WorkspaceAuthorization $ SlackInstall pid onboarding
    (pid, _, Just team, Just slackUser, Just verifier) -> Just $ EvidenceAuthorization $ SlackEvidenceConsent stateId pid userId team slackUser verifier
    _ -> Nothing


saveSlackEvidenceGrant :: (DB es, UUID.UUIDEff :> es) => ByteString -> SlackEvidenceConsent -> SlackUserGrant -> Eff es (Either Secrets.SecretError Bool)
saveSlackEvidenceGrant key consent grant = runExceptT do
  (token, refresh, expiresAt) <- ExceptT $ sealSlackGrant key grant
  affected <-
    lift
      $ withSlackCredentialLock consent.projectId consent.userId
      . fmap HI.getRowsAffected
      . Hasql.queryTx
      $ [HI.sql|WITH consent AS (
      DELETE FROM apis.slack_install_requests request
      USING projects.project_members member, projects.projects project, users.users account, apis.slack installation, apis.slack_identities identity
      WHERE request.id = #{consent.id} AND request.user_id = #{consent.userId} AND request.project_id = #{consent.projectId}
        AND request.team_id = #{grant.teamId} AND request.slack_user_id = #{grant.slackUserId}
        AND request.consumed_at IS NOT NULL AND request.expires_at > now()
        AND installation.project_id = request.project_id AND installation.team_id = request.team_id
        AND identity.team_id = request.team_id AND identity.slack_user_id = request.slack_user_id AND identity.user_id = request.user_id
        AND member.project_id = request.project_id AND member.user_id = request.user_id
        AND project.id = member.project_id AND account.id = member.user_id|]
      <> ProjectMembers.activeMembership
      <> [HI.sql|RETURNING request.project_id, request.user_id, request.team_id, request.slack_user_id
    ) INSERT INTO apis.slack_user_credentials (project_id, user_id, team_id, slack_user_id, access_token, refresh_token, expires_at, scopes)
      SELECT project_id, user_id, team_id, slack_user_id, #{token}, #{refresh}, #{expiresAt}, #{grant.scopes} FROM consent
      ON CONFLICT (project_id, user_id, team_id, slack_user_id) DO UPDATE SET
        access_token = EXCLUDED.access_token, refresh_token = EXCLUDED.refresh_token, expires_at = EXCLUDED.expires_at, scopes = EXCLUDED.scopes, refresh_state = #{Aeson RefreshIdle}|]
  pure $ affected == 1


sealSlackGrant :: UUID.UUIDEff :> es => ByteString -> SlackUserGrant -> Eff es (Either Secrets.SecretError (Text, Maybe Text, Maybe UTCTime))
sealSlackGrant key grant = runExceptT do
  token <- ExceptT $ Secrets.encryptSecret key $ encodeUtf8 grant.accessToken
  let (refreshToken, expiresAt) = case grant.lifetime of
        NonExpiring -> (Nothing, Nothing)
        Rotating refresh expires -> (Just refresh, Just expires)
  refresh <- traverse (ExceptT . Secrets.encryptSecret key . encodeUtf8) refreshToken
  pure (token, refresh, expiresAt)


data SlackEvidenceSubject = SlackEvidenceSubject {projectId :: Projects.ProjectId, userId :: Projects.UserId, teamId :: Text, slackUserId :: Text}
  deriving stock (Generic)
  deriving anyclass (Aeson.ToJSON, HI.DecodeRow)


data SlackCredentialFailure = SlackNotConnected | SlackAccountAmbiguous | SlackRefreshPending UTCTime | SlackRefreshUncertain | SlackRefreshRevoked | SlackRefreshStorageFailure Secrets.SecretError
  deriving stock (Eq, Show)


memberSlackEvidenceSubject :: DB es => Projects.ProjectId -> Projects.UserId -> Eff es (Either SlackCredentialFailure SlackEvidenceSubject)
memberSlackEvidenceSubject project user = do
  candidates <-
    Hasql.interp @[SlackEvidenceSubject]
      [HI.sql|SELECT credential.project_id, credential.user_id, credential.team_id, credential.slack_user_id
    FROM apis.slack_user_credentials credential
    JOIN apis.slack installation ON installation.project_id = credential.project_id AND installation.team_id = credential.team_id
    JOIN apis.slack_identities identity ON identity.team_id = credential.team_id AND identity.slack_user_id = credential.slack_user_id AND identity.user_id = credential.user_id
    WHERE credential.project_id = #{project} AND credential.user_id = #{user} LIMIT 2|]
  pure $ case candidates of
    [subject] -> Right subject
    [] -> Left SlackNotConnected
    _ -> Left SlackAccountAmbiguous


data SlackRefreshState = RefreshIdle | RefreshClaimed UUID.UUID UTCTime | RefreshUncertain UUID.UUID UTCTime
  deriving stock (Eq, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)


data SlackCredentialSnapshot = SlackCredentialSnapshot {version :: Text, refreshState :: SlackRefreshState, grant :: SlackUserGrant}


resolveSlackEvidenceGrant :: (DB es, HTTP.HTTP :> es, Time.Time :> es, UUID.UUIDEff :> es) => ByteString -> Text -> Text -> SlackEvidenceSubject -> Eff es (Either SlackCredentialFailure SlackUserGrant)
resolveSlackEvidenceGrant key clientId clientSecret subject = do
  now <- Time.currentTime
  credential <- readSlackCredential key subject
  case credential of
    Nothing -> pure $ Left SlackNotConnected
    Just snapshot -> case snapshot.grant.lifetime of
      NonExpiring -> pure $ Right snapshot.grant
      Rotating refresh expires
        | expires > addUTCTime 60 now -> pure $ Right snapshot.grant
        | otherwise -> case snapshot.refreshState of
            RefreshIdle -> refreshCredential now snapshot refresh
            RefreshClaimed _ started -> current $ if now < addUTCTime 120 started then SlackRefreshPending (addUTCTime 120 started) else SlackRefreshUncertain
            RefreshUncertain{} -> current SlackRefreshUncertain
  where
    refreshCredential now snapshot refresh = do
      owner <- UUID.genUUID
      let claimedState = RefreshClaimed owner now
          claimed = snapshot{refreshState = claimedState}
          uncertain = void $ changeSlackRefreshState subject claimed $ RefreshUncertain owner now
      acquired <- changeSlackRefreshState subject snapshot claimedState
      if not acquired
        then current $ SlackRefreshPending $ addUTCTime 1 now
        else
          ( do
              response <-
                RateLimit.withRateLimits subject.teamId (OAuth.exchangeUserToken clientId clientSecret $ OAuth.RefreshUserToken $ OAuth.SlackTokenRefresh subject.teamId subject.slackUserId snapshot.grant.scopes refresh)
                  `Exception.catch` \(err :: RateLimit.SlackRateLimited) -> do
                    void $ changeSlackRefreshState subject claimed RefreshIdle
                    Exception.throwIO err
              case response of
                Left OAuth.OAuthRevoked -> do
                  removed <- withSlackCredentialLock subject.projectId subject.userId $ Hasql.queryTx @HI.RowsAffected $ [HI.sql|DELETE FROM apis.slack_user_credentials target|] <> slackCredentialVersionSql subject claimed
                  if HI.getRowsAffected removed == 1 then pure $ Left SlackRefreshRevoked else current SlackRefreshRevoked
                Left _ -> uncertain >> current SlackRefreshUncertain
                Right grant -> do
                  sealed <- sealSlackGrant key grant
                  case sealed of
                    Left failure -> uncertain $> Left (SlackRefreshStorageFailure failure)
                    Right (token, nextRefresh, expiresAt) -> do
                      withSlackCredentialLock subject.projectId subject.userId
                        . Hasql.executeTx
                        $ [HI.sql|UPDATE apis.slack_user_credentials target SET access_token = #{token}, refresh_token = #{nextRefresh}, expires_at = #{expiresAt}, scopes = #{grant.scopes}, refresh_state = #{Aeson RefreshIdle}|]
                        <> slackCredentialVersionSql subject claimed
                      current SlackNotConnected
          )
            `Exception.onException` uncertain
    current failure = do
      now <- Time.currentTime
      credential <- readSlackCredential key subject
      pure $ case credential of
        Nothing -> Left SlackNotConnected
        Just snapshot -> if usableSlackGrant now snapshot.grant then Right snapshot.grant else Left failure


changeSlackRefreshState :: DB es => SlackEvidenceSubject -> SlackCredentialSnapshot -> SlackRefreshState -> Eff es Bool
changeSlackRefreshState subject snapshot next = withSlackCredentialLock subject.projectId subject.userId do
  affected <- Hasql.queryTx @HI.RowsAffected $ [HI.sql|UPDATE apis.slack_user_credentials target SET refresh_state = #{Aeson next}|] <> slackCredentialVersionSql subject snapshot
  pure $ HI.getRowsAffected affected == 1


slackCredentialVersionSql :: SlackEvidenceSubject -> SlackCredentialSnapshot -> HI.Sql
slackCredentialVersionSql subject snapshot =
  [HI.sql| WHERE target.project_id = #{subject.projectId} AND target.user_id = #{subject.userId} AND target.team_id = #{subject.teamId} AND target.slack_user_id = #{subject.slackUserId}
    AND target.access_token = #{snapshot.version} AND target.refresh_state = #{Aeson snapshot.refreshState}
    AND EXISTS (SELECT 1|]
    <> slackCredentialScopeSql subject
    <> [HI.sql|)|]


getSlackEvidenceGrant :: (DB es, Time.Time :> es) => ByteString -> Projects.ProjectId -> Projects.UserId -> Text -> Text -> Eff es (Maybe SlackUserGrant)
getSlackEvidenceGrant key pid userId teamId slackUserId = do
  now <- Time.currentTime
  credential <- readSlackCredential key $ SlackEvidenceSubject pid userId teamId slackUserId
  pure $ (.grant) <$> (credential >>= \snapshot -> snapshot <$ guard (usableSlackGrant now snapshot.grant))


usableSlackGrant :: UTCTime -> SlackUserGrant -> Bool
usableSlackGrant now grant = case grant.lifetime of
  NonExpiring -> True
  Rotating _ expires -> expires > now


readSlackCredential :: DB es => ByteString -> SlackEvidenceSubject -> Eff es (Maybe SlackCredentialSnapshot)
readSlackCredential key subject = do
  stored <-
    Hasql.interpOne @(Text, Maybe Text, Maybe UTCTime, V.Vector Text, Aeson SlackRefreshState)
      $ [HI.sql|SELECT credential.access_token, credential.refresh_token, credential.expires_at, credential.scopes, credential.refresh_state|]
      <> slackCredentialScopeSql subject
  pure $ stored >>= \(token, refresh, expiresAt, scopes, Aeson refreshState) -> do
    accessToken <- decodeUtf8 <$> rightToMaybe (Secrets.decryptSecret key token)
    refreshToken <- traverse (fmap decodeUtf8 . rightToMaybe . Secrets.decryptSecret key) refresh
    lifetime <- case (refreshToken, expiresAt) of
      (Nothing, Nothing) -> Just NonExpiring
      (Just refreshValue, Just expiry) -> Just $ Rotating refreshValue expiry
      _ -> Nothing
    pure $ SlackCredentialSnapshot token refreshState $ SlackUserGrant subject.teamId subject.slackUserId accessToken lifetime scopes


slackCredentialScopeSql :: SlackEvidenceSubject -> HI.Sql
slackCredentialScopeSql subject =
  [HI.sql| FROM apis.slack_user_credentials credential
      JOIN apis.slack installation ON installation.project_id = credential.project_id AND installation.team_id = credential.team_id
      JOIN apis.slack_identities identity ON identity.team_id = credential.team_id AND identity.slack_user_id = credential.slack_user_id AND identity.user_id = credential.user_id
      JOIN projects.project_members member ON member.project_id = credential.project_id AND member.user_id = credential.user_id
      JOIN projects.projects project ON project.id = member.project_id JOIN users.users account ON account.id = member.user_id
      WHERE credential.project_id = #{subject.projectId} AND credential.user_id = #{subject.userId} AND credential.team_id = #{subject.teamId} AND credential.slack_user_id = #{subject.slackUserId}|]
    <> ProjectMembers.activeMembership


revokeSlackEvidence :: DB es => Projects.ProjectId -> Projects.UserId -> Eff es ()
revokeSlackEvidence pid userId = Hasql.transaction TxS.ReadCommitted TxS.Write $ revokeSlackEvidenceTx pid userId MemberGrants


data SlackGrantSelection = MemberGrants | SlackUserGrants Text (V.Vector Text)


revokeSlackEvidenceTx :: Projects.ProjectId -> Projects.UserId -> SlackGrantSelection -> Tx.Transaction ()
revokeSlackEvidenceTx pid userId selection = withSlackCredentialLockTx pid userId do
  let selected = case selection of
        MemberGrants -> [HI.sql|TRUE|]
        SlackUserGrants team users -> [HI.sql|team_id = #{team} AND slack_user_id = ANY(#{users})|]
  Hasql.executeTx
    $ [HI.sql|WITH pending AS (DELETE FROM apis.slack_install_requests WHERE project_id = #{pid} AND user_id = #{userId} AND team_id IS NOT NULL AND (|]
    <> selected
    <> [HI.sql|)) DELETE FROM apis.slack_user_credentials WHERE project_id = #{pid} AND user_id = #{userId} AND (|]
    <> selected
    <> [HI.sql|)|]


-- Finish the receipt with the cleanup so a retry cannot revoke a later consent.
recordSlackTokenRevocation :: DB es => Text -> Text -> V.Vector Text -> Eff es ()
recordSlackTokenRevocation team eventId users = Hasql.transaction TxS.ReadCommitted TxS.Write do
  receipts <-
    Hasql.queryTx @[UUIDId "slack_event"]
      [HI.sql|SELECT id FROM apis.slack_events WHERE team_id = #{team} AND event_id = #{eventId}
      AND processed_at IS NULL AND payload #>> '{event,type}' = 'tokens_revoked'
      AND COALESCE(payload #> '{event,tokens,oauth}', '[]'::jsonb) = #{Aeson $ Aeson.toJSON users}
      FOR UPDATE|]
  for_ receipts \receipt -> do
    members <-
      Hasql.queryTx @[(Projects.ProjectId, Projects.UserId)]
        [HI.sql|SELECT project_id, user_id FROM (
        SELECT project_id, user_id FROM apis.slack_user_credentials WHERE team_id = #{team} AND slack_user_id = ANY(#{users})
        UNION SELECT project_id, user_id FROM apis.slack_install_requests WHERE team_id = #{team} AND slack_user_id = ANY(#{users})
      ) affected ORDER BY project_id, user_id|]
    for_ members \(pid, userId) -> revokeSlackEvidenceTx pid userId $ SlackUserGrants team users
    Hasql.executeTx [HI.sql|UPDATE apis.slack_events SET processed_at = now() WHERE id = #{receipt}|]


-- Separate statements give disconnect a fresh snapshot after an in-flight grant commits.
withSlackCredentialLock :: DB es => Projects.ProjectId -> Projects.UserId -> Tx.Transaction a -> Eff es a
withSlackCredentialLock pid userId = Hasql.transaction TxS.ReadCommitted TxS.Write . withSlackCredentialLockTx pid userId


withSlackCredentialLockTx :: Projects.ProjectId -> Projects.UserId -> Tx.Transaction a -> Tx.Transaction a
withSlackCredentialLockTx pid userId action = do
  void $ Hasql.queryTx @[HI.OneColumn Projects.UserId] [HI.sql|SELECT user_id FROM projects.project_members WHERE project_id = #{pid} AND user_id = #{userId} FOR UPDATE|]
  action


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
