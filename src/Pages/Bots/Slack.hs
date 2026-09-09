{-# LANGUAGE PackageImports #-}

module Pages.Bots.Slack (slackFormPostH, linkIdentityGetH, linkIdentityPostH, SlackLinkForm (..), startInstallGetH, processSlackEvent, verifySlackSignature, linkProjectGetH, slackActionsH, SlackEventPayload, slackEventsPostH, getSlackChannels, getSlackChannelInfo, SlackChannelsResponse (..), SlackActionForm (..), externalOptionsH, slackInteractionsH, SlackInteraction (..), sendSlackWelcomeMessage, sendSlackWelcomeViaWebhook, logWelcomeMessageFailure) where

import BackgroundJobs.Types qualified as BgJobs
import Control.Lens ((.~), (^.), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as KEM
import Data.Aeson.KeyMap qualified as AEKM
import Data.Aeson.Lens (key, _Bool, _String)
import Data.Aeson.Types qualified as AE
import Data.ByteArray qualified as BA
import Data.ByteString.Base16 qualified as B16
import Data.Default (Default (def))
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.UUID qualified as UUID
import Data.Effectful.Wreq (
  HTTP,
  defaults,
  getWith,
  postWith,
  responseBody,
 )
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Hasql.Interpolate qualified as HI
import Lucid qualified as H
import "cryptonite" Crypto.Hash (Digest, SHA256, hashlazy)
import "cryptonite" Crypto.MAC.HMAC qualified as HMAC

import Control.Concurrent (threadDelay)
import Control.Exception (ErrorCall (..))
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Effectful (Eff, IOE, type (:>))
import Effectful.Concurrent.Async (race_)
import Effectful.Error.Static (runErrorNoCallStack, throwError)
import Effectful.Log qualified as Log
import Effectful.Reader.Static (ask, asks)
import Effectful.Time qualified as Time
import Models.Apis.Incidents qualified as Incidents
import Models.Apis.Integrations (SlackData (..), getDashboardsForSlack, getProjectSlackData, getSlackDataByTeamId, insertAccessToken, updateSlackDefaultChannel)
import Models.Apis.Integrations qualified as Integrations
import Models.Apis.Issues qualified as Issues
import Models.Projects.DashboardTemplates qualified as DashboardTemplates
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types (renderSimpleQuery, statusIsSuccessful)
import Network.Wreq qualified as Wreq
import Network.Wreq.Types (FormParam)
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig, PageCtx (..), bodyWrapper, currProject, pageTitle, sessM)
import Pages.Bots.Utils (BotResponse (..), BotType (..), Channel, authHeader, botEmoji, botReplyPayload, contentTypeHeader, detectReportIntent, getLoadingMessage, imageBlock, installedResponse, mrkdwn, plainTxt, runBotQuery, textBlock, withBotThread)
import Pkg.AI qualified as AI
import Pkg.Components.Widget (Widget (..), widgetPngUrl)
import Pkg.DeriveUtils (UUIDId (..), idFromText)
import PyF
import Relude hiding (ask, asks)
import Servant.API (Header)
import Servant.API qualified as Servant
import Servant.API.ResponseHeaders (Headers, addHeader)
import Servant.Server (ServerError (errBody), err400, err401, err403, err503)
import System.Config (AuthContext (backgroundScope, env, pool), EnvConfig (..))
import System.Tracing (forkBackground)
import System.Types (ATAuthCtx, ATBackgroundCtx, ATBaseCtx, DB, RespHeaders, addRespHeaders)
import UnliftIO (withRunInIO)
import UnliftIO.Exception (bracket_, catch, throwIO, tryAny)
import Web.FormUrlEncoded (FromForm, urlDecodeAsForm)


-- | Log-and-return-Nothing helper: missing slackData is always an anomaly (the caller
-- either just received an event from Slack or is acting on behalf of an authed project).
withSlackData :: Log.Log :> es => Text -> AE.Value -> Eff es (Maybe SlackData) -> (SlackData -> Eff es a) -> Eff es (Maybe a)
withSlackData logMsg logFields lookupSlackData k =
  lookupSlackData >>= \case
    Nothing -> Nothing <$ Log.logAttention logMsg logFields
    Just sd -> Just <$> k sd


withSlackDataByTeam :: (DB es, Log.Log :> es) => Text -> Text -> (SlackData -> Eff es a) -> Eff es (Maybe a)
withSlackDataByTeam ctx teamId = withSlackData "Missing SlackData for team_id" (AE.object ["context" AE..= ctx, "team_id" AE..= teamId]) (getSlackDataByTeamId teamId)


withProjectSlackDataLogged :: (DB es, Log.Log :> es) => Text -> Projects.ProjectId -> (SlackData -> Eff es a) -> Eff es (Maybe a)
withProjectSlackDataLogged ctx pid = withSlackData "Missing SlackData for project" (AE.object ["context" AE..= ctx, "project_id" AE..= pid]) (getProjectSlackData pid)


logWelcomeMessageFailure :: Log.Log :> es => Text -> SomeException -> Eff es ()
logWelcomeMessageFailure channelId err =
  Log.logAttention ("Failed to send Slack welcome message" :: Text)
    $ AE.object ["error" AE..= show @Text err, "channel" AE..= channelId]


data IncomingWebhook = IncomingWebhook
  { channel :: Text
  , channelId :: Text
  , url :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] IncomingWebhook


data TokenResponseTeam = TokenResponseTeam
  { id :: Text
  , name :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] TokenResponseTeam


-- | Only the fields we consume; a failed exchange has no @access_token@ so decoding
-- fails there, which is the success check (no need to carry @ok@).
data TokenResponse = TokenResponse
  { accessToken :: Text
  , incomingWebhook :: IncomingWebhook
  , team :: TokenResponseTeam
  , scope :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] TokenResponse


exchangeCodeForToken :: (HTTP :> es, Log.Log :> es) => Text -> Text -> Text -> Text -> Eff es (Maybe TokenResponse)
exchangeCodeForToken clientId clientSecret redirectUri code = do
  let formData :: [FormParam]
      formData =
        [ "client_id" Wreq.:= clientId
        , "client_secret" Wreq.:= clientSecret
        , "code" Wreq.:= code
        , "redirect_uri" Wreq.:= redirectUri
        ]

  response <- postWith (defaults & contentTypeHeader "application/x-www-form-urlencoded; charset=utf-8") "https://slack.com/api/oauth.v2.access" formData
  let responseBdy = response ^. responseBody
  case AE.decode responseBdy of
    Just token -> pure $ Just token
    Nothing ->
      -- Slack returns {"ok":false,"error":"bad_redirect_uri"} etc. here; log
      -- the error field so install failures are diagnosable. Never log raw
      -- body (could contain access_token on partial/odd responses).
      Nothing <$ Log.logAttention "Slack oauth.v2.access token exchange failed" (AE.object ["error" AE..= either Relude.id (const "unparseable_response") (parseSlackOkOrErr responseBdy)])


startInstallGetH :: Projects.ProjectId -> Bool -> ATAuthCtx (Headers '[Header "Location" Text] Servant.NoContent)
startInstallGetH pid onboarding = do
  sess <- Projects.getSession
  stateId <- UUIDId <$> UUID.genUUID
  allowed <- Integrations.createSlackInstall stateId sess.user.id pid onboarding
  unless allowed $ throwError err403
  envCfg <- asks env
  let params =
        [ ("client_id", envCfg.slackClientId)
        , ("scope", "assistant:write,chat:write,commands,incoming-webhook,files:write,app_mentions:read,channels:read,groups:read,channels:history,groups:history,im:history,mpim:history,chat:write.public")
        , ("redirect_uri", envCfg.slackRedirectUri)
        , ("state", stateId.toText)
        ]
      url = "https://slack.com/oauth/v2/authorize" <> decodeUtf8 (renderSimpleQuery True $ map (bimap encodeUtf8 encodeUtf8) params)
  pure $ addHeader url Servant.NoContent


newtype SlackLinkForm = SlackLinkForm {projectId :: Projects.ProjectId}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


linkIdentityGetH :: UUIDId "slack_link" -> ATAuthCtx (RespHeaders (H.Html ()))
linkIdentityGetH linkId = do
  sess <- Projects.getSession
  request <- Integrations.getSlackLink linkId >>= maybe (throwError err403{errBody = "This Slack link has expired or was already used. Ask Monoscope in Slack for a new link."}) pure
  projects <- Integrations.slackLinkProjects request.teamId sess.user.id
  addRespHeaders
    $ bodyWrapper def{sessM = Just sess, pageTitle = "Link Slack account"}
    $ H.main_ [H.class_ "mx-auto max-w-lg p-6 space-y-4"] do
      H.h1_ [H.class_ "text-xl font-semibold text-textStrong"] "Link your Slack account"
      H.p_ [H.class_ "text-sm text-textWeak"] $ H.toHtml $ "Connect Slack user " <> request.slackUserId <> " in workspace " <> request.teamId <> " to your Monoscope account."
      if null projects
        then H.p_ [H.class_ "text-sm text-textWeak"] "You need access to a project connected to this Slack workspace. Ask a project admin to invite you, then request a new link in Slack."
        else H.form_ [H.method_ "post", H.action_ ("/slack/link/" <> linkId.toText), H.class_ "space-y-4"] do
          H.label_ [H.for_ "slack-project", H.class_ "block text-sm font-medium"] "Project to investigate"
          H.select_ [H.id_ "slack-project", H.name_ "projectId", H.required_ "", H.class_ "select select-bordered w-full"]
            $ for_ projects \project -> H.option_ [H.value_ project.projectId.toText] $ H.toHtml project.title
          H.button_ [H.type_ "submit", H.class_ "btn btn-primary"] "Link Slack account"


linkIdentityPostH :: UUIDId "slack_link" -> SlackLinkForm -> ATAuthCtx (RespHeaders (H.Html ()))
linkIdentityPostH linkId form = do
  sess <- Projects.getSession
  linked <- Integrations.completeSlackLink linkId sess.user.id form.projectId
  unless linked $ throwError err403{errBody = "This link cannot be used with this account or project. Request a new link in Slack and check your project access."}
  addRespHeaders
    $ bodyWrapper def{sessM = Just sess, pageTitle = "Slack account linked"}
    $ H.main_ [H.class_ "mx-auto max-w-lg p-6 space-y-4"] do
      H.h1_ [H.class_ "text-xl font-semibold text-textStrong"] "Slack account linked"
      H.p_ [H.class_ "text-sm text-textWeak"] "Return to Slack and send your question again. Monoscope will check your project access before investigating."


linkProjectGetH :: Maybe Text -> Maybe Text -> ATAuthCtx (Headers '[Header "Location" Text] BotResponse)
linkProjectGetH slack_code stateM = do
  sess <- Projects.getSession
  stateId <- maybe (throwError err400) pure $ stateM >>= idFromText
  install <- Integrations.consumeSlackInstall stateId sess.user.id >>= maybe (throwError err403) pure
  let pid = install.projectId
      isOnboarding = install.onboarding
  let bwconf = (def :: BWConfig){sessM = Nothing, currProject = Nothing, pageTitle = "Slack app installed"}
  envCfg <- asks env
  pool <- asks pool
  token <- exchangeCodeForToken envCfg.slackClientId envCfg.slackClientSecret envCfg.slackRedirectUri (fromMaybe "" slack_code)
  project <- Projects.projectById pid
  case (token, project) of
    (Just token', Just project') -> do
      -- Cross-workspace re-install: if the previous apis.slack row was bound to a
      -- different Slack workspace (team_id), channels on @everyone belonging to the
      -- old workspace are orphaned by the new bot token and would produce
      -- channel_not_found on every alert. Clear them before installing the new one.
      existing <- getProjectSlackData pid
      whenJust existing \prev -> when (prev.teamId /= token'.team.id) do
        Log.logAttention ("Slack re-install switching workspaces; clearing old channels" :: Text) $ AE.object ["project_id" AE..= pid, "old_team_id" AE..= prev.teamId, "new_team_id" AE..= token'.team.id]
        ProjectMembers.removeSlackChannelsFromEveryoneTeam pid
      void $ insertAccessToken pid token'.team.id token'.incomingWebhook.channelId token'.team.name token'.accessToken token'.incomingWebhook.channel token'.incomingWebhook.url (Just $ V.fromList $ filter (not . T.null) $ map T.strip $ T.splitOn "," token'.scope)
      void $ liftIO $ withResource pool $ \conn -> createJob conn "background_jobs" $ BgJobs.SlackNotification pid ("Monoscope Bot has been linked to your project: " <> project'.title)
      wasAdded <- ProjectMembers.addSlackChannelToEveryoneTeam pid token'.incomingWebhook.channelId
      when wasAdded do
        -- Bot isn't auto-joined to the picked channel (esp. private ones),
        -- so post the welcome via the channel-bound incoming webhook URL.
        result <- tryAny $ sendSlackWelcomeViaWebhook token'.incomingWebhook.url project'.title
        whenLeft_ result (logWelcomeMessageFailure token'.incomingWebhook.channelId)
      pure $ installedResponse "Slack" pid isOnboarding bwconf
    _ -> pure $ addHeader ("/p/" <> pid.toText <> "/settings/integrations") $ NoTokenFound $ PageCtx bwconf ()


slackInteractionsH :: SlackInteraction -> ATBaseCtx AE.Value
slackInteractionsH interaction = do
  Log.logTrace ("Slack interaction received" :: Text) $ AE.object ["command" AE..= interaction.command, "text" AE..= interaction.text, "team_id" AE..= interaction.team_id, "channel_id" AE..= interaction.channel_id]
  authCtx <- ask @AuthContext
  principalM <- Integrations.resolveSlackPrincipal interaction.team_id interaction.user_id Nothing
  case principalM of
    Nothing ->
      traceResp
        $ AE.object
          [ "response_type" AE..= ("ephemeral" :: Text)
          , "text" AE..= ("Link your Monoscope account by mentioning Monoscope or sending it a direct message, then retry this command. If already linked, check your project access and Slack installation." :: Text)
          ]
    Just principal -> do
      slackData <- getProjectSlackData principal.projectId >>= maybe (throwError err403) pure
      unless (slackData.teamId == interaction.team_id) $ throwError err403
      let access = AI.SlackAccess interaction.team_id interaction.user_id principal.userId
      case interaction.command of
        "/monoscope-here" -> do
          permission <- ProjectMembers.getUserPermission principal.projectId principal.userId
          unless (permission == Just ProjectMembers.PAdmin) $ throwError err403
          AI.requireAgentAccess access principal.projectId
          runMonoscopeHere interaction slackData >>= traceResp
        "/dashboard" -> do
          dashboards <- V.fromList <$> getDashboardsForSlack principal.projectId
          when (V.null dashboards) $ throwError err400{errBody = "No dashboards found for this project"}
          AI.requireAgentAccess access principal.projectId
          triggerSlackModal slackData.botToken "open" $ AE.object ["trigger_id" AE..= interaction.trigger_id, "view" AE..= dashboardView (SlackDashboardContext principal.projectId interaction.user_id interaction.channel_id Nothing) (V.fromList [dashboardSelectBlock "dashboard-select" "*Select dashboard*" dashboards])]
          traceResp $ textResp "modal opened"
        _ -> do
          forkBackground authCtx.backgroundScope ("Slack slash command (team " <> interaction.team_id <> ")")
            $ runBotQuery Slack (sendSlackFollowupResponse interaction.response_url . botReplyPayload) authCtx.env access principal.projectId interaction.text (pure Nothing)
          traceResp $ textResp $ getLoadingMessage (detectReportIntent interaction.text)
  where
    traceResp resp = resp <$ Log.logTrace ("Slack interaction response" :: Text) resp

    runMonoscopeHere :: SlackInteraction -> SlackData -> ATBaseCtx AE.Value
    runMonoscopeHere inter slackData = do
      -- Refresh apis.slack's display cache for the "default" channel and
      -- ensure the team routes alerts there.
      _ <- updateSlackDefaultChannel slackData.projectId inter.channel_id Nothing
      wasAdded <- ProjectMembers.addSlackChannelToEveryoneTeam slackData.projectId inter.channel_id
      when wasAdded
        $ Projects.projectById slackData.projectId
        >>= \case
          Nothing -> Log.logAttention ("Slack install references missing project" :: Text) $ AE.object ["project_id" AE..= slackData.projectId, "team_id" AE..= inter.team_id]
          Just project -> flip whenLeft_ (logWelcomeMessageFailure inter.channel_id) =<< tryAny (sendSlackWelcomeMessage slackData.botToken inter.channel_id project.title)
      let channelDisplay = if T.null inter.channel_name then "this channel" else "#" <> inter.channel_name
      pure
        $ AE.object
          [ "response_type" AE..= ("in_channel" :: Text)
          , "blocks"
              AE..= AE.Array
                ( V.fromList
                    [ textBlock "header" $ plainTxt (botEmoji "success" <> " Notification channel set")
                    , textBlock "section" $ mrkdwn ("*" <> channelDisplay <> "* will now receive:")
                    , textBlock "section" $ mrkdwn ("• " <> botEmoji "error" <> " Error alerts\n• " <> botEmoji "chart" <> " Daily & weekly reports\n• " <> botEmoji "warning" <> " Anomaly detections\n\nYou can also configure channels on the web dashboard.")
                    ]
                )
          , "replace_original" AE..= True
          , "delete_original" AE..= True
          ]


newtype SlackActionForm = SlackActionForm {payload :: Text}
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON, FromForm)


newtype SlackUser = SlackUser {id :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON)


-- | A signed workspace identity still requires the clicking user's live project access.
data SlackAction = SlackAction
  { type_ :: Text
  , view :: SlackView
  , actions :: Maybe [SAction]
  , user :: SlackUser
  , team :: Maybe SlackTeamRef
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.StripSuffix "_"]] SlackAction


newtype SlackTeamRef = SlackTeamRef {id :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON)


data SlackView = SlackView
  { private_metadata :: Text
  , id :: Text
  , state :: Maybe AE.Value
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON)


newtype SlackOption = SlackOption {value :: Text}
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON)


data SAction = SAction
  { action_id :: Text
  , selected_option :: Maybe SlackOption
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON)


data SlackDashboardContext = SlackDashboardContext
  { projectId :: Projects.ProjectId
  , userId :: Text
  , channelId :: Text
  , dashboardId :: Maybe Dashboards.DashboardId
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


slackActionsH :: SlackActionForm -> ATBaseCtx AE.Value
slackActionsH action = do
  envCfg <- asks env
  slackAction <- either (const $ throwError err400) pure $ AE.eitherDecode @SlackAction $ encodeUtf8 action.payload
  context <- either (const $ throwError err400{errBody = "Reopen /dashboard to share a widget."}) pure $ AE.eitherDecode @SlackDashboardContext $ encodeUtf8 slackAction.view.private_metadata
  team <- maybe (throwError err403) pure slackAction.team
  unless (context.userId == slackAction.user.id && not (T.null context.channelId)) $ throwError err403
  principal <- Integrations.resolveSlackPrincipal team.id slackAction.user.id (Just context.projectId) >>= maybe (throwError err403) pure
  slackData <- getProjectSlackData context.projectId >>= maybe (throwError err403) pure
  unless (slackData.teamId == team.id) $ throwError err403
  let access = AI.SlackAccess team.id slackAction.user.id principal.userId
      requireAccess = AI.requireAgentAccess access context.projectId
      updateModal ctx blocks = do
        requireAccess
        triggerSlackModal slackData.botToken "update" $ AE.object ["view_id" AE..= slackAction.view.id, "view" AE..= dashboardView ctx blocks]
      loadDashboard did = do
        requireAccess
        vm <- Dashboards.getDashboardByProjectId context.projectId did >>= maybe (throwError err403) pure
        templates <- DashboardTemplates.getDashboardTemplates envCfg.liveReloadDashboards
        maybe (throwError err400{errBody = "This dashboard is unavailable. Reopen /dashboard."}) pure $ DashboardTemplates.loadDashboardFromVM templates vm
      withWidget selected perform = do
        did <- maybe (throwError err400) pure context.dashboardId
        dashboard <- loadDashboard did
        widget <- maybe (throwError err400{errBody = "This widget changed. Reopen /dashboard."}) pure $ find ((== selected) . widgetSelectionId) dashboard.widgets
        let title = fromMaybe "Untitled" widget.title
        requireAccess
        chartUrl <- widgetPngUrl envCfg.apiKeyEncryptionSecretKey envCfg.hostUrl context.projectId widget Nothing Nothing Nothing
        perform did dashboard title chartUrl
  case slackAction.type_ of
    "block_actions" -> case slackAction.actions >>= viaNonEmpty head of
      Just a | a.action_id == "dashboard-select" -> do
        did <- maybe (throwError err400) pure $ a.selected_option >>= idFromText . (.value)
        dashboard <- loadDashboard did
        updateModal context{dashboardId = Just did} $ selectBlocks dashboard.widgets V.empty
      Just a | a.action_id == "widget-select" -> do
        selected <- maybe (throwError err400) (pure . (.value)) a.selected_option
        withWidget selected \_ dashboard title chartUrl ->
          updateModal context $ selectBlocks dashboard.widgets $ V.singleton $ imageBlock chartUrl title
      _ -> pure ()
    "view_submission" -> do
      selected <- maybe (throwError err400) pure $ slackAction.view.state >>= lookupSelectedValueByKey "widget-select"
      withWidget selected \did _ title chartUrl -> do
        requireAccess
        sendSlackChatMessage slackData.botToken
          $ AE.object
            [ "channel" AE..= context.channelId
            , "blocks"
                AE..= AE.Array
                  ( V.fromList
                      [ textBlock "section" $ mrkdwn $ "<" <> envCfg.hostUrl <> "p/" <> context.projectId.toText <> "/dashboards/" <> did.toText <> "|" <> title <> ">"
                      , textBlock "section" $ mrkdwn $ "Shared by <@" <> slackAction.user.id <> "> using /dashboard"
                      , imageBlock chartUrl title
                      ]
                  )
            ]
    _ -> pure ()
  pure $ AE.object []


-- | Identify the exact widget definition, including templates without widget IDs.
-- Reordering preserves selection; changing a widget invalidates its old option.
widgetSelectionId :: Widget -> Text
widgetSelectionId widget = show (hashlazy (AE.encode widget) :: Digest SHA256)


selectBlocks :: [Widget] -> V.Vector AE.Value -> V.Vector AE.Value
selectBlocks widgets extra =
  V.singleton (dashboardSelectBlock "widget-select" "*Select widget*" opts) <> extra
  where
    opts = V.fromList $ map (\widget -> (fromMaybe "Untitled" widget.title, widgetSelectionId widget)) widgets


-- | Slash-command / modal reply that replaces the invoking message.
textResp :: Text -> AE.Value
textResp t = AE.object ["text" AE..= t, "replace_original" AE..= True, "delete_original" AE..= True]


-- | Slack view state nests the selected value under @values.<blockId>.<actionId>@;
-- both ids are the same here.
lookupSelectedValueByKey :: Text -> AE.Value -> Maybe Text
lookupSelectedValueByKey key' v = v ^? key "values" . key k . key k . key "selected_option" . key "value" . _String
  where
    k = KEM.fromText key'


-- | Slack's response_url is ephemeral (30 min / 5 uses). A 4xx means the URL
-- expired or the channel/user is gone; surface it so failed slash-command
-- follow-ups don't vanish silently.
sendSlackFollowupResponse :: Text -> AE.Value -> ATBaseCtx ()
sendSlackFollowupResponse responseUrl content = do
  Log.logTrace ("Slack followup response" :: Text) content
  rs <- postWith (defaults & contentTypeHeader "application/json") (toString responseUrl) content
  unless (statusIsSuccessful (rs ^. Wreq.responseStatus))
    $ Log.logAttention "Slack followup POST non-2xx"
    $ AE.object
      [ "response_url_prefix" AE..= T.take 60 responseUrl
      , "status" AE..= show @Text (rs ^. Wreq.responseStatus)
      , "body" AE..= T.take 200 (decodeUtf8 @Text (toStrict (rs ^. Wreq.responseBody)))
      ]


-- | views.open / views.update returns ok:false on expired trigger_id, bad
-- view definition, etc. Log on non-ok so failed modals don't silently vanish
-- (user clicks /dashboard, nothing happens, no trace in logs).
triggerSlackModal :: Text -> Text -> AE.Value -> ATBaseCtx ()
triggerSlackModal token action content =
  whenLeftM_ (slackApi token ("views." <> action) content) \err ->
    Log.logAttention "Slack views API rejected" $ AE.object ["action" AE..= action, "error" AE..= err]


data SlackInteraction = SlackInteraction
  { team_id :: Text
  , command :: Text
  , text :: Text
  , response_url :: Text
  , trigger_id :: Text
  , -- , api_app_id :: Text
    channel_id :: Text
  , channel_name :: Text
  , user_id :: Text
  -- , enterprise_id :: Maybe Text
  -- , enterprise_name :: Maybe Text
  -- , team_domain :: Text
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON, FromForm)


-- | Merge two Slack JSON objects (for adding channel/thread_ts to block content)
mergeSlackContent :: AE.Value -> AE.Value -> AE.Value
mergeSlackContent (AE.Object o1) (AE.Object o2) = AE.Object (o1 <> o2)
mergeSlackContent v _ = v


dashboardView :: SlackDashboardContext -> V.Vector AE.Value -> AE.Value
dashboardView privateData blocks =
  AE.object
    [ "type" AE..= "modal"
    , "callback_id" AE..= "monoscope-dashboard"
    , "title"
        AE..= AE.object
          [ "type" AE..= "plain_text"
          , "text" AE..= "Share a dashboard widget"
          ]
    , "blocks"
        AE..= AE.Array blocks
    , "private_metadata" AE..= (decodeUtf8 (AE.encode privateData) :: Text)
    , "submit" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= "Send to channel"]
    ]


dashboardSelectBlock :: Text -> Text -> V.Vector (Text, Text) -> AE.Value
dashboardSelectBlock selectId heading options =
  AE.object
    [ "type" AE..= "section"
    , "block_id" AE..= selectId
    , "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= heading]
    , "accessory"
        AE..= AE.object
          [ "action_id" AE..= selectId
          , "type" AE..= "static_select"
          , "placeholder" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= "Select a dashboard template"]
          , "options" AE..= AE.Array opts
          ]
    ]
  where
    opts = V.map (\(text, value) -> AE.object ["text" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= if T.null text then "Untitled" else text], "value" AE..= value]) options


externalOptionsH :: SlackActionForm -> ATBaseCtx AE.Value
externalOptionsH _ =
  pure
    $ AE.object
      [ "options" AE..= AE.Array (V.fromList [AE.object ["text" AE..= "Option 1", "value" AE..= "option1"], AE.object ["text" AE..= "Option 2", "value" AE..= "option2"]])
      ]


-- | Parse Slack's uniform ok/error envelope. Returns @Right ()@ when
-- @ok:true@, @Left err@ otherwise (including unparseable bodies).
-- Slack's REST endpoints (chat.postMessage, views.open, views.update, etc.)
-- all return HTTP 200 on semantic failures; the real status is in the body.
parseSlackOkOrErr :: LByteString -> Either Text ()
parseSlackOkOrErr body
  | (v >>= (^? key "ok" . _Bool)) == Just True = Right ()
  | otherwise = Left $ fromMaybe "unparseable_response" $ v >>= (^? key "error" . _String)
  where
    v = AE.decode @AE.Value body


-- | POST to @https://slack.com/api/\<method\>@, decoding Slack's ok/error envelope.
slackApi :: HTTP :> es => Text -> Text -> AE.Value -> Eff es (Either Text ())
slackApi token method content =
  parseSlackOkOrErr . (^. responseBody) <$> postWith (defaults & contentTypeHeader "application/json" & authHeader "Bearer" token) (toString $ "https://slack.com/api/" <> method) content


-- | Post to chat.postMessage and log attention on non-ok responses so silent
-- drops (bot not invited, archived channel, etc.) surface in monitoring.
sendSlackChatMessage :: (HTTP :> es, Log.Log :> es) => Text -> AE.Value -> Eff es ()
sendSlackChatMessage token content =
  whenLeftM_ (slackApi token "chat.postMessage" content) \err ->
    Log.logAttention "Slack chat.postMessage rejected" $ AE.object ["channel_id" AE..= fromMaybe "" (content ^? key "channel" . _String), "error" AE..= err]


-- | Like 'sendSlackChatMessage' but throws on non-ok — for callers that
-- wrap with 'tryAny' and want reachability as an exception (e.g. the
-- integrations save handler's per-channel welcome probe).
sendSlackChatMessageChecked :: (HTTP :> es, IOE :> es) => Text -> AE.Value -> Eff es ()
sendSlackChatMessageChecked token content =
  whenLeftM_ (slackApi token "chat.postMessage" content) \err ->
    liftIO $ throwIO $ ErrorCall $ "slack chat.postMessage failed: " <> toString err


welcomeBlocks :: Text -> AE.Object
welcomeBlocks projectTitle =
  AEKM.fromList
    [ "blocks"
        AE..= AE.Array
          ( V.fromList
              [ AE.object
                  [ "type" AE..= "section"
                  , "text"
                      AE..= AE.object
                        [ "type" AE..= "mrkdwn"
                        , "text"
                            AE..= [fmt|🟢 *Monoscope connected!*

This channel will now receive notifications for *{projectTitle}*.|]
                        ]
                  ]
              ]
          )
    ]


sendSlackWelcomeMessage :: (HTTP :> es, IOE :> es) => Text -> Text -> Text -> Eff es ()
sendSlackWelcomeMessage token channelId projectTitle =
  sendSlackChatMessageChecked token $ AE.Object $ AEKM.insert "channel" (AE.String channelId) (welcomeBlocks projectTitle)


-- | Post the welcome message via an OAuth-time incoming webhook. Used when
-- the bot user isn't a member of the channel (typical for private channels
-- picked at install time). Webhook URL is channel-bound, so no "channel"
-- field (Slack rejects it on webhooks). Slack webhooks respond with body
-- "ok" on success — a 200 with a different body means the payload was
-- rejected semantically (invalid_payload, channel_is_archived, no_service).
-- Throws on either HTTP error or body≠ok so tryAny-wrapping callers surface
-- the failure as a welcome-message-failed log with context.
sendSlackWelcomeViaWebhook :: (HTTP :> es, IOE :> es) => Text -> Text -> Eff es ()
sendSlackWelcomeViaWebhook webhookUrl projectTitle = do
  rs <- postWith (defaults & contentTypeHeader "application/json") (toString webhookUrl) (AE.Object $ welcomeBlocks projectTitle)
  let body = rs ^. Wreq.responseBody
  unless (body == "ok" || body == "\"ok\"")
    $ liftIO
    $ throwIO
    $ ErrorCall
    $ "slack webhook rejected payload: "
    <> toString (decodeUtf8 @Text (toStrict body))


data SlackEventPayload
  = UrlVerification {challenge :: Text}
  | EventCallback {team_id :: Text, event_id :: Text, api_app_id :: Text, event :: SlackEvent}
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier DAE.CamelToSnake, DAE.SumTaggedObject "type" "contents"] SlackEventPayload


newtype SlackEvent = SlackEvent AE.Object
  deriving stock (Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON)


data SlackEventHeader = SlackEventHeader
  { eventType :: Text
  , bot_id :: Maybe Text
  , subtype :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier (DAE.Rename "eventType" "type")] SlackEventHeader


data SlackMessage = SlackMessage
  { text :: Text
  , channel :: Text
  , thread_ts :: Maybe Text
  , user :: Text
  , ts :: Text
  , channel_type :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON)


data SlackAssistantThread = SlackAssistantThread
  { user_id :: Text
  , channel_id :: Text
  , thread_ts :: Text
  , context :: AE.Object
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data SlackAssistantEvent = SlackAssistantEvent {assistant_thread :: SlackAssistantThread, event_ts :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON)


data SlackStopEvent = SlackStopEvent {channel :: Text, thread_ts :: Text, user :: Text, event_ts :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON)


data SlackEventKind
  = UserMessage SlackMessage
  | AppMention SlackMessage
  | AgentSessionStopped SlackStopEvent
  | BotMessage
  | MessageEdit
  | AssistantStarted SlackAssistantEvent
  | AssistantContextChanged SlackAssistantEvent
  | UnknownEvent SlackEventHeader
  deriving stock (Generic, Show)


-- | Keep the wire object intact for receipt verification and future event types.
-- Only ordinary human messages are decoded as investigation input.
classifySlackEvent :: SlackEvent -> AE.Parser SlackEventKind
classifySlackEvent wire = do
  header <- AE.parseJSON @SlackEventHeader $ AE.toJSON wire
  let decode :: AE.FromJSON a => AE.Parser a
      decode = AE.parseJSON $ AE.toJSON wire
      human constructor = do
        value <- decode
        unless (all (isJust . Incidents.slackTimestamp) $ value.ts : maybeToList value.thread_ts) $ fail "Invalid Slack message timestamp"
        when (any T.null [value.user, value.channel]) $ fail "Missing Slack message identity"
        pure $ constructor value
      assistant constructor = do
        value <- decode
        unless (all (isJust . Incidents.slackTimestamp) [value.event_ts, value.assistant_thread.thread_ts]) $ fail "Invalid Slack assistant timestamp"
        when (any T.null [value.assistant_thread.user_id, value.assistant_thread.channel_id]) $ fail "Missing Slack assistant identity"
        pure $ constructor value
  case (header.eventType, header.subtype) of
    ("message", Just "message_changed") -> pure MessageEdit
    ("message", Just "bot_message") -> pure BotMessage
    ("message", Nothing) | isJust header.bot_id -> pure BotMessage
    ("message", Nothing) -> human UserMessage
    ("app_mention", Nothing) | isNothing header.bot_id -> human AppMention
    ("agent_session_stopped", _) -> do
      value <- decode
      unless (all (isJust . Incidents.slackTimestamp) [value.event_ts, value.thread_ts]) $ fail "Invalid Slack stop timestamp"
      when (any T.null [value.user, value.channel]) $ fail "Missing Slack stop identity"
      pure $ AgentSessionStopped value
    ("assistant_thread_started", _) -> assistant AssistantStarted
    ("assistant_thread_context_changed", _) -> assistant AssistantContextChanged
    _ -> pure $ UnknownEvent header


-- | Verify Slack's raw request body before decoding it. Both past and future
-- timestamps are bounded to five minutes; the digest comparison is constant-time.
verifySlackSignature :: Text -> UTCTime -> Maybe Text -> Maybe Text -> ByteString -> Bool
verifySlackSignature secret now timestamp signature body = fromMaybe False do
  guard $ not $ T.null secret
  ts <- timestamp
  guard $ not (T.null ts) && T.all (\c -> c >= '0' && c <= '9') ts
  seconds <- readMaybe @Integer $ toString ts
  guard $ abs (utcTimeToPOSIXSeconds now - fromInteger seconds) <= 300
  digest <- signature >>= T.stripPrefix "v0=" >>= rightToMaybe . B16.decode . encodeUtf8
  let signed = "v0:" <> encodeUtf8 ts <> ":" <> body
      expected = BA.convert (HMAC.hmac (encodeUtf8 secret :: ByteString) signed :: HMAC.HMAC SHA256) :: ByteString
  pure $ BA.constEq expected digest


requireSlackSignature :: ByteString -> Maybe Text -> Maybe Text -> ATBaseCtx ()
requireSlackSignature body timestamp signature = do
  envCfg <- asks env
  when (T.null envCfg.slackSigningSecret) $ throwError err503
  now <- Time.currentTime
  unless (verifySlackSignature envCfg.slackSigningSecret now timestamp signature body) $ throwError err401


slackFormPostH :: FromForm a => (a -> ATBaseCtx AE.Value) -> ByteString -> Maybe Text -> Maybe Text -> ATBaseCtx AE.Value
slackFormPostH handler body timestamp signature = do
  requireSlackSignature body timestamp signature
  either (const $ throwError err400) handler $ urlDecodeAsForm $ fromStrict body


slackEventsPostH :: ByteString -> Maybe Text -> Maybe Text -> ATBaseCtx AE.Value
slackEventsPostH body timestamp signature = do
  requireSlackSignature body timestamp signature
  payload <- either (const $ throwError err400) pure $ AE.eitherDecodeStrict @SlackEventPayload body
  case payload of
    UrlVerification challenge -> pure $ AE.object ["challenge" AE..= challenge]
    EventCallback{team_id, event_id, event} -> do
      kind <- either (const $ throwError err400) pure $ AE.parseEither classifySlackEvent event
      receiptId <- UUIDId <$> UUID.genUUID
      let job = Aeson $ AE.toJSON $ BgJobs.ProcessSlackEvent receiptId
      Hasql.interpExecute_
        [HI.sql|WITH receipt AS (
          INSERT INTO apis.slack_events (id, team_id, event_id, payload)
          VALUES (#{receiptId}, #{team_id}, #{event_id}, #{Aeson payload})
          ON CONFLICT (team_id, event_id) DO NOTHING RETURNING id
        ) INSERT INTO background_jobs (run_at, status, payload)
          SELECT now(), 'queued', #{job} FROM receipt|]
      case kind of
        AgentSessionStopped _ -> Integrations.recordSlackStop team_id event_id
        _ -> pure ()
      pure $ AE.object []


processSlackEvent :: UUIDId "slack_event" -> ATBackgroundCtx ()
processSlackEvent receiptId = do
  pending <-
    Hasql.interpOne @(HI.OneColumn (Aeson SlackEventPayload))
      [HI.sql|SELECT payload FROM apis.slack_events WHERE id = #{receiptId} AND processed_at IS NULL|]
  for_ pending \(HI.OneColumn (Aeson payload)) -> case payload of
    UrlVerification _ -> throwIO $ ErrorCall "Stored Slack event is not a callback"
    EventCallback{team_id, event_id, event} -> do
      kind <- either (throwIO . ErrorCall) pure $ AE.parseEither classifySlackEvent event
      let process = do
            stillPending <- Hasql.interpOne @(HI.OneColumn Bool) [HI.sql|SELECT TRUE FROM apis.slack_events WHERE id = #{receiptId} AND processed_at IS NULL|]
            when (isJust stillPending) do
              envCfg <- asks env
              captured <- Incidents.captureSlackRoot envCfg.slackAppId receiptId
              when (Just "monoscope_incident_root" == (AE.toJSON event ^? key "metadata" . key "event_type" . _String) && not captured)
                $ Log.logAttention "Slack root observation did not match the configured app or incident destination" (AE.object ["receipt_id" AE..= receiptId])
              result <- runErrorNoCallStack @ServerError $ handleEventCallback envCfg kind team_id event_id
              either (throwIO . ErrorCall . show) pure result
              Hasql.interpExecute_ [HI.sql|UPDATE apis.slack_events SET processed_at = now() WHERE id = #{receiptId}|]
      case kind of
        UserMessage message -> withThreadLock team_id message process
        AppMention message -> withThreadLock team_id message process
        _ -> process
  where
    -- A transaction-scoped advisory lock stays on one checked-out connection.
    -- Other DB operations use their usual pools; stop ingress never waits on it.
    withThreadLock workspaceId message action = do
      connectionPool <- asks pool
      let lockKey = "slack-investigation:" <> decodeUtf8 (toStrict $ AE.encode ([workspaceId, message.channel, fromMaybe message.ts message.thread_ts] :: [Text]))
      withRunInIO \run -> withResource connectionPool \conn -> PGS.withTransaction conn do
        acquired <- PGS.query conn "SELECT pg_try_advisory_xact_lock(hashtextextended(?, 0))" (PGS.Only lockKey)
        unless (acquired == [PGS.Only True]) $ throwIO SlackThreadBusy
        run
          $ race_
            ( forever $ liftIO do
                void (PGS.query_ conn "SELECT 1" :: IO [PGS.Only Int])
                threadDelay 500_000
            )
            action

    handleEventCallback envCfg kind workspaceId eventId =
      case kind of
        AgentSessionStopped _ -> Integrations.recordSlackStop workspaceId eventId
        UserMessage message -> handleMessage False envCfg message workspaceId
        AppMention message -> handleMessage True envCfg message workspaceId
        AssistantStarted session -> saveAssistantContext workspaceId session
        AssistantContextChanged session -> saveAssistantContext workspaceId session
        BotMessage -> Log.logTrace "Slack bot message retained without starting an investigation" (AE.object [])
        MessageEdit -> Log.logTrace "Slack message edit retained without starting an investigation" (AE.object [])
        UnknownEvent header -> Log.logTrace "Unsupported Slack event retained" (AE.object ["team_id" AE..= workspaceId, "event_type" AE..= header.eventType, "subtype" AE..= header.subtype])

    handleMessage mentioned envCfg event workspaceId = do
      let threadTs = fromMaybe event.ts event.thread_ts
      threadProject <- Integrations.slackThreadProject workspaceId event.channel threadTs
      if mentioned || event.channel_type == Just "im" || isJust threadProject
        then
          Integrations.resolveSlackPrincipal workspaceId event.user threadProject >>= \case
            Nothing -> do
              linkId <- UUIDId <$> UUID.genUUID
              request <- Integrations.createSlackLink linkId receiptId >>= maybe (throwError err403) pure
              void $ withSlackDataByTeam "Slack identity link" workspaceId \slackData -> do
                let content =
                      AE.object
                        [ "channel" AE..= request.channelId
                        , "user" AE..= request.slackUserId
                        , "text" AE..= ("Link your Monoscope account and choose a project before investigating: <" <> envCfg.hostUrl <> "slack/link/" <> request.id.toText <> "|Link account>. This private link expires in 15 minutes.")
                        ]
                result <- slackApi slackData.botToken "chat.postEphemeral" content
                either (throwIO . ErrorCall . toString) pure result
            Just principal -> do
              bound <- Integrations.bindSlackInvestigation principal workspaceId event.channel threadTs
              unless bound $ throwError err403{errBody = "Slack investigation thread belongs to another project"}
              void $ withProjectSlackDataLogged "Slack authorized investigation" principal.projectId \slackData ->
                if Integrations.slackAgentScopesGranted slackData
                  then processThreadedEvent principal envCfg slackData event workspaceId threadTs
                  else do
                    AI.requireAgentAccess (AI.SlackAccess workspaceId event.user principal.userId) principal.projectId
                    result <-
                      slackApi slackData.botToken "chat.postEphemeral"
                        $ AE.object
                          [ "channel" AE..= event.channel
                          , "user" AE..= event.user
                          , "text" AE..= ("Reconnect Slack from <" <> envCfg.hostUrl <> "p/" <> principal.projectId.toText <> "/settings/integrations|project integrations> to grant Agent permissions before investigating. A project admin must complete the reconnect.")
                          ]
                    either (throwIO . ErrorCall . toString) pure result
        else Log.logTrace "Slack message is outside an active investigation" (AE.object ["team_id" AE..= workspaceId, "channel_id" AE..= event.channel])

    processThreadedEvent principal envCfg slackData event workspaceId threadTs = do
      unless (slackData.teamId == workspaceId) $ throwError err403
      let access = AI.SlackInvestigationAccess $ AI.SlackInvestigation workspaceId event.user principal.userId event.channel threadTs event.ts
          addThread c = mergeSlackContent c (AE.object ["channel" AE..= event.channel, "thread_ts" AE..= threadTs])
          historyMessage message =
            (if not (T.null envCfg.slackAppId) && message.app_id == Just envCfg.slackAppId then Issues.ChatAssistant else Issues.ChatUser, message.text)
          resolveThread =
            Just
              <$> withBotThread
                Slack
                slackData.projectId
                (Issues.slackScopedConversationId slackData.projectId workspaceId event.channel threadTs)
                Issues.CTSlackThread
                (AE.object ["channel_id" AE..= event.channel, "thread_ts" AE..= threadTs, "team_id" AE..= (workspaceId :: Text)])
                (fmap (map historyMessage . filter ((/= event.ts) . (.ts))) <$> getChannelMessages access slackData.projectId slackData.botToken event.channel threadTs event.ts)
      ( do
          AI.requireAgentAccess access slackData.projectId
          bracket_
            (setSessionStatus slackData.botToken event.channel threadTs Processing)
            ( whenLeftM_ (tryAny $ setSessionStatus slackData.botToken event.channel threadTs Active) \err ->
                Log.logAttention "Slack session status cleanup failed" $ AE.object ["channel_id" AE..= event.channel, "thread_ts" AE..= threadTs, "error" AE..= show @Text err]
            )
            ( race_
                (forever $ AI.requireAgentAccess access slackData.projectId >> liftIO (threadDelay 500_000))
                (runBotQuery Slack (sendSlackChatMessage slackData.botToken . addThread . botReplyPayload) envCfg access slackData.projectId event.text resolveThread)
            )
        )
        `catch` \AI.AgentStopped -> do
          AI.requireAgentAccess (AI.SlackAccess workspaceId event.user principal.userId) slackData.projectId
          sendSlackChatMessage slackData.botToken $ addThread $ AE.object ["text" AE..= ("Investigation stopped. Send another message to continue." :: Text)]


data SlackThreadBusy = SlackThreadBusy
  deriving stock (Generic, Show)
  deriving anyclass (Exception)


data SlackSessionStatus = Processing | Active
  deriving stock (Generic, Show)
  deriving (AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier DAE.CamelToSnake] SlackSessionStatus


data SlackSessionUpdate = SlackSessionUpdate
  { channel_id :: Text
  , thread_ts :: Text
  , status :: SlackSessionStatus
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.ToJSON)


setSessionStatus :: (HTTP :> es, IOE :> es) => Text -> Text -> Text -> SlackSessionStatus -> Eff es ()
setSessionStatus token channel thread status = do
  result <- slackApi token "agents.sessions.setStatus" $ AE.toJSON $ SlackSessionUpdate channel thread status
  either (throwIO . ErrorCall . toString . ("Slack session status rejected: " <>)) pure result


-- | Slack navigation context is untrusted input, not project authorization.
-- Compare event timestamps so a delayed start cannot overwrite newer context.
saveAssistantContext :: DB es => Text -> SlackAssistantEvent -> Eff es ()
saveAssistantContext workspaceId event = do
  let session = event.assistant_thread
  accepted <-
    Hasql.interpOne @(HI.OneColumn Bool)
      [HI.sql|INSERT INTO apis.slack_assistant_threads (team_id, channel_id, thread_ts, user_id, context, context_event_ts)
      VALUES (#{workspaceId}, #{session.channel_id}, #{session.thread_ts}, #{session.user_id}, #{Aeson session.context}, #{event.event_ts}::text::numeric)
      ON CONFLICT (team_id, channel_id, thread_ts) DO UPDATE
      SET context = CASE WHEN slack_assistant_threads.context_event_ts < EXCLUDED.context_event_ts
            THEN EXCLUDED.context ELSE slack_assistant_threads.context END,
          context_event_ts = GREATEST(slack_assistant_threads.context_event_ts, EXCLUDED.context_event_ts)
      WHERE slack_assistant_threads.user_id = EXCLUDED.user_id
      RETURNING TRUE|]
  when (isNothing accepted) $ throwIO $ ErrorCall "Slack assistant thread owner changed"


data SlackThreadedMessage = SlackThreadedMessage
  { text :: Text
  , ts :: Text
  , app_id :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON)


data SlackChannelsResponse = SlackChannelsResponse
  { ok :: Bool
  , channels :: Maybe [Channel]
  , error :: Maybe Text
  , needed :: Maybe Text
  , provided :: Maybe Text
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON)


-- | Look up a single channel by id. Used to render saved channels (DMs, private
-- channels the bot isn't a member of) that don't show up in conversations.list.
data SlackChannelInfoResponse = SlackChannelInfoResponse
  { ok :: Bool
  , channel :: Maybe Channel
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON)


getSlackChannelInfo :: (HTTP :> es, Log.Log :> es) => Text -> Text -> Eff es (Maybe Channel)
getSlackChannelInfo token channelId = do
  r <- getWith (defaults & authHeader "Bearer" token & Wreq.param "channel" .~ [channelId]) "https://slack.com/api/conversations.info"
  case AE.eitherDecode @SlackChannelInfoResponse (r ^. responseBody) of
    Right resp | resp.ok -> pure resp.channel
    Right resp -> Nothing <$ Log.logAttention "Slack conversations.info returned ok=false" (AE.object ["channel" AE..= channelId, "ok" AE..= resp.ok])
    Left err -> Nothing <$ Log.logAttention "Error decoding Slack conversations.info" (AE.object ["error" AE..= err, "channel" AE..= channelId])


getSlackChannels :: (HTTP :> es, Log.Log :> es) => Text -> Text -> Eff es (Maybe SlackChannelsResponse)
getSlackChannels token team_id = do
  let opts = defaults & authHeader "Bearer" token & Wreq.params .~ [("team_id", team_id), ("types", "public_channel,private_channel"), ("exclude_archived", "true"), ("limit", "1000")]
  r <- getWith opts "https://slack.com/api/conversations.list"
  let resBody = r ^. responseBody
  case AE.eitherDecode resBody of
    Right val ->
      Just val
        <$ unless
          val.ok
          ( Log.logAttention "Slack conversations.list returned ok=false"
              $ AE.object ["error" AE..= val.error, "needed" AE..= val.needed, "provided" AE..= val.provided, "team_id" AE..= team_id]
          )
    Left err -> Nothing <$ Log.logAttention "Error decoding Slack channels response" (AE.object ["error" AE..= err, "body" AE..= decodeUtf8 @Text (toStrict resBody)])


data SlackThreadedMessageResponse = SlackThreadedMessageResponse
  { ok :: Bool
  , messages :: Maybe [SlackThreadedMessage]
  , has_more :: Maybe Bool
  , response_metadata :: Maybe SlackResponseMetadata
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON)


newtype SlackResponseMetadata = SlackResponseMetadata {next_cursor :: Maybe Text}
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON)


getChannelMessages :: (DB es, HTTP :> es, Log.Log :> es) => AI.AgentAccess -> Projects.ProjectId -> Text -> Text -> Text -> Text -> Eff es (Maybe [SlackThreadedMessage])
getChannelMessages access pid token channelId ts latest = page [] Nothing
  where
    requireAccess = AI.requireAgentAccess access pid
    page seen cursor = do
      requireAccess
      let params = [("channel", channelId), ("ts", ts), ("latest", latest), ("inclusive", "false"), ("limit", "200")] <> maybe [] (\value -> [("cursor", value)]) cursor
      response <- getWith (defaults & authHeader "Bearer" token & Wreq.params .~ params) "https://slack.com/api/conversations.replies"
      requireAccess
      case AE.eitherDecode @SlackThreadedMessageResponse (response ^. responseBody) of
        Right res
          | res.ok
          , Just messages <- res.messages ->
              case res.response_metadata >>= (.next_cursor) >>= guarded (not . T.null . T.strip) of
                Just next | next `notElem` seen -> fmap (messages <>) <$> page (next : seen) (Just next)
                Just _ -> failed "Repeated history cursor"
                Nothing | res.has_more == Just True -> failed "Incomplete history page without a cursor"
                Nothing -> pure $ Just messages
        Right _ -> failed "Slack rejected the history request"
        Left err -> failed $ toText err
    failed reason = Nothing <$ Log.logAttention "Slack thread backfill failed" (AE.object ["error" AE..= reason, "channel" AE..= channelId, "ts" AE..= ts])
