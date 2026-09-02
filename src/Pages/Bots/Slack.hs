module Pages.Bots.Slack (linkProjectGetH, slackActionsH, SlackEventPayload, slackEventsPostH, getSlackChannels, getSlackChannelInfo, SlackChannelsResponse (..), SlackActionForm, externalOptionsH, slackInteractionsH, SlackInteraction (..), sendSlackWelcomeMessage, sendSlackWelcomeViaWebhook, logWelcomeMessageFailure) where

import BackgroundJobs qualified as BgJobs
import Control.Lens ((.~), (^.), (^?))
import Data.Aeson (withObject)
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as KEM
import Data.Aeson.KeyMap qualified as AEKM
import Data.Aeson.Lens (key, _Bool, _String)
import Data.Default (Default (def))
import Data.Effectful.Wreq (
  HTTP,
  defaults,
  getWith,
  postWith,
  responseBody,
 )
import Data.Pool (withResource)
import Data.Text qualified as T

import Control.Exception (ErrorCall (..))
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Effectful (Eff, IOE, type (:>))
import Effectful.Error.Static (throwError)
import Effectful.Log qualified as Log
import Effectful.Reader.Static (ask, asks)
import Models.Apis.Integrations (SlackData (..), getDashboardsForSlack, getProjectSlackData, getSlackDataByTeamId, insertAccessToken, updateSlackDefaultChannel)
import Models.Apis.Issues qualified as Issues
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types (statusIsSuccessful)
import Network.Wreq qualified as Wreq
import Network.Wreq.Types (FormParam)
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig, PageCtx (..), currProject, pageTitle, sessM)
import Pages.Bots.Utils (BotErrorType (..), BotResponse (..), BotType (..), Channel, authHeader, botEmoji, contentTypeHeader, detectReportIntent, formatBotError, getLoadingMessage, imageBlock, installedResponse, mrkdwn, parseInstallState, plainTxt, runBotQuery, textBlock, withBotThread)
import Pkg.Components.Widget (Widget (..), widgetPngUrl)
import Pkg.DeriveUtils (idFromText)
import PyF
import Relude hiding (ask, asks)
import Relude.Extra.Tuple (dup)
import Servant.API (Header)
import Servant.API.ResponseHeaders (Headers, addHeader)
import Servant.Server (ServerError (errBody), err400)
import System.Config (AuthContext (backgroundScope, env, pool), EnvConfig (..))
import System.Tracing (forkBackground)
import System.Types (ATBaseCtx, DB)
import UnliftIO.Exception (throwIO, tryAny)
import Web.FormUrlEncoded (FromForm)


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


linkProjectGetH :: Maybe Text -> Maybe Text -> ATBaseCtx (Headers '[Header "Location" Text] BotResponse)
linkProjectGetH slack_code stateM = do
  let (pidM, isOnboarding) = parseInstallState stateM
  let bwconf = (def :: BWConfig){sessM = Nothing, currProject = Nothing, pageTitle = "Slack app installed"}
  case pidM of
    Nothing -> pure $ addHeader "" $ NoTokenFound $ PageCtx bwconf ()
    Just pid -> do
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
          void $ insertAccessToken pid token'.team.id token'.incomingWebhook.channelId token'.team.name token'.accessToken token'.incomingWebhook.channel token'.incomingWebhook.url
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
  case interaction.command of
    "/monoscope-here" ->
      withSlackDataByTeam "/monoscope-here" interaction.team_id (runMonoscopeHere interaction)
        <&> fromMaybe workspaceNotLinkedResp
        >>= traceResp
    "/dashboard" -> do
      dashboards <- V.fromList <$> getDashboardsForSlack interaction.team_id
      when (V.null dashboards) $ throwError err400{errBody = "No dashboards found for this project"}
      slackDataM <- withSlackDataByTeam "/dashboard" interaction.team_id \slackData ->
        triggerSlackModal slackData.botToken "open" $ AE.object ["trigger_id" AE..= interaction.trigger_id, "view" AE..= dashboardView interaction.channel_id (V.fromList [dashboardSelectBlock "dashboard-select" "*Select dashboard*" dashboards])]
      when (isNothing slackDataM) $ throwError err400{errBody = "This Slack workspace is not linked to a Monoscope project. Please reinstall the Monoscope app."}
      traceResp $ textResp "modal opened"
    _ -> do
      slackDataM <- getSlackDataByTeamId interaction.team_id
      when (isNothing slackDataM) $ Log.logAttention ("Slack slash command for unlinked workspace" :: Text) $ AE.object ["team_id" AE..= interaction.team_id, "command" AE..= interaction.command]
      forkBackground authCtx.backgroundScope ("Slack slash command (team " <> interaction.team_id <> ")")
        $ maybe
          (sendSlackFollowupResponse interaction.response_url (formatBotError Slack ServiceError))
          (\sd -> runBotQuery Slack (sendSlackFollowupResponse interaction.response_url) authCtx.env sd.projectId interaction.text (pure Nothing))
          slackDataM
      traceResp $ textResp $ getLoadingMessage (detectReportIntent interaction.text)
  where
    traceResp resp = resp <$ Log.logTrace ("Slack interaction response" :: Text) resp

    workspaceNotLinkedResp =
      AE.object
        [ "response_type" AE..= ("ephemeral" :: Text)
        , "text" AE..= ("This Slack workspace is not linked to a Monoscope project. Please install the Monoscope app from your project's integrations page." :: Text)
        , "replace_original" AE..= True
        , "delete_original" AE..= True
        ]

    runMonoscopeHere :: SlackInteraction -> SlackData -> ATBaseCtx AE.Value
    runMonoscopeHere inter slackData = do
      -- Refresh apis.slack's display cache for the "default" channel and
      -- ensure the team routes alerts there.
      _ <- updateSlackDefaultChannel inter.team_id inter.channel_id Nothing
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


-- | @team@ is what ties an interaction to a project: everything else in the
-- payload is chosen by the sender's client, including the dashboard id in a
-- selected option, so it is the only field a scoped lookup can trust.
-- 'Maybe' because Slack omits it on some payload shapes; absent means we cannot
-- establish a tenant and must not answer.
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


data SlackOption = SlackOption
  { text :: AE.Value
  , value :: Text
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON)


data SAction = SAction
  { action_id :: Text
  , selected_option :: Maybe SlackOption
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON)


slackActionsH :: SlackActionForm -> ATBaseCtx AE.Value
slackActionsH action = do
  authCtx <- ask @AuthContext
  case AE.eitherDecode @SlackAction (encodeUtf8 action.payload) of
    Left err -> throwError err400{errBody = "Invalid action payload: " <> encodeUtf8 (toText err)}
    Right slackAction -> case slackAction.type_ of
      "block_actions" -> case slackAction.actions >>= viaNonEmpty head of
        Just a | a.action_id == "dashboard-select" -> maybe (pure $ textResp "No dashboard selected") (handleDashboardSelect slackAction) a.selected_option
        Just a | a.action_id == "widget-select" -> maybe noAction (updateWidgetModal authCtx slackAction . (.value)) a.selected_option
        _ -> noAction
      "view_submission" -> handleViewSubmission authCtx slackAction
      _ -> noAction
  where
    noAction = pure $ AE.object []

    -- Scoped to the workspace's own project. @opt.value@ is the dashboard id the
    -- sender's client submitted, so looking it up unscoped renders another
    -- tenant's dashboard into this workspace's modal.
    handleDashboardSelect slackAction opt = case (slackAction.team, idFromText opt.value) of
      (Just team, Just did) ->
        getSlackDataByTeamId team.id
          >>= maybe (pure Nothing) (\sd -> Dashboards.getDashboardByProjectId sd.projectId did)
          >>= maybe noAction (\dashboardVM -> updateDashboardModal slackAction dashboardVM opt.text)
      _ -> noAction

    handleViewSubmission authCtx slackAction = do
      let meta = slackAction.view.private_metadata
          pid = metaField 1 meta
          widgetTitle = fromMaybe "" $ slackAction.view.state >>= lookupSelectedValueByKey "widget-select"
          dashBoardId = fromMaybe "" $ slackAction.view.state >>= lookupSelectedValueByKey "dashboard-select"
          heading = "<" <> authCtx.env.hostUrl <> "p/" <> pid <> "/dashboards/" <> dashBoardId <> "|" <> widgetTitle <> ">"
          content =
            AE.object
              [ "channel" AE..= metaField 0 meta
              , "blocks"
                  AE..= AE.Array
                    ( V.fromList
                        [ textBlock "section" (mrkdwn heading)
                        , textBlock "section" $ mrkdwn ("Shared by <@" <> slackAction.user.id <> "> using /dashboard")
                        , imageBlock (metaField 3 meta) widgetTitle
                        ]
                    )
              ]
      case idFromText pid of
        Nothing -> Log.logAttention ("Slack view_submission with unparseable pid" :: Text) $ AE.object ["private_metadata" AE..= meta]
        Just projectId -> void $ withProjectSlackDataLogged "slackActionsH.view_submission" projectId \sd ->
          sendSlackChatMessage sd.botToken content
      noAction

    updateDashboardModal slackAction dashboardVM dashboardText = do
      let baseTemplate = fromMaybe "" dashboardVM.baseTemplate
      dashboardM <- liftIO $ Dashboards.readDashboardFile "static/public/dashboards" (toString baseTemplate)
      case dashboardM of
        Nothing -> Log.logAttention "Slack updateDashboardModal: readDashboardFile failed" $ AE.object ["base_template" AE..= baseTemplate, "project_id" AE..= dashboardVM.projectId]
        Just dashboard -> do
          let pMeta = T.intercalate "___" [metaField 0 slackAction.view.private_metadata, dashboardVM.projectId.toText, baseTemplate]
          void $ withProjectSlackDataLogged "slackActionsH.updateDashboardModal" dashboardVM.projectId \sd ->
            triggerSlackModal sd.botToken "update" $ AE.object ["view_id" AE..= slackAction.view.id, "view" AE..= dashboardView pMeta (selectBlocks dashboard.widgets V.empty)]
      pure $ textResp $ "Selected dashboard: " <> show dashboardText

    updateWidgetModal authCtx slackAction widgetTitle = do
      let meta = slackAction.view.private_metadata
          pid = metaField 1 meta
          baseTemplate = metaField 2 meta
      dashboardM <- liftIO $ Dashboards.readDashboardFile "static/public/dashboards" (toString baseTemplate)
      case dashboardM of
        Nothing -> Log.logAttention "Slack updateWidgetModal: readDashboardFile failed" $ AE.object ["base_template" AE..= baseTemplate, "project_id" AE..= pid]
        Just dashboard ->
          whenJust (find ((== widgetTitle) . fromMaybe "Untitled-" . (.title)) dashboard.widgets) \w -> whenJust (idFromText pid) \projectId -> do
            chartUrl' <- widgetPngUrl authCtx.env.apiKeyEncryptionSecretKey authCtx.env.hostUrl projectId w Nothing Nothing Nothing
            let privateMeta = T.intercalate "___" [metaField 0 meta, pid, baseTemplate, chartUrl']
            void $ withProjectSlackDataLogged "slackActionsH.updateWidgetModal" projectId \sd ->
              triggerSlackModal sd.botToken "update" $ AE.object ["view_id" AE..= slackAction.view.id, "view" AE..= dashboardView privateMeta (selectBlocks dashboard.widgets (V.singleton $ imageBlock chartUrl' widgetTitle))]
      noAction


-- | @private_metadata@ is a "___"-joined tuple: channelId, projectId, baseTemplate, chartUrl.
metaField :: Int -> Text -> Text
metaField i = fromMaybe "" . (!!? i) . T.splitOn "___"


-- | The dashboard + widget pickers, plus any trailing blocks (e.g. a chart preview).
selectBlocks :: [Widget] -> V.Vector AE.Value -> V.Vector AE.Value
selectBlocks widgets extra =
  V.fromList [dashboardSelectBlock "dashboard-select" "*Select dashboard*" opts, dashboardSelectBlock "widget-select" "*Select widget*" opts] <> extra
  where
    opts = V.fromList $ map (dup . fromMaybe "Untitled-" . (.title)) widgets


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


dashboardView :: Text -> V.Vector AE.Value -> AE.Value
dashboardView privateData blocks =
  AE.object
    [ "type" AE..= "modal"
    , "callback_id" AE..= ""
    , "title"
        AE..= AE.object
          [ "type" AE..= "plain_text"
          , "text" AE..= "Share a dashboard widget"
          ]
    , "blocks"
        AE..= AE.Array blocks
    , "private_metadata" AE..= privateData
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


externalOptionsH :: AE.Value -> ATBaseCtx AE.Value
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


data EventCallbackData = EventCallbackData
  { team_id :: Text
  , event :: SlackEvent
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON)


data SlackEventPayload
  = UrlVerification Text
  | EventCallback EventCallbackData
  deriving stock (Show)


instance AE.FromJSON SlackEventPayload where
  parseJSON = withObject "SlackEventPayload" \v ->
    v AE..: "type" >>= \case
      ("url_verification" :: Text) -> UrlVerification <$> v AE..: "challenge"
      "event_callback" -> EventCallback <$> AE.parseJSON (AE.Object v)
      other -> fail $ "Unsupported Slack event type: " <> show other


data SlackEvent = SlackMessageEvent
  { text :: Text
  , channel :: Text
  , thread_ts :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON)


slackEventsPostH :: SlackEventPayload -> ATBaseCtx AE.Value
slackEventsPostH payload = do
  envCfg <- asks env
  scopeM <- asks @AuthContext (.backgroundScope)
  case payload of
    UrlVerification challenge -> pure $ AE.object ["challenge" AE..= challenge]
    EventCallback cb -> do
      forkBackground scopeM ("Slack event callback (team " <> cb.team_id <> ")") $ handleEventCallback envCfg cb.event cb.team_id
      pure $ AE.object []
  where
    handleEventCallback envCfg event workspaceId = void $ withSlackDataByTeam "handleEventCallback" workspaceId \slackData -> case event.thread_ts of
      Nothing -> do
        Log.logTrace "Slack fallback_error_message (non-threaded event)" $ AE.object ["team_id" AE..= (workspaceId :: Text), "channel_id" AE..= event.channel]
        sendSlackChatMessage slackData.botToken (mergeSlackContent (formatBotError Slack ServiceError) (AE.object ["channel" AE..= event.channel]))
      Just threadTs -> processThreadedEvent envCfg slackData event workspaceId threadTs

    processThreadedEvent envCfg slackData event workspaceId threadTs = do
      let addThread c = mergeSlackContent c (AE.object ["channel" AE..= event.channel, "thread_ts" AE..= threadTs])
          resolveThread =
            Just
              <$> withBotThread
                Slack
                slackData.projectId
                (Issues.slackThreadToConversationId event.channel threadTs)
                Issues.CTSlackThread
                (AE.object ["channel_id" AE..= event.channel, "thread_ts" AE..= threadTs, "team_id" AE..= (workspaceId :: Text)])
                (fmap (map ((Issues.ChatUser,) . (.text)) . (.messages)) <$> getChannelMessages slackData.botToken event.channel threadTs)
      runBotQuery Slack (sendSlackChatMessage slackData.botToken . addThread) envCfg slackData.projectId event.text resolveThread


newtype SlackThreadedMessage = SlackThreadedMessage {text :: Text}
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


newtype SlackThreadedMessageResponse = SlackThreadedMessageResponse {messages :: [SlackThreadedMessage]}
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON)


getChannelMessages :: (HTTP :> es, Log.Log :> es) => Text -> Text -> Text -> Eff es (Maybe SlackThreadedMessageResponse)
getChannelMessages token channelId ts = do
  response <- getWith (defaults & contentTypeHeader "application/json" & authHeader "Bearer" token & Wreq.params .~ [("channel", channelId), ("ts", ts)]) "https://slack.com/api/conversations.replies"
  let responseBdy = response ^. responseBody
  case AE.eitherDecode responseBdy of
    Right res -> pure $ Just res
    Left err -> Nothing <$ Log.logAttention "Slack conversations.replies decode failed" (AE.object ["error" AE..= err, "channel" AE..= channelId, "ts" AE..= ts, "body" AE..= decodeUtf8 @Text (toStrict responseBdy)])
