{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pages.Bots.Slack (linkProjectGetH, slackActionsH, SlackEventPayload, slackEventsPostH, getSlackChannels, getSlackChannelInfo, SlackChannelsResponse (..), SlackActionForm, externalOptionsH, slackInteractionsH, SlackInteraction (..), sendSlackWelcomeMessage, logWelcomeMessageFailure) where

import BackgroundJobs qualified as BgJobs
import Control.Lens ((.~), (^.))
import Data.Aeson (withObject)
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as KEM

import Data.Aeson.Types (parseMaybe)
import Data.Default (Default (def))
import Data.Effectful.Wreq (
  HTTP,
  defaults,
  getWith,
  header,
  postWith,
  responseBody,
 )
import Data.Pool (withResource)
import Data.Text qualified as T

import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Effectful (Eff, type (:>))
import Effectful.Concurrent (forkIO)
import Effectful.Error.Static (throwError)
import Effectful.Log qualified as Log
import Effectful.Reader.Static (ask, asks)
import Effectful.Time qualified as Time
import Models.Apis.Integrations (SlackData (..), getDashboardsForSlack, getProjectSlackData, getSlackDataByTeamId, insertAccessToken, updateSlackNotificationChannel)
import Models.Apis.Issues qualified as Issues
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Network.Wreq qualified as Wreq
import Network.Wreq.Types (FormParam)
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig, PageCtx (..), currProject, pageTitle, sessM)
import Pages.Bots.Utils (BotErrorType (..), BotResponse (..), BotType (..), Channel, QueryIntent (..), botEmoji, contentTypeHeader, detectReportIntent, dispatchAIResponse, formatBotError, formatHistoryAsContext, formatReportForSlack, getLoadingMessage, processAIQuery, processReportQuery)
import Pkg.AI qualified as AI
import Pkg.Components.Widget (Widget (..), widgetPngUrl)
import Pkg.DeriveUtils (idFromText)
import PyF
import Relude hiding (ask, asks)
import Servant.API (Header)
import Servant.API.ResponseHeaders (Headers, addHeader)
import Servant.Server (ServerError (errBody), err400)
import System.Config (AuthContext (env, pool), EnvConfig (..))
import System.Types (ATBaseCtx, DB)
import UnliftIO.Exception (tryAny)
import Web.FormUrlEncoded (FromForm)


-- | Log-and-return-Nothing helper: missing slackData is always an anomaly (the caller
-- either just received an event from Slack or is acting on behalf of an authed project).
withSlackDataByTeam :: (DB es, Log.Log :> es) => Text -> Text -> (SlackData -> Eff es a) -> Eff es (Maybe a)
withSlackDataByTeam ctx teamId k =
  getSlackDataByTeamId teamId >>= \case
    Nothing -> do
      Log.logAttention ("Missing SlackData for team_id" :: Text) $ AE.object ["context" AE..= ctx, "team_id" AE..= teamId]
      pure Nothing
    Just sd -> Just <$> k sd


withProjectSlackDataLogged :: (DB es, Log.Log :> es) => Text -> Projects.ProjectId -> (SlackData -> Eff es a) -> Eff es (Maybe a)
withProjectSlackDataLogged ctx pid k =
  getProjectSlackData pid >>= \case
    Nothing -> do
      Log.logAttention ("Missing SlackData for project" :: Text) $ AE.object ["context" AE..= ctx, "project_id" AE..= pid]
      pure Nothing
    Just sd -> Just <$> k sd


logWelcomeMessageFailure :: Log.Log :> es => Text -> SomeException -> Eff es ()
logWelcomeMessageFailure channelId err =
  Log.logAttention ("Failed to send Slack welcome message" :: Text)
    $ AE.object ["error" AE..= show @Text err, "channel" AE..= channelId]


data IncomingWebhook = IncomingWebhook
  { channel :: Text
  , channelId :: Text
  , configurationUrl :: Text
  , url :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] IncomingWebhook


data TokenResponseTeam = TokenResponseTeam
  { id :: Text
  , name :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] TokenResponseTeam


data TokenResponse = TokenResponse
  { ok :: Bool
  , accessToken :: Text
  , incomingWebhook :: IncomingWebhook
  , team :: TokenResponseTeam
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] TokenResponse


exchangeCodeForToken :: HTTP :> es => Text -> Text -> Text -> Text -> Eff es (Maybe TokenResponse)
exchangeCodeForToken clientId clientSecret redirectUri code = do
  let formData :: [FormParam]
      formData =
        [ "client_id" Wreq.:= clientId
        , "client_secret" Wreq.:= clientSecret
        , "code" Wreq.:= code
        , "redirect_uri" Wreq.:= redirectUri
        ]

  let hds = header "Content-Type" .~ ["application/x-www-form-urlencoded; charset=utf-8"]
  response <- postWith (defaults & hds) "https://slack.com/api/oauth.v2.access" formData
  let responseBdy = response ^. responseBody
  case AE.decode responseBdy of
    Just token -> do
      return $ Just token
    Nothing -> return Nothing


-- | Parse state parameter: "projectId" or "projectId__onboarding"
parseSlackState :: Maybe Text -> (Maybe Projects.ProjectId, Bool)
parseSlackState stateM = (pidM, isOnboarding)
  where
    parts = maybe [] (T.splitOn "__") stateM
    pidM = Projects.projectIdFromText =<< viaNonEmpty head parts
    isOnboarding = length parts > 1


linkProjectGetH :: Maybe Text -> Maybe Text -> ATBaseCtx (Headers '[Header "Location" Text] BotResponse)
linkProjectGetH slack_code stateM = do
  let (pidM, isOnboarding) = parseSlackState stateM
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
          void $ insertAccessToken pid token'.incomingWebhook.url token'.team.id token'.incomingWebhook.channelId token'.team.name token'.accessToken token'.incomingWebhook.channel
          void $ Projects.enableNotificationChannel pid Projects.NSlack
          void $ liftIO $ withResource pool $ \conn -> createJob conn "background_jobs" $ BgJobs.SlackNotification pid ("Monoscope Bot has been linked to your project: " <> project'.title)
          wasAdded <- ProjectMembers.addSlackChannelToEveryoneTeam pid token'.incomingWebhook.channelId
          when wasAdded do
            result <- tryAny $ sendSlackWelcomeMessage token'.accessToken token'.incomingWebhook.channelId project'.title
            whenLeft_ result (logWelcomeMessageFailure token'.incomingWebhook.channelId)
          if isOnboarding
            then pure $ addHeader ("/p/" <> pid.toText <> "/onboarding?step=NotifChannel") $ NoContent $ PageCtx bwconf ()
            else pure $ addHeader "" $ BotLinked $ PageCtx bwconf ("Slack", Just pid)
        _ -> pure $ addHeader ("/p/" <> pid.toText <> "/settings/integrations") $ NoTokenFound $ PageCtx bwconf ()


slackInteractionsH :: SlackInteraction -> ATBaseCtx AE.Value
slackInteractionsH interaction = do
  Log.logTrace ("Slack interaction received" :: Text) $ AE.object ["command" AE..= interaction.command, "text" AE..= interaction.text, "team_id" AE..= interaction.team_id, "channel_id" AE..= interaction.channel_id]
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  case interaction.command of
    "/monoscope-here" -> do
      resp <-
        withSlackDataByTeam "/monoscope-here" interaction.team_id (runMonoscopeHere interaction)
          <&> fromMaybe workspaceNotLinkedResp
      Log.logTrace ("Slack interaction response" :: Text) resp
      pure resp
    "/dashboard" -> do
      dashboardsList <- getDashboardsForSlack interaction.team_id
      let dashboards = V.fromList dashboardsList
      when (V.null dashboards) $ throwError err400{errBody = "No dashboards found for this project"}
      slackDataM <- withSlackDataByTeam "/dashboard" interaction.team_id \slackData ->
        triggerSlackModal slackData.botToken "open" $ AE.object ["trigger_id" AE..= interaction.trigger_id, "view" AE..= dashboardView interaction.channel_id (V.fromList [dashboardViewOne dashboards])]
      case slackDataM of
        Nothing -> throwError err400{errBody = "This Slack workspace is not linked to a Monoscope project. Please reinstall the Monoscope app."}
        Just () -> do
          let resp = AE.object ["text" AE..= "modal opened", "replace_original" AE..= True, "delete_original" AE..= True]
          Log.logTrace ("Slack interaction response" :: Text) resp
          pure resp
    _ -> do
      slackDataM <- getSlackDataByTeamId interaction.team_id
      when (isNothing slackDataM) $ Log.logAttention ("Slack slash command for unlinked workspace" :: Text) $ AE.object ["team_id" AE..= interaction.team_id, "command" AE..= interaction.command]
      void $ forkIO $ do
        resultE <- tryAny $ case slackDataM of
          Nothing -> sendSlackFollowupResponse interaction.response_url (formatBotError Slack ServiceError)
          Just slackData -> handleAskCommand interaction slackData authCtx
        whenLeft_ resultE \err -> Log.logAttention "Slack slash command background task failed" $ AE.object ["error" AE..= show @Text err, "team_id" AE..= interaction.team_id]
      let loadingMsg = getLoadingMessage (detectReportIntent interaction.text)
          resp = AE.object ["text" AE..= loadingMsg, "replace_original" AE..= True, "delete_original" AE..= True]
      Log.logTrace ("Slack interaction response" :: Text) resp
      pure resp
  where
    workspaceNotLinkedResp =
      AE.object
        [ "response_type" AE..= ("ephemeral" :: Text)
        , "text" AE..= ("This Slack workspace is not linked to a Monoscope project. Please install the Monoscope app from your project's integrations page." :: Text)
        , "replace_original" AE..= True
        , "delete_original" AE..= True
        ]

    runMonoscopeHere :: SlackInteraction -> SlackData -> ATBaseCtx AE.Value
    runMonoscopeHere inter slackData = do
      _ <- updateSlackNotificationChannel inter.team_id inter.channel_id
      void $ Projects.enableNotificationChannel slackData.projectId Projects.NSlack
      wasAdded <- ProjectMembers.addSlackChannelToEveryoneTeam slackData.projectId inter.channel_id
      when wasAdded do
        projectM <- Projects.projectById slackData.projectId
        case projectM of
          Nothing -> Log.logAttention ("Slack install references missing project" :: Text) $ AE.object ["project_id" AE..= slackData.projectId, "team_id" AE..= inter.team_id]
          Just project -> do
            result <- tryAny $ sendSlackWelcomeMessage slackData.botToken inter.channel_id project.title
            whenLeft_ result (logWelcomeMessageFailure inter.channel_id)
      let channelDisplay = if T.null inter.channel_name then "this channel" else "#" <> inter.channel_name
      pure
        $ AE.object
          [ "response_type" AE..= ("in_channel" :: Text)
          , "blocks"
              AE..= AE.Array
                ( V.fromList
                    [ AE.object ["type" AE..= ("header" :: Text), "text" AE..= AE.object ["type" AE..= ("plain_text" :: Text), "text" AE..= (botEmoji "success" <> " Notification channel set"), "emoji" AE..= True]]
                    , AE.object ["type" AE..= ("section" :: Text), "text" AE..= AE.object ["type" AE..= ("mrkdwn" :: Text), "text" AE..= ("*" <> channelDisplay <> "* will now receive:")]]
                    , AE.object
                        [ "type" AE..= ("section" :: Text)
                        , "text"
                            AE..= AE.object
                              [ "type" AE..= ("mrkdwn" :: Text)
                              , "text" AE..= ("• " <> botEmoji "error" <> " Error alerts\n• " <> botEmoji "chart" <> " Daily & weekly reports\n• " <> botEmoji "warning" <> " Anomaly detections\n\nYou can also configure channels on the web dashboard.")
                              ]
                        ]
                    ]
                )
          , "replace_original" AE..= True
          , "delete_original" AE..= True
          ]

    handleAskCommand :: SlackInteraction -> SlackData -> AuthContext -> ATBaseCtx ()
    handleAskCommand inter slackData authCtx = do
      let envCfg = authCtx.env
      case detectReportIntent inter.text of
        ReportIntent reportType -> do
          reportResult <- processReportQuery slackData.projectId reportType envCfg
          case reportResult of
            Left err -> do
              let resp = AE.object ["text" AE..= err, "response_type" AE..= "in_channel", "replace_original" AE..= True, "delete_original" AE..= True]
              Log.logTrace ("Slack followup response (report error)" :: Text) resp
              sendSlackFollowupResponse inter.response_url resp
            Right (report, eventsUrl, errorsUrl) -> do
              let resp = formatReportForSlack report slackData.projectId envCfg eventsUrl errorsUrl inter.channel_id
              Log.logTrace ("Slack followup response (report)" :: Text) resp
              sendSlackFollowupResponse inter.response_url resp
        GeneralQueryIntent -> do
          result <- processAIQuery slackData.projectId inter.text Nothing envCfg.openaiModel envCfg.openaiApiKey
          case result of
            Left _ -> do
              let resp = formatBotError Slack ServiceError
              Log.logTrace ("Slack followup response (AI error)" :: Text) resp
              sendSlackFollowupResponse inter.response_url resp
            Right resp ->
              dispatchAIResponse
                Slack
                envCfg
                slackData.projectId
                inter.text
                resp
                (sendSlackFollowupResponse inter.response_url)
                getBotContentWithUrl


newtype SlackActionForm = SlackActionForm {payload :: Text}
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON, FromForm)


data SlackUser = SlackUser
  { id :: Text
  , username :: Text
  , team_id :: Text
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON)


data SlackAction = SlackAction
  { type_ :: Text
  , token :: Text
  , trigger_id :: Text
  , view :: SlackView
  , actions :: Maybe [SAction]
  , user :: SlackUser
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.StripSuffix "_"]] SlackAction


data SlackView = SlackView
  { private_metadata :: Text
  , blocks :: [AE.Value]
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
  { type_a :: Text
  , action_id :: Text
  , block_id :: Text
  , selected_option :: Maybe SlackOption
  , action_ts :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.Rename "type_a" "type"]] SAction


slackActionsH :: SlackActionForm -> ATBaseCtx AE.Value
slackActionsH action = do
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  let result = AE.eitherDecode (encodeUtf8 action.payload) :: Either String SlackAction
  case result of
    Left err -> do
      throwError err400{errBody = "Invalid action payload: " <> encodeUtf8 (toText err)}
    Right slackAction -> handleSlackAction authCtx slackAction
  where
    handleSlackAction authCtx slackAction = case slackAction.type_ of
      "block_actions" -> handleBlockActions authCtx slackAction
      "view_submission" -> handleViewSubmission authCtx slackAction
      _ -> handleUnknownActionType

    handleBlockActions authCtx slackAction =
      case viaNonEmpty head $ fromMaybe [] slackAction.actions of
        Nothing -> handleUnknownActionType
        Just actionType -> case actionType.action_id of
          "dashboard-select" -> handleDashboardSelect slackAction actionType
          "widget-select" -> handleWidgetSelect authCtx slackAction actionType
          _ -> handleUnknownActionType

    handleDashboardSelect slackAction actionType = case actionType.selected_option of
      Just opt -> do
        dashboardVMM <- maybe (pure Nothing) Dashboards.getDashboardById (idFromText opt.value)
        case dashboardVMM of
          Nothing -> pure $ AE.object []
          Just dashboardVM -> updateDashboardModal slackAction dashboardVM opt.text
      Nothing -> pure $ AE.object ["text" AE..= "No dashboard selected", "replace_original" AE..= True, "delete_original" AE..= True]

    handleWidgetSelect authCtx slackAction actionType = case actionType.selected_option of
      Just opt -> updateWidgetModal authCtx slackAction opt.value
      Nothing -> handleUnknownActionType

    handleViewSubmission authCtx slackAction = do
      let view = slackAction.view
          metas = T.splitOn "___" view.private_metadata
          channelId = fromMaybe "" $ viaNonEmpty head metas
          pid = fromMaybe "" $ viaNonEmpty head $ fromMaybe [] $ viaNonEmpty tail metas
          image_url = fromMaybe "" $ viaNonEmpty last metas
          dashBoardId = slackAction.view.state >>= lookupSelectedValueByKey "dashboard-select"
          widgetTitle = slackAction.view.state >>= lookupSelectedValueByKey "widget-select"
          url = authCtx.env.hostUrl <> "p/" <> pid <> "/dashboards/" <> fromMaybe "" dashBoardId
          heading = "<" <> url <> "|" <> fromMaybe "" widgetTitle <> ">"
          content =
            AE.object
              [ "channel" AE..= channelId
              , "blocks"
                  AE..= AE.Array
                    ( V.fromList
                        [ AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= heading]]
                        , AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("Shared by <@" <> slackAction.user.id <> "> using /dashboard")]]
                        , dashboardWidgetView image_url (fromMaybe "" widgetTitle)
                        ]
                    )
              ]
      case idFromText pid of
        Nothing -> Log.logAttention ("Slack view_submission with unparseable pid" :: Text) $ AE.object ["private_metadata" AE..= view.private_metadata]
        Just projectId -> void $ withProjectSlackDataLogged "slackActionsH.view_submission" projectId \sd ->
          sendSlackChatMessage sd.botToken content
      handleUnknownActionType

    updateDashboardModal slackAction dashboardVM dashboardText = do
      let baseTemplate = fromMaybe "" dashboardVM.baseTemplate
      dashboardM <- liftIO $ Dashboards.readDashboardFile "static/public/dashboards" (toString baseTemplate)
      case dashboardM of
        Nothing -> Log.logAttention "Slack updateDashboardModal: readDashboardFile failed" $ AE.object ["base_template" AE..= baseTemplate, "project_id" AE..= dashboardVM.projectId]
        Just dashboard -> do
          let widgets = V.fromList $ (\w -> (fromMaybe "Untitled-" w.title, fromMaybe "Untitled-" w.title)) <$> dashboard.widgets
              channelId = fromMaybe "" $ viaNonEmpty head $ T.splitOn "___" slackAction.view.private_metadata
              pMeta = channelId <> "___" <> dashboardVM.projectId.toText <> "___" <> baseTemplate
          void $ withProjectSlackDataLogged "slackActionsH.updateDashboardModal" dashboardVM.projectId \sd ->
            triggerSlackModal sd.botToken "update" $ AE.object ["view_id" AE..= slackAction.view.id, "view" AE..= dashboardView pMeta (V.fromList [dashboardViewOne widgets, dashboardViewTwo widgets])]
      pure $ AE.object ["text" AE..= ("Selected dashboard: " <> show dashboardText), "replace_original" AE..= True, "delete_original" AE..= True]

    updateWidgetModal authCtx slackAction widgetTitle = do
      let metas = T.splitOn "___" slackAction.view.private_metadata
          channelId = fromMaybe "" $ viaNonEmpty head metas
          res = fromMaybe [] $ viaNonEmpty tail metas
          pid = fromMaybe "" $ viaNonEmpty head res
          res' = fromMaybe [] $ viaNonEmpty tail res
          baseTemplate = fromMaybe "" $ viaNonEmpty head res'

      dashboardM <- liftIO $ Dashboards.readDashboardFile "static/public/dashboards" (toString baseTemplate)
      case dashboardM of
        Nothing -> Log.logAttention "Slack updateWidgetModal: readDashboardFile failed" $ AE.object ["base_template" AE..= baseTemplate, "project_id" AE..= pid]
        Just dashboard -> do
          let widgets = V.fromList $ (\w -> (fromMaybe "Untitled-" w.title, fromMaybe "Untitled-" w.title)) <$> dashboard.widgets
              widget = find (\w -> fromMaybe "Untitled-" w.title == widgetTitle) dashboard.widgets
          whenJust widget $ \w -> whenJust (idFromText pid) $ \projectId -> do
            chartUrl' <- widgetPngUrl authCtx.env.apiKeyEncryptionSecretKey authCtx.env.hostUrl projectId w Nothing Nothing Nothing
            let blocks = V.fromList [dashboardViewOne widgets, dashboardViewTwo widgets, dashboardWidgetView chartUrl' widgetTitle]
                privateMeta = channelId <> "___" <> pid <> "___" <> baseTemplate <> "___" <> chartUrl'
            void $ withProjectSlackDataLogged "slackActionsH.updateWidgetModal" projectId \sd ->
              triggerSlackModal sd.botToken "update" $ AE.object ["view_id" AE..= slackAction.view.id, "view" AE..= dashboardView privateMeta blocks]
      handleUnknownActionType

    handleUnknownActionType = pure $ AE.object []


lookupSelectedValueByKey :: Text -> AE.Value -> Maybe Text
lookupSelectedValueByKey key' = parseMaybe parser
  where
    key = KEM.fromText key'
    parser = AE.withObject "state" $ \o -> do
      values <- o AE..: "values"
      inner <- values AE..: key
      field <- inner AE..: key
      selected <- field AE..: "selected_option"
      selected AE..: "value"


sendSlackFollowupResponse :: Text -> AE.Value -> ATBaseCtx ()
sendSlackFollowupResponse responseUrl content = do
  _ <- postWith (defaults & contentTypeHeader "application/json") (toString responseUrl) content
  pass


triggerSlackModal :: Text -> Text -> AE.Value -> ATBaseCtx ()
triggerSlackModal token action content = do
  let url = toString $ "https://slack.com/api/views." <> action
  res <- postWith (defaults & header "Authorization" .~ [encodeUtf8 $ "Bearer " <> token] & contentTypeHeader "application/json") url content
  pass


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


-- | Build Slack message content with a chart image URL using Block Kit
getBotContentWithUrl :: Text -> Text -> Text -> Text -> AE.Value
getBotContentWithUrl question query query_url imageUrl =
  AE.object
    [ "blocks"
        AE..= AE.Array
          ( V.fromList
              [ AE.object ["type" AE..= ("header" :: Text), "text" AE..= AE.object ["type" AE..= ("plain_text" :: Text), "text" AE..= (botEmoji "chart" <> " " <> question), "emoji" AE..= True]]
              , AE.object ["type" AE..= ("image" :: Text), "image_url" AE..= imageUrl, "alt_text" AE..= ("Chart: " <> question)]
              , AE.object ["type" AE..= ("context" :: Text), "elements" AE..= AE.Array (V.fromList [AE.object ["type" AE..= ("mrkdwn" :: Text), "text" AE..= ("*Query:* `" <> query <> "`")]])]
              , AE.object ["type" AE..= ("actions" :: Text), "elements" AE..= AE.Array (V.fromList [AE.object ["type" AE..= ("button" :: Text), "action_id" AE..= ("view-log-explorer" :: Text), "text" AE..= AE.object ["type" AE..= ("plain_text" :: Text), "text" AE..= (botEmoji "search" <> " View in Log Explorer"), "emoji" AE..= True], "url" AE..= query_url]])]
              ]
          )
    , "response_type" AE..= ("in_channel" :: Text)
    , "replace_original" AE..= True
    ]


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


dashboardViewOne :: V.Vector (Text, Text) -> AE.Value
dashboardViewOne options =
  AE.object
    [ "type" AE..= "section"
    , "block_id" AE..= "dashboard-select"
    , "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= "*Select dashboard*"]
    , "accessory"
        AE..= AE.object
          [ "action_id" AE..= "dashboard-select"
          , "type" AE..= "static_select"
          , "placeholder" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= "Select a dashboard template"]
          , "options" AE..= AE.Array opts
          ]
    ]
  where
    opts = V.map (\(text, value) -> AE.object ["text" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= if T.null text then "Untitled" else text], "value" AE..= value]) options


dashboardViewTwo :: V.Vector (Text, Text) -> AE.Value
dashboardViewTwo options =
  AE.object
    [ "type" AE..= "section"
    , "block_id" AE..= "widget-select"
    , "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= "*Select widget*"]
    , "accessory"
        AE..= AE.object
          [ "action_id" AE..= "widget-select"
          , "type" AE..= "static_select"
          , "placeholder" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= "Select a dashboard template"]
          , "options" AE..= AE.Array opts
          ]
    ]
  where
    opts = V.map (\(text, value) -> AE.object ["text" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= text], "value" AE..= value]) options


dashboardWidgetView :: Text -> Text -> AE.Value
dashboardWidgetView image_url widgetTitle =
  AE.object
    [ "type" AE..= "image"
    , "image_url" AE..= image_url
    , "alt_text" AE..= widgetTitle
    ]


externalOptionsH :: AE.Value -> ATBaseCtx AE.Value
externalOptionsH val = do
  pure
    $ AE.object
      [ "options" AE..= AE.Array (V.fromList [AE.object ["text" AE..= "Option 1", "value" AE..= "option1"], AE.object ["text" AE..= "Option 2", "value" AE..= "option2"]])
      ]


sendSlackChatMessage :: HTTP :> es => Text -> AE.Value -> Eff es ()
sendSlackChatMessage token content = do
  let url = "https://slack.com/api/chat.postMessage"
  let opts = defaults & header "Content-Type" .~ ["application/json"] & header "Authorization" .~ [encodeUtf8 $ "Bearer " <> token]
  rs <- postWith opts (toString url) content
  pass


sendSlackWelcomeMessage :: HTTP :> es => Text -> Text -> Text -> Eff es ()
sendSlackWelcomeMessage token channelId projectTitle = do
  let message =
        AE.object
          [ "channel" AE..= channelId
          , "blocks"
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
  sendSlackChatMessage token message


data UrlVerificationData = UrlVerificationData
  { token :: Text
  , challenge :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON)


data EventCallbackData = EventCallbackData
  { token :: Text
  , team_id :: Text
  , api_app_id :: Text
  , event :: SlackEvent
  , event_id :: Text
  , event_time :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON)


data SlackEventPayload
  = UrlVerification UrlVerificationData
  | EventCallback EventCallbackData
  deriving (Show)


instance AE.FromJSON SlackEventPayload where
  parseJSON = withObject "SlackEventPayload" \v -> do
    typ <- v AE..: "type"
    case typ of
      "url_verification" -> UrlVerification <$> AE.parseJSON (AE.Object v)
      "event_callback" -> EventCallback <$> AE.parseJSON (AE.Object v)
      other -> fail $ "Unsupported Slack event type: " ++ show other


data SlackEvent = SlackMessageEvent
  { type_ :: Text
  , user :: Text
  , text :: Text
  , ts :: Text
  , channel :: Text
  , event_ts :: Text
  , thread_ts :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.StripSuffix "_"]] SlackEvent


slackEventsPostH :: SlackEventPayload -> ATBaseCtx AE.Value
slackEventsPostH payload = do
  envCfg <- asks env
  now <- Time.currentTime
  case payload of
    UrlVerification (UrlVerificationData _ challenge) -> pure $ AE.object ["challenge" AE..= challenge]
    EventCallback cb -> do
      void $ forkIO $ do
        resultE <- tryAny $ handleEventCallback envCfg cb.event cb.team_id now
        whenLeft_ resultE \err -> Log.logAttention "Slack event callback background task failed" $ AE.object ["error" AE..= show @Text err, "team_id" AE..= cb.team_id]
      pure $ AE.object []
  where
    handleEventCallback envCfg event teamId now = void $ withSlackDataByTeam "handleEventCallback" teamId \slackData -> case event.thread_ts of
      Nothing -> sendSlackChatMessage slackData.botToken (mergeSlackContent (formatBotError Slack ServiceError) (AE.object ["channel" AE..= event.channel]))
      Just threadTs -> processThreadedEvent envCfg slackData event teamId threadTs now

    processThreadedEvent envCfg slackData event teamId threadTs now = do
      -- Generate deterministic conversation ID from channel + thread_ts
      let convId = Issues.slackThreadToConversationId event.channel threadTs

      -- Ensure conversation exists
      _ <- Issues.getOrCreateConversation slackData.projectId convId Issues.CTSlackThread (AE.object ["channel_id" AE..= event.channel, "thread_ts" AE..= threadTs, "team_id" AE..= teamId])

      -- Load existing history from DB
      existingHistory <- Issues.selectChatHistory convId

      -- One-time migration: if DB is empty, fetch from Slack API and migrate
      when (null existingHistory) $ do
        lockAcquired <- Issues.tryAcquireChatMigrationLock convId
        when lockAcquired $ do
          result <- tryAny $ do
            replies <- getChannelMessages slackData.botToken event.channel threadTs
            case replies of
              Nothing -> Log.logAttention "Slack chat migration: getChannelMessages failed" $ AE.object ["conv_id" AE..= show @Text convId, "channel" AE..= event.channel, "thread_ts" AE..= threadTs]
              Just messages -> forM_ messages.messages \m ->
                Issues.insertChatMessage slackData.projectId convId "user" m.text Nothing Nothing
          Issues.releaseChatMigrationLock convId
          whenLeft_ result \err ->
            Log.logAttention "Slack chat migration failed" $ AE.object ["error" AE..= show @Text err, "conv_id" AE..= show @Text convId]

      processMessages envCfg event slackData convId threadTs now

    processMessages envCfg event slackData convId threadTs now = do
      -- Build thread context from DB history
      dbMessages <- Issues.selectChatHistory convId
      let threadContext = formatHistoryAsContext "Slack" $ map AI.dbMessageToLLMMessage dbMessages

      -- Use processAIQuery with thread context
      result <- processAIQuery slackData.projectId event.text (Just threadContext) envCfg.openaiModel envCfg.openaiApiKey
      case result of
        Left _ -> sendSlackChatMessage slackData.botToken (mergeSlackContent (formatBotError Slack ServiceError) (AE.object ["channel" AE..= event.channel, "thread_ts" AE..= threadTs]))
        Right resp -> do
          -- Save user message and bot response to DB
          Issues.insertChatMessage slackData.projectId convId "user" event.text Nothing Nothing
          whenJust resp.query \q -> Issues.insertChatMessage slackData.projectId convId "assistant" q Nothing Nothing

          let addThread c = mergeSlackContent c (AE.object ["channel" AE..= event.channel, "thread_ts" AE..= threadTs])
          dispatchAIResponse
            Slack
            envCfg
            slackData.projectId
            event.text
            resp
            (sendSlackChatMessage slackData.botToken . addThread)
            getBotContentWithUrl


data SlackThreadedMessage = SlackThreadedMessage
  { type_ :: Text
  , user :: Text
  , text :: Text
  , thread_ts :: Text
  , ts :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.StripSuffix "_"]] SlackThreadedMessage


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
  let opts = defaults & header "Authorization" .~ ["Bearer " <> encodeUtf8 token] & Wreq.param "channel" .~ [channelId]
  r <- getWith opts "https://slack.com/api/conversations.info"
  case AE.eitherDecode @SlackChannelInfoResponse (r ^. responseBody) of
    Right resp | resp.ok -> pure resp.channel
    Right resp -> Nothing <$ Log.logAttention "Slack conversations.info returned ok=false" (AE.object ["channel" AE..= channelId, "ok" AE..= resp.ok])
    Left err -> Nothing <$ Log.logAttention "Error decoding Slack conversations.info" (AE.object ["error" AE..= err, "channel" AE..= channelId])


getSlackChannels :: (HTTP :> es, Log.Log :> es) => Text -> Text -> Eff es (Maybe SlackChannelsResponse)
getSlackChannels token team_id = do
  let url = "https://slack.com/api/conversations.list"
      opts =
        defaults
          & header "Authorization"
          .~ ["Bearer " <> encodeUtf8 token]
            & Wreq.param "team_id"
          .~ [team_id]
            & Wreq.param "types"
          .~ ["public_channel,private_channel"]
            & Wreq.param "exclude_archived"
          .~ ["true"]
            & Wreq.param "limit"
          .~ ["1000"]

  r <- getWith opts url
  let resBody = r ^. responseBody
  case AE.eitherDecode resBody of
    Right val -> do
      unless val.ok
        $ Log.logAttention "Slack conversations.list returned ok=false"
        $ AE.object
          [ "error" AE..= val.error
          , "needed" AE..= val.needed
          , "provided" AE..= val.provided
          , "team_id" AE..= team_id
          ]
      pure $ Just val
    Left err -> do
      Log.logAttention "Error decoding Slack channels response"
        $ AE.object ["error" AE..= err, "body" AE..= decodeUtf8 @Text (toStrict resBody)]
      pure Nothing


data SlackThreadedMessageResponse = SlackThreadedMessageResponse
  { ok :: Bool
  , messages :: [SlackThreadedMessage]
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON)


getChannelMessages :: (HTTP :> es, Log.Log :> es) => Text -> Text -> Text -> Eff es (Maybe SlackThreadedMessageResponse)
getChannelMessages token channelId ts = do
  let url = "https://slack.com/api/conversations.replies"
      opts = defaults & header "Content-Type" .~ ["application/json"] & header "Authorization" .~ [encodeUtf8 $ "Bearer " <> token]
      params = [("channel", channelId), ("ts", ts)]
  response <- getWith (opts & Wreq.params .~ params) (toString url)
  let responseBdy = response ^. responseBody
  case AE.eitherDecode responseBdy of
    Right res -> pure $ Just res
    Left err -> do
      Log.logAttention "Slack conversations.replies decode failed" $ AE.object ["error" AE..= err, "channel" AE..= channelId, "ts" AE..= ts, "body" AE..= decodeUtf8 @Text (toStrict responseBdy)]
      pure Nothing
