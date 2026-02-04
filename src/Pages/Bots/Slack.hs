{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pages.Bots.Slack (linkProjectGetH, slackActionsH, SlackEventPayload, slackEventsPostH, getSlackChannels, SlackChannelsResponse (..), SlackActionForm, externalOptionsH, slackInteractionsH, SlackInteraction) where

import BackgroundJobs qualified as BgJobs
import Control.Lens ((.~), (^.))
import Data.Aeson (withObject)
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as KEM
import Data.Aeson.KeyMap qualified as KEMP
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
import Models.Apis.Issues qualified as Issues
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Slack (SlackData (..), getDashboardsForSlack, getSlackDataByTeamId, insertAccessToken, updateSlackNotificationChannel)
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.Projects qualified as Projects
import Network.Wreq qualified as Wreq
import Network.Wreq.Types (FormParam)
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig, PageCtx (..), currProject, pageTitle, sessM)
import Pages.Bots.Utils (AIQueryResult (..), BotErrorType (..), BotResponse (..), BotType (..), Channel, QueryIntent (..), botEmoji, contentTypeHeader, detectReportIntent, formatBotError, formatHistoryAsContext, formatReportForSlack, getLoadingMessage, handleTableResponse, processAIQuery, processReportQuery, widgetPngUrl)
import Pkg.AI qualified as AI
import Pkg.Components.Widget (Widget (..))
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (idFromText)
import Pkg.Parser (parseQueryToAST)
import Relude hiding (ask, asks)
import Servant.API (Header)
import Servant.API.ResponseHeaders (Headers, addHeader)
import Servant.Server (ServerError (errBody), err400)
import System.Config (AuthContext (env, pool), EnvConfig (..))
import System.Types (ATBaseCtx)
import Utils (toUriStr)
import Web.FormUrlEncoded (FromForm)


data IncomingWebhook = IncomingWebhook
  { channel :: Text
  , channelId :: Text
  , configurationUrl :: Text
  , url :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] IncomingWebhook


newtype TokenResponseTeam = TokenResponseTeam
  { id :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] TokenResponseTeam


data TokenResponse = TokenResponse
  { ok :: Bool
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


linkProjectGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> ATBaseCtx (Headers '[Header "Location" Text] BotResponse)
linkProjectGetH pid slack_code onboardingM = do
  envCfg <- asks env
  pool <- asks pool
  let client_id = envCfg.slackClientId
  let client_secret = envCfg.slackClientSecret
  let redirect_uri = envCfg.slackRedirectUri
  token <- exchangeCodeForToken client_id client_secret (redirect_uri <> pid.toText <> if isJust onboardingM then "?onboarding=true" else "") (fromMaybe "" slack_code)
  let bwconf =
        (def :: BWConfig)
          { sessM = Nothing
          , currProject = Nothing
          , pageTitle = "Slack app installed"
          -- , enableBrowserMonitoring = envCfg.enableBrowserMonitoring
          }
  project <- Projects.projectById pid
  case (token, project) of
    (Just token', Just project') -> do
      n <- insertAccessToken pid token'.incomingWebhook.url token'.team.id token'.incomingWebhook.channelId
      -- Create a background job to send the Slack notification
      _ <- liftIO $ withResource pool $ \conn ->
        createJob conn "background_jobs" $ BgJobs.SlackNotification pid ("Monoscope Bot has been linked to your project: " <> project'.title)
      case onboardingM of
        Just _ -> pure $ addHeader ("/p/" <> pid.toText <> "/onboarding?step=NotifChannel") $ NoContent $ PageCtx bwconf ()
        Nothing -> pure $ addHeader "" $ BotLinked $ PageCtx bwconf "Slack"
    (_, _) -> pure $ addHeader ("/p/" <> pid.toText <> "/onboarding?step=NotifChannel") $ NoTokenFound $ PageCtx bwconf ()


slackInteractionsH :: SlackInteraction -> ATBaseCtx AE.Value
slackInteractionsH interaction = do
  Log.logTrace ("Slack interaction received" :: Text) $ AE.object ["command" AE..= interaction.command, "text" AE..= interaction.text, "team_id" AE..= interaction.team_id, "channel_id" AE..= interaction.channel_id]
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  case interaction.command of
    "/here" -> do
      _ <- updateSlackNotificationChannel interaction.team_id interaction.channel_id
      let channelDisplay = if T.null interaction.channel_name then "this channel" else "#" <> interaction.channel_name
          resp =
            AE.object
              [ "response_type" AE..= "in_channel"
              , "blocks"
                  AE..= AE.Array
                    ( V.fromList
                        [ AE.object ["type" AE..= "header", "text" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= (botEmoji "success" <> " Notification channel set"), "emoji" AE..= True]]
                        , AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*" <> channelDisplay <> "* will now receive:")]]
                        , AE.object
                            [ "type" AE..= "section"
                            , "text"
                                AE..= AE.object
                                  [ "type" AE..= "mrkdwn"
                                  , "text" AE..= ("• " <> botEmoji "error" <> " Error alerts\n• " <> botEmoji "chart" <> " Daily & weekly reports\n• " <> botEmoji "warning" <> " Anomaly detections")
                                  ]
                            ]
                        ]
                    )
              , "replace_original" AE..= True
              , "delete_original" AE..= True
              ]
      Log.logTrace ("Slack interaction response" :: Text) resp
      pure resp
    "/dashboard" -> do
      dashboardsList <- getDashboardsForSlack interaction.team_id
      let dashboards = V.fromList dashboardsList
      when (V.null dashboards) $ throwError err400{errBody = "No dashboards found for this project"}
      _ <- triggerSlackModal authCtx.env.slackBotToken "open" $ AE.object ["trigger_id" AE..= interaction.trigger_id, "view" AE..= dashboardView interaction.channel_id (V.fromList [dashboardViewOne dashboards])]
      let resp = AE.object ["text" AE..= "modal opened", "replace_original" AE..= True, "delete_original" AE..= True]
      Log.logTrace ("Slack interaction response" :: Text) resp
      pure resp
    _ -> do
      slackDataM <- getSlackDataByTeamId interaction.team_id
      void $ forkIO $ do
        case slackDataM of
          Nothing -> sendSlackFollowupResponse interaction.response_url (formatBotError Slack ServiceError)
          Just slackData -> handleAskCommand interaction slackData authCtx
      let loadingMsg = getLoadingMessage (detectReportIntent interaction.text)
          resp = AE.object ["text" AE..= loadingMsg, "replace_original" AE..= True, "delete_original" AE..= True]
      Log.logTrace ("Slack interaction response" :: Text) resp
      pure resp
  where
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
          result <- processAIQuery slackData.projectId inter.text Nothing envCfg.openaiApiKey
          case result of
            Left _ -> do
              let resp = formatBotError Slack ServiceError
              Log.logTrace ("Slack followup response (AI error)" :: Text) resp
              sendSlackFollowupResponse inter.response_url resp
            Right AIQueryResult{..} -> do
              Log.logTrace ("Slack AI query result" :: Text) $ AE.object ["query" AE..= query, "visualization" AE..= visualization]
              let (from, to) = timeRangeStr
              case visualization of
                Just vizType -> do
                  let wType = Widget.mapChatTypeToWidgetType vizType
                      chartType = Widget.mapWidgetTypeToChartType wType
                      query_url = envCfg.hostUrl <> "p/" <> slackData.projectId.toText <> "/log_explorer?viz_type=" <> chartType <> "&query=" <> toUriStr query
                  imageUrl <- widgetPngUrl envCfg.apiKeyEncryptionSecretKey envCfg.hostUrl slackData.projectId def{Widget.wType = wType, Widget.query = Just query} Nothing (Just from) (Just to)
                  let content = getBotContentWithUrl inter.text query query_url imageUrl
                  Log.logTrace ("Slack followup response (chart)" :: Text) content
                  _ <- sendSlackFollowupResponse inter.response_url content
                  pass
                Nothing -> case parseQueryToAST query of
                  Left err -> do
                    let resp = formatBotError Slack (QueryParseError query)
                    Log.logTrace ("Slack followup response (parse error)" :: Text) resp
                    sendSlackFollowupResponse inter.response_url resp
                  Right query' -> do
                    tableAsVecE <- RequestDumps.selectLogTable slackData.projectId query' query Nothing (fromTime, toTime) [] Nothing Nothing
                    let resp = handleTableResponse Slack tableAsVecE envCfg slackData.projectId query
                    Log.logTrace ("Slack followup response (table)" :: Text) resp
                    _ <- sendSlackFollowupResponse inter.response_url resp
                    pass


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

    handleBlockActions authCtx slackAction = do
      let actionTypeM = viaNonEmpty head $ fromMaybe [] slackAction.actions
      case actionTypeM of
        Nothing -> handleUnknownActionType
        Just actionType -> case actionType.action_id of
          "dashboard-select" -> handleDashboardSelect authCtx slackAction actionType
          "widget-select" -> handleWidgetSelect authCtx slackAction actionType
          _ -> handleUnknownActionType

    handleDashboardSelect authCtx slackAction actionType = do
      let selectedOption = actionType.selected_option
      case selectedOption of
        Just opt -> do
          let dashboardId = opt.value
          dashboardVMM <- maybe (pure Nothing) Dashboards.getDashboardById (idFromText dashboardId)
          case dashboardVMM of
            Nothing -> pure $ AE.object []
            Just dashboardVM -> updateDashboardModal authCtx slackAction dashboardVM opt.text
        Nothing -> pure $ AE.object ["text" AE..= "No dashboard selected", "replace_original" AE..= True, "delete_original" AE..= True]

    handleWidgetSelect authCtx slackAction actionType = do
      let selectedOption = actionType.selected_option
      case selectedOption of
        Just opt -> updateWidgetModal authCtx slackAction opt.value
        Nothing -> handleUnknownActionType

    handleViewSubmission authCtx slackAction = do
      let view = slackAction.view
          privateMeta = view.private_metadata
          metas = T.splitOn "___" privateMeta

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
      sendSlackChatMessage authCtx.env.slackBotToken content
      handleUnknownActionType

    updateDashboardModal authCtx slackAction dashboardVM dashboardText = do
      dashboardM <- liftIO $ Dashboards.readDashboardFile "static/public/dashboards" (toString $ fromMaybe "" dashboardVM.baseTemplate)
      whenJust dashboardM $ \dashboard -> do
        let widgets = V.fromList $ (\w -> (fromMaybe "Untitled-" w.title, fromMaybe "Untitled-" w.title)) <$> dashboard.widgets
            channelId = fromMaybe "" $ viaNonEmpty head $ T.splitOn "___" slackAction.view.private_metadata
            pMeta = channelId <> "___" <> dashboardVM.projectId.toText <> "___" <> fromMaybe "" dashboardVM.baseTemplate
        _ <- triggerSlackModal authCtx.env.slackBotToken "update" $ AE.object ["view_id" AE..= slackAction.view.id, "view" AE..= dashboardView pMeta (V.fromList [dashboardViewOne widgets, dashboardViewTwo widgets])]
        pass
      pure $ AE.object ["text" AE..= ("Selected dashboard: " <> show dashboardText), "replace_original" AE..= True, "delete_original" AE..= True]

    updateWidgetModal authCtx slackAction widgetTitle = do
      let metas = T.splitOn "___" slackAction.view.private_metadata
          channelId = fromMaybe "" $ viaNonEmpty head metas
          res = fromMaybe [] $ viaNonEmpty tail metas
          pid = fromMaybe "" $ viaNonEmpty head res
          res' = fromMaybe [] $ viaNonEmpty tail res
          baseTemplate = fromMaybe "" $ viaNonEmpty head res'

      dashboardM <- liftIO $ Dashboards.readDashboardFile "static/public/dashboards" (toString baseTemplate)
      whenJust dashboardM $ \dashboard -> do
        let widgets = V.fromList $ (\w -> (fromMaybe "Untitled-" w.title, fromMaybe "Untitled-" w.title)) <$> dashboard.widgets
            widget = find (\w -> fromMaybe "Untitled-" w.title == widgetTitle) dashboard.widgets
        whenJust widget $ \w -> whenJust (idFromText pid) $ \projectId -> do
          chartUrl' <- widgetPngUrl authCtx.env.apiKeyEncryptionSecretKey authCtx.env.hostUrl projectId w Nothing Nothing Nothing
          let blocks = V.fromList [dashboardViewOne widgets, dashboardViewTwo widgets, dashboardWidgetView chartUrl' widgetTitle]
              privateMeta = channelId <> "___" <> pid <> "___" <> baseTemplate <> "___" <> chartUrl'
          _ <- triggerSlackModal authCtx.env.slackBotToken "update" $ AE.object ["view_id" AE..= slackAction.view.id, "view" AE..= dashboardView privateMeta blocks]
          pass
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
              [ AE.object ["type" AE..= "header", "text" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= (botEmoji "chart" <> " " <> T.take 140 question), "emoji" AE..= True]]
              , AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Query:* `" <> query <> "`")]]
              , AE.object ["type" AE..= "divider"]
              , AE.object ["type" AE..= "image", "image_url" AE..= imageUrl, "alt_text" AE..= ("Chart visualization for: " <> T.take 100 question)]
              , AE.object
                  [ "type" AE..= "actions"
                  , "elements"
                      AE..= AE.Array
                        ( V.fromList
                            [ AE.object
                                [ "type" AE..= "button"
                                , "action_id" AE..= "view-log-explorer-chart"
                                , "text" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= (botEmoji "search" <> " View in Log Explorer"), "emoji" AE..= True]
                                , "url" AE..= query_url
                                , "style" AE..= "primary"
                                ]
                            ]
                        )
                  ]
              ]
          )
    , "response_type" AE..= "in_channel"
    , "replace_original" AE..= True
    , "delete_original" AE..= True
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
      void $ forkIO $ handleEventCallback envCfg cb.event cb.team_id now
      pure $ AE.object []
  where
    handleEventCallback envCfg event teamId now =
      case event.thread_ts of
        Nothing -> sendSlackChatMessage envCfg.slackBotToken (mergeSlackContent (formatBotError Slack ServiceError) (AE.object ["channel" AE..= event.channel]))
        Just threadTs -> processThreadedEvent envCfg event teamId threadTs now

    processThreadedEvent envCfg event teamId threadTs now = do
      slackDataM <- getSlackDataByTeamId teamId
      case slackDataM of
        Nothing -> pass
        Just slackData -> do
          -- Generate deterministic conversation ID from channel + thread_ts
          let convId = Issues.slackThreadToConversationId event.channel threadTs

          -- Ensure conversation exists
          _ <-
            Issues.getOrCreateConversation
              slackData.projectId
              convId
              Issues.CTSlackThread
              (AE.object ["channel_id" AE..= event.channel, "thread_ts" AE..= threadTs, "team_id" AE..= teamId])

          -- Load existing history from DB
          existingHistory <- Issues.selectChatHistory convId

          -- One-time migration: if DB is empty, fetch from Slack API and migrate
          when (null existingHistory) $ do
            replies <- getChannelMessages envCfg.slackBotToken event.channel threadTs
            case replies of
              Just messages -> forM_ messages.messages $ \m ->
                Issues.insertChatMessage slackData.projectId convId "user" m.text Nothing Nothing
              Nothing -> pass

          -- Process the current message with history from DB
          processMessages envCfg event slackData convId threadTs now

    processMessages envCfg event slackData convId threadTs now = do
      -- Build thread context from DB history
      dbMessages <- Issues.selectChatHistory convId
      let threadContext = formatHistoryAsContext "Slack" $ map AI.dbMessageToLLMMessage dbMessages

      -- Use processAIQuery with thread context
      result <- processAIQuery slackData.projectId event.text (Just threadContext) envCfg.openaiApiKey
      case result of
        Left _ -> sendSlackChatMessage envCfg.slackBotToken (mergeSlackContent (formatBotError Slack ServiceError) (AE.object ["channel" AE..= event.channel, "thread_ts" AE..= threadTs]))
        Right AIQueryResult{..} -> do
          -- Save user message and bot response to DB
          Issues.insertChatMessage slackData.projectId convId "user" event.text Nothing Nothing
          Issues.insertChatMessage slackData.projectId convId "assistant" query Nothing Nothing

          let (from, to) = timeRangeStr
          case visualization of
            Just vizType -> do
              let wType = Widget.mapChatTypeToWidgetType vizType
                  chartType = Widget.mapWidgetTypeToChartType wType
                  query_url = envCfg.hostUrl <> "p/" <> slackData.projectId.toText <> "/log_explorer?viz_type=" <> chartType <> "&query=" <> toUriStr query
              imageUrl <- widgetPngUrl envCfg.apiKeyEncryptionSecretKey envCfg.hostUrl slackData.projectId def{Widget.wType = wType, Widget.query = Just query} Nothing (Just from) (Just to)
              let content' = getBotContentWithUrl event.text query query_url imageUrl
                  content = mergeSlackContent content' (AE.object ["channel" AE..= event.channel, "thread_ts" AE..= threadTs])
              _ <- sendSlackChatMessage envCfg.slackBotToken content
              pass
            Nothing -> case parseQueryToAST query of
              Left _ -> sendSlackChatMessage envCfg.slackBotToken (mergeSlackContent (formatBotError Slack (QueryParseError query)) (AE.object ["channel" AE..= event.channel, "thread_ts" AE..= threadTs]))
              Right query' -> do
                tableAsVecE <- RequestDumps.selectLogTable slackData.projectId query' query Nothing (fromTime, toTime) [] Nothing Nothing
                let content' = handleTableResponse Slack tableAsVecE envCfg slackData.projectId query
                    content = case content' of
                      AE.Object o -> AE.Object (o <> KEMP.fromList [("channel", AE.String event.channel), ("thread_ts", AE.String threadTs)])
                      _ -> content'
                _ <- sendSlackChatMessage envCfg.slackBotToken content
                pass


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
  , channels :: [Channel]
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON)


getSlackChannels :: (HTTP :> es, Log.Log :> es) => Text -> Text -> Eff es (Maybe SlackChannelsResponse)
getSlackChannels token team_id = do
  let url = "https://slack.com/api/conversations.list"
      opts =
        defaults & header "Authorization" .~ ["Bearer " <> encodeUtf8 token] & Wreq.param "team_id" .~ [team_id]

  r <- getWith opts url
  let resBody = r ^. responseBody
  case AE.eitherDecode resBody of
    Right val -> return $ Just val
    Left err -> do
      Log.logAttention ("Error decoding Slack channels response: " <> toText err) ()
      return Nothing


data SlackThreadedMessageResponse = SlackThreadedMessageResponse
  { ok :: Bool
  , messages :: [SlackThreadedMessage]
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON)


getChannelMessages :: HTTP :> es => Text -> Text -> Text -> Eff es (Maybe SlackThreadedMessageResponse)
getChannelMessages token channelId ts = do
  let url = "https://slack.com/api/conversations.replies"
      opts = defaults & header "Content-Type" .~ ["application/json"] & header "Authorization" .~ [encodeUtf8 $ "Bearer " <> token]
      params = [("channel", channelId), ("ts", ts)]
  response <- getWith (opts & Wreq.params .~ params) (toString url)
  let responseBdy = response ^. responseBody
  case AE.eitherDecode responseBdy of
    Right res -> return $ Just res
    Left err -> do
      return Nothing
