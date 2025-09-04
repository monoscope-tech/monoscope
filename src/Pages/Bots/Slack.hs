{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pages.Bots.Slack (linkProjectGetH, slackActionsH, SlackEventPayload, slackEventsPostH, SlackActionForm, externalOptionsH, slackInteractionsH, SlackInteraction) where

import BackgroundJobs qualified as BgJobs
import Control.Lens ((.~), (^.))
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
import Data.Time qualified as Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Deriving.Aeson qualified as DAE
import Effectful (Eff, type (:>))
import Effectful.Concurrent (forkIO)
import Effectful.Error.Static (throwError)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask, asks)
import Effectful.Time qualified as Time
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Slack (SlackData (..), getDashboardsForSlack, getSlackDataByTeamId, insertAccessToken, updateSlackNotificationChannel)
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.Projects qualified as Projects
import Network.Wreq qualified as Wreq
import Network.Wreq.Types (FormParam)
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig, PageCtx (..), currProject, pageTitle, sessM)
import Pages.Bots.Utils (BotResponse (..), BotType (..), contentTypeHeader, handleTableResponse)
import Pkg.AI (callOpenAIAPI, systemPrompt)
import Pkg.AI qualified as AI
import Pkg.Components.Widget (Widget (..))
import Pkg.Components.Widget qualified as Widget
import Pkg.Parser (parseQueryToAST)
import Relude hiding (ask, asks)
import Servant.API (Header)
import Servant.API.ResponseHeaders (Headers, addHeader)
import Servant.Server (ServerError (errBody), err400)
import System.Config (AuthContext (env, pool), EnvConfig (..))
import System.Types (ATBaseCtx)
import Utils (toUriStr)
import Utils qualified
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
          }
  project <- liftIO $ withPool pool $ Projects.projectById pid
  case (token, project) of
    (Just token', Just project') -> do
      n <- liftIO $ withPool pool do
        insertAccessToken pid token'.incomingWebhook.url token'.team.id token'.incomingWebhook.channelId
      -- Create a background job to send the Slack notification
      _ <- liftIO $ withResource pool $ \conn ->
        createJob conn "background_jobs" $ BgJobs.SlackNotification pid ("Monoscope Bot has been linked to your project: " <> project'.title)
      case onboardingM of
        Just _ -> pure $ addHeader ("/p/" <> pid.toText <> "/onboarding?step=NotifChannel") $ NoContent $ PageCtx bwconf ()
        Nothing -> pure $ addHeader "" $ BotLinked $ PageCtx bwconf "Slack"
    (_, _) -> pure $ addHeader ("/p/" <> pid.toText <> "/onboarding?step=NotifChannel") $ NoTokenFound $ PageCtx bwconf ()


slackInteractionsH :: SlackInteraction -> ATBaseCtx AE.Value
slackInteractionsH interaction = do
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  case interaction.command of
    "/here" -> do
      _ <- updateSlackNotificationChannel interaction.team_id interaction.channel_id
      pure $ AE.object ["response_type" AE..= "in_channel", "text" AE..= "Done, you'll be receiving project notifcations here going forward", "replace_original" AE..= True, "delete_original" AE..= True]
    "/dashboard" -> do
      dashboards <- getDashboardsForSlack interaction.team_id
      when (null dashboards) $ throwError err400{errBody = "No dashboards found for this project"}
      _ <- triggerSlackModal authCtx.env.slackBotToken "open" $ (AE.object ["trigger_id" AE..= interaction.trigger_id, "view" AE..= (dashboardView interaction.channel_id $ V.fromList [(dashboardViewOne dashboards)])])
      pure $ AE.object ["text" AE..= "modal opened", "replace_original" AE..= True, "delete_original" AE..= True]
    _ -> do
      slackDataM <- dbtToEff $ getSlackDataByTeamId interaction.team_id
      void $ forkIO $ do
        case slackDataM of
          Nothing -> sendSlackFollowupResponse interaction.response_url (AE.object ["text" AE..= "Error: something went wrong"])
          Just slackData -> handleAskCommand interaction slackData authCtx
      pure $ AE.object ["text" AE..= "monoscope is working...", "replace_original" AE..= True, "delete_original" AE..= True]
  where
    handleAskCommand :: SlackInteraction -> SlackData -> AuthContext -> ATBaseCtx ()
    handleAskCommand inter slackData authCtx = do
      now <- Time.currentTime
      let envCfg = authCtx.env
      let question = inter.text
          fullPrompt = systemPrompt <> "\n\nUser query: " <> question
      result <- liftIO $ callOpenAIAPI fullPrompt envCfg.openaiApiKey
      case result of
        Left err -> sendSlackFollowupResponse inter.response_url (AE.object ["text" AE..= "Error: something went wrong"])
        Right p -> do
          let res = AI.getAskLLMResponse p
          case res of
            Left _ -> sendSlackFollowupResponse inter.response_url (AE.object ["text" AE..= "Error: something went wrong"])
            Right AI.ChatLLMResponse{..} -> do
              let from' = timeRange >>= viaNonEmpty head
              let to' = timeRange >>= viaNonEmpty last
              let (fromT, toT, rangeM) = Utils.parseTime from' to' Nothing now
                  from = fromMaybe "" $ rangeM >>= Just . fst
                  to = fromMaybe "" $ rangeM >>= Just . snd
              case visualization of
                Just vizType -> do
                  let chartType = Widget.mapWidgetTypeToChartType $ Widget.mapChatTypeToWidgetType vizType
                      opts = "&q=" <> toUriStr query <> "&p=" <> slackData.projectId.toText <> "&t=" <> chartType <> "&from=" <> toUriStr from <> "&to=" <> toUriStr to
                      query_url = authCtx.env.hostUrl <> "p/" <> slackData.projectId.toText <> "/log_explorer?viz_type=" <> chartType <> "&query=" <> toUriStr query
                      content' = getBotContent question query query_url opts authCtx.env.chartShotUrl now
                      content = AE.object ["attachments" AE..= content', "response_type" AE..= "in_channel", "replace_original" AE..= True, "delete_original" AE..= True]
                  _ <- sendSlackFollowupResponse inter.response_url content
                  pass
                Nothing -> do
                  let queryAST = parseQueryToAST query
                  case queryAST of
                    Left err -> sendSlackFollowupResponse inter.response_url (AE.object ["text" AE..= "Error: something went wrong"])
                    Right query' -> do
                      tableAsVecE <- RequestDumps.selectLogTable slackData.projectId query' query Nothing (fromT, toT) [] Nothing Nothing
                      let content = handleTableResponse Slack tableAsVecE envCfg slackData.projectId query
                      _ <- sendSlackFollowupResponse inter.response_url content
                      pass
      pass


data SlackActionForm = SlackActionForm {payload :: Text}
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
  deriving (Generic, Show)


instance AE.FromJSON SlackAction where
  parseJSON = AE.genericParseJSON AE.defaultOptions{AE.fieldLabelModifier = \f -> if f == "type_" then "type" else f}


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
  deriving (Generic, Show)


-- deriving anyclass (AE.FromJSON)

instance AE.FromJSON SAction where
  parseJSON = AE.genericParseJSON AE.defaultOptions{AE.fieldLabelModifier = \f -> if f == "type_a" then "type" else f}


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
          dashboardVMM <- Dashboards.getDashboardById dashboardId
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
        _ <- triggerSlackModal authCtx.env.slackBotToken "update" $ AE.object ["view_id" AE..= slackAction.view.id, "view" AE..= (dashboardView pMeta $ V.fromList [dashboardViewOne widgets, dashboardViewTwo widgets])]
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
            widget = find (\w -> (fromMaybe "Untitled-" w.title) == widgetTitle) dashboard.widgets
        whenJust widget $ \w -> do
          now <- Time.currentTime
          let widgetQuery = "&widget=" <> toUriStr (decodeUtf8 $ AE.encode $ AE.toJSON w)
              chartUrl' = chartImageUrl ("&p=" <> pid <> widgetQuery) authCtx.env.chartShotUrl now
              blocks = V.fromList [dashboardViewOne widgets, dashboardViewTwo widgets, dashboardWidgetView chartUrl' widgetTitle]
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


chartImageUrl :: Text -> Text -> Time.UTCTime -> Text
chartImageUrl options baseUrl now =
  let timeMs = show $ floor (utcTimeToPOSIXSeconds now * 1000)
   in baseUrl <> "?time=" <> timeMs <> options


getBotContent :: Text -> Text -> Text -> Text -> Text -> Time.UTCTime -> AE.Value
getBotContent question query query_url chartOptions baseUrl now =
  AE.Array
    ( V.fromList
        [ AE.object
            [ "color" AE..= "#0068ff"
            , "title" AE..= question
            , "markdown_in" AE..= (AE.Array $ V.fromList ["text"])
            , "title_link" AE..= query_url
            , "image_url" AE..= chartImageUrl chartOptions baseUrl now
            , "fields" AE..= AE.Array (V.fromList [AE.object ["title" AE..= "Query used", "value" AE..= query]])
            , "actions" AE..= AE.Array (V.fromList [AE.object ["type" AE..= "button", "text" AE..= "View in log explorer", "url" AE..= query_url]])
            ]
        ]
    )


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


data SlackEventPayload
  = UrlVerification
      { token :: Text
      , challenge :: Text
      }
  | EventCallback
      { token :: Text
      , team_id :: Text
      , api_app_id :: Text
      , event :: SlackEvent
      , event_id :: Text
      , event_time :: Int
      }
  deriving (Show)


-- Custom parser that switches on "type"
instance AE.FromJSON SlackEventPayload where
  parseJSON = AE.withObject "SlackEventPayload" $ \v -> do
    typ <- v AE..: "type"
    case typ of
      "url_verification" ->
        UrlVerification <$> v AE..: "token" <*> v AE..: "challenge"
      "event_callback" ->
        EventCallback
          <$> v
          AE..: "token"
          <*> v
          AE..: "team_id"
          <*> v
          AE..: "api_app_id"
          <*> v
          AE..: "event"
          <*> v
          AE..: "event_id"
          <*> v
          AE..: "event_time"
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
  deriving (Generic, Show)


instance AE.FromJSON SlackEvent where
  parseJSON = AE.genericParseJSON AE.defaultOptions{AE.fieldLabelModifier = \f -> if f == "type_" then "type" else f}


slackEventsPostH :: SlackEventPayload -> ATBaseCtx AE.Value
slackEventsPostH payload = do
  envCfg <- asks env
  now <- Time.currentTime
  case payload of
    UrlVerification _ challenge -> pure $ AE.object ["challenge" AE..= challenge]
    EventCallback{..} -> do
      void $ forkIO $ handleEventCallback envCfg event team_id now
      pure $ AE.object []
  where
    handleEventCallback envCfg event teamId now =
      case event.thread_ts of
        Nothing -> sendSlackChatMessage envCfg.slackBotToken (AE.object ["text" AE..= "Sorry I wasn't able to process your request", "channel" AE..= event.channel])
        Just threadTs -> processThreadedEvent envCfg event teamId threadTs now

    processThreadedEvent envCfg event teamId threadTs now = do
      replies <- getChannelMessages envCfg.slackBotToken event.channel (fromMaybe "" event.thread_ts)
      slackDataM <- dbtToEff $ getSlackDataByTeamId teamId
      handleThreadedReplies envCfg event slackDataM replies threadTs now

    handleThreadedReplies envCfg event slackDataM replies threadTs now =
      case slackDataM of
        Nothing -> pass
        Just slackData ->
          case replies of
            Just messages -> processMessages envCfg event slackData messages threadTs now
            Nothing -> pass

    processMessages envCfg event slackData messages threadTs now = do
      let fullPrompt = threadsPrompt messages.messages event.text
      result <- liftIO $ callOpenAIAPI fullPrompt envCfg.openaiApiKey
      case result of
        Left _ -> sendSlackChatMessage envCfg.slackBotToken (AE.object ["text" AE..= "Sorry I wasn't able to process your request", "channel" AE..= event.channel, "thread_ts" AE..= threadTs])
        Right r -> do
          let res = AI.getAskLLMResponse r
          case res of
            Left _ -> sendSlackChatMessage envCfg.slackBotToken (AE.object ["text" AE..= "Sorry I wasn't able to process your request", "channel" AE..= event.channel, "thread_ts" AE..= threadTs])
            Right rr -> handleQueryResult envCfg event slackData rr threadTs now

    handleQueryResult envCfg event slackData res threadTs now = do
      let from' = res.timeRange >>= viaNonEmpty head
      let to' = res.timeRange >>= viaNonEmpty last
      let (fromT, toT, rangeM) = Utils.parseTime from' to' Nothing now
          from = fromMaybe "" $ rangeM >>= Just . fst
          to = fromMaybe "" $ rangeM >>= Just . snd
      case res.visualization of
        Just vizType -> sendChartResponse envCfg event slackData res.query vizType threadTs now from to
        Nothing -> do
          let queryAST = parseQueryToAST res.query
          case queryAST of
            Left err -> do
              sendSlackChatMessage envCfg.slackBotToken (AE.object ["text" AE..= "Sorry I wasn't able to process your request", "channel" AE..= event.channel, "thread_ts" AE..= threadTs])
            Right query' -> do
              tableAsVecE <- RequestDumps.selectLogTable slackData.projectId query' res.query Nothing (fromT, toT) [] Nothing Nothing
              let content' = handleTableResponse Slack tableAsVecE envCfg slackData.projectId res.query
                  content = case content' of
                    AE.Object o -> AE.Object (o <> KEMP.fromList [("channel", AE.String event.channel), ("thread_ts", AE.String threadTs)])
                    _ -> content'
              _ <- sendSlackChatMessage envCfg.slackBotToken content
              pass

    sendChartResponse envCfg event slackData query vizType threadTs now from to = do
      let chartType = Widget.mapWidgetTypeToChartType $ Widget.mapChatTypeToWidgetType vizType
          opts = "&q=" <> toUriStr query <> "&p=" <> slackData.projectId.toText <> "&t=" <> chartType <> "&from=" <> toUriStr from <> "&to=" <> toUriStr to
          query_url = envCfg.hostUrl <> "p/" <> slackData.projectId.toText <> "/log_explorer?viz_type=" <> chartType <> "&query=" <> toUriStr query
          content' = getBotContent event.text query query_url opts envCfg.chartShotUrl now
          content = AE.object ["attachments" AE..= content', "channel" AE..= event.channel, "thread_ts" AE..= threadTs]
      _ <- sendSlackChatMessage envCfg.slackBotToken content
      pass


data SlackThreadedMessage = SlackThreadedMessage
  { type_ :: Text
  , user :: Text
  , text :: Text
  , thread_ts :: Text
  , ts :: Text
  }
  deriving (Generic, Show)


instance AE.FromJSON SlackThreadedMessage where
  parseJSON = AE.genericParseJSON AE.defaultOptions{AE.fieldLabelModifier = \f -> if f == "type_" then "type" else f}


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


threadsPrompt :: [SlackThreadedMessage] -> Text -> Text
threadsPrompt msgs question = prompt
  where
    msgs' = (\x -> AE.object ["message" AE..= x.text]) <$> msgs
    msgJson = decodeUtf8 $ AE.encode $ AE.Array (V.fromList msgs')
    threadPrompt =
      unlines
        $ [ "\n\nTHREADS:"
          , "- this query is  part of a Slack conversation thread. Use previous messages provited in the thread for additional context if needed."
          , "- the user query is the main one to answer, but earlier messages may contain important clarifications or parameters."
          , "\nPrevious thread messages in json:\n"
          ]
        <> [msgJson]
        <> ["\n\nUser query: " <> question]

    prompt = systemPrompt <> threadPrompt
