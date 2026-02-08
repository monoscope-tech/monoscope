{-# LANGUAGE PackageImports #-}

module Pages.Bots.Discord (linkDiscordGetH, discordInteractionsH, getDiscordChannels, DiscordInteraction) where

import Data.Aeson qualified as AE
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Default (Default (def))
import Data.Text qualified as T
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Effectful.Error.Static (throwError)
import Effectful.Reader.Static (ask, asks)
import Models.Apis.Slack (DiscordData (..), getDashboardsForDiscord, getDiscordData, insertDiscordData, updateDiscordNotificationChannel)
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Relude hiding (ask, asks)
import "cryptonite" Crypto.Error qualified as Crypto
import "cryptonite" Crypto.PubKey.Ed25519 qualified as Ed25519

import Control.Lens ((.~), (^.))
import Data.Effectful.Wreq (
  HTTP,
  defaults,
  getWith,
  header,
  postWith,
  responseBody,
 )
import Data.Time qualified as Time
import Effectful (Eff, type (:>))
import Effectful.Log qualified as Log
import Effectful.Time qualified as Time
import Models.Apis.Issues qualified as Issues
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Dashboards qualified as Dashboards
import Network.Wreq qualified as Wreq
import Network.Wreq.Types (FormParam)
import Pages.Bots.Utils (BotErrorType (..), BotResponse (..), BotType (..), Channel, QueryIntent (..), authHeader, botEmoji, contentTypeHeader, detectReportIntent, formatBotError, formatHistoryAsContext, formatReportForDiscord, formatTextResponse, handleTableResponse, processAIQuery, processReportQuery, widgetPngUrl)
import Pkg.AI qualified as AI
import Pkg.Components.TimePicker qualified as TP
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (idFromText)
import Pkg.Parser (parseQueryToAST)
import Servant.API (Header)
import Servant.API.ResponseHeaders (Headers, addHeader)
import Servant.Server (ServerError (errBody), err400, err404)
import System.Config (AuthContext (env), EnvConfig (..))
import System.Types (ATBaseCtx)
import Utils (toUriStr)
import Utils qualified


linkDiscordGetH :: Maybe Text -> Maybe Text -> Maybe Text -> ATBaseCtx (Headers '[Header "Location" Text] BotResponse)
linkDiscordGetH pidM' codeM guildIdM = do
  envCfg <- asks env
  let res = pidM' >>= Just . T.splitOn "__"
  let pidM = res >>= Projects.projectIdFromText . fromMaybe "" . viaNonEmpty head
  let isOnboarding = maybe False (\r -> length r > 1) res
  let bwconf =
        (def :: BWConfig)
          { sessM = Nothing
          , currProject = Nothing
          , pageTitle = "Discord app installed"
          , config = envCfg
          }
  case (pidM, codeM, guildIdM) of
    (Just pid, Just code, Just guildId) -> do
      r' <- exchangeCodeForTokenDiscord envCfg.discordClientId envCfg.discordClientSecret code envCfg.discordRedirectUri
      case r' of
        Just t -> do
          _ <- insertDiscordData pid guildId
          _ <- registerDiscordCommands envCfg.discordClientId envCfg.discordBotToken guildId
          if isOnboarding
            then pure $ addHeader ("/p/" <> pid.toText <> "/onboarding?step=NotifChannel") $ NoContent $ PageCtx bwconf ()
            else pure $ addHeader "" $ BotLinked $ PageCtx bwconf "Discord"
        Nothing -> pure $ addHeader "" $ DiscordError $ PageCtx def ()
    _ ->
      pure $ addHeader "" $ DiscordError $ PageCtx def ()


exchangeCodeForTokenDiscord :: HTTP :> es => Text -> Text -> Text -> Text -> Eff es (Maybe Text)
exchangeCodeForTokenDiscord clientId clientSecret code redirectUri = do
  let url = "https://discord.com/api/oauth2/token"
  let body :: [FormParam]
      body =
        [ "client_id" Wreq.:= clientId
        , "client_secret" Wreq.:= clientSecret
        , "grant_type" Wreq.:= ("authorization_code" :: Text)
        , "code" Wreq.:= code
        , "redirect_uri" Wreq.:= redirectUri
        ]
      opts = defaults & header "Content-Type" .~ ["application/x-www-form-urlencoded"]

  res <- postWith opts url body

  let status = res ^. Wreq.responseStatus . Wreq.statusCode
  if status >= 200 && status < 300
    then pure (Just "success")
    else pure Nothing


-- Discord interaction type
data InteractionType = Ping | ApplicationCommand | MessageComponent
  deriving (Eq, Show)


instance AE.FromJSON InteractionType where
  parseJSON = AE.withScientific "InteractionType" \n -> case round n of
    1 -> pure Ping
    _ -> pure ApplicationCommand


-- Interaction data
data DiscordInteraction = Interaction
  { interaction_type :: InteractionType
  , id :: Text
  , token :: Text
  , data_i :: Maybe InteractionData
  , channel_id :: Maybe Text
  , guild_id :: Maybe Text
  , channel :: Maybe DiscordThreadChannel
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.Rename "interaction_type" "type", DAE.Rename "data_i" "data"]] DiscordInteraction


data ThreadMetadata = ThreadMetadata
  { archive_timestamp :: Text
  , archived :: Bool
  , auto_archive_duration :: Int
  , create_timestamp :: Text
  , locked :: Bool
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON)


data DiscordThreadChannel = DiscordThreadChannel
  { id :: Text
  , name :: Text
  , guild_id :: Maybe Text
  , type_ :: Int
  , parent_id :: Maybe Text
  , owner_id :: Maybe Text
  , thread_metadata :: Maybe ThreadMetadata
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.StripSuffix "_"]] DiscordThreadChannel


data InteractionOption = InteractionOption
  { name :: Text
  , value :: AE.Value
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- Slash command data
data InteractionData
  = CommandData
      { name :: Text
      , options :: Maybe [InteractionOption]
      }
  | MessageComponentData
      { component_type :: Int
      , custom_id :: Text
      , values :: [Text]
      }
  deriving (Generic, Show)
  deriving anyclass (AE.ToJSON)


instance AE.FromJSON InteractionData where
  parseJSON = AE.withObject "InteractionData" $ \v -> do
    componentType <- v AE..:? "component_type"
    case componentType of
      Just 3 ->
        MessageComponentData
          <$> v
          AE..: "component_type"
          <*> v
          AE..: "custom_id"
          <*> v
          AE..: "values"
      _ ->
        CommandData
          <$> v
          AE..: "name"
          <*> v
          AE..:? "options"


getDiscordChannels :: (HTTP :> es, Log.Log :> es) => Text -> Text -> Eff es [Channel]
getDiscordChannels token guildId = do
  let url = "https://discord.com/api/v10/guilds/" <> toString guildId <> "/channels"
      opts = defaults & header "Authorization" .~ ["Bot " <> encodeUtf8 token]
  r <- getWith opts url
  let body = r ^. responseBody
  case AE.eitherDecode body of
    Right val -> return val
    Left err -> do
      Log.logAttention ("Error decoding discord channels response: " <> toText err) ()
      return []


discordInteractionsH :: BS.ByteString -> Maybe BS.ByteString -> Maybe BS.ByteString -> ATBaseCtx AE.Value
discordInteractionsH rawBody signatureM timestampM = do
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  let envCfg = authCtx.env
  validateSignature envCfg signatureM timestampM rawBody
  interaction <- parseInteraction rawBody
  Log.logTrace ("Discord interaction received" :: Text) $ AE.object ["type" AE..= show interaction.interaction_type, "guild_id" AE..= interaction.guild_id, "channel_id" AE..= interaction.channel_id, "data" AE..= interaction.data_i]

  case interaction.interaction_type of
    Ping -> pure $ AE.object ["type" AE..= 1]
    ApplicationCommand -> handleApplicationCommand interaction envCfg authCtx
    MessageComponent -> handleApplicationCommand interaction envCfg authCtx
  where
    validateSignature envCfg (Just sig) (Just tme) body
      | verifyDiscordSignature (encodeUtf8 envCfg.discordPublicKey) sig tme body = pass
      | otherwise = throwError err404{errBody = "Invalid signature"}
    validateSignature _ _ _ _ = throwError err404{errBody = "Invalid signature"}

    handleApplicationCommand :: DiscordInteraction -> EnvConfig -> AuthContext -> ATBaseCtx AE.Value
    handleApplicationCommand interaction envCfg authCtx = do
      cmdData <- case data_i interaction of
        Nothing -> throwError err400{errBody = "No command data provided"}
        Just cmd -> pure cmd
      discordData <- getDiscordData (fromMaybe "" (maybe interaction.guild_id (\c -> c.guild_id) interaction.channel))
      case discordData of
        Nothing -> pure $ AE.object ["type" AE..= (4 :: Int), "data" AE..= formatBotError Discord ServiceError]
        Just d -> handleCommand cmdData interaction envCfg authCtx d

    parseInteraction :: BS.ByteString -> ATBaseCtx DiscordInteraction
    parseInteraction rawBody' = do
      case AE.eitherDecode (fromStrict rawBody') of
        Left e -> throwError err404{errBody = "Invalid interaction data"}
        Right interaction -> pure interaction

    handleCommand :: InteractionData -> DiscordInteraction -> EnvConfig -> AuthContext -> DiscordData -> ATBaseCtx AE.Value
    handleCommand cmdData interaction envCfg authCtx discordData =
      case cmdData of
        CommandData name options -> do
          case name of
            "monoscope" -> do
              handleAskCommand options interaction envCfg authCtx discordData
              pure $ AE.object []
            "here" -> do
              case (interaction.channel_id, interaction.guild_id) of
                (Just channelId, Just guildId) -> do
                  _ <- updateDiscordNotificationChannel guildId channelId
                  pure $ hereSuccessResponse
                _ -> pure $ contentResponse "No channel ID provided"
            "dashboard" -> do
              handleDashboard options interaction envCfg authCtx discordData
              pure $ AE.object []
            _ -> pure $ contentResponse "The command is not recognized"
        MessageComponentData{..} -> do
          case custom_id of
            "dashboard-select" -> do
              _ <- sendDeferredResponse interaction.id interaction.token envCfg.discordBotToken
              let dashboardId = fromMaybe "" $ viaNonEmpty head values
              dashboardVMM <- maybe (pure Nothing) Dashboards.getDashboardById (idFromText dashboardId)
              case dashboardVMM of
                Nothing -> pass
                Just dashboardVM -> do
                  dashboardM <- liftIO $ Dashboards.readDashboardFile "static/public/dashboards" (toString $ fromMaybe "_overview.yaml" dashboardVM.baseTemplate)
                  whenJust dashboardM $ \dashboard -> do
                    let widgets' = (\w -> (fromMaybe "Untitled-" w.title, fromMaybe "Untitled-" w.title)) <$> dashboard.widgets
                        widgets = V.fromList $ (\(k, id') -> (k, id' <> "___" <> dashboardId)) <$> widgets'
                        contents = discordSelectContent widgets "widget-select" "Select a widget"
                    sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken contents
            "widget-select" -> do
              _ <- sendDeferredResponse interaction.id interaction.token envCfg.discordBotToken
              let val = T.splitOn "___" $ fromMaybe "" $ viaNonEmpty head values
              case val of
                [widget, dashboardId] -> do
                  dashboardVMM <- maybe (pure Nothing) Dashboards.getDashboardById (idFromText dashboardId)
                  case dashboardVMM of
                    Nothing -> pass
                    Just dashboardVM -> do
                      dashboardM <- liftIO $ Dashboards.readDashboardFile "static/public/dashboards" (toString $ fromMaybe "_overview.yaml" dashboardVM.baseTemplate)
                      whenJust dashboardM $ \dashboard -> do
                        let widgetM = find (\w -> fromMaybe "Untitled-" w.title == widget) dashboard.widgets
                        whenJust widgetM $ \w -> do
                          chartUrl' <- widgetPngUrl envCfg.apiKeyEncryptionSecretKey envCfg.hostUrl discordData.projectId w Nothing Nothing Nothing
                          let url = envCfg.hostUrl <> "p/" <> discordData.projectId.toText <> "/dashboards/" <> dashboardId
                              content = sharedWidgetContent widget chartUrl' url
                          sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken content
                _ -> pass
              pass
            _ -> pass
          pure $ AE.object []

    handleDashboard :: Maybe [InteractionOption] -> DiscordInteraction -> EnvConfig -> AuthContext -> DiscordData -> ATBaseCtx ()
    handleDashboard cmData interaction envCfg authCtx discordData = do
      _ <- sendDeferredResponse interaction.id interaction.token envCfg.discordBotToken
      dashboards <- getDashboardsForDiscord (fromMaybe "" interaction.guild_id)
      let content = discordSelectContent (V.fromList dashboards) "dashboard-select" "Select a dashboard"
      sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken content

    handleAskCommand :: Maybe [InteractionOption] -> DiscordInteraction -> EnvConfig -> AuthContext -> DiscordData -> ATBaseCtx ()
    handleAskCommand options interaction envCfg authCtx discordData = do
      now <- Time.currentTime
      _ <- sendDeferredResponse interaction.id interaction.token envCfg.discordBotToken

      -- Extract user query from options
      let userQuery = case options of
            Just (InteractionOption{value = AE.String q} : _) -> q
            _ -> ""

      -- Check for report intent first (fast path)
      case detectReportIntent userQuery of
        ReportIntent reportType -> do
          reportResult <- processReportQuery discordData.projectId reportType envCfg
          case reportResult of
            Left err -> sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken (AE.object ["content" AE..= err])
            Right (report, eventsUrl, errorsUrl) -> sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken $ formatReportForDiscord report discordData.projectId envCfg eventsUrl errorsUrl
        GeneralQueryIntent -> case interaction.channel of
          Just DiscordThreadChannel{type_ = 11, id = threadId} -> do
            let convId = Issues.discordThreadToConversationId threadId
            _ <- Issues.getOrCreateConversation discordData.projectId convId Issues.CTDiscordThread (AE.object ["channel_id" AE..= interaction.channel_id, "guild_id" AE..= interaction.guild_id])
            existingHistory <- Issues.selectChatHistory convId
            when (null existingHistory) $ do
              msgs <- getThreadStarterMessage interaction envCfg.discordBotToken
              case msgs of
                Just messages -> forM_ messages $ \m ->
                  let role = if m.author.username == "APItoolkit" then "assistant" else "user"
                   in Issues.insertChatMessage discordData.projectId convId role m.content Nothing Nothing
                Nothing -> pass
            dbMessages <- Issues.selectChatHistory convId
            let threadContext = formatHistoryAsContext "Discord" $ map AI.dbMessageToLLMMessage dbMessages
            result <- processAIQuery discordData.projectId userQuery (Just threadContext) envCfg.openaiApiKey
            Issues.insertChatMessage discordData.projectId convId "user" userQuery Nothing Nothing
            case result of
              Left _ -> sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken (formatBotError Discord ServiceError)
              Right resp -> do
                whenJust resp.query \q -> Issues.insertChatMessage discordData.projectId convId "assistant" q Nothing Nothing
                sendDiscordResponse options interaction envCfg authCtx discordData resp now
          _ -> do
            result <- processAIQuery discordData.projectId userQuery Nothing envCfg.openaiApiKey
            case result of
              Left _ -> sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken (formatBotError Discord ServiceError)
              Right resp -> sendDiscordResponse options interaction envCfg authCtx discordData resp now


sendDiscordResponse :: Maybe [InteractionOption] -> DiscordInteraction -> EnvConfig -> AuthContext -> DiscordData -> AI.LLMResponse -> Time.UTCTime -> ATBaseCtx ()
sendDiscordResponse options interaction envCfg authCtx discordData resp now =
  let (fromTimeM, toTimeM, rangeM) = maybe (Nothing, Nothing, Nothing) (TP.parseTimeRange now) resp.timeRange
      (from, to) = fromMaybe ("", "") rangeM
      query = fromMaybe "" resp.query
      hasQuery = isJust resp.query
      hasExplanation = isJust resp.explanation
   in case (hasQuery, hasExplanation) of
        (False, True) -> sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken $ formatTextResponse Discord (fromMaybe "No insights available" resp.explanation)
        (True, False) -> handleWidgetResponse query fromTimeM toTimeM from to
        (True, True) -> do
          handleWidgetResponse query fromTimeM toTimeM from to
          whenJust resp.explanation \c -> sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken $ formatTextResponse Discord c
        (False, False) -> sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken $ formatTextResponse Discord "No response available"
  where
    handleWidgetResponse query fromTimeM toTimeM from to = case resp.visualization of
      Just vizType -> do
        let widgetType = Widget.mapChatTypeToWidgetType vizType
            chartType = Widget.mapWidgetTypeToChartType widgetType
            query_url = authCtx.env.hostUrl <> "p/" <> discordData.projectId.toText <> "/log_explorer?viz_type=" <> chartType <> ("&query=" <> toUriStr query)
            question = case options of
              Just (InteractionOption{value = AE.String q} : _) -> q
              _ -> "[?]"
        imageUrl <- widgetPngUrl authCtx.env.apiKeyEncryptionSecretKey authCtx.env.hostUrl discordData.projectId def{Widget.wType = widgetType, Widget.query = Just query} Nothing (Just from) (Just to)
        let content = getBotContentWithUrl question query query_url imageUrl
        sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken content
      Nothing -> case parseQueryToAST query of
        Left _ -> sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken (formatBotError Discord (QueryParseError query))
        Right query' -> do
          tableAsVecE <- RequestDumps.selectLogTable discordData.projectId query' query Nothing (fromTimeM, toTimeM) [] Nothing Nothing
          sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken $ handleTableResponse Discord tableAsVecE envCfg discordData.projectId query


contentResponse :: Text -> AE.Value
contentResponse msg = AE.object ["type" AE..= 4, "data" AE..= AE.object ["content" AE..= msg]]


-- | Structured success response for /here command
hereSuccessResponse :: AE.Value
hereSuccessResponse =
  AE.object
    [ "type" AE..= (4 :: Int)
    , "data"
        AE..= AE.object
          [ "flags" AE..= (32768 :: Int)
          , "components"
              AE..= AE.Array
                ( V.singleton
                    $ AE.object
                      [ "type" AE..= (17 :: Int)
                      , "accent_color" AE..= (5763719 :: Int) -- Green accent
                      , "components"
                          AE..= AE.Array
                            ( V.fromList
                                [ AE.object ["type" AE..= (10 :: Int), "content" AE..= (botEmoji "success" <> " **Notification channel set**")]
                                , AE.object ["type" AE..= (10 :: Int), "content" AE..= "This channel will now receive:"]
                                , AE.object ["type" AE..= (10 :: Int), "content" AE..= ("• " <> botEmoji "error" <> " Error alerts\n• " <> botEmoji "chart" <> " Daily & weekly reports\n• " <> botEmoji "warning" <> " Anomaly detections")]
                                ]
                            )
                      ]
                )
          ]
    ]


sendDeferredResponse :: HTTP :> es => Text -> Text -> Text -> Eff es ()
sendDeferredResponse interactionId interactionToken botToken = do
  let url = toString $ "https://discord.com/api/v10/interactions/" <> interactionId <> "/" <> interactionToken <> "/callback"
      payload = AE.encode $ AE.object ["type" AE..= 5]
  _ <- postWith (defaults & authHeader botToken & contentTypeHeader "application/json") url payload
  pass


sendJsonFollowupResponse :: (HTTP :> es, Log.Log :> es) => Text -> Text -> Text -> AE.Value -> Eff es ()
sendJsonFollowupResponse appId interactionToken botToken content = do
  Log.logTrace ("Discord followup response" :: Text) content
  let followupUrl = toString $ "https://discord.com/api/v10/webhooks/" <> appId <> "/" <> interactionToken
  _ <- postWith (defaults & authHeader botToken & contentTypeHeader "application/json") followupUrl content
  pass


verifyDiscordSignature :: ByteString -> ByteString -> ByteString -> ByteString -> Bool
verifyDiscordSignature publicKey signatureHex timestamp rawBody = fromRight False $ do
  s <- first show $ Base16.decode signatureHex
  sig <- first show $ Crypto.eitherCryptoError $ Ed25519.signature s
  pkBytes <- first show $ Base16.decode publicKey
  pk <- first show $ Crypto.eitherCryptoError $ Ed25519.publicKey pkBytes
  pure $ Ed25519.verify pk (timestamp <> rawBody) sig


-- Data types for Discord API responses
newtype DiscordUser = DiscordUser
  { username :: Text
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON)


data DiscordMessage = DiscordMessage
  { content :: Text
  , author :: DiscordUser
  , embeds :: AE.Value
  , timestamp :: Text
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON)


getThreadStarterMessage :: (HTTP :> es, Log.Log :> es) => DiscordInteraction -> Text -> Eff es (Maybe [DiscordMessage])
getThreadStarterMessage interaction botToken = do
  case interaction.channel_id of
    Just channelId -> case interaction.channel of
      Just DiscordThreadChannel{type_ = 11, parent_id = Just pId} -> do
        let baseUrl = "https://discord.com/api/v10/channels/"
            url = toString $ baseUrl <> channelId <> "/messages?limit=50"
            starterMessageUrl = toString $ baseUrl <> pId <> "/messages/" <> channelId
            opts = defaults & authHeader botToken & contentTypeHeader "application/json"
        response <- getWith opts url
        response' <- getWith opts starterMessageUrl
        case AE.eitherDecode (response ^. responseBody) of
          Left err -> do
            Log.logAttention ("Error decoding Discord thread messages: " <> toText err) ()
            return Nothing
          Right messages -> do
            case AE.eitherDecode (response' ^. responseBody) of
              Left err -> do
                return $ Just messages
              Right (message :: DiscordMessage) -> return $ Just (messages <> [message])
      _ -> pure Nothing
    Nothing -> pure Nothing


getBotContentWithUrl :: Text -> Text -> Text -> Text -> AE.Value
getBotContentWithUrl question query query_url imageUrl =
  AE.object
    [ "flags" AE..= (32768 :: Int)
    , "components"
        AE..= AE.Array
          ( V.singleton
              $ AE.object
                [ "type" AE..= (17 :: Int)
                , "accent_color" AE..= (26879 :: Int)
                , "components"
                    AE..= AE.Array
                      ( V.fromList
                          [ AE.object ["type" AE..= (10 :: Int), "content" AE..= (botEmoji "chart" <> " **" <> question <> "**")]
                          , AE.object ["type" AE..= (12 :: Int), "items" AE..= AE.Array (V.singleton $ AE.object ["media" AE..= AE.object ["url" AE..= imageUrl], "description" AE..= ("Chart visualization: " <> question)])]
                          , AE.object ["type" AE..= (10 :: Int), "content" AE..= ("**Query:** `" <> query <> "`")]
                          , AE.object ["type" AE..= (1 :: Int), "components" AE..= AE.Array (V.fromList [AE.object ["type" AE..= (2 :: Int), "label" AE..= (botEmoji "search" <> " View in Log Explorer"), "url" AE..= query_url, "style" AE..= (5 :: Int)]])]
                          ]
                      )
                ]
          )
    ]


sharedWidgetContent :: Text -> Text -> Text -> AE.Value
sharedWidgetContent widgetTitle chartUrl dashboardUrl =
  AE.object
    [ "flags" AE..= (32768 :: Int)
    , "components"
        AE..= AE.Array
          ( V.singleton
              $ AE.object
                [ "type" AE..= (17 :: Int)
                , "accent_color" AE..= (26879 :: Int)
                , "components"
                    AE..= AE.Array
                      ( V.fromList
                          [ AE.object ["type" AE..= (10 :: Int), "content" AE..= (botEmoji "chart" <> " **" <> widgetTitle <> "**")]
                          , AE.object ["type" AE..= (12 :: Int), "items" AE..= AE.Array (V.singleton $ AE.object ["media" AE..= AE.object ["url" AE..= chartUrl], "description" AE..= ("Dashboard widget: " <> widgetTitle)])]
                          , AE.object ["type" AE..= (1 :: Int), "components" AE..= AE.Array (V.fromList [AE.object ["type" AE..= (2 :: Int), "label" AE..= "Open dashboard", "url" AE..= dashboardUrl, "style" AE..= (5 :: Int)]])]
                          ]
                      )
                ]
          )
    ]


discordSelectContent :: V.Vector (Text, Text) -> Text -> Text -> AE.Value
discordSelectContent dashboards sId placeholder =
  AE.object
    [ "flags" AE..= 64
    , "components"
        AE..= AE.Array
          ( V.fromList
              [ AE.object
                  [ "type" AE..= 1
                  , "components" AE..= AE.Array (V.fromList [AE.object ["type" AE..= 3, "custom_id" AE..= sId, "placeholder" AE..= placeholder, "options" AE..= opts]])
                  ]
              ]
          )
    ]
  where
    opts = AE.Array $ V.map (\(t, i) -> AE.object ["label" AE..= t, "value" AE..= i]) dashboards


registerDiscordCommands :: HTTP :> es => Text -> Text -> Text -> Eff es (Either Text ())
registerDiscordCommands appId botToken guildId = do
  let url = toString $ "https://discord.com/api/v10/applications/" <> appId <> "/guilds/" <> guildId <> "/commands"
      monoscopeCommand =
        AE.object
          [ "name" AE..= ("monoscope" :: Text)
          , "description" AE..= ("Ask about your project or get reports" :: Text)
          , "type" AE..= (1 :: Int)
          , "options" AE..= AE.Array (V.fromList [AE.object ["name" AE..= ("question" :: Text), "description" AE..= ("Your question in natural language" :: Text), "type" AE..= (3 :: Int), "required" AE..= True]])
          ]
      hereCommand = AE.object ["name" AE..= ("here" :: Text), "description" AE..= ("Channel for monoscope to send notifications" :: Text), "type" AE..= (1 :: Int)]
      dashboard = AE.object ["name" AE..= ("dashboard" :: Text), "description" AE..= ("Select and share dashboard widget" :: Text), "type" AE..= (1 :: Int)]
      opts = defaults & authHeader botToken & contentTypeHeader "application/json"
  _ <- postWith opts url (AE.encode monoscopeCommand)
  _ <- postWith opts url (AE.encode hereCommand)
  _ <- postWith opts url (AE.encode dashboard)
  pure $ Right ()
