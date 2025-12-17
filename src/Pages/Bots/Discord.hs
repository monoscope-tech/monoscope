{-# LANGUAGE PackageImports #-}

module Pages.Bots.Discord (linkDiscordGetH, discordInteractionsH, getDiscordChannels, DiscordInteraction) where

import Data.Aeson qualified as AE
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Default (Default (def))
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful.Error.Static (throwError)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
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
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Dashboards qualified as Dashboards
import Network.HTTP.Types (urlEncode)
import Network.Wreq qualified as Wreq
import Network.Wreq.Types (FormParam)
import Pages.Bots.Utils (BotResponse (..), BotType (..), Channel, authHeader, chartImageUrl, contentTypeHeader, handleTableResponse)
import Pkg.AI (callOpenAIAPI, systemPrompt)
import Pkg.AI qualified as AI
import Pkg.Components.Widget qualified as Widget
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
          _ <- dbtToEff $ insertDiscordData pid guildId
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
  deriving (Generic, Show)


data ThreadMetadata = ThreadMetadata
  { archive_timestamp :: Text
  , archived :: Bool
  , auto_archive_duration :: Int
  , create_timestamp :: Text
  , locked :: Bool
  }
  deriving (Generic, Show)


instance AE.FromJSON ThreadMetadata


data DiscordThreadChannel = DiscordThreadChannel
  { id :: Text
  , name :: Text
  , guild_id :: Maybe Text
  , type_ :: Int
  , parent_id :: Maybe Text
  , owner_id :: Maybe Text
  , thread_metadata :: Maybe ThreadMetadata
  }
  deriving (Generic, Show)


instance AE.FromJSON DiscordThreadChannel where
  parseJSON = AE.genericParseJSON AE.defaultOptions{AE.fieldLabelModifier = \f -> if f == "type_" then "type" else f}


instance AE.FromJSON DiscordInteraction where
  parseJSON = AE.genericParseJSON AE.defaultOptions{AE.fieldLabelModifier = \f -> if f == "data_i" then "data" else if f == "interaction_type" then "type" else f}


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


data InteractionOption = InteractionOption
  { name :: Text
  , value :: AE.Value
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON)


getDiscordChannels :: (HTTP :> es, Log.Log :> es) => Text -> Text -> Eff es [Channel]
getDiscordChannels token guildId = do
  let url = "https://discord.com/api/v10/guilds/" <> toString guildId <> "/channels"
      opts = defaults & header "Authorization" .~ ["Bot " <> encodeUtf8 token]
  r <- getWith opts url
  let body = r ^. responseBody
  case AE.eitherDecode body of
    Right val -> return val
    Left err -> do
      Log.logAttention ("Error decoding Slack channels response: " <> toText err) ()
      return []


-- pure $ fromRight [] $ AE.eitherDecode body

discordInteractionsH :: BS.ByteString -> Maybe BS.ByteString -> Maybe BS.ByteString -> ATBaseCtx AE.Value
discordInteractionsH rawBody signatureM timestampM = do
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  let envCfg = authCtx.env
  validateSignature envCfg signatureM timestampM rawBody
  interaction <- parseInteraction rawBody

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
        Nothing -> pure $ contentResponse "Sorry, there was an error processing your request"
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
            "ask" -> do
              handleAskCommand options interaction envCfg authCtx discordData
              pure $ AE.object []
            "here" -> do
              case (interaction.channel_id, interaction.guild_id) of
                (Just channelId, Just guildId) -> do
                  _ <- updateDiscordNotificationChannel guildId channelId
                  pure $ contentResponse "Got it, notifications and alerts on your project will now be sent to this channel"
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
              dashboardVMM <- Dashboards.getDashboardById dashboardId
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
                  dashboardVMM <- Dashboards.getDashboardById dashboardId
                  case dashboardVMM of
                    Nothing -> pass
                    Just dashboardVM -> do
                      dashboardM <- liftIO $ Dashboards.readDashboardFile "static/public/dashboards" (toString $ fromMaybe "_overview.yaml" dashboardVM.baseTemplate)
                      whenJust dashboardM $ \dashboard -> do
                        let widgetM = find (\w -> fromMaybe "Untitled-" w.title == widget) dashboard.widgets
                        whenJust widgetM $ \w -> do
                          now <- Time.currentTime
                          let widgetQuery = "&widget=" <> decodeUtf8 (urlEncode True (toStrict $ AE.encode $ AE.toJSON w))
                              chartUrl' = chartImageUrl ("&p=" <> discordData.projectId.toText <> widgetQuery) envCfg.chartShotUrl now
                              url = envCfg.hostUrl <> "p/" <> discordData.projectId.toText <> "/dashboards/" <> dashboardId
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
      let content = discordSelectContent dashboards "dashboard-select" "Select a dashboard"
      sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken content

    handleAskCommand :: Maybe [InteractionOption] -> DiscordInteraction -> EnvConfig -> AuthContext -> DiscordData -> ATBaseCtx ()
    handleAskCommand options interaction envCfg authCtx discordData = do
      now <- Time.currentTime
      _ <- sendDeferredResponse interaction.id interaction.token envCfg.discordBotToken
      fullPrompt <- buildPrompt options interaction envCfg
      result <- liftIO $ callOpenAIAPI fullPrompt envCfg.openaiApiKey
      case result of
        Left err -> do
          sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken (AE.object ["content" AE..= "Sorry, there was an error processing your request"])
        Right r -> do
          let llmJsn = AI.getAskLLMResponse r
          case llmJsn of
            Left errMsg -> do
              sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken (AE.object ["content" AE..= ("Error parsing LLM response: " <> errMsg)])
            Right AI.ChatLLMResponse{..} -> do
              let from' = timeRange >>= viaNonEmpty head
              let to' = timeRange >>= viaNonEmpty last
              let (fromT, toT, rangeM) = Utils.parseTime from' to' Nothing now
                  from = fromMaybe "" $ rangeM >>= Just . fst
                  to = fromMaybe "" $ rangeM >>= Just . snd
              case visualization of
                Just vizType -> do
                  let chartType = Widget.mapWidgetTypeToChartType $ Widget.mapChatTypeToWidgetType vizType

                      query_url = authCtx.env.hostUrl <> "p/" <> discordData.projectId.toText <> "/log_explorer?viz_type=" <> chartType <> ("&query=" <> toUriStr query)
                      opts = "&q=" <> toUriStr query <> "&p=" <> discordData.projectId.toText <> "&t=" <> chartType <> "&from=" <> toUriStr from <> "&to=" <> toUriStr to
                      question = case options of
                        Just (InteractionOption{value = AE.String q} : _) -> q
                        _ -> "[?]"
                      content = getBotContent question query query_url opts authCtx.env.chartShotUrl now
                  sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken content
                Nothing -> do
                  let queryAST = parseQueryToAST query
                  case queryAST of
                    Left err -> do
                      _ <- sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken (AE.object ["content" AE..= ("Error parsing query: " <> err)])
                      pass
                    Right query' -> do
                      tableAsVecE <- RequestDumps.selectLogTable discordData.projectId query' query Nothing (fromT, toT) [] Nothing Nothing
                      let content = handleTableResponse Discord tableAsVecE envCfg discordData.projectId query
                      sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken content


buildPrompt :: Maybe [InteractionOption] -> DiscordInteraction -> EnvConfig -> ATBaseCtx Text
buildPrompt cmdOptions interaction envCfg = do
  threadMsgs <- getThreadStarterMessage interaction envCfg.discordBotToken
  pure $ case cmdOptions of
    Just (InteractionOption{value = AE.String q} : _) -> case threadMsgs of
      Just msgs -> threadsPrompt (reverse msgs) q
      _ -> systemPrompt <> "\n\nUser query: " <> q
    _ -> ""


contentResponse :: Text -> AE.Value
contentResponse msg = AE.object ["type" AE..= 4, "data" AE..= AE.object ["content" AE..= msg]]


sendDeferredResponse :: HTTP :> es => Text -> Text -> Text -> Eff es ()
sendDeferredResponse interactionId interactionToken botToken = do
  let url = toString $ "https://discord.com/api/v10/interactions/" <> interactionId <> "/" <> interactionToken <> "/callback"
      payload = AE.encode $ AE.object ["type" AE..= 5]
  _ <- postWith (defaults & authHeader botToken & contentTypeHeader "application/json") url payload
  pass


sendJsonFollowupResponse :: HTTP :> es => Text -> Text -> Text -> AE.Value -> Eff es ()
sendJsonFollowupResponse appId interactionToken botToken content = do
  let followupUrl = toString $ "https://discord.com/api/v10/webhooks/" <> appId <> "/" <> interactionToken
  _ <- postWith (defaults & authHeader botToken & contentTypeHeader "application/json") followupUrl content
  pass


threadsPrompt :: [DiscordMessage] -> Text -> Text
threadsPrompt msgs question = prompt
  where
    msgs' = (\x -> AE.object ["message" AE..= AE.object ["content" AE..= x.content, "embeds" AE..= x.embeds]]) <$> filter (\x -> x.author.username == "APItoolkit") msgs
    msgJson = decodeUtf8 $ AE.encode $ AE.Array (V.fromList msgs')
    threadPrompt =
      unlines
        $ [ "\n\nTHREADS:"
          , "- this query is  part of a DISCORD conversation thread. Use previous messages provited in the thread for additional context if needed."
          , "- the user query is the main one to answer, but earlier messages may contain important clarifications or parameters."
          , "\nPrevious thread messages in json:\n"
          ]
        <> [msgJson]
        <> ["\n\nUser query: " <> question]

    prompt = systemPrompt <> threadPrompt


verifyDiscordSignature
  :: ByteString
  -> ByteString
  -> ByteString
  -> ByteString
  -> Bool
verifyDiscordSignature publicKey signatureHex timestamp rawBody =
  case Base16.decode signatureHex of
    Right s ->
      case Ed25519.signature s of
        Crypto.CryptoFailed _ -> False
        Crypto.CryptoPassed sig -> case Base16.decode publicKey of
          Right pkBytes ->
            case Ed25519.publicKey pkBytes of
              Crypto.CryptoFailed e -> False
              Crypto.CryptoPassed pk ->
                let message = timestamp <> rawBody
                 in Ed25519.verify pk message sig
          _ -> False
    _ -> False


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


getBotContent :: Text -> Text -> Text -> Text -> Text -> Time.UTCTime -> AE.Value
getBotContent question query query_url chartOptions baseUrl now =
  AE.object
    [ "flags" AE..= 32768
    , "components"
        AE..= AE.Array
          ( V.singleton
              $ AE.object
                [ "type" AE..= 17
                , "accent_color" AE..= 26879
                , "components"
                    AE..= AE.Array
                      ( V.fromList
                          [ AE.object ["type" AE..= 10, "content" AE..= ("### " <> question)]
                          , AE.object ["type" AE..= 12, "items" AE..= AE.Array (V.singleton $ AE.object ["media" AE..= AE.object ["url" AE..= chartImageUrl chartOptions baseUrl now]])]
                          , AE.object ["type" AE..= 10, "content" AE..= ("**Query used:** " <> query)]
                          , AE.object ["type" AE..= 1, "components" AE..= AE.Array (V.fromList [AE.object ["type" AE..= 2, "label" AE..= "Open explorer", "url" AE..= query_url, "style" AE..= 5]])]
                          ]
                      )
                ]
          )
    ]


sharedWidgetContent :: Text -> Text -> Text -> AE.Value
sharedWidgetContent widgetTitle chartUrl dashboardUrl =
  AE.object
    [ "flags" AE..= 32768
    , "components"
        AE..= AE.Array
          ( V.singleton
              $ AE.object
                [ "type" AE..= 17
                , "accent_color" AE..= 26879
                , "components"
                    AE..= AE.Array
                      ( V.fromList
                          [ AE.object ["type" AE..= 10, "content" AE..= ("### " <> widgetTitle)]
                          , AE.object ["type" AE..= 12, "items" AE..= AE.Array (V.singleton $ AE.object ["media" AE..= AE.object ["url" AE..= chartUrl]])]
                          , AE.object ["type" AE..= 1, "components" AE..= AE.Array (V.fromList [AE.object ["type" AE..= 2, "label" AE..= "Open dashboard", "url" AE..= dashboardUrl, "style" AE..= 5]])]
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
      askCommand =
        AE.object
          [ "name" AE..= ("ask" :: Text)
          , "description" AE..= ("Ask a question about your project using natural language" :: Text)
          , "type" AE..= 1
          , "options" AE..= AE.Array (V.fromList [AE.object ["name" AE..= "question", "description" AE..= "Your question in natural language", "type" AE..= 3, "required" AE..= True]])
          ]

      hereCommand = AE.object ["name" AE..= "here", "description" AE..= "Channel for monoscope to send notifications", "type" AE..= 1]
      dashboard = AE.object ["name" AE..= "dashboard", "description" AE..= "Select and share dashboard widget", "type" AE..= 1]
      opts = defaults & authHeader botToken & contentTypeHeader "application/json"

  _ <- postWith opts url (AE.encode askCommand)
  _ <- postWith opts url (AE.encode hereCommand)
  _ <- postWith opts url (AE.encode dashboard)
  pure $ Right ()
