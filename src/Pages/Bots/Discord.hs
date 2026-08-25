module Pages.Bots.Discord (linkDiscordGetH, discordInteractionsH, getDiscordChannels, DiscordInteraction) where

import Data.Aeson qualified as AE
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Default (Default (def))
import Data.Text qualified as T
import Deriving.Aeson qualified as DAE
import Effectful.Error.Static (throwError)
import Effectful.Ki qualified as Ki
import Effectful.Reader.Static (asks)
import Models.Apis.Integrations (DiscordData (..), getDashboardsForDiscord, getDiscordData, insertDiscordData)
import Models.Projects.ProjectMembers qualified as ProjectMembers
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
import Effectful (Eff, type (:>))
import Effectful.Log qualified as Log
import Models.Apis.Issues qualified as Issues
import Models.Projects.Dashboards qualified as Dashboards
import Network.Wreq qualified as Wreq
import Network.Wreq.Types (FormParam)
import Pages.Bots.Utils (BotErrorType (..), BotResponse (..), BotType (..), Channel, QueryIntent (..), authHeader, botEmoji, contentTypeHeader, detectReportIntent, dispatchAIResponse, formatBotError, formatHistoryAsContext, formatReportForDiscord, processAIQuery, processReportQuery)
import Pkg.AI qualified as AI
import Pkg.Components.Widget (Widget (..), widgetPngUrl)
import Pkg.DeriveUtils (idFromText)
import Servant.API (Header)
import Servant.API.ResponseHeaders (Headers, addHeader)
import Servant.Server (ServerError (errBody), err400, err401, err404)
import System.Config (AuthContext (env), EnvConfig (..))
import System.Types (ATBaseCtx)


linkDiscordGetH :: Maybe Text -> Maybe Text -> Maybe Text -> ATBaseCtx (Headers '[Header "Location" Text] BotResponse)
linkDiscordGetH pidM' codeM guildIdM = do
  envCfg <- asks env
  let parts = T.splitOn "__" <$> pidM'
      pidM = parts >>= viaNonEmpty head >>= Projects.projectIdFromText
      bwconf = (def :: BWConfig){sessM = Nothing, currProject = Nothing, pageTitle = "Discord app installed", config = envCfg}
      discordErr = addHeader "" $ DiscordError $ PageCtx def ()
  case (pidM, codeM, guildIdM) of
    (Just pid, Just code, Just guildId) -> do
      ok <- exchangeCodeForTokenDiscord envCfg.discordClientId envCfg.discordClientSecret code envCfg.discordRedirectUri
      if not ok
        then pure discordErr
        else do
          _ <- insertDiscordData pid guildId
          registerDiscordCommands envCfg.discordClientId envCfg.discordBotToken guildId
          pure
            $ if maybe False ((> 1) . length) parts
              then addHeader ("/p/" <> pid.toText <> "/onboarding?step=NotifChannel") $ NoContent $ PageCtx bwconf ()
              else addHeader "" $ BotLinked $ PageCtx bwconf ("Discord", Just pid)
    _ -> pure discordErr


exchangeCodeForTokenDiscord :: HTTP :> es => Text -> Text -> Text -> Text -> Eff es Bool
exchangeCodeForTokenDiscord clientId clientSecret code redirectUri = do
  let body :: [FormParam]
      body =
        [ "client_id" Wreq.:= clientId
        , "client_secret" Wreq.:= clientSecret
        , "grant_type" Wreq.:= ("authorization_code" :: Text)
        , "code" Wreq.:= code
        , "redirect_uri" Wreq.:= redirectUri
        ]
  res <- postWith (defaults & header "Content-Type" .~ ["application/x-www-form-urlencoded"]) "https://discord.com/api/oauth2/token" body
  let status = res ^. Wreq.responseStatus . Wreq.statusCode
  pure $ status >= 200 && status < 300


-- | Discord interaction type. Discord sends 1 for PING; every other numeric type
-- (application command, message component, …) is dispatched down the same path.
data InteractionType = Ping | ApplicationCommand
  deriving (Eq, Show)


instance AE.FromJSON InteractionType where
  parseJSON = AE.withScientific "InteractionType" \n -> pure $ bool ApplicationCommand Ping (round n == (1 :: Int))


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


data DiscordThreadChannel = DiscordThreadChannel
  { id :: Text
  , type_ :: Int
  , guild_id :: Maybe Text
  , parent_id :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.StripSuffix "_"]] DiscordThreadChannel


data InteractionOption = InteractionOption
  { name :: Text
  , value :: AE.Value
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


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
  parseJSON = AE.withObject "InteractionData" \v ->
    v AE..:? "component_type" >>= \case
      Just (3 :: Int) -> MessageComponentData <$> v AE..: "component_type" <*> v AE..: "custom_id" <*> v AE..: "values"
      _ -> CommandData <$> v AE..: "name" <*> v AE..:? "options"


getDiscordChannels :: (HTTP :> es, Log.Log :> es) => Text -> Text -> Eff es [Channel]
getDiscordChannels token guildId = do
  r <- getWith (defaults & header "Authorization" .~ ["Bot " <> encodeUtf8 token]) (toString $ "https://discord.com/api/v10/guilds/" <> guildId <> "/channels")
  either (\err -> [] <$ Log.logAttention ("Error decoding discord channels response: " <> toText err) ()) pure $ AE.eitherDecode (r ^. responseBody)


discordInteractionsH :: BS.ByteString -> Maybe BS.ByteString -> Maybe BS.ByteString -> ATBaseCtx AE.Value
discordInteractionsH rawBody signatureM timestampM = do
  envCfg <- asks @AuthContext env
  validateSignature envCfg signatureM timestampM rawBody
  interaction <- either (const $ throwError err404{errBody = "Invalid interaction data"}) pure $ AE.eitherDecode (fromStrict rawBody)
  Log.logTrace ("Discord interaction received" :: Text) $ AE.object ["type" AE..= show interaction.interaction_type, "guild_id" AE..= interaction.guild_id, "channel_id" AE..= interaction.channel_id, "data" AE..= interaction.data_i]

  case interaction.interaction_type of
    Ping -> pure $ AE.object ["type" AE..= (1 :: Int)]
    ApplicationCommand -> handleApplicationCommand interaction envCfg
  where
    validateSignature envCfg sigM tmeM body = do
      valid <- maybe (pure False) (\(sig, tme) -> verifyDiscordSignature (encodeUtf8 envCfg.discordPublicKey) sig tme body) ((,) <$> sigM <*> tmeM)
      unless valid $ throwError err401{errBody = "Invalid or missing signature"}

    handleApplicationCommand :: DiscordInteraction -> EnvConfig -> ATBaseCtx AE.Value
    handleApplicationCommand interaction envCfg = do
      cmdData <- maybe (throwError err400{errBody = "No command data provided"}) pure interaction.data_i
      guildId <- maybe (throwError err400{errBody = "Missing guild_id in interaction"}) pure $ maybe interaction.guild_id (.guild_id) interaction.channel
      maybe (pure $ AE.object ["type" AE..= (4 :: Int), "data" AE..= formatBotError Discord ServiceError]) (handleCommand cmdData interaction envCfg) =<< getDiscordData guildId

    handleCommand :: InteractionData -> DiscordInteraction -> EnvConfig -> DiscordData -> ATBaseCtx AE.Value
    handleCommand cmdData interaction envCfg discordData = case cmdData of
      CommandData name options -> case name of
        "monoscope" -> AE.object [] <$ handleAskCommand options interaction envCfg discordData
        -- The guild is implicit: `discordData` was resolved from interaction.guild_id above,
        -- so discordData.projectId IS this guild's project. We only need the channel used.
        "here" -> case interaction.channel_id of
          Just channelId -> hereSuccessResponse <$ ProjectMembers.addDiscordChannelToEveryoneTeam discordData.projectId channelId
          Nothing -> pure $ contentResponse "No channel ID provided"
        "dashboard" -> do
          deferAck envCfg interaction
          dashboards <- getDashboardsForDiscord (fromMaybe "" interaction.guild_id)
          AE.object [] <$ followup envCfg interaction (discordSelectContent dashboards "dashboard-select" "Select a dashboard")
        _ -> pure $ contentResponse "The command is not recognized"
      MessageComponentData{custom_id, values} -> do
        let selected = fromMaybe "" $ viaNonEmpty head values
        case custom_id of
          "dashboard-select" -> do
            deferAck envCfg interaction
            withDashboard discordData.projectId selected \dashboard -> do
              let widgets = (\w -> let t = fromMaybe "Untitled-" w.title in (t, t <> "___" <> selected)) <$> dashboard.widgets
              followup envCfg interaction $ discordSelectContent widgets "widget-select" "Select a widget"
          "widget-select" -> do
            deferAck envCfg interaction
            case T.splitOn "___" selected of
              [widget, dashboardId] -> withDashboard discordData.projectId dashboardId \dashboard ->
                whenJust (find (\w -> fromMaybe "Untitled-" w.title == widget) dashboard.widgets) \w -> do
                  chartUrl <- widgetPngUrl envCfg.apiKeyEncryptionSecretKey envCfg.hostUrl discordData.projectId w Nothing Nothing Nothing
                  followup envCfg interaction $ sharedWidgetContent widget chartUrl (envCfg.hostUrl <> "p/" <> discordData.projectId.toText <> "/dashboards/" <> dashboardId)
              _ -> pass
          _ -> pass
        pure $ AE.object []

    -- Resolve a dashboard id to its on-disk template, running the continuation if both exist.
    --
    -- Scoped to the interaction's own project: @dashboardId@ arrives inside the
    -- component @custom_id@, which is whatever the sender's client sent, so an
    -- unscoped lookup here renders another tenant's widget into this channel.
    withDashboard :: Projects.ProjectId -> Text -> (Dashboards.Dashboard -> ATBaseCtx ()) -> ATBaseCtx ()
    withDashboard pid dashboardId act = whenJust (idFromText dashboardId) \did ->
      whenJustM (Dashboards.getDashboardByProjectId pid did) \dashboardVM -> do
        dashboardM <- liftIO $ Dashboards.readDashboardFile "static/public/dashboards" (toString $ fromMaybe "_overview.yaml" dashboardVM.baseTemplate)
        whenJust dashboardM act

    handleAskCommand :: Maybe [InteractionOption] -> DiscordInteraction -> EnvConfig -> DiscordData -> ATBaseCtx ()
    handleAskCommand options interaction envCfg discordData = do
      deferAck envCfg interaction
      let userQuery = optionText options ""
          send = followup envCfg interaction
          runQuery ctx = processAIQuery envCfg.enableTimefusionReads discordData.projectId userQuery ctx envCfg.openaiModel envCfg.openaiApiKey
          respond = either (const $ send $ formatBotError Discord ServiceError) \resp ->
            dispatchAIResponse Discord envCfg discordData.projectId (optionText options "[?]") resp send getBotContentWithUrl
      case detectReportIntent userQuery of
        ReportIntent reportType ->
          processReportQuery discordData.projectId reportType envCfg
            >>= send
            . either (\err -> AE.object ["content" AE..= err]) (\(report, eventsUrl, errorsUrl) -> formatReportForDiscord report discordData.projectId envCfg eventsUrl errorsUrl)
        GeneralQueryIntent -> case interaction.channel of
          Just DiscordThreadChannel{type_ = 11, id = threadId} -> do
            let convId = Issues.textToConversationId threadId
            _ <- Issues.getOrCreateConversation discordData.projectId convId Issues.CTDiscordThread (AE.object ["channel_id" AE..= interaction.channel_id, "guild_id" AE..= interaction.guild_id])
            existingHistory <- Issues.selectChatHistory convId
            -- Advisory lock: only one interaction seeds the thread's history.
            when (null existingHistory) $ whenM (Issues.tryAcquireChatMigrationLock convId) do
              msgs <- getThreadStarterMessage interaction envCfg.discordBotToken
              for_ (fold msgs) \m ->
                Issues.insertChatMessage discordData.projectId convId (if m.author.username `elem` ["APItoolkit", "Monoscope"] then Issues.ChatAssistant else Issues.ChatUser) m.content Nothing Nothing
            threadContext <- formatHistoryAsContext "Discord" . map AI.dbMessageToLLMMessage <$> Issues.selectChatHistory convId
            result <- runQuery (Just threadContext)
            Issues.insertChatMessage discordData.projectId convId Issues.ChatUser userQuery Nothing Nothing
            whenRight_ result \resp -> whenJust resp.query \q -> Issues.insertChatMessage discordData.projectId convId Issues.ChatAssistant q Nothing Nothing
            respond result
          _ -> runQuery Nothing >>= respond


-- | First slash-command option's string value, or a fallback.
optionText :: Maybe [InteractionOption] -> Text -> Text
optionText options fallback = case options of
  Just (InteractionOption{value = AE.String q} : _) -> q
  _ -> fallback


contentResponse :: Text -> AE.Value
contentResponse msg = AE.object ["type" AE..= 4, "data" AE..= AE.object ["content" AE..= msg]]


-- Discord "components v2" building blocks (flag 32768 opts the message into the new layout).
containerContent :: Int -> [AE.Value] -> AE.Value
containerContent accent components =
  AE.object ["flags" AE..= (32768 :: Int), "components" AE..= ([AE.object ["type" AE..= (17 :: Int), "accent_color" AE..= accent, "components" AE..= components]] :: [AE.Value])]


textComponent :: Text -> AE.Value
textComponent content = AE.object ["type" AE..= (10 :: Int), "content" AE..= content]


galleryComponent :: Text -> Text -> AE.Value
galleryComponent url description = AE.object ["type" AE..= (12 :: Int), "items" AE..= ([AE.object ["media" AE..= AE.object ["url" AE..= url], "description" AE..= description]] :: [AE.Value])]


linkButton :: Text -> Text -> AE.Value
linkButton label url = AE.object ["type" AE..= (1 :: Int), "components" AE..= ([AE.object ["type" AE..= (2 :: Int), "label" AE..= label, "url" AE..= url, "style" AE..= (5 :: Int)]] :: [AE.Value])]


hereSuccessResponse :: AE.Value
hereSuccessResponse =
  AE.object
    [ "type" AE..= (4 :: Int)
    , "data"
        AE..= containerContent
          5763719 -- Green accent
          [ textComponent $ botEmoji "success" <> " **Notification channel set**"
          , textComponent "This channel will now receive:"
          , textComponent $ "• " <> botEmoji "error" <> " Error alerts\n• " <> botEmoji "chart" <> " Daily & weekly reports\n• " <> botEmoji "warning" <> " Anomaly detections"
          ]
    ]


getBotContentWithUrl :: Text -> Text -> Text -> Text -> AE.Value
getBotContentWithUrl question query query_url imageUrl =
  containerContent
    26879
    [ textComponent $ botEmoji "chart" <> " **" <> question <> "**"
    , galleryComponent imageUrl ("Chart visualization: " <> question)
    , textComponent $ "**Query:** `" <> query <> "`"
    , linkButton (botEmoji "search" <> " View in Log Explorer") query_url
    ]


sharedWidgetContent :: Text -> Text -> Text -> AE.Value
sharedWidgetContent widgetTitle chartUrl dashboardUrl =
  containerContent
    26879
    [ textComponent $ botEmoji "chart" <> " **" <> widgetTitle <> "**"
    , galleryComponent chartUrl ("Dashboard widget: " <> widgetTitle)
    , linkButton "Open dashboard" dashboardUrl
    ]


discordSelectContent :: [(Text, Text)] -> Text -> Text -> AE.Value
discordSelectContent choices sId placeholder =
  AE.object
    [ "flags" AE..= 64
    , "components"
        AE..= ( [ AE.object
                    [ "type" AE..= (1 :: Int)
                    , "components" AE..= ([AE.object ["type" AE..= (3 :: Int), "custom_id" AE..= sId, "placeholder" AE..= placeholder, "options" AE..= map (\(t, v) -> AE.object ["label" AE..= t, "value" AE..= v]) choices]] :: [AE.Value])
                    ]
                ]
                  :: [AE.Value]
              )
    ]


discordJsonPost :: HTTP :> es => Text -> Text -> AE.Value -> Eff es ()
discordJsonPost botToken url payload = void $ postWith (defaults & authHeader "Bot" botToken & contentTypeHeader "application/json") (toString url) payload


-- | ACK the interaction within Discord's 3s window so the real answer can be sent as a followup.
deferAck :: HTTP :> es => EnvConfig -> DiscordInteraction -> Eff es ()
deferAck envCfg i = discordJsonPost envCfg.discordBotToken ("https://discord.com/api/v10/interactions/" <> i.id <> "/" <> i.token <> "/callback") (AE.object ["type" AE..= (5 :: Int)])


followup :: (HTTP :> es, Log.Log :> es) => EnvConfig -> DiscordInteraction -> AE.Value -> Eff es ()
followup envCfg i content = do
  Log.logTrace ("Discord followup response" :: Text) content
  discordJsonPost envCfg.discordBotToken ("https://discord.com/api/v10/webhooks/" <> envCfg.discordClientId <> "/" <> i.token) content


verifyDiscordSignature :: Log.Log :> es => ByteString -> ByteString -> ByteString -> ByteString -> Eff es Bool
verifyDiscordSignature publicKey signatureHex timestamp rawBody =
  either (\err -> False <$ Log.logTrace ("Discord signature verification failed: " <> toText err) ()) pure result
  where
    result = do
      sig <- first show . Crypto.eitherCryptoError . Ed25519.signature =<< first show (Base16.decode signatureHex)
      pk <- first show . Crypto.eitherCryptoError . Ed25519.publicKey =<< first show (Base16.decode publicKey)
      pure $ Ed25519.verify pk (timestamp <> rawBody) sig


newtype DiscordUser = DiscordUser
  { username :: Text
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON)


data DiscordMessage = DiscordMessage
  { content :: Text
  , author :: DiscordUser
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON)


getThreadStarterMessage :: (HTTP :> es, Ki.StructuredConcurrency :> es, Log.Log :> es) => DiscordInteraction -> Text -> Eff es (Maybe [DiscordMessage])
getThreadStarterMessage interaction botToken = case (interaction.channel_id, interaction.channel) of
  (Just channelId, Just DiscordThreadChannel{type_ = 11, parent_id = Just pId}) -> do
    let baseUrl = "https://discord.com/api/v10/channels/"
        opts = defaults & authHeader "Bot" botToken & contentTypeHeader "application/json"
    -- Plain Ki.fork (not forkWithCtx): Data.Effectful.Wreq.getWith is not
    -- OTel-instrumented, so the child fibers emit no spans — there is
    -- nothing to parent and propagation would only add IOE constraint
    -- noise to this signature.
    (response, response') <- Ki.scoped \scope -> do
      t1 <- Ki.fork scope $ getWith opts (toString $ baseUrl <> channelId <> "/messages?limit=50")
      t2 <- Ki.fork scope $ getWith opts (toString $ baseUrl <> pId <> "/messages/" <> channelId)
      liftA2 (,) (Ki.atomically $ Ki.await t1) (Ki.atomically $ Ki.await t2)
    case AE.eitherDecode (response ^. responseBody) of
      Left err -> Nothing <$ Log.logAttention ("Error decoding Discord thread messages: " <> toText err) ()
      Right messages -> pure $ Just $ messages <> maybeToList (rightToMaybe (AE.eitherDecode (response' ^. responseBody)) :: Maybe DiscordMessage)
  _ -> pure Nothing


registerDiscordCommands :: HTTP :> es => Text -> Text -> Text -> Eff es ()
registerDiscordCommands appId botToken guildId =
  for_ commands
    $ discordJsonPost botToken ("https://discord.com/api/v10/applications/" <> appId <> "/guilds/" <> guildId <> "/commands")
  where
    commands =
      [ AE.object
          [ "name" AE..= ("monoscope" :: Text)
          , "description" AE..= ("Ask about your project or get reports" :: Text)
          , "type" AE..= (1 :: Int)
          , "options" AE..= ([AE.object ["name" AE..= ("question" :: Text), "description" AE..= ("Your question in natural language" :: Text), "type" AE..= (3 :: Int), "required" AE..= True]] :: [AE.Value])
          ]
      , AE.object ["name" AE..= ("here" :: Text), "description" AE..= ("Channel for monoscope to send notifications" :: Text), "type" AE..= (1 :: Int)]
      , AE.object ["name" AE..= ("dashboard" :: Text), "description" AE..= ("Select and share dashboard widget" :: Text), "type" AE..= (1 :: Int)]
      ]
