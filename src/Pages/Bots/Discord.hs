module Pages.Bots.Discord (linkDiscordGetH, discordInteractionsH, DiscordInteraction) where

import Crypto.Error qualified as Crypto
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.Aeson qualified as AE
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Default (Default (def))
import Data.Text qualified as T
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (query_, withPool)
import Deriving.Aeson qualified as DAE
import Effectful.Concurrent (forkIO)
import Effectful.Error.Static (throwError)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask, asks)
import Lucid
import Models.Apis.Slack (DiscordData (..), SlackData (..), getDashboardsForSlack, getDiscordData, getSlackDataByTeamId, insertAccessToken, insertDiscordData, updateDiscordNotificationChannel, updateSlackNotificationChannel)
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (BWConfig, PageCtx (..), currProject, pageTitle, sessM)
import Pages.Components (navBar)
import Pkg.Mail (sendSlackMessage)
import Relude hiding (ask, asks)

import Control.Lens ((.~), (^.))
import Data.Aeson.Key qualified as KEM
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types (parseMaybe)
import Data.Effectful.Wreq (
  HTTP,
  Options,
  defaults,
  getWith,
  header,
  postWith,
  responseBody,
 )
import Data.Time qualified as Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Effectful (Eff, type (:>))
import Effectful.Time qualified as Time
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Dashboards qualified as Dashboards
import Network.HTTP.Types (urlEncode)
import Network.Wreq qualified as Wreq
import Network.Wreq.Types (FormParam)
import Pages.Bots.Utils (BotResponse (..), BotType (..), authHeader, chartImageUrl, contentTypeHeader, handleTableResponse)
import Pkg.Components.Widget (Widget (..))
import Pkg.Components.Widget qualified as Widget
import Pkg.Parser (parseQueryToAST)
import Servant.API (Header)
import Servant.API.ResponseHeaders (Headers, addHeader)
import Servant.Server (ServerError (errBody), err400, err401)
import System.Config (AuthContext (env, pool), EnvConfig (..))
import System.Types (ATBaseCtx)
import Utils (callOpenAIAPI, faSprite_, getDurationNSMS, listToIndexHashMap, lookupVecBoolByKey, lookupVecIntByKey, lookupVecTextByKey, systemPrompt)
import Web.FormUrlEncoded (FromForm)


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
          }
  case (pidM, codeM, guildIdM) of
    (Just pid, Just code, Just guildId) -> do
      r' <- exchangeCodeForTokenDiscord envCfg.discordClientId envCfg.discordClientSecret code envCfg.discordRedirectUri
      case r' of
        Just t -> do
          _ <- dbtToEff $ insertDiscordData pid guildId
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
data InteractionType = Ping | ApplicationCommand
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
  , channel :: Maybe Channel
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


data Channel = Channel
  { id :: Text
  , name :: Text
  , guild_id :: Maybe Text
  , type_ :: Int
  , parent_id :: Maybe Text
  , owner_id :: Maybe Text
  , thread_metadata :: Maybe ThreadMetadata
  }
  deriving (Generic, Show)


instance AE.FromJSON Channel where
  parseJSON = AE.genericParseJSON AE.defaultOptions{AE.fieldLabelModifier = \f -> if f == "type_" then "type" else f}


instance AE.FromJSON DiscordInteraction where
  parseJSON = AE.genericParseJSON AE.defaultOptions{AE.fieldLabelModifier = \f -> if f == "data_i" then "data" else if f == "interaction_type" then "type" else f}


-- Slash command data
data InteractionData = InteractionData
  { name :: Text
  , options :: Maybe [InteractionOption]
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON)


data InteractionOption = InteractionOption
  { name :: Text
  , value :: AE.Value
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON)


discordInteractionsH :: BS.ByteString -> Maybe BS.ByteString -> Maybe BS.ByteString -> ATBaseCtx AE.Value
discordInteractionsH rawBody signatureM timestampM = do
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  let envCfg = authCtx.env

  validateSignature envCfg signatureM timestampM rawBody
  interaction <- parseInteraction rawBody

  case interaction.interaction_type of
    Ping -> pure $ AE.object ["type" AE..= 1]
    ApplicationCommand -> handleApplicationCommand interaction envCfg authCtx
  where
    validateSignature envCfg (Just sig) (Just tme) body
      | verifyDiscordSignature (encodeUtf8 envCfg.discordPublicKey) sig tme body = pass
      | otherwise = throwError err401{errBody = "Invalid signature"}
    validateSignature _ _ _ _ = throwError err401{errBody = "Invalid signature"}

    handleApplicationCommand :: DiscordInteraction -> EnvConfig -> AuthContext -> ATBaseCtx AE.Value
    handleApplicationCommand interaction envCfg authCtx = do
      cmdData <- case data_i interaction of
        Nothing -> throwError err400{errBody = "No command data provided"}
        Just cmd -> pure cmd
      discordData <- getDiscordData (fromMaybe "" interaction.guild_id)
      case discordData of
        Nothing -> pure $ contentResponse "Sorry, there was an error processing your request"
        Just d -> handleCommand cmdData interaction envCfg authCtx d

    parseInteraction :: BS.ByteString -> ATBaseCtx DiscordInteraction
    parseInteraction rawBody' = do
      case AE.decodeStrict' rawBody' of
        Nothing -> throwError err401{errBody = "Invalid interaction data"}
        Just interaction -> pure interaction

    handleCommand :: InteractionData -> DiscordInteraction -> EnvConfig -> AuthContext -> DiscordData -> ATBaseCtx AE.Value
    handleCommand cmdData interaction envCfg authCtx discordData =
      case cmdData.name of
        "ask" -> handleAskCommand cmdData interaction envCfg authCtx discordData
        "here" -> do
          case (interaction.channel_id, interaction.guild_id) of
            (Just channelId, Just guildId) -> do
              _ <- updateDiscordNotificationChannel guildId channelId
              pure $ contentResponse "Got it, notifications and alerts on your project will now be sent to this channel"
            _ -> pure $ contentResponse "No channel ID provided"
        _ -> pure $ contentResponse "The command is not recognized"

    handleAskCommand :: InteractionData -> DiscordInteraction -> EnvConfig -> AuthContext -> DiscordData -> ATBaseCtx AE.Value
    handleAskCommand cmdData interaction envCfg authCtx discordData = do
      now <- Time.currentTime
      _ <- sendDeferredResponse interaction.id interaction.token envCfg.discordBotToken
      fullPrompt <- buildPrompt cmdData interaction envCfg
      result <- liftIO $ callOpenAIAPI fullPrompt envCfg.openaiApiKey
      case result of
        Left err -> do
          _ <- sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken (AE.object ["content" AE..= "Sorry, there was an error processing your request"])
          pure $ AE.object []
        Right (query, vizTypeM) -> do
          case vizTypeM of
            Just vizType -> do
              let chartType = Widget.mapWidgetTypeToChartType $ Widget.mapChatTypeToWidgetType vizType
                  query_url = authCtx.env.hostUrl <> "p/" <> discordData.projectId.toText <> "/log_explorer?viz_type=" <> chartType <> ("&query=" <> decodeUtf8 (urlEncode True $ encodeUtf8 query))
                  opts = "&q=" <> (decodeUtf8 $ urlEncode True (encodeUtf8 query)) <> "&p=" <> discordData.projectId.toText <> "&t=" <> chartType
                  question = case cmdData.options of
                    Just (InteractionOption{value = AE.String q} : _) -> q
                    _ -> "[?]"
                  content = getBotContent question query query_url opts authCtx.env.chartShotUrl now
              sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken content
              pure $ contentResponse "Generated query: "
            Nothing -> do
              let queryAST = parseQueryToAST query
              case queryAST of
                Left err -> do
                  _ <- sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken (AE.object ["content" AE..= ("Error parsing query: " <> err)])
                  pure $ AE.object []
                Right query' -> do
                  tableAsVecE <- RequestDumps.selectLogTable discordData.projectId query' query Nothing (Nothing, Nothing) [] Nothing Nothing
                  let content = handleTableResponse Discord tableAsVecE envCfg discordData.projectId query
                  _ <- sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken content
                  pure $ AE.object []


buildPrompt :: InteractionData -> DiscordInteraction -> EnvConfig -> ATBaseCtx Text
buildPrompt cmdData interaction envCfg = do
  threadMsgs <- getThreadStarterMessage interaction envCfg.discordBotToken
  pure $ case cmdData.options of
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


getThreadStarterMessage :: HTTP :> es => DiscordInteraction -> Text -> Eff es (Maybe [DiscordMessage])
getThreadStarterMessage interaction botToken = do
  case interaction.channel_id of
    Just channelId -> case interaction.channel of
      Just Channel{type_ = 11, parent_id = Just pId} -> do
        let baseUrl = "https://discord.com/api/v10/channels/"
            url = toString $ baseUrl <> channelId <> "/messages?limit=50"
            starterMessageUrl = toString $ baseUrl <> pId <> "/messages/" <> channelId
            opts = defaults & authHeader botToken & contentTypeHeader "application/json"
        response <- getWith opts url
        response' <- getWith opts starterMessageUrl
        case AE.eitherDecode (response ^. responseBody) of
          Left err -> do
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
                          , AE.object
                              [ "type" AE..= 12
                              , "items"
                                  AE..= AE.Array (V.singleton $ AE.object ["media" AE..= AE.object ["url" AE..= (chartImageUrl chartOptions baseUrl now)]])
                              ]
                          , AE.object ["type" AE..= 10, "content" AE..= ("**Query used:** " <> query)]
                          , AE.object
                              [ "type" AE..= 1
                              , "components"
                                  AE..= AE.Array (V.fromList [AE.object ["type" AE..= 2, "label" AE..= "Open explorer", "url" AE..= query_url, "style" AE..= 5]])
                              ]
                          ]
                      )
                ]
          )
    ]
