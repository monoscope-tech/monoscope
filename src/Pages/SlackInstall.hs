{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pages.SlackInstall (linkProjectGetH, linkDiscordGetH, discordInteractionsH, DiscordInteraction, SlackLink, slackInteractionsH, SlackInteraction) where

import Control.Concurrent (forkIO)
import Crypto.Error qualified as Crypto
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.Aeson qualified as AE
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Default (Default (def))
import Data.Text qualified as T
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Deriving.Aeson qualified as DAE
import Effectful.Error.Static (throwError)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask, asks)
import Lucid
import Models.Apis.Slack (DiscordData (..), SlackData (..), getDiscordData, getSlackDataByTeamId, insertAccessToken, insertDiscordData, updateDiscordNotificationChannel, updateSlackNotificationChannel)
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (BWConfig, PageCtx (..), currProject, pageTitle, sessM)
import Pkg.Components (navBar)
import Pkg.Mail (sendSlackMessage)
import Relude hiding (ask, asks)
import Control.Lens ((.~), (^.))
import Data.Effectful.Wreq (
  HTTP,
  Options,
  defaults,
  getWith,
  header,
  postWith,
  responseBody,
 )
import Effectful (Eff, type (:>))
import Network.HTTP.Types (urlEncode)
import Network.Wreq qualified as Wreq
import Network.Wreq.Types (FormParam)
import Pkg.Components.Widget qualified as Widget
import Servant.API (Header)
import Servant.API.ResponseHeaders (Headers, addHeader)
import Servant.Server (ServerError (errBody), err400, err401)
import System.Config (AuthContext (env, pool), EnvConfig (..))
import System.Types (ATBaseCtx)
import Utils (callOpenAIAPI, faSprite_, systemPrompt)
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


linkProjectGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> ATBaseCtx (Headers '[Header "Location" Text] SlackLink)
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
      sendSlackMessage pid ("APItoolkit Bot has been linked to your project: " <> project'.title)
      case onboardingM of
        Just _ -> pure $ addHeader ("/p/" <> pid.toText <> "/onboarding?step=NotifChannel") $ NoContent $ PageCtx bwconf ()
        Nothing -> pure $ addHeader "" $ SlackLinked $ PageCtx bwconf ()
    (_, _) -> pure $ addHeader ("/p/" <> pid.toText <> "/onboarding?step=NotifChannel") $ NoTokenFound $ PageCtx bwconf ()


data SlackLink
  = SlackLinked (PageCtx ())
  | DiscordLinked (PageCtx ())
  | NoTokenFound (PageCtx ())
  | DiscordError (PageCtx ())
  | NoContent (PageCtx ())


instance ToHtml SlackLink where
  toHtml (SlackLinked (PageCtx bwconf ())) = toHtml $ PageCtx bwconf installedSuccess
  toHtml (DiscordLinked (PageCtx bwconf ())) = toHtml $ PageCtx bwconf installedSuccessDisocrd
  toHtml (DiscordError (PageCtx bwconf ())) = toHtml $ PageCtx bwconf discordError
  toHtml (NoTokenFound (PageCtx bwconf ())) = toHtml $ PageCtx bwconf noTokenFound
  toHtml (NoContent (PageCtx bwconf ())) = toHtml $ PageCtx bwconf ""
  toHtmlRaw = toHtml


noTokenFound :: Html ()
noTokenFound = do
  navBar
  section_ [class_ "h-full mt-[80px] w-[1000px] flex flex-col items-center mx-auto"] do
    h3_ [class_ "text-5xl font-semibold my-8"] "Token Not Found"
    p_ [class_ "text-2xl"] "No slack access token found, reinstall the APItoolkit slack app to try again."


discordError :: Html ()
discordError = do
  navBar
  section_ [class_ "h-full mt-[80px] w-[1000px] flex flex-col items-center mx-auto"] do
    faSprite_ "circle-exclamation" "solid" "text-red-500 h-10 w-10"
    h3_ [class_ "text-4xl font-bold my-6 text-red-600"] "Uh-oh! Something went wrong"
    p_
      [class_ "text-xl text-gray-700 text-center max-w-prose mb-4"]
      "We hit a snag while trying to install the Discord bot. Don’t worry — it happens!"
    p_
      [class_ "text-md text-gray-600 text-center max-w-prose"]
      "This could be due to not adding discord from the integrations page, click on add to discord on the integrations page to try again."


installedSuccess :: Html ()
installedSuccess = do
  navBar
  section_ [class_ "h-full mt-[80px] w-[1000px] flex flex-col items-center mx-auto"] do
    div_ [class_ "flex flex-col border px-6 py-16 mt-16 rounded-2xl items-center"] do
      faSprite_ "check" "regular" "h-10 w-10 text-green-500"
      h3_ [class_ "text-3xl font-semibold my-8"] "APItoolkit Slack App Installed"
      p_ [class_ "text-gray-600 text-center max-w-prose"] "APItoolkit Bot Slack app has been connected to your project successfully. You can now recieve notifications on slack."


installedSuccessDisocrd :: Html ()
installedSuccessDisocrd = do
  navBar
  section_ [class_ "h-full mt-[80px] w-[1000px] flex flex-col items-center mx-auto"] do
    div_ [class_ "flex flex-col border px-6 py-16 mt-16 rounded-2xl items-center"] do
      faSprite_ "check" "regular" "h-10 w-10 text-green-500"
      h3_ [class_ "text-3xl font-semibold my-8"] "APItoolkit Discord Bot Installed"
      p_
        [class_ "text-gray-600 text-center max-w-prose mb-4"]
        "APItoolkit Bot has been successfully added to your Discord server. You're all set to receive real-time alerts and interact with your API data — right from Discord!"

      h4_ [class_ "text-xl font-semibold mt-8 mb-4"] "Available Slash Commands:"
      ul_ [class_ "text-left text-gray-700 space-y-2"] do
        li_ do
          span_ [class_ "font-mono bg-gray-100 px-2 py-1 rounded"] "/ask"
          span_ " – Ask any question about your project’s API behavior or errors."
        li_ do
          span_ [class_ "font-mono bg-gray-100 px-2 py-1 rounded"] "/here"
          span_ " – Quickly link this channel to receive alerts. No more digging through logs."


linkDiscordGetH :: Maybe Text -> Maybe Text -> Maybe Text -> ATBaseCtx (Headers '[Header "Location" Text] SlackLink)
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
      _ <- registerDiscordCommands envCfg.discordClientId envCfg.discordBotToken guildId
      _ <- dbtToEff $ insertDiscordData pid guildId
      if isOnboarding
        then pure $ addHeader ("/p/" <> pid.toText <> "/onboarding?step=NotifChannel") $ NoContent $ PageCtx bwconf ()
        else pure $ addHeader "" $ DiscordLinked $ PageCtx def ()
    _ ->
      pure $ addHeader "" $ DiscordError $ PageCtx def ()


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
    Ping -> pure $ AE.object ["type" AE..= (1 :: Int)]
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
              let reqBody = getChartData query vizType authCtx discordData.projectId
                  baseUrl = authCtx.env.chartShotUrl
                  content =
                    AE.object
                      [ "embeds"
                          AE..= AE.Array (V.fromList [AE.object ["title" AE..= "Here is your chart", "image" AE..= AE.object ["url" AE..= chartImageUrl reqBody baseUrl]]])
                      ]
              sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken content
              pure $ contentResponse "Generated query: "
            Nothing -> do
              _ <- sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken (AE.object ["content" AE..= ("Generated query: " <> query)])
              pure $ AE.object []


getChartData :: Text -> Text -> AuthContext -> Projects.ProjectId -> AE.Value
getChartData query vizType authCtx pid =
  let widgetJson = createWidgetJson vizType pid query
      chartType = Widget.mapWidgetTypeToChartType $ Widget.mapChatTypeToWidgetType vizType
   in AE.object ["q" AE..= query, "p" AE..= pid.toText, "e" AE..= widgetJson, "t" AE..= chartType]


-- Helper functions
buildPrompt :: InteractionData -> DiscordInteraction -> EnvConfig -> ATBaseCtx Text
buildPrompt cmdData interaction envCfg = do
  threadMsgs <- getThreadStarterMessage interaction envCfg.discordBotToken
  pure $ case cmdData.options of
    Just (InteractionOption{value = AE.String q} : _) -> case threadMsgs of
      Just msgs -> threadsPrompt msgs q
      _ -> systemPrompt <> "\n\nUser query: " <> q
    _ -> ""


createWidgetJson :: Text -> Projects.ProjectId -> Text -> AE.Value
createWidgetJson vizType projectId query =
  Widget.widgetToECharts
    $ (def :: Widget.Widget)
      { Widget.wType = Widget.mapChatTypeToWidgetType vizType
      , Widget.standalone = Just True
      , Widget.hideSubtitle = Just True
      , Widget.yAxis = Just (def{Widget.showOnlyMaxLabel = Just True})
      , Widget.summarizeBy = Just Widget.SBMax
      , Widget.layout = Just (def{Widget.w = Just 6, Widget.h = Just 4})
      , Widget.unit = Just "ms"
      , Widget.hideLegend = Just True
      , Widget._projectId = Just projectId
      , Widget.query = Just query
      }


contentResponse :: Text -> AE.Value
contentResponse msg = AE.object ["type" AE..= (4 :: Int), "data" AE..= AE.object ["content" AE..= msg]]


threadsPrompt :: [DiscordMessage] -> Text -> Text
threadsPrompt msgs question = prompt
  where
    msgs' = (\x -> "- @" <> x.author.username <> " :" <> x.content) <$> msgs
    threadPrompt =
      unlines
        $ [ "\n\nTHREADS:"
          , "- this query is  part of a conversation thread. Use previous messages provited in the thread for additional context if needed."
          , "- the user query is the main one to answer, but earlier messages may contain important clarifications or parameters."
          , "Previous messages in this thread:"
          ]
          <> msgs'
          <> ["\n\nCurrent user query: " <> question]

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

      hereCommand = AE.object ["name" AE..= "here", "description" AE..= "Channel for apitoolkit to send notifications", "type" AE..= 1]
      opts = defaults & authHeader botToken & contentTypeHeader "application/json"

  _ <- postWith opts url (AE.encode askCommand)
  _ <- postWith opts url (AE.encode hereCommand)
  pure $ Right ()


sendDeferredResponse :: HTTP :> es => Text -> Text -> Text -> Eff es ()
sendDeferredResponse interactionId interactionToken botToken = do
  let url = toString $ "https://discord.com/api/v10/interactions/" <> interactionId <> "/" <> interactionToken <> "/callback"
      payload = AE.encode $ AE.object ["type" AE..= (5 :: Int)]
  _ <- postWith (defaults & authHeader botToken & contentTypeHeader "application/json") url payload
  pass


authHeader :: Text -> Data.Effectful.Wreq.Options -> Data.Effectful.Wreq.Options
authHeader token = header "Authorization" .~ [encodeUtf8 $ "Bot " <> token]


contentTypeHeader :: Text -> Data.Effectful.Wreq.Options -> Data.Effectful.Wreq.Options
contentTypeHeader contentType = header "Content-Type" .~ [encodeUtf8 contentType]


data BufferResponse = BufferResponse
  { bufferType :: String
  , bufferData :: [Word8]
  }
  deriving (Generic, Show)


instance AE.FromJSON BufferResponse where
  parseJSON = AE.withObject "BufferResponse" \o ->
    BufferResponse
      <$> o
        AE..: "type"
      <*> o
        AE..: "data"




sendJsonFollowupResponse :: HTTP :> es => Text -> Text -> Text -> AE.Value -> Eff es ()
sendJsonFollowupResponse appId interactionToken botToken content = do
  let followupUrl = toString $ "https://discord.com/api/v10/webhooks/" <> appId <> "/" <> interactionToken
  _ <- postWith (defaults & authHeader botToken & contentTypeHeader "application/json") followupUrl content
  pass


slackInteractionsH :: SlackInteraction -> ATBaseCtx AE.Value
slackInteractionsH interaction = do
  case interaction.command of
    "here" -> do
      _ <- updateSlackNotificationChannel interaction.team_id interaction.channel_id
      pure $ AE.object ["response_type" AE..= "in_channel", "text" AE..= "Done, you'll be receiving project notifcations here going forward"]
    _ -> do
      slackDataM <- dbtToEff $ getSlackDataByTeamId interaction.team_id
      authCtx <- Effectful.Reader.Static.ask @AuthContext
      void $ liftIO $ forkIO $ do
        case slackDataM of
          Nothing -> sendSlackFollowupResponse interaction.response_url (AE.object ["text" AE..= "Error: something went wrong"])
          Just slackData -> handleAskCommand interaction slackData authCtx
      pure $ AE.object ["response_type" AE..= "in_channel", "text" AE..= "apitoolkit is working..."]
  where
    handleAskCommand :: SlackInteraction -> SlackData -> AuthContext -> IO ()
    handleAskCommand inter slackData authCtx = do
      let envCfg = authCtx.env
      let question = inter.text
          fullPrompt = systemPrompt <> "\n\nUser query: " <> question
      result <- liftIO $ callOpenAIAPI fullPrompt envCfg.openaiApiKey
      case result of
        Left err ->
          sendSlackFollowupResponse inter.response_url (AE.object ["text" AE..= "Error: something went wrong"])
        Right (query, vizTypeM) -> do
          case vizTypeM of
            Just vizType -> do
              -- let reqBody = getChartData query vizType authCtx slackData.projectId
              -- _ <- replyWithChartImage interaction reqBody envCfg.discordBotToken envCfg.discordClientId
              let content = AE.object ["response_type" AE..= "in_channel", "text" AE..= ("Generated query: " <> query <> "\n\n" <> vizType <> slackData.projectId.toText)]
              sendSlackFollowupResponse inter.response_url content
              pass
            Nothing -> do
              let content = AE.object ["response_type" AE..= "in_channel", "text" AE..= ("Generated query: " <> query)]
              sendSlackFollowupResponse inter.response_url content

      pass


sendSlackFollowupResponse :: Text -> AE.Value -> IO ()
sendSlackFollowupResponse responseUrl content = do
  _ <- Wreq.postWith (defaults & contentTypeHeader "application/json") (toString responseUrl) content
  pass


data SlackInteraction = SlackInteraction
  { team_id :: Text
  , command :: Text
  , text :: Text
  , response_url :: Text
  , trigger_id :: Text
  , api_app_id :: Text
  , channel_id :: Text
  , channel_name :: Text
  , user_id :: Text
  , enterprise_id :: Maybe Text
  , enterprise_name :: Maybe Text
  , team_domain :: Text
  }
  deriving (Generic, Show)
  deriving anyclass (FromForm, AE.FromJSON)


chartImageUrl :: AE.Value -> Text -> Text
chartImageUrl options baseUrl =
  let jsonBS = toStrict (AE.encode options)
      encoded = urlEncode True jsonBS
   in baseUrl <> "?opts=" <> decodeUtf8 encoded
