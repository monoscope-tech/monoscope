-- TODO: temporary, to work with current logic
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pages.SlackInstall (linkProjectGetH, postH, registerGlobalDiscordCommands, linkDiscordGetH, discordInteractionsH, DiscordInteraction, DiscordInteractionResponse, LinkProjectsForm, updateWebHook, SlackLink) where

-- import Crypto.Error (CryptoFailable (..), eitherCryptoError)

-- import Crypto.PubKey.Ed25519 (publicKey, signature, verify)

import Crypto.Error qualified as Crypto

import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.Aeson
import Data.Aeson qualified as AE
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Default (Default (def))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Database.PostgreSQL.Entity.DBT (withPool)
import Deriving.Aeson qualified as DAE
import Effectful.Error.Static (throwError)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask, asks)
import Lucid
import Models.Apis.Slack (getDiscordData, insertAccessToken, insertDiscordData, updateDiscordNotificationChannel)
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig, PageCtx (..), currProject, pageTitle, sessM)
import Pkg.Components (navBar)
import Pkg.Mail (sendSlackMessage)
import Relude hiding (ask, asks)

import Control.Exception (try)
import Control.Lens hiding ((.=))
import Data.Aeson.QQ (aesonQQ)
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Effectful (raise)
import Effectful.Internal.Monad (subsume)
import Effectful.Time qualified as Time
import Langchain.LLM.Core qualified as LLM
import Langchain.LLM.OpenAI (OpenAI (..))
import Network.HTTP.Client (RequestBody (..))
import Network.HTTP.Client.MultipartFormData (PartM, partFileRequestBody)
import Network.Wreq
import Pages.Charts.Charts (queryMetrics)
import Pages.Charts.Charts qualified as Chart
import Pkg.Components.Widget (Widget (..))
import Pkg.Components.Widget qualified as Widget
import Pkg.Parser (QueryComponents (..), SqlQueryCfg (..), defSqlQueryCfg, parseQuery, parseQueryToAST, queryASTToComponents)
import Servant.API (Header)
import Servant.API.ResponseHeaders (Headers, addHeader)
import Servant.Server
import System.Config (AuthContext (env, pool), EnvConfig (..))
import System.Types (ATAuthCtx, ATBaseCtx, RespHeaders, addRespHeaders, addSuccessToast, atAuthToBase)
import Utils (callOpenAIAPI, faSprite_, systemPrompt)
import Web.FormUrlEncoded (FromForm)


data LinkProjectsForm = LinkProjectsForm
  { projects :: [Text]
  , webhookUrl :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


data IncomingWebhook = IncomingWebhook
  { channel :: Text
  , channelId :: Text
  , configurationUrl :: Text
  , url :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] IncomingWebhook


data TokenResponse = TokenResponse
  { ok :: Bool
  , incomingWebhook :: IncomingWebhook
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] TokenResponse


exchangeCodeForToken :: Text -> Text -> Text -> Text -> IO (Maybe TokenResponse)
exchangeCodeForToken clientId clientSecret redirectUri code = do
  let formData :: [FormParam]
      formData =
        [ "client_id" := clientId
        , "client_secret" := clientSecret
        , "code" := code
        , "redirect_uri" := redirectUri
        ]

  let hds = header "Content-Type" .~ ["application/x-www-form-urlencoded; charset=utf-8"]
  response <- postWith (defaults & hds) "https://slack.com/api/oauth.v2.access" formData
  let responseBdy = response ^. responseBody
  case AE.decode responseBdy of
    Just token -> do
      return $ Just token
    Nothing -> return Nothing


updateWebHook :: Projects.ProjectId -> LinkProjectsForm -> ATAuthCtx (RespHeaders (Html ()))
updateWebHook pid LinkProjectsForm{projects, webhookUrl} = do
  appCtx <- ask @AuthContext
  sess' <- Sessions.getSession

  _ <- dbtToEff $ insertAccessToken [pid.toText] webhookUrl
  addSuccessToast "Webhook url updated successfully" Nothing
  addRespHeaders $ span_ [] "Projects linked successfully"


postH :: LinkProjectsForm -> ATAuthCtx (RespHeaders (Html ()))
postH LinkProjectsForm{projects, webhookUrl} = do
  appCtx <- ask @AuthContext
  sess' <- Sessions.getSession

  _ <- dbtToEff $ insertAccessToken projects webhookUrl
  addSuccessToast "Slack account linked to project(s), successfully" Nothing
  addRespHeaders $ span_ [] "Projects linked successfully"


linkProjectGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> ATBaseCtx (Headers '[Header "Location" Text] SlackLink)
linkProjectGetH pid slack_code onboardingM = do
  envCfg <- asks env
  pool <- asks pool
  let client_id = envCfg.slackClientId
  let client_secret = envCfg.slackClientSecret
  let redirect_uri = envCfg.slackRedirectUri
  token <- liftIO $ exchangeCodeForToken client_id client_secret (redirect_uri <> pid.toText <> if isJust onboardingM then "?onboarding=true" else "") (fromMaybe "" slack_code)
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
        insertAccessToken [pid.toText] token'.incomingWebhook.url
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
  let pidM = pidM' >>= Projects.projectIdFromText
  case (pidM, codeM, guildIdM) of
    (Just pid, Just code, Just guildId) -> do
      _ <- dbtToEff $ insertDiscordData pid guildId
      pure $ addHeader "" $ DiscordLinked $ PageCtx def ()
    _ ->
      pure $ addHeader "" $ DiscordError $ PageCtx def ()


-- Discord interaction type
data InteractionType = Ping | ApplicationCommand
  deriving (Eq, Show)


instance FromJSON InteractionType where
  parseJSON = withScientific "InteractionType" $ \n -> case round n of
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


instance FromJSON ThreadMetadata


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


instance FromJSON Channel where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = \f -> if f == "type_" then "type" else f}


instance FromJSON DiscordInteraction where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = \f -> if f == "data_i" then "data" else if f == "interaction_type" then "type" else f}


-- Slash command data
data InteractionData = InteractionData
  { name :: Text
  , options :: Maybe [InteractionOption]
  }
  deriving (Generic, Show)


instance FromJSON InteractionData


data InteractionOption = InteractionOption
  { name :: Text
  , value :: Value
  }
  deriving (Generic, Show)


instance FromJSON InteractionOption


-- Outgoing response
data DiscordInteractionResponse
  = CommandResponse
      { response_type :: Int
      , data_ :: Maybe InteractionApplicationCommandCallbackData
      }
  | PingResponse
      { response_type :: Int
      }
  deriving (Generic, Show)


instance ToJSON DiscordInteractionResponse where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = \f -> if f == "data_" then "data" else if f == "response_type" then "type" else f}


data InteractionApplicationCommandCallbackData = InteractionApplicationCommandCallbackData
  { content :: Text
  }
  deriving (Generic, Show)


instance ToJSON InteractionApplicationCommandCallbackData


discordInteractionsH :: BS.ByteString -> Maybe BS.ByteString -> Maybe BS.ByteString -> ATBaseCtx AE.Value
discordInteractionsH rawBody signatureM timestampM = do
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  now <- Time.currentTime
  let envCfg = authCtx.env

  validateSignature envCfg signatureM timestampM rawBody

  -- Parse interaction
  interaction <- parseInteraction rawBody

  case interaction.interaction_type of
    Ping -> pure $ AE.object ["type" .= (1 :: Int)]
    ApplicationCommand -> handleApplicationCommand interaction envCfg now authCtx
  where
    validateSignature envCfg (Just sig) (Just tme) body
      | verifyDiscordSignature (encodeUtf8 envCfg.discordPublicKey) sig tme body = pure ()
      | otherwise = throwError err401{errBody = "Invalid signature"}
    validateSignature _ _ _ _ = throwError err401{errBody = "Invalid signature"}


parseInteraction :: BS.ByteString -> ATBaseCtx DiscordInteraction
parseInteraction rawBody = do
  case AE.decodeStrict' rawBody of
    Nothing -> throwError err401{errBody = "Invalid interaction data"}
    Just interaction -> pure interaction


handleApplicationCommand :: DiscordInteraction -> EnvConfig -> UTCTime -> AuthContext -> ATBaseCtx AE.Value
handleApplicationCommand interaction envCfg now authCtx = do
  cmdData <- case data_i interaction of
    Nothing -> throwError err400{errBody = "No command data provided"}
    Just cmd -> pure cmd

  discordData <- getDiscordData (fromMaybe "" interaction.guild_id)
  case discordData of
    Nothing -> pure $ contentResponse "Sorry, there was an error processing your request"
    Just d -> handleCommand cmdData interaction envCfg now authCtx d


handleCommand :: InteractionData -> DiscordInteraction -> EnvConfig -> UTCTime -> AuthContext -> DiscordData -> ATBaseCtx AE.Value
handleCommand cmdData interaction envCfg now authCtx discordData =
  case cmdData.name of
    "ask" -> handleAskCommand cmdData interaction envCfg now authCtx discordData
    "here" -> handleHereCommand interaction
    _ -> pure $ contentResponse "The command is not recognized"


handleAskCommand :: InteractionData -> DiscordInteraction -> EnvConfig -> UTCTime -> AuthContext -> DiscordData -> ATBaseCtx AE.Value
handleAskCommand cmdData interaction envCfg now authCtx discordData = do
  _ <- liftIO $ sendDeferredResponse interaction.id interaction.token envCfg.discordBotToken
  fullPrompt <- buildPrompt cmdData interaction envCfg
  result <- liftIO $ callOpenAIAPI fullPrompt envCfg.openaiApiKey
  case result of
    Left err -> do 
      _ <- liftIO $ sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken "Sorry, there was an error processing your request"
      pure $ AE.object[]
    Right (query, vizTypeM) -> do
      traceShowM query
      traceShowM vizTypeM
      case vizTypeM of
        Just vizType -> handleVisualization query vizType interaction envCfg now authCtx discordData
        Nothing -> do 
          _ <- liftIO $ sendJsonFollowupResponse envCfg.discordClientId interaction.token envCfg.discordBotToken ("Generated query: " <> query)
          pure $ AE.object[]

handleVisualization :: Text -> Text -> DiscordInteraction -> EnvConfig -> UTCTime -> AuthContext -> DiscordData -> ATBaseCtx AE.Value
handleVisualization query vizType interaction envCfg now authCtx discordData = do
  let widgetJson = createWidgetJson vizType discordData.projectId query
      chartType = Widget.mapWidgetTypeToChartType $ Widget.mapChatTypeToWidgetType vizType

  queryAST <- case parseQueryToAST query of
    Left err -> throwError err400{errBody = "Invalid query"}
    Right ast -> pure ast
  let sqlQueryComponents =
        (defSqlQueryCfg discordData.projectId now Nothing Nothing)
          { dateRange = (Nothing, Nothing)
          }
      (_, qc) = queryASTToComponents sqlQueryComponents queryAST
      sqlQuery = maybeToMonoid qc.finalSummarizeQuery

  metricData <- liftIO $ Chart.fetchMetricsData Chart.DTMetric sqlQuery now Nothing Nothing authCtx

  let reqBody = AE.object ["chartType".= chartType, "chartData" .= AE.toJSON metricData, "echartOptions" .= widgetJson]

  _ <- liftIO $ replyWithChartImage interaction reqBody envCfg.discordBotToken envCfg.discordClientId
  pure $ contentResponse "Generated query: "


handleHereCommand :: DiscordInteraction -> ATBaseCtx AE.Value
handleHereCommand interaction =
  case (interaction.channel_id, interaction.guild_id) of
    (Just channelId, Just guildId) -> do
      _ <- updateDiscordNotificationChannel guildId channelId
      pure $ contentResponse "Got it, notifications and alerts on your project will now be sent to this channel"
    _ -> pure $ contentResponse "No channel ID provided"


-- Helper functions
buildPrompt :: InteractionData -> DiscordInteraction -> EnvConfig -> ATBaseCtx Text
buildPrompt cmdData interaction envCfg = do
  threadMsgs <- liftIO $ getThreadStarterMessage interaction envCfg.discordBotToken
  pure $ case cmdData.options of
    Just (InteractionOption{value = String q} : _) -> case threadMsgs of
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
contentResponse msg = AE.object ["type" .= (4 :: Int), "data" .= AE.object ["content" .= msg]]


threadsPrompt :: [DiscordMessage] -> Text -> Text
threadsPrompt msgs question = prompt
  where
    msgs' = (\x -> "- @" <> x.author.username <> " :" <> x.content) <$> msgs
    threadPrompt =
      T.unlines
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
data DiscordUser = DiscordUser
  { username :: Text
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON)


data DiscordMessage = DiscordMessage
  { content :: Text
  , author :: DiscordUser
  , timestamp :: Text
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON)


getThreadStarterMessage :: DiscordInteraction -> Text -> IO (Maybe [DiscordMessage])
getThreadStarterMessage interaction botToken = do
  case interaction.channel_id of
    Just channelId -> case interaction.channel of
      Just Channel{type_ = 11, parent_id = Just pId} -> do
        let baseUrl = "https://discord.com/api/v10/channels/"
            url = T.unpack $ baseUrl <> channelId <> "/messages?limit=50"
            starterMessageUrl = T.unpack $ baseUrl <> pId <> "/messages/" <> channelId
            opts = defaults & authHeader botToken & contentTypeHeader "application/json"
        response <- getWith opts url
        response' <- getWith opts starterMessageUrl
        case eitherDecode (response ^. responseBody) of
          Left err -> do
            return Nothing
          Right messages -> do
            case eitherDecode (response' ^. responseBody) of
              Left err -> do
                return $ Just messages
              Right (message :: DiscordMessage) -> return $ Just (messages <> [message])
      _ -> pure Nothing
    Nothing -> pure $ Nothing


registerGlobalDiscordCommands :: T.Text -> T.Text -> IO (Either T.Text ())
registerGlobalDiscordCommands appId botToken = do
  let url =
        T.unpack
          $ "https://discord.com/api/v10/applications/"
            <> appId
            <> "/commands"

      askCommand =
        AE.object
          [ "name" .= ("ask" :: T.Text)
          , "description" .= ("Ask a question about your project using natural language" :: T.Text)
          , "type" .= 1
          , "options"
              .= ( AE.Array
                     $ V.fromList
                       [ AE.object
                           [ "name" .= ("quest" :: T.Text)
                           , "description" .= ("Your question in natural language" :: T.Text)
                           , "type" .= (3 :: Int) -- STRING
                           , "required" .= True
                           ]
                       ]
                 )
          ]

      queryCommand =
        AE.object
          [ "name" .= ("query" :: T.Text)
          , "description" .= ("query data on your project" :: T.Text)
          , "type" .= 1
          , "options"
              .= ( AE.Array
                     $ V.fromList
                       [ AE.object
                           [ "name" .= ("query" :: T.Text)
                           , "description" .= ("Enter query" :: T.Text)
                           , "type" .= 3
                           , "required" .= True
                           ]
                       ]
                 )
          ]
      hereCommand =
        AE.object
          [ "name" .= ("here" :: T.Text)
          , "description" .= ("Channel for apitoolkit to send notifications" :: T.Text)
          , "type" .= 1
          ]

      commandBody = AE.Array $ V.fromList [askCommand, queryCommand, hereCommand]

      opts = defaults & authHeader botToken & contentTypeHeader "application/json"

  result <- try $ putWith opts url (AE.encode commandBody) :: IO (Either SomeException (Response LBS.ByteString))

  case result of
    Left err -> pure $ Left (T.pack $ show err)
    Right resp ->
      if (resp ^. responseStatus . statusCode) `elem` [200, 201]
        then pure $ Right ()
        else pure $ Left $ TE.decodeUtf8 . LBS.toStrict $ resp ^. responseBody


sendDeferredResponse :: Text -> Text -> Text -> IO ()
sendDeferredResponse interactionId interactionToken botToken = do
  let url = T.unpack $ "https://discord.com/api/v10/interactions/" <> interactionId <> "/" <> interactionToken <> "/callback"
      payload = encode $ object ["type" .= (5 :: Int)]
  _ <- postWith (defaults & authHeader botToken & contentTypeHeader "application/json") url payload
  pure ()


authHeader :: Text -> Network.Wreq.Options -> Network.Wreq.Options
authHeader token = header "Authorization" .~ [TE.encodeUtf8 $ "Bot " <> token]


contentTypeHeader :: Text -> Network.Wreq.Options -> Network.Wreq.Options
contentTypeHeader contentType = header "Content-Type" .~ [TE.encodeUtf8 contentType]


data BufferResponse = BufferResponse
  { bufferType :: String
  , bufferData :: [Word8]
  }
  deriving (Generic, Show)


instance FromJSON BufferResponse where
  parseJSON = withObject "BufferResponse" $ \o ->
    BufferResponse
      <$> o
        .: "type"
      <*> o
        .: "data"


replyWithChartImage :: DiscordInteraction -> AE.Value -> Text -> Text -> IO ()
replyWithChartImage interaction chartOption botToken appId = do
  let interactionToken = interaction.token
  -- _ <- sendDeferredResponse interaction.id interaction.token botToken
  let chartshotUrl = "http://localhost:3001/generate-chart"
      chartReqPayload = encode chartOption

  chartResp <- post chartshotUrl chartReqPayload
  let responseBody' = chartResp ^. responseBody

  let followupUrl = T.unpack $ "https://discord.com/api/v10/webhooks/" <> appId <> "/" <> interactionToken
      payloadJson =
        encode
          $ object
            [ "content" .= ("Here's your chart!" :: T.Text)
            , "attachments"
                .= V.fromList
                  [ object
                      [ "id" .= (0 :: Int)
                      , "filename" .= ("chart.png" :: T.Text)
                      ]
                  ]
            ]
  case eitherDecode responseBody' :: Either String BufferResponse of
    Left s -> do
      _ <- postWith (defaults & authHeader botToken & contentTypeHeader "application/json") followupUrl payloadJson
      pure ()
    Right imageArray -> do
      let imageBytes = LBS.fromStrict $ BS.pack imageArray.bufferData
      let parts' :: [PartM IO]
          parts' =
            [ partLBS "payload_json" payloadJson
            , partFileRequestBody "files[0]" "chart.png" (RequestBodyLBS imageBytes) & partContentType .~ (Just "image/png")
            ]

      _ <- postWith (defaults & authHeader botToken & contentTypeHeader "multipart/form-data") followupUrl parts'
      pure ()


sendJsonFollowupResponse :: Text -> Text -> Text -> Text  -> IO ()
sendJsonFollowupResponse appId interactionToken botToken content = do
  let followupUrl = T.unpack $ "https://discord.com/api/v10/webhooks/" <> appId <> "/" <> interactionToken
      payload = AE.object["content" .= content]
  _ <- postWith (defaults & authHeader botToken & contentTypeHeader "application/json") followupUrl payload
  pure ()