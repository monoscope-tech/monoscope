{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pages.SlackInstall (linkProjectGetH, linkDiscordGetH, discordInteractionsH, slackActionsH, SlackActionForm, DiscordInteraction, SlackLink, externalOptionsH, slackInteractionsH, SlackInteraction) where

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
        Nothing -> pure $ addHeader "" $ BotLinked $ PageCtx bwconf "Slack"
    (_, _) -> pure $ addHeader ("/p/" <> pid.toText <> "/onboarding?step=NotifChannel") $ NoTokenFound $ PageCtx bwconf ()


data SlackLink
  = BotLinked (PageCtx Text)
  | NoTokenFound (PageCtx ())
  | DiscordError (PageCtx ())
  | NoContent (PageCtx ())


instance ToHtml SlackLink where
  toHtml (BotLinked (PageCtx bwconf bot)) = toHtml $ PageCtx bwconf $ installedSuccess bot
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


installedSuccess :: Text -> Html ()
installedSuccess botPlatform = do
  navBar
  section_ [class_ "min-h-screen  flex flex-col justify-center"] do
    div_ [class_ "max-w-4xl mx-auto px-6"] do
      div_ [class_ "bg-bgBase border border-strokeWeak rounded-3xl border border-fillWeak overflow-hidden"] do
        div_ [class_ "bg-gradient-to-r from-green-400 to-blue-500 px-8 py-10 text-center"] do
          div_ [class_ "inline-flex items-center justify-center w-16 h-16 bg-white rounded-full mb-4 shadow-lg"] do
            faSprite_ "check" "regular" "h-8 w-8 text-green-500"
          h1_ [class_ "text-3xl font-semibold text-white mb-4"] "Installation Complete!"
          p_ [class_ "text-blue-100 font-semibold max-w-2xl mx-auto"] $ toHtml $ "APItoolkit Bot has been successfully added to your " <> botPlatform <> " server"
        div_ [class_ "px-8 py-12"] do
          div_ [class_ "text-center mb-12"] do
            h2_ [class_ "font-semibold text-textStrong mb-4"] "You're All Set! 🚀"
            p_ [class_ "text-textWeak text-sm mx-auto max-w-2xl "] $ toHtml $ "Start receiving real-time alerts and interact with your API data directly from " <> botPlatform <> ". Your team can now stay on top of API performance without leaving your chat."
          div_ [class_ "max-w-3xl mx-auto"] do
            h3_ [class_ "font-semibold text-textStrong mb-8 text-center"] "Available Commands"
            div_ [class_ "grid gap-6 md:grid-cols-2"] do
              div_ [class_ "bg-gradient-to-br from-purple-50 to-blue-50 rounded-2xl p-6 border border-purple-100"] do
                div_ [class_ "flex items-start space-x-4"] do
                  div_ [class_ "flex-shrink-0"] do
                    div_ [class_ "inline-flex items-center justify-center w-12 h-12 bg-purple-500 rounded-xl"] do
                      span_ [class_ "text-white font-bold text-lg"] "?"
                  div_ [class_ "flex-1"] do
                    div_ [class_ "flex items-center space-x-2 mb-3"] do
                      span_ [class_ "font-mono bg-purple-100 text-purple-800 px-3 py-1 rounded-lg font-semibold"] "/ask"
                      span_ [class_ "bg-purple-500 text-white text-xs px-2 py-1 rounded-full"] "AI Powered"
                    p_ [class_ "text-textStrong text-sm"] $ toHtml $ "Ask questions about your API in natural language and get instant insights with logs and charts delivered right to " <> botPlatform <> "."
              div_ [class_ "bg-gradient-to-br from-green-50 to-teal-50 rounded-2xl p-6 border border-green-100"] do
                div_ [class_ "flex items-start space-x-4"] do
                  div_ [class_ "flex-shrink-0"] do
                    div_ [class_ "inline-flex items-center justify-center w-12 h-12 bg-green-500 rounded-xl"] do
                      faSprite_ "bell" "regular" "h-6 w-6 text-white"
                  div_ [class_ "flex-1"] do
                    div_ [class_ "flex items-center space-x-2 mb-3"] do
                      span_ [class_ "font-mono bg-green-100 text-green-800 px-3 py-1 rounded-lg font-semibold"] "/here"
                      span_ [class_ "bg-green-500 text-white text-xs px-2 py-1 rounded-full"] "Alerts"
                    p_ [class_ "text-textStrong text-sm"]
                      $ "Set up this channel to receive automated error reports, weekly summaries, and daily performance alerts."


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
                  content = getBotContent Discord question query query_url opts authCtx.env.chartShotUrl now
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


handleTableResponse :: BotType -> Either Text (V.Vector (V.Vector AE.Value), [Text], Int) -> EnvConfig -> Projects.ProjectId -> Text -> AE.Value
handleTableResponse target tableAsVecE envCfg projectId query =
  case tableAsVecE of
    Left err -> case target of
      Discord -> AE.object ["content" AE..= "Error processing query"]
      _ -> AE.object ["text" AE..= "Error processing query: "]
    Right tableAsVec -> do
      let (requestVecs, colNames, resultCount) = tableAsVec
          colIdxMap = listToIndexHashMap colNames
          tableData = recsVecToTableData requestVecs colIdxMap
          url' = envCfg.hostUrl <> "p/" <> projectId.toText <> "/log_explorer?query=" <> (decodeUtf8 $ urlEncode True $ encodeUtf8 query)
          explorerLink = "[Open in log explorer](" <> url' <> ")"
          content = "**Total events (" <> show resultCount <> ")**\n**Query used:** " <> query <> "\n\n" <> tableData <> "\n" <> explorerLink
       in case target of
            Discord -> AE.object ["content" AE..= content]
            _ ->
              AE.object
                [ "blocks"
                    AE..= AE.Array
                      ( V.fromList
                          [ AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= ("Total events (*" <> show resultCount <> "*)")]]
                          , AE.object ["type" AE..= "context", "elements" AE..= AE.Array (V.fromList [AE.object ["type" AE..= "mrkdwn", "text" AE..= ("*Query used:* " <> query)]])]
                          , AE.object ["type" AE..= "section", "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= tableData]]
                          , AE.object
                              [ "type" AE..= "actions"
                              , "elements" AE..= AE.Array (V.fromList [AE.object ["type" AE..= "button", "text" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= "🔍 View in log explorer", "emoji" AE..= True], "url" AE..= url']])
                              ]
                          ]
                      )
                , "response_type" AE..= "in_channel"
                , "replace_original" AE..= True
                , "delete_original" AE..= True
                ]


recsVecToTableData :: V.Vector (V.Vector AE.Value) -> (HashMap Text Int) -> Text
recsVecToTableData recsVec colIdxMap =
  formatSpans
    $ map
      ( \v ->
          TableData
            { timestamp = fromMaybe "" $ lookupVecTextByKey v colIdxMap "timestamp"
            , servicename = fromMaybe "" $ lookupVecTextByKey v colIdxMap "service"
            , spanname = fromMaybe "" $ lookupVecTextByKey v colIdxMap "span_name"
            , duration = toText $ getDurationNSMS $ fromIntegral $ lookupVecIntByKey v colIdxMap "duration"
            , hasErrors = lookupVecBoolByKey v colIdxMap "errors"
            }
      )
      (V.toList (V.take 20 recsVec))


padRight :: Int -> Text -> Text
padRight n s = T.take n (s <> T.replicate n " ")


formatSpanRow :: TableData -> Text
formatSpanRow spn = padRight 18 spn.timestamp <> " " <> padRight 15 spn.servicename <> " " <> padRight 20 spn.spanname <> " " <> padRight 8 spn.duration <> " " <> (if spn.hasErrors then "❌" else "✅")


formatSpans :: [TableData] -> Text
formatSpans spans =
  let hd = padRight 20 "TIME" <> " " <> padRight 15 "SERVICE" <> " " <> padRight 20 "SPAN NAME" <> " " <> padRight 8 "DURATION" <> " STATUS"
      rows = map formatSpanRow spans
   in "```\n" <> unlines (hd : rows) <> "```"


data TableData = TableData
  { timestamp :: Text
  , servicename :: Text
  , spanname :: Text
  , duration :: Text
  , hasErrors :: Bool
  }
  deriving (Generic, Show)


-- timestamp servicename spanname duration

-- Helper functions
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


threadsPrompt :: [DiscordMessage] -> Text -> Text
threadsPrompt msgs question = prompt
  where
    msgs' = (\x -> AE.object ["message" AE..= AE.object ["content" AE..= x.content, "embeds" AE..= x.embeds]]) <$> filter (\x -> x.author.username == "APItoolkit") msgs
    msgJson = decodeUtf8 $ AE.encode $ AE.Array (V.fromList msgs')
    threadPrompt =
      unlines
        $ [ "\n\nTHREADS:"
          , "- this query is  part of a conversation thread. Use previous messages provited in the thread for additional context if needed."
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


sendDeferredResponse :: HTTP :> es => Text -> Text -> Text -> Eff es ()
sendDeferredResponse interactionId interactionToken botToken = do
  let url = toString $ "https://discord.com/api/v10/interactions/" <> interactionId <> "/" <> interactionToken <> "/callback"
      payload = AE.encode $ AE.object ["type" AE..= 5]
  _ <- postWith (defaults & authHeader botToken & contentTypeHeader "application/json") url payload
  pass


authHeader :: Text -> Data.Effectful.Wreq.Options -> Data.Effectful.Wreq.Options
authHeader token = header "Authorization" .~ [encodeUtf8 $ "Bot " <> token]


contentTypeHeader :: Text -> Data.Effectful.Wreq.Options -> Data.Effectful.Wreq.Options
contentTypeHeader contentType = header "Content-Type" .~ [encodeUtf8 contentType]


sendJsonFollowupResponse :: HTTP :> es => Text -> Text -> Text -> AE.Value -> Eff es ()
sendJsonFollowupResponse appId interactionToken botToken content = do
  let followupUrl = toString $ "https://discord.com/api/v10/webhooks/" <> appId <> "/" <> interactionToken
  _ <- postWith (defaults & authHeader botToken & contentTypeHeader "application/json") followupUrl content
  pass


slackInteractionsH :: SlackInteraction -> ATBaseCtx AE.Value
slackInteractionsH interaction = do
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  case interaction.command of
    "/here" -> do
      _ <- updateSlackNotificationChannel interaction.team_id interaction.channel_id
      pure $ AE.object ["response_type" AE..= "in_channel", "text" AE..= "Done, you'll be receiving project notifcations here going forward", "replace_original" AE..= True, "delete_original" AE..= True]
    "/dashboard" -> do
      dashboards <- getDashboardsForSlack interaction.team_id
      triggerSlackModal authCtx.env.slackBotToken "open" $ (AE.object ["trigger_id" AE..= interaction.trigger_id, "view" AE..= (dashboardView interaction.channel_id $ V.fromList [(dashboardViewOne dashboards)])])
      pure $ AE.object ["text" AE..= "modal opened", "replace_original" AE..= True, "delete_original" AE..= True]
    _ -> do
      slackDataM <- dbtToEff $ getSlackDataByTeamId interaction.team_id
      void $ forkIO $ do
        case slackDataM of
          Nothing -> sendSlackFollowupResponse interaction.response_url (AE.object ["text" AE..= "Error: something went wrong"])
          Just slackData -> handleAskCommand interaction slackData authCtx
      pure $ AE.object ["text" AE..= "apitoolkit is working...", "replace_original" AE..= True, "delete_original" AE..= True]
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
        Right (query, vizTypeM) -> do
          case vizTypeM of
            Just vizType -> do
              let chartType = Widget.mapWidgetTypeToChartType $ Widget.mapChatTypeToWidgetType vizType
                  opts = "&q=" <> (decodeUtf8 $ urlEncode True (encodeUtf8 query)) <> "&p=" <> slackData.projectId.toText <> "&t=" <> chartType
                  query_url = authCtx.env.hostUrl <> "p/" <> slackData.projectId.toText <> "/log_explorer?viz_type=" <> chartType <> "&query=" <> (decodeUtf8 $ urlEncode True (encodeUtf8 query))
                  content = getBotContent Slack question query query_url opts authCtx.env.chartShotUrl now

              _ <- sendSlackFollowupResponse inter.response_url content
              pass
            Nothing -> do
              let queryAST = parseQueryToAST query
              case queryAST of
                Left err -> sendSlackFollowupResponse inter.response_url (AE.object ["text" AE..= "Error: something went wrong"])
                Right query' -> do
                  tableAsVecE <- RequestDumps.selectLogTable slackData.projectId query' query Nothing (Nothing, Nothing) [] Nothing Nothing
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
      throwError err400{errBody = "Invalid action payload: " <> encodeUtf8 (T.pack err)}
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
                        , AE.object ["type" AE..= "context", "elements" AE..= AE.Array (V.singleton $ AE.object ["type" AE..= "plain_text", "text" AE..= ("Shared by @" <> slackAction.user.username <> " using /dashboard")])]
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
          let query = w.query >>= (\x -> Just $ "&q=" <> decodeUtf8 (urlEncode True $ encodeUtf8 x))
              sql = w.sql >>= (\x -> Just $ "&sql=" <> decodeUtf8 (urlEncode True $ encodeUtf8 x))
              query' = fromMaybe (fromMaybe "" sql) query
              vizType = Widget.mapWidgetTypeToChartType w.wType
              chartUrl' = chartImageUrl (query' <> "&p=" <> pid <> "&t=" <> vizType) authCtx.env.chartShotUrl now
              blocks = V.fromList [dashboardViewOne widgets, dashboardViewTwo widgets, dashboardWidgetView chartUrl' widgetTitle]
              privateMeta = channelId <> "___" <> pid <> "___" <> baseTemplate <> "___" <> chartUrl'
          _ <- triggerSlackModal authCtx.env.slackBotToken "update" $ AE.object ["view_id" AE..= slackAction.view.id, "view" AE..= dashboardView privateMeta blocks]
          pass
      handleUnknownActionType

    handleUnknownActionType = pure $ AE.object []


lookupSelectedValueByKey :: Text -> AE.Value -> Maybe Text
lookupSelectedValueByKey key' val = parseMaybe parser val
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
  _ <- postWith (defaults & header "Authorization" .~ [encodeUtf8 $ "Bearer " <> token] & contentTypeHeader "application/json") url content
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


data BotType = Slack | Discord


getBotContent :: BotType -> Text -> Text -> Text -> Text -> Text -> Time.UTCTime -> AE.Value
getBotContent target question query query_url chartOptions baseUrl now =
  case target of
    Slack ->
      AE.object
        [ "attachments"
            AE..= AE.Array
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
        , "response_type" AE..= "in_channel"
        , "replace_original" AE..= True
        , "delete_original" AE..= True
        ]
    _ ->
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
    , "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= "*Dashboard*"]
    , "accessory"
        AE..= AE.object
          [ "action_id" AE..= "dashboard-select"
          , "type" AE..= "static_select"
          , "placeholder" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= "Select a dashboard template"]
          , "options" AE..= AE.Array (opts)
          ]
    ]
  where
    opts = V.map (\(text, value) -> AE.object ["text" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= text], "value" AE..= value]) options


dashboardViewTwo :: V.Vector (Text, Text) -> AE.Value
dashboardViewTwo options =
  AE.object
    [ "type" AE..= "section"
    , "block_id" AE..= "widget-select"
    , "text" AE..= AE.object ["type" AE..= "mrkdwn", "text" AE..= "*Widget*"]
    , "accessory"
        AE..= AE.object
          [ "action_id" AE..= "widget-select"
          , "type" AE..= "static_select"
          , "placeholder" AE..= AE.object ["type" AE..= "plain_text", "text" AE..= "Select a dashboard template"]
          , "options" AE..= AE.Array (opts)
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
  _ <- postWith opts (toString url) content
  pass
