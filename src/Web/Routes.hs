module Web.Routes (server, genAuthServerContext, KeepPrefixExp, LogDataFieldExp, widgetPngGetH, widgetGetH, chartsDataGetH, ApiV1Routes, apiV1Server, apiV1OpenApiSpec, mcpProjectTools) where

import Control.Lens
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Default (def)
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.Wreq qualified as Wreq
import Data.IP qualified as IP
import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.OpenApi (OpenApi)
import Data.Ord (clamp)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.UUID qualified as UUID
import Deriving.Aeson.Stock qualified as DAE
import Effectful (Eff)
import Effectful qualified as Eff
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static (ask)
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Local qualified as State
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Log (Logger)
import Lucid
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogQueries qualified as LogQueries
import Models.Projects.ImpactReviews qualified as ImpactReviews
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types qualified as H
import Network.Wai (Request)
import Network.Wai qualified as Wai
import Pages.AIThreads qualified as AIThreads
import Pages.BodyWrapper (NavigationResponse, PageCtx)
import Pages.Bots.Discord qualified as Discord
import Pages.Bots.Slack qualified as Slack
import Pages.Bots.Utils qualified as BotUtils
import Pages.Bots.Whatsapp qualified as Whatsapp
import Pages.Charts.Charts qualified as Charts
import Pages.CodeContext qualified as PageCodeContext
import Pages.CommandPalette qualified as CommandPalette
import Pages.Containers qualified as Containers
import Pages.Dashboards qualified as Dashboards
import Pages.Endpoints qualified as ApiCatalog
import Pages.GitSync qualified as GitSync
import Pages.Infrastructure qualified as Infrastructure
import Pages.Issues qualified as IssuesPage
import Pages.LogExplorer.LiveTail qualified as LiveTail
import Pages.LogExplorer.Log qualified as Log
import Pages.LogExplorer.LogItem qualified as LogItem
import Pages.LogExplorer.QueryLibrary qualified as QueryLibrary
import Pages.Monitors qualified as Testing
import Pages.Onboarding qualified as Onboarding
import Pages.Projects qualified as CreateProject
import Pages.Projects qualified as Integrations
import Pages.Projects qualified as ListProjects
import Pages.Projects qualified as ManageMembers
import Pages.Replay qualified as Replay
import Pages.Reports qualified as Reports
import Pages.ServiceMap qualified as ServiceMap
import Pages.Settings qualified as Settings
import Pages.Share qualified as Share
import Pages.Telemetry qualified as Metrics
import Pages.Telemetry qualified as Trace
import Pkg.AI qualified as AI
import Pkg.Components.Table qualified as Table
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.EmailTemplates qualified as ET
import Pkg.Git qualified as Git
import Pkg.Parser qualified as Parser
import Relude hiding (ask)
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.QueryParam.Server.Record ()
import Servant.Server.Generic (AsServerT, genericServeTWithContext)
import System.Config (AuthContext (..), DeploymentEnv (..), EnvConfig (..))
import System.Exit (ExitCode (..))
import System.Logging qualified as Log
import System.Process.Typed (byteStringInput, proc, readProcess, setStdin)
import System.Timeout (timeout)
import System.Types (ATAuthCtx, ATBaseCtx, ApiPrincipal (..), HXRedirectDest, HXReswap (..), Mime, ProjectToolCtx, RespHeaders, TriggerEvents, XWidgetJSON, addRespHeaders, atAuthToBase)
import UnliftIO (withRunInIO)
import UnliftIO.Exception (handle)
import Utils qualified
import Web.ApiHandlers (ApiV1Routes (..), apiV1OpenApiSpec)
import Web.ApiHandlers qualified as ApiH
import Web.ApiTypes (ApiActor (..))
import Web.ApiTypes qualified as ApiT
import Web.Auth (APItoolkitAuthContext, ApiKeyAuthContext, apiKeyAuthHandler, authHandler, servantRejection)
import Web.Auth qualified as Auth
import Web.Cookie (SetCookie)
import Web.Links
import Web.MCP qualified as MCP
import "base16-bytestring" Data.ByteString.Base16 qualified as B16
import "cryptohash-md5" Crypto.Hash.MD5 qualified as MD5


data RawJSON
  deriving (Accept) via Mime "application/json"


instance MimeUnrender RawJSON BS.ByteString where
  mimeUnrender _ = Right . toStrict


-- | Preserve Slack's form bytes until the handler verifies their signature.
data RawForm
  deriving (Accept) via FormUrlEncoded


instance MimeUnrender RawForm BS.ByteString where
  mimeUnrender _ = Right . toStrict


-- | OTLP/HTTP wire format (raw protobuf bytes; decoding happens in the handler).
data OTLPProto
  deriving (Accept) via Mime "application/x-protobuf"


instance MimeUnrender OTLPProto BS.ByteString where
  mimeUnrender _ = Right . toStrict


instance MimeRender OTLPProto BS.ByteString where
  mimeRender _ = fromStrict


-- | OTLP/HTTP export action, injected from System.Server. Kept abstract here:
-- importing Opentelemetry.OtlpServer would drag proto-lens's orphan IsLabel
-- instances into scope and break the generic-lens #labels used by widget code.
-- Its second argument is the sender's address: a browser exporting directly is the visitor.
type OtlpHttpHandler = Maybe Text -> Maybe Text -> BS.ByteString -> IO (Either Text BS.ByteString)


-- | Run an OTLP/HTTP export handler; protobuf decode failures surface as 400.
otlpHttpH :: OtlpHttpHandler -> Maybe Text -> Maybe Text -> BS.ByteString -> ATBaseCtx BS.ByteString
otlpHttpH export keyM sender body =
  liftIO (export keyM sender body) >>= either (\e -> Error.throwError err400{errBody = fromStrict (encodeUtf8 e)}) pure


instance Servant.MimeRender JSON ByteString where
  mimeRender _ = fromStrict


type Routes :: Type -> Type
data Routes mode = Routes
  { public :: mode :- "public" :> Servant.Raw
  , cookieProtected :: mode :- AuthProtect "optional-cookie-auth" :> Servant.NamedRoutes CookieProtectedRoutes
  , ping :: mode :- "ping" :> Get '[PlainText] Text
  , status :: mode :- "status" :> Get '[JSON] Status
  , login :: mode :- "login" :> QPT "redirect_to" :> QPT "screen_hint" :> QPT "login_hint" :> SessionRedirect NoContent
  , toLogin :: mode :- "to_login" :> QPT "redirect_to" :> SessionRedirect NoContent
  , logout :: mode :- "logout" :> SessionRedirect NoContent
  , setLanguage :: mode :- "set_language" :> Capture "lang" Text :> QPT "redirect_to" :> SessionRedirect NoContent
  , authCallback :: mode :- "auth_callback" :> QPT "code" :> QPT "state" :> QPT "redirect_to" :> Header "Cookie" ByteString :> Header "User-Agent" ByteString :> Header "CF-IPCountry" ByteString :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] (Html ()))
  , -- OTLP/HTTP ingestion (protobuf) — collector-less SDK exporters
    otlpTracesPost :: mode :- "v1" :> "traces" :> Header "x-api-key" Text :> Header "X-Real-IP" Text :> RemoteHost :> ReqBody '[OTLPProto] BS.ByteString :> Post '[OTLPProto] BS.ByteString
  , otlpLogsPost :: mode :- "v1" :> "logs" :> Header "x-api-key" Text :> Header "X-Real-IP" Text :> RemoteHost :> ReqBody '[OTLPProto] BS.ByteString :> Post '[OTLPProto] BS.ByteString
  , shareLinkGet :: mode :- "share" :> "r" :> Capture "shareID" UUID.UUID :> Get '[HTML] Share.ShareLinkGet
  , shareReplaySessionGet :: mode :- "share" :> "r" :> Capture "shareID" UUID.UUID :> "replay_session" :> Capture "sessionId" UUID.UUID :> Get '[JSON] Replay.ReplaySessionResp
  , discordLinkProjectGet :: mode :- "discord" :> "oauth" :> "callback" :> QPT "state" :> QPT "code" :> QPT "guild_id" :> LocationRedirect BotUtils.BotResponse
  , discordInteractions :: mode :- "discord" :> "interactions" :> ReqBody '[RawJSON] BS.ByteString :> Header "X-Signature-Ed25519" BS.ByteString :> Header "X-Signature-Timestamp" BS.ByteString :> Post '[JSON] AE.Value
  , slackInteractions :: mode :- "interactions" :> "slack" :> ReqBody '[RawForm] BS.ByteString :> Header "X-Slack-Request-Timestamp" Text :> Header "X-Slack-Signature" Text :> Post '[JSON] AE.Value
  , slackActionsPost :: mode :- "actions" :> "slack" :> ReqBody '[RawForm] BS.ByteString :> Header "X-Slack-Request-Timestamp" Text :> Header "X-Slack-Signature" Text :> Post '[JSON] AE.Value
  , slackEventsPost :: mode :- "slack" :> "events" :> ReqBody '[RawJSON] BS.ByteString :> Header "X-Slack-Request-Timestamp" Text :> Header "X-Slack-Signature" Text :> Post '[JSON] AE.Value
  , externalOptionsGet :: mode :- "interactions" :> "external_options" :> ReqBody '[RawForm] BS.ByteString :> Header "X-Slack-Request-Timestamp" Text :> Header "X-Slack-Signature" Text :> Post '[JSON] AE.Value
  , whatsappIncomingPost :: mode :- "whatsapp" :> "incoming" :> ReqBody '[FormUrlEncoded] Whatsapp.TwilioWhatsAppMessage :> Post '[JSON] AE.Value
  , clientMetadata :: mode :- "api" :> "client_metadata" :> Header "Authorization" Text :> Get '[JSON] Auth.ClientMetadata
  , lemonWebhook :: mode :- "webhook" :> "lemon-squeezy" :> Header "X-Signature" Text :> ReqBody '[RawJSON] BS.ByteString :> Post '[HTML] (Html ())
  , stripeWebhook :: mode :- "webhook" :> "stripe" :> Header "Stripe-Signature" Text :> ReqBody '[RawJSON] BS.ByteString :> Post '[HTML] (Html ())
  , -- Kept at its original path so hooks configured before multi-host support keep delivering.
    githubWebhook :: mode :- "webhook" :> "github" :> Header "X-Hub-Signature-256" Text :> Header "X-GitHub-Event" Text :> ReqBody '[RawJSON] BS.ByteString :> Post '[JSON] AE.Value
  , -- One route per host. The host is in the path rather than sniffed from the headers because
    -- it decides which signature scheme to verify with, and that must not be attacker-chosen.
    -- Every host's signature and event headers are captured; each arm reads only its own.
    gitWebhook
      :: mode
        :- "webhook"
          :> "git"
          :> Capture "host" Git.GitHost
          :> Header "X-Hub-Signature" Text -- Bitbucket
          :> Header "X-Gitea-Signature" Text
          :> Header "webhook-signature" Text -- GitLab 19.1+
          :> Header "X-Gitlab-Token" Text -- GitLab, pre-19.1
          :> Header "webhook-id" Text
          :> Header "webhook-timestamp" Text
          :> Header "X-Gitea-Event" Text
          :> Header "X-Gitlab-Event" Text
          :> Header "X-Event-Key" Text -- Bitbucket
          :> ReqBody '[RawJSON] BS.ByteString
          :> Post '[JSON] AE.Value
  , avatarGet :: mode :- "api" :> "avatar" :> Capture "user_id" Projects.UserId :> RawM
  , widgetPngGet :: mode :- WidgetPngGet
  , proofMediaGet :: mode :- ProofMediaGet
  , proxyLanding :: mode :- "proxy" :> CaptureAll "path" Text :> Get '[HTML] (RespHeaders Onboarding.ProxiedPage)
  , deviceCode :: mode :- "api" :> "device" :> "code" :> Post '[JSON] Auth.DeviceCodeResponse
  , deviceToken :: mode :- "api" :> "device" :> "token" :> QPT "device_code" :> Post '[JSON] Auth.DeviceTokenResponse
  , emailPreviewList :: mode :- "dev" :> "emails" :> Get '[HTML] (Html ())
  , emailPreview :: mode :- "dev" :> "emails" :> Capture "template" Text :> Get '[HTML] (Html ())
  , apiV1 :: mode :- "api" :> "v1" :> AuthProtect "api-key-auth" :> NamedRoutes ApiV1Routes
  , apiV1OpenApi :: mode :- "api" :> "v1" :> "openapi.json" :> Get '[JSON] OpenApi
  }
  deriving stock (Generic)


server :: OtlpHttpHandler -> OtlpHttpHandler -> Routes (AsServerT ATBaseCtx)
server otlpTraces otlpLogs =
  let projectTools = Just mcpProjectTools
      -- The proxy's X-Real-IP (nginx overwrites whatever a client sent), else the connecting socket.
      sender realIp peer = realIp <|> show . fst <$> IP.fromSockAddr peer
   in Routes
        { public = Servant.serveDirectoryWebApp "./static/public"
        , ping = pure "pong"
        , status = statusH
        , login = Auth.loginH
        , toLogin = Auth.loginRedirectH
        , logout = Auth.logoutH
        , setLanguage = Auth.setLanguageH
        , authCallback = Auth.authCallbackH
        , otlpTracesPost = \key realIp -> otlpHttpH otlpTraces key . sender realIp
        , otlpLogsPost = \key realIp -> otlpHttpH otlpLogs key . sender realIp
        , shareLinkGet = Share.shareLinkGetH
        , shareReplaySessionGet = Share.shareReplaySessionGetH
        , discordLinkProjectGet = Discord.linkDiscordGetH
        , discordInteractions = Discord.discordInteractionsH projectTools
        , slackInteractions = Slack.slackFormPostH $ Slack.slackInteractionsHWithTools projectTools
        , slackActionsPost = Slack.slackFormPostH Slack.slackActionsH
        , slackEventsPost = Slack.slackEventsPostH
        , externalOptionsGet = Slack.slackFormPostH Slack.externalOptionsH
        , whatsappIncomingPost = Whatsapp.whatsappIncomingPostH projectTools
        , clientMetadata = Auth.clientMetadataH
        , lemonWebhook = Settings.webhookPostH
        , stripeWebhook = Settings.stripeWebhookPostH
        , githubWebhook = \sigM eventM body -> GitSync.gitWebhookPostH Git.GitHub (Git.WebhookReq eventM sigM Nothing Nothing Nothing body)
        , gitWebhook = \host bbSig giteaSig glSig glToken glId glTs giteaEvent glEvent bbEvent body ->
            GitSync.gitWebhookPostH
              host
              Git.WebhookReq
                { event = asum [giteaEvent, glEvent, bbEvent]
                , signature = asum [giteaSig, glSig, bbSig]
                , gitlabToken = glToken
                , gitlabWebhookId = glId
                , gitlabTimestamp = glTs
                , body = body
                }
        , avatarGet = avatarGetH
        , widgetPngGet = widgetPngGetH
        , proofMediaGet = proofMediaGetH
        , proxyLanding = \path ->
            Onboarding.proxyLandingH path
              & State.evalState @TriggerEvents mempty
              & State.evalState @HXRedirectDest Nothing
              & State.evalState @XWidgetJSON Nothing
              & State.evalState (HXReswap Nothing)
        , deviceCode = Auth.deviceCodeH
        , deviceToken = Auth.deviceTokenH
        , emailPreviewList = emailPreviewListH
        , emailPreview = emailPreviewH
        , apiV1 = apiV1Server
        , apiV1OpenApi = pure apiV1OpenApiSpec
        , cookieProtected = \sessionWithCookies ->
            Servant.hoistServerWithContext
              (Proxy @(Servant.NamedRoutes CookieProtectedRoutes))
              (Proxy @'[APItoolkitAuthContext])
              (atAuthToBase sessionWithCookies)
              (cookieProtectedServer projectTools)
        }


-- API v1 server
apiV1Server :: ApiPrincipal -> Servant.ServerT (NamedRoutes ApiV1Routes) ATBaseCtx
apiV1Server = apiV1ServerAs ApiKeyPrincipal


apiV1ServerAs :: ApiActor -> ApiPrincipal -> Servant.ServerT (NamedRoutes ApiV1Routes) ATBaseCtx
apiV1ServerAs actor principal = ApiH.apiV1Server actor principal Log.queryEvents Dashboards.apiDashboardData \request ->
  withRunInIO \run -> run $ MCP.handleJsonRpc mcpToolRegistry (apiV1App run actor principal) principal.projectId request


apiV1App :: ProjectToolCtx es => (forall a. Eff es a -> IO a) -> ApiActor -> ApiPrincipal -> Servant.Application
apiV1App run actor principal =
  genericServeTWithContext
    (Handler . ExceptT . run . Error.runErrorNoCallStack @ServerError . Eff.inject)
    (apiV1ServerAs actor principal)
    Servant.EmptyContext


-- | MCP tool registry — REST tools (from OpenAPI) + composite workflow tools,
-- built once at startup.
{-# NOINLINE mcpToolRegistry #-}
mcpToolRegistry :: Map Text (MCP.Tool MCP.Dispatch)
mcpToolRegistry = MCP.allTools apiV1OpenApiSpec $ \pid tz input -> do
  authCtx <- Reader.ask @AuthContext
  AI.runNlSearch pid authCtx.env tz input


mcpProjectTools :: AI.ProjectTools
mcpProjectTools =
  AI.ProjectTools
    { AI.catalog = mcpToolRegistry
    , AI.invoke = \principal pid name args ->
        case Map.lookup name mcpToolRegistry of
          Nothing -> pure $ MCP.toolError $ "Unknown tool: " <> name
          Just tool -> withRunInIO \run ->
            run
              $ Error.runErrorNoCallStack @ServerError (Eff.inject $ MCP.runTool (apiV1App run principal (Authenticated pid)) pid tool $ KM.fromMapText args)
              <&> either (MCP.toolError . MCP.problemText . errBody) id
    }


-- Cookie Protected server
cookieProtectedServer :: Maybe AI.ProjectTools -> Servant.ServerT (Servant.NamedRoutes CookieProtectedRoutes) ATAuthCtx
cookieProtectedServer projectTools =
  CookieProtectedRoutes
    { -- Dashboard handlers
      dashboardRedirectGet = Dashboards.entrypointRedirectGetH "_overview.yaml" ["overview", "http", "logs", "traces", "events"]
    , slackInstallGet = Slack.startInstallGetH
    , slackEvidenceGet = Slack.startEvidenceGetH
    , slackEvidenceDisconnectPost = Slack.disconnectEvidencePostH
    , slackIdentityGet = Slack.linkIdentityGetH
    , slackIdentityPost = Slack.linkIdentityPostH
    , slackLinkProjectGet = Slack.linkProjectGetH
    , endpointDetailsRedirect = Dashboards.entrypointRedirectGetH "endpoint-stats.yaml" ["endpoints", "http", "events"]
    , rumRedirect = Dashboards.rumRedirectH
    , dashboardViewPost = Dashboards.dashboardViewPostH
    , dashboardViewDelete = Dashboards.dashboardViewDeleteH
    , dashboardViewsGet = Dashboards.dashboardViewsGetH
    , dashboardsGet = Dashboards.dashboardGetH
    , dashboardNewGet = Dashboards.dashboardNewGetH
    , dashboardsGetList = Dashboards.dashboardsGetH
    , dashboardsPost = Dashboards.dashboardsPostH
    , dashboardSettledPost = Dashboards.dashboardSettledPostH
    , dashboardRepositoryGet = GitSync.dashboardRepositoryGetH
    , dashboardRepositoryPost = GitSync.dashboardRepositoryPostH
    , dashboardWidgetPut = Dashboards.dashboardWidgetPutWithMonitorH
    , dashboardWidgetReorderPatchH = Dashboards.dashboardWidgetReorderPatchH
    , dashboardDelete = Dashboards.dashboardDeleteH
    , dashboardRenamePatch = Dashboards.dashboardRenamePatchH
    , dashboardDuplicatePost = Dashboards.dashboardDuplicatePostH
    , dashboardStarPost = Dashboards.dashboardStarPostH
    , dashboardDuplicateWidget = Dashboards.dashboardDuplicateWidgetPostH
    , dashboardWidgetNewGet = Dashboards.dashboardWidgetNewGetH
    , dashboardWidgetExpandGet = Dashboards.dashboardWidgetExpandGetH
    , widgetAlertUpsert = Dashboards.widgetAlertUpsertH
    , widgetAlertDelete = Dashboards.widgetAlertDeleteH
    , widgetAlertToggle = Dashboards.widgetAlertToggleH
    , dashboardBulkActionPost = Dashboards.dashboardBulkActionPostH
    , dashboardTabGet = Dashboards.dashboardTabGetH
    , dashboardTabRenamePatch = Dashboards.dashboardTabRenamePatchH
    , dashboardYamlGet = Dashboards.dashboardYamlGetH
    , dashboardSwitcherGet = Dashboards.dashboardSwitcherGetH
    , dashboardYamlPut = Dashboards.dashboardYamlPutH
    , -- API handlers
      apiGet = Settings.apiGetH
    , apiDelete = Settings.apiDeleteH
    , apiPatch = Settings.apiActivateH
    , apiPost = Settings.apiPostH
    , -- Chart and widget handlers
      chartsDataGet = chartsDataGetH
    , chartsDataStreamGet = chartsDataStreamGetH
    , widgetPost = Widget.widgetPostH
    , widgetGet = widgetGetH
    , widgetSqlPreview = Dashboards.widgetSqlPreviewGetH
    , widgetSqlText = Dashboards.widgetSqlTextGetH
    , -- Slack/Discord handlers
      reportsGet = Reports.reportsGetH
    , reportsLiveGet = Reports.reportsLiveGetH
    , reportsSingleGet = Reports.singleReportGetH
    , reportsPost = Reports.reportsPostH
    , shareLinkPost = Share.shareLinkPostH
    , replaySessionGet = Replay.replaySessionGetH
    , replaySessionManifestGet = Replay.replaySessionManifestGetH
    , replaySessionShardGet = Replay.replaySessionShardGetH
    , bringS3 = Settings.bringS3GetH
    , bringS3Post = Settings.brings3PostH
    , bringS3Remove = Settings.brings3RemoveH
    , gitSyncSettings = \pid -> PageCodeContext.repositoriesGetH pid (Just PageCodeContext.Overview) Nothing
    , projectWikiGet = PageCodeContext.wikiGetH
    , projectMemoryGet = PageCodeContext.memoryGetH
    , projectMemoryPost = PageCodeContext.memoryPostH
    , repositoriesGet = PageCodeContext.repositoriesGetH
    , serviceRepositoriesGet = PageCodeContext.serviceRepositoriesGetH
    , repositoryConnectGet = PageCodeContext.repositoryConnectGetH
    , repositoryConnectPost = PageCodeContext.repositoryConnectPostH
    , repositoryTokenGet = PageCodeContext.repositoryTokenGetH
    , repositoryTokenPost = PageCodeContext.repositoryTokenPostH
    , repositoryGet = PageCodeContext.repositoryGetH
    , repositoryDelete = PageCodeContext.repositoryDeleteH
    , repositoryDashboardGet = GitSync.repositoryDashboardGetH
    , repositoryDashboardPost = GitSync.repositoryDashboardPostH
    , repositoryAnalyzePost = PageCodeContext.repositoryAnalyzePostH
    , repositoryResumePost = PageCodeContext.repositoryResumePostH
    , repositoryFolderPost = PageCodeContext.repositoryFolderPostH
    , repositoryConflictPost = PageCodeContext.repositoryConflictPostH
    , repositorySourceEnabledPost = PageCodeContext.repositorySourceEnabledPostH
    , repositorySourceGet = PageCodeContext.repositorySourceGetH
    , repositorySourcePost = PageCodeContext.repositorySourcePostH
    , repositorySourceDelete = PageCodeContext.repositorySourceDeleteH
    , repositoryReviewsPost = PageCodeContext.repositoryReviewsPostH
    , gitSyncSettingsPost = GitSync.gitSyncSettingsPostH
    , gitSyncSettingsDelete = GitSync.gitSyncSettingsDeleteH
    , gitSyncRepositoryUpdate = GitSync.gitSyncSettingsUpdateH
    , gitSyncRepositoryRetry = GitSync.gitSyncRepositoryRetryH
    , gitSyncRepositoryPause = GitSync.gitSyncRepositoryPauseH
    , gitSyncRepositoryDelete = GitSync.gitSyncRepositoryDeleteH
    , codeMappingsSettings = \pid -> PageCodeContext.repositoriesGetH pid (Just PageCodeContext.Overview)
    , codeMappingsEditor = PageCodeContext.codeMappingsEditorGetH
    , codeMappingsPost = PageCodeContext.codeMappingsPostH
    , impactReviewSettingsPost = PageCodeContext.impactReviewSettingsPostH
    , impactReviewRetry = PageCodeContext.impactReviewRetryH
    , issueFixGet = PageCodeContext.issueFixGetH
    , issueFixMedia = PageCodeContext.issueFixMediaH
    , issueFixPackage = PageCodeContext.issueFixPackageH
    , codeMappingsDelete = PageCodeContext.codeMappingsDeleteH
    , prometheusSettingsGet = Settings.prometheusGetH
    , prometheusSettingsPost = Settings.prometheusPostH
    , prometheusSettingsTest = Settings.prometheusTestH
    , prometheusSettingsEdit = Settings.prometheusUpdateH
    , prometheusSettingsToggle = Settings.prometheusToggleH
    , prometheusSettingsDelete = Settings.prometheusDeleteH
    , githubAppInstall = GitSync.githubAppInstallH
    , githubAppCallback = GitSync.githubAppCallbackH
    , githubAppRepos = GitSync.githubAppReposH
    , githubAppSelectRepo = GitSync.githubAppSelectRepoH
    , -- Billing handlers
      manageBillingGet = Settings.manageBillingGetH
    , -- Endpoint handlers
      endpointListGet = ApiCatalog.endpointListGetH
    , apiCatalogGet = ApiCatalog.apiCatalogH
    , apiCatalogBulkAction = ApiCatalog.apiCatalogBulkActionH
    , apiCatalogDocs = ApiCatalog.apiDocsH
    , apiCatalogSpecJson = ApiCatalog.apiSpecJsonH
    , apiCatalogSpecYaml = ApiCatalog.apiSpecYamlH
    , -- Command palette
      commandPaletteGet = CommandPalette.commandPaletteItemsH
    , commandPaletteRecentPost = CommandPalette.commandPaletteRecentPostH
    , aiRoutinesGet = AIThreads.routinesGetH
    , aiRoutineInstallPost = AIThreads.routineInstallPostH
    , aiThreadsGet = AIThreads.threadsGetH
    , aiOperationGet = AIThreads.operationGetH
    , aiOperationReviewPost = AIThreads.operationReviewPostH
    , aiRoutineRunGet = AIThreads.routineRunGetH
    , aiRoutineInstructionsPost = AIThreads.routineInstructionsPostH
    , aiThreadGet = AIThreads.threadGetH
    , aiThreadsPost = AIThreads.startThreadPostHWithTools projectTools
    , aiThreadPost = AIThreads.threadPostHWithTools projectTools
    , aiThreadTitlePost = AIThreads.threadTitlePostH
    , aiThreadDelete = AIThreads.threadDeleteH
    , aiRoutinePost = AIThreads.routinePostH
    , aiRoutinePausePost = AIThreads.routinePausePostH
    , aiRoutineCancelPost = AIThreads.routineCancelPostH
    , aiRoutineResumePost = AIThreads.routineResumePostH
    , aiRoutinePolicyPost = AIThreads.routinePolicyPostH
    , aiRoutineDestinationPost = AIThreads.routineDestinationPostH
    , aiRoutineDelete = AIThreads.routineDeleteH
    , -- Device auth
      deviceApprove = Auth.deviceApproveH False
    , deviceApprovePost = Auth.deviceApproveH True
    , -- Sub-route handlers
      projects = projectsServer
    , logExplorer = logExplorerServer
    , issues = issuesServer projectTools
    , monitors = monitorsServer
    , traces = telemetryServer
    }


logExplorerServer :: Projects.ProjectId -> Servant.ServerT LogExplorerRoutes ATAuthCtx
logExplorerServer pid =
  LogExplorerRoutes'
    { logExplorerGet = Log.apiLogH pid
    , -- 'LogQueries.BrowserParams' is only echoed back to the browser; the handlers read scope from the session.
      logExplorerDataGet = const $ Log.logExplorerDataH pid
    , logExplorerPatternsGet = const $ Log.logPatternsH pid
    , logExplorerSessionsGet = const $ Log.logSessionsH pid
    , logExplorerSchemaGet = Log.logExplorerSchemaH pid
    , logExplorerValidateGet = Log.logExplorerValidateH pid
    , logExplorerFacetsGet = Log.logExplorerFacetsH pid
    , queryLibraryGet = QueryLibrary.queryLibraryH pid
    , saveQueryPost = QueryLibrary.saveQueryH pid
    , deleteQueryPost = QueryLibrary.deleteQueryH pid
    , alertFormGet = Log.alertFormH pid
    , logExplorerItemDetailedGet = LogItem.expandAPIlogItemH pid
    , logExplorerRelatedMetricsGet = Metrics.relatedMetricsGetH pid
    , logExplorerExpandGet = Log.apiLogExpandH pid
    , aiSearchPost = AIThreads.aiSearchH pid
    , codeContextGet = PageCodeContext.codeContextH pid
    , liveTailGet = LiveTail.liveTailGetH pid
    , liveTailRecordGet = LiveTail.liveTailRecordH pid
    , liveTailRegisterPost = LiveTail.liveTailRegisterH pid
    , liveTailStreamGet = LiveTail.liveTailStreamH pid
    , liveTailRenewPost = LiveTail.liveTailRenewH pid
    , liveTailDelete = LiveTail.liveTailDeleteH pid
    }


-- Issues server
issuesServer :: Maybe AI.ProjectTools -> Projects.ProjectId -> Servant.ServerT IssuesRoutes ATAuthCtx
issuesServer projectTools pid =
  IssuesRoutes'
    { acknowledgeGet = IssuesPage.acknowledgeIssueGetH pid True
    , unAcknowledgeGet = \aid -> IssuesPage.acknowledgeIssueGetH pid False aid Nothing
    , archiveGet = \iid -> IssuesPage.archiveIssueGetH pid iid . Just . fromMaybe Issues.ArchiveIndefinite
    , resolveGet = IssuesPage.resolveIssueGetH pid
    , triagePost = IssuesPage.triagePostH pid
    , commentPost = IssuesPage.commentPostH pid
    , viewedPost = IssuesPage.issueViewedPostH pid
    , viewSavePost = IssuesPage.saveViewPostH pid
    , viewDeletePost = IssuesPage.deleteViewPostH pid
    , linkPost = IssuesPage.linkPostH pid
    , unarchiveGet = \iid -> IssuesPage.archiveIssueGetH pid iid Nothing
    , bulkActionsPost = IssuesPage.issueBulkActionsPostH pid
    , listGet = IssuesPage.issueListGetH pid
    , detailGet = IssuesPage.issueDetailGetH pid
    , stepGet = IssuesPage.issueStepGetH pid
    , detailHashGet = IssuesPage.issueDetailHashGetH pid
    , assignErrorPost = IssuesPage.assignErrorPostH pid
    , resolveErrorPost = IssuesPage.resolveErrorPostH pid
    , errorSubscriptionPost = IssuesPage.errorSubscriptionPostH pid
    , aiChatPost = IssuesPage.aiChatPostHWithTools projectTools pid
    , aiChatHistoryGet = IssuesPage.aiChatHistoryGetH pid
    , sampleGet = IssuesPage.issueSampleGetH pid
    , activityGet = IssuesPage.issueActivityGetH pid
    , errorGroupMembersGet = IssuesPage.errorGroupMembersGetH pid
    , errorUnmergePost = IssuesPage.errorUnmergePostH pid
    }


telemetryServer :: Projects.ProjectId -> Servant.ServerT TelemetryRoutes ATAuthCtx
telemetryServer pid =
  TelemetryRoutes'
    { tracesGet = Trace.traceH pid
    , metricsOVGetH = Metrics.metricsOverViewGetH pid
    , dataPointCountsGetH = Metrics.dataPointCountsGetH pid
    , metricDetailsGetH = Metrics.metricDetailsGetH pid
    , metricRelatedGetH = Metrics.metricRelatedGetH pid
    , metricBreakdownGetH = Metrics.metricBreakdownGetH pid
    , metricExemplarsGetH = Metrics.metricExemplarsGetH pid
    , metricCardGetH = Metrics.metricCardGetH pid
    , serviceMapGetH = ServiceMap.serviceMapGetH pid
    , metricServicesGetH = Metrics.metricServicesGetH pid
    , hostsGetH = Infrastructure.hostsGetH pid
    , hostDetailGetH = Infrastructure.hostDetailGetH pid
    , infrastructureContainersGetH = Containers.containersGetH pid
    , infrastructureContainerDetailGetH = Containers.containerDetailGetH pid
    , imagesGetH = Infrastructure.imagesGetH pid
    , imageDetailGetH = Infrastructure.imageDetailGetH pid
    , kubernetesGetH = Infrastructure.kubernetesGetH pid
    , kubernetesDetailGetH = Infrastructure.kubernetesDetailGetH pid
    , hostMapGetH = Infrastructure.hostMapGetH pid
    , containersGetH = Containers.containersGetH pid
    , containerDetailGetH = Containers.containerDetailGetH pid
    }


monitorsServer :: Projects.ProjectId -> Servant.ServerT MonitorsRoutes ATAuthCtx
monitorsServer pid =
  MonitorsRoutes'
    { listGet = Testing.unifiedMonitorsGetH pid
    , overviewGet = Testing.unifiedMonitorOverviewH pid
    , cronGet = Testing.cronMonitorsGetH pid
    , cronPost = Testing.cronMonitorPostH pid
    , cronDelete = Testing.cronMonitorDeleteH pid
    , uptimeGet = Testing.uptimeChecksGetH pid
    , uptimePost = Testing.uptimeCheckPostH pid
    , uptimeToggle = Testing.uptimeCheckToggleH pid
    , uptimeDelete = Testing.uptimeCheckDeleteH pid
    , alertUpsertPost = Testing.alertUpsertPostH pid
    , alertSingleGet = Testing.alertSingleGetH pid
    , alertSingleToggleActive = Testing.alertSingleToggleActiveH pid
    , alertMutePost = Testing.alertMuteH pid
    , alertUnmutePost = Testing.alertUnmuteH pid
    , alertResolvePost = Testing.alertResolveH pid
    , alertDeleteRoute = Testing.alertDeleteH pid
    , teamAlertsGetH = Testing.teamAlertsGetH pid
    , alertTeamDeleteH = Testing.alertTeamDeleteH pid
    , alertBulkAction = Testing.alertBulkActionH pid
    }


projectsServer :: Servant.ServerT ProjectsRoutes ATAuthCtx
projectsServer =
  ProjectsRoutes'
    { listGet = ListProjects.listProjectsGetH
    , onboardingProject = CreateProject.projectOnboardingH
    , createPost = CreateProject.createProjectPostH
    , settingsGet = CreateProject.projectSettingsGetH
    , integrationGet = Integrations.integrationsSettingsGetH
    , deleteGet = CreateProject.deleteProjectGetH
    , notificationsUpdateChannelPost = Integrations.updateNotificationsChannel
    , pagerdutyConnect = Integrations.pagerdutyConnectH
    , pagerdutyDisconnect = Integrations.pagerdutyDisconnectH
    , slackDisconnect = Integrations.slackDisconnectH
    , notificationsTestPost = Settings.notificationsTestPostH
    , notificationsTestHistoryGet = Settings.notificationsTestHistoryGetH
    , membersManageGet = ManageMembers.manageMembersGetH
    , membersManagePost = ManageMembers.manageMembersPostH
    , membersDeleteH = ManageMembers.deleteMemberH
    , teamsManageGet = ManageMembers.manageTeamsGetH
    , teamsManagePost = ManageMembers.manageTeamPostH
    , teamGet = ManageMembers.teamGetH
    , teamBulkAction = ManageMembers.manageTeamBulkActionH
    , manageSubscriptionGet = ManageMembers.manageSubGetH
    , stripeCheckout = ManageMembers.stripeCheckoutInitH
    , onboading = \pid -> Onboarding.onboardingGetH pid . (rightToMaybe =<<)
    , onboardingInfoPost = Onboarding.onboardingInfoPostH
    , onboardingConfPost = Onboarding.onboardingConfPostH
    , onboardingPhoneEmailsPost = Onboarding.phoneEmailPostH
    , onboardingIntegrationCheck = Onboarding.checkIntegrationGet
    , onboardingPricingUpdate = CreateProject.pricingUpdateH
    , onboardingDismissChecklist = Onboarding.dismissChecklistH
    , onboardingSkipped = \pid -> Onboarding.onboardingStepSkipped pid . (rightToMaybe =<<)
    }


data Status = Status
  { dbVersion :: Maybe Text
  , gitHash :: Text
  , gitCommitDate :: Text
  }
  deriving stock (Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake Status


statusH :: ATBaseCtx Status
statusH = Status <$> Hasql.interpOne [HI.sql| select version() |] <*> envOr "GIT_HASH" <*> envOr "GIT_COMMIT_DATE"
  where
    envOr k = maybe "dev" toText <$> lookupEnv k


-- | 'RawM' so the response carries the image's own Content-Type: a route content type
-- adds a second header, and nginx keeps the first (an SVG served as octet-stream is a
-- broken image).
avatarGetH :: Projects.UserId -> Request -> (Wai.Response -> IO Wai.ResponseReceived) -> ATBaseCtx Wai.ResponseReceived
avatarGetH userId _ send = do
  userM <- Projects.userById userId
  let name = foldMap (\u -> u.firstName <> " " <> u.lastName) userM
      initialsImg = pure ("image/svg+xml", "public, max-age=60", avatarInitialsSvg name)
      fetchImage url ct cache fallback = handle (\(_ :: SomeException) -> fallback) $ Wreq.get (toString url) <&> \r -> (ct, cache, r ^. Wreq.responseBody)
      gravatar u = fetchImage (gravatarUrl (CI.original u.email)) "image/png" "public, max-age=604800" initialsImg
  (ct, cache, body) <- case userM of
    Nothing -> initialsImg
    Just u
      | T.null u.displayImageUrl -> gravatar u
      | otherwise -> fetchImage u.displayImageUrl "image/jpeg" "public, max-age=604800, immutable" (gravatar u)
  liftIO $ send $ Wai.responseLBS H.status200 [(H.hContentType, ct), (H.hCacheControl, cache)] body


-- | Missing Gravatars fall back to locally rendered initials.
--
-- >>> gravatarUrl "Foo@Example.com"
-- "https://www.gravatar.com/avatar/b48def645758b95537d4424c84d1a9ff?d=404"
gravatarUrl :: Text -> Text
gravatarUrl email =
  "https://www.gravatar.com/avatar/" <> decodeUtf8 (B16.encode $ MD5.hash $ encodeUtf8 $ T.toLower email) <> "?d=404"


-- | Last-resort avatar, drawn locally so it renders when both remote sources fail.
--
-- >>> zipWith T.isInfixOf [">ET<", ">?<"] $ map (decodeUtf8 . avatarInitialsSvg) ["  e2e   test user ", ""]
-- [True,True]
avatarInitialsSvg :: Text -> LBS.ByteString
avatarInitialsSvg name =
  let initials = T.toUpper $ T.take 2 $ foldMap (T.take 1) $ words name
   in encodeUtf8 $ "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 128 128\"><rect width=\"128\" height=\"128\" rx=\"64\" fill=\"#e5e7eb\"/><text x=\"64\" y=\"64\" dy=\".35em\" text-anchor=\"middle\" font-family=\"system-ui,sans-serif\" font-size=\"52\" font-weight=\"600\" fill=\"#4b5563\">" <> (if T.null initials then "?" else initials) <> "</text></svg>"


-- | @/chart_data@ is the one query path whose SQL text arrives from the browser
-- (a dashboard variable re-running its statement, see @reloadVarWhitelist@), and
-- @db_source=postgres@ points it at the pool holding the application's own tables
-- rather than telemetry. So it has to clear the same gates dashboard SQL clears
-- server-side in 'LogQueries.executeSecuredQuery': SELECT-only, and scoped to the
-- project being asked about. Statements defined server-side (widget YAML, the PNG
-- export) never come through here and keep their existing freedom.
-- The statement still carries @{{project_id}}@ here, so substitute it the way
-- 'Charts.queryMetrics' is about to and ask the question of the result.
--
-- >>> let pid = UUIDId UUID.nil :: Projects.ProjectId
-- >>> clientPostgresSqlRejection (Just Hasql.SqlTimefusion) (Just pid) (Just "select 1 from otel_logs_and_spans")
-- Nothing
-- >>> clientPostgresSqlRejection (Just Hasql.SqlPostgres) (Just pid) (Just "select hash::text from apis.endpoints limit 5")
-- Just "Query must filter by project_id"
-- >>> clientPostgresSqlRejection (Just Hasql.SqlPostgres) (Just pid) (Just "select hash::text from apis.endpoints where project_id='{{project_id}}'")
-- Nothing
clientPostgresSqlRejection :: Maybe Hasql.SqlSource -> Maybe Projects.ProjectId -> Maybe Text -> Maybe Text
clientPostgresSqlRejection dbSource pidM querySqlM = case (dbSource, pidM, Utils.nonEmptyT querySqlM) of
  (Just Hasql.SqlPostgres, Just pid, Just sql)
    | let resolved = T.replace "{{project_id}}" pid.toText sql ->
        if
          | not (LogQueries.validateSqlQuery resolved) -> Just "Query contains disallowed operations"
          | not (LogQueries.hasProjectIdFilter resolved pid) -> Just "Query must filter by project_id"
          | otherwise -> Nothing
  _ -> Nothing


-- | @rollup_sql@ is a Postgres statement over an hourly rollup that answers the same
-- series as the chart's KQL query, for hours from @rollup_from@ on. It replaces the
-- query when coverage and whole-hour boundaries permit it, with no environment or
-- service scope. A partial first hour cannot use hourly counts: filtering its bucket
-- drops in-range events, while including it counts events before the requested start.
-- A partial last hour is safe only while it is still the current hour.
--
-- >>> import Data.Time (addUTCTime)
-- >>> let now = Parser.fixedUTCTime; t = Utils.isoT . (`addUTCTime` now) . (* 3600); since s = TimePicker.TimePicker (Just s) Nothing Nothing; abs' a b = TimePicker.TimePicker Nothing (Just $ t a) (Just $ t b)
-- >>> let r from = [("rollup_sql", Just "R"), ("rollup_from", Just $ t from)]; route tp ps = fst $ rollupRoute now tp ps (Nothing, Nothing)
-- >>> (route (since "24H") (r (-72)), route (since "3H") (r (-72)), route (abs' (-30) (-26)) (r (-72)))
-- (Just SqlPostgres,Just SqlPostgres,Just SqlPostgres)
-- >>> (route (since "24H") (r (-12)), route (since "24H") (("environment", Just "prod") : r (-72)), route (since "24H") (take 1 $ r (-72)))
-- (Nothing,Nothing,Nothing)
-- >>> (route (abs' (-2 + 1/6) (-1)) (r (-72)), route (abs' (-2) (-1 + 1/6)) (r (-72)))
-- (Nothing,Nothing)
-- >>> fst $ rollupRoute (addUTCTime 1800 now) (since "1H") (r (-72)) (Nothing, Nothing)
-- Nothing
rollupRoute :: UTCTime -> TimePicker.TimePicker -> [(Text, Maybe Text)] -> (Maybe Hasql.SqlSource, Maybe Text) -> (Maybe Hasql.SqlSource, Maybe Text)
rollupRoute now tp params source = case (param "rollup_sql", iso8601ParseM . toString =<< param "rollup_from", fromM) of
  (Just rollup, Just covered, Just start)
    | all (isNothing . param) ["environment", "service"]
    , start >= covered
    , start == Utils.truncateHour start
    , all (\end -> end == Utils.truncateHour end || end >= now) toM ->
        (Just Hasql.SqlPostgres, Just rollup)
  _ -> source
  where
    param k = Utils.nonEmptyT $ join $ L.lookup k params
    (fromM, toM, _) = TimePicker.parseTimeRange now tp


-- | The @/chart_data@ handler shape, shared by the buffered and the streamed route.
type ChartDataH a = Maybe Hasql.SqlSource -> Maybe Charts.DataType -> Maybe Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Parser.BinDensity -> [(Text, Maybe Text)] -> ATAuthCtx a


chartsDataGetH :: ChartDataH Charts.MetricsData
chartsDataGetH = chartDataH Charts.queryMetrics


chartsDataStreamGetH :: ChartDataH (Headers '[Header "Cache-Control" Text, Header "X-Accel-Buffering" Text] (SourceIO AE.Value))
chartsDataStreamGetH = chartDataH Charts.queryMetricsStream


-- | Scope the request, route it to the rollup when one covers the window, and gate
-- client SQL (see 'clientPostgresSqlRejection') before either runner sees it.
chartDataH :: ChartDataH a -> ChartDataH a
chartDataH run dbSource0 dt pid q qSql0 since fromD toD src density params = do
  let param k = join $ L.lookup k params
  case pid of
    Just p -> Dashboards.authorizeClientSql p qSql0 (param "rollup_sql") (param "sql_sig")
    Nothing -> when (isJust $ Utils.nonEmptyT qSql0 <|> Utils.nonEmptyT (param "rollup_sql")) $ Error.throwError err400{errBody = "SQL needs a project"}
  scopedParams <- chartScopeParams pid params
  now <- Time.currentTime
  let (dbSource, qSql) = rollupRoute now (TimePicker.TimePicker since fromD toD) scopedParams (dbSource0, qSql0)
  whenJust (clientPostgresSqlRejection dbSource pid qSql) \msg -> Error.throwError err400{errBody = toLazy (encodeUtf8 msg)}
  -- The browser names the widget it draws and the query is shaped for it here, as for a
  -- server-rendered one, so a grouped `summarize` stays categorical on a top list.
  let shaped = maybe q (\wt -> fst $ Widget.chartQuery def{Widget.wType = wt, Widget.query = q <|> Just ""}) (rightToMaybe . parseQueryParam =<< param "widget_type")
  run dbSource dt pid shaped qSql since fromD toD src density scopedParams


-- | The environment cookie is an authenticated, sticky project selection. Service is sticky
-- on ordinary telemetry pages, but dashboards opt out: dashboards that need it declare a
-- variable and must not inherit an invisible selector from the page the reader visited first.
chartScopeParams :: Maybe Projects.ProjectId -> [(Text, Maybe Text)] -> ATAuthCtx [(Text, Maybe Text)]
chartScopeParams pid params = case pid of
  Nothing -> pure params
  Just projectId -> do
    (session, _) <- Projects.sessionAndProject projectId
    pure $ [("environment", session.environment)] <> [("service", session.service) | isNothing $ join $ L.lookup "dashboard_id" params] <> filter (\(k, _) -> k `notElem` ["environment", "service", "dashboard_id"]) params


widgetGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> [(Text, Maybe Text)] -> ATAuthCtx (RespHeaders Widget.Widget)
widgetGetH pid widgetJsonM widgetZM sinceStr fromDStr toDStr allParams = do
  -- widgetZ is what we render now (see 'Widget.widgetFetchUrl'); widgetJSON stays readable
  -- for hand-built URLs and for pages still open from before the change.
  widgetJson <- maybe (pure widgetJsonM) (fmap (<|> widgetJsonM) . Widget.decodeWidgetZ) widgetZM
  widget <-
    AE.eitherDecode (encodeUtf8 $ fromMaybe "" widgetJson)
      & either (const $ Error.throwError err400{errBody = "Invalid or missing widgetJSON parameter"}) pure
  Dashboards.authorizeClientSql pid widget.sql widget.rollupSql widget.sqlSig
  now <- Time.currentTime
  let scopeMarker = ("dashboard_id",) . Just <$> widget._dashboardId
  scopedParams <- chartScopeParams (Just pid) $ maybeToList scopeMarker <> allParams
  -- The page's query filters the widget's own, as the browser composes them for /chart_data:
  -- the explorer's top list has no query but the page's.
  let pageQuery = Utils.nonEmptyT $ join $ L.lookup "query" allParams
      widgetWithPid = widget & #_projectId ?~ pid & #query .~ Utils.nonEmptyT (Just $ T.intercalate " | " $ catMaybes [pageQuery, widget.query])
      isEager = widgetWithPid.eager == Just True || widgetWithPid.wType `elem` [Widget.WTTable, Widget.WTTopList, Widget.WTStat, Widget.WTAnomalies]
  processedWidget <-
    if isEager
      then Dashboards.processEagerWidget pid now (sinceStr, fromDStr, toDStr) scopedParams widgetWithPid
      else
        Charts.queryMetrics widgetWithPid.dbSource (Just Charts.DTMetric) (Just pid) widgetWithPid.query widgetWithPid.sql sinceStr fromDStr toDStr Nothing (Just $ Parser.binDensityFor $ Just $ Widget.mapWidgetTypeToChartType widgetWithPid.wType) scopedParams
          <&> \m -> widgetWithPid & #dataset ?~ Widget.toWidgetDataset m
  addRespHeaders processedWidget


-- | Proof media never changes under a signed name: keys are not reused within a run.
proofMediaGetH :: Projects.ProjectId -> UUIDId "pr_review" -> ImpactReviews.ProofFile -> Maybe Text -> Request -> (Wai.Response -> IO Wai.ResponseReceived) -> ATBaseCtx Wai.ResponseReceived
proofMediaGetH pid rid file sigM _ send = do
  ctx <- ask @AuthContext
  whenLeft_ (BotUtils.verifyWidgetSignature ctx.env.apiKeyEncryptionSecretKey pid (ImpactReviews.proofSignedPath rid file) sigM) \err -> Error.throwError err403{errBody = err}
  bytes <- maybe (Error.throwError err404) pure =<< ImpactReviews.proofFile pid rid file
  let mime = case file.format of ImpactReviews.Gif -> "image/gif"; ImpactReviews.Mp4 -> "video/mp4"; ImpactReviews.Cast -> "application/x-asciicast"
  liftIO $ send $ Wai.responseLBS H.status200 [(H.hContentType, mime), (H.hCacheControl, "public, max-age=31536000, immutable")] (toLazy bytes)


-- | Public endpoint for rendering widgets to PNG (for bot embeds). Requires valid HMAC signature.
widgetPngGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> [(Text, Maybe Text)] -> ATBaseCtx (Headers '[Header "Cache-Control" Text] LBS.ByteString)
widgetPngGetH pid widgetJsonM widgetZM sinceStr fromDStr toDStr widthM heightM sigM allParams = do
  ctx <- ask @AuthContext
  let fallback = fromMaybe "" widgetJsonM
  v <- maybe (pure fallback) (fmap (fromMaybe fallback) . Widget.decodeWidgetZ) widgetZM
  Log.logInfo "widgetPngGetH: request" $ AE.object ["widgetJson_len" AE..= T.length v]
  whenLeft_ (BotUtils.verifyWidgetSignature ctx.env.apiKeyEncryptionSecretKey pid v sigM) \err -> Error.throwError $ err403{errBody = err}

  widget <- either (const $ Error.throwError err400{errBody = "Invalid or missing widgetJSON parameter"}) pure $ AE.eitherDecode (encodeUtf8 v)
  let (defaultWidth, defaultHeight) = Widget.pngExportSize widget.pngProfile
      width = clamp (100, 2000) $ fromMaybe defaultWidth widthM
      height = clamp (100, 2000) $ fromMaybe defaultHeight heightM
  -- The signature covers the dataset as well as the query. Saved report charts
  -- carry their measured data, and must remain usable after telemetry expires.
  chart <- case widget.dataset of
    Just _ -> pure $ Right widget
    Nothing -> do
      md <- Dashboards.widgetMetrics pid (sinceStr, fromDStr, toDStr) allParams $ widget & #_projectId ?~ pid & #eager ?~ True
      pure $ case md.error of
        Just queryError -> Left queryError
        Nothing ->
          Right
            $ widget
            & #dataset ?~ case widget.wType of
              Widget.WTStat -> def{Widget.source = AE.Null, Widget.value = md.dataFloat}
              _ -> Widget.toWidgetDataset $ Widget.foldSeries 8 md
  whenLeft_ chart \queryError ->
    Log.logAttention "widgetPngGetH: chart query unavailable" $ AE.object ["widgetId" AE..= widget.id, "error" AE..= queryError]

  let darkMode = case L.lookup "appearance" allParams of
        Just (Just "dark") -> True
        Just (Just "light") -> False
        -- Slack cannot tell us the reader's theme; a dark card reads in both.
        _ -> widget.theme == Just "dark" || widget.pngProfile == Just Widget.PngSlack
      input =
        AE.encode
          $ AE.object
            [ "echarts"
                AE..= either
                  (const $ AE.object ["graphic" AE..= AE.object ["type" AE..= ("text" :: Text), "left" AE..= ("center" :: Text), "top" AE..= ("middle" :: Text), "style" AE..= AE.object ["text" AE..= ("Chart unavailable\nOpen the incident or monitor for details." :: Text), "fontSize" AE..= (18 :: Int), "fill" AE..= (bool "#334155" "#cbd5e1" darkMode :: Text), "textAlign" AE..= ("center" :: Text)]]])
                  (Widget.widgetToECharts . (#theme ?~ bool "default" "dark" darkMode))
                  chart
            , "mode" AE..= (bool "chart" "table" (isRight chart && widget.wType `elem` [Widget.WTTable, Widget.WTTopList]) :: Text)
            , "profile" AE..= fromMaybe Widget.PngStandard widget.pngProfile
            , "width" AE..= width
            , "height" AE..= height
            , "theme" AE..= fromMaybe "default" widget.theme
            , -- Presentation only, like width/height; the signed widget data is unchanged.
              "darkMode" AE..= darkMode
            ]

  pngBytes <-
    liftIO (timeout 30000000 $ readProcess $ setStdin (byteStringInput input) $ proc (maybe "./chart-cli" toString ctx.env.chartCliPath) []) >>= \case
      Nothing -> Log.logAttention "widgetPngGetH: chart render timed out" (AE.object ["widgetId" AE..= widget.id, "width" AE..= width, "height" AE..= height]) *> Error.throwError err504{errBody = "Chart rendering timed out"}
      Just (ExitSuccess, bytes, _) -> pure bytes
      Just (ExitFailure code, _, errOut) -> Log.logAttention "widgetPngGetH: chart render failed" (AE.object ["exitCode" AE..= code, "stderr" AE..= decodeUtf8 @Text (toStrict errOut), "widgetId" AE..= widget.id]) *> Error.throwError err500{errBody = "Chart rendering failed"}

  pure $ addHeader @"Cache-Control" (if isLeft chart then "no-store" else bool "public, max-age=300" "public, max-age=31536000, immutable" $ isJust fromDStr && isJust toDStr) pngBytes


guardDevOnly :: ATBaseCtx ()
guardDevOnly = do
  ctx <- ask @AuthContext
  unless (ctx.config.environment == Dev) $ Error.throwError err404


-- | Single source for both the index and the per-template preview, so a listed
-- template can never be missing a renderer.
emailTemplates :: [(Text, ATBaseCtx (Text, Html ()))]
emailTemplates =
  [ ("project-invite", pure ET.sampleProjectInvite)
  , ("project-created", pure ET.sampleProjectCreated)
  , ("project-deleted", pure ET.sampleProjectDeleted)
  , ("weekly-report", pure $ ET.sampleWeeklyReport (Just "https://placehold.co/600x200?text=Events+Chart") (Just "https://placehold.co/600x200?text=Errors+Chart"))
  ,
    ( "runtime-errors"
    , do
        ctx <- ask @AuthContext
        chartUrl <-
          Widget.widgetPngUrl
            ctx.env.apiKeyEncryptionSecretKey
            ctx.env.hostUrl
            (UUIDId UUID.nil)
            def{Widget.wType = Widget.WTTimeseries, Widget.query = Just "project_id == \"00000000-0000-0000-0000-000000000000\" and level == \"ERROR\" | summarize count(*) by bin_auto(timestamp)", Widget.theme = Just "roma"}
            (Just "24H")
            Nothing
            Nothing
        pure $ ET.sampleRuntimeErrors chartUrl
    )
  , ("anomaly-endpoint", pure ET.sampleAnomalyEndpoint)
  , ("issue-assigned", pure ET.sampleIssueAssigned)
  ]


emailPreviewListH :: ATBaseCtx (Html ())
emailPreviewListH = do
  guardDevOnly
  pure $ ET.emailWrapper "Email Templates" $ ET.emailBody do
    h1_ "Email Template Previews"
    p_ "Click a template to preview:"
    ul_ $ forM_ emailTemplates \(name, _) ->
      li_ [class_ "my-2"] $ a_ [href_ $ "/dev/emails/" <> name] $ toHtml name


emailPreviewH :: Text -> ATBaseCtx (Html ())
emailPreviewH templateName = do
  guardDevOnly
  uncurry ET.emailWrapper <$> fromMaybe (pure ("Unknown", p_ "Template not found")) (L.lookup templateName emailTemplates)


-- | The context that will be made available to request handlers. We supply the
-- "cookie-auth"-tagged request handler defined above, so that the 'HasServer' instance
-- of 'AuthProtect' can extract the handler and run it on the request.
genAuthServerContext :: Logger -> AuthContext -> Servant.Context '[APItoolkitAuthContext, ApiKeyAuthContext, Servant.ErrorFormatters]
genAuthServerContext logger env = authHandler logger env :. apiKeyAuthHandler logger env :. errorFormatters env :. EmptyContext


errorFormatters :: AuthContext -> Servant.ErrorFormatters
errorFormatters env =
  Servant.defaultErrorFormatters
    { Servant.notFoundErrorFormatter = servantRejection env.config ApiT.NoRoute
    , Servant.bodyParserErrorFormatter = \_ req msg -> servantRejection env.config (ApiT.InvalidRequest $ toText msg) req
    , Servant.urlParseErrorFormatter = \_ req msg -> servantRejection env.config (ApiT.InvalidRequest $ toText msg) req
    }


-- The page-defined types "Web.Links" names but cannot import.
type instance Opaque "AIThreads.AIChatForm" = AIThreads.AIChatForm
type instance Opaque "AIThreads.AIPage" = AIThreads.AIPage
type instance Opaque "AIThreads.EmptyTitle" = AIThreads.EmptyTitle
type instance Opaque "AIThreads.NewThreadError" = AIThreads.NewThreadError
type instance Opaque "AIThreads.NoReviewClasses" = AIThreads.NoReviewClasses
type instance Opaque "AIThreads.OperationReviewForm" = AIThreads.OperationReviewForm
type instance Opaque "AIThreads.OperationReviewed" = AIThreads.OperationReviewed
type instance Opaque "AIThreads.PolicySaved" = AIThreads.PolicySaved
type instance Opaque "AIThreads.PromptError" = AIThreads.PromptError
type instance Opaque "AIThreads.RoutineDestinationForm" = AIThreads.RoutineDestinationForm
type instance Opaque "AIThreads.RoutineForm" = AIThreads.RoutineForm
type instance Opaque "AIThreads.RoutineInstructionsForm" = AIThreads.RoutineInstructionsForm
type instance Opaque "AIThreads.RoutinePolicyForm" = AIThreads.RoutinePolicyForm
type instance Opaque "AIThreads.RoutineTemplateForm" = AIThreads.RoutineTemplateForm
type instance Opaque "AIThreads.TitleForm" = AIThreads.TitleForm
type instance Opaque "AIThreads.TurnError" = AIThreads.TurnError
type instance Opaque "AIThreads.UnknownTemplate" = AIThreads.UnknownTemplate
type instance Opaque "ApiCatalog.ApiDocsPage" = ApiCatalog.ApiDocsPage
type instance Opaque "ApiCatalog.CatalogBulkAction" = ApiCatalog.CatalogBulkAction
type instance Opaque "ApiCatalog.CatalogBulkRejection" = ApiCatalog.CatalogBulkRejection
type instance Opaque "ApiCatalog.EnpReqStatsVM" = ApiCatalog.EnpReqStatsVM
type instance Opaque "ApiCatalog.HostBulkActionForm" = ApiCatalog.HostBulkActionForm
type instance Opaque "ApiCatalog.HostEventsVM" = ApiCatalog.HostEventsVM
type instance Opaque "ApiT.LogResult" = ApiT.LogResult
type instance Opaque "BotUtils.BotResponse" = BotUtils.BotResponse
type instance Opaque "CommandPalette.RecentForm" = CommandPalette.RecentForm
type instance Opaque "Containers.ContainersGet" = Containers.ContainersGet
type instance Opaque "CreateProject.BillingRedirect" = CreateProject.BillingRedirect
type instance Opaque "CreateProject.BillingRejection" = CreateProject.BillingRejection
type instance Opaque "CreateProject.CreateProject" = CreateProject.CreateProject
type instance Opaque "CreateProject.CreateProjectForm" = CreateProject.CreateProjectForm
type instance Opaque "CreateProject.DemoProjectProtected" = CreateProject.DemoProjectProtected
type instance Opaque "CreateProject.PricingUpdateForm" = CreateProject.PricingUpdateForm
type instance Opaque "CreateProject.PricingUpdated" = CreateProject.PricingUpdated
type instance Opaque "CreateProject.ProjectDeleted" = CreateProject.ProjectDeleted
type instance Opaque "CreateProject.StripeCheckoutForm" = CreateProject.StripeCheckoutForm
type instance Opaque "CreateProject.SubscriptionLookupFailed" = CreateProject.SubscriptionLookupFailed
type instance Opaque "Dashboards.DashboardBulkActionForm" = Dashboards.DashboardBulkActionForm
type instance Opaque "Dashboards.DashboardForm" = Dashboards.DashboardForm
type instance Opaque "Dashboards.DashboardGet" = Dashboards.DashboardGet
type instance Opaque "Dashboards.DashboardNew" = Dashboards.DashboardNew
type instance Opaque "Dashboards.DashboardRenameForm" = Dashboards.DashboardRenameForm
type instance Opaque "Dashboards.DashboardRes" = Dashboards.DashboardRes
type instance Opaque "Dashboards.DashboardSettleSample" = Dashboards.DashboardSettleSample
type instance Opaque "Dashboards.DashboardViewForm" = Dashboards.DashboardViewForm
type instance Opaque "Dashboards.DashboardYaml" = Dashboards.DashboardYaml
type instance Opaque "Dashboards.DashboardSwitcher" = Dashboards.DashboardSwitcher
type instance Opaque "Dashboards.DashboardsGet" = Dashboards.DashboardsGet
type instance Opaque "Dashboards.InvalidViewName" = Dashboards.InvalidViewName
type instance Opaque "Dashboards.SavedViews" = Dashboards.SavedViews
type instance Opaque "Dashboards.SqlPreview" = Dashboards.SqlPreview
type instance Opaque "Dashboards.StarButton" = Dashboards.StarButton
type instance Opaque "Dashboards.TabRenameForm" = Dashboards.TabRenameForm
type instance Opaque "Dashboards.TabRenameRes" = Dashboards.TabRenameRes
type instance Opaque "Dashboards.WidgetAlertForm" = Dashboards.WidgetAlertForm
type instance Opaque "Dashboards.WidgetEditor" = Dashboards.WidgetEditor
type instance Opaque "Dashboards.WidgetMonitorRejection" = Dashboards.WidgetMonitorRejection
type instance Opaque "Dashboards.WidgetMonitorRemoved" = Dashboards.WidgetMonitorRemoved
type instance Opaque "Dashboards.WidgetMonitorSaved" = Dashboards.WidgetMonitorSaved
type instance Opaque "Dashboards.WidgetMonitorStatus" = Dashboards.WidgetMonitorStatus
type instance Opaque "Dashboards.WidgetReorderItem" = Dashboards.WidgetReorderItem
type instance Opaque "Dashboards.WidgetSaved" = Dashboards.WidgetSaved
type instance Opaque "Dashboards.YamlForm" = Dashboards.YamlForm
type instance Opaque "Dashboards.YamlSave" = Dashboards.YamlSave
type instance Opaque "GitSync.DashboardRepositoryForm" = GitSync.DashboardRepositoryForm
type instance Opaque "GitSync.DashboardRepositoryGet" = GitSync.DashboardRepositoryGet
type instance Opaque "GitSync.GitHubConnect" = GitSync.GitHubConnect
type instance Opaque "GitSync.GitSyncForm" = GitSync.GitSyncForm
type instance Opaque "GitSync.GitSyncRemoval" = GitSync.GitSyncRemoval
type instance Opaque "GitSync.GitSyncSettings" = GitSync.GitSyncSettings
type instance Opaque "GitSync.RepoSelectForm" = GitSync.RepoSelectForm
type instance Opaque "GitSync.RepositoryDashboardForm" = GitSync.RepositoryDashboardForm
type instance Opaque "GitSync.RepositoryDashboardGet" = GitSync.RepositoryDashboardGet
type instance Opaque "GitSync.SelectRejection" = GitSync.SelectRejection
type instance Opaque "GitSync.SyncPanels" = GitSync.SyncPanels
type instance Opaque "Infrastructure.DetailGet" = Infrastructure.DetailGet
type instance Opaque "Infrastructure.HostMapGet" = Infrastructure.HostMapGet
type instance Opaque "Infrastructure.HostsGet" = Infrastructure.HostsGet
type instance Opaque "Infrastructure.ImagesGet" = Infrastructure.ImagesGet
type instance Opaque "Infrastructure.KubernetesGet" = Infrastructure.KubernetesGet
type instance Opaque "Integrations.ChannelsRejection" = Integrations.ChannelsRejection
type instance Opaque "Integrations.IntegrationsConfig" = Integrations.IntegrationsConfig
type instance Opaque "Integrations.IntegrationsSaved" = Integrations.IntegrationsSaved
type instance Opaque "Integrations.NotifListForm" = Integrations.NotifListForm
type instance Opaque "Integrations.PagerdutyConnectForm" = Integrations.PagerdutyConnectForm
type instance Opaque "Integrations.PagerdutyKeyTooShort" = Integrations.PagerdutyKeyTooShort
type instance Opaque "Integrations.SlackNotInstalled" = Integrations.SlackNotInstalled
type instance Opaque "IssuesPage.AIChatForm" = IssuesPage.AIChatForm
type instance Opaque "IssuesPage.AssignErrorForm" = IssuesPage.AssignErrorForm
type instance Opaque "IssuesPage.AssigneeChanged" = IssuesPage.AssigneeChanged
type instance Opaque "IssuesPage.CommentForm" = IssuesPage.CommentForm
type instance Opaque "IssuesPage.ErrorGroupMembers" = IssuesPage.ErrorGroupMembers
type instance Opaque "IssuesPage.ErrorPatternRejection" = IssuesPage.ErrorPatternRejection
type instance Opaque "IssuesPage.ErrorResolved" = IssuesPage.ErrorResolved
type instance Opaque "IssuesPage.ErrorSubscribed" = IssuesPage.ErrorSubscribed
type instance Opaque "IssuesPage.ErrorSubscriptionForm" = IssuesPage.ErrorSubscriptionForm
type instance Opaque "IssuesPage.IssueAction" = IssuesPage.IssueAction
type instance Opaque "IssuesPage.IssueActivityFeed" = IssuesPage.IssueActivityFeed
type instance Opaque "IssuesPage.IssueBulkForm" = IssuesPage.IssueBulkForm
type instance Opaque "IssuesPage.IssueChatHistory" = IssuesPage.IssueChatHistory
type instance Opaque "IssuesPage.IssueDetail" = IssuesPage.IssueDetail
type instance Opaque "IssuesPage.IssueSample" = IssuesPage.IssueSample
type instance Opaque "IssuesPage.IssueTriage" = IssuesPage.IssueTriage
type instance Opaque "IssuesPage.IssueVM" = IssuesPage.IssueVM
type instance Opaque "IssuesPage.LinkForm" = IssuesPage.LinkForm
type instance Opaque "IssuesPage.PatternUnmerged" = IssuesPage.PatternUnmerged
type instance Opaque "IssuesPage.SaveViewForm" = IssuesPage.SaveViewForm
type instance Opaque "IssuesPage.TimelineLogged" = IssuesPage.TimelineLogged
type instance Opaque "IssuesPage.TimelineRejection" = IssuesPage.TimelineRejection
type instance Opaque "IssuesPage.TriageForm" = IssuesPage.TriageForm
type instance Opaque "IssuesPage.TriageRejection" = IssuesPage.TriageRejection
type instance Opaque "IssuesPage.UnnamedView" = IssuesPage.UnnamedView
type instance Opaque "IssuesPage.ViewChanged" = IssuesPage.ViewChanged
type instance Opaque "ListProjects.ListProjectsGet" = ListProjects.ListProjectsGet
type instance Opaque "LiveTail.EventStream" = LiveTail.EventStream
type instance Opaque "LiveTail.LiveTailGet" = LiveTail.LiveTailGet
type instance Opaque "LiveTail.RegisterResponse" = LiveTail.RegisterResponse
type instance Opaque "Log.AlertForm" = Log.AlertForm
type instance Opaque "Log.FacetsView" = Log.FacetsView
type instance Opaque "Log.LogsGet" = Log.LogsGet
type instance Opaque "Log.PatternsView" = Log.PatternsView
type instance Opaque "Log.QueryValidation" = Log.QueryValidation
type instance Opaque "Log.SessionsView" = Log.SessionsView
type instance Opaque "LogItem.ApiItemDetailed" = LogItem.ApiItemDetailed
type instance Opaque "ManageMembers.ManageMembers" = ManageMembers.ManageMembers
type instance Opaque "ManageMembers.ManageMembersForm" = ManageMembers.ManageMembersForm
type instance Opaque "ManageMembers.ManageTeams" = ManageMembers.ManageTeams
type instance Opaque "ManageMembers.MemberRejection" = ManageMembers.MemberRejection
type instance Opaque "ManageMembers.MemberRemoved" = ManageMembers.MemberRemoved
type instance Opaque "ManageMembers.TBulkActionForm" = ManageMembers.TBulkActionForm
type instance Opaque "ManageMembers.TeamForm" = ManageMembers.TeamForm
type instance Opaque "ManageMembers.TeamRejection" = ManageMembers.TeamRejection
type instance Opaque "ManageMembers.TeamSaved" = ManageMembers.TeamSaved
type instance Opaque "ManageMembers.TeamsDeleteRejection" = ManageMembers.TeamsDeleteRejection
type instance Opaque "ManageMembers.TeamsDeleted" = ManageMembers.TeamsDeleted
type instance Opaque "Metrics.DataPointCounts" = Metrics.DataPointCounts
type instance Opaque "Metrics.MetricDetailsGet" = Metrics.MetricDetailsGet
type instance Opaque "Metrics.MetricExemplarsGet" = Metrics.MetricExemplarsGet
type instance Opaque "Metrics.MetricFragment" = Metrics.MetricFragment
type instance Opaque "Metrics.MetricServices" = Metrics.MetricServices
type instance Opaque "Metrics.MetricsOverViewGet" = Metrics.MetricsOverViewGet
type instance Opaque "Metrics.RelatedMetrics" = Metrics.RelatedMetrics
type instance Opaque1 "NavigationResponse" = NavigationResponse
type instance Opaque "Onboarding.ChecklistDismissed" = Onboarding.ChecklistDismissed
type instance Opaque "Onboarding.IntegrationCheck" = Onboarding.IntegrationCheck
type instance Opaque "Onboarding.NoEventsYet" = Onboarding.NoEventsYet
type instance Opaque "Onboarding.NotifChannelForm" = Onboarding.NotifChannelForm
type instance Opaque "Onboarding.OnboardingConfForm" = Onboarding.OnboardingConfForm
type instance Opaque "Onboarding.OnboardingGet" = Onboarding.OnboardingGet
type instance Opaque "Onboarding.OnboardingInfoForm" = Onboarding.OnboardingInfoForm
type instance Opaque "Onboarding.OnboardingPhoneEmailsPost" = Onboarding.OnboardingPhoneEmailsPost
type instance Opaque "Onboarding.OnboardingStepPost" = Onboarding.OnboardingStepPost
type instance Opaque "PageCodeContext.CodeMappingForm" = PageCodeContext.CodeMappingForm
type instance Opaque "PageCodeContext.ConflictForm" = PageCodeContext.ConflictForm
type instance Opaque "PageCodeContext.FolderForm" = PageCodeContext.FolderForm
type instance Opaque "PageCodeContext.FrameSource" = PageCodeContext.FrameSource
type instance Opaque "PageCodeContext.MappingRejection" = PageCodeContext.MappingRejection
type instance Opaque "PageCodeContext.MappingsPanel" = PageCodeContext.MappingsPanel
type instance Opaque "PageCodeContext.MemoryForm" = PageCodeContext.MemoryForm
type instance Opaque "PageCodeContext.RepositoriesGet" = PageCodeContext.RepositoriesGet
type instance Opaque "PageCodeContext.RepositoryConnectForm" = PageCodeContext.RepositoryConnectForm
type instance Opaque "PageCodeContext.RepositoryConnectGet" = PageCodeContext.RepositoryConnectGet
type instance Opaque "PageCodeContext.RepositoryGet" = PageCodeContext.RepositoryGet
type instance Opaque "PageCodeContext.RepositoryRemoval" = PageCodeContext.RepositoryRemoval
type instance Opaque "PageCodeContext.RepositorySourceGet" = PageCodeContext.RepositorySourceGet
type instance Opaque "PageCodeContext.RepositoryTokenForm" = PageCodeContext.RepositoryTokenForm
type instance Opaque "PageCodeContext.RepositoryTokenGet" = PageCodeContext.RepositoryTokenGet
type instance Opaque1 "PageCodeContext.ReviewChange" = PageCodeContext.ReviewChange
type instance Opaque "PageCodeContext.SourceContextForm" = PageCodeContext.SourceContextForm
type instance Opaque "PageCodeContext.WikiGet" = PageCodeContext.WikiGet
type instance Opaque1 "PageCtx" = PageCtx
type instance Opaque "QueryLibrary.QueryLibraryView" = QueryLibrary.QueryLibraryView
type instance Opaque "QueryLibrary.SaveQueryForm" = QueryLibrary.SaveQueryForm
type instance Opaque "Reports.ReportsGet" = Reports.ReportsGet
type instance Opaque "Reports.ReportsPost" = Reports.ReportsPost
type instance Opaque "ServiceMap.ServiceMapGet" = ServiceMap.ServiceMapGet
type instance Opaque "Settings.ApiGet" = Settings.ApiGet
type instance Opaque "Settings.ApiKeyNotFound" = Settings.ApiKeyNotFound
type instance Opaque "Settings.ApiMut" = Settings.ApiMut
type instance Opaque "Settings.BillingGet" = Settings.BillingGet
type instance Opaque "Settings.GenerateAPIKeyForm" = Settings.GenerateAPIKeyForm
type instance Opaque "Settings.NotificationTestHistoryGet" = Settings.NotificationTestHistoryGet
type instance Opaque "Settings.PrometheusForm" = Settings.PrometheusForm
type instance Opaque "Settings.PrometheusGet" = Settings.PrometheusGet
type instance Opaque "Settings.PrometheusMut" = Settings.PrometheusMut
type instance Opaque "Settings.ScrapeProbe" = Settings.ScrapeProbe
type instance Opaque "Settings.StorageGet" = Settings.StorageGet
type instance Opaque "Settings.TestForm" = Settings.TestForm
type instance Opaque "Settings.TestOutcome" = Settings.TestOutcome
type instance Opaque "Share.ShareLinkPost" = Share.ShareLinkPost
type instance Opaque "Slack.SlackLinkForm" = Slack.SlackLinkForm
type instance Opaque1 "Table.Table" = Table.Table
type instance Opaque1 "Table.TableResponse" = Table.TableResponse
type instance Opaque1 "Table.TableRows" = Table.TableRows
type instance Opaque "Testing.Alert" = Testing.Alert
type instance Opaque "Testing.AlertUpsertForm" = Testing.AlertUpsertForm
type instance Opaque "Testing.CronForm" = Testing.CronForm
type instance Opaque "Testing.CronList" = Testing.CronList
type instance Opaque "Testing.CronMonitors" = Testing.CronMonitors
type instance Opaque "Testing.CronRejection" = Testing.CronRejection
type instance Opaque "Testing.MonitorActed" = Testing.MonitorActed
type instance Opaque "Testing.UnifiedMonitorItem" = Testing.UnifiedMonitorItem
type instance Opaque "Testing.UptimeChecks" = Testing.UptimeChecks
type instance Opaque "Testing.UptimeForm" = Testing.UptimeForm
type instance Opaque "Testing.UptimeList" = Testing.UptimeList
type instance Opaque "Testing.UptimeRejection" = Testing.UptimeRejection
type instance Opaque "Trace.TraceDetailsGet" = Trace.TraceDetailsGet
type instance Opaque "Widget.Widget" = Widget.Widget
