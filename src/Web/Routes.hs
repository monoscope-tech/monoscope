module Web.Routes (server, genAuthServerContext) where

import Data.Aeson
import Data.Aeson qualified as AE
import Data.Pool (Pool)
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Deriving.Aeson qualified as DAE
import Effectful (runPureEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.PostgreSQL.Transact (queryOne_)
import Effectful.Reader.Static (runReader)
import GitHash (giCommitDate, giHash, tGitInfoCwd)
import Log (Logger)
import Lucid (Html)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields.Types qualified as Fields (FieldId)
import Models.Apis.Reports qualified as ReportsM
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types (notFound404)
import Pages.Anomalies.Routes qualified as AnomaliesRoutes
import Pages.Anomalies.Server qualified as AnomaliesRoutes
import Pages.Api qualified as Api
import Pages.AutoComplete qualified as AutoComplete
import Pages.Charts.Charts qualified as Charts
import Pages.Dashboard qualified as Dashboard
import Pages.Endpoints.Routes qualified as EndpointsRoutes
import Pages.Endpoints.Server qualified as EndpointsRoutes
import Pages.Fields.FieldDetails qualified as FieldDetails
import Pages.IntegrationGuides qualified as IntegrationGuides
import Pages.LogExplorer.Routes qualified as LogExplorerRoutes
import Pages.LogExplorer.Server qualified as LogExplorerRoutes
import Pages.Monitors.Routes qualified as MonitorsRoutes
import Pages.Monitors.Server qualified as MonitorsRoutes
import Pages.Onboarding qualified as Onboarding
import Pages.Projects.Routes qualified as ProjectsRoutes
import Pages.Projects.Server qualified as ProjectsRoutes
import Pages.RedactedFields (RedactFieldForm)
import Pages.RedactedFields qualified as RedactedFields
import Pages.Reports qualified as Reports
import Pages.Share qualified as Share
import Pages.SlackInstall qualified as SlackInstall
import Pages.Specification.GenerateSwagger qualified as GenerateSwagger
import Pages.Specification.Routes qualified as SpecificationRoutes
import Pages.Specification.Server qualified as SpecificationRoutes
import Pages.Survey qualified as Survey
import Relude
import Servant (AuthProtect, Capture, Context (..), Delete, FormUrlEncoded, Get, Header, Headers, JSON, NoContent, PlainText, Post, QueryParam, ReqBody, StdMethod (GET), Verb, (:>))
import Servant qualified
import Servant.API.Generic
import Servant.HTML.Lucid (HTML)
import Servant.Htmx
import Servant.Server.Generic (AsServerT)
import System.Config (AuthContext)
import System.Types
import Web.Auth (APItoolkitAuthContext, authHandler)
import Web.Auth qualified as Auth
import Web.ClientMetadata qualified as ClientMetadata
import Web.Cookie (SetCookie)
import Web.Error


type QPT a = QueryParam a Text


type GetRedirect = Verb 'GET 302


type role Routes nominal


data Routes mode = Routes
  { assets :: mode :- "assets" :> Servant.Raw
  , public :: mode :- "public" :> Servant.Raw
  , cookieProtected :: mode :- AuthProtect "optional-cookie-auth" :> Servant.NamedRoutes CookieProtectedRoutes
  , ping :: mode :- "ping" :> Get '[PlainText] Text
  , status :: mode :- "status" :> Get '[JSON] Status
  , login :: mode :- "login" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
  , toLogin :: mode :- "to_login" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
  , logout :: mode :- "logout" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
  , authCallback :: mode :- "auth_callback" :> QPT "code" :> QPT "state" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] (Html ()))
  , shareLinkGet :: mode :- "share" :> "r" :> Capture "shareID" UUID.UUID :> Get '[HTML] (Html ())
  , slackInstallGet :: mode :- "slack" :> "oauth" :> "callback" :> QPT "code" :> Get '[HTML] (Html ())
  , slackLinkProjectGet :: mode :- "slack" :> "oauth" :> "callback" :> Capture "project_id" Projects.ProjectId :> QPT "code" :> Get '[HTML] (Html ())
  , clientMetadata :: mode :- "api" :> "client_metadata" :> Header "Authorization" Text :> Get '[JSON] ClientMetadata.ClientMetadata
  }
  deriving stock (Generic)


server
  :: Pool PG.Connection
  -> Routes (AsServerT ATBaseCtx)
server pool =
  Routes
    { assets = Servant.serveDirectoryWebApp "./static/public/assets"
    , public = Servant.serveDirectoryWebApp "./static/public"
    , ping = pingH
    , status = statusH
    , login = Auth.loginH
    , toLogin = Auth.loginRedirectH
    , logout = Auth.logoutH
    , authCallback = Auth.authCallbackH
    , shareLinkGet = Share.shareLinkGetH
    , slackInstallGet = SlackInstall.getH
    , slackLinkProjectGet = SlackInstall.linkProjectGetH
    , clientMetadata = ClientMetadata.clientMetadataH
    , cookieProtected = \sessionWithCookies ->
        Servant.hoistServerWithContext
          (Proxy @(Servant.NamedRoutes CookieProtectedRoutes))
          (Proxy @'[APItoolkitAuthContext])
          ( \page ->
              page
                & Effectful.Reader.Static.runReader sessionWithCookies
          )
          cookieProtectedServer
    }


type role CookieProtectedRoutes nominal


data CookieProtectedRoutes mode = CookieProtectedRoutes
  { dashboardGet :: mode :- "p" :> ProjectId :> QPT "from" :> QPT "to" :> QPT "since" :> Get '[HTML] (Html ())
  , projects :: mode :- ProjectsRoutes.Routes
  , onboardingGet :: mode :- "p" :> ProjectId :> "onboarding" :> QPB "polling" :> QPB "redirected" :> QPT "current_tab" :> Get '[HTML] (Html ())
  , anomalies :: mode :- "p" :> ProjectId :> "anomalies" :> AnomaliesRoutes.Routes
  , logExplorer :: mode :- "p" :> ProjectId :> LogExplorerRoutes.Routes
  , endpoints :: mode :- "p" :> ProjectId :> EndpointsRoutes.Routes
  , monitors :: mode :- "p" :> ProjectId :> MonitorsRoutes.Routes
  , specification :: mode :- "p" :> ProjectId :> SpecificationRoutes.Routes
  , apiGet :: mode :- "p" :> ProjectId :> "apis" :> Get '[HTML] (Html ())
  , apiDelete :: mode :- "p" :> ProjectId :> "apis" :> Capture "keyID" ProjectApiKeys.ProjectApiKeyId :> Delete '[HTML] (Headers '[HXTrigger] (Html ()))
  , apiPost :: mode :- "p" :> ProjectId :> "apis" :> ReqBody '[FormUrlEncoded] Api.GenerateAPIKeyForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
  , slackInstallPost :: mode :- "slack" :> "link-projects" :> ReqBody '[FormUrlEncoded] SlackInstall.LinkProjectsForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
  , slackLinkProjectsGet :: mode :- "slack" :> "link-projects" :> QPT "code" :> Get '[HTML] (Html ())
  , slackUpdateWebhook :: mode :- "p" :> ProjectId :> "slack" :> "webhook" :> ReqBody '[FormUrlEncoded] SlackInstall.LinkProjectsForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
  , redactedFieldsGet :: mode :- "p" :> ProjectId :> "redacted_fields" :> Get '[HTML] (Html ())
  , redactedFieldsPost :: mode :- "p" :> ProjectId :> "redacted_fields" :> ReqBody '[FormUrlEncoded] RedactFieldForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
  , reportsGet :: mode :- "p" :> ProjectId :> "reports" :> QPT "page" :> HXRequest :> HXBoosted :> Get '[HTML] (Html ())
  , reportsSingleGet :: mode :- "p" :> ProjectId :> "reports" :> Capture "report_id" ReportsM.ReportId :> Get '[HTML] (Html ())
  , reportsPost :: mode :- "p" :> ProjectId :> "reports_notif" :> Capture "report_type" Text :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
  , shareLinkPost :: mode :- "p" :> ProjectId :> "share" :> ReqBody '[FormUrlEncoded] Share.ReqForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
<<<<<<< HEAD
=======
  , outgoingGet :: mode :- "p" :> ProjectId :> "outgoing" :> QPT "sort" :> QPT "" :> QPT "activeTab" :> Get '[HTML] (Html ())
>>>>>>> 12fe8de (made the ui of the outgoing page similiar to the endpoint page and also added the search bar and tabs functionality)
  , queryBuilderAutocomplete :: mode :- "p" :> ProjectId :> "query_builder" :> "autocomplete" :> QPT "category" :> QPT "prefix" :> Get '[JSON] AE.Value
  , swaggerGenerateGet :: mode :- "p" :> ProjectId :> "generate_swagger" :> Get '[JSON] AE.Value
  , chartsGet :: mode :- "charts_html" :> QP "chart_type" Charts.ChartType :> QPT "query_raw" :> QueryParam "pid" Projects.ProjectId :> QP "group_by" Charts.GroupBy :> QP "query_by" [Charts.QueryBy] :> QP "num_slots" Int :> QP "limit" Int :> QP "theme" Text :> QPT "id" :> QP "show_legend" Bool :> QPT "since" :> QPT "from" :> QPT "to" :> Get '[HTML] (Html ())
  , surveyPut :: mode :- "p" :> ProjectId :> "survey" :> ReqBody '[FormUrlEncoded] Survey.SurveyForm :> Post '[HTML] (Headers '[HXTrigger, HXRedirect] (Html ()))
  , surveyGet :: mode :- "p" :> ProjectId :> "about_project" :> Get '[HTML] (Html ())
  , editField :: mode :- "p" :> ProjectId :> "fields" :> Capture "field_id" Fields.FieldId :> ReqBody '[FormUrlEncoded] FieldDetails.EditFieldForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
  , integrationGuides :: mode :- "p" :> ProjectId :> "integration_guides" :> QPT "sdk" :> QPT "error_reporting" :> QPT "dependency_monitoring" :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)


cookieProtectedServer :: Servant.ServerT (Servant.NamedRoutes CookieProtectedRoutes) ATAuthCtx
cookieProtectedServer =
  CookieProtectedRoutes
    { dashboardGet = Dashboard.dashboardGetH
    , projects = ProjectsRoutes.server
    , onboardingGet = Onboarding.onboardingGetH
    , logExplorer = LogExplorerRoutes.server
    , anomalies = AnomaliesRoutes.server
    , endpoints = EndpointsRoutes.server
    , monitors = MonitorsRoutes.server
    , specification = SpecificationRoutes.server
    , apiGet = Api.apiGetH
    , apiDelete = Api.apiDeleteH
    , apiPost = Api.apiPostH
    , slackInstallPost = SlackInstall.postH
    , slackLinkProjectsGet = SlackInstall.linkProjectsGetH
    , slackUpdateWebhook = SlackInstall.updateWebHook
    , redactedFieldsGet = RedactedFields.redactedFieldsGetH
    , redactedFieldsPost = RedactedFields.redactedFieldsPostH
    , reportsGet = Reports.reportsGetH
    , reportsSingleGet = Reports.singleReportGetH
    , reportsPost = Reports.reportsPostH
    , shareLinkPost = Share.shareLinkPostH
    , queryBuilderAutocomplete = AutoComplete.getH
    , swaggerGenerateGet = GenerateSwagger.generateGetH
    , chartsGet = Charts.chartsGetH
    , surveyPut = Survey.surveyPutH
    , surveyGet = Survey.surveyGetH
    , editField = FieldDetails.fieldPutH
    , integrationGuides = IntegrationGuides.getH
    }


-- | The context that will be made available to request handlers. We supply the
-- "cookie-auth"-tagged request handler defined above, so that the 'HasServer' instance
-- of 'AuthProtect' can extract the handler and run it on the request.
genAuthServerContext :: Logger -> AuthContext -> Servant.Context '[APItoolkitAuthContext, Servant.ErrorFormatters]
genAuthServerContext logger env = authHandler logger env :. errorFormatters env :. EmptyContext


errorFormatters :: AuthContext -> Servant.ErrorFormatters
errorFormatters env =
  Servant.defaultErrorFormatters{Servant.notFoundErrorFormatter = notFoundPage env}


notFoundPage :: AuthContext -> Servant.NotFoundErrorFormatter
notFoundPage env _req =
  let result = runPureEff $ runErrorNoCallStack $ renderError env notFound404
   in case result of
        Left err -> err
        Right _ -> Servant.err404


data Status = Status
  { dbVersion :: Maybe Text
  , gitHash :: Text
  , gitCommitDate :: Text
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Status


statusH :: ATBaseCtx Status
statusH = do
  let query = [sql| select version(); |]
  versionM <- queryOne_ query
  let version = versionM <&> \(PG.Only v) -> v
  let gi = $$tGitInfoCwd
  pure
    Status
      { dbVersion = version
      , gitHash = toText $ giHash gi
      , gitCommitDate = toText $ giCommitDate gi
      }


pingH :: ATBaseCtx Text
pingH = do
  pure "pong"


-- When bystring is returned for json, simply return the bytestring
instance Servant.MimeRender JSON ByteString where
  mimeRender _ = fromStrict


type QP a b = QueryParam a b


type QPU a = QueryParam a UTCTime


type QPB a = QueryParam a Bool


type QPI a = QueryParam a Int


type QEID a = QueryParam a Endpoints.EndpointId


type ProjectId = Capture "projectID" Projects.ProjectId
