module Web.Routes (server, genAuthServerContext) where

import Data.Aeson
import Data.Pool (Pool)
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Deriving.Aeson qualified as DAE
import Effectful (runPureEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.PostgreSQL.Transact (queryOne_)
import Effectful.Reader.Static (runReader)
import GitHash (giCommitDate, giHash, tGitInfoCwd)
import Log (Logger)
import Utils
import Data.UUID qualified as UUID
import Lucid (Html)
import Servant.API.UVerb
import Servant.Htmx
import Network.HTTP.Types (notFound404)
import Relude
import Servant (AuthProtect, Capture, Context (..), FormUrlEncoded, Get, Header, Headers, JSON, NoContent, Patch, PlainText, Post, QueryFlag, QueryParam, ReqBody, StdMethod (GET), Verb, (:>))
import Servant qualified
import Servant.API.Generic
import Servant.HTML.Lucid (HTML)
import Servant.Htmx (HXRedirect)
import Servant.Server.Generic (AsServerT)
import System.Config (AuthContext, EnvConfig)
import Web.Auth (APItoolkitAuthContext, authHandler)
import Web.Auth qualified as Auth
import Web.Error
import System.Types
import Web.Cookie (SetCookie)
import Data.Time (UTCTime)
import Pages.Projects.CreateProject qualified as CreateProject
import Pages.Projects.ListProjects qualified as ListProjects
import Pages.Projects.ManageMembers (ManageMembersForm)
import Pages.Projects.ManageMembers qualified as ManageMembers
import Pages.RedactedFields (RedactFieldForm)
import Pages.RedactedFields qualified as RedactedFields
import Pages.Log qualified as Log
import Pages.Reports qualified as Reports
import Pages.Share qualified as Share
import Pages.SlackInstall qualified as SlackInstall
import Pages.Onboarding qualified as Onboarding
import Pages.Survey qualified as Survey
import Pages.Anomalies.AnomalyList qualified as AnomalyList
import Models.Apis.Endpoints qualified as Endpoints;
import Models.Projects.Projects qualified as Projects;
import Models.Apis.Anomalies qualified as Anomalies;
import Pages.LogExplorer.LogItem  qualified as LogItem



type QPT a = QueryParam a Text
type GetRedirect = Verb 'GET 302


data Routes mode = Routes
  { assets :: mode :- "public" :> Servant.Raw
  , cookieProtected :: mode :- AuthProtect "optional-cookie-auth" :> Servant.NamedRoutes CookieProtectedRoutes
  , ping :: mode :- "ping" :> Get '[PlainText] Text
  , status :: mode :- "status" :> Get '[JSON] Status
  , login :: mode :- "login" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
  , toLogin :: mode :- "to_login" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
  , logout :: mode :- "logout" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
  , authCallback :: mode :- "auth_callback" :> QPT "code" :> QPT "state" :> GetRedirect '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] (Html ()))
  }
  deriving stock (Generic)


server
  :: Pool PG.Connection
  -> Routes (AsServerT ATBaseCtx)
server pool =
  Routes
    { assets = Servant.serveDirectoryWebApp "./static/public"
    , ping = pingH
    , status = statusH
    , login = Auth.loginH
    , toLogin = Auth.loginRedirectH
    , logout = Auth.logoutH
    , authCallback = Auth.authCallbackH
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


data CookieProtectedRoutes mode = CookieProtectedRoutes
  { projectListGet :: mode :-  UVerb 'GET '[HTML] (GetOrRedirect)
  , projectCreateGet :: mode :- "p" :> "new" :> Get '[HTML] (Html ()) -- p represents project
  , projectCreatePost :: mode :-   "p" :> "new" :> ReqBody '[FormUrlEncoded] CreateProject.CreateProjectForm :> Post '[HTML] (Headers '[HXTrigger, HXRedirect] (Html ()))
  , projectSettingsGet :: mode :-  "p" :> ProjectId :> "settings" :> Get '[HTML] (Html ())
  , projectDeleteGet :: mode :- "p" :> ProjectId :> "delete" :> Get '[HTML] (Headers '[HXTrigger, HXRedirect] (Html ()))
  , notificationsUpdateChannelPost :: mode :- "p" :> ProjectId :> "notifications-channels" :> ReqBody '[FormUrlEncoded] CreateProject.NotifListForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
  , membersManageGet :: mode :- "p" :> ProjectId :> "manage_members" :> Get '[HTML] (Html ()) 
  , membersManagePost :: mode :- "p" :> ProjectId :> "manage_members" :> ReqBody '[FormUrlEncoded] ManageMembersForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
  , onboardingGet :: mode :- "p" :> ProjectId :> "onboarding" :> QPB "polling" :> QPB "redirected" :> QPT "current_tab" :> Get '[HTML] (Html ())
  , logExplorerGet :: mode :- "p" :> ProjectId :> "log_explorer" :> QPT "query" :> QPT "cols" :> QPU "cursor" :> QPT "since" :> QPT "from" :> QPT "to" :> QPT "layout" :> HXRequest :> HXBoosted :> Get '[HTML] (Html ())
  , logExplorerItemGet :: mode :- "p" :> ProjectId :> "log_explorer" :> Capture "logItemID" UUID.UUID :> Capture "createdAt" UTCTime :> Get '[HTML] (Html ())
  , logExplorerItemDetailedGet :: mode :- "p" :> ProjectId :> "log_explorer" :> Capture "logItemID" UUID.UUID :> Capture "createdAt" UTCTime :> "detailed" :> Get '[HTML] (Html ())
     , anomalyAcknowlegeGet :: mode :-  "p" :> ProjectId :> "anomalies" :> Capture "anomalyID" Anomalies.AnomalyId :> "acknowlege" :> Get '[HTML] (Html ())
      , anomalyUnAcknowlegeGet :: mode :-  "p" :> ProjectId :> "anomalies" :> Capture "anomalyID" Anomalies.AnomalyId :> "unacknowlege" :> Get '[HTML] (Html ())
      , anomalyArchiveGet :: mode :-  "p" :> ProjectId :> "anomalies" :> Capture "anomalyID" Anomalies.AnomalyId :> "archive" :> Get '[HTML] (Html ())
      , anomalyUnarchiveGet :: mode :-  "p" :> ProjectId :> "anomalies" :> Capture "anomalyID" Anomalies.AnomalyId :> "unarchive" :> Get '[HTML] (Html ())
      , anomalyBulkActionsPost :: mode :-  "p" :> ProjectId :> "anomalies" :> "bulk_actions" :> Capture "action" Text :> ReqBody '[FormUrlEncoded] AnomalyList.AnomalyBulkForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
      , anomalyListGet :: mode :-  "p" :> ProjectId :> "anomalies" :> QPT "layout" :> QPT "ackd" :> QPT "archived" :> QPT "sort" :> QPT "page" :> QPT "load_more" :> QEID "endpoint" :> HXRequest :> HXBoosted :> Get '[HTML] (Html ())

  }
  deriving stock (Generic)


cookieProtectedServer :: Servant.ServerT (Servant.NamedRoutes CookieProtectedRoutes) ATAuthCtx 
cookieProtectedServer =
  CookieProtectedRoutes
    { projectListGet = ListProjects.listProjectsGetH 
      , projectCreateGet = CreateProject.createProjectGetH 
      , projectCreatePost = CreateProject.createProjectPostH
      , projectSettingsGet = CreateProject.projectSettingsGetH 
      , projectDeleteGet = CreateProject.deleteProjectGetH
      , notificationsUpdateChannelPost = CreateProject.updateNotificationsChannel 
      , membersManageGet = ManageMembers.manageMembersGetH
      , membersManagePost = ManageMembers.manageMembersPostH
      , onboardingGet = Onboarding.onboardingGetH
      , logExplorerGet = Log.apiLogH
      , logExplorerItemGet = LogItem.apiLogItemH 
      , logExplorerItemDetailedGet = LogItem.expandAPIlogItemH
      , anomalyAcknowlegeGet = AnomalyList.acknowlegeAnomalyGetH
      , anomalyUnAcknowlegeGet = AnomalyList.unAcknowlegeAnomalyGetH
      , anomalyArchiveGet = AnomalyList.archiveAnomalyGetH
      , anomalyUnarchiveGet = AnomalyList.unArchiveAnomalyGetH
      , anomalyBulkActionsPost = AnomalyList.anomalyBulkActionsPostH
      , anomalyListGet = AnomalyList.anomalyListGetH 
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
