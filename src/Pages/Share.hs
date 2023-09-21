module Pages.Share (ReqForm, shareLinkPostH, shareLinkGetH) where

import Config
import Data.Aeson as Aeson
import Data.Default (def)
import Data.Text
import Data.Time (ZonedTime, getZonedTime)
import Data.UUID qualified as UUID

import Data.Vector qualified as V

import Lucid.Hyperscript (__)
import Models.Apis.RequestDumps qualified as RequestDumps

import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Lucid
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions

import Web.FormUrlEncoded (FromForm)

import Data.Aeson.QQ (aesonQQ)
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector (Vector)
import Data.Vector.Primitive (Vector (Vector))
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Simple hiding (execute, query)
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Transact (DBT)
import Gogol.Prelude (addHeader)
import Pages.BodyWrapper (BWConfig, bodyWrapper, currProject, pageTitle, sessM)
import Pages.Log qualified as Log

import Relude
import Servant (Headers)
import Servant.Htmx (HXTrigger)

data ReqForm = ReqForm
  { expiresIn :: Text
  , reqId :: UUID.UUID
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)

data Swagger = Swagger
  { id :: UUID.UUID
  , projectId :: Projects.ProjectId
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  , swaggerJson :: Value
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "swagger_jsons", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Swagger)

shareLinkPostH :: Sessions.PersistentSession -> Projects.ProjectId -> ReqForm -> DashboardM (Headers '[HXTrigger] (Html ()))
shareLinkPostH sess pid reqForm = do
  pool <- asks pool
  currentTime <- liftIO getZonedTime
  let rid = reqForm.reqId
  let expIn = reqForm.expiresIn
  let lis = ["1 hour", "2 hours", "8 hours"] :: [Text]
  if Relude.elem expIn lis
    then do
      inId <- liftIO UUIDV4.nextRandom
      res <- liftIO $ withPool pool $ execute Insert [sql| INSERT INTO apis.share_requests VALUES (?,?,?,?, current_timestamp + interval ?,?) |] (inId, pid, currentTime, currentTime, expIn, rid)
      pure $ addHeader "" $ copyLink $ show inId
    else do
      let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"closeModal": "","successToast": ["Thanks for taking the survey"]}|]
      pure $ addHeader hxTriggerData $ div_ [class_ "mt-3"] ""

copyLink :: Text -> Html ()
copyLink rid = do
  let url = "https://app.apitoolkit.io/share/r/" <> rid
  div_ [class_ "flex gap-2 items-center"] $ do
    div_ [class_ "mt-2 text-sm text-green-700"] $ do
      p_ "Secure share url."
      strong_ [class_ "block pt-2 text-gray-500 truncate ...", id_ "shareURL"] $ toHtml url
    button_
      [ type_ "button"
      , class_ "self-end bg-green-500 px-2 py-1.5 text-white rounded-md text-sm font-medium text-green-800 hover:bg-green-300 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-green-50 focus:ring-green-600"
      , [__|
        on click
          if 'clipboard' in window.navigator then
            call navigator.clipboard.writeText(#shareURL's innerText)
            send successToast(value:['URL copied to clipboard']) to <body/>
          end
          |]
      ]
      "Copy URL"

shareLinkGetH :: UUID.UUID -> DashboardM (Html ())
shareLinkGetH sid = do
  pool <- asks pool
  res <- liftIO $ withPool pool $ getRequest (show sid)
  let req = if V.length res > 0 then Just $ V.head res else Nothing
  let bwconf =
        (def :: BWConfig)
          { sessM = Nothing
          , currProject = Nothing
          , pageTitle = "Data Seeding"
          }

  pure $ bodyWrapper bwconf $ sharePage req

sharePage :: Maybe RequestDumps.RequestDumpLogItem -> Html ()
sharePage req = do
  section_ [class_ "h-full bg-blue-500"] do
    case req of
      Just r -> Log.expandAPIlogItem' r
      Nothing -> div_ [] do
        h1_ [] "No Found"

getRequest :: Text -> DBT IO (V.Vector RequestDumps.RequestDumpLogItem)
getRequest rid = query Select q (Only rid)
 where
  q =
    [sql|
      SELECT
          rd.id AS id,
          rd.created_at AS created_at,
          rd.host AS host,
          rd.url_path AS urlPath,
          rd.method AS method,
          rd.raw_url AS rawUrl,
          rd.referer AS referer,
          rd.path_params AS path_params,
          rd.status_code AS status_code,
          rd.query_params AS query_params,
          rd.request_body AS request_body,
          rd.response_body AS response_body,
          rd.request_headers AS request_headers,
          rd.response_headers AS response_headers,
          COUNT(*) OVER() AS full_count,
          rd.duration_ns AS duration_ns,
          rd.sdk_type AS sdk_type
      FROM apis.share_requests AS sr
      JOIN apis.request_dumps AS rd ON sr.request_dump_id = rd.id
      WHERE sr.id = ? AND sr.expired_at > current_timestamp;
    |]
