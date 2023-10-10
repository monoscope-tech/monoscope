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

import NeatInterpolation
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
  let lis = ["1 hour", "8 hours", "1 day"] :: [Text]
  if Relude.elem expIn lis
    then do
      inId <- liftIO UUIDV4.nextRandom
      res <- liftIO $ withPool pool $ execute Insert [sql| INSERT INTO apis.share_requests VALUES (?,?,?,?, current_timestamp + interval ?,?) |] (inId, pid, currentTime, currentTime, expIn, rid)
      pure $ addHeader "" $ copyLink $ show inId
    else do
      let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"closeModal": "","errorToast": ["Invalid expiry interval"]}|]
      pure $ addHeader hxTriggerData $ getShareLink rid


copyLink :: Text -> Html ()
copyLink rid = do
  let url = "https://app.apitoolkit.io/share/r/" <> rid
  div_ [class_ "flex gap-2 items-center"] do
    div_ [class_ "mt-2 text-sm text-green-700"] do
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
          , pageTitle = "Share request log"
          }

  pure $ bodyWrapper bwconf $ sharePage req


sharePage :: Maybe RequestDumps.RequestDumpLogItem -> Html ()
sharePage req = do
  nav_ [id_ "main-navbar", class_ "fixed z-20 top-0 w-full w-full px-6 py-4 border-b bg-white flex flex-row justify-between"] do
    div_ [class_ "flex justify-between items-center gap-4 w-[1000px] mx-auto"] do
      a_ [href_ "https://apitoolkit.io", class_ "flex items-center text-gray-500 hover:text-gray-700"] do
        img_
          [ class_ "h-12 sd-hidden"
          , src_ "/assets/svgs/logo.svg"
          ]
        img_
          [ class_ "h-12 w-10 hidden sd-show"
          , src_ "/assets/svgs/logo_mini.svg"
          ]
  section_ [class_ "h-full mt-[80px] w-[1000px] flex flex-col items-center mx-auto"] do
    h3_ [class_ "text-5xl text-left mb-16 w-full font-semibold my-8"] "Shared Request Log"
    case req of
      Just r -> Log.expandAPIlogItem' r False
      Nothing -> div_ [class_ "flex flex-col gap-4 mt-[80px] text-center"] do
        h1_ [class_ "font-bold text-3xl"] "Request Log Not Found"
        p_ [class_ "text-gray-500 text-xl"] "This shared request log URL does not exist or has expired"
  script_
    [text|
function changeTab(tabId, parent) {
  const p = document.getElementById(parent);
  const tabLinks = p.querySelectorAll('.sdk_tab');
  tabLinks.forEach(link => link.classList.remove('sdk_tab_active'));
  const clickedTabLink = document.getElementById(tabId);
  clickedTabLink.classList.add('sdk_tab_active')
  const tabContents = p.querySelectorAll('.sdk_tab_content');
  tabContents.forEach(content => {
    content.classList.add("hidden")
    content.classList.remove ("sdk_tab_content_active")
  });
  const tabContent = document.getElementById(tabId + '_json');
  tabContent.classList.remove("hidden")
  setTimeout(()=>{tabContent.classList.add("sdk_tab_content_active")},10)
}

function toggleExpireOptions (event) {
    event.preventDefault()
    event.stopPropagation()
    const container = document.querySelector('#expire_container')
    if(container) {
     container.classList.toggle('hidden')
    }
}

function getShareLink(event) {
  const reqId = event.target.getAttribute ("data-req-id")
  document.querySelector('#req_id_input').value = reqId
  htmx.trigger('#share_log_form','submit')
}

function expireChanged(event) {
    event.preventDefault()
    event.stopPropagation()
    const current = document.querySelector('#toggle_expires_btn')
    if(current && current.firstChild) {
       current.firstChild.innerText = "Expires in: " + event.target.getAttribute("data-expire-value")
       document.querySelector("#expire_input").value = event.target.getAttribute("data-expire-value")
    }
}
  |]
  style_
    [text|
    .tree-children {
      display: block;
    }
    .tree-children-count { display: none; }
    .collapsed .tree-children {
      display: none !important; 
    }
    .collapsed .tree-children-count {display: inline !important;}
    .collapsed .children {display: inline-block; padding-left:0}
    .collapsed .closing-token {padding-left:0}
  |]


getRequest :: Text -> DBT IO (V.Vector RequestDumps.RequestDumpLogItem)
getRequest sid = query Select q (Only sid)
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
          rd.sdk_type AS sdk_type,
          rd.parent_id AS parent_id,
          rd.service_version as service_version,
          rd.errors AS errors,
          rd.tags AS tags
          rd.request_type as request_type,
      FROM apis.share_requests AS sr
      JOIN apis.request_dumps AS rd ON sr.request_dump_id = rd.id
      WHERE sr.id = ? AND sr.expired_at > current_timestamp;
    |]


getShareLink :: UUID.UUID -> Html ()
getShareLink rid = do
  div_ [class_ "relative", style_ "width:150px", onblur_ "document.getElementById('expire_container').classList.add('hidden')"] do
    button_
      [ onclick_ "toggleExpireOptions(event)"
      , id_ "toggle_expires_btn"
      , class_ "w-full flex gap-2 text-gray-600 justify_between items-center cursor-pointer px-2 py-1 border rounded focus:ring-2 focus:ring-blue-200 active:ring-2 active:ring-blue-200"
      ]
      do
        p_ [style_ "width: calc(100% - 25px)", class_ "text-sm truncate ..."] "Expires in: 1 hour"
        img_ [src_ "/assets/svgs/select_chevron.svg", style_ "height:15px; width:15px"]
    div_ [id_ "expire_container", class_ "absolute hidden bg-white border shadow w-full overflow-y-auto", style_ "top:100%; max-height: 300px; z-index:9"] do
      ["1 hour", "8 hours", "1 day"] & mapM_ \sw -> do
        button_
          [ onclick_ "expireChanged(event)"
          , term "data-expire-value" sw
          , class_ "p-2 w-full text-left truncate ... hover:bg-blue-100 hover:text-black"
          ]
          $ toHtml sw
  button_
    [ class_ "flex flex-col gap-1 bg-blue-500 px-2 py-1 rounded text-white"
    , term "data-req-id" (show rid)
    , onclick_ "getShareLink(event)"
    ]
    "Get share link"
