module Pages.Share (ReqForm, shareLinkPostH, shareLinkGetH) where

import Data.Aeson as Aeson
import Data.Aeson.QQ (aesonQQ)
import Data.Default (def)
import Data.Text ()
import Data.Time (UTCTime, ZonedTime, getZonedTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple hiding (execute, query)
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Transact (DBT)
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Reader.Static (ask, asks)
import Gogol.Prelude (addHeader)
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation
import Network.URI (escapeURIString, isUnescapedInURI, isUnreserved)
import Pages.BodyWrapper (BWConfig, bodyWrapper, currProject, pageTitle, sessM)
import Pages.LogExplorer.LogItem qualified as LogItem
import Pkg.Components (navBar)
import PyF
import Relude hiding (ask, asks)
import Relude.Unsafe qualified as Unsafe
import Servant (Headers)
import Servant.Htmx (HXTrigger)
import System.Config
import System.Types
import Web.FormUrlEncoded (FromForm)


data ReqForm = ReqForm
  { expiresIn :: Text
  , reqId :: UUID.UUID
  , reqCreatedAt :: UTCTime
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


shareLinkPostH :: Projects.ProjectId -> ReqForm -> ATAuthCtx (Headers '[HXTrigger] (Html ()))
shareLinkPostH pid reqForm = do
  currentTime <- liftIO getZonedTime
  let rid = reqForm.reqId
  let expIn = reqForm.expiresIn
  let lis = ["1 hour", "8 hours", "1 day"] :: [Text]
  if expIn `elem` lis
    then do
      inId <- liftIO UUIDV4.nextRandom
      res <-
        dbtToEff
          $ execute
            Insert
            [sql| INSERT INTO apis.share_requests (id, project_id, expired_at, request_dump_id, request_created_at) 
                                  VALUES (?,?, current_timestamp + interval ?,?,?) |]
            (inId, pid, expIn, rid, reqForm.reqCreatedAt)
      pure $ addHeader "" $ copyLink $ show inId
    else do
      let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"closeModal": "","errorToast": ["Invalid expiry interval"]}|]
      pure $ addHeader hxTriggerData ""


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


shareLinkGetH :: UUID.UUID -> ATBaseCtx (Html ())
shareLinkGetH sid = do
  -- FIXME: handle errors
  reqM <- dbtToEff $ do
    Just (createdAt, rId, pId) <- queryOne Select [sql|SELECT request_created_at, request_dump_id, project_id FROM apis.share_requests where id=? limit 1|] (Only sid)
    RequestDumps.selectRequestDumpByProjectAndId pId createdAt rId
  let bwconf =
        (def :: BWConfig)
          { sessM = Nothing
          , currProject = Nothing
          , pageTitle = "Share request log"
          }
  pure $ bodyWrapper bwconf $ sharePage reqM


sharePage :: Maybe RequestDumps.RequestDumpLogItem -> Html ()
sharePage req = do
  navBar
  section_ [class_ "h-full mt-[80px] w-[1000px] flex flex-col items-center mx-auto"] do
    h3_ [class_ "text-5xl text-left mb-16 w-full font-semibold my-8"] "Shared Request Log"
    case req of
      Just r -> LogItem.expandAPIlogItem' r.projectId r False
      Nothing -> div_ [class_ "flex flex-col gap-4 mt-[80px] text-center"] do
        h1_ [class_ "font-bold text-3xl"] "Request Log Not Found"
        p_ [class_ "text-gray-500 text-xl"] "This shared request log URL does not exist or has expired"
