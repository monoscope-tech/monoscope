module Pages.Share (ReqForm (..), shareLinkPostH, shareLinkGetH, ShareLinkGet (..), ShareLinkPost (..)) where

import Data.Default (def)
import Data.Text ()
import Data.Time (UTCTime, getZonedTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Database.PostgreSQL.Entity.DBT (QueryNature (Insert, Select), execute, queryOne)
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Lucid
import Lucid.Hyperscript (__)
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (BWConfig, PageCtx (..), currProject, pageTitle, sessM)
import Pages.LogExplorer.LogItem qualified as LogItem
import Pkg.Components (navBar)
import Relude
import System.Types (ATAuthCtx, ATBaseCtx, RespHeaders, addErrorToast, addRespHeaders)
import Web.FormUrlEncoded (FromForm)


data ReqForm = ReqForm
  { expiresIn :: Text
  , reqId :: UUID.UUID
  , reqCreatedAt :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)


shareLinkPostH :: Projects.ProjectId -> ReqForm -> ATAuthCtx (RespHeaders ShareLinkPost)
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
      addRespHeaders $ ShareLinkPost $ show inId
    else do
      addErrorToast "Invalid expiry interval" Nothing
      addRespHeaders ShareLinkPostError


data ShareLinkPost
  = ShareLinkPost Text
  | ShareLinkPostError


instance ToHtml ShareLinkPost where
  toHtml (ShareLinkPost shareId) = toHtml $ copyLink shareId
  toHtml ShareLinkPostError = toHtml ""
  toHtmlRaw = toHtml


copyLink :: Text -> Html ()
copyLink rid = do
  let url = "https://app.apitoolkit.io/share/r/" <> rid
  div_ [class_ "flex gap-2 items-center"] do
    div_ [class_ "mt-2  text-green-700"] do
      p_ "Secure share url."
      strong_ [class_ "block pt-2 text-gray-500 truncate ...", id_ "shareURL"] $ toHtml url
    button_
      [ type_ "button"
      , class_ "self-end bg-green-500 px-2 py-1.5 text-white rounded-md  font-medium text-green-800 hover:bg-green-300 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-green-50 focus:ring-green-600"
      , [__|
        on click
          if 'clipboard' in window.navigator then
            call navigator.clipboard.writeText(#shareURL's innerText)
            send successToast(value:['URL copied to clipboard']) to <body/>
          end
          |]
      ]
      "Copy URL"


shareLinkGetH :: UUID.UUID -> ATBaseCtx ShareLinkGet
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
  pure $ ShareLinkGet $ PageCtx bwconf reqM


data ShareLinkGet = ShareLinkGet (PageCtx (Maybe RequestDumps.RequestDumpLogItem))


instance ToHtml ShareLinkGet where
  toHtml (ShareLinkGet (PageCtx conf reqM)) = toHtml $ PageCtx conf $ sharePage reqM
  toHtmlRaw = toHtml


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
