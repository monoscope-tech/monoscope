module Pages.Share (ReqForm (..), shareLinkPostH, shareLinkGetH, ShareLinkGet (..), ShareLinkPost (..)) where

import Data.Aeson qualified as AE
import Data.Default (def)
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Database.PostgreSQL.Entity.DBT (execute, queryOne)
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Lucid
import Lucid.Hyperscript (__)
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Pages.BodyWrapper (BWConfig, PageCtx (..), currProject, pageTitle, sessM)
import Pages.LogExplorer.LogItem qualified as LogItem
import Pages.Telemetry.Spans qualified as Spans
import Pkg.Components (navBar)
import Relude
import System.Types (ATAuthCtx, ATBaseCtx, RespHeaders, addRespHeaders)
import Utils (faSprite_, jsonValueToHtmlTree)
import Web.FormUrlEncoded (FromForm)


data ReqForm = ReqForm
  { expiresIn :: Text
  , reqId :: UUID.UUID
  , reqCreatedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


shareLinkPostH :: Projects.ProjectId -> UUID.UUID -> UTCTime -> Maybe Text -> ATAuthCtx (RespHeaders ShareLinkPost)
shareLinkPostH pid eventId createdAt reqTypeM = do
  let eventType = fromMaybe "request" reqTypeM
  shareId <- liftIO UUIDV4.nextRandom
  res <-
    dbtToEff
      $ execute
        [sql| INSERT INTO apis.share_events (id, project_id, event_id, event_type, event_created_at)
                              VALUES (?,?,?,?,?) |]
        (shareId, pid, eventId, eventType, createdAt)
  addRespHeaders $ ShareLinkPost $ show shareId


data ShareLinkPost
  = ShareLinkPost Text
  | ShareLinkPostError


instance ToHtml ShareLinkPost where
  toHtml (ShareLinkPost shareId) = toHtml $ copyLink shareId
  toHtml ShareLinkPostError = toHtml ""
  toHtmlRaw = toHtml


copyLink :: Text -> Html ()
copyLink rid = do
  div_ [id_ "invite-modal-container"] $ do
    input_ [type_ "checkbox", id_ "shareModal", class_ "modal-toggle", checked_]
    div_ [class_ "modal p-8", role_ "dialog"] do
      div_ [class_ "modal-box flex flex-col gap-4"] $ do
        div_ [class_ "p-3 bg-[#0acc91]/5 rounded-full w-max border-[#067a57]/20 gap-2 inline-flex"]
          $ faSprite_ "copy" "regular" "h-6 w-6 text-green-500"
        span_ [class_ " text-textStrong text-2xl font-semibold"] "Copy Share Link"
        div_ [class_ "text-[#000833]/60"] "Share this link with anyone to give them access to this event. Lasts for 48 hours only."
        div_ [class_ "h-1 w-full  bg-fillWeak"] pass
        div_ [class_ "flex-col gap-6 flex"] $ do
          let url = "https://app.apitoolkit.io/share/r/" <> rid
          div_ [class_ "flex flex-col gap-2 items-center"] do
            div_ [class_ "mt-2  text-green-700"] do
              strong_ [class_ "block pt-2 text-textWeak text-xs truncate ...", id_ "shareURL"] $ toHtml url
          button_
            [ type_ "button"
            , class_ "self-start bg-green-500 px-2 py-1.5 text-white rounded-md text-sm font-medium hover:bg-green-300"
            , [__|
               on click
                 if 'clipboard' in window.navigator then
                   call navigator.clipboard.writeText(#shareURL's innerText)
                   send successToast(value:['URL copied to clipboard']) to <body/>
                 end
                 |]
            ]
            "Copy url"
      label_ [class_ "modal-backdrop", Lucid.for_ "shareModal"] "Close"


shareLinkGetH :: UUID.UUID -> ATBaseCtx ShareLinkGet
shareLinkGetH sid = do
  -- FIXME: handle errors
  r <- dbtToEff $ queryOne [sql|SELECT project_id, event_id, event_type, event_created_at FROM apis.share_events where id=? and created_at > current_timestamp - interval '48 hours' limit 1|] (Only sid)
  uiM <- do
    case r of
      Just (pid, eventId, eventType, createdAt) -> do
        case eventType of
          "span" -> do
            spanItem <- Telemetry.spanRecordByProjectAndId pid createdAt eventId
            pure case spanItem of
              Just spn -> Just $ Spans.expandedSpanItem pid spn Nothing Nothing Nothing
              Nothing -> Nothing
          "log" -> do
            logItem <- Telemetry.logRecordByProjectAndId pid createdAt eventId
            pure case logItem of
              Just req -> Just $ apiLogItemView pid (AE.toJSON req)
              Nothing -> Nothing
          _ -> do
            reqM <- dbtToEff $ RequestDumps.selectRequestDumpByProjectAndId pid createdAt eventId
            pure case reqM of
              Just rq -> Just $ LogItem.expandAPIlogItem' rq.projectId rq False
              Nothing -> Nothing
      Nothing -> pure Nothing

  let bwconf =
        (def :: BWConfig)
          { sessM = Nothing
          , currProject = Nothing
          , pageTitle = "Share request log"
          }
  pure $ ShareLinkGet $ PageCtx bwconf uiM


newtype ShareLinkGet = ShareLinkGet (PageCtx (Maybe (Html ())))


instance ToHtml ShareLinkGet where
  toHtml (ShareLinkGet (PageCtx conf reqM)) = toHtml $ PageCtx conf $ sharePage reqM
  toHtmlRaw = toHtml


sharePage :: Maybe (Html ()) -> Html ()
sharePage req = do
  navBar
  section_ [class_ "h-full mt-[80px] w-[1000px] flex flex-col items-center mx-auto"] do
    h3_ [class_ "text-3xl text-left mb-16 w-full font-semibold text-textStrong my-8"] "Shared event"
    case req of
      Just r -> r
      Nothing -> div_ [class_ "flex flex-col gap-4 mt-[80px] text-center"] do
        h1_ [class_ "font-bold text-3xl"] "Event Not Found"
        p_ [class_ "text-gray-500 text-xl"] "This shared request log URL does not exist or has expired"


apiLogItemView :: Projects.ProjectId -> AE.Value -> Html ()
apiLogItemView pid req = do
  let reqJson = decodeUtf8 $ AE.encode req
  button_
    [ class_ "btn btn-sm bg-base-100"
    , onclick_ "window.downloadJson(event)"
    , term "data-reqJson" reqJson
    ]
    (span_ [] "Download" >> faSprite_ "arrow-down-to-line" "regular" "h-3 w-3")
  jsonValueToHtmlTree req Nothing
