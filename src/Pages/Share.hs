module Pages.Share (ReqForm (..), shareLinkPostH, shareLinkGetH, shareReplaySessionGetH, ShareLinkGet (..), ShareLinkPost (..)) where

import Data.Aeson qualified as AE
import Data.Default (def)
import Data.Effectful.Hasql qualified as Hasql
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Effectful.Reader.Static qualified
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Lucid
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Components (ModalCfg (..), emptyState_, modalWith_)
import Pages.LogExplorer.LogItem qualified as LogItem
import Pages.Replay qualified as Replay
import Pages.Telemetry qualified as PTelemetry
import Pkg.DeriveUtils (unAesonTextMaybe)
import Relude
import Servant (err404)
import System.Config (AuthContext (..))
import System.Types (ATAuthCtx, ATBaseCtx, RespHeaders, addRespHeaders)
import UnliftIO.Exception (throwIO)
import Web.FormUrlEncoded (FromForm)


-- | Result of resolving a share id: missing entirely, expired, or live with hours-remaining + body.
data ShareView
  = ShareMissing
  | ShareExpired
  | ShareLive
      { hoursLeft :: !Int
      , breakdown :: !(Maybe (Html ())) -- full Timeline/Waterfall/Services trace breakdown when this is a trace
      , detail :: !(Html ()) -- LogItem.expandedItemView; its built-in header supplies the event summary
      , replay :: !(Maybe (Text, Text, UUID.UUID)) -- (shareId, projectId, sessionId) for the session-replay player
      }
  deriving stock (Generic)


data ReqForm = ReqForm
  { expiresIn :: Text
  , reqId :: UUID.UUID
  , reqCreatedAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


shareLinkPostH :: Projects.ProjectId -> UUID.UUID -> UTCTime -> Maybe Text -> ATAuthCtx (RespHeaders ShareLinkPost)
shareLinkPostH pid eventId createdAt reqTypeM = do
  _ <- Projects.sessionAndProject pid
  let eventType = fromMaybe "request" reqTypeM
  shareId <- liftIO UUIDV4.nextRandom
  void
    $ Hasql.interpExecute
      [HI.sql| INSERT INTO apis.share_events (id, project_id, event_id, event_type, event_created_at)
                              VALUES (#{shareId},#{pid},#{eventId},#{eventType},#{createdAt}) |]
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
  let url = "https://app.monoscope.tech/share/r/" <> rid
  div_ [id_ "invite-modal-container"] do
    modalWith_ "shareModal" def{autoOpen = True, boxClass = "max-w-md p-6"} Nothing do
      h3_ [class_ "text-textStrong text-lg font-semibold"] "Share link"
      p_ [class_ "text-textWeak text-sm"] "Anyone with this link can view the event. Expires in 48 hours."
      div_ [class_ "flex items-center gap-2 pt-2"] do
        input_
          [ type_ "text"
          , readonly_ "readonly"
          , id_ "shareURL"
          , value_ url
          , class_ "flex-1 min-w-0 bg-fillWeaker border border-strokeWeak rounded-md px-3 py-1.5 font-mono text-xs text-textWeak cursor-text transition-colors focus:outline-hidden focus:border-strokeBrand"
          , [__|on focus call my.select()|]
          ]
        button_
          [ type_ "button"
          , class_ "shrink-0 bg-fillSuccess-weak text-textSuccess px-3 py-1.5 rounded-md text-sm font-medium hover:bg-fillSuccess-strong hover:text-white transition-colors focus-visible:outline-hidden focus-visible:ring-2 focus-visible:ring-strokeBrand focus-visible:ring-offset-2"
          , [__|
             on click
               if 'clipboard' in window.navigator then
                 call navigator.clipboard.writeText(#shareURL.value)
                 send successToast(value:['URL copied to clipboard']) to <body/>
               end
               |]
          ]
          "Copy link"


-- | Resolved share row. Fetched without a SQL time filter so callers can
-- distinguish missing from expired.
data ShareRow = ShareRow
  { pid :: Projects.ProjectId
  , eventId :: UUID.UUID
  , eventType :: Text
  , eventCreatedAt :: UTCTime
  , hoursLeft :: Int
  }


resolveShare :: UUID.UUID -> UTCTime -> ATBaseCtx (Maybe ShareRow)
resolveShare sid now = do
  rowM <-
    Hasql.interpOne
      [HI.sql|SELECT project_id, event_id, event_type, event_created_at, created_at
              FROM apis.share_events WHERE id=#{sid} LIMIT 1|]
  pure $ rowM <&> \(pid, eid, ty :: Text, eca, createdAt :: UTCTime) ->
    ShareRow pid eid ty eca (ceiling (diffUTCTime (addUTCTime (48 * 3600) createdAt) now / 3600))


-- | 404 helper.
note404 :: Maybe a -> ATBaseCtx a
note404 = maybe (throwIO err404) pure


shareLinkGetH :: UUID.UUID -> ATBaseCtx ShareLinkGet
shareLinkGetH sid = do
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  now <- Time.currentTime
  view <-
    resolveShare sid now >>= \case
      Nothing -> pure ShareMissing
      Just row | row.hoursLeft <= 0 -> pure ShareExpired
      Just row -> resolveBody sid now row
  let bwconf =
        (def :: BWConfig)
          { sessM = Nothing
          , currProject = Nothing
          , pageTitle = "Shared event"
          , config = authCtx.config
          }
  pure $ ShareLinkGet $ PageCtx bwconf view


resolveBody :: UUID.UUID -> UTCTime -> ShareRow -> ATBaseCtx ShareView
resolveBody _ _ row | row.eventType == "log" = do
  Telemetry.logRecordByProjectAndId row.pid row.eventCreatedAt row.eventId <&> \case
    Just req -> ShareLive row.hoursLeft Nothing (LogItem.expandedItemView row.pid req Nothing Nothing Nothing) Nothing
    Nothing -> ShareMissing
resolveBody sid now row = do
  Telemetry.spanRecordByProjectAndId row.pid row.eventCreatedAt row.eventId >>= \case
    Nothing -> pure ShareMissing
    Just anchor -> do
      breakdownM <- runMaybeT do
        tid <- hoistMaybe $ anchor.context >>= (.trace_id) >>= guarded (not . T.null)
        traceItem <- MaybeT $ Telemetry.getTraceDetails row.pid tid (Just row.eventCreatedAt) now
        spans <- lift $ Telemetry.getSpanRecordsByTraceId row.pid tid (Just row.eventCreatedAt) now
        let recs = V.mapMaybe Telemetry.convertOtelLogsAndSpansToSpanRecord (V.fromList spans)
        pure $ PTelemetry.tracePage row.pid traceItem recs
      let replayInfo =
            Telemetry.atMapText "session.id" (unAesonTextMaybe anchor.attributes)
              >>= UUID.fromText
              <&> \s -> (UUID.toText sid, row.pid.toText, s)
      pure
        $ ShareLive
          row.hoursLeft
          breakdownM
          (LogItem.expandedItemView row.pid anchor Nothing Nothing Nothing)
          replayInfo


newtype ShareLinkGet = ShareLinkGet (PageCtx ShareView)


instance ToHtml ShareLinkGet where
  toHtml (ShareLinkGet (PageCtx conf v)) = toHtml $ PageCtx conf $ sharePage v
  toHtmlRaw = toHtml


-- | Share-scoped replay fetcher. Grants access to the replay session iff the
-- anchor span attached to the share id has a matching `session.id` attribute
-- and the share link is unexpired. Rejects with 404 otherwise so enumeration
-- of project session ids via a leaked share id stays impossible.
shareReplaySessionGetH :: UUID.UUID -> UUID.UUID -> ATBaseCtx AE.Value
shareReplaySessionGetH sid sessionId = do
  now <- Time.currentTime
  row <- resolveShare sid now >>= note404
  when (row.hoursLeft <= 0 || row.eventType == "log") $ throwIO err404
  anchor <- Telemetry.spanRecordByProjectAndId row.pid row.eventCreatedAt row.eventId >>= note404
  let attrSessionId = Telemetry.atMapText "session.id" (unAesonTextMaybe anchor.attributes) >>= UUID.fromText
  when (attrSessionId /= Just sessionId) $ throwIO err404
  project <- Projects.projectById row.pid >>= note404
  Replay.fetchReplaySession project sessionId


-- | Hide embedded controls that don't apply on a standalone share page
-- (sidebar close, internal share button marked data-share-hide).
shareViewStyles :: Html ()
shareViewStyles = style_ ".share-view .detail-close-btn,.share-view [data-share-hide]{display:none!important}.share-view .urlPath{user-select:text}"


sharePage :: ShareView -> Html ()
sharePage v = do
  shareViewStyles
  shareTopBar $ case v of
    ShareLive{hoursLeft} -> Just hoursLeft
    _ -> Nothing
  section_ [class_ "share-view max-w-6xl mx-auto w-full px-4 pt-6 flex flex-col gap-6"] $ case v of
    ShareLive{breakdown, detail, replay} -> do
      whenJust breakdown $ \bd -> div_ [class_ "border border-strokeWeak rounded-lg bg-bgBase"] bd
      whenJust replay $ \(shareId, projectId, sessionId) -> do
        let replayUrl = "/share/r/" <> shareId <> "/replay_session/" <> UUID.toText sessionId
        div_ [class_ "flex flex-col gap-2"] do
          div_ [class_ "flex items-center gap-2 px-1"] do
            span_ [class_ "text-xs uppercase tracking-wider text-textWeak font-medium"] "Session replay"
            span_ [class_ "text-xs text-textWeak truncate"] $ toHtml $ UUID.toText sessionId
          termWith
            "session-replay"
            [ term "projectId" projectId
            , term "initialSession" (UUID.toText sessionId)
            , term "sessionUrl" replayUrl
            , term "hideControls" "1"
            , class_ "block"
            ]
            pass
      div_ [id_ "share-detail-inner", class_ "border border-strokeWeak rounded-lg bg-bgBase overflow-hidden"] detail
    ShareExpired -> emptyState_ (Just "clock") "Link expired" "This share link was valid for 48 hours and has passed its expiry. Ask the sender for a fresh link." (Just "https://monoscope.tech") "Learn about Monoscope"
    ShareMissing -> emptyState_ (Just "empty") "Event not found" "This share link doesn't exist or the underlying event is no longer available." (Just "https://monoscope.tech") "Learn about Monoscope"


-- | Slim sticky top bar: logo, "Shared event" label, expiry pill, About link.
shareTopBar :: Maybe Int -> Html ()
shareTopBar hoursLeftM = do
  nav_ [class_ "sticky top-0 z-20 h-12 w-full border-b border-strokeWeak bg-bgBase/90 backdrop-blur-sm"] do
    div_ [class_ "max-w-6xl mx-auto h-full px-4 flex items-center justify-between gap-4 flex-nowrap"] do
      div_ [class_ "flex items-center gap-3 min-w-0 flex-nowrap"] do
        a_ [href_ "https://monoscope.tech", target_ "_blank", class_ "flex items-center shrink-0"] do
          img_ [class_ "h-5 w-auto dark:hidden", src_ "/public/assets/svgs/logo_black.svg"]
          img_ [class_ "h-5 w-auto hidden dark:block", src_ "/public/assets/svgs/logo_white.svg"]
        span_ [class_ "hidden sm:inline-block h-4 w-px bg-strokeWeak shrink-0"] ""
        span_ [class_ "text-[11px] uppercase tracking-wider text-textWeak font-medium whitespace-nowrap shrink-0"] "Shared event"
        whenJust hoursLeftM \h -> do
          let cls = bool "text-textWeak border-strokeWeak bg-fillWeaker" "text-textError border-strokeError-strong/40 bg-fillError-weak" (h <= 6) :: Text
          span_ [class_ $ "text-[11px] px-2 py-0.5 rounded-full border whitespace-nowrap shrink-0 " <> cls]
            $ toHtml @Text ("Expires in " <> show h <> "h")
      a_
        [href_ "https://monoscope.tech", target_ "_blank", class_ "text-xs font-medium text-textBrand hover:underline shrink-0 whitespace-nowrap"]
        "About Monoscope ↗"
