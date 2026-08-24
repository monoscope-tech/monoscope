module Pages.Share (shareLinkPostH, shareLinkGetH, shareReplaySessionGetH, ShareLinkGet (..), ShareLinkPost (..)) where

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
import Models.Apis.ShareEvents qualified as ShareEvents
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Components (EmptyStateAction (..), EmptyStateCfg (..), ModalCfg (..), emptyState_, modalWith_)
import Pages.LogExplorer.LogItem qualified as LogItem
import Pages.Replay qualified as Replay
import Pages.Telemetry qualified as PTelemetry
import Pkg.DeriveUtils (unAesonTextMaybe)
import Relude
import Servant (err404)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, ATBaseCtx, RespHeaders, addRespHeaders)
import UnliftIO.Exception (throwIO)


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


shareLinkPostH :: Projects.ProjectId -> UUID.UUID -> UTCTime -> Maybe Text -> ATAuthCtx (RespHeaders ShareLinkPost)
shareLinkPostH pid eventId createdAt reqTypeM = do
  _ <- Projects.sessionAndProject pid
  _ <- Telemetry.otelRecordByProjectAndId pid createdAt eventId `whenNothingM` throwIO err404
  shareId <- liftIO UUIDV4.nextRandom
  ShareEvents.createShareLink shareId pid eventId (fromMaybe "request" reqTypeM) createdAt
  addRespHeaders $ ShareLinkPost $ UUID.toText shareId


newtype ShareLinkPost = ShareLinkPost Text


instance ToHtml ShareLinkPost where
  toHtml (ShareLinkPost rid) = toHtml @(Html ()) $ div_ [id_ "invite-modal-container"] do
    modalWith_ "shareModal" def{autoOpen = True, boxClass = "max-w-md p-6"} Nothing do
      h3_ [class_ "text-textStrong text-lg font-semibold"] "Share link"
      p_ [class_ "text-textWeak text-sm"] "Anyone with this link can view the event. Expires in 48 hours."
      div_ [class_ "flex items-center gap-2 pt-2"] do
        input_
          [ type_ "text"
          , readonly_ "readonly"
          , id_ "shareURL"
          , value_ $ "https://app.monoscope.tech/share/r/" <> rid
          , class_ "flex-1 min-w-0 bg-fillWeaker border border-strokeWeak rounded-md px-3 py-1.5 font-mono text-xs text-textWeak cursor-text transition-colors focus:outline-hidden focus:border-strokeBrand-strong"
          , onfocus_ "this.select()"
          ]
        button_
          [ type_ "button"
          , class_ "shrink-0 bg-fillSuccess-weak text-textSuccess px-3 py-1.5 rounded-md text-sm font-medium hover:bg-fillSuccess-strong hover:text-white transition-colors focus-visible:outline-hidden focus-visible:ring-2 focus-visible:ring-strokeBrand-strong focus-visible:ring-offset-2"
          , [__|
             on click
               if 'clipboard' in window.navigator then
                 call navigator.clipboard.writeText(#shareURL.value)
                 send successToast(value:['URL copied to clipboard']) to <body/>
               end
               |]
          ]
          "Copy link"
  toHtmlRaw = toHtml


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
resolveShare sid now =
  Hasql.interpOne
    [HI.sql|SELECT project_id, event_id, event_type, event_created_at, created_at
            FROM apis.share_events WHERE id=#{sid} LIMIT 1|]
    <&> fmap \(pid, eid, ty :: Text, eca, createdAt :: UTCTime) ->
      ShareRow pid eid ty eca (ceiling (diffUTCTime (addUTCTime (48 * 3600) createdAt) now / 3600))


shareLinkGetH :: UUID.UUID -> ATBaseCtx ShareLinkGet
shareLinkGetH sid = do
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  now <- Time.currentTime
  view <-
    resolveShare sid now >>= \case
      Nothing -> pure ShareMissing
      Just row | row.hoursLeft <= 0 -> pure ShareExpired
      Just row -> resolveBody sid now row
  pure $ ShareLinkGet $ PageCtx (def :: BWConfig){pageTitle = "Shared event", config = authCtx.config} view


resolveBody :: UUID.UUID -> UTCTime -> ShareRow -> ATBaseCtx ShareView
resolveBody sid now row =
  Telemetry.otelRecordByProjectAndId row.pid row.eventCreatedAt row.eventId >>= \case
    Nothing -> pure ShareMissing
    Just anchor -> do
      let detail = LogItem.expandedItemView row.pid anchor Nothing Nothing
      if row.eventType == "log"
        then pure $ ShareLive row.hoursLeft Nothing detail Nothing
        else do
          useTf <- (.env.enableTimefusionReads) <$> Effectful.Reader.Static.ask @AuthContext
          breakdownM <- runMaybeT do
            tid <- hoistMaybe $ anchor.context >>= (.trace_id) >>= guarded (not . T.null)
            (traceItem, spans) <- MaybeT $ Telemetry.getTraceDetails useTf row.pid tid (Just row.eventCreatedAt) now
            pure $ PTelemetry.tracePage row.pid traceItem (V.mapMaybe Telemetry.convertOtelLogsAndSpansToSpanRecord (V.fromList spans)) Nothing
          let replayInfo =
                Telemetry.atMapText "session.id" (unAesonTextMaybe anchor.attributes)
                  >>= UUID.fromText
                  <&> (UUID.toText sid,row.pid.toText,)
          pure $ ShareLive row.hoursLeft breakdownM detail replayInfo


newtype ShareLinkGet = ShareLinkGet (PageCtx ShareView)


instance ToHtml ShareLinkGet where
  toHtml (ShareLinkGet (PageCtx conf v)) = toHtml $ PageCtx conf $ sharePage v
  toHtmlRaw = toHtml


-- | Share-scoped replay fetcher. Grants access to the replay session iff the
-- anchor span attached to the share id has a matching `session.id` attribute
-- and the share link is unexpired. Rejects with 404 otherwise so enumeration
-- of project session ids via a leaked share id stays impossible.
shareReplaySessionGetH :: UUID.UUID -> UUID.UUID -> ATBaseCtx Replay.ReplaySessionResp
shareReplaySessionGetH sid sessionId = do
  now <- Time.currentTime
  row <- resolveShare sid now `whenNothingM` throwIO err404
  when (row.hoursLeft <= 0 || row.eventType == "log") $ throwIO err404
  anchor <- Telemetry.otelRecordByProjectAndId row.pid row.eventCreatedAt row.eventId `whenNothingM` throwIO err404
  when ((Telemetry.atMapText "session.id" (unAesonTextMaybe anchor.attributes) >>= UUID.fromText) /= Just sessionId) $ throwIO err404
  project <- Projects.projectById row.pid `whenNothingM` throwIO err404
  Replay.fetchReplaySession project sessionId


sharePage :: ShareView -> Html ()
sharePage v = do
  -- Hide embedded controls that don't apply on a standalone share page
  -- (sidebar close, internal share button marked data-share-hide).
  style_ ".share-view .detail-close-btn,.share-view [data-share-hide]{display:none!important}.share-view .urlPath{user-select:text}"
  shareTopBar case v of
    ShareLive{hoursLeft} -> Just hoursLeft
    ShareExpired -> Nothing
    ShareMissing -> Nothing
  section_ [class_ "share-view max-w-6xl mx-auto w-full px-4 pt-6 flex flex-col gap-6"] $ case v of
    ShareLive{breakdown, detail, replay} -> do
      whenJust breakdown $ div_ [class_ "border border-strokeWeak rounded-lg bg-bgBase"]
      whenJust replay \(shareId, projectId, sessionId) ->
        div_ [class_ "flex flex-col gap-2"] do
          div_ [class_ "flex items-center gap-2 px-1"] do
            span_ [class_ "text-xs uppercase tracking-wider text-textWeak font-medium"] "Session replay"
            span_ [class_ "text-xs text-textWeak truncate"] $ toHtml $ UUID.toText sessionId
          termWith
            "session-replay"
            [ term "projectId" projectId
            , term "initialSession" (UUID.toText sessionId)
            , term "sessionUrl" $ "/share/r/" <> shareId <> "/replay_session/" <> UUID.toText sessionId
            , term "hideControls" "1"
            , class_ "block"
            ]
            pass
      div_ [id_ "share-detail-inner", class_ "border border-strokeWeak rounded-lg bg-bgBase overflow-hidden"] detail
    ShareExpired -> blank "clock" "Link expired" "This share link was valid for 48 hours and has passed its expiry. Ask the sender for a fresh link."
    ShareMissing -> blank "empty" "Event not found" "This share link doesn't exist or the underlying event is no longer available."
  where
    blank i = emptyState_ def{icon = Just i, action = ESLink "https://monoscope.tech" "Learn about Monoscope"}


-- | Slim sticky top bar: logo, "Shared event" label, expiry pill, About link.
shareTopBar :: Maybe Int -> Html ()
shareTopBar hoursLeftM =
  nav_ [class_ "sticky top-0 z-20 h-12 w-full border-b border-strokeWeak bg-bgBase/90 backdrop-blur-sm"] do
    div_ [class_ "max-w-6xl mx-auto h-full px-4 flex items-center justify-between gap-4 flex-nowrap"] do
      div_ [class_ "flex items-center gap-3 min-w-0 flex-nowrap"] do
        a_ [href_ "https://monoscope.tech", target_ "_blank", class_ "flex items-center shrink-0"] do
          img_ [class_ "h-5 w-auto dark:hidden", src_ "/public/assets/svgs/logo_black.svg"]
          img_ [class_ "h-5 w-auto hidden dark:block", src_ "/public/assets/svgs/logo_white.svg"]
        span_ [class_ "hidden sm:inline-block h-4 w-px bg-strokeWeak shrink-0"] ""
        span_ [class_ "text-2xs uppercase tracking-wider text-textWeak font-medium whitespace-nowrap shrink-0"] "Shared event"
        whenJust hoursLeftM \h -> do
          let cls = bool "text-textWeak border-strokeWeak bg-fillWeaker" "text-textError border-strokeError-strong/40 bg-fillError-weak" (h <= 6) :: Text
          span_ [class_ $ "text-2xs px-2 py-0.5 rounded-full border whitespace-nowrap shrink-0 " <> cls]
            $ toHtml @Text ("Expires in " <> show h <> "h")
      a_
        [href_ "https://monoscope.tech", target_ "_blank", class_ "text-xs font-medium text-textBrand hover:underline shrink-0 whitespace-nowrap"]
        "About Monoscope ↗"
