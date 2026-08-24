module Pages.Anomalies (
  anomalyListGetH,
  anomalyBulkActionsPostH,
  acknowledgeAnomalyGetH,
  archiveAnomalyGetH,
  anomalyDetailGetH,
  AnomalyBulkForm (..),
  AnomalyListGet (..),
  anomalyAcknowledgeButton,
  anomalyArchiveButton,
  anomalyDetailHashGetH,
  AnomalyAction (..),
  IssueVM (..),
  AssignErrorForm (..),
  assignErrorPostH,
  resolveErrorPostH,
  ErrorSubscriptionForm (..),
  errorSubscriptionPostH,
  -- AI Chat
  AIChatForm (..),
  aiChatPostH,
  aiChatHistoryGetH,
  -- Activity
  issueActivityGetH,
  -- Pattern group members
  errorGroupMembersGetH,
  errorUnmergePostH,
  -- Shared rendering helpers
  issueCardCompact_,
)
where

import BackgroundJobs qualified
import Data.Aeson qualified as AE
import Data.Aeson.Types (Parser, parseMaybe)
import Data.CaseInsensitive qualified as CI
import Data.Default (def)
import Data.Effectful.Hasql qualified as Hasql
import Data.HashMap.Strict qualified as HM
import Data.Map qualified as Map
import Data.Ord (clamp)
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, diffUTCTime, formatTime)
import Data.Time.Clock.POSIX qualified as POSIX
import Data.Time.LocalTime (ZonedTime, zonedTimeToUTC)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.Newtypes (Aeson (..), getAeson)
import Deriving.Aeson qualified as DAE
import Effectful.Concurrent.Async (concurrently)
import Effectful.Error.Static (throwError)
import Effectful.Exception (trySync)
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Effectful.Timeout (timeout)
import Hasql.Interpolate qualified as HI
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (TermRaw (termRaw), makeAttribute)
import Lucid.Htmx (hxGet_, hxIndicator_, hxPost_, hxSwap_, hxTarget_, hxTrigger_)
import Lucid.Hyperscript (__)
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.ErrorPatterns (ErrorPatternId (..))
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogPatterns (sourceFieldLabel)
import Models.Apis.Monitors qualified as Monitors
import Models.Apis.PatternMerge qualified as PatternMerge
import Models.Apis.SchemaCatalog qualified as SchemaCatalog
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects (User (id))
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Schema qualified as Schema
import Models.Telemetry.Telemetry qualified as Telemetry
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx, navTabAttrs)
import Pages.Charts.Charts qualified as Charts
import Pages.Components (EmptyStateAction (..), EmptyStateCfg (..), EmptyStateSize (..), colorChip_, compactTimeAgo, durationMenu_, durationQuery, emptyState_, metadataChip_, periodToggle_, resizer_, sparkline_, untilLabel)
import Pages.LogExplorer.Log (virtualTable)
import Pages.Telemetry (traceFragmentUrl)
import Pkg.AI qualified as AI
import Pkg.Components.Table (BulkAction (..), Column (..), Config (..), Features (..), FilterMenu (..), FilterOption (..), Pagination (..), SearchMode (..), TabFilter (..), TabFilterOpt (..), Table (..), TableHeaderActions (..), TableRows (..), ZeroState (..), col, withAttrs, withColHeaderExtra)
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..), assetUrl)
import Pkg.SchemaLearning.Catalog (FacetData (..), FacetSummary (..), FacetValue (..))
import PyF (fmt)
import Relude hiding (ask)
import Servant (err400, errBody)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Logging qualified as Log
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast, addTriggerEvent)
import Text.Time.Pretty (prettyTimeAuto)
import Utils (LoadingSize (..), LoadingType (..), checkFreeTierStatus, faSprite_, formatOffset, formatUTC, formatWithCommas, htmxOverlayIndicator_, loadingIndicator_, lookupValueText, renderMarkdown, toUriStr)
import Web.FormUrlEncoded (FromForm)


newtype AnomalyBulkForm = AnomalyBulk
  { itemId :: [Text]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


-- | Acknowledge (on=True) or un-acknowledge (on=False) an issue. @durationM@ is
-- the silence window in minutes; absent means indefinitely — until the issue
-- regresses or someone un-acks it. Acknowledging is the *only* way to stop
-- notifications for an issue, so it cascades to the underlying anomaly rows too.
acknowledgeAnomalyGetH :: Projects.ProjectId -> Bool -> Anomalies.AnomalyId -> Maybe Int -> ATAuthCtx (RespHeaders AnomalyAction)
acknowledgeAnomalyGetH pid enable aid durationM = do
  (sess, _) <- Projects.sessionAndProject pid
  now <- Time.currentTime
  let issueId = UUIDId aid.unUUIDId
      window = maybe Issues.AckIndefinite Issues.AckFor durationM
      until' = Issues.ackUntil now window
  ackState <-
    if enable
      then do
        void $ Issues.setAckState pid [issueId] $ Just Issues.AckSet{at = now, by = Just sess.user.id, window}
        Issues.logIssueActivity issueId Issues.IEAcknowledged (Just sess.user.id) (Just $ AE.object ["until" AE..= until'])
        hashes <- Anomalies.acknowledgeAnomalies sess.user.id until' (V.singleton (UUID.toText aid.unUUIDId))
        void $ Anomalies.acknowlegeCascade sess.user.id until' (V.fromList hashes)
        addSuccessToast (untilLabel "Acknowledged" now until' <> " \x2014 notifications paused") Nothing
        pure $ Just until'
      else do
        void $ Issues.setAckState pid [issueId] Nothing
        void $ Hasql.interpExecute [HI.sql| update apis.anomalies set acknowledged_by=null, acknowledged_at=null where id=#{aid} |]
        Issues.logIssueActivity issueId Issues.IEUnacknowledged (Just sess.user.id) Nothing
        addSuccessToast "Back in the Inbox \x2014 notifications resumed" Nothing
        pure Nothing
  addTriggerEvent "issuesListChanged" AE.Null
  addRespHeaders $ Acknowlege pid issueId now ackState


-- | Archive (on=True) or un-archive (on=False) an anomaly/issue.
archiveAnomalyGetH :: Projects.ProjectId -> Bool -> Anomalies.AnomalyId -> ATAuthCtx (RespHeaders AnomalyAction)
archiveAnomalyGetH pid enable aid = do
  (sess, _) <- Projects.sessionAndProject pid
  archivedAt <- if enable then Just <$> Time.currentTime else pure Nothing
  void $ Hasql.interpExecute [HI.sql| update apis.issues set archived_at=#{archivedAt} where id=#{aid} |]
  void $ Hasql.interpExecute [HI.sql| update apis.anomalies set archived_at=#{archivedAt} where id=#{aid} |]
  Issues.logIssueActivity (UUIDId aid.unUUIDId) (if enable then Issues.IEArchived else Issues.IEUnarchived) (Just sess.user.id) Nothing
  addSuccessToast (bool "Restored to the Inbox" "Archived \x2014 notifications stopped" enable) Nothing
  addTriggerEvent "issuesListChanged" AE.Null
  addRespHeaders $ Archive pid (UUIDId aid.unUUIDId) enable


data AnomalyAction
  = -- | @Just until@ when acknowledged, alongside the render clock.
    Acknowlege Projects.ProjectId Issues.IssueId UTCTime (Maybe UTCTime)
  | Archive Projects.ProjectId Issues.IssueId Bool
  | Bulk


instance ToHtml AnomalyAction where
  toHtml (Acknowlege pid aid now untilM) = toHtml $ anomalyAcknowledgeButton pid aid now untilM
  toHtml (Archive pid aid is_arch) = toHtml $ anomalyArchiveButton pid aid is_arch
  toHtml Bulk = ""
  toHtmlRaw = toHtml


-- | Bulk lifecycle transitions, triggering a toast and list reload. @duration@
-- (minutes) applies to @acknowledge@ only; absent acknowledges indefinitely.
anomalyBulkActionsPostH :: Projects.ProjectId -> Text -> Maybe Int -> AnomalyBulkForm -> ATAuthCtx (RespHeaders AnomalyAction)
anomalyBulkActionsPostH pid action durationM items = do
  (sess, _) <- Projects.sessionAndProject pid
  if null items.itemId
    then do
      addErrorToast "No items selected" Nothing
      addRespHeaders Bulk
    else do
      now <- Time.currentTime
      let vIds = V.fromList items.itemId
          issueIds = UUIDId <$> mapMaybe UUID.fromText items.itemId
          window = maybe Issues.AckIndefinite Issues.AckFor durationM
          until' = Issues.ackUntil now window
      (eventType, msg) <- case action of
        "acknowledge" -> do
          ths <- Anomalies.acknowledgeAnomalies sess.user.id until' vIds
          void $ Anomalies.acknowlegeCascade sess.user.id until' (V.fromList ths)
          pure (Issues.IEAcknowledged, untilLabel "Acknowledged" now until' <> " \x2014 notifications paused")
        "unacknowledge" -> do
          void $ Issues.setAckState pid issueIds Nothing
          pure (Issues.IEUnacknowledged, "Back in the Inbox \x2014 notifications resumed")
        "archive" -> do
          void $ Anomalies.archiveAnomaliesAndIssues vIds
          pure (Issues.IEArchived, "Archived \x2014 notifications stopped")
        "unarchive" -> do
          void $ Issues.setArchiveState pid issueIds Nothing
          pure (Issues.IEUnarchived, "Restored to the Inbox")
        _ -> throwError err400{errBody = "unhandled anomaly bulk action: " <> encodeUtf8 action}
      forM_ issueIds \u -> Issues.logIssueActivity u eventType (Just sess.user.id) Nothing
      addSuccessToast msg Nothing
      addTriggerEvent "issuesListChanged" AE.Null
      addRespHeaders Bulk


anomalyDetailGetH :: Projects.ProjectId -> Issues.IssueId -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
anomalyDetailGetH pid issueId firstM sinceM = anomalyDetailCore pid firstM sinceM \_ -> Issues.selectIssueById issueId


anomalyDetailHashGetH :: Projects.ProjectId -> Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
anomalyDetailHashGetH pid issueId firstM sinceM = anomalyDetailCore pid firstM sinceM \_ -> Issues.selectIssueByHash pid issueId


anomalyDetailCore :: Projects.ProjectId -> Maybe Text -> Maybe Text -> (Projects.ProjectId -> ATAuthCtx (Maybe Issues.Issue)) -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
anomalyDetailCore pid firstM sinceM fetchIssue = do
  (sess, project, bw) <- mkPageCtx pid
  issueM <- fetchIssue pid
  now <- Time.currentTime
  useTf <- (.env.enableTimefusionReads) <$> ask @AuthContext
  let baseBwconf = bw{pageTitle = "Issues", menuItem = Just "Issues"}
  case issueM of
    Nothing ->
      addRespHeaders
        $ PageCtx baseBwconf{pageTitle = "Issue Not Found"}
        $ emptyState_
          def{icon = Just "circle-xmark", action = ESLink ("/p/" <> pid.toText <> "/issues") "Back to Issues"}
          "Issue not found"
          "This issue may have been resolved, merged, or the link may be outdated."
    Just issue -> do
      let tp = TimePicker.TimePicker (Just $ fromMaybe (defaultSinceRange issue.createdAt now) sinceM) Nothing Nothing
          (rangeStart, rangeEnd, _) = TimePicker.parseTimeRange now tp
      errorM <- bool (pure Nothing) (ErrorPatterns.getErrorPatternLByHash pid issue.targetHash now) (issue.issueType == Issues.RuntimeException)
      canResolve <- case errorM of
        Nothing -> pure False
        Just errL -> do
          userPermission <- ProjectMembers.getUserPermission pid sess.user.id
          pure $ userPermission >= Just ProjectMembers.PEdit || errL.base.assigneeId == Just sess.user.id
      let bwconf =
            baseBwconf
              { prePageTitle = Just "Issues"
              , pageTitle = "#" <> show issue.seqNum
              , headContent = Just do highlightJsHead_; style_ "#crisp-chatbox { display: none !important; }"
              , pageActions = Just $ div_ [class_ "flex gap-2"] do
                  anomalyAcknowledgeButton pid (UUIDId issue.id.unUUIDId) now (zonedTimeToUTC <$> issue.acknowledgedUntil <* issue.acknowledgedAt)
                  anomalyArchiveButton pid (UUIDId issue.id.unUUIDId) (isJust issue.archivedAt)
                  when (issue.issueType == Issues.RuntimeException)
                    $ whenJust errorM \errL -> do
                      errorResolveAction pid errL.base.id errL.base.state canResolve
                      errorSubscriptionAction pid errL.base
              }
      -- Every trace lookup carries the point-in-time the issue is known to have
      -- happened, so the query stays a ±5min window instead of a multi-day
      -- trace_id scan (a full-table scan on TF; see 2026-07-21 crash).
      let isFirst = isJust firstM
      mTraceRef <- case issue.issueType of
        Issues.RuntimeException ->
          pure $ errorM >>= \errL -> (,zonedTimeToUTC $ bool errL.base.updatedAt errL.base.createdAt isFirst) <$> bool errL.base.recentTraceId errL.base.firstTraceId isFirst
        Issues.ApiChange -> case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (d :: Issues.APIChangeData) ->
            Telemetry.getEndpointTraceId pid d.endpointMethod d.endpointPath isFirst now
          _ -> pure Nothing
        Issues.QueryAlert -> pure Nothing
        Issues.LogPattern -> pure Nothing
        Issues.LogPatternRateChange -> pure Nothing
      -- The trace is supporting evidence, not the page. It used to be fetched here
      -- and a cold read of a multi-thousand-span trace took >56s, so the gateway
      -- 504'd the whole issue. The Investigation panel now pulls it as its own
      -- HTMX fragment, and the only thing this page still needs from the trace is
      -- the session id for the replay section — one scalar, not 1300 rows.
      replaySession <- flip foldMapM mTraceRef \(tId, tTs) ->
        Hasql.withHasqlTimefusion useTf
          $ listToMaybe @Text
          <$> Hasql.interp
            [HI.sql| SELECT attributes___session___id FROM otel_logs_and_spans
                      WHERE project_id = #{pid.toText}
                        AND timestamp BETWEEN #{addUTCTime (-300) tTs} AND #{addUTCTime 300 tTs}
                        AND context___trace_id = #{tId}
                        AND attributes___session___id IS NOT NULL AND attributes___session___id <> ''
                      LIMIT 1 |]
      sampleOverride <-
        if issue.issueType `elem` [Issues.LogPattern, Issues.LogPatternRateChange]
          then do
            let pidTxt = pid.toText
                patHash = "pat:" <> issue.targetHash
                from = fromMaybe (addUTCTime (-3600) now) rangeStart
                to = fromMaybe now rangeEnd
            listToMaybe @(V.Vector Text)
              <$> Hasql.interp
                [HI.sql| SELECT summary FROM otel_logs_and_spans
                          WHERE project_id = #{pidTxt}
                            AND timestamp BETWEEN #{from} AND #{to}
                            AND #{patHash} = ANY(hashes)
                          ORDER BY timestamp DESC
                          LIMIT 1 |]
          else pure Nothing
      addRespHeaders $ PageCtx bwconf $ anomalyDetailPage pid issue mTraceRef replaySession errorM now isFirst tp sampleOverride


-- | Unescape JSON-ish whitespace/quotes embedded in summary tokens.
unescSummary :: Text -> Text
unescSummary = T.replace "\\\"" "\"" . T.replace "\\n" " " . T.replace "\\t" " "


-- | Style->class mapping shared by the chip and inline-text summary renderers.
-- When @wrap@ is True (chips) the classes gain whitespace-pre-wrap/break-* suffixes.
summaryTokenClass :: Bool -> Text -> Text
summaryTokenClass wrap = \case
  s | "badge-" `T.isPrefixOf` s -> "cbadge-sm " <> s <> badgeWrap
  s | s `elem` ["text-textWeak", "text-weak"] -> "text-textWeak text-xs" <> textWrap
  "text-textStrong" -> "text-textStrong text-xs font-medium" <> textWrap
  _ -> "cbadge-sm badge-neutral" <> badgeWrap
  where
    badgeWrap = bool "" " whitespace-pre-wrap break-all" wrap
    textWrap = bool "" " whitespace-pre-wrap break-words" wrap


-- | Render one @field;style⇒value@ summary token. In chip mode (@wrap@) whitespace
-- is preserved and the @right-@ style prefix (subdued right-rail metadata) is honoured.
summaryToken_ :: Monad m => Bool -> Text -> HtmlT m ()
summaryToken_ wrap token = case T.breakOn "⇒" token of
  (_, "") -> span_ [class_ $ bool "mr-1" "text-textWeak text-xs whitespace-pre-wrap break-words" wrap] $ toHtml $ unescSummary token
  (left, rest) ->
    let (field, style) = case T.breakOn ";" left of
          (f, s) | not (T.null s) -> (f, T.drop 1 s)
          _ -> ("", left)
        cls = summaryTokenClass wrap $ bool style (fromMaybe style $ T.stripPrefix "right-" style) wrap
     in span_
          ([class_ $ cls <> bool " mr-1 inline-block" " inline-block max-w-full" wrap] <> [term "data-tippy-content" field | not (T.null field)])
          (toHtml $ unescSummary $ T.drop 1 rest)


-- | Smart default time range based on anomaly age.
-- Picks a range ~2x the anomaly age so the data fills the chart.
defaultSinceRange :: ZonedTime -> UTCTime -> Text
defaultSinceRange createdAt now
  | ageH < 1 = "1H"
  | ageH < 3 = "3H"
  | ageH < 6 = "6H"
  | ageH < 24 = "24H"
  | ageH < 72 = "3D"
  | ageH < 168 = "7D"
  | otherwise = "14D"
  where
    ageH = diffUTCTime now (zonedTimeToUTC createdAt) / 3600


-- | A single user-journey event attached to a span as a JSON-encoded array under the
-- @breadcrumbs@ attribute. @kind@/@payload@ stand in for the JSON keys @type@/@data@
-- (renamed because @type_@ would clash with @Lucid.type_@ since field selectors are enabled).
data Breadcrumb = Breadcrumb
  { kind :: Text
  , message :: Maybe Text
  , payload :: Maybe AE.Value
  , timestamp :: Integer
  }
  deriving stock (Generic, Show)
  deriving
    (AE.FromJSON)
    via DAE.CustomJSON
          '[ DAE.OmitNothingFields
           , DAE.FieldLabelModifier '[DAE.Rename "kind" "type", DAE.Rename "payload" "data"]
           ]
          Breadcrumb


-- | Convert a UTCTime to epoch-milliseconds (the unit used by Breadcrumb).
utcToEpochMs :: UTCTime -> Integer
utcToEpochMs = floor . (* 1000) . POSIX.utcTimeToPOSIXSeconds


-- | Source 1: legacy stringified JSON array under @attributes.breadcrumbs@.
breadcrumbsFromCustomAttr :: Telemetry.SpanRecord -> [Breadcrumb]
breadcrumbsFromCustomAttr sr = fromMaybe [] do
  raw <- Telemetry.atMapText "breadcrumbs" sr.attributes
  AE.decodeStrict (encodeUtf8 raw)


-- | Source 2: OTel-native span events. Some SDKs record breadcrumbs as span events with
-- attribute keys prefixed @sentry.breadcrumb.*@; we also handle plain OTel events
-- (using the event name as the kind and @attributes.message\/body@ as the message).
breadcrumbsFromSpanEvents :: Telemetry.SpanRecord -> [Breadcrumb]
breadcrumbsFromSpanEvents sr = foldMap (map toBreadcrumb) (parseMaybe AE.parseJSON sr.events :: Maybe [Telemetry.SpanEvent])
  where
    toBreadcrumb ev =
      let attrs = ev.eventAttributes
          sentryKind = lookupValueText attrs "sentry.breadcrumb.category" <|> lookupValueText attrs "sentry.breadcrumb.type"
          msg =
            asum
              $ lookupValueText attrs
              <$> ["sentry.breadcrumb.message", "message", "body", "exception.message"]
       in Breadcrumb
            { kind = fromMaybe ev.eventName sentryKind
            , message = msg
            , payload = Just attrs
            , timestamp = utcToEpochMs ev.eventTime
            }


-- | Source 3: trace-scoped log records — every record in the trace that isn't the error
-- span itself. For backend traces this surfaces "user logged in -> queried db -> exception"
-- without any custom instrumentation.
breadcrumbsFromTraceLogs :: Text -> Telemetry.SpanRecord -> [Breadcrumb]
breadcrumbsFromTraceLogs errorSpanId sr =
  [ Breadcrumb
      { kind = sr.spanName
      , message = sr.statusMessage <|> Telemetry.atMapText "body" sr.attributes <|> Telemetry.atMapText "message" sr.attributes
      , payload = AE.toJSON <$> sr.attributes
      , timestamp = utcToEpochMs sr.startTime
      }
  | sr.spanId /= errorSpanId
  ]


-- | Combine all breadcrumb sources for a trace, dedupe near-duplicates emitted by
-- overlapping instrumentation (e.g. an SDK that ships both legacy attr + OTel events),
-- and sort chronologically.
extractBreadcrumbs :: V.Vector Telemetry.SpanRecord -> Maybe (NonEmpty Breadcrumb)
extractBreadcrumbs spans =
  let recs = V.toList spans
      errorSpanId = maybe "" (.spanId) $ viaNonEmpty last $ sortOn (.startTime) recs
      raw =
        concatMap breadcrumbsFromCustomAttr recs
          <> concatMap breadcrumbsFromSpanEvents recs
          <> concatMap (breadcrumbsFromTraceLogs errorSpanId) recs
      dedupKey bc = (bc.timestamp, bc.kind, T.take 80 $ fromMaybe "" bc.message)
   in nonEmpty $ sortOn dedupKey $ ordNubOn dedupKey raw


-- | Icon id + tailwind colour class for a breadcrumb @type@.
breadcrumbVisual :: Text -> (Text, Text)
breadcrumbVisual t
  | t == "click" = ("arrow-pointer", "text-fillBrand-strong")
  | t `elem` ["navigation", "nav"] = ("globe", "text-fillSuccess-strong")
  | t `elem` ["xhr", "fetch"] = ("wifi", "text-fillInformation-strong")
  | t == "console.error" = ("terminal", "text-fillError-strong")
  | t == "console.warn" = ("terminal", "text-fillWarning-strong")
  | otherwise = ("terminal", "text-textWeak")


-- | Compact selector / url summary from a breadcrumb's @data@ blob.
breadcrumbDataSummary :: AE.Value -> Maybe Text
breadcrumbDataSummary (AE.String s) = Just s
breadcrumbDataSummary v = asum $ lookupValueText v <$> ["selector", "url"]


-- | User-journey breadcrumb section, rendered inline inside an existing card.
-- Emits nothing when the trace carries no parseable breadcrumbs.
userJourneySection_ :: V.Vector Telemetry.SpanRecord -> Html ()
userJourneySection_ spans = whenJust (extractBreadcrumbs spans) \crumbs -> do
  let crumbList = toList crumbs
      total = length crumbList
      base = (head crumbs).timestamp
      lastIdx = total - 1
      renderCrumb idx bc = do
        let (icn, iconColor) = breadcrumbVisual bc.kind
            isTerminal = idx == lastIdx
            timeLabel
              | idx == 0 = toText $ formatTime defaultTimeLocale "%b %-e, %H:%M:%S" $ POSIX.posixSecondsToUTCTime $ realToFrac (fromIntegral bc.timestamp / 1000 :: Double)
              | otherwise = formatOffset base bc.timestamp
        div_ [class_ $ bool "relative flex gap-2.5 px-4 py-2 border-l-2 border-transparent hover:bg-fillWeaker" "relative flex gap-2.5 px-4 py-2 border-l-2 border-strokeError-strong bg-fillError-weak" isTerminal] do
          div_ [class_ "flex flex-col items-center pt-0.5 shrink-0"] do
            faSprite_ icn "regular" $ "w-3 h-3 " <> iconColor
            unless isTerminal $ div_ [class_ "w-px flex-1 bg-strokeWeak mt-1"] ""
          div_ [class_ "min-w-0 flex-1 flex flex-col gap-0.5"] do
            div_ [class_ "flex items-center gap-2 flex-wrap"] do
              span_ [class_ "text-xs tabular-nums text-textWeak shrink-0"] $ toHtml timeLabel
              span_ [class_ $ "text-xs font-medium " <> iconColor] $ toHtml bc.kind
            -- Long messages clamp to 3 lines; clicking the row toggles a `expanded` class via hyperscript
            -- to remove the clamp. Plain class toggle is more reliable than relying on `details[open]`
            -- propagating through Tailwind's named-group `open` variant for a `display:-webkit-box` reset.
            let expandable cls val =
                  div_
                    [ class_ "group/bc cursor-pointer flex items-start gap-1"
                    , [__|on click toggle .is-open on me|]
                    ]
                    do
                      span_ [class_ $ cls <> " group-[.is-open]/bc:line-clamp-none group-[.is-open]/bc:!block min-w-0 flex-1"] $ toHtml val
                      faSprite_ "chevron-down" "regular" "w-3 h-3 text-textWeak shrink-0 mt-1 group-[.is-open]/bc:rotate-180 transition-transform"
            whenJust bc.message
              $ expandable "text-sm text-textStrong line-clamp-3 break-words whitespace-pre-wrap"
            whenJust (bc.payload >>= breadcrumbDataSummary)
              $ expandable "font-mono text-xs text-textWeak line-clamp-2 break-all"
  div_ [class_ "border-t border-strokeWeak"] do
    div_ [class_ "px-4 py-2 flex items-center gap-2 bg-fillWeaker/40"] do
      faSprite_ "route" "regular" "w-3 h-3 text-textWeak"
      span_ [class_ "text-2xs font-semibold text-textWeak uppercase tracking-wide"] "User journey"
      span_ [class_ "text-2xs text-textWeak"] $ toHtml $ show total <> " event" <> bool "s" "" (total == 1) <> " before error"
    div_ [class_ "max-h-80 overflow-y-auto py-1"]
      $ traverse_ (uncurry renderCrumb) (zip [0 :: Int ..] crumbList)


-- | The user-journey half needs the issue's trace, which is the slow read on this
-- page — so the whole panel arrives through the one fragment it already used for
-- issue events, with the trace reference passed along rather than pre-fetched.
activityPanel_ :: Projects.ProjectId -> Text -> Text -> Maybe (Text, UTCTime) -> Html ()
activityPanel_ pid issueId extraClass traceRef = do
  let activityUrl =
        "/p/" <> pid.toText <> "/issues/" <> issueId <> "/activity"
          <> foldMap (\(tId, tTs) -> "?trace_id=" <> toUriStr tId <> "&trace_ts=" <> toUriStr (formatUTC tTs)) traceRef
  details_ [class_ $ "surface-raised rounded-2xl group/activity overflow-hidden " <> extraClass, term "open" ""] do
    summary_ [class_ "px-4 py-3 flex items-center gap-2 cursor-pointer list-none [&::-webkit-details-marker]:hidden"] do
      faSprite_ "clock-rotate-left" "regular" "w-3.5 h-3.5 text-textWeak"
      span_ [class_ "text-xs font-semibold text-textWeak uppercase tracking-wide"] "Activity"
      faSprite_ "chevron-down" "regular" "w-3 h-3 text-textWeak shrink-0 ml-auto group-open/activity:rotate-180 transition-transform"
    div_ [id_ "issue-activity", hxGet_ activityUrl, hxTrigger_ "intersect once", hxSwap_ "innerHTML"]
      $ div_ [class_ "p-4 flex justify-center"]
      $ loadingIndicator_ LdSM LdDots


-- | A wrapping row of @(icon, iconColour, label, value)@ entries, as used by the
-- Error/Endpoint detail panels.
detailRow_ :: [(Text, Text, Text, Text)] -> Html ()
detailRow_ =
  div_ [class_ "flex flex-wrap items-center gap-x-5 gap-y-2"] . mapM_ \(icn, iconColor, lbl, value) ->
    div_ [class_ "flex items-center gap-1.5 whitespace-nowrap"] do
      faSprite_ icn "regular" $ "w-3 h-3 " <> iconColor
      span_ [class_ "text-xs text-textWeak"] $ toHtml lbl <> ":"
      span_ [class_ "text-xs font-medium"] $ toHtml value


-- | Right-hand "<title> details" card shown next to the volume chart.
detailCard_ :: Text -> Html () -> Html ()
detailCard_ title body = div_ [class_ "lg:w-72 shrink-0 surface-raised rounded-2xl overflow-hidden"] do
  div_ [class_ "px-4 py-3 border-b border-strokeWeak flex items-center gap-2"] do
    faSprite_ "circle-info" "regular" "w-3.5 h-3.5 text-textWeak"
    span_ [class_ "text-xs font-semibold text-textWeak uppercase tracking-wide"] $ toHtml title
  div_ [class_ "p-4 flex flex-col gap-3"] body


-- | Banner stating, in words, what the issue's current state means for
-- notifications. The whole point of the ack window is that a reader never has
-- to guess whether alerts are still coming.
issueStatusStrip_ :: UTCTime -> Issues.Issue -> Html ()
issueStatusStrip_ now issue = whenJust banner \(icon, cls, msg) ->
  div_ [class_ $ "flex items-center gap-2 rounded-lg border px-3 py-2 text-sm " <> cls] do
    faSprite_ icon "regular" "w-4 h-4 shrink-0"
    span_ [] $ toHtml msg
  where
    banner
      | isJust issue.archivedAt = Just ("archive", "border-strokeWeak bg-fillWeaker text-textWeak", "Archived — hidden from the Inbox and never notified. Unarchive to bring it back." :: Text)
      | Just until' <- zonedTimeToUTC <$> issue.acknowledgedUntil <* issue.acknowledgedAt =
          Just ("bell-slash", "border-strokeSuccess-weak bg-fillSuccess-weak text-textSuccess", untilLabel "Acknowledged" now until' <> " \x2014 notifications are paused. This issue returns to the Inbox when the window ends or it regresses.")
      | otherwise = Nothing


-- | @traceRef@ is the (trace id, when-it-happened) the Investigation panel loads
-- its waterfall from — the panel fetches it itself, so a slow trace can't hold up
-- this page. @replaySession@ is the one value the page still needs out of that
-- trace, resolved by a scalar lookup rather than by reading every span.
anomalyDetailPage :: Projects.ProjectId -> Issues.Issue -> Maybe (Text, UTCTime) -> Maybe Text -> Maybe ErrorPatterns.ErrorPatternL -> UTCTime -> Bool -> TimePicker.TimePicker -> Maybe (V.Vector Text) -> Html ()
anomalyDetailPage pid issue traceRef replaySession errM now isFirst tp sampleOverride = do
  let (_, _, currentRange) = TimePicker.parseTimeRange now tp
      issueId = UUID.toText issue.id.unUUIDId
      severityBadge "critical" = span_ [class_ "inline-flex items-center justify-center rounded-md px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 bg-fillError-weak text-fillError-strong border-2 border-strokeError-strong shadow-sm"] "CRITICAL"
      severityBadge "warning" = span_ [class_ "inline-flex items-center justify-center rounded-md px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 bg-fillWarning-weak text-fillWarning-strong border border-strokeWarning-weak shadow-sm"] "WARNING"
      severityBadge _ = pass
  div_ [class_ "flex h-full overflow-hidden relative group/ai"] do
    -- LEFT: scrollable main content
    div_ [class_ "flex-1 min-w-0 min-h-0 overflow-y-auto max-md:pt-5 pt-8 max-md:px-3 px-4 pb-8 max-md:space-y-3 space-y-4"] do
      -- Header: title
      issueStatusStrip_ now issue
      h3_ [class_ "max-md:text-xl text-2xl font-semibold text-textStrong flex flex-wrap items-center gap-1"] $ if "⇒" `T.isInfixOf` issue.title then renderSummaryText_ issue.title else toHtml issue.title
      unless (issue.recommendedAction == Issues.defaultRecommendedAction)
        $ p_ [class_ "text-sm text-textWeak max-w-3xl"]
        $ toHtml issue.recommendedAction
      -- Metadata chips + issue type content
      let createdChip = colorChip_ "text-fillInformation-strong bg-fillInformation-weak" "calendar" $ "Created " <> toText (prettyTimeAuto now (zonedTimeToUTC issue.createdAt))
          -- Prefer a real captured log line (sampleOverride) over the stored
          -- sample, which the drain pipeline often normalises down to the same
          -- placeholders as the template. The override is the original summary
          -- vector — render each element as a single chip so values with
          -- internal whitespace (user-agents, page titles) stay intact.
          logPatternCards sourceField logPattern sampleMessage = div_ [class_ "flex flex-col gap-4"] do
            div_ [class_ "surface-raised rounded-2xl overflow-hidden"] do
              div_ [class_ "px-4 py-3 border-b border-strokeWeak flex items-center gap-2"] do
                span_ [class_ "text-xs font-semibold text-textWeak uppercase tracking-wide"] "Log Pattern"
                span_ [class_ "badge badge-sm badge-ghost"] $ toHtml $ sourceFieldLabel sourceField
              renderLogContent_ logPattern
            let renderSample :: Html () -> Html ()
                renderSample body = div_ [class_ "surface-raised rounded-2xl overflow-hidden"] do
                  div_ [class_ "px-4 py-3 border-b border-strokeWeak"] $ span_ [class_ "text-xs font-semibold text-textWeak uppercase tracking-wide"] "Sample Message"
                  body
            maybe
              (whenJust (mfilter ((/= T.strip logPattern) . T.strip) sampleMessage) (renderSample . renderLogContent_))
              (renderSample . div_ [class_ "flex flex-wrap items-center gap-1 p-4 max-h-80 overflow-y-auto"] . V.mapM_ (summaryToken_ True))
              (mfilter (not . V.null) sampleOverride)
      div_ [class_ "flex flex-wrap gap-2 items-center"] do
        severityBadge (display issue.severity)
        issueTypeLabel issue.issueType issue.critical
        case issue.issueType of
          Issues.LogPattern -> withIssueDataH @Issues.LogPatternData issue.issueData \d -> do
            logLevelChip_ d.logLevel d.logPattern
            metadataChip_ "server" $ fromMaybe "Unknown" d.serviceName
            metadataChip_ "tally" $ show d.occurrenceCount <> " occurrences"
            metadataChip_ "clock" $ "First seen " <> compactTimeAgo (toText $ prettyTimeAuto now d.firstSeenAt)
          Issues.LogPatternRateChange -> withIssueDataH @Issues.LogPatternRateChangeData issue.issueData \d -> do
            logLevelChip_ d.logLevel d.logPattern
            metadataChip_ "arrow-trend-up" $ display d.changeDirection
            metadataChip_ "percent" $ Issues.showPct d.changePercent <> " change"
            metadataChip_ "gauge-high" $ Issues.showRate d.currentRatePerHour <> " current"
            metadataChip_ "chart-line" $ Issues.showRate d.baselineMean <> " baseline"
          Issues.RuntimeException -> pass -- First/Last seen shown in Error Details panel
          Issues.QueryAlert -> createdChip
          Issues.ApiChange -> createdChip
      -- Seed URL params with default time range so standalone chart widgets can read it
      script_ [fmt|document.addEventListener('DOMContentLoaded',function(){{if(!new URLSearchParams(location.search).get('since'))window.setParams({{since:'{fromMaybe "1H" tp.since}'}})}});|]
      -- Volume chart + issue type content
      let volumeChart_ chartTitle = whenJust (Issues.hashPrefix issue.issueType) \prefix -> do
            let hashQuery = "hashes[*]==\"" <> prefix <> issue.targetHash <> "\" | summarize count(*) by bin_auto(timestamp)"
                refreshId = "anomaly-chart-refresh"
            div_ [id_ refreshId, class_ "hidden", term "_" "on submit trigger 'update-query' on window"] ""
            div_ [class_ "surface-raised rounded-2xl overflow-hidden"] do
              div_ [class_ "px-4 py-2 flex flex-wrap items-center justify-between gap-x-3 gap-y-1 border-b border-strokeWeak"] do
                span_ [class_ "text-xs font-semibold text-textWeak uppercase tracking-wide"] $ toHtml chartTitle
                div_ [class_ "flex items-center gap-2"] do
                  TimePicker.timepicker_ (Just refreshId) currentRange Nothing
                  TimePicker.refreshButton_
              div_ [class_ "h-24"]
                $ Widget.widget_
                  (def :: Widget.Widget)
                    { Widget.standalone = Just True
                    , Widget.naked = Just True
                    , Widget.id = Just $ issueId <> "-pattern-volume"
                    , Widget.wType = Widget.WTTimeseries
                    , Widget.showTooltip = Just True
                    , Widget.query = Just hashQuery
                    , Widget._projectId = Just issue.projectId
                    , Widget.hideLegend = Just True
                    , Widget.hideSubtitle = Just True
                    }
      let patternLayout sf lp sm =
            div_ [class_ "flex flex-col lg:flex-row gap-4 lg:items-start"] do
              div_ [class_ "min-w-0 flex-1 flex flex-col gap-4"] do
                volumeChart_ "Pattern Volume"
                logPatternCards sf lp sm
              activityPanel_ pid issueId "lg:w-80 shrink-0" traceRef
      case issue.issueType of
        Issues.LogPattern -> withIssueDataH @Issues.LogPatternData issue.issueData \d ->
          patternLayout d.sourceField d.logPattern d.sampleMessage
        Issues.LogPatternRateChange -> withIssueDataH @Issues.LogPatternRateChangeData issue.issueData \d ->
          patternLayout d.sourceField d.logPattern d.sampleMessage
        Issues.RuntimeException -> withIssueDataH @Issues.RuntimeExceptionData issue.issueData \exceptionData -> do
          let trimmedStack = T.strip exceptionData.stackTrace
              hasStack = not $ T.null trimmedStack
              errorFirstLine = if hasStack then fromMaybe trimmedStack $ viaNonEmpty head $ lines trimmedStack else exceptionData.errorMessage
          -- Chart + Error Details in one row
          div_ [class_ "flex flex-col lg:flex-row gap-4 lg:items-start"] do
            div_ [class_ "min-w-0 flex-1"] $ volumeChart_ "Error Frequency"
            whenJust errM \errL -> do
              let err = errL.base
              detailCard_ "Error Details" do
                whenJust ((,) <$> exceptionData.requestMethod <*> exceptionData.requestPath) \(method, path) ->
                  div_ [class_ "mb-1"] do
                    span_ [class_ $ "relative cbadge-sm badge-" <> method <> " whitespace-nowrap"] $ toHtml method
                    span_ [class_ "ml-2 text-sm text-textWeak"] $ toHtml path
                detailRow_
                  [ ("calendar", "text-fillBrand-strong", "First seen", compactTimeAgo $ toText $ prettyTimeAuto now (zonedTimeToUTC err.createdAt))
                  , ("calendar", "text-fillBrand-strong", "Last seen", compactTimeAgo $ toText $ prettyTimeAuto now (zonedTimeToUTC err.updatedAt))
                  ]
                detailRow_
                  [ ("code", "text-fillWarning-strong", "Stack", fromMaybe "Unknown stack" err.errorData.runtime)
                  , ("server", "text-fillSuccess-strong", "Service", fromMaybe "Unknown service" err.errorData.serviceName)
                  ]
          -- Stack trace + Activity (Activity column merges User Journey + issue events)
          div_ [class_ "flex flex-col lg:flex-row gap-4 lg:items-start"] do
            div_ [class_ "min-w-0 flex-1"]
              $ details_ [class_ "surface-raised rounded-2xl group/details", term "open" "", term "_" "init if window.innerWidth < 768 remove @open from me"]
              $ do
                summary_ [class_ "px-4 py-3 flex items-center gap-2 cursor-pointer list-none [&::-webkit-details-marker]:hidden"] do
                  faSprite_ "code" "regular" "w-3.5 h-3.5 text-textWeak"
                  span_ [class_ "text-xs font-semibold text-textWeak uppercase tracking-wide shrink-0"] "Stack Trace"
                  span_ [class_ "text-xs text-fillError-strong truncate min-w-0 flex-1"] $ toHtml errorFirstLine
                  faSprite_ "chevron-down" "regular" "w-3 h-3 text-textWeak shrink-0 ml-auto group-open/details:rotate-180 transition-transform"
                div_ [class_ "border-t border-strokeWeak"] do
                  -- Full error message first — visible whether or not we have a stack trace, since the
                  -- summary truncates and the user expanded specifically to read it in full.
                  unless (T.null exceptionData.errorMessage) $ div_ [class_ "px-4 py-3 border-b border-strokeWeak"] do
                    span_ [class_ "text-2xs font-semibold text-textWeak uppercase tracking-wide block mb-1"] "Error message"
                    pre_ [class_ "text-sm leading-relaxed text-fillError-strong whitespace-pre-wrap break-words font-mono"] $ toHtml exceptionData.errorMessage
                  if hasStack
                    then
                      div_ [class_ "max-h-80 overflow-y-auto"]
                        $ pre_ [class_ "text-sm leading-relaxed overflow-x-auto whitespace-pre-wrap px-4 py-3"]
                        $ code_ []
                        $ toHtml trimmedStack
                    else div_ [class_ "px-4 py-4 flex items-start gap-2 text-textWeak"] do
                      faSprite_ "circle-info" "regular" "w-4 h-4 shrink-0 mt-0.5"
                      span_ [class_ "text-xs"] "No stack trace captured — common for browser console errors. Check the User Journey for the events that led up to it."
            activityPanel_ pid issueId "lg:w-80 shrink-0" traceRef
          -- Similar patterns
          whenJust errM \errL -> similarPatternsSection_ pid errL.base.id
        Issues.QueryAlert -> withIssueDataH @Issues.QueryAlertData issue.issueData \alertData ->
          div_ [class_ "mb-4"] do
            span_ [class_ "text-xs text-textWeak mb-2 block font-semibold uppercase tracking-wide"] "Query"
            div_ [class_ "bg-fillInformation-weak border border-strokeInformation-weak rounded-lg p-3 text-sm font-mono text-fillInformation-strong max-w-2xl overflow-x-auto"] $ toHtml alertData.queryExpression
        Issues.ApiChange -> withIssueDataH @Issues.APIChangeData issue.issueData \d -> do
          let fieldChip color f = span_ [class_ $ "font-mono text-xs px-2 py-0.5 rounded bg-fillWeaker " <> color] $ toHtml f
              fieldList :: Text -> Text -> Text -> V.Vector Text -> Html ()
              fieldList lbl color icn fields
                | V.null fields = pass
                | otherwise = div_ [class_ "flex flex-col gap-1.5"] do
                    div_ [class_ "flex items-center gap-1.5"] do
                      faSprite_ icn "regular" $ "w-3 h-3 " <> color
                      span_ [class_ $ "text-xs font-semibold uppercase tracking-wide " <> color] $ toHtml lbl
                      span_ [class_ "text-xs text-textWeak"] $ toHtml $ "(" <> show (V.length fields) <> ")"
                    div_ [class_ "flex flex-wrap gap-1"] $ V.forM_ fields (fieldChip color)
              hasFieldChanges = not (V.null d.newFields) || not (V.null d.deletedFields) || not (V.null d.modifiedFields)
          -- Endpoint chip line
          div_ [class_ "flex flex-wrap items-center gap-3"] do
            span_ [class_ $ "cbadge-sm whitespace-nowrap badge-" <> d.endpointMethod] $ toHtml d.endpointMethod
            span_ [class_ "font-mono bg-fillWeaker px-2 py-1 rounded text-sm text-textStrong"] $ toHtml d.endpointPath
            span_ [class_ "flex items-center gap-1.5 text-sm text-textWeak"] do
              faSprite_ "server" "regular" "h-3 w-3"
              toHtml d.endpointHost
          -- Chart + endpoint details panel side-by-side
          div_ [class_ "flex flex-col lg:flex-row gap-4 lg:items-start"] do
            div_ [class_ "min-w-0 flex-1"] $ volumeChart_ "Request Trend"
            detailCard_ "Endpoint Details" do
              detailRow_
                [ ("calendar", "text-fillBrand-strong", "First seen", compactTimeAgo $ toText $ prettyTimeAuto now (zonedTimeToUTC issue.createdAt))
                , ("calendar", "text-fillBrand-strong", "Last seen", compactTimeAgo $ toText $ prettyTimeAuto now (zonedTimeToUTC issue.updatedAt))
                ]
              detailRow_
                [ ("server", "text-fillSuccess-strong", "Service", fromMaybe "Unknown service" issue.service)
                , ("hashtag", "text-fillBrand-strong", "Requests", formatWithCommas (fromIntegral issue.affectedRequests :: Double))
                ]
          -- Field changes (or "new endpoint" hint) + Activity panel
          div_ [class_ "flex flex-col lg:flex-row gap-4 lg:items-start"] do
            div_ [class_ "min-w-0 flex-1"]
              $ if hasFieldChanges
                then div_ [class_ "surface-raised rounded-2xl overflow-hidden"] do
                  div_ [class_ "px-4 py-3 border-b border-strokeWeak flex items-center gap-2"] do
                    faSprite_ "list-check" "regular" "w-3.5 h-3.5 text-textWeak"
                    span_ [class_ "text-xs font-semibold text-textWeak uppercase tracking-wide"] "Field Changes"
                  div_ [class_ "p-4 flex flex-col gap-4"] do
                    fieldList "New" "text-fillSuccess-strong" "plus" d.newFields
                    fieldList "Deleted" "text-fillError-strong" "minus" d.deletedFields
                    fieldList "Modified" "text-fillWarning-strong" "code" d.modifiedFields
                else div_ [class_ "surface-raised rounded-2xl px-4 py-6 flex flex-col items-center gap-2 text-center"] do
                  faSprite_ "rocket" "regular" "w-5 h-5 text-fillBrand-strong"
                  span_ [class_ "text-sm text-textStrong"] "New endpoint discovered"
                  span_
                    [class_ "text-xs text-textWeak max-w-sm"]
                    "This endpoint started receiving traffic. Inspect the originating request in Investigation below to see headers, body, and call site."
            activityPanel_ pid issueId "lg:w-80 shrink-0" traceRef
      let isLogPatternIssue = issue.issueType `elem` ([Issues.LogPattern, Issues.LogPatternRateChange] :: [Issues.IssueType])
      -- Escape closes the open span panel before it exits fullscreen — the panel's close
      -- button advertises "Close · Esc", and the same precedence as the log explorer's shell.
      div_
        [ class_ "surface-raised rounded-2xl overflow-hidden group/inv"
        , id_ "error-details-container"
        , makeAttribute "tabindex" "-1"
        , -- Same contract as the log explorer's #apiLogsPage: senders `send toggleFullscreen`
          -- to the container, which is the only receiver and owns the state flip.
          -- `the first <…/> exists`, not a bare `<…/>`: a query literal evaluates to a lazy
          -- query object that is truthy even when it matches nothing, so `if <sel/>` never
          -- falls through to the fullscreen branch.
          [__|on toggleFullscreen(active)
                default active to (I do not match .investigation-fullscreen)
                if active add .investigation-fullscreen to me
                otherwise remove .investigation-fullscreen from me
                end
                call window.scrollTo({top:0})
              end
              on keydown[key is 'Escape'] from window
                if the first <#trace_details_container.open/> exists
                  send closeDetailPanel to #trace_details_container
                otherwise if I match .investigation-fullscreen
                  send toggleFullscreen(active: false) to me
                end|]
        ]
        do
          div_ [class_ "max-md:px-3 px-4 border-b border-strokeWeak flex max-md:flex-col md:items-center md:justify-between"] do
            div_ [class_ "flex items-center gap-2 max-md:py-1.5"] do
              faSprite_ "magnifying-glass-chart" "regular" "w-3.5 h-3.5 text-textWeak"
              h3_ [class_ "text-xs font-semibold text-textWeak uppercase tracking-wide"] "Investigation"
            div_ [class_ "flex items-center max-md:overflow-x-auto max-md:-mx-4 max-md:px-4 max-md:pb-1.5"] do
              let aUrl = "/p/" <> pid.toText <> "/issues/" <> issueId
                  navLink (href, isActive, tooltip, lbl) = a_ [href_ href, class_ $ bool "text-textWeak hover:text-textStrong" "text-textBrand font-medium" isActive <> " text-xs py-2.5 max-md:px-2 px-3 cursor-pointer transition-colors", term "data-tippy-content" tooltip] $ toHtml lbl
                  tabBtn (target, lbl, isActive) = button_ [class_ $ "text-xs py-2.5 max-md:px-2 px-3 cursor-pointer err-tab font-medium" <> bool "" " t-tab-active" isActive, onclick_ $ "navigatable(this, '" <> target <> "', '#error-details-container', 't-tab-active', 'err')"] $ toHtml lbl
              forM_ ([(aUrl <> "?first_occurrence=true", isFirst, "Show first trace the error occured", "First"), (aUrl, not isFirst, "Show recent trace the error occured", "Recent")] :: [(Text, Bool, Text, Text)]) navLink
              span_ [class_ "mx-3 w-px h-4 bg-strokeWeak max-md:mx-2"] pass
              forM_ ([("#span-content", "Trace", not isLogPatternIssue), ("#log-content", "Logs", isLogPatternIssue)] :: [(Text, Text, Bool)]) tabBtn
              span_ [class_ "mx-2 w-px h-4 bg-strokeWeak max-md:mx-1"] pass
              -- Icon state is CSS-driven off the container's fullscreen class; the click only
              -- sends the event. tippy, not daisyUI: the card is `overflow-hidden`, which clips
              -- daisyUI's ::before bubble (see the tooltip rules in page-chrome.ts).
              button_ [class_ "p-1.5 rounded hover:bg-fillWeaker cursor-pointer transition-colors max-md:hidden", Aria.label_ "Toggle fullscreen", term "data-tippy-content" "Expand · Esc to exit", [__|on click send toggleFullscreen to #error-details-container|]] do
                faSprite_ "expand" "regular" "w-3 h-3 text-textWeak group-[.investigation-fullscreen]/inv:hidden"
                faSprite_ "compress" "regular" "w-3 h-3 text-textWeak hidden group-[.investigation-fullscreen]/inv:block"
          div_ [class_ "max-md:p-1 p-2 w-full overflow-x-hidden investigation-content"] do
            -- The trace ships its own details panel (#trace_details_container), so this tab renders
            -- no second one — clicking a span replaces the open panel instead of stacking another.
            div_ [class_ $ bool "" "hidden " isLogPatternIssue <> "w-full lg:h-[70vh] err-tab-content", id_ "span-content"] do
              -- The waterfall arrives on its own: a cold read of a multi-thousand-span
              -- trace took >56s and used to 504 this entire page. `load`, not
              -- `intersect` — the pane is full-height and its trigger never scrolls
              -- into view, which would leave it stuck on the spinner.
              div_ [id_ "trace_container", class_ "w-full h-full min-w-0"] case traceRef of
                Nothing -> div_ [class_ "flex items-center justify-center h-48"] $ emptyState_ def{icon = Just "inbox-full", size = ESCompact} "No trace data available for this issue." ""
                Just (tId, tTs) ->
                  div_
                    [ hxGet_ $ traceFragmentUrl pid tId (Just tTs) True
                    , hxTrigger_ "load"
                    , hxSwap_ "outerHTML"
                    , class_ "h-48 flex items-center justify-center"
                    ]
                    $ loadingIndicator_ LdMD LdSpinner

          div_ [id_ "log-content", class_ $ bool "hidden " "" isLogPatternIssue <> "err-tab-content flex flex-col lg:flex-row w-full lg:h-[70vh]"] do
            let pickerParams = mconcat ["&" <> key <> "=" <> toUriStr v | (key, Just v) <- [("since", tp.since), ("from", tp.from), ("to", tp.to)], not (T.null v)]
                isoT t = toUriStr $ toText $ formatTime defaultTimeLocale "%FT%TZ" t
                lastSeen = zonedTimeToUTC $ maybe issue.createdAt (.base.updatedAt) errM
                (logsQuery, logsParams) = case (Issues.hashPrefix issue.issueType, asum ([errM >>= (.base.recentTraceId), fst <$> traceRef] :: [Maybe Text])) of
                  (Just prefix, _) | isLogPatternIssue -> ("hashes[*]==\"" <> prefix <> issue.targetHash <> "\"", pickerParams)
                  (_, Just tId) -> ("kind==\"log\" AND context___trace_id==\"" <> tId <> "\"", pickerParams)
                  -- ~24% of error patterns never captured a trace id (log records carry no trace
                  -- context; spans always do). The old empty-string fallback rendered
                  -- @context___trace_id==""@, which filters nothing — the tab dumped the project's
                  -- entire retention window (6s / 550KB of unrelated logs). With no trace to pin
                  -- to, scope to the issue's service over +/-5min around when it was last seen.
                  _ ->
                    ( "kind==\"log\"" <> foldMap (\s -> " AND service==\"" <> s <> "\"") issue.service
                    , "&from=" <> isoT (addUTCTime (-300) lastSeen) <> "&to=" <> isoT (addUTCTime 300 lastSeen)
                    )
            div_ [class_ "grow-1 min-w-0 h-full"]
              $ virtualTable pid (Just ("/p/" <> pid.toText <> "/log_explorer/data?json=true&query=" <> toUriStr logsQuery <> logsParams)) Nothing
            div_ [class_ "transition-opacity duration-200 mx-1 hidden lg:block", id_ "resizer-details_width-wrapper"] $ resizer_ "log_details_container" "details_width" False
            div_
              [ class_ "details-panel grow-0 relative shrink-0 h-full overflow-y-auto overflow-x-hidden c-scroll lg:w-1/2 investigation-details"
              , id_ "log_details_container"
              , -- Last-click-wins; see the matching note in Pages.LogExplorer.Log.detailsPanel.
                term "hx-sync" "this:replace"
              , [__|on closeDetailPanel
                set my *width to '0px'
                remove .bg-fillBrand-strong from <.item-row.bg-fillBrand-strong/>
                add .opacity-0 .pointer-events-none to #resizer-details_width-wrapper
                call updateUrlState('details_width', '', 'delete')
              end
              on htmx:after:swap if event.target is me
                set my *width to ''
                remove .opacity-0 .pointer-events-none from #resizer-details_width-wrapper
                if window.innerWidth < 1024 call me.scrollIntoView({behavior:'smooth', block:'start'}) end
              end|]
              ]
              $ htmxOverlayIndicator_ "details_indicator"

      whenJust replaySession \sessionId ->
        div_ [class_ "surface-raised rounded-2xl overflow-hidden", id_ "replay-section"] do
          div_ [class_ "max-md:px-3 px-4 py-2.5 border-b border-strokeWeak flex items-center gap-2"] do
            faSprite_ "video" "regular" "w-3.5 h-3.5 text-textWeak"
            h3_ [class_ "text-xs font-semibold text-textWeak uppercase tracking-wide"] "Session Replay"
          termRaw "session-replay" [id_ "sessionReplay", term "initialSession" sessionId, term "consoleOpen" "true", term "fullWidth" "true", class_ "block w-full", term "projectId" pid.toText, term "containerId" "sessionPlayerWrapper"] ("" :: Text)

      -- Every other issue type already renders an Activity panel beside its own content.
      when (issue.issueType == Issues.QueryAlert) $ activityPanel_ pid issueId "" traceRef

    -- RIGHT: Inline collapsible AI chat panel (checkbox + group-has CSS, persists to localStorage)
    input_
      [ type_ "checkbox"
      , id_ "ai-panel-toggle"
      , class_ "hidden"
      , -- The event name must be quoted: hyperscript tokenizes the `-` in a bare
        -- `load-chat` as minus, which failed to parse and left the panel never loading.
        [__|init set my.checked to (localStorage.getItem('ai-panel-open') == 'true')
              if my.checked trigger 'load-chat' on #ai-response-container end
            end
            on change
              call localStorage.setItem('ai-panel-open', my.checked)
              if my.checked trigger 'load-chat' on #ai-response-container end
            end|]
      ]
    label_ [Lucid.for_ "ai-panel-toggle", class_ "absolute right-0 top-3 z-10 flex items-center gap-1.5 bg-fillBrand-strong text-white px-2 py-2.5 rounded-l-lg cursor-pointer shadow-md hover:opacity-90 transition-opacity group-has-[#ai-panel-toggle:checked]/ai:hidden", Aria.label_ "Open AI Assistant"] do
      faSprite_ "sparkles" "regular" "w-3.5 h-3.5"
    div_ [class_ "hidden group-has-[#ai-panel-toggle:checked]/ai:block"] $ resizer_ "ai_chat_container" "ai_width" False
    div_ [id_ "ai_chat_container", class_ "hidden group-has-[#ai-panel-toggle:checked]/ai:flex w-[420px] shrink-0 h-full overflow-hidden flex-col bg-bgBase border-l border-t border-strokeWeak"] do
      div_ [class_ "shrink-0 px-4 py-2.5 border-b border-strokeWeak flex items-center justify-between"] do
        div_ [class_ "flex items-center gap-2"] do
          faSprite_ "sparkles" "regular" "w-3.5 h-3.5 text-fillBrand-strong"
          span_ [class_ "text-xs font-semibold text-textWeak uppercase tracking-wide"] "AI Assistant"
        label_ [Lucid.for_ "ai-panel-toggle", class_ "p-1.5 rounded-lg hover:bg-fillWeaker cursor-pointer transition-colors tap-target", Aria.label_ "Close AI Assistant"]
          $ faSprite_ "xmark" "regular" "w-3 h-3 text-textWeak"
      anomalyAIChatBody_ pid issue.id


errorAssigneeSection :: Projects.ProjectId -> Maybe ErrorPatterns.ErrorPatternId -> Maybe Projects.UserId -> V.Vector ProjectMembers.ProjectMemberVM -> Html ()
errorAssigneeSection pid errIdM assigneeIdM members = do
  div_ [id_ "error-assignee", class_ "flex flex-col gap-2 border-t border-strokeWeak pt-3"] do
    span_ [class_ "text-xs text-textWeak"] "Assignee"
    case errIdM of
      Nothing ->
        select_ [class_ "select select-sm w-full", disabled_ "true", name_ "assigneeId"] do
          option_ [value_ ""] "Unassigned"
      Just errId -> do
        let actionUrl = "/p/" <> pid.toText <> "/issues/errors/" <> UUID.toText errId.unErrorPatternId <> "/assign"
        form_ [hxPost_ actionUrl, hxTarget_ "#error-assignee", hxSwap_ "outerHTML", hxTrigger_ "change"] do
          select_
            ( [class_ "select select-sm w-full", name_ "assigneeId"]
                <> [disabled_ "true" | V.null members]
            )
            $ do
              option_ ([value_ ""] <> [selected_ "true" | isNothing assigneeIdM]) "Unassigned"
              forM_ members \member -> do
                let memberIdText = UUID.toText $ Projects.getUserId member.userId
                    fullName = T.strip $ member.first_name <> " " <> member.last_name
                    emailText = CI.original member.email
                    label =
                      if T.null fullName
                        then emailText
                        else fullName <> " (" <> emailText <> ")"
                option_
                  ([value_ memberIdText] <> [selected_ "true" | assigneeIdM == Just member.userId])
                  $ toHtml label


errorResolveAction :: Projects.ProjectId -> ErrorPatterns.ErrorPatternId -> ErrorPatterns.ErrorState -> Bool -> Html ()
errorResolveAction pid errId errState canResolve =
  when canResolve do
    let actionUrl = "/p/" <> pid.toText <> "/issues/errors/" <> UUID.toText errId.unErrorPatternId <> "/resolve"
    div_ [id_ "error-resolve-action"] do
      if errState == ErrorPatterns.ESResolved
        then button_ [class_ "btn btn-sm btn-ghost text-textWeak", disabled_ "true"] do
          faSprite_ "circle-check" "regular" "w-4 h-4"
          span_ [class_ "max-md:hidden"] "Resolved"
        else button_
          [ class_ "btn btn-sm btn-ghost gap-1.5 text-textSuccess hover:bg-fillSuccess-weak"
          , Aria.label_ "Resolve issue"
          , hxPost_ actionUrl
          , hxTarget_ "#error-resolve-action"
          , hxSwap_ "outerHTML"
          ]
          do
            faSprite_ "circle-check" "regular" "w-4 h-4"
            span_ [class_ "max-md:hidden"] "Resolve"


errorSubscriptionAction :: Projects.ProjectId -> ErrorPatterns.ErrorPattern -> Html ()
errorSubscriptionAction pid err = do
  let isActive = err.subscribed
  form_
    [ id_ "issue-subscription-action"
    , class_ "flex items-center gap-2"
    , hxPost_ $ "/p/" <> pid.toText <> "/issues/errors/" <> UUID.toText err.id.unErrorPatternId <> "/subscribe"
    , hxTarget_ "#issue-subscription-action"
    , hxSwap_ "outerHTML"
    , hxTrigger_ "change"
    ]
    do
      span_ [class_ "text-xs text-textWeak flex items-center gap-1"] do
        faSprite_ "bell" "regular" "w-3 h-3"
        span_ [class_ "max-md:hidden"] "Notify every"
      select_ [class_ "select select-sm max-md:w-20 w-36", name_ "notifyEveryMinutes", Aria.label_ "Notification frequency"] do
        option_ ([value_ "0"] <> [selected_ "true" | not isActive]) "Off"
        forM_ ([(10, "10 min"), (20, "20 min"), (30, "30 min"), (60, "1 hr"), (360, "6 hrs"), (1440, "24 hrs")] :: [(Int, Text)]) \(val, label) ->
          option_ ([value_ (show val)] <> [selected_ "true" | isActive && val == err.notifyEveryMinutes]) (toHtml label)


newtype AssignErrorForm = AssignErrorForm
  { assigneeId :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


newtype ErrorSubscriptionForm = ErrorSubscriptionForm
  { notifyEveryMinutes :: Maybe Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


assignErrorPostH :: Projects.ProjectId -> UUID.UUID -> AssignErrorForm -> ATAuthCtx (RespHeaders (Html ()))
assignErrorPostH pid errUuid form = do
  (sess, _project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  let errId = ErrorPatterns.ErrorPatternId errUuid
      assigneeIdM = form.assigneeId >>= UUID.fromText <&> Projects.UserId
  members <- V.fromList <$> ProjectMembers.selectActiveProjectMembers pid
  errM <- ErrorPatterns.getErrorPatternById errId
  let render eidM aidM = addRespHeaders $ errorAssigneeSection pid eidM aidM members
      isMember = all (\uid -> any (\m -> m.userId == uid) members) assigneeIdM
  case errM of
    Nothing -> addErrorToast "Error not found" Nothing >> render Nothing Nothing
    Just err
      | err.projectId /= pid -> addErrorToast "Error not found for this project" Nothing >> render (Just err.id) err.assigneeId
      | not isMember -> addErrorToast "Assignee must be an active project member" Nothing >> render (Just err.id) err.assigneeId
      | assigneeIdM == err.assigneeId -> addSuccessToast "Assignee unchanged" Nothing >> render (Just err.id) err.assigneeId
      | otherwise -> do
          now <- Time.currentTime
          void $ ErrorPatterns.setErrorPatternAssignee err.id assigneeIdM now
          whenJust assigneeIdM \assigneeId ->
            void $ liftIO $ withResource appCtx.pool \conn ->
              createJob conn "background_jobs" $ BackgroundJobs.ErrorAssigned pid err.id assigneeId
          issueM <- Issues.selectIssueByHash pid err.hash
          let event = maybe Issues.IEUnassigned (const Issues.IEAssigned) assigneeIdM
              meta = assigneeIdM <&> \uid -> AE.object ["assignee_id" AE..= uid]
          whenJust issueM \issue -> Issues.logIssueActivity issue.id event (Just sess.user.id) meta
          addSuccessToast "Assignee updated" Nothing
          render (Just err.id) assigneeIdM


resolveErrorPostH :: Projects.ProjectId -> UUID.UUID -> ATAuthCtx (RespHeaders (Html ()))
resolveErrorPostH pid errUuid = do
  (sess, _project) <- Projects.sessionAndProject pid
  errM <- ErrorPatterns.getErrorPatternById (ErrorPatterns.ErrorPatternId errUuid)
  userPermission <- ProjectMembers.getUserPermission pid sess.user.id
  let canResolve err = userPermission >= Just ProjectMembers.PEdit || err.assigneeId == Just sess.user.id
  case errM of
    Nothing -> addErrorToast "Error not found" Nothing >> addRespHeaders mempty
    Just err
      | err.projectId /= pid -> addErrorToast "Error not found for this project" Nothing >> addRespHeaders mempty
      | not (canResolve err) -> do
          addErrorToast "You do not have permission to resolve this error" Nothing
          addRespHeaders $ errorResolveAction pid err.id err.state False
      | otherwise -> do
          when (err.state /= ErrorPatterns.ESResolved) do
            now <- Time.currentTime
            void $ ErrorPatterns.updateErrorPatternState err.id ErrorPatterns.ESResolved now
            issueM <- Issues.selectIssueByHash pid err.hash
            whenJust issueM \issue -> Issues.logIssueActivity issue.id Issues.IEResolved (Just sess.user.id) Nothing
          addSuccessToast "Error resolved" Nothing
          addRespHeaders $ errorResolveAction pid err.id ErrorPatterns.ESResolved True


errorSubscriptionPostH :: Projects.ProjectId -> UUID.UUID -> ErrorSubscriptionForm -> ATAuthCtx (RespHeaders (Html ()))
errorSubscriptionPostH pid errUuid form = do
  (_sess, _project) <- Projects.sessionAndProject pid
  let errId = ErrorPatterns.ErrorPatternId errUuid
  errM <- ErrorPatterns.getErrorPatternById errId
  case errM of
    Nothing -> addErrorToast "Error not found" Nothing >> addRespHeaders mempty
    Just err
      | err.projectId /= pid -> addErrorToast "Error not found for this project" Nothing >> addRespHeaders mempty
      | otherwise -> do
          let notifyEveryRaw = fromMaybe 0 form.notifyEveryMinutes
              notifyEvery = clamp (1, 1440) $ if notifyEveryRaw == 0 then 30 else notifyEveryRaw
              shouldSubscribe = notifyEveryRaw > 0
          now <- Time.currentTime
          void $ ErrorPatterns.updateErrorPatternSubscription err.id shouldSubscribe notifyEvery now
          addSuccessToast (if shouldSubscribe then "Notifications enabled" else "Notifications disabled") Nothing
          addRespHeaders $ errorSubscriptionAction pid err{ErrorPatterns.subscribed = shouldSubscribe, ErrorPatterns.notifyEveryMinutes = notifyEvery}


-- | Form for AI chat input
newtype AIChatForm = AIChatForm {query :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


-- | System prompt for anomaly investigation AI
anomalySystemPrompt :: UTCTime -> Text
anomalySystemPrompt now =
  unlines
    [ "You are Monoscope's anomaly-investigation assistant — an expert debugger embedded in the issue detail page. The user is on-call and trying to understand a specific issue. You have access to its details, errors, stack traces, and trace data, plus tools that fetch live telemetry."
    , ""
    , "Tone: precise, technical, calm. Answer like a senior SRE pairing on a debug — direct, no fluff."
    , ""
    , "## Current Context"
    , "CURRENT TIME (UTC): " <> show now
    , "Use the current time to interpret relative phrases (e.g. \"last 2 hours\" → `{\"since\": \"2H\"}`)."
    , ""
    , "## Telemetry Schema"
    , "<schema>"
    , Schema.generateSchemaForAI Schema.telemetrySchema
    , "</schema>"
    , ""
    , AI.kqlGuide
    , ""
    , AI.outputFormatInstructions
    , ""
    , "## How To Investigate"
    , "1. Identify the likely root cause from the error type, stack trace, and surrounding telemetry."
    , "2. Use the issue's service / method / path context to narrow down."
    , "3. Suggest concrete debugging steps or fixes — name files, fields, queries when possible."
    , ""
    , "## Tool-Use Policy (overrides the workflow in <output_format>)"
    , "- Analysis questions (\"What could cause this?\", \"Suggest a fix\") → answer DIRECTLY from the issue context. Do NOT call tools, and do NOT call `run_query`."
    , "- Chart / visualization requests (\"plot errors over time\", \"show a chart of...\") → build the KQL query and `widgets` config directly from <schema>. Do NOT call `get_schema`, `get_field_values`, or `run_query` — the chat panel renders the chart from the query alone."
    , "- Call tools ONLY when the answer must contain actual data values from the live store (e.g. \"top 5 services by error count\" where real numbers are required)."
    , ""
    , "## Response Format"
    , "- Lead with a single-sentence summary."
    , "- Follow with bullets or short paragraphs — never walls of text."
    , "- The chat panel is ~400px wide, so brevity matters."
    , "- For chart requests, prioritize a correct KQL query and a data-driven explanation."
    ]


-- | Handle AI chat POST request
-- Designed to power the AI chat in the anomalies page. The chat thread is loaded via htmx and theres an input which when submitted gets sent here.
aiChatPostH :: Projects.ProjectId -> Issues.IssueId -> AIChatForm -> ATAuthCtx (RespHeaders (Html ()))
aiChatPostH pid issueId form
  | T.length form.query > 4000 = addRespHeaders $ aiChatResponse_ pid form.query "Query too long. Maximum 4000 characters allowed." Nothing Nothing Nothing
  | otherwise = do
      appCtx <- ask @AuthContext
      now <- Time.currentTime
      let convId = UUIDId issueId.unUUIDId :: UUIDId "conversation"
      void $ Issues.getOrCreateConversation pid convId Issues.CTAnomaly (AE.object ["issue_id" AE..= issueId])
      issueM <- Issues.selectIssueById issueId
      maybe (respond Nothing convId "Issue not found. Unable to analyze." Nothing Nothing True) (processIssue appCtx now convId) issueM
  where
    respond systemPromptM convId response widgets toolCalls includeUserMsg = do
      when includeUserMsg $ Issues.insertChatMessage pid convId Issues.ChatUser form.query Nothing Nothing
      Issues.insertChatMessage pid convId Issues.ChatAssistant response (AE.toJSON <$> widgets) (AE.toJSON <$> toolCalls)
      addRespHeaders $ aiChatResponse_ pid form.query response widgets toolCalls systemPromptM

    processIssue appCtx now convId issue = do
      fullSystemPrompt <- buildSystemPromptForIssue pid issue now
      let config = (AI.defaultAgenticConfig pid){AI.facetContext = Nothing, AI.customContext = Just fullSystemPrompt, AI.conversationId = Just convId, AI.conversationType = Just Issues.CTAnomaly, AI.systemPromptOverride = Just $ anomalySystemPrompt now, AI.useTimefusion = appCtx.env.enableTimefusionReads}
      result <- AI.runAgenticChatWithHistory config form.query appCtx.config.openaiModel appCtx.config.openaiApiKey
      either
        (\err -> respond (Just fullSystemPrompt) convId ("I encountered an error while analyzing this issue: " <> err) Nothing Nothing False)
        (handleChatResult (Just fullSystemPrompt) convId)
        result

    handleChatResult systemPromptM convId chatResult =
      either
        (\_ -> respond systemPromptM convId chatResult.response Nothing (Just chatResult.toolCalls) False)
        ( \aiResp ->
            let ws = guarded (not . null) $ take 10 aiResp.widgets
                txt = fromMaybe (bool chatResult.response "Here are the requested visualizations:" $ isJust ws) $ mfilter (not . T.null) aiResp.explanation
             in respond systemPromptM convId txt ws (Just chatResult.toolCalls) False
        )
        (AI.parseAgenticResponse chatResult)


-- | Handle AI chat history GET request
aiChatHistoryGetH :: Projects.ProjectId -> Issues.IssueId -> ATAuthCtx (RespHeaders (Html ()))
aiChatHistoryGetH pid issueId = do
  _ <- Projects.sessionAndProject pid
  now <- Time.currentTime
  Issues.selectIssueById issueId >>= \case
    Nothing -> addRespHeaders $ aiChatHistoryView_ pid []
    Just issue -> do
      systemPrompt <- buildSystemPromptForIssue pid issue now
      messages <- Issues.selectChatHistory (UUIDId issueId.unUUIDId :: UUIDId "conversation")
      addRespHeaders $ aiChatHistoryWithSystemPrompt_ pid systemPrompt messages


-- | Build complete system prompt for an issue (shared between POST and GET)
buildSystemPromptForIssue :: Projects.ProjectId -> Issues.Issue -> UTCTime -> ATAuthCtx Text
buildSystemPromptForIssue pid issue now = do
  useTf <- (.env.enableTimefusionReads) <$> ask @AuthContext
  errorM <- bool (pure Nothing) (ErrorPatterns.getErrorPatternByHash pid issue.endpointHash) (issue.issueType == Issues.RuntimeException)
  (traceDataM, spans) <- maybe (pure (Nothing, V.empty)) (fetchTrace useTf) errorM
  alertContextM <- case (issue.issueType, AE.fromJSON @Issues.QueryAlertData (getAeson issue.issueData)) of
    (Issues.QueryAlert, AE.Success alertData) -> do
      let twoDaysAgo = addUTCTime (-172800) now
      monitorM <- runMaybeT do
        monitorId <- hoistMaybe $ UUID.fromText alertData.queryId
        MaybeT $ Monitors.queryMonitorById (Monitors.QueryMonitorId monitorId)
      metricsData <- Charts.queryMetrics Nothing (Just Charts.DTMetric) (Just pid) (Just alertData.queryExpression) Nothing Nothing (Just $ show twoDaysAgo) (Just $ show now) Nothing []
      pure $ Just (alertData, monitorM, metricsData)
    _ -> pure Nothing
  facetSummaryM <- SchemaCatalog.getFacetSummary pid "otel_logs_and_spans" (addUTCTime (-86400) now) now
  pure
    $ unlines
      [ anomalySystemPrompt now
      , ""
      , "--- FACET SUMMARY ---"
      , maybe "" formatFacetSummaryForAI facetSummaryM
      , ""
      , "--- ISSUE CONTEXT ---"
      , buildAIContext issue errorM traceDataM spans alertContextM
      ]
  where
    fetchTrace useTf err =
      fromMaybe (Nothing, V.empty) <$> runMaybeT do
        tId <- hoistMaybe err.recentTraceId
        (trData, spans) <- MaybeT $ Telemetry.getTraceDetails useTf pid tId (Just $ zonedTimeToUTC err.updatedAt) now
        pure (Just trData, V.fromList spans)
    buildAIContext iss errM trDataM spans alertContextM =
      unlines
        $ catMaybes
          [ Just "## Issue Details"
          , Just $ "- **Title**: " <> iss.title
          , Just $ "- **Type**: " <> show iss.issueType
          , Just $ "- **Severity**: " <> display iss.severity
          , Just $ "- **Service**: " <> Issues.serviceLabel iss.service
          , Just $ "- **Recommended Action**: " <> iss.recommendedAction
          , alertContextM <&> \(alertData, monitorM, metricsData) -> formatCompleteAlertContext alertData monitorM metricsData
          , errM <&> \err ->
              unlines
                [ ""
                , "## Error Details"
                , "- **Error Type**: " <> err.errorType
                , "- **Message**: " <> err.message
                , "- **Stack Trace**:"
                , "```"
                , err.errorData.stackTrace
                , "```"
                , maybe "" ("- **Service Name**: " <>) err.errorData.serviceName
                , maybe "" ("- **Request Method**: " <>) err.errorData.requestMethod
                , maybe "" ("- **Request Path**: " <>) err.errorData.requestPath
                ]
          , trDataM >>= \tr ->
              Just
                $ unlines
                  [ ""
                  , "## Trace Context"
                  , "- **Trace ID**: " <> tr.traceId
                  , "- **Duration**: " <> show tr.traceDurationNs <> "ns"
                  , "- **Span Count**: " <> show (V.length spans)
                  ]
          , if V.null spans
              then Nothing
              else
                Just
                  $ unlines
                    [ ""
                    , "## Span Breakdown"
                    , unlines $ V.toList $ flip V.map (V.take 10 spans) $ \s ->
                        "- " <> fromMaybe "unknown" s.name <> " (" <> maybe "n/a" show s.duration <> "ns)"
                    ]
          ]
    formatCompleteAlertContext alertData monitorM metricsData =
      unlines
        $ catMaybes
          [ Just ""
          , Just "## Alert Configuration"
          , Just ""
          , Just "### Query & Thresholds"
          , Just $ "- **Alert Query (KQL)**: `" <> alertData.queryExpression <> "`"
          , Just $ "- **Alert Threshold**: " <> show alertData.thresholdValue <> " (trigger when " <> display alertData.thresholdType <> ")"
          , Just $ "- **Current Value**: " <> show alertData.actualValue
          , Just $ "- **Triggered At**: " <> formatUTC alertData.triggeredAt
          , Just ""
          , Just "### Execution Schedule"
          , monitorM <&> \m -> "- **Check Interval**: Every " <> show m.checkIntervalMins <> " minutes"
          , monitorM <&> \m -> "- **Sustained Duration Required**: " <> show m.thresholdSustainedForMins <> " minutes (threshold must be exceeded for this long)"
          , monitorM <&> \m -> "- **Last Evaluated**: " <> formatUTC m.lastEvaluated
          , Just ""
          , Just "### Trigger Conditions"
          , monitorM <&> \m -> "- **Trigger Direction**: " <> bool "Alert when value EXCEEDS threshold (>)" "Alert when value DROPS BELOW threshold (<)" m.triggerLessThan
          , monitorM >>= (.warningThreshold) <&> \wt -> "- **Warning Threshold**: " <> show wt <> " (warning level)"
          , monitorM >>= (.alertRecoveryThreshold) <&> \art -> "- **Alert Recovery**: Alert clears when value returns to " <> show art
          , monitorM >>= (.warningRecoveryThreshold) <&> \wrt -> "- **Warning Recovery**: Warning clears when value returns to " <> show wrt
          , Just ""
          , Just "### Current Status"
          , monitorM <&> \m -> "- **Status**: " <> show m.currentStatus <> " (last check: " <> show m.currentValue <> ")"
          , monitorM >>= (.alertLastTriggered) <&> \t -> "- **Last Alert Triggered**: " <> formatUTC t
          , monitorM >>= (.warningLastTriggered) <&> \t -> "- **Last Warning Triggered**: " <> formatUTC t
          , Just ""
          , Just "### Notification Configuration"
          , monitorM <&> \m -> "- **Alert Title**: " <> m.alertConfig.title
          , monitorM <&> \m -> "- **Severity**: " <> m.alertConfig.severity
          , monitorM <&> \m -> "- **Email Recipients**: " <> if m.alertConfig.emailAll then "All project members" else T.intercalate ", " (V.toList $ fmap CI.original m.alertConfig.emails)
          , monitorM <&> \m -> if V.null m.alertConfig.slackChannels then "" else "- **Slack Channels**: " <> T.intercalate ", " (V.toList m.alertConfig.slackChannels)
          , guard (metricsData.rowsCount > 0) $> formatQueryResults metricsData
          , guard (metricsData.rowsCount == 0 && isNothing monitorM) $> "\n_Note: Monitor record was deleted. Only basic alert data available._"
          ]
    formatQueryResults md =
      let timestampIdx = V.findIndex (== "timestamp") md.headers
          formatRow = V.imap \idx -> \case
            Just n | Just idx == timestampIdx -> formatUTC $ POSIX.posixSecondsToUTCTime $ realToFrac n
            Just val -> show val
            Nothing -> "N/A"
       in unlines
            [ ""
            , "## Alert Query Results"
            , ""
            , "Recent data points from the alert query (last " <> show (floor md.rowsCount :: Int) <> " measurements):"
            , ""
            , maybe "" formatQueryStats md.stats
            , ""
            , "| " <> T.intercalate " | " (V.toList md.headers) <> " |"
            , "|" <> T.replicate (V.length md.headers) "-------|"
            , unlines $ V.toList $ flip V.map (V.take 20 md.dataset) \row ->
                "| " <> T.intercalate " | " (V.toList $ formatRow row) <> " |"
            , if md.rowsCount > 20 then "... (" <> show (floor md.rowsCount - 20 :: Int) <> " more rows)" else ""
            ]
      where
        formatQueryStats stats =
          unlines
            [ "**Statistics:**"
            , "- Min: " <> show stats.min
            , "- Max: " <> show stats.max
            , "- Mean: " <> show stats.mean <> " (average)"
            , "- Sum: " <> show stats.sum
            , "- Count: " <> show stats.count <> " data points"
            ]
    formatFacetSummaryForAI summary =
      let FacetData facetMap = summary.facetJson
          formatField (fieldName, values) =
            "- "
              <> fieldName
              <> ": "
              <> T.intercalate ", " (map (\fv -> fv.value <> " (" <> show fv.count <> ")") $ take 10 values)
              <> bool "" ", ..." (length values > 10)
          topFields = take 30 $ sortOn (\(_, vs) -> negate $ sum $ map (.count) vs) $ HM.toList facetMap
       in unlines
            $ "Available telemetry fields (top values by frequency):"
            : map formatField topFields
              <> ["... and " <> show (HM.size facetMap - 30) <> " more fields" | HM.size facetMap > 30]


-- | Render a single chat response (user question + AI answer)
aiChatResponse_ :: Projects.ProjectId -> Text -> Text -> Maybe [Widget.Widget] -> Maybe [AI.ToolCallInfo] -> Maybe Text -> Html ()
aiChatResponse_ pid userQuery explanation widgetsM toolCallsM systemPromptM =
  div_ [class_ "animate-fade-in py-3 border-b border-strokeWeak last:border-b-0"] do
    -- User question
    div_ [class_ "flex items-start gap-2.5 mb-3"] do
      chatAvatar_ "bg-fillWeak" "user" "text-textWeak"
      p_ [class_ "text-sm text-textStrong mt-0.5"] $ toHtml userQuery
    -- AI response
    div_ [class_ "flex items-start gap-2.5"] do
      chatAvatar_ "bg-fillBrand-weak" "sparkles" "text-fillBrand-strong"
      div_ [class_ "flex-1 min-w-0 mt-0.5"] do
        div_ [class_ "prose prose-sm text-textStrong max-w-none leading-relaxed"] $ renderMarkdown explanation
        whenJust widgetsM \widgets -> do
          let processedWidgets = maybe widgets (`processWidgetsWithToolData` widgets) toolCallsM
          div_ [class_ "grid grid-cols-1 gap-3 mt-3"] $ forM_ processedWidgets \widget ->
            div_ [class_ "w-full aspect-[3/1]"] $ Widget.widget_ widget{Widget._projectId = Just pid}
    -- Collapsed debug info (tool calls + system prompt)
    let toolCalls = fromMaybe [] toolCallsM
    unless (null toolCalls && isNothing systemPromptM)
      $ details_ [class_ "mt-2 ml-[2.125rem] border border-strokeWeak rounded-lg text-xs group/debug"] do
        summary_ [class_ "cursor-pointer px-2.5 py-1.5 text-textWeak hover:bg-fillWeaker list-none flex items-center gap-1.5"] do
          faSprite_ "chevron-right" "regular" "w-2.5 h-2.5 transition-transform group-open/debug:rotate-90"
          span_ [] $ toHtml @Text $ bool "System context" (show (length toolCalls) <> " tool calls") (not (null toolCalls))
        div_ [class_ "px-2.5 py-2 border-t border-strokeWeak bg-fillWeaker/50"] do
          forM_ toolCalls toolCallView_
          whenJust systemPromptM \sp ->
            details_ [class_ $ bool "" "mt-2 border-t border-strokeWeak pt-2 " (not (null toolCalls)) <> "group/sp"] do
              summary_ [class_ "cursor-pointer text-textWeak hover:text-textStrong list-none flex items-center gap-1.5"] do
                faSprite_ "chevron-right" "regular" "w-2.5 h-2.5 transition-transform group-open/sp:rotate-90"
                span_ [] "System Prompt"
              div_ [class_ "mt-1 font-mono whitespace-pre-wrap text-textWeak max-h-48 overflow-y-auto"] $ toHtml sp
  where
    chatAvatar_ bg icon color = div_ [class_ $ "shrink-0 w-6 h-6 rounded-full flex items-center justify-center " <> bg] $ faSprite_ icon "regular" ("w-3 h-3 " <> color)


-- | Render a single tool call
toolCallView_ :: AI.ToolCallInfo -> Html ()
toolCallView_ tc =
  div_ [class_ "flex flex-col gap-1 py-2 border-b border-strokeWeak last:border-0"] do
    div_ [class_ "flex items-center gap-2 flex-wrap"] do
      span_ [class_ "font-mono text-xs px-2 py-0.5 bg-fillWeak rounded"] $ toHtml tc.name
      whenJust (Map.lookup "query" tc.args) $ span_ [class_ "text-xs text-textWeak break-all"] . toHtml . show
    unless (T.null tc.resultPreview) $ div_ [class_ "text-xs text-textWeak font-mono pl-4 whitespace-pre-wrap break-all"] $ toHtml $ "→ " <> tc.resultPreview


withIssueDataH :: (AE.FromJSON a, Applicative m) => Aeson AE.Value -> (a -> m ()) -> m ()
withIssueDataH d = whenJust (parseMaybe AE.parseJSON $ getAeson d)


-- | Process widgets to use cached tool call data (no re-query)
processWidgetsWithToolData :: [AI.ToolCallInfo] -> [Widget.Widget] -> [Widget.Widget]
processWidgetsWithToolData toolCalls = map \w -> case w.query >>= findToolCallData toolCalls of
  Nothing -> w
  Just rawJson -> maybe w (\ds -> w{Widget.dataset = Just ds, Widget.eager = Just True}) (toolDataToDataset rawJson)


-- | Find matching tool call data for a widget query
findToolCallData :: [AI.ToolCallInfo] -> Text -> Maybe AE.Value
findToolCallData toolCalls widgetQuery = listToMaybe [rd | tc <- toolCalls, tc.name == "run_query", Just (AE.String q) <- [Map.lookup "query" tc.args], norm q == norm widgetQuery, Just rd <- [tc.rawData]]
  where
    norm = unwords . words -- normalize for comparison (whitespace-insensitive)


-- | Convert tool call raw data to WidgetDataset
toolDataToDataset :: AE.Value -> Maybe Widget.WidgetDataset
toolDataToDataset json = flip parseMaybe json $ AE.withObject "RawData" \obj -> do
  headers <- obj AE..: "headers" :: Parser [Text]
  dataRows <- obj AE..: "data" :: Parser (V.Vector (V.Vector AE.Value))
  count <- obj AE..:? "count"
  let source = AE.toJSON $ V.cons (AE.toJSON <$> V.fromList headers) (fmap AE.toJSON <$> dataRows)
  pure Widget.WidgetDataset{source, rowsPerMin = Nothing, value = count, from = Nothing, to = Nothing, stats = Nothing}


-- | Render chat history using forM_ over paired messages
aiChatHistoryView_ :: Projects.ProjectId -> [Issues.AIChatMessage] -> Html ()
aiChatHistoryView_ pid msgs = forM_ (pairUserAssistant msgs) \(u, a) -> do
  let (explanation, widgets) = parseStoredContent a.content a.widgets
  aiChatResponse_ pid u.content explanation widgets (parseStoredJSON @[AI.ToolCallInfo] a.metadata) Nothing


-- | Render chat history with system prompt as first message
aiChatHistoryWithSystemPrompt_ :: Projects.ProjectId -> Text -> [Issues.AIChatMessage] -> Html ()
aiChatHistoryWithSystemPrompt_ pid systemPrompt msgs = do
  details_ [class_ "my-2 border border-strokeWeak rounded-lg group/sp"] do
    summary_ [class_ "cursor-pointer px-2.5 py-1.5 text-xs text-textWeak hover:bg-fillWeaker list-none flex items-center gap-1.5"] do
      faSprite_ "chevron-right" "regular" "w-2.5 h-2.5 transition-transform group-open/sp:rotate-90"
      faSprite_ "file-lines" "regular" "w-3 h-3"
      span_ [] "System Prompt"
    div_ [class_ "px-2.5 py-2 border-t border-strokeWeak bg-fillWeaker/50 text-xs font-mono whitespace-pre-wrap text-textWeak max-h-48 overflow-y-auto"] $ toHtml systemPrompt
  aiChatHistoryView_ pid msgs


-- | Pair user messages with their following assistant responses, skipping unpaired
pairUserAssistant :: [Issues.AIChatMessage] -> [(Issues.AIChatMessage, Issues.AIChatMessage)]
pairUserAssistant (u : a : rest) | u.role == Issues.ChatUser && a.role == Issues.ChatAssistant = (u, a) : pairUserAssistant rest
pairUserAssistant (_ : rest) = pairUserAssistant rest
pairUserAssistant [] = []


-- | Decode a stored JSONB value (anomaly metadata, widget lists) into a typed payload.
parseStoredJSON :: AE.FromJSON a => Maybe (Aeson AE.Value) -> Maybe a
parseStoredJSON = (>>= parseMaybe AE.parseJSON . getAeson)


-- | Parse stored content - try JSON format first (stripping code blocks), fall back to plain text
parseStoredContent :: Text -> Maybe (Aeson AE.Value) -> (Text, Maybe [Widget.Widget])
parseStoredContent content storedWidgets =
  case AI.parseLLMResponse content of
    Right aiResp -> (fromMaybe "" aiResp.explanation, guarded (not . null) aiResp.widgets)
    Left _ -> (content, parseStoredJSON @[Widget.Widget] storedWidgets)


-- | AI Chat body (response container + input bar, no header)
anomalyAIChatBody_ :: Projects.ProjectId -> Issues.IssueId -> Html ()
anomalyAIChatBody_ pid issueId = do
  let issueIdT = UUID.toText issueId.unUUIDId
      baseUrl = "/p/" <> pid.toText <> "/issues/" <> issueIdT
  div_
    [ id_ "ai-response-container"
    , class_ "flex-1 overflow-y-auto flex flex-col px-3"
    , hxGet_ $ baseUrl <> "/ai_chat/history"
    , hxTrigger_ "load-chat once"
    , -- htmx 4's native event detail carries no `elt` (the compat shim backfills it only on
      -- the legacy aliases), so branch on the swap target via `event.target` instead.
      term "hx-on::after:swap" "window.evalScriptsFromContent && window.evalScriptsFromContent(event.target === this ? this : this.lastElementChild); this.lastElementChild?.scrollIntoView({behavior: 'smooth', block: 'start'})"
    ]
    ""
  div_ [class_ "shrink-0 border-t border-strokeWeak p-3 flex flex-col gap-2"] do
    form_
      [ hxPost_ $ baseUrl <> "/ai_chat"
      , hxTarget_ "#ai-response-container"
      , hxSwap_ "beforeend"
      , hxIndicator_ "#ai-chat-loader"
      , term "hx-on::after:request" "this.reset()"
      ]
      $ div_ [class_ "flex items-center gap-2 bg-fillWeaker rounded-lg px-3 py-2 has-[:focus]:ring-1 has-[:focus]:ring-strokeBrand-weak transition-shadow"] do
        input_
          [ class_ "flex-1 bg-transparent border-none outline-none text-textStrong placeholder-textWeak text-sm"
          , placeholder_ "Ask about this issue..."
          , name_ "query"
          , id_ "ai-chat-input"
          , autocomplete_ "off"
          ]
        span_ [class_ "htmx-indicator", id_ "ai-chat-loader"] $ faSprite_ "spinner" "regular" "w-4 h-4 animate-spin text-iconBrand"
        button_ [type_ "submit", class_ "p-1.5 rounded-lg bg-fillBrand-strong text-white hover:opacity-90 transition-opacity tap-target cursor-pointer", Aria.label_ "Send message"] $ faSprite_ "arrow-right" "regular" "w-3.5 h-3.5"
    div_ [class_ "flex gap-1.5 flex-wrap"] $ forM_ ["What could cause this?", "Show related logs", "Suggest a fix"] \txt ->
      button_
        [ type_ "button"
        , class_ "text-xs px-2 py-1.5 rounded-full bg-fillWeaker text-textWeak hover:text-textStrong hover:bg-fillWeak transition-colors cursor-pointer tap-target"
        , term "_" $ "on click set #ai-chat-input.value to '" <> txt <> "' then call #ai-chat-input.form.requestSubmit()"
        ]
        $ toHtml @Text txt


anomalyListGetH
  :: Projects.ProjectId
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Endpoints.EndpointId
  -> Maybe Text
  -> [Text]
  -> [Text]
  -> Maybe Text
  -> Maybe Text
  -> ATAuthCtx (RespHeaders AnomalyListGet)
anomalyListGetH pid _layoutM filterTM sortM timeFilter pageM perPageM loadM _endpointM periodM serviceFilters typeFilters _hxRequestM _hxBoostedM = do
  (_, project, bw) <- mkPageCtx pid
  let (ackd, archived, currentFilterTab) = case filterTM of
        Just "Inbox" -> (Just False, Just False, "Inbox")
        Just "Acknowledged" -> (Just True, Nothing, "Acknowledged")
        Just "Archived" -> (Nothing, Just True, "Archived")
        _ -> (Just False, Just False, "Inbox")
      filterV = fromMaybe "14d" timeFilter
      pageInt = fromMaybe 0 $ readMaybe . toString =<< pageM
      perPage = fromMaybe 25 $ readMaybe . toString =<< perPageM
      currentSort = fromMaybe "-created_at" sortM
      period = fromMaybe "24h" periodM
  freeTierStatus <- checkFreeTierStatus pid project.paymentPlan
  currTime <- Time.currentTime
  ((issues, totalCount), (availableServices, availableTypes)) <-
    concurrently
      (Issues.selectIssues pid ackd archived perPage (pageInt * perPage) Nothing (Just currentSort) period serviceFilters typeFilters)
      ( concurrently
          (Hasql.interp [HI.sql| SELECT DISTINCT service FROM apis.issues WHERE project_id = #{pid} AND service IS NOT NULL |])
          (Hasql.interp [HI.sql| SELECT DISTINCT issue_type::text FROM apis.issues WHERE project_id = #{pid} |])
      )

  let filterParams = foldMap ("&service=" <>) serviceFilters <> foldMap ("&type=" <>) typeFilters
      baseUrl = "/p/" <> pid.toText <> "/issues?filter=" <> currentFilterTab <> "&sort=" <> currentSort <> "&period=" <> period <> filterParams
      paginationConfig =
        Pagination
          { currentPage = pageInt
          , perPage = perPage
          , totalCount = totalCount
          , baseUrl = baseUrl
          , targetId = "anomalyListContainer"
          }
      serviceMenu = FilterMenu{label = "Service", paramName = "service", multiSelect = True, options = map (\s -> FilterOption{label = s, value = s, isActive = s `elem` serviceFilters}) availableServices}
      typeMenu = FilterMenu{label = "Type", paramName = "type", multiSelect = True, options = map (\t -> FilterOption{label = t, value = t, isActive = t `elem` typeFilters}) availableTypes}
      issuesVM = V.fromList $ map (IssueVM False currTime filterV) issues
      tableActions =
        TableHeaderActions
          { baseUrl
          , targetId = "anomalyListContainer"
          , sortOptions =
              [ ("Newest", "Most recently created", "-created_at")
              , ("Oldest", "Oldest issues first", "+created_at")
              , ("Recently Updated", "Most recently updated", "-updated_at")
              , ("Name (A-Z)", "Sort alphabetically", "+title")
              , ("Name (Z-A)", "Sort reverse alphabetically", "-title")
              ]
          , currentSort
          , filterMenus = [serviceMenu | not (null availableServices)] <> [typeMenu | not (null availableTypes)]
          , activeFilters = [("Service", serviceFilters) | not (null serviceFilters)] <> [("Type", typeFilters) | not (null typeFilters)]
          , headerExtra = Nothing
          }
      issuesTable =
        Table
          { config =
              def
                { elemID = "anomalyListForm"
                , containerId = Just "anomalyListContainer"
                , addPadding = True
                , renderAsTable = True
                , bulkActionsInHeader = Just 0
                , refreshOnEvent = Just ("issuesListChanged", baseUrl)
                }
          , columns = issueColumns pid period (Just $ periodToggle_ baseUrl "anomalyListContainer" period)
          , rows = issuesVM
          , features =
              def
                { rowId = Just issueRowId
                , rowAttrs = Just issueRowAttrs
                , bulkActions = issueBulkActions pid currentFilterTab
                , search = Just ClientSide
                , tableHeaderActions = Just tableActions
                , pagination = if totalCount > 0 then Just paginationConfig else Nothing
                , zeroState = Just $ issueZeroState pid currentFilterTab
                }
          }
      bwconf =
        bw
          { pageTitle = "Issues"
          , menuItem = Just "Issues"
          , freeTierStatus = freeTierStatus
          , headContent = Just highlightJsHead_
          , navTabs = Just $ div_ [class_ "flex items-center gap-1.5"] do
              toHtml
                $ TabFilter
                  { current = currentFilterTab
                  , currentURL = baseUrl
                  , clientSide = False
                  , options =
                      [ TabFilterOpt "Inbox" Nothing Nothing
                      , TabFilterOpt "Acknowledged" Nothing Nothing
                      , TabFilterOpt "Archived" Nothing Nothing
                      ]
                  }
              -- Each tab differs only in how long the silence lasts and whether
              -- the issue can come back; saying so is what stops "acknowledged"
              -- from being a mystery.
              span_ [class_ "tooltip tooltip-bottom", data_ "tip" (tabBlurb currentFilterTab)]
                $ faSprite_ "circle-info" "regular" "h-4 w-4 text-iconNeutral"
          }
  addRespHeaders
    $ if loadM == Just "true"
      then ALRows $ TableRows{columns = issueColumns pid period Nothing, rows = issuesVM, emptyState = Nothing, renderAsTable = True, rowId = Just issueRowId, rowAttrs = Just issueRowAttrs, pagination = if totalCount > 0 then Just paginationConfig else Nothing}
      else ALPage $ PageCtx bwconf issuesTable


-- | One line under the tab strip saying what the tab *means*.
tabBlurb :: Text -> Text
tabBlurb = \case
  "Acknowledged" -> "Someone owns these. Notifications are paused until the acknowledgement expires or the issue regresses."
  "Archived" -> "Not actionable. Hidden and never notified — unarchive to bring one back."
  _ -> "Needs triage. Acknowledge to pause notifications, or archive if it isn't actionable."


-- | Bulk actions offered on each tab: only transitions that make sense from the
-- state you're looking at.
issueBulkActions :: Projects.ProjectId -> Text -> [BulkAction]
issueBulkActions pid tab =
  [ BulkAction{icon = Just i, title = t, uri = "/p/" <> pid.toText <> "/issues/bulk_actions/" <> a}
  | (i, t, a) <- case tab of
      "Acknowledged" -> [("arrow-rotate-left", "Unacknowledge", "unacknowledge"), ("archive", "Archive", "archive")]
      "Archived" -> [("arrow-rotate-left", "Unarchive", "unarchive")]
      _ -> [("check", "Acknowledge", "acknowledge"), ("archive", "Archive", "archive")]
  ]


issueZeroState :: Projects.ProjectId -> Text -> ZeroState
issueZeroState pid = \case
  "Acknowledged" ->
    ZeroState "circle-check" "Nothing acknowledged" "Acknowledge an issue to pause its notifications while you work on it." "Go to Inbox" (Right $ inboxUrl <> "Inbox")
  "Archived" ->
    ZeroState "archive" "Nothing archived" "Archive the issues that aren't worth acting on. They stay hidden and never notify." "Go to Inbox" (Right $ inboxUrl <> "Inbox")
  _ ->
    ZeroState "empty-set" "Nothing to triage" "New issues and errors land here automatically once you integrate an SDK." "View SDK setup guides" (Right "https://monoscope.tech/docs/sdks/")
  where
    inboxUrl = "/p/" <> pid.toText <> "/issues?filter="


data AnomalyListGet
  = ALPage (PageCtx (Table IssueVM))
  | ALRows (TableRows IssueVM)


instance ToHtml AnomalyListGet where
  toHtml (ALPage pg) = toHtml pg
  toHtml (ALRows rows) = toHtml rows
  toHtmlRaw = toHtml


issueRowAttrs :: IssueVM -> [Attribute]
issueRowAttrs (IssueVM _ _ _ issue) = [class_ $ "group/row hover:bg-fillWeaker " <> bg] <> sty
  where
    (bg, sty) = case display issue.base.severity of
      "critical" -> ("bg-fillError-weak", [style_ "box-shadow: inset 3px 0 0 var(--color-fillError-strong)"])
      "warning" -> ("bg-fillWarning-weak", [style_ "box-shadow: inset 3px 0 0 var(--color-fillWarning-strong)"])
      _ -> ("", [])


issueRowId :: IssueVM -> Text
issueRowId (IssueVM _ _ _ issue) = issue.base.id.toText


-- | (icon, colorClass, tooltip) — uses shape+color so status isn't color-only
anomalyStatusIndicator :: Bool -> Bool -> Text -> (Text, Text, Text)
anomalyStatusIndicator _ True _ = ("archive", "text-fillStrong", "Archived \x2014 hidden, no notifications")
anomalyStatusIndicator True False _ = ("bell-slash", "text-fillSuccess-strong", "Acknowledged \x2014 notifications paused")
anomalyStatusIndicator False False "critical" = ("octagon-exclamation", "text-fillError-strong", "Critical")
anomalyStatusIndicator False False "warning" = ("triangle-alert", "text-fillWarning-strong", "Warning")
anomalyStatusIndicator False False _ = ("circle-alert", "text-textWeak", "Active")


data IssueVM = IssueVM Bool UTCTime Text Issues.IssueL
  deriving stock (Show)


issueColumns :: Projects.ProjectId -> Text -> Maybe (Html ()) -> [Column IssueVM]
issueColumns pid period toggleM =
  [ col "Issue" (renderIssueMainCol pid) & withAttrs [class_ "min-w-0 max-w-0 w-full"]
  , col ("Events (" <> period <> ")") renderIssueEventsCol & withAttrs [class_ "w-24 max-md:hidden"]
  , col "Last Seen" renderIssueDateCol & withAttrs [class_ "w-24 max-md:hidden"]
  , col "Activity" renderIssueChartCol & withAttrs [class_ "w-40 max-md:hidden"] & maybe identity withColHeaderExtra toggleM
  ]


renderIssueEventsCol :: IssueVM -> Html ()
renderIssueEventsCol (IssueVM isWidget _ _ issue) =
  unless isWidget
    $ span_ [class_ $ "tabular-nums font-medium text-sm " <> countStyle issue.eventCount]
    $ toHtml
    $ formatWithCommas (fromIntegral issue.eventCount)
  where
    countStyle n
      | n >= 100 = "text-fillError-strong"
      | n >= 10 = "text-fillWarning-strong"
      | otherwise = "text-textStrong"


renderIssueDateCol :: IssueVM -> Html ()
renderIssueDateCol (IssueVM _ currTime _ issue) =
  span_ [class_ "text-xs text-textWeak"] $ toHtml $ compactTimeAgo $ toText $ prettyTimeAuto currTime $ zonedTimeToUTC issue.base.createdAt


renderIssueChartCol :: IssueVM -> Html ()
renderIssueChartCol (IssueVM _ _ _ issue) = sparkline_ $ V.toList issue.activityBuckets


highlightJsHead_ :: Monad m => HtmlT m ()
highlightJsHead_ = do
  link_ [rel_ "stylesheet", href_ (assetUrl "/public/assets/deps/highlightjs/atom-one-light.min.css"), media_ "screen", id_ "hljs-light"]
  link_ [rel_ "stylesheet", href_ (assetUrl "/public/assets/deps/highlightjs/atom-one-dark.min.css"), media_ "screen", id_ "hljs-dark"]
  script_ [src_ (assetUrl "/public/assets/deps/highlightjs/highlight.min.js")] ("" :: Text)
  script_ [src_ (assetUrl "/public/assets/deps/highlightjs/sql.min.js")] ("" :: Text)
  script_
    """
    function setHljsTheme() {
      const dark = document.body.getAttribute('data-theme') === 'dark';
      document.getElementById('hljs-light').disabled = dark;
      document.getElementById('hljs-dark').disabled = !dark;
    }
    function highlightSnippets(root) { root.querySelectorAll('code:not(.hljs)').forEach(el => hljs.highlightElement(el)); }
    document.addEventListener('DOMContentLoaded', () => { setHljsTheme(); highlightSnippets(document); });
    document.addEventListener('htmx:after:swap', e => highlightSnippets(e.detail.elt));
    """


renderLogContent_ :: Monad m => Text -> HtmlT m ()
renderLogContent_ txt =
  if "⇒" `T.isInfixOf` txt
    then div_ [class_ "flex flex-wrap items-center gap-1 p-4 max-h-80 overflow-y-auto"] $ renderSummaryText_ txt
    else div_ [class_ "p-4 max-h-80 overflow-y-auto"] $ pre_ [class_ "text-sm text-textWeak font-mono whitespace-pre-wrap [&_code.hljs]:!bg-transparent [&_code.hljs]:!p-0"] $ code_ [] $ toHtml txt


renderSummaryText_ :: Monad m => Text -> HtmlT m ()
renderSummaryText_ = traverse_ (summaryToken_ False) . words


renderIssueTitle_ :: Issues.IssueL -> Html ()
renderIssueTitle_ Issues.IssueL{base}
  | T.null title = "(Untitled)"
  | "⇒" `T.isInfixOf` title = renderSummaryText_ title
  | looksLikeRawPattern title = span_ [class_ "font-mono text-xs break-all"] $ renderWithPlaceholders_ title
  | otherwise = renderWithPlaceholders_ title
  where
    title =
      foldl'
        (\t pfx -> fromMaybe t $ T.stripPrefix pfx t)
        base.title
        ["New Log Pattern: ", "Log Pattern Spike: ", "Log Pattern Drop: ", "New Log Pattern Detected: "]
    looksLikeRawPattern t = any (`T.isInfixOf` t) [";right-", "v{", "<*>", "]{", "ERROR ERROR"]


-- | Render text with <> placeholders styled as distinct tokens
renderWithPlaceholders_ :: Monad m => Text -> HtmlT m ()
renderWithPlaceholders_ = mconcat . intersperse (span_ [class_ "text-textWeak opacity-60"] "<>") . map toHtml . T.splitOn "<>"


renderIssueMainCol :: Projects.ProjectId -> IssueVM -> Html ()
renderIssueMainCol pid (IssueVM _ currTime period issue) = do
  let b = issue.base
      isAcknowledged = isJust b.acknowledgedAt
      isArchived = isJust b.archivedAt
      (icon, iconColor, tooltip) = anomalyStatusIndicator isAcknowledged isArchived (display b.severity)
      issueUrl = "/p/" <> pid.toText <> "/issues/" <> b.id.toText
      stateBadges = do
        severityBadge_ (display b.severity)
        issueStateBadge_ issue.latestStateEvent
        ackBadge_ currTime b
  div_ [class_ "flex flex-col gap-1 py-0.5 min-w-0"] do
    div_ [class_ "flex items-center gap-2 min-w-0"] do
      div_ [class_ "text-sm line-clamp-2 min-w-0"] do
        span_ [class_ $ "inline-flex align-middle mr-1 " <> iconColor, title_ tooltip, Aria.label_ tooltip] $ faSprite_ icon "regular" "w-3.5 h-3.5"
        span_ [class_ "text-xs tabular-nums mr-1 text-textWeak max-md:text-textStrong max-md:font-medium"] $ toHtml $ "#" <> show b.seqNum <> " "
        a_ ([href_ issueUrl, class_ "font-medium text-textStrong hover:text-textBrand transition-colors"] <> navTabAttrs) $ renderIssueTitle_ issue
      span_ [class_ "shrink-0 flex items-center gap-1.5 max-md:hidden"] stateBadges
      div_ [class_ "shrink-0 flex gap-1 items-center opacity-0 group-hover/row:opacity-100 has-[:focus-within]:opacity-100 transition-opacity max-md:hidden"] do
        inlineBtn (bool "Acknowledge \x2014 pause notifications" "Unacknowledge \x2014 resume notifications" isAcknowledged) (bool "check" "arrow-rotate-left" isAcknowledged) (hxGet_ $ issueUrl <> bool "/acknowledge" "/unacknowledge" isAcknowledged) []
        unless isAcknowledged
          $ durationMenu_ ("ack-pop-" <> b.id.toText) "Acknowledge for\x2026" (\q -> [hxGet_ $ issueUrl <> "/acknowledge" <> durationQuery "duration" q, hxSwap_ "none"]) \popId ->
            inlineBtn "Acknowledge for a set time" "clock" (term "popovertarget" popId) [style_ $ "anchor-name: --anchor-" <> popId]
        inlineBtn (bool "Archive \x2014 hide it and stop notifying" "Unarchive \x2014 move back to the Inbox" isArchived) "archive" (hxGet_ $ issueUrl <> bool "/archive" "/unarchive" isArchived) []
    div_ [class_ "hidden max-md:flex items-center gap-1.5 flex-wrap"] stateBadges
    div_ [class_ "max-md:hidden"] $ issuePreview_ issue
    div_ [class_ "hidden max-md:flex items-center justify-between text-xs text-textWeak"] do
      div_ [class_ "flex items-center gap-1.5"] do
        span_ [class_ $ "tabular-nums" <> bool "" " font-medium text-textStrong" (issue.eventCount > 100)] $ toHtml $ show issue.eventCount <> bool " event" " events" (issue.eventCount /= 1) <> " (" <> period <> ")"
        span_ [class_ "opacity-30"] "·"
        span_ [] $ toHtml $ compactTimeAgo $ toText $ prettyTimeAuto currTime $ zonedTimeToUTC b.createdAt
      div_ [class_ "flex items-center gap-3"] do
        button_ [type_ "button", class_ "cursor-pointer text-textBrand tap-target font-medium", hxSwap_ "none", hxGet_ $ issueUrl <> bool "/acknowledge" "/unacknowledge" isAcknowledged] $ toHtml $ bool "Ack" "Unack" isAcknowledged
        button_ [type_ "button", class_ "cursor-pointer text-textBrand tap-target font-medium", hxSwap_ "none", hxGet_ $ issueUrl <> bool "/archive" "/unarchive" isArchived] $ toHtml $ bool "Archive" "Unarchive" isArchived
  where
    -- Rows swap nothing: the handler fires `issuesListChanged` and the table
    -- reloads, so an acknowledged row actually leaves the Inbox.
    inlineBtn tip icon hxAction extraAttrs =
      button_ ([type_ "button", term "data-tippy-content" tip, Aria.label_ tip, class_ "cursor-pointer hover:text-textBrand transition-colors tap-target", hxSwap_ "none", hxAction] <> extraAttrs)
        $ faSprite_ icon "regular" "h-3.5 w-3.5"


issueCardCompact_ :: Projects.ProjectId -> UTCTime -> Issues.IssueL -> Html ()
issueCardCompact_ pid now issue = do
  let b = issue.base
      (icon, iconColor, tooltip) = anomalyStatusIndicator (isJust b.acknowledgedAt) (isJust b.archivedAt) (display b.severity)
      issueUrl = "/p/" <> pid.toText <> "/issues/" <> b.id.toText
  a_ ([href_ issueUrl, class_ "block border border-strokeWeak rounded-xl p-3 hover:bg-bgRaised transition-colors"] <> navTabAttrs) do
    div_ [class_ "flex items-center gap-2 min-w-0"] do
      span_ [class_ $ "shrink-0 " <> iconColor, title_ tooltip, Aria.label_ tooltip] $ faSprite_ icon "regular" "w-3.5 h-3.5"
      span_ [class_ "text-xs text-textWeak shrink-0 tabular-nums"] $ toHtml $ "#" <> show b.seqNum
      span_ [class_ "text-sm font-medium text-textStrong truncate min-w-0"] $ renderIssueTitle_ issue
      severityBadge_ (display b.severity)
      span_ [class_ "text-xs text-textWeak shrink-0 ml-auto"] $ toHtml $ compactTimeAgo $ toText $ prettyTimeAuto now $ zonedTimeToUTC b.createdAt
    issuePreview_ issue


severityBadge_ :: Text -> Html ()
severityBadge_ = \case
  "critical" -> span_ [class_ "badge badge-sm bg-fillError-weak text-fillError-strong border border-strokeError-strong"] "CRITICAL"
  "warning" -> span_ [class_ "badge badge-sm bg-fillWarning-weak text-fillWarning-strong border border-strokeWarning-weak"] "WARNING"
  _ -> pass


issueStateBadge_ :: Maybe Issues.IssueEvent -> Html ()
issueStateBadge_ = \case
  Just Issues.IERegressed -> badge "bg-fillError-weak text-fillError-strong border-strokeError-strong" "REGRESSED"
  Just Issues.IEEscalated -> badge "bg-fillError-weak text-fillError-strong border-strokeError-strong" "ESCALATED"
  Just Issues.IEResolved -> badge "bg-fillSuccess-weak text-fillSuccess-strong border-strokeSuccess-strong" "RESOLVED"
  Just Issues.IEAutoResolved -> badge "bg-fillSuccess-weak text-fillSuccess-strong border-strokeSuccess-strong" "RESOLVED"
  Just Issues.IEReopened -> badge "bg-fillWarning-weak text-fillWarning-strong border-strokeWarning-weak" "REOPENED"
  Just Issues.IEAckExpired -> badge "bg-fillWarning-weak text-fillWarning-strong border-strokeWarning-weak" "ACK EXPIRED"
  _ -> pass
  where
    badge cls = span_ [class_ $ "badge badge-sm border " <> cls]


issuePreview_ :: Issues.IssueL -> Html ()
issuePreview_ Issues.IssueL{base} = div_ [class_ "flex items-center gap-2 min-w-0 overflow-hidden text-xs text-textWeak"] do
  issueTypeBadge base.issueType base.critical
  whenJust base.service $ span_ [class_ "shrink-0", term "data-tippy-content" "Service"] . toHtml
  span_ [class_ "shrink-0 opacity-40"] "·"
  snippet
  where
    snippet = case base.issueType of
      Issues.RuntimeException -> withIssueDataH @Issues.RuntimeExceptionData base.issueData \d ->
        previewSnippet $ d.errorType <> ": " <> d.errorMessage
      Issues.QueryAlert -> withIssueDataH @Issues.QueryAlertData base.issueData \d ->
        previewSnippet d.queryExpression
      Issues.LogPattern -> withIssueDataH @Issues.LogPatternData base.issueData \d ->
        logPatternPreview d.logPattern d.sampleMessage
      Issues.LogPatternRateChange -> withIssueDataH @Issues.LogPatternRateChangeData base.issueData \d ->
        logPatternPreview d.logPattern d.sampleMessage
      Issues.ApiChange -> withIssueDataH @Issues.APIChangeData base.issueData \d ->
        previewSnippet $ d.endpointMethod <> " " <> d.endpointPath <> if T.null d.endpointHost then "" else " on " <> d.endpointHost
    previewSnippet txt = span_ [class_ "font-mono truncate min-w-0", term "data-tippy-content" txt] $ renderWithPlaceholders_ $ unescSummary txt
    summaryPreview txt = span_ [class_ "truncate min-w-0"] $ renderSummaryText_ txt
    logPatternPreview pat sampleMsg
      | "⇒" `T.isInfixOf` pat = summaryPreview pat
      | Just msg <- sampleMsg, not (T.null msg) = previewSnippet msg
      | otherwise = previewSnippet pat


-- | "Ack'd · 6h left" / "Acknowledged indefinitely" chip. Silence has an end, and
-- the list is where you need to see it without opening anything.
ackBadge_ :: UTCTime -> Issues.Issue -> Html ()
ackBadge_ now b = whenJust (zonedTimeToUTC <$> b.acknowledgedUntil <* b.acknowledgedAt) \until' ->
  let lbl = untilLabel "Ack'd" now until'
   in span_ [class_ "badge badge-sm badge-ghost gap-1 shrink-0", term "data-tippy-content" $ untilLabel "Acknowledged" now until' <> " \x2014 notifications are paused"] do
        faSprite_ "bell-slash" "regular" "h-3 w-3"
        toHtml lbl


-- | Acknowledge control for the issue detail header. Unacknowledged: a primary
-- button (silence until it regresses) joined to a caret opening the duration
-- menu. Acknowledged: the remaining time, which un-acknowledges on click.
anomalyAcknowledgeButton :: Projects.ProjectId -> Issues.IssueId -> UTCTime -> Maybe UTCTime -> Html ()
anomalyAcknowledgeButton pid aid now untilM = div_ [id_ ctlId, class_ "inline-flex"] case untilM of
  Just until' ->
    button_
      ( [ type_ "button"
        , class_ "btn btn-sm gap-1.5 bg-fillSuccess-weak text-textSuccess border-strokeSuccess-weak tooltip tooltip-bottom"
        , data_ "tip" "Notifications are paused. Unacknowledge to resume them."
        , Aria.label_ "Unacknowledge issue"
        ]
          <> req "/unacknowledge"
      )
      do
        faSprite_ "bell-slash" "regular" "w-4 h-4"
        span_ [class_ "max-md:hidden"] $ toHtml $ untilLabel "Acknowledged" now until'
  Nothing -> div_ [class_ "join"] do
    button_
      ( [ type_ "button"
        , class_ "btn btn-sm btn-primary join-item gap-1.5 tooltip tooltip-bottom"
        , data_ "tip" "Pause notifications until this regresses"
        , Aria.label_ "Acknowledge issue"
        ]
          <> req "/acknowledge"
      )
      do
        faSprite_ "check" "regular" "w-4 h-4"
        span_ [class_ "max-md:hidden"] "Acknowledge"
    durationMenu_ (ctlId <> "-menu") "Acknowledge for\x2026" (\q -> req $ "/acknowledge" <> durationQuery "duration" q) \popId ->
      button_ [type_ "button", class_ "btn btn-sm btn-primary join-item px-2", term "popovertarget" popId, style_ $ "anchor-name: --anchor-" <> popId, Aria.label_ "Acknowledge for a set time"]
        $ faSprite_ "chevron-down" "regular" "w-3 h-3"
  where
    ctlId = "ack-ctl-" <> aid.toText
    req path = [term "hx-preload" "false", hxGet_ $ "/p/" <> pid.toText <> "/issues/" <> aid.toText <> path, hxTarget_ ("#" <> ctlId), hxSwap_ "outerHTML"]


anomalyArchiveButton :: Projects.ProjectId -> Issues.IssueId -> Bool -> Html ()
anomalyArchiveButton pid aid archived =
  button_
    [ type_ "button"
    , class_ $ "btn btn-sm gap-1.5 tooltip tooltip-bottom btn-ghost" <> bool "" " bg-fillWarning-weak text-textWarning border-strokeWarning-weak" archived
    , data_ "tip" $ bool "Not actionable \x2014 hide it and stop notifying" "Move back to the Inbox" archived
    , Aria.label_ $ bool "Archive issue" "Unarchive issue" archived
    , term "hx-preload" "false"
    , hxGet_ $ "/p/" <> pid.toText <> "/issues/" <> aid.toText <> bool "/archive" "/unarchive" archived
    , hxSwap_ "outerHTML"
    ]
    do
      faSprite_ "archive" "regular" "w-4 h-4"
      span_ [class_ "max-md:hidden"] $ toHtml $ bool @Text "Archive" "Unarchive" archived


issueTypeLabel :: Issues.IssueType -> Bool -> Html ()
issueTypeLabel issueType critical = span_ [class_ $ "flex items-center gap-1.5 text-xs font-medium " <> color] do
  faSprite_ icon "regular" "w-3 h-3"; toHtml txt
  where
    (color, icon, txt) = issueTypeMeta issueType critical


issueTypeBadge :: Issues.IssueType -> Bool -> Html ()
issueTypeBadge issueType critical = span_ [class_ $ "flex items-center gap-1 text-2xs whitespace-nowrap " <> color, term "data-tippy-content" fullTxt] do
  faSprite_ icon "regular" "w-3 h-3 shrink-0"; toHtml shortTxt
  where
    (color, icon, fullTxt) = issueTypeMeta issueType critical
    shortTxt = case issueType of
      Issues.LogPattern -> "Log"
      Issues.LogPatternRateChange -> "Rate"
      _ -> fullTxt


-- | Render a log level chip, inferring ERROR severity from the pattern when the
-- log_level field is missing (e.g. http logs lacking an explicit level but with
-- 4xx/5xx status). Mirrors @isIssueWorthy@ in BackgroundJobs.
logLevelChip_ :: Monad m => Maybe Text -> Text -> HtmlT m ()
logLevelChip_ logLevel pat =
  let effective = case T.toUpper <$> logLevel of
        Just l
          | l `elem` ["ERROR", "FATAL", "CRITICAL"] -> Just "ERROR"
          | l `elem` ["WARN", "WARNING"] -> Just "WARN"
        other
          | any (`T.isInfixOf` pat) ["status;badge-error⇒ERROR", "status_code;badge-4xx", "status_code;badge-5xx"] -> Just "ERROR"
          | otherwise -> other
      (cls, icon, label) = case effective of
        Just "ERROR" -> ("text-fillError-strong bg-fillError-weak", "triangle-alert", "ERROR")
        Just "WARN" -> ("text-fillWarning-strong bg-fillWarning-weak", "triangle-alert", "WARN")
        Just l -> ("", "circle-dot", l)
        Nothing -> ("", "circle-dot", "Unknown")
   in colorChip_ cls icon label


issueTypeMeta :: Issues.IssueType -> Bool -> (Text, Text, Text)
issueTypeMeta issueType critical = case issueType of
  Issues.RuntimeException -> ("text-fillError-strong", "triangle-alert", "Error")
  Issues.QueryAlert -> ("text-fillWarning-strong", "zap", "Alert")
  Issues.LogPattern -> ("text-fillInformation-strong", "file-text", "Log Pattern")
  Issues.LogPatternRateChange -> ("text-fillWarning-strong", "activity", "Rate Change")
  Issues.ApiChange | critical -> ("text-fillError-strong", "exclamation-triangle", "Breaking")
  Issues.ApiChange -> ("text-fillInformation-strong", "info", "Incremental")


issueActivityGetH :: Projects.ProjectId -> Issues.IssueId -> Maybe Text -> Maybe UTCTime -> ATAuthCtx (RespHeaders (Html ()))
issueActivityGetH pid issueId traceIdM traceTsM = do
  (_sess, _project) <- Projects.sessionAndProject pid
  activities <- Issues.selectIssueActivity pid issueId
  now <- Time.currentTime
  env <- (.env) <$> ask @AuthContext
  -- The journey comes out of the issue's trace, the one read on this page big
  -- enough to time out; it is bounded here so a slow trace costs the journey, not
  -- the issue-events timeline beside it.
  journeySpans <- flip foldMapM ((,) <$> traceIdM <*> traceTsM) \(tId, tTs) -> do
    res <- trySync $ timeout (env.traceViewTimeoutSecs * 1_000_000) $ Telemetry.getTraceDetailsForView env.enableTimefusionReads pid tId (Just tTs) now
    case res of
      Right (Just (Just (_, spans))) -> pure spans
      Right (Just Nothing) -> pure V.empty
      Right Nothing -> V.empty <$ Log.logAttention "ISSUE_JOURNEY_FETCH_TIMEOUT" (AE.object ["issue_id" AE..= issueId, "trace_id" AE..= tId])
      Left e -> V.empty <$ Log.logAttention "ISSUE_JOURNEY_FETCH_FAILED" (AE.object ["issue_id" AE..= issueId, "trace_id" AE..= tId, "error" AE..= show @Text e])
  let userIds = ordNub $ mapMaybe (.createdBy) activities
  users :: [Projects.User] <-
    if null userIds
      then pure []
      else Hasql.interp [HI.sql| SELECT id, created_at, updated_at, deleted_at, active, first_name, last_name, display_image_url, email, is_sudo, phone_number FROM users.users WHERE id = ANY(#{userIds}::uuid[]) |]
  let userMap = Map.fromList $ map (\u -> (u.id, u)) users
  addRespHeaders do
    userJourneySection_ journeySpans
    div_ [class_ "border-t border-strokeWeak"] do
      div_ [class_ "px-4 py-2 flex items-center gap-2 bg-fillWeaker/40"] do
        faSprite_ "circle-info" "regular" "w-3 h-3 text-textWeak"
        span_ [class_ "text-2xs font-semibold text-textWeak uppercase tracking-wide"] "Issue events"
    issueActivityTimeline_ userMap now activities


issueActivityTimeline_ :: Map.Map Projects.UserId Projects.User -> UTCTime -> [Issues.IssueActivity] -> Html ()
issueActivityTimeline_ userMap now activities
  | null activities = div_ [class_ "p-4 text-sm text-textWeak text-center"] "No activity yet."
  | otherwise = div_ [class_ "p-4 flex flex-col gap-0"] $ forM_ activities \a -> do
      let (icon, color, label) = eventDisplay a.event
          actorText = foldMap (\uid -> foldMap (\u -> " by " <> CI.original u.email) $ Map.lookup uid userMap) a.createdBy
      div_ [class_ "flex items-start gap-3 relative pl-4 pb-4 border-l-2 border-strokeWeak ml-2"] do
        div_ [class_ $ "absolute -left-[9px] top-0.5 w-4 h-4 rounded-full flex items-center justify-center " <> color]
          $ faSprite_ icon "regular" "w-2.5 h-2.5"
        div_ [class_ "flex flex-col gap-0.5 min-w-0"] do
          span_ [class_ "text-sm text-textStrong"] $ toHtml $ label <> actorText
          span_ [class_ "text-xs text-textWeak"] $ toHtml $ compactTimeAgo $ toText $ prettyTimeAuto now a.createdAt


eventDisplay :: Issues.IssueEvent -> (Text, Text, Text)
eventDisplay = \case
  Issues.IECreated -> ("plus", "bg-fillSuccess-weak text-fillSuccess-strong", "Created")
  Issues.IEAcknowledged -> ("bell-slash", "bg-fillBrand-weak text-fillBrand-strong", "Acknowledged")
  Issues.IEUnacknowledged -> ("arrow-rotate-left", "bg-fillWeaker text-textWeak", "Unacknowledged")
  Issues.IEAckExpired -> ("clock", "bg-fillWarning-weak text-fillWarning-strong", "Acknowledgement expired \x2014 back in the Inbox")
  Issues.IEArchived -> ("box-archive", "bg-fillWeaker text-textWeak", "Archived")
  Issues.IEUnarchived -> ("box-archive", "bg-fillWeaker text-textWeak", "Unarchived")
  Issues.IEResolved -> ("check-double", "bg-fillSuccess-weak text-fillSuccess-strong", "Resolved")
  Issues.IEReopened -> ("arrow-rotate-left", "bg-fillWarning-weak text-fillWarning-strong", "Reopened")
  Issues.IERegressed -> ("arrow-trend-up", "bg-fillError-weak text-fillError-strong", "Regressed")
  Issues.IEAssigned -> ("user-plus", "bg-fillBrand-weak text-fillBrand-strong", "Assigned")
  Issues.IEUnassigned -> ("user-minus", "bg-fillWeaker text-textWeak", "Unassigned")
  Issues.IEAutoResolved -> ("wand-magic-sparkles", "bg-fillSuccess-weak text-fillSuccess-strong", "Auto-resolved")
  Issues.IEEscalated -> ("arrow-up", "bg-fillError-weak text-fillError-strong", "Escalated")


errorGroupMembersGetH :: Projects.ProjectId -> UUID.UUID -> ATAuthCtx (RespHeaders (Html ()))
errorGroupMembersGetH pid errorId = do
  _ <- Projects.sessionAndProject pid
  members <- PatternMerge.getErrorPatternGroupMembers (ErrorPatternId errorId)
  addRespHeaders
    $ unless (null members)
    $ div_ [class_ "surface-raised rounded-2xl overflow-hidden mt-4"] do
      div_ [class_ "px-4 py-3 border-b border-strokeWeak flex items-center gap-2"] do
        faSprite_ "layer-group" "regular" "w-4 h-4 text-iconNeutral"
        span_ [class_ "text-sm font-medium text-textStrong"] "Similar Patterns"
        span_ [class_ "badge badge-sm badge-ghost tabular-nums"] $ toHtml $ show (length members) <> " merged"
      div_ [class_ "p-4 flex flex-col gap-2"] do
        forM_ members \member -> do
          let memberId = UUID.toText member.id.unErrorPatternId
              unmergeUrl = "/p/" <> pid.toText <> "/issues/errors/" <> memberId <> "/unmerge"
          div_ [class_ "flex items-center justify-between p-3 bg-fillWeaker rounded-lg", id_ $ "member-" <> memberId] do
            div_ [class_ "flex flex-col gap-1 min-w-0"] do
              span_ [class_ "text-sm font-medium text-textStrong truncate"] $ toHtml $ member.errorType <> ": " <> member.message
              span_ [class_ "text-xs text-textWeak"] $ toHtml $ "Hash: " <> member.hash
            button_
              [ class_ "btn btn-xs btn-ghost tap-target"
              , Aria.label_ "Unmerge pattern"
              , hxPost_ unmergeUrl
              , hxTarget_ $ "#member-" <> memberId
              , hxSwap_ "outerHTML"
              ]
              do
                faSprite_ "code-branch" "regular" "w-3 h-3"
                "Unmerge"


errorUnmergePostH :: Projects.ProjectId -> UUID.UUID -> ATAuthCtx (RespHeaders (Html ()))
errorUnmergePostH pid errorId = do
  _ <- Projects.sessionAndProject pid
  void $ PatternMerge.unmergeErrorPattern (ErrorPatternId errorId)
  addSuccessToast "Pattern unmerged" Nothing
  addRespHeaders $ div_ [class_ "p-3 bg-fillSuccess-weak rounded-lg text-sm text-fillSuccess-strong"] "Pattern unmerged successfully"


similarPatternsSection_ :: Projects.ProjectId -> ErrorPatterns.ErrorPatternId -> Html ()
similarPatternsSection_ pid errorId = do
  let errorIdText = UUID.toText errorId.unErrorPatternId
      groupUrl = "/p/" <> pid.toText <> "/issues/errors/" <> errorIdText <> "/group_members"
  div_
    [ hxGet_ groupUrl
    , hxTrigger_ "load"
    , hxSwap_ "innerHTML"
    , id_ $ "similar-patterns-" <> errorIdText
    ]
    pass
