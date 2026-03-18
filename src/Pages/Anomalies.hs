module Pages.Anomalies (
  anomalyListGetH,
  anomalyBulkActionsPostH,
  escapedQueryPartial,
  acknowledgeAnomalyGetH,
  unAcknowledgeAnomalyGetH,
  archiveAnomalyGetH,
  unArchiveAnomalyGetH,
  anomalyDetailGetH,
  AnomalyBulkForm (..),
  AnomalyListGet (..),
  anomalyAcknowledgeButton,
  anomalyArchiveButton,
  anomalyDetailHashGetH,
  AnomalyAction (..),
  IssueVM (..),
  issueColumns,
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
import Data.HashMap.Strict qualified as HM
import Data.Map qualified as Map
import Data.Ord (clamp)
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX qualified as POSIX
import Data.Time.LocalTime (ZonedTime, zonedTimeToUTC)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.Newtypes (Aeson (..), getAeson)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Effectful.Concurrent.Async (concurrently)
import Effectful.Error.Static (throwError)
import Effectful.PostgreSQL qualified as PG
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import GHC.Records (HasField)
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (TermRaw (termRaw), makeAttribute, makeElement)
import Lucid.Htmx (hxGet_, hxIndicator_, hxPost_, hxPushUrl_, hxSelect_, hxSwap_, hxTarget_, hxTrigger_)
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.ErrorPatterns (ErrorPatternId (..))
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.Fields (FacetData (..), FacetSummary (..), FacetValue (..))
import Models.Apis.Fields qualified as Fields
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogPatterns (sourceFieldLabel)
import Models.Apis.Monitors qualified as Monitors
import Models.Apis.PatternMerge qualified as PatternMerge
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects (User (id))
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Schema qualified as Schema
import Models.Telemetry.Telemetry qualified as Telemetry
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), navTabAttrs)
import Pages.Charts.Charts qualified as Charts
import Pages.Components (colorChip_, emptyState_, metadataChip_, resizer_)
import Pages.LogExplorer.Log (virtualTable)
import Pages.Telemetry (tracePage)
import Pkg.AI qualified as AI
import Pkg.Components.Table (BulkAction (..), Column (..), Config (..), Features (..), FilterMenu (..), FilterOption (..), Pagination (..), SearchMode (..), TabFilter (..), TabFilterOpt (..), Table (..), TableHeaderActions (..), TableRows (..), ZeroState (..), col, withAttrs, withColHeaderExtra)
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..), hashAssetFile)
import PyF (fmt)
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import Servant (err400, errBody)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast)
import Text.Time.Pretty (prettyTimeAuto)
import Utils (LoadingSize (..), LoadingType (..), checkFreeTierStatus, deleteParam, escapedQueryPartial, faSprite_, formatUTC, formatWithCommas, htmxOverlayIndicator_, loadingIndicator_, lookupValueText, methodFillColor, renderMarkdown, toUriStr)
import Web.FormUrlEncoded (FromForm)


newtype AnomalyBulkForm = AnomalyBulk
  { anomalyId :: [Text]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


acknowledgeAnomalyGetH :: Projects.ProjectId -> Anomalies.AnomalyId -> Maybe Text -> ATAuthCtx (RespHeaders AnomalyAction)
acknowledgeAnomalyGetH pid aid hostM = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  let issueId = UUIDId aid.unUUIDId
  _ <- Issues.acknowledgeIssue issueId sess.user.id
  Issues.logIssueActivity issueId Issues.IEAcknowledged (Just sess.user.id) Nothing
  let text_id = V.fromList [UUID.toText aid.unUUIDId]
  v <- Anomalies.acknowledgeAnomalies sess.user.id text_id
  _ <- Anomalies.acknowlegeCascade sess.user.id (V.fromList v)
  addRespHeaders $ Acknowlege pid (UUIDId aid.unUUIDId) True


unAcknowledgeAnomalyGetH :: Projects.ProjectId -> Anomalies.AnomalyId -> ATAuthCtx (RespHeaders AnomalyAction)
unAcknowledgeAnomalyGetH pid aid = do
  (sess, project) <- Projects.sessionAndProject pid
  let q = [sql| update apis.anomalies set acknowledged_by=null, acknowledged_at=null where id=? |]
  let qI = [sql| update apis.issues set acknowledged_by=null, acknowledged_at=null where id=? |]
  _ <- PG.execute qI (Only aid)
  _ <- PG.execute q (Only aid)
  Issues.logIssueActivity (UUIDId aid.unUUIDId) Issues.IEUnacknowledged (Just sess.user.id) Nothing
  addRespHeaders $ Acknowlege pid (UUIDId aid.unUUIDId) False


archiveAnomalyGetH :: Projects.ProjectId -> Anomalies.AnomalyId -> ATAuthCtx (RespHeaders AnomalyAction)
archiveAnomalyGetH pid aid = do
  (sess, project) <- Projects.sessionAndProject pid
  now <- Time.currentTime
  let q = [sql| update apis.anomalies set archived_at=? where id=? |]
  let qI = [sql| update apis.issues set archived_at=? where id=? |]
  _ <- PG.execute qI (now, aid)
  _ <- PG.execute q (now, aid)
  Issues.logIssueActivity (UUIDId aid.unUUIDId) Issues.IEArchived (Just sess.user.id) Nothing
  addRespHeaders $ Archive pid (UUIDId aid.unUUIDId) True


unArchiveAnomalyGetH :: Projects.ProjectId -> Anomalies.AnomalyId -> ATAuthCtx (RespHeaders AnomalyAction)
unArchiveAnomalyGetH pid aid = do
  (sess, project) <- Projects.sessionAndProject pid
  let q = [sql| update apis.anomalies set archived_at=null where id=? |]
  let qI = [sql| update apis.issues set archived_at=null where id=? |]
  _ <- PG.execute qI (Only aid)
  _ <- PG.execute q (Only aid)
  Issues.logIssueActivity (UUIDId aid.unUUIDId) Issues.IEUnarchived (Just sess.user.id) Nothing
  addRespHeaders $ Archive pid (UUIDId aid.unUUIDId) False


data AnomalyAction
  = Acknowlege Projects.ProjectId Issues.IssueId Bool
  | Archive Projects.ProjectId Issues.IssueId Bool
  | Bulk


instance ToHtml AnomalyAction where
  toHtml (Acknowlege pid aid is_ack) = toHtml $ anomalyAcknowledgeButton pid aid is_ack ""
  toHtml (Archive pid aid is_arch) = toHtml $ anomalyArchiveButton pid aid is_arch
  toHtml Bulk = ""
  toHtmlRaw = toHtml


-- | Bulk acknowledge/archive anomalies, triggering a notification and list reload
anomalyBulkActionsPostH :: Projects.ProjectId -> Text -> AnomalyBulkForm -> ATAuthCtx (RespHeaders AnomalyAction)
anomalyBulkActionsPostH pid action items = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  if null items.anomalyId
    then do
      addErrorToast "No items selected" Nothing
      addRespHeaders Bulk
    else do
      eventType <- case action of
        "acknowledge" -> do
          v <- Anomalies.acknowledgeAnomalies sess.user.id (V.fromList items.anomalyId)
          void $ Anomalies.acknowlegeCascade sess.user.id (V.fromList v)
          pure Issues.IEAcknowledged
        "archive" -> do
          now <- Time.currentTime
          void $ PG.execute [sql| update apis.anomalies set archived_at=? where id=ANY(?::uuid[]) |] (now, V.fromList items.anomalyId)
          pure Issues.IEArchived
        _ -> throwError err400{errBody = "unhandled anomaly bulk action: " <> encodeUtf8 action}
      issueIds <- map (\(Only i) -> i :: Issues.IssueId) <$> PG.query [sql| SELECT DISTINCT issue_id FROM apis.anomalies WHERE id=ANY(?::uuid[]) AND issue_id IS NOT NULL |] (Only $ PGArray items.anomalyId)
      forM_ issueIds \iid -> Issues.logIssueActivity iid eventType (Just sess.user.id) Nothing
      addSuccessToast (action <> "d items Successfully") Nothing
      addRespHeaders Bulk


anomalyDetailGetH :: Projects.ProjectId -> Issues.IssueId -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
anomalyDetailGetH pid issueId firstM sinceM =
  anomalyDetailCore pid firstM sinceM $ \_ ->
    Issues.selectIssueById issueId


anomalyDetailHashGetH :: Projects.ProjectId -> Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
anomalyDetailHashGetH pid issueId firstM sinceM = anomalyDetailCore pid firstM sinceM \_ -> Issues.selectIssueByHash pid issueId


anomalyDetailCore :: Projects.ProjectId -> Maybe Text -> Maybe Text -> (Projects.ProjectId -> ATAuthCtx (Maybe Issues.Issue)) -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
anomalyDetailCore pid firstM sinceM fetchIssue = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  issueM <- fetchIssue pid
  now <- Time.currentTime
  let baseBwconf = (def :: BWConfig){sessM = Just sess, currProject = Just project, pageTitle = "Issues", menuItem = Just "Issues", config = appCtx.config}
  case issueM of
    Nothing -> do
      addErrorToast "Issue not found" Nothing
      addRespHeaders
        $ PageCtx baseBwconf
        $ toHtml ("Issue not found" :: Text)
    Just issue -> do
      let tp = TimePicker.TimePicker (Just $ fromMaybe (defaultSinceRange issue.createdAt now) sinceM) Nothing Nothing
      errorM <-
        issue.issueType & \case
          Issues.RuntimeException -> ErrorPatterns.getErrorPatternLByHash pid issue.targetHash now
          _ -> pure Nothing
      (members, canResolve) <- case errorM of
        Just _ -> do
          userPermission <- ProjectMembers.getUserPermission pid sess.user.id
          ms <- V.fromList <$> ProjectMembers.selectActiveProjectMembers pid
          let cr =
                userPermission
                  >= Just ProjectMembers.PEdit
                  || maybe False (\errL -> errL.base.assigneeId == Just sess.user.id) errorM
          pure (ms, cr)
        Nothing -> pure (V.empty, False)
      let bwconf =
            baseBwconf
              { prePageTitle = Just "Issues"
              , pageTitle = "#" <> show issue.seqNum
              , headContent = Just do highlightJsHead_; style_ "#crisp-chatbox { display: none !important; }"
              , pageActions = Just $ div_ [class_ "flex gap-2"] do
                  anomalyAcknowledgeButton pid (UUIDId issue.id.unUUIDId) (isJust issue.acknowledgedAt) ""
                  anomalyArchiveButton pid (UUIDId issue.id.unUUIDId) (isJust issue.archivedAt)
                  when (issue.issueType == Issues.RuntimeException)
                    $ whenJust errorM \errL -> do
                      errorResolveAction pid errL.base.id errL.base.state canResolve
                      errorSubscriptionAction pid errL.base
              }
      (trItem, spanRecs) <-
        fromMaybe (Nothing, V.empty) <$> runMaybeT do
          errL <- hoistMaybe errorM
          let err = errL.base
              isFirst = isJust firstM
          tId <- hoistMaybe $ bool err.recentTraceId err.firstTraceId isFirst
          traceItem <- MaybeT $ Telemetry.getTraceDetails pid tId Nothing now
          otelLogs <- lift $ Telemetry.getSpanRecordsByTraceId pid traceItem.traceId (Just traceItem.traceStartTime) now
          pure (Just traceItem, V.catMaybes $ Telemetry.convertOtelLogsAndSpansToSpanRecord <$> V.fromList otelLogs)
      addRespHeaders $ PageCtx bwconf $ anomalyDetailPage pid issue trItem spanRecs errorM now (isJust firstM) members tp


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


-- | Abbreviate time unit (e.g., "hours" → "hrs")
--
-- >>> abbreviateUnit "hours"
-- "hrs"
-- >>> abbreviateUnit "minute"
-- "min"
-- >>> abbreviateUnit "days"
-- "days"
abbreviateUnit :: Text -> Text
abbreviateUnit "hours" = "hrs"
abbreviateUnit "hour" = "hr"
abbreviateUnit "minutes" = "mins"
abbreviateUnit "minute" = "min"
abbreviateUnit "seconds" = "secs"
abbreviateUnit "second" = "sec"
abbreviateUnit w = w


-- | Compact time ago display (e.g., "23 hrs ago" instead of "23 hours ago")
compactTimeAgo :: Text -> Text
compactTimeAgo = unwords . map abbreviateUnit . words


-- | Stat box for time display with number large and unit small. Empty input renders nothing.
timeStatBox_ :: Text -> String -> Html ()
timeStatBox_ title timeStr
  | null timeStr = pass
  | (num : rest) <- words $ toText timeStr =
      div_ [class_ "bg-fillWeaker rounded-3xl flex flex-col gap-3 p-5 border border-strokeWeak"] do
        div_ [class_ "flex flex-col gap-1"] do
          span_ [class_ "font-bold text-textStrong"] do
            span_ [class_ "text-4xl tabular-nums"] $ toHtml num
            span_ [class_ "text-sm text-textWeak"] $ toHtml $ " " <> unwords (map abbreviateUnit rest)
          div_ [class_ "flex gap-2 items-center text-sm text-textWeak"] $ p_ [] $ toHtml title
  | otherwise = pass


activityPanel_ :: Projects.ProjectId -> Text -> Text -> Html ()
activityPanel_ pid issueId extraClass = do
  let activityUrl = "/p/" <> pid.toText <> "/issues/" <> issueId <> "/activity"
  details_ [class_ $ unwords $ filter (not . T.null) ["surface-raised rounded-2xl group/activity", extraClass], term "open" ""] do
    summary_ [class_ "px-4 py-3 flex items-center gap-2 cursor-pointer list-none [&::-webkit-details-marker]:hidden"] do
      faSprite_ "clock-rotate-left" "regular" "w-3.5 h-3.5 text-textWeak"
      span_ [class_ "text-xs font-semibold text-textWeak uppercase tracking-wide"] "Activity"
      faSprite_ "chevron-down" "regular" "w-3 h-3 text-textWeak shrink-0 ml-auto group-open/activity:rotate-180 transition-transform"
    div_ [id_ "issue-activity", class_ "border-t border-strokeWeak", hxGet_ activityUrl, hxTrigger_ "intersect once", hxSwap_ "innerHTML"]
      $ div_ [class_ "p-4 flex justify-center"]
      $ loadingIndicator_ LdSM LdDots


anomalyDetailPage :: Projects.ProjectId -> Issues.Issue -> Maybe Telemetry.Trace -> V.Vector Telemetry.SpanRecord -> Maybe ErrorPatterns.ErrorPatternL -> UTCTime -> Bool -> V.Vector ProjectMembers.ProjectMemberVM -> TimePicker.TimePicker -> Html ()
anomalyDetailPage pid issue tr spanRecs errM now isFirst members tp = do
  let (_, _, currentRange) = TimePicker.parseTimeRange now tp
      issueId = UUID.toText issue.id.unUUIDId
      sevBase = "inline-flex items-center justify-center rounded-md px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 "
      severityBadge "critical" = span_ [class_ $ sevBase <> "bg-fillError-weak text-fillError-strong border-2 border-strokeError-strong shadow-sm"] "CRITICAL"
      severityBadge "warning" = span_ [class_ $ sevBase <> "bg-fillWarning-weak text-fillWarning-strong border border-strokeWarning-weak shadow-sm"] "WARNING"
      severityBadge _ = pass
  div_ [class_ "flex h-full overflow-hidden relative group/ai"] do
    -- LEFT: scrollable main content
    div_ [class_ "flex-1 min-w-0 min-h-0 overflow-y-auto max-md:pt-5 pt-8 max-md:px-3 px-4 pb-8 max-md:space-y-3 space-y-4"] do
      -- Header: title
      h3_ [class_ "max-md:text-xl text-2xl font-semibold text-textStrong flex flex-wrap items-center gap-1"] $ if "⇒" `T.isInfixOf` issue.title then renderSummaryText_ issue.title else toHtml issue.title
      unless (issue.recommendedAction == Issues.defaultRecommendedAction)
        $ p_ [class_ "text-sm text-textWeak max-w-3xl"]
        $ toHtml issue.recommendedAction
      -- Metadata chips + issue type content
      let createdChip = colorChip_ "text-fillInformation-strong bg-fillInformation-weak" "calendar" $ "Created " <> toText (prettyTimeAuto now (zonedTimeToUTC issue.createdAt))
          logPatternCards sourceField logPattern sampleMessage = div_ [class_ "flex flex-col gap-4"] do
            _ <- div_ [class_ "surface-raised rounded-2xl overflow-hidden"] do
              div_ [class_ "px-4 py-3 border-b border-strokeWeak flex items-center gap-2"] do
                span_ [class_ "text-xs font-semibold text-textWeak uppercase tracking-wide"] "Log Pattern"
                span_ [class_ "badge badge-sm badge-ghost"] $ toHtml $ sourceFieldLabel sourceField
              renderLogContent_ logPattern
            whenJust sampleMessage \msg ->
              div_ [class_ "surface-raised rounded-2xl overflow-hidden"] do
                div_ [class_ "px-4 py-3 border-b border-strokeWeak"] $ span_ [class_ "text-xs font-semibold text-textWeak uppercase tracking-wide"] "Sample Message"
                renderLogContent_ msg
      div_ [class_ "flex flex-wrap gap-2 items-center"] do
        severityBadge issue.severity
        issueTypeLabel issue.issueType issue.critical
        case issue.issueType of
          Issues.LogPattern -> withIssueDataH @Issues.LogPatternData issue.issueData \d -> do
            metadataChip_ "circle-dot" $ fromMaybe "Unknown" d.logLevel
            metadataChip_ "server" $ fromMaybe "Unknown" d.serviceName
            metadataChip_ "tally" $ show d.occurrenceCount <> " occurrences"
            metadataChip_ "clock" $ "First seen " <> compactTimeAgo (toText $ prettyTimeAuto now d.firstSeenAt)
          Issues.LogPatternRateChange -> withIssueDataH @Issues.LogPatternRateChangeData issue.issueData \d -> do
            metadataChip_ "arrow-trend-up" $ display d.changeDirection
            metadataChip_ "percent" $ Issues.showPct d.changePercent <> " change"
            metadataChip_ "gauge-high" $ Issues.showRate d.currentRatePerHour <> " current"
            metadataChip_ "chart-line" $ Issues.showRate d.baselineMean <> " baseline"
          Issues.RuntimeException -> pass -- First/Last seen shown in Error Details panel
          _ -> createdChip
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
      case issue.issueType of
        Issues.LogPattern -> withIssueDataH @Issues.LogPatternData issue.issueData \d -> do
          volumeChart_ "Pattern Volume"
          logPatternCards d.sourceField d.logPattern d.sampleMessage
        Issues.LogPatternRateChange -> withIssueDataH @Issues.LogPatternRateChangeData issue.issueData \d -> do
          volumeChart_ "Pattern Volume"
          logPatternCards d.sourceField d.logPattern d.sampleMessage
        Issues.RuntimeException -> withIssueDataH @Issues.RuntimeExceptionData issue.issueData \exceptionData -> do
          let errorFirstLine = fromMaybe exceptionData.stackTrace $ viaNonEmpty head $ lines exceptionData.stackTrace
              detailItem (icn, iconColor, lbl, value) = div_ [class_ "flex items-center gap-1.5 whitespace-nowrap"] do
                faSprite_ icn "regular" $ "w-3 h-3 " <> iconColor
                span_ [class_ "text-xs text-textWeak"] $ toHtml lbl <> ":"
                span_ [class_ "text-xs font-medium"] $ toHtml value
          -- Chart + Error Details in one row
          div_ [class_ "flex flex-col lg:flex-row gap-4 lg:items-start"] do
            div_ [class_ "min-w-0 flex-1"] $ volumeChart_ "Error Frequency"
            whenJust errM \errL -> do
              let err = errL.base
              div_ [class_ "lg:w-72 shrink-0 surface-raised rounded-2xl overflow-hidden"] do
                div_ [class_ "px-4 py-3 border-b border-strokeWeak flex items-center gap-2"] do
                  faSprite_ "circle-info" "regular" "w-3.5 h-3.5 text-textWeak"
                  span_ [class_ "text-xs font-semibold text-textWeak uppercase tracking-wide"] "Error Details"
                div_ [class_ "p-4 flex flex-col gap-3"] do
                  whenJust ((,) <$> exceptionData.requestMethod <*> exceptionData.requestPath) \(method, path) ->
                    div_ [class_ "mb-1"] do
                      span_ [class_ $ "relative cbadge-sm badge-" <> method <> " whitespace-nowrap"] $ toHtml method
                      span_ [class_ "ml-2 text-sm text-textWeak"] $ toHtml path
                  div_ [class_ "flex flex-wrap items-center gap-x-5 gap-y-2"]
                    $ forM_
                      [ ("calendar" :: Text, "text-fillBrand-strong" :: Text, "First seen" :: Text, compactTimeAgo $ toText $ prettyTimeAuto now (zonedTimeToUTC err.createdAt))
                      , ("calendar" :: Text, "text-fillBrand-strong" :: Text, "Last seen" :: Text, compactTimeAgo $ toText $ prettyTimeAuto now (zonedTimeToUTC err.updatedAt))
                      ]
                      detailItem
                  div_ [class_ "flex flex-wrap items-center gap-x-5 gap-y-2"]
                    $ forM_
                      [ ("code" :: Text, "text-fillWarning-strong" :: Text, "Stack" :: Text, fromMaybe "Unknown stack" err.errorData.runtime)
                      , ("server" :: Text, "text-fillSuccess-strong" :: Text, "Service" :: Text, fromMaybe "Unknown service" err.errorData.serviceName)
                      ]
                      detailItem
          -- Stack trace + Activity side by side
          div_ [class_ "flex flex-col lg:flex-row gap-4 lg:items-start"] do
            div_ [class_ "min-w-0 flex-1"]
              $ details_ [class_ "surface-raised rounded-2xl group/details", term "open" "", term "_" "init if window.innerWidth < 768 remove @open from me"]
              $ do
                summary_ [class_ "px-4 py-3 flex items-center gap-2 cursor-pointer list-none [&::-webkit-details-marker]:hidden"] do
                  faSprite_ "code" "regular" "w-3.5 h-3.5 text-textWeak"
                  span_ [class_ "text-xs font-semibold text-textWeak uppercase tracking-wide shrink-0"] "Stack Trace"
                  span_ [class_ "text-xs text-fillError-strong truncate"] $ toHtml errorFirstLine
                  faSprite_ "chevron-down" "regular" "w-3 h-3 text-textWeak shrink-0 ml-auto group-open/details:rotate-180 transition-transform"
                div_ [class_ "max-h-80 overflow-y-auto border-t border-strokeWeak"]
                  $ pre_ [class_ "text-sm leading-relaxed overflow-x-auto whitespace-pre-wrap"]
                  $ code_ []
                  $ toHtml exceptionData.stackTrace
            activityPanel_ pid issueId "lg:w-72 shrink-0"
          -- Similar patterns
          whenJust errM \errL -> similarPatternsSection_ pid errL.base.id
        Issues.QueryAlert -> withIssueDataH @Issues.QueryAlertData issue.issueData \alertData ->
          div_ [class_ "mb-4"] do
            span_ [class_ "text-xs text-textWeak mb-2 block font-semibold uppercase tracking-wide"] "Query"
            div_ [class_ "bg-fillInformation-weak border border-strokeInformation-weak rounded-lg p-3 text-sm font-mono text-fillInformation-strong max-w-2xl overflow-x-auto"] $ toHtml alertData.queryExpression
        Issues.ApiChange -> withIssueDataH @Issues.APIChangeData issue.issueData \d -> do
          div_ [class_ "flex items-center gap-3 mb-4 p-3 rounded-lg"] do
            span_ [class_ $ "badge " <> methodFillColor d.endpointMethod] $ toHtml d.endpointMethod
            span_ [class_ "monospace bg-fillWeaker px-2 py-1 rounded text-sm text-textStrong"] $ toHtml d.endpointPath
            div_ [class_ "w-px h-4 bg-strokeWeak"] ""
            span_ [class_ "flex items-center gap-1.5 text-sm text-textWeak"] do
              faSprite_ "server" "regular" "h-3 w-3"
              toHtml d.endpointHost
          div_ [class_ "grid grid-cols-4 lg:grid-cols-8 gap-4 mb-4"] do
            timeStatBox_ "First Seen" $ prettyTimeAuto now (zonedTimeToUTC issue.createdAt)
            div_ [class_ "col-span-4"]
              $ Widget.widget_
              $ (def :: Widget.Widget)
                { Widget.standalone = Just True
                , Widget.id = Just $ issueId <> "-api-change-timeline"
                , Widget.naked = Just True
                , Widget.wType = Widget.WTTimeseries
                , Widget.title = Just "Request trend"
                , Widget.showTooltip = Just True
                , Widget.xAxis = Just (def{Widget.showAxisLabel = Just True})
                , Widget.yAxis = Just (def{Widget.showOnlyMaxLabel = Just True})
                , Widget.query = Just $ "attributes.http.request.method==\"" <> d.endpointMethod <> "\" AND attributes.http.route==\"" <> d.endpointPath <> "\" | summarize count(*) by bin_auto(timestamp)"
                , Widget._projectId = Just issue.projectId
                , Widget.hideLegend = Just True
                }
      div_ [class_ "surface-raised rounded-2xl overflow-hidden", id_ "error-details-container", makeAttribute "tabindex" "-1", onkeydown_ "if(event.key==='Escape'&&this.classList.contains('investigation-fullscreen'))document.getElementById('investigation-fullscreen-btn').click()"] do
        div_ [class_ "max-md:px-3 px-4 border-b border-strokeWeak flex max-md:flex-col md:items-center md:justify-between"] do
          div_ [class_ "flex items-center gap-2 max-md:py-1.5"] do
            faSprite_ "magnifying-glass-chart" "regular" "w-3.5 h-3.5 text-textWeak"
            h3_ [class_ "text-xs font-semibold text-textWeak uppercase tracking-wide"] "Investigation"
            button_ [class_ "p-1 rounded hover:bg-fillWeaker cursor-pointer transition-colors max-md:hidden", Aria.label_ "Toggle fullscreen", id_ "investigation-fullscreen-btn", onclick_ "var c=document.getElementById('error-details-container'),u=this.querySelector('use'),h=u.getAttribute('href');c.classList.toggle('investigation-fullscreen');u.setAttribute('href',c.classList.contains('investigation-fullscreen')?h.replace('#expand','#compress'):h.replace('#compress','#expand'));window.scrollTo({top:0})"] do
              faSprite_ "expand" "regular" "w-3 h-3 text-textWeak"
          div_ [class_ "flex items-center max-md:overflow-x-auto max-md:-mx-4 max-md:px-4 max-md:pb-1.5"] do
            let aUrl = "/p/" <> pid.toText <> "/issues/" <> issueId
                navLink (href, isActive, tooltip, lbl) = a_ [href_ href, class_ $ bool "text-textWeak hover:text-textStrong" "text-textBrand font-medium" isActive <> " text-xs py-2.5 max-md:px-2 px-3 cursor-pointer transition-colors", term "data-tippy-content" tooltip] $ toHtml lbl
                tabBtn (target, lbl, isActive) = button_ [class_ $ "text-xs py-2.5 max-md:px-2 px-3 cursor-pointer err-tab font-medium" <> bool "" " t-tab-active" isActive, onclick_ $ "navigatable(this, '" <> target <> "', '#error-details-container', 't-tab-active', 'err')"] $ toHtml lbl
            forM_ [(aUrl <> "?first_occurrence=true", isFirst, "Show first trace the error occured" :: Text, "First" :: Text), (aUrl, not isFirst, "Show recent trace the error occured" :: Text, "Recent" :: Text)] navLink
            span_ [class_ "mx-3 w-px h-4 bg-strokeWeak max-md:mx-2"] pass
            forM_ [("#span-content" :: Text, "Trace" :: Text, True), ("#log-content" :: Text, "Logs" :: Text, False), ("#replay-content" :: Text, "Replay" :: Text, False)] tabBtn
        div_ [class_ "max-md:p-1 p-2 w-full overflow-x-hidden investigation-content"] do
          div_ [class_ "flex flex-col lg:flex-row w-full err-tab-content", id_ "span-content"] do
            div_ [id_ "trace_container", class_ "grow-1 lg:max-w-[80%] lg:w-1/2 lg:min-w-[20%] shrink-1"]
              $ maybe
                ( div_ [class_ "flex flex-col items-center justify-center h-48"] do
                    faSprite_ "inbox-full" "regular" "w-6 h-6 text-iconNeutral"
                    span_ [class_ "mt-2 text-sm text-textWeak"] "No trace data available for this error."
                )
                (\t -> tracePage pid t spanRecs)
                tr
            div_ [class_ "transition-opacity duration-200 mx-1 hidden lg:block", id_ "resizer-details_width-wrapper"] $ resizer_ "log_details_container" "details_width" False
            div_ [class_ "grow-0 relative shrink-0 overflow-y-auto overflow-x-hidden max-h-[500px] lg:w-1/2 w-c-scroll overflow-y-auto investigation-details", id_ "log_details_container", term "hx-on::after-swap" "if(window.innerWidth<1024)this.scrollIntoView({behavior:'smooth',block:'start'})"] do
              htmxOverlayIndicator_ "details_indicator"
              whenJust (spanRecs V.!? 0) \sr ->
                div_ [hxGet_ $ "/p/" <> pid.toText <> "/log_explorer/" <> sr.uSpanId <> "/" <> formatUTC sr.timestamp <> "/detailed", hxTarget_ "#log_details_container", hxSwap_ "innerHtml", hxTrigger_ "intersect once", hxIndicator_ "#details_indicator", term "hx-sync" "this:replace"] pass

        div_ [id_ "log-content", class_ "hidden err-tab-content"] do
          virtualTable pid (Just ("/p/" <> pid.toText <> "/log_explorer?json=true&query=" <> toUriStr ("kind==\"log\" AND context___trace_id==\"" <> fromMaybe "" (errM >>= (.base.recentTraceId)) <> "\""))) Nothing

        div_ [id_ "replay-content", class_ "hidden err-tab-content"] do
          let withSessionIds = V.catMaybes $ (\sr -> (`lookupValueText` "id") =<< Map.lookup "session" =<< sr.attributes) <$> spanRecs
          bool
            (emptyState_ (Just "video") "No Replay Available" "No session replays associated with this trace" (Just "https://monoscope.tech/docs/sdks/Javascript/browser/") "Session Replay Guide")
            (div_ [class_ "border border-r border-l w-max mx-auto"] $ termRaw "session-replay" [id_ "sessionReplay", term "initialSession" $ V.head withSessionIds, class_ "shrink-1 flex flex-col", term "projectId" pid.toText, term "containerId" "sessionPlayerWrapper"] ("" :: Text))
            (not $ V.null withSessionIds)

      when (issue.issueType /= Issues.RuntimeException) $ activityPanel_ pid issueId ""

    -- RIGHT: Inline collapsible AI chat panel (checkbox + group-has CSS, persists to localStorage)
    input_ [type_ "checkbox", id_ "ai-panel-toggle", class_ "hidden", onchange_ "localStorage.setItem('ai-panel-open', this.checked); if(this.checked) htmx.trigger('#ai-response-container','load-chat')"]
    script_ """(function(){var e=document.getElementById('ai-panel-toggle');e.checked=localStorage.getItem('ai-panel-open')==='true';if(e.checked)htmx.trigger('#ai-response-container','load-chat')})()"""
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
  let isDisabled = isNothing errIdM || V.null members
  div_ [id_ "error-assignee", class_ "flex flex-col gap-2 border-t border-strokeWeak pt-3"] do
    span_ [class_ "text-xs text-textWeak"] "Assignee"
    case errIdM of
      Nothing ->
        select_ ([class_ "select select-sm w-full", disabled_ "true"] <> [name_ "assigneeId"]) do
          option_ [value_ ""] "Unassigned"
      Just errId -> do
        let actionUrl = "/p/" <> pid.toText <> "/issues/errors/" <> UUID.toText errId.unErrorPatternId <> "/assign"
        form_ [hxPost_ actionUrl, hxTarget_ "#error-assignee", hxSwap_ "outerHTML", hxTrigger_ "change"] do
          select_
            ( [class_ "select select-sm w-full", name_ "assigneeId"]
                <> [disabled_ "true" | isDisabled]
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


errorSubscriptionAction :: (HasField "id" err ErrorPatterns.ErrorPatternId, HasField "notifyEveryMinutes" err Int, HasField "subscribed" err Bool) => Projects.ProjectId -> err -> Html ()
errorSubscriptionAction pid err = do
  let isActive = err.subscribed
  let notifyEvery = err.notifyEveryMinutes
  let actionUrl = "/p/" <> pid.toText <> "/issues/errors/" <> UUID.toText err.id.unErrorPatternId <> "/subscribe"
  form_
    [ id_ "issue-subscription-action"
    , class_ "flex items-center gap-2"
    , hxPost_ actionUrl
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
        let opts :: [(Int, Text)]
            opts = [(10, "10 min"), (20, "20 min"), (30, "30 min"), (60, "1 hr"), (360, "6 hrs"), (1440, "24 hrs")]
        forM_ opts \(val, label) ->
          option_ ([value_ (show val)] <> [selected_ "true" | isActive && val == notifyEvery]) (toHtml label)


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
            void $ ErrorPatterns.resolveErrorPattern err.id now
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
    [ "You are an expert debugging assistant helping to investigate application issues and anomalies."
    , "You have access to issue details, error information, stack traces, and trace data."
    , ""
    , "CURRENT TIME (UTC): " <> show now
    , "Use this to interpret relative time requests (e.g., 'last 2 hours' → {\"since\": \"2H\"})"
    , ""
    , "When analyzing issues:"
    , "1. Explain the likely root cause based on the error type and stack trace"
    , "2. Consider the context (service, method, path) for better insights"
    , "3. Suggest specific debugging steps or fixes"
    , "4. ONLY use tools when the user explicitly asks for data not in the provided context"
    , ""
    , "IMPORTANT - TOOL USAGE GUIDELINES:"
    , "- For analysis questions (e.g., 'What could cause this?', 'Suggest a fix'), answer DIRECTLY from the provided context WITHOUT calling tools"
    , "- For chart/visualization requests (e.g., 'plot errors over time', 'show a chart of...'), construct the KQL query directly from the schema - do NOT call tools"
    , "- Only use tools when you need to fetch ACTUAL DATA values to include in your response (e.g., 'what are the top 5 services by error count?' where you need real numbers)"
    , "- The schema above tells you all available fields - use it to build queries without calling get_schema or get_field_values"
    , ""
    , "OUTPUT PRIORITY: For chart/visualization requests, ensure the KQL query is correct and the explanation analyzes patterns in the data."
    , ""
    , "RESPONSE FORMAT: Keep responses concise and scannable. Lead with a 1-sentence summary, then use bullet points or short paragraphs. Avoid walls of text. The chat panel is narrow (~400px), so brevity matters."
    , ""
    , Schema.generateSchemaForAI Schema.telemetrySchema
    , ""
    , AI.kqlGuide
    , ""
    , AI.outputFormatInstructions
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
      when includeUserMsg $ Issues.insertChatMessage pid convId "user" form.query Nothing Nothing
      Issues.insertChatMessage pid convId "assistant" response (AE.toJSON <$> widgets) (AE.toJSON <$> toolCalls)
      addRespHeaders $ aiChatResponse_ pid form.query response widgets toolCalls systemPromptM

    processIssue appCtx now convId issue = do
      -- Build system prompt (shared logic)
      fullSystemPrompt <- buildSystemPromptForIssue pid issue now
      let systemPrompt = anomalySystemPrompt now
          config = (AI.defaultAgenticConfig pid){AI.facetContext = Nothing, AI.customContext = Just fullSystemPrompt, AI.conversationId = Just convId, AI.conversationType = Just Issues.CTAnomaly, AI.systemPromptOverride = Just systemPrompt}
      result <- AI.runAgenticChatWithHistory config form.query appCtx.config.openaiModel appCtx.config.openaiApiKey
      either
        (\err -> respond (Just fullSystemPrompt) convId ("I encountered an error while analyzing this issue: " <> err) Nothing Nothing False)
        (handleChatResult (Just fullSystemPrompt) convId)
        result

    handleChatResult systemPromptM convId chatResult =
      either
        (\_ -> respond systemPromptM convId chatResult.response Nothing (Just chatResult.toolCalls) False)
        ( \aiResp ->
            let ws = if null aiResp.widgets then Nothing else Just (take 10 aiResp.widgets)
                txt = fromMaybe (bool chatResult.response "Here are the requested visualizations:" $ isJust ws) $ mfilter (not . T.null) aiResp.explanation
             in respond systemPromptM convId txt ws (Just chatResult.toolCalls) False
        )
        (AI.parseAgenticResponse chatResult)


-- | Handle AI chat history GET request
aiChatHistoryGetH :: Projects.ProjectId -> Issues.IssueId -> ATAuthCtx (RespHeaders (Html ()))
aiChatHistoryGetH pid issueId = do
  (sess, project) <- Projects.sessionAndProject pid
  now <- Time.currentTime
  -- Get issue to build system prompt
  issueM <- Issues.selectIssueById issueId
  case issueM of
    Nothing -> addRespHeaders $ aiChatHistoryView_ pid []
    Just issue -> do
      -- Build system prompt context (reuse logic from aiChatPostH)
      systemPrompt <- buildSystemPromptForIssue pid issue now
      -- Load chat history
      let convId = UUIDId issueId.unUUIDId :: UUIDId "conversation"
      messages <- Issues.selectChatHistory convId
      -- Render with system prompt prepended
      addRespHeaders $ aiChatHistoryWithSystemPrompt_ pid systemPrompt messages


-- | Build complete system prompt for an issue (shared between POST and GET)
buildSystemPromptForIssue :: Projects.ProjectId -> Issues.Issue -> UTCTime -> ATAuthCtx Text
buildSystemPromptForIssue pid issue now = do
  errorM <- bool (pure Nothing) (ErrorPatterns.getErrorPatternByHash pid issue.endpointHash) (issue.issueType == Issues.RuntimeException)
  (traceDataM, spans) <- maybe (pure (Nothing, V.empty)) fetchTrace errorM
  alertContextM <- case (issue.issueType, AE.fromJSON @Issues.QueryAlertData (getAeson issue.issueData)) of
    (Issues.QueryAlert, AE.Success alertData) -> do
      let twoDaysAgo = addUTCTime (-172800) now
      monitorM <- runMaybeT do
        monitorId <- hoistMaybe $ UUID.fromText alertData.queryId
        MaybeT $ Monitors.queryMonitorById (Monitors.QueryMonitorId monitorId)
      metricsData <- Charts.queryMetrics (Just Charts.DTMetric) (Just pid) (Just alertData.queryExpression) Nothing Nothing (Just $ show twoDaysAgo) (Just $ show now) Nothing []
      pure $ Just (alertData, monitorM, metricsData)
    _ -> pure Nothing
  let issueContext = unlines ["--- ISSUE CONTEXT ---", buildAIContext issue errorM traceDataM spans alertContextM]
      dayAgo = addUTCTime (-86400) now
  facetSummaryM <- Fields.getFacetSummary pid "otel_logs_and_spans" dayAgo now
  let systemPrompt = anomalySystemPrompt now
      fullSystemPrompt = unlines [systemPrompt, "", "--- FACET SUMMARY ---", maybe "" formatFacetSummaryForAI facetSummaryM, "", issueContext]
  pure fullSystemPrompt
  where
    fetchTrace err =
      fromMaybe (Nothing, V.empty) <$> runMaybeT do
        tId <- hoistMaybe err.recentTraceId
        trData <- MaybeT $ Telemetry.getTraceDetails pid tId (Just $ zonedTimeToUTC err.updatedAt) now
        spans <- lift $ Telemetry.getSpanRecordsByTraceId pid trData.traceId (Just trData.traceStartTime) now
        pure (Just trData, V.fromList spans)
    buildAIContext iss errM trDataM spans alertContextM =
      unlines
        $ catMaybes
          [ Just "## Issue Details"
          , Just $ "- **Title**: " <> iss.title
          , Just $ "- **Type**: " <> show iss.issueType
          , Just $ "- **Severity**: " <> iss.severity
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
          , Just $ "- **Alert Threshold**: " <> show alertData.thresholdValue <> " (trigger when " <> alertData.thresholdType <> ")"
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
          formatRow = V.imap formatCell
          formatCell idx cellM = case cellM of
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
            let topValues = take 10 values
                formattedValues = map (\fv -> fv.value <> " (" <> show fv.count <> ")") topValues
                valueStr = T.intercalate ", " formattedValues
                ellipsis = if length values > 10 then ", ..." else ""
             in "- " <> fieldName <> ": " <> valueStr <> ellipsis
          sortedFields = sortOn (\(_, vs) -> negate $ sum $ map (.count) vs) $ HM.toList facetMap
          topFields = take 30 sortedFields
       in unlines
            $ "Available telemetry fields (top values by frequency):"
            : map formatField topFields
              ++ ["... and " <> show (HM.size facetMap - 30) <> " more fields" | HM.size facetMap > 30]


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
        hasContent = not (null toolCalls) || isJust systemPromptM
    when hasContent
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


parseStoredJSON :: AE.FromJSON a => Maybe (Aeson AE.Value) -> Maybe a
parseStoredJSON = (>>= parseMaybe AE.parseJSON . getAeson)


withIssueDataH :: (AE.FromJSON a, Applicative m) => Aeson AE.Value -> (a -> m ()) -> m ()
withIssueDataH d f = case AE.fromJSON (getAeson d) of
  AE.Success v -> f v
  _ -> pass


-- | Process widgets to use cached tool call data (no re-query)
processWidgetsWithToolData :: [AI.ToolCallInfo] -> [Widget.Widget] -> [Widget.Widget]
processWidgetsWithToolData toolCalls = map \w -> case w.query >>= findToolCallData toolCalls of
  Nothing -> w
  Just rawJson -> maybe w (\ds -> w{Widget.dataset = Just ds, Widget.eager = Just True}) (toolDataToDataset rawJson)


-- | Find matching tool call data for a widget query
findToolCallData :: [AI.ToolCallInfo] -> Text -> Maybe AE.Value
findToolCallData toolCalls widgetQuery = listToMaybe [rd | tc <- toolCalls, tc.name == "run_query", Just q <- [Map.lookup "query" tc.args >>= getText], normalizeQuery q == normalizeQuery widgetQuery, Just rd <- [tc.rawData]]
  where
    getText (AE.String t) = Just t
    getText _ = Nothing


-- | Normalize query for comparison (remove whitespace variations)
normalizeQuery :: Text -> Text
normalizeQuery = unwords . words


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
pairUserAssistant (u : a : rest) | u.role == "user" && a.role == "assistant" = (u, a) : pairUserAssistant rest
pairUserAssistant (_ : rest) = pairUserAssistant rest
pairUserAssistant [] = []


-- | Parse stored content - try JSON format first (stripping code blocks), fall back to plain text
parseStoredContent :: Text -> Maybe (Aeson AE.Value) -> (Text, Maybe [Widget.Widget])
parseStoredContent content storedWidgets =
  case AI.parseLLMResponse content of
    Right aiResp -> (fromMaybe "" aiResp.explanation, if null aiResp.widgets then Nothing else Just aiResp.widgets)
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
    , term "hx-on::after-swap" "window.evalScriptsFromContent && window.evalScriptsFromContent(event.detail.elt === this ? this : this.lastElementChild); this.lastElementChild?.scrollIntoView({behavior: 'smooth', block: 'start'})"
    ]
    ""
  div_ [class_ "shrink-0 border-t border-strokeWeak p-3 flex flex-col gap-2"] do
    form_
      [ hxPost_ $ baseUrl <> "/ai_chat"
      , hxTarget_ "#ai-response-container"
      , hxSwap_ "beforeend"
      , hxIndicator_ "#ai-chat-loader"
      , term "hx-on::after-request" "this.reset()"
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
        , onclick_ $ "document.getElementById('ai-chat-input').value = '" <> txt <> "'; document.getElementById('ai-chat-input').form.requestSubmit();"
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
anomalyListGetH pid layoutM filterTM sortM timeFilter pageM perPageM loadM endpointM periodM serviceFilters typeFilters hxRequestM hxBoostedM = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  let (ackd, archived, currentFilterTab) = case filterTM of
        Just "Inbox" -> (Just False, Just False, "Inbox")
        Just "Acknowledged" -> (Just True, Nothing, "Acknowledged")
        Just "Archived" -> (Nothing, Just True, "Archived")
        _ -> (Just False, Just False, "Inbox")

  let filterV = fromMaybe "14d" timeFilter
      pageInt = maybe 0 (Unsafe.read . toString) pageM
      perPage = maybe 25 (Unsafe.read . toString) perPageM
      currentSort = fromMaybe "-created_at" sortM

  freeTierStatus <- checkFreeTierStatus pid project.paymentPlan
  currTime <- liftIO getCurrentTime

  let period = fromMaybe "24h" periodM
  ((issues, totalCount), (availableServices, availableTypes)) <-
    concurrently
      (Issues.selectIssues pid Nothing ackd archived perPage (pageInt * perPage) Nothing (Just currentSort) period serviceFilters typeFilters)
      ( concurrently
          (map (\(Only s) -> s) <$> PG.query [sql| SELECT DISTINCT service FROM apis.issues WHERE project_id = ? AND service IS NOT NULL |] (Only pid))
          (map (\(Only t) -> t) <$> PG.query [sql| SELECT DISTINCT issue_type::text FROM apis.issues WHERE project_id = ? |] (Only pid))
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
  let serviceMenu = FilterMenu{label = "Service", paramName = "service", multiSelect = True, options = map (\s -> FilterOption{label = s, value = s, isActive = s `elem` serviceFilters}) availableServices}
      typeMenu = FilterMenu{label = "Type", paramName = "type", multiSelect = True, options = map (\t -> FilterOption{label = t, value = t, isActive = t `elem` typeFilters}) availableTypes}
      issuesVM = V.fromList $ map (IssueVM False False currTime filterV) issues
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
  let issuesTable =
        Table
          { config = def{elemID = "anomalyListForm", containerId = Just "anomalyListContainer", addPadding = True, renderAsTable = True, bulkActionsInHeader = Just 0}
          , columns = issueColumnsWithToggle pid period (Just $ periodToggle_ baseUrl "anomalyListContainer" period)
          , rows = issuesVM
          , features =
              def
                { rowId = Just \(IssueVM _ _ _ _ issue) -> Issues.issueIdText issue.id
                , rowAttrs = Just issueRowAttrs
                , bulkActions =
                    [ BulkAction{icon = Just "check", title = "Acknowledge", uri = "/p/" <> pid.toText <> "/issues/bulk_actions/acknowledge"}
                    , BulkAction{icon = Just "archive", title = "Archive", uri = "/p/" <> pid.toText <> "/issues/bulk_actions/archive"}
                    ]
                , search = Just ClientSide
                , tableHeaderActions = Just tableActions
                , pagination = if totalCount > 0 then Just paginationConfig else Nothing
                , zeroState =
                    Just
                      $ ZeroState
                        { icon = "empty-set"
                        , title = "No Issues Or Errors."
                        , description = "Issues and errors appear here automatically once you integrate an SDK."
                        , actionText = "View SDK setup guides"
                        , destination = Right "https://monoscope.tech/docs/sdks/"
                        }
                }
          }
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Issues"
          , menuItem = Just "Issues"
          , freeTierStatus = freeTierStatus
          , config = appCtx.config
          , headContent = Just highlightJsHead_
          , navTabs =
              Just
                $ toHtml
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
          }
  addRespHeaders $ case (layoutM, hxRequestM, hxBoostedM, loadM) of
    (_, _, _, Just "true") -> ALRows $ TableRows{columns = issueColumns pid period, rows = issuesVM, emptyState = Nothing, renderAsTable = True, rowId = Just \(IssueVM _ _ _ _ issue) -> Issues.issueIdText issue.id, rowAttrs = Just issueRowAttrs, pagination = if totalCount > 0 then Just paginationConfig else Nothing}
    _ -> ALPage $ PageCtx bwconf issuesTable


data AnomalyListGet
  = ALPage (PageCtx (Table IssueVM))
  | ALRows (TableRows IssueVM)


instance ToHtml AnomalyListGet where
  toHtml (ALPage pg) = toHtml pg
  toHtml (ALRows rows) = toHtml rows
  toHtmlRaw = toHtml


issueRowAttrs :: IssueVM -> [Attribute]
issueRowAttrs (IssueVM _ _ _ _ issue) =
  [class_ $ "group/row hover:bg-fillWeaker " <> bg] <> case issue.severity of
    "critical" -> [style_ "box-shadow: inset 3px 0 0 var(--color-fillError-strong)"]
    "warning" -> [style_ "box-shadow: inset 3px 0 0 var(--color-fillWarning-strong)"]
    _ -> []
  where
    bg = case issue.severity of
      "critical" -> "bg-fillError-weak"
      "warning" -> "bg-fillWarning-weak"
      _ -> ""


-- | (icon, iconStyle, colorClass, tooltip) — uses shape+color so status isn't color-only
anomalyStatusIndicator :: Bool -> Bool -> Text -> (Text, Text, Text, Text)
anomalyStatusIndicator _ True _ = ("archive", "regular", "text-fillStrong", "Archived")
anomalyStatusIndicator True False _ = ("circle-check", "regular", "text-fillSuccess-strong", "Acknowledged")
anomalyStatusIndicator False False "critical" = ("octagon-exclamation", "regular", "text-fillError-strong", "Critical")
anomalyStatusIndicator False False "warning" = ("triangle-alert", "regular", "text-fillWarning-strong", "Warning")
anomalyStatusIndicator False False _ = ("circle-alert", "regular", "text-textWeak", "Active")


data IssueVM = IssueVM Bool Bool UTCTime Text Issues.IssueL
  deriving stock (Show)


periodToggle_ :: Text -> Text -> Text -> Html ()
periodToggle_ baseUrl targetId currentPeriod =
  span_ [class_ "inline-flex rounded-md border border-strokeWeak overflow-hidden"] do
    forM_ ["24h" :: Text, "7d" :: Text] \val ->
      let url = deleteParam "period" baseUrl <> "&period=" <> val
       in button_
            [ class_ $ "cursor-pointer px-2 py-0.5 text-xs font-medium transition-colors " <> bool "text-textWeak hover:bg-fillWeaker hover:text-textStrong" "bg-fillWeak text-textStrong" (val == currentPeriod)
            , type_ "button"
            , hxGet_ url
            , hxTarget_ $ "#" <> targetId
            , hxSelect_ $ "#" <> targetId
            , hxSwap_ "outerHTML"
            , hxPushUrl_ "true"
            ]
            $ toHtml val


issueColumns :: Projects.ProjectId -> Text -> [Column IssueVM]
issueColumns pid period = issueColumnsWithToggle pid period Nothing


issueColumnsWithToggle :: Projects.ProjectId -> Text -> Maybe (Html ()) -> [Column IssueVM]
issueColumnsWithToggle pid period toggleM =
  [ col "Issue" (renderIssueMainCol pid) & withAttrs [class_ "min-w-0 max-w-0 w-full"]
  , col ("Events (" <> period <> ")") renderIssueEventsCol & withAttrs [class_ "w-24 max-md:hidden"]
  , col "Last Seen" renderIssueDateCol & withAttrs [class_ "w-24 max-md:hidden"]
  , col "Activity" renderIssueChartCol & withAttrs [class_ "w-40 max-md:hidden"] & maybe identity withColHeaderExtra toggleM
  ]


renderIssueEventsCol :: IssueVM -> Html ()
renderIssueEventsCol (IssueVM _ isWidget _ _ issue) =
  unless isWidget
    $ span_ [class_ $ "tabular-nums font-medium " <> countStyle issue.eventCount]
    $ toHtml
    $ formatWithCommas (fromIntegral issue.eventCount)
  where
    countStyle n
      | n >= 100 = "text-sm text-fillError-strong"
      | n >= 10 = "text-sm text-fillWarning-strong"
      | otherwise = "text-sm text-textStrong"


renderIssueDateCol :: IssueVM -> Html ()
renderIssueDateCol (IssueVM _ _ currTime _ issue) =
  span_ [class_ "text-xs text-textWeak"] $ toHtml $ compactTimeAgo $ toText $ prettyTimeAuto currTime $ zonedTimeToUTC issue.createdAt


renderIssueChartCol :: IssueVM -> Html ()
renderIssueChartCol (IssueVM _ _ _ _ issue) = sparkline_ buckets
  where
    PGArray buckets = issue.activityBuckets


sparkline_ :: [Int] -> Html ()
sparkline_ buckets
  | null buckets || all (== 0) buckets = span_ [class_ "text-textWeak text-xs"] "-"
  | otherwise = do
      let peakVal = foldr max 1 buckets
          peak = fromIntegral @Int @Double peakVal
          n = length buckets
          h = 40 :: Int
          barZone = 32 :: Int
          peakLabel = formatCompact peakVal
          labelW = T.length peakLabel * 9 + 4
          gap = 2 :: Int
          barW = max 2 (120 `div` n - gap)
          barsEnd = n * (barW + gap)
          topPad = h - barZone
          peakIdx = maybe 0 fst $ viaNonEmpty head $ filter ((== peakVal) . snd) $ zip [0 ..] buckets
          lineX1 = peakIdx * (barW + gap) + barW
          lineX2 = barsEnd + 2
          w = lineX2 + labelW
      with
        (makeElement "svg")
        [ makeAttribute "viewBox" $ toText [fmt|0 0 {w} {h}|]
        , style_ [fmt|width:100%;height:{h}px|]
        , makeAttribute "preserveAspectRatio" "xMinYMid meet"
        ]
        $ do
          forM_ (zip [0 ..] buckets) \(i, v) -> do
            let barH = max 2 (round @Double @Int $ (fromIntegral v / peak) * fromIntegral barZone)
            with
              (makeElement "rect")
              [ makeAttribute "x" $ show $ i * (barW + gap)
              , makeAttribute "y" $ show $ h - barH
              , makeAttribute "width" $ show barW
              , makeAttribute "height" $ show barH
              , makeAttribute "rx" "1.5"
              , makeAttribute "fill" "var(--color-chart-default)"
              , makeAttribute "opacity" "0.7"
              ]
              ""
          with
            (makeElement "line")
            [ makeAttribute "x1" $ show lineX1
            , makeAttribute "y1" $ show topPad
            , makeAttribute "x2" $ show lineX2
            , makeAttribute "y2" $ show topPad
            , makeAttribute "stroke" "var(--color-textWeak)"
            , makeAttribute "stroke-width" "0.7"
            , makeAttribute "stroke-dasharray" "3,2"
            ]
            ""
          with
            (makeElement "text")
            [ makeAttribute "x" $ show (lineX2 + 1)
            , makeAttribute "y" $ show (topPad + 4)
            , makeAttribute "text-anchor" "start"
            , makeAttribute "fill" "var(--color-textWeak)"
            , makeAttribute "font-size" "11"
            , makeAttribute "font-family" "system-ui"
            ]
            $ toHtml peakLabel
  where
    formatCompact n
      | n >= 1000000 = toText [fmt|{fromIntegral @Int @Double n / 1000000:.1f}M|]
      | n >= 1000 = toText [fmt|{fromIntegral @Int @Double n / 1000:.1f}K|]
      | otherwise = show n


highlightJsHead_ :: Monad m => HtmlT m ()
highlightJsHead_ = do
  link_ [rel_ "stylesheet", href_ $(hashAssetFile "/public/assets/deps/highlightjs/atom-one-light.min.css"), media_ "screen", id_ "hljs-light"]
  link_ [rel_ "stylesheet", href_ $(hashAssetFile "/public/assets/deps/highlightjs/atom-one-dark.min.css"), media_ "screen", id_ "hljs-dark"]
  script_ [src_ $(hashAssetFile "/public/assets/deps/highlightjs/highlight.min.js")] ("" :: Text)
  script_ [src_ $(hashAssetFile "/public/assets/deps/highlightjs/sql.min.js")] ("" :: Text)
  script_
    """
    function setHljsTheme() {
      const dark = document.body.getAttribute('data-theme') === 'dark';
      document.getElementById('hljs-light').disabled = dark;
      document.getElementById('hljs-dark').disabled = !dark;
    }
    function highlightSnippets(root) { root.querySelectorAll('code:not(.hljs)').forEach(el => hljs.highlightElement(el)); }
    document.addEventListener('DOMContentLoaded', () => { setHljsTheme(); highlightSnippets(document); });
    document.addEventListener('htmx:afterSettle', e => highlightSnippets(e.detail.elt));
    """


renderLogContent_ :: Monad m => Text -> HtmlT m ()
renderLogContent_ txt =
  if "⇒" `T.isInfixOf` txt
    then div_ [class_ "flex flex-wrap items-center gap-1 p-4 max-h-80 overflow-y-auto"] $ renderSummaryText_ txt
    else div_ [class_ "p-4 max-h-80 overflow-y-auto"] $ pre_ [class_ "text-sm text-textWeak font-mono whitespace-pre-wrap [&_code.hljs]:!bg-transparent [&_code.hljs]:!p-0"] $ code_ [] $ toHtml txt


renderSummaryText_ :: Monad m => Text -> HtmlT m ()
renderSummaryText_ txt = forM_ (words txt) \token ->
  case T.breakOn "⇒" token of
    (_, "") -> span_ [class_ "mr-1"] $ toHtml $ unesc token
    (left, rest) -> do
      let value = unesc $ T.drop 1 rest
          (field, style) = case T.breakOn ";" left of
            (f, s) | not (T.null s) -> (f, T.drop 1 s)
            _ -> ("", left)
          cls = case style of
            s | "badge-" `T.isPrefixOf` s -> "cbadge-sm " <> s
            "neutral" -> "cbadge-sm badge-neutral"
            "text-textWeak" -> "text-textWeak text-xs"
            "text-weak" -> "text-textWeak text-xs"
            "text-textStrong" -> "text-textStrong text-xs font-medium"
            _ -> "cbadge-sm badge-neutral"
          tipAttr = [term "data-tippy-content" field | not (T.null field)]
      span_ ([class_ $ cls <> " mr-1 inline-block"] <> tipAttr) $ toHtml value
  where
    unesc = T.replace "\\\"" "\"" . T.replace "\\n" " " . T.replace "\\t" " "


renderIssueTitle_ :: Issues.IssueL -> Html ()
renderIssueTitle_ issue
  | T.null title = "(Untitled)"
  | "⇒" `T.isInfixOf` title = renderSummaryText_ title
  | looksLikeRawPattern title = span_ [class_ "font-mono text-[0.8125rem] break-all"] $ renderWithPlaceholders_ title
  | otherwise = renderWithPlaceholders_ title
  where
    title = stripIssuePrefix issue.title
    stripIssuePrefix =
      foldl' (\t pfx -> fromMaybe t $ T.stripPrefix pfx t)
        <*> const
          ["New Log Pattern: ", "Log Pattern Spike: ", "Log Pattern Drop: ", "New Log Pattern Detected: "]
    looksLikeRawPattern t = any (`T.isInfixOf` t) [";right-", "v{", "<*>", "]{", "ERROR ERROR"]


-- | Render text with <> placeholders styled as distinct tokens
renderWithPlaceholders_ :: Monad m => Text -> HtmlT m ()
renderWithPlaceholders_ = go
  where
    placeholder = span_ [class_ "text-textWeak opacity-60"] "<>"
    go t = case T.breakOn "<>" t of
      (before, "") -> toHtml before
      (before, rest) -> do toHtml before; placeholder; go (T.drop 2 rest)


renderIssueMainCol :: Projects.ProjectId -> IssueVM -> Html ()
renderIssueMainCol pid (IssueVM _ _ currTime period issue) = do
  let isAcknowledged = isJust issue.acknowledgedAt
      isArchived = isJust issue.archivedAt
      (icon, iconStyle, iconColor, tooltip) = anomalyStatusIndicator isAcknowledged isArchived issue.severity
      issueUrl = "/p/" <> pid.toText <> "/issues/" <> Issues.issueIdText issue.id
  div_ [class_ "flex flex-col gap-1 py-0.5 min-w-0"] do
    div_ [class_ "flex items-center gap-2 min-w-0"] do
      div_ [class_ "text-sm line-clamp-2 min-w-0"] do
        span_ [class_ $ "inline-flex align-middle mr-1 " <> iconColor, title_ tooltip, Aria.label_ tooltip] $ faSprite_ icon iconStyle "w-3.5 h-3.5"
        span_ [class_ "text-xs tabular-nums mr-1 text-textWeak max-md:text-textStrong max-md:font-medium"] $ toHtml $ "#" <> show issue.seqNum <> " "
        a_ ([href_ issueUrl, class_ "font-medium text-textStrong hover:text-textBrand transition-colors"] <> navTabAttrs) $ renderIssueTitle_ issue
      span_ [class_ "shrink-0 flex items-center gap-1.5 max-md:hidden"] do
        severityBadge_ issue.severity
        issueStateBadge_ issue.latestStateEvent
        when isAcknowledged $ span_ [class_ "badge badge-sm badge-ghost gap-1"] do faSprite_ "check" "regular" "h-3 w-3"; "Ack'd"
      div_ [class_ "shrink-0 flex gap-1 items-center opacity-0 group-hover/row:opacity-100 has-[:focus-within]:opacity-100 transition-opacity max-md:hidden"] do
        inlineBtn (bool "Acknowledge" "Unacknowledge" isAcknowledged) "check" (hxGet_ $ issueUrl <> bool "/acknowledge" "/unacknowledge" isAcknowledged) []
        inlineBtn (bool "Archive" "Unarchive" isArchived) "archive" (hxGet_ $ issueUrl <> bool "/archive" "/unarchive" isArchived) []
    div_ [class_ "hidden max-md:flex items-center gap-1.5 flex-wrap"] do
      severityBadge_ issue.severity
      issueStateBadge_ issue.latestStateEvent
    div_ [class_ "max-md:hidden"] $ issuePreview_ issue
    div_ [class_ "hidden max-md:flex items-center justify-between text-xs text-textWeak"] do
      div_ [class_ "flex items-center gap-1.5"] do
        span_ [class_ $ "tabular-nums" <> bool "" " font-medium text-textStrong" (issue.eventCount > 100)] $ toHtml $ show issue.eventCount <> bool " event" " events" (issue.eventCount /= 1) <> " (" <> period <> ")"
        span_ [class_ "opacity-30"] "·"
        span_ [] $ toHtml $ compactTimeAgo $ toText $ prettyTimeAuto currTime $ zonedTimeToUTC issue.createdAt
      div_ [class_ "flex items-center gap-3"] do
        button_ [type_ "button", class_ "cursor-pointer text-textBrand tap-target font-medium", hxSwap_ "outerHTML", hxTarget_ "closest .itemsListItem", hxGet_ $ issueUrl <> bool "/acknowledge" "/unacknowledge" isAcknowledged] $ toHtml $ bool "Ack" "Unack" isAcknowledged
        button_ [type_ "button", class_ "cursor-pointer text-textBrand tap-target font-medium", hxSwap_ "outerHTML", hxTarget_ "closest .itemsListItem", hxGet_ $ issueUrl <> bool "/archive" "/unarchive" isArchived] $ toHtml $ bool "Archive" "Unarchive" isArchived
  where
    inlineBtn tip icon hxAction extraAttrs =
      button_ ([type_ "button", term "data-tippy-content" tip, class_ "cursor-pointer hover:text-textBrand transition-colors tap-target", hxSwap_ "outerHTML", hxTarget_ "closest .itemsListItem", hxAction] <> extraAttrs)
        $ faSprite_ icon "regular" "h-3.5 w-3.5"


issueCardCompact_ :: Projects.ProjectId -> UTCTime -> Issues.IssueL -> Html ()
issueCardCompact_ pid now issue = do
  let (icon, iconStyle, iconColor, tooltip) = anomalyStatusIndicator (isJust issue.acknowledgedAt) (isJust issue.archivedAt) issue.severity
      issueUrl = "/p/" <> pid.toText <> "/issues/" <> Issues.issueIdText issue.id
  a_ ([href_ issueUrl, class_ "block border border-strokeWeak rounded-xl p-3 hover:bg-bgRaised transition-colors"] <> navTabAttrs) do
    div_ [class_ "flex items-center gap-2 min-w-0"] do
      span_ [class_ $ "shrink-0 " <> iconColor, title_ tooltip, Aria.label_ tooltip] $ faSprite_ icon iconStyle "w-3.5 h-3.5"
      span_ [class_ "text-xs text-textWeak shrink-0 tabular-nums"] $ toHtml $ "#" <> show issue.seqNum
      span_ [class_ "text-sm font-medium text-textStrong truncate min-w-0"] $ renderIssueTitle_ issue
      severityBadge_ issue.severity
      span_ [class_ "text-xs text-textWeak shrink-0 ml-auto"] $ toHtml $ compactTimeAgo $ toText $ prettyTimeAuto now $ zonedTimeToUTC issue.createdAt
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
  _ -> pass
  where
    badge cls = span_ [class_ $ "badge badge-sm border " <> cls]


issuePreview_ :: Issues.IssueL -> Html ()
issuePreview_ issue = div_ [class_ "flex items-center gap-2 min-w-0 overflow-hidden text-xs text-textWeak"] do
  issueTypeBadge issue.issueType issue.critical
  whenJust issue.service $ span_ [class_ "shrink-0", term "data-tippy-content" "Service"] . toHtml
  span_ [class_ "shrink-0 opacity-40"] "·"
  snippet
  where
    snippet = case issue.issueType of
      Issues.RuntimeException -> withIssueDataH @Issues.RuntimeExceptionData issue.issueData \d ->
        previewSnippet $ d.errorType <> ": " <> d.errorMessage
      Issues.QueryAlert -> withIssueDataH @Issues.QueryAlertData issue.issueData \d ->
        previewSnippet d.queryExpression
      Issues.LogPattern -> withIssueDataH @Issues.LogPatternData issue.issueData \d ->
        logPatternPreview d.logPattern d.sampleMessage
      Issues.LogPatternRateChange -> withIssueDataH @Issues.LogPatternRateChangeData issue.issueData \d ->
        logPatternPreview d.logPattern d.sampleMessage
      Issues.ApiChange -> withIssueDataH @Issues.APIChangeData issue.issueData \d ->
        previewSnippet $ d.endpointMethod <> " " <> d.endpointPath
    previewSnippet txt = span_ [class_ "font-mono truncate min-w-0", term "data-tippy-content" txt] $ renderWithPlaceholders_ $ unescapeJson txt
    summaryPreview txt = span_ [class_ "truncate min-w-0"] $ renderSummaryText_ txt
    logPatternPreview pat sampleMsg
      | "⇒" `T.isInfixOf` pat = summaryPreview pat
      | Just msg <- sampleMsg, not (T.null msg) = previewSnippet msg
      | otherwise = previewSnippet pat
    unescapeJson = T.replace "\\\"" "\"" . T.replace "\\n" " " . T.replace "\\t" " "


anomalyAcknowledgeButton :: Projects.ProjectId -> Issues.IssueId -> Bool -> Text -> Html ()
anomalyAcknowledgeButton pid aid acked host = do
  let acknowledgeAnomalyEndpoint = "/p/" <> pid.toText <> "/issues/" <> Issues.issueIdText aid <> if acked then "/unacknowledge" else "/acknowledge?host=" <> host
  a_
    [ class_ $ "btn btn-sm gap-1.5 " <> if acked then "bg-fillSuccess-weak text-textSuccess border-strokeSuccess-weak" else "btn-primary"
    , term "data-tippy-content" $ if acked then "unacknowledge issue" else "acknowledge issue"
    , Aria.label_ $ if acked then "Unacknowledge issue" else "Acknowledge issue"
    , hxGet_ acknowledgeAnomalyEndpoint
    , hxSwap_ "outerHTML"
    ]
    do
      faSprite_ "check" "regular" "w-4 h-4"
      span_ [class_ "max-md:hidden"] $ if acked then "Acknowledged" else "Acknowledge"


anomalyArchiveButton :: Projects.ProjectId -> Issues.IssueId -> Bool -> Html ()
anomalyArchiveButton pid aid archived = do
  let archiveAnomalyEndpoint = "/p/" <> pid.toText <> "/issues/" <> Issues.issueIdText aid <> if archived then "/unarchive" else "/archive"
  a_
    [ class_ $ "btn btn-sm btn-ghost gap-1.5 " <> if archived then "bg-fillWarning-weak text-textWarning border-strokeWarning-weak" else ""
    , term "data-tippy-content" $ if archived then "unarchive" else "archive"
    , Aria.label_ $ if archived then "Unarchive issue" else "Archive issue"
    , hxGet_ archiveAnomalyEndpoint
    , hxSwap_ "outerHTML"
    ]
    do
      faSprite_ "archive" "regular" "w-4 h-4"
      span_ [class_ "max-md:hidden"] $ if archived then "Unarchive" else "Archive"


issueTypeLabel :: Issues.IssueType -> Bool -> Html ()
issueTypeLabel issueType critical = span_ [class_ $ "flex items-center gap-1.5 text-xs font-medium " <> color] do
  faSprite_ icon "regular" "w-3 h-3"; toHtml txt
  where
    (color, icon, txt) = issueTypeMeta issueType critical


issueTypeBadge :: Issues.IssueType -> Bool -> Html ()
issueTypeBadge issueType critical = span_ [class_ $ "flex items-center gap-1 text-[11px] whitespace-nowrap " <> color, term "data-tippy-content" fullTxt] do
  faSprite_ icon "regular" "w-3 h-3 shrink-0"; toHtml shortTxt
  where
    (color, icon, fullTxt) = issueTypeMeta issueType critical
    shortTxt = case issueType of
      Issues.LogPattern -> "Log"
      Issues.LogPatternRateChange -> "Rate"
      _ -> fullTxt


issueTypeMeta :: Issues.IssueType -> Bool -> (Text, Text, Text)
issueTypeMeta issueType critical = case issueType of
  Issues.RuntimeException -> ("text-fillError-strong", "triangle-alert", "Error")
  Issues.QueryAlert -> ("text-fillWarning-strong", "zap", "Alert")
  Issues.LogPattern -> ("text-fillInformation-strong", "file-text", "Log Pattern")
  Issues.LogPatternRateChange -> ("text-fillWarning-strong", "activity", "Rate Change")
  Issues.ApiChange | critical -> ("text-fillError-strong", "exclamation-triangle", "Breaking")
  Issues.ApiChange -> ("text-fillInformation-strong", "info", "Incremental")


issueActivityGetH :: Projects.ProjectId -> Issues.IssueId -> ATAuthCtx (RespHeaders (Html ()))
issueActivityGetH pid issueId = do
  (_sess, _project) <- Projects.sessionAndProject pid
  activities <- Issues.selectIssueActivity pid issueId
  now <- Time.currentTime
  let userIds = ordNub $ mapMaybe (.createdBy) activities
  users :: [Projects.User] <- if null userIds then pure [] else PG.query [sql| SELECT id, created_at, updated_at, deleted_at, active, first_name, last_name, display_image_url, email, phone_number, is_sudo FROM users.users WHERE id = ANY(?::uuid[]) |] (Only $ V.fromList userIds)
  let userMap = Map.fromList $ map (\u -> (u.id, u)) users
  addRespHeaders $ issueActivityTimeline_ userMap now activities


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
  Issues.IEAcknowledged -> ("check", "bg-fillBrand-weak text-fillBrand-strong", "Acknowledged")
  Issues.IEUnacknowledged -> ("rotate-left", "bg-fillWeaker text-textWeak", "Unacknowledged")
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
