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
)
where

import BackgroundJobs qualified as BackgroundJobs
import Data.Aeson qualified as AE
import Data.Aeson.Types (Parser, parseMaybe)
import Data.CaseInsensitive qualified as CI
import Data.Default (def)
import Data.HashMap.Strict qualified as HM
import Data.Map qualified as Map
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX qualified as POSIX
import Data.Time.LocalTime (zonedTimeToUTC)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.Newtypes (Aeson (..), getAeson)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.PostgreSQL qualified as PG
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import GHC.Records (HasField)
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (TermRaw (termRaw))
import Lucid.Htmx (hxGet_, hxIndicator_, hxPost_, hxSwap_, hxTarget_, hxTrigger_)
import Models.Apis.Anomalies (FieldChange (..), PayloadChange (..))
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Errors (ErrorId (..))
import Models.Apis.Errors qualified as Errors
import Models.Apis.Fields (FacetData (..), FacetSummary (..), FacetValue (..))
import Models.Apis.Fields qualified as Fields
import Models.Apis.Issues qualified as Issues
import Models.Apis.Monitors qualified as Monitors
import Models.Apis.RequestDumps qualified as RequestDump
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects (User (id))
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Schema qualified as Schema
import Models.Telemetry.Telemetry qualified as Telemetry
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Charts.Charts qualified as Charts
import Pages.Components (BadgeColor (..), PanelCfg (..), emptyState_, iconBadgeSq_, panel_, resizer_, statBox_)
import Pages.LogExplorer.Log (virtualTable)
import Pages.Telemetry (tracePage)
import Pkg.AI qualified as AI
import Pkg.Components.Table (BulkAction (..), Column (..), Config (..), Features (..), Pagination (..), SearchMode (..), TabFilter (..), TabFilterOpt (..), Table (..), TableHeaderActions (..), TableRows (..), ZeroState (..), col, withAttrs)
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..))
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast)
import Text.MMark qualified as MMark
import Text.Time.Pretty (prettyTimeAuto)
import Utils (changeTypeFillColor, checkFreeTierExceeded, escapedQueryPartial, faSprite_, formatUTC, htmxOverlayIndicator_, lookupValueText, methodFillColor, statusFillColor, toUriStr)
import Web.FormUrlEncoded (FromForm)


newtype AnomalyBulkForm = AnomalyBulk
  { anomalyId :: [Text]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


acknowledgeAnomalyGetH :: Projects.ProjectId -> Anomalies.AnomalyId -> Maybe Text -> ATAuthCtx (RespHeaders AnomalyAction)
acknowledgeAnomalyGetH pid aid hostM = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  let issueId = UUIDId aid.unUUIDId
  _ <- Issues.acknowledgeIssue issueId sess.user.id
  let text_id = V.fromList [UUID.toText aid.unUUIDId]
  v <- Anomalies.acknowledgeAnomalies sess.user.id text_id
  _ <- Anomalies.acknowlegeCascade sess.user.id (V.fromList v)
  addRespHeaders $ Acknowlege pid (UUIDId aid.unUUIDId) True


unAcknowledgeAnomalyGetH :: Projects.ProjectId -> Anomalies.AnomalyId -> ATAuthCtx (RespHeaders AnomalyAction)
unAcknowledgeAnomalyGetH pid aid = do
  (sess, project) <- Sessions.sessionAndProject pid
  let q = [sql| update apis.anomalies set acknowledged_by=null, acknowledged_at=null where id=? |]
  let qI = [sql| update apis.issues set acknowledged_by=null, acknowledged_at=null where id=? |]
  _ <- PG.execute qI (Only aid)
  _ <- PG.execute q (Only aid)
  addRespHeaders $ Acknowlege pid (UUIDId aid.unUUIDId) False


archiveAnomalyGetH :: Projects.ProjectId -> Anomalies.AnomalyId -> ATAuthCtx (RespHeaders AnomalyAction)
archiveAnomalyGetH pid aid = do
  (sess, project) <- Sessions.sessionAndProject pid
  let q = [sql| update apis.anomalies set archived_at=NOW() where id=? |]
  let qI = [sql| update apis.issues set archived_at=NOW() where id=? |]
  _ <- PG.execute qI (Only aid)
  _ <- PG.execute q (Only aid)
  addRespHeaders $ Archive pid (UUIDId aid.unUUIDId) True


unArchiveAnomalyGetH :: Projects.ProjectId -> Anomalies.AnomalyId -> ATAuthCtx (RespHeaders AnomalyAction)
unArchiveAnomalyGetH pid aid = do
  (sess, project) <- Sessions.sessionAndProject pid
  let q = [sql| update apis.anomalies set archived_at=null where id=? |]
  let qI = [sql| update apis.issues set archived_at=null where id=? |]
  _ <- PG.execute qI (Only aid)
  _ <- PG.execute q (Only aid)
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
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  if null items.anomalyId
    then do
      addErrorToast "No items selected" Nothing
      addRespHeaders Bulk
    else do
      _ <- case action of
        "acknowledge" -> do
          v <- Anomalies.acknowledgeAnomalies sess.user.id (V.fromList items.anomalyId)
          _ <- Anomalies.acknowlegeCascade sess.user.id (V.fromList v)
          pass
        "archive" -> do
          _ <- PG.execute [sql| update apis.anomalies set archived_at=NOW() where id=ANY(?::uuid[]) |] (Only $ V.fromList items.anomalyId)
          pass
        _ -> error $ "unhandled anomaly bulk action state " <> action
      addSuccessToast (action <> "d items Successfully") Nothing
      addRespHeaders Bulk


anomalyDetailGetH :: Projects.ProjectId -> Issues.IssueId -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
anomalyDetailGetH pid issueId firstM =
  anomalyDetailCore pid firstM $ \_ ->
    Issues.selectIssueById issueId


anomalyDetailHashGetH :: Projects.ProjectId -> Text -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
anomalyDetailHashGetH pid issueId firstM = anomalyDetailCore pid firstM \_ -> Issues.selectIssueByHash pid issueId


anomalyDetailCore :: Projects.ProjectId -> Maybe Text -> (Projects.ProjectId -> ATAuthCtx (Maybe Issues.Issue)) -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
anomalyDetailCore pid firstM fetchIssue = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  issueM <- fetchIssue pid
  members <- V.fromList <$> ProjectMembers.selectActiveProjectMembers pid
  userPermission <- ProjectMembers.getUserPermission pid sess.user.id
  now <- Time.currentTime
  let baseBwconf = (def :: BWConfig){sessM = Just sess, currProject = Just project, pageTitle = "Anomaly Detail", config = appCtx.config}
  case issueM of
    Nothing -> do
      addErrorToast "Issue not found" Nothing
      addRespHeaders
        $ PageCtx baseBwconf
        $ toHtml ("Issue not found" :: Text)
    Just issue -> do
      errorM <-
        issue.issueType & \case
          Issues.RuntimeException -> Errors.getErrorLByHash pid issue.targetHash
          _ -> pure Nothing
      let canResolve =
            userPermission
              == Just ProjectMembers.PAdmin
              || any (\err -> err.assigneeId == Just sess.user.id) errorM
      let bwconf =
            baseBwconf
              { pageActions = Just $ div_ [class_ "flex gap-2"] do
                  anomalyAcknowledgeButton pid (UUIDId issue.id.unUUIDId) (isJust issue.acknowledgedAt) ""
                  anomalyArchiveButton pid (UUIDId issue.id.unUUIDId) (isJust issue.archivedAt)
                  when (issue.issueType == Issues.RuntimeException) do
                    whenJust errorM \err ->
                      errorResolveAction pid err.id err.state canResolve
                    whenJust errorM \err ->
                      errorSubscriptionAction pid err
              }
      (trItem, spanRecs) <- case errorM of
        Just err -> do
          let targetTIdM = maybe err.recentTraceId (const err.firstTraceId) firstM
              targetTme = maybe (zonedTimeToUTC err.updatedAt) (const $ zonedTimeToUTC err.createdAt) firstM
          case targetTIdM of
            Just x -> do
              trM <- Telemetry.getTraceDetails pid x (Just targetTme) now
              case trM of
                Just traceItem -> do
                  spanRecords' <-
                    Telemetry.getSpanRecordsByTraceId pid traceItem.traceId (Just traceItem.traceStartTime) now
                  pure (Just traceItem, V.fromList spanRecords')
                Nothing -> pure (Nothing, V.empty)
            Nothing -> pure (Nothing, V.empty)
        Nothing -> pure (Nothing, V.empty)
      addRespHeaders $ PageCtx bwconf $ anomalyDetailPage pid issue trItem spanRecs errorM now (isJust firstM) members


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


anomalyDetailPage :: Projects.ProjectId -> Issues.Issue -> Maybe Telemetry.Trace -> V.Vector Telemetry.OtelLogsAndSpans -> Maybe Errors.ErrorL -> UTCTime -> Bool -> V.Vector ProjectMembers.ProjectMemberVM -> Html ()
anomalyDetailPage pid issue tr otellogs errM now isFirst members = do
  let spanRecs = V.catMaybes $ Telemetry.convertOtelLogsAndSpansToSpanRecord <$> otellogs
      issueId = UUID.toText issue.id.unUUIDId
      severityBadge "critical" = span_ [class_ "inline-flex items-center justify-center rounded-md px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 bg-fillError-weak text-fillError-strong border-2 border-strokeError-strong shadow-sm"] "CRITICAL"
      severityBadge "warning" = span_ [class_ "inline-flex items-center justify-center rounded-md px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 bg-fillWarning-weak text-fillWarning-strong border border-strokeWarning-weak shadow-sm"] "WARNING"
      severityBadge _ = pass
  div_ [class_ "pt-8 mx-auto px-4 w-full flex flex-col gap-4 overflow-auto pb-32"] do
    -- Header
    div_ [class_ "flex flex-col gap-3"] do
      div_ [class_ "flex gap-2 flex-wrap items-center"] do
        issueTypeBadge issue.issueType issue.critical
        severityBadge issue.severity
      h3_ [class_ "text-textStrong text-2xl font-semibold"] $ toHtml issue.title
      p_ [class_ "text-sm text-textWeak max-w-3xl"] $ toHtml issue.recommendedAction
    let widget title q =
          div_ [class_ "col-span-4"]
            $ Widget.widget_
            $ (def :: Widget.Widget)
              { Widget.standalone = Just True
              , Widget.id = Just $ issueId <> "-timeline"
              , Widget.naked = Just True
              , Widget.wType = Widget.WTTimeseries
              , Widget.title = Just title
              , Widget.showTooltip = Just True
              , Widget.xAxis = Just (def{Widget.showAxisLabel = Just True})
              , Widget.yAxis = Just (def{Widget.showOnlyMaxLabel = Just True})
              , Widget.query = Just q
              , Widget._projectId = Just issue.projectId
              , Widget.hideLegend = Just True
              }
    -- Two Column Layout
    case issue.issueType of
      Issues.RuntimeException -> do
        case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (exceptionData :: Issues.RuntimeExceptionData) -> do
            div_ [class_ "grid grid-cols-4 lg:grid-cols-8 gap-4"] do
              -- Stats (1 column each)
              whenJust errM $ \err -> do
                statBox_ (Just pid) Nothing "Affected Requests" "" (show err.occurrences) Nothing Nothing
                statBox_ (Just pid) Nothing "Affected Clients" "" (show err.affectedUsers) Nothing Nothing
                timeStatBox_ "First Seen" $ prettyTimeAuto now $ zonedTimeToUTC err.createdAt
                timeStatBox_ "Last Seen" $ prettyTimeAuto now $ zonedTimeToUTC (fromMaybe err.updatedAt err.lastOccurredAt)
              widget "Error trend" "status_code == \"ERROR\" | summarize count(*) by bin_auto(timestamp), status_code"
            div_ [class_ "flex flex-col gap-4"] do
              div_ [class_ "grid grid-cols-2 gap-4 w-full"] do
                div_ [class_ "surface-raised rounded-2xl overflow-hidden"] do
                  div_ [class_ "px-4 py-3 border-b border-strokeWeak flex items-center justify-between"] do
                    div_ [class_ "flex items-center gap-2"] do
                      faSprite_ "code" "regular" "w-4 h-4 text-iconNeutral"
                      span_ [class_ "text-sm font-medium text-textStrong"] "Stack Trace"
                  div_ [class_ "p-4 max-h-80 overflow-y-auto"] do
                    pre_ [class_ "text-sm text-textWeak font-mono leading-relaxed overflow-x-auto whitespace-pre-wrap"] $ toHtml exceptionData.stackTrace
                whenJust errM $ \err -> do
                  div_ [class_ "surface-raised rounded-2xl overflow-hidden"] do
                    div_ [class_ "px-4 py-3 border-b border-strokeWeak flex items-center justify-between"] do
                      div_ [class_ "flex items-center gap-2"] do
                        faSprite_ "circle-info" "regular" "w-4 h-4 text-iconNeutral"
                        span_ [class_ "text-sm font-medium text-textStrong"] "Error Details"
                    div_ [class_ "p-4 flex flex-col gap-4"] do
                      case (exceptionData.requestMethod, exceptionData.requestPath) of
                        (Just method, Just path) -> do
                          div_ [class_ "mb-2"] do
                            span_ [class_ $ "relative cbadge-sm badge-" <> method <> " whitespace-nowrap"] $ toHtml method
                            span_ [class_ "ml-2 text-sm text-textWeak"] $ toHtml path
                        _ -> pass
                      div_ [class_ "flex items-center gap-4"] do
                        div_ [class_ "flex items-center gap-2"] do
                          faSprite_ "calendar" "regular" "w-3 h-3"
                          div_ [] do
                            span_ [class_ "text-xs text-textWeak"] "First seen:"
                            span_ [class_ "ml-2 text-xs"] $ toHtml $ compactTimeAgo $ toText $ prettyTimeAuto now (zonedTimeToUTC err.createdAt)
                        div_ [class_ "flex items-center gap-2"] do
                          faSprite_ "calendar" "regular" "w-3 h-3"
                          div_ [] do
                            span_ [class_ "text-xs text-textWeak"] "Last seen:"
                            span_ [class_ "ml-2 text-xs"] $ toHtml $ compactTimeAgo $ toText $ prettyTimeAuto now (zonedTimeToUTC err.updatedAt)
                      div_ [class_ "flex items-center gap-4"] do
                        div_ [class_ "flex items-center gap-2"] do
                          faSprite_ "code" "regular" "w-4 h-4"
                          div_ [] do
                            span_ [class_ "text-sm text-textWeak"] "Stack:"
                            span_ [class_ "ml-2 text-sm"] $ toHtml $ fromMaybe "Unknown stack" err.errorData.runtime

                        div_ [class_ "flex items-center gap-2"] do
                          faSprite_ "server" "regular" "w-3 h-3"
                          div_ [] do
                            span_ [class_ "text-sm text-textWeak"] "Service:"
                            span_ [class_ "ml-2 text-sm"] $ toHtml $ fromMaybe "Unknown service" err.errorData.serviceName
                      errorAssigneeSection pid (Just err.id) err.assigneeId members
          _ -> pass
      Issues.QueryAlert -> do
        case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (alertData :: Issues.QueryAlertData) -> do
            div_ [class_ "mb-4"] do
              span_ [class_ "text-sm text-textWeak mb-2 block font-medium"] "Query:"
              div_ [class_ "bg-fillInformation-weak border border-strokeInformation-weak rounded-lg p-3 text-sm font-mono text-fillInformation-strong max-w-2xl overflow-x-auto"]
                $ toHtml alertData.queryExpression
          _ -> pass
      Issues.APIChange ->
        case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (d :: Issues.APIChangeData) -> do
            div_ [class_ "flex items-center gap-3 mb-4 p-3 rounded-lg"] do
              span_ [class_ $ "badge " <> methodFillColor d.endpointMethod] $ toHtml d.endpointMethod
              span_ [class_ "monospace bg-fillWeaker px-2 py-1 rounded text-sm text-textStrong"] $ toHtml d.endpointPath
              div_ [class_ "w-px h-4 bg-strokeWeak"] ""
              span_ [class_ "flex items-center gap-1.5 text-sm text-textWeak"] do
                faSprite_ "server" "regular" "h-3 w-3"
                toHtml d.endpointHost
            -- Stats and chart
            div_ [class_ "grid grid-cols-4 lg:grid-cols-8 gap-4 mb-4"] do
              timeStatBox_ "First Seen" $ prettyTimeAuto now (zonedTimeToUTC issue.createdAt)
              widget "Request trend" $ "attributes.http.request.method==\"" <> d.endpointMethod <> "\" AND attributes.http.route==\"" <> d.endpointPath <> "\" | summarize count(*) by bin_auto(timestamp)"
          _ -> pass
      Issues.LogPattern ->
        case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (d :: Issues.LogPatternData) -> do
            div_ [class_ "grid grid-cols-2 lg:grid-cols-4 gap-4 mb-4"] do
              statBox_ (Just pid) Nothing "Log Level" "" (fromMaybe "Unknown" d.logLevel) Nothing Nothing
              statBox_ (Just pid) Nothing "Service" "" (fromMaybe "Unknown" d.serviceName) Nothing Nothing
              statBox_ (Just pid) Nothing "Occurrences" "" (show d.occurrenceCount) Nothing Nothing
              timeStatBox_ "First Seen" $ prettyTimeAuto now d.firstSeenAt
            div_ [class_ "surface-raised rounded-2xl overflow-hidden mb-4"] do
              div_ [class_ "px-4 py-3 border-b border-strokeWeak"] do
                span_ [class_ "text-sm font-medium text-textStrong"] "Log Pattern"
              div_ [class_ "p-4"] do
                pre_ [class_ "text-sm text-textWeak font-mono whitespace-pre-wrap"] $ toHtml d.logPattern
            whenJust d.sampleMessage $ \msg -> do
              div_ [class_ "surface-raised rounded-2xl overflow-hidden mb-4"] do
                div_ [class_ "px-4 py-3 border-b border-strokeWeak"] do
                  span_ [class_ "text-sm font-medium text-textStrong"] "Sample Message"
                div_ [class_ "p-4"] do
                  pre_ [class_ "text-sm text-textWeak font-mono whitespace-pre-wrap"] $ toHtml msg
          _ -> pass
      Issues.LogPatternRateChange ->
        case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (d :: Issues.LogPatternRateChangeData) -> do
            div_ [class_ "grid grid-cols-2 lg:grid-cols-4 gap-4 mb-4"] do
              statBox_ (Just pid) Nothing "Direction" "" d.changeDirection Nothing Nothing
              statBox_ (Just pid) Nothing "Change" "" (show (round d.changePercent :: Int) <> "%") Nothing Nothing
              statBox_ (Just pid) Nothing "Current Rate" "" (show (round d.currentRatePerHour :: Int) <> "/hr") Nothing Nothing
              statBox_ (Just pid) Nothing "Baseline" "" (show (round d.baselineMean :: Int) <> "/hr") Nothing Nothing
            div_ [class_ "surface-raised rounded-2xl overflow-hidden mb-4"] do
              div_ [class_ "px-4 py-3 border-b border-strokeWeak"] do
                span_ [class_ "text-sm font-medium text-textStrong"] "Log Pattern"
              div_ [class_ "p-4"] do
                pre_ [class_ "text-sm text-textWeak font-mono whitespace-pre-wrap"] $ toHtml d.logPattern
          _ -> pass
    div_ [class_ "surface-raised h-max rounded-2xl overflow-hidden", id_ "error-details-container"] do
      div_ [class_ "px-4 border-b border-b-strokeWeak flex items-center justify-between"] do
        div_ [class_ "flex items-center gap-2"] do
          faSprite_ "magnifying-glass-chart" "regular" "w-4 h-4 text-iconNeutral"
          h4_ [class_ "text-textStrong text-lg font-medium"] "Investigation"
        div_ [class_ "flex items-center"] do
          let aUrl = "/p/" <> pid.toText <> "/anomalies/" <> issueId <> ""
          a_ [href_ $ aUrl <> "?first_occurrence=true", class_ $ (if isFirst then "text-textBrand font-medium" else "text-textWeak hover:text-textStrong") <> " text-xs py-3 px-3 cursor-pointer transition-colors", term "data-tippy-content" "Show first trace the error occured"] "First"
          a_ [href_ aUrl, class_ $ (if isFirst then "text-textWeak hover:text-textStrong" else "text-textBrand font-medium") <> " text-xs py-3 px-3 cursor-pointer transition-colors", term "data-tippy-content" "Show recent trace the error occured"] "Recent"
          span_ [class_ "mx-4 w-px h-4 bg-strokeWeak"] pass
          button_ [class_ "text-xs py-3 px-3 cursor-pointer err-tab t-tab-active font-medium", onclick_ "navigatable(this, '#span-content', '#error-details-container', 't-tab-active', 'err')"] "Trace"
          button_ [class_ "text-xs py-3 px-3 cursor-pointer err-tab font-medium", onclick_ "navigatable(this, '#log-content', '#error-details-container', 't-tab-active', 'err')"] "Logs"
          button_ [class_ "text-xs py-3 px-3 cursor-pointer err-tab font-medium", onclick_ "navigatable(this, '#replay-content', '#error-details-container', 't-tab-active', 'err')"] "Replay"
      div_ [class_ "p-2 w-full overflow-x-hidden"] do
        div_ [class_ "flex w-full err-tab-content", id_ "span-content"] do
          div_ [id_ "trace_container", class_ "grow-1 max-w-[80%] w-1/2 min-w-[20%] shrink-1"] do
            whenJust tr $ \t ->
              tracePage pid t spanRecs
            unless (isJust tr)
              $ div_ [class_ "flex flex-col items-center justify-center h-48"] do
                faSprite_ "inbox-full" "regular" "w-6 h-6 text-textWeak"
                span_ [class_ "mt-2 text-sm text-textWeak"] "No trace data available for this error."
          div_ [class_ "transition-opacity duration-200 mx-1", id_ "resizer-details_width-wrapper"] $ resizer_ "log_details_container" "details_width" False
          div_ [class_ "grow-0 relative shrink-0 overflow-y-auto overflow-x-hidden max-h-[500px] w-1/2 w-c-scroll overflow-x-hidden overflow-y-auto", id_ "log_details_container"] do
            span_ [class_ "htmx-indicator query-indicator absolute loading left-1/2 -translate-x-1/2 loading-dots absoute z-10 top-10", id_ "details_indicator"] ""
            let (spanId, createdAt) = case spanRecs V.!? 0 of
                  Just sr -> (sr.uSpanId, formatUTC sr.timestamp)
                  Nothing -> ("", "")
            let url = "/p/" <> pid.toText <> "/log_explorer/" <> spanId <> "/" <> createdAt <> "/detailed"
            div_ [hxGet_ url, hxTarget_ "#log_details_container", hxSwap_ "innerHtml", hxTrigger_ "intersect one", hxIndicator_ "#details_indicator", term "hx-sync" "this:replace"] pass

        div_ [id_ "log-content", class_ "hidden err-tab-content"] do
          div_ [class_ "flex flex-col gap-4"] do
            virtualTable pid (Just ("/p/" <> pid.toText <> "/log_explorer?json=true&query=" <> toUriStr ("kind==\"log\" AND context___trace_id==\"" <> fromMaybe "" (errM >>= (\x -> x.recentTraceId)) <> "\"")))

        div_ [id_ "replay-content", class_ "hidden err-tab-content"] do
          let withSessionIds = V.catMaybes $ V.map (\sr -> (`lookupValueText` "id") =<< Map.lookup "session" =<< sr.attributes) spanRecs
          unless (V.null withSessionIds) do
            let sessionId = V.head withSessionIds
            div_ [class_ "border border-r border-l w-max mx-auto"]
              $ termRaw "session-replay" [id_ "sessionReplay", term "initialSession" sessionId, class_ "shrink-1 flex flex-col", term "projectId" pid.toText, term "containerId" "sessionPlayerWrapper"] ("" :: Text)
          when (V.null withSessionIds)
            $ div_ [class_ "flex flex-col gap-4"] do
              emptyState_ (Just "video") "No Replay Available" "No session replays associated with this trace" (Just "https://monoscope.tech/docs/sdks/Javascript/browser/") "Session Replay Guide"

    -- AI Chat section (inline with page content)
    anomalyAIChat_ pid issue.id


errorAssigneeSection :: Projects.ProjectId -> Maybe Errors.ErrorId -> Maybe Projects.UserId -> V.Vector ProjectMembers.ProjectMemberVM -> Html ()
errorAssigneeSection pid errIdM assigneeIdM members = do
  let isDisabled = isNothing errIdM || V.null members
  div_ [id_ "error-assignee", class_ "flex flex-col gap-2 border-t border-strokeWeak pt-3"] do
    span_ [class_ "text-xs text-textWeak"] "Assignee"
    case errIdM of
      Nothing ->
        select_ ([class_ "select select-sm w-full", disabled_ "true"] <> [name_ "assigneeId"]) do
          option_ [value_ ""] "Unassigned"
      Just errId -> do
        let actionUrl = "/p/" <> pid.toText <> "/anomalies/errors/" <> UUID.toText errId.unErrorId <> "/assign"
        form_ [hxPost_ actionUrl, hxTarget_ "#error-assignee", hxSwap_ "outerHTML", hxTrigger_ "change"] do
          select_
            ( [class_ "select select-sm w-full", name_ "assigneeId"]
                <> [disabled_ "true" | isDisabled]
            )
            $ do
              option_ ([value_ ""] <> [selected_ "true" | isNothing assigneeIdM]) "Unassigned"
              forM_ members \member -> do
                let memberIdText = UUID.toText $ Projects.getUserId member.userId
                let fullName = T.strip $ member.first_name <> " " <> member.last_name
                let emailText = CI.original member.email
                let label =
                      if T.null fullName
                        then emailText
                        else fullName <> " (" <> emailText <> ")"
                option_
                  ([value_ memberIdText] <> [selected_ "true" | assigneeIdM == Just member.userId])
                  $ toHtml label


errorResolveAction :: Projects.ProjectId -> Errors.ErrorId -> Errors.ErrorState -> Bool -> Html ()
errorResolveAction pid errId errState canResolve =
  when canResolve do
    let actionUrl = "/p/" <> pid.toText <> "/anomalies/errors/" <> UUID.toText errId.unErrorId <> "/resolve"
    div_ [id_ "error-resolve-action"] do
      if errState == Errors.ESResolved
        then button_ [class_ "btn btn-sm btn-ghost text-textWeak", disabled_ "true"] "Resolved"
        else
          button_
            [ class_ "btn btn-sm btn-ghost text-textSuccess hover:bg-fillSuccess-weak"
            , hxPost_ actionUrl
            , hxTarget_ "#error-resolve-action"
            , hxSwap_ "outerHTML"
            ]
            "Resolve"


errorSubscriptionAction :: (HasField "id" err Errors.ErrorId, HasField "notifyEveryMinutes" err Int, HasField "subscribed" err Bool) => Projects.ProjectId -> err -> Html ()
errorSubscriptionAction pid err = do
  let isActive = err.subscribed
  let notifyEvery = err.notifyEveryMinutes
  let actionUrl = "/p/" <> pid.toText <> "/anomalies/errors/" <> UUID.toText err.id.unErrorId <> "/subscribe"
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
        "Subscribe"
      select_ [class_ "select select-sm w-32", name_ "notifyEveryMinutes"] do
        option_ ([value_ "0"] <> [selected_ "true" | not isActive]) "Off"
        let opts :: [(Int, Text)]
            opts = [(10, "10 mins"), (20, "20 mins"), (30, "30 mins"), (60, "1 hour"), (360, "6 hours"), (1440, "24 hours")]
        forM_ opts \(val, label) ->
          option_ ([value_ (show val)] <> [selected_ "true" | isActive && val == notifyEvery]) (toHtml label)


newtype AssignErrorForm = AssignErrorForm
  { assigneeId :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


data ErrorSubscriptionForm = ErrorSubscriptionForm
  { notifyEveryMinutes :: Maybe Int
  , action :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


assignErrorPostH :: Projects.ProjectId -> UUID.UUID -> AssignErrorForm -> ATAuthCtx (RespHeaders (Html ()))
assignErrorPostH pid errUuid form = do
  (_sess, _project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  let errId = Errors.ErrorId errUuid
      assigneeIdM = form.assigneeId >>= UUID.fromText <&> Projects.UserId
  members <- V.fromList <$> ProjectMembers.selectActiveProjectMembers pid
  errM <- Errors.getErrorById errId
  let render eidM aidM = addRespHeaders $ errorAssigneeSection pid eidM aidM members
      isMember = maybe True (\uid -> any (\m -> m.userId == uid) members) assigneeIdM
  case errM of
    Nothing -> addErrorToast "Error not found" Nothing >> render Nothing Nothing
    Just err
      | err.projectId /= pid -> addErrorToast "Error not found for this project" Nothing >> render (Just err.id) err.assigneeId
      | not isMember -> addErrorToast "Assignee must be an active project member" Nothing >> render (Just err.id) err.assigneeId
      | assigneeIdM == err.assigneeId -> addSuccessToast "Assignee unchanged" Nothing >> render (Just err.id) err.assigneeId
      | otherwise -> do
          _ <- Errors.setErrorAssignee err.id assigneeIdM
          whenJust assigneeIdM \assigneeId ->
            void $ liftIO $ withResource appCtx.pool \conn ->
              createJob conn "background_jobs" $ BackgroundJobs.ErrorAssigned pid err.id assigneeId
          addSuccessToast "Assignee updated" Nothing
          errM' <- Errors.getErrorById err.id
          render (errM' <&> (.id)) (errM' >>= (.assigneeId))


resolveErrorPostH :: Projects.ProjectId -> UUID.UUID -> ATAuthCtx (RespHeaders (Html ()))
resolveErrorPostH pid errUuid = do
  (sess, _project) <- Sessions.sessionAndProject pid
  errM <- Errors.getErrorById (Errors.ErrorId errUuid)
  userPermission <- ProjectMembers.getUserPermission pid sess.user.id
  let canResolve err = userPermission == Just ProjectMembers.PAdmin || err.assigneeId == Just sess.user.id
  case errM of
    Nothing -> addErrorToast "Error not found" Nothing >> addRespHeaders mempty
    Just err
      | err.projectId /= pid -> addErrorToast "Error not found for this project" Nothing >> addRespHeaders mempty
      | not (canResolve err) -> do
          addErrorToast "You do not have permission to resolve this error" Nothing
          addRespHeaders $ errorResolveAction pid err.id err.state False
      | otherwise -> do
          when (err.state /= Errors.ESResolved) $ void $ Errors.resolveError err.id
          addSuccessToast "Error resolved" Nothing
          addRespHeaders $ errorResolveAction pid err.id Errors.ESResolved True


errorSubscriptionPostH :: Projects.ProjectId -> UUID.UUID -> ErrorSubscriptionForm -> ATAuthCtx (RespHeaders (Html ()))
errorSubscriptionPostH pid errUuid form = do
  (_sess, _project) <- Sessions.sessionAndProject pid
  let errId = Errors.ErrorId errUuid
  errM <- Errors.getErrorById errId
  case errM of
    Nothing -> addErrorToast "Error not found" Nothing >> addRespHeaders mempty
    Just err
      | err.projectId /= pid -> addErrorToast "Error not found for this project" Nothing >> addRespHeaders mempty
      | otherwise -> do
          let notifyEveryRaw = fromMaybe 0 form.notifyEveryMinutes
          let notifyEvery = max 1 $ min 1440 $ if notifyEveryRaw == 0 then 30 else notifyEveryRaw
          let action' = fromMaybe "" form.action
          let shouldSubscribe =
                case action' of
                  "unsubscribe" -> False
                  "subscribe" -> True
                  _ -> notifyEveryRaw > 0
          if shouldSubscribe
            then do
              _ <- Errors.updateErrorSubscription err.id True notifyEvery
              addSuccessToast "Subscribed to error" Nothing
              errM' <- Errors.getErrorById err.id
              maybe (addRespHeaders mempty) (addRespHeaders . errorSubscriptionAction pid) errM'
            else do
              _ <- Errors.updateErrorSubscription err.id False notifyEvery
              addSuccessToast "Unsubscribed from error" Nothing
              errM' <- Errors.getErrorById err.id
              maybe (addRespHeaders mempty) (addRespHeaders . errorSubscriptionAction pid) errM'


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
      result <- AI.runAgenticChatWithHistory config form.query appCtx.config.openaiApiKey
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
  (sess, project) <- Sessions.sessionAndProject pid
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
  errorM <- bool (pure Nothing) (Anomalies.errorByHash pid issue.endpointHash) (issue.issueType == Issues.RuntimeException)
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
          , ("- **Service**: " <>) <$> iss.service
          , Just $ "- **Recommended Action**: " <> iss.recommendedAction
          , alertContextM <&> \(alertData, monitorM, metricsData) -> formatCompleteAlertContext alertData monitorM metricsData
          , errM >>= \err ->
              Just
                $ unlines
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
  div_ [class_ "surface-raised rounded-2xl p-6 animate-fade-in max-w-3xl mx-auto w-full"] do
    -- User question
    div_ [class_ "flex items-start gap-3 mb-4 pb-4 border-b border-strokeWeak"] do
      iconBadgeSq_ NeutralBadge "user"
      p_ [class_ "text-sm text-textStrong"] $ toHtml userQuery
    whenJust systemPromptM
      $ div_ [class_ "mb-4"]
      . panel_ def{icon = Just "terminal", collapsible = Just False} "System Prompt"
      . div_ [class_ "text-xs font-mono whitespace-pre-wrap text-textWeak max-h-96 overflow-y-auto"]
      . toHtml
    -- Behind the scenes section (tool calls)
    whenJust toolCallsM \toolCalls ->
      unless (null toolCalls)
        $ details_ [class_ "mb-4 border border-strokeWeak rounded-lg"] do
          summary_ [class_ "cursor-pointer px-4 py-2 text-sm text-textWeak hover:bg-fillWeaker list-none flex items-center gap-2"] do
            faSprite_ "magnifying-glass-chart" "regular" "w-4 h-4"
            toHtml $ "Behind the scenes: " <> show (length toolCalls) <> " tool calls"
          div_ [class_ "px-4 py-3 border-t border-strokeWeak bg-fillWeaker/50"] $ forM_ toolCalls toolCallView_
    -- AI response
    div_ [class_ "flex items-start gap-3"] do
      iconBadgeSq_ BrandBadge "sparkles"
      div_ [class_ "flex-1 flex flex-col gap-4"] do
        div_ [class_ "prose prose-sm text-textStrong max-w-none"] $ renderMarkdown explanation
    whenJust widgetsM \widgets -> do
      let processedWidgets = maybe widgets (`processWidgetsWithToolData` widgets) toolCallsM
      div_ [class_ "grid grid-cols-1 gap-4 mt-4"] $ forM_ processedWidgets \widget ->
        div_ [class_ "w-full aspect-[3/1]"] $ Widget.widget_ widget{Widget._projectId = Just pid}


-- | Render a single tool call
toolCallView_ :: AI.ToolCallInfo -> Html ()
toolCallView_ tc =
  div_ [class_ "flex flex-col gap-1 py-2 border-b border-strokeWeak last:border-0"] do
    div_ [class_ "flex items-center gap-2 flex-wrap"] do
      span_ [class_ "font-mono text-xs px-2 py-0.5 bg-fillWeak rounded"] $ toHtml tc.name
      whenJust (Map.lookup "query" tc.args) $ span_ [class_ "text-xs text-textWeak break-all"] . toHtml . show
    unless (T.null tc.resultPreview) $ div_ [class_ "text-xs text-textWeak font-mono pl-4 whitespace-pre-wrap break-all"] $ toHtml $ "→ " <> tc.resultPreview


renderMarkdown :: Text -> Html ()
renderMarkdown md = case MMark.parse "" md of
  Left _ -> toHtml md
  Right doc -> toHtmlRaw $ MMark.render doc


parseStoredJSON :: AE.FromJSON a => Maybe (Aeson AE.Value) -> Maybe a
parseStoredJSON = (>>= parseMaybe AE.parseJSON . getAeson)


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
  div_ [class_ "surface-raised rounded-2xl p-6 max-w-3xl mx-auto w-full mb-4"] do
    panel_ def{icon = Just "file-text", collapsible = Just False} "System Prompt"
      $ div_ [class_ "text-xs font-mono whitespace-pre-wrap text-textWeak max-h-96 overflow-y-auto"]
      $ toHtml systemPrompt
  -- Render chat history
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


-- | AI Chat Component with inline responses and floating input
anomalyAIChat_ :: Projects.ProjectId -> Issues.IssueId -> Html ()
anomalyAIChat_ pid issueId = do
  let issueIdT = UUID.toText issueId.unUUIDId
  -- Response container (inline with page content)
  div_
    [ id_ "ai-response-container"
    , class_ "flex flex-col gap-4"
    , hxGet_ $ "/p/" <> pid.toText <> "/anomalies/" <> issueIdT <> "/ai_chat/history"
    , hxTrigger_ "load"
    , term "hx-on::after-swap" "window.evalScriptsFromContent && window.evalScriptsFromContent(event.detail.elt === this ? this : this.lastElementChild); this.lastElementChild?.scrollIntoView({behavior: 'smooth', block: 'start'})"
    ]
    ""
  -- Floating input bar
  div_ [class_ "fixed bottom-6 left-1/2 -translate-x-1/2 w-full max-w-2xl px-4", style_ "z-index: 9999;"] do
    div_ [class_ "!bg-bgRaised/95 !surface-raised rounded-2xl p-4 shadow-2xl shadow-fillBrand-strong/20 border border-strokeBrand-weak ring-1 ring-fillBrand-weak/30"] do
      form_
        [ hxPost_ $ "/p/" <> pid.toText <> "/anomalies/" <> issueIdT <> "/ai_chat"
        , hxTarget_ "#ai-response-container"
        , hxSwap_ "beforeend"
        , hxIndicator_ "#ai-chat-loader"
        , term "hx-on::after-request" "this.reset()"
        ]
        do
          div_ [class_ "flex items-center gap-3"] do
            iconBadgeSq_ BrandBadge "sparkles"
            input_
              [ class_ "flex-1 bg-transparent border-none outline-none text-textStrong placeholder-textWeak text-sm"
              , placeholder_ "Ask about this issue... e.g., 'What could cause this error?'"
              , name_ "query"
              , id_ "ai-chat-input"
              , autocomplete_ "off"
              ]
            span_ [class_ "htmx-indicator", id_ "ai-chat-loader"] $ faSprite_ "spinner" "regular" "w-4 h-4 animate-spin text-iconBrand"
            button_ [type_ "submit", class_ "p-2 rounded-lg bg-fillBrand-strong text-white hover:opacity-90 transition-opacity"] $ faSprite_ "arrow-right" "regular" "w-4 h-4"
          -- Suggested prompts
          div_ [class_ "flex gap-2 mt-3 pt-3 border-t border-strokeWeak flex-wrap"] do
            suggestionBtn_ "What could cause this?"
            suggestionBtn_ "Show related logs"
            suggestionBtn_ "Suggest a fix"
  where
    suggestionBtn_ txt =
      button_
        [ type_ "button"
        , class_ "text-xs px-2 py-1 rounded-lg bg-fillWeaker border border-strokeWeak text-textWeak hover:text-textStrong hover:border-strokeBrand-weak transition-colors cursor-pointer"
        , onclick_ $ "document.getElementById('ai-chat-input').value = '" <> txt <> "'; document.getElementById('ai-chat-input').form.requestSubmit();"
        ]
        $ toHtml txt


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
  -> Maybe Text
  -> ATAuthCtx (RespHeaders AnomalyListGet)
anomalyListGetH pid layoutM filterTM sortM timeFilter pageM perPageM loadM endpointM hxRequestM hxBoostedM = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  let (ackd, archived, currentFilterTab) = case filterTM of
        Just "Inbox" -> (Just False, Just False, "Inbox")
        Just "Acknowleged" -> (Just True, Nothing, "Acknowleged")
        Just "Archived" -> (Nothing, Just True, "Archived")
        _ -> (Just False, Just False, "Inbox")

  let filterV = fromMaybe "14d" timeFilter
      pageInt = maybe 0 (Unsafe.read . toString) pageM
      perPage = maybe 25 (Unsafe.read . toString) perPageM
      currentSort = fromMaybe "-created_at" sortM

  freeTierExceeded <- checkFreeTierExceeded pid project.paymentPlan
  currTime <- liftIO getCurrentTime

  (issues, totalCount) <- Issues.selectIssues pid Nothing ackd archived perPage (pageInt * perPage) Nothing (Just currentSort)

  let baseUrl = "/p/" <> pid.toText <> "/anomalies?filter=" <> currentFilterTab <> "&sort=" <> currentSort
      paginationConfig =
        Pagination
          { currentPage = pageInt
          , perPage = perPage
          , totalCount = totalCount
          , baseUrl = baseUrl
          , targetId = "anomalyListContainer"
          }
  let issuesVM = V.fromList $ map (IssueVM False False currTime filterV) issues
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
          , filterMenus = []
          , activeFilters = []
          }
  let issuesTable =
        Table
          { config = def{elemID = "anomalyListForm", containerId = Just "anomalyListContainer", addPadding = True, renderAsTable = True, bulkActionsInHeader = Just 0}
          , columns = issueColumns pid
          , rows = issuesVM
          , features =
              def
                { rowId = Just \(IssueVM _ _ _ _ issue) -> Issues.issueIdText issue.id
                , rowAttrs = Just $ const [class_ "group/row hover:bg-fillWeaker"]
                , bulkActions =
                    [ BulkAction{icon = Just "check", title = "Acknowledge", uri = "/p/" <> pid.toText <> "/anomalies/bulk_actions/acknowledge"}
                    , BulkAction{icon = Just "inbox-full", title = "Archive", uri = "/p/" <> pid.toText <> "/anomalies/bulk_actions/archive"}
                    ]
                , search = Just ClientSide
                , tableHeaderActions = Just tableActions
                , pagination = if totalCount > 0 then Just paginationConfig else Nothing
                , zeroState =
                    Just
                      $ ZeroState
                        { icon = "empty-set"
                        , title = "No Issues Or Errors."
                        , description = "Start monitoring errors that happened during a request."
                        , actionText = "Error reporting guide"
                        , destination = Right "https://monoscope.tech/docs/sdks/nodejs/expressjs/#reporting-errors-to-monoscope"
                        }
                }
          }
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Issues: Changes, Alerts & Errors"
          , menuItem = Just "Changes & Errors"
          , freeTierExceeded = freeTierExceeded
          , config = appCtx.config
          , navTabs =
              Just
                $ toHtml
                $ TabFilter
                  { current = currentFilterTab
                  , currentURL = baseUrl
                  , clientSide = False
                  , options =
                      [ TabFilterOpt "Inbox" Nothing Nothing
                      , TabFilterOpt "Acknowleged" Nothing Nothing
                      , TabFilterOpt "Archived" Nothing Nothing
                      ]
                  }
          }
  addRespHeaders $ case (layoutM, hxRequestM, hxBoostedM, loadM) of
    (_, _, _, Just "true") -> ALRows $ TableRows{columns = issueColumns pid, rows = issuesVM, emptyState = Nothing, renderAsTable = True, rowId = Just \(IssueVM _ _ _ _ issue) -> Issues.issueIdText issue.id, rowAttrs = Just $ const [class_ "group/row hover:bg-fillWeaker"], pagination = if totalCount > 0 then Just paginationConfig else Nothing}
    _ -> ALPage $ PageCtx bwconf issuesTable


data AnomalyListGet
  = ALPage (PageCtx (Table IssueVM))
  | ALRows (TableRows IssueVM)


instance ToHtml AnomalyListGet where
  toHtml (ALPage pg) = toHtml pg
  toHtml (ALRows rows) = toHtml rows
  toHtmlRaw = toHtml


anomalyAccentColor :: Bool -> Bool -> Text
anomalyAccentColor _ True = "bg-fillStrong"
anomalyAccentColor True False = "bg-fillSuccess-weak"
anomalyAccentColor False False = "bg-fillError-strong"


data IssueVM = IssueVM Bool Bool UTCTime Text Issues.IssueL
  deriving stock (Show)


issueColumns :: Projects.ProjectId -> [Column IssueVM]
issueColumns pid =
  [ col "Issue" (renderIssueMainCol pid) & withAttrs [class_ "py-4"]
  , col "Events" renderIssueEventsCol & withAttrs [class_ "w-36 text-center align-top pt-4"]
  , col "Activity" renderIssueChartCol & withAttrs [class_ "w-60 align-top pt-4"]
  ]


renderIssueEventsCol :: IssueVM -> Html ()
renderIssueEventsCol (IssueVM _ isWidget _ _ issue) =
  unless isWidget
    $ span_ [class_ "tabular-nums text-xl", term "data-tippy-content" "Events for this Issue in the last 14days"]
    $ show issue.eventCount


renderIssueChartCol :: IssueVM -> Html ()
renderIssueChartCol (IssueVM _ _ _ _ issue) =
  div_ [class_ "w-56 h-12 px-3"]
    $ Widget.widget_
    $ (def :: Widget.Widget)
      { Widget.standalone = Just True
      , Widget.id = Just $ Issues.issueIdText issue.id
      , Widget.wType = Widget.WTTimeseries
      , Widget.title = Just $ Issues.issueIdText issue.id
      , Widget.showTooltip = Just False
      , Widget.naked = Just True
      , Widget.xAxis = Just (def{Widget.showAxisLabel = Just False})
      , Widget.yAxis = Just (def{Widget.showOnlyMaxLabel = Just True})
      , Widget.query = Just "status_code == \"ERROR\" | summarize count(*) by bin(timestamp, 1h)"
      , Widget._projectId = Just issue.projectId
      , Widget.hideLegend = Just True
      }


renderIssueMainCol :: Projects.ProjectId -> IssueVM -> Html ()
renderIssueMainCol pid (IssueVM hideByDefault isWidget currTime timeFilter issue) = do
  let timeSinceString = prettyTimeAuto currTime $ zonedTimeToUTC issue.createdAt
      statusColor = anomalyAccentColor (isJust issue.acknowledgedAt) (isJust issue.archivedAt)

  -- Title and badges row with status indicator
  div_ [class_ $ "flex gap-3 mb-3 flex-wrap " <> if isWidget then "flex-col" else " items-center "] do
    -- Status indicator dot
    unless isWidget $ div_ [class_ $ statusColor <> " w-2 h-2 rounded-full shrink-0 mt-1.5"] ""
    a_ [class_ "text-textStrong text-base", href_ $ "/p/" <> pid.toText <> "/anomalies/" <> Issues.issueIdText issue.id] $ toHtml issue.title
    -- Issue type badge
    div_ [class_ "flex items-center gap-2"] do
      issueTypeBadge issue.issueType issue.critical
      -- Severity badge
      case issue.severity of
        "critical" -> span_ [class_ "inline-flex items-center justify-center rounded-md px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 bg-fillError-weak text-fillError-strong border-2 border-strokeError-strong shadow-sm"] "CRITICAL"
        "warning" -> span_ [class_ "inline-flex items-center justify-center rounded-md px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 bg-fillWarning-weak text-fillWarning-strong border border-strokeWarning-weak shadow-sm"] "WARNING"
        _ -> pass

  -- Metadata row (method, endpoint, service, time)
  div_ [class_ "flex items-center gap-4 text-sm text-textWeak mb-3 flex-wrap"] do
    -- Method and endpoint (for API changes)
    when (issue.issueType == Issues.APIChange) do
      case AE.fromJSON (getAeson issue.issueData) of
        AE.Success (apiData :: Issues.APIChangeData) -> do
          div_ [class_ "flex items-center gap-2"] do
            span_ [class_ $ "badge " <> methodFillColor apiData.endpointMethod] $ toHtml apiData.endpointMethod
            span_ [class_ "monospace bg-fillWeak px-2 py-1 rounded text-xs text-textStrong"] $ toHtml apiData.endpointPath
        _ -> pass
    -- Service badge
    span_ [class_ "flex items-center gap-1"] do
      div_ [class_ "w-3 h-3 bg-fillYellow rounded-sm"] ""
      span_ [class_ "text-textStrong"] $ toHtml $ fromMaybe "unknown-service" issue.service
    -- Time since
    span_ [class_ "text-textWeak"] $ toHtml timeSinceString

  -- Statistics row (only for API changes)
  when (issue.issueType == Issues.APIChange) do
    let allChanges = getAeson issue.requestPayloads <> getAeson issue.responsePayloads :: [Anomalies.PayloadChange]
        countChange (!b, !i, !t) c = case c.changeType of
          Anomalies.Breaking -> (b + 1, i, t + 1)
          Anomalies.Incremental -> (b, i + 1, t + 1)
          _ -> (b, i, t + 1)
        (breakingChanges, incrementalChanges, totalChanges) = foldl' countChange (0, 0, 0) allChanges
    div_ [class_ "flex items-center gap-4 text-sm mb-4 p-3 bg-fillWeak rounded-lg"] do
      span_ [class_ "text-textWeak"] do
        strong_ [class_ "text-textStrong"] $ toHtml $ show totalChanges
        " total changes"
      div_ [class_ "w-px h-4 bg-strokeWeak"] ""
      span_ [class_ "text-textWeak"] do
        strong_ [class_ "text-fillError-strong"] $ toHtml $ show breakingChanges
        " breaking"
        when (breakingChanges > 0 && totalChanges > 0) $ span_ [class_ "text-xs tabular-nums ml-1 bg-fillError-weak text-fillError-strong px-1.5 py-0.5 rounded"] $ toHtml $ show (round (fromIntegral breakingChanges / fromIntegral totalChanges * 100 :: Float) :: Int) <> "%"
      div_ [class_ "w-px h-4 bg-strokeWeak"] ""
      span_ [class_ "text-textWeak"] do
        strong_ [class_ "text-fillSuccess-strong"] $ toHtml $ show incrementalChanges
        " incremental"
      div_ [class_ "w-px h-4 bg-strokeWeak"] ""
      span_ [class_ "text-textWeak"] do
        strong_ [class_ "text-textBrand"] $ toHtml $ show totalChanges
        " payloads affected"

  -- Stack trace for runtime exceptions or Query for alerts
  case issue.issueType of
    Issues.RuntimeException -> case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (exceptionData :: Issues.RuntimeExceptionData) ->
        div_ [class_ "border border-strokeError-weak rounded-lg group/er mb-4"] do
          label_ [class_ "text-sm text-textWeak font semibold rounded-lg p-2 flex gap-2 items-center"] do
            faSprite_ "chevron-right" "regular" "h-3 w-3 group-has-[.err-input:checked]/er:rotate-90"
            "Stack trace"
            input_ [class_ "err-input w-0 h-0 opacity-0", type_ "checkbox"]
          div_ [class_ "bg-fillError-weak p-4 overflow-x-scroll hidden group-has-[.err-input:checked]/er:block text-sm monospace text-fillError-strong"] $ pre_ [class_ "whitespace-pre-wrap "] $ toHtml exceptionData.stackTrace
      _ -> pass
    Issues.QueryAlert -> case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (alertData :: Issues.QueryAlertData) ->
        div_ [class_ "mb-4"] do
          span_ [class_ "text-sm text-textWeak mb-2 block font-medium"] "Query:"
          div_ [class_ "bg-fillInformation-weak border border-strokeInformation-weak rounded-lg p-3 text-sm monospace text-fillInformation-strong max-w-2xl overflow-x-auto"] $ toHtml alertData.queryExpression
      _ -> pass
    Issues.LogPattern -> case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.LogPatternData) ->
        div_ [class_ "border border-strokeWeak rounded-lg mb-4"] do
          label_ [class_ "text-sm text-textWeak font-semibold rounded-lg p-2 flex gap-2 items-center cursor-pointer"] do
            toHtml $ fromMaybe "LOG" d.logLevel <> " pattern (" <> show d.occurrenceCount <> " occurrences)"
          div_ [class_ "bg-fillWeak p-4 overflow-x-scroll group-has-[.lp-input:checked]/lp:block text-sm monospace text-textStrong"] $ pre_ [class_ "whitespace-pre-wrap"] $ toHtml d.logPattern
      _ -> pass
    Issues.LogPatternRateChange -> case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.LogPatternRateChangeData) ->
        div_ [class_ "border border-strokeWeak rounded-lg group/lpr mb-4"] do
          label_ [class_ "text-sm text-textWeak font-semibold rounded-lg p-2 flex gap-2 items-center cursor-pointer"] do
            faSprite_ "chevron-right" "regular" "h-3 w-3 group-has-[.lpr-input:checked]/lpr:rotate-90"
            toHtml $ "Rate " <> d.changeDirection <> " (" <> show (round d.changePercent :: Int) <> "%)"
            input_ [class_ "lpr-input w-0 h-0 opacity-0", type_ "checkbox"]
          div_ [class_ "bg-fillWeak p-4 overflow-x-scroll hidden group-has-[.lpr-input:checked]/lpr:block text-sm monospace text-textStrong"] $ pre_ [class_ "whitespace-pre-wrap"] $ toHtml d.logPattern
      _ -> pass
    _ -> pass

  -- Recommended action
  div_ [class_ "border-l-4 border-strokeBrand pl-4 mb-4"] $ p_ [class_ "text-sm text-textStrong leading-relaxed"] $ toHtml issue.recommendedAction

  -- Collapsible payload changes (only for API changes)
  when (issue.issueType == Issues.APIChange) $ details_ [class_ "group mb-4"] do
    summary_ [class_ "inline-flex items-center cursor-pointer whitespace-nowrap text-sm font-medium transition-all rounded-md gap-1.5 text-textBrand hover:text-textBrand/80 list-none"] do
      faSprite_ "chevron-right" "regular" "h-4 w-4 mr-1 transition-transform group-open:rotate-90"
      "View detailed payload changes"
    div_ [class_ "mt-4 border border-strokeWeak rounded-lg overflow-hidden bg-bgRaised"] $ renderPayloadChanges issue

  -- Action buttons
  div_ [class_ "flex items-center gap-3 mt-4 pt-4 border-t border-strokeWeak"] do
    button_ [class_ "inline-flex items-center justify-center whitespace-nowrap text-sm font-medium transition-all h-8 rounded-md gap-1.5 px-3 text-textBrand hover:text-textBrand/80 hover:bg-fillBrand-weak"] do
      faSprite_ "eye" "regular" "w-4 h-4"
      span_ [class_ "leading-none"] "view related logs"
    button_ [class_ "inline-flex items-center justify-center whitespace-nowrap text-sm font-medium transition-all h-8 rounded-md gap-1.5 px-3 border bg-background hover:text-accent-foreground text-textBrand border-strokeBrand-strong hover:bg-fillBrand-weak"] do
      faSprite_ "code" "regular" "w-4 h-4"
      span_ [class_ "leading-none"] "View Full Schema"
    -- Acknowledge button
    let isAcknowledged = isJust issue.acknowledgedAt
    let acknowledgeEndpoint = "/p/" <> issue.projectId.toText <> "/anomalies/" <> Issues.issueIdText issue.id <> if isAcknowledged then "/unacknowledge" else "/acknowledge"
    button_
      [ class_ $ "inline-flex items-center justify-center whitespace-nowrap text-sm font-medium transition-all h-8 rounded-md gap-1.5 px-3 " <> if isAcknowledged then "bg-fillSuccess-weak text-fillSuccess-strong border border-strokeSuccess-weak hover:bg-fillSuccess-weak/80" else "bg-fillPrimary text-textInverse-strong hover:bg-fillPrimary/90"
      , hxGet_ acknowledgeEndpoint
      , hxSwap_ "outerHTML"
      , hxTarget_ "closest .itemsListItem"
      ]
      do
        faSprite_ "check" "regular" "w-4 h-4"
        span_ [class_ "leading-none"] $ if isAcknowledged then "Acknowledged" else "Acknowledge"
    -- Archive button
    let isArchived = isJust issue.archivedAt
    let archiveEndpoint = "/p/" <> issue.projectId.toText <> "/anomalies/" <> Issues.issueIdText issue.id <> if isArchived then "/unarchive" else "/archive"
    button_
      [ class_ $ "inline-flex items-center justify-center whitespace-nowrap text-sm font-medium transition-all h-8 rounded-md gap-1.5 px-3 " <> if isArchived then "bg-fillWarning-weak text-fillWarning-strong border border-strokeWarning-weak hover:bg-fillWarning-weak/80" else "border border-strokeWeak text-textStrong hover:bg-fillWeak"
      , hxGet_ archiveEndpoint
      , hxSwap_ "outerHTML"
      , hxTarget_ "closest .itemsListItem"
      ]
      do
        faSprite_ "archive" "regular" "w-4 h-4"
        span_ [class_ "leading-none"] $ if isArchived then "Unarchive" else "Archive"


-- Render payload changes section
renderPayloadChanges :: Issues.IssueL -> Html ()
renderPayloadChanges issue =
  when (issue.issueType == Issues.APIChange) do
    let requestChanges = getAeson issue.requestPayloads :: [Anomalies.PayloadChange]
    let responseChanges = getAeson issue.responsePayloads :: [Anomalies.PayloadChange]

    when (not (null requestChanges) || not (null responseChanges)) do
      div_ [class_ "border border-strokeWeak rounded-lg overflow-hidden bg-bgRaised group/payloadtabs"] do
        div_ [class_ "flex flex-col gap-2"] do
          -- Tab navigation using radio buttons
          div_ [role_ "tablist", Aria.orientation_ "horizontal", class_ "text-muted-foreground h-9 items-center justify-center rounded-xl p-[3px] w-full grid grid-cols-2 bg-fillWeak"] do
            -- Response tab (default active)
            label_
              [ role_ "tab"
              , class_ "h-[calc(100%-1px)] flex-1 justify-center rounded-xl border border-transparent px-2 py-1 text-sm font-medium whitespace-nowrap transition-all flex items-center gap-2 cursor-pointer has-[:checked]:bg-bgRaised has-[:checked]:text-textStrong bg-transparent text-textWeak"
              ]
              do
                input_ [type_ "radio", name_ ("payload-tab-" <> Issues.issueIdText issue.id), class_ "hidden payload-tab-response", checked_]
                faSprite_ "arrow-right" "regular" "w-4 h-4"
                span_ [] $ "Response Payloads (" <> show (length responseChanges) <> ")"

            -- Request tab
            label_
              [ role_ "tab"
              , class_ "h-[calc(100%-1px)] flex-1 justify-center rounded-xl border border-transparent px-2 py-1 text-sm font-medium whitespace-nowrap transition-all flex items-center gap-2 cursor-pointer has-[:checked]:bg-bgRaised has-[:checked]:text-textStrong bg-transparent text-textWeak"
              ]
              do
                input_ [type_ "radio", name_ ("payload-tab-" <> Issues.issueIdText issue.id), class_ "hidden payload-tab-request"]
                faSprite_ "arrow-right" "regular" "w-4 h-4 rotate-180"
                span_ [] $ "Request Payloads (" <> show (length requestChanges) <> ")"

          -- Tab panels
          -- Response panel (visible when response tab is selected)
          div_
            [ role_ "tabpanel"
            , class_ "flex-1 outline-none p-4 space-y-4 hidden group-has-[.payload-tab-response:checked]/payloadtabs:block"
            ]
            $ if null responseChanges
              then div_ [class_ "text-center py-8 text-textWeak"] "No response payload changes"
              else forM_ responseChanges (renderPayloadChange True)

          -- Request panel (visible when request tab is selected)
          div_
            [ role_ "tabpanel"
            , class_ "flex-1 outline-none p-4 space-y-4 hidden group-has-[.payload-tab-request:checked]/payloadtabs:block"
            ]
            $ if null requestChanges
              then div_ [class_ "text-center py-8 text-textWeak"] "No request payload changes"
              else forM_ requestChanges (renderPayloadChange False)


-- Render individual payload change
renderPayloadChange :: Bool -> Anomalies.PayloadChange -> Html ()
renderPayloadChange isResponse change =
  div_ [class_ "border border-strokeWeak rounded-lg p-4 bg-fillWeak"] do
    -- Status code/method badges and info
    div_ [class_ "flex items-center gap-3 mb-3 flex-wrap"] do
      -- Status code or method badge
      case (change.statusCode, change.method) of
        (Just statusCode, _) ->
          span_ [class_ $ "badge " <> statusFillColor statusCode] $ toHtml $ show statusCode <> " " <> fromMaybe "" change.statusText
        (_, Just method) ->
          span_ [class_ "badge bg-fillInformation-strong"] $ toHtml method
        _ -> pass

      -- Content type badge
      span_ [class_ "badge-outline border-strokeWeak text-textWeak bg-bgRaised"] $ toHtml change.contentType

      -- Change type badge
      case change.changeType of
        Anomalies.Breaking ->
          span_ [class_ "badge bg-fillError-strong"] do
            faSprite_ "circle-x" "regular" "w-3 h-3 mr-1"
            "Breaking"
        Anomalies.Incremental ->
          span_ [class_ "badge bg-fillInformation-strong"] do
            faSprite_ "info" "regular" "w-3 h-3 mr-1"
            "Incremental"
        Anomalies.Safe ->
          span_ [class_ "badge bg-fillSuccess-strong"] do
            faSprite_ "circle-check" "regular" "w-3 h-3 mr-1"
            "Safe"

    -- Description
    when (T.length change.description > 0) do
      p_ [class_ "text-sm text-textWeak mb-4 leading-relaxed"] $ toHtml change.description

    -- Field changes section
    unless (null change.changes) do
      div_ [class_ "space-y-3"] do
        -- Section header
        div_ [class_ "flex items-center gap-2 pb-2 border-b border-strokeWeak"] do
          faSprite_ "code" "regular" "w-4 h-4 text-iconNeutral"
          span_ [class_ "font-medium text-textStrong"]
            $ case (change.statusCode, change.statusText) of
              (Just code, Just txt) -> toHtml $ show code <> " Response Changes"
              _ -> "Payload Changes"
          span_ [class_ "badge-outline border-strokeWeak text-textWeak bg-fillWeak"] $ toHtml change.contentType

        -- Individual field changes
        div_ [class_ "space-y-3"] do
          forM_ change.changes renderFieldChange

    -- Examples section
    when (T.length change.exampleBefore > 0 || T.length change.exampleAfter > 0) do
      div_ [class_ "mt-4 space-y-3"] do
        span_ [class_ "text-sm font-medium text-textStrong"] "Example Payloads:"
        div_ [class_ "grid grid-cols-2 gap-4"] do
          when (T.length change.exampleBefore > 0) do
            div_ [] do
              span_ [class_ "text-xs text-textWeak block mb-1 font-medium"] "Before:"
              pre_ [class_ "bg-fillError-weak text-fillError-strong p-3 rounded text-xs overflow-x-auto border border-strokeError-weak"] do
                toHtml change.exampleBefore
          when (T.length change.exampleAfter > 0) do
            div_ [] do
              span_ [class_ "text-xs text-textWeak block mb-1 font-medium"] "After:"
              pre_ [class_ "bg-fillSuccess-weak text-fillSuccess-strong p-3 rounded text-xs overflow-x-auto border border-strokeSuccess-weak"] do
                toHtml change.exampleAfter


-- Render individual field change
renderFieldChange :: Anomalies.FieldChange -> Html ()
renderFieldChange fieldChange =
  div_ [class_ "border border-strokeWeak rounded-lg p-4 bg-bgRaised"] do
    -- Field name and change kind badges
    div_ [class_ "flex items-start justify-between gap-4 mb-3"] do
      div_ [class_ "flex items-center gap-2 flex-wrap"] do
        -- Field path in monospace
        span_ [class_ "monospace text-sm bg-fillWeak px-2 py-1 rounded text-textStrong"] $ toHtml fieldChange.path

        -- Change kind badge
        let kindText = case fieldChange.changeKind of
              Anomalies.Modified -> "modified"
              Anomalies.Added -> "added"
              Anomalies.Removed -> "removed"
        span_ [class_ $ "badge-outline " <> changeTypeFillColor kindText] $ toHtml kindText

        -- Breaking badge if applicable
        when fieldChange.breaking do
          span_ [class_ "badge bg-fillError-strong"] do
            faSprite_ "triangle-alert" "regular" "w-3 h-3 mr-1"
            "Breaking"

    -- Change description
    when (T.length fieldChange.changeDescription > 0) do
      p_ [class_ "text-sm text-textWeak mb-3 leading-relaxed"] $ toHtml fieldChange.changeDescription

    -- Type and value changes
    div_ [class_ "space-y-3"] do
      -- Type changes (if modified)
      when (fieldChange.changeKind == Anomalies.Modified || fieldChange.changeKind == Anomalies.Added || fieldChange.changeKind == Anomalies.Removed) do
        div_ [class_ "grid grid-cols-2 gap-4"] do
          when (isJust fieldChange.oldType) do
            div_ [] do
              span_ [class_ "text-xs text-textWeak block mb-1 font-medium"] "Previous Type:"
              code_ [class_ "block bg-fillError-weak text-fillError-strong px-3 py-2 rounded text-xs border border-strokeError-weak"] do
                toHtml $ fromMaybe "" fieldChange.oldType
          when (isJust fieldChange.newType) do
            div_ [] do
              span_ [class_ "text-xs text-textWeak block mb-1 font-medium"] "New Type:"
              code_ [class_ "block bg-fillSuccess-weak text-fillSuccess-strong px-3 py-2 rounded text-xs border border-strokeSuccess-weak"] do
                toHtml $ fromMaybe "" fieldChange.newType

      -- Value examples (if available)
      when (isJust fieldChange.oldValue || isJust fieldChange.newValue) do
        div_ [class_ "grid grid-cols-2 gap-4"] do
          when (isJust fieldChange.oldValue) do
            div_ [] do
              span_ [class_ "text-xs text-textWeak block mb-1 font-medium"] "Previous Value:"
              code_ [class_ "block bg-fillError-weak text-fillError-strong px-3 py-2 rounded text-xs monospace whitespace-pre-wrap border border-strokeError-weak"] do
                toHtml $ fromMaybe "" fieldChange.oldValue
          when (isJust fieldChange.newValue) do
            div_ [] do
              span_ [class_ "text-xs text-textWeak block mb-1 font-medium"] "New Value:"
              code_ [class_ "block bg-fillSuccess-weak text-fillSuccess-strong px-3 py-2 rounded text-xs monospace whitespace-pre-wrap border border-strokeSuccess-weak"] do
                toHtml $ fromMaybe "" fieldChange.newValue


anomalyAcknowledgeButton :: Projects.ProjectId -> Issues.IssueId -> Bool -> Text -> Html ()
anomalyAcknowledgeButton pid aid acked host = do
  let acknowledgeAnomalyEndpoint = "/p/" <> pid.toText <> "/anomalies/" <> Issues.issueIdText aid <> if acked then "/unacknowledge" else "/acknowledge?host=" <> host
  a_
    [ class_ $ "btn btn-sm gap-1.5 " <> if acked then "bg-fillSuccess-weak text-textSuccess border-strokeSuccess-weak" else "btn-primary"
    , term "data-tippy-content" "acknowledge issue"
    , hxGet_ acknowledgeAnomalyEndpoint
    , hxSwap_ "outerHTML"
    ]
    do
      faSprite_ "check" "regular" "w-4 h-4"
      if acked then "Acknowledged" else "Acknowledge"


anomalyArchiveButton :: Projects.ProjectId -> Issues.IssueId -> Bool -> Html ()
anomalyArchiveButton pid aid archived = do
  let archiveAnomalyEndpoint = "/p/" <> pid.toText <> "/anomalies/" <> Issues.issueIdText aid <> if archived then "/unarchive" else "/archive"
  a_
    [ class_ $ "btn btn-sm btn-ghost gap-1.5 " <> if archived then "bg-fillWarning-weak text-textWarning border-strokeWarning-weak" else ""
    , term "data-tippy-content" $ if archived then "unarchive" else "archive"
    , hxGet_ archiveAnomalyEndpoint
    , hxSwap_ "outerHTML"
    ]
    do
      faSprite_ "archive" "regular" "w-4 h-4"
      if archived then "Unarchive" else "Archive"


issueTypeBadge :: Issues.IssueType -> Bool -> Html ()
issueTypeBadge issueType critical = badge cls icon txt
  where
    (cls, icon, txt) = case issueType of
      Issues.RuntimeException -> ("bg-fillError-strong", "triangle-alert", "ERROR")
      Issues.QueryAlert -> ("bg-fillWarning-strong", "zap", "ALERT")
      Issues.LogPattern -> ("bg-fillInformation-strong", "file-text", "LOG PATTERN")
      Issues.LogPatternRateChange -> ("bg-fillWarning-strong", "trending-up", "RATE CHANGE")
      Issues.APIChange
        | critical -> ("bg-fillError-strong", "exclamation-triangle", "BREAKING")
        | otherwise -> ("bg-fillInformation-strong", "info", "Incremental")
    badge c i t = span_ [class_ $ "badge " <> c] do faSprite_ i "regular" "w-3 h-3"; t
