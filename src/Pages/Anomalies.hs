{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Pages.Anomalies (
  anomalyListGetH,
  anomalyBulkActionsPostH,
  escapedQueryPartial,
  acknowledgeAnomalyGetH,
  unAcknowledgeAnomalyGetH,
  archiveAnomalyGetH,
  unArchiveAnomalyGetH,
  anomalyListSlider,
  anomalyDetailGetH,
  AnomalyBulkForm (..),
  AnomalyListGet (..),
  anomalyAcknowledgeButton,
  anomalyArchiveButton,
  anomalyDetailHashGetH,
  AnomalyAction (..),
  IssueVM (..),
  issueColumns,
)
where

import Data.Aeson qualified as AE
import Data.Default (def)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.LocalTime (zonedTimeToUTC)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.Newtypes (getAeson)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.PostgreSQL qualified as PG
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (TermRaw (termRaw))
import Lucid.Htmx (hxGet_, hxIndicator_, hxSwap_, hxTarget_, hxTrigger_)
import Lucid.Hyperscript (__)
import Models.Apis.Anomalies (FieldChange (..), PayloadChange (..))
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Issues qualified as Issues
import Models.Apis.RequestDumps qualified as RequestDump
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users (User (id))
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Components (emptyState_, resizer_, statBox_)
import Pages.LogExplorer.Log (virtualTable)
import Pages.Telemetry (tracePage)
import Pkg.Components.Table (BulkAction (..), Column (..), Config (..), Features (..), Pagination (..), SearchMode (..), TabFilter (..), TabFilterOpt (..), Table (..), TableHeaderActions (..), TableRows (..), ZeroState (..), col, renderRowWithColumns, withAttrs)
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..))
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import System.Config (AuthContext (..))
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast)
import Text.Time.Pretty (prettyTimeAuto)
import Utils (changeTypeFillColor, checkFreeTierExceeded, escapedQueryPartial, faSprite_, formatUTC, lookupValueText, methodFillColor, statusFillColor, toUriStr)
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
  -- Convert to Issues.IssueId for new system
  let issueId = UUIDId aid.unUUIDId
  -- Use new Issues acknowledge function
  _ <- Issues.acknowledgeIssue issueId sess.user.id
  -- Still use old cascade for compatibility
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


-- When given a list of anomalyIDs and an action, said action would be applied to the anomalyIDs.
-- Then a notification should be triggered, as well as an action to reload the anomaly List.
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
anomalyDetailHashGetH pid issueId firstM =
  anomalyDetailCore pid firstM $ \_ ->
    Issues.selectIssueByHash pid issueId


anomalyDetailCore :: Projects.ProjectId -> Maybe Text -> (Projects.ProjectId -> ATAuthCtx (Maybe Issues.Issue)) -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
anomalyDetailCore pid firstM fetchIssue = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext

  issueM <- fetchIssue pid

  let bwconf = (def :: BWConfig){sessM = Just sess, currProject = Just project, pageTitle = "Anomaly Detail", config = appCtx.config}
  now <- Time.currentTime
  case issueM of
    Nothing -> do
      addErrorToast "Issue not found" Nothing
      addRespHeaders
        $ PageCtx bwconf
        $ toHtml ("Issue not found" :: Text)
    Just issue -> do
      errorM <-
        issue.issueType & \case
          Issues.RuntimeException -> Anomalies.errorByHash pid issue.endpointHash
          _ -> pure Nothing
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
      addRespHeaders $ PageCtx bwconf $ anomalyDetailPage pid issue trItem spanRecs errorM now (isJust firstM)


anomalyDetailPage :: Projects.ProjectId -> Issues.Issue -> Maybe Telemetry.Trace -> V.Vector Telemetry.OtelLogsAndSpans -> Maybe Anomalies.ATError -> UTCTime -> Bool -> Html ()
anomalyDetailPage pid issue tr otellogs errM now isFirst = do
  let spanRecs = V.catMaybes $ Telemetry.convertOtelLogsAndSpansToSpanRecord <$> otellogs
      issueId = UUID.toText issue.id.unUUIDId
  div_ [class_ "pt-2 mx-auto px-4 w-full flex flex-col gap-4 h-full overflow-auto"] do
    div_ [] do
      div_ [class_ "flex gap-3 mb-3 flex-wrap items-center "] do
        div_ [class_ "flex items-center gap-2"] do
          case issue.issueType of
            Issues.RuntimeException ->
              span_ [class_ "badge bg-fillError-strong"] do
                faSprite_ "triangle-alert" "regular" "w-3 h-3"
                "ERROR"
            Issues.QueryAlert ->
              span_ [class_ "badge bg-fillWarning-strong"] do
                faSprite_ "zap" "regular" "w-3 h-3"
                "ALERT"
            Issues.APIChange ->
              if issue.critical
                then span_ [class_ "badge bg-fillError-strong"] do
                  faSprite_ "exclamation-triangle" "regular" "w-3 h-3"
                  "BREAKING"
                else span_ [class_ "badge bg-fillInformation-strong"] do
                  faSprite_ "info" "regular" "w-3 h-3 mr-0.5"
                  "Incremental"

          -- Severity badge
          case issue.severity of
            "critical" -> span_ [class_ "inline-flex items-center justify-center rounded-md px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 bg-fillError-weak text-fillError-strong border-2 border-strokeError-strong shadow-sm"] "CRITICAL"
            "warning" -> span_ [class_ "inline-flex items-center justify-center rounded-md px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 bg-fillWarning-weak text-fillWarning-strong border border-strokeWarning-weak shadow-sm"] "WARNING"
            _ -> pass
        h3_ [class_ "text-textStrong text-2xl font-medium"] $ toHtml issue.title
      p_ [class_ "text-sm text-textWeak"] $ toHtml issue.recommendedAction

    -- Metrics Bar
    div_ [class_ "flex items-center justify-between mb-6"] do
      div_ [class_ "flex items-center gap-4"] do
        statBox_ (Just pid) Nothing "Affected Requests" "How the error occurred" (show issue.affectedRequests) Nothing Nothing
        statBox_ (Just pid) Nothing "Affected Clients" "Number of unique clients affected" (show issue.affectedClients) Nothing Nothing
      div_ [class_ "w-96 h-28 rounded-xl overflow-hidden border p-2 border-strokeWeak bg-fillWeaker"]
        $ Widget.widget_
        $ (def :: Widget.Widget)
          { Widget.standalone = Just True
          , Widget.id = Just issueId
          , Widget.wType = Widget.WTTimeseries
          , Widget.title = Just "Error trends"
          , Widget.showTooltip = Just False
          , Widget.naked = Just True
          , Widget.xAxis = Just (def{Widget.showAxisLabel = Just False})
          , Widget.yAxis = Just (def{Widget.showOnlyMaxLabel = Just True})
          , Widget.query = Just "status_code == \"ERROR\" | summarize count(*) by bin(timestamp, 1h)"
          , Widget._projectId = Just issue.projectId
          , Widget.hideLegend = Just True
          }
    -- Two Column Layout
    div_ [class_ "flex flex-col gap-4"] do
      div_ [class_ "grid grid-cols-2 gap-4 w-full"] do
        case issue.issueType of
          Issues.RuntimeException -> do
            case AE.fromJSON (getAeson issue.issueData) of
              AE.Success (exceptionData :: Issues.RuntimeExceptionData) -> do
                div_ [class_ "bg-fillWeaker border border-strokeWeak rounded-lg"] do
                  div_ [class_ "px-4 py-2 border-b border-strokeWeak flex items-center justify-between"] do
                    div_ [class_ "flex items-center gap-2"] do
                      span_ [class_ "text-sm font-medium text-textStrong"] "Stack Trace"
                  div_ [class_ "p-4"] do
                    pre_ [class_ "text-sm text-textWeak font-mono leading-relaxed overflow-x-auto"] $ toHtml exceptionData.stackTrace
                whenJust errM $ \err -> do
                  div_ [class_ "bg-fillWeaker border border-strokeWeak rounded-lg"] do
                    div_ [class_ "px-4 py-2 border-b border-strokeWeak flex items-center justify-between"] do
                      div_ [class_ "flex items-center gap-2"] do
                        span_ [class_ "text-sm font-medium text-textStrong"] "Error details"
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
                            span_ [class_ "text-sm text-textWeak"] "First seen:"
                            span_ [class_ "ml-2 text-sm"] $ toHtml $ prettyTimeAuto now (zonedTimeToUTC err.createdAt)

                        div_ [class_ "flex items-center gap-2"] do
                          faSprite_ "calendar" "regular" "w-3 h-3"
                          div_ [] do
                            span_ [class_ "text-sm text-textWeak"] "Last seen:"
                            span_ [class_ "ml-2 text-sm"] $ toHtml $ prettyTimeAuto now (zonedTimeToUTC err.updatedAt)
                      div_ [class_ "flex items-center gap-4"] do
                        div_ [class_ "flex items-center gap-2"] do
                          faSprite_ "code" "regular" "w-4 h-4"
                          div_ [] do
                            span_ [class_ "text-sm text-textWeak"] "Stack:"
                            span_ [class_ "ml-2 text-sm"] $ toHtml $ fromMaybe "Unknown stack" err.errorData.stack

                        div_ [class_ "flex items-center gap-2"] do
                          faSprite_ "server" "regular" "w-3 h-3"
                          div_ [] do
                            span_ [class_ "text-sm text-textWeak"] "Service:"
                            span_ [class_ "ml-2 text-sm"] $ toHtml $ fromMaybe "Unknown service" err.errorData.serviceName
              _ -> pass
          Issues.QueryAlert -> do
            case AE.fromJSON (getAeson issue.issueData) of
              AE.Success (alertData :: Issues.QueryAlertData) -> do
                div_ [class_ "mb-4"] do
                  span_ [class_ "text-sm text-textWeak mb-2 block font-medium"] "Query:"
                  div_ [class_ "bg-fillInformation-weak border border-strokeInformation-weak rounded-lg p-3 text-sm font-mono text-fillInformation-strong max-w-2xl overflow-x-auto"]
                    $ toHtml alertData.queryExpression
              _ -> pass
          _ -> pass

      div_ [class_ "w-full border border-strokeWeak rounded-lg mb-5", id_ "error-details-container"] do
        div_ [class_ "px-4  border-b border-b-strokeWeak flex items-center justify-between"] do
          h4_ [class_ "text-textStrong text-lg font-medium"] "Overview"
          div_ [class_ "flex items-center"] do
            let aUrl = "/p/" <> pid.toText <> "/anomalies/" <> issueId <> ""
            a_ [href_ $ aUrl <> "?first_occurrence=true", class_ $ (if isFirst then "text-textBrand font-medium" else "") <> " text-xs py-3 px-3 border-b border-b-transparent cursor-pointer font-medium", term "data-tippy-content" "Show first trace the error occured"] "First"
            a_ [href_ aUrl, class_ $ (if isFirst then "" else "text-textBrand font-medium") <> " text-xs py-3 px-3 border-b border-b-transparent cursor-pointer font-medium", term "data-tippy-content" "Show recent trace the error occured"] "Recent"
            span_ [class_ "mx-6 text-textWeak w-1 h-3 bg-fillWeak"] pass
            button_ [class_ "text-xs py-3 px-3 border-b border-b-transparent cursor-pointer err-tab  t-tab-active font-medium", onclick_ "navigatable(this, '#span-content', '#error-details-container', 't-tab-active', 'err')"] "Trace"
            button_ [class_ "text-xs py-3 px-3 border-b border-b-transparent cursor-pointer err-tab font-medium", onclick_ "navigatable(this, '#log-content', '#error-details-container', 't-tab-active', 'err')"] "Logs"
            button_ [class_ "text-xs py-3 px-3 border-b border-b-transparent cursor-pointer err-tab font-medium", onclick_ "navigatable(this, '#replay-content', '#error-details-container', 't-tab-active', 'err')"] "Replay"
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
                emptyState_ "No Replay Available" "No session replays associated with this trace" (Just "https://monoscope.tech/docs/sdks/Javascript/browser/") "Session Replay Guide"


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
                        , destination = Right "https://monoscope.tech/docs/sdks/nodejs/expressjs/#reporting-errors-to-apitoolkit"
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
    (Just "slider", Just "true", _, _) -> ALSlider currTime pid endpointM (Just $ V.fromList $ map (IssueVM True False currTime filterV) issues)
    (_, _, _, Just "true") -> ALRows $ TableRows{columns = issueColumns pid, rows = issuesVM, emptyState = Nothing, renderAsTable = True, rowId = Just \(IssueVM _ _ _ _ issue) -> Issues.issueIdText issue.id, rowAttrs = Just $ const [class_ "group/row hover:bg-fillWeaker"], pagination = if totalCount > 0 then Just paginationConfig else Nothing}
    _ -> ALPage $ PageCtx bwconf issuesTable


data AnomalyListGet
  = ALPage (PageCtx (Table IssueVM))
  | ALRows (TableRows IssueVM)
  | ALSlider UTCTime Projects.ProjectId (Maybe Endpoints.EndpointId) (Maybe (V.Vector IssueVM))


instance ToHtml AnomalyListGet where
  toHtml (ALSlider utcTime pid eid issue) = toHtmlRaw $ anomalyListSlider utcTime pid eid issue
  toHtml (ALPage pg) = toHtml pg
  toHtml (ALRows rows) = toHtml rows
  toHtmlRaw = toHtml


anomalyListSlider :: UTCTime -> Projects.ProjectId -> Maybe Endpoints.EndpointId -> Maybe (V.Vector IssueVM) -> Html ()
anomalyListSlider _ _ _ (Just []) = ""
anomalyListSlider _ pid eid Nothing = do
  div_ [hxGet_ $ "/p/" <> pid.toText <> "/anomalies?layout=slider" <> maybe "" (\x -> "&endpoint=" <> x.toText) eid, hxSwap_ "outerHTML", hxTrigger_ "load"] do
    div_ [class_ "flex justify-between mt-5 pb-2"] do
      div_ [class_ "flex flex-row"] do
        a_ [href_ "#", [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .parent-slider)|]] $ faSprite_ "chevron-down" "regular" "h-4 mr-3 mt-1 w-4"
        span_ [class_ "text-lg text-textStrong"] "Ongoing Issues and Monitors"
      div_ [class_ "flex flex-row mt-2"] ""
anomalyListSlider currTime _ _ (Just issues) = do
  let anomalyIds = T.replace "\"" "'" $ show $ fmap (Issues.issueIdText . (\(IssueVM _ _ _ _ issue) -> issue.id)) issues
  let totalAnomaliesTxt = toText $ if length issues > 10 then ("10+" :: Text) else show (length issues)
  div_ do
    script_ [text| var rem = (x,y)=>((x%y)==0?1:(x%y)); |]
    script_
      [type_ "text/hyperscript"]
      [text| init set $$currentAnomaly to 0 then set $$anomalyIds to $anomalyIds
          def setAnomalySliderPag()
            set #anomalySliderPagination.innerHTML to ($$currentAnomaly+1)+'/$totalAnomaliesTxt '
          end |]
    div_ [class_ "flex justify-between mt-5 pb-2"] do
      div_ [class_ "flex flex-row"] do
        a_ [href_ "#", [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .parent-slider)|]] $ faSprite_ "chevron-down" "regular" "h-4 mr-3 mt-1 w-4"
        span_ [class_ "text-lg text-textStrong"] "Ongoing Issues and Monitors"
      div_ [class_ "flex items-center gap-2 mt-2"] do
        a_
          [ class_ "cursor-pointer"
          , [__|on click hide #{$anomalyIds[$currentAnomaly]} then
                          js($currentAnomaly, $anomalyIds) return (Math.max(0, $currentAnomaly-1) % $anomalyIds.length) end then
                          set $currentAnomaly to it then show #{$anomalyIds[$currentAnomaly]} then setAnomalySliderPag()|]
          ]
          $ faSprite_ "arrow-left" "regular" "h-4 w-4"
        span_ [src_ " mx-4", id_ "anomalySliderPagination"] "1/1"
        a_
          [ class_ "cursor-pointer"
          , [__|on click hide #{$anomalyIds[$currentAnomaly]} then
                js($currentAnomaly, $anomalyIds) return (($currentAnomaly+1) % $anomalyIds.length) end then
                set $currentAnomaly to it then show #{$anomalyIds[$currentAnomaly]} then setAnomalySliderPag()|]
          ]
          $ faSprite_ "arrow-right" "regular" "h-4 w-4"

    div_
      [ class_ "parent-slider"
      , [__|init setAnomalySliderPag() then show #{$anomalyIds[$currentAnomaly]} |]
      ]
      $ forM_ issues renderIssueForSlider
  where
    renderIssueForSlider vm@(IssueVM hideByDefault _ _ _ issue) =
      renderRowWithColumns
        [ class_ $ "flex gap-8 items-start itemsListItem " <> if hideByDefault then "surface-raised rounded-2xl" else "px-0.5 py-4"
        , style_ (if hideByDefault then "display:none" else "")
        , id_ $ Issues.issueIdText issue.id
        ]
        (issueColumns issue.projectId)
        vm


-- anomalyAccentColor isAcknowleged isArchived
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
      span_ [class_ "text-textStrong"] $ toHtml issue.service
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
        when (breakingChanges > 0 && totalChanges > 0) $ span_ [class_ "text-xs ml-1 bg-fillError-weak text-fillError-strong px-1.5 py-0.5 rounded"] $ toHtml $ show (round (fromIntegral breakingChanges / fromIntegral totalChanges * 100 :: Float) :: Int) <> "%"
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
            do
              if null responseChanges
                then div_ [class_ "text-center py-8 text-textWeak"] "No response payload changes"
                else forM_ responseChanges (renderPayloadChange True)

          -- Request panel (visible when request tab is selected)
          div_
            [ role_ "tabpanel"
            , class_ "flex-1 outline-none p-4 space-y-4 hidden group-has-[.payload-tab-request:checked]/payloadtabs:block"
            ]
            do
              if null requestChanges
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
    [ class_
        $ "inline-flex items-center gap-2 cursor-pointer py-2 px-3 rounded-xl  "
        <> (if acked then "bg-fillSuccess-weak text-textSuccess" else "btn-primary")
    , term "data-tippy-content" "acknowledge issue"
    , hxGet_ acknowledgeAnomalyEndpoint
    , hxSwap_ "outerHTML"
    ]
    do
      faSprite_ "check" "regular" "w-4 h-4"
      span_ [class_ "leading-none"] $ if acked then "Acknowleged" else "Acknowlege"


anomalyArchiveButton :: Projects.ProjectId -> Issues.IssueId -> Bool -> Html ()
anomalyArchiveButton pid aid archived = do
  let archiveAnomalyEndpoint = "/p/" <> pid.toText <> "/anomalies/" <> Issues.issueIdText aid <> if archived then "/unarchive" else "/archive"
  a_
    [ class_
        $ "inline-flex items-center gap-2 cursor-pointer py-2 px-3 rounded-xl "
        <> (if archived then " bg-fillSuccess-weak text-textSuccess" else "btn-primary")
    , term "data-tippy-content" $ if archived then "unarchive" else "archive"
    , hxGet_ archiveAnomalyEndpoint
    , hxSwap_ "outerHTML"
    ]
    do
      faSprite_ "archive" "regular" "w-4 h-4"
      span_ [class_ "leading-none"] $ if archived then "Unarchive" else "Archive"


issueTypeBadge :: Issues.IssueType -> Bool -> Html ()
issueTypeBadge issueType critical = badge cls icon txt
  where
    (cls, icon, txt) = case issueType of
      Issues.RuntimeException -> ("bg-fillError-strong", "triangle-alert", "ERROR")
      Issues.QueryAlert -> ("bg-fillWarning-strong", "zap", "ALERT")
      Issues.APIChange
        | critical -> ("bg-fillError-strong", "exclamation-triangle", "BREAKING")
        | otherwise -> ("bg-fillInformation-strong", "info", "Incremental")
    badge c i t = span_ [class_ $ "badge " <> c] do faSprite_ i "regular" "w-3 h-3"; t
