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
  -- AI Chat
  AIChatForm (..),
  aiChatPostH,
  aiChatHistoryGetH,
  -- Error actions
  ErrorAssignForm (..),
  errorActionPostH,
  errorAssignPostH,
)
where

import Data.Aeson qualified as AE
import Data.Aeson.Types (parseMaybe)
import Data.Default (def)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.LocalTime (zonedTimeToUTC)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.Newtypes (Aeson (..), getAeson)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Deriving.Aeson qualified as DAE
import Effectful.PostgreSQL qualified as PG
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (TermRaw (termRaw))
import Lucid.Htmx (hxGet_, hxIndicator_, hxPost_, hxSwap_, hxTarget_, hxTrigger_)
import Lucid.Hyperscript (__)
import Models.Apis.Anomalies (FieldChange (..), PayloadChange (..))
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Errors qualified as Errors
import Models.Apis.Fields.Facets qualified as Facets
import Models.Apis.Issues qualified as Issues
import Models.Apis.RequestDumps qualified as RequestDump
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Schema qualified as Schema
import Models.Telemetry.Telemetry qualified as Telemetry
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users (User (id), UserId)
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Components (emptyState_, resizer_, statBox_)
import Pages.LogExplorer.Log (virtualTable)
import Pages.Telemetry (tracePage)
import Pkg.AI qualified as AI
import Pkg.Components.Table (BulkAction (..), Column (..), Config (..), Features (..), Pagination (..), SearchMode (..), TabFilter (..), TabFilterOpt (..), Table (..), TableHeaderActions (..), TableRows (..), ZeroState (..), col, renderRowWithColumns, withAttrs)
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..))
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast)
import Text.MMark qualified as MMark
import Text.Time.Pretty (prettyTimeAuto)
import Utils (changeTypeFillColor, checkFreeTierExceeded, escapedQueryPartial, faSprite_, formatUTC, getDurationNSMS, lookupValueText, methodFillColor, statusFillColor, toUriStr)
import Web.FormUrlEncoded (FromForm)


newtype AnomalyBulkForm = AnomalyBulk
  { anomalyId :: [Text]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


-- | Helper type to extract common endpoint fields from any issue data type
data IssueEndpointInfo = IssueEndpointInfo
  { endpointMethod :: Text
  , endpointPath :: Text
  }
  deriving stock (Generic)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] IssueEndpointInfo


-- | Card style variants for anomaly metric cards
data CardStyle = CardNeutral | CardHighlight | CardWarning | CardError | CardInfo
  deriving (Eq)


-- | Metric card data for anomaly displays
data MetricCard = MetricCard
  { label :: Text
  , icon :: Text
  , value :: Html ()
  , style :: CardStyle
  , arrowIcon :: Maybe Text
  }


-- | Get CSS classes for a card style
cardStyleClasses :: CardStyle -> (Text, Text, Text)
cardStyleClasses = \case
  CardNeutral -> ("border-strokeWeak", "", "text-textWeak")
  CardHighlight -> ("border-strokeInformation-weak", "bg-fillInformation-weak", "text-fillInformation-strong")
  CardWarning -> ("border-strokeWarning-weak", "bg-fillWarning-weak", "text-fillWarning-strong")
  CardError -> ("border-strokeError-weak", "bg-fillError-weak", "text-fillError-strong")
  CardInfo -> ("border-strokeInformation-weak", "bg-fillInformation-weak", "text-fillInformation-strong")


-- | Render a single metric card
renderMetricCard :: MetricCard -> Html ()
renderMetricCard card = do
  let (borderClass, bgClass, iconColorClass) = cardStyleClasses card.style
      valueColorClass = case card.style of
        CardNeutral -> "text-textStrong"
        _ -> iconColorClass
  div_ [class_ $ "rounded-lg border p-4 " <> borderClass <> " " <> bgClass] do
    div_ [class_ "flex items-center justify-between mb-2"] do
      span_ [class_ "text-xs text-textWeak uppercase tracking-wide"] $ toHtml card.label
      faSprite_ card.icon "regular" $ "w-4 h-4 " <> iconColorClass
    div_ [class_ $ "flex items-center gap-1 " <> valueColorClass] do
      span_ [class_ "text-xl font-bold"] card.value
      whenJust card.arrowIcon $ \arrow ->
        faSprite_ arrow "regular" "w-4 h-4"


-- | Render the full anomaly metrics section with cards and alert box
anomalyMetricsSection :: [MetricCard] -> CardStyle -> Text -> Text -> Html ()
anomalyMetricsSection cards alertStyle alertTitle alertMessage = do
  let (_, alertBgClass, alertIconClass) = cardStyleClasses alertStyle
  div_ [class_ "mb-4"] do
    div_ [class_ "grid grid-cols-4 gap-3"] do
      forM_ cards renderMetricCard
    div_ [class_ $ "mt-3 p-4 rounded-lg flex items-start gap-3 " <> alertBgClass] do
      faSprite_ "circle-exclamation" "regular" $ "w-5 h-5 flex-shrink-0 mt-0.5 " <> alertIconClass
      div_ [] do
        div_ [class_ "font-semibold text-textStrong text-sm"] $ toHtml alertTitle
        div_ [class_ "text-sm text-textWeak mt-1"] $ toHtml alertMessage


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
anomalyDetailHashGetH pid issueId firstM =
  anomalyDetailCore pid firstM $ \_ ->
    Issues.selectIssueByHash pid issueId


anomalyDetailCore :: Projects.ProjectId -> Maybe Text -> (Projects.ProjectId -> ATAuthCtx (Maybe Issues.Issue)) -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
anomalyDetailCore pid firstM fetchIssue = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext

  issueM <- fetchIssue pid
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
      let isErrorRelated = issue.issueType `elem` [Issues.RuntimeException, Issues.ErrorEscalating, Issues.ErrorRegressed]
          isResolved = maybe False (\e -> e.state == Errors.ESResolved) errorM
          bwconf =
            baseBwconf
              { pageActions = Just $ div_ [class_ "flex gap-2"] do
                  anomalyAcknowledgeButton pid (UUIDId issue.id.unUUIDId) (isJust issue.acknowledgedAt) ""
                  anomalyArchiveButton pid (UUIDId issue.id.unUUIDId) (isJust issue.archivedAt)
                  -- Add error-specific buttons for runtime error issues
                  when isErrorRelated $ do
                    errorResolveButton_ pid issue.targetHash isResolved
              }
      -- Helper to fetch trace and spans given a trace ID
      let fetchTraceData traceIdM timeHint = case traceIdM of
            Just trId -> do
              trM <- Telemetry.getTraceDetails pid trId timeHint now
              case trM of
                Just traceItem -> do
                  spanRecords' <- Telemetry.getSpanRecordsByTraceId pid traceItem.traceId (Just traceItem.traceStartTime) now
                  pure (Just traceItem, V.fromList spanRecords')
                Nothing -> pure (Nothing, V.empty)
            Nothing -> pure (Nothing, V.empty)

      (trItem, spanRecs) <- case errorM of
        Just err -> do
          let targetTIdM = maybe err.recentTraceId (const err.firstTraceId) firstM
              targetTme = maybe (zonedTimeToUTC err.updatedAt) (const $ zonedTimeToUTC err.createdAt) firstM
          fetchTraceData targetTIdM (Just targetTme)
        Nothing -> case issue.issueType of
          Issues.NewShape -> do
            shapeM <- Shapes.getShapeByHash pid issue.targetHash
            case shapeM of
              Just shape -> do
                let targetTIdM = maybe shape.recentTraceId (const shape.firstTraceId) firstM
                fetchTraceData targetTIdM (Just $ shape.createdAt)
              Nothing -> pure (Nothing, V.empty)
          Issues.NewEndpoint -> do
            endpointM <- Endpoints.getEndpointByHash pid issue.targetHash
            case endpointM of
              Just endpoint -> do
                let targetTIdM = maybe endpoint.recentTraceId (const endpoint.firstTraceId) firstM
                fetchTraceData targetTIdM (Just endpoint.createdAt)
              Nothing -> pure (Nothing, V.empty)
          Issues.EndpointLatencyDegradation -> do
            case AE.fromJSON (getAeson issue.issueData) of
              AE.Success (d :: Issues.EndpointLatencyDegradationData) -> do
                let targetTIdM = d.sampleTraceIds V.!? 0
                fetchTraceData targetTIdM (Just d.detectedAt)
              _ -> pure (Nothing, V.empty)
          Issues.EndpointErrorRateSpike -> do
            endpointM <- Endpoints.getEndpointByHash pid issue.targetHash
            case endpointM of
              Just endpoint -> do
                let targetTIdM = maybe endpoint.recentTraceId (const endpoint.firstTraceId) firstM
                fetchTraceData targetTIdM (Just endpoint.createdAt)
              Nothing -> pure (Nothing, V.empty)
          Issues.EndpointVolumeRateChange -> do
            endpointM <- Endpoints.getEndpointByHash pid issue.targetHash
            case endpointM of
              Just endpoint -> do
                let targetTIdM = maybe endpoint.recentTraceId (const endpoint.firstTraceId) firstM
                fetchTraceData targetTIdM (Just endpoint.createdAt)
              Nothing -> pure (Nothing, V.empty)
          _ -> pure (Nothing, V.empty)
      addRespHeaders $ PageCtx bwconf $ anomalyDetailPage pid issue trItem spanRecs errorM now (isJust firstM)


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
            span_ [class_ "text-4xl"] $ toHtml num
            span_ [class_ "text-sm text-textWeak"] $ toHtml $ " " <> unwords (map abbreviateUnit rest)
          div_ [class_ "flex gap-2 items-center text-sm text-textWeak"] $ p_ [] $ toHtml title
  | otherwise = pass


anomalyDetailPage :: Projects.ProjectId -> Issues.Issue -> Maybe Telemetry.Trace -> V.Vector Telemetry.OtelLogsAndSpans -> Maybe Errors.ErrorL -> UTCTime -> Bool -> Html ()
anomalyDetailPage pid issue tr otellogs errM now isFirst = do
  let spanRecs = V.catMaybes $ Telemetry.convertOtelLogsAndSpansToSpanRecord <$> otellogs
      issueId = UUID.toText issue.id.unUUIDId
  div_ [class_ "pt-8 mx-auto px-4 w-full flex flex-col gap-4 overflow-auto pb-32"] do
    -- Header
    div_ [class_ "flex flex-col gap-3"] do
      div_ [class_ "flex gap-2 flex-wrap items-center"] do
        issueTypeBadge issue.issueType issue.critical
        case issue.severity of
          "critical" -> span_ [class_ "inline-flex items-center justify-center rounded-md px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 bg-fillError-weak text-fillError-strong border-2 border-strokeError-strong shadow-sm"] "CRITICAL"
          "warning" -> span_ [class_ "inline-flex items-center justify-center rounded-md px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 bg-fillWarning-weak text-fillWarning-strong border border-strokeWarning-weak shadow-sm"] "WARNING"
          _ -> pass
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
          _ -> pass
      Issues.QueryAlert -> do
        case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (alertData :: Issues.QueryAlertData) -> do
            div_ [class_ "mb-4"] do
              span_ [class_ "text-sm text-textWeak mb-2 block font-medium"] "Query:"
              div_ [class_ "bg-fillInformation-weak border border-strokeInformation-weak rounded-lg p-3 text-sm font-mono text-fillInformation-strong max-w-2xl overflow-x-auto"]
                $ toHtml alertData.queryExpression
          _ -> pass
      Issues.NewEndpoint ->
        case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (d :: Issues.NewEndpointData) -> do
            div_ [class_ "flex items-center gap-3 mb-4 p-3 rounded-lg"] do
              span_ [class_ $ "badge " <> methodFillColor d.endpointMethod] $ toHtml d.endpointMethod
              span_ [class_ "monospace bg-fillWeaker px-2 py-1 rounded text-sm text-textStrong"] $ toHtml d.endpointPath
              div_ [class_ "w-px h-4 bg-strokeWeak"] ""
              span_ [class_ "flex items-center gap-1.5 text-sm text-textWeak"] do
                faSprite_ "server" "regular" "h-3 w-3"
                toHtml d.endpointHost
            -- Stats and chart
            div_ [class_ "grid grid-cols-4 lg:grid-cols-8 gap-4 mb-4"] do
              timeStatBox_ "First Seen" $ prettyTimeAuto now d.firstSeenAt
              widget "Request trend" $ "attributes.http.request.method==\"" <> d.endpointMethod <> "\" AND attributes.http.route==\"" <> d.endpointPath <> "\" | summarize count(*) by bin_auto(timestamp)"
          _ -> pass
      Issues.NewShape ->
        case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (d :: Issues.NewShapeData) -> do
            div_ [class_ "flex items-center gap-3 mb-4 p-3 rounded-lg"] do
              span_ [class_ $ "badge " <> methodFillColor d.endpointMethod] $ toHtml d.endpointMethod
              span_ [class_ "monospace bg-fillWeaker px-2 py-1 rounded text-sm text-textStrong"] $ toHtml d.endpointPath
              div_ [class_ "w-px h-4 bg-strokeWeak"] ""

            div_ [class_ "grid grid-cols-2 lg:grid-cols-4 gap-4 mb-4"] do
              statBox_ (Just pid) Nothing "Status Code" "" (show d.statusCode) Nothing Nothing
              statBox_ (Just pid) Nothing "New Fields" "" (show $ V.length d.newFields) Nothing Nothing
              statBox_ (Just pid) Nothing "Deleted Fields" "" (show $ V.length d.deletedFields) Nothing Nothing
            unless (V.null d.newFields) do
              div_ [class_ "surface-raised rounded-2xl overflow-hidden mb-4"] do
                div_ [class_ "px-4 py-3 border-b border-strokeWeak"] do
                  span_ [class_ "text-sm font-medium text-textStrong"] "New Fields"
                div_ [class_ "p-4"] do
                  ul_ [class_ "list-disc list-inside text-sm text-textWeak"] do
                    forM_ d.newFields $ \f -> li_ [] $ toHtml f
            unless (V.null d.deletedFields) do
              div_ [class_ "surface-raised rounded-2xl overflow-hidden mb-4"] do
                div_ [class_ "px-4 py-3 border-b border-strokeWeak"] do
                  span_ [class_ "text-sm font-medium text-textError"] "Deleted Fields"
                div_ [class_ "p-4"] do
                  ul_ [class_ "list-disc list-inside text-sm text-textWeak"] do
                    forM_ d.deletedFields $ \f -> li_ [] $ toHtml f
          _ -> pass
      Issues.FieldChange ->
        case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (d :: Issues.FieldChangeData) -> do
            div_ [class_ "grid grid-cols-2 lg:grid-cols-4 gap-4 mb-4"] do
              statBox_ (Just pid) Nothing "Endpoint" "" (d.endpointMethod <> " " <> d.endpointPath) Nothing Nothing
              statBox_ (Just pid) Nothing "Field Path" "" d.keyPath Nothing Nothing
              statBox_ (Just pid) Nothing "Change Type" "" d.changeType Nothing Nothing
              statBox_ (Just pid) Nothing "New Type" "" d.newType Nothing Nothing
            whenJust d.previousType $ \prev -> do
              div_ [class_ "mb-4 p-4 bg-fillWeaker rounded-lg border border-strokeWeak"] do
                span_ [class_ "text-sm text-textWeak"] "Previous type: "
                span_ [class_ "text-sm font-mono text-textStrong"] $ toHtml prev
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
      Issues.ErrorEscalating ->
        case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (d :: Issues.ErrorEscalatingData) -> do
            div_ [class_ "grid grid-cols-2 lg:grid-cols-4 gap-4 mb-4"] do
              statBox_ (Just pid) Nothing "Escalation Rate" "" (show d.escalationRate <> "x") Nothing Nothing
              statBox_ (Just pid) Nothing "Window" "" d.escalationWindow Nothing Nothing
              statBox_ (Just pid) Nothing "Last Hour" "" (show d.occurrences1h) Nothing Nothing
              statBox_ (Just pid) Nothing "Last 24h" "" (show d.occurrences24h) Nothing Nothing
            div_ [class_ "surface-raised rounded-2xl overflow-hidden mb-4"] do
              div_ [class_ "px-4 py-3 border-b border-strokeWeak"] do
                span_ [class_ "text-sm font-medium text-textStrong"] "Error Details"
              div_ [class_ "p-4 flex flex-col gap-2"] do
                div_ [] do
                  span_ [class_ "text-sm text-textWeak"] "Exception: "
                  span_ [class_ "text-sm font-mono text-textStrong"] $ toHtml d.errorType
                div_ [] do
                  span_ [class_ "text-sm text-textWeak"] "Message: "
                  span_ [class_ "text-sm text-textStrong"] $ toHtml d.errorMessage
                whenJust d.serviceName $ \svc -> do
                  div_ [] do
                    span_ [class_ "text-sm text-textWeak"] "Service: "
                    span_ [class_ "text-sm text-textStrong"] $ toHtml svc
          _ -> pass
      Issues.ErrorRegressed ->
        case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (d :: Issues.ErrorRegressedData) -> do
            let quietDays = d.quietPeriodMinutes `div` 1440
                quietHours = (d.quietPeriodMinutes `mod` 1440) `div` 60
                quietStr = if quietDays > 0 then show quietDays <> " days" else show quietHours <> " hours"
            div_ [class_ "grid grid-cols-2 lg:grid-cols-4 gap-4 mb-4"] do
              statBox_ (Just pid) Nothing "Quiet Period" "" quietStr Nothing Nothing
              statBox_ (Just pid) Nothing "Previous Occurrences" "" (show d.previousOccurrences) Nothing Nothing
              statBox_ (Just pid) Nothing "New Occurrences" "" (show d.newOccurrences) Nothing Nothing
              timeStatBox_ "Regressed At" $ prettyTimeAuto now d.regressedAt
            div_ [class_ "surface-raised rounded-2xl overflow-hidden mb-4"] do
              div_ [class_ "px-4 py-3 border-b border-strokeWeak"] do
                span_ [class_ "text-sm font-medium text-textStrong"] "Error Details"
              div_ [class_ "p-4 flex flex-col gap-2"] do
                div_ [] do
                  span_ [class_ "text-sm text-textWeak"] "Exception: "
                  span_ [class_ "text-sm font-mono text-textStrong"] $ toHtml d.errorType
                div_ [] do
                  span_ [class_ "text-sm text-textWeak"] "Message: "
                  span_ [class_ "text-sm text-textStrong"] $ toHtml d.errorMessage
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
      Issues.EndpointLatencyDegradation ->
        case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (d :: Issues.EndpointLatencyDegradationData) -> do
            div_ [class_ "flex items-center gap-3 mb-4 p-3 rounded-lg"] do
              span_ [class_ $ "badge " <> methodFillColor d.endpointMethod] $ toHtml d.endpointMethod
              span_ [class_ "monospace bg-fillWeaker px-2 py-1 rounded text-sm text-textStrong"] $ toHtml d.endpointPath
              div_ [class_ "w-px h-4 bg-strokeWeak"] ""
            div_ [class_ "grid grid-cols-2 lg:grid-cols-4 gap-4 mb-4"] do
              statBox_ (Just pid) Nothing "Percentile" "" d.percentile Nothing Nothing
              statBox_ (Just pid) Nothing "Current Latency" "" (show (round d.currentLatencyMs :: Int) <> "ms") Nothing Nothing
              statBox_ (Just pid) Nothing "Baseline" "" (show (round d.baselineLatencyMs :: Int) <> "ms") Nothing Nothing
            div_ [class_ "p-4 bg-fillWarning-weak rounded-lg border border-strokeWarning-weak mb-4"] do
              span_ [class_ "text-sm text-fillWarning-strong font-medium"] $ toHtml $ "Latency increased by " <> show (round d.degradationPercent :: Int) <> "% (z-score: " <> show (round d.zScore :: Int) <> ")"
          _ -> pass
      Issues.EndpointErrorRateSpike ->
        case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (d :: Issues.EndpointErrorRateSpikeData) -> do
            div_ [class_ "flex items-center gap-3 mb-4 p-3 rounded-lg"] do
              span_ [class_ $ "badge " <> methodFillColor d.endpointMethod] $ toHtml d.endpointMethod
              span_ [class_ "monospace bg-fillWeaker px-2 py-1 rounded text-sm text-textStrong"] $ toHtml d.endpointPath
              div_ [class_ "w-px h-4 bg-strokeWeak"] ""

            div_ [class_ "grid grid-cols-2 lg:grid-cols-4 gap-4 mb-4"] do
              statBox_ (Just pid) Nothing "Error Rate" "" (show (round (d.currentErrorRate * 100) :: Int) <> "%") Nothing Nothing
              statBox_ (Just pid) Nothing "Errors" "" (show d.errorCount <> "/" <> show d.totalRequests) Nothing Nothing
              statBox_ (Just pid) Nothing "Baseline" "" (show (round (d.baselineErrorRate * 100) :: Int) <> "%") Nothing Nothing
            unless (V.null d.topErrorTypes) do
              div_ [class_ "surface-raised rounded-2xl overflow-hidden mb-4"] do
                div_ [class_ "px-4 py-3 border-b border-strokeWeak"] do
                  span_ [class_ "text-sm font-medium text-textStrong"] "Top Error Types"
                div_ [class_ "p-4"] do
                  ul_ [class_ "list-disc list-inside text-sm text-textWeak"] do
                    forM_ d.topErrorTypes $ \e -> li_ [class_ "font-mono"] $ toHtml e
          _ -> pass
      Issues.EndpointVolumeRateChange ->
        case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (d :: Issues.EndpointVolumeRateChangeData) -> do
            div_ [class_ "flex items-center gap-3 mb-4 p-3 rounded-lg"] do
              span_ [class_ $ "badge " <> methodFillColor d.endpointMethod] $ toHtml d.endpointMethod
              span_ [class_ "monospace bg-fillWeaker px-2 py-1 rounded text-sm text-textStrong"] $ toHtml d.endpointPath
              div_ [class_ "w-px h-4 bg-strokeWeak"] ""
            div_ [class_ "grid grid-cols-2 lg:grid-cols-4 gap-4 mb-4"] do
              statBox_ (Just pid) Nothing "Direction" "" d.changeDirection Nothing Nothing
              statBox_ (Just pid) Nothing "Current Rate" "" (show (round d.currentRatePerHour :: Int) <> "/hr") Nothing Nothing
              statBox_ (Just pid) Nothing "Baseline" "" (show (round d.baselineRatePerHour :: Int) <> "/hr") Nothing Nothing
            let alertClass = if d.changeDirection == "drop" then "bg-fillWarning-weak border-strokeWarning-weak text-fillWarning-strong" else "bg-fillInformation-weak border-strokeInformation-weak text-fillInformation-strong"
            div_ [class_ $ "p-4 rounded-lg border mb-4 " <> alertClass] do
              span_ [class_ "text-sm font-medium"] $ toHtml $ "Traffic " <> d.changeDirection <> " by " <> show (round (abs d.changePercent) :: Int) <> "%"
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
              emptyState_ "No Replay Available" "No session replays associated with this trace" (Just "https://monoscope.tech/docs/sdks/Javascript/browser/") "Session Replay Guide"

    -- AI Chat section (inline with page content)
    anomalyAIChat_ pid issue.id


-- | Form for AI chat input
newtype AIChatForm = AIChatForm {query :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


-- | Form for error assignment
newtype ErrorAssignForm = ErrorAssignForm {assigneeId :: UserId}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


-- | AI response structure from OpenAI
data AIInvestigationResponse = AIInvestigationResponse
  { explanation :: Text
  , widgets :: Maybe [Widget.Widget]
  , suggestedQuery :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Build comprehensive context for AI from issue, error, and trace data
buildAIContext :: Issues.Issue -> Maybe Anomalies.ATError -> Maybe Telemetry.Trace -> V.Vector Telemetry.OtelLogsAndSpans -> Text
buildAIContext issue errM trDataM spans =
  unlines
    $ catMaybes
      [ Just "## Issue Details"
      , Just $ "- **Title**: " <> issue.title
      , Just $ "- **Type**: " <> show issue.issueType
      , Just $ "- **Severity**: " <> issue.severity
      , Just $ "- **Service**: " <> fromMaybe "" issue.service
      , Just $ "- **Affected Requests**: 0"
      , Just $ "- **Affected Clients**: 0"
      , Just $ "- **Recommended Action**: " <> issue.recommendedAction
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


-- | System prompt for anomaly investigation AI
anomalySystemPrompt :: Text
anomalySystemPrompt =
  unlines
    [ "You are an expert debugging assistant helping to investigate application issues and anomalies."
    , "You have access to issue details, error information, stack traces, and trace data."
    , ""
    , "When analyzing issues:"
    , "1. Explain the likely root cause based on the error type and stack trace"
    , "2. Consider the context (service, method, path) for better insights"
    , "3. Suggest specific debugging steps or fixes"
    , "4. When helpful, suggest KQL queries to find related logs or data"
    , ""
    , Schema.generateSchemaForAI Schema.telemetrySchema
    , ""
    , "You can include widget configurations to visualize data. Available widget types:"
    , "- logs: Show log entries matching a query"
    , "- timeseries: Bar chart visualization"
    , "- timeseries_line: Line chart visualization"
    , "- stat: Single statistic value"
    , ""
    , "Widget structure:"
    , "{ \"widgetType\": \"logs|timeseries|timeseries_line|stat\","
    , "  \"query\": \"KQL query string\","
    , "  \"title\": \"Widget title\" }"
    , ""
    , "Respond with JSON in this structure:"
    , "{"
    , "  \"explanation\": \"Your detailed analysis and recommendations in markdown format\","
    , "  \"widgets\": [optional array of widget configs],"
    , "  \"suggestedQuery\": \"optional KQL query for further investigation\""
    , "}"
    , ""
    , "Keep explanations concise but helpful. Use markdown formatting for readability."
    , "IMPORTANT: Respond with valid JSON only, no code blocks or additional text."
    ]


-- | Handle AI chat POST request
aiChatPostH :: Projects.ProjectId -> Issues.IssueId -> AIChatForm -> ATAuthCtx (RespHeaders (Html ()))
aiChatPostH pid issueId form
  | T.length form.query > 4000 = addRespHeaders $ aiChatResponse_ pid form.query "Query too long. Maximum 4000 characters allowed." Nothing
  | otherwise = do
      (sess, project) <- Sessions.sessionAndProject pid
      appCtx <- ask @AuthContext
      now <- Time.currentTime

      let convId = UUIDId issueId.unUUIDId :: UUIDId "conversation"
      _ <- Issues.getOrCreateConversation pid convId Issues.CTAnomaly (AE.object ["issue_id" AE..= issueId])

      issueM <- Issues.selectIssueById issueId
      case issueM of
        Nothing -> do
          let response = "Issue not found. Unable to analyze."
          Issues.insertChatMessage pid convId "user" form.query Nothing Nothing
          Issues.insertChatMessage pid convId "assistant" response Nothing Nothing
          addRespHeaders $ aiChatResponse_ pid form.query response Nothing
        Just issue -> do
          errorM <- case issue.issueType of
            Issues.RuntimeException -> Anomalies.errorByHash pid issue.endpointHash
            Issues.ErrorEscalating ->
              case AE.fromJSON (getAeson issue.issueData) of
                AE.Success (d :: Issues.ErrorEscalatingData) -> Anomalies.errorByHash pid d.errorHash
                _ -> pure Nothing
            Issues.ErrorRegressed ->
              case AE.fromJSON (getAeson issue.issueData) of
                AE.Success (d :: Issues.ErrorRegressedData) -> Anomalies.errorByHash pid d.errorHash
                _ -> pure Nothing
            _ -> pure Nothing

          (traceDataM, spans) <- case errorM of
            Just err -> do
              let targetTIdM = err.recentTraceId
                  targetTime = zonedTimeToUTC err.updatedAt
              case targetTIdM of
                Just tId -> do
                  trM <- Telemetry.getTraceDetails pid tId (Just targetTime) now
                  case trM of
                    Just trData -> do
                      spanRecs <- Telemetry.getSpanRecordsByTraceId pid trData.traceId (Just trData.traceStartTime) now
                      pure (Just trData, V.fromList spanRecs)
                    Nothing -> pure (Nothing, V.empty)
                Nothing -> pure (Nothing, V.empty)
            Nothing ->
              -- For EndpointLatencyDegradation, try to fetch trace from sampleTraceIds
              case issue.issueType of
                Issues.EndpointLatencyDegradation ->
                  case AE.fromJSON (getAeson issue.issueData) of
                    AE.Success (d :: Issues.EndpointLatencyDegradationData) ->
                      case V.headM d.sampleTraceIds of
                        Just tId -> do
                          trM <- Telemetry.getTraceDetails pid tId Nothing now
                          case trM of
                            Just trData -> do
                              spanRecs <- Telemetry.getSpanRecordsByTraceId pid trData.traceId (Just trData.traceStartTime) now
                              pure (Just trData, V.fromList spanRecs)
                            Nothing -> pure (Nothing, V.empty)
                        Nothing -> pure (Nothing, V.empty)
                    _ -> pure (Nothing, V.empty)
                _ -> pure (Nothing, V.empty)

          let context = buildAIContext issue errorM traceDataM spans
              anomalyContext = unlines [anomalySystemPrompt, "", "--- ISSUE CONTEXT ---", context]
              dayAgo = addUTCTime (-86400) now
          facetSummaryM <- Facets.getFacetSummary pid "otel_logs_and_spans" dayAgo now
          let config =
                (AI.defaultAgenticConfig pid)
                  { AI.facetContext = facetSummaryM
                  , AI.customContext = Just anomalyContext
                  , AI.conversationId = Just convId
                  , AI.conversationType = Just Issues.CTAnomaly
                  }

          Issues.insertChatMessage pid convId "user" form.query Nothing Nothing
          result <- AI.runAgenticChatWithHistory config form.query appCtx.config.openaiApiKey

          case result of
            Left err -> do
              let errorResponse = "I encountered an error while analyzing this issue: " <> err
              Issues.insertChatMessage pid convId "assistant" errorResponse Nothing Nothing
              addRespHeaders $ aiChatResponse_ pid form.query errorResponse Nothing
            Right responseText -> do
              let parsed = AE.eitherDecode (fromStrict $ encodeUtf8 responseText) :: Either String AIInvestigationResponse
              case parsed of
                Left _ -> do
                  Issues.insertChatMessage pid convId "assistant" responseText Nothing Nothing
                  addRespHeaders $ aiChatResponse_ pid form.query responseText Nothing
                Right aiResp -> do
                  let limitedWidgets = take 10 <$> aiResp.widgets -- max 10 to prevent abuse
                  Issues.insertChatMessage pid convId "assistant" aiResp.explanation (Just $ AE.toJSON limitedWidgets) Nothing
                  addRespHeaders $ aiChatResponse_ pid form.query aiResp.explanation limitedWidgets


-- | Handle AI chat history GET request
aiChatHistoryGetH :: Projects.ProjectId -> Issues.IssueId -> ATAuthCtx (RespHeaders (Html ()))
aiChatHistoryGetH pid issueId = do
  (sess, project) <- Sessions.sessionAndProject pid
  let convId = UUIDId issueId.unUUIDId :: UUIDId "conversation"
  messages <- Issues.selectChatHistory convId
  addRespHeaders $ aiChatHistoryView_ pid messages


-- =============================================================================
-- Error-specific action handlers
-- =============================================================================

errorActionPostH :: Projects.ProjectId -> Text -> Text -> ATAuthCtx (RespHeaders (Html ()))
errorActionPostH pid errorHash action =
  case action of
    "resolve" -> do
      _ <- Errors.updateErrorStateByProjectAndHash pid errorHash Errors.ESResolved
      addSuccessToast "Error resolved" Nothing
      addRespHeaders $ errorResolveButton_ pid errorHash True
    "unresolve" -> do
      _ <- Errors.updateErrorStateByProjectAndHash pid errorHash Errors.ESOngoing
      addSuccessToast "Error unresolved" Nothing
      addRespHeaders $ errorResolveButton_ pid errorHash False
    _ -> do
      addErrorToast "Unknown action" Nothing
      addRespHeaders pass


-- | Assign an error to a user
errorAssignPostH :: Projects.ProjectId -> Text -> ErrorAssignForm -> ATAuthCtx (RespHeaders (Html ()))
errorAssignPostH pid errorHash form =
  withError pid errorHash "Error assigned" (`Errors.assignError` form.assigneeId)
    $ errorAssignButton_ pid errorHash (Just form.assigneeId)


-- | Helper for error actions
withError :: Projects.ProjectId -> Text -> Text -> (Errors.ErrorId -> ATAuthCtx Int64) -> Html () -> ATAuthCtx (RespHeaders (Html ()))
withError pid errorHash successMsg action responseHtml = do
  _ <- Sessions.sessionAndProject pid
  errorM <- Errors.getErrorByHash pid errorHash
  case errorM of
    Nothing -> do
      addErrorToast "Error not found" Nothing
      addRespHeaders pass
    Just err -> do
      _ <- action err.id
      addSuccessToast successMsg Nothing
      addRespHeaders responseHtml


-- | Render a single chat response (user question + AI answer)
aiChatResponse_ :: Projects.ProjectId -> Text -> Text -> Maybe [Widget.Widget] -> Html ()
aiChatResponse_ pid userQuery explanation widgetsM =
  div_ [class_ "surface-raised rounded-2xl p-6 animate-fade-in max-w-3xl mx-auto"] do
    -- User question
    div_ [class_ "flex items-start gap-3 mb-4 pb-4 border-b border-strokeWeak"] do
      div_ [class_ "p-2 rounded-lg bg-fillWeak shrink-0"] $ faSprite_ "user" "regular" "w-4 h-4 text-iconNeutral"
      p_ [class_ "text-sm text-textStrong"] $ toHtml userQuery
    -- AI response
    div_ [class_ "flex items-start gap-3"] do
      div_ [class_ "p-2 rounded-lg bg-fillBrand-weak shrink-0"] $ faSprite_ "sparkles" "regular" "w-4 h-4 text-iconBrand"
      div_ [class_ "flex-1 flex flex-col gap-4"] do
        div_ [class_ "prose prose-sm text-textStrong max-w-none"] $ renderMarkdown explanation
    whenJust widgetsM \widgets ->
      div_ [class_ "grid grid-cols-1 gap-4 mt-4"] do
        forM_ widgets \widget -> Widget.widget_ widget{Widget._projectId = Just pid}


renderMarkdown :: Text -> Html ()
renderMarkdown md = case MMark.parse "" md of
  Left _ -> toHtml md
  Right doc -> toHtmlRaw $ MMark.render doc


-- | Parse widgets from stored JSON
parseStoredWidgets :: Maybe (Aeson AE.Value) -> Maybe [Widget.Widget]
parseStoredWidgets = (>>= parseMaybe AE.parseJSON . getAeson)


-- | Render chat history, pairing user messages with their following assistant response
aiChatHistoryView_ :: Projects.ProjectId -> [Issues.AIChatMessage] -> Html ()
aiChatHistoryView_ pid = go
  where
    go (u : a : rest) | u.role == "user" && a.role == "assistant" = do
      aiChatResponse_ pid u.content a.content (parseStoredWidgets a.widgets)
      go rest
    go (_ : rest) = go rest -- skip unpaired/malformed messages
    go [] = pass


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
            div_ [class_ "p-2 rounded-lg bg-fillBrand-weak shrink-0"] $ faSprite_ "sparkles" "regular" "h-4 w-4 text-iconBrand"
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


anomalyListGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Endpoints.EndpointId -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders AnomalyListGet)
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
    script_ """var rem = (x,y)=>((x%y)==0?1:(x%y));"""
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
    case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: IssueEndpointInfo) -> do
        div_ [class_ "flex items-center gap-2"] do
          span_ [class_ $ "badge " <> methodFillColor d.endpointMethod] $ toHtml d.endpointMethod
          span_ [class_ "monospace bg-fillWeak px-2 py-1 rounded text-xs text-textStrong"] $ toHtml d.endpointPath
      _ -> pass
    -- Service badge
    span_ [class_ "flex items-center gap-1"] do
      div_ [class_ "w-3 h-3 bg-fillYellow rounded-sm"] ""
      span_ [class_ "text-textStrong"] $ toHtml $ fromMaybe "" issue.service
    -- Time since
    span_ [class_ "text-textWeak"] $ toHtml timeSinceString

    -- Statistics row (only for API changes)
    when (issue.issueType `elem` [Issues.NewShape]) do
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

  -- Issue-specific details
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
    Issues.NewEndpoint -> case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.NewEndpointData) ->
        let shapeCount = V.length d.initialShapes
         in div_ [class_ "mb-4 p-3 bg-fillInformation-weak border border-strokeInformation-weak rounded-lg"] do
              div_ [class_ "flex items-center justify-between"] do
                div_ [class_ "flex items-center gap-3"] do
                  div_ [class_ "flex items-center gap-1.5 text-sm"] do
                    faSprite_ "square-dashed" "regular" "h-3.5 w-3.5 text-fillInformation-strong"
                    span_ [class_ "font-medium text-fillInformation-strong"] $ toHtml $ show shapeCount <> " shape" <> (if shapeCount == 1 then "" else "s")
                  div_ [class_ "flex items-center gap-1.5 text-sm text-textWeak"] do
                    faSprite_ "server" "regular" "h-3 w-3"
                    span_ [] $ toHtml d.endpointHost
      _ -> pass
    Issues.NewShape -> case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.NewShapeData) -> do
        div_ [class_ "mb-4"] do
          div_ [class_ "flex items-center gap-2 text-sm mb-2"] do
            span_ [class_ "font-medium text-textStrong"] $ toHtml d.endpointMethod
            span_ [class_ "text-textWeak"] $ toHtml d.endpointPath
            span_ [class_ "text-xs px-2 py-0.5 rounded bg-fillWeak"] $ toHtml $ "Status " <> show d.statusCode
          unless (V.null d.newFields) do
            div_ [class_ "text-xs text-textWeak"] $ toHtml $ "+" <> show (V.length d.newFields) <> " new fields"
          unless (V.null d.deletedFields) do
            div_ [class_ "text-xs text-textError"] $ toHtml $ "-" <> show (V.length d.deletedFields) <> " deleted fields"
      _ -> pass
    Issues.FieldChange -> case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.FieldChangeData) ->
        div_ [class_ "mb-4 p-3 bg-fillWeak border border-strokeWeak rounded-lg"] do
          div_ [class_ "text-sm"] do
            span_ [class_ "text-textWeak"] "Field: "
            span_ [class_ "font-mono text-textStrong"] $ toHtml d.keyPath
          div_ [class_ "text-xs text-textWeak mt-1"] $ toHtml $ d.changeType <> " on " <> d.endpointMethod <> " " <> d.endpointPath
      _ -> pass
    Issues.LogPattern -> case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.LogPatternData) ->
        div_ [class_ "border border-strokeWeak rounded-lg mb-4"] do
          label_ [class_ "text-sm text-textWeak font-semibold rounded-lg p-2 flex gap-2 items-center cursor-pointer"] do
            toHtml $ fromMaybe "LOG" d.logLevel <> " pattern (" <> show d.occurrenceCount <> " occurrences)"
          div_ [class_ "bg-fillWeak p-4 overflow-x-scroll group-has-[.lp-input:checked]/lp:block text-sm monospace text-textStrong"] $ pre_ [class_ "whitespace-pre-wrap"] $ toHtml d.logPattern
      _ -> pass
    Issues.ErrorEscalating -> case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.ErrorEscalatingData) ->
        div_ [class_ "mb-4 p-3 bg-fillError-weak border border-strokeError-weak rounded-lg"] do
          div_ [class_ "text-sm text-fillError-strong font-medium"] $ toHtml d.errorType
          div_ [class_ "text-xs text-textWeak mt-1"] $ toHtml $ show d.escalationRate <> "x increase in " <> d.escalationWindow
      _ -> pass
    Issues.ErrorRegressed -> case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.ErrorRegressedData) ->
        div_ [class_ "mb-4 p-3 bg-fillError-weak border border-strokeError-weak rounded-lg"] do
          div_ [class_ "text-sm text-fillError-strong font-medium"] $ toHtml d.errorType
          div_ [class_ "text-xs text-textWeak mt-1"] $ toHtml $ "Regressed after " <> show (d.quietPeriodMinutes `div` 60) <> " hours quiet"
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
    Issues.EndpointLatencyDegradation -> case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.EndpointLatencyDegradationData) ->
        let cards =
              [ MetricCard "Percentile" "chart-line" (toHtml d.percentile) CardNeutral Nothing
              , MetricCard "Baseline" "clock" (toHtml $ getDurationNSMS (round d.baselineLatencyMs)) CardNeutral Nothing
              , MetricCard "Current" "chart-line" (toHtml $ getDurationNSMS (round d.currentLatencyMs)) CardHighlight Nothing
              , MetricCard "Change" "circle-exclamation" (toHtml $ "+" <> show (round d.degradationPercent :: Int) <> "%") CardWarning (Just "arrow-up")
              ]
         in anomalyMetricsSection cards CardWarning "Performance Degradation Detected" issue.recommendedAction
      _ -> pass
    Issues.EndpointErrorRateSpike -> case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.EndpointErrorRateSpikeData) ->
        let cards =
              [ MetricCard "Error Rate" "percent" (toHtml $ show (round (d.currentErrorRate * 100) :: Int) <> "%") CardNeutral Nothing
              , MetricCard "Baseline" "clock" (toHtml $ show (round (d.baselineErrorRate * 100) :: Int) <> "%") CardNeutral Nothing
              , MetricCard "Errors" "hashtag" (toHtml $ show d.errorCount <> "/" <> show d.totalRequests) CardNeutral Nothing
              , MetricCard "Spike" "circle-exclamation" (toHtml $ "+" <> show (round d.spikePercent :: Int) <> "%") CardError (Just "arrow-up")
              ]
         in anomalyMetricsSection cards CardError "Error Rate Spike Detected" issue.recommendedAction
      _ -> pass
    Issues.EndpointVolumeRateChange -> case AE.fromJSON (getAeson issue.issueData) of
      AE.Success (d :: Issues.EndpointVolumeRateChangeData) ->
        let isDrop = d.changeDirection == "drop"
            cardStyle = if isDrop then CardWarning else CardInfo
            arrowIcon = if isDrop then "arrow-down" else "arrow-up"
            changeSign = if isDrop then "" else "+"
            alertTitle = if isDrop then "Traffic Drop Detected" else "Traffic Surge Detected"
            cards =
              [ MetricCard "Direction" arrowIcon (span_ [class_ "capitalize"] $ toHtml d.changeDirection) cardStyle Nothing
              , MetricCard "Current" "gauge-high" (toHtml $ show (round d.currentRatePerHour :: Int) <> "/hr") CardNeutral Nothing
              , MetricCard "Baseline" "clock" (toHtml $ show (round d.baselineRatePerHour :: Int) <> "/hr") CardNeutral Nothing
              , MetricCard "Change" "circle-exclamation" (toHtml $ changeSign <> show (round (abs d.changePercent) :: Int) <> "%") cardStyle (Just arrowIcon)
              ]
         in anomalyMetricsSection cards cardStyle alertTitle issue.recommendedAction
      _ -> pass

  -- Recommended action (only show for issue types that don't have built-in alert boxes)
  unless (issue.issueType `elem` [Issues.EndpointLatencyDegradation, Issues.EndpointErrorRateSpike, Issues.EndpointVolumeRateChange]) do
    div_ [class_ "border-l-4 border-strokeBrand pl-4 mb-4"] $ p_ [class_ "text-sm text-textStrong leading-relaxed"] $ toHtml issue.recommendedAction

  -- Action buttons
  let logsQuery = case issue.issueType of
        Issues.NewEndpoint -> case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (d :: Issues.NewEndpointData) -> Just $ "attributes.http.request.method==\"" <> d.endpointMethod <> "\" AND attributes.http.route==\"" <> d.endpointPath <> "\""
          _ -> Nothing
        Issues.NewShape -> case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (d :: Issues.NewShapeData) -> Just $ "attributes.http.request.method==\"" <> d.endpointMethod <> "\" AND attributes.http.route==\"" <> d.endpointPath <> "\""
          _ -> Nothing
        Issues.LogPattern -> case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (d :: Issues.LogPatternData) -> Just $ "log_pattern=\"" <> d.logPattern <> "\""
          _ -> Nothing
        Issues.LogPatternRateChange -> case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (d :: Issues.LogPatternRateChangeData) -> Just $ "log_pattern=\"" <> d.logPattern <> "\""
          _ -> Nothing
        _ -> Nothing
      logsUrl = (\q -> "/p/" <> pid.toText <> "/log_explorer?query=" <> toUriStr q) <$> logsQuery

  div_ [class_ "flex items-center gap-3 mt-4 pt-4 border-t border-strokeWeak"] do
    whenJust logsUrl \url ->
      a_ [href_ url, class_ "inline-flex items-center justify-center whitespace-nowrap text-sm font-medium transition-all h-8 rounded-md gap-1.5 px-3 text-textBrand hover:text-textBrand/80 hover:bg-fillBrand-weak"] do
        faSprite_ "eye" "regular" "w-4 h-4"
        span_ [class_ "leading-none"] "View related logs"
    -- View Full Schema button (only for schema-related issue types)
    when (issue.issueType `elem` [Issues.NewEndpoint, Issues.NewShape, Issues.FieldChange]) do
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
      if acked then "Acknowledged" else "Acknowledge"


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
      if archived then "Unarchive" else "Archive"


-- | Resolve/Unresolve error button
errorResolveButton_ :: Projects.ProjectId -> Text -> Bool -> Html ()
errorResolveButton_ pid errorHash isResolved = do
  let endpoint = "/p/" <> pid.toText <> "/anomalies/errors/" <> errorHash <> "/actions/" <> if isResolved then "unresolve" else "resolve"
  a_
    [ class_
        $ "inline-flex items-center gap-2 cursor-pointer py-2 px-3 rounded-xl "
        <> (if isResolved then "bg-fillSuccess-weak text-textSuccess" else "btn-primary")
    , term "data-tippy-content" $ if isResolved then "Reopen error" else "Resolve error"
    , hxPost_ endpoint
    , hxSwap_ "outerHTML"
    ]
    do
      faSprite_ (if isResolved then "rotate-left" else "circle-check") "regular" "w-4 h-4"
      if isResolved then "Resolved" else "Resolve"


-- | Assign error button (dropdown)
errorAssignButton_ :: Projects.ProjectId -> Text -> Maybe UserId -> Html ()
errorAssignButton_ pid errorHash assigneeM = do
  let isAssigned = isJust assigneeM
  div_ [class_ "relative group/assign"] do
    button_
      [ class_
          $ "inline-flex items-center gap-2 cursor-pointer py-2 px-3 rounded-xl "
          <> (if isAssigned then "bg-fillBrand-weak text-textBrand" else "btn-primary")
      , type_ "button"
      ]
      do
        faSprite_ "user-plus" "regular" "w-4 h-4"
        if isAssigned then "Assigned" else "Assign"
    -- Dropdown will be populated with team members via HTMX if needed
    div_
      [ class_ "absolute right-0 top-full mt-1 bg-fill rounded-lg shadow-lg border border-strokeWeak hidden group-hover/assign:block min-w-[200px] z-10"
      , id_ $ "assign-dropdown-" <> errorHash
      ]
      do
        div_ [class_ "p-2 text-xs text-textWeak"] "Click to see assignees"


issueTypeBadge :: Issues.IssueType -> Bool -> Html ()
issueTypeBadge issueType critical = badge cls icon txt
  where
    (cls, icon, txt) = case issueType of
      Issues.RuntimeException -> ("bg-fillError-strong", "triangle-alert", "Error")
      Issues.QueryAlert -> ("bg-fillWarning-strong", "zap", "ALERT")
      Issues.NewEndpoint -> ("bg-fillInformation-strong", "plus-circle", "Info")
      Issues.NewShape
        | critical -> ("bg-fillError-strong", "exclamation-triangle", "Breaking")
        | otherwise -> ("bg-fillInformation-strong", "shapes", "Info")
      Issues.FieldChange
        | critical -> ("bg-fillError-strong", "exclamation-triangle", "Breaking")
        | otherwise -> ("bg-fillWarning-strong", "pen-to-square", "Incremental")
      Issues.LogPattern -> ("bg-fillInformation-strong", "file-lines", "Info")
      Issues.ErrorEscalating -> ("bg-fillError-strong", "arrow-trend-up", "E")
      Issues.ErrorRegressed -> ("bg-fillError-strong", "rotate-left", "Alert")
      Issues.LogPatternRateChange -> ("bg-fillWarning-strong", "chart-line", "RATE CHANGE")
      Issues.EndpointLatencyDegradation -> ("bg-fillWarning-strong", "clock", "Alert")
      Issues.EndpointErrorRateSpike -> ("bg-fillError-strong", "chart-line-up", "Alert")
      Issues.EndpointVolumeRateChange -> ("bg-fillWarning-strong", "arrows-up-down", "Alert")
    badge c i t = span_ [class_ $ "badge " <> c] do faSprite_ i "regular" "w-3 h-3"; t
