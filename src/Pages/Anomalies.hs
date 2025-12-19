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
  AnomalyBulkForm (..),
  AnomalyListGet (..),
  anomalyAcknowledgeButton,
  anomalyArchiveButton,
  AnomalyAction (..),
  IssueVM (..),
  issueColumns,
)
where

import Data.Aeson qualified as AE
import Data.Default (def)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.LocalTime (zonedTimeToUTC)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (execute)
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.Newtypes (getAeson)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx (hxGet_, hxSwap_, hxTarget_, hxTrigger_)
import Lucid.Hyperscript (__)
import Models.Apis.Anomalies (FieldChange (..), PayloadChange (..))
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Issues qualified as Issues
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users (User (id))
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pkg.Components.Table (BulkAction (..), Column (..), Config (..), Features (..), SearchMode (..), SortConfig (..), TabFilter (..), TabFilterOpt (..), Table (..), TableRows (..), ZeroState (..), col, renderRowWithColumns, withAttrs)
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..))
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import System.Config (AuthContext (..))
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast)
import Text.Time.Pretty (prettyTimeAuto)
import Utils (changeTypeFillColor, checkFreeTierExceeded, escapedQueryPartial, faSprite_, methodFillColor, statusFillColor)
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
  _ <- dbtToEff $ Issues.acknowledgeIssue issueId sess.user.id
  -- Still use old cascade for compatibility
  let text_id = V.fromList [UUID.toText aid.unUUIDId]
  v <- dbtToEff $ Anomalies.acknowledgeAnomalies sess.user.id text_id
  _ <- dbtToEff $ Anomalies.acknowlegeCascade sess.user.id v
  addRespHeaders $ Acknowlege pid (UUIDId aid.unUUIDId) True


unAcknowledgeAnomalyGetH :: Projects.ProjectId -> Anomalies.AnomalyId -> ATAuthCtx (RespHeaders AnomalyAction)
unAcknowledgeAnomalyGetH pid aid = do
  (sess, project) <- Sessions.sessionAndProject pid
  let q = [sql| update apis.anomalies set acknowledged_by=null, acknowledged_at=null where id=? |]
  let qI = [sql| update apis.issues set acknowledged_by=null, acknowledged_at=null where id=? |]
  _ <- dbtToEff $ execute qI (Only aid)
  _ <- dbtToEff $ execute q (Only aid)
  addRespHeaders $ Acknowlege pid (UUIDId aid.unUUIDId) False


archiveAnomalyGetH :: Projects.ProjectId -> Anomalies.AnomalyId -> ATAuthCtx (RespHeaders AnomalyAction)
archiveAnomalyGetH pid aid = do
  (sess, project) <- Sessions.sessionAndProject pid
  let q = [sql| update apis.anomalies set archived_at=NOW() where id=? |]
  let qI = [sql| update apis.issues set archived_at=NOW() where id=? |]
  _ <- dbtToEff $ execute qI (Only aid)
  _ <- dbtToEff $ execute q (Only aid)
  addRespHeaders $ Archive pid (UUIDId aid.unUUIDId) True


unArchiveAnomalyGetH :: Projects.ProjectId -> Anomalies.AnomalyId -> ATAuthCtx (RespHeaders AnomalyAction)
unArchiveAnomalyGetH pid aid = do
  (sess, project) <- Sessions.sessionAndProject pid
  let q = [sql| update apis.anomalies set archived_at=null where id=? |]
  let qI = [sql| update apis.issues set archived_at=null where id=? |]
  _ <- dbtToEff $ execute qI (Only aid)
  _ <- dbtToEff $ execute q (Only aid)
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
          v <- dbtToEff $ Anomalies.acknowledgeAnomalies sess.user.id (V.fromList items.anomalyId)
          _ <- dbtToEff $ Anomalies.acknowlegeCascade sess.user.id v
          pass
        "archive" -> do
          _ <- dbtToEff $ execute [sql| update apis.anomalies set archived_at=NOW() where id=ANY(?::uuid[]) |] (Only $ V.fromList items.anomalyId)
          pass
        _ -> error $ "unhandled anomaly bulk action state " <> action
      addSuccessToast (action <> "d items Successfully") Nothing
      addRespHeaders Bulk


anomalyListGetH
  :: Projects.ProjectId
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
anomalyListGetH pid layoutM filterTM sortM timeFilter pageM loadM endpointM hxRequestM hxBoostedM = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  let (ackd, archived, currentFilterTab) = case filterTM of
        Just "Inbox" -> (False, False, "Inbox")
        Just "Acknowleged" -> (True, False, "Acknowleged")
        Just "Archived" -> (False, True, "Archived")
        _ -> (False, False, "Inbox")

  -- let fLimit = 10
  let filterV = fromMaybe "14d" timeFilter

  let pageInt = maybe 0 (Unsafe.read . toString) pageM

  freeTierExceeded <- dbtToEff $ checkFreeTierExceeded pid project.paymentPlan
  currTime <- liftIO getCurrentTime

  let fLimit = 10
  issues <- dbtToEff $ Issues.selectIssues pid Nothing (Just ackd) (Just archived) fLimit (pageInt * fLimit) Nothing

  let currentURL = mconcat ["/p/", pid.toText, "/anomalies?layout=", fromMaybe "false" layoutM, "&ackd=", show ackd, "&archived=", show archived]
      nextFetchUrl = case layoutM of
        Just "slider" -> Nothing
        _ ->
          if V.length issues < fLimit
            then Nothing
            else Just $ currentURL <> "&load_more=true&page=" <> show (pageInt + 1)
  let issuesVM = V.map (IssueVM False False currTime filterV) issues
  let issuesTable =
        Table
          { config = def{elemID = "anomalyListForm", addPadding = True}
          , columns = issueColumns pid
          , rows = issuesVM
          , features =
              def
                { rowId = Just \(IssueVM _ _ _ _ issue) -> Issues.issueIdText issue.id
                , bulkActions =
                    [ BulkAction{icon = Just "check", title = "acknowledge", uri = "/p/" <> pid.toText <> "/anomalies/bulk_actions/acknowledge"}
                    , BulkAction{icon = Just "inbox-full", title = "archive", uri = "/p/" <> pid.toText <> "/anomalies/bulk_actions/archive"}
                    ]
                , search = Just ClientSide
                , sort =
                    Just
                      $ SortConfig
                        { current = fromMaybe "events" sortM
                        , currentURL = currentURL
                        , options =
                            [ ("First Seen", "First time the issue occured", "first_seen")
                            , ("Last Seen", "Last time the issue occured", "last_seen")
                            , ("Events", "Number of events", "events")
                            ]
                        }
                , pagination = (,"both") <$> nextFetchUrl
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
                  , currentURL
                  , clientSide = False
                  , options =
                      [ TabFilterOpt "Inbox" Nothing Nothing
                      , TabFilterOpt "Acknowleged" Nothing Nothing
                      , TabFilterOpt "Archived" Nothing Nothing
                      ]
                  }
          }
  addRespHeaders $ case (layoutM, hxRequestM, hxBoostedM, loadM) of
    (Just "slider", Just "true", _, _) -> ALSlider currTime pid endpointM (Just $ V.map (IssueVM True False currTime filterV) issues)
    (_, _, _, Just "true") -> ALRows $ TableRows{nextUrl = nextFetchUrl, columns = issueColumns pid, rows = issuesVM, emptyState = Nothing} -- For load more - only rows
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
      $ V.mapM_ renderIssueForSlider issues
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
  [ col "" renderIssueCheckboxCol & withAttrs [class_ "h-4 flex space-x-3 w-8 items-center justify-center"]
  , col "Issue" (renderIssueMainCol pid) & withAttrs [class_ "flex-1 min-w-0"]
  , col "Events" renderIssueEventsCol & withAttrs [class_ "w-36 flex items-start justify-center"]
  , col "Activity" renderIssueChartCol & withAttrs [class_ "flex items-start justify-center"]
  ]


renderIssueCheckboxCol :: IssueVM -> Html ()
renderIssueCheckboxCol (IssueVM hideByDefault isWidget _ _ issue) =
  unless isWidget do
    a_ [class_ $ anomalyAccentColor (isJust issue.acknowledgedAt) (isJust issue.archivedAt) <> " w-2 h-full"] ""
    input_ [term "aria-label" "Select Issue", class_ "bulkactionItemCheckbox checkbox checkbox-md checked:checkbox-primary", type_ "checkbox", name_ "anomalyId", value_ $ Issues.issueIdText issue.id]


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

  do
    -- Title and badges row
    div_ [class_ $ "flex gap-3 mb-3 flex-wrap " <> if isWidget then "flex-col" else " items-center "] do
      h3_ [class_ "text-textStrong text-base"] $ toHtml issue.title

      -- Issue type badge
      div_ [class_ "flex items-center gap-2"] do
        -- Type badge
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

    -- Metadata row (method, endpoint, service, time)
    div_ [class_ "flex items-center gap-4 text-sm text-textWeak mb-3 flex-wrap"] do
      -- Method and endpoint (for API changes)
      when (issue.issueType == Issues.APIChange) do
        case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (apiData :: Issues.APIChangeData) -> do
            div_ [class_ "flex items-center gap-2"] do
              span_ [class_ $ "badge " <> methodFillColor apiData.endpointMethod] $ toHtml apiData.endpointMethod
              -- Endpoint path
              span_ [class_ "font-mono bg-fillWeak px-2 py-1 rounded text-xs text-textStrong"] $ toHtml apiData.endpointPath
          _ -> pass

      -- Service badge
      span_ [class_ "flex items-center gap-1"] do
        div_ [class_ "w-3 h-3 bg-fillYellow rounded-sm"] ""
        span_ [class_ "text-textStrong"] $ toHtml issue.service

      -- Time since
      span_ [class_ "text-textWeak"] $ toHtml timeSinceString

    -- Statistics row (only for API changes)
    when (issue.issueType == Issues.APIChange) do
      let requestChanges = getAeson issue.requestPayloads :: [Anomalies.PayloadChange]
      let responseChanges = getAeson issue.responsePayloads :: [Anomalies.PayloadChange]
      let allChanges = requestChanges ++ responseChanges
      let breakingChanges = length $ filter (\c -> c.changeType == Anomalies.Breaking) allChanges
      let incrementalChanges = length $ filter (\c -> c.changeType == Anomalies.Incremental) allChanges
      let totalChanges = length allChanges
      let affectedRequests = length requestChanges + length responseChanges

      div_ [class_ "flex items-center gap-4 text-sm mb-4 p-3 bg-fillWeak rounded-lg"] do
        span_ [class_ "text-textWeak"] do
          strong_ [class_ "text-textStrong"] $ toHtml $ show totalChanges
          " total changes"

        div_ [class_ "w-px h-4 bg-strokeWeak"] ""

        span_ [class_ "text-textWeak"] do
          strong_ [class_ "text-fillError-strong"] $ toHtml $ show breakingChanges
          " breaking"
          when (breakingChanges > 0 && totalChanges > 0) do
            span_ [class_ "text-xs ml-1 bg-fillError-weak text-fillError-strong px-1.5 py-0.5 rounded"] do
              toHtml $ show (round (fromIntegral breakingChanges / fromIntegral totalChanges * 100 :: Float) :: Int) <> "%"

        div_ [class_ "w-px h-4 bg-strokeWeak"] ""

        span_ [class_ "text-textWeak"] do
          strong_ [class_ "text-fillSuccess-strong"] $ toHtml $ show incrementalChanges
          " incremental"

        div_ [class_ "w-px h-4 bg-strokeWeak"] ""

        span_ [class_ "text-textWeak"] do
          strong_ [class_ "text-textBrand"] $ toHtml $ show affectedRequests
          " payloads affected"

    -- Stack trace for runtime exceptions or Query for alerts
    case issue.issueType of
      Issues.RuntimeException -> do
        case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (exceptionData :: Issues.RuntimeExceptionData) -> do
            div_ [class_ "border border-strokeError-weak rounded-lg group/er mb-4"] do
              label_ [class_ "text-sm text-textWeak font semibold rounded-lg p-2 flex gap-2 items-center"] do
                faSprite_ "chevron-right" "regular" "h-3 w-3 group-has-[.err-input:checked]/er:rotate-90"
                "Stack trace"
                input_ [class_ "err-input w-0 h-0 opacity-0", type_ "checkbox"]
              div_ [class_ "bg-fillError-weak p-4 overflow-x-scroll hidden group-has-[.err-input:checked]/er:block text-sm font-mono text-fillError-strong"] do
                pre_ [class_ "whitespace-pre-wrap "] $ toHtml exceptionData.stackTrace
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

    -- Recommended action
    div_ [class_ "border-l-4 border-strokeBrand pl-4 mb-4"] do
      p_ [class_ "text-sm text-textStrong leading-relaxed"] $ toHtml issue.recommendedAction

    -- Collapsible payload changes (only for API changes)
    when (issue.issueType == Issues.APIChange) do
      details_ [class_ "group mb-4"] do
        summary_ [class_ "inline-flex items-center cursor-pointer whitespace-nowrap text-sm font-medium transition-all rounded-md gap-1.5 text-textBrand hover:text-textBrand/80 list-none"] do
          faSprite_ "chevron-right" "regular" "h-4 w-4 mr-1 transition-transform group-open:rotate-90"
          "View detailed payload changes"

        -- Payload details content
        div_ [class_ "mt-4 border border-strokeWeak rounded-lg overflow-hidden bg-bgRaised"] do
          renderPayloadChanges issue

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
        [ class_
            $ "inline-flex items-center justify-center whitespace-nowrap text-sm font-medium transition-all h-8 rounded-md gap-1.5 px-3 "
            <> if isAcknowledged
              then "bg-fillSuccess-weak text-fillSuccess-strong border border-strokeSuccess-weak hover:bg-fillSuccess-weak/80"
              else "bg-fillPrimary text-textInverse-strong hover:bg-fillPrimary/90"
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
        [ class_
            $ "inline-flex items-center justify-center whitespace-nowrap text-sm font-medium transition-all h-8 rounded-md gap-1.5 px-3 "
            <> if isArchived
              then "bg-fillWarning-weak text-fillWarning-strong border border-strokeWarning-weak hover:bg-fillWarning-weak/80"
              else "border border-strokeWeak text-textStrong hover:bg-fillWeak"
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
        span_ [class_ "font-mono text-sm bg-fillWeak px-2 py-1 rounded text-textStrong"] $ toHtml fieldChange.path

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
              code_ [class_ "block bg-fillError-weak text-fillError-strong px-3 py-2 rounded text-xs font-mono whitespace-pre-wrap border border-strokeError-weak"] do
                toHtml $ fromMaybe "" fieldChange.oldValue
          when (isJust fieldChange.newValue) do
            div_ [] do
              span_ [class_ "text-xs text-textWeak block mb-1 font-medium"] "New Value:"
              code_ [class_ "block bg-fillSuccess-weak text-fillSuccess-strong px-3 py-2 rounded text-xs font-mono whitespace-pre-wrap border border-strokeSuccess-weak"] do
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
