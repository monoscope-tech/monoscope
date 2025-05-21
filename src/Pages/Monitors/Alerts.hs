module Pages.Monitors.Alerts (
  alertSingleGetH,
  convertToQueryMonitor,
  monitorCreatePostH,
  alertSingleToggleActiveH,
  alertListGetH,
  alertUpsertPostH,
  editAlert_,
  AlertUpsertForm (..),
  Alert (..),
  MonitorCreate (..),
)
where

import Data.CaseInsensitive qualified as CI
import Data.Default
import Data.Either.Extra (fromRight')
import Data.Time.Clock (UTCTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Time qualified as Time
import Lucid
import Lucid.Base
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Log qualified as LogList
import Pkg.Components qualified as Components
import Pkg.Parser (defSqlQueryCfg, finalAlertQuery, fixedUTCTime, parseQueryToComponents, presetRollup)
import Pkg.THUtils qualified as THUtils
import Relude
import System.Types
import Text.Slugify
import Utils
import Web.FormUrlEncoded (FromForm)


data AlertUpsertForm = AlertUpsertForm
  { alertId :: Maybe Text
  , alertThreshold :: Int
  , warningThreshold :: Maybe Text
  , recipientEmails :: [Text]
  , recipientSlacks :: [Text]
  , recipientEmailAll :: Maybe Bool
  , -- , checkIntervalMins :: Int
    direction :: Text
  , title :: Text
  , severity :: Text
  , subject :: Text
  , message :: Text
  , query :: Text
  , since :: Text
  , from :: Text
  , to :: Text
  -- TODO: support source for alerts. and hence alerts on traces, metrics etc
  -- , source :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


convertToQueryMonitor :: Projects.ProjectId -> UTCTime -> Monitors.QueryMonitorId -> AlertUpsertForm -> Monitors.QueryMonitor
convertToQueryMonitor projectId now queryMonitorId alertForm =
  -- FIXME: handle errors correctly, not crashing
  let sqlQueryCfg = (defSqlQueryCfg projectId fixedUTCTime Nothing Nothing){presetRollup = Just "5m"}
      (_, qc) = fromRight' $ parseQueryToComponents sqlQueryCfg alertForm.query
      warningThresholdInt = readMaybe . toString =<< alertForm.warningThreshold
      alertConfig =
        Monitors.MonitorAlertConfig
          { severity = alertForm.severity
          , title = alertForm.title
          , subject = alertForm.subject
          , message = alertForm.message
          , emails = V.fromList $ map CI.mk alertForm.recipientEmails
          , emailAll = fromMaybe False alertForm.recipientEmailAll
          , slackChannels = V.fromList alertForm.recipientSlacks
          }
   in Monitors.QueryMonitor
        { id = queryMonitorId
        , createdAt = now
        , updatedAt = now
        , projectId = projectId
        , checkIntervalMins = 0 -- alertForm.checkIntervalMins
        , alertThreshold = alertForm.alertThreshold
        , warningThreshold = warningThresholdInt
        , logQuery = alertForm.query
        , logQueryAsSql = fromMaybe "" qc.finalAlertQuery
        , lastEvaluated = now
        , warningLastTriggered = Nothing
        , alertLastTriggered = Nothing
        , triggerLessThan = alertForm.direction == "below"
        , thresholdSustainedForMins = 0 -- Placeholder, set according to your logic
        , alertConfig
        , deletedAt = Nothing
        , deactivatedAt = Nothing
        }


alertUpsertPostH :: Projects.ProjectId -> AlertUpsertForm -> ATAuthCtx (RespHeaders Alert)
alertUpsertPostH pid form = do
  let alertId = form.alertId >>= UUID.fromText
  queryMonitorId <- liftIO $ case alertId of
    Just alertId' -> pure (Monitors.QueryMonitorId alertId')
    Nothing -> Monitors.QueryMonitorId <$> UUID.nextRandom
  now <- Time.currentTime
  let queryMonitor = convertToQueryMonitor pid now queryMonitorId form

  _ <- dbtToEff $ Monitors.queryMonitorUpsert queryMonitor
  addSuccessToast "Monitor was updated successfully" Nothing
  addRespHeaders $ AlertNoContent ""


alertListGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders Alert)
alertListGetH pid = do
  monitors <- dbtToEff $ Monitors.queryMonitorsAll pid
  addRespHeaders $ AlertListGet monitors


alertSingleToggleActiveH :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATAuthCtx (RespHeaders Alert)
alertSingleToggleActiveH pid monitorId = do
  _ <- dbtToEff $ Monitors.monitorToggleActiveById monitorId

  monitors <- dbtToEff $ Monitors.queryMonitorsAll pid
  addRespHeaders $ AlertListGet monitors


alertSingleGetH :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATAuthCtx (RespHeaders Alert)
alertSingleGetH pid monitorId = do
  monitor <- dbtToEff $ Monitors.queryMonitorById monitorId
  addRespHeaders $ AlertSingle pid monitor


data Alert
  = AlertListGet (V.Vector Monitors.QueryMonitor)
  | AlertSingle Projects.ProjectId (Maybe Monitors.QueryMonitor)
  | AlertNoContent Text


instance ToHtml Alert where
  toHtml (AlertListGet monitors) = toHtml $ queryMonitors_ monitors
  toHtml (AlertSingle pid monitor) = toHtml $ alertSingleComp pid monitor
  toHtml (AlertNoContent msg) = toHtml msg
  toHtmlRaw = toHtml


alertSingleComp :: Projects.ProjectId -> Maybe Monitors.QueryMonitor -> Html ()
alertSingleComp pid monitor = do
  div_ [] do
    a_ [class_ "border-y p-3 block cursor-pointer", hxGet_ $ "/p/" <> pid.toText <> "/alerts", hxTarget_ "#alertsListContainer"] "‹ Back to alerts list"
    div_ [class_ "p-3"] $ editAlert_ pid monitor


editAlert_ :: Projects.ProjectId -> Maybe Monitors.QueryMonitor -> Html ()
editAlert_ pid monitorM = do
  let monitor = fromMaybe (def :: Monitors.QueryMonitor) monitorM
  let isNewMonitor = UUID.null monitor.id.unQueryMonitorId
  form_
    [ class_ "join join-vertical w-full max-w-3xl"
    , hxPost_ $ "/p/" <> pid.toText <> "/alerts"
    , hxVals_ "js:{query:getQueryFromEditor(), since: getTimeRange().since, from: getTimeRange().from, to:getTimeRange().to, cols:params().cols, layout:'all'}"
    , hxSwap_ "none"
    , termRaw "hx-on::after-request" "this.reset()"
    , [__|on intersection(intersecting) having threshold 0.5
              if intersecting
                 updateMarkAreas('reqsChartsEC',#warningThreshold.value, #alertThreshold.value)
                 set #custom_range_input's value to '24H'
                 then set #currentRange's innerText to 'Last 24 Hours'
                 then htmx.trigger('#log_explorer_form', 'submit')
              end
          on htmx:afterSettle from #reqsChartsECP
            updateMarkAreas('reqsChartsEC',#warningThreshold.value, #alertThreshold.value)
      |]
    ]
    do
      input_ [name_ "alertId", value_ $ if isNewMonitor then "" else monitor.id.toText, type_ "hidden"]

      div_ [class_ "flex gap-5 py-5"] do
        label_ [class_ " flex items-center gap-2 justify-between pl-5 text-lg pr-5"] "Alert Title"
        input_ [class_ "grow input ", type_ "text", placeholder_ "Title of alert", name_ "title", value_ monitor.alertConfig.title]

      div_ [class_ "collapse collapse-arrow join-item border border-base-300"] do
        input_ [class_ "", name_ "createAlertAccordion", checked_, type_ "radio", required_ ""]
        div_ [class_ "collapse-title text-xl font-medium  "] do
          span_ [class_ "badge badge-error mr-3"] "1"
          "Alert conditions"
        div_ [class_ "collapse-content"] do
          div_ [class_ "py-3"] do
            span_ "Trigger when the metric is "
            select_ [class_ "select inline-block mx-2 ", name_ "direction"] do
              option_ [selected_ ""] "above"
              option_ "below"
            span_ " the threshold for the last"
            select_
              [ class_ "select inline-block mx-2 "
              , [__| on change log me.value then
                      if me.value=='5' set :val to '24H' else if me.value=='60' set :val to '7D' end
                       then set #custom_range_input's value to :val
                       then set #currentRange's innerText to ('Last '+:val)
                       then htmx.trigger('#log_explorer_form', 'submit') |]
              ]
              do
                -- option_ [value_ "1"] "1 minute"
                option_ [value_ "5"] "5 minutes"
                option_ [value_ "60"] "1 hour"
          div_ [class_ "space-y-2"] do
            div_ [class_ "flex gap-5"] do
              label_ [class_ "flex items-center gap-2 w-1/3 justify-between pl-5"] do
                span_ [class_ ""] "Alert threshold"
                faSprite_ "chevron-right" "solid" "w-3 h-3"
              input_
                [ class_ "grow input input-error "
                , id_ "alertThreshold"
                , [__|on input updateMarkAreas('reqsChartsEC',#warningThreshold.value, #alertThreshold.value) |]
                , type_ "number"
                , placeholder_ "Enter value"
                , name_ "alertThreshold"
                , required_ ""
                , value_ $ if monitor.alertThreshold == 0 then "" else show monitor.alertThreshold
                ]
            div_ [class_ "flex gap-5"] do
              label_ [class_ " flex items-center gap-2 w-1/3 justify-between pl-5"] do
                span_ "Warning threshold"
                faSprite_ "chevron-right" "solid" "w-3 h-3"
              input_
                [ class_ "grow input input-warning"
                , id_ "warningThreshold"
                , [__|on input updateMarkAreas('reqsChartsEC',#warningThreshold.value, #alertThreshold.value) |]
                , type_ "number"
                , placeholder_ "optional"
                , name_ "warningThreshold"
                , value_ $ maybe "" show monitor.warningThreshold
                ]

      div_ [class_ "collapse collapse-arrow join-item border border-base-300"] do
        input_ [class_ "", name_ "createAlertAccordion", type_ "radio"]
        div_ [class_ "collapse-title text-xl font-medium "] do
          span_ [class_ "badge badge-error mr-3"] "2"
          "Alert Message"
        div_ [class_ "collapse-content space-y-4"] do
          fieldset_ [class_ "fieldset w-full"] do
            label_ [class_ "label"] "Severity"
            select_ [class_ "select w-full", name_ "severity"] do
              option_ "Info"
              option_ "Warning"
              option_ "Error"
              option_ "Critical"
          fieldset_ [class_ "fieldset w-full"] do
            label_ [class_ "label"] "Subject"
            input_ [placeholder_ "Error: Error subject", class_ "input w-full", name_ "subject", value_ monitor.alertConfig.subject]
          fieldset_ [class_ "fieldset w-full"] do
            label_ [class_ "label"] "Message"
            textarea_
              [placeholder_ "Alert Message", class_ "textarea textarea-md w-full", name_ "message"]
              $ toHtml
              $ if monitor.alertConfig.message == ""
                then [text| The alert's value is too high. Check the APItoolkit Alerts to debug |]
                else monitor.alertConfig.message

      div_ [class_ "collapse collapse-arrow join-item border border-base-300 space-y-4"] do
        input_ [class_ "", name_ "createAlertAccordion", type_ "radio"]
        div_ [class_ "collapse-title text-xl font-medium "] do
          span_ [class_ "badge badge-error mr-3"] "2"
          "Notification Channels"
        div_ [class_ "collapse-content"] do
          h4_ [class_ "text-lg"] "Add individuals, teams or channels that should be notified when this alert triggers"
          p_ "Alert rules with no recipients will still be triggered and can be viewed form the Changes and Errors page"
          section_ [class_ "relative space-y-4 space-x-4 py-3", id_ "recipientListParent"] do
            div_ [class_ "dropdown", id_ "addRecipientDropdown"] do
              div_
                [ tabindex_ "0"
                , role_ "button"
                , class_ "btn m-1"
                -- [__|on click toggle .dropdown-open on the closest .dropdown|]
                ]
                "Add recipient"
              ul_ [tabindex_ "0", style_ "bottom:100%;top:auto", class_ "bottom-full top-auto dropdown-content z-1 menu p-2 shadow-sm bg-base-100 rounded-box w-52 min-w-[15rem]"] do
                li_ $ a_ [[__|on click put #addRecipientEmailAllTmpl.innerHTML after #addRecipientDropdown then _hyperscript.processNode(#recipientListParent) |]] "Email everyone"
                li_ $ a_ [[__|on click put #addRecipientEmailTmpl.innerHTML after #addRecipientDropdown then _hyperscript.processNode(#recipientListParent) |]] "Email ..."
                li_ $ a_ [[__|on click put #addRecipientSlackTmpl.innerHTML after #addRecipientDropdown then _hyperscript.processNode(#recipientListParent) |]] "To default Slack channel"
            when monitor.alertConfig.emailAll addRecipientEmailAllTmpl_
            forM_ monitor.alertConfig.emails addRecipientEmailTmpl_
            forM_ monitor.alertConfig.slackChannels addRecipientSlackTmpl_

      div_ [class_ "py-5"] do
        button_ [type_ "submit", class_ "btn btn-success"]
          $ if isNewMonitor
            then "Create Alert"
            else "Update Alert"

  template_ [id_ "addRecipientSlackTmpl"] $ addRecipientSlackTmpl_ ""
  template_ [id_ "addRecipientEmailTmpl"] $ addRecipientEmailTmpl_ (CI.mk "")
  template_ [id_ "addRecipientEmailAllTmpl"] addRecipientEmailAllTmpl_


addRecipientSlackTmpl_ :: Text -> Html ()
addRecipientSlackTmpl_ channel =
  label_ [class_ "input inline-flex items-center gap-2"] do
    "Slack"
    input_ [class_ "grow", class_ "input", placeholder_ "#channelName", type_ "text", required_ "", name_ "recipientSlack"]
    a_ [class_ "badge badge-base", [__|on click remove the closest parent <label/>|]] $ faSprite_ "xmark" "solid" "w-3 h-3"


addRecipientEmailTmpl_ :: CI.CI Text -> Html ()
addRecipientEmailTmpl_ email =
  label_ [class_ "input inline-flex items-center gap-2"] do
    "Email"
    input_ [class_ "grow", class_ "input", placeholder_ "name@site.com", type_ "email", required_ "", name_ "recipientEmail"]
    a_ [class_ "badge badge-base", [__|on click remove the closest parent <label/>|]] $ faSprite_ "xmark" "solid" "w-3 h-3"


addRecipientEmailAllTmpl_ :: Html ()
addRecipientEmailAllTmpl_ =
  label_ [class_ "input inline-flex items-center gap-2"] do
    "Email Everyone"
    input_ [class_ "grow", class_ "input", placeholder_ "name@site.com", type_ "hidden", value_ "True", name_ "recipientEmailAll"]
    a_ [class_ "badge badge-base", [__|on click remove the closest parent <label/>|]] $ faSprite_ "xmark" "solid" "w-3 h-3"


queryMonitors_ :: V.Vector Monitors.QueryMonitor -> Html ()
queryMonitors_ monitors = do
  table_ [class_ "table text-lg min-w-[65ch]"] do
    thead_ $ tr_ do
      th_ "Title"
      th_ ""
    tbody_ do
      V.forM_ monitors \monitor -> tr_ do
        let editAction =
              [__|
on click
if ('URLSearchParams' in window)
    make a URLSearchParams from window.location.search called :searchParams
    call :searchParams.set("foo", "bar")
    set :newRelativePathQuery to window.location.pathname + '?' + :searchParams.toString()
    call history.pushState(null, '', newRelativePathQuery)
end
            |]
        let editURI = "/p/" <> monitor.projectId.toText <> "/alerts/" <> monitor.id.toText
        td_
          [ class_ $ if isJust monitor.deactivatedAt then "line-through" else ""
          , hxTarget_ "#alertsListContainer"
          , hxGet_ editURI
          , editAction
          ]
          $ toHtml monitor.alertConfig.title
        td_ [] do
          a_
            [ class_ "btn btn-ghost btn-xs"
            , hxTarget_ "#alertsListContainer"
            , hxGet_ editURI
            , editAction
            ]
            "edit"
          a_
            [ class_ "btn btn-ghost btn-xs"
            , hxTarget_ "#alertsListContainer"
            , hxPost_ $ "/p/" <> monitor.projectId.toText <> "/alerts/" <> monitor.id.toText <> "/toggle_active"
            ]
            if isJust monitor.deactivatedAt then "reactivate" else "deactivate"


----------------------------------------------------------------------------
-- Monitor create Post
----------------------------------------------------------------------------

data MonitorCreate = MCSelectType | MCMetric Projects.ProjectId Text | MCAPITests Projects.ProjectId Text


alertConditions_ :: Html ()
alertConditions_ = div_ [class_ "space-y-4"] $ do
  triggerCondition
  div_ [class_ "space-y-2"] do
    thresholdInput_ ">" "Alert" (Just "red-500") "Enter value"
    thresholdInput_ ">" "Warning" (Just "yellow-500") "Optional"
  div_ [class_ "flex items-center space-x-2"] $ do
    span_ "If data is missing for 5 minutes"
    select_ [class_ selectClass] $ option_ "Show last known status"
  details_ [class_ "mt-6"] $ do
    summary_ [class_ "cursor-pointer"] "Advanced options"
    div_ [class_ "mt-4 space-y-4 "] $ recoveryThresholds >> evaluationOptions


triggerCondition :: Html ()
triggerCondition = div_ [class_ "flex items-center space-x-2"] $ do
  span_ "Trigger when the evaluated value is"
  select_ [class_ selectClass] $ option_ "above"
  span_ "the threshold"


thresholdInput_ :: Text -> Text -> Maybe Text -> Text -> Html ()
thresholdInput_ sign label colorM placeholder = div_ [class_ "flex items-center space-x-4"] $ do
  whenJust colorM \color -> div_ [class_ $ "w-1 h-6 rounded-sm bg-" <> color] mempty
  span_ [class_ "w-52 inline-block"] $ toHtml $ label <> " threshold:"
  div_ [class_ "space-x-5"] do
    span_ $ toHtml sign
    input_ [type_ "text", class_ inputClass, placeholder_ placeholder]


recoveryThresholds :: Html ()
recoveryThresholds = div_ [class_ "border-l-2 border-l-slate-300 pl-4 space-y-2"] do
  h3_ [class_ "font-normal text-base"] "Recovery Thresholds"
  thresholdInput_ "<=" "Alert recovery" Nothing "Optional"
  thresholdInput_ "<=" "Warning recovery" Nothing "Optional"


evaluationOptions :: Html ()
evaluationOptions = div_ [class_ "border-l-2 border-l-slate-300 pl-4 space-y-2"] do
  h3_ [class_ "font-normal text-base"] "Evaluation options"
  div_ [class_ "flex items-center space-x-2"] $ do
    span_ "Delay monitor evaluation by"
    input_ [type_ "text", class_ $ inputClass <> " w-16", value_ "0"]
    span_ "seconds."
  div_ [class_ "flex items-center space-x-2"] $ do
    select_ [class_ selectClass] $ option_ "Do not require"
    span_ "a full window of data for evaluation."


selectClass, inputClass :: Text
selectClass = "border border-gray-300 rounded-sm px-2 py-1"
inputClass = "border border-gray-300 rounded-sm px-2 py-1"


instance ToHtml MonitorCreate where
  toHtmlRaw = toHtml
  toHtml MCSelectType = toHtml monitorSelectType_
  toHtml (MCMetric pid metricType) = toHtml $ monitorMetric_ pid Nothing
  toHtml (MCAPITests pid metricType) = toHtml $ monitorAPITests_ pid Nothing


monitorAPITests_ :: Projects.ProjectId -> Maybe Monitors.QueryMonitor -> Html ()
monitorAPITests_ pid monitorM = do
  div_ "api tests"


monitorMetric_ :: Projects.ProjectId -> Maybe Monitors.QueryMonitor -> Html ()
monitorMetric_ pid monitorM = section_ [class_ "px-8 py-5 space-y-5 group/pg overflow-y-scroll h-full"] do
  let monitor = fromMaybe (def :: Monitors.QueryMonitor) monitorM
  -- let isNewMonitor = UUID.null monitor.id.unQueryMonitorId

  div_
    [ id_ "reqsChartsECP"
    , class_ "px-5 mt-5 aspect-4/1"
    , hxGet_ $ "/charts_html?id=reqsChartsEC&show_legend=true&pid=" <> pid.toText
    , hxTrigger_ "load,  htmx:beforeRequest from:#log_explorer_form"
    , hxVals_ "js:{query_raw:window.getQueryFromEditor(), since: getTimeRange().since, from: getTimeRange().from, to:getTimeRange().to, cols:params().cols, layout:'all', source: params().source}"
    , hxSwap_ "innerHTML"
    ]
    ""
  ul_ [class_ "timeline timeline-snap-icon timeline-vertical timeline-compact"] do
    li_ [] do
      div_ [class_ "timeline-middle"] $ span_ [class_ "inline-block rounded-full bg-primary text-base-100 h-7 w-7 flex items-center justify-center "] "1"
      div_ [class_ "timeline-end space-y-5"] do
        h2_ [class_ ""] do
          faSprite_ "chevron-down" "regular" "h-4 w-4 mx-2"
          span_ [] "Choose the detection method"
        div_ [class_ "pl-8 pb-8 space-y-3"] do
          div_ [class_ "join"] do
            button_ [class_ "btn btn-xs btn-primary"] $ faSprite_ "chart-waterfall" "regular" "w-3 h-3" >> "Threshold Alert"
            button_ [class_ "btn btn-xs"] $ faSprite_ "chart-line-up-down" "regular" "w-3 h-3" >> "Change Alert"
          p_ [class_ "text-xs"] do
            span_ [] "An alert is triggered whenever a "
            a_ [class_ "underline"] "metric crosses a threshold"
      hr_ []
    li_ [] do
      hr_ []
      div_ [class_ "timeline-middle"] $ span_ [class_ "inline-block rounded-full bg-primary text-base-100 h-7 w-7 flex items-center justify-center "] "2"
      div_ [class_ "timeline-end space-y-5"] do
        h2_ [class_ ""] do
          faSprite_ "chevron-down" "regular" "h-4 w-4 mx-2"
          span_ [] "Define the metric"
        div_ [class_ "pl-8 pb-8 space-y-3 min-w-[750px]"] do
          LogList.logQueryBox_ pid Nothing "requests" Nothing "{}" V.empty V.empty
          div_ [class_ "border-l-2 border-l-slate-300 pl-4 space-y-2"] do
            h3_ [class_ "font-normal text-base"] "Evaluation Details"
            div_ [class_ "flex items-center gap-2"] do
              fieldset_ [class_ "fieldset"] do
                label_ [class_ "label"] "Evaluate the"
                select_ [class_ "select select-xs "] $ mapM_ (option_ []) ["average", "maximum", "minimum", "sum"]
              fieldset_ [class_ "fieldset"] do
                label_ [class_ "label label-text"] "Of the query over the"
                select_ [class_ "select select-xs "] $ mapM_ (option_ []) ["last 5 minutes", "last 10minutes", "last 15minutes", "last 30minutes", "last 1 hour", "last 1 day", "last 1 week"]
      hr_ []
    li_ [] do
      hr_ []
      div_ [class_ "timeline-middle"] $ span_ [class_ "inline-block rounded-full bg-primary text-base-100 h-7 w-7 flex items-center justify-center "] "3"
      div_ [class_ "timeline-end space-y-5"] do
        h2_ [class_ ""] do
          faSprite_ "chevron-down" "regular" "h-4 w-4 mx-2"
          span_ [] "Set alert conditions"
        div_ [class_ "pl-8 pb-8 space-y-3 "] alertConditions_
      hr_ []
    li_ [] do
      hr_ []
      div_ [class_ "timeline-middle"] $ span_ [class_ "inline-block rounded-full bg-primary text-base-100 h-7 w-7 flex items-center justify-center "] "4"
      div_ [class_ "timeline-end space-y-5"] do
        h2_ [class_ ""] do
          faSprite_ "chevron-down" "regular" "h-4 w-4 mx-2"
          span_ [] "Configure notification message"
        div_ [class_ "pl-8 pb-8 space-y-3 "] do
          div_ [class_ "space-y-4"] do
            fieldset_ [class_ "fieldset w-full"] do
              label_ [class_ "label"] "Severity"
              select_ [class_ "select select-xs w-full", name_ "severity"] do
                option_ "Info"
                option_ "Warning"
                option_ "Error"
                option_ "Critical"
            fieldset_ [class_ "fieldset w-full"] do
              label_ [class_ "label"] "Subject"
              input_ [placeholder_ "Error: Error subject", class_ "input input-xs nw-full", name_ "subject", value_ monitor.alertConfig.subject]
            fieldset_ [class_ "fieldset w-full"] do
              label_ [class_ "label"] "Message"
              textarea_
                [placeholder_ "Alert Message", class_ "textarea  textarea-xs w-full", name_ "message"]
                $ toHtml
                $ if monitor.alertConfig.message == ""
                  then [text| The alert's value is too high. Check the APItoolkit Alerts to debug |]
                  else monitor.alertConfig.message

            div_ [class_ "border-l-2 border-l-slate-300 pl-4 space-y-2"] do
              h3_ [class_ "font-normal text-base"] "Recovery Thresholds"
              p_ [] "Send notifications for alert status periodically as long as the monitor has not recovered"
              div_ [class_ "flex items-center gap-2"] do
                input_ [class_ "toggle toggle-sm", type_ "checkbox", name_ "notifyAfterCheck"]
                span_ "If this monitor is not acknowleged or resoved, notify renotify every"
                select_ [class_ "select select-xs ", name_ "notifyAfter"] $ mapM_ (option_ []) ["10mins", "20mins", "30mins", "1hour", "6hours", "24hours"]
              div_ [class_ "flex items-center gap-2"] do
                input_ [class_ "toggle toggle-sm", type_ "checkbox", name_ "stopAfterCheck"]
                span_ "Stop renotifying after "
                input_ [type_ "text", value_ "0", name_ "stopAfter"]
                span_ "occurences."
      hr_ []
    li_ [] do
      hr_ []
      div_ [class_ "timeline-middle"] $ span_ [class_ "inline-block rounded-full bg-primary text-base-100 h-7 w-7 flex items-center justify-center "] "5"
      div_ [class_ "timeline-end space-y-5"] do
        h2_ [class_ ""] do
          faSprite_ "chevron-down" "regular" "h-4 w-4 mx-2"
          span_ [] "Configure notification Channels"
        div_ [class_ "pl-8 pb-8 space-y-3 "] do
          div_ do
            p_ "Add individuals, teams or channels that should be notified when this alert triggers"
            p_ "Alert rules with no recipients will still be triggered and can be viewed form the Changes and Errors page"
          section_ [class_ "relative space-y-4 space-x-4 py-3", id_ "recipientListParent"] do
            div_ [class_ "dropdown", id_ "addRecipientDropdown"] do
              div_
                [ tabindex_ "0"
                , role_ "button"
                , class_ "btn m-1"
                -- [__|on click toggle .dropdown-open on the closest .dropdown|]
                ]
                "Add recipient"
              ul_ [tabindex_ "0", style_ "bottom:100%;top:auto", class_ "bottom-full top-auto dropdown-content z-1 menu p-2 shadow-sm bg-base-100 rounded-box w-52 min-w-[15rem]"] do
                li_ $ a_ [[__|on click put #addRecipientEmailAllTmpl.innerHTML after #addRecipientDropdown then _hyperscript.processNode(#recipientListParent) |]] "Email everyone"
                li_ $ a_ [[__|on click put #addRecipientEmailTmpl.innerHTML after #addRecipientDropdown then _hyperscript.processNode(#recipientListParent) |]] "Email ..."
                li_ $ a_ [[__|on click put #addRecipientSlackTmpl.innerHTML after #addRecipientDropdown then _hyperscript.processNode(#recipientListParent) |]] "To default Slack channel"
            when monitor.alertConfig.emailAll addRecipientEmailAllTmpl_
            forM_ monitor.alertConfig.emails addRecipientEmailTmpl_
            forM_ monitor.alertConfig.slackChannels addRecipientSlackTmpl_
            template_ [id_ "addRecipientSlackTmpl"] $ addRecipientSlackTmpl_ ""
            template_ [id_ "addRecipientEmailTmpl"] $ addRecipientEmailTmpl_ (CI.mk "")
            template_ [id_ "addRecipientEmailAllTmpl"] addRecipientEmailAllTmpl_
  div_ [class_ "sticky bottom-4 p-5 bg-white border-t border-slate-500 flex space-between justify-between items-center"] do
    h3_ "Complete all steps to proceed"
    div_ [class_ "flex gap-5 mr-6"] do
      button_ [class_ "btn btn-sm btn-outline"] "Test Notifications"
      button_ [class_ "btn btn-sm btn-secondary"] "Create"


monitorSelectType_ :: Html ()
monitorSelectType_ = section_ [class_ "px-8 py-5 space-y-5 group/pg"] do
  h2_ [class_ "text-2xl py-3"] "Select a Monitor type"
  div_ [class_ "grid grid-cols-5"] do
    div_ [class_ "col-span-1"] do
      h3_ [class_ ""] "Issues"
      mapM_ (inputRadio_ "monitor-type") ["Errors", "API Changes"]
      h3_ [class_ "mt-5"] "Synthetics"
      mapM_ (inputRadio_ "monitor-type") ["Healthcheck/Pings", "Multistep API test"]
      h3_ [class_ "mt-5"] "Logs"
      mapM_ (inputRadio_ "monitor-type") ["Throughput", "Error Logs"]
    -- h3_ [class_ "mt-5"] "Performance"
    -- mapM_ (inputRadio_ "monitor-type") ["Latency"]
    div_ [class_ "col-span-3"] do
      monitorTypeDetail_
        "Errors"
        $( THUtils.markdown
             [text|
           Errors are groups of exceptions that have a similar stacktrace. Set an alert for new issues, when an issue changes state, frequency of errors, or users affected by an issue.
           ![](/public/pages/monitors/alerts-wizard-issues.022643ec283774262884.svg)
           Examples
            - When an errorr was seen over 100times in the last 2 days
            - Send an email and a slack message to the team
          |]
         )
        "?monitor-type=errors"
      monitorTypeDetail_
        "API changes"
        $( THUtils.markdown
             [text|
           Errors are groups of exceptions that have a similar stacktrace. Set an alert for new issues, when an issue changes state, frequency of errors, or users affected by an issue.
           ![](/public/pages/monitors/alerts-wizard-issues.022643ec283774262884.svg)
           Examples
            - When an errorr was seen over 100times in the last 2 days
            - Send an email and a slack message to the team
          |]
         )
        "?monitor-type=errors"
      monitorTypeDetail_
        "Healthcheck/Pings"
        $( THUtils.markdown
             [text|
           Errors are groups of exceptions that have a similar stacktrace. Set an alert for new issues, when an issue changes state, frequency of errors, or users affected by an issue.
           ![](/public/pages/monitors/alerts-wizard-issues.022643ec283774262884.svg)
           Examples
            - When an errorr was seen over 100times in the last 2 days
            - Send an email and a slack message to the team
          |]
         )
        "?monitor-type=healthcheckpings"
      monitorTypeDetail_
        "Multistep API test"
        $( THUtils.markdown
             [text|
           Errors are groups of exceptions that have a similar stacktrace. Set an alert for new issues, when an issue changes state, frequency of errors, or users affected by an issue.
           ![](/public/pages/monitors/alerts-wizard-issues.022643ec283774262884.svg)
           Examples
            - When an errorr was seen over 100times in the last 2 days
            - Send an email and a slack message to the team
          |]
         )
        ""
      monitorTypeDetail_
        "Throughput"
        $( THUtils.markdown
             [text|
           Errors are groups of exceptions that have a similar stacktrace. Set an alert for new issues, when an issue changes state, frequency of errors, or users affected by an issue.
           ![](/public/pages/monitors/alerts-wizard-issues.022643ec283774262884.svg)
           Examples
            - When an errorr was seen over 100times in the last 2 days
            - Send an email and a slack message to the team
          |]
         )
        ""
      monitorTypeDetail_
        "Error Logs"
        $( THUtils.markdown
             [text|
           Errors are groups of exceptions that have a similar stacktrace. Set an alert for new issues, when an issue changes state, frequency of errors, or users affected by an issue.
           ![](/public/pages/monitors/alerts-wizard-issues.022643ec283774262884.svg)
           Examples
            - When an errorr was seen over 100times in the last 2 days
            - Send an email and a slack message to the team
          |]
         )
        ""


monitorTypeDetail_ :: Text -> Html () -> Text -> Html ()
monitorTypeDetail_ title content uri = do
  let slug = slugify title
  div_ [class_ [text|border divide-y divide-gray-100 rounded-md hidden group-has-[.${slug}:checked]/pg:block |]] do
    h3_ [class_ "bg-base-200 px-4 py-2 text-lg"] $ toHtml title
    div_ [class_ "prose px-4 py-4"] content
    div_ [class_ "text-right px-4 py-3"] $ a_ [class_ "btn btn-sm btn-success", href_ uri] "Continue"


inputRadio_ :: Text -> Text -> Html ()
inputRadio_ name lbel = fieldset_ [class_ "fieldset"] $ label_ [class_ "label cursor-pointer justify-start items-start gap-2"] do
  input_ ([type_ "radio", name_ name, class_ $ "radio radio-xs " <> slugify lbel] <> [checked_ | lbel == "Errors"])
  span_ [class_ ""] $ toHtml lbel


monitorCreatePostH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx MonitorCreate))
monitorCreatePostH pid monitorType = do
  (sess, project) <- Sessions.sessionAndProject pid
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Create Monitor"
          , pageActions = Just $ Components.timepicker_ (Just "log_explorer_form") Nothing
          }
  case monitorType of
    Just "errors" -> addRespHeaders $ PageCtx bwconf (MCMetric pid "errors")
    Just "healthcheckpings" -> addRespHeaders $ PageCtx bwconf (MCAPITests pid "healthcheckpings")
    _ -> addRespHeaders $ PageCtx bwconf MCSelectType
