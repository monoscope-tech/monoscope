module Pages.Monitors.MetricMonitors (monitorCreateGetH, MonitorCreate, configureNotificationMessage_, configureNotificationChannels_) where

import Data.Default
import Data.List qualified as L
import Data.Vector qualified as V
import Lucid
import Lucid.Htmx
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Projects qualified as Projects
import Models.Tests.Testing qualified as Testing
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Log qualified as LogList
import Pkg.Components.ItemsList qualified as Components
import Pkg.Components.LogQueryBox (LogQueryBoxConfig (..))
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.THUtils qualified as THUtils
import Relude
import System.Types
import Text.Slugify
import Utils


data MonitorCreate
  = MCSelectType
  | MCMetric Projects.ProjectId Text
  | MCAPITests Projects.ProjectId Text


data MonitorType = MonitorType
  { label :: Text
  , group :: Text
  , content :: Html ()
  , uri :: Text
  }


monitorCreateGetH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx MonitorCreate))
monitorCreateGetH pid monitorType = do
  (sess, project) <- Sessions.sessionAndProject pid
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Create Monitor"
          , prePageTitle = Just "Monitors & Alerts"
          , pageActions = Just $ TimePicker.timepicker_ (Just "log_explorer_form") Nothing
          }
  case monitorType of
    Just "errors" -> addRespHeaders $ PageCtx bwconf (MCMetric pid "errors")
    Just "healthcheckpings" -> addRespHeaders $ PageCtx bwconf (MCAPITests pid "healthcheckpings")
    _ -> addRespHeaders $ PageCtx bwconf MCSelectType


selectClass, inputClass :: Text
selectClass = "border border-gray-300 rounded-sm px-2 py-1"
inputClass = "border border-gray-300 rounded-sm px-2 py-1"


monitorTypes :: [MonitorType]
monitorTypes =
  [ MonitorType "Errors" "Issues" errorsContent "?monitor-type=errors"
  , MonitorType "API Changes" "Issues" apiChangesContent "?monitor-type=api-changes"
  , MonitorType "Healthcheck/Pings" "Synthetics" healthcheckContent "?monitor-type=healthcheckpings"
  , MonitorType "Multistep API test" "Synthetics" multistepApiTestContent "?monitor-type=multistep-api-test"
  , MonitorType "Throughput" "Logs" throughputContent "?monitor-type=throughput"
  , MonitorType "Error Logs" "Logs" errorLogsContent "?monitor-type=error-logs"
  ]


errorsContent, apiChangesContent, healthcheckContent, multistepApiTestContent, throughputContent, errorLogsContent :: Html ()
errorsContent =
  $( THUtils.markdown
       [text|
           Errors are groups of exceptions that have a similar stacktrace. Set an alert for new issues, when an issue changes state, frequency of errors, or users affected by an issue.
           ![](/public/pages/monitors/alerts-wizard-issues.022643ec283774262884.svg)
           Examples
            - When an errorr was seen over 100times in the last 2 days
            - Send an email and a slack message to the team
          |]
   )
apiChangesContent =
  $( THUtils.markdown
       [text|
           Errors are groups of exceptions that have a similar stacktrace. Set an alert for new issues, when an issue changes state, frequency of errors, or users affected by an issue.
           ![](/public/pages/monitors/alerts-wizard-issues.022643ec283774262884.svg)
           Examples
            - When an errorr was seen over 100times in the last 2 days
            - Send an email and a slack message to the team
          |]
   )
healthcheckContent =
  $( THUtils.markdown
       [text|
           Errors are groups of exceptions that have a similar stacktrace. Set an alert for new issues, when an issue changes state, frequency of errors, or users affected by an issue.
           ![](/public/pages/monitors/alerts-wizard-issues.022643ec283774262884.svg)
           Examples
            - When an errorr was seen over 100times in the last 2 days
            - Send an email and a slack message to the team
          |]
   )
multistepApiTestContent =
  $( THUtils.markdown
       [text|
           Errors are groups of exceptions that have a similar stacktrace. Set an alert for new issues, when an issue changes state, frequency of errors, or users affected by an issue.
           ![](/public/pages/monitors/alerts-wizard-issues.022643ec283774262884.svg)
           Examples
            - When an errorr was seen over 100times in the last 2 days
            - Send an email and a slack message to the team
          |]
   )
throughputContent =
  $( THUtils.markdown
       [text|
           Errors are groups of exceptions that have a similar stacktrace. Set an alert for new issues, when an issue changes state, frequency of errors, or users affected by an issue.
           ![](/public/pages/monitors/alerts-wizard-issues.022643ec283774262884.svg)
           Examples
            - When an errorr was seen over 100times in the last 2 days
            - Send an email and a slack message to the team
          |]
   )
errorLogsContent =
  $( THUtils.markdown
       [text|
           Errors are groups of exceptions that have a similar stacktrace. Set an alert for new issues, when an issue changes state, frequency of errors, or users affected by an issue.
           ![](/public/pages/monitors/alerts-wizard-issues.022643ec283774262884.svg)
           Examples
            - When an errorr was seen over 100times in the last 2 days
            - Send an email and a slack message to the team
          |]
   )


-- Timeline steps

timelineSteps :: Projects.ProjectId -> Components.TimelineSteps
timelineSteps pid =
  Components.TimelineSteps
    [ Components.TimelineStep "Choose the detection method" chooseDetectionMethod_
    , Components.TimelineStep "Define the metric" (defineTheMetric_ pid)
    , Components.TimelineStep "Set alert conditions" alertConditions_
    , -- , Components.TimelineStep "Configure notification message" configureNotificationMessage_
      Components.TimelineStep "Configure notification Channels" configureNotificationChannels_
    ]
    Nothing


-- Content placeholders for TimelineSteps

chooseDetectionMethod_ :: Html ()
chooseDetectionMethod_ = do
  div_ [class_ "join"] do
    button_ [class_ "btn btn-xs btn-primary"] $ faSprite_ "chart-waterfall" "regular" "w-3 h-3" >> "Threshold Alert"
    button_ [class_ "btn btn-xs"] $ faSprite_ "chart-line-up-down" "regular" "w-3 h-3" >> "Change Alert"
  p_ [class_ "text-xs"] do
    span_ [] "An alert is triggered whenever a "
    a_ [class_ "underline"] "metric crosses a threshold"


defineTheMetric_ :: Projects.ProjectId -> Html ()
defineTheMetric_ pid = do
  div_ [class_ " max-w-[750px]"] do
    LogList.logQueryBox_
      LogQueryBoxConfig
        { pid = pid
        , currentRange = Nothing
        , source = Just "requests"
        , targetSpan = Nothing
        , query = Nothing
        , vizType = Nothing
        , queryLibRecent = V.empty
        , queryLibSaved = V.empty
        , updateUrl = False
        , targetWidgetPreview = Nothing
        }
  div_ [class_ "border-l-2 border-l-slate-300 pl-4 space-y-2"] do
    h3_ [class_ "font-normal text-base"] "Evaluation Details"
    div_ [class_ "flex items-center gap-2"] do
      fieldset_ [class_ "fieldset"] do
        label_ [class_ "label"] "Evaluate the"
        select_ [class_ "select select-xs "] $ mapM_ (option_ []) ["average", "maximum", "minimum", "sum"]
      fieldset_ [class_ "fieldset"] do
        label_ [class_ "label"] "Of the query over the"
        select_ [class_ "select select-xs "] $ mapM_ (option_ []) ["last 5 minutes", "last 10minutes", "last 15minutes", "last 30minutes", "last 1 hour", "last 1 day", "last 1 week"]


configureNotificationMessage_ :: Maybe Testing.Collection -> Html ()
configureNotificationMessage_ colM = do
  let (severity, subject, message, naf, saf, nfc, sfc) = case colM of
        Just col -> (col.alertSeverity, col.alertSubject, col.alertMessage, col.notifyAfter, col.stopAfter, col.notifyAfterCheck, col.stopAfterCheck)
        Nothing -> ("Info", "Error: Error subject", "Alert Message", "10 minutes", "0", False, False)
  div_ [class_ "space-y-4 bg-fillWeaker p-4 rounded-2xl"] do
    div_ [class_ "p-4 bg-slate-50 rounded-xl"] do
      div_ [class_ "flex items-center w-full gap-2"] do
        fieldset_ [class_ "fieldset"] do
          label_ [class_ "label"] $ span_ [class_ "label-text font-medium"] "Severity"
          select_ [class_ "select select-sm shadow-none w-28", name_ "alertSeverity"] do
            option_ [selected_ "" | severity == "Info"] "Info"
            option_ [selected_ "" | severity == "Warning"] "Warning"
            option_ [selected_ "" | severity == "Error"] "Error"
            option_ [selected_ "" | severity == "Critical"] "Critical"
        fieldset_ [class_ "fieldset w-full"] do
          label_ [class_ "label font-medium"] "Subject"
          input_ [placeholder_ "Error: Error subject", class_ "input shadow-none input-sm w-full", name_ "alertSubject", value_ subject]
      fieldset_ [class_ "fieldset w-full my-3"] do
        label_ [class_ "label"] "Message"
        textarea_
          [placeholder_ "Alert Message", class_ "textarea  shadow-none p-2 rounded-2xl textarea-xs w-full", name_ "alertMessage", value_ message]
          $ toHtml message
      div_ [class_ "space-y-2 py-4"] do
        h3_ [class_ "text-slate-600 font-medium"] "Recovery Thresholds"
        p_ [class_ "text-sm font-medium"] "Send notifications for alert status periodically as long as the monitor has not recovered"
        div_ [class_ "flex items-center gap-2 pt-4"] do
          input_ $ [class_ "checkbox checkbox-sm", type_ "checkbox", name_ "notifyAfterCheck"] ++ [checked_ | nfc]
          span_ "If this monitor is not acknowleged or resoved, notify renotify every"
          select_ [class_ "select select-xs shadow-none", name_ "notifyAfter"]
            $ mapM_ (\v -> option_ [selected_ "" | v == naf] $ toHtml v) ["10 mins", "20 mins", "30 mins", "1 hour", "6 hours", "24 hours"]
        div_ [class_ "flex items-center gap-2"] do
          input_ $ [class_ "checkbox checkbox-sm", type_ "checkbox", name_ "stopAfterCheck"] ++ [checked_ | sfc]
          span_ "Stop renotifying after "
          input_ [type_ "number", class_ "input input-xs shadow-none w-20", value_ saf, name_ "stopAfter"]
          span_ "occurences."


configureNotificationChannels_ :: Html ()
configureNotificationChannels_ = do
  -- let monitor = (def :: Monitors.QueryMonitor)
  div_ do
    p_ [class_ "space-x-2"] do
      "Run your test every"
      input_ [class_ "input w-24 text-center", type_ "number", value_ "1", name_ "scheduleCount"]
      select_ [class_ "select ", name_ "scheduleUnits"] do
        option_ "seconds"
        option_ "minutes"
        option_ "hours"
        option_ "days"


-- Helper functions

groupedMonitorTypes :: [MonitorType] -> [(Text, [MonitorType])]
groupedMonitorTypes = map toGroup . L.groupBy (\a b -> a.group == b.group)
  where
    toGroup [] = error "Unexpected empty group"
    toGroup ms@(m : _) = (m.group, ms)


inputRadio_ :: Text -> Text -> Html ()
inputRadio_ name label = fieldset_ [class_ "fieldset"] $ label_ [class_ "label cursor-pointer justify-start items-start gap-2"] $ do
  input_ ([type_ "radio", name_ name, class_ $ "radio radio-xs " <> slugify label] <> [checked_ | label == "Errors"])
  span_ [class_ "label-text"] $ toHtml label


monitorTypeDetail_ :: MonitorType -> Html ()
monitorTypeDetail_ m = do
  let slug = slugify m.label
  div_ [class_ [text|border divide-y divide-gray-100 rounded-md hidden group-has-[.${slug}:checked]/pg:block |]] $ do
    h3_ [class_ "bg-base-200 px-4 py-2 text-lg"] $ toHtml m.label
    div_ [class_ "prose px-4 py-4"] m.content
    div_ [class_ "text-right px-4 py-3"] $ a_ [class_ "btn btn-sm btn-success", href_ m.uri] "Continue"


-- Reusable components

thresholdInput_ :: Text -> Text -> Maybe Text -> Text -> Html ()
thresholdInput_ sign label colorM placeholder = div_ [class_ "flex items-center space-x-4"] $ do
  whenJust colorM \color -> div_ [class_ $ "w-1 h-6 rounded-sm bg-" <> color] mempty
  span_ [class_ "w-52 inline-block"] $ toHtml $ label <> " threshold:"
  div_ [class_ "space-x-5"] $ do
    span_ $ toHtml sign
    input_ [type_ "text", class_ inputClass, placeholder_ placeholder]


-- Alert Conditions

alertConditions_ :: Html ()
alertConditions_ = div_ [class_ "space-y-4"] $ do
  triggerCondition_
  div_ [class_ "space-y-2"] $ do
    thresholdInput_ ">" "Alert" (Just "red-500") "Enter value"
    thresholdInput_ ">" "Warning" (Just "yellow-500") "Optional"
  div_ [class_ "flex items-center space-x-2"] $ do
    span_ "If data is missing for 5 minutes"
    select_ [class_ selectClass] $ option_ "Show last known status"
  details_ [class_ "mt-6"] $ do
    summary_ [class_ "cursor-pointer"] "Advanced options"
    div_ [class_ "mt-4 space-y-4 "] $ recoveryThresholds_ >> evaluationOptions_


triggerCondition_ :: Html ()
triggerCondition_ = div_ [class_ "flex items-center space-x-2"] $ do
  span_ "Trigger when the evaluated value is"
  select_ [class_ selectClass] $ option_ "above"
  span_ "the threshold"


recoveryThresholds_ :: Html ()
recoveryThresholds_ = div_ [class_ "border-l-2 border-l-slate-300 pl-4 space-y-2"] $ do
  h3_ [class_ "font-normal text-base"] "Recovery Thresholds"
  thresholdInput_ "<=" "Alert recovery" Nothing "Optional"
  thresholdInput_ "<=" "Warning recovery" Nothing "Optional"


evaluationOptions_ :: Html ()
evaluationOptions_ = div_ [class_ "border-l-2 border-l-slate-300 pl-4 space-y-2"] $ do
  h3_ [class_ "font-normal text-base"] "Evaluation options"
  div_ [class_ "flex items-center space-x-2"] $ do
    span_ "Delay monitor evaluation by"
    input_ [type_ "text", class_ $ inputClass <> " w-16", value_ "0"]
    span_ "seconds."
  div_ [class_ "flex items-center space-x-2"] $ do
    select_ [class_ selectClass] $ option_ "Do not require"
    span_ "a full window of data for evaluation."


instance ToHtml MonitorCreate where
  toHtmlRaw = toHtml
  toHtml MCSelectType = toHtml monitorSelectType_
  toHtml (MCMetric pid _) = toHtml $ monitorMetric_ pid Nothing
  toHtml (MCAPITests pid _) = toHtml $ monitorAPITests_ pid Nothing


monitorSelectType_ :: Html ()
monitorSelectType_ = section_ [class_ "px-8 py-5 space-y-5 group/pg"] $ do
  h2_ [class_ "text-2xl py-3"] "Select a Monitor type"
  div_ [class_ "grid grid-cols-5"] $ do
    div_ [class_ "col-span-1"] $ forM_ (groupedMonitorTypes monitorTypes) $ \(grp, mts) -> do
      h3_ [class_ "mt-5"] $ toHtml grp
      forM_ mts $ \mt -> inputRadio_ "monitor-type" (label mt)
    div_ [class_ "col-span-3"] $ forM_ monitorTypes monitorTypeDetail_


monitorMetric_ :: Projects.ProjectId -> Maybe Monitors.QueryMonitor -> Html ()
monitorMetric_ pid monitorM = section_ [class_ "px-8 py-5 space-y-5 group/pg overflow-y-scroll h-full"] $ do
  div_
    [ id_ "reqsChartsECP"
    , class_ "px-5 mt-5 aspect-4/1"
    , hxGet_ $ "/charts_html?id=reqsChartsEC&show_legend=true&pid=" <> pid.toText
    , hxTrigger_ "load,  htmx:beforeRequest from:#log_explorer_form"
    , hxVals_ "js:{query:window.getQueryFromEditor(), since: getTimeRange().since, from: getTimeRange().from, to:getTimeRange().to, cols:params().cols, layout:'all', source: params().source}"
    , hxSwap_ "innerHTML"
    ]
    ""
  toHtml $ timelineSteps pid
  div_ [class_ "sticky bottom-4 p-5 bg-white border-t border-slate-500 flex space-between justify-between items-center"] $ do
    h3_ "Complete all steps to proceed"
    div_ [class_ "flex gap-5 mr-6"] $ do
      button_ [class_ "btn btn-sm btn-outline"] "Test Notifications"
      button_ [class_ "btn btn-sm btn-secondary"] "Create"


monitorAPITests_ :: Projects.ProjectId -> Maybe Monitors.QueryMonitor -> Html ()
monitorAPITests_ _ _ = div_ "API tests"
