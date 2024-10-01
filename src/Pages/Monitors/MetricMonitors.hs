module Pages.Monitors.MetricMonitors (monitorCreateGetH, MonitorCreate, configureNotificationMessage_, configureNotificationChannels_) where

import Data.CaseInsensitive qualified as CI
import Data.Default
import Data.Either.Extra (fromRight')
import Data.List (groupBy)
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
import Relude.Unsafe qualified as Unsafe
import System.Types
import Text.Slugify
import Utils
import Web.FormUrlEncoded (FromForm)


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
          { sessM = Just sess.persistentSession
          , currProject = Just project
          , pageTitle = "Create Monitor"
          , pageActions = Just $ Components.timepicker_ (Just "log_explorer_form") Nothing
          }
  case monitorType of
    Just "errors" -> addRespHeaders $ PageCtx bwconf (MCMetric pid "errors")
    Just "healthcheckpings" -> addRespHeaders $ PageCtx bwconf (MCAPITests pid "healthcheckpings")
    _ -> addRespHeaders $ PageCtx bwconf MCSelectType


selectClass, inputClass :: Text
selectClass = "border border-gray-300 rounded px-2 py-1"
inputClass = "border border-gray-300 rounded px-2 py-1"


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
    $ [ Components.TimelineStep "Choose the detection method" chooseDetectionMethod_
      , Components.TimelineStep "Define the metric" (defineTheMetric_ pid)
      , Components.TimelineStep "Set alert conditions" alertConditions_
      , Components.TimelineStep "Configure notification message" configureNotificationMessage_
      , Components.TimelineStep "Configure notification Channels" configureNotificationChannels_
      ]


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
  div_ [class_ " max-w-[750px]"] $ LogList.logQueryBox_ pid Nothing "requests"
  div_ [class_ "border-l-2 border-l-slate-300 pl-4 space-y-2"] do
    h3_ [class_ "font-normal text-base"] "Evaluation Details"
    div_ [class_ "flex items-center gap-2"] do
      div_ [class_ "form-control"] do
        label_ [class_ "label label-text"] "Evaluate the"
        select_ [class_ "select select-xs select-bordered"] $ mapM_ (option_ []) ["average", "maximum", "minimum", "sum"]
      div_ [class_ "form-control"] do
        label_ [class_ "label label-text"] "Of the query over the"
        select_ [class_ "select select-xs select-bordered"] $ mapM_ (option_ []) ["last 5 minutes", "last 10minutes", "last 15minutes", "last 30minutes", "last 1 hour", "last 1 day", "last 1 week"]


configureNotificationMessage_ :: Html ()
configureNotificationMessage_ = do
  let monitor = (def :: Monitors.QueryMonitor)
  div_ [class_ "space-y-4 max-w-[700px]"] do
    div_ [class_ "form-control w-full"] do
      label_ [class_ "label"] $ span_ [class_ "label-text"] "Severity"
      select_ [class_ "select select-xs select-bordered w-full", name_ "severity"] do
        option_ "Info"
        option_ "Warning"
        option_ "Error"
        option_ "Critical"
    div_ [class_ "form-control w-full"] do
      label_ [class_ "label"] $ span_ [class_ "label-text"] "Subject"
      input_ [placeholder_ "Error: Error subject", class_ "input input-xs input-bordered  w-full", name_ "subject", value_ monitor.alertConfig.subject]
    div_ [class_ "form-control w-full"] do
      label_ [class_ "label"] $ span_ [class_ "label-text"] "Message"
      textarea_
        [placeholder_ "Alert Message", class_ "textarea  textarea-bordered textarea-xs w-full", name_ "message"]
        $ toHtml
        $ if monitor.alertConfig.message == ""
          then [text| The alert's value is too high. Check the APItoolkit Alerts to debug |]
          else monitor.alertConfig.message

    div_ [class_ "border-l-2 border-l-slate-300 pl-4 space-y-2"] do
      h3_ [class_ "font-normal text-base"] "Recovery Thresholds"
      p_ [] "Send notifications for alert status periodically as long as the monitor has not recovered"
      div_ [class_ "flex items-center gap-2"] do
        input_ [class_ "toggle toggle-sm", type_ "checkbox"]
        span_ "If this monitor is not acknowleged or resoved, notify renotify every"
        select_ [class_ "select select-xs select-bordered"] $ mapM_ (option_ []) ["10mins", "20mins", "30mins", "1hour", "6hours", "24hours"]
      div_ [class_ "flex items-center gap-2"] do
        input_ [class_ "toggle toggle-sm", type_ "checkbox"]
        span_ "Stop renotifying after "
        input_ [type_ "text", value_ "0"]
        span_ "occurences."


configureNotificationChannels_ :: Html ()
configureNotificationChannels_ = do
  let monitor = (def :: Monitors.QueryMonitor)
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
      ul_ [tabindex_ "0", style_ "bottom:100%;top:auto", class_ "bottom-full top-auto dropdown-content z-[1] menu p-2 shadow bg-base-100 rounded-box w-52 in-w-[15rem]"] do
        li_ $ a_ [[__|on click put #addRecipientEmailAllTmpl.innerHTML after #addRecipientDropdown then _hyperscript.processNode(#recipientListParent) |]] "Email everyone"
        li_ $ a_ [[__|on click put #addRecipientEmailTmpl.innerHTML after #addRecipientDropdown then _hyperscript.processNode(#recipientListParent) |]] "Email ..."
        li_ $ a_ [[__|on click put #addRecipientSlackTmpl.innerHTML after #addRecipientDropdown then _hyperscript.processNode(#recipientListParent) |]] "To default Slack channel"
    when monitor.alertConfig.emailAll addRecipientEmailAllTmpl_
    forM_ monitor.alertConfig.emails addRecipientEmailTmpl_
    forM_ monitor.alertConfig.slackChannels addRecipientSlackTmpl_
    template_ [id_ "addRecipientSlackTmpl"] $ addRecipientSlackTmpl_ ""
    template_ [id_ "addRecipientEmailTmpl"] $ addRecipientEmailTmpl_ (CI.mk "")
    template_ [id_ "addRecipientEmailAllTmpl"] addRecipientEmailAllTmpl_


addRecipientSlackTmpl_ :: Text -> Html ()
addRecipientSlackTmpl_ channel =
  label_ [class_ "input input-bordered inline-flex items-center gap-2"] do
    "Slack"
    input_ [class_ "grow", class_ "input", placeholder_ "#channelName", type_ "text", required_ "", name_ "recipientSlack"]
    a_ [class_ "badge badge-base", [__|on click remove the closest parent <label/>|]] $ faSprite_ "xmark" "solid" "w-3 h-3"


addRecipientEmailTmpl_ :: CI.CI Text -> Html ()
addRecipientEmailTmpl_ email =
  label_ [class_ "input input-bordered inline-flex items-center gap-2"] do
    "Email"
    input_ [class_ "grow", class_ "input", placeholder_ "name@site.com", type_ "email", required_ "", name_ "recipientEmail"]
    a_ [class_ "badge badge-base", [__|on click remove the closest parent <label/>|]] $ faSprite_ "xmark" "solid" "w-3 h-3"


addRecipientEmailAllTmpl_ :: Html ()
addRecipientEmailAllTmpl_ =
  label_ [class_ "input input-bordered inline-flex items-center gap-2"] do
    "Email Everyone"
    input_ [class_ "grow", class_ "input", placeholder_ "name@site.com", type_ "hidden", value_ "True", name_ "recipientEmailAll"]
    a_ [class_ "badge badge-base", [__|on click remove the closest parent <label/>|]] $ faSprite_ "xmark" "solid" "w-3 h-3"


-- Helper functions

groupedMonitorTypes :: [MonitorType] -> [(Text, [MonitorType])]
groupedMonitorTypes = map toGroup . groupBy (\a b -> a.group == b.group)
  where
    toGroup [] = error "Unexpected empty group"
    toGroup ms@(m : _) = (m.group, ms)


inputRadio_ :: Text -> Text -> Html ()
inputRadio_ name label = div_ [class_ "form-control"] $ label_ [class_ "label cursor-pointer justify-start items-start gap-2"] $ do
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

formControlSelect_ :: Text -> Text -> [Text] -> Html ()
formControlSelect_ name label options = div_ [class_ "form-control w-full"] $ do
  label_ [class_ "label"] $ span_ [class_ "label-text"] (toHtml label)
  select_ [class_ "select select-xs select-bordered w-full", name_ name] $ forM_ options (option_ [] . toHtml)


thresholdInput_ :: Text -> Text -> Maybe Text -> Text -> Html ()
thresholdInput_ sign label colorM placeholder = div_ [class_ "flex items-center space-x-4"] $ do
  whenJust colorM \color -> div_ [class_ $ "w-1 h-6 rounded bg-" <> color] mempty
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
  toHtml MCSelectType = toHtml $ monitorSelectType_
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
    , class_ "px-5 mt-5"
    , style_ "height:250px"
    , hxGet_ $ "/charts_html?id=reqsChartsEC&show_legend=true&pid=" <> pid.toText
    , hxTrigger_ "load,  htmx:beforeRequest from:#log_explorer_form"
    , hxVals_ "js:{query_raw:window.getQueryFromEditor(), since: getTimeRange().since, from: getTimeRange().from, to:getTimeRange().to, cols:params().cols, layout:'all', source: params().source}"
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
