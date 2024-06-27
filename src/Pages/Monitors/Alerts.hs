module Pages.Monitors.Alerts (
  alertSingleGetH,
  convertToQueryMonitor,
  alertSingleToggleActiveH,
  alertListGetH,
  alertUpsertPostH,
  editAlert_,
  AlertUpsertForm (..),
  Alert,
) where

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
import NeatInterpolation (text)
import Pkg.Parser (defSqlQueryCfg, finalAlertQuery, fixedUTCTime, parseQueryToComponents, presetRollup)
import Relude
import System.Types
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
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)


convertToQueryMonitor :: Projects.ProjectId -> UTCTime -> Monitors.QueryMonitorId -> AlertUpsertForm -> Monitors.QueryMonitor
convertToQueryMonitor projectId now queryMonitorId alertForm =
  -- FIXME: handle errors correctly, not crashing
  let sqlQueryCfg = (defSqlQueryCfg projectId fixedUTCTime){presetRollup = Just "5m"}
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


alertUpsertPostH :: Projects.ProjectId -> AlertUpsertForm -> ATAuthCtx (RespHeaders (Alert))
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


alertListGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Alert))
alertListGetH pid = do
  monitors <- dbtToEff $ Monitors.queryMonitorsAll pid
  addRespHeaders $ AlertListGet monitors


alertSingleToggleActiveH :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATAuthCtx (RespHeaders (Alert))
alertSingleToggleActiveH pid monitorId = do
  _ <- dbtToEff $ Monitors.monitorToggleActiveById monitorId

  monitors <- dbtToEff $ Monitors.queryMonitorsAll pid
  addRespHeaders $ AlertListGet monitors


alertSingleGetH :: Projects.ProjectId -> Monitors.QueryMonitorId -> ATAuthCtx (RespHeaders (Alert))
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
        input_ [class_ "grow input input-bordered", type_ "text", placeholder_ "Title of alert", name_ "title", value_ monitor.alertConfig.title]

      div_ [class_ "collapse collapse-arrow join-item border border-base-300"] do
        input_ [class_ "", name_ "createAlertAccordion", checked_, type_ "radio", required_ ""]
        div_ [class_ "collapse-title text-xl font-medium  "] do
          span_ [class_ "badge badge-error mr-3"] "1"
          "Alert conditions"
        div_ [class_ "collapse-content"] do
          div_ [class_ "py-3"] do
            span_ "Trigger when the metric is "
            select_ [class_ "select select-bordered inline-block mx-2 ", name_ "direction"] do
              option_ [selected_ ""] "above"
              option_ "below"
            span_ " the threshold for the last"
            select_
              [ class_ "select select-bordered inline-block mx-2 "
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
                [ class_ "grow input input-bordered input-error "
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
                [ class_ "grow input input-bordered input-warning"
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
          div_ [class_ "form-control w-full"] do
            label_ [class_ "label"] $ span_ [class_ "label-text"] "Severity"
            select_ [class_ "select select-bordered w-full", name_ "severity"] do
              option_ "Info"
              option_ "Warning"
              option_ "Error"
              option_ "Critical"
          div_ [class_ "form-control w-full"] do
            label_ [class_ "label"] $ span_ [class_ "label-text"] "Subject"
            input_ [placeholder_ "Error: Error subject", class_ "input input-bordered  w-full", name_ "subject", value_ monitor.alertConfig.subject]
          div_ [class_ "form-control w-full"] do
            label_ [class_ "label"] $ span_ [class_ "label-text"] "Message"
            textarea_
              [placeholder_ "Alert Message", class_ "textarea textarea-bordered textarea-md w-full", name_ "message"]
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
              ul_ [tabindex_ "0", style_ "bottom:100%;top:auto", class_ "bottom-full top-auto dropdown-content z-[1] menu p-2 shadow bg-base-100 rounded-box w-52 min-w-[15rem]"] do
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
