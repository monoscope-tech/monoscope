module Pages.Telemetry.Metrics (metricsOverViewGetH, MetricsOverViewGet (..)) where

import Effectful.Time qualified as Time
import Lucid
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Relude
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)

import Data.Default
import Models.Users.Sessions qualified as Sessions
import Pkg.Components qualified as Components
import Utils (parseTime)


metricsOverViewGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders MetricsOverViewGet)
metricsOverViewGetH pid tabM fromM toM sinceM hxBoostedM = do
  (sess, project) <- Sessions.sessionAndProject pid
  now <- Time.currentTime
  -- let tab = maybe "datapoints" (\t -> if t == "datapoints" then t else "charts") tabM
  let (_, _, currentRange) = parseTime fromM toM sinceM now
      bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession
          , currProject = Just project
          , pageTitle = "Metrics"
          , navTabs = Just $ div_ [class_ "tabs tabs-boxed tabs-md tabs-outline items-center bg-slate-200 text-slate-500"] do
              a_ [onclick_ "window.setQueryParamAndReload('source', 'requests')", role_ "tab", class_ "tab py-1.5 !h-auto tab-active"] "Overview"
              a_ [onclick_ "window.setQueryParamAndReload('source', 'logs')", role_ "tab", class_ "tab py-1.5 !h-auto "] "Explorer"
          , pageActions = Just $ Components.timepicker_ Nothing currentRange
          }

  addRespHeaders $ MetricsOVDataPointMain $ PageCtx bwconf pid


data MetricsOverViewGet
  = MetricsOVDataPointMain (PageCtx Projects.ProjectId)
  | MetricsOVChartsMain (PageCtx Projects.ProjectId)


instance ToHtml MetricsOverViewGet where
  toHtml (MetricsOVDataPointMain (PageCtx bwconf pid)) = toHtml $ PageCtx bwconf $ dataPointsPage pid
  toHtml _ = div_ [class_ "flex flex-col gap-2"] "Hello world"
  toHtmlRaw = toHtml


overViewTabs :: Projects.ProjectId -> Text -> Html ()
overViewTabs pid tab = do
  div_ [class_ "w-max mt-5"] do
    div_ [class_ "tabs tabs-boxed tabs-md tabs-outline items-center bg-slate-200 text-slate-500"] do
      a_ [onclick_ "window.setQueryParamAndReload('tab', 'datapoints')", role_ "tab", class_ $ "tab py-1.5 !h-auto  " <> if tab == "datapoints" then "tab-active" else ""] "Datapoints"
      a_ [onclick_ "window.setQueryParamAndReload('tab', 'charts')", role_ "tab", class_ $ "tab py-1.5 !h-auto " <> if tab == "charts" then "tab-active" else ""] "Charts List"


dataPointsPage :: Projects.ProjectId -> Html ()
dataPointsPage pid = do
  div_ [class_ "flex flex-col gap-2 px-4", id_ "main-content"] $ do
    overViewTabs pid "datapoints"


metricsExploreGet :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
metricsExploreGet pid = do
  addRespHeaders $ div_ [class_ "flex flex-col gap-2"] $ "Hello world"
