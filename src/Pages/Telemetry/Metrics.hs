module Pages.Telemetry.Metrics (metricsOverViewGetH, MetricsOverViewGet (..)) where

import Data.Vector qualified as V
import Effectful.Time qualified as Time
import Lucid
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Relude
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)

import Data.Default
import Data.List (foldl')
import Data.Map qualified as Map
import Data.Text qualified as T

import Models.Users.Sessions qualified as Sessions
import Pkg.Components qualified as Components
import Utils (faSprite_, parseTime)

import Data.Aeson qualified as AE
import Data.Function (on)
import Data.List qualified as L
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Lucid.Htmx
import Lucid.Hyperscript (__)


metricsOverViewGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders MetricsOverViewGet)
metricsOverViewGetH pid tabM fromM toM sinceM hxBoostedM = do
  (sess, project) <- Sessions.sessionAndProject pid
  now <- Time.currentTime
  let tab = maybe "datapoints" (\t -> if t == "datapoints" then t else "charts") tabM
  let (from, to, currentRange) = parseTime fromM toM sinceM now
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
  if tab == "datapoints"
    then do
      dataPoints <- Telemetry.getDataPointsData pid (from, to)
      addRespHeaders $ MetricsOVDataPointMain $ PageCtx bwconf (pid, dataPoints)
    else do
      addRespHeaders $ MetricsOVDataPointMain $ PageCtx bwconf (pid, [])


data MetricsOverViewGet
  = MetricsOVDataPointMain (PageCtx (Projects.ProjectId, V.Vector Telemetry.MetricDataPoint))
  | MetricsOVChartsMain (PageCtx Projects.ProjectId)


instance ToHtml MetricsOverViewGet where
  toHtml (MetricsOVDataPointMain (PageCtx bwconf (pid, datapoints))) = toHtml $ PageCtx bwconf $ dataPointsPage pid datapoints
  toHtml _ = div_ [class_ "flex flex-col gap-2"] "Hello world"
  toHtmlRaw = toHtml


overViewTabs :: Projects.ProjectId -> Text -> Html ()
overViewTabs pid tab = do
  div_ [class_ "w-max mt-5"] do
    div_ [class_ "tabs tabs-boxed tabs-md tabs-outline items-center bg-slate-200 text-slate-500"] do
      a_ [onclick_ "window.setQueryParamAndReload('tab', 'datapoints')", role_ "tab", class_ $ "tab py-1.5 !h-auto  " <> if tab == "datapoints" then "tab-active" else ""] "Datapoints"
      a_ [onclick_ "window.setQueryParamAndReload('tab', 'charts')", role_ "tab", class_ $ "tab py-1.5 !h-auto " <> if tab == "charts" then "tab-active" else ""] "Charts List"


dataPointsPage :: Projects.ProjectId -> V.Vector Telemetry.MetricDataPoint -> Html ()
dataPointsPage pid metrics = do
  div_ [class_ "flex flex-col gap-2 px-6 h-[calc(100%-60px)] overflow-y-scroll", id_ "main-content"] $ do
    overViewTabs pid "datapoints"
    div_
      [ class_ "w-full rounded-2xl mt-4 border flex flex-col"
      ]
      do
        div_ [class_ "flex px-4 justify-between py-3 text-sm font-medium border-b text-slate-900"] $ do
          div_ [class_ " w-[calc(40vw-46px)]"] "Metric"
          div_ [class_ "w-[10vw] "] "Sources"
          div_ [class_ "w-[8vw] ml-2"] "Datapoint"
        -- div_ [class_ "w-[10vw] ml-2"] "Referenced in"
        div_ [class_ "w-full"] $ do
          let tr = buildMetricTree $ V.toList $ (.metricName) <$> metrics
          let metrMap = Map.fromList $ V.toList $ V.map (\mdp -> (mdp.metricName, mdp)) metrics
          metricsTree pid tr metrMap


metricsExploreGet :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
metricsExploreGet pid = do
  addRespHeaders $ div_ [class_ "flex flex-col gap-2"] "Hello world"


data MetricNode = MetricNode
  { parent :: Text
  , current :: Text
  }
  deriving (Show, Eq)


data MetricTree = MetricTree
  { spanRecord :: MetricNode
  , children :: [MetricTree]
  }
  deriving (Show, Generic)


pathToNodes :: Text -> [MetricNode]
pathToNodes path =
  let segments = T.splitOn "." path
   in zipWith toNode ("___root___" : scanl1 combine segments) segments
  where
    combine acc segment = acc <> "." <> segment
    toNode = MetricNode


buildMetricNodes :: [Text] -> [MetricNode]
buildMetricNodes = concatMap pathToNodes


buildMetricMap :: [MetricNode] -> Map (Maybe Text) [MetricNode]
buildMetricMap = foldr insertNode Map.empty
  where
    insertNode :: MetricNode -> Map (Maybe Text) [MetricNode] -> Map (Maybe Text) [MetricNode]
    insertNode sp m =
      let key = if parent sp == "___root___" then Nothing else Just (parent sp)
          newEntry = [sp]
       in Map.insertWith
            (\new old -> if sp `elem` old then old else new ++ old)
            key
            newEntry
            m


buildTree :: Map (Maybe Text) [MetricNode] -> Maybe Text -> [MetricTree]
buildTree metricMap parentId =
  case Map.lookup parentId metricMap of
    Nothing -> []
    Just metrics ->
      [ MetricTree
        MetricNode
          { parent = mt.parent
          , current = mt.current
          }
        (buildTree metricMap (if mt.parent == "___root___" then Just mt.current else Just $ mt.parent <> "." <> mt.current))
      | mt <- metrics
      ]


buildMetricTree :: [Text] -> [MetricTree]
buildMetricTree metrics =
  let metricsNodes = buildMetricNodes metrics
      metricMap = buildMetricMap metricsNodes
   in buildTree metricMap Nothing


metricsTree :: Projects.ProjectId -> [MetricTree] -> Map Text Telemetry.MetricDataPoint -> Html ()
metricsTree pid records dp = do
  div_ [class_ "px-4 py-2 flex flex-col gap-2"] do
    forM_ (zip [0 ..] records) \(i, c) -> do
      buildTree_ pid c 0 True dp


buildTree_ :: Projects.ProjectId -> MetricTree -> Int -> Bool -> Map Text Telemetry.MetricDataPoint -> Html ()
buildTree_ pid sp level isLasChild dp = do
  let hasChildren = not $ null sp.children
  let paddingLeft = show (35 * level + 46) <> "px)" -- why 35 ? I have no clue
  div_ [class_ "flex items-start w-full relative span-filterble"] do
    when (level /= 0) $ div_ [class_ "w-8 shrink-0 ml-2 h-[1px] mt-2 bg-slate-200"] pass
    unless (level == 0) $ div_ [class_ "absolute -top-3 left-2 border-l h-5 border-l-slate-200"] pass
    unless isLasChild $ div_ [class_ "absolute top-1 left-2 border-l h-full border-l-slate-200"] pass
    div_ [class_ "flex flex-col w-full grow-1 shrink-1 border-slate-200 relative"] do
      when hasChildren $ div_ [class_ "absolute top-1 left-2 border-l h-2 border-l-slate-200"] pass
      div_
        [ class_ "w-full cursor-pointer flex tree_opened justify-between max-w-full items-center h-5 hover:bg-slate-100"
        , [__| on click toggle .tree_opened on me|]
        ]
        do
          div_ [class_ "flex w-full justify-between items-center overflow-x-hidden"] do
            div_ [class_ "flex items-center overflow-y-hidden", style_ $ "width: calc(40vw - " <> paddingLeft] do
              when hasChildren $ faSprite_ "chevron-up" "regular" "toggler rotate-90 w-4 border border-slate-200 h-4 shadow-sm rounded px-0.5 z-50 bg-slate-50 mr-1 shrink-0 text-slate-950"
              unless (sp.spanRecord.parent == "___root___") $ span_ [class_ "text-slate-400"] $ toHtml $ sp.spanRecord.parent <> "."
              span_ [class_ "text-slate-900 "] $ toHtml sp.spanRecord.current
              when hasChildren $ span_ [class_ "badge badge-ghost text-xs"] $ toHtml $ show $ length sp.children
            unless hasChildren $ do
              let fullPath = (if sp.spanRecord.parent == "___root___" then "" else sp.spanRecord.parent <> ".") <> sp.spanRecord.current
              let target = Map.lookup fullPath dp
              whenJust target $ \t -> do
                span_ [class_ "w-[10vw] truncate"] $ toHtml $ T.intercalate ", " $ V.toList t.serviceNames
                div_ [class_ "w-[8vw]"] do
                  span_ [class_ "badge badge-ghost"] $ show t.dataPointsCount
      -- div_ [class_ "flex w-[10vw] items-center text-xs"] do
      --   div_ [class_ "flex gap-1 items-center badge badge-ghost"] do
      --     faSprite_ "dashboard" "regular" "w-4 h-4"
      --     span_ "0"
      --   div_ [class_ "flex gap-1 items-center badge badge-ghost"] do
      --     faSprite_ "dashboard" "regular" "w-4 h-4"
      --     span_ "10"
      --   div_ [class_ "flex gap-1 items-center badge badge-ghost"] do
      --     faSprite_ "caution" "regular" "w-4 h-4"
      --     span_ "5"

      when hasChildren $ do
        div_ [class_ "flex-col hidden children_container gap-2 mt-2"] do
          forM_ (zip [0 ..] sp.children) \(i, c) -> do
            buildTree_ pid c (level + 1) (i == length sp.children - 1) dp
