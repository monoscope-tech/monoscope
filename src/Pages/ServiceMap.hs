-- | The global service map: how data flows through the whole system for a time range —
-- entry points, instrumented services, the calls between them, and the uninstrumented
-- dependencies they reach (databases, queues, third-party hosts).
--
-- The graph is read from the @service_dependency_edges@ rollup rather than derived per
-- request: deriving it needs a self-join of the span table, which is the query shape that
-- has repeatedly OOM-killed TimeFusion in production. See @docs/service-map-spec.md@.
module Pages.ServiceMap (serviceMapGetH, ServiceMapGet (..), ServiceMapPageData (..), ServiceMapScope (..)) where

import Data.Time (addUTCTime)
import Data.Vector qualified as V
import Effectful.Time qualified as Time
import Lucid
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.ServiceGraph (ServiceGraph (..), ServiceNode (..), drawnEdges, drawnNodes, endpointDependencyGraphForRange, serviceGraphForRange)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx)
import Pages.Components (searchInput_)
import Pkg.Components.ServiceMap (endpointDependencyMapPanel_, serviceMapPanel_)
import Pkg.Components.TimePicker qualified as TimePicker
import Relude
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (explorerNavTabs_, faSprite_, getServiceColors, nonEmptyT, parseTime)


newtype ServiceMapGet = ServiceMapPage (PageCtx ServiceMapPageData)


data ServiceMapPageData = ServiceMapPageData
  { pid :: Projects.ProjectId
  , graph :: ServiceGraph
  , scope :: ServiceMapScope
  }


data ServiceMapScope = GlobalServiceMap (Maybe Text) | EndpointServiceMap Text


instance ToHtml ServiceMapGet where
  toHtml (ServiceMapPage (PageCtx conf pd)) = toHtml $ PageCtx conf $ serviceMapPage_ pd
  toHtmlRaw = toHtml


serviceMapGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders ServiceMapGet)
serviceMapGetH pid fromM toM sinceM envM endpointHashM = do
  (_, _, bw) <- mkPageCtx pid
  now <- Time.currentTime
  -- An empty ?env= is "all environments", so clearing the facet is a link like any other.
  let (from, to, currentRange) = parseTime fromM toM sinceM now
      env = nonEmptyT envM
      endpointHash = nonEmptyT endpointHashM
      scope = maybe (GlobalServiceMap env) EndpointServiceMap endpointHash
      (fetchGraph, prePageTitle, pageTitle) = case scope of
        GlobalServiceMap selectedEnv -> (serviceGraphForRange pid selectedEnv, Nothing, "Explorer")
        EndpointServiceMap endpoint -> (endpointDependencyGraphForRange pid endpoint, Just "Explorer", "Endpoint Dependency Map")
  graph <- fetchGraph (fromMaybe (addUTCTime (-86400) now) from) (fromMaybe now to)
  let bwconf =
        bw
          { prePageTitle
          , pageTitle
          , menuItem = Just "Explorer"
          , navTabs = Just $ explorerNavTabs_ pid "Service Map"
          , pageActions = Just $ TimePicker.liveDataControls_ Nothing currentRange Nothing TimePicker.RefreshOnly
          }
  addRespHeaders $ ServiceMapPage $ PageCtx bwconf $ ServiceMapPageData pid graph scope


serviceMapPage_ :: ServiceMapPageData -> Html ()
serviceMapPage_ pd = div_ [class_ "w-full h-full overflow-y-auto c-scroll p-4 pt-1 flex flex-col gap-3"] do
  div_ [class_ "flex items-center gap-2 text-xs text-textWeak"] do
    faSprite_ "diagram-project" "regular" "w-3.5 h-3.5 text-iconNeutral"
    toHtml $ case pd.scope of
      EndpointServiceMap{} -> show (V.length (drawnEdges pd.graph)) <> " direct dependencies"
      GlobalServiceMap{} -> show (V.length (drawnNodes pd.graph)) <> " services · " <> show (V.length (drawnEdges pd.graph)) <> " dependencies"
    -- Search dims rather than removes, so a filtered view never silently severs a path.
    searchInput_ "ml-auto border-strokeWeak bg-fillWeaker w-56 max-md:w-32" "Filter services" [term "hx-on:input" "window.serviceMapFilter(this.value)"]
  case pd.scope of
    EndpointServiceMap{} -> endpointDependencyMapPanel_ pd.pid "endpoint-service-map" pd.graph serviceColors
    GlobalServiceMap selectedEnv -> serviceMapPanel_ pd.pid "global-service-map" pd.graph serviceColors selectedEnv
  where
    -- Same hash-assigned colours as the trace waterfall, so a service looks the same
    -- wherever it appears.
    serviceColors = getServiceColors $ V.map (.label) $ V.filter (\n -> not n.inferred && n.label /= "") pd.graph.nodes
