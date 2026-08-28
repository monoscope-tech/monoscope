-- | The global service map: how data flows through the whole system for a time range —
-- entry points, instrumented services, the calls between them, and the uninstrumented
-- dependencies they reach (databases, queues, third-party hosts).
--
-- The graph is read from the @service_dependency_edges@ rollup rather than derived per
-- request: deriving it needs a self-join of the span table, which is the query shape that
-- has repeatedly OOM-killed TimeFusion in production. See @docs/service-map-spec.md@.
module Pages.ServiceMap (serviceMapGetH, ServiceMapGet (..), ServiceMapPageData (..)) where

import Data.Text qualified as T
import Data.Time (addUTCTime)
import Data.Vector qualified as V
import Effectful.Reader.Static qualified as Reader
import Effectful.Time qualified as Time
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.ServiceGraph (ServiceGraph (..), ServiceNode (..), drawnEdges, drawnNodes, serviceGraphForRange)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx)
import Pkg.Components.ServiceMap (serviceMapPanel_)
import Pkg.Components.TimePicker qualified as TimePicker
import Relude
import System.Config (AuthContext)
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (explorerNavTabs_, faSprite_, getServiceColors, nonEmptyT, parseTime)


newtype ServiceMapGet = ServiceMapPage (PageCtx ServiceMapPageData)


data ServiceMapPageData = ServiceMapPageData
  { pid :: Projects.ProjectId
  , graph :: ServiceGraph
  , env :: Maybe Text
  }


instance ToHtml ServiceMapGet where
  toHtml (ServiceMapPage (PageCtx conf pd)) = toHtml $ PageCtx conf $ serviceMapPage_ pd
  toHtmlRaw = toHtml


serviceMapGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders ServiceMapGet)
serviceMapGetH pid fromM toM sinceM envM = do
  (_, _, bw) <- mkPageCtx pid
  _ <- Reader.ask @AuthContext
  now <- Time.currentTime
  let (from, to, currentRange) = parseTime fromM toM sinceM now
  -- An empty ?env= is "all environments", so clearing the facet is a link like any other.
  graph <- serviceGraphForRange pid (nonEmptyT envM) (fromMaybe (addUTCTime (-86400) now) from) (fromMaybe now to)
  let bwconf =
        bw
          { prePageTitle = Just "Explorer"
          , pageTitle = "Service Map"
          , menuItem = Just "Explorer"
          , navTabs = Just $ explorerNavTabs_ pid "Service Map"
          , pageActions = Just $ div_ [class_ "inline-flex gap-2"] do
              TimePicker.timepicker_ Nothing currentRange Nothing
              TimePicker.refreshButton_
          }
  addRespHeaders $ ServiceMapPage $ PageCtx bwconf $ ServiceMapPageData pid graph (nonEmptyT envM)


serviceMapPage_ :: ServiceMapPageData -> Html ()
serviceMapPage_ pd = div_ [class_ "w-full h-full overflow-y-auto c-scroll p-4 flex flex-col gap-3"] do
  div_ [class_ "flex items-center gap-2 text-xs text-textWeak"] do
    faSprite_ "diagram-project" "regular" "w-3.5 h-3.5 text-iconNeutral"
    toHtml
      $ show (V.length (drawnNodes pd.graph))
      <> " services · "
      <> show (V.length (drawnEdges pd.graph))
      <> " dependencies"
    -- Search dims rather than removes, so a filtered view never silently severs a path.
    input_
      [ type_ "search"
      , class_ "ml-auto input input-sm border border-strokeWeak bg-fillWeaker rounded-lg w-56 max-md:w-32"
      , Aria.label_ "Filter services"
      , placeholder_ "Filter services"
      , -- `call`, not `send`: hyperscript parses an event name as an identifier path, so a
        -- dashed custom-event name is a parse error and the whole attribute is dropped.
        [__|on input call window.serviceMapFilter(me.value) then halt|]
      ]
  serviceMapPanel_ pd.pid "global-service-map" pd.graph serviceColors pd.env
  where
    -- Same hash-assigned colours as the trace waterfall, so a service looks the same
    -- wherever it appears.
    serviceColors = getServiceColors $ V.fromList [n.label | n <- V.toList pd.graph.nodes, not n.inferred, not (T.null n.label)]
