-- | Lucid shell for a service map. Shared by the trace-level map and the global map so
-- both get the same chrome, legend, states and accessible dependency table; only the
-- graph payload differs.
--
-- The canvas is never the only representation: the same payload is rendered as a
-- semantic dependency table underneath, which is what makes the map readable on a phone,
-- keyboard-navigable, and functional in a shared link with no JS.
module Pkg.Components.ServiceMap (serviceMapPanel_, serviceMapLegend_) where

import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEKey
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Vector qualified as V
import Lucid
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.ServiceGraph (MapStats (..), NodeKind (..), ServiceEdge (..), ServiceGraph (..), ServiceNode (..))
import Relude
import Utils (faSprite_, prettyPrintCount)


-- | Render the map shell for @graph@ into a container with id @elId@. The graph travels
-- as an embedded @application/json@ payload rather than an HTMX swap: HTMX swaps HTML,
-- and a canvas renderer needs a model, not markup.
serviceMapPanel_ :: Projects.ProjectId -> Text -> ServiceGraph -> HM.HashMap Text Text -> Html ()
serviceMapPanel_ pid elId graph colors = div_ [class_ "w-full flex flex-col gap-3"] do
  case graph.error of
    Just msg -> emptyState_ "triangle-exclamation" "Couldn't load the service map" msg
    Nothing
      | V.null graph.nodes -> emptyState_ "diagram-project" "No service activity in this range" "Once traced requests arrive, the services they touch and the calls between them appear here."
      | otherwise -> do
          when graph.truncated
            $ div_ [class_ "flex items-center gap-2 text-xs text-textWeak px-1"]
            $ do
              faSprite_ "circle-info" "regular" "w-3.5 h-3.5 text-iconNeutral"
              toHtml $ "Showing the " <> show (V.length graph.nodes) <> " busiest services; quieter ones are hidden."
          serviceMapLegend_ graph
          div_
            [ class_ "border border-strokeWeak rounded-2xl w-full h-[520px] max-md:h-[360px] relative"
            , id_ elId
            , term "data-service-map" elId
            , -- Base for the node menu's links; the renderer only appends the query.
              term "data-map-base" ("/p/" <> pid.toText)
            ]
            $ nodeMenu_ elId
          script_ [type_ "application/json", id_ $ elId <> "-data"] $ decodeUtf8 @Text $ AE.encode graph
          script_ [type_ "application/json", id_ $ elId <> "-colors"]
            $ decodeUtf8 @Text
            $ AE.encode
            $ AE.object [AEKey.fromText k AE..= v | (k, v) <- HM.toList colors]
          dependencyTable_ graph


-- | Click menu for a node, in the shape Datadog uses: the actions you actually want next
-- from a service on a map. Rendered once by Lucid and reused for every node — the renderer
-- only positions it and rewrites the hrefs, so no markup is ever built in JavaScript.
nodeMenu_ :: Text -> Html ()
nodeMenu_ elId =
  div_
    [ class_ "absolute hidden z-50 min-w-56 rounded-lg border border-strokeWeak bg-bgRaised shadow-lg py-1 text-sm"
    , id_ $ elId <> "-menu"
    , term "data-service-menu" ""
    ]
    do
      div_ [class_ "px-3 py-1.5 font-semibold text-textStrong truncate border-b border-strokeWeak mb-1", term "data-menu-title" ""] ""
      forM_ menuItems \(action, icon, label) ->
        a_
          [ class_ "flex items-center gap-2 px-3 py-1.5 text-textStrong hover:bg-fillWeak cursor-pointer"
          , term "data-menu-action" action
          , href_ "#"
          ]
          do
            faSprite_ icon "regular" "w-3.5 h-3.5 text-iconNeutral shrink-0"
            toHtml label


-- | @data-menu-action@ is the contract with the renderer, which fills in the href per node.
-- \"inspect\" has no href: it isolates the node's upstream and downstream in place.
menuItems :: [(Text, Text, Text)]
menuItems =
  [ ("inspect", "arrow-right", "Inspect")
  , ("traces", "list-tree", "View in trace search")
  , ("logs", "file-lines", "View logs")
  , ("metrics", "chart-line", "View metrics")
  , ("monitors", "bell", "View monitors")
  ]


-- | Shape + dash carry node type, so the legend has to teach the shapes; colour alone is
-- never the only signal.
serviceMapLegend_ :: ServiceGraph -> Html ()
serviceMapLegend_ graph =
  div_ [class_ "flex flex-wrap items-center gap-x-4 gap-y-1 px-1 text-xs text-textWeak"]
    $ forM_ present
    $ \k -> div_ [class_ "flex items-center gap-1.5"] do
      faSprite_ (kindIcon k) "regular" "w-3 h-3 text-iconNeutral"
      span_ [] $ toHtml $ kindLabel k
  where
    present = ordNub [n.kind | n <- V.toList graph.nodes]


kindIcon :: NodeKind -> Text
kindIcon = \case
  NKEntry -> "arrow-right-to-bracket"
  NKService -> "circle"
  NKDatabase -> "database"
  NKQueue -> "layer-group"
  NKExternal -> "cloud"
  NKUnknown -> "circle-question"


kindLabel :: NodeKind -> Text
kindLabel = \case
  NKEntry -> "Entry point"
  NKService -> "Service"
  NKDatabase -> "Database"
  NKQueue -> "Queue"
  NKExternal -> "External"
  NKUnknown -> "Unknown"


-- | The no-JS, screen-reader and small-screen representation of the same graph.
dependencyTable_ :: ServiceGraph -> Html ()
dependencyTable_ graph = details_ [class_ "group border border-strokeWeak rounded-2xl overflow-hidden"] do
  summary_ [class_ "flex items-center gap-2 px-3 py-2 text-sm text-textStrong cursor-pointer select-none"] do
    faSprite_ "chevron-right" "regular" "w-3 h-3 text-iconNeutral transition-transform group-open:rotate-90"
    toHtml $ "Dependencies (" <> show (V.length graph.edges) <> ")"
  div_ [class_ "overflow-x-auto"] $ table_ [class_ "w-full text-xs"] do
    thead_ [class_ "text-textWeak border-b border-strokeWeak"]
      $ tr_
      $ forM_ ["Caller", "Callee", "Requests", "Errors", "p95"]
      $ \h -> th_ [class_ "text-left font-medium px-3 py-1.5 whitespace-nowrap"] h
    tbody_ $ forM_ (sortOn (\e -> Down e.stats.requests) $ V.toList graph.edges) \e -> tr_ [class_ "border-b border-strokeWeak last:border-0"] do
      td_ [class_ "px-3 py-1.5 whitespace-nowrap"] $ toHtml $ nodeDisplay graph e.source
      td_ [class_ "px-3 py-1.5 whitespace-nowrap"] $ toHtml $ nodeDisplay graph e.target
      td_ [class_ "px-3 py-1.5 tabular-nums"] $ toHtml $ prettyPrintCount $ fromIntegral e.stats.requests
      td_ [class_ $ "px-3 py-1.5 tabular-nums" <> bool "" " text-textError" (e.stats.errors > 0)]
        $ toHtml
        $ if e.stats.errors == 0 then "0" else show e.stats.errors <> " (" <> pct e.stats.errorRate <> ")"
      td_ [class_ "px-3 py-1.5 tabular-nums"] $ toHtml $ prettyDuration e.stats.p95Ns


-- | Edges reference node keys; show the human label the node carries.
nodeDisplay :: ServiceGraph -> Text -> Text
nodeDisplay graph k
  | T.null k = "Entry point"
  | otherwise = maybe k (.label) $ V.find (\n -> n.key == k) graph.nodes


pct :: Double -> Text
pct r = T.take 4 (show (r * 100)) <> "%"


prettyDuration :: Int64 -> Text
prettyDuration ns
  | ns <= 0 = "—"
  | ns < 1_000_000 = show (ns `div` 1000) <> "µs"
  | ns < 1_000_000_000 = show (ns `div` 1_000_000) <> "ms"
  | otherwise = T.take 4 (show (fromIntegral ns / 1e9 :: Double)) <> "s"


emptyState_ :: Text -> Text -> Text -> Html ()
emptyState_ icon title body =
  div_ [class_ "border border-strokeWeak rounded-2xl w-full min-h-[260px] flex flex-col items-center justify-center gap-2 text-center px-6"] do
    faSprite_ icon "regular" "w-5 h-5 text-iconNeutral"
    span_ [class_ "text-sm text-textStrong font-medium"] $ toHtml title
    span_ [class_ "text-xs text-textWeak max-w-md"] $ toHtml body
