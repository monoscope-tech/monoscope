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
import Models.Telemetry.ServiceGraph (MapStats (..), NodeKind (..), ServiceEdge (..), ServiceGraph (..), ServiceNode (..), drawnEdges, drawnNodes)
import Relude
import Utils (faSprite_, prettyPrintCount)


-- | Render the map shell for @graph@ into a container with id @elId@. The graph travels
-- as an embedded @application/json@ payload rather than an HTMX swap: HTMX swaps HTML,
-- and a canvas renderer needs a model, not markup.
serviceMapPanel_ :: Projects.ProjectId -> Text -> ServiceGraph -> HM.HashMap Text Text -> Html ()
serviceMapPanel_ pid elId graph colors = div_ [class_ "w-full flex flex-col gap-3 relative"] do
  case graph.error of
    Just msg -> emptyState_ "triangle-exclamation" "Couldn't load the service map" msg
    Nothing
      | V.null graph.nodes -> emptyState_ "diagram-project" "No service activity in this range" "Once traced requests arrive, the services they touch and the calls between them appear here."
      | otherwise -> do
          when graph.truncated
            $ div_ [class_ "flex items-center gap-2 text-xs text-textWeak px-1"]
            $ do
              faSprite_ "circle-info" "regular" "w-3.5 h-3.5 text-iconNeutral"
              toHtml
                $ "Showing the "
                <> show (V.length (drawnNodes graph))
                <> " busiest dependencies. Quieter ones are folded away — search to find a specific one."
          serviceMapLegend_ graph colors
          scopeChip_
          -- The viewport scrolls; the canvas inside it is sized to the graph. Fitting a big
          -- graph into a fixed box is what shrinks cards until they overlap, so the map is
          -- allowed to be bigger than its window and you pan to the rest.
          div_
            [ class_ "border border-strokeWeak rounded-2xl w-full h-[620px] max-md:h-[420px] relative overflow-auto c-scroll"
            , id_ elId
            , term "data-service-map" elId
            , -- Base for the node menu's links; the renderer only appends the query.
              term "data-map-base" ("/p/" <> pid.toText)
            ]
            $ div_ [class_ "absolute inset-0", term "data-map-canvas" ""] pass
          -- Outside the scrolling viewport: an absolutely positioned menu inside a scroll
          -- container is clipped by it the moment the node is near an edge.
          nodeMenu_ elId
          script_ [type_ "application/json", id_ $ elId <> "-data"] $ decodeUtf8 @Text $ AE.encode graph
          script_ [type_ "application/json", id_ $ elId <> "-colors"]
            $ decodeUtf8 @Text
            $ AE.encode
            $ AE.object [AEKey.fromText k AE..= v | (k, v) <- HM.toList colors]
          dependencyTable_ graph


-- | The active scope, in the shape Datadog puts above its flow map: a dismissable chip
-- saying which service the map is currently answering for. Rendered once and toggled by the
-- renderer, which only writes text into it — the markup is Lucid's, as always.
scopeChip_ :: Html ()
scopeChip_ =
  div_ [class_ "hidden items-center gap-2 px-1 text-xs", term "data-map-scope" ""] do
    span_ [class_ "inline-flex items-center gap-1.5 rounded-md border border-strokeWeak bg-fillWeak px-2 py-1"] do
      span_ [class_ "text-textWeak"] "Service:"
      span_ [class_ "font-medium text-textStrong truncate max-w-64", term "data-scope-label" ""] ""
      button_
        [ class_ "text-textWeak hover:text-textStrong cursor-pointer"
        , term "data-scope-clear" ""
        , type_ "button"
        , term "aria-label" "Clear service scope"
        ]
        "×"
    span_ [class_ "text-textWeak", term "data-scope-count" ""] ""


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
-- \"inspect\" and \"focus\" have no href: the first isolates the node's upstream and
-- downstream in place, the second scopes the whole map to it and re-lays it out.
menuItems :: [(Text, Text, Text)]
menuItems =
  [ ("focus", "diagram-project", "Focus on this service")
  , ("inspect", "arrow-right", "Inspect")
  , ("traces", "list-tree", "View in trace search")
  , ("logs", "file-lines", "View logs")
  , ("metrics", "chart-line", "View metrics")
  , ("monitors", "bell", "View monitors")
  ]


-- | The card says what a node /is/ (its DB\/EXT\/QUEUE tag) and how it is doing (its
-- numbers). What a card cannot say for itself is the two encodings carried by its border,
-- so those are what the legend teaches — plus the health scale, because a threshold is a
-- promise about what red means and a reader is entitled to know where it sits.
serviceMapLegend_ :: ServiceGraph -> HM.HashMap Text Text -> Html ()
serviceMapLegend_ graph colors = div_ [class_ "flex flex-col gap-2"] do
  div_ [class_ "flex flex-wrap items-center gap-x-4 gap-y-1 px-1 text-xs text-textWeak"] do
    forM_ borderLegend \(cls, label) -> div_ [class_ "flex items-center gap-1.5"] do
      div_ [class_ $ "w-4 h-2.5 rounded-sm shrink-0 " <> cls] pass
      span_ [] $ toHtml label
    forM_ healthLegend \(cls, label) -> div_ [class_ "flex items-center gap-1.5"] do
      div_ [class_ $ "w-2.5 h-2.5 rounded-full shrink-0 " <> cls] pass
      span_ [] $ toHtml label
  -- One swatch is not a colour key, it is a lone label pretending to be one.
  when (length services > 1) $ div_ [class_ "flex items-start gap-3 px-1 text-xs text-textWeak"] do
    span_ [class_ "shrink-0 font-medium"] "Service colors"
    ul_ [class_ "flex flex-wrap max-md:flex-nowrap gap-x-3 gap-y-1 max-md:overflow-x-auto c-scroll", term "aria-label" "Service colors"]
      $ forM_ services
      $ \service -> li_ [class_ "flex items-center gap-1.5 shrink-0", term "data-service-color" service] do
        div_ [class_ $ "w-2.5 h-2.5 rounded-full shrink-0 " <> HM.findWithDefault "bg-fillNeutral-strong" service colors] pass
        span_ [class_ "truncate max-w-40"] $ toHtml service
  where
    borderLegend :: [(Text, Text)]
    borderLegend =
      [ ("border border-strokeStrong", "Instrumented service")
      , ("border border-dashed border-strokeStrong", "Uninstrumented dependency")
      ]
    healthLegend :: [(Text, Text)]
    healthLegend =
      [ ("bg-fillWarning-strong", "Elevated errors (1%+)")
      , ("bg-fillError-strong", "Failing (5%+)")
      ]
    services = ordNub [n.label | n <- V.toList graph.nodes, n.kind == NKService, not n.inferred, not (T.null n.label)]


-- | The no-JS, screen-reader and small-screen representation of the same graph.
dependencyTable_ :: ServiceGraph -> Html ()
dependencyTable_ graph = details_ [class_ "group border border-strokeWeak rounded-2xl overflow-hidden"] do
  summary_ [class_ "flex items-center gap-2 px-3 py-2 text-sm text-textStrong cursor-pointer select-none"] do
    faSprite_ "chevron-right" "regular" "w-3 h-3 text-iconNeutral transition-transform group-open:rotate-90"
    toHtml $ "Dependencies (" <> show (V.length (drawnEdges graph)) <> ")"
  div_ [class_ "overflow-x-auto"] $ table_ [class_ "w-full text-xs"] do
    thead_ [class_ "text-textWeak border-b border-strokeWeak"]
      $ tr_
      $ forM_ ["Caller", "Callee", "Requests", "Errors", "p95"]
      $ \h -> th_ [class_ "text-left font-medium px-3 py-1.5 whitespace-nowrap"] h
    tbody_ $ forM_ (sortOn (\e -> Down e.stats.requests) $ V.toList (drawnEdges graph)) \e -> tr_ [class_ "border-b border-strokeWeak last:border-0"] do
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
  | otherwise = maybe k describe $ V.find (\n -> n.key == k) graph.nodes
  where
    describe n = n.label <> maybe "" (\c -> " (" <> show c <> " endpoints)") n.memberCount


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
