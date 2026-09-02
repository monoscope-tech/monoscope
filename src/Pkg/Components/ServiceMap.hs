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
import Data.Default (def)
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Vector qualified as V
import Lucid
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.ServiceGraph (MapStats (..), NodeKind (..), ServiceEdge (..), ServiceGraph (..), ServiceNode (..), drawnEdges, drawnNodes)
import Pages.Components (EmptyStateCfg (..), EmptyStateSize (..), emptyState_)
import Relude
import Utils (faSprite_, prettyPrintCount)


-- | Render the map shell for @graph@ into a container with id @elId@. The graph travels
-- as an embedded @application/json@ payload rather than an HTMX swap: HTMX swaps HTML,
-- and a canvas renderer needs a model, not markup.
serviceMapPanel_ :: Projects.ProjectId -> Text -> ServiceGraph -> HM.HashMap Text Text -> Maybe Text -> Html ()
serviceMapPanel_ pid elId graph colors selectedEnv = div_ [class_ "w-full flex flex-col gap-3 relative"] do
  case graph.error of
    Just msg -> mapEmpty_ "triangle-exclamation" "Couldn't load the service map" msg
    Nothing
      | V.null graph.nodes -> mapEmpty_ "diagram-project" "No service activity in this range" "Once traced requests arrive, the services they touch and the calls between them appear here."
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
          envFacet_ pid graph selectedEnv
          scopeChip_
          -- A React-Flow-shaped canvas, without React: a clipping viewport, one pane that the
          -- renderer translates and scales, an SVG layer for the edges, and the nodes as real
          -- DOM cards above it. Nothing here is drawn to a bitmap, so the cards are styled by
          -- our own tokens, carry sprite icons, and are focusable — none of which a canvas
          -- node can be.
          div_
            [ class_ "border border-strokeStrong rounded-2xl w-full h-[720px] max-md:h-[460px] relative overflow-hidden touch-none select-none"
            , id_ elId
            , term "data-service-map" elId
            , -- Base for the node menu's links; the renderer only appends the query.
              term "data-map-base" ("/p/" <> pid.toText)
            ]
            do
              div_ [class_ "absolute inset-0 origin-top-left", term "data-map-pane" ""] do
                svg_ [class_ "absolute overflow-visible pointer-events-none", term "data-map-edges" "", term "width" "1", term "height" "1"]
                  -- Lucid has no SVG element vocabulary; `term` names them directly. One shared
                  -- marker definition rather than an arrow per edge.
                  $ term "defs"
                  $ term
                    "marker"
                    [ id_ $ elId <> "-arrow"
                    , term "viewBox" "0 0 10 10"
                    , term "refX" "8"
                    , term "refY" "5"
                    , term "markerWidth" "5"
                    , term "markerHeight" "5"
                    , term "orient" "auto"
                    ]
                  $ term "path" [term "d" "M 0 1 L 9 5 L 0 9 z", term "fill" "context-stroke"] (mempty :: Html ())
                div_ [class_ "absolute inset-0", term "data-map-nodes" ""] pass
              zoomControls_
              nodeCardTemplate_
          -- Outside the scrolling viewport: an absolutely positioned menu inside a scroll
          -- container is clipped by it the moment the node is near an edge.
          nodeMenu_ elId
          script_ [type_ "application/json", id_ $ elId <> "-data"] $ decodeUtf8 @Text $ AE.encode graph
          script_ [type_ "application/json", id_ $ elId <> "-colors"]
            $ decodeUtf8 @Text
            $ AE.encode
            $ AE.object [AEKey.fromText k AE..= v | (k, v) <- HM.toList colors]
          dependencyTable_ graph


-- | The Env facet, in the shape Datadog puts beside its flow map. Server-side, because the
-- environment is a rollup dimension rather than something the payload can be filtered down
-- to: picking one is a new query, so it is a link and it survives a reload and a share.
--
-- Hidden entirely when nothing in range reports an environment — a facet with no choices in
-- it is furniture, not a control.
envFacet_ :: Projects.ProjectId -> ServiceGraph -> Maybe Text -> Html ()
envFacet_ pid graph selected =
  unless (V.null graph.environments) $ div_ [class_ "flex flex-wrap items-center gap-1.5 px-1 text-xs"] do
    span_ [class_ "text-textWeak font-medium mr-0.5"] "Env"
    forM_ (Nothing : (Just <$> V.toList graph.environments)) \opt ->
      a_
        [ class_
            $ "rounded-md border px-2 py-0.5 "
            <> bool "border-strokeWeak text-textWeak hover:bg-fillWeak" "border-strokeBrand-strong bg-fillBrand-weak text-textStrong font-medium" (opt == selected)
        , href_ $ "/p/" <> pid.toText <> "/service_map" <> maybe "" ("?env=" <>) opt
        ]
        $ toHtml
        $ fromMaybe "All" opt


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


-- | Zoom affordances. `roam` used to be on with nothing to say so — the canvas panned and
-- zoomed and no one could tell. Real buttons also give the gesture a keyboard equivalent.
zoomControls_ :: Html ()
zoomControls_ =
  div_ [class_ "absolute bottom-3 right-3 z-20 flex flex-col rounded-lg border border-strokeWeak bg-bgRaised shadow-sm overflow-hidden"]
    $ forM_ ([("zoom-in", "plus", "Zoom in"), ("zoom-out", "minus", "Zoom out"), ("fit", "expand", "Fit to view")] :: [(Text, Text, Text)])
    $ \(action, icon, label) ->
      button_
        [ class_ "p-1.5 hover:bg-fillWeak cursor-pointer border-b border-strokeWeak last:border-0"
        , type_ "button"
        , term "data-map-zoom" action
        , term "aria-label" label
        , title_ label
        ]
        $ faSprite_ icon "regular" "w-3.5 h-3.5 text-iconNeutral"


-- | The node card, authored once here and cloned per drawn node by the renderer. A template
-- rather than a card per node in the payload: a project at the cap carries 150 drawn nodes and
-- up to 1200 collapsed members, and rendering all of them to hide most would be ten thousand
-- elements the reader never sees. Cloning Lucid-authored markup is not building HTML in JS.
nodeCardTemplate_ :: Html ()
nodeCardTemplate_ =
  template_ [term "data-node-card" ""] $ do
    div_
      [ class_ "absolute flex w-[150px] overflow-hidden rounded-[3px] border bg-bgRaised text-xs leading-4 cursor-pointer focus:outline-none focus-visible:ring-2 focus-visible:ring-strokeBrand-strong"
      , term "data-node" ""
      , tabindex_ "0"
      ]
      do
        div_ [class_ "min-w-0 flex-1 px-2 py-1.5"] do
          div_ [class_ "flex items-center gap-1"] do
            span_ [class_ "shrink-0 text-iconNeutral", term "data-node-icon" ""] pass
            span_ [class_ "truncate font-semibold text-textStrong", term "data-node-name" ""] pass
            span_ [class_ "shrink-0 tabular-nums text-textWeak hidden", term "data-node-count" ""] pass
          div_ [class_ "text-textWeak", term "data-node-errors" ""] pass
          div_ [class_ "text-textWeak", term "data-node-latency" ""] pass
          span_ [class_ "mt-0.5 inline-block rounded-[2px] bg-fillWeak px-1 py-px tabular-nums text-textStrong", term "data-node-rps" ""] pass
        -- The health bar runs the full height of the card at its right edge, which is where
        -- Datadog puts it and why it reads without being looked at.
        span_ [class_ "w-[4px] shrink-0", term "data-node-health" ""] pass


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
--
-- \"events\" and \"logs\" land on the same explorer. Events includes a service's spans and
-- logs; the logs action adds the kind filter.
menuItems :: [(Text, Text, Text)]
menuItems =
  [ ("focus", "diagram-project", "Focus on this service")
  , ("inspect", "arrow-right", "Inspect")
  , ("events", "list-tree", "View events (spans & logs)")
  , ("logs", "file-lines", "View logs only")
  , ("metrics", "chart-line", "View metrics")
  , ("monitors", "bell", "View monitors")
  ]


-- | The card says what a node /is/ (its DB\/EXT\/QUEUE tag) and how it is doing (its
-- numbers). What a card cannot say for itself is the two encodings carried by its border,
-- so those are what the legend teaches — plus the health scale, because a threshold is a
-- promise about what red means and a reader is entitled to know where it sits.
serviceMapLegend_ :: ServiceGraph -> HM.HashMap Text Text -> Html ()
serviceMapLegend_ graph colors = div_ [class_ "flex flex-col gap-2"] do
  -- Written out rather than folded over a list of class strings: Tailwind's scanner has to
  -- see each set of classes as a literal at the element that wears them, and a reader should
  -- not have to jump to a `where` clause to learn what a swatch looks like.
  div_ [class_ "flex flex-wrap items-center gap-x-4 gap-y-1 px-1 text-xs text-textWeak"] do
    div_ [class_ "flex items-center gap-1.5"] do
      div_ [class_ "w-4 h-2.5 rounded-sm shrink-0 border border-strokeStrong"] pass
      span_ [] "Instrumented service"
    div_ [class_ "flex items-center gap-1.5"] do
      div_ [class_ "w-4 h-2.5 rounded-sm shrink-0 border border-dashed border-strokeStrong"] pass
      span_ [] "Uninstrumented dependency"
    div_ [class_ "flex items-center gap-1.5"] do
      div_ [class_ "w-2.5 h-2.5 rounded-full shrink-0 bg-fillWarning-strong"] pass
      span_ [] "Elevated errors (1%+)"
    div_ [class_ "flex items-center gap-1.5"] do
      div_ [class_ "w-2.5 h-2.5 rounded-full shrink-0 bg-fillError-strong"] pass
      span_ [] "Failing (5%+)"
  -- One swatch is not a colour key, it is a lone label pretending to be one.
  when (length services > 1) $ div_ [class_ "flex items-start gap-3 px-1 text-xs text-textWeak"] do
    span_ [class_ "shrink-0 font-medium"] "Service colors"
    ul_ [class_ "flex flex-wrap max-md:flex-nowrap gap-x-3 gap-y-1 max-md:overflow-x-auto c-scroll", term "aria-label" "Service colors"]
      $ forM_ services
      $ \service -> li_ [class_ "flex items-center gap-1.5 shrink-0", term "data-service-color" service] do
        div_ [class_ $ "w-2.5 h-2.5 rounded-full shrink-0 " <> HM.findWithDefault "bg-fillStrong" service colors] pass
        span_ [class_ "truncate max-w-40"] $ toHtml service
  where
    services = ordNub [n.label | n <- V.toList graph.nodes, n.kind == NKService, not n.inferred, not (T.null n.label)]


-- | The no-JS, screen-reader and small-screen representation of the same graph.
dependencyTable_ :: ServiceGraph -> Html ()
dependencyTable_ graph = details_ [class_ "group border border-strokeStrong rounded-2xl overflow-hidden"] do
  summary_ [class_ "flex items-center gap-2 px-3 py-2 text-sm text-textStrong cursor-pointer select-none"] do
    faSprite_ "chevron-right" "regular" "w-3 h-3 text-iconNeutral transition-transform group-open:rotate-90"
    toHtml $ "Dependencies (" <> show (V.length (drawnEdges graph)) <> ")"
  div_ [class_ "overflow-x-auto"] $ table_ [class_ "w-full text-xs"] do
    thead_ [class_ "text-textWeak border-b border-strokeWeak"]
      $ tr_
      $ forM_ ["Caller", "Callee", "Requests", "Errors", "p95 latency"]
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


-- | The map's empty/error slot: the shared compact empty state inside the fixed-height
-- card the canvas would otherwise occupy, so the panel does not collapse. Named apart
-- from 'Pages.Components.emptyState_', which it delegates to — it used to shadow it.
mapEmpty_ :: Text -> Text -> Text -> Html ()
mapEmpty_ icon title body =
  div_ [class_ "border border-strokeStrong rounded-2xl w-full min-h-[260px] flex items-center justify-center px-6"]
    $ emptyState_ def{icon = Just icon, size = ESCompact} title body
