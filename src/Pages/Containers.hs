-- | The Containers page: every running container, whatever the runtime.
--
-- Modelled on Datadog's Containers Explorer, which deliberately puts Kubernetes and Docker
-- containers in one table and makes the runtime a facet rather than a separate page. Datadog
-- has no Swarm view at all — Swarm is tags on the generic container list — so a single list is
-- the faithful shape, not a compromise.
--
-- One bounded query feeds the table, its facet menus and its counts; filtering and faceting
-- happen here in Haskell over that result. That keeps the store doing exactly one short-window
-- read per page view, which matters because wide aggregates over @otel_metrics@ are the query
-- shape that has repeatedly OOM-killed TimeFusion.
module Pages.Containers (containersGetH, containerDetailGetH, ContainersGet (..), ContainerVM (..), ContainerFilters (..), applyFilters, runtimeLabel, formatBytes, coresText, freshnessLabel, emDash_, readyBadge_, kqlFilter, nodeField, clusterField) where

import Data.Default (def)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (NominalDiffTime, diffUTCTime)
import Data.Vector qualified as V
import Effectful.Reader.Static qualified as Reader
import Effectful.Time qualified as Time
import Lucid
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Containers (ContainerRow (..), Runtime (..), Scope (..), containersInWindowCached, cpuPctOfLimit, freshnessWindow, memPctOfLimit, runtimeOf)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx, navTabAttrs)
import Pages.Components (Deferred (..), EmptyStateAction (..), EmptyStateCfg (..), RefreshingDeferred (..), emptyState_, factGrid_, metaChip_, tableSkeleton_, timeRefreshListener_, withDeferredBody)
import Pkg.Components.Table (Column, Config (..), Features (..), SearchMode (..), Table (..), ZeroState (..), col, facetActions, facetValues, singleSelectFilter, withAttrs, withColHeaderExtra)
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget qualified as Widget
import Relude
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (drawerRowAttrs_, faSprite_, formatBytes, infrastructureNavTabs_, kqlQuoted, showFFloat')


-- | A row plus the project it belongs to, so column renderers can build pivot links without
-- every one of them taking the id as a second argument.
data ContainerVM = ContainerVM
  { pid :: Projects.ProjectId
  , window :: TimePicker.TimeWindow
  , row :: ContainerRow
  }


-- | The four facets, exactly as they arrive from the query string. Single-select: one
-- namespace, one node, one image, one runtime at a time. Datadog allows multi-select here;
-- one value per facet is the honest v1 and keeps the handler signature readable.
data ContainerFilters = ContainerFilters
  { runtime :: Maybe Text
  , namespace :: Maybe Text
  , node :: Maybe Text
  , image :: Maybe Text
  , cluster :: Maybe Text
  }
  deriving stock (Eq, Show)


runtimeLabel :: Runtime -> Text
runtimeLabel = \case Kubernetes -> "kubernetes"; Docker -> "docker"; Host -> "host"


-- | Narrow the list to the selected facets. An absent or empty facet matches everything, so
-- clearing a filter is just dropping the query parameter.
--
-- >>> let r n ns = (emptyRow n) {namespace = ns, podName = Just "p"}
-- >>> let rows = V.fromList [r "a" (Just "default"), r "b" (Just "kube-system")]
-- >>> V.map (.containerName) $ applyFilters (nsFilter (Just "default")) rows
-- ["a"]
-- >>> V.map (.containerName) $ applyFilters noFilters rows
-- ["a","b"]
-- >>> V.map (.containerName) $ applyFilters (nsFilter (Just "")) rows
-- ["a","b"]
applyFilters :: ContainerFilters -> V.Vector ContainerRow -> V.Vector ContainerRow
applyFilters f = V.filter \r ->
  and
    [ matches f.runtime (Just $ runtimeLabel $ runtimeOf r)
    , matches f.namespace r.namespace
    , matches f.node r.nodeName
    , matches f.image r.image
    , matches f.cluster r.cluster
    ]
  where
    matches selected actual = all (\s -> T.null s || Just s == actual) selected


containersGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders ContainersGet)
containersGetH pid runtimeM namespaceM nodeM imageM clusterM fromParam toParam sinceParam deferredM = do
  (_, _, bw) <- mkPageCtx pid
  appCtx <- Reader.ask @AuthContext
  now <- Time.currentTime
  let window = TimePicker.mkTimeWindow now fromParam toParam sinceParam
      filters = ContainerFilters runtimeM namespaceM nodeM imageM clusterM
      path = "/p/" <> pid.toText <> "/infrastructure/containers"
      baseUrl = TimePicker.windowUrl path [] window
      deferredUrl = TimePicker.windowUrl path (mapMaybe sequenceA [("runtime", runtimeM), ("namespace", namespaceM), ("node", nodeM), ("image", imageM), ("cluster", clusterM)] <> [("deferred", "1")]) window
  body <- withDeferredBody deferredM "containersContainer" deferredUrl (tableSkeleton_ 10) do
    allRows <- containersInWindowCached appCtx.infrastructureCache (pid, window.fromQuery, window.toQuery, window.sinceQuery) (TimePicker.cacheTtl window) appCtx.env.enableTimefusionReads pid window.fromTime window.toTime
    let
      -- Busiest first: during an incident the container burning CPU is the one you came for.
      rows = sortOn (Down . (.cpuCores)) $ V.toList $ applyFilters filters allRows
      -- Facet values come from the unfiltered result, so choosing a namespace never empties
      -- the node or image menus — the way a facet bar is expected to behave.
      menu label param selected fromRow = singleSelectFilter label param selected $ facetValues fromRow allRows
    pure
      Table
        { -- bulkActionsInHeader is what puts the header action group — and with it the whole
          -- filter dropdown — into a column header. Leaving it unset renders no facet menus
          -- at all, however many are configured.
          config = def{elemID = "containersForm", containerId = Just "containersContainer", addPadding = True, renderAsTable = True, bulkActionsInHeader = Just 0}
        , columns = containerColumns
        , rows = V.fromList $ map (ContainerVM pid window) rows
        , features =
            def
              { header = Just $ timeRefreshListener_ "containersContainer" deferredUrl
              , search = Just ClientSide
              , searchPlaceholder = Just "Search containers"
              , rowAttrs = Just $ drawerRowAttrs_ . detailUrl
              , tableHeaderActions =
                  Just
                    $ facetActions
                      baseUrl
                      "containersContainer"
                      [ menu "Runtime" "runtime" filters.runtime (Just . runtimeLabel . runtimeOf)
                      , -- Falls back to k8s.cluster.uid, so the menu is populated even
                        -- before a collector sets the human-readable cluster name.
                        menu "Cluster" "cluster" filters.cluster (.cluster)
                      , menu "Namespace" "namespace" filters.namespace (.namespace)
                      , menu "Node" "node" filters.node (.nodeName)
                      , menu "Image" "image" filters.image (.image)
                      ]
              , showFilterRail = True
              , resultSummary = Just $ "Showing " <> show (length rows) <> " of " <> show (V.length allRows) <> " containers reporting in the final " <> freshnessLabel window <> " of this range"
              , exportName = Just "containers"
              , zeroState =
                  Just
                    ZeroState
                      { icon = "cube"
                      , title = "No containers reporting"
                      , description = "Point an OpenTelemetry Collector with the kubeletstats, k8s_cluster or docker_stats receivers at this project and your containers appear here."
                      , action = ESLink "https://monoscope.tech/docs/sdks/infrastructure/kubernetes" "Collector setup guide"
                      }
              }
        }
  let bwconf =
        bw
          { prePageTitle = Nothing
          , pageTitle = "Infrastructure"
          , menuItem = Just "Infrastructure"
          , navTabs = Just $ infrastructureNavTabs_ pid "Containers" window.fromQuery window.toQuery window.sinceQuery
          , pageActions = Just $ div_ [class_ "inline-flex items-center gap-2", data_ "default-window" "5M"] do
              TimePicker.liveDataControls_ Nothing window.currentRange Nothing TimePicker.RefreshOnly
          , serviceOptions = V.empty
          }
  addRespHeaders $ ContainersPage $ PageCtx bwconf body
  where
    containerColumns :: [Column ContainerVM]
    containerColumns =
      [ col "Container" renderNameCol & withAttrs [class_ "w-80 max-w-80 overflow-hidden max-md:w-auto max-md:max-w-none"]
      , col "Pod" (textCell (.podName)) & withAttrs [class_ "w-72 max-w-72 overflow-hidden max-lg:hidden"]
      , col "Namespace" (textCell (.namespace)) & withAttrs [class_ "w-32 max-lg:hidden"]
      , col "Node" (textCell (.nodeName)) & withAttrs [class_ "w-36 max-md:hidden"]
      , col "CPU (cores)" (numCell (showFFloat' 3) (.cpuCores)) & withAttrs [class_ "w-24 text-right"]
      , col "CPU limit used" (pctCell "CPU limit used" cpuPctOfLimit) & withAttrs [class_ "w-36 max-md:hidden"]
      , col "Memory" (numCell formatBytes (.memBytes)) & withAttrs [class_ "w-28 text-right max-md:hidden"]
      , col "Memory limit used" (pctCell "Memory limit used" memPctOfLimit) & withAttrs [class_ "w-40 max-md:hidden"]
      , col "Restarts" (numCell (showFFloat' 0) (.restarts))
          & withColHeaderExtra
            ( let explanation = "Latest cumulative restart count reported within the selected time range. This is not the number of restarts during the range."
               in span_
                    [ class_ "inline-flex cursor-help rounded-sm text-iconNeutral focus-visible:outline-2 focus-visible:outline-offset-2"
                    , tabindex_ "0"
                    , role_ "note"
                    , term "aria-label" explanation
                    , data_ "tippy-content" explanation
                    ]
                    $ faSprite_ "circle-info" "regular" "h-3 w-3"
            )
          & withAttrs [class_ "w-20 text-right max-md:hidden"]
      , col "Ready" (readyCell . (.row)) & withAttrs [class_ "w-16 max-md:hidden"]
      ]

    -- A cell is its accessor plus how to draw a present value; absence is always the em dash.
    cell :: (ContainerRow -> Maybe a) -> (a -> Html ()) -> ContainerVM -> Html ()
    cell f render vm = maybe emDash_ render (f vm.row)

    textCell :: (ContainerRow -> Maybe Text) -> ContainerVM -> Html ()
    textCell f = cell f \v -> span_ [class_ "truncate block text-textWeak", data_ "tippy-content" v] $ toHtml v

    numCell :: (Double -> Text) -> (ContainerRow -> Maybe Double) -> ContainerVM -> Html ()
    numCell fmt f = cell f (span_ [class_ "whitespace-nowrap text-textStrong tabular-nums"] . toHtml . fmt)

    -- Percentage plus a bar. The bar is the second, non-colour signal the design principles
    -- require: at a glance you read fullness from length even where colour is unavailable.
    pctCell :: Text -> (ContainerRow -> Maybe Double) -> ContainerVM -> Html ()
    pctCell label f = cell f \frac ->
      let value = showFFloat' 0 (frac * 100) <> "%"
       in div_ [class_ "flex items-center gap-1.5", data_ "tippy-content" $ label <> ": " <> value] do
            span_ [class_ "tabular-nums text-textStrong w-10 text-right shrink-0"] $ toHtml value
            div_ [class_ "h-1.5 grow rounded-full bg-fillWeak overflow-hidden"]
              -- Length is the non-colour cue. Hue adds the operational threshold.
              $ div_ [class_ $ "h-full rounded-full " <> if | frac >= 0.9 -> "bg-fillError-strong" | frac >= 0.75 -> "bg-fillWarning-strong" | otherwise -> "bg-fillBrand-strong", style_ $ "width:" <> showFFloat' 0 (min 1 frac * 100) <> "%"] mempty

    renderNameCol :: ContainerVM -> Html ()
    renderNameCol vm = div_ [class_ "flex flex-col gap-0.5 min-w-0 overflow-hidden"] do
      div_ [class_ "flex items-center gap-2 min-w-0"] do
        span_ [class_ "tooltip tooltip-right shrink-0 inline-flex", term "data-tip" $ "Runtime: " <> runtimeLabel (runtimeOf vm.row)]
          $ runtimeIcon_ vm.row "w-3.5 h-3.5 fill-iconNeutral"
        span_ [class_ "min-w-0 truncate font-medium text-textStrong", data_ "tippy-content" $ "Container: " <> vm.row.containerName] $ toHtml vm.row.containerName
        span_ [class_ "hidden max-md:inline-flex"] $ readyCell vm.row
      div_ [class_ "flex items-center gap-2 min-w-0 overflow-hidden text-xs text-textWeak max-md:flex-wrap"] do
        whenJust vm.row.image \img ->
          let image = shortImage img <> maybe "" (":" <>) vm.row.imageTag
           in span_ [class_ "min-w-0 flex-1 truncate", data_ "tippy-content" $ "Image: " <> image] $ toHtml image
        -- The badge is a flex box, so the ellipsis has to live on an inner block or the name clips at its start.
        whenJust vm.row.workload \workload -> span_ [class_ "badge badge-xs badge-ghost min-w-0 max-w-36 truncate", data_ "tippy-content" $ "Workload: " <> workload] $ span_ [class_ "min-w-0 truncate"] $ toHtml workload
      div_ [class_ "hidden max-lg:flex flex-col gap-0.5 text-xs leading-normal text-textWeak"]
        $ forM_ ([("Pod", vm.row.podName), ("Namespace", vm.row.namespace), ("Cluster", vm.row.cluster), ("Node / host", vm.row.nodeName)] :: [(Text, Maybe Text)]) \(label, value) ->
          whenJust value \v -> span_ [class_ "whitespace-normal break-all"] $ toHtml $ label <> ": " <> v

    -- The drawer reads the window the row was rendered from, so it shows the same numbers the
    -- row does and hits the same cached snapshot the list already fetched.
    detailUrl :: ContainerVM -> Text
    detailUrl vm =
      TimePicker.windowUrl
        ("/p/" <> vm.pid.toText <> "/infrastructure/containers/detail")
        ([("container", vm.row.containerName), ("scope", display vm.row.scope)] <> mapMaybe sequenceA [("pod", vm.row.podName), ("cluster", vm.row.cluster), ("namespace", vm.row.namespace), ("node", vm.row.nodeName)])
        vm.window


newtype ContainersGet = ContainersPage (PageCtx (Deferred (Table ContainerVM)))
  deriving (ToHtml) via (PageCtx (RefreshingDeferred (Table ContainerVM)))


-- | A missing value is an em dash, never a zero. Datadog is explicit that without a limit it
-- "cannot infer the usage percentage", and a fabricated 0% read at 3 AM is worse than a blank.
emDash_ :: Html ()
emDash_ = span_ [class_ "text-textWeak"] "—"


readyCell :: ContainerRow -> Html ()
readyCell row = maybe emDash_ (readyBadge_ . (> 0)) row.ready


-- | Ready is the expected state, so it reads as text with a dot; only the exception gets a filled badge.
readyBadge_ :: Bool -> Html ()
readyBadge_ True = span_ [class_ "inline-flex items-center gap-1.5 whitespace-nowrap text-textStrong", data_ "tippy-content" "Latest reported readiness: Ready"] $ span_ [class_ "h-1.5 w-1.5 rounded-full bg-fillSuccess-strong", term "aria-hidden" "true"] mempty >> "Ready"
readyBadge_ False = span_ [class_ "badge badge-sm badge-error whitespace-nowrap", data_ "tippy-content" "Latest reported readiness: Not ready"] "Not ready"


coresText :: Double -> Text
coresText value = showFFloat' 3 value <> " cores"


-- | How far back the snapshot actually looked: 'freshnessWindow', or the whole range when that is shorter.
freshnessLabel :: TimePicker.TimeWindow -> Text
freshnessLabel window =
  let effectiveDuration = min freshnessWindow $ diffUTCTime window.toTime window.fromTime
      (minutes, fraction) = properFraction (effectiveDuration / 60) :: (Integer, NominalDiffTime)
   in if fraction == 0 then show minutes <> "m" else show effectiveDuration


runtimeIcon_ :: ContainerRow -> Text -> Html ()
runtimeIcon_ r = faSprite_ (case runtimeOf r of Kubernetes -> "cube"; Docker -> "layer-group"; Host -> "server") "solid"


-- | Datadog's @short_image@: the last path segment, which is the lowest-cardinality useful
-- image identity and the reason its scatter plot groups on it by default.
--
-- >>> shortImage "ghcr.io/open-telemetry/demo"
-- "demo"
-- >>> shortImage "redis"
-- "redis"
shortImage :: Text -> Text
shortImage = T.takeWhileEnd (/= '/')


-- | The drawer for one container. It reuses the list query rather than adding a second one:
-- the window is short and the result already carries every field the panel shows.
containerDetailGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Scope -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
containerDetailGetH pid containerM podM scopeM clusterM namespaceM nodeM fromParam toParam sinceParam = do
  appCtx <- Reader.ask @AuthContext
  now <- Time.currentTime
  let window = TimePicker.mkTimeWindow now fromParam toParam sinceParam
  rows <- containersInWindowCached appCtx.infrastructureCache (pid, window.fromQuery, window.toQuery, window.sinceQuery) (TimePicker.cacheTtl window) appCtx.env.enableTimefusionReads pid window.fromTime window.toTime
  -- Generated links identify missing metadata exactly; legacy name/pod links must be unique.
  let found = V.filter (\r -> Just r.containerName == containerM && r.podName == podM && maybe True (\scope -> (r.scope, r.cluster, r.namespace, r.nodeName) == (scope, clusterM, namespaceM, nodeM)) scopeM) rows
  addRespHeaders $ case V.toList found of
    [r] -> containerDetail_ window r
    [] -> emptyState_ def{icon = Just "cube", action = ESNone} "This container is no longer reporting." ""
    _ -> emptyState_ def{icon = Just "cube", action = ESLink (TimePicker.windowUrl ("/p/" <> pid.toText <> "/infrastructure/containers") [] window) "View containers"} "Several containers match this link." "Select a container from the list to view its metrics."
  where
    containerDetail_ :: TimePicker.TimeWindow -> ContainerRow -> Html ()
    containerDetail_ window r = div_ [class_ "min-h-full"] do
      header_ [class_ "border-b border-strokeWeak px-5 py-4"] do
        div_ [class_ "flex flex-wrap items-center gap-2"] do
          runtimeIcon_ r "h-4 w-4 text-iconNeutral"
          h2_ [class_ "break-all text-lg font-semibold text-textStrong"] $ toHtml r.containerName
          readyCell r
        whenJust r.image \image -> div_ [class_ "mt-1 break-all text-xs text-textWeak"] $ toHtml $ image <> maybe "" (":" <>) r.imageTag
        div_ [class_ "mt-2 flex flex-wrap gap-1.5"] $ forM_ metadata $ uncurry metaChip_
      main_ [class_ "space-y-5 p-5"] do
        section_ [class_ "space-y-3"] do
          div_ [class_ "flex flex-wrap items-center justify-between gap-2"] do
            h3_ [class_ "font-semibold text-textStrong"] "Container summary"
            span_ [class_ "text-xs text-textWeak"] $ toHtml $ "Signal coverage " <> show availableSignals <> "/4"
          factGrid_
            "grid-cols-4 max-sm:grid-cols-2"
            [ ("CPU used", dash $ coresText <$> r.cpuCores)
            , ("CPU / limit", pct $ cpuPctOfLimit r)
            , ("Memory used", dash $ formatBytes <$> r.memBytes)
            , ("Memory / limit", pct $ memPctOfLimit r)
            ]
          when (availableSignals < 4) $ p_ [class_ "rounded-md bg-fillInformation-weak px-3 py-2 text-sm text-textWeak"] "Usage or limit telemetry is missing in this time range. A dash means the collector did not report that signal; it is never treated as zero."
        section_ [class_ "space-y-2 border-t border-strokeWeak pt-4"] do
          h3_ [class_ "font-semibold text-textStrong"] "Requests and limits"
          table_ [class_ "table table-xs rounded-lg border border-strokeWeak"] do
            thead_ $ tr_ $ mapM_ (th_ . toHtml) (["", "Request", "Limit"] :: [Text])
            tbody_ do
              tr_ $ mapM_ (td_ . toHtml) ["CPU (cores)", dash (showFFloat' 3 <$> r.cpuRequest), dash (showFFloat' 3 <$> r.cpuLimit)]
              tr_ $ mapM_ (td_ . toHtml) ["Memory", dash (formatBytes <$> r.memRequest), dash (formatBytes <$> r.memLimit)]
        section_ [class_ "space-y-3 border-t border-strokeWeak pt-4"] do
          h3_ [class_ "font-semibold text-textStrong"] "Metrics"
          div_ [class_ "grid grid-cols-2 gap-3 max-xl:grid-cols-1"]
            $ forM_ ([("container-detail-cpu", "CPU usage", "cores", "container.cpu.usage"), ("container-detail-memory", "Memory working set", "bytes", "container.memory.working_set")] :: [(Text, Text, Text, Text)]) \(wid, title, unit, metric) ->
              div_ [class_ "min-h-56"] $ Widget.widget_ $ infrastructureWidget pid wid title unit (metricQuery metric)
        div_ [class_ "flex flex-wrap gap-2 border-t border-strokeWeak pt-4"] $ forM_ pivots \(label, url) ->
          a_ ([href_ url, class_ "btn btn-sm"] <> navTabAttrs) $ toHtml label
      where
        dash = fromMaybe "—"
        pct = dash . fmap (\value -> showFFloat' 0 (value * 100) <> "%")
        metadata = mapMaybe sequenceA [("Cluster", r.cluster), ("Pod", r.podName), ("Namespace", r.namespace), ("Node / host", r.nodeName), ("Workload", r.workload)]
        availableSignals = length $ catMaybes [r.cpuCores, r.cpuLimit, r.memBytes, r.memLimit]
        subject =
          kqlFilter
            $ ( case r.scope of
                  ScopeContainer -> [("coalesce(resource.k8s.container.name, resource.container.name)", Just r.containerName), ("resource.k8s.pod.name", r.podName)]
                  ScopePod -> [("resource.k8s.pod.name", Just r.containerName)]
                  ScopeHost -> [(nodeField, Just r.containerName)]
              )
            <> if r.scope == ScopeHost then [] else [("resource.k8s.namespace.name", r.namespace), (clusterField, r.cluster), (nodeField, r.nodeName)]
        metricQuery metric = "metrics | where metric_name == \"" <> metric <> "\" and " <> subject <> " | summarize avg(value) by bin_auto(timestamp)"
        pivots =
          [ ("View logs", TimePicker.windowUrl ("/p/" <> pid.toText <> "/log_explorer") [("query", subject)] window)
          , ("View metrics", TimePicker.windowUrl ("/p/" <> pid.toText <> "/metrics") [("metric_prefix", maybe "container." (const "k8s.") r.podName)] window)
          ]


-- | Equality on every field; a missing value must be absent (@== null@), not unconstrained.
kqlFilter :: [(Text, Maybe Text)] -> Text
kqlFilter fields = T.intercalate " and " [field <> "==" <> maybe "null" kqlQuoted value | (field, value) <- fields]


nodeField, clusterField :: Text
nodeField = "coalesce(resource.k8s.node.name, resource.host.name)"
clusterField = "coalesce(resource.k8s.cluster.name, resource.k8s.cluster.uid)"


infrastructureWidget :: Projects.ProjectId -> Text -> Text -> Text -> Text -> Widget.Widget
infrastructureWidget pid wid title unit query =
  (Widget.infraTimeseries pid wid title unit query)
    { Widget.description = Just $ "Average " <> (case unit of "cores" -> "CPU usage"; "bytes" -> "working-set memory"; _ -> "value") <> " reported for each container in every time bucket."
    }


-- $setup
-- `namespace` names a field of both ContainerFilters and ContainerRow, so a record update
-- mentioning only it is ambiguous in the doctest session. These build filters
-- positionally rather than by update.
-- >>> :set -XOverloadedStrings -XOverloadedRecordDot
-- >>> import Data.Vector qualified as V
-- >>> import Models.Telemetry.Containers (ContainerRow (..), Scope (..))
-- >>> let emptyRow n = ContainerRow n ScopeContainer Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
-- >>> let noFilters = ContainerFilters Nothing Nothing Nothing Nothing Nothing
-- >>> let nsFilter ns = ContainerFilters Nothing ns Nothing Nothing Nothing
