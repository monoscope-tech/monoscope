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
module Pages.Containers (containersGetH, containerDetailGetH, ContainersGet (..), ContainerVM (..), ContainerFilters (..), applyFilters, facetValues) where

import Data.Default (def)
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful.Reader.Static qualified as Reader
import Effectful.Time qualified as Time
import Lucid
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Containers (ContainerRow (..), Runtime (..), containersInWindow, cpuPctOfLimit, memPctOfLimit, runtimeOf)
import Numeric (showFFloat)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx, navTabAttrs)
import Pkg.Components.Table (Column, Config (..), Features (..), FilterMenu (..), FilterOption (..), SearchMode (..), Table (..), TableHeaderActions (..), ZeroState (..), col, withAttrs)
import Relude
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (drawerLoadAttrs_, explorerNavTabs_, faSprite_, toUriStr)


-- | A row plus the project it belongs to, so column renderers can build pivot links without
-- every one of them taking the id as a second argument.
data ContainerVM = ContainerVM
  { pid :: Projects.ProjectId
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
  }
  deriving stock (Eq, Show)


runtimeLabel :: Runtime -> Text
runtimeLabel = \case Kubernetes -> "kubernetes"; Docker -> "docker"


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
    ]
  where
    matches selected actual = maybe True (\s -> T.null s || Just s == actual) selected


-- | The distinct values a facet offers, sorted, drawn from the unfiltered result so selecting
-- one facet never empties the menus of the others.
--
-- >>> facetValues (.namespace) (V.fromList [r "a" (Just "b"), r "c" (Just "a"), r "d" Nothing])
-- ["a","b"]
facetValues :: (ContainerRow -> Maybe Text) -> V.Vector ContainerRow -> [Text]
facetValues f = Relude.sort . ordNub . filter (not . T.null) . mapMaybe f . V.toList


containersGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders ContainersGet)
containersGetH pid runtimeM namespaceM nodeM imageM = do
  (_, _, bw) <- mkPageCtx pid
  appCtx <- Reader.ask @AuthContext
  now <- Time.currentTime
  allRows <- containersInWindow appCtx.env.enableTimefusionReads pid now
  let filters = ContainerFilters runtimeM namespaceM nodeM imageM
      -- Busiest first: during an incident the container burning CPU is the one you came for.
      rows = sortOn (Down . (.cpuCores)) $ V.toList $ applyFilters filters allRows
      baseUrl = "/p/" <> pid.toText <> "/containers"
      -- Facet values come from the unfiltered result, so choosing a namespace never empties
      -- the node or image menus — the way a facet bar is expected to behave.
      menu label param selected fromRow =
        FilterMenu
          { label
          , paramName = param
          , multiSelect = False
          , options = [FilterOption{label = v, value = v, isActive = Just v == selected} | v <- facetValues fromRow allRows]
          }
      table =
        Table
          { -- bulkActionsInHeader is what puts the header action group — and with it the whole
            -- filter dropdown — into a column header. Leaving it unset renders no facet menus
            -- at all, however many are configured.
            config = def{elemID = "containersForm", containerId = Just "containersContainer", addPadding = True, renderAsTable = True, bulkActionsInHeader = Just 0}
          , columns = containerColumns
          , rows = V.fromList $ map (ContainerVM pid) rows
          , features =
              def
                { search = Just ClientSide
                , rowAttrs = Just $ const [class_ "group/row hover:bg-fillWeaker"]
                , tableHeaderActions =
                    Just
                      TableHeaderActions
                        { baseUrl
                        , targetId = "containersContainer"
                        , sortOptions = []
                        , currentSort = ""
                        , headerExtra = Nothing
                        , filterMenus =
                            [ menu "Runtime" "runtime" filters.runtime (Just . runtimeLabel . runtimeOf)
                            , menu "Namespace" "namespace" filters.namespace (.namespace)
                            , menu "Node" "node" filters.node (.nodeName)
                            , menu "Image" "image" filters.image (.image)
                            ]
                        , activeFilters = activeFilterChips filters
                        }
                , zeroState =
                    Just
                      ZeroState
                        { icon = "cube"
                        , title = "No containers reporting"
                        , description = "Point an OpenTelemetry Collector with the kubeletstats, k8s_cluster or docker_stats receivers at this project and your containers appear here."
                        , actionText = "Collector setup guide"
                        , destination = Right "https://monoscope.tech/docs/sdks/infrastructure/kubernetes"
                        }
                }
          }
      bwconf =
        bw
          { prePageTitle = Just "Explorer"
          , pageTitle = "Containers"
          , menuItem = Just "Explorer"
          , navTabs = Just $ explorerNavTabs_ pid "Containers"
          }
  addRespHeaders $ ContainersPage $ PageCtx bwconf table


activeFilterChips :: ContainerFilters -> [(Text, [Text])]
activeFilterChips f =
  [ (label, [v])
  | (label, sel) <- [("Runtime", f.runtime), ("Namespace", f.namespace), ("Node", f.node), ("Image", f.image)]
  , v <- maybeToList sel
  , not (T.null v)
  ]


newtype ContainersGet = ContainersPage (PageCtx (Table ContainerVM))


instance ToHtml ContainersGet where
  toHtml (ContainersPage pg) = toHtml pg
  toHtmlRaw = toHtml


containerColumns :: [Column ContainerVM]
containerColumns =
  [ col "Container" renderNameCol & withAttrs [class_ "min-w-0 max-w-0 w-full"]
  , col "Pod" (textCell (.podName)) & withAttrs [class_ "w-48 max-lg:hidden"]
  , col "Namespace" (textCell (.namespace)) & withAttrs [class_ "w-32 max-lg:hidden"]
  , col "Node" (textCell (.nodeName)) & withAttrs [class_ "w-36 max-md:hidden"]
  , col "CPU" (numCell 3 (.cpuCores)) & withAttrs [class_ "w-20 text-right"]
  , col "CPU % lim" (pctCell cpuPctOfLimit) & withAttrs [class_ "w-24 max-md:hidden"]
  , col "Memory" (\vm -> plainCell $ formatBytes <$> vm.row.memBytes) & withAttrs [class_ "w-24 text-right"]
  , col "Mem % lim" (pctCell memPctOfLimit) & withAttrs [class_ "w-24 max-md:hidden"]
  , col "Restarts" (numCell 0 (.restarts)) & withAttrs [class_ "w-20 text-right max-md:hidden"]
  , col "Ready" readyCell & withAttrs [class_ "w-16 max-md:hidden"]
  ]


-- | A missing value is an em dash, never a zero. Datadog is explicit that without a limit it
-- "cannot infer the usage percentage", and a fabricated 0% read at 3 AM is worse than a blank.
plainCell :: Maybe Text -> Html ()
plainCell = maybe (span_ [class_ "text-textWeak"] "—") (span_ [class_ "text-textStrong tabular-nums"] . toHtml)


textCell :: (ContainerRow -> Maybe Text) -> ContainerVM -> Html ()
textCell f vm = case f vm.row of
  Nothing -> span_ [class_ "text-textWeak"] "—"
  Just v -> span_ [class_ "truncate block text-textWeak", term "data-tippy-content" v] $ toHtml v


numCell :: Int -> (ContainerRow -> Maybe Double) -> ContainerVM -> Html ()
numCell places f vm = plainCell $ toText . showFFloat' places <$> f vm.row


-- | Percentage plus a bar. The bar is the second, non-colour signal the design principles
-- require: at a glance you read fullness from length even where colour is unavailable.
pctCell :: (ContainerRow -> Maybe Double) -> ContainerVM -> Html ()
pctCell f vm = case f vm.row of
  Nothing -> span_ [class_ "text-textWeak"] "—"
  Just frac -> div_ [class_ "flex items-center gap-1.5"] do
    span_ [class_ "tabular-nums text-textStrong w-10 text-right shrink-0"] $ toHtml $ showFFloat' 0 (frac * 100) <> "%"
    div_ [class_ "h-1.5 grow rounded-full bg-fillWeak overflow-hidden"]
      -- Ratios legitimately exceed 1.0, so the bar clamps while the number keeps the truth.
      $ div_ [class_ $ "h-full rounded-full " <> if frac >= 0.9 then "bg-fillError-strong" else "bg-fillBrand-strong", style_ $ "width:" <> showFFloat' 0 (min 1 frac * 100) <> "%"] mempty


readyCell :: ContainerVM -> Html ()
readyCell vm = case vm.row.ready of
  Nothing -> span_ [class_ "text-textWeak"] "—"
  Just v
    | v > 0 -> span_ [class_ "badge badge-sm badge-success"] "Ready"
    | otherwise -> span_ [class_ "badge badge-sm badge-error"] "Not ready"


renderNameCol :: ContainerVM -> Html ()
renderNameCol vm = div_ [class_ "flex flex-col gap-0.5 min-w-0"] do
  div_ [class_ "flex items-center gap-2 min-w-0"] do
    span_ [class_ "tooltip tooltip-right shrink-0 inline-flex", term "data-tip" $ runtimeLabel $ runtimeOf vm.row]
      $ faSprite_ (case runtimeOf vm.row of Kubernetes -> "cube"; Docker -> "layer-group") "solid" "w-3.5 h-3.5 fill-iconNeutral"
    button_
      ( [class_ "font-medium text-textStrong hover:text-textBrand transition-colors truncate min-w-0 text-left cursor-pointer", type_ "button"]
          <> drawerLoadAttrs_ (detailUrl vm)
      )
      $ toHtml vm.row.containerName
  div_ [class_ "flex items-center gap-2 min-w-0 text-xs text-textWeak"] do
    whenJust vm.row.image \img -> span_ [class_ "truncate"] $ toHtml $ shortImage img <> maybe "" (":" <>) vm.row.imageTag
    whenJust vm.row.workload $ span_ [class_ "badge badge-xs badge-ghost shrink-0"] . toHtml


-- | Datadog's @short_image@: the last path segment, which is the lowest-cardinality useful
-- image identity and the reason its scatter plot groups on it by default.
--
-- >>> shortImage "ghcr.io/open-telemetry/demo"
-- "demo"
-- >>> shortImage "redis"
-- "redis"
shortImage :: Text -> Text
shortImage = T.takeWhileEnd (/= '/')


detailUrl :: ContainerVM -> Text
detailUrl vm =
  "/p/"
    <> vm.pid.toText
    <> "/containers/detail?container="
    <> toUriStr vm.row.containerName
    <> foldMap (("&pod=" <>) . toUriStr) vm.row.podName


-- | The drawer for one container. It reuses the list query rather than adding a second one:
-- the window is short and the result already carries every field the panel shows.
containerDetailGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
containerDetailGetH pid containerM podM = do
  appCtx <- Reader.ask @AuthContext
  now <- Time.currentTime
  rows <- containersInWindow appCtx.env.enableTimefusionReads pid now
  let found = V.find (\r -> Just r.containerName == containerM && r.podName == podM) rows
  addRespHeaders $ maybe (div_ [class_ "p-4 text-textWeak"] "This container is no longer reporting.") (containerDetail_ pid) found


containerDetail_ :: Projects.ProjectId -> ContainerRow -> Html ()
containerDetail_ pid r = div_ [class_ "flex flex-col gap-4 p-4"] do
  div_ [class_ "flex flex-col gap-1"] do
    h2_ [class_ "text-lg font-semibold text-textStrong break-all"] $ toHtml r.containerName
    div_ [class_ "text-xs text-textWeak break-all"] $ toHtml $ fromMaybe "" r.image <> maybe "" (":" <>) r.imageTag

  dl_ [class_ "grid grid-cols-2 gap-x-4 gap-y-2 text-sm"] $ forM_ facts \(k, v) -> do
    dt_ [class_ "text-textWeak"] $ toHtml k
    dd_ [class_ "text-textStrong tabular-nums break-all"] $ toHtml v

  -- Requests and limits get their own block: they are the denominators the list's percentage
  -- columns use, so when a cell reads "—" this is where you see why.
  div_ [class_ "flex flex-col gap-1"] do
    h3_ [class_ "text-xs font-semibold uppercase text-textWeak"] "Requests and limits"
    table_ [class_ "table table-xs"] do
      thead_ $ tr_ $ mapM_ (th_ . toHtml) (["", "Request", "Limit"] :: [Text])
      tbody_ do
        tr_ $ mapM_ (td_ . toHtml) ["CPU (cores)", dash (showFFloat' 3 <$> r.cpuRequest), dash (showFFloat' 3 <$> r.cpuLimit)]
        tr_ $ mapM_ (td_ . toHtml) ["Memory", dash (formatBytes <$> r.memRequest), dash (formatBytes <$> r.memLimit)]

  div_ [class_ "flex flex-wrap gap-2"] $ forM_ pivots \(label, url) ->
    a_ ([href_ url, class_ "btn btn-sm btn-outline"] <> navTabAttrs) $ toHtml label
  where
    dash = fromMaybe "—"
    facts =
      [(k, v) | (k, Just v) <- [("Pod", r.podName), ("Namespace", r.namespace), ("Node / host", r.nodeName), ("Workload", r.workload)]]
        <> [("Restarts", showFFloat' 0 v) | Just v <- [r.restarts]]
    -- The list query names flattened columns directly; KQL on the metrics source lowers
    -- resource.* to a JSON probe instead, which is the form the shipped container dashboards
    -- already use — so these pivots are written the KQL way, not the column way.
    kqlSubject = maybe ("resource.container.name==\"" <> r.containerName <> "\"") (\p -> "resource.k8s.pod.name==\"" <> p <> "\"") r.podName
    pivots =
      [ ("View logs", "/p/" <> pid.toText <> "/log_explorer?query=" <> toUriStr kqlSubject)
      , ("View metrics", "/p/" <> pid.toText <> "/metrics?metric_prefix=" <> toUriStr (maybe "container." (const "k8s.") r.podName))
      ]


-- | Fixed-decimal rendering.
--
-- >>> showFFloat' 3 0.0158
-- "0.016"
-- >>> showFFloat' 0 79.6
-- "80"
showFFloat' :: Int -> Double -> Text
showFFloat' places x = toText $ showFFloat (Just places) x ""


-- | Binary units, matching how a kubelet limit is written (@1Gi@, not @1GB@).
--
-- >>> formatBytes 0
-- "0 B"
-- >>> formatBytes 1536
-- "1.5 KiB"
-- >>> formatBytes 1073741824
-- "1 GiB"
formatBytes :: Double -> Text
formatBytes = go ["B", "KiB", "MiB", "GiB", "TiB", "PiB"]
  where
    go (u : us) v
      | abs v >= 1024 && not (null us) = go us (v / 1024)
      | otherwise = trim v <> " " <> u
    go [] v = showFFloat' 1 v
    trim v = let r = showFFloat' 1 v in fromMaybe r (T.stripSuffix ".0" r)


-- $setup
-- `namespace` names a field of both ContainerFilters and ContainerRow, so a record update
-- mentioning only it is ambiguous in the doctest session. These build filters
-- positionally rather than by update.
-- >>> :set -XOverloadedStrings -XOverloadedRecordDot
-- >>> import Data.Vector qualified as V
-- >>> import Models.Telemetry.Containers (ContainerRow (..))
-- >>> let emptyRow n = ContainerRow n Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
-- >>> let noFilters = ContainerFilters Nothing Nothing Nothing Nothing
-- >>> let nsFilter ns = ContainerFilters Nothing ns Nothing Nothing
