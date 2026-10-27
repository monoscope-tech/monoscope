module Pkg.Components.Table (
  Table (..),
  inventoryTable,
  TableRows (..),
  TableResponse (..),
  Column (..),
  Config (..),
  Features (..),
  SelectControl (..),
  TableTabs (..),
  selectControl_,
  facetsConfig,
  countedFilter,
  SearchMode (..),
  BulkAction (..),
  TabFilter (..),
  TabFilterOpt (..),
  SortConfig (..),
  SortableConfig (..),
  TreeConfig (..),
  ZeroState (..),
  EmptyStateAction (..),
  -- Pagination
  Pagination (..),
  -- Header actions (sort/filter dropdowns in header)
  TableHeaderActions (..),
  FilterMenu (..),
  FilterOption (..),
  multiSelectFilter,
  facetActions,
  -- Sorting types and utilities
  SortOrder (..),
  SortField (..),
  parseSortParam,
  -- Column builders
  col,
  withSort,
  withAttrs,
  withColHeaderExtra,
) where

import Data.Default (Default (..))
import Data.Foldable.WithIndex (iforM_)
import Data.List (lookup)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as V
import GHC.Records (HasField (getField))
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Pages.BodyWrapper (PageCtx)
import Pages.Components (EmptyStateAction (..), EmptyStateCfg (..), emptyState_, filterInputAttr_, keyboardActivateAttr_, searchInput_, timeRefreshListener_)
import Pkg.Components.Facets (FacetChoice (..), FacetContent (..), FacetNode (..), FacetOverflow (..), facetChoice_)
import Pkg.Components.Facets qualified as Facets
import Relude
import Utils (TabStrip (..), deleteParam, faSprite_, navTabAttrs, navTabStrip_, popoverPanel_, popoverTrigger_, prettyPrintCount, toUriStr)


-- Core Types

type role Column nominal
data Column a where
  Column
    :: { name :: Text
       , render :: a -> Html ()
       , attrs :: [Attribute]
       , sortField :: Maybe Text
       , align :: Maybe Text
       , headerExtra :: Maybe (Html ()) -- Extra content rendered after column name in th
       }
    -> Column a


type role Table nominal
data Table a = Table
  { config :: Config
  , columns :: [Column a]
  , rows :: V.Vector a
  , features :: Features a
  }


inventoryTable :: Text -> Text -> Table a
inventoryTable name url =
  Table
    { config = def{elemID = name <> "Form", containerId = Just target, addPadding = True, renderAsTable = True}
    , columns = []
    , rows = V.empty
    , features = def{header = Just $ timeRefreshListener_ target url, search = Just ClientSide, searchPlaceholder = Just $ "Search " <> name, customizeColumns = True, exportName = Just name}
    }
  where
    target = name <> "Container"


-- TableRows for pagination - renders rows + pagination footer
type role TableRows nominal
data TableRows a = TableRows
  { columns :: [Column a]
  , rows :: V.Vector a
  , renderAsTable :: Bool
  , rowId :: Maybe (a -> Text)
  , rowGroup :: Maybe (a -> Maybe Text)
  , rowAttrs :: Maybe (a -> [Attribute])
  , pagination :: Maybe Pagination
  }


-- Pagination configuration for table footer
data Pagination = Pagination
  { currentPage :: Int -- 0-indexed
  , perPage :: Int -- 25, 50, or 100
  , totalCount :: Int -- Total items from COUNT(*) OVER()
  , baseUrl :: Text -- URL without page/per_page params
  , targetId :: Text -- HTMX target container id
  }
  deriving stock (Eq, Show)


data SearchMode = ClientSide | ServerSide Text


-- | Features default to no optional behavior.
--
-- >>> let html = renderText $ toHtml (Table def [] V.empty def :: Table ())
-- >>> any @[] (`TL.isInfixOf` html) ["facet-rail", "<input", "<button", "<select", "data-nav-tab-strip"]
-- False
data Features a = Features
  { rowLink :: Maybe (a -> Text)
  , rowId :: Maybe (a -> Text)
  , rowLabel :: Maybe (a -> Text)
  , rowGroup :: Maybe (a -> Maybe Text)
  , rowAttrs :: Maybe (a -> [Attribute])
  , selectRow :: Maybe (a -> Bool)
  , bulkActions :: [BulkAction]
  , search :: Maybe SearchMode
  , searchPlaceholder :: Maybe Text
  , sort :: Maybe SortConfig
  , sortableColumns :: Maybe SortableConfig -- HTMX-powered column sorting
  , tableHeaderActions :: Maybe TableHeaderActions -- Sort/filter dropdowns in header
  , pagination :: Maybe Pagination -- Page-based pagination with per-page selector
  , zeroState :: Maybe ZeroState
  , header :: Maybe (Html ())
  , treeConfig :: Maybe (TreeConfig a)
  , facets :: Maybe TableHeaderActions
  , resultSummary :: Maybe Text
  , exportName :: Maybe Text
  , customizeColumns :: Bool
  , grouping :: Maybe SelectControl
  , tabs :: Maybe TableTabs
  }
  deriving stock (Generic)
  deriving anyclass (Default)


-- $setup
-- >>> :set -XTypeApplications -XOverloadedRecordDot -XOverloadedStrings
-- >>> import Data.Default (def)
-- >>> import Data.Vector qualified as V
-- >>> import Data.Text.Lazy qualified as TL
-- >>> import Lucid (renderText, toHtml)


data Config = Config
  { tableClasses :: Text
  , thClasses :: Text
  , tdClasses :: Text
  , containerClasses :: Text
  , showHeader :: Bool
  , elemID :: Text
  , containerId :: Maybe Text -- Outer container id for HTMX targeting
  , refreshOnEvent :: Maybe (Text, Text)
  -- ^ (event name, GET url): container re-fetches itself when the named HX-Trigger event
  -- fires anywhere on the page. /Requires/ 'containerId' to be set; without it the
  -- table has no element to attach the listener to and refreshOnEvent is a no-op.
  , deferredUrl :: Maybe Text
  -- ^ GET url the container re-fetches once on load, replacing itself. Lets a page ship a
  -- cheap shell (rows without expensive stats) and fill it in afterwards, so a slow
  -- aggregate never blocks first paint. Like 'refreshOnEvent', requires 'containerId'.
  , renderAsTable :: Bool -- True for table mode, False for list mode
  , addPadding :: Bool -- When True, wraps table in div with px-4 pt-2 pb-2 padding
  , bulkActionsInHeader :: Maybe Int -- Column index (0-based) to place bulk actions in header; Nothing uses toolbar
  , noSurface :: Bool -- When True, removes surface-raised class from grid wrapper (for embedded tables)
  , noDividers :: Bool -- When True, removes divide-y separators between rows
  }


data SelectControl = SelectControl
  { name :: Text
  , label :: Text
  , current :: Text
  , options :: [(Text, Text)] -- (value, label)
  , baseUrl :: Text
  , targetId :: Text
  }


data TableTabs = TableTabs
  { label :: Text
  , options :: [(Text, Text, Bool)] -- (label, url, active)
  }


-- Supporting Types

-- | A toolbar action over the checked rows. With @choices@ it opens a menu and each
-- @(label, uri)@ posts the selection to its own uri (bulk priority, bulk assign).
data BulkAction = BulkAction
  { icon :: Maybe Text
  , title :: Text
  , uri :: Text
  , choices :: [(Text, Text)]
  }


data TabFilter = TabFilter
  { current :: Text
  , currentURL :: Text
  , options :: [TabFilterOpt]
  }


data TabFilterOpt = TabFilterOpt
  { name :: Text
  , count :: Maybe Int
  }


data SortConfig = SortConfig
  { current :: Text
  , currentURL :: Text
  , options :: [(Text, Text, Text)] -- (title, description, identifier)
  }


data ZeroState = ZeroState
  { icon :: Text
  , title :: Text
  , description :: Text
  , action :: EmptyStateAction
  }


-- Sortable column config for HTMX-powered column sorting
data SortableConfig = SortableConfig
  { currentSort :: Text -- e.g. "+name" or "-updated_at"
  , baseUrl :: Text -- URL without sort param
  , targetId :: Text -- HTMX target for partial update
  }
  deriving stock (Eq, Show)


-- Tree config for hierarchical row display
data TreeConfig a = TreeConfig
  { rowLevel :: a -> Int
  , rowPath :: a -> Text
  , isGroupRow :: a -> Bool
  }


-- Sorting types for database queries
data SortOrder = Asc | Desc deriving stock (Eq, Show)


data SortField = SortField
  { fieldName :: Text
  , order :: SortOrder
  }
  deriving stock (Eq, Show)


instance HasField "toSql" SortField Text where
  getField (SortField name Asc) = name <> " ASC NULLS FIRST"
  getField (SortField name Desc) = name <> " DESC NULLS LAST"


-- Header Actions (sort/filter dropdowns in table header)
data TableHeaderActions = TableHeaderActions
  { baseUrl :: Text
  , targetId :: Text
  , sortOptions :: [(Text, Text, Text)] -- (title, description, sortKey)
  , currentSort :: Text
  , filterMenus :: [FilterMenu]
  , activeFilters :: [(Text, [Text])] -- (category, [values])
  }


data FilterMenu = FilterMenu
  { label :: Text -- e.g. "Tags"
  , paramName :: Text -- query param name e.g. "tag"
  , options :: [FilterOption]
  , multiSelect :: Bool -- True for multi-select, False for single-select
  }
  deriving stock (Eq, Show)


data FilterOption = FilterOption
  { label :: Text
  , value :: Text
  , isActive :: Bool
  , count :: Maybe Int
  }
  deriving stock (Eq, Show)


multiSelectFilter :: Text -> Text -> [Text] -> [Text] -> FilterMenu
multiSelectFilter label paramName selected values =
  FilterMenu
    { label
    , paramName
    , options = [FilterOption value value (value `elem` selected) Nothing | value <- values]
    , multiSelect = True
    }


-- | Count each inventory row once per value, before applying any filters.
--
-- >>> map (\o -> (o.value, o.count)) (countedFilter "Runtime" "runtime" Nothing id (V.fromList [["a", "a"], ["a", "b"], [""]])).options
-- [("a",Just 2),("b",Just 1)]
countedFilter :: Text -> Text -> Maybe Text -> (a -> [Text]) -> V.Vector a -> FilterMenu
countedFilter label paramName selected valuesOf rows = FilterMenu label paramName options False
  where
    counts = Map.fromListWith (+) [(value, 1) | row <- V.toList rows, value <- ordNub (valuesOf row), not (T.null value)]
    options = [FilterOption value value (Just value == selected) (Just count) | (value, count) <- Map.toAscList counts]


-- | Header actions for a filter-only inventory table. The active-filter chips are read back
-- off the menus, so a facet's label and selected value are written exactly once.
facetActions :: Text -> Text -> [FilterMenu] -> TableHeaderActions
facetActions baseUrl targetId filterMenus =
  TableHeaderActions
    { baseUrl = foldl' withQuery baseUrl [menu.paramName <> "=" <> toUriStr opt.value | menu <- filterMenus, opt <- menu.options, opt.isActive]
    , targetId
    , sortOptions = []
    , currentSort = ""
    , filterMenus
    , activeFilters = [(menu.label, selected) | menu <- filterMenus, let selected = [o.value | o <- menu.options, o.isActive], not $ null selected]
    }


-- Default Instances

instance Default Config where
  def =
    Config
      { tableClasses = "table table-sm w-full relative"
      , thClasses = "text-left bg-bgAlternate sticky top-0 overflow-hidden"
      , tdClasses = "px-4 py-4"
      , containerClasses = "w-full mx-auto space-y-2"
      , showHeader = True
      , elemID = "tableContainer"
      , containerId = Nothing
      , refreshOnEvent = Nothing
      , deferredUrl = Nothing
      , renderAsTable = False
      , addPadding = False
      , bulkActionsInHeader = Nothing
      , noSurface = False
      , noDividers = False
      }


-- ToHtml Instance

instance ToHtml (Table a) where
  {-# INLINE toHtml #-}
  toHtml = toHtmlRaw . renderTable
  {-# INLINE toHtmlRaw #-}
  toHtmlRaw = toHtmlRaw . renderTable


-- TableRows ToHtml - only renders rows + pagination link for load more

instance ToHtml (TableRows a) where
  toHtml = toHtmlRaw . renderTableRows
  toHtmlRaw = toHtmlRaw . renderTableRows


-- | A table endpoint's response: the whole page, or only the rows a load-more/search swap appends.
data TableResponse a = TablePage (PageCtx (Table a)) | TableRowsOnly (TableRows a)


instance ToHtml (TableResponse a) where
  toHtml = \case TablePage p -> toHtml p; TableRowsOnly r -> toHtml r
  toHtmlRaw = toHtml


{-# INLINE renderTableRows #-}
renderTableRows :: forall a. TableRows a -> Html ()
renderTableRows tr = do
  renderBody []
    $ Table
      { config = (def :: Config){renderAsTable = tr.renderAsTable}
      , columns = tr.columns
      , rows = tr.rows
      , features = (def :: Features a){rowId = tr.rowId, rowAttrs = tr.rowAttrs, rowGroup = tr.rowGroup}
      }
  whenJust tr.pagination renderPaginationFooter


-- Tab Filter ToHtml

instance ToHtml TabFilter where
  toHtmlRaw = toHtml
  toHtml tf =
    navTabStrip_ ViewTabs "Filter" "tabs-xs md:tabs-sm" [(tabLabel opt, withQuery (deleteParam "filter" tf.currentURL) ("filter=" <> toUriStr opt.name), opt.name == tf.current) | opt <- tf.options]
    where
      tabLabel :: Monad m => TabFilterOpt -> HtmlT m ()
      tabLabel opt = do
        span_ $ toHtml opt.name
        whenJust opt.count \c -> when (c > 0) $ span_ [class_ "absolute top-[1px] -right-[5px] text-textInverse-strong text-xs font-medium rounded-full px-1 bg-fillError-strong"] $ show c


-- Shared bits

-- | Append a query fragment to a url, picking @?@ or @&@.
--
-- The separator must be computed on the url as passed — single-select filters
-- delete the target param first, so a page whose only param was the filter
-- gets @?@ back, not a malformed @&@ (regression: /x&filter=bar):
--
-- >>> withQuery (deleteParam "filter" "/x?filter=old") "filter=bar"
-- "/x?filter=bar"
-- >>> withQuery "/x?a=1" "filter=bar"
-- "/x?a=1&filter=bar"
withQuery :: Text -> Text -> Text
withQuery url q = url <> bool "?" "&" ("?" `T.isInfixOf` url) <> q


-- | htmx attrs that fetch @url@ and swap the target container with the same container from the response.
swapTarget_ :: Text -> Text -> [Attribute]
swapTarget_ tid url = [hxGet_ url, hxTarget_ $ "#" <> tid, hxSelect_ $ "#" <> tid, hxPushUrl_ "true", hxSwap_ "outerHTML"]


selectAllCheckbox_ :: Html ()
selectAllCheckbox_ = input_ [term "aria-label" "Select All", type_ "checkbox", class_ "checkbox h-6 w-6 checked:checkbox-primary", [__| on click set .bulkactionItemCheckbox.checked to my.checked |]]


-- | One row of a sort dropdown; @linkAttrs@ carries either an href or the htmx swap attrs.
sortOption_ :: Bool -> [Attribute] -> Text -> Text -> Html ()
sortOption_ isActive linkAttrs title desc =
  a_ ([class_ $ "flex flex-row px-3 py-2 hover:bg-fillBrand-weak rounded-md cursor-pointer " <> bool "" " text-textBrand " isActive] <> linkAttrs) do
    div_ [class_ "flex flex-col items-center justify-center px-1"] $ if isActive then faSprite_ "icon-checkmark4" "solid" "w-4 h-4" else div_ [class_ "w-4 h-4"] ""
    div_ [class_ "grow"] do
      span_ [class_ "block text-sm font-medium"] $ toHtml title
      span_ [class_ "block text-xs text-textWeak"] $ toHtml desc


sortLoader_ :: Html ()
sortLoader_ = div_ [class_ "p-12 fixed rounded-lg shadow-sm bg-bgOverlay top-1/2 left-1/2 transform -translate-x-1/2 -translate-y-1/2 htmx-indicator loading loading-dots loading-md", id_ "sortLoader"] ""


-- | Icon + label button that opens the popover @popId@ (sort/filter dropdown triggers).
dropdownTriggerBtn_ :: Text -> Text -> Text -> Html ()
dropdownTriggerBtn_ icon label popId =
  button_ ([class_ "btn btn-xs shadow-none text-xs font-normal border border-strokeWeak text-textWeak bg-transparent", type_ "button"] <> popoverTrigger_ popId) do
    faSprite_ icon "regular" "h-3 w-3"
    span_ $ toHtml label


-- | Sort dropdown: trigger + popover of options. @optAttrs@ turns a sort key into its link/htmx attrs;
-- the first option is the implicit default when @current@ is empty.
sortDropdown_ :: Text -> [Attribute] -> [(Text, Text, Text)] -> Text -> Text -> (Text -> [Attribute]) -> Html ()
sortDropdown_ popId panelAttrs opts current fallbackLabel optAttrs = do
  let defaultKey = maybe "" (\(_, _, k) -> k) (listToMaybe opts)
  dropdownTriggerBtn_ "sort" (maybe fallbackLabel (\(t, _, _) -> t) $ find (\(_, _, k) -> k == current) opts) popId
  div_ (panelAttrs <> popoverPanel_ popId) $ forM_ opts \(title, desc, k) ->
    sortOption_ (current == k || (current == "" && k == defaultKey)) (optAttrs k <> [hxIndicator_ "#sortLoader"]) title desc


-- Core Rendering Functions

renderTable :: Table a -> Html ()
renderTable tbl =
  let isEmpty = V.null tbl.rows && isJust tbl.features.zeroState
      tableMain = do
        sequence_ tbl.features.header
        forM_ tbl.features.tabs $ \tabs -> navTabStrip_ ViewTabs tabs.label "tabs-sm mx-3 w-fit" [(toHtml label, url, active) | (label, url, active) <- tabs.options]
        when (isJust tbl.features.resultSummary || isJust tbl.features.exportName || tbl.features.customizeColumns || isJust tbl.features.grouping || isJust tbl.features.facets) $ renderResultToolbar tbl
        -- Scrolls horizontally rather than clipping. `overflow-hidden` silently cut off whatever
        -- did not fit: Containers, Kubernetes and Images each need 1460-1820px and get ~1250
        -- with the facet rail open, so 200-570px of columns were unreachable — no scrollbar, no
        -- way to see them at all.
        --
        -- Only the x axis changes: `overflow-hidden` already set overflow-y to hidden, so this
        -- box was a scroll container in both axes before and still is, and the sticky `thead`
        -- resolves against exactly what it did before (verified in-browser).
        div_ [class_ $ "grid overflow-x-auto overflow-y-hidden my-0 group/grid" <> if tbl.config.noSurface then "" else " surface-table", id_ $ tbl.config.elemID <> "_grid"] do
          let divCls = if tbl.config.noDividers then "" else " divide-y"
          form_ [class_ $ "flex flex-col w-full" <> divCls, id_ tbl.config.elemID, onkeydown_ "return event.key != 'Enter' || event.target.tagName != 'INPUT';"] do
            when ((isJust tbl.features.rowId || isJust tbl.features.sort) && isNothing tbl.config.bulkActionsInHeader) $ renderToolbar tbl
            when isEmpty $ whenJust tbl.features.zeroState \zs ->
              emptyState_ def{icon = Just zs.icon, action = zs.action} zs.title zs.description
            unless isEmpty
              $ div_ [class_ "w-full flex-col"] do
                whenJust tbl.features.search \_ -> span_ [id_ "searchIndicator", class_ "htmx-indicator loading loading-sm loading-dots mx-auto"] ""
                div_ [id_ "rowsContainer", class_ divCls] do
                  renderRows tbl
      tableContent = div_ ([class_ $ tbl.config.containerClasses <> bool " pb-24" "" isEmpty, id_ $ tbl.config.elemID <> "_page"] <> [data_ "table-columns" tbl.config.elemID | tbl.features.customizeColumns]) do
        unless isEmpty $ whenJust tbl.features.search $ renderSearch tbl.config.elemID (fromMaybe "Search" tbl.features.searchPlaceholder)
        case tbl.features.facets of
          Just facets -> div_ [class_ "flex items-start gap-4 max-lg:flex-col"] do
            Facets.sidebar_ $ facetsConfig facets
            div_ [class_ "min-w-0 w-full flex-1 space-y-3"] tableMain
          Nothing -> tableMain
        -- Pagination footer outside the raised surface
        whenJust tbl.features.pagination renderPaginationFooter
        when (isJust tbl.features.treeConfig) treeScript
      paddedContent = if tbl.config.addPadding then div_ [class_ "max-md:px-2 px-4 pt-2 pb-2"] tableContent else tableContent
      -- A deferred fill-in wins over the event refresh: the response it swaps in carries
      -- its own refreshOnEvent, so the listener is re-established rather than lost.
      refreshAttrs cid = case (tbl.config.deferredUrl, tbl.config.refreshOnEvent) of
        (Just url, _) -> swapSelf url "load"
        (Nothing, Just (evt, url)) -> swapSelf url $ evt <> " from:body"
        (Nothing, Nothing) -> []
        where
          -- hx-preload off: the body preloads on hover, which would refetch this container.
          swapSelf url trig = [hxGet_ url, hxTrigger_ trig, hxTarget_ "this", hxSwap_ "outerHTML", hxSelect_ $ "#" <> cid, term "hx-preload" "false"]
   in maybe paddedContent (\cid -> div_ ([class_ "w-full table-refresh", id_ cid] <> refreshAttrs cid) paddedContent) tbl.config.containerId


renderRows :: Table a -> Html ()
renderRows tbl
  | tbl.config.renderAsTable =
      table_ [class_ $ tbl.config.tableClasses <> if tbl.config.noDividers then " no-dividers" else ""] do
        when tbl.config.showHeader
          $ thead_ do
            tr_ do
              when (isJust tbl.features.rowId)
                $ th_ [class_ $ tbl.config.thClasses <> " w-8 max-md:hidden"] selectAllCheckbox_
              forM_ (zip [0 ..] tbl.columns) \(idx, c) -> do
                let sortable = (,) <$> c.sortField <*> tbl.features.sortableColumns
                    -- Lucid concatenates duplicate class_ attributes with NO separator, and a
                    -- column built with `withAttrs [class_ …]` brings its own — so this value
                    -- leads with a space. Without it the column's last class and thClasses'
                    -- first class fuse into one nonsense token and both are lost, which is how
                    -- header widths and `max-md:hidden` silently stopped applying.
                    thAttrs = [class_ $ " " <> tbl.config.thClasses <> " " <> fromMaybe "" c.align <> bool "" " cursor-pointer hover:bg-fillWeak" (isJust sortable)]
                    sortAttrs = foldMap (\(field, cfg) -> swapTarget_ cfg.targetId (toggleSortUrl cfg field)) sortable
                    sortOrder = sortable >>= \(field, cfg) -> lookup cfg.currentSort [("-" <> field, Desc), ("+" <> field, Asc)]
                th_ (c.attrs <> thAttrs <> sortAttrs <> [data_ "column-key" c.name]) do
                  span_ [class_ $ "flex items-center gap-2 min-w-0" <> bool "" " flex-wrap" (tbl.config.bulkActionsInHeader == Just idx)] do
                    span_ [class_ $ bool "max-md:hidden" "" (idx > 0)] $ toHtml c.name
                    sequence_ c.headerExtra
                    whenJust sortOrder \case
                      Asc -> faSprite_ "arrow-up" "regular" "w-3 h-3"
                      Desc -> faSprite_ "arrow-down" "regular" "w-3 h-3"
                    when (isJust sortable && isNothing sortOrder) $ faSprite_ "arrows-up-down" "regular" "w-3 h-3 opacity-30"
                    when (tbl.config.bulkActionsInHeader == Just idx) do
                      -- Selection actions replace nothing and appear only once a row is checked.
                      span_ [class_ "hidden group-has-[.bulkactionItemCheckbox:checked]/grid:md:inline-flex gap-2 ml-2 items-center"] do
                        forM_ tbl.features.bulkActions $ bulkActionBtn_ "btn-xs" "h-3 w-3"
                        unless (null tbl.features.bulkActions || isNothing tbl.features.tableHeaderActions) $ span_ [class_ "w-px h-5 bg-strokeWeak mx-1"] ""
                      -- Sort/filter give way to the selection actions so the header stays one line.
                      whenJust tbl.features.tableHeaderActions $ span_ [class_ $ bool "group-has-[.bulkactionItemCheckbox:checked]/grid:md:hidden" "" (null tbl.features.bulkActions)] . renderHeaderTableActions
        renderBody [id_ $ tbl.config.elemID <> "_tbody"] tbl
  | otherwise = renderBody [] tbl


-- | Row container: @tbody@ in table mode, plain div in list mode. Shared with 'renderTableRows'
-- so htmx-swapped pages stay markup-identical to page 1.
renderBody :: [Attribute] -> Table a -> Html ()
renderBody extra tbl
  | not tbl.config.renderAsTable = div_ [class_ "stagger-fade"] $ V.mapM_ (renderListRow tbl) tbl.rows
  | Just groupOf <- tbl.features.rowGroup =
      if V.null tbl.rows then body pass else groupedRows groupOf
  | otherwise = body $ V.mapM_ (renderTableRow tbl) tbl.rows
  where
    body = tbody_ (extra <> [class_ "stagger-fade"])
    columnCount = length tbl.columns + bool 0 1 (isJust tbl.features.rowId)
    groups groupOf = sortWith (fst . head) $ NE.groupAllWith (\(idx, row) -> maybeToRight idx $ groupOf row) $ zip [0 :: Int ..] (V.toList tbl.rows)
    groupedRows groupOf = iforM_ (groups groupOf) $ \idx ((_, row) :| siblings) ->
      let labelM = groupOf row
          collapsible = isNothing tbl.features.grouping && isJust labelM && not (null siblings)
          groupLabel label suffix = toHtml label >> span_ [class_ "ms-2 text-xs font-normal text-textWeak tabular-nums"] (toHtml $ show (1 + length siblings) <> suffix)
       in tbody_
            ([class_ $ if collapsible then "stagger-fade bg-fillWeaker/30 not-has-[.itemsListItem:not(.hidden)]:hidden not-has-[>tr:first-child_.row-group-toggle[open]]:[&>tr:not(:first-child)]:hidden" else if isJust tbl.features.grouping then "stagger-fade not-has-[.itemsListItem:not(.hidden)]:hidden" <> bool "" " bg-fillWeaker/30" (isJust labelM) else "stagger-fade"] <> if idx == 0 then extra else [])
            do
              forM_ labelM $ \label ->
                if isJust tbl.features.grouping
                  then tr_ [data_ "table-group" label] $ td_ [colspan_ $ show columnCount, class_ "px-4 py-3 text-sm font-medium text-textStrong"] $ groupLabel label " items"
                  else
                    when collapsible
                      $ tr_ [data_ "table-group" label]
                      $ td_ [colspan_ $ show columnCount, class_ "p-0"]
                      $ details_ [class_ "row-group-toggle group/disclosure", [__|on input from closest <[id$='_page']/> if event.target.type is "search" set my.open to true end|]]
                      $ summary_ [class_ "cursor-pointer px-4 py-3 text-sm font-medium text-textStrong focus-visible:outline-offset-[-2px]"]
                      $ groupLabel label " items on this page"
              mapM_ (renderTableRow tbl) (row : map snd siblings)


treeRowAttrs :: a -> TreeConfig a -> [Attribute]
treeRowAttrs row tc =
  [ data_ "tree-path" (tc.rowPath row)
  , data_ "tree-level" (show $ tc.rowLevel row)
  ]
    <> if tc.isGroupRow row
      then
        [ data_ "tree-group" "true"
        , data_ "tree-collapsed" "false"
        , term "aria-expanded" "true"
        , role_ "button"
        , tabindex_ "0"
        , term "aria-label" $ "Toggle metric namespace " <> tc.rowPath row
        , onclick_ "toggleTreeRow(this)"
        , keyboardActivateAttr_
        ]
      else []


-- List mode: render columns in a flex container (no table wrapper/headers)
{-# INLINE renderListRow #-}
renderListRow :: Table a -> a -> Html ()
renderListRow tbl row = div_ (treeAttrs <> rowAttrs <> [class_ " flex gap-4 md:gap-8 items-start itemsListItem py-3 hover:bg-fillWeak transition-colors duration-75 "]) $ forM_ tbl.columns \c -> div_ c.attrs $ c.render row
  where
    rowAttrs = maybe [] ($ row) tbl.features.rowAttrs
    treeAttrs = maybe [] (treeRowAttrs row) tbl.features.treeConfig


-- | Table mode: render as table rows with columns. A @rowAttrs@ class stays separate from the row's own classes.
--
-- >>> import Lucid (class_)
-- >>> let tbl = Table (def :: Config){renderAsTable = True} [col "c" (const "x")] (V.singleton ()) (def :: Features ()){rowAttrs = Just (const [class_ "hover:bg-fillWeaker"])}
-- >>> "hover:bg-fillWeaker hover:bg-fillWeak" `TL.isInfixOf` renderText (toHtml tbl)
-- True
{-# INLINE renderTableRow #-}
renderTableRow :: Table a -> a -> Html ()
renderTableRow tbl row =
  tr_ ([class_ $ " hover:bg-fillWeak transition-colors duration-75 itemsListItem" <> bool "" " cursor-pointer" (isTreeGroup || isJust tbl.features.rowLink) <> " "] <> treeAttrs <> rowAttrs <> linkHandler) do
    whenJust tbl.features.rowId \getId ->
      td_ [class_ "w-8 align-top pt-4 max-md:hidden"]
        $ input_
        $ [term "aria-label" $ "Select " <> maybe "Item" ($ row) tbl.features.rowLabel, class_ "bulkactionItemCheckbox checkbox checkbox-md checked:checkbox-primary", type_ "checkbox", name_ "itemId", value_ $ getId row]
        <> [checked_ | maybe False ($ row) tbl.features.selectRow]
    forM_ tbl.columns $ \c -> td_ (c.attrs <> (class_ <$> maybeToList c.align) <> [data_ "column-key" c.name]) $ c.render row
  where
    rowAttrs = maybe [] ($ row) tbl.features.rowAttrs
    treeAttrs = maybe [] (treeRowAttrs row) tbl.features.treeConfig
    isTreeGroup = maybe False (\tc -> tc.isGroupRow row) tbl.features.treeConfig
    -- Lucid joins duplicate class_ attributes with no separator, so the row class is space-padded for rowAttrs classes.
    linkHandler = maybe [] (\getLink -> [hxGet_ (getLink row), hxPushUrl_ "true"] <> navTabAttrs) tbl.features.rowLink


-- | Bulk action button: enabled (via CSS) only while some row checkbox is checked.
bulkActionBtn_ :: Text -> Text -> BulkAction -> Html ()
bulkActionBtn_ btnSize iconSize blkA
  | null blkA.choices = button_ [class_ $ "btn " <> btnSize <> " normal-case btn-disabled group-has-[.bulkactionItemCheckbox:checked]/grid:text-textStrong group-has-[.bulkactionItemCheckbox:checked]/grid:bg-bgRaised group-has-[.bulkactionItemCheckbox:checked]/grid:border-strokeStrong group-has-[.bulkactionItemCheckbox:checked]/grid:pointer-events-auto!", hxPost_ blkA.uri, hxSwap_ "none"] label
  -- A top-layer popover: a nested dropdown is clipped by the header cell's overflow. It stays
  -- inside the form, so each choice still posts the checked rows.
  | otherwise = do
      button_ ([type_ "button", class_ $ "btn " <> btnSize <> " normal-case btn-disabled group-has-[.bulkactionItemCheckbox:checked]/grid:text-textStrong group-has-[.bulkactionItemCheckbox:checked]/grid:bg-bgRaised group-has-[.bulkactionItemCheckbox:checked]/grid:border-strokeStrong group-has-[.bulkactionItemCheckbox:checked]/grid:pointer-events-auto!"] <> popoverTrigger_ popId) label
      div_ (popoverPanel_ popId <> [class_ "dropdown dropdown-start menu z-50 min-w-40 mt-1 rounded-md border border-strokeWeak bg-bgRaised p-1 text-sm normal-case font-normal shadow-lg", [__|on click call me.hidePopover()|]])
        $ forM_ blkA.choices \(lbl, uri) -> button_ [type_ "button", class_ "w-full cursor-pointer rounded px-3 py-1.5 text-left text-textStrong hover:bg-fillWeaker", hxPost_ uri, hxSwap_ "none"] $ toHtml lbl
  where
    popId = "bulk-" <> T.intercalate "-" (words $ T.toLower blkA.title)
    label = do
      whenJust blkA.icon \icon -> faSprite_ icon "regular" $ iconSize <> " inline-block"
      span_ [class_ "ml-1"] $ toHtml blkA.title


renderHeaderTableActions :: TableHeaderActions -> Html ()
renderHeaderTableActions actions = span_ [class_ "inline-flex flex-wrap items-center gap-2 ml-2"] do
  unless (null actions.sortOptions) $ renderSortDropdown actions
  unless (null actions.filterMenus) $ renderFilterDropdown actions
  renderFilterChips actions


renderFilterChips :: TableHeaderActions -> Html ()
renderFilterChips actions = forM_ actions.filterMenus \menu -> forM_ (filter (.isActive) menu.options) \opt ->
  button_ ([type_ "button", class_ "inline-flex items-center gap-1 rounded-md border border-strokeWeak bg-fillWeak px-2 py-0.5 text-xs text-textStrong hover:bg-fillWeaker max-md:whitespace-normal", Aria.label_ $ "Remove filter " <> menu.label <> ": " <> opt.label] <> swapTarget_ actions.targetId (deleteParamValue menu.paramName opt.value actions.baseUrl)) do
    span_ [class_ "text-textWeak"] $ toHtml $ menu.label <> ":"
    toHtml opt.label
    faSprite_ "xmark" "regular" "w-2.5 h-2.5 text-textWeak"


clearFiltersUrl :: TableHeaderActions -> Text
clearFiltersUrl actions = foldl' (\url menu -> deleteParam menu.paramName url) actions.baseUrl actions.filterMenus


-- | Adapt URL filters to the same sidebar data Explorer supplies.
facetsConfig :: TableHeaderActions -> Facets.Config
facetsConfig actions = Facets.Config (actions.targetId <> "-facets") "" (Just $ swapTarget_ actions.targetId $ clearFiltersUrl actions) (Just [common]) Facets.Collapsible
  where
    common =
      FacetNode "Common Filters" Nothing True [] Nothing
        $ FacetChildren
          [ FacetNode
              { label = menu.label
              , path = Just menu.paramName
              , expanded = (index < 5 && not (null menu.options)) || any (.isActive) menu.options
              , attrs = []
              , actions = Nothing
              , content = FacetValues (map (filterChoice actions menu) $ sortOn (not . (.isActive)) menu.options) ExpandValues
              }
          | (index, menu) <- zip [0 :: Int ..] actions.filterMenus
          ]


renderSortDropdown :: TableHeaderActions -> Html ()
renderSortDropdown actions = do
  div_ [class_ "inline-block", data_ "tippy-content" "Sort by"]
    $ sortDropdown_ "sortDropdown" [class_ "dropdown dropdown-start bg-bgRaised p-1 text-sm normal-case border border-strokeWeak z-50 w-72 rounded-md shadow-lg mt-1"] actions.sortOptions actions.currentSort "Sort"
    $ \k -> swapTarget_ actions.targetId (withQuery (deleteParam "sort" actions.baseUrl) $ "sort=" <> toUriStr k)
  sortLoader_


renderFilterDropdown :: TableHeaderActions -> Html ()
renderFilterDropdown actions = do
  let popId = "filterDropdown"
  div_ [class_ "inline-block", data_ "tippy-content" "Filter by"] do
    dropdownTriggerBtn_ "filter" ("Filter" <> if null actions.activeFilters then "" else " (" <> show (sum $ map (length . snd) actions.activeFilters) <> ")") popId
    div_
      ([class_ "dropdown dropdown-start bg-bgRaised p-1 text-sm normal-case border border-strokeWeak z-50 w-60 rounded-md shadow-lg mt-1"] <> popoverPanel_ popId)
      do
        div_ [class_ "flex items-center justify-between px-3 py-2 text-sm font-semibold text-textStrong border-b border-strokeWeak"] do
          span_ "Select Filter"
          button_
            ([type_ "button", class_ "text-xs text-textBrand cursor-pointer flex items-center gap-1"] <> swapTarget_ actions.targetId (clearFiltersUrl actions))
            ("Clear all" >> faSprite_ "xmark" "regular" "w-3 h-3")
        div_ [class_ "p-1"] $ forM_ actions.filterMenus (renderFilterMenuItem actions)


renderFilterMenuItem :: TableHeaderActions -> FilterMenu -> Html ()
renderFilterMenuItem actions menu = div_ [class_ "relative"] do
  let subPopId = "filterSub_" <> menu.paramName
  button_
    ([class_ "flex items-center justify-between w-full px-3 py-2 text-sm rounded hover:bg-fillWeak cursor-pointer", type_ "button"] <> popoverTrigger_ subPopId)
    do
      span_ $ toHtml $ "By " <> menu.label
      faSprite_ "chevron-right" "regular" "w-3 h-3"
  div_
    [ class_ "dropdown dropdown-right bg-bgRaised rounded-lg shadow-lg w-48 normal-case border border-strokeWeak max-h-60 overflow-y-auto"
    , term "popover" "auto"
    , id_ subPopId
    , style_ $ "position-try: flip-inline; position-anchor: --anchor-" <> subPopId
    ]
    do
      div_ [class_ "px-3 py-2 text-sm font-semibold text-textStrong border-b border-strokeWeak"] $ toHtml menu.label
      div_ [class_ "p-1"] $ forM_ menu.options (facetChoice_ . filterChoice actions menu)


filterChoice :: TableHeaderActions -> FilterMenu -> FilterOption -> FacetChoice
filterChoice actions menu opt = FacetChoice{label = opt.label, multiSelect = menu.multiSelect, inputAttrs, marker = Nothing, count = prettyPrintCount <$> opt.count}
  where
    paramVal = menu.paramName <> "=" <> toUriStr opt.value
    -- For multi-select: toggle this value; for single-select: replace all with this value
    url
      | opt.isActive = deleteParamValue menu.paramName opt.value actions.baseUrl
      | menu.multiSelect = withQuery actions.baseUrl paramVal
      | otherwise = withQuery (deleteParam menu.paramName actions.baseUrl) paramVal
    inputAttrs =
      [ Aria.label_ opt.label
      , value_ opt.value
      , name_ $ actions.targetId <> "_filter_" <> menu.paramName
      , hxTrigger_ "change"
      ]
        <> swapTarget_ actions.targetId url
        <> [checked_ | opt.isActive]


-- | Remove a specific param=value pair from a URL (for multi-select filter toggle)
deleteParamValue :: Text -> Text -> Text -> Text
deleteParamValue key val url = fixQueryStart $ T.replace ("&" <> pair) "" $ T.replace ("?" <> pair) "?" url
  where
    pair = key <> "=" <> toUriStr val
    fixQueryStart t = case T.breakOn "&" t of
      (before, after) | not (T.null after) && not ("?" `T.isInfixOf` before) -> before <> "?" <> T.drop 1 after
      _ -> t


renderToolbar :: Table a -> Html ()
renderToolbar tbl =
  div_ [class_ $ "flex py-3 gap-8 items-center " <> if tbl.config.renderAsTable then "" else "bg-fillWeaker"] do
    when (isJust tbl.features.rowId && not tbl.config.renderAsTable)
      $ div_ [class_ "h-4 flex space-x-3 w-8 items-center"] do
        span_ [class_ "w-2 h-full"] ""
        selectAllCheckbox_

    div_ [class_ "grow flex flex-row gap-2"] do
      forM_ tbl.features.bulkActions $ bulkActionBtn_ "btn-sm" "h-4 w-4"
      whenJust tbl.features.sort renderSortMenu


renderSearch :: Text -> Text -> SearchMode -> Html ()
renderSearch elemID searchPlaceholder searchMode =
  searchInput_ "w-full h-9 bg-transparent border-strokeWeak shadow-none" searchPlaceholder case searchMode of
    ServerSide url -> [name_ "search", id_ "search_box", hxTrigger_ "keyup changed delay:500ms", hxGet_ url, hxTarget_ "#rowsContainer", hxSwap_ "innerHTML", hxIndicator_ "#searchIndicator"]
    ClientSide -> [filterInputAttr_ $ "#" <> elemID <> "_page .itemsListItem"]


renderSortMenu :: SortConfig -> Html ()
renderSortMenu sortCfg = do
  div_ [class_ "inline-block"]
    $ sortDropdown_ "sortMenuDiv" [hxBoost_ "true", class_ "dropdown dropdown-end bg-bgRaised p-1 text-sm border border-strokeWeak mt-2 w-72 origin-top-right rounded-md shadow-lg"] sortCfg.options sortCfg.current sortCfg.current
    $ \k -> [href_ $ withQuery (deleteParam "sort" sortCfg.currentURL) ("sort=" <> toUriStr k)]
  sortLoader_


renderResultToolbar :: Table a -> Html ()
renderResultToolbar tbl = div_ [class_ "flex min-h-9 flex-wrap items-center justify-between gap-2"] do
  whenJust tbl.features.resultSummary $ \summary -> span_ [class_ "text-sm text-textWeak", role_ "status", Aria.live_ "polite"] $ toHtml summary
  forM_ tbl.features.facets $ \facets -> div_ [class_ "flex flex-wrap gap-2"] $ renderFilterChips facets
  div_ [class_ "ml-auto flex flex-wrap items-center gap-2"] do
    forM_ tbl.features.grouping selectControl_
    forM_ tbl.features.exportName $ \name ->
      button_
        [ type_ "button"
        , class_ "btn btn-xs border border-strokeWeak bg-transparent font-normal text-textWeak shadow-none"
        , onclick_ $ "window.exportTableCsv('#" <> tbl.config.elemID <> "_grid table','" <> name <> ".csv')"
        ]
        $ faSprite_ "download" "solid" "h-3 w-3"
        >> "Export"
    when tbl.features.customizeColumns do
      button_ ([type_ "button", class_ "btn btn-xs border border-strokeWeak bg-transparent font-normal text-textWeak shadow-none"] <> popoverTrigger_ (tbl.config.elemID <> "-columns")) do
        faSprite_ "table-columns" "regular" "h-3 w-3"
        "Customize"
      -- CSS owns visibility; the checkbox only restores a stored choice. @data-forced@ marks a
      -- user choice, so ticking a column that a breakpoint hides shows it.
      style_ $ flip foldMap tbl.columns \c ->
        let rule toggle display = "#" <> tbl.config.elemID <> "_page:has([data-column-toggle=\"" <> c.name <> "\"]" <> toggle <> ") [data-column-key=\"" <> c.name <> "\"]{display:" <> display <> "}"
         in rule "[data-forced]:not(:checked)" "none" <> rule "[data-forced]:checked" "table-cell"
      div_
        ( popoverPanel_ (tbl.config.elemID <> "-columns")
            <> [ class_ "dropdown dropdown-end z-50 mt-1 w-56 rounded-md border border-strokeWeak bg-bgRaised p-2 text-sm shadow-lg"
               , [__|on beforetoggle if event.newState is 'open' send syncColumn to <[data-column-toggle]/> in me end
          on resize from window if me.matches(':popover-open') send syncColumn to <[data-column-toggle]/> in me end|]
               ]
        )
        do
          span_ [class_ "block px-2 pb-1 text-xs font-semibold text-textStrong"] "Visible columns"
          forM_ tbl.columns $ \column -> label_ [class_ "flex cursor-pointer items-center gap-2 rounded px-2 py-1.5 hover:bg-fillWeak"] do
            input_
              [ type_ "checkbox"
              , checked_
              , class_ "checkbox checkbox-xs"
              , data_ "column-toggle" column.name
              , data_ "column-storage" $ "table-columns:" <> tbl.config.elemID <> ":" <> column.name
              , [__|init set saved to localStorage.getItem(@data-column-storage) if saved is not null set @data-forced to 'true' then set my.checked to saved is 'true' end
                on syncColumn if not @data-forced
                  set cell to (closest <[data-table-columns]/>).querySelector('[data-column-key="' + @data-column-toggle + '"]')
                  if cell set my.checked to getComputedStyle(cell).display is not 'none' end
                end
                on change set @data-forced to 'true' then call localStorage.setItem(@data-column-storage, my.checked)|]
              ]
            toHtml column.name
          button_
            [ type_ "button"
            , class_ "btn btn-xs btn-ghost mt-1 w-full font-normal"
            , [__|on click repeat for t in <[data-column-toggle]/> in closest <[data-table-columns]/> call localStorage.removeItem(t.getAttribute('data-column-storage')) then remove @data-forced from t then send syncColumn to t end|]
            ]
            "Reset columns"


selectControl_ :: SelectControl -> Html ()
selectControl_ cfg = label_ [class_ "flex items-center gap-2 whitespace-nowrap text-xs text-textWeak"] do
  toHtml cfg.label
  select_ ([name_ cfg.name, class_ "select select-xs w-auto cursor-pointer border-strokeWeak bg-bgBase max-md:min-h-11 max-md:text-base", hxTrigger_ "change"] <> swapTarget_ cfg.targetId (deleteParam "deferred" $ deleteParam cfg.name cfg.baseUrl))
    $ forM_ cfg.options
    $ \(value, label) -> option_ ([value_ value] <> [selected_ "" | value == cfg.current]) $ toHtml label


renderPaginationFooter :: Pagination -> Html ()
renderPaginationFooter pg = div_ [class_ "flex items-center justify-between max-md:flex-wrap max-md:gap-2 px-4 py-3"] do
  div_ [class_ "flex items-center gap-4"] do
    span_ [class_ "text-sm text-textWeak tabular-nums"] $ toHtml $ show startItem <> "-" <> show endItem <> " of " <> show pg.totalCount
    div_ [class_ "flex gap-1"] do
      navBtn "chevron-left" (pg.currentPage > 0) (mkUrl (pg.currentPage - 1) pg.perPage)
      navBtn "chevron-right" (endItem < pg.totalCount) (mkUrl (pg.currentPage + 1) pg.perPage)
  div_ [class_ "max-md:hidden flex items-center gap-2"] do
    span_ [class_ "text-sm text-textWeak mr-2"] "Items per page"
    div_ [class_ "flex rounded-md border border-strokeWeak overflow-hidden"] $ forM_ [25, 50, 100] \pp ->
      button_ ([class_ $ "cursor-pointer px-3 py-1.5 text-sm font-medium transition-colors " <> if pp == pg.perPage then "bg-fillWeak text-textStrong" else "bg-bgRaised text-textWeak hover:bg-fillWeak", type_ "button"] <> if pp == pg.perPage then [] else pgAttrs (mkUrl 0 pp)) $ toHtml (show pp)
  where
    startItem = pg.currentPage * pg.perPage + 1
    endItem = min ((pg.currentPage + 1) * pg.perPage) pg.totalCount
    mkUrl page perPage = withQuery pg.baseUrl $ "page=" <> show page <> "&per_page=" <> show perPage
    pgAttrs = swapTarget_ pg.targetId
    navBtn icon enabled url = button_ ([class_ $ "p-1.5 rounded border border-strokeWeak " <> if enabled then "hover:bg-fillWeak cursor-pointer" else "opacity-40 cursor-not-allowed", type_ "button"] <> if enabled then pgAttrs url else []) $ faSprite_ icon "regular" "w-4 h-4"


treeScript :: Html ()
treeScript =
  script_
    """
    function toggleTreeRow(el) {
      var path = el.getAttribute('data-tree-path');
      var level = parseInt(el.getAttribute('data-tree-level'));
      var collapsed = el.getAttribute('data-tree-collapsed') === 'true';
      var rows = el.parentElement.querySelectorAll('tr[data-tree-path], div[data-tree-path]');
      var found = false;
      for (var i = 0; i < rows.length; i++) {
        var r = rows[i];
        if (r === el) { found = true; continue; }
        if (!found) continue;
        var rp = r.getAttribute('data-tree-path');
        if (!rp.startsWith(path + '.') && rp !== path) break;
        if (collapsed) {
          var rl = parseInt(r.getAttribute('data-tree-level'));
          if (rl === level + 1) { r.style.display = ''; }
        } else {
          r.style.display = 'none';
          if (r.getAttribute('data-tree-group') === 'true') r.setAttribute('data-tree-collapsed', 'true');
        }
      }
      el.setAttribute('data-tree-collapsed', collapsed ? 'false' : 'true');
      el.setAttribute('aria-expanded', collapsed ? 'true' : 'false');
      var chev = el.querySelector('.tree-chevron');
      if (chev) chev.classList.toggle('rotate-90');
    }
    """


-- Column Builders

col :: Text -> (a -> Html ()) -> Column a
col name render =
  Column
    { name
    , render = \row -> let content = render row in if TL.null (TL.strip $ renderText content) then span_ [class_ "text-textDisabled"] "-" else content
    , attrs = []
    , sortField = Nothing
    , align = Nothing
    , headerExtra = Nothing
    }


withSort :: Text -> Column a -> Column a
withSort field column = column{sortField = Just field}


withAttrs :: [Attribute] -> Column a -> Column a
withAttrs as column = column{attrs = as}


withColHeaderExtra :: Html () -> Column a -> Column a
withColHeaderExtra h column = column{headerExtra = Just h}


-- Sorting Utilities

-- Parse sort param like "+name,-updated_at" into sort fields
-- Optionally takes field name overrides (e.g. [("name", ["first_name", "last_name"])])
parseSortParam :: Text -> Maybe [(Text, [Text])] -> [SortField]
parseSortParam sortP overridesM = concatMap parseField (T.splitOn "," sortP)
  where
    parseField txt = case T.uncons txt of
      Just ('+', f) -> fields f Asc
      Just ('-', f) -> fields f Desc
      _ -> []
    fields f order = map (`SortField` order) $ fromMaybe [f] (lookup (T.toLower f) =<< overridesM)


-- Generate URL toggling the sort direction of a column
toggleSortUrl :: SortableConfig -> Text -> Text
toggleSortUrl cfg field = withQuery (deleteParam "sort" cfg.baseUrl) $ "sort=" <> toUriStr (bool "+" "-" (cfg.currentSort == "+" <> field) <> field)
