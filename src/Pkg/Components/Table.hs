{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Pkg.Components.Table (
  Table (..),
  TableRows (..),
  Column (..),
  Config (..),
  Features (..),
  SearchMode (..),
  BulkAction (..),
  TabFilter (..),
  TabFilterOpt (..),
  SortConfig (..),
  SortableConfig (..),
  ZeroState (..),
  SimpleZeroState (..),
  -- Header actions (sort/filter dropdowns in header)
  TableHeaderActions (..),
  FilterMenu (..),
  FilterOption (..),
  -- Sorting types and utilities
  SortOrder (..),
  SortField (..),
  parseSortParam,
  sortFieldsToSQL,
  mkFilter,
  -- Column builders
  col,
  withSort,
  withAttrs,
  withAlign,
  renderRowWithColumns,
  simpleZeroState,
) where

import Data.Default (Default (..))
import Data.List (lookup)
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Records (HasField (getField))
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Pages.Components (emptyState_)
import PyF (fmt)
import Relude hiding (lookup)
import Utils (deleteParam, faSprite_, toUriStr)


-- Core Types

type role Column nominal
data Column a where
  Column
    :: { name :: Text
       , render :: a -> Html ()
       , attrs :: [Attribute]
       , sortField :: Maybe Text
       , align :: Maybe Text
       }
    -> Column a


type role Table nominal
data Table a = Table
  { config :: Config
  , columns :: [Column a]
  , rows :: V.Vector a
  , features :: Features a
  }


-- TableRows for pagination - only renders rows + pagination link
type role TableRows nominal
data TableRows a = TableRows
  { nextUrl :: Maybe Text
  , columns :: [Column a]
  , rows :: V.Vector a
  , emptyState :: Maybe SimpleZeroState
  }


-- Simple zero state for TableRows (just icon and message)
data SimpleZeroState = SimpleZeroState
  { icon :: Text
  , message :: Text
  }


data SearchMode = ClientSide | ServerSide Text


data Features a = Features
  { rowLink :: Maybe (a -> Text)
  , rowId :: Maybe (a -> Text)
  , rowAttrs :: Maybe (a -> [Attribute])
  , selectRow :: Maybe (a -> Bool)
  , bulkActions :: [BulkAction]
  , search :: Maybe SearchMode
  , tabs :: Maybe TabFilter
  , sort :: Maybe SortConfig
  , sortableColumns :: Maybe SortableConfig -- HTMX-powered column sorting
  , tableHeaderActions :: Maybe TableHeaderActions -- Sort/filter dropdowns in header
  , pagination :: Maybe (Text, Text) -- (nextUrl, trigger: "click" | "intersect" | "both")
  , zeroState :: Maybe ZeroState
  , header :: Maybe (Html ())
  }


data Config = Config
  { tableClasses :: Text
  , thClasses :: Text
  , tdClasses :: Text
  , containerClasses :: Text
  , showHeader :: Bool
  , elemID :: Text
  , renderAsTable :: Bool -- True for table mode, False for list mode
  , addPadding :: Bool -- When True, wraps table in div with px-6 pt-4 pb-2 padding
  , bulkActionsInHeader :: Maybe Int -- Column index (0-based) to place bulk actions in header; Nothing uses toolbar
  }


-- Supporting Types

data BulkAction = BulkAction
  { icon :: Maybe Text
  , title :: Text
  , uri :: Text
  }


data TabFilter = TabFilter
  { current :: Text
  , currentURL :: Text
  , options :: [TabFilterOpt]
  , clientSide :: Bool -- When True, uses JS navigation; when False, uses links
  }


data TabFilterOpt = TabFilterOpt
  { name :: Text
  , count :: Maybe Int
  , targetId :: Maybe Text -- For client-side tabs, the target element ID
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
  , actionText :: Text
  , destination :: Either Text Text
  }


-- Sortable column config for HTMX-powered column sorting
data SortableConfig = SortableConfig
  { currentSort :: Text -- e.g. "+name" or "-updated_at"
  , baseUrl :: Text -- URL without sort param
  , targetId :: Text -- HTMX target for partial update
  }
  deriving stock (Eq, Show)


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
  deriving stock (Eq, Show)


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
  }
  deriving stock (Eq, Show)


-- Default Instances

instance Default (Features a) where
  def =
    Features
      { rowLink = Nothing
      , rowId = Nothing
      , rowAttrs = Nothing
      , selectRow = Nothing
      , bulkActions = []
      , search = Nothing
      , tabs = Nothing
      , sort = Nothing
      , sortableColumns = Nothing
      , tableHeaderActions = Nothing
      , pagination = Nothing
      , zeroState = Nothing
      , header = Nothing
      }


instance Default Config where
  def =
    Config
      { tableClasses = "table table-sm w-full relative"
      , thClasses = "text-left bg-fillWeaker sticky top-0"
      , tdClasses = "px-6 py-4"
      , containerClasses = "w-full mx-auto space-y-4 overflow-y-scroll h-full"
      , showHeader = True
      , elemID = "tableContainer"
      , renderAsTable = False
      , addPadding = False
      , bulkActionsInHeader = Nothing
      }


-- ToHtml Instance

instance ToHtml (Table a) where
  {-# INLINE toHtml #-}
  toHtml tbl = toHtmlRaw $ renderTable tbl
  {-# INLINE toHtmlRaw #-}
  toHtmlRaw tbl = toHtmlRaw $ renderTable tbl


-- TableRows ToHtml - only renders rows + pagination link for load more

instance ToHtml (TableRows a) where
  toHtml tr = toHtmlRaw $ renderTableRows tr
  toHtmlRaw tr = toHtmlRaw $ renderTableRows tr


{-# INLINE renderTableRows #-}
renderTableRows :: TableRows a -> Html ()
renderTableRows tr
  | V.null tr.rows = whenJust tr.emptyState renderSimpleZeroState
  | otherwise = do
      V.forM_ tr.rows \row ->
        div_ [class_ "flex gap-8 items-start itemsListItem"] do
          forM_ tr.columns \c -> div_ c.attrs $ c.render row
      whenJust tr.nextUrl (`renderPaginationLink` "both")


-- Tab Filter ToHtml

instance ToHtml TabFilter where
  toHtmlRaw = toHtml
  toHtml tf =
    if tf.clientSide
      then div_ [class_ "justify-start items-start gap-4 flex mb-6 text-sm"] do
        forM_ tf.options \opt ->
          let targetId = fromMaybe ("#" <> toUriStr opt.name <> "_content") opt.targetId
              isActive = opt.name == tf.current
           in button_
                [ onclick_ $ "navigatable(this, '" <> targetId <> "', '#main-content', 't-tab-active')"
                , class_ $ "flex items-center gap-4 a-tab border-b border-b-strokeWeak px-3 py-2" <> if isActive then " t-tab-active" else ""
                ]
                do
                  toHtml opt.name
                  whenJust opt.count \c -> span_ [class_ "text-textDisabled text-xs font-normal"] $ toHtml $ show c
      else div_ [class_ "tabs tabs-box tabs-outline items-center"] do
        let uri = deleteParam "filter" tf.currentURL
        forM_ tf.options \opt ->
          a_
            [ href_ $ uri <> "&filter=" <> toUriStr opt.name
            , role_ "tab"
            , class_ $ "tab h-auto! " <> if opt.name == tf.current then "tab-active text-textStrong" else ""
            ]
            do
              span_ $ toHtml opt.name
              whenJust opt.count $ span_ [class_ "absolute top-[1px] -right-[5px] text-textInverse-strong text-xs font-medium rounded-full px-1 bg-fillError-strong"] . show


-- Core Rendering Functions

renderTable :: Table a -> Html ()
renderTable tbl =
  let tableContent = div_ [class_ tbl.config.containerClasses, id_ $ tbl.config.elemID <> "_page"] do
        whenJust tbl.features.search renderSearch
        whenJust tbl.features.header id

        div_
          [ class_ "grid surface-raised overflow-hidden my-0 group/grid"
          , id_ $ tbl.config.elemID <> "_grid"
          ]
          do
            form_
              [ class_ "flex flex-col divide-y w-full"
              , id_ tbl.config.elemID
              , onkeydown_ "return event.key != 'Enter';"
              ]
              do
                when ((isJust tbl.features.rowId || isJust tbl.features.sort) && isNothing tbl.config.bulkActionsInHeader)
                  $ renderToolbar tbl

                when (V.null tbl.rows) $ whenJust tbl.features.zeroState renderZeroState

                div_ [class_ "w-full flex-col"] do
                  whenJust tbl.features.search \_ ->
                    span_ [id_ "searchIndicator", class_ "htmx-indicator loading loading-sm loading-dots mx-auto"] ""
                  div_ [id_ "rowsContainer", class_ "divide-y"] do
                    renderRows tbl
                    whenJust tbl.features.pagination $ uncurry renderPaginationLink
   in if tbl.config.addPadding
        then div_ [class_ "px-6 pt-4 pb-2"] tableContent
        else tableContent


renderRows :: Table a -> Html ()
renderRows tbl =
  if tbl.config.renderAsTable
    then div_ [class_ "overflow-hidden rounded-lg border border-strokeWeak"] do
      table_ [class_ tbl.config.tableClasses] do
        when tbl.config.showHeader
          $ thead_ do
            tr_ do
              when (isJust tbl.features.rowId)
                $ th_ [class_ $ tbl.config.thClasses <> " w-8"]
                $ input_
                  [ term "aria-label" "Select All"
                  , type_ "checkbox"
                  , class_ "checkbox h-6 w-6 checked:checkbox-primary"
                  , [__| on click set .bulkactionItemCheckbox.checked to my.checked |]
                  ]
              forM_ (zip [0 ..] tbl.columns) \(idx, c) -> do
                let baseAttrs = [class_ $ tbl.config.thClasses <> maybe "" (" " <>) c.align]
                    sortAttrs = case (c.sortField, tbl.features.sortableColumns) of
                      (Just field, Just cfg) ->
                        [ hxGet_ $ toggleSortUrl cfg field
                        , hxTarget_ $ "#" <> cfg.targetId
                        , hxSelect_ $ "#" <> cfg.targetId
                        , hxPushUrl_ "true"
                        , hxSwap_ "outerHTML"
                        , class_ "cursor-pointer hover:bg-fillWeak"
                        ]
                      _ -> []
                    isSorted = case (c.sortField, tbl.features.sortableColumns) of
                      (Just field, Just cfg) -> ("+" <> field == cfg.currentSort) || ("-" <> field == cfg.currentSort)
                      _ -> False
                    sortOrder = case (c.sortField, tbl.features.sortableColumns) of
                      (Just field, Just cfg) | "-" <> field == cfg.currentSort -> Just Desc
                      (Just field, Just cfg) | "+" <> field == cfg.currentSort -> Just Asc
                      _ -> Nothing
                th_ (baseAttrs <> sortAttrs) do
                  span_ [class_ "flex items-center gap-2"] do
                    toHtml c.name
                    when isSorted $ case sortOrder of
                      Just Asc -> faSprite_ "arrow-up" "regular" "w-3 h-3"
                      Just Desc -> faSprite_ "arrow-down" "regular" "w-3 h-3"
                      Nothing -> pure ()
                    when (isJust c.sortField && isJust tbl.features.sortableColumns && not isSorted) $ faSprite_ "arrows-up-down" "regular" "w-3 h-3 opacity-30"
                    when (tbl.config.bulkActionsInHeader == Just idx) do
                      renderHeaderBulkActions tbl.features.bulkActions
                      whenJust tbl.features.tableHeaderActions renderHeaderTableActions
        tbody_ do
          V.mapM_ (renderTableRow tbl) tbl.rows
    else V.mapM_ (renderListRow tbl) tbl.rows


-- List mode: render columns in a flex container (no table wrapper/headers)
{-# INLINE renderListRow #-}
renderListRow :: Table a -> a -> Html ()
renderListRow tbl row = div_ (rowAttrs <> [class_ "flex gap-8 items-start itemsListItem py-3"]) $ forM_ tbl.columns \c -> div_ c.attrs $ c.render row
  where
    rowAttrs = maybe [] ($ row) tbl.features.rowAttrs


-- Table mode: render as table rows with columns
{-# INLINE renderTableRow #-}
renderTableRow :: Table a -> a -> Html ()
renderTableRow tbl row =
  tr_ (rowAttrs <> linkHandler) do
    when (isJust tbl.features.rowId)
      $ td_ [class_ "w-8"] do
        whenJust tbl.features.rowId \getId ->
          input_
            $ [ term "aria-label" "Select Item"
              , class_ "bulkactionItemCheckbox checkbox checkbox-md checked:checkbox-primary"
              , type_ "checkbox"
              , name_ "itemId"
              , value_ $ getId row
              ]
            <> [checked_ | isSelected]

    forM_ tbl.columns \c -> td_ (c.attrs <> colAttrs c) $ c.render row
  where
    rowAttrs = maybe [] ($ row) tbl.features.rowAttrs
    linkHandler = maybe [] (\getLink -> [class_ "cursor-pointer", hxGet_ (getLink row), hxPushUrl_ "true"]) tbl.features.rowLink
    isSelected = maybe False (\f -> f row) tbl.features.selectRow
    colAttrs c = foldMap (\a -> [class_ a]) c.align


renderHeaderBulkActions :: [BulkAction] -> Html ()
renderHeaderBulkActions bulkActions =
  span_ [class_ "inline-flex gap-2 ml-2"] do
    forM_ bulkActions \blkA ->
      button_
        [ class_ "btn btn-xs btn-disabled group-has-[.bulkactionItemCheckbox:checked]/grid:btn-primary group-has-[.bulkactionItemCheckbox:checked]/grid:pointer-events-auto!"
        , hxPost_ blkA.uri
        , hxSwap_ "none"
        ]
        do
          whenJust blkA.icon \icon -> faSprite_ icon "regular" "h-3 w-3 inline-block"
          span_ [class_ "ml-1"] $ toHtml blkA.title


renderHeaderTableActions :: TableHeaderActions -> Html ()
renderHeaderTableActions actions = span_ [class_ "inline-flex gap-2 ml-2"] do
  unless (null actions.sortOptions) $ renderSortDropdown actions
  unless (null actions.filterMenus) $ renderFilterDropdown actions


renderSortDropdown :: TableHeaderActions -> Html ()
renderSortDropdown actions = do
  let defaultSort = maybe "" (\(_, _, k) -> k) (listToMaybe actions.sortOptions)
      currentLabel = maybe "Sort" (\(t, _, _) -> t) $ find (\(_, _, k) -> k == actions.currentSort) actions.sortOptions
      baseUrl' = deleteParam "sort" actions.baseUrl
      separator = if T.isInfixOf "?" baseUrl' then "&" else "?"
      popId = "sortDropdown"

  div_ [class_ "inline-block", data_ "tippy-content" "Sort by"] do
    button_
      [ class_ "btn btn-sm shadow-none text-sm font-medium bg-fillWeaker border text-textWeak border-strokeWeak"
      , type_ "button"
      , term "popovertarget" popId
      , style_ $ "anchor-name: --anchor-" <> popId
      ]
      do
        span_ $ toHtml currentLabel
        faSprite_ "sort" "regular" "h-4 w-4 stroke-iconNeutral"
    div_
      [ id_ popId
      , term "popover" "auto"
      , class_ "dropdown dropdown-start menu bg-bgRaised p-1 text-sm border border-strokeWeak z-50 w-72 rounded-md shadow-lg mt-1"
      , style_ $ "position-try: flip-block; position-anchor: --anchor-" <> popId
      ]
      do
        forM_ actions.sortOptions \(title, desc, sortKey) -> do
          let isActive = actions.currentSort == sortKey || (actions.currentSort == "" && sortKey == defaultSort)
              url = baseUrl' <> separator <> "sort=" <> toUriStr sortKey
          a_
            [ class_ $ "block flex flex-row px-3 py-2 hover:bg-fillBrand-weak rounded-md cursor-pointer " <> if isActive then " text-textBrand " else ""
            , hxGet_ url
            , hxTarget_ $ "#" <> actions.targetId
            , hxSelect_ $ "#" <> actions.targetId
            , hxPushUrl_ "true"
            , hxSwap_ "outerHTML"
            , hxIndicator_ "#sortLoader"
            ]
            do
              div_ [class_ "flex flex-col items-center justify-center px-3"] $ if isActive then faSprite_ "icon-checkmark4" "solid" "w-4 h-5" else div_ [class_ "w-4 h-5"] ""
              div_ [class_ "grow space-y-1"] do
                span_ [class_ "block text-lg"] $ toHtml title
                span_ [class_ "block"] $ toHtml desc
  div_ [class_ "p-12 fixed rounded-lg shadow-sm bg-base-100 top-1/2 left-1/2 transform -translate-x-1/2 -translate-y-1/2 htmx-indicator loading loading-dots loading-md", id_ "sortLoader"] ""


renderFilterDropdown :: TableHeaderActions -> Html ()
renderFilterDropdown actions = do
  let hasActiveFilters = not $ null actions.activeFilters
      activeCount = sum $ map (length . snd) actions.activeFilters
      popId = "filterDropdown"
  div_ [class_ "inline-block", data_ "tippy-content" "Filter by"] do
    button_
      [ class_ "btn btn-sm shadow-none text-sm font-medium bg-fillWeaker border text-textWeak border-strokeWeak"
      , type_ "button"
      , term "popovertarget" popId
      , style_ $ "anchor-name: --anchor-" <> popId
      ]
      do
        span_ $ toHtml $ "Filter" <> if hasActiveFilters then " (" <> show activeCount <> ")" else ""
        faSprite_ "filter" "regular" "h-4 w-4 stroke-iconNeutral"
    div_
      [ id_ popId
      , term "popover" "auto"
      , class_ "dropdown dropdown-start menu bg-bgRaised p-1 text-sm border border-strokeWeak z-50 w-60 rounded-md shadow-lg mt-1"
      , style_ $ "position-try: flip-block; position-anchor: --anchor-" <> popId
      ]
      do
        div_ [class_ "flex items-center justify-between px-3 py-2 text-sm font-semibold text-textStrong border-b border-strokeWeak"] do
          span_ "Select Filter"
          a_
            [ class_ "text-xs text-textBrand cursor-pointer flex items-center gap-1"
            , hxGet_ actions.baseUrl
            , hxTarget_ $ "#" <> actions.targetId
            , hxSelect_ $ "#" <> actions.targetId
            , hxPushUrl_ "true"
            , hxSwap_ "outerHTML"
            ]
            ("Clear all" >> faSprite_ "xmark" "regular" "w-3 h-3")
        div_ [class_ "p-1"] $ forM_ actions.filterMenus (renderFilterMenuItem actions)


renderFilterMenuItem :: TableHeaderActions -> FilterMenu -> Html ()
renderFilterMenuItem actions menu = div_ [class_ "relative"] do
  let subPopId = "filterSub_" <> menu.paramName
  button_
    [ class_ "flex items-center justify-between w-full px-3 py-2 text-sm rounded hover:bg-fillWeak cursor-pointer"
    , type_ "button"
    , term "popovertarget" subPopId
    , style_ $ "anchor-name: --anchor-" <> subPopId
    ]
    do
      span_ $ toHtml $ "By " <> menu.label
      faSprite_ "chevron-right" "regular" "w-3 h-3"
  div_
    [ class_ "dropdown dropdown-right menu bg-bgRaised rounded-lg shadow-lg w-48 border border-strokeWeak max-h-60 overflow-y-auto"
    , term "popover" "auto"
    , id_ subPopId
    , style_ $ "position-try: flip-inline; position-anchor: --anchor-" <> subPopId
    ]
    do
      div_ [class_ "px-3 py-2 text-sm font-semibold text-textStrong border-b border-strokeWeak"] $ toHtml menu.label
      div_ [class_ "p-1"] $ forM_ menu.options (renderFilterOption actions menu)


renderFilterOption :: TableHeaderActions -> FilterMenu -> FilterOption -> Html ()
renderFilterOption actions menu opt = label_ [class_ "flex items-center gap-3 px-3 py-2 rounded cursor-pointer hover:bg-fillWeak"] do
  let separator = if T.isInfixOf "?" actions.baseUrl then "&" else "?"
      paramVal = menu.paramName <> "=" <> toUriStr opt.value
      -- For multi-select: toggle this value; for single-select: replace all with this value
      url
        | menu.multiSelect && opt.isActive = deleteParamValue menu.paramName opt.value actions.baseUrl
        | menu.multiSelect = actions.baseUrl <> separator <> paramVal
        | otherwise = deleteParam menu.paramName actions.baseUrl <> separator <> paramVal
      inputType = if menu.multiSelect then "checkbox" else "radio"
  input_
    $ [ type_ inputType
      , class_ $ if menu.multiSelect then "checkbox checkbox-xs" else "radio radio-xs"
      , value_ opt.value
      , name_ $ "filter_" <> menu.paramName -- group radios by param name
      , hxGet_ url
      , hxTarget_ $ "#" <> actions.targetId
      , hxSelect_ $ "#" <> actions.targetId
      , hxPushUrl_ "true"
      , hxSwap_ "outerHTML"
      , hxTrigger_ "change"
      ]
    <> [checked_ | opt.isActive]
  span_ [class_ "text-sm"] $ toHtml opt.label


-- | Remove a specific param=value pair from a URL (for multi-select filter toggle)
deleteParamValue :: Text -> Text -> Text -> Text
deleteParamValue key val url = T.replace needle "" url
  where
    encodedVal = toUriStr val
    needle = "&" <> key <> "=" <> encodedVal


renderToolbar :: Table a -> Html ()
renderToolbar tbl =
  div_ [class_ $ "flex py-3 gap-8 items-center " <> if tbl.config.renderAsTable then "" else "bg-fillWeaker"] do
    when (isJust tbl.features.rowId && not tbl.config.renderAsTable) do
      div_ [class_ "h-4 flex space-x-3 w-8 items-center"] do
        span_ [class_ "w-2 h-full"] ""
        input_
          [ term "aria-label" "Select All"
          , type_ "checkbox"
          , class_ "checkbox h-6 w-6 checked:checkbox-primary"
          , [__| on click set .bulkactionItemCheckbox.checked to my.checked |]
          ]

    div_ [class_ "grow flex flex-row gap-2"] do
      forM_ tbl.features.bulkActions \blkA ->
        button_
          [ class_ "btn btn-sm btn-disabled group-has-[.bulkactionItemCheckbox:checked]/grid:text-white group-has-[.bulkactionItemCheckbox:checked]/grid:bg-fillBrand-strong group-has-[.bulkactionItemCheckbox:checked]/grid:pointer-events-auto!"
          , hxPost_ blkA.uri
          , hxSwap_ "none"
          ]
          do
            whenJust blkA.icon \icon -> faSprite_ icon "regular" "h-4 w-4 inline-block"
            span_ (toHtml blkA.title)

      whenJust tbl.features.sort renderSortMenu


renderSearch :: SearchMode -> Html ()
renderSearch searchMode =
  label_ [class_ "input input-sm flex w-full h-10 bg-fillWeak border border-strokeStrong shadow-none overflow-hidden items-center gap-2"] do
    faSprite_ "magnifying-glass" "regular" "w-4 h-4 opacity-70"
    case searchMode of
      ServerSide url ->
        input_
          [ type_ "text"
          , class_ "grow"
          , name_ "search"
          , id_ "search_box"
          , placeholder_ "Search"
          , hxTrigger_ "keyup changed delay:500ms"
          , hxGet_ url
          , hxTarget_ "#rowsContainer"
          , hxSwap_ "innerHTML"
          , hxIndicator_ "#searchIndicator"
          ]
      ClientSide ->
        input_
          [ type_ "text"
          , class_ "grow"
          , placeholder_ "Search"
          , [__| on input show .itemsListItem in #tableContainer_page when its textContent.toLowerCase() contains my value.toLowerCase() |]
          ]


renderSortMenu :: SortConfig -> Html ()
renderSortMenu sortCfg = do
  let currentURL' = deleteParam "sort" sortCfg.currentURL
  let defaultSort = maybe "" (\(_, _, i) -> i) (listToMaybe sortCfg.options)
  let currentSortTitle = maybe sortCfg.current (\(t, _, _) -> t) $ find (\(_, _, identifier) -> identifier == sortCfg.current) sortCfg.options

  div_ [class_ "dropdown dropdown-end inline-block"] do
    a_ [class_ "btn btn-sm shadow-none text-sm font-medium bg-fillWeaker border text-textWeak border-strokeWeak", tabindex_ "0"] do
      span_ $ toHtml currentSortTitle
      faSprite_ "sort" "regular" "h-4 w-4 stroke-iconNeutral"

    div_
      [ id_ "sortMenuDiv"
      , hxBoost_ "true"
      , class_ "dropdown-content bg-base-100 p-1 text-sm border border-black-30 z-50 mt-2 w-72 origin-top-right rounded-md shadow-lg"
      , tabindex_ "0"
      ]
      do
        sortCfg.options & mapM_ \(title, desc, identifier) -> do
          let isActive = sortCfg.current == identifier || (sortCfg.current == "" && identifier == defaultSort)
          a_
            [ class_ $ "block flex flex-row px-3 py-2 hover:bg-fillBrand-weak rounded-md cursor-pointer " <> (if isActive then " text-textBrand " else "")
            , href_ $ currentURL' <> "&sort=" <> toUriStr identifier
            , hxIndicator_ "#sortLoader"
            ]
            do
              div_ [class_ "flex flex-col items-center justify-center px-3"]
                $ if isActive then faSprite_ "icon-checkmark4" "solid" "w-4 h-5" else div_ [class_ "w-4 h-5"] ""
              div_ [class_ "grow space-y-1"] do
                span_ [class_ "block text-lg"] $ toHtml title
                span_ [class_ "block"] $ toHtml desc
  div_
    [ class_ "p-12 fixed rounded-lg shadow-sm bg-base-100 top-1/2 left-1/2 transform -translate-x-1/2 -translate-y-1/2 htmx-indicator loading loading-dots loading-md"
    , id_ "sortLoader"
    ]
    ""


-- Helper function for rendering pagination link
renderPaginationLink :: Text -> Text -> Html ()
renderPaginationLink url trigger =
  a_
    [ class_ "cursor-pointer flex justify-center items-center p-1 text-textBrand bg-fillBrand-weak hover:bg-fillBrand-weak text-center min-h-[2.5rem]"
    , hxTrigger_ $ case trigger of
        "click" -> "click"
        "intersect" -> "intersect once"
        _ -> "click, intersect once" -- "both" or default
    , hxSwap_ "outerHTML"
    , hxGet_ url
    , hxIndicator_ "#rowsIndicator"
    ]
    do
      span_ [class_ "inline-block"] "Load more"
      span_ [id_ "rowsIndicator", class_ "ml-2 htmx-indicator loading loading-dots loading-md inline-block"] ""


renderZeroState :: ZeroState -> Html ()
renderZeroState zs = do
  let url = case zs.destination of
        Left labelId -> labelId
        Right destination -> destination
  emptyState_ zs.title zs.description (Just url) zs.actionText


renderSimpleZeroState :: SimpleZeroState -> Html ()
renderSimpleZeroState zs =
  div_ [class_ "flex items-center justify-center gap-2 py-4 text-textWeak"] do
    faSprite_ zs.icon "regular" "h-4 w-4"
    span_ [class_ "text-sm"] $ toHtml zs.message


simpleZeroState :: Text -> Text -> SimpleZeroState
simpleZeroState icon message = SimpleZeroState{icon, message}


-- Column Builders

col :: Text -> (a -> Html ()) -> Column a
col name render =
  Column
    { name = name
    , render = render
    , attrs = []
    , sortField = Nothing
    , align = Nothing
    }


withSort :: Text -> Column a -> Column a
withSort field column = column{sortField = Just field}


withAttrs :: [Attribute] -> Column a -> Column a
withAttrs as column = column{attrs = as}


withAlign :: Text -> Column a -> Column a
withAlign a column = column{align = Just a}


-- Helper to render a row using columns
renderRowWithColumns :: [Attribute] -> [Column a] -> a -> Html ()
renderRowWithColumns attrs columns row =
  div_ attrs do
    forM_ columns \c ->
      div_ c.attrs $ c.render row


-- Sorting Utilities


-- Parse sort param like "+name,-updated_at" into sort fields
-- Optionally takes field name overrides (e.g. [("name", ["first_name", "last_name"])])
parseSortParam :: Text -> Maybe [(Text, [Text])] -> [SortField]
parseSortParam sortP overridesM = concatMap parseField (T.splitOn "," sortP)
  where
    overrideLookup = fromMaybe [] overridesM
    parseField txt
      | Just sortField <- T.stripPrefix "+" txt = createSortFields sortField Asc
      | Just sortField <- T.stripPrefix "-" txt = createSortFields sortField Desc
      | otherwise = []
    createSortFields sortField order =
      let matchingFields = fromMaybe [sortField] (lookup (T.toLower sortField) overrideLookup)
       in map (`SortField` order) matchingFields


-- Generate ORDER BY clause from sort fields
sortFieldsToSQL :: [SortField] -> Text
sortFieldsToSQL sortFields
  | null sortFields = ""
  | otherwise = "ORDER BY " <> T.intercalate ", " (map (.toSql) sortFields)


-- Generate SQL filter clause for a list of values
-- e.g. mkFilter "status" "text" id ["active", "pending"] => "status = ANY(ARRAY['active','pending']::text[])"
mkFilter :: Text -> Text -> (a -> Text) -> [a] -> Maybe Text
mkFilter col sqlType txtF values =
  guard (not $ null values) $> [fmt|{col} = ANY(ARRAY[{T.intercalate "," (map (quote . txtF) values)}]::{sqlType}[])|]
  where
    quote txt = [fmt|'{txt}'|]


-- Toggle sort direction for a column, returning the new sort param
toggleSortParam :: Text -> Text -> Text
toggleSortParam currentSort field
  | currentSort == "+" <> field = "-" <> field
  | otherwise = "+" <> field


-- Generate URL with new sort param
toggleSortUrl :: SortableConfig -> Text -> Text
toggleSortUrl cfg field =
  let urlWithoutSort = deleteParam "sort" cfg.baseUrl
      separator = if T.isInfixOf "?" urlWithoutSort then "&" else "?"
   in urlWithoutSort <> separator <> "sort=" <> toUriStr (toggleSortParam cfg.currentSort field)
