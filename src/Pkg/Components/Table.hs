{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Pkg.Components.Table (
  Table (..),
  TableRows (..),
  Column (..),
  Config (..),
  Features (..),
  BulkAction (..),
  SearchConfig (..),
  TabFilter (..),
  TabFilterOpt (..),
  SortConfig (..),
  PaginationConfig (..),
  LoadTrigger (..),
  ZeroState (..),
  HtmxConfig (..),
  Formatter (..),
  ColumnType (..),
  ProgressConfig (..),
  ProgressType (..),
  ProgressVariant (..),
  col,
  withSort,
  withAttrs,
  withAlign,
  withFormatter,
  withProgress,
) where

import Data.Default (Default (..))
import Data.Vector qualified as V
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Pages.Components (emptyState_)
import Relude
import Utils (deleteParam, faSprite_)


-- Core Types

type role Column nominal
data Column a where
  Column
    :: { name :: Text
       , render :: a -> Html ()
       , attrs :: [Attribute]
       , sortField :: Maybe Text
       , align :: Maybe Text
       , format :: Maybe Formatter
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
data TableRows a = TableRows (Maybe Text) [Column a] (V.Vector a) -- nextUrl, columns, rows


data Features a = Features
  { rowLink :: Maybe (a -> Text)
  , rowId :: Maybe (a -> Text)
  , rowAttrs :: Maybe (a -> [Attribute])
  , selectRow :: Maybe (a -> Bool)
  , bulkActions :: [BulkAction]
  , search :: Maybe SearchConfig
  , tabs :: Maybe TabFilter
  , sort :: Maybe SortConfig
  , pagination :: Maybe PaginationConfig
  , zeroState :: Maybe ZeroState
  , header :: Maybe (Html ())
  , htmx :: Maybe HtmxConfig
  }


data Config = Config
  { tableClasses :: Text
  , thClasses :: Text
  , tdClasses :: Text
  , containerClasses :: Text
  , showHeader :: Bool
  , elemID :: Text
  , renderAsTable :: Bool -- True for table mode, False for list mode
  }


-- Supporting Types

data Formatter = Formatter
  { columnType :: Maybe ColumnType
  , unit :: Maybe Text
  , progress :: Maybe ProgressConfig
  , link :: Maybe (Text -> Text)
  }


data ColumnType = CTNumber | CTDuration | CTText


data ProgressConfig = ProgressConfig
  { progressType :: ProgressType
  , variant :: ProgressVariant
  }


data ProgressType = ColumnPercent | ValuePercent
data ProgressVariant = PVDefault | PVError | PVWarning | PVSuccess


data BulkAction = BulkAction
  { icon :: Maybe Text
  , title :: Text
  , uri :: Text
  }


data SearchConfig = SearchConfig
  { serverSide :: Bool
  , viaQueryParam :: Maybe Text
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
  }


data PaginationConfig = PaginationConfig
  { nextUrl :: Maybe Text
  , trigger :: LoadTrigger
  }


data LoadTrigger = OnClick | OnIntersect | Both


data ZeroState = ZeroState
  { icon :: Text
  , title :: Text
  , description :: Text
  , actionText :: Text
  , destination :: Either Text Text
  }


data HtmxConfig = HtmxConfig
  { trigger :: Text
  , target :: Text
  , swap :: Text
  }


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
      , pagination = Nothing
      , zeroState = Nothing
      , header = Nothing
      , htmx = Nothing
      }


instance Default Config where
  def =
    Config
      { tableClasses = "table table-zebra table-sm w-full relative"
      , thClasses = "text-left bg-bgRaised sticky top-0"
      , tdClasses = "px-6 py-4"
      , containerClasses = "w-full mx-auto px-6 pt-4 space-y-4 pb-16 overflow-y-scroll h-full" -- Match ItemsList's scrollable container
      , showHeader = True
      , elemID = "tableContainer"
      , renderAsTable = False -- Default to list mode for ItemsList compatibility
      }


-- ToHtml Instance

instance ToHtml a => ToHtml (Table a) where
  {-# INLINE toHtml #-}
  toHtml tbl = toHtmlRaw $ renderTable tbl
  {-# INLINE toHtmlRaw #-}
  toHtmlRaw tbl = toHtmlRaw $ renderTable tbl


-- TableRows ToHtml - only renders rows + pagination link for load more

instance ToHtml (TableRows a) where
  toHtml (TableRows nextUrl columns rows) = toHtmlRaw $ renderTableRows nextUrl columns rows
  toHtmlRaw (TableRows nextUrl columns rows) = toHtmlRaw $ renderTableRows nextUrl columns rows


renderTableRows :: Maybe Text -> [Column a] -> V.Vector a -> Html ()
renderTableRows nextUrl columns rows = do
  V.forM_ rows \row ->
    div_ [class_ "flex gap-8 items-start itemsListItem"] do
      forM_ columns \col ->
        div_ col.attrs $ col.render row
  whenJust nextUrl \url ->
    when (V.length rows > 9)
      $ a_
        [ class_ "cursor-pointer flex justify-center items-center p-1 text-textBrand bg-fillBrand-weak hover:bg-fillBrand-weak text-center"
        , hxTrigger_ "click, intersect once"
        , hxSwap_ "outerHTML"
        , hxGet_ url
        , hxIndicator_ "#rowsIndicator"
        ]
        do
          "Load more"
          span_ [id_ "rowsIndicator", class_ "ml-2 htmx-indicator loading loading-dots loading-md"] ""


-- Tab Filter ToHtml

instance ToHtml TabFilter where
  toHtmlRaw = toHtml
  toHtml tf = div_ [class_ "tabs tabs-box tabs-outline items-center"] do
    let uri = deleteParam "filter" tf.currentURL
    forM_ tf.options \opt ->
      a_
        [ href_ $ uri <> "&filter=" <> opt.name
        , role_ "tab"
        , class_ $ "tab h-auto! " <> if opt.name == tf.current then "tab-active text-textStrong" else ""
        ]
        do
          span_ $ toHtml opt.name
          whenJust opt.count $ span_ [class_ "absolute top-[1px] -right-[5px] text-textInverse-strong text-xs font-medium rounded-full px-1 bg-fillError-strong"] . show


-- Core Rendering Functions

renderTable :: ToHtml a => Table a -> Html ()
renderTable tbl = div_ [class_ tbl.config.containerClasses, id_ $ tbl.config.elemID <> "_page"] do
  whenJust tbl.features.header id
  whenJust tbl.features.search renderSearch

  div_
    [ class_ "grid card-round overflow-hidden my-0 group/grid"
    , id_ $ tbl.config.elemID <> "_grid"
    ]
    do
      form_
        [ class_ "flex flex-col divide-y w-full"
        , id_ tbl.config.elemID
        , onkeydown_ "return event.key != 'Enter';"
        ]
        do
          when (isJust tbl.features.rowId || isJust tbl.features.sort)
            $ renderToolbar tbl

          when (V.null tbl.rows) $ whenJust tbl.features.zeroState renderZeroState

          div_ [class_ "w-full flex-col"] do
            whenJust tbl.features.search \_ ->
              span_ [id_ "searchIndicator", class_ "htmx-indicator loading loading-sm loading-dots mx-auto"] ""
            div_ [id_ "rowsContainer", class_ "divide-y"] do
              renderRows tbl
              whenJust tbl.features.pagination renderPagination


renderRows :: Table a -> Html ()
renderRows tbl =
  if tbl.config.renderAsTable
    then V.mapM_ (renderTableRow tbl) tbl.rows
    else V.mapM_ (renderListRow tbl) tbl.rows


-- List mode: render columns in a flex container (no table wrapper/headers)
renderListRow :: Table a -> a -> Html ()
renderListRow tbl row =
  div_ (rowAttrs <> [class_ "flex gap-8 items-start itemsListItem"]) do
    forM_ tbl.columns \col ->
      div_ col.attrs $ col.render row
  where
    rowAttrs = maybe [] ($ row) tbl.features.rowAttrs


-- Table mode: render as table rows with columns
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

    forM_ tbl.columns \col ->
      td_ (col.attrs <> colAttrs col)
        $ col.render row
  where
    rowAttrs = maybe [] ($ row) tbl.features.rowAttrs
    linkHandler = [] -- TODO: add link handler for table mode
    isSelected = maybe False (\f -> f row) tbl.features.selectRow
    colAttrs col = maybe [] (const [class_ $ maybeToMonoid col.align]) col.align


renderToolbar :: Table a -> Html ()
renderToolbar tbl =
  div_ [class_ "flex py-3 gap-8 items-center bg-fillWeaker"] do
    div_ [class_ "h-4 flex space-x-3 w-8 items-center"] do
      span_ [class_ "w-2 h-full"] ""
      when (isJust tbl.features.rowId)
        $ input_
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
            whenJust blkA.icon \icon -> faSprite_ icon "solid" "h-4 w-4 inline-block"
            span_ (toHtml blkA.title)

      whenJust tbl.features.sort renderSortMenu


renderSearch :: SearchConfig -> Html ()
renderSearch cfg =
  label_ [class_ "input input-sm flex w-full h-10 bg-fillWeak border border-strokeStrong shadow-none overflow-hidden items-center gap-2"] do
    faSprite_ "magnifying-glass" "regular" "w-4 h-4 opacity-70"
    case cfg.viaQueryParam of
      Just param ->
        input_
          [ type_ "text"
          , class_ "grow"
          , name_ "search"
          , id_ "search_box"
          , placeholder_ "Search"
          , hxTrigger_ "keyup changed delay:500ms"
          , hxGet_ param
          , hxTarget_ "#rowsContainer"
          , hxSwap_ "innerHTML"
          , hxIndicator_ "#searchIndicator"
          ]
      Nothing ->
        input_
          [ type_ "text"
          , class_ "grow"
          , placeholder_ "Search"
          , [__| on input show .itemsListItem in #tableContainer_page when its textContent.toLowerCase() contains my value.toLowerCase() |]
          ]


renderSortMenu :: SortConfig -> Html ()
renderSortMenu sortCfg = do
  let sortMenu =
        [ ("First Seen", "First time the issue occured", "first_seen")
        , ("Last Seen", "Last time the issue occured", "last_seen")
        , ("Events", "Number of events", "events")
        ]
          :: [(Text, Text, Text)]
  let currentURL' = deleteParam "sort" sortCfg.currentURL
  let currentSortTitle = maybe "First Seen" (\(t, _, _) -> t) $ find (\(_, _, identifier) -> identifier == sortCfg.current) sortMenu

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
        sortMenu & mapM_ \(title, desc, identifier) -> do
          let isActive = sortCfg.current == identifier || (sortCfg.current == "" && identifier == "first_seen")
          a_
            [ class_ $ "block flex flex-row px-3 py-2 hover:bg-fillBrand-weak rounded-md cursor-pointer " <> (if isActive then " text-textBrand " else "")
            , href_ $ currentURL' <> "&sort=" <> identifier
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


renderPagination :: PaginationConfig -> Html ()
renderPagination cfg = whenJust cfg.nextUrl \url ->
  a_
    [ class_ "cursor-pointer flex justify-center items-center p-1 text-textBrand bg-fillBrand-weak hover:bg-fillBrand-weak text-center"
    , hxTrigger_ $ case cfg.trigger of
        OnClick -> "click"
        OnIntersect -> "intersect once"
        Both -> "click, intersect once"
    , hxSwap_ "outerHTML"
    , hxGet_ url
    , hxIndicator_ "#rowsIndicator"
    ]
    do
      "Load more"
      span_ [id_ "rowsIndicator", class_ "ml-2 htmx-indicator loading loading-dots loading-md"] ""


renderZeroState :: ZeroState -> Html ()
renderZeroState zs = do
  let url = case zs.destination of
        Left labelId -> labelId
        Right destination -> destination
  emptyState_ zs.title zs.description (Just url) zs.actionText


-- Column Builders

col :: Text -> (a -> Html ()) -> Column a
col name render =
  Column
    { name = name
    , render = render
    , attrs = []
    , sortField = Nothing
    , align = Nothing
    , format = Nothing
    }


withSort :: Text -> Column a -> Column a
withSort field column = column{sortField = Just field}


withAttrs :: [Attribute] -> Column a -> Column a
withAttrs as column = column{attrs = as}


withAlign :: Text -> Column a -> Column a
withAlign a column = column{align = Just a}


withFormatter :: Formatter -> Column a -> Column a
withFormatter f column = column{format = Just f}


withProgress :: ProgressConfig -> Column a -> Column a
withProgress p column = column{format = Just $ Formatter Nothing Nothing (Just p) Nothing}
