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
  ZeroState (..),
  col,
  withSort,
  withAttrs,
  withAlign,
  renderRowWithColumns,
) where

import Data.Default (Default (..))
import Data.Vector qualified as V
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Pages.Components (emptyState_)
import Relude
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
data TableRows a = TableRows (Maybe Text) [Column a] (V.Vector a) -- nextUrl, columns, rows


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
  , actionText :: Text
  , destination :: Either Text Text
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
      }


instance Default Config where
  def =
    Config
      { tableClasses = "table table-zebra table-sm w-full relative"
      , thClasses = "text-left bg-bgRaised sticky top-0"
      , tdClasses = "px-6 py-4"
      , containerClasses = "w-full mx-auto px-6 pt-4 space-y-4 pb-16 overflow-y-scroll h-full"
      , showHeader = True
      , elemID = "tableContainer"
      , renderAsTable = False
      }


-- ToHtml Instance

instance ToHtml (Table a) where
  {-# INLINE toHtml #-}
  toHtml tbl = toHtmlRaw $ renderTable tbl
  {-# INLINE toHtmlRaw #-}
  toHtmlRaw tbl = toHtmlRaw $ renderTable tbl


-- TableRows ToHtml - only renders rows + pagination link for load more

instance ToHtml (TableRows a) where
  toHtml (TableRows nextUrl columns rows) = toHtmlRaw $ renderTableRows nextUrl columns rows
  toHtmlRaw (TableRows nextUrl columns rows) = toHtmlRaw $ renderTableRows nextUrl columns rows


{-# INLINE renderTableRows #-}
renderTableRows :: Maybe Text -> [Column a] -> V.Vector a -> Html ()
renderTableRows nextUrl columns rows = do
  V.forM_ rows \row ->
    div_ [class_ "flex gap-8 items-start itemsListItem"] do
      forM_ columns \c -> div_ c.attrs $ c.render row
  whenJust nextUrl (`renderPaginationLink` "both")


-- Tab Filter ToHtml

instance ToHtml TabFilter where
  toHtmlRaw = toHtml
  toHtml tf = div_ [class_ "tabs tabs-box tabs-outline items-center"] do
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
renderTable tbl = div_ [class_ tbl.config.containerClasses, id_ $ tbl.config.elemID <> "_page"] do
  whenJust tbl.features.header id
  whenJust tbl.features.search renderSearch

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
          when (isJust tbl.features.rowId || isJust tbl.features.sort)
            $ renderToolbar tbl

          when (V.null tbl.rows) $ whenJust tbl.features.zeroState renderZeroState

          div_ [class_ "w-full flex-col"] do
            whenJust tbl.features.search \_ ->
              span_ [id_ "searchIndicator", class_ "htmx-indicator loading loading-sm loading-dots mx-auto"] ""
            div_ [id_ "rowsContainer", class_ "divide-y"] do
              renderRows tbl
              whenJust tbl.features.pagination $ uncurry renderPaginationLink


renderRows :: Table a -> Html ()
renderRows tbl =
  if tbl.config.renderAsTable
    then V.mapM_ (renderTableRow tbl) tbl.rows
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
