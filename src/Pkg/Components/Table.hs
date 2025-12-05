{-# LANGUAGE OverloadedStrings #-}

module Pkg.Components.Table (
  Table (..),
  TableColumn (..),
  TableRow,
  TableCell (..),
  renderTable,
  defaultTable,
  mkColumn,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Lucid
import Lucid.Hyperscript (__)
import Relude
import Utils (faSprite_)


data TableColumn = TableColumn
  { columnHeader :: T.Text
  , columnKey :: T.Text
  , columnWidth :: Maybe Text
  , columnRender :: Maybe (TableCell -> Html ())
  , columnSortable :: Bool
  , columnCheckBox :: Bool
  , columnActionable :: Bool
  }


data TableCell where
  CellText :: Text -> TableCell
  CellArray :: [TableCell] -> TableCell
  CellCustom :: ToHtml a => a -> TableCell
  CellEmpty :: TableCell
  CellCheckbox :: Text -> TableCell


-- | Type alias for table row data using Map for efficient lookup
type TableRow = Map.Map T.Text TableCell


-- | Table configuration
data Table = Table
  { tableId :: Maybe T.Text
  , tableClass :: T.Text
  , tableColumns :: [TableColumn]
  , tableHasSearch :: Bool
  , tableRows :: [TableRow]
  , tableHasCheckboxes :: Bool
  , tableActions :: Maybe ([Html ()])
  , tableEmptyMessage :: Maybe T.Text
  , tableCaption :: Maybe T.Text
  , rowAction :: Maybe (TableRow -> Text)
  }


-- | Smart constructor for a basic column
mkColumn :: T.Text -> T.Text -> TableColumn
mkColumn header key =
  TableColumn
    { columnHeader = header
    , columnKey = key
    , columnWidth = Nothing
    , columnRender = Nothing
    , columnSortable = True
    , columnCheckBox = False
    , columnActionable = True
    }


-- | Default table configuration
defaultTable :: [TableColumn] -> [TableRow] -> Table
defaultTable cols rows =
  Table
    { tableId = Nothing
    , tableClass = ""
    , tableColumns = cols
    , tableRows = rows
    , tableEmptyMessage = Just "No data available"
    , tableCaption = Nothing
    , tableHasSearch = False
    , tableHasCheckboxes = True
    , tableActions = Nothing
    , rowAction = Nothing
    }


-- | Render a table to HTML
renderTable :: Table -> Html ()
renderTable tbl = div_ [class_ ""] do
  div_ [class_ "flex items-center justify-between mb-4 gap-8"] do
    when tbl.tableHasSearch do
      div_ [class_ "flex gap-2 w-full shrink-1"] do
        label_ [class_ "input input-sm flex-1 flex bg-fillWeaker  max-w-xl min-w-sm border-strokeWeak shadow-none overflow-hidden items-center gap-2"] do
          faSprite_ "magnifying-glass" "regular" "w-3 h-3 opacity-70"
          input_
            [ type_ "text"
            , placeholder_ "Search..."
            , class_ ""
            , [__|on keyup if the event's key is 'Escape' set my value to '' then trigger keyup
                         else show <.searchable/> in the next <table/> when its textContent.toLowerCase() contains my value.toLowerCase() |]
            ]
    div_ [class_ "flex items-center gap-2 w-max grow-1 shrink-0"] do
      whenJust tbl.tableActions
        $ \actions ->
          div_ [class_ "flex gap-2"] $ mapM_ toHtml actions
  table_ (tableAttrs tbl) $ do
    whenJust (tableCaption tbl) $ \caption ->
      caption_ $ toHtml caption
    thead_ [class_ "bg-fillWeaker"] do
      tr_ do
        mapM_ (renderHeader tbl) (tableColumns tbl)
    tbody_
      $ if null (tbl.tableRows)
        then renderEmptyState tbl
        else mapM_ (renderRow (tableColumns tbl) (tableHasCheckboxes tbl) tbl) tbl.tableRows
  where
    tableAttrs t =
      let baseClass = "table " <> tableClass t
          idAttr = maybe [] (\i -> [id_ i]) (tableId t)
       in [class_ baseClass, role_ "table"] <> idAttr


renderHeader :: Table -> TableColumn -> Html ()
renderHeader tbl col =
  if col.columnCheckBox
    then
      th_ [style_ "width: 40px;"]
        $ input_
          [ type_ "checkbox"
          , class_ "checkbox checkbox-sm"
          , [__|on click set .checkbox.checked to my.checked|]
          ]
    else th_ (headerAttrs col) $ do
      toHtml (columnHeader col)
      when (columnSortable col) $ do
        span_ [class_ "sort-icon", role_ "button"]
          $ faSprite_ "arrow-up-down" "regular" "w-3.5 h-3.5 ml-2"
  where
    headerAttrs c =
      let widthAttr = maybe [] (\w -> [style_ $ "width: " <> w]) (columnWidth c)
       in widthAttr <> if col.columnSortable then [class_ "cursor-pointer select-none"] else []


renderRow :: [TableColumn] -> Bool -> Table -> TableRow -> Html ()
renderRow cols hasCheckboxes tbl rowData = do
  let actionAttr = case tbl.rowAction of
        Just actionFn ->
          let scr = actionFn rowData
           in [term "_" scr]
        Nothing -> ([] :: [Attribute])

  tr_ [class_ ("searchable hover:bg-fillWeaker " <> (if isJust tbl.rowAction then "cursor-pointer" else ""))] do
    mapM_ (renderCell rowData actionAttr) cols


renderCell :: TableRow -> [Attribute] -> TableColumn -> Html ()
renderCell rowData atrr col =
  td_ (if col.columnActionable then atrr else []) $ case columnRender col of
    Just renderFn -> renderFn cellValue
    Nothing -> renderDefaultCell cellValue
  where
    cellValue = Map.findWithDefault CellEmpty (columnKey col) rowData


renderDefaultCell :: TableCell -> Html ()
renderDefaultCell (CellText txt) = toHtml txt
renderDefaultCell (CellArray arr) =
  ul_ $ mapM_ (li_ . renderDefaultCell) arr
renderDefaultCell (CellCustom a) = toHtml a
renderDefaultCell CellEmpty = ""
renderDefaultCell (CellCheckbox name) =
  input_ [type_ "checkbox", name_ name, class_ "checkbox checkbox-sm tr-checkbox"]


renderEmptyState :: Table -> Html ()
renderEmptyState tbl =
  tr_
    $ td_ [colspan_ (show $ length $ tableColumns tbl), class_ "text-center"]
    $ toHtml
    $ fromMaybe "No data available" (tableEmptyMessage tbl)
