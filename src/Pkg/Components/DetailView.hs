module Pkg.Components.DetailView (
  -- * Detail view types
  DetailView (..),
  DetailRow (..),
  DiffPair (..),

  -- * Builders
  detailView,
  kvRow,
  kvRowHtml,
  badgeRow,
  diffRow,

  -- * Rendering
  renderDetailView,
  renderDetailRow,
  renderDiffPair,
) where

import Data.Text qualified as T
import Lucid
import Relude
import Utils (faSprite_)


-- | A detail/key-value view for displaying entity properties
data DetailView = DetailView
  { title :: Maybe Text
  , rows :: [DetailRow]
  , containerClass :: Text
  }


-- | A single key-value row in a detail view
data DetailRow
  = -- | Simple text key-value
    KVText
      { label :: Text
      , value :: Text
      }
  | -- | Key with HTML value
    KVHtml
      { label :: Text
      , valueHtml :: Html ()
      }
  | -- | Key with badge value
    KVBadge
      { label :: Text
      , badgeText :: Text
      , badgeClass :: Text
      }
  | -- | Before/after diff display
    KVDiff
      { label :: Text
      , diff :: DiffPair
      }


-- | Before/after comparison for diffs and changes
data DiffPair = DiffPair
  { beforeLabel :: Text
  , beforeValue :: Text
  , afterLabel :: Text
  , afterValue :: Text
  }


-- | Create a detail view
detailView :: [DetailRow] -> DetailView
detailView rs =
  DetailView
    { title = Nothing
    , rows = rs
    , containerClass = "space-y-2"
    }


-- | Simple text key-value row
kvRow :: Text -> Text -> DetailRow
kvRow = KVText


-- | Key-value row with HTML content
kvRowHtml :: Text -> Html () -> DetailRow
kvRowHtml = KVHtml


-- | Key-value row with a badge
badgeRow :: Text -> Text -> Text -> DetailRow
badgeRow = KVBadge


-- | Diff/change row showing before and after
diffRow :: Text -> DiffPair -> DetailRow
diffRow = KVDiff


-- | Render a complete detail view
renderDetailView :: DetailView -> Html ()
renderDetailView dv = do
  whenJust dv.title \t ->
    h3_ [class_ "text-sm font-medium text-textStrong mb-3"] $ toHtml t
  div_ [class_ dv.containerClass] $
    forM_ dv.rows renderDetailRow


-- | Render a single detail row
renderDetailRow :: DetailRow -> Html ()
renderDetailRow = \case
  KVText lbl val ->
    div_ [class_ "flex justify-between items-center py-1.5"] do
      span_ [class_ "text-sm text-textWeak"] $ toHtml lbl
      span_ [class_ "text-sm text-textStrong"] $ toHtml val
  KVHtml lbl valH ->
    div_ [class_ "flex justify-between items-center py-1.5"] do
      span_ [class_ "text-sm text-textWeak"] $ toHtml lbl
      valH
  KVBadge lbl bText bCls ->
    div_ [class_ "flex justify-between items-center py-1.5"] do
      span_ [class_ "text-sm text-textWeak"] $ toHtml lbl
      span_ [class_ $ "badge " <> bCls] $ toHtml bText
  KVDiff lbl dp ->
    div_ [class_ "py-1.5"] do
      span_ [class_ "text-sm text-textWeak block mb-2"] $ toHtml lbl
      renderDiffPair dp


-- | Render a before/after diff comparison
renderDiffPair :: DiffPair -> Html ()
renderDiffPair dp = div_ [class_ "grid grid-cols-2 gap-4"] do
  when (not $ T.null dp.beforeValue) do
    div_ [] do
      span_ [class_ "text-xs text-textWeak block mb-1 font-medium"] $ toHtml dp.beforeLabel
      code_ [class_ "block bg-fillError-weak text-fillError-strong px-3 py-2 rounded text-xs overflow-x-auto border border-strokeError-weak"] $
        toHtml dp.beforeValue
  when (not $ T.null dp.afterValue) do
    div_ [] do
      span_ [class_ "text-xs text-textWeak block mb-1 font-medium"] $ toHtml dp.afterLabel
      code_ [class_ "block bg-fillSuccess-weak text-fillSuccess-strong px-3 py-2 rounded text-xs overflow-x-auto border border-strokeSuccess-weak"] $
        toHtml dp.afterValue
