module Pkg.Components.PageHeader (
  -- * Page header types
  PageHeader (..),
  Breadcrumb (..),

  -- * Builders
  pageHeader,
  withSubtitle,
  withBreadcrumbs,
  withActions,

  -- * Rendering
  renderPageHeader,

  -- * Section helpers
  collapsibleSection,
  card,
) where

import Lucid
import Lucid.Hyperscript (__)
import Relude
import Utils (faSprite_)


-- | Page header with title, optional subtitle, breadcrumbs, and action buttons
data PageHeader = PageHeader
  { title :: Text
  , subtitle :: Maybe Text
  , breadcrumbs :: [Breadcrumb]
  , actions :: Maybe (Html ()) -- Action buttons on the right
  }


-- | Breadcrumb navigation item
data Breadcrumb = Breadcrumb
  { label :: Text
  , url :: Maybe Text -- Nothing for current/last item
  }


-- | Create a simple page header with just a title
pageHeader :: Text -> PageHeader
pageHeader t =
  PageHeader
    { title = t
    , subtitle = Nothing
    , breadcrumbs = []
    , actions = Nothing
    }


withSubtitle :: Text -> PageHeader -> PageHeader
withSubtitle s ph = ph{subtitle = Just s}


withBreadcrumbs :: [Breadcrumb] -> PageHeader -> PageHeader
withBreadcrumbs bs ph = ph{breadcrumbs = bs}


withActions :: Html () -> PageHeader -> PageHeader
withActions a ph = ph{actions = Just a}


-- | Render a page header
renderPageHeader :: PageHeader -> Html ()
renderPageHeader ph = do
  -- Breadcrumbs
  unless (null ph.breadcrumbs) do
    nav_ [class_ "text-sm breadcrumbs mb-2"] do
      ol_ [class_ "flex items-center gap-1.5 text-textWeak"] do
        forM_ ph.breadcrumbs \bc -> li_ [class_ "flex items-center gap-1.5"] do
          case bc.url of
            Just u -> a_ [href_ u, class_ "hover:text-textStrong transition-colors"] $ toHtml bc.label
            Nothing -> span_ [class_ "text-textStrong font-medium"] $ toHtml bc.label

  -- Title bar
  div_ [class_ "flex justify-between items-center mb-6"] do
    div_ [class_ "flex flex-col gap-1"] do
      h2_ [class_ "text-xl font-semibold text-textStrong"] $ toHtml ph.title
      whenJust ph.subtitle \s ->
        p_ [class_ "text-sm text-textWeak"] $ toHtml s
    whenJust ph.actions id


-- | Collapsible section with details/summary pattern
-- Reusable across Monitors, Projects, and other pages
collapsibleSection :: Text -> Maybe Text -> Html () -> Html ()
collapsibleSection title subtitleM content =
  details_ [class_ "bg-bgBase rounded-xl border border-strokeWeak overflow-hidden"] do
    summary_
      [ class_ "p-4 cursor-pointer list-none flex items-center justify-between gap-2"
      , [__|on click toggle .rotate-180 on the next <svg/> in me|]
      ]
      do
        div_ [class_ "flex items-center gap-2"] do
          span_ [class_ "font-medium text-sm"] $ toHtml title
          whenJust subtitleM $ span_ [class_ "text-xs text-textWeak"] . toHtml
        faSprite_ "chevron-down" "regular" "w-3.5 h-3.5 text-iconNeutral transition-transform duration-150"
    div_ [class_ "px-4 pb-4"] content


-- | Standard card container used across the app
card :: Html () -> Html ()
card = div_ [class_ "rounded-lg bg-bgRaised border border-strokeWeak p-4"]
