module Pkg.Components.Facets (Config (..), Layout (..), FacetNode (..), FacetContent (..), FacetOverflow (..), FacetChoice (..), sidebar_, facetNode_, facetChoice_, facetTail_, facetHeadCount) where

import Lucid
import Lucid.Hyperscript (__)
import Pages.Components (detailsClosedBelowAttr_, filterInputAttr_, searchInput_)
import Relude
import Utils (LoadingSize (..), LoadingType (..), faSprite_, loadingIndicator_, prettyPrintCount)


data Layout = Plain | Collapsible


data Config = Config
  { elemId :: Maybe Text
  , extraClass :: Text
  , clear :: Maybe [Attribute]
  , content :: Maybe [FacetNode]
  , layout :: Layout
  }


-- | Explorer's facet sidebar, also embedded by inventory tables and the host map.
sidebar_ :: Config -> Html ()
sidebar_ cfg = case cfg.layout of
  Plain -> rail
  Collapsible -> details_ [open_ "", class_ "w-68 shrink-0 max-lg:w-full", detailsClosedBelowAttr_ 1024] do
    summary_ [class_ "flex cursor-pointer list-none items-center gap-2 rounded px-2 py-2 text-xs font-semibold text-textStrong hover:bg-fillWeak lg:hidden [&::-webkit-details-marker]:hidden"] "Filters"
    rail
  where
    rail = div_ ([class_ $ "facet-rail flex w-68 shrink-0 flex-col gap-2 text-sm text-textWeak max-md:w-full " <> cfg.extraClass <> case cfg.layout of { Plain -> ""; Collapsible -> " max-lg:w-full" }, data_ "component" "facet-rail"] <> [id_ x | x <- maybeToList cfg.elemId]) do
      searchInput_
        "sticky top-0 z-10 h-9 w-[calc(100%-1rem)] mx-2 border-strokeStrong bg-bgBase"
        "Search filters"
        [name_ "facet-search", filterInputAttr_ ".facet-rail .facet-item, .facet-rail .facet-section"]
      forM_ cfg.clear $ \attrs -> button_ ([type_ "button", class_ "flex items-center justify-between rounded px-2 py-1.5 text-xs text-textBrand hover:bg-fillWeak"] <> attrs) $ "Clear all" >> faSprite_ "xmark" "regular" "h-3 w-3"
      div_ [class_ "facets-container"] $ maybe (div_ [class_ "px-1 py-4 text-xs italic text-textWeak"] "Filters are still being built for this project.") (mapM_ facetNode_) cfg.content


data FacetNode = FacetNode
  { label :: Text
  , path :: Maybe Text
  , expanded :: Bool
  , attrs :: [Attribute]
  , actions :: Maybe (Html ())
  , content :: FacetContent
  }


data FacetContent
  = FacetValues [FacetChoice] FacetOverflow
  | FacetChildren [FacetNode]
  | FacetLoading


facetNode_ :: FacetNode -> Html ()
facetNode_ node = do
  let grouped = case node.content of FacetValues{} -> False; FacetChildren{} -> True; FacetLoading -> True
  details_ ([class_ $ "facet-section block border-t border-strokeWeak [&[open]>summary_.facet-chevron]:rotate-0 " <> bool "" "facet-section-group" grouped, data_ "component" "facet-section"] <> node.attrs <> [open_ "" | node.expanded]) do
    summary_ [class_ "flex cursor-pointer list-none items-center gap-2 rounded px-2 py-2 text-textStrong hover:bg-fillWeak [&::-webkit-details-marker]:hidden"] do
      faSprite_ "chevron-down" "regular" "facet-chevron h-2.5 w-2.5 shrink-0 -rotate-90 transition-transform"
      div_ [class_ "flex min-w-0 flex-1 items-center justify-between gap-2"] do
        span_ ([class_ $ "truncate text-sm " <> bool "font-normal" "font-medium" grouped] <> [term "data-tippy-content" p | p <- maybeToList node.path]) $ toHtml node.label
        forM_ node.actions $ \actions -> div_ [class_ "inline-block", [__|on click halt the event's bubbling|]] actions
    case node.content of
      FacetChildren children -> div_ [class_ "facets-container"] $ mapM_ facetNode_ children
      FacetLoading -> div_ [class_ "facet-group-loader htmx-indicator flex h-8 items-center justify-center"] $ loadingIndicator_ LdXS LdSpinner
      FacetValues choices overflow -> values_ choices overflow
  where
    values_ choices overflow = div_ [class_ "facet-values pl-5 pr-1 mb-1 space-y-1"] do
      let (visible, hidden) = splitAt facetHeadCount choices
          moreLabel = "+ More (" <> prettyPrintCount (length hidden) <> ")"
      if null choices
        then div_ [class_ "facet-empty px-1 py-1 text-xs italic text-textWeak"] "no values in window"
        else forM_ visible facetChoice_
      unless (null hidden) $ case overflow of
        ExpandValues -> facetTail_ False hidden
        FetchValues attrs -> div_ [class_ "facet-tail"] $ button_ ([type_ "button", class_ "facet-more text-textBrand text-xs px-1 py-0.5 cursor-pointer hover:underline"] <> attrs) $ toHtml moreLabel


facetTail_ :: Bool -> [FacetChoice] -> Html ()
facetTail_ expanded choices = details_ ([class_ "facet-tail group/tail"] <> [open_ "" | expanded]) do
  summary_ [class_ "list-none cursor-pointer text-textBrand text-xs px-1 py-0.5 hover:underline focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-strokeBrand-strong"] do
    span_ [class_ "group-open/tail:hidden"] $ toHtml $ "+ More (" <> count <> ")"
    span_ [class_ "hidden group-open/tail:inline"] $ toHtml $ "− Less (" <> count <> ")"
  div_ [class_ "space-y-1"] $ mapM_ facetChoice_ choices
  where
    count = prettyPrintCount $ length choices


-- | Local inventories expand in place; Explorer fetches the tail lazily.
data FacetOverflow = ExpandValues | FetchValues [Attribute]


-- | The initial field and its lazy tail must use the same cutoff.
facetHeadCount :: Int
facetHeadCount = 5


data FacetChoice = FacetChoice
  { label :: Text
  , inputAttrs :: [Attribute]
  , marker :: Maybe (Html ())
  , count :: Maybe Text
  }


facetChoice_ :: FacetChoice -> Html ()
facetChoice_ choice = label_ [class_ "facet-item flex min-h-7 cursor-pointer items-center justify-between gap-2 rounded px-2 py-1 hover:bg-fillWeak max-md:min-h-9", data_ "component" "facet-option"] do
  div_ [class_ "flex min-w-0 flex-1 items-center gap-2"] do
    input_ $ [type_ "checkbox", class_ "checkbox checkbox-xs max-md:checkbox-sm"] <> choice.inputAttrs
    sequence_ choice.marker
    span_ [class_ "facet-value truncate text-xs", term "data-tippy-content" choice.label] $ toHtml choice.label
  forM_ choice.count $ \n -> span_ [class_ "facet-count shrink-0 text-xs tabular-nums text-textWeak"] $ toHtml n
