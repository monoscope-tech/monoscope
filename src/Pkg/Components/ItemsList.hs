module Pkg.Components.ItemsList (
  itemsList_,
  itemsPage_,
  itemRows_,
  ItemsRows (..),
  ItemsPage (..),
  BulkAction (..),
  ZeroState (..),
  Heading (..),
  ItemsListCfg (..),
  TabFilter (..),
  TabFilterOpt (..),
  SortCfg (..),
  SearchCfg (..),
)
where

import Data.Time (UTCTime)
import Data.Tuple.Extra (fst3)
import Data.Vector qualified as V
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Relude
import Utils (deleteParam, escapedQueryPartial, faSprite_)


data ItemsListCfg = ItemsListCfg
  { currentURL :: Text
  , sort :: Maybe SortCfg
  , projectId :: Projects.ProjectId
  , currTime :: UTCTime
  , elemID :: Text
  , nextFetchUrl :: Maybe Text
  , zeroState :: Maybe ZeroState
  , tabsFilter :: Maybe TabFilter
  , heading :: Maybe Heading
  , bulkActions :: [BulkAction]
  , search :: Maybe SearchCfg
  }


data SearchCfg = SearchCfg
  {viaQueryParam :: Maybe Text}


data BulkAction = BulkAction
  { icon :: Maybe Text
  , title :: Text
  , uri :: Text
  }


data SortCfg = SortCfg
  { current :: Text
  }


data Heading = Heading
  { pageTitle :: Html ()
  , rightComponent :: Maybe (Html ())
  , subSection :: Maybe (Html ())
  }


data TabFilter = TabFilter
  { current :: Text
  , options :: [TabFilterOpt]
  }


data TabFilterOpt = TabFilterOpt
  { name :: Text
  , count :: Maybe Int
  }


data ZeroState = ZeroState
  { icon :: Text
  , title :: Text
  , description :: Text
  , actionText :: Text
  , destination :: Either Text Text -- Either LabelID URL
  }


type role ItemsPage representational
data ItemsPage a = ItemsPage ItemsListCfg (V.Vector a)


instance ToHtml a => ToHtml (ItemsPage a) where
  {-# INLINE toHtml #-}
  toHtml (ItemsPage cfg items) = toHtmlRaw $ itemsPage_ cfg items
  {-# INLINE toHtmlRaw #-}
  toHtmlRaw (ItemsPage cfg items) = toHtmlRaw $ itemsPage_ cfg items


itemsPage_ :: ToHtml a => ItemsListCfg -> V.Vector a -> Html ()
itemsPage_ listCfg items = div_ [class_ "w-full mx-auto px-16 pt-10 pb-24 overflow-y-scroll h-full space-y-6", id_ "itemsListPage"] $ do
  whenJust listCfg.heading \heading -> do
    div_ [class_ "flex justify-between"] do
      h3_ [class_ "text-xl text-slate-700 flex gap-1 place-items-center"] $ heading.pageTitle
      fromMaybe "" heading.rightComponent
    fromMaybe "" heading.subSection
  whenJust listCfg.tabsFilter \tabsFilter -> do
    let uri = deleteParam "filter" listCfg.currentURL
    div_ [class_ "py-2 px-2 space-x-6 border-b border-slate-20 text-sm font-light", hxBoost_ "true", role_ "tablist"] $ forM_ tabsFilter.options \opt ->
      a_
        [ class_ $ "inline-block py-2  " <> if opt.name == tabsFilter.current then " font-bold text-black " else ""
        , role_ "tab"
        , href_ $ uri <> "&filter=" <> escapedQueryPartial opt.name
        ]
        $ do
          span_ $ toHtml opt.name
          whenJust opt.count \countV -> span_ [class_ "absolute top-[1px] -right-[5px] text-white text-xs font-medium rounded-full px-1 bg-red-500"] $ show countV
  itemsList_ listCfg items


itemsList_ :: ToHtml a => ItemsListCfg -> V.Vector a -> Html ()
itemsList_ listCfg items =
  div_ [class_ "card-round overflow-hidden", id_ "anomalyListBelowTab", hxGet_ listCfg.currentURL, hxSwap_ "outerHTML", hxTrigger_ "refreshMain"] do
    form_ [class_ "flex flex-col bg-white divide-y ", id_ listCfg.elemID] do
      let currentURL' = deleteParam "sort" listCfg.currentURL
      let sortMenu =
            [ ("First Seen", "First time the issue occured", "first_seen")
            , ("Last Seen", "Last time the issue occured", "last_seen")
            , ("Events", "Number of events", "events")
            ]
              :: [(Text, Text, Text)]
      div_
        [class_ "flex py-3 gap-8 items-center  bg-gray-50"]
        do
          div_ [class_ "h-4 flex space-x-3 w-8 items-center"] do
            span_ [class_ " w-2 h-full"] ""
            input_
              [ term "aria-label" "Select Issue"
              , type_ "checkbox"
              , class_ "checkbox  checkbox-md checked:checkbox-primary"
              , [__| on click set .bulkactionItemCheckbox.checked to my.checked |]
              ]
          div_ [class_ " grow flex flex-row gap-2"] do
            forM_ listCfg.bulkActions \blkA -> button_ [class_ "btn btn-sm  border-black hover:shadow-2xl btn-disabled group-has-[.bulkactionItemCheckbox:checked]/grid:!btn-outline group-has-[.bulkactionItemCheckbox:checked]/grid:!pointer-events-auto  ", hxPost_ blkA.uri, hxSwap_ "none"] do
              whenJust blkA.icon \icon -> faSprite_ icon "solid" "h-4 w-4 inline-block"
              span_ (toHtml blkA.title)

            whenJust listCfg.search \search -> do
              div_ [hxGet_ currentURL', hxTarget_ "#rowsContainer", hxSwap_ "innerHTML", id_ "searchThing", hxVals_ "js:{search:getSearchVal()}"] do
                label_ [class_ "input input-sm input-bordered flex  overflow-hidden items-center gap-2", [__|on click halt|]] do
                  case search.viaQueryParam of
                    Just param -> do
                      input_ [type_ "text", class_ "grow", id_ "search_box", placeholder_ "Search"]
                      button_ [class_ "bg-blue-500 w-max text-white px-2 translate-x-2 rounded", [__|on click send click() to #searchThing|]] do
                        faSprite_ "magnifying-glass" "regular" "w-5 h-5 p-1"
                    Nothing -> do
                      input_ [type_ "text", class_ "grow", placeholder_ "Search", [__| on input show .itemsListItem in #itemsListPage when its textContent.toLowerCase() contains my value.toLowerCase() |]]
                      faSprite_ "magnifying-glass" "regular" "w-4 h-4 opacity-70"

          whenJust listCfg.sort \sortCfg -> do
            let currentSortTitle = maybe "First Seen" fst3 $ find (\(_, _, identifier) -> identifier == sortCfg.current) sortMenu
            div_ [class_ "dropdown dropdown-end inline-block"] do
              a_ [class_ "btn btn-sm btn-outline border-black hover:shadow-2xl", tabindex_ "0"] do
                faSprite_ "sort" "solid" "h-4 w-4"
                span_ $ toHtml currentSortTitle
              div_ [id_ "sortMenuDiv", hxBoost_ "true", class_ "dropdown-content bg-base-100 p-1 text-sm border border-black-30 z-50 mt-2 w-72 origin-top-right rounded-md shadow-lg ", tabindex_ "0"] do
                sortMenu & mapM_ \(title, desc, identifier) -> do
                  let isActive = sortCfg.current == identifier || (sortCfg.current == "" && identifier == "first_seen")
                  a_
                    [ class_ $ "block flex flex-row px-3 py-2 hover:bg-blue-50 rounded-md cursor-pointer " <> (if isActive then " text-blue-800 " else "")
                    , href_ $ currentURL' <> "&sort=" <> identifier
                    , hxIndicator_ "#sortLoader"
                    ]
                    do
                      div_ [class_ "flex flex-col items-center justify-center px-3"]
                        $ if isActive then faSprite_ "icon-checkmark4" "solid" "w-4 h-5" else div_ [class_ "w-4 h-5"] ""
                      div_ [class_ "grow space-y-1"] do
                        span_ [class_ "block text-lg"] $ toHtml title
                        span_ [class_ "block "] $ toHtml desc

          div_ [class_ "flex justify-center font-base w-60 content-between gap-14"] do
            span_ "GRAPH"
            div_ [class_ " space-x-2 font-base text-sm"] $ (a_ [class_ "cursor-pointer"] "24h" >> a_ [class_ "cursor-pointer font-bold text-base"] "14d")
          div_ [class_ "w-36 flex items-center justify-center"] $ span_ [class_ "font-base"] "EVENTS"
          div_ [class_ "p-12 fixed rounded-lg shadow bg-white top-1/2 left-1/2 transform -translate-x-1/2 -translate-y-1/2 htmx-indicator loading loading-dots loading-md", id_ "sortLoader"] ""

      when (null items) $ whenJust listCfg.zeroState \zeroState -> section_ [class_ "mx-auto w-max p-5 sm:py-10 sm:px-16 items-center flex my-10 gap-16"] do
        div_ [] $ faSprite_ zeroState.icon "solid" "h-24 w-24"
        div_ [class_ "flex flex-col gap-2"] do
          h2_ [class_ "text-2xl font-bold"] $ toHtml zeroState.title
          p_ $ toHtml zeroState.description
          case zeroState.destination of
            Right destination -> a_ [href_ destination, class_ "w-max btn btn-indigo -ml-1 text-md"] $ toHtml zeroState.actionText
            Left labelId -> label_ [Lucid.for_ labelId, class_ "w-max btn btn-indigo -ml-1 text-md"] $ toHtml zeroState.actionText
      div_ [id_ "rowsContainer"] do
        itemRows_ listCfg.nextFetchUrl items

    script_
      [text|
       function getSearchVal() {
        var searchVal = document.getElementById("search_box").value;
        return searchVal;
       }
    |]


type role ItemsRows representational
data ItemsRows a = ItemsRows (Maybe Text) (V.Vector a) -- Text represents nextFetchUrl


instance ToHtml a => ToHtml (ItemsRows a) where
  {-# INLINE toHtml #-}
  toHtml (ItemsRows nextFetchUrl items) = toHtmlRaw $ itemRows_ nextFetchUrl items
  {-# INLINE toHtmlRaw #-}
  toHtmlRaw (ItemsRows nextFetchUrl items) = toHtmlRaw $ itemRows_ nextFetchUrl items


itemRows_ :: (Monad m, ToHtml a) => Maybe Text -> V.Vector a -> HtmlT m ()
itemRows_ nextFetchUrl items = do
  mapM_ (toHtml) items
  whenJust nextFetchUrl \url ->
    when (length items > 10)
      $ a_ [class_ "cursor-pointer block p-1 blue-800 bg-blue-100 hover:bg-blue-200 text-center", hxTrigger_ "click", hxSwap_ "outerHTML", hxGet_ url] do
        span_ [class_ "htmx-indicator loading loading-dots loading-md"] "" >> "LOAD MORE"
