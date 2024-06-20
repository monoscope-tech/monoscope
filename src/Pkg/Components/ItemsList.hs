module Pkg.Components.ItemsList (
  itemsList_,
  itemsPage_,
  itemRows_,
  ZeroState (..),
  Heading (..),
  ItemsListCfg (..),
  TabFilter (..),
  TabFilterOpt (..),
  SortCfg (..),
)
where

import Data.Time (UTCTime)
import Data.Tuple.Extra (fst3)
import Data.Vector qualified as V
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Relude
import Utils (deleteParam, escapedQueryPartial, faSprite_, mIcon_)


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
  , destination :: Text
  }


itemsPage_ :: ItemsListCfg -> V.Vector a -> (ItemsListCfg -> a -> Html ()) -> Html ()
itemsPage_ listCfg items renderItem = div_ [class_ "w-full mx-auto px-16 pt-10 pb-24 overflow-y-scroll h-full space-y-6"] $ do
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
  itemsList_ listCfg items renderItem


itemsList_ :: ItemsListCfg -> V.Vector a -> (ItemsListCfg -> a -> Html ()) -> Html ()
itemsList_ listCfg items renderItem = div_ [class_ "grid grid-cols-5 card-round", id_ "anomalyListBelowTab", hxGet_ listCfg.currentURL, hxSwap_ "outerHTML", hxTrigger_ "refreshMain"] do
  form_ [class_ "col-span-5 bg-white divide-y ", id_ listCfg.elemID] do
    let bulkActionBase = "/p/" <> listCfg.projectId.toText <> "/anomalies/bulk_actions"
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
        div_ [class_ "h-4 flex space-x-3 w-8"] do
          a_ [class_ " w-2 h-full"] ""
            >> input_
              [ term "aria-label" "Select Issue"
              , type_ "checkbox"
              , [__| on click set .bulkactionItemCheckbox.checked to my.checked |]
              ]
        div_ [class_ " grow flex flex-row gap-2"] do
          button_ [class_ "btn btn-sm btn-outline border-black hover:shadow-2xl", hxPost_ $ bulkActionBase <> "/acknowlege", hxSwap_ "none"] "✓ acknowlege"
          button_ [class_ "btn btn-sm btn-outline space-x-1 border-black hover:shadow-2xl", hxPost_ $ bulkActionBase <> "/archive", hxSwap_ "none"] do
            faSprite_ "inbox-full" "solid" "h-4 w-4 inline-block"
            span_ "archive"
        whenJust listCfg.sort \sortCfg -> do
          let currentSortTitle = maybe "First Seen" fst3 $ find (\(_, _, identifier) -> identifier == sortCfg.current) sortMenu
          div_ [class_ "relative inline-block"] do
            a_ [class_ "btn btn-sm btn-outline border-black hover:shadow-2xl space-x-2", [__|on click toggle .hidden on #sortMenuDiv |]] do
              mIcon_ "sort" "h-4 w-4"
              span_ $ toHtml currentSortTitle
            div_ [id_ "sortMenuDiv", hxBoost_ "true", class_ "p-1 hidden text-sm border border-black-30 absolute right-0 z-10 mt-2 w-72 origin-top-right rounded-md bg-white shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none", tabindex_ "-1"] do
              sortMenu & mapM_ \(title, desc, identifier) -> do
                let isActive = sortCfg.current == identifier || (sortCfg.current == "" && identifier == "first_seen")
                a_
                  [ class_ $ "block flex flex-row px-3 py-2 hover:bg-blue-50 rounded-md cursor-pointer " <> (if isActive then " text-blue-800 " else "")
                  , href_ $ currentURL' <> "&sort=" <> identifier
                  , hxIndicator_ "#sortLoader"
                  ]
                  do
                    div_ [class_ "flex flex-col items-center justify-center px-3"] do
                      if isActive then mIcon_ "checkmark4" "w-4 h-5" else mIcon_ "" "w-4 h-5"
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
        a_ [href_ zeroState.destination, class_ "w-max btn btn-indigo -ml-1 text-md"] $ toHtml zeroState.actionText
    itemRows_ listCfg.nextFetchUrl (renderItem listCfg) items


itemRows_ :: Maybe Text -> (a -> Html ()) -> V.Vector a -> Html ()
itemRows_ nextFetchUrl renderItem items = do
  mapM_ (renderItem) items
  whenJust nextFetchUrl \url ->
    when (length items > 10)
      $ a_ [class_ "cursor-pointer block p-1 blue-800 bg-blue-100 hover:bg-blue-200 text-center", hxTrigger_ "click", hxSwap_ "outerHTML", hxGet_ url] do
        span_ [class_ "htmx-indicator loading loading-dots loading-md"] "" >> "LOAD MORE"
