module Pkg.Components.ItemsList (
  itemsList_,
  itemsPage_,
  itemRows_,
  ItemsRows (..),
  ItemsPage (..),
  BulkAction (..),
  ZeroState (..),
  ItemsListCfg (..),
  SortCfg (..),
  SearchCfg (..),
  TimelineSteps (..),
  TimelineStep (..),
)
where

import Data.Foldable.WithIndex (iforM_)
import Data.Time (UTCTime)
import Data.Tuple.Extra (fst3)
import Data.Vector qualified as V
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Tests.Testing qualified as Testing
import Pages.Components (emptyState_)
import Relude
import Utils (deleteParam, faSprite_)


data ItemsListCfg = ItemsListCfg
  { currentURL :: Text
  , sort :: Maybe SortCfg
  , filter :: Maybe Text
  , projectId :: Projects.ProjectId
  , currTime :: UTCTime
  , elemID :: Text
  , nextFetchUrl :: Maybe Text
  , zeroState :: Maybe ZeroState
  , bulkActions :: [BulkAction]
  , search :: Maybe SearchCfg
  , heading :: Maybe (Html ())
  }


newtype SearchCfg = SearchCfg
  {viaQueryParam :: Maybe Text}


data BulkAction = BulkAction
  { icon :: Maybe Text
  , title :: Text
  , uri :: Text
  }


newtype SortCfg = SortCfg
  { current :: Text
  }


data ZeroState = ZeroState
  { icon :: Text
  , title :: Text
  , description :: Text
  , actionText :: Text
  , destination :: Either Text Text
  }


type role ItemsPage representational


data ItemsPage a = ItemsPage ItemsListCfg (V.Vector a)


instance ToHtml a => ToHtml (ItemsPage a) where
  toHtml (ItemsPage cfg items) = toHtmlRaw $ itemsPage_ cfg items
  toHtmlRaw (ItemsPage cfg items) = toHtmlRaw $ itemsPage_ cfg items


itemsPage_ :: ToHtml a => ItemsListCfg -> V.Vector a -> Html ()
itemsPage_ listCfg items = div_ [class_ "w-full mx-auto px-6 pt-4 space-y-4 pb-16 overflow-y-scroll h-full", id_ "itemsListPage"] $ do
  itemsList_ listCfg items


itemsList_ :: ToHtml a => ItemsListCfg -> V.Vector a -> Html ()
itemsList_ listCfg items = do
  let currentURL' = deleteParam "sort" listCfg.currentURL
  whenJust listCfg.search \search -> do
    label_ [class_ "input input-sm flex h-10 bg-fillWeak border border-strokeStrong shadow-none overflow-hidden items-center gap-2"] do
      faSprite_ "magnifying-glass" "regular" "w-4 h-4 opacity-70"
      case search.viaQueryParam of
        Just param ->
          input_
            [ type_ "text"
            , class_ "grow"
            , name_ "search"
            , id_ "search_box"
            , placeholder_ "Search"
            , hxTrigger_ "keyup changed delay:500ms"
            , hxGet_ currentURL'
            , hxTarget_ "#rowsContainer"
            , hxSwap_ "innerHTML"
            , id_ "searchThing"
            , hxIndicator_ "#searchIndicator"
            ]
        Nothing -> do
          input_
            [ type_ "text"
            , class_ "grow"
            , placeholder_ "Search"
            , [__| on input show .itemsListItem in #itemsListPage when its textContent.toLowerCase() contains my value.toLowerCase() |]
            ]
  div_ [class_ "grid card-round overflow-hidden  my-0 group/grid", id_ "anomalyListBelowTab", hxGet_ listCfg.currentURL, hxSwap_ "outerHTML", hxTrigger_ "refreshMain"] do
    form_ [class_ "flex flex-col divide-y w-full ", id_ listCfg.elemID, onkeydown_ "return event.key != 'Enter';"] do
      let sortMenu =
            [ ("First Seen", "First time the issue occured", "first_seen")
            , ("Last Seen", "Last time the issue occured", "last_seen")
            , ("Events", "Number of events", "events")
            ]
              :: [(Text, Text, Text)]

      div_ [class_ "flex py-3 gap-8 items-center  bg-gray-50"] do
        div_ [class_ "h-4 flex space-x-3 w-8 items-center"] do
          span_ [class_ " w-2 h-full"] ""
          input_
            [ term "aria-label" "Select Issue"
            , type_ "checkbox"
            , class_ "checkbox h-6 w-6 checked:checkbox-primary"
            , [__| on click set .bulkactionItemCheckbox.checked to my.checked |]
            ]

        div_ [class_ " grow flex flex-row gap-2"] do
          forM_ listCfg.bulkActions \blkA -> button_
            [ class_ "btn btn-sm btn-disabled group-has-[.bulkactionItemCheckbox:checked]/grid:!btn-primary group-has-[.bulkactionItemCheckbox:checked]/grid:!pointer-events-auto  "
            , hxPost_ blkA.uri
            , hxSwap_ "none"
            ]
            do
              whenJust blkA.icon \icon -> faSprite_ icon "solid" "h-4 w-4 inline-block"
              span_ (toHtml blkA.title)

          whenJust listCfg.sort \sortCfg -> do
            let currentSortTitle = maybe "First Seen" fst3 $ find (\(_, _, identifier) -> identifier == sortCfg.current) sortMenu
            div_ [class_ "dropdown dropdown-end inline-block"] do
              a_ [class_ "btn btn-sm shadow-none text-sm font-medium bg-slate-100 border text-slate-600 border-slate-300 ", tabindex_ "0"] do
                span_ $ toHtml currentSortTitle
                faSprite_ "sort" "regular" "h-4 w-4 stroke-slate-600"

              div_
                [ id_ "sortMenuDiv"
                , hxBoost_ "true"
                , class_ "dropdown-content bg-base-100 p-1 text-sm border border-black-30 z-50 mt-2 w-72 origin-top-right rounded-md shadow-lg "
                , tabindex_ "0"
                ]
                do
                  sortMenu & mapM_ \(title, desc, identifier) -> do
                    let isActive = sortCfg.current == identifier || (sortCfg.current == "" && identifier == "first_seen")
                    a_
                      [ class_ $ "block flex flex-row px-3 py-2 hover:bg-blue-50 rounded-md cursor-pointer " <> (if isActive then " text-blue-800 " else "")
                      , href_ $ currentURL' <> "&sort=" <> identifier
                      , hxIndicator_ "#sortLoader"
                      ]
                      do
                        div_ [class_ "flex flex-col items-center justify-center px-3"] $
                          if isActive then faSprite_ "icon-checkmark4" "solid" "w-4 h-5" else div_ [class_ "w-4 h-5"] ""
                        div_ [class_ "grow space-y-1"] do
                          span_ [class_ "block text-lg"] $ toHtml title
                          span_ [class_ "block "] $ toHtml desc
        div_ [class_ "w-36 flex items-center justify-center"] $ span_ [class_ "font-base text-sm"] "Events"
        div_ [class_ "flex justify-center w-60 items-center text-sm  content-between gap-2"] do
          span_ [] "Chart"
          div_ [class_ "rounded-lg border grid grid-cols-2 w-max h-7 bg-slate-200 overflow-hidden"] do
            let selectedFilter = fromMaybe "14d" listCfg.filter -- Default to "14d" if Nothing
            a_ [class_ $ "cursor-pointer px-1.5 flex items-center text-xs h-full rounded " <> (if selectedFilter == "24h" then "bg-white" else ""), href_ $ currentURL' <> "&since=24h"] "24h"
            a_ [class_ $ "cursor-pointer px-1.5 flex items-center text-xs h-full rounded " <> (if selectedFilter == "14d" then "bg-white" else ""), href_ $ currentURL' <> "&since=14d"] "14d"
        div_
          [ class_ "p-12 fixed rounded-lg shadow bg-base-100 top-1/2 left-1/2 transform -translate-x-1/2 -translate-y-1/2 htmx-indicator loading loading-dots loading-md"
          , id_ "sortLoader"
          ]
          ""

      when (null items) $ whenJust listCfg.zeroState \zeroState -> do
        let url = case zeroState.destination of
              Left labelId -> labelId
              Right destination -> destination
        emptyState_ zeroState.title zeroState.description (Just url) zeroState.actionText

      div_ [class_ "w-full flex flex-col"] do
        span_ [id_ "searchIndicator", class_ "htmx-indicator loading loading-sm loading-dots mx-auto"] ""
        div_ [id_ "rowsContainer", class_ "divide-y"] $ itemRows_ listCfg.nextFetchUrl items


type role ItemsRows representational


data ItemsRows a = ItemsRows (Maybe Text) (V.Vector a) -- Text represents nextFetchUrl


instance ToHtml a => ToHtml (ItemsRows a) where
  toHtml (ItemsRows nextFetchUrl items) = toHtmlRaw $ itemRows_ nextFetchUrl items
  toHtmlRaw (ItemsRows nextFetchUrl items) = toHtmlRaw $ itemRows_ nextFetchUrl items


itemRows_ :: (Monad m, ToHtml a) => Maybe Text -> V.Vector a -> HtmlT m ()
itemRows_ nextFetchUrl items = do
  mapM_ toHtml items
  whenJust nextFetchUrl \url ->
    when (length items > 9) $
      a_
        [ class_ "cursor-pointer flex justify-center items-center p-1 blue-800 bg-blue-100 hover:bg-blue-200 text-center"
        , hxTrigger_ "click, intersect once"
        , hxSwap_ "outerHTML"
        , hxGet_ url
        , hxIndicator_ "#rowsIndicator"
        ]
        do
          "Load more"
          span_ [id_ "rowsIndicator", class_ "ml-2 htmx-indicator loading loading-dots loading-md"] ""


---------------------------------------------------------------------
--   TimelineSteps
---------------------------------------------------------------------

-- TimelineSteps is used to render numbered timeline sections
-- where each section has a number a title and content
data TimelineSteps = TimelineSteps [TimelineStep] (Maybe Testing.Collection)


data TimelineStep = TimelineStep
  { title :: Text
  , content :: Html ()
  }


instance ToHtml TimelineSteps where
  toHtmlRaw = toHtml
  toHtml (TimelineSteps steps colM) = toHtml $ timelineSteps_ steps colM


timelineSteps_ :: [TimelineStep] -> Maybe Testing.Collection -> Html ()
timelineSteps_ steps colM =
  ul_ [class_ "timeline timeline-snap-icon timeline-vertical timeline-compact pb-8"] $ do
    iforM_ steps $ \idx step -> li_ [class_ "group/tm"] $ do
      when (idx > 0) $ hr_ [style_ "width:2px"]
      div_ [class_ "timeline-middle "] do
        span_
          [class_ "rounded-full bg-primary text-base-100 h-7 w-7 flex items-center justify-center "]
          (toHtml $ show $ idx + 1)
      div_ [class_ "timeline-end space-y-5 w-full"] $ do
        div_ [class_ "flex items-center justify-between"] do
          label_ [class_ "text-lg flex gap-2 items-center pt-1"] $ do
            span_ [class_ "font-medium ml-2 text-gray-900"] (toHtml step.title)
            input_ ([type_ "checkbox", class_ "hidden tm-toggle"] <> [checked_ | idx == 0])
            faSprite_ "chevron-up" "regular" "h-4 rounded-full rotate-180  bg-fillWeak  text-textStrong p-1 w-4 text-white group-has-[.tm-toggle:checked]/tm:rotate-0"
          when (idx == 0) $ do
            whenJust colM $ \col ->
              div_ [class_ "flex items-center gap-6"] do
                label_ [class_ "relative inline-flex items-center cursor-pointer space-x-1"] do
                  input_ [type_ "checkbox", class_ "checkbox checkbox-sm rounded editormode", id_ "test-code-toggle", onchange_ "codeToggle(event)"] >> span_ [class_ "text-sm  text-textWeak font-medium"] "Code editor"
                button_
                  [ class_ "flex items-center gap-1 font-medium rounded-lg text-brand underline"
                  , hxPatch_ $ "/p/" <> col.projectId.toText <> "/monitors/" <> col.id.toText
                  , hxParams_ "stepsData"
                  , hxExt_ "json-enc"
                  , hxVals_ "js:{stepsData: saveStepData()}"
                  , hxTarget_ "#step-results-parent"
                  , hxSwap_ "innerHTML"
                  , hxIndicator_ "#step-results-indicator"
                  ]
                  "Run all"

        div_ [class_ "pl-2 pb-8 space-y-3 hidden group-has-[.tm-toggle:checked]/tm:block"] step.content
      when (idx < (length steps - 1)) $ hr_ [style_ "width:2px"]
