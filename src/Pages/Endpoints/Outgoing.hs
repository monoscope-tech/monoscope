module Pages.Endpoints.Outgoing (outgoingGetH) where

import Data.Default (def)
import Data.Text qualified as T
import Data.Tuple.Extra (fst3)
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Lucid
import Lucid.Htmx (hxBoost_, hxGet_, hxIndicator_, hxSwap_, hxTrigger_)
import Lucid.Hyperscript (__)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.Anomalies.AnomalyList qualified as AnomalyList
import Pages.BodyWrapper (BWConfig (currProject, pageTitle, sessM), bodyWrapper)
import PyF qualified
import Relude hiding (ask, asks)
import System.Types
import Utils (faSprite_, mIcon_)


outgoingGetH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
outgoingGetH pid sortM = do
  (sess, project) <- Sessions.sessionAndProject pid
  hostsAndEvents <- dbtToEff $ Endpoints.dependenciesAndEventsCount pid (fromMaybe "events" sortM)
  let bwconf =
        def
          { sessM = Just sess.persistentSession
          , currProject = Just project
          , pageTitle = "Dependencies"
          }
  addRespHeaders $ bodyWrapper bwconf $ outgoingPage pid (fromMaybe "events" sortM) hostsAndEvents


sortOptions :: [(Text, Text, Text)]
sortOptions =
  [ ("First Seen", "First time the issue occured", "first_seen")
  , ("Last Seen", "Last time the issue occured", "last_seen")
  , ("Events", "Number of events", "events")
  ]


outgoingPage :: Projects.ProjectId -> Text -> V.Vector Endpoints.HostEvents -> Html ()
outgoingPage pid sortV hostsEvents = do
  div_ [class_ "w-full mx-auto px-16 pt-10 pb-24 overflow-y-scroll h-full"] $ do
    h3_ [class_ "text-xl text-slate-700 flex gap-1 place-items-center mb-10"] "Outbound Integrations"
    div_ [class_ "grid grid-cols-5 card-round", id_ "anomalyListBelowTab", hxTrigger_ "refreshMain"] $ outgoingList' pid sortV hostsEvents


outgoingList' :: Projects.ProjectId -> Text -> V.Vector Endpoints.HostEvents -> Html ()
outgoingList' pid sortV hostsEvents = form_ [class_ "col-span-5 bg-white divide-y ", id_ "anomalyListForm"] $ do
  div_ [class_ "flex py-3 gap-8 items-center  bg-gray-50"] do
    div_ [class_ "h-4 flex space-x-3 w-8"] do
      a_ [class_ " w-2 h-full"] "" >> input_ [term "aria-label" "Select Issue", type_ "checkbox"]
    div_ [class_ " grow flex flex-row gap-2"] do
      button_ [class_ "btn btn-sm btn-outline border-black hover:shadow-2xl", hxSwap_ "none"] "HOST"
    div_ [class_ "relative inline-block"] do
      let currentSortTitle = maybe "Events" fst3 $ find (\(_, _, identifier) -> identifier == sortV) sortOptions
      a_ [class_ "btn-sm bg-transparent border-black hover:shadow-2xl space-x-2 cursor-pointer", [__|on click toggle .hidden on #sortMenuDiv |]]
        $ mIcon_ "sort" "h-4 w-4"
        >> (span_ $ toHtml currentSortTitle)
      div_ [id_ "sortMenuDiv", hxBoost_ "true", class_ "p-1 hidden text-sm border border-black-30 absolute right-0 z-10 mt-2 w-72 origin-top-right rounded-md bg-white shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none", tabindex_ "-1"] do
        sortOptions & mapM_ \(title, desc, identifier) -> do
          let isActive = sortV == identifier
          a_
            [ class_ $ "block flex flex-row px-3 py-2 hover:bg-blue-50 rounded-md cursor-pointer " <> (if isActive then " text-blue-800 " else "")
            , href_ $ "/p/" <> pid.toText <> "/outgoing?sort=" <> identifier
            , hxIndicator_ "#sortLoading"
            ]
            do
              div_ [class_ "flex flex-col items-center justify-center px-3"]
                $ if isActive then mIcon_ "checkmark4" "w-4 h-5" else mIcon_ "" "w-4 h-5"
              div_ [class_ "grow space-y-1"] do
                span_ [class_ "block text-lg"] $ toHtml title
                span_ [class_ "block "] $ toHtml desc
    div_ [id_ "sortLoading", class_ "htmx-indicator fixed top-1/2 left-1/2 -translate-1/2 rounded-lg bg-white shadow p-10 border"]
      $ span_ [class_ "loading loading-dots loading-md"] ""

    div_ [class_ "flex justify-center font-base w-60 content-between gap-14"] do
      span_ "GRAPH"
      div_ [class_ " space-x-2 font-base text-sm"] $ do
        a_ [class_ "cursor-pointer"] "24h"
        a_ [class_ "cursor-pointer font-bold text-base"] "14d"
    div_ [class_ "w-36 flex items-center justify-center"] $ span_ [class_ "font-base"] "EVENTS"
  div_ [class_ "w-full flex flex-row p-3"] $ do
    div_ [class_ "relative flex w-full bg-white py-2 px-3 border-solid border border-gray-200 h-10"] $ do
      faSprite_ "magnifying-glass" "regular" "h-5 w-5"
      input_
        [ type_ "text"
        , [__| on input show .endpoint_item in #endpoints_container when its textContent.toLowerCase() contains my value.toLowerCase() |]
        , class_ "dataTable-search w-full h-full p-2 text-gray-500 font-normal focus:outline-none"
        , placeholder_ "Search endpoints..."
        ]
  when (null hostsEvents) $ section_ [class_ "mx-auto w-max p-5 sm:py-10 sm:px-16 items-center flex my-10 gap-16"] do
    div_ [] do
      faSprite_ "empty-set" "solid" "h-24 w-24"
    div_ [class_ "flex flex-col gap-2"] do
      h2_ [class_ "text-2xl font-bold"] "No Outgoing Request Monitored."
      p_ "You're currently not monitoring your outbound integrations."
      a_ [href_ $ "/p/" <> pid.toText <> "/integration_guides#outgoing-request-monitoring", class_ "w-max btn btn-indigo -ml-1 text-md"] "See monitoring guide"
  renderOutgoing pid hostsEvents


renderOutgoing :: Projects.ProjectId -> V.Vector Endpoints.HostEvents -> Html ()
renderOutgoing pid hostsEvents = do
  div_ [class_ "flex flex-col"] do
    forM_ hostsEvents $ \host -> do
      div_ [class_ "flex py-4 gap-8 items-center endpoint_item "] do
        div_ [class_ "h-4 flex space-x-3 w-8 "] do
          a_ [class_ " w-2 h-full"] ""
          input_ [term "aria-label" "Select Issue", class_ "endpoint_anomaly_input", type_ "checkbox"]
        div_ [class_ "space-y-3 grow"] do
          div_ [class_ "space-x-3"] do
            a_ [class_ "inline-block font-bold space-x-2"] $ do
              a_ [href_ $ "/p/" <> pid.toText <> "/endpoints?host=" <> host.host, class_ "text-blue-500 hover:text-slate-600"] $ toHtml (T.replace "http://" "" $ T.replace "https://" "" host.host)
              a_ [href_ $ "/p/" <> pid.toText <> "/log_explorer?query=host%3D%3D" <> "\"" <> host.host <> "\"", class_ "text-blue-500 hover:text-slate-600"] $ "View logs"
        div_ [class_ "flex items-center justify-center "]
          $ div_
            [ class_ "w-56 h-12 px-3"
            , hxGet_ $ "/charts_html?pid=" <> pid.toText <> "&since=14D&query_raw=" <> AnomalyList.escapedQueryPartial [PyF.fmt|host=="{host.host}" | timechart [1d]|]
            , hxTrigger_ "intersect once"
            , hxSwap_ "innerHTML"
            ]
            ""
        div_ [class_ "w-36 flex items-center justify-center"] $ span_ [class_ "tabular-nums text-xl", term "data-tippy-content" "Events for this Anomaly in the last 14days"] $ toHtml (show host.eventCount)
