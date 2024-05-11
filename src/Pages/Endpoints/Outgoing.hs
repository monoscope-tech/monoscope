module Pages.Endpoints.Outgoing (outgoingGetH) where

import Data.Default (def)
import Data.Text qualified as T
import Data.Tuple.Extra (fst3)
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Lucid
import Lucid.Htmx (hxBoost_, hxGet_, hxSwap_, hxTrigger_)
import Lucid.Hyperscript (__)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.Anomalies.AnomalyList qualified as AnomalyList
import Pages.BodyWrapper (BWConfig (currProject, pageTitle, sessM), bodyWrapper)
import PyF qualified
import Relude hiding (ask, asks)
import System.Types (ATAuthCtx)
import Utils (faIcon_, mIcon_)


outgoingGetH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (Html ())
outgoingGetH pid sortM = do
  (sess, project) <- Sessions.sessionAndProject pid
  hostsAndEvents <- dbtToEff $ Endpoints.dependenciesAndEventsCount pid (fromMaybe "events" sortM)
  let bwconf =
        def
          { sessM = Just sess.persistentSession
          , currProject = Just project
          , pageTitle = "Dependencies"
          }
  pure $ bodyWrapper bwconf $ outgoingPage pid (fromMaybe "events" sortM) hostsAndEvents


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

    div_ [class_ "w-full card-round"] $ do
      -- Labels for each column`
      div_ [class_ "col-span-4 bg-white divide-y"] $ do
        div_ [class_ "col-span-4 py-2 space-x-4 border-b border-slate-20 pt-4 text-sm font-light flex justify-between items-center px-2 bg-gray-50 font-base "] $ do
          div_ [class_ ""] $ do
            div_ [class_ "w-36 flex items-center justify-center"] $ span_ [class_ "font-base font-medium text-black"] "HOST"
          div_ [class_ "flex item-center"] $ do
            div_ [class_ "relative inline-block"] $ do
              let currentSortTitle = maybe "Events" fst3 $ find (\(_, _, identifier) -> identifier == sortV) sortOptions
              a_ [class_ "btn-sm bg-transparent cursor-pointer border-black hover:shadow-2xl space-x-2", [__|on click toggle .hidden on #sortMenuDiv |]] do
                mIcon_ "sort" "h-4 w-4"
                span_ $ toHtml currentSortTitle
              div_ [id_ "sortMenuDiv", hxBoost_ "true", class_ "p-1 hidden text-sm border border-black-30 absolute right-0 z-10 mt-2 w-72 origin-top-right rounded-md bg-white shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none", tabindex_ "-1"] do
                sortOptions & mapM_ \(title, desc, identifier) -> do
                  let isActive = sortV == identifier
                  a_
                    [ class_ $ "block flex flex-row px-3 py-2 hover:bg-blue-50 rounded-md cursor-pointer " <> (if isActive then " text-blue-800 " else "")
                    , href_ $ "/p/" <> pid.toText <> "/outgoing?sort=" <> identifier
                    ]
                    do
                      div_ [class_ "flex flex-col items-center justify-center px-3"] do
                        if isActive then mIcon_ "checkmark4" "w-4 h-5" else mIcon_ "" "w-4 h-5"
                      div_ [class_ "grow space-y-1"] do
                        span_ [class_ "block text-lg"] $ toHtml title
                        span_ [class_ "block "] $ toHtml desc
            div_ [class_ "flex justify-center font-base w-60 content-between gap-14 font-medium text-black"] do
              span_ "GRAPH"
            div_ [class_ "w-36 flex items-center justify-center"] $ span_ [class_ "font-base font-medium text-black"] "EVENTS"
      div_ [class_ "w-full bg-white border-b border-slate-20"] $ do
        div_ [class_ "w-full flex flex-row p-3"] $ do
          div_ [class_ "relative flex w-full bg-white py-2 px-3 border-solid border border-gray-200 h-10"] $ do
            faIcon_ "fa-magnifying-glass" "fa-light fa-magnifying-glass" "h-5 w-5"
            input_
              [ type_ "text"
              , [__| on input show .endpoint_item in #endpoints_container when its textContent.toLowerCase() contains my value.toLowerCase() |]
              , class_ "dataTable-search w-full h-full p-2 text-gray-500 font-normal focus:outline-none"
              , placeholder_ "Search dependencies..."
              ]

      -- Data rows
      div_ [class_ "col-span-4 bg-white divide-y"] $ do
        forM_ hostsEvents $ \host -> do
          div_ [class_ "border-b border-gray-200 outgoing_item flex justify-between px-4 py-2 align-center mt-8"] $ do
            div_ [class_ "flex-1 w-48  justify-center"] $ a_ [href_ $ "/p/" <> pid.toText <> "/endpoints?host=" <> host.host, class_ "text-blue-500 hover:text-slate-600"] $ toHtml (T.replace "http://" "" $ T.replace "https://" "" host.host)
            div_ [class_ " flex-1 w-32 justify-center  text-center"] $ do
              a_ [href_ $ "/p/" <> pid.toText <> "/log_explorer?query=host%3D%3D" <> "\"" <> host.host <> "\"", class_ "text-blue-500 hover:text-slate-600"] $ "View logs"
            div_ [class_ "w-64 mb-4 justify-center flex-1 items-center"] $ do
              div_
                [ class_ "w-56 h-12 px-3"
                , hxGet_ $ "/charts_html?pid=" <> pid.toText <> "&since=14D&query_raw=" <> AnomalyList.escapedQueryPartial [PyF.fmt|host=="{host.host}" | timechart [1d]|]
                , hxTrigger_ "intersect once"
                , hxSwap_ "innerHTML"
                ]
                ""
            div_ [class_ " w-24 text-center"] $ toHtml (show host.eventCount)

      when (null hostsEvents) $ section_ [class_ "mx-auto w-max p-5 sm:py-10 sm:px-16 items-center flex my-10 gap-16"] do
        div_ [] do
          faIcon_ "fa fa-solid fa-empty-set" "fa-solid fa-empty-set" "h-24 w-24"
        div_ [class_ "flex flex-col gap-2"] do
          h2_ [class_ "text-2xl font-bold"] "No Outgoing Request Monitored."
          p_ "You're currently not monitoring your outbound integrations."
          a_ [href_ $ "/p/" <> pid.toText <> "/integration_guides#outgoing-request-monitoring", class_ "w-max btn btn-indigo -ml-1 text-md"] "See monitoring guide"
