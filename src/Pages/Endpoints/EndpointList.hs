module Pages.Endpoints.EndpointList (endpointListGetH, renderEndpoint) where

import Data.Default (def)
import Data.Text (toLower)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Clock (getCurrentTime)
import Data.Tuple.Extra (fst3)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT (
  withPool,
 )
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Reader.Static (ask, asks)
import Fmt (commaizeF, fixedF, fmt, (+|), (|+))
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript.QuasiQuoter
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Models.Projects.Projects qualified as Projets
import Models.Users.Sessions qualified as Sessions
import Pages.Anomalies.AnomalyList qualified as AnomalyList
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Pages.Charts.Charts qualified as Charts
import Pages.NonMember
import Pages.Onboarding qualified as Onboarding
import PyF qualified
import Prelude hiding (ask, asks)
import Relude.Unsafe qualified as Unsafe
import System.Config
import System.Types
import Utils (deleteParam, faIcon_, faSprite_, mIcon_, textToBool, userIsProjectMember)


data ParamInput = ParamInput
  { currentURL :: Text
  , archived :: Bool
  , sort :: Text
  , ackd :: Bool
  }


endpointListGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (Html ())
endpointListGetH pid layoutM ackdM archivedM hostM projectHostM sortM hxRequestM hxBoostedM hxCurrentURL = do
  let ackd = maybe True textToBool ackdM
  let archived = maybe False textToBool archivedM

  -- TODO: temporary, to work with current logic
  appCtx <- ask @AuthContext
  let env = appCtx.config
  sess' <- Sessions.getSession
  let sess = Unsafe.fromJust sess'.persistentSession
  let currUserId = sess.userId

  isMember <- dbtToEff $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      (project, endpointStats, projHosts, inbox) <- dbtToEff do
        project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
        endpointStats <- case hostM of
          Just h -> Endpoints.dependencyEndpointsRequestStatsByProject pid h
          Nothing -> Endpoints.endpointRequestStatsByProject pid ackd archived projectHostM sortM
        projHosts <- Endpoints.getProjectHosts pid
        inbox <- Endpoints.countEndpointInbox pid
        pure (project, endpointStats, projHosts, inbox)
      let bwconf =
            (def :: BWConfig)
              { sessM = Just sess
              , currProject = project
              , pageTitle = "Endpoints"
              }
      let currentURL = "/p/" <> pid.toText <> "/endpoints?layout=" <> fromMaybe "false" layoutM <> "&ackd=" <> fromMaybe "true" ackdM <> "&archived=" <> fromMaybe "false" archivedM <> "&sort=" <> fromMaybe "event" sortM
      currTime <- liftIO getCurrentTime
      let paramInput =
            ParamInput
              { currentURL = currentURL
              , ackd = ackd
              , archived = archived
              , sort = fromMaybe "events" sortM
              }
      let elementBelowTabs =
            div_ [class_ "grid grid-cols-5", hxGet_ paramInput.currentURL, hxSwap_ "outerHTML", hxTrigger_ "refreshMain"]
              $ endpointList' paramInput currTime pid endpointStats inbox
      case (hxRequestM, hxBoostedM) of
        (Just "true", Just "false") -> pure elementBelowTabs
        (Just "true", Nothing) -> pure elementBelowTabs
        _ -> pure $ bodyWrapper bwconf $ endpointListPage paramInput pid currTime endpointStats projHosts hostM projectHostM inbox


endpointListPage :: ParamInput -> Projects.ProjectId -> UTCTime -> Vector Endpoints.EndpointRequestStats -> Vector Endpoints.Host -> Maybe Text -> Maybe Text -> Int -> Html ()
endpointListPage paramInput pid currTime endpoints hosts hostM pHostM inbox_count = div_ [class_ "w-full mx-auto px-16 pt-10 pb-24 overflow-y-scroll h-full"] $ do
  h3_ [class_ "text-xl text-slate-700 flex gap-1 place-items-center"] do
    case hostM of
      Just h -> do
        span_ [] "Endpoints for dependency: "
        span_ [class_ "text-blue-500 font-bold"] $ toHtml h
      Nothing -> "Endpoints"
  when (isNothing hostM) $ div_ [class_ "mt-8 flex  items-center gap-2"] do
    span_ [class_ "font-bold"] "Host "
    div_ [class_ "relative flex items-center border rounded focus:ring-2 focus:ring-blue-200 active:ring-2 active:ring-blue-200", style_ "width:220px"] do
      button_
        [ [__| on click toggle .hidden on #hosts_container |]
        , data_ "current" "1"
        , class_ "w-full flex text-slate-600 justify_between items-center cursor-pointer px-2 py-1"
        ]
        do
          span_ [class_ "ml-1 text-sm text-slate-600"] $ toHtml $ fromMaybe "Select host" pHostM
      faIcon_ "fa-chevron-down" "fa-light fa-chevron-down" "h-4 w-4"
      div_ [id_ "hosts_container", class_ "absolute hidden bg-white border shadow w-full overflow-y-auto", style_ "top:100%; max-height: 300px; z-index:9"] do
        div_ [class_ "flex flex-col"] do
          forM_ hosts $ \host -> do
            let prm = "p-2 w-full text-left truncate ... hover:bg-blue-100 hover:text-black"
            a_
              [ class_ prm
              , href_ $ paramInput.currentURL <> "&project_host=" <> host.host
              ]
              do
                span_ [class_ "ml-2 text-sm text-slate-600"] $ toHtml host.host

  div_ [class_ "py-2 px-2 space-x-6 border-b border-slate-20 mt-6 mb-8 text-sm font-light", hxBoost_ "true"] do
    let uri = deleteParam "archived" $ deleteParam "ackd" paramInput.currentURL
    let forHost = not (T.null $ fromMaybe "" hostM)
    a_
      [ class_ $ "inline-block py-2 " <> if paramInput.ackd && not paramInput.archived then " font-bold text-black " else ""
      , href_ $ uri <> "&ackd=true&archived=false" <> maybe "" ("&project_host=" <>) pHostM
      ]
      "Active"
    a_
      [ class_ $ "relative inline-block  py-2 " <> if not paramInput.ackd && not paramInput.archived then " font-bold text-black " else "" <> if forHost then " cursor-not-allowed" else ""
      , href_ $ if forHost then "#" else uri <> "&ackd=false&archived=false" <> maybe "" ("&project_host=" <>) pHostM
      ]
      do
        span_ [] "Inbox"
        when (inbox_count > 0) $ span_ [class_ "absolute top-[1px] -right-[5px] text-white text-xs font-medium rounded-full px-1 bg-red-500"] $ show inbox_count
    a_
      [ class_ $ "inline-block  py-2 " <> if paramInput.archived then " font-bold text-black " else "" <> if forHost then " cursor-not-allowed" else ""
      , href_ $ if forHost then "#" else uri <> "&archived=true" <> maybe "" ("&project_host=" <>) pHostM
      ]
      "Archived"
  div_ [class_ "grid grid-cols-5 card-round", id_ "anomalyListBelowTab", hxGet_ paramInput.currentURL, hxSwap_ "outerHTML", hxTrigger_ "refreshMain"] $ endpointList' paramInput currTime pid endpoints inbox_count


endpointList' :: ParamInput -> UTCTime -> Projets.ProjectId -> Vector Endpoints.EndpointRequestStats -> Int -> Html ()
endpointList' paramInput currTime pid enps inbox_count = form_ [class_ "col-span-5 bg-white divide-y ", id_ "anomalyListForm"] $ do
  let bulkActionBase = "/p/" <> pid.toText <> "/anomalies/bulk_actions"
  let currentURL' = deleteParam "sort" paramInput.currentURL
  let sortMenu =
        [ ("First Seen", "First time the endpoint was seen", "first_seen")
        , ("Last Seen", "Last time the endpoint was seen", "last_seen")
        , ("Events", "Number of events", "events")
        ]
          :: [(Text, Text, Text)]
  let currentSortTitle = maybe "First Seen" fst3 $ find (\(_, _, identifier) -> identifier == paramInput.sort) sortMenu
  div_ [class_ "flex py-3 gap-8 items-center  bg-gray-50"] do
    div_ [class_ "h-4 flex space-x-3 w-8"] do
      a_ [class_ " w-2 h-full"] ""
      input_ [term "aria-label" "Select Issue", type_ "checkbox"]
    div_ [class_ " grow flex flex-row gap-2"] do
      button_ [class_ "btn btn-sm btn-outline border-black hover:shadow-2xl", hxPost_ $ bulkActionBase <> "/acknowlege", hxSwap_ "none"] "✓ acknowlege"
      button_ [class_ "btn btn-sm btn-outline space-x-1 border-black hover:shadow-2xl", hxPost_ $ bulkActionBase <> "/archive", hxSwap_ "none"] do
        faSprite_ "fa-inbox" "solid" "h-4 w-4 inline-block"
        span_ "archive"
    div_ [class_ "relative inline-block"] do
      a_ [class_ "btn-sm bg-transparent border-black hover:shadow-2xl space-x-2 cursor-pointer", [__|on click toggle .hidden on #sortMenuDiv |]] do
        mIcon_ "sort" "h-4 w-4"
        span_ $ toHtml currentSortTitle
      div_ [id_ "sortMenuDiv", hxBoost_ "true", class_ "p-1 hidden text-sm border border-black-30 absolute right-0 z-10 mt-2 w-72 origin-top-right rounded-md bg-white shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none", tabindex_ "-1"] do
        sortMenu & mapM_ \(title, desc, identifier) -> do
          let isActive = paramInput.sort == identifier || (paramInput.sort == "" && identifier == "first_seen")
          a_
            [ class_ $ "block flex flex-row px-3 py-2 hover:bg-blue-50 rounded-md cursor-pointer " <> (if isActive then " text-blue-800 " else "")
            , href_ $ currentURL' <> "&sort=" <> identifier
            , hxIndicator_ "#sortLoading"
            ]
            do
              div_ [class_ "flex flex-col items-center justify-center px-3"] do
                if isActive then mIcon_ "checkmark4" "w-4 h-5" else mIcon_ "" "w-4 h-5"
              div_ [class_ "grow space-y-1"] do
                span_ [class_ "block text-lg"] $ toHtml title
                span_ [class_ "block "] $ toHtml desc
    div_ [id_ "sortLoading", class_ "htmx-indicator fixed top-1/2 left-1/2 -translate-1/2 rounded-lg bg-white shadow p-10 border"] do
      span_ [class_ "loading loading-dots loading-md"] ""

    div_ [class_ "flex justify-center font-base w-60 content-between gap-14"] do
      span_ "GRAPH"
      div_ [class_ " space-x-2 font-base text-sm"] $ do
        a_ [class_ "cursor-pointer"] "24h"
        a_ [class_ "cursor-pointer font-bold text-base"] "14d"
    div_ [class_ "w-36 flex items-center justify-center"] $ span_ [class_ "font-base"] "EVENTS"
  div_ [class_ "w-full flex flex-row p-3"] $ do
    div_ [class_ "relative flex w-full bg-white py-2 px-3 border-solid border border-gray-200 h-10"] $ do
      faIcon_ "fa-magnifying-glass" "fa-light fa-magnifying-glass" "h-5 w-5"
      input_
        [ type_ "text"
        , [__| on input show .endpoint_item in #endpoints_container when its textContent.toLowerCase() contains my value.toLowerCase() |]
        , class_ "dataTable-search w-full h-full p-2 text-gray-500 font-normal focus:outline-none"
        , placeholder_ "Search endpoints..."
        ]
  when (null enps && inbox_count == 0) $ div_ [class_ "w-full p-20 items-center"] $ do
    h2_ [class_ "text-2xl font-medium mb-4"] "No Endpoints: You haven't integrated APIToolkit in your application yet"
    Onboarding.integrateApiToolkit "<YOUR_API_KEY>" "express"
  when (null enps && inbox_count > 0) $ div_ [class_ "flex flex-col text-center justify-center items-center h-32"] $ do
    strong_ "No endpoints yet."
    p_ "Check Inbox to acknowlege new endpoints"
  div_ [id_ "endpoints_container"] do
    mapM_ (renderEndpoint (paramInput.ackd && not paramInput.archived) currTime) enps


endpointAccentColor :: Bool -> Bool -> Text
endpointAccentColor _ True = "bg-slate-400"
endpointAccentColor True False = "bg-green-200"
endpointAccentColor False False = "bg-red-800"


renderEndpoint :: Bool -> UTCTime -> Endpoints.EndpointRequestStats -> Html ()
renderEndpoint activePage currTime enp = do
  div_ [class_ "flex py-4 gap-8 items-center endpoint_item"] do
    div_ [class_ "h-4 flex space-x-3 w-8 "] do
      a_ [class_ $ endpointAccentColor True {- isJust enp.acknowlegedAt -} True {- isJust enp.archivedAt -} <> " w-2 h-full"] ""
      let anomalyId = UUID.toText enp.anomalyId
      input_ [term "aria-label" "Select Issue", class_ "endpoint_anomaly_input", type_ "checkbox", name_ "anomalyId", value_ anomalyId]
    div_ [class_ "space-y-3 grow"] do
      div_ [class_ "space-x-3"] do
        a_ [class_ "inline-block font-bold text-blue-700 space-x-2", href_ ("/p/" <> enp.projectId.toText <> "/endpoints/" <> Endpoints.endpointIdText enp.endpointId)] $ do
          span_ [class_ $ "endpoint endpoint-" <> toLower enp.method, data_ "enp-urlMethod" enp.method] $ toHtml enp.method
          span_ [class_ " inconsolata text-base text-slate-700", data_ "enp-urlPath" enp.urlPath] $ toHtml $ if T.null enp.urlPath then "/" else T.take 150 enp.urlPath
      unless activePage do
        div_ [class_ "flex items-center gap-2 mt-5"] do
          AnomalyList.anomalyArchiveButton enp.projectId (Anomalies.AnomalyId enp.anomalyId) (isJust enp.archivedAt)
          AnomalyList.anomalyAcknowlegeButton enp.projectId (Anomalies.AnomalyId enp.anomalyId) (isJust enp.acknowlegedAt)
    div_ [class_ "flex items-center justify-center "]
      $ div_
        [ class_ "w-56 h-12 px-3"
        , hxGet_ $ "/charts_html?pid=" <> enp.projectId.toText <> "&since=14D&query_raw=" <> AnomalyList.escapedQueryPartial [PyF.fmt|endpoint_hash=="{enp.endpointHash}" | timechart [1d]|]
        , hxTrigger_ "intersect once"
        , hxSwap_ "innerHTML"
        ]
        ""

    div_ [class_ "w-36 flex items-center justify-center"] $ span_ [class_ "tabular-nums text-xl", term "data-tippy-content" "Events for this Anomaly in the last 14days"] $ toHtml @String $ fmt $ commaizeF enp.totalRequests


meter__ :: Double -> Html ()
meter__ prcntg = div_ [class_ "shadow w-full bg-slate-200 h-2.5 "] $ do
  div_ [class_ "shadow bg-blue-500 h-full", style_ $ fmt $ "width: " +| fixedF 2 prcntg |+ "%"] ""

-------------------------------------------------------------------------------------------
-- TODO: This section holds the navbar for pagination.
-- TODO: We should uncomment it when we have bandwidth to implement endpoint pagination
-------------------------------------------------------------------------------------------
-- table footer
-- README: Hiding the pagination logic because while pagination is important,
-- TODO: SHow this pagination when we're ready to actually implement the pagination logic.
-- we might not quickly have companies who have enough endpoints that they need pagination. So it's better to focus on important topics.
-- div_ [class_ "flex flex-row mt-5 justify-between hidden"] $ do
--   div_ [class_ "flex flex-row"] $ do
--     button_ [class_ "bg-transparent place-content-center py-2 px-3 mx-3 flex flex-row border-solid border border-gray-200 rounded-xl h-10"] $ do
--       span_ [class_ "text-sm text-slate-500 mr-1"] "10"
--       img_ [src_ "/assets/svgs/cheveron-down.svg", class_ "h-3 w-3 mt-1 mx-1"]
--     span_ [src_ "text-gray-200 mt-6 font-light text-base"] "Showing 1 - 10 of 100"
--   div_ [class_ "flex flex-row"] $ do
--     button_ [class_ "bg-gray-100 h-10 w-10 mx-1 px-2 flex flex-row rounded-xl py-3 place-content-center"] $ do
--       img_ [src_ "/assets/svgs/arrowleft1.svg", class_ "-mr-1"]
--       img_ [src_ "/assets/svgs/arrowleft1.svg", class_ "-ml-1"]
--     button_ [class_ "bg-gray-100 h-10 w-10 mx-1 px-2 rounded-xl py-1 place-content-center"] $ do
--       img_ [src_ "/assets/svgs/arrowleft1.svg", class_ "ml-1"]
--     button_ [class_ "bg-blue-700 h-10 w-10 mx-1 px-2 rounded-xl py-1"] $ do
--       span_ [class_ "text-white text-bold"] "1"
--     button_ [class_ "bg-transparent h-10 w-10 mx-1 px-2 rounded-xl py-1 hover:bg-gray-100 "] $ do
--       span_ [class_ "text-slate-700 text-bold"] "2"
--     button_ [class_ "bg-transparent h-10 w-10 mx-1 px-2 rounded-xl py-1 hover:bg-gray-100 "] $ do
--       span_ [class_ "text-slate-700 text-bold"] "3"
--     button_ [class_ "bg-transparent h-10 w-10 mx-1 px-2 rounded-xl py-1 hover:bg-gray-100 "] $ do
--       span_ [class_ "text-slate-700 text-bold"] "..."
--     button_ [class_ "bg-transparent h-10 w-10 mx-1 px-2 rounded-xl py-1 hover:bg-gray-100 "] $ do
--       span_ [class_ "text-slate-700 text-bold"] "5"
--     button_ [class_ "bg-[#304FFD]/20 place-content-center h-10 mx-1 rounded-xl py-1 w-10"] $ do
--       img_ [src_ "/assets/svgs/arrowright1.svg", class_ "ml-3"]
--     button_ [class_ "bg-blue-700/20 place-content-center h-10 mx-1 flex flex-row rounded-xl py-3 w-10"] $ do
--       img_ [src_ "/assets/svgs/arrowright1.svg", class_ "-mr-1"]
--       img_ [src_ "/assets/svgs/arrowright1.svg", class_ "-ml-1"]
