module Pages.Endpoints.EndpointList (endpointListGetH, renderEndpoint) where

import Data.Default (def)
import Data.Text (toLower)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Fmt (commaizeF, fmt)
import Lucid
import Lucid.Htmx (hxBoost_, hxGet_, hxSwap_, hxTrigger_)
import Lucid.Hyperscript.QuasiQuoter (__)
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.Anomalies.AnomalyList qualified as AnomalyList
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Pkg.Components.ItemsList qualified as ItemsList
import PyF qualified
import Relude hiding (ask, asks)
import System.Config (AuthContext)
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (deleteParam, faSprite_, textToBool)


data ParamInput = ParamInput
  { currentURL :: Text
  , archived :: Bool
  , sort :: Text
  , ackd :: Bool
  }


endpointListGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
endpointListGetH pid layoutM ackdM archivedM hostM projectHostM sortM hxRequestM hxBoostedM hxCurrentURL = do
  (sess, project) <- Sessions.sessionAndProject pid
  let ackd = maybe True textToBool ackdM
  let archived = maybe False textToBool archivedM

  appCtx <- ask @AuthContext
  endpointStats <- dbtToEff $ case hostM of
    Just h -> Endpoints.dependencyEndpointsRequestStatsByProject pid h
    Nothing -> Endpoints.endpointRequestStatsByProject pid ackd archived projectHostM sortM
  projHosts <- dbtToEff $ Endpoints.getProjectHosts pid
  inbox <- dbtToEff $ Endpoints.countEndpointInbox pid
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession
          , currProject = Just project
          , pageTitle = "Endpoints"
          }
  let currentURL = "/p/" <> pid.toText <> "/endpoints?layout=" <> fromMaybe "false" layoutM <> "&ackd=" <> fromMaybe "true" ackdM <> "&archived=" <> fromMaybe "false" archivedM <> "&sort=" <> fromMaybe "event" sortM
  currTime <- Time.currentTime
  let paramInput =
        ParamInput
          { currentURL = currentURL
          , ackd = ackd
          , archived = archived
          , sort = fromMaybe "events" sortM
          }
  case (hxRequestM, hxBoostedM) of
    _ -> addRespHeaders $ bodyWrapper bwconf $ endpointListPage paramInput pid currTime endpointStats projHosts hostM projectHostM inbox


endpointListPage :: ParamInput -> Projects.ProjectId -> UTCTime -> Vector Endpoints.EndpointRequestStats -> Vector Endpoints.Host -> Maybe Text -> Maybe Text -> Int -> Html ()
endpointListPage paramInput@ParamInput{..} pid currTime endpoints hosts hostM pHostM inbox_count = div_ [class_ "w-full mx-auto px-16 pt-10 pb-24 overflow-y-scroll h-full"] $ do
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
      faSprite_ "chevron-down" "light" "h-4 w-4"
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
  let listCfg =
        ItemsList.ItemsListCfg
          { projectId = pid
          , nextFetchUrl = Nothing
          , zeroState =
              Just $
                ItemsList.ZeroState
                  { icon = "empty-set"
                  , title = "Waiting for events"
                  , description = "You're currently not sending any data to APItoolkit from your backends yet."
                  , actionText = "Read the setup guide"
                  , destination = "/p/" <> listCfg.projectId.toText <> "/integration_guides"
                  }
          , elemID = "anomalyListForm"
          , ..
          }
  ItemsList.itemsList_ listCfg endpoints \_ -> (renderEndpoint (paramInput.ackd && not paramInput.archived) listCfg.currTime)


endpointAccentColor :: Bool -> Bool -> Text
endpointAccentColor _ True = "bg-slate-400"
endpointAccentColor True False = "bg-green-200"
endpointAccentColor False False = "bg-red-800"


renderEndpoint :: Bool -> UTCTime -> Endpoints.EndpointRequestStats -> Html ()
renderEndpoint activePage currTime enp = do
  div_ [class_ "flex py-4 gap-8 items-center endpoint_item "] do
    div_ [class_ "h-4 flex space-x-3 w-8 "] do
      a_ [class_ $ endpointAccentColor True {- isJust enp.acknowlegedAt -} True {- isJust enp.archivedAt -} <> " w-2 h-full"] ""
      let anomalyId = UUID.toText enp.anomalyId
      input_ [term "aria-label" "Select Issue", class_ "endpoint_anomaly_input", type_ "checkbox", name_ "anomalyId", value_ anomalyId]
    div_ [class_ "space-y-3 grow"] do
      div_ [class_ "space-x-3"] do
        a_ [class_ "inline-block font-bold text-red-700 space-x-2", href_ ("/p/" <> enp.projectId.toText <> "/endpoints/" <> Endpoints.endpointIdText enp.endpointId)] $ do
          span_ [class_ $ "endpoint endpoint-" <> toLower enp.method, data_ "enp-urlMethod" enp.method] $ toHtml enp.method
          span_ [class_ " inconsolata text-base text-slate-700", data_ "enp-urlPath" enp.urlPath] $ toHtml $ if T.null enp.urlPath then "/" else T.take 150 enp.urlPath
      unless activePage do
        div_ [class_ "flex items-center gap-2 mt-5"] do
          AnomalyList.anomalyArchiveButton enp.projectId (Anomalies.AnomalyId enp.anomalyId) (isJust enp.archivedAt)
          AnomalyList.anomalyAcknowlegeButton enp.projectId (Anomalies.AnomalyId enp.anomalyId) (isJust enp.acknowlegedAt)
    div_ [class_ "flex items-center justify-center "] $
      div_
        [ class_ "w-56 h-12 px-3"
        , hxGet_ $ "/charts_html?pid=" <> enp.projectId.toText <> "&since=14D&query_raw=" <> AnomalyList.escapedQueryPartial [PyF.fmt|endpoint_hash=="{enp.endpointHash}" | timechart [1d]|]
        , hxTrigger_ "intersect once"
        , hxSwap_ "innerHTML"
        ]
        ""
    div_ [class_ "w-36 flex items-center justify-center"] $ span_ [class_ "tabular-nums text-xl", term "data-tippy-content" "Events for this Anomaly in the last 14days"] $ toHtml @String $ fmt $ commaizeF enp.totalRequests
