module Pages.Endpoints.EndpointList (endpointListGetH, renderEndpoint) where

import Data.Default (def)
import Data.Text (toLower)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Time qualified as Time
import Fmt (commaizeF, fmt)
import Lucid
import Lucid.Htmx (hxGet_, hxSwap_, hxTrigger_)
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.Anomalies.AnomalyList qualified as AnomalyList
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Pkg.Components.ItemsList qualified as ItemsList
import PyF qualified
import Relude hiding (ask, asks)
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (deleteParam)


endpointListGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
endpointListGetH pid layoutM filterTM hostM projectHostM' sortM hxRequestM hxBoostedM hxCurrentURL = do
  (sess, project) <- Sessions.sessionAndProject pid
  let (ackd, archived, currentFilterTab) = case filterTM of
        Just "Active" -> (True, False, "Active")
        Just "Inbox" -> (False, False, "Inbox")
        Just "Archived" -> (False, False, "Archived")
        _ -> (True, False, "Active")

  let projectHostM = projectHostM' >>= (\t -> if t == "" then Nothing else Just t)
  endpointStats <- dbtToEff $ case hostM of
    Just h -> Endpoints.dependencyEndpointsRequestStatsByProject pid h
    Nothing -> Endpoints.endpointRequestStatsByProject pid ackd archived projectHostM sortM
  projHosts <- dbtToEff $ Endpoints.getProjectHosts pid
  inboxCount <- dbtToEff $ Endpoints.countEndpointInbox pid
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession
          , currProject = Just project
          , pageTitle = "Endpoints"
          }
  let currentURL = "/p/" <> pid.toText <> "/endpoints?layout=" <> fromMaybe "false" layoutM <> "&filter=" <> fromMaybe "" filterTM <> "&sort=" <> fromMaybe "event" sortM <> "&project_host=" <> fromMaybe "" hostM
  currTime <- Time.currentTime
  let listCfg =
        ItemsList.ItemsListCfg
          { projectId = pid
          , nextFetchUrl = Nothing
          , sort = Just $ ItemsList.SortCfg{current = fromMaybe "events" sortM}
          , bulkActions =
              [ ItemsList.BulkAction{icon = Just "check", title = "acknowlege", uri = "/p/" <> pid.toText <> "/anomalies/bulk_actions/acknowlege"}
              , ItemsList.BulkAction{icon = Just "inbox-full", title = "archive", uri = "/p/" <> pid.toText <> "/anomalies/bulk_actions/archive"}
              ]
          , tabsFilter =
              Just
                $ ItemsList.TabFilter
                  { current = currentFilterTab
                  , options =
                      [ ItemsList.TabFilterOpt{name = "Active", count = Nothing}
                      , ItemsList.TabFilterOpt{name = "Inbox", count = if inboxCount > 0 then Just inboxCount else Nothing}
                      , ItemsList.TabFilterOpt{name = "Archived", count = Nothing}
                      ]
                  }
          , heading =
              Just
                $ ItemsList.Heading
                  { pageTitle = case hostM of
                      Just h -> span_ [] "Endpoints for dependency: " >> (span_ [class_ "text-blue-500 font-bold"] $ toHtml h)
                      Nothing -> "Endpoints"
                  , rightComponent = Nothing
                  , subSection = Just $ hostFilter_ currentURL projHosts hostM projectHostM
                  }
          , search = Just $ ItemsList.SearchCfg{viaQueryParam = Nothing}
          , zeroState =
              Just
                $ ItemsList.ZeroState
                  { icon = "empty-set"
                  , title = "Waiting for events"
                  , description = "You're currently not sending any data to APItoolkit from your backends yet."
                  , actionText = "Read the setup guide"
                  , destination = "/p/" <> listCfg.projectId.toText <> "/integration_guides"
                  }
          , elemID = "anomalyListForm"
          , ..
          }
  addRespHeaders $ bodyWrapper bwconf $ ItemsList.itemsPage_ listCfg endpointStats \_ -> (renderEndpoint (ackd && not archived) listCfg.currTime)


hostFilter_ :: Text -> Vector Endpoints.Host -> Maybe Text -> Maybe Text -> Html ()
hostFilter_ currentURL hosts hostM pHostM = when (isNothing hostM) $ div_ [class_ "mt-8 flex  items-center gap-2"] do
  span_ [class_ "font-bold"] "Host "
  div_ [class_ "dropdown"] do
    div_ [tabindex_ "0", role_ "btn", class_ "input input-sm"] $ toHtml $ fromMaybe "Select host" pHostM
    ul_ [tabindex_ "0", class_ "dropdown-content z-[1] menu p-2 shadow bg-base-100 rounded-box w-52 h-96 overflow-y-scroll"] do
      let uri = deleteParam "project_host" currentURL
      forM_ hosts \host -> li_ [] $ a_ [class_ "prm", href_ $ uri <> "&project_host=" <> host.host] $ toHtml host.host


endpointAccentColor :: Bool -> Bool -> Text
endpointAccentColor _ True = "bg-slate-400"
endpointAccentColor True False = "bg-green-200"
endpointAccentColor False False = "bg-red-800"


renderEndpoint :: Bool -> UTCTime -> Endpoints.EndpointRequestStats -> Html ()
renderEndpoint activePage currTime enp = do
  div_ [class_ "flex py-4 gap-8 items-center endpoint_item itemsListItem"] do
    div_ [class_ "h-4 flex space-x-3 w-8 justify-center items-center"] do
      a_ [class_ $ endpointAccentColor True {- isJust enp.acknowlegedAt -} True {- isJust enp.archivedAt -} <> " w-2 h-full"] ""
      let anomalyId = UUID.toText enp.anomalyId
      input_ [term "aria-label" "Select Issue", class_ "endpoint_anomaly_input bulkactionItemCheckbox checkbox checkbox-md checked:checkbox-primary", type_ "checkbox", name_ "anomalyId", value_ anomalyId]
    div_ [class_ "space-y-3 grow"] do
      div_ [class_ "space-x-3"] do
        a_ [class_ "inline-block font-bold text-red-700 space-x-2", href_ ("/p/" <> enp.projectId.toText <> "/endpoints/" <> Endpoints.endpointIdText enp.endpointId)] $ do
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
