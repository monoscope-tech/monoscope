module Pages.Endpoints.EndpointList (endpointListGetH, renderEndpoint, EndpointRequestStatsVM (..), EnpReqStatsVM (..)) where

import Data.Default (def)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Time qualified as Time
import Fmt (commaizeF, fmt)
import Lucid
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pkg.Components qualified as Components
import Pkg.Components.ItemsList qualified as ItemsList
import Pkg.Components.Widget (WidgetAxis (..))
import Pkg.Components.Widget qualified as Widget
import PyF qualified
import Relude hiding (ask, asks)
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils qualified


endpointListGetH
  :: Projects.ProjectId
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> ATAuthCtx (RespHeaders EndpointRequestStatsVM)
endpointListGetH pid pageM layoutM filterTM hostM requestTypeM sortM hxRequestM hxBoostedM hxCurrentURL loadMoreM searchM = do
  (sess, project) <- Sessions.sessionAndProject pid
  let (ackd, archived, currentFilterTab) = case filterTM of
        Just "Active" -> (True, False, "Active")
        Just "Inbox" -> (False, False, "Inbox")
        Just "Archived" -> (False, True, "Archived")
        _ -> (True, False, "Active")

  let host = maybeToMonoid $ hostM >>= \t -> if t == "" then Nothing else Just t
  let page = fromMaybe 0 $ readMaybe (toString $ fromMaybe "" pageM)
  endpointStats <- dbtToEff $ Endpoints.endpointRequestStatsByProject pid ackd archived (Just host) sortM searchM page (fromMaybe "" requestTypeM)
  inboxCount <- dbtToEff $ Endpoints.countEndpointInbox pid host (fromMaybe "Incoming" requestTypeM)
  let requestType = fromMaybe "Incoming" requestTypeM
  let currentURL = [PyF.fmt|/p/{pid.toText}/endpoints?layout={fromMaybe "false" layoutM}&filter={fromMaybe "" filterTM}&sort={fromMaybe "event" sortM}&request_type={requestType}&host={host}|]
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Endpoints for " <> host
          , pageActions =
              Just $
                a_ [class_ "btn btn-sm btn-primary space-x-2", href_ $ "/p/" <> pid.toText <> "/documentation?host=" <> host] do
                  Utils.faSprite_ "plus" "regular" "h-4" >> "OpenAPI/Swagger"
          , navTabs =
              Just $
                toHtml $
                  Components.TabFilter
                    { current = currentFilterTab
                    , currentURL
                    , options =
                        [ Components.TabFilterOpt{name = "Active", count = Nothing}
                        , Components.TabFilterOpt{name = "Inbox", count = Just inboxCount}
                        , Components.TabFilterOpt{name = "Archived", count = Nothing}
                        ]
                    }
          }

  let nextFetchUrl = currentURL <> "&page=" <> show (page + 1) <> "&load_more=true"
  currTime <- Time.currentTime
  let endpReqVM = V.map (EnpReqStatsVM False currTime) endpointStats
  case (loadMoreM, searchM) of
    (Just _, _) -> addRespHeaders $ EndpointsListRows $ ItemsList.ItemsRows (Just nextFetchUrl) endpReqVM
    (_, Just _) -> addRespHeaders $ EndpointsListRows $ ItemsList.ItemsRows (Just nextFetchUrl) endpReqVM
    _ -> do
      let listCfg =
            ItemsList.ItemsListCfg
              { projectId = pid
              , nextFetchUrl = Just nextFetchUrl
              , sort = Just ItemsList.SortCfg{current = fromMaybe "events" sortM}
              , filter = Nothing
              , bulkActions =
                  [ ItemsList.BulkAction{icon = Just "check", title = "acknowlege", uri = "/p/" <> pid.toText <> "/anomalies/bulk_actions/acknowlege"}
                  , ItemsList.BulkAction{icon = Just "inbox-full", title = "archive", uri = "/p/" <> pid.toText <> "/anomalies/bulk_actions/archive"}
                  ]
              , heading = Just case hostM of
                  Just h -> span_ [] "Endpoints for dependency: " >> span_ [class_ "text-brand font-bold"] (toHtml h)
                  Nothing -> "Endpoints"
              , search = Just $ ItemsList.SearchCfg{viaQueryParam = Just (maybeToMonoid searchM)}
              , zeroState =
                  Just $
                    ItemsList.ZeroState
                      { icon = "empty-set"
                      , title = "Waiting for events"
                      , description = "You're currently not sending any data to APItoolkit from your backends yet."
                      , actionText = "Read the setup guide"
                      , destination = Right $ "https://apitoolkit.io/docs/sdks/"
                      }
              , elemID = "anomalyListForm"
              , ..
              }
      addRespHeaders $ EndpointsListPage $ PageCtx bwconf (ItemsList.ItemsPage listCfg endpReqVM)


data EnpReqStatsVM = EnpReqStatsVM Bool UTCTime Endpoints.EndpointRequestStats
  deriving stock (Show)


instance ToHtml EnpReqStatsVM where
  {-# INLINE toHtml #-}
  toHtml (EnpReqStatsVM hideByDefault currTime enp) = toHtmlRaw $ renderEndpoint hideByDefault currTime enp
  toHtmlRaw = toHtml


data EndpointRequestStatsVM
  = EndpointsListPage (PageCtx (ItemsList.ItemsPage EnpReqStatsVM))
  | EndpointsListRows (ItemsList.ItemsRows EnpReqStatsVM)


instance ToHtml EndpointRequestStatsVM where
  {-# INLINE toHtml #-}
  toHtml (EndpointsListPage pg) = toHtml pg
  toHtml (EndpointsListRows rows) = toHtml rows
  toHtmlRaw :: Monad m => EndpointRequestStatsVM -> HtmlT m ()
  toHtmlRaw = toHtml


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
        a_ [class_ "inline-block font-bold text-red-700 space-x-2", href_ ("/p/" <> enp.projectId.toText <> "/endpoints/details?var-endpointHash=" <> enp.endpointHash <> "&var-host=" <> enp.host)] $ do
          span_ [class_ $ "endpoint endpoint-" <> T.toLower enp.method, data_ "enp-urlMethod" enp.method] $ toHtml enp.method
          span_ [class_ " inconsolata text-base text-slate-700", data_ "enp-urlPath" enp.urlPath] $ toHtml $ if T.null enp.urlPath then "/" else T.take 150 enp.urlPath
        a_ [class_ "text-brand  hover:text-slate-600", href_ ("/p/" <> enp.projectId.toText <> "/log_explorer?query=" <> "url_path==\"" <> enp.urlPath <> "\"")] "View logs"
    div_ [class_ "w-36 flex items-center justify-center"] $ span_ [class_ "tabular-nums text-xl", term "data-tippy-content" "Events for this Anomaly in the last 14days"] $ toHtml @String $ fmt $ commaizeF enp.totalRequests
    div_ [class_ "flex items-center justify-center w-60 h-10"] $
      div_ [class_ "w-56 h-12 px-3"] $
        Widget.widget_ $
          (def :: Widget.Widget)
            { Widget.standalone = Just True
            , Widget.id = Just enp.endpointHash
            , Widget.title = Just enp.endpointHash
            , Widget.showTooltip = Just False
            , Widget.naked = Just True
            , Widget.xAxis = Just (def{showAxisLabel = Just False})
            , Widget.yAxis = Just (def{showOnlyMaxLabel = Just True})
            , Widget.query = Just $ "endpoint_hash==\"" <> enp.endpointHash <> "\" | timechart [1h]"
            , Widget._projectId = Just enp.projectId
            , Widget.hideLegend = Just True
            }
