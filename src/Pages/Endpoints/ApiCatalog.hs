module Pages.Endpoints.ApiCatalog (apiCatalogH, HostEventsVM (..), endpointListGetH, renderEndpoint, CatalogList (..), EndpointRequestStatsVM (..), EnpReqStatsVM (..)) where

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
import Pkg.Components.ItemsList (TabFilter (..), TabFilterOpt (..))
import Pkg.Components.ItemsList qualified as ItemsList
import Pkg.Components.Widget (WidgetAxis (..))
import Pkg.Components.Widget qualified as Widget
import PyF qualified
import Relude hiding (ask, asks)
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (checkFreeTierExceeded)
import Utils qualified


apiCatalogH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> ATAuthCtx (RespHeaders CatalogList)
apiCatalogH pid sortM timeFilter requestTypeM skipM = do
  (sess, project) <- Sessions.sessionAndProject pid

  let requestType = fromMaybe "Incoming" requestTypeM

  hostsAndEvents <- dbtToEff $ Endpoints.dependenciesAndEventsCount pid requestType (fromMaybe "events" sortM) (fromMaybe 0 skipM)
  freeTierExceeded <- dbtToEff $ checkFreeTierExceeded pid project.paymentPlan

  currTime <- Time.currentTime

  let sortV = fromMaybe "events" sortM
  let filterV = fromMaybe "14d" timeFilter

  let currentURL = "/p/" <> pid.toText <> "/api_catalog?sort=" <> sortV <> "&request_type=" <> requestType <> "&since=" <> filterV
      nextFetchUrl = Just $ currentURL <> "&skip=" <> maybe "10" (\x -> show $ 10 + x) skipM

  let listCfg =
        ItemsList.ItemsListCfg
          { projectId = pid
          , sort = Just $ ItemsList.SortCfg{current = sortV}
          , filter = Just filterV
          , currentURL = currentURL
          , currTime
          , bulkActions =
              [ ItemsList.BulkAction{icon = Just "check", title = "acknowledge", uri = "/p/" <> pid.toText <> "/anomalies/bulk_actions/acknowledge"}
              , ItemsList.BulkAction{icon = Just "inbox-full", title = "archive", uri = "/p/" <> pid.toText <> "/anomalies/bulk_actions/archive"}
              ]
          , search = Just $ ItemsList.SearchCfg{viaQueryParam = Nothing}
          , nextFetchUrl
          , heading = Nothing
          , zeroState =
              Just
                $ ItemsList.ZeroState
                  { icon = "empty-set"
                  , title = "No " <> requestType <> " Requests Monitored."
                  , description = "You're currently not monitoring your " <> T.toLower requestType <> " integrations."
                  , actionText = "See monitoring guide"
                  , destination = Right "https://apitoolkit.io/docs/sdks/nodejs/expressjs/#monitoring-axios-requests"
                  }
          , elemID = "anomalyListForm"
          }

  let bwconf =
        def
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "API Catalog"
          , freeTierExceeded = freeTierExceeded
          , navTabs = Just $ div_ [class_ "tabs tabs-box tabs-outline p-0  bg-fillWeak  text-textWeak border items-center border"] do
              a_ [href_ $ "/p/" <> pid.toText <> "/api_catalog?sort=" <> sortV <> "&request_type=Incoming", role_ "tab", class_ $ "tab " <> if requestType == "Incoming" then "tab-active text-textStrong border border-strokeStrong" else ""] "Incoming"
              a_ [href_ $ "/p/" <> pid.toText <> "/api_catalog?sort=" <> sortV <> "&request_type=Outgoing", role_ "tab", class_ $ "tab " <> if requestType == "Outgoing" then "tab-active text-textStrong border border-strokeStrong" else ""] "Outgoing"
          }
  case skipM of
    Just _ -> addRespHeaders $ CatalogListRows $ ItemsList.ItemsRows nextFetchUrl $ V.map (\host -> HostEventsVM pid host filterV requestType) hostsAndEvents
    _ -> addRespHeaders $ CatalogListPage $ PageCtx bwconf (ItemsList.ItemsPage listCfg $ V.map (\host -> HostEventsVM pid host filterV requestType) hostsAndEvents)


data HostEventsVM = HostEventsVM Projects.ProjectId Endpoints.HostEvents Text Text


data CatalogList = CatalogListPage (PageCtx (ItemsList.ItemsPage HostEventsVM)) | CatalogListRows (ItemsList.ItemsRows HostEventsVM)


instance ToHtml CatalogList where
  toHtml (CatalogListPage pg) = toHtml pg
  toHtml (CatalogListRows r) = toHtml r
  toHtmlRaw = toHtml


instance ToHtml HostEventsVM where
  toHtml :: Monad m => HostEventsVM -> HtmlT m ()
  toHtml (HostEventsVM pid he timeFilter requestType) = toHtmlRaw $ renderapiCatalog pid he timeFilter requestType
  toHtmlRaw :: Monad m => HostEventsVM -> HtmlT m ()
  toHtmlRaw = toHtml


renderapiCatalog :: Projects.ProjectId -> Endpoints.HostEvents -> Text -> Text -> Html ()
renderapiCatalog pid host timeFilter requestType = div_ [class_ "flex py-4 gap-8 items-center itemsListItem"] do
  div_ [class_ "h-4 flex space-x-3 w-8 "] do
    a_ [class_ "w-2 h-full"] ""
    input_ [term "aria-label" "Select Issue", class_ "endpoint_anomaly_input bulkactionItemCheckbox checkbox checkbox-md checked:checkbox-primary", type_ "checkbox", name_ "hostId", value_ host.host]

  div_ [class_ "space-y-3 grow"] do
    div_ [class_ "space-x-3"] do
      a_ [class_ "inline-block font-bold space-x-2"] $ do
        a_ [href_ $ "/p/" <> pid.toText <> "/endpoints?host=" <> host.host <> "&request_type=" <> requestType, class_ " hover:text-textWeak"] $ toHtml (T.replace "http://" "" $ T.replace "https://" "" host.host)
        a_ [href_ $ "/p/" <> pid.toText <> "/log_explorer?query=attributes.net.host.name%3D%3D" <> "\"" <> host.host <> "\"", class_ "text-textBrand hover:text-textWeak text-xs"] "View logs"

  div_ [class_ "w-36 flex items-center justify-center"]
    $ span_ [class_ "tabular-nums text-xl", term "data-tippy-content" "Events for this Anomaly in the last 14 days"]
    $ toHtml @String
    $ fmt (commaizeF host.eventCount)

  div_ [class_ "flex items-center justify-center "] do
    div_ [class_ "w-56 h-12 px-3"]
      $ Widget.widget_
      $ (def :: Widget.Widget)
        { Widget.standalone = Just True
        , Widget.title = Just host.host
        , Widget.showTooltip = Just False
        , Widget.naked = Just True
        , Widget.xAxis = Just (def{Widget.showAxisLabel = Just False})
        , Widget.yAxis = Just (def{Widget.showOnlyMaxLabel = Just True})
        , Widget.query = Just $ "attributes.http.host==\"" <> host.host <> "\" | summarize count(*) by bin(timestamp, 1h)"
        , Widget._projectId = Just pid
        , Widget.hideLegend = Just True
        }


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
  let hostParam = hostM >>= \h -> if h == "" then Nothing else Just h
  endpointStats <- dbtToEff $ Endpoints.endpointRequestStatsByProject pid ackd archived hostParam sortM searchM page (fromMaybe "" requestTypeM)
  inboxCount <- dbtToEff $ Endpoints.countEndpointInbox pid host (fromMaybe "Incoming" requestTypeM)
  freeTierExceeded <- dbtToEff $ checkFreeTierExceeded pid project.paymentPlan

  let requestType = fromMaybe "Incoming" requestTypeM
  let currentURL = [PyF.fmt|/p/{pid.toText}/endpoints?layout={fromMaybe "false" layoutM}&filter={fromMaybe "" filterTM}&sort={fromMaybe "event" sortM}&request_type={requestType}&host={host}|]
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , prePageTitle = Just "API Catalog"
          , pageTitle = "Endpoints for " <> host
          , freeTierExceeded = freeTierExceeded
          , pageActions =
              Just
                $ a_ [class_ "btn btn-sm btn-primary space-x-2", href_ $ "/p/" <> pid.toText <> "/documentation?host=" <> host] do
                  Utils.faSprite_ "plus" "regular" "h-4" >> "OpenAPI/Swagger"
          , navTabs =
              Just
                $ toHtml
                $ TabFilter
                  { current = currentFilterTab
                  , currentURL
                  , options =
                      [ TabFilterOpt{name = "Active", count = Nothing}
                      , TabFilterOpt{name = "Inbox", count = Just inboxCount}
                      , TabFilterOpt{name = "Archived", count = Nothing}
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
                  Just h -> span_ [] "Endpoints for dependency: " >> span_ [class_ "text-textBrand font-bold"] (toHtml h)
                  Nothing -> "Endpoints"
              , search = Just $ ItemsList.SearchCfg{viaQueryParam = Just (maybeToMonoid searchM)}
              , zeroState =
                  Just
                    $ ItemsList.ZeroState
                      { icon = "empty-set"
                      , title = "Waiting for events"
                      , description = "You're currently not sending any data to APItoolkit from your backends yet."
                      , actionText = "Read the setup guide"
                      , destination = Right "https://apitoolkit.io/docs/sdks/"
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
endpointAccentColor _ True = "bg-fillWeaker"
endpointAccentColor True False = "bg-fillSuccess-weak"
endpointAccentColor False False = "bg-fillError-strong"


renderEndpoint :: Bool -> UTCTime -> Endpoints.EndpointRequestStats -> Html ()
renderEndpoint activePage currTime enp = do
  div_ [class_ "flex py-4 gap-8 items-center endpoint_item itemsListItem"] do
    div_ [class_ "h-4 flex space-x-3 w-8 justify-center items-center"] do
      a_ [class_ $ endpointAccentColor True {- isJust enp.acknowlegedAt -} True {- isJust enp.archivedAt -} <> " w-2 h-full"] ""
      let anomalyId = UUID.toText enp.anomalyId
      input_ [term "aria-label" "Select Issue", class_ "endpoint_anomaly_input bulkactionItemCheckbox checkbox checkbox-md checked:checkbox-primary", type_ "checkbox", name_ "anomalyId", value_ anomalyId]
    div_ [class_ "space-y-3 grow"] do
      div_ [class_ "space-x-3"] do
        a_ [class_ "inline-block font-bold text-textError space-x-2", href_ ("/p/" <> enp.projectId.toText <> "/endpoints/details?var-endpointHash=" <> enp.endpointHash <> "&var-host=" <> enp.host)] $ do
          span_ [class_ $ "endpoint endpoint-" <> T.toLower enp.method, data_ "enp-urlMethod" enp.method] $ toHtml enp.method
          span_ [class_ " inconsolata text-base text-textStrong", data_ "enp-urlPath" enp.urlPath] $ toHtml $ if T.null enp.urlPath then "/" else T.take 150 enp.urlPath
        a_ [class_ "text-textBrand  hover:text-textStrong", href_ ("/p/" <> enp.projectId.toText <> "/log_explorer?query=" <> "attributes.http.route==\"" <> enp.urlPath <> "\"")] "View logs"
    div_ [class_ "w-36 flex items-center justify-center"] $ span_ [class_ "tabular-nums text-xl", term "data-tippy-content" "Events for this Anomaly in the last 14days"] $ toHtml @String $ fmt $ commaizeF enp.totalRequests
    div_ [class_ "flex items-center justify-center w-60 h-10"]
      $ div_ [class_ "w-56 h-12 px-3"]
      $ Widget.widget_
      $ (def :: Widget.Widget)
        { Widget.standalone = Just True
        , Widget.id = Just enp.endpointHash
        , Widget.title = Just enp.endpointHash
        , Widget.showTooltip = Just False
        , Widget.naked = Just True
        , Widget.xAxis = Just (def{showAxisLabel = Just False})
        , Widget.yAxis = Just (def{showOnlyMaxLabel = Just True})
        , Widget.query = Just $ "endpoint_hash==\"" <> enp.endpointHash <> "\" | summarize count(*) by bin(timestamp, 1h), status_code"
        , Widget._projectId = Just enp.projectId
        , Widget.hideLegend = Just True
        }
