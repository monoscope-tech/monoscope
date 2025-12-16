module Pages.Endpoints.ApiCatalog (apiCatalogH, HostEventsVM (..), endpointListGetH, CatalogList (..), EndpointRequestStatsVM (..), EnpReqStatsVM (..)) where

import Data.Default (def)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Fmt (commaizeF, fmt)
import Lucid
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pkg.Components.Table (Column (..), Config (..), Features (..), SearchMode (..), SortConfig (..), TabFilter (..), TabFilterOpt (..), Table (..), TableRows (..), ZeroState (..), col, withAttrs)
import Pkg.Components.Widget (WidgetAxis (..))
import Pkg.Components.Widget qualified as Widget
import PyF qualified
import Relude hiding (ask, asks)
import System.Config (AuthContext (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (checkFreeTierExceeded)


apiCatalogH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> ATAuthCtx (RespHeaders CatalogList)
apiCatalogH pid sortM timeFilter requestTypeM skipM = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext

  let requestType = fromMaybe "Incoming" requestTypeM
  let sortV = fromMaybe "events" sortM
  let filterV = fromMaybe "24H" timeFilter

  hostsAndEvents <- dbtToEff $ Endpoints.dependenciesAndEventsCount pid requestType sortV (fromMaybe 0 skipM) filterV
  freeTierExceeded <- dbtToEff $ checkFreeTierExceeded pid project.paymentPlan

  currTime <- Time.currentTime

  let currentURL = "/p/" <> pid.toText <> "/api_catalog?sort=" <> sortV <> "&request_type=" <> requestType
      nextFetchUrl =
        if V.length hostsAndEvents < 20
          then Nothing
          else Just $ currentURL <> "&skip=" <> maybe "20" (\x -> show $ 20 + x) skipM
  let hostsVM = V.map (\host -> HostEventsVM pid host filterV requestType) hostsAndEvents
  let catalogTable =
        Table
          { config = def{elemID = "anomalyListForm"}
          , columns = catalogColumns pid requestType
          , rows = hostsVM
          , features =
              def
                { rowId = Just \(HostEventsVM _ he _ _) -> he.host
                , bulkActions = [] -- No bulk actions for dependencies
                , search = Just ClientSide
                , sort =
                    Just
                      $ SortConfig
                        { current = sortV
                        , currentURL = currentURL
                        , options =
                            [ ("Latest", "Most recently accessed", "events")
                            , ("Alphabetical", "Sort by dependency name", "name")
                            ]
                        }
                , pagination = (, "both") <$> nextFetchUrl
                , zeroState =
                    Just
                      $ ZeroState
                        { icon = "empty-set"
                        , title = "No " <> requestType <> " Requests Monitored."
                        , description = "You're currently not monitoring your " <> T.toLower requestType <> " integrations."
                        , actionText = "See monitoring guide"
                        , destination = Right "https://monoscope.tech/docs/sdks/nodejs/expressjs/#monitoring-axios-requests"
                        }
                }
          }

  let bwconf =
        def
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "API Catalog"
          , freeTierExceeded = freeTierExceeded
          , navTabs = Just $ div_ [class_ "tabs tabs-box tabs-outline items-center"] do
              a_ [href_ $ "/p/" <> pid.toText <> "/api_catalog?sort=" <> sortV <> "&request_type=Incoming", role_ "tab", class_ $ "tab h-auto! " <> if requestType == "Incoming" then "tab-active text-textStrong" else ""] "Incoming"
              a_ [href_ $ "/p/" <> pid.toText <> "/api_catalog?sort=" <> sortV <> "&request_type=Outgoing", role_ "tab", class_ $ "tab h-auto! " <> if requestType == "Outgoing" then "tab-active text-textStrong" else ""] "Outgoing"
          }
  case skipM of
    Just _ -> addRespHeaders $ CatalogListRows $ TableRows nextFetchUrl (catalogColumns pid requestType) hostsVM
    _ -> addRespHeaders $ CatalogListPage $ PageCtx bwconf catalogTable


data HostEventsVM = HostEventsVM Projects.ProjectId Endpoints.HostEvents Text Text


catalogColumns :: Projects.ProjectId -> Text -> [Column HostEventsVM]
catalogColumns pid requestType =
  [ col "" renderCatalogCheckboxCol & withAttrs [class_ "h-4 flex space-x-3 w-8"]
  , col "Dependency" (renderCatalogMainCol pid requestType) & withAttrs [class_ "space-y-3 grow"]
  , col "Events" renderCatalogEventsCol & withAttrs [class_ "w-36 flex items-center justify-center"]
  , col "Activity" renderCatalogChartCol & withAttrs [class_ "flex items-center justify-center"]
  ]


renderCatalogCheckboxCol :: HostEventsVM -> Html ()
renderCatalogCheckboxCol (HostEventsVM _ he _ _) = do
  a_ [class_ "w-2 h-full"] ""
  input_ [term "aria-label" "Select Issue", class_ "endpoint_anomaly_input bulkactionItemCheckbox checkbox checkbox-md checked:checkbox-primary", type_ "checkbox", name_ "hostId", value_ he.host]


renderCatalogEventsCol :: HostEventsVM -> Html ()
renderCatalogEventsCol (HostEventsVM _ he _ _) =
  span_ [class_ "tabular-nums text-xl", term "data-tippy-content" "Events for this Anomaly in the last 14 days"]
    $ toHtml @String
    $ fmt (commaizeF he.eventCount)


renderCatalogChartCol :: HostEventsVM -> Html ()
renderCatalogChartCol (HostEventsVM pid he _ _) =
  div_ [class_ "w-56 h-12 px-3"]
    $ Widget.widget_
    $ (def :: Widget.Widget)
      { Widget.standalone = Just True
      , Widget.title = Just he.host
      , Widget.wType = Widget.WTTimeseries
      , Widget.showTooltip = Just False
      , Widget.naked = Just True
      , Widget.xAxis = Just (def{Widget.showAxisLabel = Just False})
      , Widget.yAxis = Just (def{Widget.showOnlyMaxLabel = Just True})
      , Widget.query = Just $ "attributes.http.host==\"" <> he.host <> "\" | summarize count(*) by bin_auto(timestamp)"
      , Widget._projectId = Just pid
      , Widget.hideLegend = Just True
      }


renderCatalogMainCol :: Projects.ProjectId -> Text -> HostEventsVM -> Html ()
renderCatalogMainCol pid requestType (HostEventsVM _ he _ _) =
  div_ [class_ "space-x-3"] do
    a_ [class_ "inline-block font-bold space-x-2"] $ do
      a_ [href_ $ "/p/" <> pid.toText <> "/endpoints?host=" <> he.host <> "&request_type=" <> requestType, class_ " hover:text-textWeak"] $ toHtml (T.replace "http://" "" $ T.replace "https://" "" he.host)
      a_ [href_ $ "/p/" <> pid.toText <> "/log_explorer?query=attributes.net.host.name%3D%3D" <> "\"" <> he.host <> "\"", class_ "text-textBrand hover:text-textWeak text-xs"] "View logs"


data CatalogList = CatalogListPage (PageCtx (Table HostEventsVM)) | CatalogListRows (TableRows HostEventsVM)


instance ToHtml CatalogList where
  toHtml (CatalogListPage pg) = toHtml pg
  toHtml (CatalogListRows r) = toHtml r
  toHtmlRaw = toHtml


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
  appCtx <- ask @AuthContext
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
          , config = appCtx.env
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

  currTime <- Time.currentTime
  let endpReqVM = V.map (EnpReqStatsVM False currTime) endpointStats
      nextFetchUrl =
        if V.length endpointStats < 30
          then Nothing
          else Just $ currentURL <> "&page=" <> show (page + 1) <> "&load_more=true"
  let endpointsTable =
        Table
          { config = def{elemID = "anomalyListForm"}
          , columns = endpointColumns pid
          , rows = endpReqVM
          , features =
              def
                { rowId = Just \(EnpReqStatsVM _ _ enp) -> enp.endpointHash
                , bulkActions = [] -- No bulk actions for endpoints
                , search = Just (ServerSide currentURL)
                , sort =
                    Just
                      $ SortConfig
                        { current = fromMaybe "events" sortM
                        , currentURL = currentURL
                        , options =
                            [ ("Most Active", "Most requests", "events")
                            , ("Alphabetical", "Sort by endpoint path", "name")
                            ]
                        }
                , pagination = (, "both") <$> nextFetchUrl
                , zeroState =
                    Just
                      $ ZeroState
                        { icon = "empty-set"
                        , title = "Waiting for events"
                        , description = "You're currently not sending any data to monoscope from your backends yet."
                        , actionText = "Read the setup guide"
                        , destination = Right "https://monoscope.tech/docs/sdks/"
                        }
                , header = Just $ div_ [class_ "mb-4"] $ case hostM of
                    Just h -> span_ [] "Endpoints for dependency: " >> span_ [class_ "text-textBrand font-bold"] (toHtml h)
                    Nothing -> "Endpoints"
                }
          }
  case (loadMoreM, searchM) of
    (Just _, _) -> addRespHeaders $ EndpointsListRows $ TableRows nextFetchUrl (endpointColumns pid) endpReqVM
    (_, Just _) -> addRespHeaders $ EndpointsListRows $ TableRows nextFetchUrl (endpointColumns pid) endpReqVM
    _ -> addRespHeaders $ EndpointsListPage $ PageCtx bwconf endpointsTable


data EnpReqStatsVM = EnpReqStatsVM Bool UTCTime Endpoints.EndpointRequestStats
  deriving stock (Show)


endpointColumns :: Projects.ProjectId -> [Column EnpReqStatsVM]
endpointColumns pid =
  [ col "" renderEndpointCheckboxCol & withAttrs [class_ "h-4 flex space-x-3 w-8 justify-center items-center"]
  , col "Endpoint" (renderEndpointMainCol pid) & withAttrs [class_ "space-y-3 grow"]
  , col "Events" renderEndpointEventsCol & withAttrs [class_ "w-36 flex items-center justify-center"]
  , col "Activity" renderEndpointChartCol & withAttrs [class_ "flex items-center justify-center w-60 h-10"]
  ]


renderEndpointCheckboxCol :: EnpReqStatsVM -> Html ()
renderEndpointCheckboxCol (EnpReqStatsVM _ _ enp) = do
  a_ [class_ $ endpointAccentColor True True <> " w-2 h-full"] ""
  input_ [term "aria-label" "Select Issue", class_ "endpoint_anomaly_input bulkactionItemCheckbox checkbox checkbox-md checked:checkbox-primary", type_ "checkbox", name_ "endpointHash", value_ enp.endpointHash]


renderEndpointEventsCol :: EnpReqStatsVM -> Html ()
renderEndpointEventsCol (EnpReqStatsVM _ _ enp) =
  span_ [class_ "tabular-nums text-xl", term "data-tippy-content" "Events for this Anomaly in the last 14days"]
    $ toHtml @String
    $ fmt
    $ commaizeF enp.totalRequests


renderEndpointChartCol :: EnpReqStatsVM -> Html ()
renderEndpointChartCol (EnpReqStatsVM _ _ enp) =
  div_ [class_ "w-56 h-12 px-3"]
    $ Widget.widget_
    $ (def :: Widget.Widget)
      { Widget.standalone = Just True
      , Widget.wType = Widget.WTTimeseries
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


renderEndpointMainCol :: Projects.ProjectId -> EnpReqStatsVM -> Html ()
renderEndpointMainCol pid (EnpReqStatsVM _ _ enp) =
  div_ [class_ "space-x-3"] do
    a_ [class_ "inline-block font-bold text-textError space-x-2", href_ ("/p/" <> pid.toText <> "/endpoints/details?var-endpointHash=" <> enp.endpointHash <> "&var-host=" <> enp.host)] $ do
      span_ [class_ $ "endpoint endpoint-" <> T.toLower enp.method, data_ "enp-urlMethod" enp.method] $ toHtml enp.method
      span_ [class_ " inconsolata text-base text-textStrong", data_ "enp-urlPath" enp.urlPath] $ toHtml $ if T.null enp.urlPath then "/" else T.take 150 enp.urlPath
    a_ [class_ "text-textBrand  hover:text-textStrong", href_ ("/p/" <> pid.toText <> "/log_explorer?query=" <> "attributes.http.route==\"" <> enp.urlPath <> "\"")] "View logs"


data EndpointRequestStatsVM
  = EndpointsListPage (PageCtx (Table EnpReqStatsVM))
  | EndpointsListRows (TableRows EnpReqStatsVM)


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
