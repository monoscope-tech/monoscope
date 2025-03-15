module Pages.Endpoints.ApiCatalog (apiCatalogH, HostEventsVM (..)) where

import Data.Default (def)
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Time qualified as Time
import Fmt (commaizeF, fmt)
import Lucid
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (currProject, pageTitle, sessM), PageCtx (..), navTabs)
import Pkg.Components.ItemsList qualified as ItemsList
import Pkg.Components.Widget qualified as Widget
import Relude hiding (ask, asks)
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)


apiCatalogH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx (ItemsList.ItemsPage HostEventsVM)))
apiCatalogH pid sortM timeFilter requestTypeM = do
  (sess, project) <- Sessions.sessionAndProject pid

  let requestType = fromMaybe "Incoming" requestTypeM

  hostsAndEvents <- dbtToEff $ Endpoints.dependenciesAndEventsCount pid requestType (fromMaybe "events" sortM)

  currTime <- Time.currentTime

  let sortV = fromMaybe "events" sortM
  let filterV = fromMaybe "14d" timeFilter

  let currentURL = "/p/" <> pid.toText <> "/api_catalog?sort=" <> sortV <> "&request_type=" <> requestType

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
          , nextFetchUrl = Nothing
          , heading = Nothing
          , zeroState =
              Just
                $ ItemsList.ZeroState
                  { icon = "empty-set"
                  , title = "No " <> requestType <> " Requests Monitored."
                  , description = "You're currently not monitoring your " <> T.toLower requestType <> " integrations."
                  , actionText = "See monitoring guide"
                  , destination = Right $ "https://apitoolkit.io/docs/sdks/nodejs/expressjs/#monitoring-axios-requests"
                  }
          , elemID = "anomalyListForm"
          }

  let bwconf =
        def
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "API Catalog"
          , navTabs = Just $ div_ [class_ "tabs tabs-box tabs-outline p-0  bg-fillWeak  text-textWeak border items-center border"] do
              a_ [href_ $ "/p/" <> pid.toText <> "/api_catalog?sort=" <> sortV <> "&request_type=Incoming", role_ "tab", class_ $ "tab " <> if requestType == "Incoming" then "tab-active text-textStrong border border-strokeStrong" else ""] "Incoming"
              a_ [href_ $ "/p/" <> pid.toText <> "/api_catalog?sort=" <> sortV <> "&request_type=Outgoing", role_ "tab", class_ $ "tab " <> if requestType == "Outgoing" then "tab-active text-textStrong border border-strokeStrong" else ""] "Outgoing"
          }

  addRespHeaders $ PageCtx bwconf (ItemsList.ItemsPage listCfg $ V.map (\host -> HostEventsVM pid host filterV requestType) hostsAndEvents)


data HostEventsVM = HostEventsVM Projects.ProjectId Endpoints.HostEvents Text Text


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
        a_ [href_ $ "/p/" <> pid.toText <> "/endpoints?host=" <> host.host <> "&request_type=" <> requestType, class_ " hover:text-slate-600"] $ toHtml (T.replace "http://" "" $ T.replace "https://" "" host.host)
        a_ [href_ $ "/p/" <> pid.toText <> "/log_explorer?query=host%3D%3D" <> "\"" <> host.host <> "\"", class_ "text-brand hover:text-slate-600 text-xs"] "View logs"

  div_ [class_ "w-36 flex items-center justify-center"]
    $ span_ [class_ "tabular-nums text-xl", term "data-tippy-content" "Events for this Anomaly in the last 14 days"]
    $ toHtml @String
    $ fmt
    $ commaizeF host.eventCount

  div_ [class_ "flex items-center justify-center "] $ do
    div_ [class_ "w-56 h-12 px-3"]
      $ Widget.widget_
      $ (def :: Widget.Widget)
        { Widget.standalone = Just True
        , Widget.title = Just host.host
        , Widget.showTooltip = Just False
        , Widget.naked = Just True
        , Widget.xAxis = Just (def{Widget.showAxisLabel = Just False})
        , Widget.yAxis = Just (def{Widget.showOnlyMaxLabel = Just True})
        , Widget.query = Just $ "host==\"" <> host.host <> "\" | timechart [1h]"
        , Widget._projectId = Just pid
        , Widget.hideLegend = Just True
        }
