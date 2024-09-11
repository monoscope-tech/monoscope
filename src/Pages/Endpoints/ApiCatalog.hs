module Pages.Endpoints.ApiCatalog (apiCatalogH, HostEventsVM (..)) where

import Data.Default (def)
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Time qualified as Time
import Fmt (commaizeF, fmt)
import Lucid
import Lucid.Htmx (hxGet_, hxSwap_, hxTrigger_) -- HTMX integration for frontend interactivity.
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.Anomalies.AnomalyList qualified as AnomalyList
import Pages.BodyWrapper (BWConfig (currProject, pageTitle, sessM), PageCtx (..), navTabs)
import Pkg.Components.ItemsList qualified as ItemsList -- Package for the reusable list component.
import PyF qualified -- PyF for formatted strings.
import Relude hiding (ask, asks) -- Standard library extensions with certain functions hidden.
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders) -- Importing custom types for handling responses.

-- Function to handle the API catalog page.
apiCatalogH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx (ItemsList.ItemsPage HostEventsVM)))
apiCatalogH pid sortM timeFilter requestTypeM = do
  -- Fetch user session and project details based on the project ID.
  (sess, project) <- Sessions.sessionAndProject pid

  -- Default the request type to "Incoming" if not provided.
  let requestType = fromMaybe "Incoming" requestTypeM

  -- Fetch hosts and events from the database with sorting and request type filtering.
  hostsAndEvents <- dbtToEff $ Endpoints.dependenciesAndEventsCount pid requestType (fromMaybe "events" sortM)

  currTime <- Time.currentTime

  -- Extract or default the sort and filter values.
  let sortV = fromMaybe "events" sortM
  let filterV = fromMaybe "24h" timeFilter

  -- Build the current URL with appropriate query parameters.
  let currentURL = "/p/" <> pid.toText <> "/api_catalog?sort=" <> sortV <> "&request_type=" <> requestType

  -- Configure the items list for rendering events related to hosts.
  let listCfg =
        ItemsList.ItemsListCfg
          { projectId = pid
          , sort = Just $ ItemsList.SortCfg{current = sortV}
          , filter = Just filterV
          , currentURL = currentURL
          , currTime
          , bulkActions =
              [ -- Define bulk actions like "acknowledge" and "archive" for anomalies.
                ItemsList.BulkAction{icon = Just "check", title = "acknowledge", uri = "/p/" <> pid.toText <> "/anomalies/bulk_actions/acknowledge"}
              , ItemsList.BulkAction{icon = Just "inbox-full", title = "archive", uri = "/p/" <> pid.toText <> "/anomalies/bulk_actions/archive"}
              ]
          , search = Just $ ItemsList.SearchCfg{viaQueryParam = Nothing}
          , nextFetchUrl = Nothing
          , heading = Nothing
          , zeroState = -- Define the empty state when no requests are being monitored.
              Just
                $ ItemsList.ZeroState
                  { icon = "empty-set"
                  , title = "No " <> requestType <> " Requests Monitored."
                  , description = "You're currently not monitoring your " <> T.toLower requestType <> " integrations."
                  , actionText = "See monitoring guide"
                  , destination = Right $ "/p/" <> listCfg.projectId.toText <> "/integration_guides#" <> T.toLower requestType <> "-request-monitoring"
                  }
          , elemID = "anomalyListForm"
          }

  -- Define the page's body wrapper configuration (metadata for rendering).
  let bwconf =
        def
          { sessM = Just sess.persistentSession -- User session management.
          , currProject = Just project -- Current project information.
          , pageTitle = "API Catalog" -- Page title for the API catalog.
          , navTabs = Just $ div_ [class_ "tabs tabs-boxed border"] do -- Navigation tabs for "Incoming" and "Outgoing" requests.
              a_ [href_ $ "/p/" <> pid.toText <> "/api_catalog?sort=" <> sortV <> "&request_type=Incoming", role_ "tab", class_ $ "tab " <> if requestType == "Incoming" then "tab-active" else ""] "Incoming"
              a_ [href_ $ "/p/" <> pid.toText <> "/api_catalog?sort=" <> sortV <> "&request_type=Outgoing", role_ "tab", class_ $ "tab " <> if requestType == "Outgoing" then "tab-active" else ""] "Outgoing"
          }
  
  -- Add response headers and render the page context (items list).
  addRespHeaders $ PageCtx bwconf (ItemsList.ItemsPage listCfg $ V.map (\host -> HostEventsVM pid host filterV requestType) hostsAndEvents)


-- Data type for representing host events in the ViewModel.
data HostEventsVM = HostEventsVM Projects.ProjectId Endpoints.HostEvents Text Text


-- ToHtml instance to define how HostEventsVM should be converted to HTML.
instance ToHtml HostEventsVM where
  toHtml :: Monad m => HostEventsVM -> HtmlT m ()
  toHtml (HostEventsVM pid he timeFilter requestType) = toHtmlRaw $ renderapiCatalog pid he timeFilter requestType -- Convert HostEventsVM to HTML using renderapiCatalog function.
  toHtmlRaw :: Monad m => HostEventsVM -> HtmlT m ()
  toHtmlRaw = toHtml -- Raw HTML rendering.

-- Function to render the API catalog for a specific project, host, and request type.
renderapiCatalog :: Projects.ProjectId -> Endpoints.HostEvents -> Text -> Text -> Html ()
renderapiCatalog pid host timeFilter requestType = div_ [class_ "flex py-4 gap-8 items-center itemsListItem"] do
  -- Checkbox for selecting an issue in the list.
  div_ [class_ "h-4 flex space-x-3 w-8 "] do
    a_ [class_ "w-2 h-full"] ""
    input_ [term "aria-label" "Select Issue", class_ "endpoint_anomaly_input bulkactionItemCheckbox checkbox checkbox-md checked:checkbox-primary", type_ "checkbox", name_ "hostId", value_ host.host]

  -- Main content section displaying the host and actions (view logs, etc.).
  div_ [class_ "space-y-3 grow"] do
    div_ [class_ "space-x-3"] do
      a_ [class_ "inline-block font-bold space-x-2"] $ do
        -- Display host and link to endpoint details.
        a_ [href_ $ "/p/" <> pid.toText <> "/endpoints?host=" <> host.host <> "&request_type=" <> requestType, class_ " hover:text-slate-600"] $ toHtml (T.replace "http://" "" $ T.replace "https://" "" host.host)
        -- Link to view logs for this host.
        a_ [href_ $ "/p/" <> pid.toText <> "/log_explorer?query=host%3D%3D" <> "\"" <> host.host <> "\"", class_ "text-blue-500 hover:text-slate-600 text-xs"] "View logs"

  -- Section for fetching and displaying a time chart via HTMX.
  div_ [class_ "flex items-center justify-center "] $ do
    -- Conditionally use the filter for the chart.
    div_ 
      [ class_ "w-56 h-12 px-3"
      , hxGet_ $ "/charts_html?pid=" <> pid.toText <> "&since=" <> (if timeFilter == "14d" then "14D" else "24h") <> "&query_raw=" <> AnomalyList.escapedQueryPartial [PyF.fmt|host=="{host.host}" | timechart [1d]|] -- Fetch the chart for this host.
      , hxTrigger_ "intersect once" -- Load the chart when the element is in view.
      , hxSwap_ "innerHTML" -- Replace the content with the fetched chart.
      ]
      ""

  -- Display the number of events for this anomaly in the last 14 days.
  div_ [class_ "w-36 flex items-center justify-center"] $
    span_ [class_ "tabular-nums text-xl", term "data-tippy-content" "Events for this Anomaly in the last 14 days"] $ toHtml @String $ fmt $ commaizeF host.eventCount
