module Pages.Endpoints.Outgoing (outgoingGetH) where

import Data.Default (def)
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Time qualified as Time
import Lucid
import Lucid.Htmx (hxGet_, hxSwap_, hxTrigger_)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.Anomalies.AnomalyList qualified as AnomalyList
import Pages.BodyWrapper (BWConfig (currProject, pageTitle, sessM), bodyWrapper)
import Pkg.Components.ItemsList qualified as ItemsList
import PyF qualified
import Relude hiding (ask, asks)
import System.Types


outgoingGetH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
outgoingGetH pid sortM = do
  (sess, project) <- Sessions.sessionAndProject pid
  hostsAndEvents <- dbtToEff $ Endpoints.dependenciesAndEventsCount pid (fromMaybe "events" sortM)
  currTime <- Time.currentTime
  let sortV = (fromMaybe "events" sortM)
  let listCfg =
        ItemsList.ItemsListCfg
          { projectId = pid
          , sort = sortV
          , currentURL = "/p/" <> pid.toText <> "/outgoing?sort=" <> sortV
          , currTime
          , heading = Nothing
          , nextFetchUrl = Nothing
          , tabsFilter = Nothing
          , zeroState =
              Just
                $ ItemsList.ZeroState
                  { icon = "empty-set"
                  , title = "No Outgoing Requests Monitored."
                  , description = "You're currently not monitoring your outbound integrations."
                  , actionText = "See monitoring guide"
                  , destination = "/p/" <> listCfg.projectId.toText <> "/integration_guides#outgoing-request-monitoring"
                  }
          , elemID = "anomalyListForm"
          }
  let bwconf =
        def
          { sessM = Just sess.persistentSession
          , currProject = Just project
          , pageTitle = "Outgoing Integrations"
          }
  addRespHeaders $ bodyWrapper bwconf $ outgoingPage listCfg pid sortV hostsAndEvents


outgoingPage :: ItemsList.ItemsListCfg -> Projects.ProjectId -> Text -> V.Vector Endpoints.HostEvents -> Html ()
outgoingPage listCfg pid sortV hostsEvents = do
  div_ [class_ "w-full mx-auto px-16 pt-10 pb-24 overflow-y-scroll h-full"] $ do
    h3_ [class_ "text-xl text-slate-700 flex gap-1 place-items-center mb-10"] "Outgoing Integrations"
    ItemsList.itemsList_ listCfg hostsEvents \_ -> renderOutgoing pid


renderOutgoing :: Projects.ProjectId -> Endpoints.HostEvents -> Html ()
renderOutgoing pid host = div_ [class_ "flex py-4 gap-8 items-center "] do
  div_ [class_ "h-4 flex space-x-3 w-8 "] do
    a_ [class_ "w-2 h-full"] ""
    input_ [term "aria-label" "Select Issue", class_ "endpoint_anomaly_input", type_ "checkbox", name_ "hostId", value_ host.host]

  div_ [class_ "space-y-3 grow"] do
    div_ [class_ "space-x-3"] do
      a_ [class_ "inline-block font-bold space-x-2"] $ do
        a_ [href_ $ "/p/" <> pid.toText <> "/endpoints?host=" <> host.host, class_ " hover:text-slate-600"] $ toHtml (T.replace "http://" "" $ T.replace "https://" "" host.host)
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
