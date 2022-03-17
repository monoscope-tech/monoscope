module Pages.Anomalies.AnomalyList (anomalyListGetH) where

import Config
import Data.Time (defaultTimeLocale, formatTime, utc)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields qualified as Fields
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Optics.Core ((%), (^.), (^?), _Just)
import Pages.BodyWrapper (bodyWrapper)
import Relude

anomalyListGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
anomalyListGetH sess pid = do
  pool <- asks pool
  (project, anomalies) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      anomalies <- Anomalies.selectAnomalies pid
      pure (project, anomalies)
  pure $ bodyWrapper (Just sess) project "Anomalies" $ anomalyList anomalies

anomalyList :: Vector Anomalies.AnomalyVM -> Html ()
anomalyList anomalies = do
  div_ [class_ "container mx-auto  px-4 pt-10 pb-24 h-full overflow-y-scroll"] $ do
    div_ [class_ "flex justify-between"] $ do
      h3_ [class_ "text-xl text-slate-700 flex place-items-center"] "Anomalies"
      div_ [class_ "flex flex-row"] $ do
        button_ [class_ "bg-white rounded-xl py-2 px-4 m-3 h-10 flex flex-row"] $ do
          img_ [src_ "/assets/svgs/download.svg", class_ "h-4 w-6"]
          span_ [class_ "text-sm"] "Export"
          img_ [src_ "/assets/svgs/cheveron-down.svg", class_ "h-3 w-3 mt-1 mx-1"]
        button_ [class_ "bg-blue-700 h-10  px-2 rounded-xl py-1 mt-3 "] $ do
          img_ [src_ "/assets/svgs/white-plus.svg", class_ "text-white h-4 w-6 text-bold"]
    div_ [class_ "grid grid-cols-5"] $ do
      div_ [class_ "col-span-1"] $ do
        div_ [] "endpoints"
      div_ [class_ "col-span-4 space-y-2"] $ do
        anomalies & mapM_ fieldAnomaly

fieldAnomaly :: Anomalies.AnomalyVM -> Html ()
fieldAnomaly anomaly = case anomaly ^. #anomalyType of
  Anomalies.ATField -> do
    div_ [class_ "bg-white border-2 border-gray-100 rounded-xl p-8 hover:bg-blue-50 parent-hover cursor-pointer"] $ do
      div_ [class_ "clear-both"] $ do
        div_ [class_ "inline-block"] $ do
          img_ [src_ "/assets/svgs/anomalies/fields.svg", class_ "inline w-4 h-4"]
          strong_ [class_ "font-semibold"] "  Fields"
        div_ [class_ "float-right flex items-center gap-2"] $ do
          a_
            [ class_ "inline-block child-hover cursor-pointer py-2 px-3 rounded border border-gray-200 text-xs hover:shadow shadow-blue-100",
              term "data-tippy-content" "archive"
            ]
            $ do
              img_ [src_ "/assets/svgs/anomalies/archive.svg", class_ "h-4 w-4"]
          a_ [class_ "inline-block child-hover cursor-pointer py-2 px-3 rounded border border-gray-200 text-xs hover:shadow shadow-blue-100"] "Details  → "
      div_ [class_ "flex flex-row basis-0 gap-5"] $ do
        div_ [class_ "flex-1"] $ do
          div_ [class_ "pt-5"] $ do
            p_ [class_ "text-lg"] $ do
              span_ "A new field "
              a_ [class_ "inline-block px-2 text-blue-800"] $ toHtml $ "`" <> fromMaybe "" (anomaly ^? #field % _Just % #keyPathStr) <> "`"
              span_ "was added to"
              a_
                [ class_ "text-blue-800 inline-block px-2",
                  href_ $ maybe "" Endpoints.endpointToUrlPath (anomaly ^. #endpoint)
                ]
                $ toHtml $ "`" <> fromMaybe "" (anomaly ^? #endpoint % _Just % #urlPath) <> "`"
              time_ [class_ "inline-block"] $ toHtml @String $ "on " <> formatTime defaultTimeLocale "%F %R" (anomaly ^. #createdAt)
              span_ ". Was this intended? "
        div_ [class_ "flex-1"] $ do
          let fieldGraphId = "field-" <> maybe "" Fields.fieldIdText (anomaly ^? #field % _Just % #id)
          p_ [class_ "border-0 border-b-2  border-gray-100 border py-2 mb-1"] "Count of field occurence over time"
          div_ [id_ fieldGraphId] ""
          script_
            [text|
                  new FusionCharts({
                    type: "timeseries",
                    renderAt: "$fieldGraphId",
                    width: "95%",
                    height: 350,
                    dataSource: {
                      data: new FusionCharts.DataStore().createDataTable([["2022-01-01T10:00:00Z", "200", 22], ["2022-02-05T10:00:00Z", "200", 2] , ["2022-03-05T10:00:00Z", "200", 42],["2022-04-05T10:00:00Z", "200", 2]], [{
                      "name": "Time",
                      "type": "date",
                      "format": "%Y-%m-%dT%H:%M:%S%Z" // https://www.fusioncharts.com/dev/fusiontime/fusiontime-attributes
                  }, {
                      "name": "StatusCode",
                      "type": "string"
                  },{
                      "name": "Count",
                      "type": "number"
                  }]),
                      chart: {},
                      navigator: {
                          "enabled": 0
                      },
                      series: "StatusCode",
                      yaxis: [
                        {
                          plot:[{
                            value: "Count",
                            type: "smooth-line",
                            title: ""
                          }],
                        }
                      ]
                    }
                  }).render();
                |]
  Anomalies.ATShape -> do
    div_ [class_ "bg-white border border-gray-50 rounded-xl p-5 hover:bg-blue-50"] $ do
      div_ [class_ "clear-both"] $ do
        span_ "Shapes"
        a_ [class_ "inline-block float-right"] "Details  → "
      div_ [class_ "flex flex-row"] $ do
        div_ [class_ "flex-1"] $ do
          p_ "A new field `new_field` as added to `/bla/bla` yesterday. Was this intended? "
        div_ [class_ "flex-1"] $ do
          "graph"
  Anomalies.ATEndpoint -> do
    div_ [class_ "bg-white border border-gray-50 rounded-xl p-5 hover:bg-blue-50"] $ do
      div_ [class_ "clear-both"] $ do
        span_ "Endpoint"
        a_ [class_ "inline-block float-right"] "Details  → "
      div_ [class_ "flex flex-row"] $ do
        div_ [class_ "flex-1"] $ do
          p_ "A new field `new_field` as added to `/bla/bla` yesterday. Was this intended? "
        div_ [class_ "flex-1"] $ do
          "graph"
  Anomalies.ATFormat -> do
    div_ [class_ "bg-white border border-gray-50 rounded-xl p-5 hover:bg-blue-50"] $ do
      div_ [class_ "clear-both"] $ do
        span_ "format"
        a_ [class_ "inline-block float-right"] "Details  → "
      div_ [class_ "flex flex-row"] $ do
        div_ [class_ "flex-1"] $ do
          p_ "A new field `new_field` as added to `/bla/bla` yesterday. Was this intended? "
        div_ [class_ "flex-1"] $ do
          "graph"
  Anomalies.ATUnknown -> do
    div_ [class_ "bg-white border border-gray-50 rounded-xl p-5 hover:bg-blue-50"] $ do
      div_ [class_ "clear-both"] $ do
        span_ "unknown format"
        a_ [class_ "inline-block float-right"] "Details  → "
      div_ [class_ "flex flex-row"] $ do
        div_ [class_ "flex-1"] $ do
          p_ "unknown type"
