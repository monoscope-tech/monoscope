module Pages.Anomalies.AnomalyList (anomalyListGetH, acknowlegeAnomalyGetH, unAcknowlegeAnomalyGetH, anomalyListSlider) where

import Config
import Data.Default (def)
import Data.Text (replace)
import Data.Time (defaultTimeLocale, formatTime)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Optics.Core ((^.))
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Pages.Endpoints.EndpointComponents qualified as EndpointComponents
import Relude
import Relude.Unsafe qualified as Unsafe

acknowlegeAnomalyGetH :: Sessions.PersistentSession -> Projects.ProjectId -> Anomalies.AnomalyId -> DashboardM (Html ())
acknowlegeAnomalyGetH sess pid aid = do
  pool <- asks pool
  liftIO $ withPool pool $ Anomalies.acknowlegeAnomaly aid (sess ^. #userId)
  pure $ anomalyAcknowlegeButton pid aid True

unAcknowlegeAnomalyGetH :: Sessions.PersistentSession -> Projects.ProjectId -> Anomalies.AnomalyId -> DashboardM (Html ())
unAcknowlegeAnomalyGetH sess pid aid = do
  pool <- asks pool
  liftIO $ withPool pool $ Anomalies.unAcknowlegeAnomaly aid
  pure $ anomalyAcknowlegeButton pid aid False

anomalyListGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
anomalyListGetH sess pid = do
  pool <- asks pool
  (project, anomalies) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      anomalies <- Anomalies.selectAnomalies pid
      pure (project, anomalies)

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess,
            currProject = project,
            pageTitle = "Anomalies"
          }
  pure $ bodyWrapper bwconf $ anomalyList anomalies

anomalyList :: Vector Anomalies.AnomalyVM -> Html ()
anomalyList anomalies = do
  div_ [class_ "container mx-auto  px-4 pt-10 pb-24"] $ do
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
      div_ [class_ "col-span-5 space-y-2"] $ do
        when (null anomalies) $ do
          div_ [class_ "flex card-round  text-center justify-center items-center h-32"] $ do
            strong_ "No anomalies yet"
        anomalies & mapM_ (renderAnomaly False)

anomalyListSlider :: Vector Anomalies.AnomalyVM -> Html ()
anomalyListSlider [] = ""
anomalyListSlider anomalies = do
  let anomalyIds = replace "\"" "'" $ show $ fmap (Anomalies.anomalyIdText . (^. #id)) anomalies
  div_ $ do
    script_ [text| var rem = (x,y)=>((x%y)==0?1:(x%y)); |]
    script_
      [type_ "text/hyperscript"]
      [text| 
         init set $$currentAnomaly to 1 then
              set $$anomalyIds to $anomalyIds

          def setAnomalySliderPag()
            set #anomalySliderPagination.innerHTML to $$currentAnomaly+'/'+$$anomalyIds.length
          end

         |]
    div_ [class_ ""] $ do
      div_ [class_ "flex justify-between mt-5 pb-2"] $ do
        div_ [class_ "flex flex-row"] $ do
          img_
            [ src_ "/assets/svgs/cheveron-down.svg",
              class_ "h-4 mr-3 mt-1 w-4",
              [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .parent-slider)|]
            ]
          span_ [class_ "text-lg text-slate-700"] "Ongoing Anomalies and Monitors"
        div_ [class_ "flex flex-row mt-2"] $ do
          a_
            [ class_ "cursor-pointer",
              [__|on click hide #{$anomalyIds[$currentAnomaly]} then
                            set $currentAnomaly to rem($currentAnomaly-1, $anomalyIds.length) then 
                            show #{$anomalyIds[$currentAnomaly]} then 
                            setAnomalySliderPag()|]
            ]
            $ do
              img_ [src_ "/assets/svgs/leftarrow.svg", class_ " m-2"]
          span_ [src_ " mx-4", id_ "anomalySliderPagination"] "1/1"
          a_
            [ class_ "cursor-pointer",
              [__|on click hide #{$anomalyIds[$currentAnomaly]} then
                            set $currentAnomaly to rem($currentAnomaly+1, $anomalyIds.length) then 
                            show #{$anomalyIds[$currentAnomaly]} then
                            setAnomalySliderPag()|]
            ]
            $ do
              img_ [src_ "/assets/svgs/rightarrow.svg", class_ " m-2"]

      div_
        [ class_ "parent-slider",
          [__|init setAnomalySliderPag() then show #{$anomalyIds[$currentAnomaly]} |]
        ]
        $ do
          anomalies & mapM_ (renderAnomaly True)

renderAnomaly :: Bool -> Anomalies.AnomalyVM -> Html ()
renderAnomaly hideByDefault anomaly = do
  let (anomalyTitle, chartTitle, icon) = anomalyDisplayConfig anomaly
  let anomalyId = Anomalies.anomalyIdText (anomaly ^. #id)
  let anomalyGraphId = "field-" <> anomalyId

  div_ [class_ "anomaly-item card-round px-8 py-6 hover:bg-blue-50 parent-hover cursor-pointer", style_ (if hideByDefault then "display:none" else ""), id_ anomalyId] $ do
    div_ [class_ "grid grid-cols-2 gap-5"] $ do
      div_ [class_ ""] $ do
        div_ [class_ "inline-block flex items-center space-x-2"] $ do
          img_ [src_ icon, class_ "inline w-4 h-4"]
          strong_ [class_ "font-semibold"] $toHtml $ "  " <> anomalyTitle
        div_ [class_ "py-3 space-x-2"] $ do
          case anomaly ^. #acknowlegedAt of
            Nothing -> do
              small_ [class_ "bg-red-200 text-red-900 inline-block px-3 rounded-lg"] "ONGOING"
              time_ [class_ "inline-block"] $ toHtml @String $ formatTime defaultTimeLocale "%F %R" (anomaly ^. #createdAt)
              span_ [class_ "inline-block"] "-"
              span_ "present"
            Just ackTime -> do
              small_ [class_ "bg-green-200 text-green-900 inline-block px-3 rounded-lg"] "ACKNOWLEGED"
              time_ [class_ "inline-block"] $ toHtml @String $ formatTime defaultTimeLocale "%F %R" (anomaly ^. #createdAt)
              span_ [class_ "inline-block"] "-"
              time_ [class_ "inline-block"] $ toHtml @String $ formatTime defaultTimeLocale "%F %R" ackTime
        div_ [class_ "pt-5 space-y-1"] $ do
          div_ $ do
            span_ "endpoint: "
            a_
              [ class_ "text-blue-800 inline-block px-2",
                href_ $ Endpoints.endpointUrlPath (anomaly ^. #projectId) (Unsafe.fromJust $ anomaly ^. #endpointId)
              ]
              $ toHtml $ fromMaybe "" (anomaly ^. #endpointMethod) <> "  " <> fromMaybe "" (anomaly ^. #endpointUrlPath)
          case anomaly ^. #anomalyType of
            Anomalies.ATShape -> do
              div_ $ do
                span_ "shape_id: "
                a_
                  [ class_ "text-blue-800 inline-block px-2"
                  ]
                  $ toHtml $ "`" <> maybe "" Shapes.shapeIdText (anomaly ^. #shapeId) <> "`"
            Anomalies.ATEndpoint -> ""
            Anomalies.ATFormat -> do
              div_ $ do
                span_ "field_path: "
                span_ $ toHtml $ fromMaybe "" (anomaly ^. #fieldKey)
              div_ $ do
                span_ "type: "
                maybe "" EndpointComponents.fieldTypeToDisplay (anomaly ^. #formatType)
              div_ $ do
                span_ "format: "
                span_ $ toHtml $ fromMaybe "" (anomaly ^. #fieldFormat)
            Anomalies.ATField -> ""
            Anomalies.ATUnknown -> ""
        p_ [class_ "pt-3 text-lg"] "Was this intended? "
      div_ [class_ "clear-both"] $ do
        div_ [class_ "float-right flex items-center gap-2"] $ do
          a_
            [ class_ "inline-block child-hover cursor-pointer py-2 px-3 rounded border border-gray-200 text-xs hover:shadow shadow-blue-100",
              term "data-tippy-content" "archive"
            ]
            $ do
              img_ [src_ "/assets/svgs/anomalies/archive.svg", class_ "h-4 w-4"]
          anomalyAcknowlegeButton
            (anomaly ^. #projectId)
            (anomaly ^. #id)
            (isJust (anomaly ^. #acknowlegedAt))

        p_ [class_ "border-0 border-b-2  border-gray-100 border py-2 mb-1"] $ toHtml chartTitle
        div_ [id_ anomalyGraphId, style_ "height:250px", class_ "w-full"] ""
        script_ $ anomalyChartScript anomaly anomalyGraphId

anomalyAcknowlegeButton :: Projects.ProjectId -> Anomalies.AnomalyId -> Bool -> Html ()
anomalyAcknowlegeButton pid aid acked = do
  let acknowlegeAnomalyEndpoint = "/p/" <> Projects.projectIdText pid <> "/anomalies/" <> Anomalies.anomalyIdText aid <> if acked then "/unacknowlege" else "/acknowlege"
  a_
    [ class_ $
        "inline-block child-hover cursor-pointer py-2 px-3 rounded border border-gray-200 text-xs hover:shadow shadow-blue-100 "
          <> (if acked then "bg-green-100 text-green-900" else " "),
      term "data-tippy-content" "acknowlege anomaly",
      hxGet_ acknowlegeAnomalyEndpoint,
      hxSwap_ "outerHTML"
    ]
    if acked then "✓ Acknowleged" else "✓ Acknowlege"

anomalyChartScript :: Anomalies.AnomalyVM -> Text -> Text
anomalyChartScript anomaly anomalyGraphId =
  let timeSeriesData = fromMaybe "[]" $ anomaly ^. #timeSeries
   in -- Adding the current day and time to the end of the chart data, so that the chart is scaled to include the current day/time
      -- currentISOTimeStringVar is declared on every page, in case they need a string for the current time in ISO format
      [text|
      new FusionCharts({
        type: "timeseries",
        renderAt: "$anomalyGraphId",
        width: "100%",
        height: 250,
        dataSource: {
          data: new FusionCharts.DataStore().createDataTable(($timeSeriesData).concat([[currentISOTimeStringVar, 0]]), 
          [{"name": "Time",
            "type": "date",
            "format": "%Y-%m-%dT%H:%M:%S%Z" // https://www.fusioncharts.com/dev/fusiontime/fusiontime-attributes
          },{"name": "Count","type": "number"}]),
          chart: {},
          navigator: {"enabled": 0},
          series: "StatusCode",
          yaxis: [{plot:[{value: "Count",type: "smooth-line"}],title: ""}]
        }
      }).render();
     |]

anomalyDisplayConfig :: Anomalies.AnomalyVM -> (Text, Text, Text)
anomalyDisplayConfig anomaly = case anomaly ^. #anomalyType of
  Anomalies.ATField -> ("New Field Found", "Field Occurences over Time", "/assets/svgs/anomalies/fields.svg")
  Anomalies.ATShape -> ("New Req/Resp Shape", "Shape Occurences over Time vs Total by all Shapes", "/assets/svgs/anomalies/fields.svg")
  Anomalies.ATEndpoint -> ("New Endpoint Found", "Endpoint occurences over time vs Total by all Endpoints", "/assets/svgs/endpoint.svg")
  Anomalies.ATFormat -> ("Field Format Detected", "Requests with the new field format", "/assets/svgs/anomalies/fields.svg")
  Anomalies.ATUnknown -> ("Unknown anomaly", "Unknown anomaly", "/assets/svgs/anomalies/fields.svg")
