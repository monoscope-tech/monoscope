module Pages.Anomalies.AnomalyList (anomalyListGetH, acknowlegeAnomalyGetH, unAcknowlegeAnomalyGetH, anomalyListSlider) where

import Config
import Data.Default (def)
import Data.Text (replace)
import Data.Text qualified as T
import Data.Time (ZonedTime, defaultTimeLocale, formatTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript
import Models.Apis.Anomalies qualified as Anomalies
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Optics.Core ((^.))
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Pages.Charts.Charts qualified as Charts
import Relude

acknowlegeAnomalyGetH :: Sessions.PersistentSession -> Projects.ProjectId -> Anomalies.AnomalyId -> DashboardM (Html ())
acknowlegeAnomalyGetH sess pid aid = do
  pool <- asks pool
  liftIO $ withPool pool $ Anomalies.acknowlegeAnomaly aid (sess.userId)
  pure $ anomalyAcknowlegeButton pid aid True

unAcknowlegeAnomalyGetH :: Sessions.PersistentSession -> Projects.ProjectId -> Anomalies.AnomalyId -> DashboardM (Html ())
unAcknowlegeAnomalyGetH sess pid aid = do
  pool <- asks pool
  liftIO $ withPool pool $ Anomalies.unAcknowlegeAnomaly aid
  pure $ anomalyAcknowlegeButton pid aid False

anomalyListGetH :: Sessions.PersistentSession -> Projects.ProjectId -> Maybe Text -> Maybe Text -> DashboardM (Html ())
anomalyListGetH sess pid layoutM hxRequestM = do
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
  case (layoutM, hxRequestM) of
    (Just "slider", Just "true") -> pure $ anomalyListSlider anomalies
    _ -> pure $ bodyWrapper bwconf $ anomalyListPage anomalies

anomalyListPage :: Vector Anomalies.AnomalyVM -> Html ()
anomalyListPage anomalies = div_ [class_ "container mx-auto  px-4 pt-10 pb-24"] $ do
  div_ [class_ "flex justify-between"] $ do
    h3_ [class_ "text-xl text-slate-700 flex place-items-center"] "Anomalies"
    div_ [class_ "flex flex-row"] $ do
      button_ [class_ "bg-white rounded-xl py-2 px-4 m-3 h-10 flex flex-row"] $ do
        img_ [src_ "/assets/svgs/download.svg", class_ "h-4 w-6"]
        span_ [class_ "text-sm"] "Export"
        img_ [src_ "/assets/svgs/cheveron-down.svg", class_ "h-3 w-3 mt-1 mx-1"]
      button_ [class_ "bg-blue-700 h-10  px-2 rounded-xl py-1 mt-3 "] $ img_ [src_ "/assets/svgs/white-plus.svg", class_ "text-white h-4 w-6 text-bold"]
  div_ [class_ "grid grid-cols-5"] $ anomalyList anomalies

anomalyList :: Vector Anomalies.AnomalyVM -> Html ()
anomalyList anomalies = div_ [class_ "col-span-5 bg-white divide-y border rounded-md "] $ do
  div_ [class_ "flex py-3 gap-8 items-center  bg-gray-50"] do
    div_ [class_ "h-4 flex space-x-3 w-8"] do
      a_ [class_ " w-2 h-full"] ""
      input_ [term "aria-label" "Select Issue", type_ "checkbox"]
    div_ [class_ "space-y-3 grow"] ""
    div_ [class_ "flex justify-center font-base w-64 content-between gap-14"] do
      span_ "GRAPH"
      div_ [class_ " space-x-2"] $ do
        a_ "24h"
        a_ [class_ "cursor-pointer font-medium"] "14d"
    div_ [class_ "w-36 flex items-center justify-center"] $ span_ [class_ "font-base"] "EVENTS"

  when (null anomalies) $ div_ [class_ "flex card-round  text-center justify-center items-center h-32"] $ do
    strong_ "No anomalies yet"
  mapM_ (renderAnomaly False) anomalies

anomalyListSlider :: Vector Anomalies.AnomalyVM -> Html ()
anomalyListSlider [] = ""
anomalyListSlider anomalies = do
  let anomalyIds = replace "\"" "'" $ show $ fmap (Anomalies.anomalyIdText . (^. #id)) anomalies
  div_ $ do
    script_ [text| var rem = (x,y)=>((x%y)==0?1:(x%y)); |]
    script_
      [type_ "text/hyperscript"]
      [text| 
         init set $$currentAnomaly to 0 then
              set $$anomalyIds to $anomalyIds

          def setAnomalySliderPag()
            set #anomalySliderPagination.innerHTML to ($$currentAnomaly+1)+'/'+$$anomalyIds.length
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
                            js($currentAnomaly, $anomalyIds) return (Math.max(0, $currentAnomaly-1) % $anomalyIds.length) end then 
                            set $currentAnomaly to it then
                            show #{$anomalyIds[$currentAnomaly]} then 
                            setAnomalySliderPag()|]
            ]
            $ img_ [src_ "/assets/svgs/leftarrow.svg", class_ " m-2"]
          span_ [src_ " mx-4", id_ "anomalySliderPagination"] "1/1"
          a_
            [ class_ "cursor-pointer",
              [__|on click hide #{$anomalyIds[$currentAnomaly]} then
                            js($currentAnomaly, $anomalyIds) return (($currentAnomaly+1) % $anomalyIds.length) end then 
                            set $currentAnomaly to it then
                            show #{$anomalyIds[$currentAnomaly]} then
                            setAnomalySliderPag()|]
            ]
            $ img_ [src_ "/assets/svgs/rightarrow.svg", class_ " m-2"]

      div_
        [ class_ "parent-slider",
          [__|init setAnomalySliderPag() then show #{$anomalyIds[$currentAnomaly]} |]
        ]
        $ mapM_ (renderAnomaly True) anomalies

anomalyTimeline :: ZonedTime -> Maybe ZonedTime -> Html ()
anomalyTimeline createdAt acknowlegedAt = small_ [class_ "inline-block  px-8 py-6 space-x-2"] $ case acknowlegedAt of
  Nothing -> do
    span_ [class_ "bg-red-200 text-red-900 inline-block px-3 rounded-lg"] "ONGOING"
    time_ [class_ "inline-block"] $ toHtml @String $ formatTime defaultTimeLocale "%F %R" createdAt
    span_ [class_ "inline-block"] "-"
    span_ "present"
  Just ackTime -> do
    span_ [class_ "bg-green-200 text-green-900 inline-block px-3 rounded-lg"] "ACKNOWLEGED"
    time_ [class_ "inline-block"] $ toHtml @String $ formatTime defaultTimeLocale "%F %R" createdAt
    span_ [class_ "inline-block"] "-"
    time_ [class_ "inline-block"] $ toHtml @String $ formatTime defaultTimeLocale "%F %R" ackTime

shapeParameterStats_ :: Int -> Int -> Int -> Html ()
shapeParameterStats_ newF deletedF updatedFF = div_ [class_ "py-4 inline-block"] do
  div_ [class_ "grid grid-cols-3 gap-2 text-center text-xs w-96"] do
    div_ [class_ "p-2 bg-emerald-100 text-emerald-900 border border-emerald-300"] do
      div_ [class_ "text-base"] $ toHtml @String $ show newF
      div_ "new fields"
    div_ [class_ " p-2 bg-slate-100 text-slate-900 border border-slate-300"] do
      div_ [class_ "text-base"] $ toHtml @String $ show updatedFF
      div_ "updated fields"
    div_ [class_ "p-2  bg-rose-100 text-rose-900 border border-rose-300"] do
      div_ [class_ "text-base"] $ toHtml @String $ show deletedF
      div_ [] "deleted fields"

anomalyItem :: Bool -> Anomalies.AnomalyVM -> Text -> Text -> Maybe (Html ()) -> Maybe (Html ()) -> Html ()
anomalyItem hideByDefault anomaly icon title subTitle content = do
  let anomalyId = Anomalies.anomalyIdText (anomaly.id)
  div_ [class_ $ "flex py-4 gap-8 " <> if hideByDefault then "card-round bg-white px-5" else "", style_ (if hideByDefault then "display:none" else ""), id_ anomalyId] do
    div_ [class_ $ "h-4 flex self-start space-x-3 w-8 " <> if hideByDefault then "hidden" else ""] do
      a_ [class_ "bg-red-800 w-2 h-full"] ""
      input_ [term "aria-label" "Select Issue", type_ "checkbox"]
    div_ [class_ "space-y-3 grow"] do
      div_ [class_ "space-x-3"] do
        a_ [class_ "inline-block font-bold text-blue-700 space-x-2"] do
          img_ [src_ icon, class_ "inline w-4 h-4"]
          span_ $ toHtml title
        small_ [class_ "inline-block text-gray-800"] $ fromMaybe (toHtml @String "") subTitle
      fromMaybe (toHtml @String "") content
      div_ [class_ "text-xs decoration-dotted underline-offset-2 space-x-4 "] do
        span_ [class_ "bg-red-50 p-1"] "ongoing"
        span_ [class_ "inline-block space-x-1"] do
          img_ [src_ icon, class_ "inline w-4 h-4"]
          span_ [class_ "decoration-black underline"] "18hr ago"
          span_ "|"
          span_ [class_ "decoration-black underline"] "2mins ago"
      div_ [class_ "flex items-center gap-2 mt-5"] do
        a_
          [ class_ "inline-block xchild-hover cursor-pointer py-2 px-3 rounded border border-gray-200 text-xs hover:shadow shadow-blue-100",
            term "data-tippy-content" "archive"
          ]
          $ img_ [src_ "/assets/svgs/anomalies/archive.svg", class_ "h-4 w-4"]
        anomalyAcknowlegeButton
          (anomaly.projectId)
          (anomaly.id)
          (isJust (anomaly.acknowlegedAt))
    let chartQuery = Just $ anomaly2ChartQuery anomaly.anomalyType anomaly.targetHash
    div_ [class_ "flex items-center justify-center "] $ div_ [class_ "w-64 h-28"] $ Charts.throughput anomaly.projectId anomaly.targetHash chartQuery Nothing (12 * 60) (Just 28) False
    div_ [class_ "w-36 flex items-center justify-center"] $ span_ [class_ "tabular-nums text-xl"] "349"

anomaly2ChartQuery :: Anomalies.AnomalyTypes -> Text -> Charts.QueryBy
anomaly2ChartQuery Anomalies.ATEndpoint = Charts.QBEndpointHash
anomaly2ChartQuery Anomalies.ATShape = Charts.QBShapeHash
anomaly2ChartQuery Anomalies.ATFormat = Charts.QBFormatHash
anomaly2ChartQuery Anomalies.ATUnknown = error "Should not convert unknown anomaly to chart"
anomaly2ChartQuery Anomalies.ATField = error "Should not see field anomaly to chart in anomaly UI. ATField gets hidden under shape"

renderAnomaly :: Bool -> Anomalies.AnomalyVM -> Html ()
renderAnomaly hideByDefault anomaly = do
  let (anomalyTitle, icon) = anomalyDisplayConfig anomaly
  case anomaly.anomalyType of
    Anomalies.ATEndpoint -> do
      let endpointTitle = fromMaybe "" (anomaly.endpointMethod) <> "  " <> fromMaybe "" (anomaly.endpointUrlPath)
      anomalyItem hideByDefault anomaly icon anomalyTitle (Just $ toHtml endpointTitle) Nothing
    Anomalies.ATShape -> do
      let endpointTitle = fromMaybe "" (anomaly.endpointMethod) <> "  " <> fromMaybe "" (anomaly.endpointUrlPath)
      let subTitle = span_ [class_ "space-x-2"] do
            a_ [class_ "cursor-pointer"] $ toHtml anomaly.targetHash
            span_ [] "in"
            span_ [] $ toHtml endpointTitle
      let shapeContent = small_ [class_ "block"] do
            shapeParameterStats_ (length anomaly.shapeNewUniqueFields) (length anomaly.shapeDeletedFields) (length anomaly.shapeUpdatedFieldFormats)
      anomalyItem hideByDefault anomaly icon anomalyTitle (Just subTitle) (Just shapeContent)
    Anomalies.ATFormat -> do
      let endpointTitle = toHtml $ fromMaybe "" (anomaly.endpointMethod) <> "  " <> fromMaybe "" (anomaly.endpointUrlPath)
      let subTitle = span_ [class_ "space-x-2"] do
            a_ [class_ "cursor-pointer"] $ toHtml $ fromMaybe "" anomaly.fieldKeyPath
            span_ [] "in"
            span_ [] $ toHtml endpointTitle
      let formatContent = div_ [class_ "block"] do
            div_ [class_ "text-sm"] do
              div_ do
                small_ "current format: "
                span_ $ maybe "" show anomaly.formatType
              div_ do
                small_ "previous formats: "
                span_ "" -- TODO: Should be comma separated list of formats for that field.
              div_ do
                small_ "examples: "
                small_ $ toHtml $ maybe "" (T.intercalate ", " . Vector.toList) anomaly.formatExamples
      anomalyItem hideByDefault anomaly icon anomalyTitle (Just subTitle) (Just formatContent)
    _ -> ""

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
  let timeSeriesData = fromMaybe "[]" $ anomaly.timeSeries
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

anomalyDisplayConfig :: Anomalies.AnomalyVM -> (Text, Text)
anomalyDisplayConfig anomaly = case anomaly.anomalyType of
  Anomalies.ATField -> ("New Field Found", "/assets/svgs/anomalies/fields.svg")
  Anomalies.ATShape -> ("New Request Shape", "/assets/svgs/anomalies/fields.svg")
  Anomalies.ATEndpoint -> ("New Endpoint", "/assets/svgs/endpoint.svg")
  Anomalies.ATFormat -> ("Modified field", "/assets/svgs/anomalies/fields.svg")
  Anomalies.ATUnknown -> ("Unknown anomaly", "/assets/svgs/anomalies/fields.svg")
