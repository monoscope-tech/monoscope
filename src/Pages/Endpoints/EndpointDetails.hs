{-# LANGUAGE TupleSections #-}

module Pages.Endpoints.EndpointDetails (endpointDetailsH, fieldDetailsPartialH) where

import Config
import Data.Aeson qualified as AE
import Data.Default (def)
import Data.Map qualified as Map
import Data.Text as T (breakOnAll, dropWhile, isInfixOf, replace, splitOn, toLower)
import Data.Time (defaultTimeLocale, formatTime)
import Data.UUID as UUID
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity.DBT (withPool)
import Fmt
import Lucid
import Lucid.HTMX
import Lucid.Hyperscript.QuasiQuoter
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields qualified as Fields
import Models.Apis.Formats qualified as Formats
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation
import Optics.Core ((^.))
import Pages.Anomalies.AnomalyList qualified as AnomaliesList
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Pages.Endpoints.EndpointComponents qualified as EndpointComponents
import Relude
import Relude.Unsafe qualified as Unsafe

fieldDetailsPartialH :: Sessions.PersistentSession -> Projects.ProjectId -> Fields.FieldId -> DashboardM (Html ())
fieldDetailsPartialH sess pid fid = do
  pool <- asks pool
  (fieldsM, formats) <- liftIO $
    withPool pool $ do
      field <- Fields.fieldById fid
      formats <- Formats.formatsByFieldId fid
      pure (field, formats)
  case fieldsM of
    Nothing -> pure ""
    Just field -> pure $ fieldDetailsView field formats

fieldDetailsView :: Fields.Field -> Vector Formats.Format -> Html ()
fieldDetailsView field formats = do
  img_ [src_ "/assets/svgs/ellipsis.svg", class_ "my-2 float-right"]
  section_ [class_ "space-y-6"] $ do
    div_ $ do
      h6_ [class_ "text-slate-700 text-xs"] "FIELD NAME"
      h3_ [class_ "text-lg text-slate-700"] $ toHtml $ field ^. #key
    div_ $ do
      h6_ [class_ "text-slate-700 text-xs"] "FIELD PATH"
      h3_ [class_ "text-lg text-slate-700"] $ toHtml $ field ^. #keyPathStr
    div_ [class_ "flex flex-row gap-9"] $ do
      div_ $ do
        h6_ [class_ "text-slate-700 text-xs"] "FIELD CATEGORY"
        h4_ [class_ "text-base text-slate-700"] $ fieldCategoryToDisplay $ field ^. #fieldCategory
      div_ [class_ "mx-5"] $ do
        h6_ [class_ "text-slate-700 text-xs"] "FORMAT OVERRIDE"
        h4_ [class_ "text-base text-slate-700"] $ toHtml $ fromMaybe "[unset]" (field ^. #fieldTypeOverride)
    div_ $ do
      h5_ [class_ "text-sm text-slate-700"] "DETECTED FIELD FORMATS AND TYPES"
      div_ [class_ "space-y-2"] $ do
        formats & mapM_ \formatV -> do
          div_ [class_ "border-l-slate-200 border-l-2 pl-2 py-2"] $ do
            div_ [class_ "flex flex-row gap-9"] $ do
              div_ [class_ "space-y-2"] $ do
                h6_ [class_ "text-slate-700 text-xs"] "TYPE"
                h4_ [class_ "text-base text-slate-700"] $ EndpointComponents.fieldTypeToDisplay $ formatV ^. #fieldType
              div_ [class_ "mx-5 space-y-2"] $ do
                h6_ [class_ "text-slate-700 text-xs"] "FORMAT"
                h4_ [class_ "text-base text-slate-700"] $ toHtml $ formatV ^. #fieldFormat
            h6_ [class_ "text-slate-600 mt-4 text-xs"] "EXAMPLE VALUES"
            ul_ [class_ "list-disc"] $ do
              formatV ^. #examples & mapM_ \ex -> do
                li_ [class_ "ml-10 text-slate-700 text-sm"] $ toHtml ex
    div_ [class_ "flex flex-row justify-between mt-10 "] $ do
      div_ [class_ " "] $ do
        h4_ [class_ "text-sm text-slate-700 mb-2"] "CREATION DATE"
        div_ [class_ "flex border border-gray-200 m-1 rounded-xl p-2"] $ do
          img_ [src_ "/assets/svgs/calender.svg", class_ "h-4 mr-2 w-4"]
          span_ [class_ "text-xs"] $ toHtml $ formatTime defaultTimeLocale "%b %d, %Y %R" (field ^. #createdAt)
      div_ [class_ " "] $ do
        h4_ [class_ "text-sm text-slate-700 mb-2"] "LAST CHANGE"
        div_ [class_ "flex border border-gray-200 m-1 justify-between rounded-xl p-2"] $ do
          img_ [src_ "/assets/svgs/calender.svg", class_ "h-4 mr-2 w-4"]
          span_ [class_ "text-xs"] $ toHtml $ formatTime defaultTimeLocale "%b %d, %Y %R" (field ^. #updatedAt)
    h6_ [class_ "mt-5 text-sm text-slate-700 mb-2"] "DESCRIPTION"
    p_ [class_ "text-gray-400 text-sm"] $ toHtml $ field ^. #description

-- | endpointDetailsH is the main handler for the endpoint details page.
-- It reuses the fieldDetailsView as well, which is used for the side navigation on the page and also exposed un the fieldDetailsPartialH endpoint
endpointDetailsH :: Sessions.PersistentSession -> Projects.ProjectId -> Endpoints.EndpointId -> DashboardM (Html ())
endpointDetailsH sess pid eid = do
  pool <- asks pool
  (endpoint, project, fieldsMap, reqsByStatsByMin, reqLatenciesRolledByStepsLabeled, anomalies) <- liftIO $
    withPool pool $ do
      endpoint <- Unsafe.fromJust <$> Endpoints.endpointRequestStatsByEndpoint eid
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      fieldsMap <- Fields.groupFieldsByCategory <$> Fields.selectFields eid
      reqsByStatsByMin <- RequestDumps.selectRequestsByStatusCodesStatByMin pid (endpoint ^. #urlPath) (endpoint ^. #method)

      let maxV = round (endpoint ^. #max) :: Int
      let steps = (maxV `quot` 100) :: Int
      let steps' = if steps == 0 then 100 else steps
      reqLatenciesRolledBySteps <- RequestDumps.selectReqLatenciesRolledBySteps maxV steps' pid (endpoint ^. #urlPath) (endpoint ^. #method)

      let reqLatencyPercentileSteps =
            ( (round (endpoint ^. #max) `quot` steps') * steps',
              (round (endpoint ^. #p90) `quot` steps') * steps',
              (round (endpoint ^. #p75) `quot` steps') * steps',
              (round (endpoint ^. #p50) `quot` steps') * steps'
            )
      let reqLatenciesRolledByStepsLabeled = Vector.toList reqLatenciesRolledBySteps & map \(x, y) -> RequestDumps.labelRequestLatency reqLatencyPercentileSteps (x, y)
      anomalies <- Anomalies.selectOngoingAnomaliesForEndpoint pid eid
      pure (endpoint, project, fieldsMap, reqsByStatsByMin, concat reqLatenciesRolledByStepsLabeled, anomalies)

  let reqsByStatsByMinJ = decodeUtf8 $ AE.encode reqsByStatsByMin
  let reqLatenciesRolledByStepsJ = decodeUtf8 $ AE.encode reqLatenciesRolledByStepsLabeled
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess,
            currProject = project,
            pageTitle = "Endpoint Details",
            menuItem = Just "Endpoints"
          }
  pure $ bodyWrapper bwconf $ endpointDetails endpoint fieldsMap reqsByStatsByMinJ reqLatenciesRolledByStepsJ anomalies

endpointDetails :: EndpointRequestStats -> Map Fields.FieldCategoryEnum [Fields.Field] -> Text -> Text -> Vector Anomalies.AnomalyVM -> Html ()
endpointDetails endpoint fieldsM reqsByStatsByMinJ reqLatenciesRolledByStepsJ anomalies = do
  div_ [class_ "w-full flex flex-row h-full overflow-hidden"] $ do
    div_ [class_ "w-2/3 p-5 h-full overflow-y-scroll"] $ do
      div_ [class_ "flex flex-row justify-between mb-10"] $ do
        div_ [class_ "flex flex-row place-items-center text-lg font-medium"] $ do
          h3_ [class_ "text-lg text-slate-700"] $ do
            span_ [class_ $ "p-1 endpoint endpoint-" <> toLower (endpoint ^. #method)] $ toHtml $ (endpoint ^. #method) <> " "
            strong_ [class_ "inconsolata text-xl"] $ toHtml (endpoint ^. #urlPath)
          img_ [src_ "/assets/svgs/cheveron-down.svg", class_ " h-4 w-4 m-2"]
        div_ [class_ "flex flex-row"] $ do
          a_ [href_ ""] $ do
            button_ [class_ "bg-white rounded-lg h-10 mt-1 "] $ do
              img_ [src_ "/assets/svgs/filter.svg", class_ "h-6 w-6 m-2"]
          a_ [href_ ""] $ do
            button_ [class_ "bg-blue-700 flex h-11 flex-row mx-2 px-3 rounded-xl py-2"] $ do
              h3_ [class_ "text-white text-sm text-bold mx-2 mt-1"] "Download Swagger"
              div_ [class_ "bg-blue-900 p-1 rounded-lg ml-2"] $ do
                img_ [src_ "/assets/svgs/whitedown.svg", class_ "text-white h-2 w-2 m-1"]
      div_ [class_ "space-y-16 pb-20"] $ do
        section_ $ AnomaliesList.anomalyListSlider anomalies
        endpointStats endpoint
        reqResSection "Request" True fieldsM
        reqResSection "Response" False fieldsM
    aside_
      [ class_ "w-1/3 h-full overflow-y-scroll bg-white border border-gray-200 p-5 sticky top-0",
        id_ "detailSidebar"
      ]
      $ do
        div_ [class_ "h-full flex flex-col items-center justify-center"] $ do
          img_ [class_ "w-36", src_ "/assets/svgs/tasks.svg"]
          h3_ [class_ "mt-2 text-lg font-medium text-gray-900"] "Nothing selected"
          p_ [class_ "mt-1 text-sm text-gray-500"] "Select a field or similar item on the left"
          p_ [class_ "mt-1 text-sm text-gray-500"] "to view more details about it here."

    script_
      [type_ "text/hyperscript"]
      [text| 
        def collapseUntil(elem, level)
          set nxtElem to (next <[data-depth]/> from elem) then
          if nxtElem's @data-depth is greater than level 
            then toggle .hidden on nxtElem 
            then collapseUntil(nxtElem, level)
        end
        |]

    script_
      [text|
        new FusionCharts({
          type: "timeseries",
          renderAt: "reqByStatusCode",
          width: "95%",
          height: 350,
          dataSource: {
            data: new FusionCharts.DataStore().createDataTable($reqsByStatsByMinJ, [{
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
                  type: "column"
                }],
                title: ""
              }
            ]
          }
        }).render();


        new FusionCharts({
          type: "column2d",
          renderAt: "reqsLatencyHistogram",
          width: "100%",
          // height: "auto",
          dataSource: {
            data:  $reqLatenciesRolledByStepsJ,
            "chart": {
                "theme": "fusion",
                "xAxisName": "Latency in ms",
                "yAxisName": "Count",
                // "numberSuffix": "K"
            },
          }
        }).render();

      |]

endpointStats :: Endpoints.EndpointRequestStats -> Html ()
endpointStats enpStats =
  section_ [class_ "space-y-3"] $ do
    div_ [class_ "flex justify-between mt-5"] $ do
      div_ [class_ "flex flex-row"] $ do
        img_
          [ src_ "/assets/svgs/cheveron-down.svg",
            class_ "h-4 mr-3 mt-1 w-4 cursor-pointer",
            [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .endpointStatsSubSection)|]
          ]
        span_ [class_ "text-lg text-slate-700"] "Endpoint Stats"
    div_ [class_ "grid grid-cols-3  gap-5 endpointStatsSubSection"] $ do
      div_ [class_ "col-span-1 content-between space-y-2"] $ do
        --
        div_ [class_ " row-span-1 col-span-1 card-round p-5 flex flex-row content-between "] $ do
          div_ $ do
            span_ "Total Anomalies"
            div_ [class_ "inline-block flex flex-row items-baseline"] $ do
              strong_ [class_ "text-xl"] $ toHtml @Text $ fmt $ commaizeF (enpStats ^. #ongoingAnomalies)
              small_ $ toHtml @Text $ fmt ("/" +| commaizeF (enpStats ^. #ongoingAnomaliesProj))
        div_ [class_ " row-span-1 col-span-1 card-round p-5 flex flex-row content-between "] $ do
          div_ $ do
            span_ "Total Requests"
            div_ [class_ "inline-block flex flex-row items-baseline"] $ do
              strong_ [class_ "text-xl"] $ toHtml @Text $ fmt $ commaizeF (enpStats ^. #totalRequests)
              small_ $ toHtml @Text $ fmt ("/" +| commaizeF (enpStats ^. #totalRequestsProj))

      div_ [class_ "col-span-2 bg-white  border border-gray-100  row-span-2 rounded-2xl p-3"] $ do
        div_ [class_ "p-4"] $ do
          select_ [] $ do
            option_ "Reqs by Status code"
            option_ "Avg Reqs per minute"
        div_ [id_ "reqByStatusCode", class_ ""] ""
      div_ [class_ "col-span-3 bg-white   border border-gray-100  rounded-xl py-3 px-6"] $ do
        div_ [class_ "p-4"] $ do
          select_ [] $ do
            option_ "Request Latency Distribution"
            option_ "Avg Reqs per minute"
        div_ [class_ "flex flex-row gap-8"] $ do
          div_ [id_ "reqsLatencyHistogram", class_ "grow"] ""
          div_ [class_ "flex-1 space-y-4 min-w-[20%]"] $ do
            strong_ [class_ "block text-right"] "Latency Percentiles"
            ul_ [class_ "space-y-1 divide-y divide-slate-100"] $ do
              percentileRow "max" $ enpStats ^. #max
              percentileRow "p99" $ enpStats ^. #p99
              percentileRow "p95" $ enpStats ^. #p95
              percentileRow "p90" $ enpStats ^. #p90
              percentileRow "p75" $ enpStats ^. #p75
              percentileRow "p50" $ enpStats ^. #p50
              percentileRow "min" $ enpStats ^. #min

percentileRow :: Text -> Double -> Html ()
percentileRow key p = do
  li_ [class_ "flex flex-row content-between justify-between"] $ do
    span_ [class_ "inline-block"] $ toHtml key
    span_ [class_ "inline-block monospace"] $ do
      span_ $ toHtml ((fmt $ fixedF 2 p) :: Text)
      span_ "ms"

-- NB: We could enable the fields cycling functionality using the groups of response list functionality on the endpoint.
-- So we go through the list and in each request or response view, only show the fields that appear in the field list.
-- We can enable a view to show all the request/response options.
reqResSection :: Text -> Bool -> Map Fields.FieldCategoryEnum [Fields.Field] -> Html ()
reqResSection title isRequest fieldsM =
  section_ [class_ "space-y-3"] $ do
    div_ [class_ "flex justify-between mt-5"] $ do
      div_ [class_ "flex flex-row"] $ do
        a_ [class_ "cursor-pointer", [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .reqResSubSection)|]] $
          img_
            [ src_ "/assets/svgs/cheveron-down.svg",
              class_ "h-4 mr-3 mt-1 w-4"
            ]
        span_ [class_ "text-lg text-slate-700"] $ toHtml title
      div_ [class_ "flex flex-row mt-2"] $ do
        img_ [src_ "/assets/svgs/leftarrow.svg", class_ " m-2"]
        span_ [src_ " mx-4"] "1/1"
        img_ [src_ "/assets/svgs/rightarrow.svg", class_ " m-2"]
    div_ [class_ "bg-white border border-gray-100 rounded-xl py-10 px-5 space-y-6 reqResSubSection"] $ do
      if isRequest
        then do
          subSubSection (title <> " Path Params") (Map.lookup Fields.FCPathParam fieldsM)
          subSubSection (title <> " Query Params") (Map.lookup Fields.FCQueryParam fieldsM)
          subSubSection (title <> " Headers") (Map.lookup Fields.FCRequestHeader fieldsM)
          subSubSection (title <> " Body") (Map.lookup Fields.FCRequestBody fieldsM)
        else do
          subSubSection (title <> " Headers") (Map.lookup Fields.FCResponseHeader fieldsM)
          subSubSection (title <> " Body") (Map.lookup Fields.FCResponseBody fieldsM)

-- | subSubSection ..
subSubSection :: Text -> Maybe [Fields.Field] -> Html ()
subSubSection title fieldsM = do
  case fieldsM of
    Nothing -> ""
    Just fields -> do
      div_ [class_ "space-y-1"] $ do
        div_ [class_ "flex flex-row items-center"] $ do
          img_
            [ src_ "/assets/svgs/cheveron-down.svg",
              class_ "h-6 mr-3 w-6 p-1 cursor-pointer",
              [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .subSectionContent)|]
            ]
          div_ [class_ "bg-gray-100 px-10 rounded-xl w-full p-4 text-sm text-slate-700"] $ toHtml title
        div_ [class_ "space-y-1 subSectionContent"] $ do
          fieldsToNormalized fields & mapM_ \(key, fieldM) -> do
            let segments = splitOn "." key
            let depth = length segments
            let depthPadding = "margin-left:" <> show (20 + (depth * 20)) <> "px"
            let displayKey = replace "»" "" $ last ("" :| segments)
            case fieldM of
              Nothing -> do
                a_
                  [ class_ "flex flex-row items-center",
                    style_ depthPadding,
                    [__| on click toggle .neg-rotate-90 on <.chevron/> in me then collapseUntil((me), (my @data-depth))  |],
                    term "data-depth" $ show depth
                  ]
                  $ do
                    img_ [src_ "/assets/svgs/cheveron-down.svg", class_ "h-6 w-6 mr-1 chevron cursor-pointer p-1"]
                    div_ [class_ "border flex flex-row border-gray-100 px-5 py-2 rounded-xl w-full"] $ do
                      input_ [type_ "checkbox", class_ " mr-12"]
                      span_ [class_ "grow text-sm text-slate-700 inline-flex items-center"] $ toHtml displayKey
                      span_ [class_ "text-sm text-slate-600 mx-12 inline-flex items-center"] $ if "»" `isInfixOf` key then "[]" else "{}"
              Just field -> do
                a_
                  [ hxGet_ $ "/p/" <> Projects.projectIdText (field ^. #projectId) <> "/fields/" <> UUID.toText (Fields.unFieldId $ field ^. #id),
                    hxTarget_ "#detailSidebar",
                    class_ "flex flex-row cursor-pointer",
                    style_ depthPadding,
                    term "data-depth" $ show depth
                  ]
                  $ do
                    img_ [src_ "/assets/svgs/cheveron-down.svg", class_ "h-4 mr-3 mt-4 w-4 ", style_ "visibility: hidden"]
                    div_ [class_ "border flex flex-row border-gray-100 px-5 py-2 rounded-xl w-full items-center"] $ do
                      input_ [type_ "checkbox", class_ " mr-12"]
                      span_ [class_ "grow text-sm text-slate-700 inline-flex items-center"] $ toHtml displayKey
                      span_ [class_ "text-sm text-slate-600 mx-12 inline-flex items-center"] $ EndpointComponents.fieldTypeToDisplay $ field ^. #fieldType
                      img_ [src_ "/assets/svgs/alert-red.svg", class_ " mr-8 ml-4 h-5"]
                      img_ [src_ "/assets/svgs/dots-vertical.svg", class_ "mx-5 h-5"]

fieldCategoryToDisplay :: Fields.FieldCategoryEnum -> Html ()
fieldCategoryToDisplay fieldType = case fieldType of
  Fields.FCRequestBody -> span_ [class_ "px-2 rounded-xl bg-slate-100 slate-800 monospace"] "Request Body"
  Fields.FCQueryParam -> span_ [class_ "px-2 rounded-xl bg-blue-100 blue-800 monospace"] "Query Param"
  Fields.FCPathParam -> span_ [class_ "px-2 rounded-xl bg-gray-100 black-800 monospace"] "Path Param"
  Fields.FCRequestHeader -> span_ [class_ "px-2 rounded-xl bg-orange-100 orange-800 monospace"] "Request Header"
  Fields.FCResponseHeader -> span_ [class_ "px-2 rounded-xl bg-stone-100 stone-800 monospace"] "Response Header"
  Fields.FCResponseBody -> span_ [class_ "px-2 rounded-xl bg-red-100 red-800 monospace"] "Response Body"

fieldsToNormalized :: [Fields.Field] -> [(Text, Maybe Fields.Field)]
fieldsToNormalized =
  sortNub . concatMap \field ->
    map
      ((,Nothing) . fst)
      ( (field ^. #keyPathStr)
          & keyPathStrToKey
          & breakOnAll "."
      )
      & (++ [(keyPathStrToKey $ field ^. #keyPathStr, Just field)])
  where
    rmvDotPrefix = T.dropWhile (== '.')
    listToUnicode = replace "[]" "»"
    keyPathStrToKey = rmvDotPrefix . listToUnicode
