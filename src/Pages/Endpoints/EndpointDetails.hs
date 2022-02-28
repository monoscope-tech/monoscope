{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Pages.Endpoints.EndpointDetails (endpointDetailsH, fieldDetailsPartialH) where

import Config
import Data.Aeson qualified as AE
import Data.Map qualified as Map
import Data.Text as T (breakOnAll, dropWhile, isInfixOf, replace, splitOn)
import Data.Time (defaultTimeLocale, formatTime)
import Data.UUID as UUID
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.HTMX
import Lucid.Svg qualified as Svg
import Models.Apis.Endpoints
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields qualified as Fields
import Models.Apis.Formats qualified as Formats
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation
import Optics.Core ((^.))
import Pages.BodyWrapper (bodyWrapper)
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
    Nothing -> pure $ toHtml ""
    Just field -> pure $ fieldDetailsView field formats

fieldDetailsView :: Fields.Field -> Vector Formats.Format -> Html ()
fieldDetailsView field formats = do
  img_ [src_ "/assets/svgs/ellipsis.svg", class_ "my-2 float-right"]
  h3_ [class_ "text-lg text-slate-700 mt-6"] $ toHtml $ field ^. #key
  formats & mapM_ \format -> do
    div_ [class_ "flex mt-5 flex-row"] $ do
      div_ [class_ ""] $ do
        h6_ [class_ "text-slate-700 text-xs"] "TYPE"
        h4_ [class_ "text-base text-slate-700"] $ toHtml $ show $ format ^. #fieldType
      div_ [class_ "mx-5"] $ do
        h6_ [class_ "text-slate-700 text-xs"] "FORMAT"
        h4_ [class_ "text-base text-slate-700"] $ toHtml $ format ^. #fieldFormat
    h6_ [class_ "text-slate-600 mt-4 text-xs"] "EXAMPLE VALUES"
    ul_ [class_ "list-disc"] $ do
      format ^. #examples & mapM_ \ex -> do
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
  (endpoint, project, fieldsMap, reqsByStatsByMin, reqLatencyPercentiles) <- liftIO $
    withPool pool $ do
      endpointM <- Endpoints.endpointById eid
      let endpoint = Unsafe.fromJust endpointM
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      fieldsV <- Fields.selectFields eid
      let fieldsMap = Fields.groupFieldsByCategory fieldsV
      reqsByStatsByMin <- RequestDumps.selectRequestsByStatusCodesStatByMin pid (endpoint ^. #urlPath) (endpoint ^. #method)

      reqLatencyPercentilesM <- RequestDumps.selectReqLatencyPercentiles pid (endpoint ^. #urlPath) (endpoint ^. #method)
      let reqLatencyPercentiles = Unsafe.fromJust reqLatencyPercentilesM
      pure (endpoint, project, fieldsMap, reqsByStatsByMin, reqLatencyPercentiles)

  let reqsByStatsByMinJ = decodeUtf8 $ AE.encode reqsByStatsByMin
  pure $ bodyWrapper (Just sess) project "Endpoint Details" $ endpointDetails endpoint fieldsMap reqsByStatsByMinJ reqLatencyPercentiles

endpointDetails :: Endpoint -> Map Fields.FieldCategoryEnum [Fields.Field] -> Text -> RequestDumps.Percentiles -> Html ()
endpointDetails endpoint fieldsM reqsByStatsByMinJ percentiles = do
  script_ [src_ "https://cdn.fusioncharts.com/fusioncharts/latest/fusioncharts.js"] ""
  script_ [src_ "https://cdn.fusioncharts.com/fusioncharts/latest/themes/fusioncharts.theme.fusion.js"] ""

  div_ [class_ "w-full flex flex-row h-screen overflow-y-scroll"] $ do
    div_ [class_ "w-2/3  p-8"] $ do
      div_ [class_ "flex flex-row justify-between mb-10"] $ do
        div_ [class_ "flex flex-row place-items-center"] $ do
          h3_ [class_ "text-lg text-slate-700"] $ toHtml $ (endpoint ^. #method) <> " " <> (endpoint ^. #urlPath)
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
      div_ [class_ "space-y-8"] $ do
        endpointStats percentiles
        reqResSection "Request" True fieldsM
        reqResSection "Response" True fieldsM
    aside_ [class_ "w-1/3 bg-white h-screen -mr-8 -mt-5 border border-gray-200 p-5 sticky top-0", id_ "detailSidebar"] $ toHtml ""
    script_
      [text|
        let data = $reqsByStatsByMinJ
        let schema = [{
            "name": "Time",
            "type": "date",
            "format": "%Y-%m-%dT%H:%M:%S%Z" // https://www.fusioncharts.com/dev/fusiontime/fusiontime-attributes
        }, {
            "name": "StatusCode",
            "type": "string"
        },{
            "name": "Count",
            "type": "number"
        }]

        let chart = {}

        let fusionDataStore = new FusionCharts.DataStore();
        let fusionTable = fusionDataStore.createDataTable(data, schema);

        new FusionCharts({
          type: "timeseries",
          renderAt: "reqByStatusCode",
          width: "95%",
          height: 350,
          dataSource: {
            data: fusionTable,
            chart: chart,
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
      |]

endpointStats :: RequestDumps.Percentiles -> Html ()
endpointStats percentiles =
  section_ $ do
    div_ [class_ "flex justify-between mt-5"] $ do
      div_ [class_ "flex flex-row"] $ do
        img_ [src_ "/assets/svgs/cheveron-down.svg", class_ "h-4 mr-3 mt-1 w-4"]
        span_ [class_ "text-lg text-slate-700"] "Endpoint Stats"
    div_ [class_ "grid grid-cols-3 grid-rows-3 grid-flow-col gap-5"] $ do
      div_ [class_ "col-span-1 bg-white  border border-gray-100  rounded-2xl p-3 flex flex-row justify-between"] $ do
        div_ [class_ "flex flex-col justify-center"] $ do
          span_ "avg Reqs per minute"
          div_ [class_ "inline-block flex flex-row content-between"] $ do
            strong_ [class_ "text-xl"] "3.5k"
            div_ [class_ "inline-flex justify-center text-red-700 "] $ do
              img_ [class_ "inline-block", src_ "/assets/svgs/down-arrow-red.svg"]
              span_ "10.5%"
        div_ $ do
          Svg.svg_ [Svg.width_ "90", Svg.height_ "88", Svg.viewBox_ "0 0 90 88", Svg.fill_ "none", xmlns_ "http://www.w3.org/2000/svg"] $ do
            Svg.mask_ [Svg.id_ "mask0_4463_66717", Svg.style_ "mask-type:alpha", Svg.maskUnits_ "userSpaceOnUse", Svg.x_ "0", Svg.y_ "0", Svg.width_ "90", height_ "88"] $ do
              Svg.rect_ [Svg.opacity_ "0.1", Svg.x_ "0.51416", Svg.width_ "89.2571", Svg.height_ "88", Svg.rx_ "22", Svg.fill_ "#FF965D"]
            Svg.g_ [Svg.mask_ "url(#mask0_4463_66717)"] $ do
              Svg.rect_ [Svg.x_ "0.51416", Svg.width_ "89.2571", Svg.height_ "88", Svg.fill_ "#FF965D"]
            Svg.path_ [Svg.d_ "M25.6685 50.55C25.6685 49.5835 26.5161 48.8 27.5618 48.8H33.5123C34.5579 48.8 35.4056 49.5835 35.4056 50.55V60.8H25.6685V50.55Z", Svg.fill_ "#FF965D"]
            Svg.path_ [Svg.d_ "M40.2744 38.7091C40.2744 37.6547 41.1221 36.8 42.1678 36.8H48.1182C49.1639 36.8 50.0116 37.6547 50.0116 38.7091V60.8H40.2744V38.7091Z", Svg.fill_ "#FF965D"]
            Svg.path_ [Svg.d_ "M54.8799 29.16C54.8799 28.0775 55.7276 27.2 56.7732 27.2H62.7237C63.7694 27.2 64.617 28.0775 64.617 29.16V60.8H54.8799V29.16Z", Svg.fill_ "#FF965D"]
      div_ [class_ "col-span-1 bg-white  border border-gray-100  rounded-xl p-3 flex flex-row content-between"] $ do
        div_ $ do
          span_ "Anomalies this week"
          div_ [class_ "inline-block flex flex-row content-between"] $ do
            strong_ [class_ "text-xl"] "3"
            div_ [class_ "inline-block text-red-700"] $ do
              img_ [class_ "inline-block", src_ "/assets/svgs/down-arrow-red.svg"]
              span_ "10.5%"
        div_ $ do
          Svg.svg_ [Svg.width_ "86", Svg.height_ "86", Svg.viewBox_ "0 0 86 86", Svg.fill_ "none", xmlns_ "http://www.w3.org/2000/svg"] $ do
            Svg.circle_ [Svg.cx_ "43", Svg.cy_ "43", Svg.r_ "40.5", Svg.stroke_ "#F8F8F8", Svg.stroke_width_ "5"]
            Svg.path_ [Svg.d_ "M43 2.5C51.5528 2.5 59.886 5.20763 66.8053 10.2348C73.7246 15.262 78.8748 22.3507 81.5178 30.4848C84.1607 38.619 84.1607 47.381 81.5178 55.5152C78.8748 63.6493 73.7246 70.738 66.8053 75.7652C59.886 80.7924 51.5528 83.5 43 83.5C34.4472 83.5 26.114 80.7924 19.1947 75.7652C12.2754 70.738 7.12516 63.6493 4.48221 55.5152C1.83926 47.381 1.83926 38.619 4.48221 30.4848", Svg.stroke_ "#FF965D", Svg.stroke_width_ "5", Svg.stroke_linecap_ "round"]
            Svg.text_ "80%"
      div_ [class_ "col-span-2 bg-white  border border-gray-100  row-span-2 rounded-2xl p-3"] $ do
        div_ [class_ "p-4"] $ do
          select_ [] $ do
            option_ "Reqs by Status code"
            option_ "Avg Reqs per minute"
        div_ [id_ "reqByStatusCode", class_ ""] ""
      div_ [class_ "col-span-1 bg-white  border border-gray-100  rounded-xl p-3 flex flex-row content-between"] $ do
        div_ $ ""
        div_ [] $ do
          ul_ $ do
            percentileRow $ percentiles ^. #max
            percentileRow $ percentiles ^. #p99
            percentileRow $ percentiles ^. #p95
            percentileRow $ percentiles ^. #p90
            percentileRow $ percentiles ^. #p75
            percentileRow $ percentiles ^. #p50
            percentileRow $ percentiles ^. #p25
            percentileRow $ percentiles ^. #p10
            percentileRow $ percentiles ^. #min

percentileRow :: Double -> Html ()
percentileRow p = do
  li_ $ do
    span_ "max"
    span_ $ toHtml $ show $ p
    span_ "ms"

-- NB: We could enable the fields cycling functionality using the groups of response list functionality on the endpoint.
-- So we go through the list and in each request or response view, only show the fields that appear in the field list.
-- We can enable a view to show all the request/response options.
reqResSection :: Text -> Bool -> Map Fields.FieldCategoryEnum [Fields.Field] -> Html ()
reqResSection title isRequest fieldsM =
  section_ [class_ "space-y-3"] $ do
    div_ [class_ "flex justify-between mt-5"] $ do
      div_ [class_ "flex flex-row"] $ do
        img_ [src_ "/assets/svgs/cheveron-down.svg", class_ "h-4 mr-3 mt-1 w-4"]
        span_ [class_ "text-lg text-slate-700"] $ toHtml title
      div_ [class_ "flex flex-row mt-2"] $ do
        img_ [src_ "/assets/svgs/leftarrow.svg", class_ " m-2"]
        span_ [src_ " mx-4"] "1/1"
        img_ [src_ "/assets/svgs/rightarrow.svg", class_ " m-2"]
    if isRequest
      then do
        subSubSection (title <> " Headers") (Map.lookup Fields.FCRequestHeader fieldsM)
        subSubSection (title <> " Query Params") (Map.lookup Fields.FCQueryParam fieldsM)
        subSubSection (title <> " Body") (Map.lookup Fields.FCRequestBody fieldsM)
      else do
        subSubSection (title <> " Headers") (Map.lookup Fields.FCResponseHeader fieldsM)
        subSubSection (title <> " Body") (Map.lookup Fields.FCResponseBody fieldsM)

-- | subSubSection ..
subSubSection :: Text -> Maybe [Fields.Field] -> Html ()
subSubSection title fieldsM = do
  case fieldsM of
    Nothing -> toHtml ""
    Just fields -> do
      div_ [class_ "bg-white border border-gray-100 rounded-xl py-10 px-5 space-y-2"] $ do
        div_ [class_ "flex flex-row "] $ do
          img_ [src_ "/assets/svgs/cheveron-down.svg", class_ "h-4 mr-3 mt-4 w-4"]
          div_ [class_ "bg-gray-100 px-10 rounded-xl w-full p-4 text-sm text-slate-700"] $ toHtml title
        fieldsToNormalized fields & mapM_ \(key, fieldM) -> do
          let segments = splitOn "." key
          let depth = length segments
          let depthPadding = "margin-left:" <> show (20 + (depth * 20)) <> "px"
          let displayKey = replace "»" "" $ last ("" :| segments)
          case fieldM of
            Nothing -> do
              a_ [class_ "flex flex-row items-center", style_ depthPadding, term "data-depth" $ show depth] $ do
                img_ [src_ "/assets/svgs/cheveron-down.svg", class_ "h-4 mr-3 mt-4 w-4"]
                div_ [class_ "border flex flex-row border-gray-100 px-5 py-2 rounded-xl w-full"] $ do
                  input_ [type_ "checkbox", class_ " mr-12"]
                  span_ [class_ "grow text-sm text-slate-700 inline-flex items-center"] $ toHtml displayKey
                  span_ [class_ "text-sm text-slate-600 mx-12 inline-flex items-center"] $ toHtml $ if "»" `isInfixOf` key then "[]" else "{}"
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
                    span_ [class_ "text-sm text-slate-600 mx-12 inline-flex items-center"] $ toHtml $ show $ field ^. #fieldType
                    img_ [src_ "/assets/svgs/alert-red.svg", class_ " mr-8 ml-4 h-5"]
                    img_ [src_ "/assets/svgs/dots-vertical.svg", class_ "mx-5 h-5"]
        div_ [class_ " border-2 border-dashed flex mb-5 flex-row border-gray-100 ml-5  px-5 p-3 rounded-xl mt-2 hidden"] $ do
          img_ [src_ "/assets/svgs/blue-plus.svg", class_ "mx-2"]
          span_ [class_ "text-blue-700 font-medium text-base "] "Add a field"

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
