{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Pages.Endpoints.EndpointDetails (endpointDetailsH, fieldDetailsPartialH, fieldsToNormalized) where

import Config
import Data.Aeson qualified as AE
import Data.Aeson.Text (encodeToLazyText)
import Data.Default (def)
import Data.Map qualified as Map
import Data.Text as T (breakOnAll, dropWhile, isSuffixOf, splitOn, toLower)
import Data.Time (UTCTime, ZonedTime, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime, secondsToNominalDiffTime, utc, utcToZonedTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity.DBT (withPool)
import Fmt
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript.QuasiQuoter
import Models.Apis.Endpoints
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields (FieldCategoryEnum)
import Models.Apis.Fields qualified as Fields
import Models.Apis.Formats qualified as Formats
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation
import Optics.Core ((^.))
import Pages.Anomalies.AnomalyList qualified as AnomaliesList
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Pages.Charts.Charts qualified as C
import Pages.Charts.Charts qualified as Charts
import Pages.Components
import Pages.Endpoints.EndpointComponents qualified as EndpointComponents
import Relude hiding (max, min)
import Relude.Unsafe qualified as Unsafe
import Text.Interpolation.Nyan (int, rmode')
import Utils
import Witch (from)

timePickerItems :: [(Text, Text)]
timePickerItems =
  [ ("1H", "Last Hour")
  , ("24H", "Last 24 Hours")
  , ("7D", "Last 7 days")
  , ("14D", "Last 14 days")
  ]

data ParamInput = ParamInput
  { currentURL :: Text
  , sinceStr :: Maybe Text
  , dateRange :: (Maybe ZonedTime, Maybe ZonedTime)
  , currentPickerTxt :: Text
  , subPage :: Text
  }
data ShapeWidthFields = ShapeWidthFields
  { status :: Int
  , hash :: Text
  , fieldsMap :: Map FieldCategoryEnum [Fields.Field]
  }
  deriving (Show)

getShapeFields :: Shapes.Shape -> Vector Fields.Field -> ShapeWidthFields
getShapeFields shape fields = ShapeWidthFields{status = shape.statusCode, hash = shape.hash, fieldsMap = fieldM}
  where
    matchedFields = Vector.filter (\field -> field.hash `Vector.elem` shape.fieldHashes) fields
    fieldM = Fields.groupFieldsByCategory matchedFields

subPageMenu :: [(Text, Text)]
subPageMenu =
  [ ("Overview", "overview")
  , ("API Docs", "api_docs")
  ]

fieldDetailsPartialH :: Sessions.PersistentSession -> Projects.ProjectId -> Fields.FieldId -> DashboardM (Html ())
fieldDetailsPartialH sess pid fid = do
  pool <- asks pool
  isMember <- liftIO $ withPool pool $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      (fieldsM, formats) <- liftIO $
        withPool pool $ do
          field <- Fields.fieldById fid
          formats <- Formats.formatsByFieldHash (maybe "" (^. #hash) field)
          pure (field, formats)
      case fieldsM of
        Nothing -> pure ""
        Just field -> pure $ fieldDetailsView field formats

fieldDetailsView :: Fields.Field -> Vector Formats.Format -> Html ()
fieldDetailsView field formats = do
  img_ [src_ "/assets/svgs/ellipsis.svg", class_ "my-2 float-right"]
  section_ [class_ "space-y-6"] $ do
    div_ $ do
      h6_ [class_ "text-slate-800 text-xs"] "FIELD NAME"
      h3_ [class_ "text-lg text-slate-800"] $ toHtml $ field.key
    div_ $ do
      h6_ [class_ "text-slate-800 text-xs"] "FIELD PATH"
      h3_ [class_ "text-base text-slate-800 monospace"] $ toHtml $ field.keyPath
    div_ [class_ "flex flex-row gap-6"] $ do
      div_ $ do
        h6_ [class_ "text-slate-800 text-xs"] "FIELD CATEGORY"
        h4_ [class_ "text-base text-slate-800"] $ EndpointComponents.fieldCategoryToDisplay $ field.fieldCategory
      div_ [class_ ""] $ do
        h6_ [class_ "text-slate-800 text-xs"] "FORMAT OVERRIDE"
        h4_ [class_ "text-base text-slate-800"] $ toHtml $ fromMaybe "[unset]" (field.fieldTypeOverride)
    div_ $ do
      h5_ [class_ "text-sm text-slate-800"] "DETECTED FIELD FORMATS AND TYPES"
      div_ [class_ "space-y-2"] $
        formats & mapM_ \formatV -> do
          div_ [class_ "border-l-slate-200 border-l-2 pl-2 py-2"] $ do
            div_ [class_ "flex flex-row gap-9"] $ do
              div_ [class_ "space-y-2"] $ do
                h6_ [class_ "text-slate-800 text-xs"] "TYPE"
                h4_ [class_ "text-base text-slate-800"] $ EndpointComponents.fieldTypeToDisplay $ formatV.fieldType
              div_ [class_ "mx-5 space-y-2"] $ do
                h6_ [class_ "text-slate-800 text-xs"] "FORMAT"
                h4_ [class_ "text-base text-slate-800"] $ toHtml $ formatV.fieldFormat
            h6_ [class_ "text-slate-600 mt-4 text-xs"] "EXAMPLE VALUES"
            ul_ [class_ "list-disc"] $ do
              formatV.examples & mapM_ \ex -> do
                li_ [class_ "ml-10 text-slate-800 text-sm"] $ toHtml $ aesonValueToText ex
    div_ [class_ "flex flex-row justify-between mt-10 "] $ do
      div_ [class_ " "] $ do
        h4_ [class_ "text-sm text-slate-800 mb-2"] "CREATION DATE"
        div_ [class_ "flex border border-gray-200 m-1 rounded-xl p-2"] $ do
          mIcon_ "calender" "h-4 mr-2 w-4"
          span_ [class_ "text-xs"] $ toHtml $ formatTime defaultTimeLocale "%b %d, %Y %R" (field.createdAt)
      div_ [class_ " "] $ do
        h4_ [class_ "text-sm text-slate-800 mb-2"] "LAST CHANGE"
        div_ [class_ "flex border border-gray-200 m-1 justify-between rounded-xl p-2"] $ do
          mIcon_ "calender" "h-4 mr-2 w-4"
          span_ [class_ "text-xs"] $ toHtml $ formatTime defaultTimeLocale "%b %d, %Y %R" (field.updatedAt)
    h6_ [class_ "mt-5 text-sm text-slate-800 mb-2"] "DESCRIPTION"
    p_ [class_ "text-slate-800 text-sm"] $ toHtml $ field.description

aesonValueToText :: AE.Value -> Text
aesonValueToText = toStrict . encodeToLazyText

-- | endpointDetailsH is the main handler for the endpoint details page.
-- It reuses the fieldDetailsView as well, which is used for the side navigation on the page and also exposed un the fieldDetailsPartialH endpoint
endpointDetailsH :: Sessions.PersistentSession -> Projects.ProjectId -> Endpoints.EndpointId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> DashboardM (Html ())
endpointDetailsH sess pid eid fromDStr toDStr sinceStr' subPageM = do
  pool <- asks pool
  isMember <- liftIO $ withPool pool $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      now <- liftIO getCurrentTime
      let sinceStr = if (isNothing fromDStr && isNothing toDStr && isNothing sinceStr') || (fromDStr == Just "") then Just "14D" else sinceStr'

      -- TODO: Replace with a duration parser.
      let (fromD, toD) = case sinceStr of
            Just "1H" -> (Just $ utcToZonedTime utc $ addUTCTime (negate $ secondsToNominalDiffTime 3600) now, Just $ utcToZonedTime utc now)
            Just "24H" -> (Just $ utcToZonedTime utc $ addUTCTime (negate $ secondsToNominalDiffTime $ 3600 * 24) now, Just $ utcToZonedTime utc now)
            Just "7D" -> (Just $ utcToZonedTime utc $ addUTCTime (negate $ secondsToNominalDiffTime $ 3600 * 24 * 7) now, Just $ utcToZonedTime utc now)
            Just "14D" -> (Just $ utcToZonedTime utc $ addUTCTime (negate $ secondsToNominalDiffTime $ 3600 * 24 * 14) now, Just $ utcToZonedTime utc now)
            Nothing -> do
              let f = utcToZonedTime utc <$> (iso8601ParseM (from @Text $ fromMaybe "" fromDStr) :: Maybe UTCTime)
              let t = utcToZonedTime utc <$> (iso8601ParseM (from @Text $ fromMaybe "" toDStr) :: Maybe UTCTime)
              (f, t)
            _ -> do
              let f = utcToZonedTime utc <$> (iso8601ParseM (from @Text $ fromMaybe "" fromDStr) :: Maybe UTCTime)
              let t = utcToZonedTime utc <$> (iso8601ParseM (from @Text $ fromMaybe "" toDStr) :: Maybe UTCTime)
              (f, t)

      (endpoint, enpStats, project, shapesWithFieldsMap, fieldsMap, reqLatenciesRolledByStepsLabeled) <- liftIO $
        withPool pool $ do
          -- Should swap names betw enp and endpoint endpoint could be endpointStats
          endpoint <- Unsafe.fromJust <$> Endpoints.endpointById eid
          enpStats <- fromMaybe (def :: EndpointRequestStats) <$> Endpoints.endpointRequestStatsByEndpoint eid
          project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
          shapes <- Shapes.shapesByEndpointHash endpoint.hash
          fields <- Fields.selectFields (endpoint.hash)
          let fieldsMap = Fields.groupFieldsByCategory fields
          let shapesWithFieldsMap = Vector.map (`getShapeFields` fields) shapes
          let maxV = round (enpStats.max) :: Int
          let steps = (maxV `quot` 100) :: Int
          let steps' = if steps == 0 then 100 else steps
          reqLatenciesRolledBySteps <- RequestDumps.selectReqLatenciesRolledBySteps maxV steps' pid (endpoint.urlPath) (endpoint.method)
          pure (endpoint, enpStats, project, Vector.toList shapesWithFieldsMap, fieldsMap, Vector.toList reqLatenciesRolledBySteps)

      let reqLatenciesRolledByStepsJ = decodeUtf8 $ AE.encode reqLatenciesRolledByStepsLabeled
      let bwconf =
            (def :: BWConfig)
              { sessM = Just sess
              , currProject = project
              , pageTitle = "Endpoint Details"
              , menuItem = Just "Endpoints"
              }
      currTime <- liftIO getCurrentTime
      let currentURL = "/p/" <> pid.toText <> "/endpoints/" <> Endpoints.endpointIdText eid <> "?from=" <> fromMaybe "" fromDStr <> "&to=" <> fromMaybe "" toDStr
      let subPage = fromMaybe "overview" subPageM
      let currentPickerTxt = case sinceStr of
            Just a -> a
            Nothing -> maybe "" (toText . formatTime defaultTimeLocale "%F %T") fromD <> " - " <> maybe "" (toText . formatTime defaultTimeLocale "%F %T") toD
      let paramInput = ParamInput{currentURL = currentURL, sinceStr = sinceStr, dateRange = (fromD, toD), currentPickerTxt = currentPickerTxt, subPage = subPage}
      pure $ bodyWrapper bwconf $ endpointDetails paramInput currTime endpoint enpStats shapesWithFieldsMap fieldsMap reqLatenciesRolledByStepsJ (fromD, toD)

endpointDetails :: ParamInput -> UTCTime -> Endpoints.Endpoint -> EndpointRequestStats -> [ShapeWidthFields] -> Map FieldCategoryEnum [Fields.Field] -> Text -> (Maybe ZonedTime, Maybe ZonedTime) -> Html ()
endpointDetails paramInput currTime endpoint endpointStats shapesWithFieldsMap fieldsM reqLatenciesRolledByStepsJ dateRange = do
  let currentURLSubPage = deleteParam "subpage" paramInput.currentURL
  div_ [class_ "w-full h-full overflow-hidden"] $ do
    div_ [class_ "w-[75%] inline-block p-5 h-full overflow-y-scroll"] $ do
      div_ [class_ "flex flex-row justify-between mb-10"] $ do
        div_ [class_ "flex flex-row place-items-center text-lg font-medium"] $ do
          h3_ [class_ "text-lg text-slate-800"] $ do
            span_ [class_ $ "p-1 endpoint endpoint-" <> toLower (endpoint.method)] $ toHtml $ (endpoint.method) <> " "
            strong_ [class_ "inconsolata text-xl"] $ toHtml (endpoint.urlPath)
          img_ [src_ "/assets/svgs/cheveron-down.svg", class_ " h-4 w-4 m-2"]
        nav_ [class_ " space-x-4"] $ do
          subPageMenu
            & mapM_ \(title, slug) ->
              a_
                [ href_ $ currentURLSubPage <> "&subpage=" <> slug
                , class_ $
                    "cursor-pointer px-3 py-2 font-medium text-sm rounded-md "
                      <> if slug == paramInput.subPage then " bg-indigo-100 text-indigo-700 " else " text-gray-500 hover:text-gray-700"
                ]
                $ toHtml title

        div_ [class_ "flex flex-row hidden"] $ do
          a_ [href_ ""] $ do
            button_ [class_ "bg-white rounded-lg h-10 mt-1 "] $ do
              img_ [src_ "/assets/svgs/filter.svg", class_ "h-6 w-6 m-2"]
          a_ [href_ ""] $ do
            button_ [class_ "bg-blue-700 flex h-11 flex-row mx-2 px-3 rounded-xl py-2"] $ do
              h3_ [class_ "text-white text-sm text-bold mx-2 mt-1"] "Download Swagger"
              div_ [class_ "bg-blue-900 p-1 rounded-lg ml-2"] $ do
                mIcon_ "whitedown" "text-white h-2 w-2 m-1"
      if paramInput.subPage == "api_docs"
        then apiDocsSubPage shapesWithFieldsMap
        else apiOverviewSubPage paramInput currTime endpointStats fieldsM reqLatenciesRolledByStepsJ dateRange

    aside_
      [ class_ "w-[25%] inline-block h-full overflow-y-auto overflow-x-hidden bg-white border border-gray-200 p-5 xsticky xtop-0 "
      , id_ "detailSidebar"
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

apiDocsSubPage :: [ShapeWidthFields] -> Html ()
apiDocsSubPage shapesWithFieldsMap = do
  div_ [class_ "space-y-8", id_ "subpage"] $ do
    div_ [class_ "flex w-full justify-between mt-2"] $ do
      div_ [class_ "flex items-center gap-2"] $ do
        span_ [class_ "font-bold text-gray-700"] "Shapes:"
        div_ [class_ "relative flex items-center border rounded focus:ring-2 focus:ring-blue-200 active:ring-2 active:ring-blue-200", style_ "width:220px"] $ do
          button_
            [ [__| on click toggle .hidden on #shapes_container |]
            , id_ "toggle_shapes_btn"
            , data_ "current" "1"
            , data_ "total" (show $ length shapesWithFieldsMap)
            , class_ "w-full flex text-gray-600 justify_between items-center cursor-pointer px-2 py-1"
            ]
            $ do
              let fstH = viaNonEmpty head shapesWithFieldsMap
              let (st, hs) = case fstH of
                    Just s -> (s.status, s.hash)
                    Nothing -> (0, "No shapes")
              let prm = "px-2 py-1 rounded text-white text-sm "
              let statusCls = if st < 400 then prm <> "bg-green-500" else prm <> "bg-red-500"
              span_ [class_ statusCls] $ show st
              span_ [class_ "ml-1 text-sm text-gray-600"] $ toHtml hs
          img_ [src_ "/assets/svgs/select_chevron.svg", style_ "height:15px; width:15px"]
          div_ [id_ "shapes_container", class_ "absolute hidden bg-white border shadow w-full overflow-y-auto", style_ "top:100%; max-height: 300px; z-index:9"] $ do
            forM_ (zip [(1 :: Int) ..] shapesWithFieldsMap) $ \(index, s) -> do
              let prm = "px-2 py-1 rounded text-white text-sm "
              let statusCls = if s.status < 400 then prm <> "bg-green-500" else prm <> "bg-red-500"
              let prm = "p-2 w-full text-left truncate ... hover:bg-blue-100 hover:text-black"
              button_
                [ class_ prm
                , id_ ("status_" <> show index)
                , [__|on click selectShape((me),(my @data-pos)) |]
                , data_ "pos" (show index)
                , data_ "status" (show s.status)
                , data_ "hash" s.hash
                ]
                $ do
                  span_ [class_ statusCls] $ show s.status
                  span_ [class_ "ml-2 text-sm text-gray-600"] $ toHtml s.hash

      div_ [class_ "flex items-center"] $ do
        img_
          [ src_ "/assets/svgs/leftarrow.svg"
          , class_ " m-2 cursor-pointer"
          , [__|on click slideReqRes('prev') |]
          ]
        let l = "1/" <> show (length shapesWithFieldsMap)
        let id = "current_indicator"
        span_ [src_ " mx-4", id_ id] l
        img_
          [ src_ "/assets/svgs/rightarrow.svg"
          , class_ "m-2 cursor-pointer"
          , [__|on click slideReqRes('next') |]
          ]
    reqResSection "Request" True shapesWithFieldsMap
    reqResSection "Response" False shapesWithFieldsMap
  script_
    [type_ "text/hyperscript"]
    [text| 
        def selectShape(elem, position)
          if position is (#toggle_shapes_btn @data-current)
            exit
          end
          remove (<span/> in #toggle_shapes_btn)
          set (#toggle_shapes_btn @data-current) to position 
          append (<span /> in elem) as HTML to #toggle_shapes_btn
          add .hidden to .Response_fields
          add .hidden to .Request_fields
          call document.querySelector ("#Request_" + position)
          remove .hidden from it
          call document.querySelector ("#Response_" + position)
          remove .hidden from it
          put (position + "/" + (#toggle_shapes_btn @data-total)) into #current_indicator
        end
      
        def slideReqRes(action)
          set current to (#toggle_shapes_btn @data-current)
          if (current is (#toggle_shapes_btn @data-total) and action is "next") or (current is "1" and action is "prev")
            exit 
          end
          if action == "next" 
            then set position to ((current as Int) + 1) 
            else set position to ((current as Int) - 1)
          end
          set (#toggle_shapes_btn @data-current) to position 
          call document.querySelector("#status_" + position)
          remove (<span/> in #toggle_shapes_btn)
          append (<span /> in it) as HTML to #toggle_shapes_btn
          add .hidden to .Response_fields
          add .hidden to .Request_fields
          call document.querySelector ("#Request_" + position)
          remove .hidden from it
          call document.querySelector ("#Response_" + position)
          remove .hidden from it
          put (position + "/" + (#toggle_shapes_btn @data-total)) into #current_indicator
        end
        |]

apiOverviewSubPage :: ParamInput -> UTCTime -> EndpointRequestStats -> Map Fields.FieldCategoryEnum [Fields.Field] -> Text -> (Maybe ZonedTime, Maybe ZonedTime) -> Html ()
apiOverviewSubPage paramInput currTime endpoint fieldsM reqLatenciesRolledByStepsJ dateRange = do
  let currentURLSearch = deleteParam "to" $ deleteParam "from" $ deleteParam "since" paramInput.currentURL
  div_ [class_ "space-y-16 pb-20", id_ "subpage"] $ do
    a_
      [ class_ "relative px-3 py-2 border border-1 border-black-200 space-x-2  inline-block relative cursor-pointer rounded-md"
      , [__| on click toggle .hidden on #timepickerBox|]
      ]
      do
        mIcon_ "clock" "h-4 w-4"
        span_ [class_ "inline-block"] $ toHtml paramInput.currentPickerTxt
        img_
          [ src_ "/assets/svgs/cheveron-down.svg"
          , class_ "h-4 w-4 inline-block"
          ]
    div_ [id_ "timepickerBox", class_ "hidden absolute z-10 mt-1  rounded-md flex"] do
      div_ [class_ "inline-block w-84 overflow-auto bg-white py-1 text-base shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none sm:text-sm"] do
        timePickerItems
          & mapM_ \(val, title) ->
            a_
              [ class_ "block text-gray-900 relative cursor-pointer select-none py-2 pl-3 pr-9 hover:bg-gray-200 "
              , href_ $ currentURLSearch <> "&since=" <> val
              ]
              $ toHtml title
        a_ [class_ "block text-gray-900 relative cursor-pointer select-none py-2 pl-3 pr-9 hover:bg-gray-200 ", [__| on click toggle .hidden on #timepickerSidebar |]] "Custom date range"
      div_ [class_ "inline-block relative hidden", id_ "timepickerSidebar"] do
        div_ [id_ "startTime", class_ "hidden"] ""
    section_ $ AnomaliesList.anomalyListSlider currTime (endpoint.projectId) (Just endpoint.endpointId) Nothing
    endpointStats endpoint reqLatenciesRolledByStepsJ dateRange

endpointStats :: Endpoints.EndpointRequestStats -> Text -> (Maybe ZonedTime, Maybe ZonedTime) -> Html ()
endpointStats enpStats@Endpoints.EndpointRequestStats{min, p50, p75, p90, p95, p99, max} reqLatenciesRolledByStepsJ dateRange@(fromD, toD) =
  section_ [class_ "space-y-3"] $ do
    div_ [class_ "flex justify-between mt-5"] $
      div_ [class_ "flex flex-row"] $ do
        img_
          [ src_ "/assets/svgs/cheveron-down.svg"
          , class_ "h-4 mr-3 mt-1 w-4 cursor-pointer"
          , [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .endpointStatsSubSection)|]
          ]
        span_ [class_ "text-lg text-slate-800"] "Endpoint Stats"
    div_ [class_ "space-y-5 endpointStatsSubSection"] $ do
      div_ [class_ "grid grid-cols-3  gap-5"] $ do
        statBox "Total Anomalies" "Total Anomalies for this endpoint this week vs total for the project" enpStats.ongoingAnomaliesProj (Just enpStats.ongoingAnomaliesProj)
        statBox "Total Requests" "Total Requests on this endpoint this week vs total for the project" enpStats.totalRequests (Just enpStats.totalRequestsProj)
        statBox "Total Time" "Total Time on this endpoint this week vs total for the project" enpStats.totalRequests (Just enpStats.totalRequestsProj)

      div_ [class_ "flex gap-5"] do
        div_ [class_ "flex-1 card-round p-3"] $ do
          div_ [class_ "p-4 space-y-6"] $ do
            select_ [] $ do
              option_ [class_ "text-2xl font-normal"] "Requests by Status Code"
            div_ [class_ "h-64 "] do
              Charts.lazy [C.QByE $ [C.QBPId enpStats.projectId, C.QBEndpointHash enpStats.endpointHash] ++ catMaybes [C.QBFrom <$> fromD, C.QBTo <$> toD], C.GByE C.GBStatusCode, C.SlotsE 120, C.ShowLegendE]

        div_ [class_ "flex-1 card-round p-3"] $ do
          div_ [class_ "p-4 space-y-6"] $ do
            select_ [] $ do
              option_ [class_ "text-2xl font-normal"] "Latency Percentiles"
            div_ [class_ "h-64 "] do
              Charts.lazy [C.QByE $ [C.QBPId enpStats.projectId, C.QBEndpointHash enpStats.endpointHash] ++ catMaybes [C.QBFrom <$> fromD, C.QBTo <$> toD], C.GByE C.GBDurationPercentile, C.SlotsE 120, C.ShowLegendE, C.TypeE C.LineCT]

      div_ [class_ "flex gap-5"] do
        div_ [class_ "flex-1 card-round p-3"] $ do
          div_ [class_ "p-4 space-y-6"] $ do
            select_ [] $ do
              option_ [class_ "text-2xl font-normal"] "Errors"
            div_ [class_ "h-64 "] do
              Charts.lazy [C.QByE $ [C.QBPId enpStats.projectId, C.QBEndpointHash enpStats.endpointHash, Charts.QBStatusCodeGT 400] ++ catMaybes [C.QBFrom <$> fromD, C.QBTo <$> toD], C.GByE C.GBStatusCode, C.SlotsE 120, C.ShowLegendE, C.Theme "roma"]

        div_ [class_ "flex-1 card-round p-3"] $ do
          div_ [class_ "p-4 space-y-6"] $ do
            select_ [] $ do
              option_ [class_ "text-2xl font-normal"] "Reqs Grouped by Endpoint"
            div_ [class_ "h-64 "] do
              Charts.lazy [C.QByE $ [C.QBPId enpStats.projectId, C.QBEndpointHash enpStats.endpointHash] ++ catMaybes [C.QBFrom <$> fromD, C.QBTo <$> toD], C.GByE C.GBEndpoint, C.SlotsE 120, C.ShowLegendE]

      div_ [class_ "col-span-3 bg-white   border border-gray-100  rounded-xl py-3 px-6"] $ do
        div_ [class_ "p-4"] $
          select_ [] $ do
            option_ "Request Latency Distribution"
            option_ "Avg Reqs per minute"
        div_ [class_ "grid grid-cols-9  gap-8 w-full"] $ do
          div_ [id_ "reqsLatencyHistogram", class_ "col-span-7 h-72"] ""
          div_ [class_ "col-span-2 space-y-4 "] $ do
            strong_ [class_ "block text-right"] "Latency Percentiles"
            ul_ [class_ "space-y-1 divide-y divide-slate-100"] $ do
              percentileRow "max" $ enpStats.max
              percentileRow "p99" $ enpStats.p99
              percentileRow "p95" $ enpStats.p95
              percentileRow "p90" $ enpStats.p90
              percentileRow "p75" $ enpStats.p75
              percentileRow "p50" $ enpStats.p50
              percentileRow "min" $ enpStats.min
        script_ [int|| latencyHistogram('reqsLatencyHistogram',{p50:#{p50}, p75:#{p75}, p90:#{p90}, p95:#{p95}, p99:#{p99}, max:#{max}},  #{reqLatenciesRolledByStepsJ}) |]

percentileRow :: Text -> Double -> Html ()
percentileRow key p = do
  let (d, unit) = fmtDuration p
  li_ [class_ "flex flex-row content-between justify-between"] $ do
    span_ [class_ "inline-block"] $ toHtml key
    span_ [class_ "inline-block font-mono"] $ do
      span_ [class_ "tabular-nums"] $ toHtml d
      span_ $ toHtml unit

fmtDuration :: Double -> (Text, Text)
fmtDuration d
  | d > 1000 = (fmt $ fixedF 2 (d / 1000), "s")
  | otherwise = (fmt $ fixedF 0 d, "ms")

-- NOTE: We could enable the fields cycling functionality using the groups of response list functionality on the endpoint.
-- So we go through the list and in each request or response view, only show the fields that appear in the field list.
-- We can enable a view to show all the request/response options.
reqResSection :: Text -> Bool -> [ShapeWidthFields] -> Html ()
reqResSection title isRequest shapesWithFieldsMap =
  section_ [class_ "space-y-3"] $ do
    div_ [class_ "flex justify-between mt-5"] $ do
      div_ [class_ "flex flex-row"] $ do
        a_ [class_ "cursor-pointer", [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .reqResSubSection)|]] $
          img_
            [ src_ "/assets/svgs/cheveron-down.svg"
            , class_ "h-4 mr-3 mt-1 w-4"
            ]
        span_ [class_ "text-lg text-slate-800"] $ toHtml title

    div_ [class_ "bg-white border border-gray-100 rounded-xl py-5 px-5 space-y-6 reqResSubSection"] $
      forM_ (zip [(1 :: Int) ..] shapesWithFieldsMap) $ \(index, s) -> do
        let sh = if index == 1 then title <> "_fields" else title <> "_fields hidden"
        div_ [class_ sh, id_ $ title <> "_" <> show index] $ do
          if isRequest
            then do
              subSubSection (title <> " Path Params") (Map.lookup Fields.FCPathParam s.fieldsMap)
              subSubSection (title <> " Query Params") (Map.lookup Fields.FCQueryParam s.fieldsMap)
              subSubSection (title <> " Headers") (Map.lookup Fields.FCRequestHeader s.fieldsMap)
              subSubSection (title <> " Body") (Map.lookup Fields.FCRequestBody s.fieldsMap)
            else do
              subSubSection (title <> " Headers") (Map.lookup Fields.FCResponseHeader s.fieldsMap)
              subSubSection (title <> " Body") (Map.lookup Fields.FCResponseBody s.fieldsMap)

-- | subSubSection ..
subSubSection :: Text -> Maybe [Fields.Field] -> Html ()
subSubSection title fieldsM =
  case fieldsM of
    Nothing -> ""
    Just fields -> do
      div_ [class_ "space-y-1 mb-4"] $ do
        div_ [class_ "flex flex-row items-center"] $ do
          img_
            [ src_ "/assets/svgs/cheveron-down.svg"
            , class_ "h-6 mr-3 w-6 p-1 cursor-pointer"
            , [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .subSectionContent)|]
            ]
          div_ [class_ "bg-gray-100 px-10 rounded-xl w-full p-4 text-sm text-slate-900 "] $ toHtml title
        div_ [class_ "space-y-1 subSectionContent"] $ do
          -- pTraceShowM $ fields
          -- pTraceShowM "========================"
          -- pTraceShowM $ fieldsToNormalized fields
          fieldsToNormalized fields & mapM_ \(key, fieldM) -> do
            let segments = splitOn "." key
            let depth = length segments
            let depthPadding = "margin-left:" <> show (20 + (depth * 20)) <> "px"
            let displayKey = last ("" :| segments)
            case fieldM of
              Nothing -> do
                a_
                  [ class_ "flex flex-row items-center"
                  , style_ depthPadding
                  , [__| on click toggle .neg-rotate-90 on <.chevron/> in me then collapseUntil((me), (my @data-depth))  |]
                  , term "data-depth" $ show depth
                  ]
                  $ do
                    img_ [src_ "/assets/svgs/cheveron-down.svg", class_ "h-6 w-6 mr-1 chevron cursor-pointer p-1"]
                    div_ [class_ "border flex flex-row border-gray-100 px-5 py-2 rounded-xl w-full"] $ do
                      input_ [type_ "checkbox", class_ " mr-12"]
                      span_ [class_ "text-sm text-slate-800 inline-flex items-center"] $ toHtml displayKey
                      span_ [class_ "text-sm text-slate-600 inline-flex items-center ml-4"] $ do
                        if "[*]" `isSuffixOf` key
                          then EndpointComponents.fieldTypeToDisplay Fields.FTList
                          else EndpointComponents.fieldTypeToDisplay Fields.FTObject
              Just field -> do
                a_
                  [ hxGet_ $ "/p/" <> field.projectId.toText <> "/fields/" <> UUID.toText (Fields.unFieldId $ field.id)
                  , hxTarget_ "#detailSidebar"
                  , class_ "flex flex-row cursor-pointer"
                  , style_ depthPadding
                  , term "data-depth" $ show depth
                  ]
                  $ do
                    img_ [src_ "/assets/svgs/cheveron-down.svg", class_ "h-4 mr-3 mt-4 w-4 ", style_ "visibility: hidden"]
                    div_ [class_ "border flex flex-row border-gray-100 px-5 py-2 rounded-xl w-full items-center"] $ do
                      input_ [type_ "checkbox", class_ " mr-12"]
                      span_ [class_ "grow text-sm text-slate-800 inline-flex items-center"] $ toHtml displayKey
                      span_ [class_ "text-sm text-slate-600 mx-12 inline-flex items-center"] $ EndpointComponents.fieldTypeToDisplay $ field.fieldType
                      img_ [src_ "/assets/svgs/alert-red.svg", class_ " mr-8 ml-4 h-5"]
                      img_ [src_ "/assets/svgs/dots-vertical.svg", class_ "mx-5 h-5"]

-- | fieldsToNormalized, gets a list of fields and returns a list of tuples with the keypath, and the field, sorted by the key path
-- >>> import Models.Apis.Fields.Types
-- >>> import Data.Default
-- >>> let v1 = (def::Field){fieldCategory=FCQueryParam, keyPath=".k1.k2[*]", key="k2[*]"}
-- >>> let v2 = (def::Field){fieldCategory=FCResponseBody, keyPath=".k1.k2[*].v", key="v"}
-- >>> let v3 = (def::Field){fieldCategory=FCResponseBody, keyPath=".k1.k2[*].x", key="x"}
-- >>> fieldsToNormalized [v1]
-- [("k1",Nothing),("k1.k2[*]",Just (Field {id = FieldId {unFieldId = 00000000-0000-0000-0000-000000000000}, createdAt = 2019-08-31 05:14:37.537084021 UTC, updatedAt = 2019-08-31 05:14:37.537084021 UTC, projectId = ProjectId {unProjectId = 00000000-0000-0000-0000-000000000000}, endpointHash = "", key = "k2[*]", fieldType = FTUnknown, fieldTypeOverride = Nothing, format = "", formatOverride = Nothing, description = "", keyPath = ".k1.k2[*]", fieldCategory = FCQueryParam, hash = ""}))]

---- >>> fieldsToNormalized [v2]
---- >>> fieldsToNormalized [v2]
fieldsToNormalized :: [Fields.Field] -> [(Text, Maybe Fields.Field)]
fieldsToNormalized =
  sortNub . concatMap \field ->
    map
      ((,Nothing) . fst)
      ( field.keyPath
          & T.dropWhile (== '.')
          & breakOnAll "."
      )
      & (++ [(T.dropWhile (== '.') $ field.keyPath, Just field)])
