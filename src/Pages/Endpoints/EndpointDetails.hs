{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Pages.Endpoints.EndpointDetails (endpointDetailsH, fieldDetailsPartialH, fieldsToNormalized, endpointDetailsWithHashH, EndpointDetailsGet) where

import Data.Aeson (KeyValue ((.=)))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEKey
import Data.Aeson.Text (encodeToLazyText)
import Data.Default (def)
import Data.List (elemIndex)
import Data.Map qualified as Map
import Data.Text (isSuffixOf, splitOn, toLower)
import Data.Text qualified as T
import Data.Time (UTCTime, ZonedTime, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime, secondsToNominalDiffTime, utc, utcToZonedTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Fmt (fixedF, fmt)
import Lucid
import Lucid.Htmx (hxGet_, hxPost_, hxSwap_, hxTarget_)
import Lucid.Hyperscript.QuasiQuoter
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields qualified as Fields
import Models.Apis.Fields.Types (Field (createdAt, description, fieldCategory, fieldType, fieldTypeOverride, hash, id, isEnum, isRequired, key, keyPath, projectId, updatedAt), FieldCategoryEnum, FieldId (unFieldId), fieldTypeToText, fieldsToNormalized)
import Models.Apis.Formats qualified as Formats
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Shapes (getShapeFields)
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation
import Pages.Anomalies.AnomalyList qualified as AnomaliesList
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Charts.Charts qualified as C
import Pages.Charts.Charts qualified as Charts
import Pages.Components (statBox)
import Pages.Endpoints.EndpointComponents qualified as EndpointComponents
import Relude hiding (ask, asks, max, min)
import Relude.Unsafe qualified as Unsafe
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders, redirectCS)
import Text.Interpolation.Nyan (int, rmode')
import Utils (deleteParam, faSprite_, getStatusColor)
import Witch (from)

timePickerItems :: [(Text, Text)]
timePickerItems =
  [ ("1H", "Last Hour"),
    ("24H", "Last 24 Hours"),
    ("7D", "Last 7 days"),
    ("14D", "Last 14 days")
  ]

data ParamInput = ParamInput
  { currentURL :: Text,
    sinceStr :: Maybe Text,
    dateRange :: (Maybe ZonedTime, Maybe ZonedTime),
    currentPickerTxt :: Text,
    subPage :: Text
  }

subPageMenu :: [(Text, Text)]
subPageMenu =
  [ ("Overview", "overview"),
    ("API Docs", "api_docs"),
    ("Shapes", "shapes")
  ]

fieldDetailsPartialH :: Projects.ProjectId -> Fields.FieldId -> ATAuthCtx (RespHeaders (Html ()))
fieldDetailsPartialH pid fid = do
  _ <- Sessions.sessionAndProject pid
  (fieldsM, formats) <- dbtToEff do
    field <- Fields.fieldById fid
    formats <- Formats.formatsByFieldHash (maybe "" (.hash) field)
    pure (field, formats)
  case fieldsM of
    Nothing -> addRespHeaders ""
    Just field -> addRespHeaders $ fieldDetailsView field formats

fieldDetailsView :: Fields.Field -> Vector Formats.Format -> Html ()
fieldDetailsView field formats = do
  div_ [id_ "modalContainer"] do
    label_ [Lucid.for_ "edit_field", class_ "cursor-pointer"] $ faSprite_ "ellipsis" "regular" "my-2 float-right"
    input_ [type_ "checkbox", id_ "edit_field", class_ "modal-toggle"]
    div_ [class_ "modal", role_ "dialog", hxSwap_ "outerHTML"] do
      form_
        [ class_ "modal-box w-1/2 max-w-3xl",
          id_ "editFieldForm",
          hxPost_ $ "/p/" <> field.projectId.toText <> "/fields/" <> show field.id.unFieldId,
          hxSwap_ "none"
        ]
        do
          input_ [type_ "hidden", name_ "fieldHash", value_ field.hash]
          input_ [type_ "hidden", name_ "fieldType", value_ $ Fields.fieldTypeToText field.fieldType]
          div_ [class_ "flex items-center py-2 border-b justify-between"] do
            h3_ [class_ "text-xl font-bold text-gray-900"] "Edit Field"
            label_ [Lucid.for_ "edit_field", class_ "modal-action rounded-full m-0 cursor-pointer bg-gray-200 pr-2 py-2 flex items-center justify-center"] do
              faSprite_ "close" "light" "h-4 w-4 inline-block"
          div_ [class_ "w-full py-3"] do
            div_ [class_ "text-xl space-y-6 overflow-y-auto", style_ "min-height:30vh;max-height:70vh; width:100%"] do
              div_ [class_ "flex items-center gap-4"] do
                label_ [Lucid.for_ "is_required", class_ "text-gray-700 text-sm font-semibold"] "Required"
                input_ [class_ "checkbox checkbox-success checkbox-sm", id_ "is_required", name_ "isRequired", type_ "checkbox", if field.isRequired then checked_ else value_ "off"]
              div_ [class_ "flex items-center gap-4"] do
                label_ [Lucid.for_ "is_enum", class_ "text-gray-700 text-sm font-semibold"] "Enum"
                input_ [class_ "checkbox checkbox-success checkbox-sm", id_ "is_enum", name_ "isEnum", type_ "checkbox", if field.isEnum then checked_ else value_ "off"]
              div_ [class_ "flex flex-col gap-2"] do
                span_ [class_ "text-gray-700 text-sm font-semibold"] "Formats"
                div_ [class_ "flex flex-col gap-2"] do
                  div_ [id_ "formats", class_ "flex flex-col gap-1 px-2 [&>input]:input [&>input]:input-sm [&>input]:input-bordered [&>input]:input-success [&>input]:w-full [&>input]:max-w-xs"] do
                    formats & mapM_ \f -> do
                      input_ [type_ "text", name_ "formats", value_ f.fieldFormat]
                  button_
                    [ class_ "rounded-full py-1 px-2 w-max bg-blue-100 text-blue-500 text-sm mt-2 flex items-center gap-1",
                      onclick_ "",
                      type_ "button",
                      [__| on click append "<input type=\"text\" name=\"formats\">"  to #formats|]
                    ]
                    do
                      span_ [] "Add"
                      faSprite_ "plus" "solid" "h-3 w-3"

              div_ [class_ "flex flex-col gap-1"] do
                label_ [Lucid.for_ "description", class_ "text-gray-700 text-sm font-semibold"] "Description"
                textarea_ [id_ "description", name_ "description", class_ "text-sm border p-2 rounded-lg text-gray-600"] $ toHtml field.description
          div_ [class_ "flex w-full justify-end items-center p-6 gap-4 space-x-2 border-t border-gray-200 rounded-b"] do
            button_
              [ class_ "btn btn-primary",
                type_ "submit"
              ]
              "Save"
      label_ [class_ "modal-backdrop", Lucid.for_ "edit_field"] "Close"

  section_ [class_ "space-y-6"] do
    div_ do
      h6_ [class_ "text-slate-800 text-xs"] "FIELD NAME"
      h3_ [class_ "text-lg text-slate-800"] $ toHtml field.key
    div_ do
      h6_ [class_ "text-slate-800 text-xs"] "FIELD PATH"
      h3_ [class_ "text-base text-slate-800 monospace"] $ toHtml field.keyPath
    div_ [class_ "flex flex-row gap-6"] do
      div_ do
        h6_ [class_ "text-slate-800 text-xs"] "FIELD CATEGORY"
        h4_ [class_ "text-base text-slate-800"] $ EndpointComponents.fieldCategoryToDisplay field.fieldCategory
      div_ [class_ ""] do
        h6_ [class_ "text-slate-800 text-xs"] "FORMAT OVERRIDE"
        h4_ [class_ "text-base text-slate-800"] $ toHtml $ fromMaybe "[unset]" field.fieldTypeOverride
    div_ [class_ "flex flex-row gap-6"] do
      when field.isRequired do
        span_ [class_ "px-2 rounded-xl bg-red-100 text-red-800 monospace"] "required"
      when field.isEnum do
        span_ [class_ "px-2 rounded-xl bg-green-100 text-green-800 monospace"] "enum"

    div_ do
      h5_ [class_ "text-sm text-slate-800"] "DETECTED FIELD FORMATS AND TYPES"
      div_ [class_ "space-y-2"]
        $ formats
        & mapM_ \formatV -> do
          div_ [class_ "border-l-slate-200 border-l-2 pl-2 py-2"] do
            div_ [class_ "flex flex-row gap-9"] do
              div_ [class_ "space-y-2"] do
                h6_ [class_ "text-slate-800 text-xs"] "TYPE"
                h4_ [class_ "text-base text-slate-800"] $ EndpointComponents.fieldTypeToDisplay formatV.fieldType
              div_ [class_ "mx-5 space-y-2"] do
                h6_ [class_ "text-slate-800 text-xs"] "FORMAT"
                h4_ [class_ "text-base text-slate-800"] $ toHtml formatV.fieldFormat
            h6_ [class_ "text-slate-600 mt-4 text-xs"] $ if field.isEnum then "ENUM VALUES" else "EXAMPLE VALUES"
            ul_ [class_ "list-disc"] do
              formatV.examples & mapM_ \ex -> do
                li_ [class_ "ml-10 text-slate-800 text-sm"] $ toHtml $ aesonValueToText ex
    div_ [class_ "flex flex-row justify-between mt-10 "] do
      div_ [class_ " "] do
        h4_ [class_ "text-sm text-slate-800 mb-2"] "CREATION DATE"
        div_ [class_ "flex border border-gray-200 m-1 rounded-xl p-2"] do
          faSprite_ "regular-calendar-days-clock" "regular" "h-4 mr-2 w-4"
          span_ [class_ "text-xs"] $ toHtml $ formatTime defaultTimeLocale "%b %d, %Y %R" field.createdAt
      div_ [class_ " "] do
        h4_ [class_ "text-sm text-slate-800 mb-2"] "LAST CHANGE"
        div_ [class_ "flex border border-gray-200 m-1 justify-between rounded-xl p-2"] do
          faSprite_ "regular-calendar-days-clock" "regular" "h-4 mr-2 w-4"
          span_ [class_ "text-xs"] $ toHtml $ formatTime defaultTimeLocale "%b %d, %Y %R" field.updatedAt
    h6_ [class_ "mt-5 text-sm text-slate-800 mb-2"] "DESCRIPTION"
    p_ [class_ "text-slate-800 text-sm"] $ toHtml field.description

aesonValueToText :: AE.Value -> Text
aesonValueToText = toStrict . encodeToLazyText

-- /p/" <> pid.toText <> "/log_explorer/item/" <> endpoint_hash
endpointDetailsWithHashH :: Projects.ProjectId -> Text -> ATAuthCtx (RespHeaders (Html ()))
endpointDetailsWithHashH pid endpoint_hash = do
  _ <- Sessions.sessionAndProject pid
  endpoint <- dbtToEff $ Endpoints.endpointByHash pid endpoint_hash
  case endpoint of
    Just e -> do
      let redirect_url = "/p/" <> pid.toText <> "/endpoints/" <> e.id.toText
      redirectCS redirect_url
      addRespHeaders $ span_ ""
    Nothing -> addRespHeaders ""

-- | endpointDetailsH is the main handler for the endpoint details page.
-- It reuses the fieldDetailsView as well, which is used for the side navigation on the page and also exposed un the fieldDetailsPartialH endpoint
endpointDetailsH :: Projects.ProjectId -> Endpoints.EndpointId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (EndpointDetailsGet))
endpointDetailsH pid eid fromDStr toDStr sinceStr' subPageM shapeHashM = do
  (sess, project) <- Sessions.sessionAndProject pid
  now <- liftIO getCurrentTime
  let sinceStr = if (isNothing fromDStr && isNothing toDStr && isNothing sinceStr') || (fromDStr == Just "") then Just "7D" else sinceStr'
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
  endpointM <- dbtToEff $ Endpoints.endpointById eid
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession,
            currProject = Just project,
            pageTitle = "Endpoint Details",
            menuItem = Just "Endpoints"
          }
  case endpointM of
    Nothing -> do
      -- TODO: Add a 404 page
      addRespHeaders $ EndpointsDetailsNotFound $ PageCtx bwconf ()
    Just endpoint -> do
      (endpoint, enpStats, shapesWithFieldsMap, fieldsMap, reqLatenciesRolledByStepsLabeled) <- dbtToEff do
        -- Should swap names betw enp and endpoint endpoint could be endpointStats
        enpStats <- fromMaybe (def :: Endpoints.EndpointRequestStats) <$> Endpoints.endpointRequestStatsByEndpoint eid
        shapes <- Shapes.shapesByEndpointHash pid endpoint.hash
        fields <- Fields.selectFields pid endpoint.hash
        let fieldsMap = Fields.groupFieldsByCategory fields
        let shapesWithFieldsMap = Vector.map (`getShapeFields` fields) shapes
        let maxV = round enpStats.max :: Int
        let steps = (maxV `quot` 100) :: Int
        let steps' = if steps == 0 then 100 else steps
        reqLatenciesRolledBySteps <- RequestDumps.selectReqLatenciesRolledBySteps maxV steps' pid endpoint.urlPath endpoint.method
        pure (endpoint, enpStats, Vector.toList shapesWithFieldsMap, fieldsMap, Vector.toList reqLatenciesRolledBySteps)
      let subPage = fromMaybe "overview" subPageM
      shapesList <- dbtToEff do
        if subPage == "shapes" then Shapes.shapesByEndpointHash pid endpoint.hash else pure []
      let reqLatenciesRolledByStepsJ = decodeUtf8 $ AE.encode reqLatenciesRolledByStepsLabeled
      currTime <- liftIO getCurrentTime
      let currentURL = "/p/" <> pid.toText <> "/endpoints/" <> Endpoints.endpointIdText eid <> "?from=" <> fromMaybe "" fromDStr <> "&to=" <> fromMaybe "" toDStr
      let subPage = fromMaybe "overview" subPageM
      let currentPickerTxt = case sinceStr of
            Just a -> a
            Nothing -> maybe "" (toText . formatTime defaultTimeLocale "%F %T") fromD <> " - " <> maybe "" (toText . formatTime defaultTimeLocale "%F %T") toD
      let paramInput = ParamInput {currentURL = currentURL, sinceStr = sinceStr, dateRange = (fromD, toD), currentPickerTxt = currentPickerTxt, subPage = subPage}
      addRespHeaders $ EndpointsDetailsMain $ PageCtx bwconf (pid, paramInput, currTime, endpoint, enpStats, shapesWithFieldsMap, fieldsMap, shapesList, shapeHashM, reqLatenciesRolledByStepsJ, (fromD, toD))

data EndpointDetailsGet
  = EndpointsDetailsMain (PageCtx (Projects.ProjectId, ParamInput, UTCTime, Endpoints.Endpoint, Endpoints.EndpointRequestStats, [Shapes.ShapeWithFields], (Map FieldCategoryEnum [Fields.Field]), (Vector Shapes.Shape), (Maybe Text), Text, (Maybe ZonedTime, Maybe ZonedTime)))
  | EndpointsDetailsNotFound (PageCtx ())

instance ToHtml EndpointDetailsGet where
  toHtml (EndpointsDetailsMain (PageCtx bwConf (pid, paramInput, currTime, endpoint, endpointStats, shapesWithFieldsMap, fieldsM, shapesList, shapeHashM, reqLatenciesRolledByStepsJ, dateRange))) = toHtml $ PageCtx bwConf $ endpointDetails pid paramInput currTime endpoint endpointStats shapesWithFieldsMap fieldsM shapesList shapeHashM reqLatenciesRolledByStepsJ dateRange
  toHtml (EndpointsDetailsNotFound (PageCtx bwConf ())) = toHtml $ PageCtx bwConf endpointDetailsNotFound
  toHtmlRaw = toHtml

endpointDetailsNotFound :: Html ()
endpointDetailsNotFound = do
  div_ [class_ "flex flex-col items-center justify-center h-full"] do
    h1_ "Endpoint not found"

endpointDetails :: Projects.ProjectId -> ParamInput -> UTCTime -> Endpoints.Endpoint -> Endpoints.EndpointRequestStats -> [Shapes.ShapeWithFields] -> Map FieldCategoryEnum [Fields.Field] -> Vector Shapes.Shape -> Maybe Text -> Text -> (Maybe ZonedTime, Maybe ZonedTime) -> Html ()
endpointDetails pid paramInput currTime endpoint endpointStats shapesWithFieldsMap fieldsM shapesList shapeHashM reqLatenciesRolledByStepsJ dateRange = do
  let currentURLSubPage = deleteParam "subpage" paramInput.currentURL
  div_ [class_ "w-full h-full overflow-hidden"] do
    div_ [class_ "w-[75%] inline-block p-5 h-full overflow-y-scroll"] do
      div_ [class_ "flex flex-row justify-between mb-10"] do
        div_ [class_ "flex flex-col"] do
          div_ [class_ "flex flex-row place-items-center text-lg font-medium"] do
            h3_ [class_ "text-lg text-slate-800"] do
              span_ [class_ $ "p-1 endpoint endpoint-" <> toLower endpoint.method] $ toHtml $ endpoint.method <> " "
              strong_ [class_ "inconsolata text-xl"] $ toHtml endpoint.urlPath
            faSprite_ "chevron-down" "light" " h-4 w-4 m-2"
          p_ [class_ "text-gray-500 text-sm ml-2"] $ toHtml endpoint.description
        nav_ [class_ " space-x-4"] do
          subPageMenu
            & mapM_ \(title, slug) ->
              a_
                [ href_ $ currentURLSubPage <> "&subpage=" <> slug,
                  class_
                    $ "cursor-pointer px-3 py-2 font-medium text-sm rounded-md "
                    <> if slug == paramInput.subPage then " bg-indigo-100 text-indigo-700 " else " text-slate-500 hover:text-gray-700"
                ]
                $ toHtml title

      case paramInput.subPage of
        "api_docs" -> apiDocsSubPage shapesWithFieldsMap shapeHashM
        "shapes" -> shapesSubPage endpoint.projectId shapesList shapesWithFieldsMap paramInput.currentURL
        _ -> apiOverviewSubPage pid paramInput currTime endpointStats fieldsM reqLatenciesRolledByStepsJ dateRange

    aside_
      [ class_ "w-[25%] inline-block h-full overflow-y-auto overflow-x-hidden bg-white border border-gray-200 p-5 xsticky xtop-0 ",
        id_ "detailSidebar"
      ]
      do
        div_ [class_ "h-full flex flex-col items-center justify-center"] do
          faSprite_ "clapperboard" "light" "w-36 h-36"
          h3_ [class_ "mt-2 text-lg font-medium text-slate-900"] "Nothing selected"
          p_ [class_ "mt-1 text-sm text-slate-500"] "Select a field or similar item on the left"
          p_ [class_ "mt-1 text-sm text-slate-500"] "to view more details about it here."

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

shapesSubPage :: Projects.ProjectId -> Vector Shapes.Shape -> [Shapes.ShapeWithFields] -> Text -> Html ()
shapesSubPage pid shapesList shapesWithFields currentURL = do
  div_ [class_ "space-y-8", id_ "subpage"] do
    div_ [class_ "flex flex-col justify-between mt-2 shadow mx-16 rounded-xl"] do
      div_ [class_ "w-full bg-gray-100 px-8 py-4"] do
        h3_ [class_ "font-semibold text-lg"] "Endpoint Shapes"
      div_ [class_ "flex flex-col items-center gap-4"] do
        forM_ shapesWithFields \shape -> do
          div_ [class_ "flex w-full p-8 items-start gap-4  hover:bg-gray-50"] do
            a_ [href_ $ currentURL <> "&subpage=api_docs" <> "&shape=" <> shape.sHash, class_ "w-full items-start justify-between gap-10 flex text-slate-700"] do
              let statuscls = getStatusColor shape.status
              span_ [class_ $ "mt-10 px-3 py-1 " <> statuscls] $ show shape.status
              span_ [class_ "mt-12 text-sm text-gray-500"] $ toHtml shape.sHash
              let request_body_keypaths = (\f -> f.keyPath) <$> fromMaybe [] (Map.lookup Fields.FCRequestBody shape.fieldsMap)
              let response_body_keypaths = (\f -> f.keyPath) <$> fromMaybe [] (Map.lookup Fields.FCResponseBody shape.fieldsMap)
              let shapeJson =
                    AE.object
                      [ "response_headers" .= buildLeafJson (fromMaybe [] (Map.lookup Fields.FCResponseHeader shape.fieldsMap)),
                        "query_params" .= buildLeafJson (fromMaybe [] (Map.lookup Fields.FCQueryParam shape.fieldsMap)),
                        "path_params" .= buildLeafJson (fromMaybe [] (Map.lookup Fields.FCPathParam shape.fieldsMap)),
                        "request_body" .= convertKeyPathsToJson request_body_keypaths (fromMaybe [] (Map.lookup Fields.FCRequestBody shape.fieldsMap)) "",
                        "response_body" .= convertKeyPathsToJson response_body_keypaths (fromMaybe [] (Map.lookup Fields.FCResponseBody shape.fieldsMap)) ""
                      ]
              let shapeJsonStr = aesonValueToText shapeJson
              div_ [class_ "text-sm text-gray-500 p-4 bg-gray-100 whitespace-prerap w-2/3 h-[200px] overflow-auto flex flex-col gap-1 shape_json", style_ "font-family: monospace", term "data-json" shapeJsonStr] pass
            div_ [class_ "mt-10"] do
              -- let chartQuery = Just $ Charts.QBShapeHash shape.sHash
              -- div_ [class_ "flex items-center justify-center "] $ div_ [class_ "w-60 h-16 px-3"] $ Charts.throughput pid shape.sHash chartQuery Nothing 14 Nothing False (Nothing, Nothing) Nothing
              "chart"
  script_ [type_ "text/javascript"] ("" :: Text)
  script_
    [text|
     const shapesJson = document.querySelectorAll('.shape_json')
     shapesJson.forEach(shape => {
      try {
       const jsonData = shape.dataset.json
       const newText = JSON.stringify(JSON.parse(jsonData), null ,4)
       shape.innerText = newText
      }catch(err) {
        console.log(err)
      }
     })
  |]

apiDocsSubPage :: [Shapes.ShapeWithFields] -> Maybe Text -> Html ()
apiDocsSubPage shapesWithFieldsMap shapeHashM = do
  let fstH = viaNonEmpty head shapesWithFieldsMap
  let index = case shapeHashM of
        Just hash -> elemIndex hash (map Shapes.sHash shapesWithFieldsMap)
        Nothing -> Nothing
  let (targetShape, targetIndex) = case index of
        Just i -> (Just (shapesWithFieldsMap Unsafe.!! i), i + 1)
        Nothing -> (fstH, 1)
  let (st, hs) = case targetShape of
        Just s -> (s.status, s.sHash)
        Nothing -> (0, "No shapes")
  div_ [class_ "space-y-8", id_ "subpage"] do
    div_ [class_ "flex w-full justify-between mt-2"] do
      div_ [class_ "flex items-center gap-2"] do
        span_ [class_ "font-bold text-slate-700"] "Shapes:"
        div_ [class_ "relative flex items-center border rounded focus:ring-2 focus:ring-blue-200 active:ring-2 active:ring-blue-200", style_ "width:220px"] do
          button_
            [ [__| on click toggle .hidden on #shapes_container |],
              id_ "toggle_shapes_btn",
              data_ "current" (show targetIndex),
              data_ "total" (show $ length shapesWithFieldsMap),
              class_ "w-full flex text-slate-600 justify_between items-center cursor-pointer px-2 py-1"
            ]
            do
              let prm = "px-2 py-1 rounded text-white text-sm "
              let statusCls = if st < 400 then prm <> "bg-green-500" else prm <> "bg-red-500"
              span_ [class_ statusCls] $ show st
              span_ [class_ "ml-1 text-sm text-slate-600"] $ toHtml hs
          faSprite_ "chevron-down" "light" "h-4 w-4"
          div_ [id_ "shapes_container", class_ "absolute hidden bg-white border shadow w-full overflow-y-auto", style_ "top:100%; max-height: 300px; z-index:9"] do
            forM_ (zip [(1 :: Int) ..] shapesWithFieldsMap) $ \(index, s) -> do
              let prm = "px-2 py-1 rounded text-white text-sm "
              let statusCls = if s.status < 400 then prm <> "bg-green-500" else prm <> "bg-red-500"
              let prm = "p-2 w-full text-left truncate ... hover:bg-blue-100 hover:text-black"
              button_
                [ class_ prm,
                  id_ ("status_" <> show index),
                  [__|on click selectShape((me),(my @data-pos)) |],
                  data_ "pos" (show index),
                  data_ "status" (show s.status),
                  data_ "hash" s.sHash
                ]
                do
                  span_ [class_ statusCls] $ show s.status
                  span_ [class_ "ml-2 text-sm text-slate-600"] $ toHtml s.sHash

      div_ [class_ "flex items-center"] do
        a_ [href_ "#", onclick_ "slideReqRes('prev')"] $ faSprite_ "arrow-left" "regular" "h-6 w-6 m-2 cursor-pointer"
        let l = show targetIndex <> "/" <> show (length shapesWithFieldsMap)
        let id = "current_indicator"
        span_ [src_ " mx-4", id_ id] l
        a_ [href_ "#", onclick_ "slideReqRes('next')"] $ faSprite_ "arrow-right" "regular" "h-6 w-6 m-2 cursor-pointer"
    reqResSection "Request" True shapesWithFieldsMap targetIndex
    reqResSection "Response" False shapesWithFieldsMap targetIndex
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

apiOverviewSubPage :: Projects.ProjectId -> ParamInput -> UTCTime -> Endpoints.EndpointRequestStats -> Map Fields.FieldCategoryEnum [Fields.Field] -> Text -> (Maybe ZonedTime, Maybe ZonedTime) -> Html ()
apiOverviewSubPage pid paramInput currTime endpoint fieldsM reqLatenciesRolledByStepsJ dateRange = do
  let currentURLSearch = deleteParam "to" $ deleteParam "from" $ deleteParam "since" paramInput.currentURL
  div_ [class_ "space-y-16 pb-20", id_ "subpage"] do
    a_
      [ class_ "relative px-3 py-2 border border-1 border-black-200 space-x-2  inline-block relative cursor-pointer rounded-md",
        [__| on click toggle .hidden on #timepickerBox|]
      ]
      do
        faSprite_ "clock" "regular" "h-4 w-4"
        span_ [class_ "inline-block"] $ toHtml paramInput.currentPickerTxt
        faSprite_ "chevron-down" "light" "h-4 w-4 inline-block"
    div_ [id_ "timepickerBox", class_ "hidden absolute z-10 mt-1  rounded-md flex"] do
      div_ [class_ "inline-block w-84 overflow-auto bg-white py-1 text-base shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none sm:text-sm"] do
        forM_ timePickerItems \(val, title) ->
          a_
            [ class_ "block text-slate-900 relative cursor-pointer select-none py-2 pl-3 pr-9 hover:bg-gray-200 ",
              href_ $ currentURLSearch <> "&since=" <> val
            ]
            $ toHtml title
        a_ [class_ "block text-slate-900 relative cursor-pointer select-none py-2 pl-3 pr-9 hover:bg-gray-200 ", [__| on click toggle .hidden on #timepickerSidebar |]] "Custom date range"
      div_ [class_ "inline-block relative hidden", id_ "timepickerSidebar"] do
        div_ [id_ "startTime", class_ "hidden"] ""
    section_ $ AnomaliesList.anomalyListSlider currTime pid (Just endpoint.endpointId) Nothing
    endpointStats endpoint reqLatenciesRolledByStepsJ dateRange

endpointStats :: Endpoints.EndpointRequestStats -> Text -> (Maybe ZonedTime, Maybe ZonedTime) -> Html ()
endpointStats enpStats@Endpoints.EndpointRequestStats {min, p50, p75, p90, p95, p99, max} reqLatenciesRolledByStepsJ dateRange@(fromD, toD) =
  section_ [class_ "space-y-3"] do
    div_ [class_ "flex justify-between mt-5"]
      $ div_
        [class_ "flex flex-row"]
        do
          a_ [href_ "#", [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .endpointStatsSubSection)|]] $ faSprite_ "chevron-down" "regular" "h-4 mr-3 mt-1 w-4 cursor-pointer"
          span_ [class_ "text-lg text-slate-800"] "Endpoint Stats"
    div_ [class_ "space-y-5 endpointStatsSubSection"] do
      div_ [class_ "grid grid-cols-3  gap-5"] do
        statBox Nothing "Total Anomalies" "Total Anomalies for this endpoint this week vs total for the project" enpStats.ongoingAnomaliesProj (Just enpStats.ongoingAnomaliesProj)
        statBox Nothing "Total Requests" "Total Requests on this endpoint this week vs total for the project" enpStats.totalRequests (Just enpStats.totalRequestsProj)
        statBox Nothing "Total Time" "Total Time on this endpoint this week vs total for the project" enpStats.totalRequests (Just enpStats.totalRequestsProj)

      div_ [class_ "flex gap-5"] do
        div_ [class_ "flex-1 card-round p-3"] do
          div_ [class_ "p-4 space-y-6"] do
            select_ [] do
              option_ [class_ "text-2xl font-normal"] "Requests by Status Code"
            div_ [class_ "h-64 "] do
              Charts.lazy [C.QByE $ [C.QBPId enpStats.projectId, C.QBEndpointHash enpStats.endpointHash] ++ catMaybes [C.QBFrom <$> fromD, C.QBTo <$> toD], C.GByE C.GBStatusCode, C.SlotsE 120, C.ShowLegendE]

        div_ [class_ "flex-1 card-round p-3"] do
          div_ [class_ "p-4 space-y-6"] do
            select_ [] do
              option_ [class_ "text-2xl font-normal"] "Latency Percentiles"
            div_ [class_ "h-64 "] do
              Charts.lazy [C.QByE $ [C.QBPId enpStats.projectId, C.QBEndpointHash enpStats.endpointHash] ++ catMaybes [C.QBFrom <$> fromD, C.QBTo <$> toD], C.GByE C.GBDurationPercentile, C.SlotsE 120, C.ShowLegendE, C.TypeE C.LineCT]

      div_ [class_ "flex gap-5"] do
        div_ [class_ "flex-1 card-round p-3"] do
          div_ [class_ "p-4 space-y-6"] do
            select_ [] do
              option_ [class_ "text-2xl font-normal"] "Errors"
            div_ [class_ "h-64 "] do
              Charts.lazy [C.QByE $ [C.QBPId enpStats.projectId, C.QBEndpointHash enpStats.endpointHash, Charts.QBStatusCodeGT 400] ++ catMaybes [C.QBFrom <$> fromD, C.QBTo <$> toD], C.GByE C.GBStatusCode, C.SlotsE 120, C.ShowLegendE, C.Theme "roma"]

        div_ [class_ "flex-1 card-round p-3"] do
          div_ [class_ "p-4 space-y-6"] do
            select_ [] do
              option_ [class_ "text-2xl font-normal"] "Reqs Grouped by Endpoint"
            div_ [class_ "h-64 "] do
              Charts.lazy [C.QByE $ [C.QBPId enpStats.projectId, C.QBEndpointHash enpStats.endpointHash] ++ catMaybes [C.QBFrom <$> fromD, C.QBTo <$> toD], C.GByE C.GBEndpoint, C.SlotsE 120, C.ShowLegendE]

      div_ [class_ "col-span-3 bg-white   border border-gray-100  rounded-xl py-3 px-6"] do
        div_ [class_ "p-4"]
          $ select_
            []
            do
              option_ "Request Latency Distribution"
              option_ "Avg Reqs per minute"
        div_ [class_ "grid grid-cols-9  gap-8 w-full"] do
          div_ [id_ "reqsLatencyHistogram", class_ "col-span-7 h-72"] ""
          div_ [class_ "col-span-2 space-y-4 "] do
            strong_ [class_ "block text-right"] "Latency Percentiles"
            ul_ [class_ "space-y-1 divide-y divide-slate-100"] do
              percentileRow "max" enpStats.max
              percentileRow "p99" enpStats.p99
              percentileRow "p95" enpStats.p95
              percentileRow "p90" enpStats.p90
              percentileRow "p75" enpStats.p75
              percentileRow "p50" enpStats.p50
              percentileRow "min" enpStats.min
        script_ [int|| latencyHistogram('reqsLatencyHistogram',{p50:#{p50}, p75:#{p75}, p90:#{p90}, p95:#{p95}, p99:#{p99}, max:#{max}},  #{reqLatenciesRolledByStepsJ}) |]

percentileRow :: Text -> Double -> Html ()
percentileRow key p = do
  let (d, unit) = fmtDuration p
  li_ [class_ "flex flex-row content-between justify-between"] do
    span_ [class_ "inline-block"] $ toHtml key
    span_ [class_ "inline-block font-mono"] do
      span_ [class_ "tabular-nums"] $ toHtml d
      span_ $ toHtml unit

fmtDuration :: Double -> (Text, Text)
fmtDuration d
  | d > 1000 = (fmt $ fixedF 2 (d / 1000), "s")
  | otherwise = (fmt $ fixedF 0 d, "ms")

-- NOTE: We could enable the fields cycling functionality using the groups of response list functionality on the endpoint.
-- So we go through the list and in each request or response view, only show the fields that appear in the field list.
-- We can enable a view to show all the request/response options.
reqResSection :: Text -> Bool -> [Shapes.ShapeWithFields] -> Int -> Html ()
reqResSection title isRequest shapesWithFieldsMap targetIndex =
  section_ [class_ "space-y-3"] do
    div_ [class_ "flex justify-between mt-5"] do
      div_ [class_ "flex flex-row"] do
        a_ [class_ "cursor-pointer", [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .reqResSubSection)|]]
          $ faSprite_ "chevron-down" "light" "h-4 mr-3 mt-1 w-4"
        span_ [class_ "text-lg text-slate-800"] $ toHtml title

    div_ [class_ "bg-white border border-gray-100 rounded-xl py-5 px-5 space-y-6 reqResSubSection"]
      $ forM_ (zip [(1 :: Int) ..] shapesWithFieldsMap)
      $ \(index, s) -> do
        let sh = if index == targetIndex then title <> "_fields" else title <> "_fields hidden"
        div_ [class_ sh, id_ $ title <> "_" <> show index] do
          if isRequest
            then do
              subSubSection (title <> " Path Params") (Map.lookup Fields.FCPathParam s.fieldsMap) Nothing
              subSubSection (title <> " Query Params") (Map.lookup Fields.FCQueryParam s.fieldsMap) Nothing
              subSubSection (title <> " Headers") (Map.lookup Fields.FCRequestHeader s.fieldsMap) Nothing
              subSubSection (title <> " Body") (Map.lookup Fields.FCRequestBody s.fieldsMap) (Just s.reqDescription)
            else do
              subSubSection (title <> " Headers") (Map.lookup Fields.FCResponseHeader s.fieldsMap) Nothing
              subSubSection (title <> " Body") (Map.lookup Fields.FCResponseBody s.fieldsMap) (Just s.resDescription)

-- | subSubSection ..
subSubSection :: Text -> Maybe [Fields.Field] -> Maybe Text -> Html ()
subSubSection title fieldsM descriptionM =
  case fieldsM of
    Nothing -> ""
    Just fields -> do
      div_ [class_ "space-y-1 mb-4"] do
        div_ [class_ "flex flex-row items-center"] do
          a_ [href_ "#", [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .subSectionContent)|]] $ faSprite_ "chevron-down" "regular" "h-6 mr-3 w-6 p-1 cursor-pointer"
          div_ [class_ "flex flex-row gap-2 bg-gray-100 px-10 rounded-xl w-full p-4 text-sm text-slate-900 "] do
            toHtml title
            p_ [class_ "text-sm text-gray-700"] $ toHtml $ fromMaybe "" descriptionM
        div_ [class_ "space-y-1 subSectionContent"] do
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
                  [ class_ "flex flex-row items-center",
                    style_ depthPadding,
                    [__| on click toggle .neg-rotate-90 on <.chevron/> in me then collapseUntil((me), (my @data-depth))  |],
                    term "data-depth" $ show depth
                  ]
                  do
                    faSprite_ "chevron-down" "light" "h-6 w-6 mr-1 chevron cursor-pointer p-1"
                    div_ [class_ "border flex flex-row border-gray-100 px-5 py-2 rounded-xl w-full"] do
                      input_ [type_ "checkbox", class_ " mr-12"]
                      span_ [class_ "text-sm text-slate-800 inline-flex items-center"] $ toHtml displayKey
                      span_ [class_ "text-sm text-slate-600 inline-flex items-center ml-4"] do
                        if "[*]" `isSuffixOf` key
                          then EndpointComponents.fieldTypeToDisplay Fields.FTList
                          else EndpointComponents.fieldTypeToDisplay Fields.FTObject
              Just field -> do
                a_
                  [ hxGet_ $ "/p/" <> field.projectId.toText <> "/fields/" <> UUID.toText (Fields.unFieldId field.id),
                    hxTarget_ "#detailSidebar",
                    class_ "flex flex-row cursor-pointer",
                    style_ depthPadding,
                    term "data-depth" $ show depth
                  ]
                  do
                    faSprite_ "chevron-down" "light" "h-4 mr-3 mt-4 w-4 invisible"
                    div_ [class_ "border flex flex-row border-gray-100 px-5 py-2 rounded-xl w-full items-center"] do
                      input_ [type_ "checkbox", class_ " mr-12"]
                      div_ [class_ "flex gap-2 items-center grow"] do
                        span_ [class_ "text-sm text-slate-800 inline-flex items-center"] $ toHtml displayKey
                        when field.isRequired do span_ [class_ "text-red-500", term "data-tippy-content" "required field"] "*"
                      span_ [class_ "text-sm text-slate-600 mx-12 inline-flex items-center"] $ EndpointComponents.fieldTypeToDisplay field.fieldType
                      faSprite_ "octagon-exclamation" "regular" " mr-8 ml-4 h-5 text-red-400"
                      faSprite_ "ellipsis-vertical" "light" "mx-5 h-5"

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
-- fieldsToNormalized :: [Fields.Field] -> [(Text, Maybe Fields.Field)]
-- fieldsToNormalized =
--   sortNub . concatMap \field ->
--     map
--       ((,Nothing) . fst)
--       ( field.keyPath
--           & T.dropWhile (== '.')
--           & breakOnAll "."
--       )
--       & (++ [(T.dropWhile (== '.') $ field.keyPath, Just field)])

buildLeafJson :: [Fields.Field] -> AE.Value
buildLeafJson = foldr (\f acc -> mergeObjects acc (AE.object [AEKey.fromText f.key AE..= fieldTypeToText f.fieldType])) (AE.object [])

mergeObjects :: AE.Value -> AE.Value -> AE.Value
mergeObjects (AE.Object obj1) (AE.Object obj2) = AE.Object (obj1 <> obj2)
mergeObjects _ _ = AE.object []

data KeyPathGroup = KeyPathGroup
  { subGoups :: [Text],
    keyPath :: Text
  }
  deriving stock (Show, Generic)

convertKeyPathsToJson :: [Text] -> [Fields.Field] -> Text -> AE.Value
convertKeyPathsToJson items categoryFields parentPath = convertToJson' groups
  where
    -- Debug logs
    -- !() = trace (show items) ()
    -- !() = trace (show categoryFields) ()
    -- !() = trace (show parentPath) ()
    groups = processItems items Map.empty

    safeTail :: Text -> Text
    safeTail txt = if T.null txt then txt else T.tail txt

    processGroup :: (Text, KeyPathGroup) -> AE.Value -> AE.Value
    processGroup (grp, keypath) parsedValue =
      let updatedJson
            | null keypath.subGoups =
                let field = find (\fi -> safeTail (parentPath <> "." <> grp) == fi.keyPath) categoryFields
                    t = extractInfo field categoryFields parentPath grp
                 in AE.object [AEKey.fromText grp AE..= t]
            | T.null grp = convertKeyPathsToJson keypath.subGoups categoryFields (parentPath <> "." <> grp)
            | otherwise =
                let ob = AE.object [AEKey.fromText grp AE..= convertKeyPathsToJson keypath.subGoups categoryFields (parentPath <> "." <> grp)]
                 in ob
       in mergeObjects updatedJson parsedValue
    convertToJson' :: Map.Map T.Text KeyPathGroup -> AE.Value
    convertToJson' grps = foldr processGroup (AE.object []) (Map.toList grps)

processItem :: T.Text -> Map.Map T.Text KeyPathGroup -> Map.Map T.Text KeyPathGroup
processItem item groups =
  let splitItems = T.splitOn "." item
      c = fromMaybe "" (viaNonEmpty head splitItems)

      tmpRoot = T.dropWhile (== '.') c
      -- newArr checks if it's a new arr identifier
      newArr = T.isSuffixOf "[*]" tmpRoot
      root
        | newArr = tmpRoot
        | length splitItems > 1 && fromMaybe "" (viaNonEmpty head (fromMaybe [] (viaNonEmpty tail splitItems))) == "[]" = tmpRoot <> "[*]"
        | otherwise = tmpRoot
      remainingItems
        | newArr = T.intercalate "." $ fromMaybe [] (viaNonEmpty tail splitItems)
        | length splitItems > 1 =
            if fromMaybe "" (viaNonEmpty head (fromMaybe [] (viaNonEmpty tail splitItems))) == "[]"
              then T.intercalate "." $ fromMaybe [] (viaNonEmpty tail (fromMaybe [] (viaNonEmpty tail splitItems)))
              else T.intercalate "." $ fromMaybe [] (viaNonEmpty tail splitItems)
        | otherwise = ""

      updatedGroups = case Map.lookup root groups of
        Just items -> case remainingItems of
          "" -> groups
          val -> Map.insert root (KeyPathGroup {subGoups = items.subGoups ++ [remainingItems], keyPath = items.keyPath <> "." <> root}) groups
        Nothing -> case remainingItems of
          "" -> Map.insert root (KeyPathGroup {subGoups = [], keyPath = "." <> root}) groups
          val -> Map.insert root (KeyPathGroup {subGoups = [remainingItems], keyPath = "." <> root}) groups
   in updatedGroups

processItems :: [T.Text] -> Map.Map T.Text KeyPathGroup -> Map.Map T.Text KeyPathGroup
processItems [] groups = groups
processItems (x : xs) groups = processItems xs updatedGroups
  where
    updatedGroups = processItem x groups

extractInfo :: Maybe Fields.Field -> [Fields.Field] -> Text -> Text -> Text
extractInfo Nothing categoryFields parentPath grp =
  let newK = T.tail parentPath <> "." <> grp
      newF = find (\fi -> newK == fi.keyPath) categoryFields
      ob = case newF of
        Just f -> fieldTypeToText f.fieldType
        Nothing -> "string"
   in ob
extractInfo (Just f) _ _ _ = fieldTypeToText f.fieldType
