{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Pages.Endpoints.EndpointDetails (endpointDetailsH, fieldDetailsPartialH) where

import Config
import Data.Map qualified as Map
import Data.Text as T (breakOnAll, dropWhile, isInfixOf, replace, splitOn)
import Data.Time (defaultTimeLocale, formatTime)
import Data.UUID as UUID
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.HTMX
import Models.Apis.Endpoints
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields qualified as Fields
import Models.Apis.Formats qualified as Formats
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Optics.Core ((^.))
import Pages.BodyWrapper (bodyWrapper)
import Relude

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
        h6_ [class_ "text-slate-600 text-xs"] "TYPE"
        h4_ [class_ "text-base text-slate-600"] $ toHtml $ show $ format ^. #fieldType
      div_ [class_ "mx-5"] $ do
        h6_ [class_ "text-slate-600 text-xs"] "FORMAT"
        h4_ [class_ "text-base text-slate-600"] $ toHtml $ format ^. #fieldFormat
    h6_ [class_ "text-slate-600 mt-4 text-xs"] "EXAMPLE VALUES"
    ul_ [class_ "list-disc"] $ do
      format ^. #examples & mapM_ \ex -> do
        li_ [class_ "ml-10 text-slate-600 text-sm"] $ toHtml ex
  div_ [class_ "flex flex-row justify-between mt-10 "] $ do
    div_ [class_ " "] $ do
      h4_ [class_ "text-sm text-slate-600 mb-2"] "CREATION DATE"
      div_ [class_ "flex border border-gray-200 m-1 rounded-xl p-2"] $ do
        img_ [src_ "/assets/svgs/calender.svg", class_ "h-4 mr-2 w-4"]
        span_ [class_ "text-xs"] $ toHtml $ formatTime defaultTimeLocale "%b %d, %Y %R" (field ^. #createdAt)
    div_ [class_ " "] $ do
      h4_ [class_ "text-sm text-slate-600 mb-2"] "LAST CHANGE"
      div_ [class_ "flex border border-gray-200 m-1 justify-between rounded-xl p-2"] $ do
        img_ [src_ "/assets/svgs/calender.svg", class_ "h-4 mr-2 w-4"]
        span_ [class_ "text-xs"] $ toHtml $ formatTime defaultTimeLocale "%b %d, %Y %R" (field ^. #updatedAt)
  h6_ [class_ "mt-5 text-sm text-slate-600 mb-2"] "DESCRIPTION"
  p_ [class_ "text-gray-400 text-sm"] $ toHtml $ field ^. #description

endpointDetailsH :: Sessions.PersistentSession -> Projects.ProjectId -> Endpoints.EndpointId -> DashboardM (Html ())
endpointDetailsH sess pid eid = do
  pool <- asks pool
  (endpointM, project, fieldsMap) <- liftIO $
    withPool pool $ do
      endpointM <- Endpoints.endpointById eid
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      fieldsV <- Fields.selectFields eid
      let fieldsMap = Fields.groupFieldsByCategory fieldsV
      pure (endpointM, project, fieldsMap)

  case endpointM of
    Nothing -> pure $ toHtml ""
    Just endpoint -> pure $ bodyWrapper (Just sess) project "Endpoint Details" (endpointDetails endpoint fieldsMap)

endpointDetails :: Endpoint -> Map Fields.FieldCategoryEnum [Fields.Field] -> Html ()
endpointDetails endpoint fieldsM = do
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
        reqResSection "Request" True fieldsM
        reqResSection "Response" True fieldsM
    aside_ [class_ "w-1/3 bg-white h-screen -mr-8 -mt-5 border border-gray-200 ml-3 p-5 sticky top-0", id_ "detailSidebar"] $ toHtml ""

-- NB: We could enable the fields cycling functionality using the groups of response list functionality on the endpoint.
-- So we go through the list and in each request or response view, only show the fields that appear in the field list.
-- We can enable a view to show all the request/response options.
reqResSection :: Text -> Bool -> Map Fields.FieldCategoryEnum [Fields.Field] -> Html ()
reqResSection title isRequest fieldsM =
  section_ $ do
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
      div_ [class_ "bg-white rounded-xl p-5 space-y-2"] $ do
        div_ [class_ "flex flex-row "] $ do
          img_ [src_ "/assets/svgs/cheveron-down.svg", class_ "h-4 mr-3 mt-4 w-4"]
          div_ [class_ "bg-gray-100 px-10 rounded-xl w-full p-4 text-sm text-slate-600"] $ toHtml title
        fieldsToNormalized fields & mapM_ \(key, fieldM) -> do
          let segments = splitOn "." key
          let depth = length segments
          let depthPadding = "margin-left:" <> show (20 + (depth * 20)) <> "px"
          let displayKey = replace "»" "" $ last ("" :| segments)
          case fieldM of
            Nothing -> do
              a_ [class_ "flex flex-row cursor-pointer", style_ depthPadding, term "data-depth" $ show depth] $ do
                img_ [src_ "/assets/svgs/cheveron-down.svg", class_ "h-4 mr-3 mt-4 w-4"]
                div_ [class_ "border flex flex-row border-gray-100 px-5 p-3 rounded-xl w-full"] $ do
                  input_ [type_ "checkbox", class_ " mr-12 m-1"]
                  span_ [class_ "grow text-sm text-slate-600"] $ toHtml displayKey
                  span_ [class_ "text-sm text-slate-500 mx-12"] $ toHtml $ if "»" `isInfixOf` key then "[]" else "{}"
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
                  div_ [class_ "border flex flex-row border-gray-100 px-5 p-3 rounded-xl w-full"] $ do
                    input_ [type_ "checkbox", class_ " mr-12 m-1"]
                    span_ [class_ "grow text-sm text-slate-600"] $ toHtml displayKey
                    span_ [class_ "text-sm text-slate-500 mx-12"] $ toHtml $ show $ field ^. #fieldType
                    img_ [src_ "/assets/svgs/alert-red.svg", class_ " mx-10 "]
                    img_ [src_ "/assets/svgs/dots-vertical.svg", class_ "mx-5"]
        div_ [class_ " border-2 border-dashed flex mb-5 flex-row border-gray-100 ml-5  px-5 p-3 rounded-xl mt-2 "] $ do
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
