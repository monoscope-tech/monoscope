{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Pages.Documentation (documentationGetH, documentationPostH, documentationPutH, SwaggerForm, SaveSwaggerForm) where

import Config
import Data.Aeson (FromJSON, ToJSON, decodeStrict, encode)
import Data.Aeson qualified as AE
import Data.Aeson.QQ (aesonQQ)
import Data.Default (def)
import Data.Digest.XXHash
import Data.Text qualified as T
import Data.Time.LocalTime (ZonedTime, getZonedTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4

import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)

import Lucid
import Lucid.Htmx
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields (parseFieldCategoryEnum, parseFieldTypes)
import Models.Apis.Fields qualified as Fields
import Models.Projects.Projects qualified as Projects
import Models.Projects.Swaggers qualified as Swaggers
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)

import Numeric (showHex)
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Pages.GenerateSwagger qualified as GenerateSwagger
import Relude
import Servant (Headers, addHeader)
import Servant.Htmx (HXTrigger)

import Data.List (nubBy)
import Models.Apis.Fields.Query qualified as Fields
import Models.Apis.Formats qualified as Formats
import Models.Apis.Shapes qualified as Shapes
import Relude.Unsafe as Unsafe hiding (head)
import Web.FormUrlEncoded (FromForm)

data SwaggerForm = SwaggerForm
  { swagger_json :: Text
  , from :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)

data FieldOperation = FieldOperation
  { action :: Text
  , keypath :: Text
  , url :: Text
  , method :: Text
  , description :: Text
  , example :: AE.Value
  , category :: Text
  , ftype :: Text
  , format :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data KeyPathData = KeyPathData
  { fkKeyPath :: Text
  , fkCategory :: Text
  , fkType :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OpShape = OpShape
  { opOperations :: V.Vector FieldOperation
  , opShapeChanged :: Bool
  , opRequestBodyKeyPaths :: V.Vector KeyPathData
  , opResponseBodyKeyPaths :: V.Vector KeyPathData
  , opRequestHeadersKeyPaths :: V.Vector KeyPathData
  , opResponseHeadersKeyPaths :: V.Vector KeyPathData
  , opQueryParamsKeyPaths :: V.Vector KeyPathData
  , opMethod :: Text
  , opUrl :: Text
  , opStatus :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OpEndpoint = OpEndpoint
  { endpointUrl :: Text
  , endpointMethod :: Text
  , endpointHost :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SaveSwaggerForm = SaveSwaggerForm
  { updated_swagger :: Text
  , swagger_id :: Text
  , endpoints :: V.Vector OpEndpoint
  , diffsInfo :: V.Vector OpShape
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

getEndpointHash :: Projects.ProjectId -> Text -> Text -> Text
getEndpointHash pid urlPath method = toText $ showHex (xxHash $ encodeUtf8 $ (UUID.toText pid.unProjectId) <> T.toUpper method <> urlPath) ""

getFieldHash :: Text -> Text -> Text -> Text -> Text
getFieldHash enpHash fcategory keypath ftype = enpHash <> toText (showHex (xxHash $ encodeUtf8 (fcategory <> keypath <> ftype)) "")

getShapeHash :: Text -> Text -> V.Vector Text -> V.Vector Text -> V.Vector Text -> V.Vector Text -> Text
getShapeHash endpointHash statusCode responseBodyKP responseHeadersKP requestBodyKP queryParamsKP = shapeHash
 where
  comb = T.concat $ sort $ V.toList $ queryParamsKP <> responseHeadersKP <> requestBodyKP <> responseBodyKP
  keyPathsHash = toText $ showHex (xxHash $ encodeUtf8 comb) ""
  shapeHash = endpointHash <> statusCode <> keyPathsHash

getEndpointFromOpEndpoint :: Projects.ProjectId -> OpEndpoint -> Endpoints.Endpoint
getEndpointFromOpEndpoint pid opEndpoint =
  let endpointHash = getEndpointHash pid opEndpoint.endpointUrl opEndpoint.endpointMethod
   in Endpoints.Endpoint
        { id = Endpoints.EndpointId UUID.nil
        , createdAt = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
        , updatedAt = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
        , projectId = pid
        , urlPath = opEndpoint.endpointUrl
        , method = T.toUpper opEndpoint.endpointMethod
        , urlParams = AE.Array []
        , hosts = [opEndpoint.endpointHost]
        , hash = endpointHash
        }

getShapeFromOpShape :: Projects.ProjectId -> ZonedTime -> OpShape -> Shapes.Shape
getShapeFromOpShape pid curTime opShape =
  Shapes.Shape
    { id = Shapes.ShapeId UUID.nil
    , projectId = pid
    , createdAt = curTime
    , updatedAt = curTime
    , approvedOn = Just curTime
    , endpointHash = endpointHash
    , queryParamsKeypaths = qpKP
    , requestBodyKeypaths = rqBKP
    , responseBodyKeypaths = rsBKP
    , requestHeadersKeypaths = rqHKP
    , responseHeadersKeypaths = rsHKP
    , fieldHashes = fieldHashes
    , hash = shapeHash
    , statusCode = fromMaybe 0 (readMaybe (toString opShape.opStatus))
    }
 where
  endpointHash = getEndpointHash pid opShape.opUrl opShape.opMethod
  qpKP = V.map (.fkKeyPath) opShape.opQueryParamsKeyPaths
  rqHKP = V.map (.fkKeyPath) opShape.opRequestHeadersKeyPaths
  rqBKP = V.map (.fkKeyPath) opShape.opRequestBodyKeyPaths
  rsHKP = V.map (.fkKeyPath) opShape.opResponseHeadersKeyPaths
  rsBKP = V.map (.fkKeyPath) opShape.opResponseBodyKeyPaths
  shapeHash = getShapeHash endpointHash opShape.opStatus rsBKP rsHKP rqBKP qpKP
  qpKPHashes = V.map (\v -> getFieldHash endpointHash v.fkCategory v.fkKeyPath v.fkType) opShape.opQueryParamsKeyPaths
  rqHKPHashes = V.map (\v -> getFieldHash endpointHash v.fkCategory v.fkKeyPath v.fkType) opShape.opRequestHeadersKeyPaths
  rqBKPHashes = V.map (\v -> getFieldHash endpointHash v.fkCategory v.fkKeyPath v.fkType) opShape.opRequestBodyKeyPaths
  rsHKPHashes = V.map (\v -> getFieldHash endpointHash v.fkCategory v.fkKeyPath v.fkType) opShape.opResponseHeadersKeyPaths
  rsBKPHashes = V.map (\v -> getFieldHash endpointHash v.fkCategory v.fkKeyPath v.fkType) opShape.opResponseBodyKeyPaths
  fieldHashes = qpKPHashes <> rqHKPHashes <> rqBKPHashes <> rsHKPHashes <> rsBKPHashes

getFieldAndFormatFromOpShape :: Projects.ProjectId -> FieldOperation -> (Fields.Field, Formats.Format)
getFieldAndFormatFromOpShape pid operation =
  let endpointHash = getEndpointHash pid operation.url operation.method
      fCategory = fromMaybe Fields.FCRequestBody (parseFieldCategoryEnum operation.category)
      fieldType = fromMaybe Fields.FTString (parseFieldTypes operation.ftype)
      fieldHash = getFieldHash endpointHash operation.category operation.keypath operation.ftype
      keyPath = operation.keypath
      format = operation.format
      formatHash = fieldHash <> toText (showHex (xxHash $ encodeUtf8 format) "")

      field =
        Fields.Field
          { createdAt = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
          , updatedAt = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
          , id = Fields.FieldId UUID.nil
          , endpointHash = endpointHash
          , projectId = pid
          , key = snd $ T.breakOnEnd "." keyPath
          , fieldType = fieldType
          , fieldTypeOverride = Nothing
          , format = format
          , formatOverride = Just ""
          , description = operation.description
          , keyPath = keyPath
          , fieldCategory = fCategory
          , hash = fieldHash
          }

      lFormat =
        Formats.Format
          { id = Formats.FormatId UUID.nil
          , createdAt = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
          , updatedAt = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
          , projectId = pid
          , fieldHash = fieldHash
          , fieldType = fieldType
          , fieldFormat = format
          , examples = V.fromList [operation.example]
          , hash = formatHash
          }
   in (field, lFormat)

flattenVector :: [V.Vector FieldOperation] -> V.Vector FieldOperation
flattenVector = V.concat

documentationPutH :: Sessions.PersistentSession -> Projects.ProjectId -> SaveSwaggerForm -> DashboardM (Headers '[HXTrigger] (Html ()))
documentationPutH sess pid SaveSwaggerForm{updated_swagger, swagger_id, endpoints, diffsInfo} = do
  pool <- asks pool
  env <- asks env
  let value = case decodeStrict (encodeUtf8 updated_swagger) of
        Just val -> val
        Nothing -> error "Failed to parse JSON: "
  res <- liftIO $ withPool pool $ do
    currentTime <- liftIO getZonedTime
    let newEndpoints = V.toList $ V.map (getEndpointFromOpEndpoint pid) endpoints
    let shapes = V.toList (V.map (getShapeFromOpShape pid currentTime) (V.filter (.opShapeChanged) diffsInfo))
    let nestedOps = V.map (.opOperations) diffsInfo
    let ops = flattenVector (V.toList nestedOps)
    let fAndF = V.toList (V.map (getFieldAndFormatFromOpShape pid) ops)
    let fields = nubBy (\x y -> x.hash == y.hash) (map fst fAndF) -- to prevent ON CONFLICT DO UPDATE command cannot affect row a second time
    let formats = nubBy (\x y -> x.hash == y.hash) (map snd fAndF) -- to prevent ON CONFLICT DO UPDATE command cannot affect row a second time
    Formats.insertFormats formats
    Fields.insertFields fields
    Shapes.insertShapes shapes
    Endpoints.insertEndpoints newEndpoints

    case swagger_id of
      "" -> do
        swaggerId <- Swaggers.SwaggerId <$> liftIO UUIDV4.nextRandom
        let swaggerToAdd = Swaggers.Swagger{id = swaggerId, projectId = pid, createdBy = sess.userId, createdAt = currentTime, updatedAt = currentTime, swaggerJson = value}
        Swaggers.addSwagger swaggerToAdd
        pass
      _ -> do
        Swaggers.updateSwagger swagger_id value
        pass

  let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"closeModal": "", "successToast": ["Swagger Saved Successfully"]}|]
  pure $ addHeader hxTriggerData ""

documentationPostH :: Sessions.PersistentSession -> Projects.ProjectId -> SwaggerForm -> DashboardM (Headers '[HXTrigger] (Html ()))
documentationPostH sess pid SwaggerForm{swagger_json, from} = do
  pool <- asks pool
  env <- asks env
  swaggerId <- Swaggers.SwaggerId <$> liftIO UUIDV4.nextRandom
  currentTime <- liftIO getZonedTime
  let value = case decodeStrict (encodeUtf8 swagger_json) of
        Just val -> val
        Nothing -> error "Failed to parse JSON"
  let swaggerToAdd =
        Swaggers.Swagger
          { id = swaggerId
          , projectId = pid
          , createdBy = sess.userId
          , createdAt = currentTime
          , updatedAt = currentTime
          , swaggerJson = value
          }

  swaggers <- liftIO $
    withPool pool $ do
      Swaggers.addSwagger swaggerToAdd
      Swaggers.swaggersByProject pid

  let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"closeModal": "", "successToast": ["Swagger uploaded Successfully"]}|]
  pure $ addHeader hxTriggerData ""

documentationGetH :: Sessions.PersistentSession -> Projects.ProjectId -> Maybe Text -> DashboardM (Html ())
documentationGetH sess pid swagger_id = do
  pool <- asks pool
  (project, swaggers, swagger, swaggerId) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      swaggers <- Swaggers.swaggersByProject pid
      currentSwagger <- case swagger_id of
        Just swId -> Swaggers.getSwaggerById swId
        Nothing -> pure Nothing
      (swaggerVal, swaggerValId) <- case (swaggers, currentSwagger) of
        (_, Just swg) -> do
          let sw = swg.swaggerJson
          let idx = show swg.id.swaggerId
          pure (sw, idx)
        ([], Nothing) -> do
          endpoints <- Endpoints.endpointsByProjectId pid
          let endpoint_hashes = V.map (.hash) endpoints
          shapes <- Shapes.shapesByEndpointHashes pid endpoint_hashes
          fields <- Fields.fieldsByEndpointHashes pid endpoint_hashes
          let field_hashes = V.map (.fHash) fields
          formats <- Formats.formatsByFieldsHashes pid field_hashes
          let (projectTitle, projectDescription) = case project of
                (Just pr) -> (toText pr.title, toText pr.description)
                Nothing -> ("__APITOOLKIT", "Edit project description")
          let gn = GenerateSwagger.generateSwagger projectTitle projectDescription endpoints shapes fields formats
          pure (gn, "")
        (val, Nothing) -> do
          let latest = V.head $ V.reverse swaggers
          let sw = latest.swaggerJson
          let idx = show latest.id.swaggerId
          pure (sw, idx)
      pure (project, V.reverse swaggers, swaggerVal, swaggerValId)

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = project
          , pageTitle = "Documentation"
          }
  pure $ bodyWrapper bwconf $ documentationsPage pid swaggers swaggerId (decodeUtf8 (encode swagger))

documentationsPage :: Projects.ProjectId -> V.Vector Swaggers.Swagger -> String -> String -> Html ()
documentationsPage pid swaggers swaggerID jsonString = do
  div_ [class_ "relative h-full"] $ do
    -- modal
    div_
      [ style_ "z-index:99999"
      , class_ "fixed pt-24 hidden justify-center z-50 w-full p-4 bg-gray-500 bg-opacity-75 overflow-y-auto inset-0 h-full max-h-full"
      , id_ "swaggerModal"
      , tabindex_ "-1"
      , onclick_ "closeModal(event)"
      ]
      $ do
        div_
          [ class_ "relative max-h-full"
          , style_ "width: min(90vw, 1000px)"
          ]
          $ do
            -- Modal content
            div_
              [ class_ "bg-white rounded-lg shadow w-full"
              -- , hxPost_ $ "/p/" <> pid.toText <> "/documentation"
              ]
              $ do
                div_ [class_ "flex items-start justify-between p-6 space-x-2  border-b rounded-t"] $ do
                  h3_ [class_ "text-xl font-semibold text-gray-900 dark:text-white"] "Save Swagger"
                -- Modal body
                div_ [class_ "w-full"] $ do
                  div_ [id_ "diff_editor_container", style_ "height:65vh; width:100%"] pass
                -- input_ [type_ "hidden", name_ "from", value_ "docs"]
                -- textarea_ [style_ "height:65vh;resize:none", name_ "swagger_json", class_ "w-full border outline-none p-4 focus:outline-none focus:border-blue-200", placeholder_ "Paste swagger here"] ""
                -- Modal footer
                div_ [class_ "flex w-full justify-end items-center p-6 space-x-2 border-t border-gray-200 rounded-b"] $ do
                  button_ [style_ "margin-right:50px", type_ "button", class_ "btn", onclick_ "closeModal(event)", id_ "close_btn"] "Close"
                  button_ [type_ "sumbit", class_ "btn btn-primary", onclick_ "parsePaths()"] "Confirm  & Save"

    -- page content

    input_ [id_ "swaggerData", type_ "hidden", value_ (toText jsonString)]
    div_ [class_ "flex flex-col h-full w-full justify-between"] $ do
      div_ [class_ "flex w-full bg-white border-b items-center justify-between px-2", style_ "top: 0; height:60px; position: sticky"] $ do
        div_ [class_ "flex items-center gap-4"] $ do
          h3_ [class_ "text-xl text-slate-700 text-2xl font-medium"] "Swagger"
          div_ [class_ "relative", style_ "width:200px"] $ do
            button_
              [ onclick_ "toggleSwaggerHistory(event)"
              , id_ "toggle_swagger_btn"
              , class_ "w-full flex gap-2 text-gray-600 justify_between items-center cursor-pointer px-2 py-1 border rounded focus:ring-2 focus:ring-blue-200 active:ring-2 active:ring-blue-200"
              ]
              $ do
                p_ [style_ "width: calc(100% - 25px)", class_ "truncate ..."] $ toHtml swaggerID
                img_ [src_ "/assets/svgs/select_chevron.svg", style_ "height:15px; width:15px"]
            div_ [id_ "swagger_history_container", class_ "absolute hidden bg-white border shadow w-full overflow-y-auto", style_ "top:100%; max-height: 300px; z-index:9"] $ do
              swaggers & mapM_ \sw -> do
                button_ [onclick_ "swaggerChanged(event)", class_ "p-2 w-full text-left truncate ... hover:bg-blue-100 hover:text-black"] $ toHtml swaggerID
        button_ [class_ "place-content-center text-md btn btn-primary", onclick_ "showModal()"] "Save swagger"

      div_ [class_ "w-full h-full", style_ "height: calc(100% - 60px)"] $ do
        div_ [id_ "columns_container", class_ "w-full h-full flex flex-row", style_ "height: 100%"] $ do
          -- loading indicator
          div_ [id_ "loading_indicator", class_ "fixed inset-0 flex justify-center bg-[rgba(0,0,0,0.4)] items-center", style_ "z-index:9999;"] $ do
            div_ [class_ "py-10 px-24 bg-white flex gap-2"] $ do
              div_ [class_ "animate-spin h-5 w-5 mr-3 rounded-full border-t border-8 border-blue-500"] pass
              span_ "Loading..."
          div_ [id_ "endpoints_container", class_ "flex flex-auto", style_ "width:30%; height:100%"] $ do
            div_ [class_ "h-full overflow-auto", style_ "width: calc(100% - 2px)"] $ do
              div_ [id_ "info_tags_container", class_ "w-full"] pass
              div_ [class_ "w-full"] $ do
                input_ [id_ "endpoints-search", type_ "text", class_ "w-full px-2 py-3 text-lg border-b border-t outline-none focus:outline-none", placeholder_ "Search.."]
                div_ [id_ "endpoint_paths_container", class_ "w-full"] pass
            div_ [onmousedown_ "mouseDown(event)", id_ "endpoints_resizer", class_ "h-full bg-neutral-400", style_ "width: 2px; cursor: col-resize; background-color: rgb(209 213 219)"] pass
          div_ [id_ "editor_container", class_ "flex flex-auto overflow-auto", style_ "width:40%; height:100%"] $ do
            div_ [class_ "h-full", style_ "width: calc(100% - 2px)"] $ do
              div_ [class_ "w-full flex gap-8 justify-end px-2 items-center", style_ "height:40px"] $ do
                div_ [onclick_ "toggleFontSize(event)", class_ "relative"] $ do
                  button_ [id_ "toggle_font", class_ "font-semibold"] "Aa"
                  div_ [id_ "toggle_dropdown_container", class_ "absolute hidden flex flex-col justify-between bg-white shadow bottom-0 rounded-b overflow-hidden", style_ "bottom:-105px;left:-50px; height: 100px;width:100px; z-index:999"] $ do
                    span_ [id_ "toggle_sm", class_ "cursor-pointer text-sm w-full px-3 py-2 hover:bg-blue-100"] "Small"
                    span_ [id_ "toggle_md", class_ "font_toggle_active cursor-pointer w-full px-3 py-2 hover:bg-blue-100"] "Medium"
                    span_ [id_ "toggle_lg", class_ "cursor-pointer text-lg w-full px-3 py-2 hover:bg-blue-100"] "Large"
                form_ [hxPost_ $ "/p/" <> pid.toText <> "/documentation/save"] $ do
                  input_ [id_ "save_swagger_input_id", name_ "swagger_id", type_ "hidden", value_ (toText swaggerID)]
                  input_ [id_ "save_swagger_input_data", name_ "updated_swagger", type_ "hidden", value_ (toText jsonString)]
              --  button_ [type_ "submit", id_ "save_swagger_btn", class_ "bg-gray-200 text-sm py-2 px-4 rounded active:bg-green-600"] "Save"
              div_ [id_ "swaggerEditor", class_ "w-full overflow-y-auto", style_ "height: calc(100% - 40px)"] pass
            div_ [onmousedown_ "mouseDown(event)", id_ "editor_resizer", class_ "h-full bg-neutral-400", style_ "width: 2px; cursor: col-resize; background-color: rgb(209 213 219);"] pass
          div_ [id_ "details_container", class_ "flex-auto overflow-y-auto", style_ "width:30%; height:100%"] $ do
            div_ [id_ "swagger-ui", class_ "relative h-full w-full bg-white overflow-auto"] pass
          button_ [class_ "absolute z-10 p-2", style_ "right: 15px", onclick_ "fullscreen()", title_ "full screen"] $ do
            img_ [src_ "/assets/svgs/fullscreen.svg", style_ "height:24px; width: 24px"]
  -- mainContent swaggers

  script_
    [text|

          function showModal() { 
            document.getElementById('swaggerModal').style.display = 'flex'; 
            const val = document.querySelector('#swaggerData').value
            let json = JSON.parse(val)
            const yamlData = jsyaml.dump(json,{indent:4})
            const modifiedValue = window.editor.getValue()
            monacoEditor.setTheme ('vs')
            if(!window.diffEditor) {
                window.diffEditor = monacoEditor.createDiffEditor(document.getElementById ('diff_editor_container'),{renderSideBySide: false})
              }
              
            diffEditor.setModel({
	         	   original: monaco.editor.createModel(yamlData, 'yaml'),
	         	   modified: monaco.editor.createModel(modifiedValue, 'yaml'),
	         });
          }

          function closeModal(event) {
            if(event.target.id === 'close_btn' || event.target.id ==='swaggerModal') {
               monacoEditor.setTheme ('nightOwl')
               document.getElementById('swaggerModal').style.display = 'none';
            }
          }
          
          function toggleDiffEditorInline(event) {
            if(window.diffEditor) {
                 diffEditor.updateOptions({ renderSideBySide: !event.target.checked });
              }
          }

          function toggleSwaggerHistory(event) {
            event.stopPropagation()
            const container = document.querySelector('#swagger_history_container')
            if(container) {
              if(container.style.display === 'block') {
                  container.style.display = 'none'
                }else {
                 container.style.display = 'block'
                }
              }
          }

          function fullscreen() {
             const fullscreenElement = document.getElementById('swagger-ui');
             if (fullscreenElement.requestFullscreen) {
               fullscreenElement.requestFullscreen();
             } else if (fullscreenElement.mozRequestFullScreen) { // Firefox
               fullscreenElement.mozRequestFullScreen();
             } else if (fullscreenElement.webkitRequestFullscreen) { // Chrome, Safari and Opera
               fullscreenElement.webkitRequestFullscreen();
             } else if (fullscreenElement.msRequestFullscreen) { // IE/Edge
               fullscreenElement.msRequestFullscreen();
             }
          }

          function swaggerChanged(event) {
            event.stopPropagation()
            const current = document.querySelector('#toggle_swagger_btn')
            const urlParams = new URLSearchParams (window.location.search)
            const swaggerId = urlParams.get ('swagger_id')
            if(current && current.firstChild) {
               const currentId = current.firstChild.innerText
               if (event.target.innerText === currentId) return
               current.firstChild.innerText = event.target.innerText
               const url = new URL(window.location.href);
               url.searchParams.set('swagger_id', event.target.innerText);
               document.getElementById("loading_indicator").style.display = 'flex';
               window.location.href = url.toString();
            }
          }

         window.addEventListener('DOMContentLoaded', function() {
           const urlParams = new URLSearchParams(window.location.search);
           const swaggerId = urlParams.get('swagger_id');
           const historyContainer = document.querySelector('#swagger_history_container')
            if(container) {
               Array.from(historyContainer.children).forEach(child => {
                if(child.innerText === swaggerId) {
                   child.classList.add("selected_swagger")
                  }
               })
              }
         });
         
         //close modals
         document.body.addEventListener('click', function() {
            document.querySelector('#swagger_history_container').style.display = 'none'
            //document.querySelector('#toggle_dropdown_container').style.display = 'none'
         })
        

       // function saveSwagger(event) {         
       //   const value = window.editor.getValue();
       //   const jsObject = jsyaml.load(value);
       //   const selectElement = document.getElementById('swaggerSelect');
       //   
       //   if (selectElement && jsObject) {
       //     const id = selectElement.value;
       //     const swagger = jsObject;
       //     
       //     const formData = new FormData();
       //     formData.append('swagger_id', id);
       //     formData.append('updated_swagger', JSON.stringify(swagger));
       //   }
       //   event.target.classList.remove('save_swagger_btn_active');
       // }
          

          function toggleFontSize(event) {
             const container = document.querySelector('#toggle_dropdown_container')
             if(event.target.id ==='toggle_font') {
                if(container) {
                  container.style.display = "flex"
                  }
              }else {
                const targetid = event.target.id
                const siblings = Array.from(event.target.parentNode.children);
                siblings.forEach(sibling => {
                        sibling.classList.remove('font_toggle_active');
                });
                event.target.classList.add('font_toggle_active');
                let fontSize = 14
                if(targetid === 'toggle_sm') {
                  fontSize = 12
                 }else if (targetid === 'toggle_lg') {
                   fontSize = 16
                 }
                 if(container) {
                  container.style.display = "none"
                  }
                  window.editor.updateOptions({ fontSize })
              }
          }

          const endpointsColumn = document.querySelector('#endpoints_container')
          const editorColumn = document.querySelector('#editor_container')
          const detailsColumn = document.querySelector('#details_container')
          const container = document.querySelector('#columns_container')
          const containerWidth = Number(window.getComputedStyle(container).width.replace('px',''))
  
          document.addEventListener('DOMContentLoaded', function (){
             endpointsColumn.style.width = (0.2 * containerWidth) + 'px'
             editorColumn.style.width = (0.4 * containerWidth) + 'px'
             detailsColumn.style.width = (0.4 * containerWidth) + 'px'
           })
        
          let mouseState = {x: 0}
          let resizeStart = false
          let target = ""

          function mouseDown(event) {
              resizeStart = true
              mouseState = {x: event.pageX}
              target = event.target.id
          }

          function handleMouseMove(event) {
            if(!resizeStart) return
            const diff = event.pageX - mouseState.x
            mouseState = {x: event.pageX}
            const deW = Number(detailsColumn.style.width.replace('px',''))
            const edW = Number(editorColumn.style.width.replace('px',''))
            const enpW = Number(endpointsColumn.style.width.replace('px',''))

            if(target === 'endpoints_resizer') {
                   endpointsColumn.style.width = (enpW + diff) + 'px'
                   editorColumn.style.width = (edW - diff) + 'px'
              }else if (target === "editor_resizer") {
                    editorColumn.style.width = (edW + diff) + 'px'
                    detailsColumn.style.width = (deW - diff) + 'px'
              }
          }

          function handleMouseup(event) {
            resizeStart = false 
            target = ""
          }

          window.addEventListener ('mousemove', handleMouseMove)
          window.addEventListener ('mouseup', handleMouseup)

          document.querySelector("#side_nav_toggler").addEventListener('click', ()=> {
           setTimeout(()=> {
              const endpointsColumn = document.querySelector('#endpoints_container')
              const editorColumn = document.querySelector('#editor_container')
              const detailsColumn = document.querySelector('#details_container')
              const container = document.querySelector('#columns_container')
              const containerWidth = Number(window.getComputedStyle(container).width.replace('px',''))
              endpointsColumn.style.width = (0.2 * containerWidth) + 'px'
              editorColumn.style.width = (0.4 * containerWidth) + 'px'
              detailsColumn.style.width = (0.4 * containerWidth) + 'px'
           })
          })
         |]

  script_ [src_ "/assets/js/monaco/vs/loader.js", defer_ "true"] ("" :: Text)
  script_
    [text|
      document.addEventListener('DOMContentLoaded', function(){
        require.config({ paths: { vs: '/assets/js/monaco/vs' } });
        require.config({ paths: { 'vs': 'https://unpkg.com/monaco-editor/min/vs' } });
		  	require(['vs/editor/editor.main'], function () {
        monaco.editor.defineTheme('nightOwl', {
           base: 'vs-dark',
           inherit: true,
           rules: [
             { token: 'comment', foreground: '#6A9955' },
             { token: 'keyword', foreground: '#C586C0' },
             { token: 'number', foreground: '#B5CEA8' },
             { token: 'string', foreground: '#CE9178' },
             { token: 'operator', foreground: '#D4D4D4' },
             { token: 'identifier', foreground: '#D4D4D4' },
             { token: 'type', foreground: '#4EC9B0' },
             { token: 'delimiter', foreground: '#D4D4D4' },
             { token: 'punctuation', foreground: '#D4D4D4' },
             { token: 'namespace', foreground: '#9CDCFE' },
             { token: 'function', foreground: '#DCDCAA' },
             { token: 'class', foreground: '#4EC9B0' },
             { token: 'variable', foreground: '#D4D4D4' }
           ],
           colors: {
             'editor.foreground': '#D4D4D4',
             'editor.background': '#011627',
             'editor.selectionBackground': '#2D3643',
             'editor.lineHighlightBackground': '#202B33',
             'editorCursor.foreground': '#D4D4D4',
             'editorWhitespace.foreground': '#404040'
           }
        });
       window.monacoEditor = monaco.editor
       const val = document.querySelector('#swaggerData').value
       let json = JSON.parse(val)
       const yamlData = jsyaml.dump(json,{indent:4})
		   window.editor = monaco.editor.create(document.getElementById('swaggerEditor'), {
            value: yamlData,
		  			language:'yaml',
            minimap:{enabled:true},
            automaticLayout : true,
            fontSize: 14,
            lineHeight: 20,
            lineNumbersMinChars: 3,
            theme: 'nightOwl'
		  		});
		   });
      })
   |]

  script_ [src_ "https://unpkg.com/swagger-ui-dist@4.5.0/swagger-ui-bundle.js", crossorigin_ "true"] ("" :: Text)
  script_ [src_ "/assets/js/swagger_endpoints.js"] ("" :: Text)
  script_ [src_ "/assets/js/build_keypaths.js"] ("" :: Text)
  script_ [src_ "https://unpkg.com/js-yaml/dist/js-yaml.min.js", crossorigin_ "true"] ("" :: Text)
  script_
    [text|
      window.onload = () => {
           const val = document.querySelector('#swaggerData').value
           let json = JSON.parse(val)
           document.getElementById ("loading_indicator").style.display = 'none'
            window.endpointsUI = new SwaggerEndPointsUI(json);
            window.endpointsUI.initialize ()
            window.ui = SwaggerUIBundle({
              spec: json,
              dom_id: '#swagger-ui',
            });    

        window.editor.onDidChangeModelContent(function(event) {
          try{
              const saveBtn = document.getElementById("save_swagger_btn")
             if(saveBtn) {
               saveBtn.classList.add("save_swagger_btn_active") 
               }
             const value = window.editor.getValue();
             const jsObject = jsyaml.load(value);
             if(jsObject) {
                const inp = document.querySelector("#save_swagger_input_data")
                const data = JSON.stringify(jsObject)
                if(inp) {
                  inp.value = JSON.stringify(jsObject)
                 }
                 window.ui.specActions.updateSpec(data)
                 endpointsUI.updateData(jsObject)
               }

          }catch(e) {
            console.log(e)
          }
        });   
      };
    |]

mainContent :: V.Vector Swaggers.Swagger -> Html ()
mainContent swaggers = do
  section_ [id_ "main-content", class_ "flex flex-col"] $ do
    div_ [class_ "-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8"] $ do
      div_ [class_ "flex flex-col py-2 align-middle inline-block w-full sm:px-6 lg:px-8"] $ do
        swaggers & mapM_ \rf -> do
          div_ [style_ "max-height:400px;", class_ "shadow overflow-y-auto border-b border-gray-200 mb-10 sm:rounded-lg"] $ do
            div_ $ do
              p_ [style_ "white-space: pre-wrap; font-family: monospace;", class_ "raw_swagger px-6 py-4 text-sm font-medium text-gray-900"] $ toHtml $ encode $ rf.swaggerJson
  script_
    [text|
         for(let swagger of Array.from(document.querySelectorAll('.raw_swagger'))) {
             try{
              const parsedSwagger = JSON.parse(swagger.textContent);
              const prettierSwagger = JSON.stringify(parsedSwagger, null, 2);
              swagger.textContent = prettierSwagger;
             }catch(e){
              console.log(e)
             }
         }
        |]
