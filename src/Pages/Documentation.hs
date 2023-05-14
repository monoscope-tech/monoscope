module Pages.Documentation (documentationGetH, documentationPostH, SwaggerForm) where

import Config
import Data.Aeson (decodeStrict, encode)
import Data.Aeson.QQ (aesonQQ)
import Data.Default (def)
import Data.Text as T
import Data.Time.LocalTime (getZonedTime)
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.Htmx
import Models.Projects.Projects qualified as Projects
import Models.Projects.Swaggers qualified as Swaggers
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude
import Servant (Headers, addHeader)
import Servant.Htmx (HXTrigger)
import Web.FormUrlEncoded (FromForm)

data SwaggerForm = SwaggerForm
  { swagger_json :: Text,
    from :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)

documentationPostH :: Sessions.PersistentSession -> Projects.ProjectId -> SwaggerForm -> DashboardM (Headers '[HXTrigger] (Html ()))
documentationPostH sess pid SwaggerForm {swagger_json, from} = do
  pool <- asks pool
  env <- asks env
  swaggerId <- Swaggers.SwaggerId <$> liftIO UUIDV4.nextRandom
  currentTime <- liftIO getZonedTime
  let value = case decodeStrict (encodeUtf8 swagger_json) of
        Just val -> val
        Nothing -> error "Failed to parse JSON"
  let swaggerToAdd =
        Swaggers.Swagger
          { id = swaggerId,
            projectId = pid,
            createdBy = sess.userId,
            createdAt = currentTime,
            updatedAt = currentTime,
            swaggerJson = value
          }

  swaggers <- liftIO $
    withPool pool $ do
      Swaggers.addSwagger swaggerToAdd
      Swaggers.swaggersByProject pid

  let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"closeModal": "", "successToast": ["Swagger uploaded Successfully"]}|]
  let v = if from == "docs" then documentationsPage pid swaggers else ""
  pure $ addHeader hxTriggerData v

documentationGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
documentationGetH sess pid = do
  pool <- asks pool
  (project, swaggers) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      swaggers <- Swaggers.swaggersByProject pid
      pure (project, swaggers)

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess,
            currProject = project,
            pageTitle = "Documentation"
          }
  pure $ bodyWrapper bwconf $ documentationsPage pid swaggers

documentationsPage :: Projects.ProjectId -> Vector Swaggers.Swagger -> Html ()
documentationsPage pid swaggers = do
  div_ [class_ "container mx-auto relative  px-4 pt-10 pb-24 h-full", id_ "main-content"] $ do
    -- modal
    div_
      [ style_ "z-index:99999",
        class_ "fixed hidden pt-24 justify-center z-50 w-full p-4 bg-gray-500 bg-opacity-75 overflow-y-auto inset-0 h-full max-h-full",
        id_ "swaggerModal",
        tabindex_ "-1",
        onclick_ "closeModal(event)"
      ]
      $ do
        div_
          [ class_ "relative w-[500px] max-h-full",
            style_ "width: min(90vw, 750px)"
          ]
          $ do
            -- Modal content
            form_
              [ class_ "relative bg-white rounded-lg shadow",
                hxPost_ $ "/p/" <> pid.toText <> "/documentation",
                hxTarget_ "#main-content"
              ]
              $ do
                div_ [class_ "flex items-start justify-between p-4 border-b rounded-t"] $ do
                  h3_ [class_ "text-xl font-semibold text-gray-900 dark:text-white"] "Upload Swagger"
                -- Modal body
                div_ [class_ "p-6 space-y-6"] $ do
                  input_ [type_ "hidden", name_ "from", value_ "docs"]
                  textarea_ [style_ "height:65vh;resize:none", name_ "swagger_json", class_ "w-full border outline-none p-4 focus:outline-none focus:border-blue-200", placeholder_ "Paste swagger here"] ""
                -- Modal footer
                div_ [class_ "flex w-full justify-end items-center p-6 space-x-2 border-t border-gray-200 rounded-b"] $ do
                  button_ [style_ "margin-right:50px", type_ "button", class_ "btn", onclick_ "closeModal(event)", id_ "close_btn"] "Close"
                  button_ [type_ "sumbit", class_ "btn btn-primary"] "Upload"

    -- page content
    div_ [class_ "flex flex-col justify-between"] $ do
      div_ [class_ "flex w-full justify-between my-3 px-2"] $ do
        h3_ [class_ "text-xl text-slate-700 text-2xl font-medium"] "Swagger History"
        button_ [class_ "place-content-center text-md btn btn-primary", onclick_ "showModal()"] "Upload swagger"
      -- search
      div_ [class_ "card-round p-5"] $ do
        div_ [class_ "w-full flex flex-row m-3"] $ do
          mainContent pid swaggers
  script_
    [text|
          function showModal() { document.getElementById('swaggerModal').style.display = 'flex'; }
          function closeModal(event) {
            if(event.target.id === 'close_btn' || event.target.id ==='swaggerModal') {
               document.getElementById('swaggerModal').style.display = 'none';
              }
             }
         |]

mainContent :: Projects.ProjectId -> Vector Swaggers.Swagger -> Html ()
mainContent pid swaggers = do
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