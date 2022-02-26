module Pages.Api (apiGetH, apiPostH, GenerateAPIKeyForm (..)) where

import Config
import Data.Aeson (encode)
import Data.Aeson.QQ (aesonQQ)
import Data.ByteString.Base64 qualified as B64
import Data.Text as T
import Data.UUID as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.HTMX
import Lucid.Hyperscript
import Models.Projects.ProjectApiKeys qualified as ProjectApiKey
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Optics.Core ((^.))
import Pages.BodyWrapper (bodyWrapper)
import Relude
import Servant (addHeader)
import Web.FormUrlEncoded (FromForm)

newtype GenerateAPIKeyForm = GenerateAPIKeyForm
  { title :: Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromForm)

apiPostH :: Sessions.PersistentSession -> Projects.ProjectId -> GenerateAPIKeyForm -> DashboardM (HeadersTrigger (Html ()))
apiPostH sess pid apiKeyForm = do
  pool <- asks pool
  env <- asks env
  projectKeyUUID <- liftIO UUIDV4.nextRandom
  let encryptedKey = ProjectApiKeys.encryptAPIKey (encodeUtf8 $ env ^. #apiKeyEncryptionSecretKey) (encodeUtf8 $ UUID.toText projectKeyUUID)
  let encryptedKeyB64 = B64.encodeBase64 encryptedKey
  let keyPrefix = T.take 8 encryptedKeyB64

  pApiKey <- liftIO $ ProjectApiKeys.newProjectApiKeys pid projectKeyUUID (title apiKeyForm) keyPrefix
  apiKeys <- liftIO $
    withPool pool $ do
      ProjectApiKeys.insertProjectApiKey pApiKey
      ProjectApiKeys.projectApiKeysByProjectId pid
  let hxTriggerData = encode [aesonQQ| {"closeModal": ""}|]
  pure $ addHeader (decodeUtf8 hxTriggerData) $ mainContent apiKeys (Just (pApiKey, encryptedKeyB64))

-- | apiGetH renders the api keys list page which includes a modal for creating the apikeys.
apiGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
apiGetH sess pid = do
  pool <- asks pool
  (project, apiKeys) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      apiKeys <- ProjectApiKeys.projectApiKeysByProjectId pid
      pure (project, apiKeys)
  pure $ bodyWrapper (Just sess) project "API Keys" $ apiKeysPage pid apiKeys

apiKeysPage :: Projects.ProjectId -> Vector ProjectApiKeys.ProjectApiKey -> Html ()
apiKeysPage pid apiKeys = do
  section_ [class_ "container mx-auto  px-4 py-10"] $ do
    div_ [class_ "flex justify-between mb-6"] $ do
      h2_ [class_ "text-slate-700 text-2xl font-medium"] "API Keys"
      button_ [class_ "btn-indigo", [__|on click remove .hidden from #generateApiKeyDialog |]] "Create an API Key"
    mainContent apiKeys Nothing
    div_
      [ class_ "hidden fixed z-10 inset-0 overflow-y-auto",
        role_ "dialog",
        id_ "generateApiKeyDialog"
      ]
      $ do
        form_
          [ hxPost_ $ "/p/" <> Projects.projectIdText pid <> "/apis",
            class_ "flex items-end justify-center min-h-screen pt-4 px-4 pb-20 text-center sm:block sm:p-0",
            hxTarget_ "#main-content",
            [__|on closeModal from body add .hidden to #generateApiKeyDialog then call me.reset()|]
          ]
          $ do
            div_ [class_ "fixed inset-0 bg-gray-500 bg-opacity-75 transition-opacity"] $ do
              span_ [class_ "hidden sm:inline-block sm:align-middle sm:h-screen"] $ toHtml "&#8203;"
            div_ [class_ "inline-block align-bottom bg-white rounded-lg px-4 pt-5 pb-4 text-left overflow-hidden shadow-xl transform transition-all sm:my-8 sm:align-middle sm:max-w-lg sm:w-full sm:p-6"] $ do
              div_ [class_ "hidden sm:block absolute top-0 right-0 pt-4 pr-4"] $ do
                button_
                  [ type_ "button",
                    class_ "bg-white rounded-md text-gray-400 hover:text-gray-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500",
                    [__|on click add .hidden to #generateApiKeyDialog|]
                  ]
                  $ do
                    span_ [class_ "sr-only"] $ toHtml "Close"
                    img_ [class_ "h-6 w-6", src_ "/assets/svgs/close.svg"]
              div_ [class_ "sm:flex sm:items-start"] $ do
                div_ [class_ "mx-auto flex-shrink-0 flex items-center justify-center h-12 w-12 rounded-full bg-red-100 sm:mx-0 sm:h-10 sm:w-10"] $ do
                  img_ [class_ "h-6 w-6 text-red-600", src_ "/assets/svgs/close.svg"]
                div_ [class_ "mt-3 text-center sm:mt-0 sm:ml-4 sm:text-left grow"] $ do
                  h3_ [class_ "text-lg leading-6 font-medium text-gray-900", id_ "modal-title"] $ toHtml "Generate an API Key"
                  div_ [class_ "mt-6 space-y-2"] $ do
                    p_ [class_ "text-sm text-gray-500"] $ toHtml "Please input a title for your API Key."
                    div_ [class_ ""] $ do
                      input_ [class_ "input-txt px-4 py-2  border w-full", type_ "text", placeholder_ "API Key Title", name_ "title"]
              div_ [class_ "mt-5 sm:mt-4 sm:flex sm:flex-row-reverse"] $ do
                button_ [type_ "submit", class_ "w-full inline-flex justify-center rounded-md border border-transparent shadow-sm px-4 py-2 bg-red-600 text-base font-medium text-white hover:bg-red-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-red-500 sm:ml-3 sm:w-auto sm:text-sm"] $ toHtml "Submit"
                button_
                  [ type_ "button",
                    class_ "mt-3 w-full inline-flex justify-center rounded-md border border-gray-300 shadow-sm px-4 py-2 bg-white text-base font-medium text-gray-700 hover:text-gray-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 sm:mt-0 sm:w-auto sm:text-sm",
                    [__|on click add .hidden to #generateApiKeyDialog|]
                  ]
                  $ toHtml "Cancel"

mainContent :: Vector ProjectApiKeys.ProjectApiKey -> Maybe (ProjectApiKey.ProjectApiKey, Text) -> Html ()
mainContent apiKeys newKeyM = do
  section_ [id_ "main-content"] $ do
    case newKeyM of
      Nothing -> toHtml ""
      Just (keyObj, newKey) -> do
        div_ [id_ "apiFeedbackSection", class_ "pb-8"] $ do
          div_ [class_ "rounded-md bg-green-50 p-4"] $ do
            div_ [class_ "flex"] $ do
              div_ [class_ "flex-shrink-0"] $ do
                img_ [class_ "h-5 w-5 text-green-400", src_ "/assets/svgs/check_circle.svg"]
              div_ [class_ "ml-3"] $ do
                h3_ [class_ "text-sm font-medium text-green-800"] $ toHtml "API Key was generated successfully"
                div_ [class_ "mt-2 text-sm text-green-700"] $ do
                  p_ $ toHtml "Please copy the generated APIKey as you would not be able to view it anymore after this message."
                  strong_ [class_ "block pt-2", id_ "newKey"] $ toHtml newKey
                div_ [class_ "mt-4"] $ do
                  div_ [class_ "-mx-2 -my-1.5 flex"] $ do
                    button_
                      [ type_ "button",
                        class_ "bg-green-50 px-2 py-1.5 rounded-md text-sm font-medium text-green-800 hover:bg-green-100 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-green-50 focus:ring-green-600",
                        [__| 
                      on click 
                        js
                            if ('clipboard' in window.navigator) {
                                navigator.clipboard.writeText(document.getElementById("newKey").innerText).then(Text=>{
                                  alert ("Copied to Clipboard")
                                })
                            }
                        end
                        |]
                      ]
                      $ toHtml "Copy Key"
                    button_
                      [ type_ "button",
                        class_ "ml-3 bg-green-50 px-2 py-1.5 rounded-md text-sm font-medium text-green-800 hover:bg-green-100 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-green-50 focus:ring-green-600",
                        [__|on click remove #apiFeedbackSection|]
                      ]
                      $ toHtml "Dismiss"
    div_ [class_ "flex flex-col"] $ do
      div_ [class_ "-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8"] $ do
        div_ [class_ "py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8"] $ do
          div_ [class_ "shadow overflow-hidden border-b border-gray-200 sm:rounded-lg"] $ do
            table_ [class_ "min-w-full divide-y divide-gray-200"] $ do
              thead_ [class_ "bg-gray-50"] $ do
                tr_ $ do
                  th_ [class_ "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"] $ toHtml "Title"
                  th_ [class_ "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"] $ toHtml "Key"
                  th_ [class_ "relative px-6 py-3"] $ do
                    span_ [class_ "sr-only"] $ toHtml "Edit"
              tbody_ [class_ "bg-white divide-y divide-gray-200"] $ do
                apiKeys & mapM_ \apiKey -> do
                  tr_ $ do
                    td_ [class_ "px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900"] $ toHtml $ apiKey ^. #title
                    td_ [class_ "px-6 py-4 whitespace-nowrap text-sm text-gray-500"] $ toHtml $ apiKey ^. #keyPrefix <> "**********"
                    td_ [class_ "px-6 py-4 whitespace-nowrap text-right text-sm font-medium"] $ do
                      a_ [class_ "text-indigo-600 hover:text-indigo-900"] $ do
                        img_ [src_ "/assets/svgs/revoke.svg", class_ "h-3 w-3 mr-2 inline-block"]
                        span_ [class_ "text-slate-500"] "Revoke"
