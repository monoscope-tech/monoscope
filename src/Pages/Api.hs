module Pages.Api (apiGetH, apiPostH, apiDeleteH, GenerateAPIKeyForm (..), ApiGet (..), ApiMut (..)) where

import Data.Base64.Types qualified as B64
import Data.ByteString.Base64 qualified as B64
import Data.Default (def)
import Data.Text qualified as T
import Data.UUID as UUID (toText)
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Htmx (hxConfirm_, hxDelete_, hxPost_, hxTarget_)
import Lucid.Hyperscript (__)
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Relude hiding (ask)
import System.Config (AuthContext (config), EnvConfig (apiKeyEncryptionSecretKey))
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast)
import Utils (faSprite_)
import Web.FormUrlEncoded (FromForm)


data GenerateAPIKeyForm = GenerateAPIKeyForm
  { title :: Text
  , from :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)


apiPostH :: Projects.ProjectId -> GenerateAPIKeyForm -> ATAuthCtx (RespHeaders ApiMut)
apiPostH pid apiKeyForm = do
  (sess, project) <- Sessions.sessionAndProject pid
  authCtx <- ask @AuthContext
  projectKeyUUID <- liftIO UUIDV4.nextRandom
  let encryptedKey = ProjectApiKeys.encryptAPIKey (encodeUtf8 authCtx.config.apiKeyEncryptionSecretKey) (encodeUtf8 $ UUID.toText projectKeyUUID)
  let encryptedKeyB64 = B64.extractBase64 $ B64.encodeBase64 encryptedKey
  pApiKey <- ProjectApiKeys.newProjectApiKeys pid projectKeyUUID (title apiKeyForm) encryptedKeyB64
  apiKeys <- dbtToEff do
    ProjectApiKeys.insertProjectApiKey pApiKey
    ProjectApiKeys.projectApiKeysByProjectId pid
  addSuccessToast "Created API Key Successfully" Nothing
  case from apiKeyForm of
    Just v -> addRespHeaders $ ApiPostCopy (Just (pApiKey, encryptedKeyB64)) True
    Nothing -> addRespHeaders $ ApiPost pid apiKeys (Just (pApiKey, encryptedKeyB64))


apiDeleteH :: Projects.ProjectId -> ProjectApiKeys.ProjectApiKeyId -> ATAuthCtx (RespHeaders ApiMut)
apiDeleteH pid keyid = do
  (sess, project) <- Sessions.sessionAndProject pid
  res <- dbtToEff $ ProjectApiKeys.revokeApiKey keyid
  apikeys <- dbtToEff $ ProjectApiKeys.projectApiKeysByProjectId pid
  if res > 0
    then addSuccessToast "Revoked API Key Successfully" Nothing
    else addErrorToast "Something went wrong" Nothing
  addRespHeaders $ ApiPost pid apikeys Nothing


data ApiMut
  = ApiPost Projects.ProjectId (V.Vector ProjectApiKeys.ProjectApiKey) (Maybe (ProjectApiKeys.ProjectApiKey, Text))
  | ApiPostCopy (Maybe (ProjectApiKeys.ProjectApiKey, Text)) Bool


instance ToHtml ApiMut where
  toHtml (ApiPost pid apiKeys m) = toHtml $ mainContent pid apiKeys m
  toHtml (ApiPostCopy m b) = toHtml $ copyNewApiKey m b
  toHtmlRaw = toHtml


-- | apiGetH renders the api keys list page which includes a modal for creating the apikeys.
apiGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders ApiGet)
apiGetH pid = do
  (sess, project) <- Sessions.sessionAndProject pid
  apiKeys <- dbtToEff $ ProjectApiKeys.projectApiKeysByProjectId pid
  requestDumps <- dbtToEff $ RequestDumps.countRequestDumpByProject pid
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "API Keys"
          , hasIntegrated = Just (requestDumps > 0)
          }
  addRespHeaders $ ApiGet $ PageCtx bwconf (pid, apiKeys)


newtype ApiGet = ApiGet (PageCtx (Projects.ProjectId, V.Vector ProjectApiKeys.ProjectApiKey))


instance ToHtml ApiGet where
  toHtml (ApiGet (PageCtx bwconf (pid, apiKeys))) = toHtml $ PageCtx bwconf $ apiKeysPage pid apiKeys
  toHtmlRaw = toHtml


apiKeysPage :: Projects.ProjectId -> V.Vector ProjectApiKeys.ProjectApiKey -> Html ()
apiKeysPage pid apiKeys = do
  section_ [class_ "w-full mx-auto  px-16 py-10 overflow-hidden overflow-y-scroll"] do
    div_ [class_ "flex justify-between mb-6"] do
      h2_ [class_ "text-slate-700 text-2xl font-medium"] "API Keys"
      button_ [class_ "btn-indigo p-2 rounded-lg", [__|on click remove .hidden from #generateApiKeyDialog |]] "Create an API Key"
    mainContent pid apiKeys Nothing
    div_
      [ class_ "hidden fixed z-30 inset-0 overflow-y-auto"
      , role_ "dialog"
      , id_ "generateApiKeyDialog"
      ]
      do
        form_
          [ hxPost_ $ "/p/" <> pid.toText <> "/apis"
          , class_ "flex items-end justify-center min-h-screen pt-4 px-4 pb-20 text-center sm:block sm:p-0"
          , hxTarget_ "#main-content"
          , [__|on closeModal from body add .hidden to #generateApiKeyDialog then call me.reset()|]
          ]
          do
            div_ [class_ "fixed inset-0 bg-gray-500 bg-opacity-75 transition-opacity"] do
              span_ [class_ "hidden sm:inline-block sm:align-middle sm:h-screen"] ""
            div_ [class_ "inline-block align-bottom bg-base-100 rounded-lg px-4 pt-5 pb-4 text-left overflow-hidden shadow-xl transform transition-all sm:my-8 sm:align-middle sm:max-w-lg sm:w-full sm:p-6"] do
              div_ [class_ "hidden sm:block absolute top-0 right-0 pt-4 pr-4"] do
                button_
                  [ type_ "button"
                  , class_ "bg-base-100 rounded-md text-gray-400 hover:text-gray-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                  , [__|on click add .hidden to #generateApiKeyDialog|]
                  ]
                  do
                    span_ [class_ "sr-only"] "Close"
                    faSprite_ "xmark" "regular" "h-6 w-6"
              div_ [class_ "sm:flex sm:items-start"] do
                div_ [class_ "mx-auto flex-shrink-0 flex items-center justify-center h-12 w-12 rounded-full bg-red-100 sm:mx-0 sm:h-10 sm:w-10"] do
                  faSprite_ "xmark" "regular" "h-6 w-6"
                div_ [class_ "mt-3 text-center sm:mt-0 sm:ml-4 sm:text-left grow"] do
                  h3_ [class_ "text-lg leading-6 font-medium text-gray-900", id_ "modal-title"] "Generate an API Key"
                  div_ [class_ "mt-6 space-y-2"] do
                    p_ [class_ " text-gray-500"] "Please input a title for your API Key."
                    div_ $ input_ [class_ "input-txt px-4 py-2  border w-full", type_ "text", placeholder_ "API Key Title", name_ "title", autofocus_]
              div_ [class_ "mt-5 sm:mt-4 sm:flex sm:flex-row-reverse"] do
                button_ [type_ "submit", class_ "w-full inline-flex justify-center rounded-md border border-transparent shadow-sm px-4 py-2 bg-red-600 text-base font-medium text-white hover:bg-red-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-red-500 sm:ml-3 sm:w-auto sm:"] "Submit"
                button_
                  [ type_ "button"
                  , class_ "mt-3 w-full inline-flex justify-center rounded-md border border-gray-300 shadow-sm px-4 py-2 bg-base-100 text-base font-medium text-gray-700 hover:text-gray-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 sm:mt-0 sm:w-auto sm:"
                  , [__|on click add .hidden to #generateApiKeyDialog|]
                  ]
                  "Cancel"


mainContent :: Projects.ProjectId -> V.Vector ProjectApiKeys.ProjectApiKey -> Maybe (ProjectApiKeys.ProjectApiKey, Text) -> Html ()
mainContent pid apiKeys newKeyM = section_ [id_ "main-content"] do
  copyNewApiKey newKeyM False
  div_ [class_ "flex flex-col"] do
    div_ [class_ "-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8"] do
      div_ [class_ "py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8"] do
        div_ [class_ "shadow overflow-hidden border-b border-gray-200 sm:rounded-lg"] do
          table_ [class_ "min-w-full divide-y divide-gray-200"] do
            thead_ [class_ "bg-gray-50"] do
              tr_ do
                th_ [class_ "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"] "Title"
                th_ [class_ "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"] "Key"
                th_ [class_ "relative px-6 py-3"] do
                  span_ [class_ "sr-only"] "Edit"
            tbody_ [class_ "bg-base-100 divide-y divide-gray-200"] do
              V.indexed apiKeys & mapM_ \(i, apiKey) -> do
                tr_ [] do
                  td_ [class_ "px-6 py-4 whitespace-nowrap  font-medium text-gray-900"] $ toHtml apiKey.title
                  td_ [class_ "px-6 py-4 whitespace-nowrap  text-gray-500 w-[500px]"] do
                    span_
                      [class_ "mr-2 w-full"]
                      $ toHtml
                      $ T.take 8 apiKey.keyPrefix
                      <> "********************************************"
                    button_
                      [ class_ "text-blue-500"
                      , term "data-key" apiKey.keyPrefix
                      , term "data-prefix" (T.take 8 apiKey.keyPrefix <> "********************************************")
                      , [__| on click  if my innerText is "show" 
                                 put  @data-key into previous <span/>
                                 put "hide" into me
                                 exit
                              end
                             if my innerText is "hide"
                                 put  @data-prefix into previous <span/>
                              put "show" into me
                             end
                            |]
                      ]
                      "show"
                    button_
                      [ class_ "text-blue-500 ml-2"
                      , term "data-key" apiKey.keyPrefix
                      , [__| on click if 'clipboard' in window.navigator then
                          call navigator.clipboard.writeText(my @data-key)
                          send successToast(value:['API Key has been copied to the Clipboard']) to <body/>
                        end
                        |]
                      ]
                      "copy"
                  td_ [class_ "px-6 py-4 whitespace-nowrap text-right  font-medium"] $ do
                    if apiKey.active
                      then do
                        button_
                          [ class_ "text-indigo-600 hover:text-indigo-900"
                          , hxDelete_ $ "/p/" <> pid.toText <> "/apis/" <> apiKey.id.toText
                          , hxConfirm_ $ "Are you sure you want to revoke " <> apiKey.title <> " API Key?"
                          , hxTarget_ "#main-content"
                          , id_ $ "key" <> show i
                          ]
                          do
                            faSprite_ "xmark" "regular" "h-3 w-3 mr-2 inline-block text-red-600"
                            span_ [class_ "text-slate-500"] "Revoke"
                      else do
                        button_
                          [class_ "text-indigo-600 hover:text-indigo-900"]
                          do
                            span_ [class_ "text-slate-500"] "Revoked"


copyNewApiKey :: Maybe (ProjectApiKeys.ProjectApiKey, Text) -> Bool -> Html ()
copyNewApiKey newKeyM hasNext =
  case newKeyM of
    Nothing -> ""
    Just (keyObj, newKey) -> do
      div_ [id_ "apiFeedbackSection", class_ "pb-8"] do
        div_ [class_ "rounded-md bg-green-50 p-4"] do
          div_ [class_ "flex"] do
            div_ [class_ "flex-shrink-0"] do
              faSprite_ "circle-check" "regular" "h-5 w-5 text-green-400"
            div_ [class_ "ml-3"] do
              h3_ [class_ " font-medium text-green-800"] "API Key was generated successfully"
              div_ [class_ "mt-2  text-green-700 py-2"] do
                strong_ [class_ "block pt-2", id_ "newKey"] $ toHtml newKey
              div_ [class_ "mt-4"] do
                div_ [class_ "-mx-2 -my-1.5 flex"] do
                  button_
                    [ type_ "button"
                    , class_ "bg-green-500 px-2 py-1.5 text-white rounded-md  font-medium text-green-800 hover:bg-green-300 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-green-50 focus:ring-green-600"
                    , [__|
                      on click
                        if 'clipboard' in window.navigator then
                          call navigator.clipboard.writeText(#newKey's innerText)
                          send successToast(value:['API Key has been added to the Clipboard']) to <body/>
                        end
                        |]
                    ]
                    "Copy Key"
                  if not hasNext
                    then do
                      button_
                        [ type_ "button"
                        , class_ "ml-3 bg-green-50 px-2 py-1.5 rounded-md  font-medium text-green-800 hover:bg-green-100 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-green-50 focus:ring-green-600"
                        , [__|on click remove #apiFeedbackSection|]
                        ]
                        "Dismiss"
                    else do
                      button_
                        [ type_ "button"
                        , class_ "ml-6 font-medium px-2 py-1.5 rounded-md font-medium text-blue-500"
                        , [__|on click call window.location.reload()|]
                        ]
                        "Next"
