module Pages.Api (apiGetH, apiPostH, apiDeleteH, GenerateAPIKeyForm (..), ApiGet (..), ApiMut (..)) where

import Data.Base64.Types qualified as B64
import Data.ByteString.Base64 qualified as B64
import Data.Default (def)
import Data.Map qualified as Map
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
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast, addTriggerEvent)
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
  addTriggerEvent "closeModal" ""
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
      label_ [class_ "btn-indigo p-2 rounded-lg", Lucid.for_ "apikey-modal"] "Create an API Key"
    mainContent pid apiKeys Nothing

    input_ [type_ "checkbox", id_ "apikey-modal", class_ "modal-toggle"]
    div_ [class_ "modal p-8", role_ "dialog", id_ "apikey-modal"] do
      div_ [class_ "modal-box flex flex-col gap-4"] $ do
        div_ [class_ "p-3 bg-[#0acc91]/5 rounded-full w-max border-[#067a57]/20 gap-2 inline-flex"]
          $ faSprite_ "key" "regular" "h-6 w-6 text-green-500"
        span_ [class_ " text-textStrong text-2xl font-semibold"] "Generate an API key"
        form_
          [ hxPost_ $ "/p/" <> pid.toText <> "/apis"
          , class_ "flex flex-col gap-4"
          , hxTarget_ "#main-content"
          , [__|on closeModal from body set #apikey-modal.checked to false |]
          ]
          do
            div_ [class_ "flex gap-1 flex-col"] do
              p_ [class_ "text-textWeak"] "Please input a title for your API key."
              div_ $ input_ [class_ "input-txt px-4 py-2  border w-full", type_ "text", placeholder_ "API Key Title", name_ "title", autofocus_]
            div_ [class_ "flex justify-end"] do
              button_
                [ type_ "submit"
                , class_ "rounded px-4 py-2 bg-green-500 text-white"
                ]
                "Submit"
      label_ [class_ "modal-backdrop", Lucid.for_ "apikey-modal"] "Close"


mainContent :: Projects.ProjectId -> V.Vector ProjectApiKeys.ProjectApiKey -> Maybe (ProjectApiKeys.ProjectApiKey, Text) -> Html ()
mainContent pid apiKeys newKeyM = section_ [id_ "main-content"] do
  copyNewApiKey newKeyM False
  div_ [class_ "flex flex-col"] do
    div_ [class_ "-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8"] do
      div_ [class_ "py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8"] do
        div_ [class_ "shadow-sm overflow-hidden border-b border-gray-200 sm:rounded-lg"] do
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
                      [ class_ "text-brand"
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
                      [ class_ "text-brand ml-2"
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
            div_ [class_ "shrink-0"] do
              faSprite_ "circle-check" "regular" "h-5 w-5 text-green-400"
            div_ [class_ "ml-3"] do
              h3_ [class_ " font-medium text-green-800"] "API Key was generated successfully"
              div_ [class_ "mt-2  text-green-700 py-2"] do
                strong_ [class_ "block pt-2", id_ "newKey"] $ toHtml newKey
              div_ [class_ "mt-4"] do
                div_ [class_ "-mx-2 -my-1.5 flex"] do
                  button_
                    [ type_ "button"
                    , class_ "bg-green-500 px-2 py-1.5 text-white rounded-md  font-medium text-green-800 hover:bg-green-300 focus:outline-hidden focus:ring-2 focus:ring-offset-2 focus:ring-offset-green-50 focus:ring-green-600"
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
                        , class_ "ml-3 bg-green-50 px-2 py-1.5 rounded-md  font-medium text-green-800 hover:bg-green-100 focus:outline-hidden focus:ring-2 focus:ring-offset-2 focus:ring-offset-green-50 focus:ring-green-600"
                        , [__|on click remove #apiFeedbackSection|]
                        ]
                        "Dismiss"
                    else do
                      button_
                        [ type_ "button"
                        , class_ "ml-6 font-medium px-2 py-1.5 rounded-md font-medium text-brand"
                        , [__|on click call window.location.reload()|]
                        ]
                        "Next"
