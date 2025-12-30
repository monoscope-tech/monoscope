{-# LANGUAGE PackageImports #-}

module Pages.Api (apiGetH, apiPostH, apiActivateH, apiDeleteH, GenerateAPIKeyForm (..), ApiGet (..), ApiMut (..)) where

import Data.Base64.Types qualified as B64
import Data.Default (def)
import Data.Text qualified as T
import Data.UUID as UUID (toText)
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Htmx (hxConfirm_, hxDelete_, hxPatch_, hxPost_, hxTarget_)
import Lucid.Hyperscript (__)
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pkg.Components.Table qualified as Table
import Relude hiding (ask)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast, addTriggerEvent)
import Utils (faSprite_)
import Web.FormUrlEncoded (FromForm)
import "base64" Data.ByteString.Base64.URL qualified as B64


data GenerateAPIKeyForm = GenerateAPIKeyForm
  { title :: Text
  , from :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


apiPostH :: Projects.ProjectId -> GenerateAPIKeyForm -> ATAuthCtx (RespHeaders ApiMut)
apiPostH pid apiKeyForm = do
  (sess, project) <- Sessions.sessionAndProject pid
  authCtx <- ask @AuthContext
  projectKeyUUID <- liftIO UUIDV4.nextRandom
  let encryptedKey = ProjectApiKeys.encryptAPIKey (encodeUtf8 authCtx.config.apiKeyEncryptionSecretKey) (encodeUtf8 $ UUID.toText projectKeyUUID)
  let encryptedKeyB64 = B64.extractBase64 $ B64.encodeBase64 encryptedKey
  pApiKey <- ProjectApiKeys.newProjectApiKeys pid projectKeyUUID (title apiKeyForm) encryptedKeyB64
  apiKeys <- do
    ProjectApiKeys.insertProjectApiKey pApiKey
    V.fromList <$> ProjectApiKeys.projectApiKeysByProjectId pid
  addSuccessToast "Created API Key Successfully" Nothing
  addTriggerEvent "closeModal" ""
  case from apiKeyForm of
    Just v -> addRespHeaders $ ApiPostCopy (Just (pApiKey, encryptedKeyB64)) True
    Nothing -> addRespHeaders $ ApiPost pid apiKeys (Just (pApiKey, encryptedKeyB64))


apiDeleteH :: Projects.ProjectId -> ProjectApiKeys.ProjectApiKeyId -> ATAuthCtx (RespHeaders ApiMut)
apiDeleteH pid keyid = do
  (sess, project) <- Sessions.sessionAndProject pid
  res <- ProjectApiKeys.revokeApiKey keyid
  apikeys <- V.fromList <$> ProjectApiKeys.projectApiKeysByProjectId pid
  if res > 0
    then addSuccessToast "Revoked API Key Successfully" Nothing
    else addErrorToast "Something went wrong" Nothing
  addRespHeaders $ ApiPost pid apikeys Nothing


apiActivateH :: Projects.ProjectId -> ProjectApiKeys.ProjectApiKeyId -> ATAuthCtx (RespHeaders ApiMut)
apiActivateH pid keyid = do
  (sess, project) <- Sessions.sessionAndProject pid
  res <- ProjectApiKeys.activateApiKey keyid
  apikeys <- V.fromList <$> ProjectApiKeys.projectApiKeysByProjectId pid
  if res > 0
    then addSuccessToast "Activated API Key Successfully" Nothing
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
  appCtx <- ask @AuthContext
  apiKeys <- V.fromList <$> ProjectApiKeys.projectApiKeysByProjectId pid
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "API keys"
          , isSettingsPage = True
          , config = appCtx.config
          }
  addRespHeaders $ ApiGet $ PageCtx bwconf (pid, apiKeys)


newtype ApiGet = ApiGet (PageCtx (Projects.ProjectId, V.Vector ProjectApiKeys.ProjectApiKey))


instance ToHtml ApiGet where
  toHtml (ApiGet (PageCtx bwconf (pid, apiKeys))) = toHtml $ PageCtx bwconf $ apiKeysPage pid apiKeys
  toHtmlRaw = toHtml


apiKeysPage :: Projects.ProjectId -> V.Vector ProjectApiKeys.ProjectApiKey -> Html ()
apiKeysPage pid apiKeys = do
  section_ [class_ "w-full mx-auto px-16 py-16 overflow-hidden overflow-y-scroll"] do
    div_ [class_ "flex justify-between items-center mb-6"] do
      div_ [class_ "flex flex-col gap-2"] do
        h2_ [class_ "text-xl font-semibold text-textWeak leading-7"] "Manage API keys"
        p_ [class_ "text-sm text-textWeak leading-tight"] "Create and revoke your API keys"
      label_ [class_ "btn btn-primary", Lucid.for_ "apikey-modal"] "Create an API key"
    mainContent pid apiKeys Nothing

    input_ [type_ "checkbox", id_ "apikey-modal", class_ "modal-toggle"]
    div_ [class_ "modal ", role_ "dialog", id_ "apikey-modal"] do
      div_ [class_ "modal-box flex flex-col p-8"] $ do
        div_ [class_ "flex w-full mb-4 justify-between items-start"] do
          div_ [class_ "p-3 bg-[#0068ff]/5 rounded-full w-max border-[#067a57]/20 gap-2 inline-flex"]
            $ faSprite_ "key" "regular" "h-6 w-6 text-textBrand"
          button_
            [ class_ "btn btn-ghost btn-sm btn-circle"
            , [__|on click set #apikey-modal.checked to false |]
            ]
            do
              faSprite_ "circle-xmark" "regular" "h-6 w-6 text-textWeak"
        span_ [class_ "text-textStrong text-2xl font-semibold mb-1"] "Generate an API key"
        form_
          [ hxPost_ $ "/p/" <> pid.toText <> "/apis"
          , class_ "flex flex-col gap-4"
          , hxTarget_ "#main-content"
          , [__|on closeModal from body set #apikey-modal.checked to false |]
          ]
          do
            div_ [class_ "flex flex-col"] do
              p_ [class_ "text-textWeak"] "Please input a title for your API key."
              div_ $ input_ [class_ "input px-4 py-2 mt-6  border w-full", type_ "text", placeholder_ "Enter your API key title", name_ "title", autofocus_]
            div_ [class_ "flex w-full"] do
              button_
                [ type_ "submit"
                , class_ "btn btn-primary w-full"
                ]
                "Create key"
      label_ [class_ "modal-backdrop", Lucid.for_ "apikey-modal"] "Close"


mainContent :: Projects.ProjectId -> V.Vector ProjectApiKeys.ProjectApiKey -> Maybe (ProjectApiKeys.ProjectApiKey, Text) -> Html ()
mainContent pid apiKeys newKeyM = section_ [id_ "main-content"] do
  copyNewApiKey newKeyM False
  let activeKeys = V.filter (\x -> x.active) apiKeys
      revokedKeys = V.filter (\x -> not x.active) apiKeys
      activeTable = makeApiKeysTable pid activeKeys "active_content"
      revokedTable = makeApiKeysTable pid revokedKeys "revoked_content"
      tabs =
        Table.TabFilter
          { current = "Active keys"
          , currentURL = ""
          , clientSide = True
          , options =
              [ Table.TabFilterOpt{name = "Active keys", count = Just $ V.length activeKeys, targetId = Just "#active_content"}
              , Table.TabFilterOpt{name = "Archived keys", count = Just $ V.length revokedKeys, targetId = Just "#revoked_content"}
              ]
          }

  toHtml tabs
  div_ [class_ "a-tab-content", id_ "active_content"] $ toHtml activeTable
  div_ [class_ "hidden a-tab-content", id_ "revoked_content"] $ toHtml revokedTable


makeApiKeysTable :: Projects.ProjectId -> V.Vector ProjectApiKeys.ProjectApiKey -> Text -> Table.Table (Int, ProjectApiKeys.ProjectApiKey)
makeApiKeysTable pid apiKeys elemId =
  Table.Table
    { config = def{Table.elemID = elemId, Table.renderAsTable = True}
    , columns = apiKeyColumns pid
    , rows = V.indexed apiKeys
    , features = def{Table.rowAttrs = Just $ const [class_ "group/row hover:bg-fillWeaker"]}
    }


apiKeyColumns :: Projects.ProjectId -> [Table.Column (Int, ProjectApiKeys.ProjectApiKey)]
apiKeyColumns pid =
  [ Table.col "Title" \(_, apiKey) ->
      span_ [class_ "text-textStrong font-semibold text-sm truncate"] $ toHtml apiKey.title
  , Table.col "Key" \(i, apiKey) -> do
      let idx = "key-" <> show i
      div_ [class_ "whitespace-nowrap w-full flex items-center text-sm text-textWeak"] do
        span_ [class_ $ "mr-2 w-full " <> idx] $ toHtml $ T.take 8 apiKey.keyPrefix <> T.replicate 20 "*"
        div_ [class_ "hidden group-hover:flex justify-between items-center gap-3"] do
          button_
            [ class_ "text-textBrand"
            , term "data-key" apiKey.keyPrefix
            , term "data-state" "hide"
            , term "data-tippy-content" "Show key"
            , term "data-prefix" (T.take 8 apiKey.keyPrefix <> T.replicate 20 "*")
            , term
                "_"
                [text|on click
                 if my @data-state is "hide"
                   put my @data-key into <.$idx/>
                   put "show" into my @data-state
                   put "Hide key" into my @data-tippy-content
                 else
                   put my @data-prefix into <.$idx/>
                   put "hide" into my @data-state
                   put "Show key" into my @data-tippy-content
                 end |]
            ]
            $ faSprite_ "eye" "regular" "h-4 w-4 text-textWeak"
          button_
            [ class_ "text-textBrand cursor-pointer"
            , term "data-key" apiKey.keyPrefix
            , [__| on click if 'clipboard' in window.navigator then
                            call navigator.clipboard.writeText(my @data-key)
                            send successToast(value:['API Key has been copied to the Clipboard']) to <body/>
                          end |]
            , term "data-tippy-content" "Copy key"
            ]
            $ faSprite_ "clipboard-copy" "regular" "h-4 w-4 text-textWeak"
          if apiKey.active
            then button_
              [ class_ "text-textWeak flex gap-2 items-center cursor-pointer"
              , hxDelete_ $ "/p/" <> pid.toText <> "/apis/" <> apiKey.id.toText
              , hxConfirm_ $ "Are you sure you want to revoke " <> apiKey.title <> " API Key?"
              , hxTarget_ "#main-content"
              , id_ $ "key" <> show i
              ]
              do
                faSprite_ "circle-xmark" "regular" "h-4 w-4 text-textError"
                span_ [class_ "text-textWeak"] "Revoke"
            else button_
              [ class_ "text-textWeak flex gap-2 items-center cursor-pointer"
              , hxPatch_ $ "/p/" <> pid.toText <> "/apis/" <> apiKey.id.toText
              , hxConfirm_ $ "Are you sure you want to activate " <> apiKey.title <> " API Key?"
              , hxTarget_ "#main-content"
              , id_ $ "key" <> show i
              ]
              do
                faSprite_ "circle-check" "regular" "h-4 w-4 text-textWeak"
                span_ [class_ "text-textWeak"] "Activate"
  ]


copyNewApiKey :: Maybe (ProjectApiKeys.ProjectApiKey, Text) -> Bool -> Html ()
copyNewApiKey newKeyM hasNext =
  case newKeyM of
    Nothing -> ""
    Just (keyObj, newKey) -> do
      div_ [id_ "apiFeedbackSection", class_ "pb-8"] do
        div_ [class_ "rounded-md bg-fillSuccess-weak p-4"] do
          div_ [class_ "flex"] do
            div_ [class_ "shrink-0"] do
              faSprite_ "circle-check" "regular" "h-5 w-5 text-textSuccess"
            div_ [class_ "ml-3"] do
              h3_ [class_ " font-medium text-textSuccess"] "API Key was generated successfully"
              div_ [class_ "mt-2  text-textSuccess py-2"] do
                strong_ [class_ "block pt-2", id_ "newKey"] $ toHtml newKey
              div_ [class_ "mt-4"] do
                div_ [class_ "-mx-2 -my-1.5 flex"] do
                  button_
                    [ type_ "button"
                    , class_ "bg-fillSuccess-strong px-2 py-1.5 text-white rounded-md  font-medium text-textSuccess hover:bg-fillSuccess-weak focus:outline-hidden focus:ring-2 focus:ring-offset-2 focus:ring-offset-fillSuccess-weak focus:ring-strokeSuccess-strong"
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
                        , class_ "ml-3 bg-green-50 px-2 py-1.5 rounded-md  font-medium text-textSuccess hover:bg-fillSuccess-weak focus:outline-hidden focus:ring-2 focus:ring-offset-2 focus:ring-offset-fillSuccess-weak focus:ring-strokeSuccess-strong"
                        , [__|on click remove #apiFeedbackSection|]
                        ]
                        "Dismiss"
                    else do
                      button_
                        [ type_ "button"
                        , class_ "ml-6 font-medium px-2 py-1.5 rounded-md font-medium text-textBrand"
                        , [__|on click call window.location.reload()|]
                        ]
                        "Next"
