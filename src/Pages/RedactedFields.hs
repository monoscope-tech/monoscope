{-# LANGUAGE NamedFieldPuns #-}

module Pages.RedactedFields (redactedFieldsGetH, redactedFieldsPostH, RedactFieldForm (..)) where

import Config
import Data.Aeson (encode)
import Data.Aeson.QQ (aesonQQ)
import Data.Default (def)
import Data.Text as T
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.HTMX
import Lucid.Hyperscript
import Models.Projects.Projects qualified as Projects
import Models.Projects.RedactedFields qualified as RedactedFields
import Models.Users.Sessions qualified as Sessions
import Optics.Core ((^.))
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude
import Servant (Headers, addHeader)
import Servant.Htmx (HXTrigger)
import Web.FormUrlEncoded (FromForm)

data RedactFieldForm = RedactFieldForm
  { path :: Text,
    description :: Text,
    endpointHash :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)

redactedFieldsPostH :: Sessions.PersistentSession -> Projects.ProjectId -> RedactFieldForm -> DashboardM (Headers '[HXTrigger] (Html ()))
redactedFieldsPostH sess pid RedactFieldForm {path, description, endpointHash} = do
  pool <- asks pool
  env <- asks env
  redactedFieldId <- RedactedFields.RedactedFieldId <$> liftIO UUIDV4.nextRandom
  -- adding path, description, endpoints via record punning
  let fieldToRedact = RedactedFields.RedactedField {id = redactedFieldId, projectId = pid, configuredVia = RedactedFields.Dashboard, ..}

  redactedFields <- liftIO $
    withPool pool $ do
      RedactedFields.redactField fieldToRedact
      RedactedFields.redactedFieldsByProject pid

  let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"closeModal": "", "successToast": ["Submitted field to be redacted, Successfully"]}|]
  pure $ addHeader hxTriggerData $ mainContent pid redactedFields

-- | redactedFieldsGetH renders the api keys list page which includes a modal for creating the apikeys.
redactedFieldsGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
redactedFieldsGetH sess pid = do
  pool <- asks pool
  (project, redactedFields) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      redactedFields <- RedactedFields.redactedFieldsByProject pid
      pure (project, redactedFields)

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess,
            currProject = project,
            pageTitle = "Redacted Fields"
          }
  pure $ bodyWrapper bwconf $ redactedFieldsPage pid redactedFields

redactedFieldsPage :: Projects.ProjectId -> Vector RedactedFields.RedactedField -> Html ()
redactedFieldsPage pid redactedFields = do
  section_ [class_ "container mx-auto  px-4 py-10 overflow-hidden overflow-y-scroll"] $ do
    div_ [class_ "flex justify-between mb-6"] $ do
      h2_ [class_ "text-slate-700 text-2xl font-medium"] "Redacted Fields"
      button_ [class_ "btn-indigo", [__|on click remove .hidden from #redactFieldDialog |]] "Create an API Key"
    mainContent pid redactedFields
    div_
      [ class_ "hidden fixed z-30 inset-0 overflow-y-auto",
        role_ "dialog",
        id_ "redactFieldDialog"
      ]
      $ do
        form_
          [ hxPost_ $ "/p/" <> Projects.projectIdText pid <> "/redacted_fields",
            class_ "flex items-end justify-center min-h-screen pt-4 px-4 pb-20 text-center sm:block sm:p-0",
            hxTarget_ "#main-content",
            [__|on closeModal from body add .hidden to #redactFieldDialog then call me.reset()|]
          ]
          $ do
            div_ [class_ "fixed inset-0 bg-gray-500 bg-opacity-75 transition-opacity"] $ do
              span_ [class_ "hidden sm:inline-block sm:align-middle sm:h-screen"] ""
            div_ [class_ "inline-block align-bottom bg-white rounded-lg px-4 pt-5 pb-4 text-left overflow-hidden shadow-xl transform transition-all sm:my-8 sm:align-middle sm:max-w-lg sm:w-full sm:p-6"] $ do
              div_ [class_ "hidden sm:block absolute top-0 right-0 pt-4 pr-4"] $ do
                button_
                  [ type_ "button",
                    class_ "bg-white rounded-md text-gray-400 hover:text-gray-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500",
                    [__|on click add .hidden to #redactFieldDialog|]
                  ]
                  $ do
                    span_ [class_ "sr-only"] "Close"
                    img_ [class_ "h-6 w-6", src_ "/assets/svgs/close.svg"]
              div_ [class_ "sm:flex sm:items-start"] $ do
                div_ [class_ "mx-auto flex-shrink-0 flex items-center justify-center h-12 w-12 rounded-full bg-red-100 sm:mx-0 sm:h-10 sm:w-10"] $ do
                  img_ [class_ "h-6 w-6 text-red-600", src_ "/assets/svgs/close.svg"]
                div_ [class_ "mt-3 text-center sm:mt-0 sm:ml-4 sm:text-left grow"] $ do
                  h3_ [class_ "text-lg leading-6 font-medium text-gray-900", id_ "modal-title"] "Redact a field path"
                  p_ [] "Redacting a field path means apitookit will strip out and discard all values of this field and will never be able to see those values."
                  div_ [class_ "mt-6 space-y-2"] $ do
                    label_ [class_ "text-sm text-gray-500"] "Please input the field path *"
                    div_ $ do
                      input_ [class_ "input-txt px-4 py-2  border w-full", type_ "text", placeholder_ "eg account.addresses.[].street", name_ "path", autofocus_]
                  div_ [class_ "mt-6 space-y-2"] $ do
                    label_ [class_ "text-sm text-gray-500"] "Description"
                    div_ $ do
                      textarea_ [class_ "input-txt px-4 py-2  border w-full", name_ "description", autofocus_] $ toHtml ("" :: Text)

              div_ [class_ "mt-5 sm:mt-4 sm:flex sm:flex-row-reverse"] $ do
                button_ [type_ "submit", class_ "w-full inline-flex justify-center rounded-md border border-transparent shadow-sm px-4 py-2 bg-red-600 text-base font-medium text-white hover:bg-red-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-red-500 sm:ml-3 sm:w-auto sm:text-sm"] "Submit"
                button_
                  [ type_ "button",
                    class_ "mt-3 w-full inline-flex justify-center rounded-md border border-gray-300 shadow-sm px-4 py-2 bg-white text-base font-medium text-gray-700 hover:text-gray-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 sm:mt-0 sm:w-auto sm:text-sm",
                    [__|on click add .hidden to #redactFieldDialog|]
                  ]
                  "Cancel"

mainContent :: Projects.ProjectId -> Vector RedactedFields.RedactedField -> Html ()
mainContent pid redactedFields = do
  section_ [id_ "main-content", class_ "flex flex-col"] $ do
    div_ [class_ "-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8"] $ do
      div_ [class_ "py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8"] $ do
        div_ [class_ "shadow overflow-hidden border-b border-gray-200 sm:rounded-lg"] $ do
          table_ [class_ "min-w-full divide-y divide-gray-200"] $ do
            thead_ [class_ "bg-gray-50"] $ do
              tr_ $ do
                th_ [class_ "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"] "Path"
                th_ [class_ "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"] "Description"
                th_ [class_ "px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider"] "Configured Via"
                th_ [class_ "relative px-6 py-3"] $ do
                  span_ [class_ "sr-only"] "Edit"
            tbody_ [class_ "bg-white divide-y divide-gray-200"] $ do
              redactedFields & mapM_ \rf -> do
                tr_ $ do
                  td_ [class_ "px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900"] $ toHtml $ rf ^. #path
                  td_ [class_ "px-6 py-4 whitespace-nowrap text-sm text-gray-500"] $ toHtml $ rf ^. #description
                  td_ [class_ "px-6 py-4 whitespace-nowrap text-sm text-gray-500"] $ toHtml @String $ show $ rf ^. #configuredVia
                  td_ [class_ "px-6 py-4 whitespace-nowrap text-right text-sm font-medium"] $ do
                    a_ [class_ "text-indigo-600 hover:text-indigo-900", href_ $ "/p/" <> Projects.projectIdText pid <> "/redacted_fields/delete"] $ do
                      img_ [src_ "/assets/svgs/revoke.svg", class_ "h-3 w-3 mr-2 inline-block"]
                      span_ [class_ "text-slate-500"] "Delete"
