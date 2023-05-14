module Pages.Documentation (documentationGetH) where

import Config
import Data.Default (def)
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Models.Projects.Projects qualified as Projects
import Models.Projects.RedactedFields qualified as RedactedFields
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude

documentationGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
documentationGetH sess pid = do
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
            pageTitle = "Documentation"
          }
  pure $ bodyWrapper bwconf $ documentationsPage pid

documentationsPage :: Projects.ProjectId -> Html ()
documentationsPage pid = do
  div_ [class_ "container mx-auto relative  px-4 pt-10 pb-24 h-full"] $ do
    -- modal
    div_
      [ style_ "background-color: rgba(0,0,0,0.5);z-index:99999",
        class_ "fixed hidden items-center justify-center top-0 left-0 right-0 z-50 w-full p-4 overflow-x-hidden overflow-y-auto md:inset-0 h-full max-h-full",
        id_ "swaggerModal",
        tabindex_ "-1"
      ]
      $ do
        div_
          [ class_ "relative w-[500px] max-h-full",
            style_ "width: min(90vw, 750px)"
          ]
          $ do
            -- Modal content
            div_ [class_ "relative bg-white rounded-lg shadow"] $ do
              div_ [class_ "flex items-start justify-between p-4 border-b rounded-t"] $ do
                h3_ [class_ "text-xl font-semibold text-gray-900 dark:text-white"] "Upload Swagger"
                button_ [type_ "button", class_ "btn bg-transparent hover:bg-gray-200 hover:text-gray-900 rounded-lg text-sm p-1.5 ml-auto inline-flex items-center", onclick_ "closeModal()"] "Close"
              -- Modal body
              div_ [class_ "p-6 space-y-6"] $ do
                textarea_ [style_ "height:65vh;resize:none", class_ "w-full border outline-none p-4 focus:outline-none focus:border-blue-200", placeholder_ "Paste swagger here"] ""
              -- Modal footer
              div_ [class_ "flex w-full justify-between items-center p-6 space-x-2 border-t border-gray-200 rounded-b"] $ do
                button_ [type_ "button", class_ "btn", onclick_ "closeModal()"] "Close"
                button_ [type_ "button", class_ "btn btn-primary"] "Upload"

    -- page content
    div_ [class_ "flex flex-col justify-between"] $ do
      div_ [class_ "flex w-full justify-between my-3 px-2"] $ do
        h3_ [class_ "text-xl text-slate-700 text-2xl font-medium"] "Swagger History"
        button_ [class_ "place-content-center text-md btn btn-primary", onclick_ "showModal()"] "Upload swagger"
      -- search
      div_ [class_ "card-round p-5"] $ do
        div_ [class_ "w-full flex flex-row m-3"] ""

  script_ "function showModal() { document.getElementById('swaggerModal').style.display = 'flex'; }"
  script_ "function closeModal(e) { document.getElementById('swaggerModal').style.display = 'none';}"
