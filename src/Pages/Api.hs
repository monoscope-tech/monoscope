module Pages.Api (apiGetH) where

import Config
import Data.UUID as UUID
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.HTMX
import qualified Models.Projects.Projects as Projects
import qualified Models.Users.Sessions as Sessions
import Pages.BodyWrapper (bodyWrapper)
import Relude


apiGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
apiGetH sess pid = do
  pool <- asks pool
  project <- liftIO $ withPool pool $ Projects.selectProjectForUser (Sessions.userId sess, pid)
  pure $ bodyWrapper (Just sess) project "API Keys" $ api 

api :: Html ()
api  = do 
  section_ [id_ "main-content"] $ do
    div_ [class_ "flex justify-between mb-6"] $ do
      h2_ [class_ "text-slate-700 text-2xl font-medium"] "API Keys"
      button_ [class_ "bg-blue-600 text-white p-3 text-sm rounded-lg"] "Create an API Key"
    section_ [] $ do
      div_ [class_ "bg-white shadow overflow-hidden p-5"] $ do   
        table_ [class_ "table-fixed w-full  mt-6"] $ do
        thead_ $ do
          tr_ [class_ "p-5 border-b border-gray-300 "] $ do
            th_ [class_ "text-left text-sm text-gray-400 font-normal "] "TITLE"
            th_ [class_ "text-left text-sm text-gray-400 font-normal"] "KEY"
            th_ [class_ "text-left text-sm text-gray-400 font-normal"] "ACTIONS"
        tbody_ $ do
          tr_ [class_ "p-5 "] $ do
            td_ [class_ " text-sm text-slate-500 font-normal"] "SendFunds"
            td_ [class_ " text-sm text-slate-500 font-normal"] "2555365**********"
            td_ [class_ "flex text-right text-sm text-slate-400"] $ do
              a_ [class_ "m-2 text-right flex", href_ " "] $ do 
                span_ [class_ "text-slate-500"] "Copy"
                img_ [src_ "/assets/svgs/copy.svg", class_ "h-3 mt-2 w-3 mx-2"]
              a_ [class_ "m-2 text-right flex", href_ " "] $ do 
                span_ [class_ "text-slate-500"] "Revoke"
                img_ [src_ "/assets/svgs/revoke.svg", class_ "h-3 mt-2 w-3 mx-2"]
          tr_ [class_ "p-5 "] $ do
            td_ [class_ " text-sm text-slate-500 font-normal"] "Lemonade"
            td_ [class_ " text-sm text-slate-500 font-normal"] "3545266**********"
            td_ [class_ "flex text-right text-sm text-slate-400"] $ do
              a_ [class_ "m-2 text-right flex", href_ " "] $ do 
                span_ [class_ "text-slate-500"] "Copy"
                img_ [src_ "/assets/svgs/copy.svg", class_ "h-3 mt-2 w-3 mx-2"]
              a_ [class_ "m-2 text-right flex", href_ " "] $ do 
                span_ [class_ "text-slate-500"] "Revoke"
                img_ [src_ "/assets/svgs/revoke.svg", class_ "h-3 mt-2 w-3 mx-2"]
            

