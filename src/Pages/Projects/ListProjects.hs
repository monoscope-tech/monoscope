module Pages.Projects.ListProjects (
  listProjectsGetH,
) where

import Config
import Data.Default (def)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity.DBT (withPool)
import Fmt
import Lucid
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude
import Servant (Union, WithStatus (..), respond)
import Utils (GetOrRedirect, redirect)

listProjectsGetH :: Sessions.PersistentSession -> DashboardM (Union GetOrRedirect)
listProjectsGetH sess = do
  pool <- asks pool
  projects <-
    if sess.isSudo
      then liftIO $ withPool pool $ Projects.selectProjectsForUser (sess.userId)
      else liftIO $ withPool pool $ Projects.selectProjectsForUser (sess.userId)
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , pageTitle = "Projects List"
          }
  let page = bodyWrapper bwconf $ listProjectsBody projects
  -- Redirect to the create projects page if there's no project under the logged in user
  if null projects
    then respond $ WithStatus @302 $ redirect "/p/new"
    else respond $ WithStatus @200 page

listProjectsBody :: Vector.Vector Projects.Project' -> Html ()
listProjectsBody projects = do
  section_ [id_ "main-content", class_ "p-6 pb-36"] $ do
    div_ [class_ "flex justify-between mb-6"] $ do
      h2_ [class_ "text-slate-700 text-2xl font-medium"] "Projects"
      a_ [class_ "btn btn-primary", href_ "/p/new"] "Create Project"
    section_ [] $ do
      div_ [class_ "bg-white shadow overflow-hidden sm:rounded-md"] $ do
        ul_ [role_ "list", class_ "divide-y divide-gray-200"] $ do
          projects & mapM_ \project -> do
            li_ $ do
              a_ [href_ ("/p/" <> project.id.toText), class_ "block hover:bg-gray-50"] $ do
                div_ [class_ "px-4 py-4 flex items-center sm:px-6"] $ do
                  div_ [class_ "min-w-0 flex-1 sm:flex sm:items-center sm:justify-between"] $ do
                    div_ [class_ "truncate"] $ do
                      div_ [class_ "text-sm"] $ do
                        p_ [class_ "block font-medium text-indigo-600 truncate py-2"] $ toHtml $ project.title
                        p_ [class_ "block flex-shrink-0 font-normal text-gray-500"] $ toHtml $ project.description
                      div_ [class_ "mt-2 flex"] $ do
                        div_ [class_ "flex items-center text-sm text-gray-500"] $ do
                          small_ $ do
                            span_ "Created on "
                            time_ [datetime_ $ fmt $  dateDashF project.createdAt] $ toHtml @Text $ fmt $ dateDashF project.createdAt
                    div_ [class_ "mt-4 flex-shrink-0 sm:mt-0 sm:ml-5"] $ do
                      div_ [class_ "flex overflow-hidden -space-x-1"] $ do
                        project.usersDisplayImages & Vector.toList & mapM_ \imgSrc -> img_ [class_ "inline-block h-6 w-6 rounded-full ring-2 ring-white", src_ imgSrc, alt_ "Dries Vincent"]
                  div_ [class_ "ml-5 flex-shrink-0 text-gray-400"] $ do
                    img_ [src_ "/assets/svgs/right_chevron.svg"]
