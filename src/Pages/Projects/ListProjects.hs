{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Pages.Projects.ListProjects (
  listProjectsGetH,
  ListProjectsGet (..),
)
where

import Data.Default (def)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect
import Fmt
import Lucid
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Relude hiding (ask, asks)
import System.Types
import Utils (faSprite_)


listProjectsGetH :: ATAuthCtx (RespHeaders ListProjectsGet)
listProjectsGetH = do
  (sess, project) <- Sessions.sessionAndProject (Projects.ProjectId UUID.nil)
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession
          , pageTitle = "Projects List"
          }

  projects <- dbtToEff $ Projects.selectProjectsForUser sess.persistentSession.userId
  let projects' =
        V.snoc
          projects
          (def :: Projects.Project')
            { Projects.title = project.title
            , Projects.description = project.description
            , Projects.createdAt = project.createdAt
            }

  addRespHeaders $ ListProjectsGet $ PageCtx bwconf projects'


newtype ListProjectsGet = ListProjectsGet {unwrap :: PageCtx (V.Vector Projects.Project')}
  deriving stock (Show)


instance ToHtml ListProjectsGet where
  toHtml (ListProjectsGet (PageCtx bwconf projects)) = toHtml $ PageCtx bwconf $ listProjectsBody projects
  toHtmlRaw = toHtml


listProjectsBody :: V.Vector Projects.Project' -> Html ()
listProjectsBody projects = do
  section_ [id_ "main-content", class_ "container mx-auto p-6 pb-36 overflow-y-scroll  h-full"] do
    div_ [class_ "flex justify-between mb-6"] do
      h2_ [class_ "text-slate-700 text-2xl font-medium"] "Projects"
      a_ [class_ "btn btn-primary", href_ "/p/new"] "Create Project"
    section_ [] do
      div_ [class_ "bg-base-100 shadow overflow-hidden sm:rounded-md"] do
        ul_ [role_ "list", class_ "divide-y divide-gray-200"] $ mapM_ projectItem_ projects


projectItem_ :: Projects.Project' -> Html ()
projectItem_ project = li_ do
  a_ [href_ ("/p/" <> project.id.toText), class_ "block hover:bg-gray-50"] do
    div_ [class_ "px-4 py-4 flex items-center sm:px-6"] do
      div_ [class_ "min-w-0 flex-1 sm:flex sm:items-center sm:justify-between"] do
        div_ [class_ "truncate"] do
          div_ [class_ "text-sm"] do
            p_ [class_ "block font-medium text-indigo-600 truncate py-2"] $ toHtml project.title
            p_ [class_ "block flex-shrink-0 font-normal text-gray-500"] $ toHtml project.description
          div_ [class_ "mt-2 flex"] do
            div_ [class_ "flex items-center text-sm text-gray-500"] do
              small_ do
                span_ "Created on "
                time_ [datetime_ $ fmt $ dateDashF project.createdAt] $ toHtml @Text $ fmt $ dateDashF project.createdAt
        div_ [class_ "mt-4 flex-shrink-0 sm:mt-0 sm:ml-5"] do
          div_ [class_ "flex overflow-hidden -space-x-1"] do
            project.usersDisplayImages & V.toList & mapM_ \imgSrc -> img_ [class_ "inline-block h-6 w-6 rounded-full ring-2 ring-white", src_ imgSrc, alt_ "Dries Vincent"]
      div_ [class_ "ml-5 flex-shrink-0 text-gray-400"] do
        faSprite_ "chevron-right" "regular" "h-3 w-3"
