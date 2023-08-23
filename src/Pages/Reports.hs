module Pages.Reports (reportsGetH) where

import Config
import Data.Default (def)

import Database.PostgreSQL.Entity.DBT (withPool)

import Data.Vector (Vector)
import Data.Vector qualified as V
import Lucid
import Models.Apis.Reports (Report (Report))
import Models.Apis.Reports qualified as Reports
import Models.Projects.Projects qualified as Projects

import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude

reportsGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
reportsGetH sess pid = do
  pool <- asks pool
  (project, reports) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      reports <- Reports.reportHistoryByProject pid
      pure (project, reports)

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = project
          , pageTitle = "Reports"
          }
  pure $ bodyWrapper bwconf $ reportsPage pid reports

reportsPage :: Projects.ProjectId -> Vector Reports.Report -> Html ()
reportsPage pid reports = do
  div_ [class_ "flex flex-col h-full w-full justify-between"] $ do
    div_ [class_ "flex w-full bg-white border-b items-center justify-between px-2", style_ "top: 0; height:60px; position: sticky"] $ do
      div_ [class_ "flex items-center gap-4"] $ do
        h3_ [class_ "text-xl text-slate-700 text-2xl font-medium"] "Reports History"
