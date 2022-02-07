module Pages.Dashboard (dashboardGetH) where

import Config
import Data.UUID as UUID
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.HTMX
import qualified Models.Projects.Projects as Projects
import qualified Models.Users.Sessions as Sessions
import Pages.BodyWrapper
import Relude

dashboardGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
dashboardGetH sess pid = do
  pool <- asks pool
  project <- liftIO $ withPool pool $ Projects.selectProjectForUser (Sessions.userId sess, pid)
  pure $ bodyWrapper (Just sess) project "Dashboard" $ h1_ "Dashboard"
