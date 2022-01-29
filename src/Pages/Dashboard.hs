module Pages.Dashboard (dashboardGetH) where

import Config
import Data.UUID as UUID
import Lucid
import Lucid.HTMX
import qualified Models.Projects.Projects as Projects
import Pages.BodyWrapper
import Relude

dashboardGetH :: Projects.ProjectId -> DashboardM (Html ())
dashboardGetH uid = do
  pure $ bodyWrapper "Dashboard" $ h1_ "Dashboard"
