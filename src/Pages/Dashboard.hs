module Pages.Dashboard (dashboardGetH) where

import Lucid
import Lucid.HTMX
import Config
import Relude
import Pages.BodyWrapper
import Data.UUID as UUID
import qualified Models.Projects.Projects as Projects



dashboardGetH :: Projects.ProjectId-> DashboardM (Html ())
dashboardGetH uid= do
  pure $ bodyWrapper "Dashboard" $ h1_ "Dashboard"
