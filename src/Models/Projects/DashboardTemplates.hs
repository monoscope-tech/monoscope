module Models.Projects.DashboardTemplates (getDashboardTemplates, loadDashboardFromVM) where

import Effectful (Eff, IOE, type (:>))
import Models.Projects.Dashboards qualified as Dashboards
import Relude


loadDashboardFromVM :: [Dashboards.Dashboard] -> Dashboards.DashboardVM -> Maybe Dashboards.Dashboard
loadDashboardFromVM templates dashVM = dashVM.schema <|> find (\d -> d.file == dashVM.baseTemplate) templates


-- TH splice: reads all dashboard YAML files from static/public/dashboards at compile time
dashboardTemplatesCompiled :: [Dashboards.Dashboard]
dashboardTemplatesCompiled = $(Dashboards.readDashboardsFromDirectory "static/public/dashboards")


-- When liveReload is True, reads from disk on every access (for dev iteration without restart).
getDashboardTemplates :: IOE :> es => Bool -> Eff es [Dashboards.Dashboard]
getDashboardTemplates liveReload
  | liveReload = liftIO $ Dashboards.readDashboardsFromDisk "static/public/dashboards"
  | otherwise = pure dashboardTemplatesCompiled
