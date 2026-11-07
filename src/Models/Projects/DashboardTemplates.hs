module Models.Projects.DashboardTemplates (getDashboardTemplates, loadDashboardFromVM, detach, dashboardTemplatesCompiled) where

import Data.FileEmbed (embedDir)
import Effectful (Eff, IOE, type (:>))
import Models.Projects.Dashboards qualified as Dashboards
import Relude
import System.Config (DeploymentEnv (Dev), EnvConfig (..))
import System.FilePath (isExtensionOf)


-- | Only a template may be read-only: a stored schema claiming it is ignored.
loadDashboardFromVM :: [Dashboards.Dashboard] -> Dashboards.DashboardVM -> Maybe Dashboards.Dashboard
loadDashboardFromVM templates dashVM = Dashboards.readOnlyTemplate templates dashVM <|> (dashVM.schema <&> \d -> d{Dashboards.readOnly = Nothing}) <|> find (\d -> d.file == dashVM.baseTemplate) templates


-- | An editable copy's row: the dashboard as it renders, cut loose from its template.
detach :: [Dashboards.Dashboard] -> Dashboards.DashboardVM -> Dashboards.DashboardVM
detach templates vm = vm{Dashboards.baseTemplate = Nothing, Dashboards.schema = loadDashboardFromVM templates vm <&> \d -> d{Dashboards.readOnly = Nothing}}


-- Embedded raw and decoded at runtime: a home-module splice here would have to link every page.
-- DashboardTemplatesSpec checks that each template decodes.
dashboardTemplatesCompiled :: [Dashboards.Dashboard]
dashboardTemplatesCompiled = rights [Dashboards.decodeDashboard f bs | (f, bs) <- sortWith fst $(embedDir "static/public/dashboards"), "yaml" `isExtensionOf` f]


-- Reload from disk only in development; deployments always use the compiled templates.
getDashboardTemplates :: IOE :> es => EnvConfig -> Eff es [Dashboards.Dashboard]
getDashboardTemplates config
  | config.environment == Dev && config.liveReloadDashboards = liftIO $ Dashboards.readDashboardsFromDisk "static/public/dashboards"
  | otherwise = pure dashboardTemplatesCompiled
