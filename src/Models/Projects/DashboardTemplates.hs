module Models.Projects.DashboardTemplates (getDashboardTemplates, loadDashboardFromVM, detach, duplicate, Editable, editable, requireEditable, updateSchema, updateTitle, dashboardTemplatesCompiled) where

import Control.Lens ((%~))
import Data.Effectful.Hasql qualified as Hasql
import Data.FileEmbed (embedDir)
import Data.Generics.Labels ()
import Data.Text qualified as T
import Data.Time (UTCTime)
import Effectful (Eff, IOE, type (:>))
import Effectful.Error.Static (Error)
import Effectful.Reader.Static (Reader, ask)
import Hasql.Interpolate qualified as HI
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.Projects qualified as Projects
import Relude hiding (Reader, ask)
import Servant (ServerError)
import System.Config (AuthContext (..), DeploymentEnv (Dev), EnvConfig (..))
import System.FilePath (isExtensionOf)
import System.Types (DB)
import Web.ApiTypes qualified as ApiT


-- | Only a template may be read-only: a stored schema claiming it is ignored.
loadDashboardFromVM :: [Dashboards.Dashboard] -> Dashboards.DashboardVM -> Maybe Dashboards.Dashboard
loadDashboardFromVM templates dashVM = Dashboards.readOnlyTemplate templates dashVM <|> (dashVM.schema <&> \d -> d{Dashboards.readOnly = Nothing}) <|> find (\d -> d.file == dashVM.baseTemplate) templates


-- | An editable copy's row: the dashboard as it renders, cut loose from its template.
detach :: [Dashboards.Dashboard] -> Dashboards.DashboardVM -> Dashboards.DashboardVM
detach templates vm = vm{Dashboards.baseTemplate = Nothing, Dashboards.schema = loadDashboardFromVM templates vm <&> \d -> d{Dashboards.readOnly = Nothing}}


-- | The copier's own detached copy, outside any repository: a shared (repository, file) key is unique.
duplicate :: (IOE :> es, Reader AuthContext :> es) => Projects.UserId -> UTCTime -> Dashboards.DashboardId -> Dashboards.DashboardVM -> Eff es Dashboards.DashboardVM
duplicate uid now did vm = do
  templates <- getDashboardTemplates . (.config) =<< ask @AuthContext
  let copy = detach templates vm
      copied = (<> " (Copy)")
  pure
    copy
      { Dashboards.id = did
      , Dashboards.createdAt = now
      , Dashboards.updatedAt = now
      , Dashboards.createdBy = uid
      , Dashboards.title = copied $ if T.null vm.title then "Untitled" else vm.title
      , Dashboards.schema = copy.schema <&> #title %~ Just . copied . fromMaybe "Untitled"
      , Dashboards.starredSince = Nothing
      , Dashboards.homepageSince = Nothing
      , Dashboards.filePath = Nothing
      , Dashboards.fileSha = Nothing
      , Dashboards.gitSyncId = Nothing
      }


-- | Proof a dashboard's content may be written; only 'editable' makes one, never for a built-in read-only dashboard.
newtype Editable = Editable Dashboards.DashboardId


editable :: (IOE :> es, Reader AuthContext :> es) => Dashboards.DashboardVM -> Eff es (Maybe Editable)
editable vm = ask @AuthContext >>= getDashboardTemplates . (.config) <&> \templates -> Editable vm.id <$ guard (isNothing $ Dashboards.readOnlyTemplate templates vm)


requireEditable :: (Error ServerError :> es, IOE :> es, Reader AuthContext :> es) => Dashboards.DashboardVM -> Eff es Editable
requireEditable vm = editable vm `whenNothingM` ApiT.refuse ApiT.DashboardReadOnly


-- | Replace a dashboard's schema, optionally stamping @updated_at@. In-page widget edits pass
-- 'Nothing' so reordering a chart does not reshuffle the "recently updated" list; git sync and
-- the API pass the sync clock.
updateSchema :: DB es => Editable -> Dashboards.Dashboard -> Maybe UTCTime -> Eff es Int64
updateSchema (Editable dashId) dashboard updatedAt =
  Hasql.interpExecute
    $ [HI.sql| UPDATE projects.dashboards SET schema = #{dashboard} |]
    <> foldMap (\t -> [HI.sql| , updated_at = #{t} |]) updatedAt
    <> [HI.sql| WHERE id = #{dashId} |]


updateTitle :: DB es => Projects.ProjectId -> Editable -> Text -> Eff es Int64
updateTitle pid (Editable dashId) title = Hasql.interpExecute [HI.sql| UPDATE projects.dashboards SET title = #{title} WHERE project_id = #{pid} AND id = #{dashId} |]


-- Embedded raw and decoded at runtime: a home-module splice here would have to link every page.
-- DashboardTemplatesSpec checks that each template decodes.
dashboardTemplatesCompiled :: [Dashboards.Dashboard]
dashboardTemplatesCompiled = rights [Dashboards.decodeDashboard f bs | (f, bs) <- sortWith fst $(embedDir "static/public/dashboards"), "yaml" `isExtensionOf` f]


-- Reload from disk only in development; deployments always use the compiled templates.
getDashboardTemplates :: IOE :> es => EnvConfig -> Eff es [Dashboards.Dashboard]
getDashboardTemplates config
  | config.environment == Dev && config.liveReloadDashboards = liftIO $ Dashboards.readDashboardsFromDisk "static/public/dashboards"
  | otherwise = pure dashboardTemplatesCompiled
