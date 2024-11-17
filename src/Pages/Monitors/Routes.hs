module Pages.Monitors.Routes (server, Routes, Routes' (..)) where

import Lucid
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Projects qualified as Projects
import Models.Tests.Testing qualified as TestingM
import Pages.BodyWrapper (PageCtx (..))
import Pages.Monitors.Alerts qualified as Alerts
import Pages.Monitors.MetricMonitors qualified as MetricMonitors
import Pages.Monitors.TestCollectionEditor qualified as TestCollectionEditor
import Pages.Monitors.Testing qualified as Testing
import Pkg.Components.ItemsList qualified as ItemsList
import Pkg.RouteUtils
import Relude
import Servant (Capture, Delete, FormUrlEncoded, GenericMode (type (:-)), Get, JSON, NamedRoutes, Patch, Post, QueryParam, ReqBody, type (:>))
import Servant qualified
import Servant.HTML.Lucid (HTML)
import System.Types (ATAuthCtx, RespHeaders)


type Routes = NamedRoutes Routes'


type role Routes' nominal

type Routes' :: forall {k}. k -> Type
data Routes' mode = Routes'
  { alertUpsertPost :: mode :- "alerts" :> ReqBody '[FormUrlEncoded] Alerts.AlertUpsertForm :> Post '[HTML] (RespHeaders Alerts.Alert)
  , alertListGet :: mode :- "alerts" :> Get '[HTML] (RespHeaders Alerts.Alert)
  , alertSingleGet :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> Get '[HTML] (RespHeaders Alerts.Alert)
  , alertSingleToggleActive :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> "toggle_active" :> Post '[HTML] (RespHeaders Alerts.Alert)
  , monitorListGet :: mode :- "monitors" :> QueryParam "filter" Text :> QueryParam "since" Text :> Get '[HTML] (RespHeaders (PageCtx (ItemsList.ItemsPage Testing.CollectionListItemVM)))
  , monitorCreatePost :: mode :- "monitors" :> "create" :> QPT "monitor-type" :> Get '[HTML] (RespHeaders (PageCtx MetricMonitors.MonitorCreate))
  , collectionGet :: mode :- "monitors" :> "collection" :> QueryParam "col_id" TestingM.CollectionId :> Get '[HTML] (RespHeaders TestCollectionEditor.CollectionGet)
  , collectionDashboardGet :: mode :- "monitors" :> Capture "collection_id" TestingM.CollectionId :> "overview" :> Get '[HTML] (RespHeaders (PageCtx (Html ())))
  , collectionStepsUpdate :: mode :- "monitors" :> "collection" :> ReqBody '[JSON] TestingM.CollectionStepUpdateForm :> Post '[HTML] (RespHeaders TestCollectionEditor.CollectionMut)
  , collectionRunTests :: mode :- "monitors" :> Capture "collection_id" TestingM.CollectionId :> QueryParam "step_index" Int :> ReqBody '[JSON] TestingM.CollectionStepUpdateForm :> Patch '[HTML] (RespHeaders TestCollectionEditor.CollectionRunTest)
  , collectionVarsPost :: mode :- "monitors" :> Capture "collection_id" TestingM.CollectionId :> "variables" :> ReqBody '[JSON] TestCollectionEditor.CollectionVariableForm :> Post '[HTML] (RespHeaders (Html ()))
  , collectionVarsDelete :: mode :- "monitors" :> Capture "collection_id" TestingM.CollectionId :> "variables" :> Capture "variable_name" Text :> Delete '[HTML] (RespHeaders (Html ()))
  }
  deriving stock (Generic)


server :: Projects.ProjectId -> Servant.ServerT Routes ATAuthCtx
server pid =
  Routes'
    { alertUpsertPost = Alerts.alertUpsertPostH pid
    , alertListGet = Alerts.alertListGetH pid
    , alertSingleGet = Alerts.alertSingleGetH pid
    , alertSingleToggleActive = Alerts.alertSingleToggleActiveH pid
    , monitorListGet = Testing.testingGetH pid
    , monitorCreatePost = MetricMonitors.monitorCreateGetH pid
    , collectionGet = TestCollectionEditor.collectionGetH pid
    , collectionStepsUpdate = TestCollectionEditor.collectionStepsUpdateH pid
    , collectionRunTests = TestCollectionEditor.collectionRunTestsH pid
    , collectionDashboardGet = Testing.collectionDashboard pid
    , collectionVarsPost = TestCollectionEditor.collectionStepVariablesUpdateH pid
    , collectionVarsDelete = TestCollectionEditor.collectionStepVariablesDeleteH pid
    }
