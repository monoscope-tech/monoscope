module Pages.Monitors.Routes (Routes, Routes' (..)) where

import Lucid (Html)
import Models.Apis.Monitors qualified as Monitors
import Models.Tests.Testing qualified as TestingM
import Pages.Monitors.Alerts qualified as Alerts
import Pages.Monitors.TestCollectionEditor qualified as Testing
import Relude
import Servant (Capture, FormUrlEncoded, GenericMode (type (:-)), Get, JSON, NamedRoutes, Patch, Post, QueryParam, ReqBody, type (:>))
import Servant.HTML.Lucid (HTML)
import System.Types (RespHeaders)


type Routes = NamedRoutes Routes'


type role Routes' nominal


data Routes' mode = Routes'
  { alertUpsertPost :: mode :- "alerts" :> ReqBody '[FormUrlEncoded] Alerts.AlertUpsertForm :> Post '[HTML] (RespHeaders (Html ()))
  , alertListGet :: mode :- "alerts" :> Get '[HTML] (RespHeaders (Html ()))
  , alertSingleGet :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> Get '[HTML] (RespHeaders (Html ()))
  , alertSingleToggleActive :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> "toggle_active" :> Post '[HTML] (RespHeaders (Html ()))
  , collectionsGet :: mode :- "testing" :> QueryParam "filter" Text :> Get '[HTML] (RespHeaders (Html ()))
  , newCollectionPost :: mode :- "testing" :> ReqBody '[JSON] Testing.CollectionStepUpdateForm :> Post '[HTML] (RespHeaders (Html ()))
  , collectionGet :: mode :- "testing" :> Capture "collection_id" TestingM.CollectionId :> Get '[HTML] (RespHeaders (Html ()))
  , collectionStepsUpdate :: mode :- "testing" :> Capture "collection_id" TestingM.CollectionId :> ReqBody '[JSON] Testing.CollectionStepUpdateForm :> Post '[HTML] (RespHeaders (Html ()))
  , collectionRunTests :: mode :- "testing" :> Capture "collection_id" TestingM.CollectionId :> QueryParam "step_index" Int :> ReqBody '[JSON] Testing.CollectionStepUpdateForm :> Patch '[HTML] (RespHeaders (Html ()))
  }
  deriving stock (Generic)
