module Pages.Monitors.Routes (Routes, Routes' (..)) where

import Data.Aeson qualified as AE
import Lucid (Html)
import Models.Apis.Monitors qualified as Monitors
import Models.Tests.Testing qualified as TestingM
import Pages.Monitors.Alerts qualified as Alerts
import Pages.Monitors.TestCollectionEditor qualified as Testing
import Pages.Monitors.Testing qualified as Testing
import Relude
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Htmx


type Routes = NamedRoutes Routes'


type QPT a = QueryParam a Text


type role Routes' nominal


data Routes' mode = Routes'
  { alertUpsertPost :: mode :- "alerts" :> ReqBody '[FormUrlEncoded] Alerts.AlertUpsertForm :> Post '[HTML] (Html ())
  , alertListGet :: mode :- "alerts" :> Get '[HTML] (Html ())
  , alertSingleGet :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> Get '[HTML] (Html ())
  , alertSingleToggleActive :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> "toggle_active" :> Post '[HTML] (Html ())
  , collectionsGet :: mode :- "testing" :> QPT "acknM" :> QPT "archM" :> Get '[HTML] (Html ())
  , newCollectionPost :: mode :- "testing" :> ReqBody '[FormUrlEncoded] Testing.TestCollectionForm :> QPT "acknM" :> QPT "archM" :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
  , collectionGet :: mode :- "testing" :> Capture "collection_id" TestingM.CollectionId :> Get '[HTML] (Html ())
  , collectionStepsUpdate :: mode :- "testing" :> Capture "collection_id" TestingM.CollectionId :> ReqBody '[JSON] Testing.CollectionStepUpdateForm :> Post '[HTML] (Html ())
  , collectionPut :: mode :- "testing" :> Capture "collection_id" TestingM.CollectionId :> Capture "action" Text :> ReqBody '[JSON] AE.Value :> Post '[HTML] (Html ())
  , collectionRunTests :: mode :- "testing" :> Capture "collection_id" TestingM.CollectionId :> QueryParam "step_index" Int :> ReqBody '[JSON] Testing.CollectionStepUpdateForm :> Patch '[HTML] (Html ())
  }
  deriving stock (Generic)
