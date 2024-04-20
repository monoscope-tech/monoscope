module Pages.Monitors.Routes (Routes, Routes' (..)) where

import Data.Aeson qualified as AE
import Lucid (Html)
import Models.Apis.Monitors qualified as Monitors
import Models.Tests.Testing qualified as TestingM
import Pages.Monitors.Alerts qualified as Alerts
import Pages.Monitors.Testing qualified as Testing
import Relude
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Htmx


type Routes = NamedRoutes Routes'


type role Routes' nominal


data Routes' mode = Routes'
  { alertUpsertPost :: mode :- "alerts" :> ReqBody '[FormUrlEncoded] Alerts.AlertUpsertForm :> Post '[HTML] (Html ())
  , alertListGet :: mode :- "alerts" :> Get '[HTML] (Html ())
  , alertSingleGet :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> Get '[HTML] (Html ())
  , alertSingleToggleActive :: mode :- "alerts" :> Capture "alert_id" Monitors.QueryMonitorId :> "toggle_active" :> Post '[HTML] (Html ())
  , collectionsGet :: mode :- "testing" :> Get '[HTML] (Html ())
  , newCollectionPost :: mode :- "testing" :> ReqBody '[FormUrlEncoded] Testing.TestCollectionForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
  , collectionGet :: mode :- "testing" :> Capture "collection_id" TestingM.CollectionId :> Get '[HTML] (Html ())
  , collectionPut :: mode :- "testing" :> Capture "collection_id" TestingM.CollectionId :> Capture "action" Text :> ReqBody '[JSON] AE.Value :> Post '[HTML] (Html ())
  , collectionStepPost :: mode :- "testing" :> "add_step" :> Capture "collection_id" TestingM.CollectionId :> ReqBody '[JSON] AE.Value :> Post '[HTML] (Html ())
  , collectionStepPut :: mode :- "testing" :> "step" :> Capture "step_id" TestingM.CollectionStepId :> ReqBody '[JSON] AE.Value :> Post '[HTML] (Html ())
  , saveFromCodePost :: mode :- "testing" :> "save_from_code" :> Capture "collection_id" TestingM.CollectionId :> ReqBody '[JSON] Testing.CodeOperationsForm :> Post '[HTML] (Html ())
  , deleteCollectionStep :: mode :- "testing" :> "step" :> Capture "step_id" TestingM.CollectionStepId :> Delete '[HTML] (Html ())
  , runTestCollection :: mode :- "testing" :> "run" :> Capture "collection_id" TestingM.CollectionId :> Post '[HTML] (Html ())
  , runTestCollectionStep :: mode :- "testing" :> "run" :> Capture "collection_id" TestingM.CollectionId :> Capture "step_id" TestingM.CollectionStepId :> Post '[HTML] (Html ())
  }
  deriving stock (Generic)
