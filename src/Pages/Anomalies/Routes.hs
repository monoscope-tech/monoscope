module Pages.Anomalies.Routes (Routes, Routes' (..)) where

import Lucid (Html)
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Pages.Anomalies.AnomalyList qualified as AnomalyList
import Relude
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Htmx


type QPT a = QueryParam a Text
type QEID a = QueryParam a Endpoints.EndpointId


type Routes = NamedRoutes Routes'


type role Routes' nominal


data Routes' mode = Routes'
  { acknowlegeGet :: mode :- Capture "anomalyID" Anomalies.AnomalyId :> "acknowlege" :> Get '[HTML] (Html ())
  , unAcknowlegeGet :: mode :- Capture "anomalyID" Anomalies.AnomalyId :> "unacknowlege" :> Get '[HTML] (Html ())
  , archiveGet :: mode :- Capture "anomalyID" Anomalies.AnomalyId :> "archive" :> Get '[HTML] (Html ())
  , unarchiveGet :: mode :- Capture "anomalyID" Anomalies.AnomalyId :> "unarchive" :> Get '[HTML] (Html ())
  , bulkActionsPost :: mode :- "bulk_actions" :> Capture "action" Text :> ReqBody '[FormUrlEncoded] AnomalyList.AnomalyBulkForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
  , listGet :: mode :- QPT "layout" :> QPT "ackd" :> QPT "archived" :> QPT "sort" :> QPT "page" :> QPT "load_more" :> QEID "endpoint" :> HXRequest :> HXBoosted :> Get '[HTML] (Html ())
  , detailsGet :: mode :- "by_hash" :> Capture "targetHash" Text :> QPT "modal" :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)
