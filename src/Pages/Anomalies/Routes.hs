module Pages.Anomalies.Routes (Routes, Routes' (..)) where

import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Pages.Anomalies.AnomalyList qualified as AnomalyList
import Relude
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Htmx
import System.Types


type QPT a = QueryParam a Text
type QEID a = QueryParam a Endpoints.EndpointId


type Routes = NamedRoutes Routes'


type role Routes' nominal

type Routes' :: forall {k}. k -> Type
data Routes' mode = Routes'
  { acknowlegeGet :: mode :- Capture "anomalyID" Anomalies.AnomalyId :> "acknowlege" :> QPT "host" :> Get '[HTML] (RespHeaders AnomalyList.AnomalyAction)
  , unAcknowlegeGet :: mode :- Capture "anomalyID" Anomalies.AnomalyId :> "unacknowlege" :> Get '[HTML] (RespHeaders AnomalyList.AnomalyAction)
  , archiveGet :: mode :- Capture "anomalyID" Anomalies.AnomalyId :> "archive" :> Get '[HTML] (RespHeaders AnomalyList.AnomalyAction)
  , unarchiveGet :: mode :- Capture "anomalyID" Anomalies.AnomalyId :> "unarchive" :> Get '[HTML] (RespHeaders AnomalyList.AnomalyAction)
  , bulkActionsPost :: mode :- "bulk_actions" :> Capture "action" Text :> ReqBody '[FormUrlEncoded] AnomalyList.AnomalyBulkForm :> Post '[HTML] (RespHeaders AnomalyList.AnomalyAction)
  , listGet :: mode :- QPT "layout" :> QPT "filter" :> QPT "sort" :> QPT "since" :> QPT "page" :> QPT "load_more" :> QEID "endpoint" :> HXRequest :> HXBoosted :> Get '[HTML] (RespHeaders AnomalyList.AnomalyListGet)
  , detailsGet :: mode :- "by_hash" :> Capture "targetHash" Text :> QPT "modal" :> Get '[HTML] (RespHeaders AnomalyList.AnomalyDetails)
  }
  deriving stock (Generic)
