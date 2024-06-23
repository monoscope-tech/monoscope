module Pages.Anomalies.Routes (Routes, Routes' (..)) where

import Lucid (Html)
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


data Routes' mode = Routes'
  { acknowlegeGet :: mode :- Capture "anomalyID" Anomalies.AnomalyId :> "acknowlege" :> Get '[HTML] (RespHeaders (Html ()))
  , unAcknowlegeGet :: mode :- Capture "anomalyID" Anomalies.AnomalyId :> "unacknowlege" :> Get '[HTML] (RespHeaders (Html ()))
  , archiveGet :: mode :- Capture "anomalyID" Anomalies.AnomalyId :> "archive" :> Get '[HTML] (RespHeaders (Html ()))
  , unarchiveGet :: mode :- Capture "anomalyID" Anomalies.AnomalyId :> "unarchive" :> Get '[HTML] (RespHeaders (Html ()))
  , bulkActionsPost :: mode :- "bulk_actions" :> Capture "action" Text :> ReqBody '[FormUrlEncoded] AnomalyList.AnomalyBulkForm :> Post '[HTML] (RespHeaders (Html ()))
  , listGet :: mode :- QPT "layout" :> QPT "filter" :> QPT "sort" :> QPT "page" :> QPT "load_more" :> QEID "endpoint" :> HXRequest :> HXBoosted :> Get '[HTML] (RespHeaders (AnomalyList.AnomalyListGet))
  , detailsGet :: mode :- "by_hash" :> Capture "targetHash" Text :> QPT "modal" :> Get '[HTML] (RespHeaders (Html ()))
  }
  deriving stock (Generic)
