module Pages.Endpoints.Routes (Routes, Routes' (..)) where

import Lucid (Html)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields.Types qualified as Fields (FieldId)
import Relude
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Htmx
import System.Types


type QPT a = QueryParam a Text


type Routes = NamedRoutes Routes'


type role Routes' nominal


data Routes' mode = Routes'
  { endpointListGet :: mode :- "endpoints" :> QPT "layout" :> QPT "filter" :> QPT "host" :> QPT "project_host" :> QPT "sort" :> HXRequest :> HXBoosted :> HXCurrentURL :> Get '[HTML] (RespHeaders (Html ()))
  , fieldDetailsPartial :: mode :- "fields" :> Capture "field_id" Fields.FieldId :> Get '[HTML] (RespHeaders (Html ()))
  , endpointDetailsWithHash :: mode :- "log_explorer" :> "endpoint" :> Capture "endpoint_hash" Text :> Get '[HTML] (RespHeaders (Html ()))
  , endpointDetails :: mode :- "endpoints" :> Capture "endpoints_id" Endpoints.EndpointId :> QPT "from" :> QPT "to" :> QPT "since" :> QPT "subpage" :> QPT "shape" :> Get '[HTML] (RespHeaders (Html ()))
  , outgoingGet :: mode :- "outgoing" :> QPT "sort" :> Get '[HTML] (RespHeaders (Html ()))
  }
  deriving stock (Generic)
