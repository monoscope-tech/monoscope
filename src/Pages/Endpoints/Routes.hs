module Pages.Endpoints.Routes (Routes, Routes' (..)) where

import Lucid (Html)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields.Types qualified as Fields (FieldId)
import Pages.BodyWrapper
import Pages.Endpoints.ApiCatalog qualified as ApiCatalog
import Pages.Endpoints.EndpointDetails qualified as EndpointDetails
import Pages.Endpoints.EndpointList qualified as EndpointList
import Pkg.Components.ItemsList qualified as ItemsList
import Relude
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Htmx
import System.Types


type QPT a = QueryParam a Text


type Routes = NamedRoutes Routes'


type role Routes' nominal


data Routes' mode = Routes'
  { endpointListGet :: mode :- "endpoints" :> QPT "page" :> QPT "layout" :> QPT "filter" :> QPT "host" :> QPT "request_type" :> QPT "sort" :> HXRequest :> HXBoosted :> HXCurrentURL :> QPT "load_more" :> QPT "search" :> Get '[HTML] (RespHeaders EndpointList.EndpointRequestStatsVM)
  , fieldDetailsPartial :: mode :- "fields" :> Capture "field_id" Fields.FieldId :> Get '[HTML] (RespHeaders (EndpointDetails.FieldDetails))
  , endpointDetailsWithHash :: mode :- "log_explorer" :> "endpoint" :> Capture "endpoint_hash" Text :> Get '[HTML] (RespHeaders (Html ()))
  , endpointDetails :: mode :- "endpoints" :> Capture "endpoints_id" Endpoints.EndpointId :> QPT "from" :> QPT "to" :> QPT "since" :> QPT "subpage" :> QPT "shape" :> Get '[HTML] (RespHeaders EndpointDetails.EndpointDetailsGet)
  , apiCatalogGet :: mode :- "api_catalog" :> QPT "sort" :> QPT "request_type" :> Get '[HTML] (RespHeaders (PageCtx (ItemsList.ItemsPage ApiCatalog.HostEventsVM)))
  }
  deriving stock (Generic)
