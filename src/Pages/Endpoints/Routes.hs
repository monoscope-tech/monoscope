module Pages.Endpoints.Routes (Routes, Routes' (..)) where

import Lucid (Html)
import Models.Apis.Endpoints qualified as Endpoints
import Pages.Endpoints.EndpointList qualified as EndpointList
import Models.Apis.Fields.Types qualified as Fields (FieldId)
import Relude
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Htmx
import Pages.Endpoints.Outgoing  qualified as Outgoing
import System.Types
import Pages.BodyWrapper
import Pkg.Components.ItemsList qualified as ItemsList


type QPT a = QueryParam a Text


type Routes = NamedRoutes Routes'


type role Routes' nominal


data Routes' mode = Routes'
  { endpointListGet :: mode :- "endpoints" :> QPT "layout" :> QPT "filter" :> QPT "host" :> QPT "project_host" :> QPT "sort" :> HXRequest :> HXBoosted :> HXCurrentURL :> Get '[HTML] (RespHeaders (PageCtx (ItemsList.ItemsPage EndpointList.EndpointRequestStatsVM)))
  , fieldDetailsPartial :: mode :- "fields" :> Capture "field_id" Fields.FieldId :> Get '[HTML] (RespHeaders (Html ()))
  , endpointDetailsWithHash :: mode :- "log_explorer" :> "endpoint" :> Capture "endpoint_hash" Text :> Get '[HTML] (RespHeaders (Html ()))
  , endpointDetails :: mode :- "endpoints" :> Capture "endpoints_id" Endpoints.EndpointId :> QPT "from" :> QPT "to" :> QPT "since" :> QPT "subpage" :> QPT "shape" :> Get '[HTML] (RespHeaders (Html ()))
  , outgoingGet :: mode :- "outgoing" :> QPT "sort" :> Get '[HTML] (RespHeaders (PageCtx (ItemsList.ItemsPage Outgoing.HostEventsVM)))
  }
  deriving stock (Generic)
