module Pages.Endpoints.Server (server) where

import Models.Projects.Projects qualified as Projects
import Pages.Endpoints.ApiCatalog qualified as ApiCatalog
import Pages.Endpoints.EndpointDetails qualified as EndpointDetails
import Pages.Endpoints.EndpointList qualified as EndpointList
import Pages.Endpoints.Routes (Routes, Routes' (..))
import Servant qualified
import System.Types (ATAuthCtx)


server :: Projects.ProjectId -> Servant.ServerT Routes ATAuthCtx
server pid =
  Routes'
    { endpointListGet = EndpointList.endpointListGetH pid
    , fieldDetailsPartial = EndpointDetails.fieldDetailsPartialH pid
    , endpointDetailsWithHash = EndpointDetails.endpointDetailsWithHashH pid
    , endpointDetails = EndpointDetails.endpointDetailsH pid
    , apiCatalogGet = ApiCatalog.apiCatalogH pid
    }
