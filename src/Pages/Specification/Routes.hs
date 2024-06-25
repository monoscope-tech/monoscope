module Pages.Specification.Routes (Routes, Routes' (..)) where

import Lucid (Html)
import Pages.BodyWrapper (PageCtx (..))
import Pages.Specification.Documentation qualified as Documentation
import Relude
import Servant
import Servant.HTML.Lucid (HTML)
import System.Types (RespHeaders)


type role Routes' nominal


type Routes = NamedRoutes Routes'


data Routes' mode = Routes'
  { documentationPut :: mode :- "documentation" :> "save" :> ReqBody '[JSON] Documentation.SaveSwaggerForm :> Post '[HTML] (RespHeaders (Html ()))
  , documentationPost :: mode :- "documentation" :> ReqBody '[FormUrlEncoded] Documentation.SwaggerForm :> Post '[HTML] (RespHeaders (Html ()))
  , documentationGet :: mode :- "documentation" :> QueryParam "swagger_id" Text :> Get '[HTML] (RespHeaders (PageCtx (Documentation.DocumentationGet)))
  }
  deriving stock (Generic)
