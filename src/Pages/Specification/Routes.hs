module Pages.Specification.Routes (Routes, Routes' (..)) where

import GHC.Generics
import Lucid (Html)
import Pages.Specification.Documentation qualified as Documentation
import Relude
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.Htmx


type role Routes' nominal


type Routes = NamedRoutes Routes'
data Routes' mode = Routes'
  { documentationPut :: mode :- "documentation" :> "save" :> ReqBody '[JSON] Documentation.SaveSwaggerForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
  , documentationPost :: mode :- "documentation" :> ReqBody '[FormUrlEncoded] Documentation.SwaggerForm :> Post '[HTML] (Headers '[HXTrigger] (Html ()))
  , documentationGet :: mode :- "documentation" :> QueryParam "swagger_id" Text :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)
