module Pages.LogExplorer.Routes (Routes, Routes' (..)) where

import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Lucid (Html)
import Relude (Generic, Text)
import Servant (
  Capture,
  GenericMode (type (:-)),
  Get,
  NamedRoutes,
  QueryParam,
  type (:>),
 )
import Servant.HTML.Lucid (HTML)
import Servant.Htmx (HXBoosted, HXRequest)


type QPU a = QueryParam a UTCTime
type QPT a = QueryParam a Text


type role Routes' nominal


type Routes = NamedRoutes Routes'
data Routes' mode = Routes'
  { logExplorerGet :: mode :- "log_explorer" :> QPT "query" :> QPT "cols" :> QPU "cursor" :> QPT "since" :> QPT "from" :> QPT "to" :> QPT "layout" :> HXRequest :> HXBoosted :> Get '[HTML] (Html ())
  , logExplorerItemGet :: mode :- "log_explorer" :> Capture "logItemID" UUID.UUID :> Capture "createdAt" UTCTime :> Get '[HTML] (Html ())
  , logExplorerItemDetailedGet :: mode :- "log_explorer" :> Capture "logItemID" UUID.UUID :> Capture "createdAt" UTCTime :> "detailed" :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)
