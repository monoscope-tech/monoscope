module Pages.LogExplorer.Routes (Routes, Routes' (..), server) where

import Data.Aeson qualified as AE
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Schema qualified as Schema
import Pages.Log qualified as Log
import Pages.LogExplorer.LogItem qualified as LogItem
import Relude (Generic, Text, Type)
import Servant (
  Capture,
  GenericMode (type (:-)),
  Get,
  HasServer (ServerT),
  JSON,
  NamedRoutes,
  Post,
  QueryParam,
  ReqBody,
  type (:>),
 )
import Servant.HTML.Lucid (HTML)
import Servant.Htmx (HXBoosted, HXRequest)
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)


type QPU a = QueryParam a UTCTime


type QPT a = QueryParam a Text


type role Routes' nominal


type Routes = NamedRoutes Routes'


-- TODO: rename layout to action
type Routes' :: Type -> Type
data Routes' mode = Routes'
  { logExplorerGet :: mode :- "log_explorer" :> QPT "query" :> QPT "cols" :> QPU "cursor" :> QPT "since" :> QPT "from" :> QPT "to" :> QPT "layout" :> QPT "source" :> QPT "target-spans" :> QPT "queryTitle" :> QPT "queryLibId" :> QPT "details_width" :> QPT "target_event" :> QPT "showTrace" :> HXRequest :> HXBoosted :> QPT "json" :> Get '[HTML, JSON] (RespHeaders Log.LogsGet)
  , logExplorerItemDetailedGet :: mode :- "log_explorer" :> Capture "logItemID" UUID.UUID :> Capture "createdAt" UTCTime :> "detailed" :> QPT "source" :> Get '[HTML] (RespHeaders LogItem.ApiItemDetailed)
  , aiSearchPost :: mode :- "log_explorer" :> "ai_search" :> ReqBody '[JSON] AE.Value :> Post '[JSON] (RespHeaders AE.Value)
  , schemaGet :: mode :- "log_explorer" :> "schema" :> Get '[JSON] (RespHeaders AE.Value)
  }
  deriving stock (Generic)


server :: Projects.ProjectId -> Servant.ServerT Routes ATAuthCtx
server pid =
  Routes'
    { logExplorerGet = Log.apiLogH pid
    , logExplorerItemDetailedGet = LogItem.expandAPIlogItemH pid
    , aiSearchPost = Log.aiSearchH pid
    , schemaGet = schemaH
    }


schemaH :: ATAuthCtx (RespHeaders AE.Value)
schemaH = addRespHeaders Schema.telemetrySchemaJson
