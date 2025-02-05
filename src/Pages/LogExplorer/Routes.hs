module Pages.LogExplorer.Routes (Routes, Routes' (..), server) where

import Data.Aeson qualified as AE
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Models.Projects.Projects qualified as Projects
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
  QueryParam,
  type (:>),
 )
import Servant.HTML.Lucid (HTML)
import Servant.Htmx (HXBoosted, HXRequest)
import System.Types (ATAuthCtx, RespHeaders)


type QPU a = QueryParam a UTCTime


type QPT a = QueryParam a Text


type role Routes' nominal


type Routes = NamedRoutes Routes'


-- TODO: rename layout to action
type Routes' :: Type -> Type
data Routes' mode = Routes'
  { logExplorerGet :: mode :- "log_explorer" :> QPT "query" :> QPT "queryAST" :> QPT "cols" :> QPU "cursor" :> QPT "since" :> QPT "from" :> QPT "to" :> QPT "layout" :> QPT "source" :> QPT "target-spans" :> QPT "queryTitle" :> QPT "queryLibId" :> HXRequest :> HXBoosted :> Get '[HTML] (RespHeaders Log.LogsGet)
  , logExplorerGetJson :: mode :- "log_explorer" :> "json" :> QPT "query" :> QPT "queryAST" :> QPT "cols" :> QPU "cursor" :> QPT "since" :> QPT "from" :> QPT "to" :> QPT "layout" :> QPT "source" :> QPT "target-spans" :> Get '[JSON] (RespHeaders AE.Value)
  , logExplorerItemGet :: mode :- "log_explorer" :> Capture "logItemID" UUID.UUID :> Capture "createdAt" UTCTime :> QPT "source" :> Get '[HTML] (RespHeaders LogItem.ApiLogItem)
  , logExplorerItemDetailedGet :: mode :- "log_explorer" :> Capture "logItemID" UUID.UUID :> Capture "createdAt" UTCTime :> "detailed" :> QPT "source" :> Get '[HTML] (RespHeaders LogItem.ApiItemDetailed)
  }
  deriving stock (Generic)


server :: Projects.ProjectId -> Servant.ServerT Routes ATAuthCtx
server pid =
  Routes'
    { logExplorerGet = Log.apiLogH pid
    , logExplorerGetJson = Log.apiLogJson pid
    , logExplorerItemGet = LogItem.apiLogItemH pid
    , logExplorerItemDetailedGet = LogItem.expandAPIlogItemH pid
    }
