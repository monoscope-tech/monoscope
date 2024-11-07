module Pages.LogExplorer.Routes (Routes, Routes' (..), server) where

import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Models.Projects.Projects qualified as Projects
import Pages.Log qualified as Log
import Pages.LogExplorer.LogItem qualified as LogItem
import Relude (Generic, Text)
import Servant (
  Capture,
  GenericMode (type (:-)),
  Get,
  HasServer (ServerT),
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


data Routes' mode = Routes'
  { logExplorerGet :: mode :- "log_explorer" :> QPT "query" :> QPT "query-ast" :> QPT "cols" :> QPU "cursor" :> QPT "since" :> QPT "from" :> QPT "to" :> QPT "layout" :> QPT "source" :> QPT "target-spans" :> HXRequest :> HXBoosted :> Get '[HTML] (RespHeaders Log.LogsGet)
  , logExplorerItemGet :: mode :- "log_explorer" :> Capture "logItemID" UUID.UUID :> Capture "createdAt" UTCTime :> QPT "source" :> Get '[HTML] (RespHeaders LogItem.ApiLogItem)
  , logExplorerItemDetailedGet :: mode :- "log_explorer" :> Capture "logItemID" UUID.UUID :> Capture "createdAt" UTCTime :> "detailed" :> QPT "source" :> Get '[HTML] (RespHeaders LogItem.ApiItemDetailed)
  }
  deriving stock (Generic)


server :: Projects.ProjectId -> Servant.ServerT Routes ATAuthCtx
server pid =
  Routes'
    { logExplorerGet = Log.apiLogH pid
    , logExplorerItemGet = LogItem.apiLogItemH pid
    , logExplorerItemDetailedGet = LogItem.expandAPIlogItemH pid
    }
