module Pages.Traces.Routes (Routes, Routes' (..), server) where

import Models.Projects.Projects qualified as Projects
import Pages.Traces.Trace qualified as Trace
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
import System.Types (ATAuthCtx, RespHeaders)


type QPT a = QueryParam a Text


type role Routes' nominal


type Routes = NamedRoutes Routes'


data Routes' mode = Routes'
  { tracesGet :: mode :- "traces" :> Capture "trace_id" Text :> QPT "span_id" :> Get '[HTML] (RespHeaders Trace.TraceDetailsGet)
  }
  deriving stock (Generic)


server :: Projects.ProjectId -> Servant.ServerT Routes ATAuthCtx
server pid =
  Routes'
    { tracesGet = Trace.traceH pid
    }
