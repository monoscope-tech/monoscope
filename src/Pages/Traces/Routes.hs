module Pages.Traces.Routes (Routes, Routes' (..), server) where

import Lucid (Html)
import Models.Projects.Projects qualified as Projects
import Pages.Traces.Spans qualified as Spans
import Pages.Traces.Trace qualified as Trace
import Relude (Generic, Text, Type)
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


type Routes' :: forall {k}. k -> Type
data Routes' mode = Routes'
  { tracesGet :: mode :- "traces" :> Capture "trace_id" Text :> QPT "span_id" :> QPT "nav" :> Get '[HTML] (RespHeaders Trace.TraceDetailsGet)
  , spanGetH :: mode :- "spans" :> Capture "trace_id" Text :> Capture "span_id" Text :> Get '[HTML] (RespHeaders (Html ()))
  }
  deriving stock (Generic)


server :: Projects.ProjectId -> Servant.ServerT Routes ATAuthCtx
server pid =
  Routes'
    { tracesGet = Trace.traceH pid
    , spanGetH = Spans.spanGetH pid
    }
