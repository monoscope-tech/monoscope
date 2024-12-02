module Pages.Telemetry.Routes (Routes, Routes' (..), server) where

import Lucid (Html)
import Models.Projects.Projects qualified as Projects
import Pages.Telemetry.Metrics
import Pages.Telemetry.Spans qualified as Spans
import Pages.Telemetry.Trace qualified as Trace
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
import Servant.Htmx (HXBoosted)
import System.Types (ATAuthCtx, RespHeaders)


type QPT a = QueryParam a Text


type role Routes' nominal


type Routes = NamedRoutes Routes'


data Routes' mode = Routes'
  { tracesGet :: mode :- "traces" :> Capture "trace_id" Text :> QPT "span_id" :> QPT "nav" :> Get '[HTML] (RespHeaders Trace.TraceDetailsGet)
  , spanGetH :: mode :- "spans" :> Capture "trace_id" Text :> Capture "span_id" Text :> Get '[HTML] (RespHeaders (Html ()))
  , metricsOVGetH :: mode :- "metrics" :> QPT "tab" :> QPT "from" :> QPT "to" :> QPT "since" :> QPT "metric_source" :> QPT "metric_prefix" :> Get '[HTML] (RespHeaders MetricsOverViewGet)
  }
  deriving stock (Generic)


server :: Projects.ProjectId -> Servant.ServerT Routes ATAuthCtx
server pid =
  Routes'
    { tracesGet = Trace.traceH pid
    , spanGetH = Spans.spanGetH pid
    , metricsOVGetH = metricsOverViewGetH pid
    }
