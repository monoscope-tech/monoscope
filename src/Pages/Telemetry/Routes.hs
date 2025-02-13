module Pages.Telemetry.Routes (Routes, Routes' (..), server) where

import Lucid (Html)
import Models.Projects.Projects qualified as Projects
import Pages.Telemetry.Metrics qualified as Metrics
import Pages.Telemetry.Spans qualified as Spans
import Pages.Telemetry.Trace qualified as Trace
import Relude (Generic, Int, Text)
import Servant (
  Capture,
  GenericMode (type (:-)),
  Get,
  HasServer (ServerT),
  NamedRoutes,
  QueryParam,
  type (:>),
 )

import Pkg.RouteUtils (GetRedirect, QPI, QPT)
import Relude
import Servant.HTML.Lucid (HTML)
import System.Types (ATAuthCtx, RespHeaders)


type role Routes' nominal


type Routes = NamedRoutes Routes'


type Routes' :: Type -> Type
data Routes' mode = Routes'
  { tracesGet :: mode :- "traces" :> Capture "trace_id" Text :> QPT "span_id" :> QPT "nav" :> Get '[HTML] (RespHeaders Trace.TraceDetailsGet)
  , spanGetH :: mode :- "spans" :> Capture "trace_id" Text :> Capture "span_id" Text :> Get '[HTML] (RespHeaders (Html ()))
  , metricsOVGetH :: mode :- "metrics" :> QPT "tab" :> QPT "from" :> QPT "to" :> QPT "since" :> QPT "metric_source" :> QPT "metric_prefix" :> QPI "cursor" :> Get '[HTML] (RespHeaders Metrics.MetricsOverViewGet)
  , metricDetailsGetH :: mode :- "metrics" :> "details" :> Capture "metric_name" Text :> QPT "from" :> QPT "to" :> QPT "since" :> QPT "metric_source" :> Get '[HTML] (RespHeaders (Html ()))
  , metricBreakdownGetH :: mode :- "metrics" :> "details" :> Capture "metric_name" Text :> "breakdown" :> QPT "label" :> Get '[HTML] (RespHeaders (Html ()))
  }
  deriving stock (Generic)


server :: Projects.ProjectId -> Servant.ServerT Routes ATAuthCtx
server pid =
  Routes'
    { tracesGet = Trace.traceH pid
    , spanGetH = Spans.spanGetH pid
    , metricsOVGetH = Metrics.metricsOverViewGetH pid
    , metricDetailsGetH = Metrics.metricDetailsGetH pid
    , metricBreakdownGetH = Metrics.metricBreakdownGetH pid
    }
