# Endpoint-to-RUM correlation contract

Endpoint Analytics can make a browser claim only when all of these identifiers are
present and agree within one project:

1. The browser request span has the endpoint's canonical `hashes` value. This is
   the same stable endpoint hash used by the matching server span, so it scopes the
   request to one endpoint without guessing from a URL.
2. Browser request/page/error spans carry a non-empty `attributes.session.id`.
   It is the join key for the endpoint-session table and affected-session cohort.
3. A replay is offered only when `projects.replay_sessions` has that exact session
   id and indexed recording files. Receiving a replay event alone is not enough.
4. A request-linked Web Vital additionally carries the same session key in metric
   attributes (`session.id` or `session_id`). Page-only vital samples remain on the
   RUM dashboard, where page attribution is truthful but endpoint attribution is not.

Absent keys are an instrumentation gap, not evidence of zero user impact. The
endpoint dashboard therefore leaves the request-linked vital table empty and explains
the required attribute instead of assigning page-level values to an endpoint.

Browser cohorts deliberately normalize the observed user-agent into browser families
(Edge, Opera, Firefox, Chrome, Safari, Other, and Unknown). They use the same
endpoint-hash scope as the session table. `Unknown browser` means the SDK supplied no
user-agent; it does not mean an additional user cohort. This avoids both a misleading
raw-user-agent cardinality explosion and invented geography/device attributes.

This matches the investigation model used by Sentry and New Relic: a replay/session
is linked through a trace/session identity, while a metric without that identity stays
at its broader page or application scope.
