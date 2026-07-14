# Product

## Register

product

## Platform

web

## Users

Primary users are developers, SREs, and on-call engineers investigating production incidents — debugging traces, reading logs, and understanding system behavior. Their defining context is high-stakes and time-pressured: a 3 AM page, a shared trace link, a customer-impacting error clock ticking. A secondary, exploratory context also exists (dashboards, endpoint discovery, browsing what a service does) but it is not the moment the interface is tuned for.

A smaller secondary audience — PMs and support — opens shared links without deep technical fluency. Shared views must stay readable for them, but the power user on call wins every conflict.

## Product Purpose

Monoscope is an open-source, OpenTelemetry-native observability platform: it ingests logs, traces, and metrics and gives engineers the cross-linked context to get from an alert to a root cause fast. It exists so an on-call engineer can land on a shared link mid-incident and reach understanding — relief, not more noise. Success is time-to-root-cause: the waterfall, the correlated logs, and the exception are one click apart, not buried.

## Positioning

Full OpenTelemetry-native observability that we run on ourselves. Monoscope dogfoods its own telemetry, so every signal we surface is one we've debugged an incident with — Datadog-grade cross-linked context and density, open-source, without the glossy sprawl.

## Brand Personality

Precise, trustworthy, calm. Confident without theatrics; dense where density earns its keep. Never breezy, never decorative, never patronizing. The emotional goal during an incident is relief — the feeling of the system finally telling you what happened — not excitement.

## Anti-references

No playful illustrations or mascots. No decorative icons above headings. No neon-on-dark. No glassmorphism. No gradient text. No cartoony empty states. Reference point is Datadog APM — info-rich, color carries meaning, cross-linked context — but without Datadog's glossier edges.

## Design Principles

Color is signal, not decoration. Service colors identify, red means error, green means success, brand blue means clickable. No meaning, no color.

Density is respect. Experienced users scan; pack the waterfall. Vary spacing intentionally — tight within rows, generous between logical groups.

Progressive disclosure earns its keep during incidents. Summary counts up front, detail one click away. Never bury the exception type two clicks deep in a 29-error trace.

Shared views are first-class. A share link may be opened by someone without an account, on a phone, during a panic — it must read standalone.

Practice what we preach. We dogfood our own product, so instrumentation and interface are held to the bar we sell.

## Accessibility & Inclusion

WCAG AA is the floor. Every error state needs a second signal beyond color (shape, position, text, or icon). Dark-mode parity is required, not an afterthought — every semantic token is verified in both themes. Reduced-motion alternatives are mandatory for every animation.
