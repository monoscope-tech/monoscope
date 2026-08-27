# gRPC payload capture in the SDKs

Every Monoscope SDK today is an HTTP middleware. A service that speaks gRPC therefore gets
no request/response payloads at all — which is most of a typical microservice estate, and all
but two services in our own OpenTelemetry demo.

The trigger for this work: instrumenting the demo's `payment` service required hand-writing a
gRPC interceptor inside the demo repo. That code worked, but it is the wrong home — every user
with a gRPC service would have to write the same thing, and each copy would drift from the
contract independently. **The capability belongs in the SDKs.**

## The contract being reproduced

Server-side (`OtlpServer.hs` `isOurSdkSpan`, `Telemetry.hs`), payloads are lifted into the
`body` column — which is what the Req/Resp Body tabs read — only when:

- the span is **named** `monoscope.http` (or legacy `apitoolkit-http-span`), and
- `http.request.body` / `http.response.body` are **base64**.

Raw JSON on the ambient span is searchable but leaves the tabs empty, which reads as the
feature being broken rather than as instrumentation that missed the contract.

### The contract is HTTP-shaped, and that is a real cost

A gRPC call has no status line, no URL, and no method in the HTTP sense, so an interceptor has
to describe itself in HTTP terms: `POST`, the RPC path as `http.route`, and 200/500 synthesised
from whether the handler errored. That mapping is lossy — a gRPC status code carries more than
"ok or not" (`NOT_FOUND`, `PERMISSION_DENIED`, `RESOURCE_EXHAUSTED`, …).

**Resolution: emit both, now.** Span attributes are additive, so every interceptor sets the
HTTP-shaped fields the lift requires *and* the honest ones alongside — `rpc.system`,
`rpc.method`, `rpc.grpc.status_code`. The server ignores what it does not read today, and if it
later learns to render gRPC natively, no SDK needs re-releasing to feed it.

Teaching the server and UI to render gRPC natively (filter by `NOT_FOUND` rather than by a
synthesised 500) is worth doing, but it is server work and independent of this. It is not a
prerequisite, and treating it as one would block every language behind it.

## Per-language tasks

Ordered by how much gRPC is actually used in that ecosystem. The shared core in each language
already does base64 + redaction (`setAttributes` in JS, the equivalent in the others), so each
interceptor should be thin — if one is getting large, it is duplicating the core.

### Tier 1 — gRPC is common here

- [ ] **Go** (`monoscope-go`) — no gRPC support at all today, and Go is the language where
      gRPC is most likely to be a service's *only* protocol. Ship both a
      `grpc.UnaryServerInterceptor` and a `grpc.UnaryClientInterceptor`, plus a note on
      streaming (see below). Highest value of the set.
- [x] **Node** (`monoscope-js`, `packages/common`) — **done, published in 1.3.1** as
      `observeGrpc`. Needs no gRPC dependency: a unary handler is just `(call, callback)`, so
      wrapping one requires nothing beyond the OpenTelemetry API the package already uses.
      Routes through `setAttributes`, so redaction comes from the caller's own JSONPath
      config. Deployed in the demo's `payment` service.

      Three things that turned out to matter, and will matter in every other language:
      protobuf decodes int64 as a `Long` (`{low, high, unsigned}`), which JSON.stringify
      renders verbatim — that defeats readability *and* any JSONPath rule written against the
      expected value; a handler that throws synchronously never reaches its callback, so
      without a guard the span leaks and the caller hangs; and capture must be best-effort so
      an unserialisable message degrades to no body rather than to an exception on the request
      path.
- [ ] **Java** (`apitoolkit-java` / `apitoolkit-springboot`) — a `ServerInterceptor`. gRPC is
      heavily used in Java shops and the SDK is already a filter-style integration, so the
      shape is familiar.
- [ ] **.NET** (`apitoolkit-dotnet`) — an `Interceptor` subclass; ASP.NET Core hosts gRPC
      first-class, so this is a natural fit.

### Tier 2 — gRPC exists but is less common

- [ ] **Python** (`monoscope-python`, `common`) — `grpc.ServerInterceptor`. Note the sync and
      `grpc.aio` APIs differ; cover both or state plainly which is supported.
- [ ] **Elixir** (`apitoolkit-phoenix`) — only if a user asks. gRPC in Elixir usually means
      the `grpc` hex package, not Phoenix, so this may want its own package rather than
      living in the Phoenix SDK.

### Tier 3 — probably not worth it

- [ ] **PHP** (`monoscope-slim`, `monoscope-laravel`, `apitoolkit-symfony`) — PHP gRPC
      *servers* are rare; the ext-grpc client is the common case. Consider client-side
      interception only, and only on request.

## Cross-cutting, decide once and apply everywhere

- [ ] **Streaming RPCs.** Unary maps cleanly onto request/response. Client-, server-, and
      bidi-streaming do not — there is no single request body. Decide: skip streaming
      entirely (capture metadata only), or capture first-N messages with an explicit cap.
      Whatever is chosen, it must be the same in every language, and **documented rather than
      silently doing nothing** — a user whose streaming RPCs show no bodies must be able to
      find out why.
- [ ] **Redaction must work on decoded protobuf objects, not JSON strings.** This is where the
      JS SDK has been bitten before: `redactFields` did `JSON.parse` on a value that was
      already an object, threw, and returned it **unredacted**. A gRPC request arrives already
      decoded in every language, so it hits exactly that path. Each implementation needs a test
      that a sensitive field in a decoded message is redacted — the demo's `payment` service
      passes a full credit card through this path.
- [ ] **Body size cap.** No SDK has one today. A gRPC message can be megabytes, and it lands
      base64-encoded on a span attribute. Pick a limit, truncate with a marker.
- [ ] **Docs.** Each SDK's page under `monoscope.tech/docs/sdks/...` needs a gRPC section, and
      the `instrument` skill's detection table needs a gRPC row — it currently routes every
      detected framework to an HTTP middleware.

## Verification bar for each language

Not "it compiles". For each SDK, before calling it done:

1. A real gRPC call through the interceptor returns its response **unchanged** — capture is
   never allowed to alter or fail the RPC.
2. The span reaches a collector named `monoscope.http`, with `http.request.body` /
   `http.response.body` that **base64-decode to the expected JSON**.
3. A sensitive field in the decoded request is `[REDACTED]` in that decoded body.
4. The error path is captured too, with the RPC's error surfaced rather than swallowed.

The demo's `payment` service is a ready-made end-to-end test bed for the Node one: it is on
the checkout path, it carries a credit card, and it exercises both the success and error paths
under continuous load.

**One process lesson from building the reference implementation, worth applying to each SDK's
own demo integration:** the module was unit-tested and correct, and the deployment still
crash-looped, because the service's Dockerfile copies source files by name and the new file was
not listed. Unit tests cannot catch that. **Boot the built image before deploying it.**
