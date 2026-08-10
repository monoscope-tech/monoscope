# 04 — Errors in source context (Sentry / Datadog parity)

## How the two references actually work

**Sentry** has three separate mechanisms, and they are worth separating because people
conflate them:

1. **SDK-attached source context.** For interpreted runtimes the SDK reads the source file
   off disk at throw time and puts `pre_context` / `context_line` / `post_context` (±5
   lines) straight on each stack frame in the event. Nothing server-side is needed. This is
   how most Python/Ruby/Node source context is produced.
2. **Uploaded artifacts.** `sentry-cli` uploads source maps (JS) or debug files with
   `--include-sources` (native), keyed by release/dist or debug-id. Server-side symbolication
   maps a minified/stripped frame back to a source file *and* carries the source text.
3. **Stack-trace linking / code mappings.** A *code mapping* pairs a stack-frame path prefix
   with `(repo, branch, source root)`. Given a frame, Sentry rewrites the path and fetches
   the blob from the SCM provider's API, then renders the surrounding lines.

**Datadog** only really does (3), under the name **Source Code Integration**: you tag
telemetry with `git.commit.sha` + `git.repository_url` (`datadog-ci git-metadata upload`
uploads the git *tree*, not the sources), install the GitHub App, and Datadog fetches the
blob at that exact commit to render the snippet inline in the error panel and flame graph.

The common shape underneath all three is one function:

```
resolve :: Frame -> Maybe SourceSnippet     -- (path, line) at a known revision -> lines around it
```

Everything else is which backend answers it.

## What monoscope already has

- Exception payloads carry `attributes___exception___stacktrace` and
  `attributes___code___{file___path, function___name, line___number}` — the OTel semantic
  convention fields are **already promoted columns** (`Telemetry.hs:1725-1730`). So for any
  SDK that follows the convention we already know the file and line without parsing anything.
- Stack traces render today as one opaque `<pre>` blob:
  `Pages/LogExplorer/LogItem.hs:480` (`renderErrors`) and
  `Pages/Anomalies.hs:588` (`RuntimeException` issue view).
- **A working GitHub integration exists**: `Models.Projects.GitSync` has the App JWT flow,
  installation tokens, repo listing, and — crucially — `fetchFileContent :: Text ->
  GitHubSync -> Text -> Eff es (Either Text ByteString)`.

That last point decides the approach. The Datadog model (mechanism 3) is *most* of the way
built already; "users upload codebases" (mechanism 2) means building storage, retention,
versioning and a CLI uploader from scratch for a strictly worse result — the snippet would
be from whatever was uploaded, not from the revision that actually threw.

## Design

**Ship mechanism 3, structured so mechanism 1 wins for free where the SDK provides it.**

### Layer 1 — Frames

`Pkg.StackTrace`: parse a raw stack-trace blob into `[Frame]`
(`function`, `file`, `line`, `inApp`), plus the trivial case where the OTel `code.*`
attributes already give a single frame. One parser per family, chosen by shape:

- `at fn (file:line:col)` — JS/TS/Node
- `File "file", line N, in fn` — Python
- `file:line:in 'fn'` — Ruby
- `\tat pkg.Class.method(File.java:N)` — Java/Kotlin
- `\tfile:line +0x…` — Go
- `#N file(line): fn()` — PHP

Pure, total, and doctested per family — this is exactly the isolated-pure-function case
doctests are for. Unparseable lines survive as `Frame`s with no file/line so the rendered
trace never loses text.

### Layer 2 — Resolution

`Models.Projects.CodeContext`: a **code mapping** per project —
`(service, path_prefix) -> (github_sync_id, source_root, branch)`, the Sentry code-mapping
model. Resolution order for a frame:

1. Snippet already on the frame (SDK-attached) → use it, fetch nothing.
2. Revision: the span's `resource.service.version` if it looks like a sha, else the
   `vcs.repository.ref.revision` / `git.commit.sha` attribute, else the mapping's branch.
   Datadog's rule, in that order.
3. Longest-prefix-matching mapping for the frame's file path → rewrite path → GitHub blob.

Cached, because a 30-frame trace on a hot issue would otherwise be 30 GitHub calls per view.

### Layer 3 — Rendering

Replace the `<pre>` blob in `renderErrors` with a frame list: collapsed by default except
the first in-app frame (Sentry's default, and the right one during an incident). An
expanded frame shows ±5 lines with the failing line marked by **both** a background and a
gutter marker — colour is never the only signal here.

Rendered in Lucid; the snippet arrives by HTMX into a per-frame target, since fetching it is
a network call we must not block the error panel on.

### Settings

A "Source code" settings page: pick a repo from the existing GitHub App installation, add
mappings, and a "test a path" box that shows what a given frame path resolves to. Reuses the
GitSync install flow wholesale — no second OAuth app.

## Plan

1. `Pkg.StackTrace` + doctests per family.
2. Migration: `projects.code_mappings`.
3. `Models.Projects.CodeContext` — CRUD + `resolveFrame`, with the cache.
4. Frame-list renderer, shared by `LogItem.renderErrors` and the `RuntimeException` issue
   view (one renderer, both call sites — no fork).
5. HTMX endpoint for a single frame's snippet.
6. Settings page + mapping editor.
7. Integration test with a golden GitHub response (`tests/golden/`, `UPDATE_GOLDEN=true`).

## Explicitly out of scope for this pass

Source-map/debug-file upload and symbolication (Sentry mechanism 2). It is a genuinely
separate subsystem — artifact storage, debug-id indexing, a CLI uploader — and it is only
needed for minified or compiled frames. The `resolve` seam above is where it plugs in when
it comes.
