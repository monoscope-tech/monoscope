# 07 — Git integration beyond GitHub

Support **GitLab** (SaaS and self-hosted), **Gitea/Forgejo** (self-hosted), and **Bitbucket
Cloud** alongside GitHub, for both features that talk to a git host: source-code context in
stack traces (read) and dashboard/monitor YAML sync (read + write + webhooks).

This plan preserves existing GitHub connections and webhook URLs. It adds token-based
connections for the other hosts. OAuth and automatic webhook creation stay out of scope.

## Implementation status (2026-08-12)

All eight work-breakdown items are implemented and the tree is green: 1017 doctests pass,
`Pages.GitSync` 24 examples / `Pages.CodeContext` 7 examples pass, and the full suite is 673
examples with 9 failures whose names match the pre-existing baseline exactly (CLI lifecycle x2,
LogExplorer x3, QueryCache x3, ServiceMap x1). `make lint` reports 11 hints, the same count as
`master` — no new ones. `hs-deep-clean` ran over all five files (50 fixes) and found one real
defect on the way: `getInstallationToken` left `postWith` unwrapped, so an `HttpException`
escaped its `Either Text` signature and surfaced as a 500 instead of a rendered error.

**All hosts ship enabled.** They were briefly behind `ENABLE_NON_GITHUB_GIT_HOSTS`, since
GitLab/Gitea/Bitbucket are covered by doctests and fixtures but have never run against those
vendors' live APIs. The flag is gone: one more env var to carry and explain cost more than the
staged rollout bought, and a host that is never offered is a host nobody reports a bug in.
GitLab, Gitea and Bitbucket are offered in the picker and accepted by the write path.

First contact with a real vendor API is therefore a user's, not ours — if a host's request
paths turn out to be wrong, the failure is a rendered connection error on the settings page,
which is the same surface a bad token already produces.

### Still open

- **Mock-server HTTP contract tests** (pagination, redirects, rate limits, malformed JSON,
  non-2xx, page-limit failure) — not written. This is the main gap and the reason the flag is
  off.
- **Live smoke test per host** — needs accounts and tokens on three hosts.
- **Metrics** (delivery step 4: API errors, webhook verification failures, pagination limits,
  sync duration, labelled by host) — not added.
- **`0126_git_rename_compat_views.sql` is scaffolding.** It re-exposes `projects.github_sync`
  and `projects.github_credentials` as auto-updatable views so a release still running the old
  code keeps working across the rename. Drop it once no instance runs pre-0125 code.
- **Migration number collision.** Prod's `schema_migrations` already contains a
  `0125_issue_ack_window.sql` from another branch that is not in this repo's history. Both are
  applied and the runner keys on filename, so nothing is broken — but master will end up with
  two `0125_*` files when that branch merges. Renumbering ours is **not** safe: it is already
  recorded in prod under this name, and re-running it would fail on the already-performed
  `ALTER TABLE ... RENAME`.


## What exists today

There is **no** multi-host config to build on. `git_provider`, `gitlab`, `bitbucket` and
`gitea` appear nowhere in `src/`, `shared/`, `static/migrations/` or `web-components/`. The
integration is GitHub all the way down:

- Tables are named for it — `projects.github_sync`, `projects.github_credentials`.
- Auth is the GitHub App flow specifically — `generateAppJWT` → `getInstallationToken`.
- Every URL is a literal `https://api.github.com/...` inside `Models.Projects.GitSync`.

Six operations are all that callers need. They already pass through few enough
places that this is tractable:

| Operation | Function today | Callers |
|---|---|---|
| read tree | `fetchGitTree` | `BackgroundJobs.withGitHubSync`, `Pages.CodeContext.deriveFromRepo` |
| read file | `fetchFileContent` | `CodeContext.fetchSnippet`, `BackgroundJobs.fetchAndParseDashboard` |
| write file | `pushFileToGit` | `BackgroundJobs.pushDashboardToGit` |
| list repos | `listInstallationRepos` | both settings pickers |
| default branch | `detectDefaultBranch` | `Pages.GitSync.gitSyncSettingsPostH` |
| verify + parse webhook | `validateWebhookSignature`, `GitHubWebhookPayload` | `Pages.GitSync.githubWebhookPostH` |

Critically, `BackgroundJobs.withGitHubSync` already resolves a `token :: Text` once and hands
it to the three API calls. Replacing that `Text` with a connection value is most of the
background-job change.

## The one abstraction

```haskell
data GitHost = GitHub | GitLab | Bitbucket | Gitea

-- | Everything that one API call needs. Do not derive Show because this value contains a token.
data GitConn = GitConn { host :: GitHost, apiBase :: BaseUrl, token :: Sensitive Text }

mkGitConn :: GitHost -> Maybe Text -> Text -> Either Text GitConn
```

`mkGitConn` is the validation boundary. It normalizes the optional origin and derives
`apiBase`. Gitea without an origin returns `Left`. Bitbucket rejects an origin because this
plan supports Bitbucket Cloud only. Reject origins with user information, query strings, or
fragments. Accept HTTPS only, except for an explicit development-only HTTP policy.

| Host | origin `Nothing` | origin `Just o` |
|---|---|---|
| GitHub | `https://api.github.com` | `{o}/api/v3` (Enterprise Server) |
| GitLab | `https://gitlab.com/api/v4` | `{o}/api/v4` |
| Bitbucket | `https://api.bitbucket.org/2.0` | **`Left`** — self-hosted is out of scope |
| Gitea | **`Left`** | `{o}/api/v1` |

The new `src/Pkg/Git.hs` module contains the host abstraction and all six operations. It is a leaf:
`W.HTTP` and `Pkg.DeriveUtils` only, no DB, no `Dashboards`, so it cannot join the
`GitSync → Widget → LogItem` cycle that forced `Pages.CodeContext` to exist as its own module.

Self-hosted origins create an SSRF boundary. The implementation must apply the product's
outbound-request policy before it sends a request. It must also resolve redirects under the
same policy. Do not log tokens or origin user information.

`RepoRef` and `TreeEntry` move here from `Models.Projects.GitSync`, which re-exports them so
call sites don't churn.

## Per-host operation matrix

Checked against current vendor documentation on 2026-08-11. Recheck these contracts during
implementation because SaaS APIs can change.

**Auth header**

| Host | Header |
|---|---|
| GitHub | `Authorization: Bearer <t>` |
| Gitea | `Authorization: token <t>` |
| GitLab | `PRIVATE-TOKEN: <t>` |
| Bitbucket | `Authorization: Bearer <t>` |

**Repo path segment** — the piece that identifies a repository in a URL:

| Host | Segment |
|---|---|
| GitHub / Gitea | `repos/{owner}/{repo}` |
| GitLab | `projects/{urlencoded owner/repo}` — the namespace nests, so `acme/team/api` → `acme%2Fteam%2Fapi` |
| Bitbucket | `repositories/{workspace}/{repo}` |

**Read tree**

| Host | Request | Shape |
|---|---|---|
| GitHub | `GET {seg}/git/trees/{ref}?recursive=1` | `{sha, tree:[{path,type,sha,size}]}` |
| Gitea | same, `?recursive=true&per_page=1000` | same (Gitea caps at `DEFAULT_GIT_TREES_PER_PAGE = 1000`) |
| GitLab | `GET {seg}/repository/tree?ref&recursive=true&per_page=100` | `[{id,name,type,path}]` — `id` is the git object SHA |
| Bitbucket | `GET {seg}/src/{ref}/?max_depth=100&pagelen=100` | `{values:[{path,type:commit_file,size}]}` — **no per-file SHA** |

**Read file**

| Host | Request | Body |
|---|---|---|
| GitHub / Gitea | `GET {seg}/contents/{path}?ref={ref}` | base64 in `.content` |
| GitLab | `GET {seg}/repository/files/{urlencoded path}/raw?ref={ref}` | raw bytes |
| Bitbucket | `GET {seg}/src/{ref}/{path}` | raw bytes |

**Write file**

| Host | Request | Response |
|---|---|---|
| GitHub / Gitea | `PUT {seg}/contents/{path}`, JSON `{message,content,branch,sha?}` | `content.sha` and a commit identifier |
| GitLab | `POST` (create) / `PUT` (update) `{seg}/repository/files/{urlencoded path}`, JSON `{branch,content,encoding,commit_message}` | `{file_path, branch}` only |
| Bitbucket | `POST {seg}/src`, form-urlencoded: field name = file path, plus `message`, `branch` | 201, **empty body** |

**List repos**

| Host | Request |
|---|---|
| GitHub | `GET /installation/repositories` → `.repositories` |
| Gitea | `GET /user/repos?limit=50` |
| GitLab | `GET /projects?membership=true&simple=true&per_page=100` → `path_with_namespace`, `default_branch` |
| Bitbucket | `GET /repositories?role=member&pagelen=100` → `.values`, `full_name`, `mainbranch.name` |

All list and tree operations must consume every page. GitHub uses `Link`; GitLab uses its
pagination headers; Gitea uses page parameters; Bitbucket returns `next`. Add a maximum-page
guard and return an error if the host exceeds it. Never return a silently truncated list.

Percent-encode path segments and query values with URI builders. Do not construct URLs by
concatenating raw owner, repository, revision, or file-path text.

**Webhooks**

| Host | Event header | Push value | Verification |
|---|---|---|---|
| GitHub | `X-GitHub-Event` | `push` | `X-Hub-Signature-256: sha256=<hex>`, HMAC-SHA256 over the raw body |
| Gitea | `X-Gitea-Event` | `push` | `X-Gitea-Signature: <hex>`, bare, HMAC-SHA256 |
| Bitbucket | `X-Event-Key` | `repo:push` | `X-Hub-Signature: sha256=<hex>`, HMAC-SHA256 over the raw body |
| GitLab 19.1+ | `X-Gitlab-Event` | `Push Hook` | `webhook-signature: v1,<base64>`; HMAC-SHA256 over `webhook-id.webhook-timestamp.body` |

For GitLab signing tokens, remove `whsec_` and base64-decode the remaining key. Accept any
space-separated `v1` signature with a constant-time comparison. Also enforce a small clock
skew for `webhook-timestamp` to reduce replay risk. Self-hosted GitLab versions before 19.1
can use the legacy plaintext `X-Gitlab-Token`; label this fallback as weaker in the UI.

All four payloads carry the repository's full name, though at different keys —
`repository.full_name` for GitHub/Gitea/Bitbucket, `project.path_with_namespace` for GitLab —
so one `parseWebhookRepo` returns `owner/repo` and the sync lookup splits on the **last** `/`.
First parse only the repository identity needed to find the row and its secret. Then verify
the signature. Parse the complete event and queue a job only after verification. The route's
host and the stored row's host must match.

## Where the hosts genuinely disagree

Four real problems, not just URL differences. These are the parts worth reviewing.

**1. GitLab and Bitbucket return no blob SHA when writing.** `pushDashboardToGit` stores the
blob SHA as `dashboards.file_sha`. `buildSyncPlan` uses it to select create, update, and
rename actions. `computeContentSha` already computes a Git blob SHA-1:
`sha1("blob <len>\0<content>")`. Use it for GitLab and Bitbucket after a write.

The second return value needs a clear contract. GitLab tree responses have no tree SHA, and
Bitbucket directory responses have no equivalent field. Rename `last_tree_sha` to
`last_revision`. Make `fetchTree` and `pushFile` return the resolved head commit ID. Resolve
the branch after each listing or write. A later pull can skip work only when that commit ID
is unchanged. Keep blob SHAs separate from this revision marker.

**2. Bitbucket's listing has no per-file SHA.** Its `src` entries carry `path`, `type`
and `size` only; the `commit.hash` is the commit you asked for, not the one that last touched
each file. So change detection on the pull side has nothing to compare. Fix: `fetchTree` takes
the path prefix the caller needs. For Bitbucket, it backfills SHAs by fetching
content and calling `computeContentSha` for the blobs under that prefix. Sync always passes a
non-empty prefix (`getDashboardsPath` returns at least `dashboards/`), so this is bounded to
the dashboard files — not the repository. Passing the prefix also lets GitLab and Bitbucket
scope the listing server-side, which is a win on every host.

Do not represent a missing SHA as an empty string. Change `TreeEntry.sha` to `Maybe Text`.
Dashboard sync requires `Just sha`; mapping derivation only uses paths. This type makes the
incomplete Bitbucket entries explicit.

**3. Bitbucket has no create/update distinction; GitLab requires one.** GitHub/Gitea
take an optional `sha` on one `PUT`. GitLab needs `POST` for a new file and `PUT` for an
existing one — which maps cleanly onto the `Maybe Text` existing-sha argument the caller
already passes. Bitbucket's `POST /src` is upsert.

GitLab's optional `last_commit_id` is a commit ID, not a blob SHA. Do not pass the existing
blob SHA in that field. Either fetch file metadata and use a typed write precondition, or
omit `last_commit_id` and document weaker concurrency protection. Prefer the typed option.

**4. Webhook verification has four schemes.** Use one `verifyWebhook` function with one arm
per host. The GitLab arm also needs the webhook ID and timestamp. Every comparison stays
constant-time with `BA.constEq`. If a row has a secret, reject a missing or invalid
signature. Require a webhook secret for new non-GitHub connections. Preserve unsigned
GitHub behavior only for existing rows, with a warning.

**The GitHub App does not carry over.** The other hosts do not use the GitHub App flow. They
use a GitLab project, group, or personal token; a Gitea token; or a Bitbucket access token.
Two consequences follow:

- `git_credentials.installation_id` stays GitHub-only; the others use `access_token`.
- The **"Repository missing? Add it to the installation on GitHub"** link and the repo picker
  behave differently: on the token hosts, the repo list is whatever the token's scope reaches,
  and widening it means reissuing the token. The Source Code page must say so per host rather
  than showing a GitHub-shaped instruction to a GitLab user.
- A repository-scoped token cannot always list sibling repositories. Keep manual
  owner/repository entry available when discovery returns only one repository or no list.

## Schema — migration `0125_git_hosts.sql`

```sql
ALTER TABLE projects.github_sync        RENAME TO git_sync;
ALTER TABLE projects.github_credentials RENAME TO git_credentials;

ALTER TABLE projects.git_sync
    ADD COLUMN IF NOT EXISTS host TEXT NOT NULL DEFAULT 'github',
    ADD COLUMN IF NOT EXISTS api_base TEXT;
ALTER TABLE projects.git_credentials
    ADD COLUMN IF NOT EXISTS host TEXT NOT NULL DEFAULT 'github',
    ADD COLUMN IF NOT EXISTS api_base TEXT;

-- Drop the old auth checks before adding the host-aware checks. PostgreSQL keeps
-- constraint names after a table rename.
ALTER TABLE projects.git_sync DROP CONSTRAINT IF EXISTS github_sync_auth_check;
ALTER TABLE projects.git_credentials DROP CONSTRAINT IF EXISTS github_credentials_auth_check;

ALTER TABLE projects.git_sync
    ADD CONSTRAINT git_sync_host_check
        CHECK (host IN ('github', 'gitlab', 'bitbucket', 'gitea')),
    ADD CONSTRAINT git_sync_auth_check
        CHECK (
            (host = 'github' AND (installation_id IS NOT NULL OR access_token IS NOT NULL))
            OR (host <> 'github' AND installation_id IS NULL AND access_token IS NOT NULL)
        );

ALTER TABLE projects.git_credentials
    ADD CONSTRAINT git_credentials_host_check
        CHECK (host IN ('github', 'gitlab', 'bitbucket', 'gitea')),
    ADD CONSTRAINT git_credentials_auth_check
        CHECK (
            (host = 'github' AND (installation_id IS NOT NULL OR access_token IS NOT NULL))
            OR (host <> 'github' AND installation_id IS NULL AND access_token IS NOT NULL)
        );

-- `acme` on GitHub and `acme` on GitLab are different grants; before this a project
-- could hold only one of them.
ALTER TABLE projects.git_credentials DROP CONSTRAINT IF EXISTS github_credentials_project_id_account_key;
ALTER TABLE projects.git_credentials ADD CONSTRAINT git_credentials_project_id_host_account_key
    UNIQUE (project_id, host, account);

-- Webhooks resolve repo → sync row by name; two hosts can both have an `acme/config`.
CREATE INDEX IF NOT EXISTS git_sync_host_repo_idx ON projects.git_sync (host, owner, repo);

ALTER TABLE projects.git_sync RENAME COLUMN last_tree_sha TO last_revision;
```

Use one transaction, as migration 0124 does. Rename the remaining GitHub-named indexes and
constraints for operational clarity. Foreign-key targets follow a PostgreSQL table rename.
The application must update every raw SQL query and entity table name in the same release.

Decide whether the stored URL is an origin or an API base. This plan stores `api_base`. The
form accepts an origin and normalizes it before insertion. Reads must not append `/api/v4`
or `/api/v1` again. Apply full URL and SSRF validation in `mkGitConn`.

The table names must describe all supported hosts. Migration 0125 performs the rename and
leaves migration 0124 unchanged.

`code_mappings` needs no change. It references a credential, and the credential now carries
the host.

## Work breakdown

| # | File | Change |
|---|---|---|
| 1 | `static/migrations/0125_git_hosts.sql` | new — above |
| 2 | `src/Pkg/Git.hs` | **new, the bulk.** Add host types, safe URL builders, paginated API operations, webhook parsing, and verification. Move `RepoRef`, `TreeEntry`, and `GitRepo` here. |
| 3 | `src/Models/Projects/GitSync.hs` | Add `host` and `apiBase` to both records. Update table names. Build `GitConn` after token resolution. Keep the GitHub App token flow. Remove host HTTP operations from this module. |
| 4 | `src/BackgroundJobs.hs` | `withGitHubSync` → `withGitSync`, resolving a `GitConn` instead of a `Text` token; three call sites take it |
| 5 | `src/Pages/GitSync.hs` | host picker in the not-connected view; token field per host; App button only for GitHub; `githubWebhookPostH` → host-aware `gitWebhookPostH` |
| 6 | `src/Pages/CodeContext.hs` | credential list shows the host; picker copy per host; `credentialRepos` via `Pkg.Git.listRepos`; the "add repositories" link becomes host-specific |
| 7 | `src/Web/Routes.hs` | Add `POST /webhook/git/:host`. Keep `/webhook/github` delegated to `GitHub`. Capture all required host headers. |
| 8 | `src/System/Config.hs` | unchanged — App config stays GitHub's |

`TreeEntry._teType :: Text` becomes `isBlob :: Bool` while it moves. Three hosts spell the
blob type differently (`blob`, `blob`, `commit_file`) and the string is compared in exactly
one place (`isDashboardFile`). Normalize at the parse boundary. Also change `sha :: Text` to
`sha :: Maybe Text`.

## Testing

- **Pure unit tests:** Cover `apiBaseFor`, URL encoding, nested GitLab groups, event names,
  payload parsing, and content SHA calculation. Include invalid origins and Bitbucket origins.
- **HTTP contract tests:** Use a local mock server and vendor response fixtures. Cover every
  operation and host. Cover pagination, redirects, rate-limit errors, empty repositories,
  malformed JSON, missing fields, non-2xx responses, and page-limit failures.
- **Webhook tests:** Use published signature vectors where vendors provide them. Cover valid,
  invalid, and missing signatures. For GitLab, cover multiple signatures, decoded `whsec_`
  keys, signed message construction, stale timestamps, and legacy tokens.
- **Database and page integration tests:** Round-trip each host and token. Confirm encrypted
  token storage, host-aware lookup, host-specific copy, and rejection of invalid auth states.
  Confirm that old GitHub rows and `/webhook/github` continue to work after migration.
- **Background-job integration tests:** Pull, create, update, rename, and delete a dashboard
  for each host through the mock server. Confirm revision-marker and blob-SHA behavior.
- **Integration, real network**: the existing `GitHub Sync E2E (Real API)` block is gated on
  `GH_TEST_PAT`. Add the same shape for `GL_TEST_PAT` / `GITEA_TEST_*` / `BB_TEST_*`, skipped
  by default. These tests need accounts and tokens on three hosts. Mock tests remain required
  because live tests do not cover failure responses reliably.

Record the current baseline when implementation starts. Compare failure names, not only
counts. A hard-coded baseline count in this plan will become stale.

## Delivery and rollback

1. Add migration and read compatibility in one release. Existing rows default to GitHub.
2. Keep the old GitHub webhook route and GitHub App flow active.
3. Hide new hosts behind one feature flag until mock contract tests and one live smoke test
   per host pass.
4. Emit structured metrics for API errors, webhook verification failures, pagination limits,
   and sync duration. Include the host, but never include a token or secret.
5. To roll back the feature, disable the flag. Do not reverse the table rename after new-host
   rows exist. Old application versions cannot read the renamed tables.

## Acceptance criteria

- Existing GitHub App and PAT connections work without reconnection.
- Each new host can list repositories, read source, pull YAML, and push YAML.
- A repository name collision across two hosts routes to the correct sync row.
- All collection endpoints consume pagination or return an explicit limit error.
- Invalid or replayed signed webhooks do not queue jobs.
- Tokens stay encrypted at rest and do not appear in logs or rendered HTML.
- Self-hosted origins pass URL and outbound-request policy checks.
- A concurrent GitLab edit does not get a blob SHA where a commit ID is required.

## Decisions taken

- **Token auth for the three new hosts, no OAuth apps.** Every one of them supports a
  user-created token that covers what we need. OAuth would mean registering three apps,
  three secrets in `EnvConfig`, and three callback flows — a much larger change for a
  marginally nicer first-run.
- **One public `Pkg.Git` API.** Keep shared types and behavior behind this module. If the
  implementation becomes large, put host adapters in internal modules and re-export only
  the common API.
- **Rename the tables now.** Cheaper during this change than as a follow-up.

## Open questions

1. **Self-hosted network policy:** Must GitLab/Gitea support private addresses, custom CAs,
   or proxies? The default is HTTPS with system trust and the standard outbound-request
   policy. Private ranges and self-signed certificates stay disabled until requirements exist.
2. **Bitbucket workspace vs user** — the credential's `account` maps to a workspace slug.
   Personal repositories under a user slug also work. Bitbucket projects are a grouping
   layer and stay out of scope unless repository discovery requires them.
3. **Credential user experience:** The current default permits independent credentials on
   both pages because source and configuration repositories can use different hosts. Confirm
   this choice during interface review.

## Out of scope

- Azure DevOps (you did not select it; its org/project/repo shape needs a third coordinate).
- Bitbucket Data Center / GitHub Enterprise Server *testing* — the code paths exist
  (`api/v3`, custom origin) but are unverified.
- Migrating an existing GitHub sync to another host in place; that is disconnect and reconnect.

## Vendor references

- [GitHub tree API](https://docs.github.com/en/rest/git/trees)
- [GitHub webhook verification](https://docs.github.com/en/webhooks/using-webhooks/validating-webhook-deliveries)
- [GitLab repository tree API](https://docs.gitlab.com/api/repositories/)
- [GitLab repository files API](https://docs.gitlab.com/api/repository_files/)
- [GitLab webhook verification](https://docs.gitlab.com/user/project/integrations/webhooks/)
- [Gitea webhook headers](https://docs.gitea.com/1.25/usage/repository/webhooks)
- [Bitbucket source API](https://developer.atlassian.com/cloud/bitbucket/rest/api-group-source/)
- [Bitbucket webhook verification](https://support.atlassian.com/bitbucket-cloud/docs/manage-webhooks/)
