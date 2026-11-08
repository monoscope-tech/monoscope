# Impact reviews v2: merge risk, widgets, triggers, proof

Status (2026-11-08): A, B and C-terminal are implemented and tested. Terminal proof is verified end to end in real VMs. Browser proof (C-browser) and D's `fetch_url`, registry proxy and extra host tools are follow-ups.

## Today
- `pull_request` webhook → `ImpactReviews.receiveEvent` → thread/run rows → job `ReviewPullRequest` → `Pkg.ImpactReview.reviewPullRequest`.
- Per-repo settings: `enabled`, `includeEvidence`. Drafts are never reviewed and there is no on-demand trigger.
- The advisory agent (Pi in an offline Gondolin VM) has read/bash/edit, `query_telemetry`, `run_regression` (one frozen bash on base and head in fresh VMs, with receipts), memory and `submit_review`.
- The GitHub comment is a flat list. The web answer passes `widgets = Nothing` although the chat renderer draws widgets.

## Goals
1. The comment leads with a merge-risk verdict and what to check, not an audit log.
2. Each evidence item is one `Widget`: rendered live on the run page, and as `widgetPngUrl` in GitHub. Same definition, two renderings.
3. A trigger setting with three options: Ready PRs (default, today's behaviour), Drafts and ready PRs, Only when asked. `@monoscope review`, `@monoscope test` and `@monoscope prove it [focus]` work in every mode.
4. Proof: PR claims are checked by running them on base and head, and recorded (asciinema for terminal/curl/CLI). The comment shows an animated GIF that links to the full video.

## A. Triggers
- Migration 0268: `pr_review_settings.trigger` (`ready` | `drafts` | `mention`, default `ready`). Backfill `enabled = false → mention`, then drop `enabled`.
- `ReviewTrigger` (WrappedEnumSC) replaces `enabled :: Bool`. `PullRequestEvent.reviewable :: Bool` becomes `PullRequestState = Open | Draft | Closed`.
- `receiveEvent` decides in SQL from trigger × state. Every event still advances `latest_revision`, so an in-flight run is superseded.
- `execute`'s draft supersede and `sameRevision`'s `not draft` must follow the run's eligibility (a requested or drafts-mode run may review a draft).
- `issue_comment` (`created`, on a PR, author association OWNER/MEMBER/COLLABORATOR, body `@monoscope review|test|prove it`) → job that fetches the PR head, records a requested run (`request` + focus on the run row), and reacts 👀.
- Deploy step outside code: the GitHub App subscribes to `issue_comment`, and reactions need `pull_requests: write` / `issues: write`.

## B. Merge risk and presentation
- The warp merge-risk rubric as types: `RiskDimension = Alignment | Impact | Verification | Rollback | ErrorDetection`, `Grade = Good | Fair | Poor | NotApplicable`.
- Haskell computes the overall level: weights 1/2/2/1.5/1, bands < 1.5 low, < 2.25 medium, otherwise high. The model never picks it. Doctest the bands.
- The model submits all five assessments with its findings. ErrorDetection is grounded in Monoscope: do the changed paths emit telemetry, and do monitors cover them?
- New result fields are optional, so stored results from earlier runs still decode.
- Every model-authored string keeps the `plain`/`clean` escaping. Only host-built links appear.
- With `includeEvidence = false`, the comment shows the risk table, grades and finding locations, and keeps production numbers and charts in Monoscope.

## C. Proof (terminal v1)
- `.monoscope/PROOF.md` is read from the base branch (a PR cannot rewrite its own rules) and cut at 20k chars. It is untrusted guidance: when to prove, how to start the app offline, what to show.
- `record_proof {claim, script}` reuses the `run_regression` path: frozen bash, a fresh VM per side, metered allocation. The script runs under `asciinema rec --command`, and the host pulls the cast back.
- The host renders `cast → gif` (agg) and `gif → mp4` (ffmpeg) with bounded size and duration, then stores digests and media in the artifact bucket.
- `claims: [{title, status: matches|differs|unverified, summary, proofKeys}]`. A matches/differs claim must cite a recorded proof key.
- Media is served by a signed route (HMAC like widget PNGs), so GitHub's image proxy can fetch the GIF.
- Limit: the guest is offline, so app-level proof needs a prepared workspace. Browser proof (Playwright `recordVideo`) needs a second guest image and is a stretch goal.

## Verification
- Doctests: risk bands, mention parsing, trigger × state.
- Integration (ImpactReviewsSpec, test interpreter): webhook modes, issue_comment, golden comment, stored widgets, claim citing an unknown proof key rejected, signed media route.
- Real-VM proof may be unverifiable on this Mac. If so, the PR says so.
- Process: `/hs-distill`, `/hs-evasion-review` and `/hs-lob-review` until clean, then `/simplify`. Run `/better-writing` on the comment copy, and `/better-layout` + `/better-ui` on the settings form and run page, each looped until clean.

## D. Research and investigation reach (from review feedback)
- The VM stays offline: it runs untrusted PR code, and open egress would let a PR exfiltrate source or abuse our network.
- Web docs: a host-side `fetch_url` tool (with an SSRF guard, size cap, text extraction). The host fetches; the guest never touches the network.
- Dependencies: registry-only egress (npm, PyPI, Hackage, crates, Go proxy, Maven) through a host proxy, if Gondolin supports a proxied NIC. Otherwise, use the prepare-script dependency archive.
- Monoscope investigation: mount monoscope-skills (`investigate`, `kql-reference`, `triage`) via `IMPACT_REVIEW_SKILLS`, and widen the host tools with the CLI's read verbs (issues, monitors, endpoint stats, trace lookup). These stay scoped to the mapped services and the review window. No API key enters the VM.
