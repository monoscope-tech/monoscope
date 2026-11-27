# Proofs: Monoscope vs Blacksmith (2026-11-24)

Blacksmith ("codesmith proof") comments on 32 Talstack pull requests; Monoscope's bot reviews
monoscope and timefusion. This compares what each produced and records what we changed.

## Running our proofs on the Talstack PRs

The Monoscope app isn't installed on Talstack-LLC, so the exact PR commits were pushed from a local
checkout to a private mirror, `monoscope-tech/talstack-proof-mirror` (mirror #1–#8 = Talstack #730,
#726, #724, #722, #721, #720, #716, #711). Our reviews post there, never on Talstack. The mirror is
mapped in the monoscope project, so the merge-risk section has no Talstack telemetry (Talstack Prod
has no repository mapping).

| Talstack PR | Blacksmith | Ours, first run (09:50 UTC) | Ours after #1024 and #1027 |
| :-- | :-- | :-- | :-- |
| #730 anonymous survey | Matches (1 claim, 4 stills) | 0/2 verified | 1 match (survey tests), main claim unverified |
| #726 People tab | Matches (7), 1 issue found | 0/3 | 2 match (non-admin view, links), 1 unverified |
| #724 HRIS leaks | Matches (4) | 0/3 | 2 match (category audiences, export fields), 2 unverified |
| #722 profile catalogue | 10 match, 2 differ, issue found | 0/3 | 1 match (celebrations) |
| #721 review templates | Matches | 0/2 | 1 match (create-cycle page), 1 unverified |
| #720 payroll | Differs (6), issue found | 0/2 | 1 match (builds), 1 unverified |
| #716 drop hris_enabled | Matches | 2 match | 1 match |
| #711 unbundle modules | Matches | not run | 2 match (gate specs), 4 unverified |

What changed between the runs: Talstack could not build in our VMs at all. The guest's cabal index
was root-owned, stackage.org was blocked, and Tailwind assets weren't built before Template Haskell
read them (#1024). It now builds cold in 22 minutes into a 3.6 GB warm workspace. Agents also
stopped loading the project into GHCi in their own 1 GB VM, which had stalled two reviews to their
45-minute budget (#1027).

Where Blacksmith is still ahead on Talstack: it starts the app with seeded roles and shows base and
head screenshots per claim; ours proves through tests and scripts. It found three issues the PRs
didn't claim; we found none. Some of our "matches" are weak ("code is buildable", "tests execute"),
which #1029 now forbids as claims. Next lever: a Talstack proof recipe (Postgres, migrations,
seeded sessions, server) in a `.monoscope/PROOF.md`, so UI claims get screenshots.

### With the app recipe (2026-11-25)

A saved recipe now starts Talstack in the verifier: Postgres, `.env.dist`, the server, and three
seeded sessions (Ada admin, Max manager, Mia member reporting to Max). Two worker bugs kept it from
working until #1034: a short terminal setup overwrote the recipe, and leases left by killed workers
pinned about 70 GB of stale workspaces, so maintenance evicted Talstack's fresh base build within
minutes and every check rebuilt cold past the 900 s limit. After the fix every check attached the
workspace; base checks take 1–2 min and head checks 5–7 min (head rebuilds the changed modules).

| Talstack PR | Blacksmith | Ours with the recipe |
| :-- | :-- | :-- |
| #726 People tab | Matches (7), 1 issue found | 2 match with UI stills and video: a member opens Company People on head, base rejects it; a manager links only to reports. 2 unverified (field masking, sidebar labels) |
| #722 profile catalogue | 10 match, 2 differ, issue found | 1 match (pay fields seeded, date of birth private by default) with base/head stills; 5 unverified |
| #721 review templates | Matches | 0/2: all three attempts retried a locator inside a closed `<dialog>` (#1035 now says to open it first) |
| #730 anonymous survey | Matches | re-run after the fix: 1 match (anonymous responses masked) |
| #724 HRIS leaks | Matches (4) | re-run: 1 match (admin export omits sensitive columns), 2 unverified |
| #716 drop hris_enabled | Matches | re-run: 1 match (schema comparison before and after the migration) |
| #711 unbundle modules | Matches | re-run: 1 match (admin flow enforces the Goals gate) |

The #726 head still is a real screenshot of the People table with the seeded roles, which is the
format Blacksmith uses; ours adds the merge-risk table and the not-verified list. Still behind:
Blacksmith covers more claims per PR and found issues the PRs didn't claim; we found none.

### Screenshots side by side (2026-11-26)

Comparing our stills with Blacksmith's on the same PRs:

- **Fonts.** Both render Messina Sans with uneven glyph spacing ("Surve ys", "Engage ment"): headless
  Chromium hints fonts in full by default. With `--font-render-hinting=none` the same page renders with
  natural kerning (verified on the worker; the line is 3% narrower). #1043 wraps the guest's Chromium so
  every agent script gets it; Blacksmith's stills still have the defect.
- **Base without a page.** On #730 our base still was a terminal error dump: the script asserted before
  its screenshot, so only head showed the UI. Blacksmith's scripts print both states and exit 0 on each
  side ("contains Jordan Rivera: true" then "false"). #1043 tells agents to screenshot before asserting.
- **Pairs that show nothing.** #1043 pairs base and head stills by name: an identical pair, or a side
  without a screenshot, goes back to the agent as advice; a changed pair gets the changed pixels boxed on
  both sides. Blacksmith shows no highlight.

### After #1043 (2026-11-26, #726 re-proved)

- 3 claims match, up from 2 (member People access, admin table, manager navigation), with three base/head
  page pairs at 2560x1600; 3 stay unverified (active-only rows, public-field masking, admin-only tabs).
- Fonts render with natural spacing on every still.
- One bounding box around all changes covered the whole page: full-page stills of a relaid page differ in
  many places (sidebar items, toolbar, a 6 px table shift). #1067 boxes each cluster instead and marks only
  local changes (at most three clusters, under a quarter of the still); relaid pages stay unmarked.
- Head checks still take 285 s against 53 s for base, so the prepared head workspace below is the next lever.

### Issues found (2026-11-27)

Blacksmith's "differs" findings test what reviewers asked for and edge cases of the change; our agent saw
only the PR title and description. #1078 and #1079 give it the conversation and inline review comments
(newest first, our own and Blacksmith's excluded) and ask for claims from reviewer asks and one at-risk
edge case. On mirror #6 (Talstack #720), with the upstream review's ask posted ("payroll pages should be
admin-only"), the re-proof reported:

- differs: Plain member is refused from payroll. The head recording shows the plain member getting
  success on the payroll dashboard and configuration route (Blacksmith's c5).
- differs: Payroll configuration member edge case, found by the agent itself.
- matches: Payroll dashboard and configuration render for the admin.

That is our first issue found that Blacksmith also found, with a recording for each claim.

### Faster head checks (2026-11-27)

A Talstack rebuild after a change to a module with 54 dependents takes 344 s serially and 194 s with
`-j4`. #1088 makes the proof guest build with `ghc-options: -j`; `-j` is left out of cabal's
configuration hash, so prepared workspaces stay warm. It applies at the next guest rebuild.

### Next: a prepared head workspace (proposed)

Reviews average 3.0 proofs, always the budget, and each head check rebuilds the PR from the base
workspace: on the mirror a head check takes 403 s median against 97 s for base (monoscope: 107 s, p90
535 s). Preparing the head once per review, while the agent reads the code, would cut every head check
to base speed and leave room for more claims. Scope for that PR:

- Prepare a workspace at the head revision, warm-started from the base build (the existing ancestor path).
- Checks lease it for the head side, the base workspace for the base side.
- Head entries must not become the lineage's `latest-*` pointer, must not be archived on build (3.6 GB
  per PR head), and need their own eviction, since prune archives before it evicts.

Not raising the proof budget: at 7 minutes per head check a fourth proof breaks the 45-minute review budget.

### Review failures (2026-11-25)

- #721's re-proof failed "exceeded its submission budget"; its automatic retry then ended "Saved progress
  was lost", which replaced the cause in the comment. #1038 stops such a chain with the real reason and
  logs each rejected draft. The next #721 run completed: the one rejection was a "matches" claim without a
  passing head recording, which the agent corrected.
- #721's agent now opens the template dialog (#1035) but then hit a Playwright strict-mode violation (the
  title also sits in a hidden preview modal); #1043 adds advice for that case.

## What Blacksmith produces

| PR | Verdict | Claims | Screenshots | Found | Not tested |
| :-- | :-- | --: | --: | :-- | --: |
| #730 anonymous survey | Matches | 1 | 4 | note: respondent still identifiable by user id | 2 |
| #726 Company People tab | Matches | 7 | 25 | issue: Reports count leaks when private; note: admin count dropped | 3 |
| #724 HRIS privacy leaks | Matches | 4 | 12 + 2 videos | — | 5 |
| #722 profile catalogue | Matches, 2 differ | 12 (2 blocked) | 17 | Base salary data drops out of view; note | 6 |
| #721 review templates | Matches | — (older layout) | video | — | — |
| #720 payroll screen | Differs | 6 (1 skipped) | 12 + video | issue: saved filter queries a missing column | 5 |
| #716 drop hris_enabled | Matches | 1 | 3 | — | — |
| #711 unbundle modules | Matches | — (older layout) | video | — | — |

Across all 32 commented Talstack PRs: 25 Matches, 1 Differs (#720), 1 dismissed (#707), and 5 in an
older layout whose verdict icon does not parse (#642, #645, #659, #687, #693). None is Unverified. PRs
before #720 have no hidden data block, so their claim and image counts do not parse. They are not
empty.

What makes these comments strong:

- **A real app with real roles.** Blacksmith builds Talstack (Haskell, GHC 9.12), runs Postgres
  and seeds companies, admins, managers and plain members. It inserts sessions directly where
  WorkOS would be needed, and says so under "not tested".
- **Base and head side by side for every claim.** Each claim has a merge-base still and a PR-head
  still, plus the commands that produced them.
- **Finds what the PR did not claim.** #726's Reports-count privacy leak, #720's saved-filter
  column, #722's salary data loss.
- **Carries results across pushes.** "Still holds on the new head", "I reran only the head side;
  the merge-base screenshots from the last run still apply", and per-claim `tested commit`. One
  comment per PR, edited in place.
- **A hidden machine-readable block** (`blacksmith:proof-data`) so coding agents can read
  verdicts, observations and image URLs without parsing HTML.
- **Honest scope.** A "not tested" list of concrete things it did not exercise, and `blocked
  (environment)` and `skipped` verdicts distinct from failures.
- `@codesmith dismiss this proof` and a 👍/👎 rating.

## What ours produced (monoscope PRs #975–#1014)

| Outcome | PRs |
| :-- | --: |
| Matches / Differs | 6 (#976, #977, #978, #983, #992, #1010) |
| Unverified | 11 |
| Last visible comment stuck at "Proving this PR · 0 of N recorded" | 9 (#975, #980, #982, #985, #994, #997, #1000, #1001, #1014) |
| Merge risk only, no proof | 3 |
| Review stopped | 2 |

Over 14 days, 73 of 207 head recordings (35%) and 26 of 208 base recordings exited 0.

We already match Blacksmith's format: stills on both sides, GIF/MP4/WebP recordings with
chapters, live progress, "Also checked", "New issue found while testing", rating links,
production-telemetry merge risk (which Blacksmith does not have). The gap is outcome.

### Why ours fail

1. **Stale progress comments (9 PRs).** All 37 "Review authorization is no longer available"
   failures in the last 7 days were a push (20) or a merge (17) landing mid-review. The run was
   superseded without touching its progress comment, so a merged PR keeps "Proving this PR · 0 of
   4 recorded … still running" forever. Each one was also logged at attention level and retried.
2. **The app never comes up (most of the 11 Unverified).** "proof app was unavailable", "missing
   hasql-pool source package", "verifier setup failed before the server started", "could not
   complete the Haskell setup". The production guest image was built 2026-11-19. The dependency
   set changed on 11-21, 11-22 and 11-23 (`hasql-pool` on 11-21), and the guest was not rebuilt.
3. **A misleading saved recipe.** The only stored monoscope recipe is
   `cd /workspace/impact-review-worker && npm ci --ignore-scripts`, from a worker-test proof. Every
   review is told to "pass it unchanged" as its setup, including UI proofs that need
   `monoscope-app`.
4. **Worker disk exhaustion (6 failures).** `resume.qcow2: hPutBuf: resource exhausted` and
   `No space left on device` on the lease and skills paths.
5. **Lost or unrestorable sessions (15)** and **budget overruns (11)**.

## Plan

1. Finalize the progress comment when a run is superseded or its PR closes. Don't log a newer
   push as a failure.
2. Rebuild the guest when the dependency set changes, not by hand.
3. Offer the saved setup as a starting point for the same kind of proof, not as an order.
4. Carry proofs across pushes: when the base is unchanged, reuse the base recording and record
   only the head.
5. Add a hidden machine-readable results block for coding agents.
6. Link Talstack (above) and compare claim for claim.

## Profile: where proof time goes (7 days)

| Check outcome | Base | Head | p50 | p90 | VM hours |
| :-- | --: | --: | --: | --: | --: |
| passed | 9 | 30 | 83–128 s | 323–567 s | 2.3 |
| failed | 135 | 115 | 63–118 s | 255–475 s | 11.0 |
| blocked (exit 126/127, compile failure) | 51 | 49 | 45 s | 52–67 s | 2.2 |
| timed out (900 s cap) | 11 | 12 | 903 s | 904 s | 5.8 |

About 21.5 VM-hours of checks ran for 104 reviews (4 checks each), and only 2.3 of those hours were
checks that passed. Head recordings fail mostly on setup (missing tools or sources in the stale
guest, a server that never starts), then on agent-written Playwright scripts (locator timeouts,
strict-mode violations, syntax errors). Reviews take 12 minutes at p50 and 45 at p90.

Progress on each item is logged below as it lands.

## Log

- **Stale progress comments (fixed, `52f7282ba`).** A run replaced by a push or merge now edits its
  progress comment to say which commit replaced it, or that the PR closed. A newer run that took the
  comment over keeps it. It is logged as info and not retried. Regression test:
  `supersededReview_finalizesItsProgressCommentInsteadOfFailing`.
- **Machine-readable results (added, `de66b503f`).** Every proof comment ends with a hidden
  `monoscope:proof-data` block: each claim's verdict, observation, base and head stills and
  recording, new issues, what could not be verified (where evidence is allowed), and merge risk.
- **Saved recipe (fixed, `bded5e197`).** The prompt offers the saved setup as the last successful
  one, which may have needed another environment, instead of ordering it reused.
- **Guest rebuild (fixed, `52f7282ba`, `bded5e197`).** Since #1013 the guest could not be rebuilt:
  its dependency stage lacked the new `proto` package and, with `Setup.hs` gone, tried to compile
  the `monoscope` library. It now builds `--only-dependencies all` like `Dockerfile.deps`.
- **New guest deployed (`70d7ab18`, 2026-11-24 02:28 UTC).** Built on the worker from this branch
  against the production server image. It contains the `hasql-pool` source, `cargo-nextest`,
  Playwright and `monoscope-app`. The old guest is kept as `/srv/monoscope/guest.prev-5df490c7`. The
  worker restarts onto it once no agent runs. Every workspace checkpoint is keyed by guest, so each
  repository's next workspace build is cold.
- **Guest rebuilds automatically (`df8fc1fb0`).** `make deploy-worker` fingerprints the files that
  define the guest and rebuilds and swaps it on the worker when they change. See "Refreshing the
  guest" in `docs/agent-operations.md`.
- **Boilerplate caveats removed (`3c3637446`).** Three host caveats opened the could-not-verify list
  of every review. The list now holds only the PR's own gaps, like Blacksmith's "not tested".

- **Carry-forward across pushes (PR #1018).** Each recording stores its claim, command and setup
  (`pr_review_proofs.script`). A review of a later push gets the previous review's claims, the
  scripts that passed on its head, and the files changed since, then re-proves those claims. That
  is Blacksmith's "still holds on the new head".
- **Evidence cap told to the agent (PR #1018).** #1017's own review pointed out that dropping the
  cap caveat also dropped truncation detection. Capped kinds now go to the agent and the log.
- **Proving without PROOF.md (PR #1018).** The prompt gives Blacksmith's recipe: start the app from
  the repository, seed one user per role with sessions inserted directly, check status codes per
  role, and name concretely what was not exercised.
- **Browser proofs fail fast (PR #1018).** A 5-second default timeout, and an `ariaSnapshot()` on
  failure so a retry can see the page's controls. PROOF.md covers hidden sections and the
  sidebar's repeated links, the two locator mistakes behind most failed UI proofs this week.
- **Master's build was broken (#1017).** `test/unit/Pkg/ParserSpec.hs` from #1014 failed
  `-Werror=name-shadowing`, so every PR's Build and Test was red.

- **Integration-test proofs could not start Postgres (PR #1018, `a558c9534`).** VM commands get a
  reset PATH, and the guest exposed Postgres only through Docker `ENV`, so `initdb` was missing and
  every Haskell integration-test proof failed in setup. #1017's own review on the new guest found
  this. The binaries are now linked into `/usr/local/bin`, and `refresh_guest`'s remote path ran for
  real to build that guest on the worker.

- **Proofs lost to busy verifier VMs (PR #1018 and worker config).** The worker admits 8 agents but
  had 2 verifier slots, and a proof needs one per side. In a week, 134 checks (50 from reviews, 84
  from fix runs) waited 45 s, got no VM and failed. Each review case cost one of its three proof
  attempts. Verifier slots are now 4 (`IMPACT_REVIEW_VM_SLOTS=8,4,1` in
  `/etc/monoscope/impact-worker.env`, backup `.bak-20261124`, effective at the next restart), and a
  check waits up to five minutes for a slot. Each side takes its slot separately, so waiting cannot
  deadlock.

- **Guest inputs follow `package.yaml` (#1020).** `monoscope.cabal` changes with every module and
  migration, so #1018's one migration rebuilt the whole guest.
- **One command for Haskell proofs (#1022).** #1021's own review, the first on the fixed worker,
  spent its three attempts on setup it improvised: `cabal build --offline` lacked the git-pinned
  `hasql-interpolate` source, plain `cabal build` failed on the Vite manifest Template Haskell
  reads, and the third sent a malformed patch. `monoscope-app --test MATCH` does the build steps
  `--server` does and runs the matching integration tests; an empty match fails, so the base fails.
  PROOF.md points Haskell proofs at it.

## Status at the end of the night (2026-11-24 ~09:00 UTC)

Merged and deployed: #1017, #1018, #1020, #1021, #1022 (app via the Deploy workflow; worker via
`make deploy-worker` at `3789fe8b`). The worker runs 4 verifier slots. Its guest rebuilds with the
`--test` command, and the worker restarts once idle.

Verified in production:

- **Superseded runs.** #1019 was merged while its review was proving. The comment became "Review
  stopped: a newer commit replaced this review…", and the worker logged "Production impact review
  superseded" at info, with no retry. (#1021 then removed the stray quotes around the commit.)
- **Guest contents.** The rebuilt guest has the `hasql-pool` and `hasql-interpolate` sources,
  `cargo-nextest`, Playwright, `initdb` and `pg_ctl` on the default PATH, and `monoscope-app`.
- **Migration 0305** applied (`pr_review_proofs.script jsonb`).

Not yet seen end to end: a Haskell integration-test proof passing through `monoscope-app --test`,
and a push that carries a passing script forward. Both need a new PR after the guest swap. Check
the next review's comment and `projects.pr_review_proofs.script`.

Open question, not changed: six reviews in a week ended with "exceeded its query budget" (20 host
tool calls for a PR review). Three of them made no proof and ran no check before running out, so
the calls went to telemetry and memory. A larger budget would only help if the agent proves first.
The session transcripts would show which calls those were.

Still behind Blacksmith: no `@monoscope dismiss`, and claims carried over are re-recorded on both
sides rather than reusing the merge-base recording.

## Left for a person

- **Install the GitHub App on Talstack-LLC** (steps at the top) to run our proofs on those PRs.
- **Stale comments already posted** on #975, #980, #982, #985, #994, #997, #1000, #1001 and #1014
  do not heal: the fix acts when a superseded run's job runs again. Edit them by hand or ignore them.
- **The guest was rebuilt twice tonight**: `70d7ab18` (02:28 UTC) and `0c44e530` (04:00 UTC, with
  `initdb`), the second through `refresh_guest`'s real remote path. That swap deleted the old 8 GB
  `guest.prev` by design and kept `70d7ab18` as `guest.prev`. A rebuild peaks near 40 GB of memory
  on the worker host. Each forced restart cost the reviews and fix runs in flight (#1016 lost one).
- **About 67 GB of older guests** on the worker (`guest.prev-20261111`, `guest.prev-ab85f39b`,
  `guest.prev-5df490c7`), on a host that ran out of disk six times this
  week. Automated rebuilds keep only one `guest.prev`. The others can go once the new guest has
  run a few reviews.
