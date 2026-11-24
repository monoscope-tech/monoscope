# Proofs: Monoscope vs Blacksmith (2026-11-24)

Blacksmith ("codesmith proof") comments on 32 Talstack pull requests; Monoscope's bot reviews
monoscope and timefusion. This compares what each produced and records what we changed.

## Can we run our proofs on the Talstack PRs?

Not yet. The Monoscope GitHub App is not installed on `Talstack-LLC` (only `claude` and
`blacksmith-sh` are), and reviews only run for repositories linked through an app installation
(`projects.git_credentials.installation_id`). Installing it needs the browser flow:

1. Install the Monoscope GitHub App on `Talstack-LLC`, selecting `talstack` only.
2. In a Monoscope project, add the code mapping for `Talstack-LLC/talstack` and set its review
   trigger to **Only when asked**, with the routine on the **Observe** policy, so nothing posts to
   Talstack pull requests while we compare.
3. Ask for `@monoscope prove it` on #730, #726, #724, #722, #721, #720, #716, #711 (or retry the
   runs from the run page).

Talstack has no `.monoscope/PROOF.md`. Blacksmith proved it with no repository guidance. That is
the bar.

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

Across all 32 commented Talstack PRs, 30 carry a Matches or Differs verdict, 1 was dismissed, and
none is Unverified. PRs before #720 use an older layout without the hidden data block, so their
claim and image counts do not parse. They are not empty.

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
| Matches / Differs | 7 |
| Unverified | 11 |
| Last visible comment stuck at "Proving this PR · 0 of N recorded" | 7 |
| Review stopped / no proof needed | 5 |

Over 14 days, 73 of 207 head recordings (35%) and 26 of 208 base recordings exited 0.

We already match Blacksmith's format: stills on both sides, GIF/MP4/WebP recordings with
chapters, live progress, "Also checked", "New issue found while testing", rating links,
production-telemetry merge risk (which Blacksmith does not have). The gap is outcome.

### Why ours fail

1. **Stale progress comments (7 PRs).** All 37 "Review authorization is no longer available"
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
