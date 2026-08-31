# Log list performance and continuity

## Outcome

The log explorer must scroll smoothly, and a background refresh must never move a reader who
has scrolled away from the newest edge.

## What was wrong, and what changed

Everything below was traced in a real browser (headless Chrome driven by puppeteer against the
demo project) before being changed. The reproduction scripts are throwaway; the measurements and
the reasoning are recorded here because the next person will not have them.

### 1. A background tick could dump buffered rows onto a reader deep in history — FIXED

The reported bug: *"scrolling down, I get pushed back to the latest logs, usually after the 15
second live update."*

Traced mechanism, in order:

1. Retention was a single threshold (`MAX_RETAINED_ROWS = 750`) that a single page of history
   overshot on arrival, so **every** load-more evicted, and every eviction bumps
   `virtualizerEpoch` and remounts the virtualizer through `keyed()`. A 76-second continuous
   scroll produced **17 remounts**.
2. A remount blanks the runway, so the browser clamps `scrollTop` to 0 — **and fires a scroll
   event for that clamp**.
3. `resumeLiveTailAtEdge` read that scroll event as "the reader came back to the newest edge"
   and flushed the whole buffered tick into the list.
4. That merge evicted from the *history* end — the rows the reader was actually reading —
   remounted again, and left them at the top on the newest rows.

Fixes:

- `resumeLiveTailAtEdge` now bails while `isRepositioning`, matching what `syncBottomPin` and
  the proximity prefetch already did. A frame of collapsed geometry is not the reader's intent.
- Retention became a pair: cross `RETENTION_LIMIT` (2,500), cut back to `MAX_RETAINED_ROWS`
  (1,500). A cut is expensive, so it is now rare instead of once per page. The same 76-second
  scroll went from **17 remounts to 2**, and the reader's scrolled distance roughly tripled
  because they were no longer being thrown back.
- The two remaining remounts were checked instrumented: the anchor row is found and the new
  `scrollTop` matches the index shift exactly, so the reader stays on their row. The absolute
  offset changes because rows above them were genuinely dropped — that part is correct.

Regressions added in `log-list-scroll-position.test.ts`; both fail without their fix (verified by
reverting each): the buffer test fails with `expected [] to have a length of 400`, the retention
test with `expected 3 to be +0` remounts.

Re-confirmed in the browser against the final bundle: 80 seconds of continuous downward scrolling
across several real 15-second ticks gave **4 remounts, zero unexplained scroll losses, and 1,500
buffered rows held rather than flushed**, with the reader reaching twice the scrolled depth they
could before (they were previously being thrown back).

**Harness fidelity fix, and why this escaped for so long:** `scrollHarness` simulated the
remount's `scrollTop = 0` clamp but never fired the scroll event the browser fires with it. Every
scroll-position test therefore ran against a list whose edge handlers never saw the collapsed
frame — the one frame on which "parked at the newest edge" is a lie. The harness now dispatches
it.

### 2. A watchdog was doing forced layout on every render — FIXED

`healBlankVirtualizer` is a last-resort recovery for a virtualizer that has stopped rendering. It
was called from `updated()` on every render, and it measured **every mounted row** with
`getBoundingClientRect`. In a CPU profile of a throttled scroll it was the largest app-level
cost: `getBoundingClientRect` was 14–16% of all samples, most of it attributed to this function,
each call forcing a synchronous style+layout flush mid-scroll.

- It no longer runs per render. Its other three triggers — the post-eviction timer (the known
  cause), the 2s watchdog, and the scroll-end timer — cover every case off the frame path.
- The check is now O(1): mounted rows are one contiguous band, so the first and last bound all
  the others. Two rectangles replace ~50.
- `resumeLiveTailAtEdge` checks its cheap state before touching the DOM, and the scroll handler
  coalesces it into one rAF instead of running per scroll event. `syncBottomPin` deliberately
  stays synchronous — deferring the bottom pin by a frame reintroduces the bug it exists to
  prevent, and it costs nothing on a newest-first list.

After: `getBoundingClientRect` fell to 8.8% of samples.

### 3. The virtualizer layout was reallocated on every render — FIXED

`.layout=${… ? {} : { type: DenseRowFlowLayout }}` allocated a fresh specifier object per render.
The virtualizer treats that as being newly configured: it reassigns `_layout.config` and
reschedules a reflow. Both are now module constants.

This is unambiguously correct, but its **magnitude is unproven** — the runtime A/B intended to
measure it failed (the private field it needed is named differently), and the environment was too
noisy for a cross-build comparison. Do not quote a number for it.

## The dominant remaining cost: `:has()` scoped to `<body>`

This is the biggest lead in the frontend and it is **not fixed**. It is written up in detail
because the evidence is strong, the fix is cross-cutting, and one measurement did not replicate.

What is solid, repeatedly:

- During a scroll, `UpdateLayoutTree` (style recalculation) is **~15 of every 25 seconds** at 4x
  CPU throttle — more than layout, paint, script and GC combined.
- Each pass restyles **~1,400–1,700 elements**, which is the entire document, ~10 times a second.
- Chrome's invalidation tracking names the cause directly:
  `StyleRecalcInvalidationTracking | Affected by :has() | HTML`, ~940 times in 10 seconds, plus
  the same on the top-level `<section>`s.

The mechanism: a `group-has-[…]/pg:` utility compiles to
`:is(:where(.group\/pg):has(:is(#x:checked)) *)` — the subject is *every element in the document*.
`<body>` carries `group/pg` (`BodyWrapper.hs`), and so do the page shells (`#apiLogsPage`,
`#dashboardPage`, …), so **every such rule anywhere in the app also applies at body scope**.
There were 74 of them. A virtualized list mutates constantly — rows enter and leave the runway
every frame — and each mutation makes Chrome re-evaluate all of them across the whole document.

One run with all `group/pg` classes stripped in the browser measured median frame **141ms → 8ms**
and **1,411 → 17 elements** per pass. **That result did not replicate on later runs**, and
stripping body-only or page-only never reproduced it. Treat the 17x as unverified.

Why it was not fixed here, having been attempted and reverted:

- The attempt rescoped the sidenav's 38 variants to `group/nav` on the `<aside>`, gave the shell
  its own group, and finally replaced the last three (mobile nav) with hand-written sibling
  selectors so `<body>` carried no group at all. Body-scope `:has()` rules went 74 → 0.
- It still showed no measured improvement, and it regressed the mobile nav: the panel stopped
  sliding in (aside stayed at `left: -240`). *Hypothesis, not verified*: Tailwind v4 emits
  `max-md:-translate-x-full` via the `translate` property rather than `transform`, so a
  hand-written `transform: translateX(0)` never wins. Test before relying on it.
- Shipping an app-wide shell refactor on an unreplicated measurement, with a known regression, is
  the wrong trade. It was reverted in full.

**How to pick this up properly.** The measurements here are unreliable because the machine had
*two other dev servers* bound to :8080 from other sessions, and the repo was being edited
concurrently, so successive runs hit different builds with different row counts. De-noise first:

- `lsof -nP -iTCP:8080 -sTCP:LISTEN` — expect exactly one listener. Two processes can both hold
  the port, one on IPv4 and one on IPv6, and `localhost` picks between them. Probe
  `http://127.0.0.1:8080` and `http://[::1]:8080` separately and check which build answers.
- Confirm the page is running the build you just made: compare the entry chunk in the served HTML
  against `dist/manifest.json`'s `index.html` entry. The shell TH-splices that hash, so a server
  that has not recompiled serves a stale bundle while the files on disk are current. (Rewriting
  the entry request to the manifest's chunk, via request interception, is a workable stopgap.)

Then:

1. Confirm the `Affected by :has() | HTML` count and the elements-per-pass with body's group
   present vs absent, several runs each, interleaved.
2. If confirmed, rescope: no `group-has-[…]/pg:` variant may sit on an ancestor of a virtualized
   list. The sidenav's 38 belong on `group/nav` on the `<aside>` (all their targets are inside
   it, and `#sidenav-toggle` is too, so the aside itself can use plain `has-[…]`). The mobile
   nav's three are sibling-reachable from `#mobile-nav-toggle`, which is a body-level sibling of
   both the backdrop and the `<section>` holding the nav — but set `translate`, not `transform`.
3. Verify all four nav states (desktop collapsed/expanded, mobile closed/open) by geometry, not
   by eye: expanded aside is 240px wide with 17 nav labels visible; mobile-open puts the aside at
   `left: 0`.

## Invariants this work must not break

- A reader away from the insertion edge never loses their visible anchor to background work.
- A frame of collapsed post-remount geometry is never read as the reader's intent.
- No background update creates an unbounded sequence of data requests.
- Nothing on the scroll path performs a synchronous forced-layout read per event.
