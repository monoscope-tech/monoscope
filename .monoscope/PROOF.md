# Proving changes in this repository

Reviews run in a VM that can read package registries and nothing else. Prove UI changes in the
real app, and CLI and script behaviour in the terminal.

## When not to prove

Record nothing when the diff shows the whole change: docs and the README, comments, formatting,
`.github/` workflows, `docker-compose.yml` and other deployment config the VM never runs, and test-only
changes the test run already checks. A recording that only prints the changed lines proves nothing.

## Choosing the evidence

Prove a claim with the fewest pieces of evidence that settle it, each the kind a reviewer would
pick for that change:

| Change | Evidence |
| --- | --- |
| A flow of user actions (open, click, submit, see the result) | Browser recording with chapters, plus a still of the deciding state on each side |
| A static visual (label, icon, badge, menu entry, modal field) | Stills only, no video, cropped to the part that changed |
| Layout or responsive behaviour | One still per width, plus a printed dump of the measured boxes (`boundingBox()` of each column or card) |
| An HTTP endpoint, redirect, auth or header | `run 'curl -si …'` steps, a `# comment` on each saying what it sends |
| Stored data or a migration | `run "psql -h /tmp -U postgres -d monoscope -c 'select …'"` before and after, on rows that existed before the change |
| A background job, worker or engine | A scenario script printing `== 1. case` headings and each resulting state |
| Telemetry | The emitted span or log, printed with only the attributes the claim is about |

A terminal proof that only prints (colours are fine; no progress bars or full-screen tools) is shown
as a still card of everything it printed, base above head, so print what proves the claim and little
else. `run 'cmd'` prints the command, its output and its exit status as the card shows them; print a
heading first saying which side and what it shows (`echo 'After: reused code on /auth_callback'`).
Redact secrets (`sed 's/session=[^;]*/session=<redacted>/'`).

When the app or a third-party service cannot run in the VM, prove what can be proved against a stub or
a direct call, and say in the claim's summary what was stubbed or not run and why ("WorkOS stubbed:
needs WORKOS_API_KEY"). A claim nothing could exercise stays unverified, with the reason.

## Haskell changes

A cold build of this repository takes over an hour, so never build it from scratch in a proof. The
review starts from a prepared workspace, whose `dist-newstyle` already holds the base's build, so
one focused test, a doctest or `monoscope-app --server` compiles only what changed. If the workspace
was not prepared (`dist-newstyle` is missing), mark the claim unverified and say which test would
prove it.

Build with the prepared workspace's flags, `cabal build --offline --ghc-options="-O0 -j2" <target>`.
Without `-O0`, cabal builds at `-O1`, every module recompiles ("Optimisation flags changed"), and the
check runs out of time. Warning flags such as `-Werror=unused-imports` do not force a rebuild.

## UI changes

Setup (unrecorded; it links and writes gitignored paths only), once per VM:

```bash
monoscope-app            # frontend only (web-components/, static/): about ten seconds
monoscope-app --server   # also src/ (Lucid pages, handlers, migrations): about two minutes
```

Plain `monoscope-app` swaps this checkout's frontend into a recent production build of the server.
`--server` builds this checkout's server too, starting from the workspace's build of the base, so
it compiles only what the pull request changed, and applies the pull request's new migrations. Use
it whenever the change touches `src/`, in record_proof's setup: building the server needs the check
VM's memory, not the agent's. Both start the app on `http://127.0.0.1:8080` against a
migrated Postgres and return once sample traffic is flowing: an hour of history from four services,
then a few requests a second, so Live Tail streams. The demo project at
`/p/00000000-0000-0000-0000-000000000000/` needs no login.

Chromium and Playwright are preinstalled; never run `playwright install` (its browser cache is
read-only, so it fails the setup).

Command: write the Playwright script into `web-components/` (so `playwright` resolves; the file is
untracked) with a quoted heredoc, so nothing in it needs escaping, then run it. Record the whole
window, sidebar included, at 1280x800, with `deviceScaleFactor: 2` so stills are sharp. The host
draws a browser window around the recording from `page.tsv` (title and URL), and the dot below shows
where each click lands:

```bash
cd web-components && cat > proof.mjs <<'JS'
import { chromium } from 'playwright';
import { writeFileSync } from 'node:fs';
const out = process.env.PROOF_MEDIA, t0 = Date.now(), chapters = [];
const step = (label) => { chapters.push(((Date.now() - t0) / 1000).toFixed(1) + '\t' + label); console.log('== ' + label); };
const browser = await chromium.launch();
const context = await browser.newContext({ viewport: { width: 1280, height: 800 }, deviceScaleFactor: 2, recordVideo: { dir: out, size: { width: 1280, height: 800 } } });
await context.addInitScript(() => addEventListener('DOMContentLoaded', () => {
  const dot = document.documentElement.appendChild(document.createElement('div'));
  dot.style.cssText = 'position:fixed;z-index:2147483647;left:-40px;width:16px;height:16px;margin:-8px 0 0 -8px;border-radius:50%;background:#2f6fedaa;border:2px solid #fff;pointer-events:none;transition:scale .1s';
  addEventListener('mousemove', (e) => { dot.style.left = e.clientX + 'px'; dot.style.top = e.clientY + 'px'; }, true);
  addEventListener('mousedown', () => { dot.style.scale = '.6'; }, true);
  addEventListener('mouseup', () => { dot.style.scale = ''; }, true);
}));
const page = await context.newPage();
await page.goto('http://127.0.0.1:8080/p/00000000-0000-0000-0000-000000000000/log_explorer');
writeFileSync(`${out}/page.tsv`, `${await page.title()}\t${page.url().replace(/^https?:\/\//, '')}\n`);
step('Open Live Tail');
await page.getByRole('link', { name: 'Live Tail' }).click();
await page.waitForTimeout(2000);
await page.getByRole('button', { name: 'Clear' }).locator('..').screenshot({ path: `${out}/Live Tail toolbar.png` });
// …the steps that show the change, each behind step('…'), then an assertion that exits 1 on the base.
await context.close(); await browser.close();
writeFileSync(`${out}/chapters.tsv`, chapters.join('\n') + '\n');
JS
node proof.mjs
```

Locate controls by role and wait for them before asserting, and assert on what the user sees, so
the base recording fails and the head passes:

```js
const clear = page.getByRole('button', { name: 'Clear' }), pause = page.getByRole('button', { name: /Pause|Resume/ });
await clear.waitFor();
const [c, p] = [await clear.boundingBox(), await pause.boundingBox()];
if (Math.abs(c.y - p.y) > 4) { console.log('Clear wrapped onto its own row'); process.exit(1); }
```

The recording is the first thing a reviewer sees, so pace it like a demo: hold about two seconds on
each state worth seeing (`await page.waitForTimeout(2000)`), and chapter only the steps a user would
take, never setup or teardown. Aim for 10 to 30 seconds. Each chapter is captioned in the video as it
plays, so word it as the step ("End the survey"). The comment's preview plays about six seconds from
the last chapter, so make the last chapter the step whose result proves the claim, and hold on that
result.

Skip the video when nothing moves: for a static visual change, take the stills and close the context
without `recordVideo`.

Write `chapters.tsv` and close the context before any `process.exit`, or the video is lost. The same
script records both sides, and the comment pairs the merge-base and pull-request stills by file name,
so name screenshots for what they frame (`Survey menu.png`), never for the outcome you expect, and take
each with the same locator on both sides (`locator.screenshot()`; a `clip` rectangle drifts when the
change moves things). Frame the component that changed with enough around it to place it (the menu
and its row, the whole top bar), not one word: a 60x16 crop of a label tells a reviewer nothing. Show
the whole page only when the claim is about the page.

For a state the sample traffic cannot produce, `web-components/bench/` mounts one component on
synthetic data without the server (`cd web-components && npx vite --config
bench/live-tail.vite.config.ts --port 3098`). It has no sidebar, so subtract the app's sidebar from
the viewport to match a window width.
