# Proving changes in this repository

Reviews run in a VM that can read package registries and nothing else. Prove UI changes in the
real app, and CLI and script behaviour in the terminal.

## When not to prove

Record nothing when the diff shows the whole change: docs and the README, comments, formatting,
`.github/` workflows, `docker-compose.yml` and other deployment config the VM never runs, and test-only
changes the test run already checks. A recording that only prints the changed lines proves nothing.

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

Command: write the Playwright script into `web-components/` (so `playwright` resolves; the file is
untracked) with a quoted heredoc, so nothing in it needs escaping, then run it. Record the whole
window, sidebar included, at 1280x800. The host draws a browser window around the recording from
`page.tsv` (title and URL), and the dot below shows where each click lands:

```bash
cd web-components && cat > proof.mjs <<'JS'
import { chromium } from 'playwright';
import { writeFileSync } from 'node:fs';
const out = process.env.PROOF_MEDIA, t0 = Date.now(), chapters = [];
const step = (label) => { chapters.push(((Date.now() - t0) / 1000).toFixed(1) + '\t' + label); console.log('== ' + label); };
const browser = await chromium.launch();
const context = await browser.newContext({ viewport: { width: 1280, height: 800 }, recordVideo: { dir: out, size: { width: 1280, height: 800 } } });
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
await page.screenshot({ path: `${out}/Live Tail toolbar at 1280px.png`, clip: { x: 0, y: 80, width: 1280, height: 60 } });
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
take, never setup or teardown. Aim for 10 to 30 seconds.

Write `chapters.tsv` and close the context before any `process.exit`, or the video is lost. The same
script records both sides, so name screenshots for what they frame (`Live Tail toolbar at
1280px.png`), never for the outcome you expect. Crop them to the part that changed: the recording
shows the whole window, the stills show the detail.

For a state the sample traffic cannot produce, `web-components/bench/` mounts one component on
synthetic data without the server (`cd web-components && npx vite --config
bench/live-tail.vite.config.ts --port 3098`). It has no sidebar, so subtract the app's sidebar from
the viewport to match a window width.
