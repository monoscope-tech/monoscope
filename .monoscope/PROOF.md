# Proving changes in this repository

Reviews run offline in a VM, and the Haskell server cannot be rebuilt there within a review's
budget. The guest ships a recent production build of it instead, so prove frontend changes in the
real app, and CLI and script behaviour in the terminal.

## Frontend changes (`web-components/`, `static/`)

Setup (unrecorded; it links and writes gitignored paths only):

```bash
monoscope-app
```

It swaps this checkout's frontend into the guest's server, starts it on `http://127.0.0.1:8080`
against a migrated Postgres, and returns once sample traffic is flowing: an hour of history from
four services, then a few requests a second, so Live Tail streams. The demo project at
`/p/00000000-0000-0000-0000-000000000000/` needs no login. Server-rendered markup (`src/Pages`)
stays the guest's build, so a change there is unverified, not disproved. The CSS is rebuilt from
this checkout, so a class this pull request removes from `src/` can unstyle the guest's markup.
Run it once per VM.

Command: write the Playwright script into `web-components/` (so `playwright` resolves; the file is
untracked) with a quoted heredoc, so nothing in it needs escaping, then run it. Record the whole
window, sidebar included, at 1280x800:

```bash
cd web-components && cat > proof.mjs <<'JS'
import { chromium } from 'playwright';
import { writeFileSync } from 'node:fs';
const out = process.env.PROOF_MEDIA, t0 = Date.now(), chapters = [];
const step = (label) => { chapters.push(((Date.now() - t0) / 1000).toFixed(1) + '\t' + label); console.log('== ' + label); };
const browser = await chromium.launch();
const context = await browser.newContext({ viewport: { width: 1280, height: 800 }, recordVideo: { dir: out, size: { width: 1280, height: 800 } } });
const page = await context.newPage();
await page.goto('http://127.0.0.1:8080/p/00000000-0000-0000-0000-000000000000/log_explorer');
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
1280px.png`), never for the outcome you expect; crop them to the part that changed.

For a state the sample traffic cannot produce, `web-components/bench/` mounts one component on
synthetic data without the server (`cd web-components && npx vite --config
bench/live-tail.vite.config.ts --port 3098`). It has no sidebar, so subtract the app's sidebar from
the viewport to match a window width.
