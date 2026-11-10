# Proving changes in this repository

Reviews run offline in a VM. The Haskell server cannot be built there within a review's budget, so prove
what runs without it: CLI and script behaviour in the terminal, and UI changes to the web components in a
browser.

## UI changes in `web-components/`

`web-components/bench/live-tail.html` mounts the real Live Tail (log list, query editor, toolbar) on
synthetic logs; add a page beside it to mount another component the same way. The guest ships the
dependencies at `/opt/proof`.

Setup (unrecorded; it links gitignored paths only):

```bash
ln -sfn /opt/proof/node_modules node_modules
ln -sfn /opt/proof/web-components/node_modules web-components/node_modules
./node_modules/.bin/tailwindcss -i static/public/assets/css/tailwind.css -o static/public/assets/css/tailwind.min.css >/tmp/tailwind.log 2>&1
cd web-components && (npx vite --config bench/live-tail.vite.config.ts --host 127.0.0.1 --port 3098 --strictPort >/tmp/vite.log 2>&1 &)
for i in $(seq 1 120); do curl -sf -o /dev/null http://127.0.0.1:3098/bench/live-tail.html && break; sleep 1; done
```

Command: write the Playwright script into `web-components/` (so `playwright` resolves; the file is
untracked) with a quoted heredoc, so nothing in it needs escaping, then run it. It records the
interaction and writes to `$PROOF_MEDIA`:

```bash
cd web-components && cat > proof.mjs <<'JS'
import { chromium } from 'playwright';
import { writeFileSync } from 'node:fs';
const out = process.env.PROOF_MEDIA, t0 = Date.now(), chapters = [];
const step = (label) => { chapters.push(((Date.now() - t0) / 1000).toFixed(1) + '\t' + label); console.log('== ' + label); };
const browser = await chromium.launch();
const context = await browser.newContext({ viewport: { width: 1280, height: 800 }, recordVideo: { dir: out, size: { width: 1280, height: 800 } } });
const page = await context.newPage();
step('Open Live Tail');
await page.goto('http://127.0.0.1:3098/bench/live-tail.html?frames=1&fields=50');
await page.waitForTimeout(4000);
await page.screenshot({ path: `${out}/Live Tail toolbar.png`, clip: { x: 0, y: 0, width: 1280, height: 160 } });
// …the steps that show the change, each behind step('…'), then an assertion that exits 1 on the base.
await context.close(); await browser.close();
writeFileSync(`${out}/chapters.tsv`, chapters.join('\n') + '\n');
JS
node proof.mjs
```

Name screenshots for what they show; crop them to the part that changed. Keep the whole run under a minute.
