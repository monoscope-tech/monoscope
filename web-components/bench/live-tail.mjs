// Live Tail under synthetic load: long-task time, frame rate and editor keydown→paint latency
// at 4x and 6x CPU. npm run bench:live-tail:build && npm run bench:live-tail:serve, then node bench/live-tail.mjs.
import { chromium } from 'playwright';
const base = process.argv[2] ?? 'http://127.0.0.1:3098/bench/live-tail.html';
const label = process.argv[3] ?? '';
const browser = await chromium.launch({ headless: true });
const p = (xs, q) => { const s = [...xs].sort((a, b) => a - b); return s.length ? +s[Math.min(s.length - 1, Math.floor(s.length * q))].toFixed(1) : 0; };
for (const slowdown of [4, 6]) {
  const page = await browser.newPage({ viewport: { width: 1440, height: 900 } });
  const errors = []; page.on('pageerror', (e) => errors.push(String(e.stack || e.message).slice(0, 400)));
  const cdp = await page.context().newCDPSession(page);
  await cdp.send('Emulation.setCPUThrottlingRate', { rate: slowdown });
  await page.goto(base);
  await page.waitForSelector('.cm-content', { timeout: 30000 });
  await page.waitForTimeout(3000); // fill the 1000-row buffer
  const steady = await page.evaluate(async () => {
    window.bench.longTasks.length = 0;
    let frames = 0; const t0 = performance.now(); let go = true;
    const tick = () => { frames++; if (go) requestAnimationFrame(tick); }; requestAnimationFrame(tick);
    await new Promise((r) => setTimeout(r, 8000)); go = false;
    const lt = window.bench.longTasks;
    return { fps: +(frames / ((performance.now() - t0) / 1000)).toFixed(1), longTaskMs: Math.round(lt.reduce((a, b) => a + b, 0)), longTasks: lt.length, maxTask: Math.round(Math.max(0, ...lt)) };
  });
  await page.evaluate(() => {
    window.samples = [];
    document.addEventListener('keydown', () => { const s = performance.now(); requestAnimationFrame(() => requestAnimationFrame(() => window.samples.push(performance.now() - s))); }, true);
  });
  await page.click('.cm-content');
  await page.keyboard.type('level == "error" and service == "svc-1"', { delay: 50 });
  await page.waitForTimeout(500);
  const typing = await page.evaluate(() => window.samples);
  const rows = await page.evaluate(() => document.querySelector('live-tail').rows.length);
  console.log(JSON.stringify({ label, slowdown, ...steady, typeP50: p(typing, 0.5), typeP95: p(typing, 0.95), typeMax: p(typing, 1), keys: typing.length, rows, errors: errors.slice(0, 3) }));
  await page.close();
}
await browser.close();
