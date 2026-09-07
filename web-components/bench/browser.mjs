import { chromium } from 'playwright';
import { writeFileSync, mkdirSync } from 'node:fs';
const browser = await chromium.launch({ headless: true });
const results = [];
const p95 = (xs) => xs.sort((a, b) => a - b)[Math.floor(xs.length * 0.95)] || 0;
try {
  for (const engine of ['textarea', 'baseline', 'monaco', 'codemirror'])
    for (const slowdown of [1, 4]) {
      const page = await browser.newPage();
      const cdp = await page.context().newCDPSession(page);
      await cdp.send('Emulation.setCPUThrottlingRate', { rate: slowdown });
      await cdp.send('Performance.enable');
      const errors = [];
      page.on('pageerror', (e) => errors.push(e.message));
      await page.goto(`http://127.0.0.1:3099/bench/editor.html?engine=${engine}&fields=1000`);
      await page.waitForFunction(() => window.bench, null, { timeout: 30000 });
      const coldCompletionMs = await page.evaluate(async () => {
        const start = performance.now();
        window.bench.focus();
        await window.bench.complete();
        return performance.now() - start;
      });
      const warmBackendMs = await page.evaluate(async () => {
        const times = [];
        for (let i = 0; i < 12; i++) {
          const start = performance.now();
          await window.bench.backend('stat');
          if (i > 1) times.push(performance.now() - start);
        }
        return times;
      });
      await page.evaluate(() => {
        window.bench.setValue('');
        window.bench.focus();
        window.bench.longTasks.length = 0;
        window.samples = [];
        document.addEventListener(
          'keydown',
          () => {
            const start = performance.now();
            requestAnimationFrame(() => requestAnimationFrame(() => window.samples.push(performance.now() - start)));
          },
          true
        );
      });
      if (process.env.QUERY_BENCH_TRACE) {
        await cdp.send('Profiler.enable');
        await cdp.send('Profiler.start');
      }
      const before = await cdp.send('Performance.getMetrics');
      await page.keyboard.type('status_code == "ERROR" and level == "WARN"', { delay: 50 });
      await page.waitForTimeout(250);
      if (process.env.QUERY_BENCH_TRACE) {
        const { profile } = await cdp.send('Profiler.stop');
        mkdirSync('bench/traces', { recursive: true });
        writeFileSync(`bench/traces/${engine}-${slowdown}.cpuprofile`, JSON.stringify(profile));
      }
      const after = await cdp.send('Performance.getMetrics');
      const data = await page.evaluate(() => ({
        startupMs: window.bench.startupMs,
        samples: window.samples,
        longTasks: window.bench.longTasks,
        value: window.bench.getValue(),
      }));
      const metrics = (list) => Object.fromEntries(list.metrics.map((m) => [m.name, m.value]));
      const a = metrics(after),
        b = metrics(before);
      const result = {
        engine,
        slowdown,
        coldCompletionMs,
        warmBackendP95Ms: p95(warmBackendMs),
        startupMs: data.startupMs,
        inputToTwoFramesP95Ms: p95(data.samples),
        longestTaskMs: Math.max(0, ...data.longTasks),
        layoutCount: a.LayoutCount - b.LayoutCount,
        heapBytes: a.JSHeapUsedSize,
        errors,
        value: data.value,
      };
      results.push(result);
      console.log(JSON.stringify(result));
      await page.close();
    }
} finally {
  await browser.close();
}
writeFileSync('bench/browser-results.json', JSON.stringify({ runtime: process.version, results }, null, 2) + '\n');

const regressions = results.filter(
  (r) =>
    r.engine === 'codemirror' &&
    (r.inputToTwoFramesP95Ms > (r.slowdown === 1 ? 50 : 100) || r.longestTaskMs > 50 || r.warmBackendP95Ms > 100 || r.errors.length)
);
if (regressions.length) throw new Error(`Query editor performance budget exceeded: ${JSON.stringify(regressions)}`);

// Large-schema and lifecycle measurements use the shipping component, not a surrogate input.
const stressBrowser = await chromium.launch({ headless: true });
const stress = [];
try {
  for (const shape of ['flat', 'nested', 'deep']) {
    const page = await stressBrowser.newPage();
    const cdp = await page.context().newCDPSession(page);
    await cdp.send('Emulation.setCPUThrottlingRate', { rate: 4 });
    await page.goto(`http://127.0.0.1:3099/bench/editor.html?fields=100000&shape=${shape}`);
    await page.waitForFunction(() => window.bench);
    const prefix = shape === 'flat' ? '' : shape === 'nested' ? 'attributes.' : 'attributes.a.b.c.d.e.f.g.h.';
    const completionMs = await page.evaluate(async (prefix) => {
      const start = performance.now();
      await window.bench.backend(prefix + 'field_99999');
      return performance.now() - start;
    }, prefix);
    await page.evaluate(async () => {
      await window.bench.cycle(10);
    });
    await cdp.send('HeapProfiler.collectGarbage');
    const before = await cdp.send('Memory.getDOMCounters');
    await page.evaluate(async () => {
      await window.bench.cycle(100);
    });
    await page.waitForTimeout(100);
    await cdp.send('HeapProfiler.collectGarbage');
    const after = await cdp.send('Memory.getDOMCounters');
    const prototype = await cdp.send('Runtime.evaluate', { expression: 'Object.getPrototypeOf(window.bench.element)' });
    const instances = await cdp.send('Runtime.queryObjects', { prototypeObjectId: prototype.result.objectId });
    const count = await cdp.send('Runtime.callFunctionOn', {
      objectId: instances.objects.objectId,
      functionDeclaration: 'function() { return this.length; }',
      returnByValue: true,
    });
    const state = await page.evaluate(() => ({
      clients: window.bench.schemaManager.clients.size,
      subscriptions: window.bench.schemaManager.listeners.size,
      pendingRequests: window.bench.schemaManager.requests.size,
    }));
    stress.push({
      shape,
      fields: 100000,
      slowdown: 4,
      firstCompletionMs: completionMs,
      domBefore: before,
      domAfter: after,
      editorInstances: count.result.value,
      state,
    });
    if (count.result.value !== 1 || state.clients !== 1 || state.subscriptions !== 1 || state.pendingRequests !== 0)
      throw new Error('Editor lifecycle leak');
    await page.close();
  }
} finally {
  await stressBrowser.close();
}
writeFileSync('bench/stress-results.json', JSON.stringify(stress, null, 2) + '\n');
