import { describe, test, expect, beforeAll } from 'vitest';

// page-chrome.ts was ~195 lines inlined into every page by BodyWrapper. Moving it into the
// bundle only works if the functions Lucid calls from inline attributes
// (onpointerdown="navigatable(...)") are still reachable as globals — a module's top-level
// declarations are NOT globals, so a missed `Object.assign(window, …)` would break tab
// switching on every page in the app with no compile error.
describe('page-chrome globals', () => {
  beforeAll(async () => {
    document.body.innerHTML = '<div id="root"></div>';
    await import('../src/page-chrome');
  });

  test('publishes the functions inline Lucid attributes call', () => {
    for (const fn of ['navigatable', 'setCookie', 'getCookie', 'filterByField', 'viewFieldPatterns']) {
      expect(typeof (window as any)[fn], `window.${fn}`).toBe('function');
    }
  });

  test('navigatable activates the clicked tab, reveals its panel and hides the others', async () => {
    document.body.innerHTML = `
      <div id="tabs">
        <button class="a-tab" id="t1"></button>
        <button class="a-tab t-active" id="t2"></button>
        <div class="a-tab-content" id="p1"></div>
        <div class="a-tab-content hidden" id="p2"></div>
      </div>`;
    const t1 = document.getElementById('t1')!;
    let sawTabVisible = false;
    document.getElementById('p2')!.addEventListener('tab-visible', () => { sawTabVisible = true; });

    (window as any).navigatable(t1, '#p2', '#tabs', 't-active');
    // The DOM writes are batched into a frame.
    await new Promise(r => requestAnimationFrame(() => r(null)));

    expect(t1.classList.contains('t-active')).toBe(true);
    expect(document.getElementById('t2')!.classList.contains('t-active')).toBe(false);
    expect(document.getElementById('p2')!.classList.contains('hidden')).toBe(false);
    expect(document.getElementById('p1')!.classList.contains('hidden')).toBe(true);
    // The trace view's timeline and service map both init lazily off this event.
    expect(sawTabVisible).toBe(true);
  });

  test('cookies round-trip through the helpers', () => {
    (window as any).setCookie('theme', 'dark');
    expect((window as any).getCookie('theme')).toBe('dark');
    expect((window as any).getCookie('nope')).toBe('');
  });
});
