import { describe, test, expect, beforeAll } from 'vitest';

// page-chrome.ts was ~195 lines inlined into every page by BodyWrapper. Moving it into the
// bundle only works if the functions Lucid calls from inline attributes remain reachable
// as globals — a module's top-level declarations are NOT globals, so a missed
// `Object.assign(window, …)` would break field actions with no compile error.
describe('page-chrome globals', () => {
  beforeAll(async () => {
    document.body.innerHTML = '<div id="root"></div>';
    await import('../src/page-chrome');
  });

  test('publishes the functions inline Lucid attributes call', () => {
    // setCookie/getCookie are NOT here: they stay inline in BodyWrapper because the theme
    // script calls getCookie mid-parse, before this deferred module runs.
    for (const fn of ['filterByField', 'viewFieldPatterns']) {
      expect(typeof (window as any)[fn], `window.${fn}`).toBe('function');
    }
  });
});
