import { describe, test, expect, beforeAll, vi } from 'vitest';

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

test('trace hover and keyboard focus display a fresh copy of the server-rendered card', async () => {
  document.body.innerHTML = `<div class="waterfall-row" tabindex="0">
    <div data-tippy-theme="trace-span"><span>query</span>
      <template data-trace-tooltip><div><p>SELECT &lt;unsafe&gt;</p></div></template>
    </div>
  </div>`;
  const instance = { show: vi.fn(), hide: vi.fn(), destroy: vi.fn() };
  const tippy = vi.fn((_element: Element, _options: { content: HTMLElement; triggerTarget: Element; interactive: boolean }) => instance);
  Object.assign(window, { tippy });
  try {
    await import('../src/page-chrome');
    if (document.readyState === 'loading') document.dispatchEvent(new Event('DOMContentLoaded'));
    const row = document.querySelector<HTMLElement>('.waterfall-row')!;
    const target = row.querySelector('span')!;
    const template = row.querySelector('template')!;
    target.dispatchEvent(new MouseEvent('mouseover', { bubbles: true }));
    const options = tippy.mock.calls[0][1];
    expect(options.content.textContent).toBe('SELECT <unsafe>');
    expect(options.content).not.toBe(template.content.firstElementChild);
    expect(options.triggerTarget).toBe(row);
    expect(options.interactive).toBe(true);
    expect(instance.show).toHaveBeenCalledOnce();
    row.dispatchEvent(new FocusEvent('focusin', { bubbles: true }));
    expect(tippy).toHaveBeenCalledTimes(2);
    expect(instance.destroy).toHaveBeenCalledOnce();
    row.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true }));
    expect(instance.hide).toHaveBeenCalledOnce();
  } finally {
    document.body.dispatchEvent(new Event('htmx:before:swap', { bubbles: true }));
    delete (window as any).tippy;
    document.body.innerHTML = '';
  }
});
