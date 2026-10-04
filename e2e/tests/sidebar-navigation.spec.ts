import { test, expect } from '@playwright/test';
import { DEMO_PROJECT, makeDashboard, deleteDashboard } from './helpers';

const project = `/p/${DEMO_PROJECT}`;
const cases = [
  ['/live_tail', '/log_explorer'],
  ['/metrics', '/log_explorer'],
  ['/service_map', '/log_explorer'],
  ['/infrastructure/containers', '/infrastructure/hosts'],
  ['/infrastructure/images', '/infrastructure/hosts'],
  ['/infrastructure/kubernetes', '/infrastructure/hosts'],
  ['/infrastructure/host-map', '/infrastructure/hosts'],
  ['/endpoints', '/api_catalog'],
  ['/apis', '/settings'],
  ['/manage_members', '/settings'],
  ['/manage_billing', '/settings'],
  ['/dashboards', '/dashboards'],
  ['/issues', '/issues'],
  ['/rum', '/rum'],
  ['/monitors', '/monitors'],
  ['/reports', '/reports'],
];

// These pages must identify the active section without application JavaScript.
test.describe('server-rendered sidebar', () => {
  test.use({ javaScriptEnabled: false });
  for (const [path, section] of cases) {
    test(`${path} highlights its sidebar section`, async ({ page }) => {
      await page.goto(project + path, { waitUntil: 'domcontentloaded' });
      const active = page.locator('#main-sidenav .main-nav-link[aria-current="page"]');
      await expect(active).toHaveCount(1);
      await expect(active).toHaveAttribute('href', project + section);
      await expect(active).toHaveClass(/\bactive\b/);
    });
  }
});

test('Explorer stays highlighted through tab navigation and browser history', async ({ page }) => {
  await page.goto(project + '/metrics');
  const explorer = page.locator(`#main-sidenav .main-nav-link[href="${project}/log_explorer"]`);
  const explorerViews = page.getByRole('navigation', { name: 'Explorer views' });
  await expect(explorer).toHaveAttribute('aria-current', 'page');
  await explorerViews.getByRole('link', { name: 'Live Tail', exact: true }).click();
  await expect(page).toHaveURL(/\/live_tail(?:\?|$)/);
  await expect(explorer).toHaveAttribute('aria-current', 'page');
  await explorerViews.getByRole('link', { name: 'Service Map', exact: true }).click();
  await expect(page).toHaveURL(/\/service_map(?:\?|$)/);
  await expect(explorer).toHaveAttribute('aria-current', 'page');
  await page.goBack();
  await expect(page).toHaveURL(/\/live_tail(?:\?|$)/);
  await expect(explorer).toHaveAttribute('aria-current', 'page');
  await page.goForward();
  await expect(page).toHaveURL(/\/service_map(?:\?|$)/);
  await expect(explorer).toHaveAttribute('aria-current', 'page');
});

// Regressions: the palette's own lazy fetch closed it on the first Cmd+K, the fuzzy filter
// hid the Ask AI row, and Ask AI navigated with the query alone, dropping its time range.
test('command palette Ask AI opens the explorer with the AI time range', async ({ page }) => {
  await page.route('**/log_explorer/ai_search', (route) =>
    route.fulfill({ contentType: 'application/json', body: JSON.stringify({ query: 'level == "ERROR"', time_range: { since: '1H' } }) }));
  await page.goto(project + '/dashboards');
  await page.locator('body').press('Control+k');
  await page.locator('#cmd-palette-input').fill('errors from checkout in the last hour');
  await page.locator('[data-ai-action]').click();
  await expect(page).toHaveURL(/\/log_explorer\?.*since=1H/);
  expect(new URL(page.url()).searchParams.get('query')).toBe('level == "ERROR"');
});

// Regression: morphing a dashboard's deeper-nested navbar into Explorer's showed the closed time-range list inline.
test('morphing from a dashboard never lays out the closed time picker inline', async ({ page }) => {
  const dash = await makeDashboard(page, `E2E Navbar Morph ${Date.now()}`);
  try {
    await page.evaluate(() => {
      const w = window as any;
      w.leaked = '';
      new MutationObserver(() => {
        const el = document.getElementById('n-timepicker-popover');
        const display = el && !el.matches(':popover-open') && getComputedStyle(el).display;
        if (display && display !== 'none') w.leaked ||= display;
      }).observe(document.documentElement, { subtree: true, childList: true, attributes: true });
    });
    await page.locator(`#main-sidenav .nav-flyout a[href^="${project}/log_explorer"]`).first().evaluate((a: HTMLElement) => a.click());
    await expect(page.locator('#log-explorer-all-traces')).toBeVisible();
    expect(await page.evaluate(() => (window as any).leaked)).toBe('');
  } finally {
    await deleteDashboard(page, dash);
  }
});

test('time picker releases its document click handler when HTMX removes it', async ({ page }) => {
  const errors: string[] = [];
  page.on('pageerror', error => errors.push(error.message));
  await page.goto(project + '/metrics');
  await expect.poll(() => page.evaluate(() => Boolean((window as any)['n-picker']))).toBe(true);
  await page.locator('#n-timepicker-root').evaluate(element => {
    element.dispatchEvent(new Event('htmx:beforeCleanupElement', { bubbles: true }));
    element.remove();
  });
  await page.locator('body').click();
  expect(errors).toEqual([]);
  expect(await page.evaluate(() => (window as any)['n-picker'])).toBeUndefined();
});

test('time picker load retry stops after its root is removed', async ({ page }) => {
  const errors: string[] = [];
  page.on('pageerror', error => errors.push(error.message));
  let release!: () => void;
  const blocked = new Promise<void>(resolve => { release = resolve; });
  await page.route('**/deps/easepick/**', async route => {
    await blocked;
    await route.continue();
  });
  await page.goto(project + '/metrics', { waitUntil: 'commit' });
  await page.locator('#n-timepicker-root').waitFor({ state: 'attached' });
  expect(await page.evaluate(() => typeof (window as any).easepick)).toBe('undefined');
  await page.locator('#n-timepicker-root').evaluate(element => element.remove());
  release();
  await expect.poll(() => page.evaluate(() => typeof (window as any).easepick)).toBe('object');
  await page.waitForTimeout(200);
  expect(errors).toEqual([]);
});
