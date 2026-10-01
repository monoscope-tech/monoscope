import { test, expect } from '@playwright/test';
import { DEMO_PROJECT } from './helpers';

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
