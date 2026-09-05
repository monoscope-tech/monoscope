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

// Read-only. aria-current confirms the client has reconciled the server markup,
// so these assertions catch a highlight that disappears after the bundle loads.
for (const [path, section] of cases) {
  test(`${path} highlights its sidebar section`, async ({ page }) => {
    await page.goto(project + path, { waitUntil: 'domcontentloaded' });
    const active = page.locator('#main-sidenav .main-nav-link[aria-current="page"]');
    await expect(active).toHaveCount(1);
    await expect(active).toHaveAttribute('href', project + section);
    await expect(active).toHaveClass(/\bactive\b/);
  });
}

test('Explorer stays highlighted through tab navigation and browser history', async ({ page }) => {
  await page.goto(project + '/metrics');
  const explorer = page.locator(`#main-sidenav .main-nav-link[href="${project}/log_explorer"]`);
  await expect(explorer).toHaveAttribute('aria-current', 'page');
  await page.locator('#main-navbar').getByRole('tab', { name: 'Live Tail', exact: true }).click();
  await expect(page).toHaveURL(/\/live_tail(?:\?|$)/);
  await expect(explorer).toHaveAttribute('aria-current', 'page');
  await page.locator('#main-navbar').getByRole('tab', { name: 'Service Map', exact: true }).click();
  await expect(page).toHaveURL(/\/service_map(?:\?|$)/);
  await expect(explorer).toHaveAttribute('aria-current', 'page');
  await page.goBack();
  await expect(page).toHaveURL(/\/live_tail(?:\?|$)/);
  await expect(explorer).toHaveAttribute('aria-current', 'page');
  await page.goForward();
  await expect(page).toHaveURL(/\/service_map(?:\?|$)/);
  await expect(explorer).toHaveAttribute('aria-current', 'page');
});
