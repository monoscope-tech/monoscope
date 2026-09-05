import { test, expect } from '@playwright/test';
import { execFileSync } from 'node:child_process';
import { DEMO_PROJECT } from './helpers';

const url = `/p/${DEMO_PROJECT}/metrics`;
// These fixtures only use the disposable database owned by scripts/e2e.sh.
function sql(query: string) {
  execFileSync('psql', ['-h', process.env.E2E_PGHOST ?? process.env.DB_HOST ?? 'localhost',
    '-p', process.env.E2E_PGPORT ?? process.env.DB_PORT ?? '5432', '-U', 'postgres',
    '-d', process.env.E2E_DB ?? 'monoscope_e2e', '-v', 'ON_ERROR_STOP=1', '-c', query],
  { env: { ...process.env, PGPASSWORD: process.env.E2E_PGPASSWORD ?? 'postgres' }, stdio: 'pipe' });
}
const cleanup = `DELETE FROM otel_metrics WHERE project_id='${DEMO_PROJECT}' AND metric_name LIKE 'uxcatalog%';
DELETE FROM otel_metrics_meta WHERE project_id='${DEMO_PROJECT}' AND metric_name LIKE 'uxcatalog%';`;

test.describe('metrics catalog', () => {
  test.describe.configure({ mode: 'default' });
  test.beforeAll(() => {
    sql(cleanup + `
      INSERT INTO otel_metrics_meta (project_id,metric_name,metric_type,metric_unit,metric_description,
        service_name,scope_name,metric_labels,first_seen_at,last_seen_at,first_timestamp,last_timestamp)
      SELECT '${DEMO_PROJECT}', 'uxcatalog' || family || '.metric.' || lpad(n::text,2,'0'),
        'GAUGE','1','Catalog browser fixture', 'ux-checkout','e2e',ARRAY['attributes.region'],now(),now(),now(),now()
      FROM unnest(ARRAY['db','runtime']) family CROSS JOIN generate_series(1,24) n;
      INSERT INTO otel_metrics (project_id,id,series_id,timestamp,metric_name,metric_type,metric_unit,value,resource,resource___service___name)
      SELECT '${DEMO_PROJECT}', gen_random_uuid(), metric_name, now() - n * interval '1 minute',
        metric_name,'GAUGE','1',n,'{"service":{"name":"ux-checkout"}}'::jsonb,'ux-checkout'
      FROM otel_metrics_meta CROSS JOIN generate_series(1,20) n
      WHERE project_id='${DEMO_PROJECT}' AND metric_name LIKE 'uxcatalog%';`);
  });
  test.afterAll(() => sql(cleanup));

  test('search filters every page and survives view changes and reload', async ({ page }) => {
    await page.goto(url + '?q=uxcatalog&since=6H');
    await page.getByRole('searchbox', { name: 'Search metrics', exact: true }).fill('uxcatalogruntime');
    await page.locator('#metric-filters').getByRole('button', { name: 'Search', exact: true }).click();
    await expect(page).toHaveURL(/q=uxcatalogruntime/);
    await expect(page.locator('#metrics-toolbar')).toContainText('24 matching metrics');
    await expect(page.locator('.metric_filterble')).toHaveCount(12);
    await page.locator('#metric-pagination').scrollIntoViewIfNeeded();
    await expect(page.locator('.metric_filterble')).toHaveCount(24);
    expect(await page.locator('.metric_filterble').evaluateAll(cards => cards.every(c => c.id.includes('uxcatalogruntime')))).toBe(true);
    await expect(page.getByText('All matching metrics loaded.')).toBeVisible();
    await page.goBack();
    await expect(page.locator('#search-input')).toHaveValue('uxcatalog');
    await page.goForward();
    await expect(page.locator('#search-input')).toHaveValue('uxcatalogruntime');
    await page.getByRole('tab', { name: 'Table', exact: true }).click();
    await expect(page).toHaveURL(/q=uxcatalogruntime/);
    expect(new URL(page.url()).searchParams.get('since')).toBe('6H');
    await expect(page.locator('#search-input')).toHaveValue('uxcatalogruntime');
    await page.reload();
    await expect(page.locator('#search-input')).toHaveValue('uxcatalogruntime');
    await page.getByRole('tab', { name: 'Charts', exact: true }).click();
    await expect(page.locator('#metrics-toolbar')).toContainText('24 matching metrics');
  });

  test('scroll loads ahead, preserves position, and keeps filters visible', async ({ page }) => {
    await page.setViewportSize({ width: 1280, height: 720 });
    await page.goto(url + '?q=uxcatalog');
    await expect(page.locator('.metric_filterble')).toHaveCount(12);
    // Hold the actual server response to verify the loading and fallback controls.
    let release!: () => void;
    const gate = new Promise<void>(resolve => { release = resolve; });
    await page.route('**/metrics?**append=true', async route => { await gate; await route.continue(); });
    await page.locator('#main-content').evaluate(el => { el.scrollTop = 420; });
    await expect(page.locator('#metric-pagination')).toHaveClass(/htmx-request/);
    const before = await page.locator('#main-content').evaluate(el => el.scrollTop);
    await expect(page.locator('#metrics-toolbar')).toBeInViewport();
    release();
    await expect(page.locator('.metric_filterble')).toHaveCount(24);
    const after = await page.locator('#main-content').evaluate(el => el.scrollTop);
    expect(Math.abs(after - before)).toBeLessThan(5);
    await expect(page.locator('#metrics-toolbar')).toBeInViewport();
  });

  test('service choices work by keyboard and groups include later catalog pages', async ({ page }) => {
    await page.goto(url + '?q=uxcatalog');
    await expect(page.getByRole('combobox', { name: 'Filter by metric group' }).locator('option[value="uxcatalogruntime."]')).toHaveCount(1);
    const servicePicker = page.locator('#metric-service-picker summary');
    await servicePicker.focus();
    await page.keyboard.press('Enter');
    await page.getByRole('searchbox', { name: 'Search services', exact: true }).fill('ux-checkout');
    const radio = page.getByRole('radio', { name: 'ux-checkout', exact: true });
    await expect(radio).toBeVisible();
    await radio.focus();
    await page.keyboard.press('Space');
    await servicePicker.focus();
    await page.keyboard.press('Enter');
    await page.locator('#metric-filters').getByRole('button', { name: 'Search', exact: true }).click();
    await expect(page).toHaveURL(/metric_source=ux-checkout/);
    await expect(servicePicker).toContainText('ux-checkout');
    await expect.poll(() => page.evaluate(() => {
      const el = document.querySelector('[data-chart-widget]');
      return (window as any).echarts?.getInstanceByDom(el)?.getOption()?.dataset?.[0]?.source?.length ?? 0;
    }), { timeout: 20000 }).toBeGreaterThan(1);
    const firstCard = page.locator('.metric_filterble').first();
    await firstCard.locator('details summary').click();
    await firstCard.getByRole('button', { name: 'attributes.region', exact: true }).click();
    await expect(firstCard).toContainText('attributes.region');
    await expect.poll(() => page.evaluate(() => {
      const el = document.querySelector('[data-chart-widget]');
      return (window as any).echarts?.getInstanceByDom(el)?.getOption()?.dataset?.[0]?.source?.length ?? 0;
    }), { timeout: 20000 }).toBeGreaterThan(1);
  });

  test('empty search offers recovery and preserves literal special characters', async ({ page }) => {
    await page.goto(url + '?q=uxcatalog');
    await page.locator('#search-input').fill('missing%_&+');
    await page.locator('#metric-filters').getByRole('button', { name: 'Search', exact: true }).click();
    await expect(page.getByText('No metrics match these filters')).toBeVisible();
    expect(new URL(page.url()).searchParams.get('q')).toBe('missing%_&+');
    await expect(page.locator('.metric_filterble')).toHaveCount(0);
    await page.getByRole('link', { name: 'Clear filters', exact: true }).first().click();
    await expect(page.locator('#search-input')).toHaveValue('');
  });

  test('failed automatic pagination has a working manual retry', async ({ page }) => {
    await page.goto(url + '?q=uxcatalog');
    await page.route('**/metrics?**append=true', route => route.fulfill({ status: 503, body: 'Temporarily unavailable' }), { times: 1 });
    const failed = page.waitForResponse(r => r.url().includes('append=true') && r.status() === 503);
    await page.locator('#metric-pagination').scrollIntoViewIfNeeded();
    await failed;
    const more = page.getByRole('link', { name: 'Load more metrics' });
    await expect(more).toBeVisible();
    await more.click();
    await expect(page.locator('.metric_filterble')).toHaveCount(24);
  });

  test('charts render real data without misleading total badges', async ({ page }) => {
    await page.goto(url + '?q=uxcatalogruntime.metric.01');
    await expect.poll(() => page.evaluate(() => {
      const el = document.querySelector('[data-chart-widget]');
      const chart = (window as any).echarts?.getInstanceByDom(el);
      return chart?.getOption()?.dataset?.[0]?.source?.length ?? 0;
    }), { timeout: 20000 }).toBeGreaterThan(1);
    await expect(page.locator('.metric_filterble [id$="Value"]:visible')).toHaveCount(0);
    await expect(page.locator('.metric_filterble')).toContainText('All values');
  });

  test('a new search cancels an older scroll request', async ({ page }) => {
    await page.setViewportSize({ width: 1280, height: 720 });
    await page.goto(url + '?q=uxcatalog');
    let release!: () => void;
    const gate = new Promise<void>(resolve => { release = resolve; });
    await page.route('**/metrics?**append=true', async route => { await gate; await route.continue().catch(() => {}); });
    const pending = page.waitForRequest(r => r.url().includes('append=true'));
    await page.locator('#main-content').evaluate(el => { el.scrollTop = 420; });
    await pending;
    await page.locator('#search-input').fill('uxcatalogruntime.metric.01');
    await page.locator('#metric-filters').getByRole('button', { name: 'Search', exact: true }).click();
    await expect(page.locator('.metric_filterble')).toHaveCount(1);
    release();
    await expect(page.locator('.metric_filterble')).toHaveCount(1);
    await expect(page.locator('.metric_filterble')).toHaveAttribute('id', /uxcatalogruntime_metric_01$/);
  });

  test('mobile toolbar fits and remains usable in both themes', async ({ page }) => {
    await page.setViewportSize({ width: 390, height: 844 });
    for (const colorScheme of ['light', 'dark'] as const) {
      await page.emulateMedia({ colorScheme });
      await page.goto(url + '?q=uxcatalog');
      await expect(page.locator('body')).toHaveAttribute('data-theme', colorScheme);
      expect(await page.evaluate(() => document.body.scrollWidth)).toBeLessThanOrEqual(390);
      await page.locator('#main-content').evaluate(el => { el.scrollTop = 500; });
      await expect(page.locator('#search-input')).toBeInViewport();
      await expect(page.locator('#metric-filters').getByRole('button', { name: 'Search', exact: true })).toBeInViewport();
    }
  });

  test('search and pagination work without JavaScript', async ({ browser }) => {
    const context = await browser.newContext({ javaScriptEnabled: false });
    const page = await context.newPage();
    await page.goto((test.info().project.use.baseURL as string) + url + '?q=uxcatalog');
    await page.locator('#search-input').fill('uxcatalogruntime');
    await page.locator('#metric-filters').getByRole('button', { name: 'Search', exact: true }).click();
    await expect(page.locator('.metric_filterble')).toHaveCount(12);
    await page.getByRole('link', { name: 'Load more metrics' }).click();
    await expect(page.locator('#main-content')).toBeVisible();
    await expect(page.locator('.metric_filterble')).toHaveCount(12);
    await expect(page.locator('.metric_filterble').first()).toHaveAttribute('id', /13$/);
    await context.close();
  });
});
