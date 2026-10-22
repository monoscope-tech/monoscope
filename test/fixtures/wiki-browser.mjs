import { chromium } from '../../e2e/node_modules/playwright/index.mjs';
import { fileURLToPath } from 'node:url';
import { existsSync } from 'node:fs';
if (process.argv[2] === '--probe') process.stdout.write(JSON.stringify(existsSync(chromium.executablePath())));
else {
  const browser = await chromium.launch({ headless: true });
  try {
    const page = await browser.newPage({ extraHTTPHeaders: { Authorization: `Bearer ${process.env.MONOSCOPE_TEST_BEARER}` } });
    const url = process.argv[2], origin = new URL(url).origin;
    const assets = [];
    const load = async url => {
      await page.goto(url);
      if (assets.length) throw new Error(assets.join('\n'));
      await page.waitForFunction(() => !!window.htmx);
    };
    await page.route('**/*', async route => {
      const target = new URL(route.request().url());
      if (/^\/public\/assets\/deps\/htmx\/[\w.-]+\.js$/.test(target.pathname))
        return route.fulfill({ path: fileURLToPath(new URL('../../static' + target.pathname.replace(/\.[a-f0-9]{8}\.js$/, '.js'), import.meta.url)), contentType: 'application/javascript' }).catch(error => { assets.push(error.message); return route.abort(); });
      return target.origin === origin && target.pathname.includes('/memories') ? route.continue() : route.abort();
    });
    await load(url);
    await page.getByLabel('Title (required)').fill('Browser memory');
    await page.getByLabel('Page content (required)').fill('Keep caller retry tokens.');
    const id = await page.locator('[name=pageId]').inputValue();
    await page.getByRole('button', { name: 'Save page', exact: true }).click();
    await page.waitForFunction(() => document.querySelector('#repository-content h2')?.textContent === 'Browser memory');
    const targets = [await page.locator('#repository-content').count()];
    await load(url);
    await page.getByRole('link', { name: 'Browser memory', exact: true }).click();
    await page.waitForFunction(() => document.querySelector('#repository-content h2')?.textContent === 'Browser memory');
    targets.push(await page.locator('#repository-content').count());
    await load(url + '?page=' + id);
    await page.getByRole('link', { name: 'New page', exact: true }).click();
    await page.waitForFunction(() => document.querySelector('#repository-content h2')?.textContent === 'New wiki page');
    targets.push(await page.locator('#repository-content').count());
    process.stdout.write(JSON.stringify({ targets, fields: [await page.getByLabel('Title (required)').inputValue(), await page.getByLabel('Page content (required)').inputValue()] }));
  } finally { await browser.close(); }
}
