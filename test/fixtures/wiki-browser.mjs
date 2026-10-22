import { chromium } from '../../e2e/node_modules/playwright/index.mjs';
import { fileURLToPath } from 'node:url';
import { existsSync } from 'node:fs';
import assert from 'node:assert/strict';
if (process.argv[2] === '--probe') process.stdout.write(JSON.stringify(existsSync(chromium.executablePath())));
else {
  const browser = await chromium.launch({ headless: true });
  try {
    const page = await browser.newPage({ extraHTTPHeaders: { Authorization: `Bearer ${process.env.MONOSCOPE_TEST_BEARER}` } });
    const url = process.argv[2], origin = new URL(url).origin;
    const assets = [];
    let releaseEditor;
    const editorLoading = new Promise(resolve => { releaseEditor = resolve; });
    const load = async url => {
      await page.goto(url);
      if (assets.length) throw new Error(assets.join('\n'));
      await page.waitForFunction(() => !!window.htmx);
    };
    await page.route('**/*', async route => {
      const target = new URL(route.request().url());
      if (/^\/public\/assets\/deps\/htmx\/[\w.-]+\.js$/.test(target.pathname))
        return route.fulfill({ path: fileURLToPath(new URL('../../static' + target.pathname.replace(/\.[a-f0-9]{8}\.js$/, '.js'), import.meta.url)), contentType: 'application/javascript' }).catch(error => { assets.push(error.message); return route.abort(); });
      if (target.origin === origin && target.pathname.startsWith('/public/assets/')) {
        if (/\/wiki-editor\.[^/]+\.js$/.test(target.pathname)) await editorLoading;
        const path = fileURLToPath(new URL('../../static' + target.pathname, import.meta.url));
        const asset = existsSync(path) ? path : path.replace(/\.[a-f0-9]{8}(?=\.[^.]+$)/, '');
        return existsSync(asset) ? route.fulfill({ path: asset }) : route.abort();
      }
      return target.origin === origin && (target.pathname.includes('/memories') || target.pathname.includes('/wiki')) ? route.continue() : route.abort();
    });
    await load(url);
    await page.getByLabel('Page path (required)').fill('runbooks/retries');
    await page.getByLabel('Title (required)').fill('Browser memory');
    const initial = 'Typed while the editor loads.';
    await page.locator('textarea[name=body]').fill(initial);
    releaseEditor();
    const rich = page.locator('wiki-editor[data-mode=rich] .ProseMirror');
    await rich.waitFor();
    assert.equal(await rich.textContent(), initial);
    assert.ok(await rich.evaluate(element => element.contains(document.activeElement)));
    await rich.fill('Keep caller retry tokens.');
    await rich.press('ControlOrMeta+z');
    await page.waitForFunction(text => document.querySelector('textarea[name=body]').value === text, initial);
    await rich.press('ControlOrMeta+Shift+z');
    await rich.press('ControlOrMeta+a');
    await rich.press('ControlOrMeta+b');
    await page.getByLabel('Editing mode').selectOption('source');
    assert.equal(await page.locator('textarea[name=body]').inputValue(), '**Keep caller retry tokens.**\n');
    await page.getByLabel('Editing mode').selectOption('rich');
    assert.equal(await rich.locator('strong').textContent(), 'Keep caller retry tokens.');
    const id = await page.locator('[name=pageId]').inputValue();
    await page.getByRole('button', { name: 'Save page', exact: true }).click();
    await page.waitForFunction(() => document.querySelector('#repository-content h2')?.textContent === 'Browser memory');
    const urls = [new URL(page.url()).pathname];
    const targets = [await page.locator('#repository-content').count()];
    await load(url);
    await page.getByRole('link', { name: 'retries', exact: true }).click();
    await page.waitForFunction(() => document.querySelector('#repository-content h2')?.textContent === 'Browser memory');
    targets.push(await page.locator('#repository-content').count());
    await load(url + '?page=' + id);
    await page.getByRole('link', { name: 'New page', exact: true }).click();
    await page.waitForFunction(() => document.querySelector('#repository-content h2')?.textContent === 'New wiki page');
    targets.push(await page.locator('#repository-content').count());
    urls.push(new URL(page.url()).pathname);
    await page.locator('wiki-editor[data-mode=rich] .ProseMirror').waitFor();
    process.stdout.write(JSON.stringify({ targets, urls, fields: [await page.getByLabel('Page path (required)').inputValue(), await page.getByLabel('Title (required)').inputValue(), await page.locator('textarea[name=body]').inputValue()] }));
  } finally { await browser.close(); }
}
