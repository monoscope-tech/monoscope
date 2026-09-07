import { test, expect } from '@playwright/test';
const ready = async (page: any, fields = 1000) => {
  await page.goto(`/bench/editor.html?fields=${fields}`);
  await page.waitForFunction(() => (window as any).bench);
};
const input = (page: any) => page.locator('.cm-content');
const set = async (page: any, text: string) =>
  page.evaluate((text) => {
    (window as any).bench.setValue(text);
    (window as any).bench.focus();
  }, text);

test('click opens the compact custom dropdown and reopens it after Escape', async ({ page }) => {
  await page.setViewportSize({ width: 1600, height: 1100 });
  await ready(page);
  await page.locator('query-editor').evaluate(el => { el.style.width = '1400px'; });
  await input(page).click();
  const popup = page.locator('.query-completion-dropdown');
  await expect(popup).toBeVisible();
  await expect(popup.getByText('Type a field name, then an operator and value', { exact: false })).toBeVisible();
  await expect(popup.getByText('More Fields', { exact: true })).toBeVisible();
  expect(await popup.locator('completion-section').first().evaluate(el => getComputedStyle(el).paddingTop)).toBe('6px');
  await expect(popup.getByRole('link', { name: 'Syntax guide ↗' })).toBeVisible();
  await expect(popup.locator('.query-completion-help')).toContainText('to navigate');
  const editorBox = await page.locator('.cm-editor').boundingBox();
  const popupBox = await popup.boundingBox();
  expect(Math.abs(popupBox!.width - 640)).toBeLessThanOrEqual(2);
  const optionBox = await popup.getByRole('option').first().boundingBox();
  expect(Math.abs(optionBox!.width - popupBox!.width)).toBeLessThanOrEqual(4);
  expect(popupBox!.y).toBeGreaterThanOrEqual(editorBox!.y + editorBox!.height);
  await page.keyboard.press('Escape');
  await expect(popup).toHaveCount(0);
  await input(page).click();
  await expect(popup).toBeVisible();
  await popup.getByRole('option', { name: /status_code/ }).click();
  await expect(input(page)).toHaveText('status_code ');
  await expect(input(page)).toBeFocused();
  await expect(popup.locator('.query-completion-hint')).toBeHidden();
});

test('completion, keyboard, history, focus and multiline editing', async ({ page }) => {
  await ready(page);
  await set(page, 'stat');
  await expect(page.getByRole('option', { name: /status_code/ })).toBeVisible();
  await page.keyboard.press('ArrowDown');
  await page.keyboard.press('Enter');
  await expect(input(page)).toHaveText('status_code ');
  await expect(page.getByRole('option', { name: /==/ }).first()).toBeVisible();
  await page.keyboard.press('Escape');
  await page.keyboard.press('ControlOrMeta+Enter');
  await page.keyboard.type('level');
  await expect.poll(() => page.evaluate(() => (window as any).bench.getValue())).toContain('\nlevel');
  await page.keyboard.press('ControlOrMeta+z');
  await expect.poll(() => page.evaluate(() => (window as any).bench.getValue())).not.toContain('level');
  await page.locator('#outside').click();
  await page.locator('#outside').focus(); // WebKit does not focus buttons on pointer activation.
  await page.evaluate(() => (window as any).bench.element.handleAddQuery('kind == "logs"', true));
  await expect(page.locator('#outside')).toBeFocused();
  await set(page, '');
  await page.keyboard.type('(');
  await expect.poll(() => page.evaluate(() => (window as any).bench.getValue())).toBe('()');
  await page.keyboard.press('Backspace');
  await expect.poll(() => page.evaluate(() => (window as any).bench.getValue())).toBe('');
});

test('large schemas, project isolation, multiple editors and reconnect', async ({ page }) => {
  await ready(page, 100000);
  await set(page, 'field_99999');
  await expect(page.getByRole('option', { name: /field_99999/ })).toBeVisible();
  await page.evaluate(async () => {
    const b = (window as any).bench;
    b.schemaManager.setSchemaData('spans', { fields: { secret: { type: 'string' } } }, 'other');
    await b.cycle(100);
    b.element.remove();
    document.getElementById('host')!.append(b.element);
    await b.element.updateComplete;
    b.focus();
  });
  await expect(input(page)).toBeFocused();
  await set(page, 'secret');
  await expect(page.getByRole('option', { name: /secret/ })).toHaveCount(0);
  await page.keyboard.type('x');
  await expect(input(page)).toContainText('secretx');
});

test('quoted pipes, stale completions and bounded long input', async ({ page }) => {
  await ready(page);
  await set(page, 'level == "a|b" and stat');
  await expect(page.getByRole('option', { name: /status_code/ })).toBeVisible();
  await set(page, 'nosuchfield == ');
  await expect(page.getByRole('option')).toHaveCount(0);
  await set(page, 'level == "INFO"\n'.repeat(7000));
  expect(await page.locator('.cm-scroller').evaluate((el) => el.clientHeight)).toBeLessThanOrEqual(240);
  await page.keyboard.type('x');
  await expect.poll(() => page.evaluate(() => (window as any).bench.getValue().endsWith('x'))).toBe(true);
});

test('worker schema loading retries and events fire once per update', async ({ page }) => {
  await ready(page);
  let requests = 0;
  await page.route('**/test-schema', (route) => {
    requests++;
    return requests === 1 ? route.fulfill({ status: 503 }) : route.fulfill({ json: { fields: { loaded_field: { type: 'string' } } } });
  });
  await page.evaluate(async () => {
    const manager = (window as any).bench.schemaManager;
    await manager.load('spans', '/test-schema', 'loaded').catch(() => {});
    await Promise.all([manager.load('spans', '/test-schema', 'loaded'), manager.load('spans', '/test-schema', 'loaded')]);
    (window as any).updates = [];
    window.addEventListener('update-query', (e: any) => (window as any).updates.push(e.detail.value));
  });
  expect(requests).toBe(2);
  await set(page, 'level == "INFO"');
  await expect.poll(() => page.evaluate(() => (window as any).updates)).toEqual(['level == "INFO"']);
  await page.evaluate(() => {
    const el = (window as any).bench.element;
    el.setAttribute('widget-editor', 'true');
    el.setAttribute('target-widget-preview', 'outside');
    (window as any).widgetUpdates = [];
    document
      .getElementById('outside')!
      .addEventListener('update-widget-query', (e: any) => (window as any).widgetUpdates.push(e.detail.value));
    el.handleAddQuery('kind == "logs"', true);
  });
  expect(await page.evaluate(() => (window as any).widgetUpdates)).toEqual(['kind == "logs"']);
  expect(await page.evaluate(() => (window as any).updates)).toEqual(['level == "INFO"']);
});

test('native loading input transfers text and selection into the editor', async ({ page }) => {
  await ready(page);
  await page.evaluate(async () => {
    // A second tag exercises Lit's first-render adoption without relying on private editor APIs.
    const Base = (window as any).bench.element.constructor;
    const el = document.createElement('query-editor-adoption');
    const textarea = document.createElement('textarea');
    textarea.dataset.queryInput = '';
    textarea.value = 'typed before loading';
    el.append(textarea);
    document.getElementById('host')!.replaceChildren(el);
    textarea.focus();
    textarea.setSelectionRange(6, 12);
    customElements.define('query-editor-adoption', class extends Base {});
    await (el as any).updateComplete;
  });
  await expect(page.locator('.cm-content')).toBeFocused();
  await expect(page.locator('.cm-content')).toHaveText('typed before loading');
  await page.keyboard.type('AFTER');
  await expect(page.locator('.cm-content')).toHaveText('typed AFTER loading');
});

test('theme changes and completion announcements preserve the document', async ({ page }) => {
  await ready(page);
  await set(page, 'stat');
  await expect(page.getByRole('option', { name: /status_code/ })).toBeVisible();
  await page.keyboard.press('ArrowDown');
  const activeId = await input(page).getAttribute('aria-activedescendant');
  expect(activeId).toBeTruthy();
  await expect(page.locator(`[id="${activeId}"]`)).toHaveAttribute('aria-selected', 'true');
  await page.keyboard.press('Escape');
  await set(page, 'spans ');
  const token = page.locator('.cm-line span').first();
  const lightColor = await token.evaluate((el) => getComputedStyle(el).color);
  await page.evaluate(() => document.body.setAttribute('data-theme', 'dark'));
  await expect.poll(() => token.evaluate((el) => getComputedStyle(el).color)).not.toBe(lightColor);
  await expect(input(page)).toHaveText('spans ');
  await page.evaluate(() => document.body.setAttribute('data-theme', 'light'));
  await expect.poll(() => token.evaluate((el) => getComputedStyle(el).color)).toBe(lightColor);
});
