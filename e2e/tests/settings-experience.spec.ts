import { test, expect } from '@playwright/test';
import { randomUUID } from 'node:crypto';
import { DEMO_PROJECT, sql } from './helpers';

const project = `/p/${DEMO_PROJECT}`;

test('Prometheus viewers can inspect targets while editors manage them on a narrow screen', async ({ page, baseURL }, testInfo) => {
  const uid = randomUUID(), sid = randomUUID(), pid = randomUUID(), target = randomUUID();
  const name = 'Production checkout metrics with a deliberately long service name';
  sql(`INSERT INTO users.users (id, email) VALUES ('${uid}', '${uid}@example.com');
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title) VALUES ('${pid}', 'Prometheus permissions');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'view');
       INSERT INTO apis.prometheus_scrape_configs (id, project_id, name, url, auth_header)
       VALUES ('${target}', '${pid}', '${name}', 'https://metrics.example.com/metrics', 'Bearer fixture-secret');`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }]);
    await page.setViewportSize({ width: 320, height: 800 });
    const path = `/p/${pid}/settings/prometheus`;
    const form = { name: 'Replacement', url: 'https://metrics.example.com/replacement' };
    for (const theme of ['light', 'dark']) {
      await page.context().addCookies([{ name: 'theme', value: theme, url: baseURL! }]);
      sql(`UPDATE projects.project_members SET permission = 'view' WHERE project_id = '${pid}' AND user_id = '${uid}';`);
      await page.goto(path);
      await expect(page.getByText('A project editor can configure Prometheus targets.', { exact: true })).toBeVisible();
      await expect(page.getByRole('link', { name: 'View metrics', exact: true })).toBeVisible();
      await expect(page.locator('#main-content').getByText(/^(Add target|Edit|Pause|Resume|Delete)$/)).toHaveCount(0);
      expect(await page.content()).not.toContain('fixture-secret');
      for (const response of [
        await page.request.post(path, { form }),
        await page.request.post(`${path}/${target}/edit`, { form }),
        await page.request.patch(`${path}/${target}`),
        await page.request.delete(`${path}/${target}`),
        await page.request.post(`${path}/test`, { form }),
      ]) expect(response.status()).toBe(403);
      await page.getByRole('searchbox', { name: 'Filter targets', exact: true }).fill('No matching target');
      await expect(page.locator('#prometheus-targets .itemsListItem')).toBeHidden();
      await page.getByRole('searchbox', { name: 'Filter targets', exact: true }).clear();
      await expect(page.getByText(name, { exact: true })).toBeVisible();

      sql(`UPDATE projects.project_members SET permission = 'edit' WHERE project_id = '${pid}' AND user_id = '${uid}';`);
      await page.reload();
      const row = page.locator('#prometheus-targets .itemsListItem');
      await expect(row.getByRole('button', { name: 'Pause', exact: true })).toBeVisible();
      expect(await row.evaluate(element => {
        const bounds = element.getBoundingClientRect();
        return bounds.left >= 0 && bounds.right <= innerWidth && element.scrollWidth <= element.clientWidth;
      })).toBe(true);
      await row.getByRole('button', { name: 'Pause', exact: true }).press('Enter');
      await expect(row.getByRole('button', { name: 'Resume', exact: true })).toBeVisible();
      await row.getByRole('button', { name: 'Resume', exact: true }).press('Enter');
      await expect(row.getByRole('button', { name: 'Pause', exact: true })).toBeVisible();
      const edit = row.getByRole('button', { name: 'Edit', exact: true });
      await edit.press('Enter');
      const dialog = page.getByRole('dialog', { name: 'Edit Prometheus target', exact: true });
      await expect(dialog.getByRole('textbox', { name: 'Name', exact: true })).toBeFocused();
      expect(await dialog.evaluate(element => element.matches(':modal'))).toBe(true);
      for (let step = 0; step < 10; step++) {
        await page.keyboard.press('Tab');
        // Native dialogs allow the browser chrome in the tab cycle, but keep page controls inert.
        expect(await dialog.evaluate(element => element.contains(document.activeElement) || document.activeElement === document.body)).toBe(true);
      }
      await page.keyboard.press('Escape');
      await expect(dialog).toBeHidden();
      await expect(edit).toBeFocused();
      await edit.press('Enter');
      const url = dialog.getByRole('textbox', { name: /^Metrics URL/ });
      await url.fill('http://127.0.0.1/metrics');
      await dialog.getByRole('button', { name: 'Test connection', exact: true }).click();
      await expect(dialog.locator('.prom-test-result')).toContainText('public');
      await Promise.all([
        page.waitForResponse(response => response.url().endsWith(`/${target}/edit`) && response.request().method() === 'POST'),
        dialog.getByRole('button', { name: 'Save changes', exact: true }).click(),
      ]);
      await expect(dialog).toBeVisible();
      await expect(dialog.getByRole('alert')).toContainText('URL must be a public');
      await expect(dialog.getByRole('alert')).toBeVisible();
      await expect(url).toHaveValue('http://127.0.0.1/metrics');
      await page.screenshot({ path: testInfo.outputPath(`prometheus-rejected-save-${theme}.png`), fullPage: true });
      await url.fill('https://metrics.example.com/metrics');
      await dialog.getByRole('button', { name: 'Save changes', exact: true }).click();
      await expect(dialog).toBeHidden();
      expect(await page.locator(`#prom-edit-${target}`).evaluate(element => element.matches(':modal'))).toBe(false);
      await expect(row.getByRole('button', { name: 'Edit', exact: true })).toBeFocused();
      await page.screenshot({ path: testInfo.outputPath(`prometheus-editor-${theme}.png`), fullPage: true });
    }
    expect(sql(`SELECT name, auth_header FROM apis.prometheus_scrape_configs WHERE id = '${target}'`).toString().trim()).toBe(`${name}|Bearer fixture-secret`);
  } finally {
    sql(`DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id = '${uid}';`);
  }
});

for (const theme of ['light', 'dark']) test(`storage preserves a saved connection on validation failure and clears it on removal (${theme})`, async ({ page, baseURL }, testInfo) => {
  const uid = randomUUID(), sid = randomUUID(), pid = randomUUID();
  sql(`INSERT INTO users.users (id, email) VALUES ('${uid}', '${uid}@example.com');
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title, s3_bucket) VALUES ('${pid}', 'Storage removal', '{"accessKey":"fixture-access","secretKey":"fixture-secret","region":"us-east-1","bucket":"fixture-bucket","endpointUrl":""}');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'admin');`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }, { name: 'theme', value: theme, url: baseURL! }]);
    await page.setViewportSize({ width: 320, height: 800 });
    await page.goto(`/p/${pid}/byob_s3`);
    await expect(page.locator('#secretKey')).toHaveValue('fixture-secret');
    await page.locator('#secretKey').fill('rejected-secret');
    await page.locator('#bucket').fill('INVALID_bucket');
    await page.getByRole('button', { name: 'Validate & Save', exact: true }).click();
    await expect(page.getByRole('alert')).toContainText('The saved connection is unchanged.');
    await expect(page.locator('#connectedInd')).toHaveText('Connected');
    await expect(page.locator('#secretKey')).toHaveValue('rejected-secret');
    await expect(page.locator('#storage-connection-content')).toHaveCount(1);
    expect(sql(`SELECT s3_bucket->>'bucket' FROM projects.projects WHERE id = '${pid}'`).toString().trim()).toBe('fixture-bucket');
    await page.screenshot({ path: testInfo.outputPath('storage-rejected-replacement.png'), fullPage: true });
    await page.locator('label[for="remove-modal"]').first().click();
    await page.getByRole('button', { name: 'Remove bucket', exact: true }).click();
    await expect(page.locator('#connectedInd')).toHaveText('Not connected');
    await expect(page.getByRole('alert')).toHaveCount(0);
    await expect(page.locator('#secretKey')).toHaveValue('');
    await expect(page.locator('#accessKey')).toHaveValue('');
    await expect(page.locator('#bucket')).toHaveValue('');
    await expect(page.locator('label[for="remove-modal"]')).toHaveCount(0);
    await expect(page.getByRole('button', { name: 'Validate & Save', exact: true })).toBeVisible();
    await page.getByRole('button', { name: 'Dismiss notification', exact: true }).click();
    await page.locator('#accessKey').fill('replacement-access');
    await page.locator('#secretKey').fill('replacement-secret');
    await page.locator('#bucket').fill('INVALID_bucket');
    await page.locator('#endpointUrl').fill('http://127.0.0.1:1');
    await Promise.all([
      page.waitForResponse(response => response.request().method() === 'POST' && response.url().endsWith(`/p/${pid}/byob_s3`)),
      page.getByRole('button', { name: 'Validate & Save', exact: true }).click(),
    ]);
    await expect(page.locator('#secretKey')).toHaveValue('replacement-secret');
    await expect(page.locator('#connectedInd')).toHaveText('Not connected');
    await expect(page.getByRole('alert')).toContainText('valid bucket name');
    expect(sql(`SELECT s3_bucket IS NULL FROM projects.projects WHERE id = '${pid}'`).toString().trim()).toBe('t');
    expect(await page.evaluate(() => document.documentElement.scrollWidth)).toBeLessThanOrEqual(320);
    sql(`UPDATE projects.projects SET s3_bucket = '{"accessKey":"fixture-access","secretKey":"fixture-secret","region":"us-east-1","bucket":"fixture-bucket","endpointUrl":""}' WHERE id = '${pid}';
         UPDATE projects.project_members SET permission = 'view' WHERE project_id = '${pid}' AND user_id = '${uid}';`);
    await page.reload();
    await expect(page.getByText('fixture-bucket', { exact: true })).toBeVisible();
    await expect(page.locator('#accessKey, #secretKey, label[for="remove-modal"]')).toHaveCount(0);
    expect(await page.content()).not.toContain('fixture-secret');
    expect((await page.request.delete(`/p/${pid}/byob_s3`)).status()).toBe(403);
    expect((await page.request.post(`/p/${pid}/byob_s3`, { form: { accessKey: 'fixture', secretKey: 'fixture', region: 'us-east-1', bucket: 'INVALID_bucket', endpointUrl: '' } })).status()).toBe(403);
  } finally {
    sql(`DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id = '${uid}';`);
  }
});

test('notification settings remain clean until the user changes a value', async ({ page }) => {
  await page.goto(`${project}/settings/integrations`);
  await expect.poll(() => page.locator('#emails_input').evaluate(element => Boolean((element as any)._tagifyInstance))).toBe(true);
  await page.waitForLoadState('networkidle');
  await expect(page.locator('#integrations-save-status')).not.toHaveText('Unsaved changes');
  await page.locator('#include-user-identity-in-alerts').click();
  await expect(page.locator('#integrations-save-status')).toHaveText('Unsaved changes');
  await page.reload();
  await page.waitForLoadState('networkidle');
  await page.locator('#notifsForm .tagify__tag__removeBtn').first().click();
  await expect(page.locator('#integrations-save-status')).toHaveText('Unsaved changes');
});

test('plan comparison stays usable on a narrow screen', async ({ page }) => {
  await page.setViewportSize({ width: 320, height: 800 });
  await page.goto(`${project}/manage_billing`);
  await page.getByText('Change plan', { exact: true }).click();
  const box = page.locator('#pricing-modal + .modal .modal-box');
  await expect(box).toBeVisible();
  await expect(box.getByRole('slider', { name: 'Total events', exact: true })).toBeVisible();
  await expect.poll(() => box.evaluate(element => {
    const bounds = element.getBoundingClientRect();
    return bounds.left >= 0 && bounds.right <= innerWidth && element.scrollWidth <= element.clientWidth;
  })).toBe(true);
});

test('Prometheus target form fits a narrow screen with advanced fields open', async ({ page }) => {
  await page.setViewportSize({ width: 320, height: 800 });
  await page.goto(`${project}/settings/prometheus`);
  await page.getByRole('button', { name: 'Add target', exact: true }).click();
  const box = page.getByRole('dialog', { name: 'Scrape a Prometheus endpoint', exact: true }).locator('.modal-box');
  await expect(box).toBeVisible();
  await box.getByText('Advanced', { exact: true }).click();
  await expect(box.getByRole('textbox', { name: 'Static labels' })).toBeVisible();
  expect(await box.evaluate(element => {
    const bounds = element.getBoundingClientRect();
    return bounds.left >= 0 && bounds.right <= innerWidth && element.scrollWidth <= element.clientWidth;
  })).toBe(true);
  await box.getByText('Cancel', { exact: true }).click();
  await expect(box).toBeHidden();
});

test('member permission edits save without requiring a new invitation', async ({ page, baseURL }) => {
  const uid = randomUUID(), other = randomUUID(), sid = randomUUID(), pid = randomUUID(), member = randomUUID();
  const invalidEmail = `invalid-invite-${randomUUID()}`;
  sql(`INSERT INTO users.users (id, email) VALUES ('${uid}', '${uid}@example.com'), ('${other}', '${other}@example.com');
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title, payment_plan) VALUES ('${pid}', 'Member settings test', 'GraduatedPricing');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'admin');
       INSERT INTO projects.project_members (id, project_id, user_id, permission) VALUES ('${member}', '${pid}', '${other}', 'edit');`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }]);
    await page.setViewportSize({ width: 320, height: 800 });
    await page.goto(`/p/${pid}/manage_members`);
    await page.locator(`#member-${member} select[name="permissions"]`).selectOption('view');
    const save = page.getByRole('button', { name: 'Save changes', exact: true });
    await expect(save).toBeEnabled();
    await page.locator('input[type="email"]').fill(invalidEmail);
    let submissions = 0;
    page.on('request', request => {
      if (request.method() === 'POST' && request.url().endsWith('/manage_members')) submissions++;
    });
    await save.click();
    await page.waitForLoadState('networkidle');
    expect(submissions).toBe(0);
    expect(sql(`SELECT permission FROM projects.project_members WHERE id = '${member}'`).toString().trim()).toBe('edit');
    expect(sql(`SELECT count(*) FROM users.users WHERE email = '${invalidEmail}'`).toString().trim()).toBe('0');
    await page.locator('input[type="email"]').clear();
    await save.click();
    await expect.poll(() => sql(`SELECT permission FROM projects.project_members WHERE id = '${member}'`).toString().trim(), { timeout: 3000 }).toBe('view');
    await expect(page.getByRole('textbox', { name: 'Invite new member email', exact: true })).toBeVisible();
    await expect(page.getByRole('combobox', { name: 'New member role', exact: true })).toBeVisible();
    expect(await page.locator('input[type="email"]').evaluate(element => element.getBoundingClientRect().right <= innerWidth)).toBe(true);
  } finally {
    sql(`DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id IN ('${uid}', '${other}') OR email = '${invalidEmail}';`);
  }
});
