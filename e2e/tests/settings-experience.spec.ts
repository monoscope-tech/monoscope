import { test, expect } from '@playwright/test';
import { randomUUID } from 'node:crypto';
import { DEMO_PROJECT, sql } from './helpers';

const project = `/p/${DEMO_PROJECT}`;

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

test('API key tabs have unique targets and retain their contents', async ({ page }) => {
  await page.goto(`${project}/apis`);
  for (const id of ['active_content', 'revoked_content']) {
    await expect(page.locator(`[id="${id}"]`)).toHaveCount(1);
  }
  await page.getByRole('tab', { name: /Archived keys/ }).check();
  await expect(page.locator('#revoked_content')).toBeVisible();
  await expect(page.locator('#active_content')).toBeHidden();
  await page.getByRole('tab', { name: /Active keys/ }).check();
  await expect(page.locator('#active_content')).toBeVisible();
});

test('Prometheus target form fits a narrow screen with advanced fields open', async ({ page }) => {
  await page.setViewportSize({ width: 320, height: 800 });
  await page.goto(`${project}/settings/prometheus`);
  await page.locator('label[for="prometheus-modal"]').first().click();
  const box = page.locator('#prometheus-modal + .modal .modal-box');
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
