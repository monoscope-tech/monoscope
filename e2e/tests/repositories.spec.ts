import { test, expect } from '@playwright/test';
import { randomUUID } from 'node:crypto';
import { DEMO_PROJECT, sql } from './helpers';

const project = `/p/${DEMO_PROJECT}`;

test('rerunning a pull request review replaces its view without nesting content', async ({ page, baseURL }) => {
  const uid = randomUUID(), sid = randomUUID(), pid = randomUUID(), thread = randomUUID(), run = randomUUID();
  const revision = 'a'.repeat(40);
  sql(`INSERT INTO users.users (id, email) VALUES ('${uid}', '${uid}@example.com');
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title) VALUES ('${pid}', 'Review recovery');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'admin');
       INSERT INTO projects.pr_review_threads (id, project_id, owner, repo, number, latest_revision, event_at)
       VALUES ('${thread}', '${pid}', 'team', 'checkout', 1, '${revision}', now());
       INSERT INTO projects.pr_review_runs (id, thread_id, revision, state, error)
       VALUES ('${run}', '${thread}', '${revision}', 'incomplete', 'Review unavailable');`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }]);
    await page.goto(`/p/${pid}/repositories?tab=reviews`);
    await page.getByText('team/checkout #1', { exact: true }).click();
    await page.getByRole('button', { name: 'Rerun review', exact: true }).click();
    await expect(page.locator('#repository-content').getByText('Queued', { exact: true })).toBeVisible();
    await expect(page.locator('#repository-content')).toHaveCount(1);
    await expect(page.getByText('Review unavailable', { exact: true })).toHaveCount(0);
    await page.getByRole('navigation', { name: 'Repository views' }).getByRole('link', { name: 'Overview', exact: true }).click();
    await expect(page.locator('#repository-content')).toHaveCount(1);
    await expect(page.getByRole('heading', { name: 'Connect the repositories behind your services', exact: true })).toBeVisible();
  } finally {
    sql(`DELETE FROM background_jobs WHERE payload->>'contents' = '${run}';
         DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id = '${uid}';`);
  }
});

test('repositories are discoverable and their views survive navigation and history', async ({ page }) => {
  await page.goto(`${project}/settings/integrations`);
  await page.locator('#main-sidenav').getByRole('link', { name: 'Repositories', exact: true }).click();
  await expect(page).toHaveURL(`${project}/repositories`);
  await expect(page.locator('#main-content').getByRole('heading', { name: 'Repositories', exact: true })).toBeVisible();
  const views = page.getByRole('navigation', { name: 'Repository views' });
  await views.getByRole('link', { name: 'Pull requests', exact: true }).click();
  await expect(page).toHaveURL(/repositories\?tab=reviews/);
  await expect(views.getByRole('link', { name: 'Pull requests', exact: true })).toHaveAttribute('aria-current', 'page');
  await views.getByRole('link', { name: 'Configuration', exact: true }).click();
  await expect(page).toHaveURL(/repositories\?tab=configuration/);
  await page.goBack();
  await expect(views.getByRole('link', { name: 'Pull requests', exact: true })).toHaveAttribute('aria-current', 'page');
  await expect(page.locator('#main-sidenav .main-nav-link[aria-current="page"]')).toHaveAttribute('href', `${project}/repositories`);
});

test('repository and storage setup fit a narrow viewport', async ({ page }) => {
  await page.setViewportSize({ width: 320, height: 800 });
  for (const path of ['/repositories', '/repositories?tab=configuration', '/byob_s3']) {
    await page.goto(project + path);
    await expect.poll(() => page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth)).toBe(true);
  }
});

test('repository token setup keeps errors local without enabling dashboard sync', async ({ page, baseURL }, testInfo) => {
  const uid = randomUUID(), sid = randomUUID(), pid = randomUUID();
  sql(`INSERT INTO users.users (id, email) VALUES ('${uid}', '${uid}@example.com');
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title) VALUES ('${pid}', 'Token connection test');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'admin');`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }]);
    await page.setViewportSize({ width: 320, height: 800 });
    await page.goto(`/p/${pid}/repositories/connect`);
    await page.getByRole('link', { name: 'Connect with a token', exact: true }).click();
    await expect(page).toHaveURL(`/p/${pid}/repositories/connect/token`);
    const panel = page.locator('#repository-token-content');
    await panel.getByRole('combobox', { name: /Git host/ }).selectOption('gitea');
    await panel.getByRole('textbox', { name: /Server URL/ }).fill('http://git.example.com');
    await panel.getByRole('textbox', { name: /Full repository name/ }).fill('team/checkout');
    await panel.getByLabel('Access token', { exact: false }).fill('fixture-token');
    await panel.getByRole('button', { name: 'Connect repository', exact: true }).press('Enter');
    await expect(panel.getByRole('alert')).toContainText('Use https://');
    await expect(panel).toHaveCount(1);
    await expect(panel.getByRole('combobox', { name: /Git host/ })).toHaveValue('gitea');
    await expect(panel.getByRole('textbox', { name: /Full repository name/ })).toHaveValue('team/checkout');
    await expect(panel.getByLabel('Access token', { exact: false })).toHaveAttribute('value', '');
    await expect(panel.getByLabel('Access token', { exact: false })).toHaveValue('fixture-token');
    expect(sql(`SELECT count(*) FROM projects.git_sync WHERE project_id = '${pid}'`).toString().trim()).toBe('0');
    expect(sql(`SELECT count(*) FROM projects.git_credentials WHERE project_id = '${pid}'`).toString().trim()).toBe('0');
    for (const theme of ['light', 'dark']) {
      await page.context().addCookies([{ name: 'theme', value: theme, url: baseURL! }]);
      await page.reload();
      await expect(panel.getByLabel('Access token', { exact: false })).toHaveValue('');
      await page.getByText('Replacing an existing connection?', { exact: true }).press('Enter');
      await expect(page.getByRole('checkbox', { name: 'Replace saved token', exact: true })).toBeVisible();
      expect(await page.evaluate(() => document.documentElement.scrollWidth)).toBeLessThanOrEqual(320);
      await page.screenshot({ path: testInfo.outputPath(`repository-token-${theme}.png`), fullPage: true });
    }
    sql(`UPDATE projects.project_members SET permission = 'view' WHERE project_id = '${pid}' AND user_id = '${uid}';`);
    expect((await page.request.get(`/p/${pid}/repositories/connect`)).status()).toBe(403);
    expect((await page.request.get(`/p/${pid}/repositories/connect/token`)).status()).toBe(403);
    expect((await page.request.post(`/p/${pid}/repositories/connect/token`, { form: { host: 'github', repoFullName: 'team/checkout', accessToken: 'forbidden' } })).status()).toBe(403);
  } finally {
    sql(`DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id = '${uid}';`);
  }
});

test('repository account choice keeps service mappings with the selected team', async ({ page, baseURL }, testInfo) => {
  const uid = randomUUID(), sid = randomUUID(), pid = randomUUID();
  const accounts = [randomUUID(), randomUUID()];
  sql(`INSERT INTO users.users (id, email, is_sudo) VALUES ('${uid}', '${uid}@example.com', true);
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title) VALUES ('${pid}', 'Repository account test');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'admin');
       INSERT INTO projects.git_credentials (id, project_id, account, installation_id, host)
       VALUES ('${accounts[0]}', '${pid}', 'team-a', 42, 'github'), ('${accounts[1]}', '${pid}', 'team-b', 43, 'github');`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }]);
    await page.setViewportSize({ width: 320, height: 800 });
    await page.goto(`/p/${pid}/repositories?tab=configuration`);
    const panel = page.locator('#code-mappings-content');
    await expect(panel.getByRole('button', { name: 'Link repository', exact: true })).toHaveCount(0);
    for (const [index, account] of accounts.entries()) {
      await panel.getByRole('combobox', { name: /Repository account/ }).selectOption(account);
      const form = panel.locator('form').filter({ has: page.getByRole('button', { name: 'Link repository', exact: true }) });
      await expect(form.locator('input[name="credentialId"]')).toHaveValue(account);
      let releaseRetry!: () => void;
      const retryGate = new Promise<void>(resolve => { releaseRetry = resolve; });
      await page.route('**/settings/code-mappings/editor**', async route => { await retryGate; await route.continue(); }, { times: 1 });
      const refreshed = page.waitForResponse(response => response.request().method() === 'GET' && response.url().includes('/settings/code-mappings/editor'));
      try {
        await panel.getByRole('button', { name: 'Retry', exact: true }).click();
        await expect(form.getByRole('textbox', { name: 'Repository *', exact: true })).toBeDisabled();
      } finally {
        releaseRetry();
      }
      await refreshed;
      await expect(form.getByRole('textbox', { name: 'Repository *', exact: true })).toBeEnabled();
      await expect(form.locator('input[name="credentialId"]')).toHaveValue(account);
      expect(await page.locator('[id]').evaluateAll(elements => {
        const ids = elements.map(element => element.id);
        return ids.filter((id, index) => ids.indexOf(id) !== index);
      })).toEqual([]);
      await form.locator('input[name="repo"]').fill('checkout');
      await form.locator('input[name="service"]').fill(`team-${index}`);
      const [response] = await Promise.all([
        page.waitForResponse(response => response.request().method() === 'POST' && response.url().endsWith('/settings/code-mappings')),
        form.getByRole('button', { name: 'Link repository', exact: true }).click(),
      ]);
      expect(response.status()).toBe(200);
      const unlink = panel.getByRole('button', { name: `Unlink team-${index === 0 ? 'a' : 'b'}/checkout`, exact: true });
      await expect(unlink).toBeVisible();
      expect(await unlink.locator('..').getByText(`team-${index === 0 ? 'a' : 'b'}/checkout`, { exact: true }).evaluate(element => element.scrollWidth <= element.clientWidth)).toBe(true);
    }
    const sourceRepository = sql(`SELECT id FROM projects.repositories WHERE project_id = '${pid}' AND owner = 'team-a'`).toString().trim();
    await page.goto(`/p/${pid}/repositories/${sourceRepository}`);
    await page.getByRole('link', { name: 'Configure source context', exact: true }).click();
    await expect(page).toHaveURL(`/p/${pid}/repositories/${sourceRepository}/source`);
    await expect(panel.getByRole('combobox', { name: /Repository account/ })).toHaveValue(accounts[0]);
    await expect(panel.getByRole('textbox', { name: 'Repository *', exact: true })).toHaveValue('team-a/checkout');
    await expect(panel.getByRole('textbox', { name: 'Repository *', exact: true })).toHaveAttribute('readonly');
    await expect(panel.getByRole('button', { name: 'Unlink team-b/checkout', exact: true })).toHaveCount(0);
    await panel.locator('input[name="service"]').fill('team-0');
    await panel.locator('input[name="ref"]').fill('release');
    await Promise.all([
      page.waitForResponse(response => response.request().method() === 'POST' && response.url().endsWith(`/repositories/${sourceRepository}/source`)),
      panel.getByRole('button', { name: 'Link repository', exact: true }).click(),
    ]);
    await expect(panel.getByRole('button', { name: 'Unlink team-a/checkout', exact: true })).toBeVisible();
    await expect(panel).toHaveCount(1);
    expect(sql(`SELECT ref FROM projects.code_mappings WHERE project_id = '${pid}' AND owner = 'team-a'`).toString().trim()).toBe('release');
    await Promise.all([
      page.waitForResponse(response => response.request().method() === 'POST' && response.url().endsWith(`/repositories/${sourceRepository}/source/reviews`)),
      panel.getByRole('button', { name: 'Save review settings', exact: true }).click(),
    ]);
    await expect(panel.getByRole('button', { name: 'Unlink team-a/checkout', exact: true })).toBeVisible();
    await expect(panel.getByRole('button', { name: 'Unlink team-b/checkout', exact: true })).toHaveCount(0);
    await expect(panel).toHaveCount(1);
    const otherMapping = sql(`SELECT id FROM projects.code_mappings WHERE project_id = '${pid}' AND owner = 'team-b'`).toString().trim();
    expect((await page.request.delete(`/p/${pid}/repositories/${sourceRepository}/source/${otherMapping}`)).status()).toBe(404);
    await page.goto(`/p/${pid}/repositories?tab=configuration`);
    for (const theme of ['dark', 'light']) {
      await page.context().addCookies([{ name: 'theme', value: theme, url: baseURL! }]);
      await page.reload();
      await expect(panel.getByRole('button', { name: 'Unlink team-a/checkout', exact: true })).toBeVisible();
      await expect(panel.getByRole('button', { name: 'Unlink team-b/checkout', exact: true })).toBeVisible();
      expect(await page.evaluate(() => document.documentElement.scrollWidth)).toBeLessThanOrEqual(320);
      await page.screenshot({ path: testInfo.outputPath(`repository-accounts-${theme}.png`), fullPage: true });
    }
    const mapping = sql(`SELECT id FROM projects.code_mappings WHERE project_id = '${pid}' LIMIT 1`).toString().trim();
    sql(`UPDATE projects.project_members SET permission = 'view' WHERE project_id = '${pid}' AND user_id = '${uid}';`);
    await page.goto(`/p/${pid}/repositories/${sourceRepository}/source`);
    await expect(panel.getByRole('button', { name: /Unlink|Link repository|Save review settings/ })).toHaveCount(0);
    await expect(panel.getByText('A project editor can configure source context and pull request reviews.')).toBeVisible();
    await expect(panel.locator('span').filter({ hasText: 'team-a/checkout' })).toBeVisible();
    await page.screenshot({ path: testInfo.outputPath('repository-source-read-only-light.png'), fullPage: true });
    await page.goto(`/p/${pid}/repositories?tab=configuration`);
    await expect(page.getByRole('link', { name: 'Install GitHub App', exact: true })).toHaveCount(0);
    await expect(page.getByRole('link', { name: 'Add repositories', exact: true })).toHaveCount(0);
    expect((await page.request.delete(`/p/${pid}/settings/code-mappings/${mapping}`)).status()).toBe(403);
    expect((await page.request.post(`/p/${pid}/repositories/${sourceRepository}/source`, { form: { repo: 'checkout', service: 'forbidden' } })).status()).toBe(403);
    expect((await page.request.delete(`/p/${pid}/repositories/${sourceRepository}/source/${otherMapping}`)).status()).toBe(403);
    expect(sql(`SELECT count(*) FROM projects.code_mappings WHERE project_id = '${pid}'`).toString().trim()).toBe('2');
    const repository = sql(`SELECT id FROM projects.repositories WHERE project_id = '${pid}' AND owner = 'team-a'`).toString().trim();
    expect((await page.request.get(`/p/${DEMO_PROJECT}/repositories/${repository}`)).status()).toBe(404);
    await page.goto(`/p/${pid}/repositories/${repository}`);
    await expect(page.getByRole('button', { name: 'Remove repository', exact: true })).toHaveCount(0);
    expect((await page.request.delete(`/p/${pid}/repositories/${repository}`)).status()).toBe(403);
    sql(`UPDATE projects.project_members SET permission = 'admin' WHERE project_id = '${pid}' AND user_id = '${uid}';`);
    const sourceMapping = sql(`SELECT id FROM projects.code_mappings WHERE project_id = '${pid}' AND owner = 'team-a'`).toString().trim();
    expect((await page.request.delete(`/p/${pid}/settings/code-mappings/${sourceMapping}`)).status()).toBe(200);
    await page.goto(`/p/${pid}/repositories`);
    await page.getByRole('link', { name: 'team-a/checkout', exact: true }).click();
    await expect(page).toHaveURL(`/p/${pid}/repositories/${repository}`);
    await expect(page.locator('#main-content').getByRole('heading', { name: 'team-a/checkout', exact: true })).toBeVisible();
    await expect(page.getByText(/No source paths are linked/)).toBeVisible();
    await expect(page.getByText(/This repository remains connected for your team/)).toBeVisible();
    expect(await page.evaluate(() => document.documentElement.scrollWidth)).toBeLessThanOrEqual(320);
    await page.screenshot({ path: testInfo.outputPath('repository-details-light.png'), fullPage: true });
    const remove = page.getByRole('button', { name: 'Remove repository', exact: true });
    await expect(remove).toBeVisible();
    const dialog = page.getByRole('dialog', { name: 'Remove team-a/checkout?' });
    for (const theme of ['light', 'dark']) {
      await page.context().addCookies([{ name: 'theme', value: theme, url: baseURL! }]);
      await page.reload();
      await remove.press('Enter');
      await expect(dialog).toBeVisible();
      await expect(dialog.getByRole('button', { name: 'Cancel', exact: true })).toBeFocused();
      await expect(dialog).toContainText('Dashboards and review history are retained.');
      expect(await dialog.evaluate(element => element.scrollWidth <= element.clientWidth)).toBe(true);
      await page.screenshot({ path: testInfo.outputPath(`repository-removal-${theme}.png`), fullPage: true });
      const contrast = await dialog.getByRole('button', { name: 'Remove repository', exact: true }).evaluate(button => {
        const context = document.createElement('canvas').getContext('2d')!;
        const style = getComputedStyle(button);
        context.fillStyle = style.backgroundColor;
        context.fillRect(0, 0, 1, 1);
        const background = context.getImageData(0, 0, 1, 1).data;
        context.fillStyle = style.color;
        context.fillRect(0, 0, 1, 1);
        const foreground = context.getImageData(0, 0, 1, 1).data;
        const luminance = (color: Uint8ClampedArray) => Array.from(color).slice(0, 3).map(channel => channel / 255).map(channel => channel <= 0.04045 ? channel / 12.92 : ((channel + 0.055) / 1.055) ** 2.4).reduce((sum, channel, index) => sum + channel * [0.2126, 0.7152, 0.0722][index], 0);
        const a = luminance(background), b = luminance(foreground);
        return (Math.max(a, b) + 0.05) / (Math.min(a, b) + 0.05);
      });
      await testInfo.attach(`removal-contrast-${theme}`, { body: JSON.stringify({ contrast }), contentType: 'application/json' });
      expect(contrast).toBeGreaterThanOrEqual(4.5);
      if (theme === 'light') {
        await page.keyboard.press('Escape');
        await expect(dialog).not.toBeVisible();
        await expect(remove).toBeFocused();
      }
    }
    await dialog.getByRole('button', { name: 'Remove repository', exact: true }).press('Enter');
    await expect(page).toHaveURL(`/p/${pid}/repositories`);
    await expect(page.getByRole('link', { name: 'team-a/checkout', exact: true })).toHaveCount(0);
    await expect(page.getByRole('link', { name: 'team-b/checkout', exact: true })).toBeVisible();
    expect(sql(`SELECT count(*) FROM projects.git_credentials WHERE project_id = '${pid}'`).toString().trim()).toBe('2');
  } finally {
    sql(`DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id = '${uid}';`);
  }
});

test('dashboard repository choice scopes file conflicts to the selected repository', async ({ page, baseURL }) => {
  const uid = randomUUID(), sid = randomUUID(), pid = randomUUID();
  const dashboards = [randomUUID(), randomUUID()], repositories = [randomUUID(), randomUUID()];
  sql(`INSERT INTO users.users (id, email) VALUES ('${uid}', '${uid}@example.com');
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title) VALUES ('${pid}', 'Dashboard repository test');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'admin');
       INSERT INTO projects.git_sync (id, project_id, owner, repo, branch, installation_id, host)
       VALUES ('${repositories[0]}', '${pid}', 'team-a', 'dashboards', 'main', 42, 'github'), ('${repositories[1]}', '${pid}', 'team-b', 'dashboards', 'main', 43, 'github');
       INSERT INTO projects.dashboards (id, project_id, created_by, title, schema)
       VALUES ('${dashboards[0]}', '${pid}', '${uid}', 'Overview', '{"widgets":[]}'::jsonb), ('${dashboards[1]}', '${pid}', '${uid}', 'Overview', '{"widgets":[]}'::jsonb);`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }]);
    await page.setViewportSize({ width: 320, height: 800 });
    await page.goto(`/p/${pid}/dashboards/${dashboards[0]}`);
    await page.getByRole('button', { name: 'Open context menu', exact: true }).click();
    await page.getByRole('link', { name: 'Repository sync', exact: true }).click();
    await expect(page).toHaveURL(`/p/${pid}/dashboards/${dashboards[0]}/repository`);
    const panel = page.locator('#dashboard-repository-content');
    sql(`UPDATE projects.project_members SET permission = 'view' WHERE project_id = '${pid}' AND user_id = '${uid}';`);
    await page.reload();
    await expect(panel.getByRole('button', { name: 'Sync dashboard', exact: true })).toHaveCount(0);
    const denied = await page.request.post(`/p/${pid}/dashboards/${dashboards[0]}/repository`, { form: { repositoryId: repositories[1] } });
    expect(denied.status()).toBe(403);
    sql(`UPDATE projects.project_members SET permission = 'admin' WHERE project_id = '${pid}' AND user_id = '${uid}';`);
    await page.reload();
    await panel.getByRole('combobox', { name: /Repository/ }).selectOption(repositories[1]);
    await panel.getByRole('button', { name: 'Sync dashboard', exact: true }).click();
    await expect(panel.getByText('Repository assigned', { exact: true })).toBeVisible();
    await expect(panel).toHaveCount(1);
    await expect(panel.getByText('team-b/dashboards', { exact: true })).toBeVisible();
    await expect(panel.getByText('dashboards/overview.yaml', { exact: true })).toBeVisible();
    await page.goto(`/p/${pid}/dashboards/${dashboards[1]}/repository`);
    await panel.getByRole('combobox', { name: /Repository/ }).selectOption(repositories[1]);
    await panel.getByRole('button', { name: 'Sync dashboard', exact: true }).click();
    await expect(panel.getByRole('alert')).toContainText('Another dashboard already uses this file');
    await expect(panel).toHaveCount(1);
    await panel.getByRole('combobox', { name: /Repository/ }).selectOption(repositories[0]);
    await panel.getByRole('button', { name: 'Sync dashboard', exact: true }).click();
    await expect(panel.getByText('team-a/dashboards', { exact: true })).toBeVisible();
    expect(await page.evaluate(() => document.documentElement.scrollWidth)).toBeLessThanOrEqual(320);
    expect(sql(`SELECT git_sync_id FROM projects.dashboards WHERE id = '${dashboards[1]}'`).toString().trim()).toBe(repositories[0]);
  } finally {
    sql(`DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id = '${uid}';`);
  }
});

test('repository dashboard setup reuses a token account and keeps retries scoped', async ({ page, baseURL }, testInfo) => {
  const uid = randomUUID(), sid = randomUUID(), pid = randomUUID();
  sql(`INSERT INTO users.users (id, email) VALUES ('${uid}', '${uid}@example.com');
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title) VALUES ('${pid}', 'Repository dashboard setup');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'admin');`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }]);
    expect((await page.request.post(`/p/${pid}/settings/git-sync`, { form: {
      host: 'gitlab', apiBase: 'https://git.example.com', owner: 'team', repo: 'checkout', branch: 'main', accessToken: 'fixture-token', webhookSecret: 'fixture-secret',
    } })).status()).toBe(200);
    const connection = sql(`SELECT id FROM projects.git_sync WHERE project_id = '${pid}'`).toString().trim();
    const repository = sql(`SELECT id FROM projects.repositories WHERE project_id = '${pid}'`).toString().trim();
    const account = sql(`SELECT id FROM projects.git_credentials WHERE project_id = '${pid}'`).toString().trim();
    expect((await page.request.delete(`/p/${pid}/settings/git-sync/${connection}`)).status()).toBe(200);
    await page.setViewportSize({ width: 320, height: 800 });
    await page.goto(`/p/${pid}/repositories/${repository}`);
    await page.getByRole('link', { name: 'Configure dashboard sync', exact: true }).click();
    await expect(page).toHaveURL(`/p/${pid}/repositories/${repository}/dashboards`);
    const panel = page.locator('#repository-dashboard-content');
    await panel.getByRole('combobox', { name: /Repository account/ }).selectOption(account);
    await panel.getByRole('textbox', { name: 'Branch', exact: true }).fill('release');
    await panel.getByRole('textbox', { name: 'Folder in repo', exact: true }).fill('/ops/');
    await panel.getByRole('button', { name: 'Enable dashboard sync', exact: true }).click();
    await expect(panel.getByText('Connected', { exact: true })).toBeVisible();
    await expect(panel).toHaveCount(1);
    await expect(panel.getByRole('textbox', { name: /Branch/ })).toHaveValue('release');
    await expect(panel.getByLabel('Webhook secret', { exact: true })).toHaveAttribute('readonly');
    const signingSecret = sql(`SELECT webhook_secret FROM projects.git_sync WHERE project_id = '${pid}'`).toString().trim();
    expect(signingSecret).toMatch(/^[0-9a-f-]{36}$/);
    expect(sql(`SELECT branch || ':' || path_prefix FROM projects.git_sync WHERE project_id = '${pid}'`).toString().trim()).toBe('release:ops');
    expect((await page.request.post(`/p/${pid}/repositories/${repository}/dashboards`, { form: { credentialId: account, branch: 'different' } })).status()).toBe(200);
    expect(sql(`SELECT branch FROM projects.git_sync WHERE project_id = '${pid}'`).toString().trim()).toBe('release');
    expect(sql(`SELECT count(*) FROM background_jobs WHERE payload->>'tag' = 'GitSyncRepository' AND payload::text LIKE '%${pid}%'`).toString().trim()).toBe('1');
    const activeConnection = sql(`SELECT id FROM projects.git_sync WHERE project_id = '${pid}'`).toString().trim();
    sql(`UPDATE projects.git_sync SET last_error = 'dashboards/overview.yaml: Remote file conflicts with a local dashboard awaiting its first push.' WHERE id = '${activeConnection}';`);
    await page.reload();
    await expect(panel.getByRole('status')).toContainText('overview.yaml');
    await Promise.all([
      page.waitForResponse(response => response.request().method() === 'POST' && response.url().endsWith(`/git-sync/${activeConnection}/retry`)),
      panel.getByRole('button', { name: 'Retry import', exact: true }).press('Enter'),
    ]);
    await expect(panel.locator(`[id="git-sync-${activeConnection}"]`)).toHaveCount(1);
    await expect(panel.getByRole('status')).toContainText('overview.yaml');
    expect(sql(`SELECT count(*) FROM background_jobs WHERE payload->>'tag' = 'GitSyncRepository' AND payload::text LIKE '%${pid}%'`).toString().trim()).toBe('2');
    expect((await page.request.post(`/p/${pid}/settings/git-sync/${randomUUID()}/retry`)).status()).toBe(404);
    expect((await page.request.post(`/p/${pid}/settings/git-sync/${randomUUID()}/pause`)).status()).toBe(404);
    expect((await page.request.post(`/p/${DEMO_PROJECT}/settings/git-sync/${activeConnection}/pause`)).status()).toBe(403);
    await expect(panel.getByRole('button', { name: 'Pause sync', exact: true })).toBeVisible();
    await Promise.all([
      page.waitForResponse(response => response.request().method() === 'POST' && response.url().endsWith(`/git-sync/${activeConnection}/pause`)),
      panel.getByRole('button', { name: 'Pause sync', exact: true }).press('Enter'),
    ]);
    expect(sql(`SELECT sync_enabled FROM projects.git_sync WHERE id = '${activeConnection}'`).toString().trim()).toBe('f');
    await expect(panel.getByRole('button', { name: 'Retry import', exact: true })).toHaveCount(0);
    await expect(panel.getByRole('button', { name: 'Pause sync', exact: true })).toHaveCount(0);
    expect((await page.request.post(`/p/${pid}/settings/git-sync/${activeConnection}/pause`)).status()).toBe(200);
    expect((await page.request.post(`/p/${pid}/settings/git-sync/${activeConnection}/retry`)).status()).toBe(200);
    expect(sql(`SELECT count(*) FROM background_jobs WHERE payload->>'tag' = 'GitSyncRepository' AND payload::text LIKE '%${pid}%'`).toString().trim()).toBe('2');
    await expect(panel.getByText('Paused', { exact: true })).toBeVisible();
    await Promise.all([
      page.waitForResponse(response => response.request().method() === 'POST' && response.url().endsWith(`/git-sync/${activeConnection}`)),
      panel.getByRole('button', { name: 'Resume sync', exact: true }).press('Enter'),
    ]);
    expect(sql(`SELECT sync_enabled FROM projects.git_sync WHERE id = '${activeConnection}'`).toString().trim()).toBe('t');
    expect(sql(`SELECT count(*) FROM background_jobs WHERE payload->>'tag' = 'GitSyncRepository' AND payload::text LIKE '%${pid}%'`).toString().trim()).toBe('3');
    await expect(panel.getByText('Sync failed', { exact: true })).toBeVisible();
    for (const theme of ['light', 'dark']) {
      await page.context().addCookies([{ name: 'theme', value: theme, url: baseURL! }]);
      await page.reload();
      expect(await panel.evaluate(element => element.scrollWidth <= element.clientWidth)).toBe(true);
      expect(await page.evaluate(() => document.documentElement.scrollWidth)).toBeLessThanOrEqual(320);
      await page.screenshot({ path: testInfo.outputPath(`repository-dashboard-setup-${theme}.png`), fullPage: true });
    }
    sql(`UPDATE projects.project_members SET permission = 'view' WHERE project_id = '${pid}' AND user_id = '${uid}';`);
    await page.reload();
    await expect(panel.getByText('A project editor can configure dashboard sync for this repository.', { exact: true })).toBeVisible();
    await expect(panel.getByRole('status')).toContainText('overview.yaml');
    await expect(panel.getByRole('button', { name: 'Retry import', exact: true })).toHaveCount(0);
    expect((await page.request.post(`/p/${pid}/settings/git-sync/${activeConnection}/retry`)).status()).toBe(403);
    expect((await page.request.post(`/p/${pid}/settings/git-sync/${activeConnection}/pause`)).status()).toBe(403);
    await expect(panel.getByText('Sync enabled', { exact: true })).toBeVisible();
    await expect(panel.getByText('release · ops/dashboards/', { exact: true })).toBeVisible();
    await expect(panel.getByRole('button', { name: 'Save', exact: true })).toHaveCount(0);
    expect((await page.request.post(`/p/${pid}/repositories/${repository}/dashboards`, { form: { credentialId: account, branch: 'forbidden' } })).status()).toBe(403);
    await page.goto(`/p/${pid}/repositories?tab=configuration`);
    await expect(page.locator(`input[value="${signingSecret}"], [data-secret="${signingSecret}"]`)).toHaveCount(0);
  } finally {
    sql(`DELETE FROM background_jobs WHERE payload::text LIKE '%${pid}%';
         DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id = '${uid}';`);
  }
});

test('shared repository connection keeps account choice and provider recovery visible', async ({ page, baseURL }, testInfo) => {
  const uid = randomUUID(), sid = randomUUID(), pid = randomUUID();
  const accounts = [randomUUID(), randomUUID()];
  sql(`INSERT INTO users.users (id, email) VALUES ('${uid}', '${uid}@example.com');
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title) VALUES ('${pid}', 'Shared repository picker');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'admin');
       INSERT INTO projects.git_credentials (id, project_id, account, installation_id, host)
       VALUES ('${accounts[0]}', '${pid}', 'team-a', 42, 'github'), ('${accounts[1]}', '${pid}', 'team-b', 43, 'github');`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }]);
    await page.setViewportSize({ width: 320, height: 800 });
    await page.goto(`/p/${pid}/repositories`);
    await page.getByRole('link', { name: 'Add repositories', exact: true }).click();
    await expect(page).toHaveURL(`/p/${pid}/repositories/connect`);
    const panel = page.locator('#repository-connect-content');
    await expect(panel.getByText('Choose an account to see the repositories it can access.', { exact: true })).toBeVisible();
    const account = panel.getByRole('combobox', { name: /Repository account/ });
    await account.selectOption(accounts[1]);
    await expect(panel.getByText(/Could not load repositories/)).toBeVisible();
    await expect(account).toHaveValue(accounts[1]);
    await expect(page.locator('#repository-connect-content')).toHaveCount(1);
    await expect(page).toHaveURL(`/p/${pid}/repositories/connect?credentialId=${accounts[1]}`);
    await page.reload();
    await expect(account).toHaveValue(accounts[1]);
    const retry = panel.getByRole('link', { name: 'Retry', exact: true });
    await account.focus();
    await page.keyboard.press('Tab');
    await expect(retry).toBeFocused();
    await Promise.all([
      page.waitForResponse(response => response.request().method() === 'GET' && response.url().endsWith(`/repositories/connect?credentialId=${accounts[1]}`)),
      page.keyboard.press('Enter'),
    ]);
    await page.waitForLoadState('networkidle');
    await expect(panel.getByText(/Could not load repositories/)).toBeVisible();
    await expect(account).toHaveValue(accounts[1]);
    await expect(retry).toBeFocused();
    await expect(page.locator('#repository-connect-content')).toHaveCount(1);
    await expect(panel.getByRole('link', { name: 'Connect GitHub account', exact: true })).toBeVisible();
    expect(await page.evaluate(() => document.documentElement.scrollWidth)).toBeLessThanOrEqual(320);
    await page.screenshot({ path: testInfo.outputPath('repository-connection-error.png'), fullPage: true });
    const emptySelection = await page.request.post(`/p/${pid}/repositories/connect?credentialId=${accounts[1]}`, { form: {} });
    expect(emptySelection.status()).toBe(200);
    expect((await emptySelection.text()).includes('No private key found')).toBe(false);
    sql(`UPDATE projects.project_members SET permission = 'view' WHERE project_id = '${pid}' AND user_id = '${uid}';`);
    const denied = await page.request.post(`/p/${pid}/repositories/connect?credentialId=${accounts[1]}`, { form: { repoFullName: 'team-b/private' } });
    expect(denied.status()).toBe(403);
    expect(sql(`SELECT count(*) FROM projects.repositories WHERE project_id = '${pid}'`).toString().trim()).toBe('0');
  } finally {
    sql(`DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id = '${uid}';`);
  }
});
