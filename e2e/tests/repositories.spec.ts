import { test, expect } from '@playwright/test';
import { randomUUID } from 'node:crypto';
import { DEMO_PROJECT, sql } from './helpers';

test('repository removal preserves active syncs and supports retry in both themes', async ({ page, baseURL }, testInfo) => {
  const uid = randomUUID(), sid = randomUUID(), pid = randomUUID(), rid = randomUUID(), sync = randomUUID(), did = randomUUID();
  sql(`INSERT INTO users.users (id, email) VALUES ('${uid}', '${uid}@example.com');
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title) VALUES ('${pid}', 'Removal recovery');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'admin');
       INSERT INTO projects.repositories (id, project_id, host, owner, repo) VALUES ('${rid}', '${pid}', 'github', 'team', 'dashboards');
       INSERT INTO projects.git_sync (id, project_id, owner, repo, branch, access_token)
       VALUES ('${sync}', '${pid}', 'team', 'dashboards', 'main', 'fixture');
       INSERT INTO projects.dashboards (id, project_id, created_by, title, schema, git_sync_id, file_path, file_sha)
       VALUES ('${did}', '${pid}', '${uid}', 'Overview', '{"widgets":[]}', '${sync}', 'overview.yaml', 'provider-blob');
       INSERT INTO projects.repository_sync_leases (sync_id, owner, expires_at)
       VALUES ('${sync}', '${randomUUID()}', now() + interval '3 minutes');`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }]);
    await page.setViewportSize({ width: 320, height: 800 });
    for (const theme of ['light', 'dark']) {
      await page.context().addCookies([{ name: 'theme', value: theme, url: baseURL! }]);
      await page.goto(`/p/${pid}/repositories/${rid}`);
      const remove = page.locator('#main-content').getByRole('button', { name: 'Remove repository', exact: true });
      await remove.press('Enter');
      const dialog = page.getByRole('dialog', { name: 'Remove team/dashboards?', exact: true });
      const confirm = dialog.getByRole('button', { name: /^Remove repository(?: Loading)?$/ });
      await expect(dialog.getByRole('button', { name: 'Cancel', exact: true })).toBeFocused();
      let releaseRemoval!: () => void;
      const gate = new Promise<void>(resolve => { releaseRemoval = resolve; });
      await page.route(`**/repositories/${rid}`, async route => { await gate; await route.continue(); }, { times: 1 });
      await confirm.press('Enter');
      try {
        await expect(confirm).toBeDisabled();
        await expect(confirm.getByRole('status', { name: 'Loading', exact: true })).toBeVisible();
      } finally { releaseRemoval(); }
      await expect(dialog.getByRole('alert')).toContainText('A sync is running');
      await expect(confirm).toBeEnabled();
      await expect(dialog).toBeVisible();
      expect(sql(`SELECT git_sync_id FROM projects.dashboards WHERE id = '${did}'`).toString().trim()).toBe(sync);
      expect(await dialog.evaluate(element => element.scrollWidth <= element.clientWidth)).toBe(true);
      await page.screenshot({ path: testInfo.outputPath(`repository-removal-busy-${theme}.png`) });
      await page.keyboard.press('Escape');
      await expect(dialog).toBeHidden();
      await expect(remove).toBeFocused();


    }
    sql(`UPDATE projects.repository_sync_leases SET expires_at = now() - interval '1 second' WHERE sync_id = '${sync}';`);
    await page.goto(`/p/${pid}/repositories/${rid}`);
    await page.locator('#main-content').getByRole('button', { name: 'Remove repository', exact: true }).press('Enter');
    await page.getByRole('dialog').getByRole('button', { name: 'Remove repository', exact: true }).press('Enter');
    await expect(page).toHaveURL(`/p/${pid}/repositories`);
    expect(sql(`SELECT git_sync_id IS NULL AND file_path IS NULL AND file_sha IS NULL FROM projects.dashboards WHERE id = '${did}'`).toString().trim()).toBe('t');
    expect(sql(`SELECT count(*) FROM projects.repository_sync_leases WHERE sync_id = '${sync}'`).toString().trim()).toBe('0');
  } finally {
    sql(`DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id = '${uid}';`);
  }
});


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
    await page.getByRole('navigation', { name: 'Repository views' }).getByRole('link', { name: 'Repositories', exact: true }).click();
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
  await expect(views.getByRole('link', { name: 'Configuration', exact: true })).toHaveCount(0);
  await views.getByRole('link', { name: 'Repositories', exact: true }).click();
  await expect(page).toHaveURL(`${project}/repositories`);
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

test('one repository list owns service links and independent capabilities in both themes', async ({ page, baseURL }, testInfo) => {
  const uid = randomUUID(), sid = randomUUID(), pid = randomUUID(), sync = randomUUID(), cron = randomUUID();
  const accounts = [randomUUID(), randomUUID()], repositories = [randomUUID(), randomUUID()];
  sql(`INSERT INTO users.users (id, email, is_sudo) VALUES ('${uid}', '${uid}@example.com', true);
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title) VALUES ('${pid}', 'Repository capabilities');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'admin');
       INSERT INTO projects.git_credentials (id, project_id, account, installation_id, host)
       VALUES ('${accounts[0]}', '${pid}', 'team-a', 42, 'github'), ('${accounts[1]}', '${pid}', 'team-b', 43, 'github');
       INSERT INTO projects.repositories (id, project_id, host, owner, repo, credential_id)
       VALUES ('${repositories[0]}', '${pid}', 'github', 'team-a', 'checkout', '${accounts[0]}'), ('${repositories[1]}', '${pid}', 'github', 'team-b', 'checkout', '${accounts[1]}');
       INSERT INTO projects.git_sync (id, project_id, owner, repo, branch, access_token) VALUES ('${sync}', '${pid}', 'team-a', 'checkout', 'main', 'fixture');
       INSERT INTO apis.cron_monitors (id, project_id, slug, name, interval_secs, grace_secs) VALUES ('${cron}', '${pid}', 'daily', 'Daily check', 3600, 60);`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }]);
    await page.setViewportSize({ width: 320, height: 800 });
    for (const [index, repository] of repositories.entries()) {
      await page.goto(`/p/${pid}/repositories`);
      await expect(page.getByRole('link', { name: 'Configuration', exact: true })).toHaveCount(0);
      await page.getByRole('link', { name: `team-${index === 0 ? 'a' : 'b'}/checkout`, exact: true }).press('Enter');
      await page.getByRole('link', { name: 'Configure source context', exact: true }).press('Enter');
      const panel = page.locator('#code-mappings-content');
      await expect(panel.getByRole('combobox', { name: /Repository account/ })).toHaveCount(0);
      await expect(panel.locator('input[name="credentialId"]')).toHaveValue(accounts[index]);
      await expect(panel.locator('input[name="repo"]')).toHaveValue(`team-${index === 0 ? 'a' : 'b'}/checkout`);
      await panel.locator('input[name="service"]').fill('checkout');
      await panel.getByRole('button', { name: 'Link service', exact: true }).press('Enter');
      await expect(panel.getByRole('button', { name: /Unlink/ })).toHaveCount(1);
      expect(sql(`SELECT credential_id FROM projects.code_mappings WHERE project_id = '${pid}' AND owner = 'team-${index === 0 ? 'a' : 'b'}'`).toString().trim()).toBe(accounts[index]);
    }
    const base = `/p/${pid}/repositories/${repositories[0]}`;
    const otherMapping = sql(`SELECT id FROM projects.code_mappings WHERE project_id = '${pid}' AND owner = 'team-b'`).toString().trim();
    expect((await page.request.delete(`${base}/source/${otherMapping}`)).status()).toBe(404);
    await page.goto(base);
    await page.getByRole('button', { name: 'Disable source context', exact: true }).press('Enter');
    await expect(page.getByRole('button', { name: 'Enable source context', exact: true })).toBeVisible();
    expect(sql(`SELECT count(*) FROM projects.code_mappings WHERE project_id = '${pid}'`).toString().trim()).toBe('2');
    await page.getByRole('button', { name: 'Save review settings', exact: true }).press('Enter');
    await expect(page.locator('#repository-detail')).toHaveCount(1);
    await expect.poll(() => sql(`SELECT enabled FROM projects.pr_review_settings WHERE project_id = '${pid}' AND owner = 'team-a'`).toString().trim()).toBe('t');
    await page.getByText('Move existing resources into this folder', { exact: true }).click();
    await page.getByRole('combobox', { name: /Cron/ }).selectOption(cron);
    await page.getByRole('combobox', { name: /Cron/ }).locator('..').locator('..').getByRole('button', { name: 'Move into folder', exact: true }).press('Enter');
    await expect(page.getByText(`monitors/cron-${cron}.yaml`, { exact: true })).toBeVisible();
    expect(sql(`SELECT local_id FROM projects.repository_resources WHERE sync_id = '${sync}' AND kind = 'cron'`).toString().trim()).toBe(cron);
    for (const theme of ['dark', 'light']) {
      await page.context().addCookies([{ name: 'theme', value: theme, url: baseURL! }]);
      await page.reload();
      expect(await page.evaluate(() => document.documentElement.scrollWidth)).toBeLessThanOrEqual(320);
      expect(await page.locator('[id]').evaluateAll(elements => elements.map(element => element.id).filter((id, index, ids) => ids.indexOf(id) !== index))).toEqual([]);
      await page.screenshot({ path: testInfo.outputPath(`repository-capabilities-${theme}.png`), fullPage: true });
    }
    sql(`UPDATE projects.project_members SET permission = 'view' WHERE project_id = '${pid}' AND user_id = '${uid}';`);
    await page.reload();
    await expect(page.getByRole('button', { name: /Enable source context|Save review settings|Move into folder|Remove repository/ })).toHaveCount(0);
    expect((await page.request.post(`${base}/source/enabled`, { form: { enabled: 'true' } })).status()).toBe(403);
    expect((await page.request.post(`${base}/folder`, { form: { kind: 'cron', localId: cron } })).status()).toBe(403);
    expect((await page.request.delete(base)).status()).toBe(403);
    expect((await page.request.get(`/p/${DEMO_PROJECT}/repositories/${repositories[0]}`)).status()).toBe(404);
  } finally {
    sql(`DELETE FROM background_jobs WHERE payload::text LIKE '%${pid}%'; DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id = '${uid}';`);
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
    await page.getByRole('link', { name: 'Configure sync', exact: true }).click();
    await expect(page).toHaveURL(`/p/${pid}/repositories/${repository}/dashboards`);
    const panel = page.locator('#repository-dashboard-content');
    await panel.getByRole('combobox', { name: /Repository account/ }).selectOption(account);
    await panel.getByRole('textbox', { name: 'Branch', exact: true }).fill('release');
    await panel.getByRole('textbox', { name: 'Folder in repo', exact: true }).fill('/ops/');
    await panel.getByRole('button', { name: 'Enable configuration sync', exact: true }).click();
    await expect(panel.getByText('Connected', { exact: true })).toBeVisible();
    await expect(panel).toHaveCount(1);
    await expect(panel.getByRole('textbox', { name: /Branch/ })).toHaveValue('release');
    await expect(panel.getByLabel('Webhook secret', { exact: true })).toHaveAttribute('readonly');
    const signingSecret = sql(`SELECT webhook_secret FROM projects.git_sync WHERE project_id = '${pid}'`).toString().trim();
    expect(signingSecret).toMatch(/^[0-9a-f-]{36}$/);
    expect(sql(`SELECT branch || ':' || path_prefix FROM projects.git_sync WHERE project_id = '${pid}'`).toString().trim()).toBe('release:ops');
    expect((await page.request.post(`/p/${pid}/repositories/${repository}/dashboards`, { form: { credentialId: account, branch: 'different' } })).status()).toBe(200);
    expect(sql(`SELECT branch FROM projects.git_sync WHERE project_id = '${pid}'`).toString().trim()).toBe('release');
    const activeConnection = sql(`SELECT id FROM projects.git_sync WHERE project_id = '${pid}'`).toString().trim();
    expect(sql(`SELECT count(*) FROM background_jobs WHERE payload->>'tag' = 'GitSyncRepository' AND payload::text LIKE '%${activeConnection}%'`).toString().trim()).toBe('1');
    sql(`UPDATE projects.git_sync SET last_error = jsonb_build_object('operation', jsonb_build_object('tag', 'ImportDashboards'), 'message', 'dashboards/overview.yaml: Remote file conflicts with a local dashboard awaiting its first push.') WHERE id = '${activeConnection}';`);
    await page.reload();
    await expect(panel.getByRole('status')).toContainText('overview.yaml');
    await Promise.all([
      page.waitForResponse(response => response.request().method() === 'POST' && response.url().endsWith(`/git-sync/${activeConnection}/retry`)),
      panel.getByRole('button', { name: 'Retry import', exact: true }).press('Enter'),
    ]);
    await expect(panel.locator(`[id="git-sync-${activeConnection}"]`)).toHaveCount(1);
    await expect(panel.getByRole('status')).toContainText('overview.yaml');
    expect(sql(`SELECT count(*) FROM background_jobs WHERE payload->>'tag' = 'GitSyncRepository' AND payload::text LIKE '%${activeConnection}%'`).toString().trim()).toBe('2');
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
    expect(sql(`SELECT count(*) FROM background_jobs WHERE payload->>'tag' = 'GitSyncRepository' AND payload::text LIKE '%${activeConnection}%'`).toString().trim()).toBe('2');
    await expect(panel.getByText('Paused', { exact: true })).toBeVisible();
    await Promise.all([
      page.waitForResponse(response => response.request().method() === 'POST' && response.url().endsWith(`/git-sync/${activeConnection}`)),
      panel.getByRole('button', { name: 'Resume sync', exact: true }).press('Enter'),
    ]);
    expect(sql(`SELECT sync_enabled FROM projects.git_sync WHERE id = '${activeConnection}'`).toString().trim()).toBe('t');
    expect(sql(`SELECT count(*) FROM background_jobs WHERE payload->>'tag' = 'GitSyncRepository' AND payload::text LIKE '%${activeConnection}%'`).toString().trim()).toBe('3');
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
    await expect(panel.getByText('A project editor can configure configuration sync for this repository.', { exact: true })).toBeVisible();
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

test('GitHub installation IDs cannot create project grants without authorization', async ({ page, baseURL }, testInfo) => {
  const uid = randomUUID(), sid = randomUUID(), pid = randomUUID();
  sql(`INSERT INTO users.users (id, email) VALUES ('${uid}', '${uid}@example.com');
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title) VALUES ('${pid}', 'Installation authorization');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'admin');`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }]);
    expect((await page.request.get(`/p/${pid}/settings/git-sync/repos?installationId=424242`)).status()).toBe(403);
    expect((await page.request.post(`/p/${pid}/settings/git-sync/select`, { form: { repoFullName: 'foreign/private', branch: 'main', installationId: '424242' } })).status()).toBe(403);
    await page.setViewportSize({ width: 320, height: 800 });
    for (const theme of ['light', 'dark']) {
      await page.context().addCookies([{ name: 'theme', value: theme, url: baseURL! }]);
      await page.goto(`/github/callback?installation_id=424242&state=${pid}:code`);
      await expect(page.getByRole('alert')).toContainText('missing, expired, or already used');
      await expect(page.getByRole('link', { name: 'Back to projects', exact: true })).toBeVisible();
      expect(await page.evaluate(() => document.documentElement.scrollWidth)).toBeLessThanOrEqual(320);
      await page.screenshot({ path: testInfo.outputPath(`repository-authorization-${theme}.png`), fullPage: true });
    }
    expect(sql(`SELECT count(*) FROM projects.git_credentials WHERE project_id = '${pid}'`).toString().trim()).toBe('0');
    expect(sql(`SELECT count(*) FROM projects.git_sync WHERE project_id = '${pid}'`).toString().trim()).toBe('0');
  } finally {
    sql(`DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id = '${uid}';`);
  }
});


test('service investigations reach only applicable repositories by keyboard in both themes', async ({ page, baseURL }, testInfo) => {
  const uid = randomUUID(), sid = randomUUID(), pid = randomUUID();
  const account = randomUUID(), enterprise = randomUUID();
  const repositories = [randomUUID(), randomUUID(), randomUUID(), randomUUID()];
  const service = 'team/api & jobs';
  sql(`INSERT INTO users.users (id, email) VALUES ('${uid}', '${uid}@example.com');
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title) VALUES ('${pid}', 'Service repositories');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'view');
       INSERT INTO projects.git_credentials (id, project_id, account, installation_id, host, api_base)
       VALUES ('${account}', '${pid}', 'acme', 42, 'github', NULL), ('${enterprise}', '${pid}', 'acme', 43, 'github', 'https://git.example.com');
       INSERT INTO projects.repositories (id, project_id, host, api_base, owner, repo, credential_id)
       VALUES ('${repositories[0]}', '${pid}', 'github', NULL, 'acme', 'checkout', '${account}'),
              ('${repositories[1]}', '${pid}', 'github', NULL, 'acme', 'shared', '${account}'),
              ('${repositories[2]}', '${pid}', 'github', NULL, 'acme', 'payments', '${account}'),
              ('${repositories[3]}', '${pid}', 'github', 'https://git.example.com', 'acme', 'checkout', '${enterprise}');
       INSERT INTO projects.code_mappings (project_id, credential_id, owner, repo, ref, service, path_prefix)
       VALUES ('${pid}', '${account}', 'acme', 'checkout', 'main', '${service}', '/app/'),
              ('${pid}', '${account}', 'acme', 'shared', 'main', NULL, '/shared/'),
              ('${pid}', '${account}', 'acme', 'payments', 'main', 'billing', '/payments/'),
              ('${pid}', '${enterprise}', 'acme', 'checkout', 'main', 'billing', '/enterprise/');
       INSERT INTO apis.service_dependency_edges_env
         (project_id, bucket, env, source_key, source_kind, target_key, target_kind, req_count, error_count, sum_duration_ns, lat_hist)
       VALUES ('${pid}', now() - interval '5 minutes', 'prod', '', 'entry', '${service}', 'service', 10, 0, 10000000, '{"80":10}'),
              ('${pid}', now() - interval '5 minutes', 'prod', '${service}', 'service', 'db:orders', 'database', 10, 0, 10000000, '{"80":10}');`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }]);
    for (const theme of ['light', 'dark']) {
      await page.context().addCookies([{ name: 'theme', value: theme, url: baseURL! }]);
      await page.setViewportSize({ width: 1200, height: 900 });
      await page.goto(`/p/${pid}/service_map?since=1H`);
      const node = page.getByRole('button', { name: `Explore ${service}`, exact: true });
      await expect(node).toBeVisible();
      await node.focus();
      await page.keyboard.press('Enter');
      const menu = page.locator('[data-service-menu]');
      await expect(menu.getByRole('link', { name: 'View repositories', exact: true })).toHaveAttribute('href', `/p/${pid}/repositories/services/${encodeURIComponent(service)}`);
      await expect(menu.locator('a:visible').first()).toBeFocused();
      await page.keyboard.press('Escape');
      await expect(menu).toBeHidden();
      await expect(node).toBeFocused();
      await page.keyboard.press('Enter');
      const link = menu.getByRole('link', { name: 'View repositories', exact: true });
      await link.focus();
      await page.keyboard.press('Enter');
      await expect(page.getByRole('heading', { name: `Repositories for ${service}`, exact: true })).toBeVisible();
      const content = page.locator('#repository-content');
      await expect(content.locator('article')).toHaveCount(2);
      await expect(content.getByRole('link', { name: 'acme/checkout', exact: true })).toHaveAttribute('href', `/p/${pid}/repositories/${repositories[0]}`);
      await expect(content.getByText('All services', { exact: true })).toBeVisible();
      await expect(page.getByRole('link', { name: 'Add repositories', exact: true })).toHaveCount(0);
      await page.setViewportSize({ width: 320, height: 800 });
      await expect.poll(() => page.evaluate(() => document.documentElement.scrollWidth)).toBe(320);
      await expect.poll(() => page.locator('#side-nav-menu').evaluate(element => element.getBoundingClientRect().right)).toBeLessThanOrEqual(0);
      await page.screenshot({ path: testInfo.outputPath(`service-repositories-${theme}.png`) });
      await page.getByRole('link', { name: 'All repositories', exact: true }).click();
      await expect(page.locator('#repository-content article')).toHaveCount(4);
      await page.goBack();
      await expect(page.getByRole('heading', { name: `Repositories for ${service}`, exact: true })).toBeVisible();
    }
    sql(`DELETE FROM projects.code_mappings WHERE project_id = '${pid}' AND service IS NULL;`);
    await page.goto(`/p/${pid}/repositories/services/unmapped`);
    await expect(page.getByRole('heading', { name: 'No repositories linked to this service', exact: true })).toBeVisible();
    await expect(page.getByText('Ask a project editor to link services to their source repositories.', { exact: true })).toBeVisible();
    await expect(page.getByRole('link', { name: 'Choose a repository', exact: true })).toHaveCount(0);
  } finally {
    sql(`DELETE FROM apis.service_dependency_edges_env WHERE project_id = '${pid}';
         DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id = '${uid}';`);
  }
});


test('export recovery queues the failed dashboard and keeps the error visible in both themes', async ({ page, baseURL }, testInfo) => {
  const uid = randomUUID(), sid = randomUUID(), pid = randomUUID(), sync = randomUUID(), rid = randomUUID(), did = randomUUID();
  sql(`INSERT INTO users.users (id, email) VALUES ('${uid}', '${uid}@example.com');
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title) VALUES ('${pid}', 'Export recovery');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'admin');
       INSERT INTO projects.repositories (id, project_id, host, owner, repo) VALUES ('${rid}', '${pid}', 'github', 'team', 'dashboards');
       INSERT INTO projects.git_sync (id, project_id, owner, repo, branch, access_token, last_error)
       VALUES ('${sync}', '${pid}', 'team', 'dashboards', 'main', 'fixture', jsonb_build_object('operation', jsonb_build_object('tag', 'ExportDashboard', 'contents', '${did}'), 'message', 'Could not export overview.yaml. Check repository access and retry.'));
       INSERT INTO projects.dashboards (id, project_id, created_by, title, schema, git_sync_id, file_path)
       VALUES ('${did}', '${pid}', '${uid}', 'Overview', '{"widgets":[]}', '${sync}', 'overview.yaml');`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }]);
    await page.setViewportSize({ width: 320, height: 800 });
    for (const [index, theme] of ['light', 'dark'].entries()) {
      await page.context().addCookies([{ name: 'theme', value: theme, url: baseURL! }]);
      await page.goto(`/p/${pid}/repositories/${rid}/dashboards`);
      const panel = page.locator('#repository-dashboard-content');
      await expect(panel.getByRole('status')).toContainText('Could not export overview.yaml');
      await expect(panel.getByRole('button', { name: 'Retry import', exact: true })).toHaveCount(0);
      const retry = panel.getByRole('button', { name: /^Retry export(?: Loading)?$/ });
      await retry.focus();
      await expect(retry).toBeFocused();
      let releaseRetry!: () => void;
      const gate = new Promise<void>(resolve => { releaseRetry = resolve; });
      await page.route(`**/settings/git-sync/${sync}/retry`, async route => { await gate; await route.continue(); }, { times: 1 });
      await retry.press('Enter');
      try {
        await expect(retry).toBeDisabled();
        await expect(retry.getByRole('status', { name: 'Loading', exact: true })).toBeVisible();
      } finally { releaseRetry(); }
      await expect(page.getByText('Retry queued', { exact: true })).toBeVisible();
      await expect(retry).toBeEnabled();
      await expect(panel.getByRole('status')).toContainText('Could not export overview.yaml');
      expect(sql(`SELECT count(*) FROM background_jobs WHERE payload->>'tag' = 'GitSyncPushDashboard' AND payload::text LIKE '%${did}%'`).toString().trim()).toBe(String(index + 1));
      expect(sql(`SELECT count(*) FROM background_jobs WHERE payload->>'tag' = 'GitSyncRepository' AND payload::text LIKE '%${sync}%'`).toString().trim()).toBe('0');
      await page.getByRole('button', { name: 'Dismiss notification', exact: true }).click();
      await expect.poll(() => page.evaluate(() => document.documentElement.scrollWidth)).toBe(320);
      await page.screenshot({ path: testInfo.outputPath(`export-recovery-${theme}.png`), fullPage: true });
      await panel.getByRole('link', { name: 'View the dashboard that failed to export', exact: true }).click();
      await expect(page).toHaveURL(`/p/${pid}/dashboards/${did}/repository`);
      await expect(page.locator('#main-content').getByRole('heading', { name: 'Dashboard repository', exact: true })).toBeVisible();
    }
    sql(`UPDATE projects.git_sync SET sync_enabled = false WHERE id = '${sync}';`);
    await page.goto(`/p/${pid}/repositories/${rid}/dashboards`);
    await expect(page.getByRole('button', { name: 'Retry export', exact: true })).toHaveCount(0);
    expect((await page.request.post(`/p/${pid}/settings/git-sync/${sync}/retry`)).status()).toBe(200);
    expect(sql(`SELECT count(*) FROM background_jobs WHERE payload->>'tag' = 'GitSyncPushDashboard' AND payload::text LIKE '%${did}%'`).toString().trim()).toBe('2');
    sql(`UPDATE projects.project_members SET permission = 'view' WHERE project_id = '${pid}' AND user_id = '${uid}';`);
    expect((await page.request.post(`/p/${pid}/settings/git-sync/${sync}/retry`)).status()).toBe(403);
  } finally {
    sql(`DELETE FROM background_jobs WHERE payload::text LIKE '%${pid}%';
         DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id = '${uid}';`);
  }
});
