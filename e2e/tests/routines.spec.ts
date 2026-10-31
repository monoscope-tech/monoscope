import { test, expect } from '@playwright/test';
import { randomUUID } from 'node:crypto';
import { sql } from './helpers';

test('routine detail panels preserve conversation drafts, run history, and artifacts', async ({ page, baseURL }, testInfo) => {
  const uid = randomUUID(), sid = randomUUID(), pid = randomUUID(), run = randomUUID(), repository = randomUUID(), archivedConversation = randomUUID(), archivedRun = randomUUID();
  const email = `${uid.replaceAll('-', '').repeat(2)}@example.test`;
  sql(`INSERT INTO users.users (id, email) VALUES ('${uid}', '${email}');
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title) VALUES ('${pid}', 'Routine lifecycle');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'admin');
       INSERT INTO projects.repositories (id, project_id, host, owner, repo) VALUES ('${repository}', '${pid}', 'github', 'team', 'checkout');`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }]);
    const root = `/p/${pid}/ai`;
    await page.goto(`${root}/routines`);
    await expect(page.getByText('No routines installed', { exact: true })).toBeVisible();
    await page.getByRole('button', { name: 'Add Slack investigation', exact: true }).click();
    await page.getByText('Routine instructions', { exact: true }).click();
    await page.getByRole('textbox', { name: 'Instructions', exact: true }).fill('Compare the deployment with checkout latency.');
    await Promise.all([
      page.waitForResponse(r => r.request().method() === 'POST' && r.url().endsWith('/routine/instructions')),
      page.getByRole('button', { name: 'Save instructions', exact: true }).click(),
    ]);
    await page.waitForLoadState('networkidle');
    const [routine, conversation] = sql(`SELECT id, conversation_id FROM apis.ai_routines WHERE project_id = '${pid}';`).toString().trim().split('|');
    sql(`INSERT INTO apis.ai_routine_runs (id, root_run_id, routine_id, project_id, conversation_id, scheduled_at, started_at, status, source, requested_by, input, instructions, template_version)
         SELECT '${run}', '${run}', id, project_id, conversation_id, now(), now(), 'running',
           '{"tag":"TriggeredRun","contents":{"tag":"SlackCommand","contents":"${conversation}"}}', '${uid}', 'Investigate checkout', instructions, template_version
         FROM apis.ai_routines WHERE id = '${routine}';
         INSERT INTO apis.ai_artifacts (project_id, conversation_id, key, title, content) VALUES ('${pid}', '${conversation}', 'checkout', 'Checkout evidence', 'Later conversation edit.');
         INSERT INTO apis.ai_chat_messages (project_id, conversation_id, role, content, metadata, created_at)
         VALUES ('${pid}', '${conversation}', 'assistant', 'I saved the checkout evidence. Continue the investigation here.',
           '[{"name":"save_artifact","args":{},"result_preview":"Saved checkout evidence","raw_data":"checkout"}]', now());
         INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES ('${pid}', '${archivedConversation}', 'web');
         INSERT INTO apis.ai_routine_runs (id, root_run_id, routine_id, project_id, conversation_id, scheduled_at, started_at, finished_at, status, source, requested_by, input, instructions, template_version)
         SELECT '${archivedRun}', '${archivedRun}', id, project_id, '${archivedConversation}', now() - interval '1 day', now() - interval '1 day', now() - interval '1 day', 'succeeded',
           '{"tag":"TriggeredRun","contents":{"tag":"SlackCommand","contents":"${archivedConversation}"}}', '${uid}', 'Earlier investigation', instructions, template_version
         FROM apis.ai_routines WHERE id = '${routine}';
         INSERT INTO apis.ai_routine_run_artifacts (run_id, key, title, content) VALUES ('${archivedRun}', 'retained', 'Earlier evidence', 'Retained evidence from a deleted source conversation.');
         DELETE FROM apis.ai_conversations WHERE project_id = '${pid}' AND conversation_id = '${archivedConversation}';`);
    await page.goto(`${root}/${conversation}`);
    const composer = page.getByRole('textbox', { name: 'Message Monoscope AI', exact: true });
    await composer.fill('Keep this follow-up while I inspect the run.');
    await page.getByRole('region', { name: 'Routine runs', exact: true }).getByRole('link').first().click();
    await expect(page).toHaveURL(`${root}/${conversation}`);
    await expect(composer).toHaveValue('Keep this follow-up while I inspect the run.');
    await expect(page.getByRole('complementary', { name: 'Conversation details', exact: true })).toBeVisible();
    const panelBounds = await page.locator('#ai-inspector').boundingBox();
    const chatBounds = await page.locator('#ai-thread-messages').boundingBox();
    expect(panelBounds!.x).toBeGreaterThanOrEqual(chatBounds!.x + chatBounds!.width - 1);
    await expect(page.getByRole('status')).toHaveText('Running');
    await expect(page.getByText(`Requested by ${email}`, { exact: true })).toBeVisible();
    await expect(page.getByText('Artifacts will appear when this run finishes.', { exact: true })).toBeVisible();
    const inspector = page.getByRole('complementary', { name: 'Conversation details', exact: true });
    const instructions = inspector.getByText('Instructions used for this run', { exact: true });
    await instructions.focus();
    await page.keyboard.press('Enter');
    await page.waitForResponse(r => r.request().method() === 'GET' && new URL(r.url()).pathname.endsWith(`/runs/${run}`));
    await expect.poll(() => instructions.evaluate(el => (el.parentElement as HTMLDetailsElement).open)).toBe(true);
    await expect(inspector.getByText('Compare the deployment with checkout latency.', { exact: true })).toBeVisible();
    sql(`INSERT INTO apis.ai_routine_run_artifacts (run_id, key, title, content) VALUES ('${run}', 'checkout', 'Checkout evidence', 'Original checkout evidence.');
         UPDATE apis.ai_routine_runs SET status = 'succeeded', finished_at = now() WHERE id = '${run}';`);
    await expect(page.getByRole('status')).toHaveText('Succeeded', { timeout: 12000 });
    await expect(page.getByRole('region', { name: 'Routine runs', exact: true }).getByRole('link').first().getByText('Succeeded', { exact: true })).toBeVisible();
    await expect(page.locator('#routine-run')).not.toHaveAttribute('hx-trigger');
    await expect(page.getByText('Original checkout evidence.', { exact: true })).toBeVisible();
    await expect(page.locator('#ai-thread-messages')).toContainText('I saved the checkout evidence. Continue the investigation here.');
    await expect(inspector).not.toContainText('I saved the checkout evidence. Continue the investigation here.');
    await expect(page.getByText('Later conversation edit.', { exact: true })).toHaveCount(0);
    await expect.poll(() => instructions.evaluate(el => (el.parentElement as HTMLDetailsElement).open)).toBe(true);
    await page.getByRole('link', { name: 'Back to conversation', exact: true }).click();
    await expect(inspector).toBeHidden();
    await page.getByRole('link', { name: 'checkout', exact: true }).click();
    await expect(inspector.getByText('Later conversation edit.', { exact: true })).toBeVisible();
    await page.screenshot({ path: testInfo.outputPath('conversation-artifact-desktop.png') });
    await expect(page).toHaveURL(`${root}/${conversation}`);
    await expect(composer).toHaveValue('Keep this follow-up while I inspect the run.');
    await page.getByRole('link', { name: 'Back to conversation', exact: true }).click();
    await expect(inspector).toBeHidden();
    await expect(composer).toHaveValue('Keep this follow-up while I inspect the run.');
    await page.getByRole('region', { name: 'Routine runs', exact: true }).getByRole('link').last().click();
    await expect(inspector.getByText('Retained evidence from a deleted source conversation.', { exact: true })).toBeVisible();
    await expect(page.getByRole('link', { name: 'All routines', exact: true })).toHaveCount(0);
    await page.getByRole('link', { name: 'Back to conversation', exact: true }).click();
    await expect(inspector).toBeHidden();
    await expect(page).toHaveURL(`${root}/${conversation}`);
    await expect(composer).toHaveValue('Keep this follow-up while I inspect the run.');
    sql(`UPDATE projects.project_members SET active = false WHERE project_id = '${pid}' AND user_id = '${uid}';`);
    await page.goto(`${root}/runs/${run}`);
    await expect(page.getByText('Original checkout evidence.', { exact: true })).toHaveCount(0);
    const renewedSession = randomUUID();
    sql(`UPDATE projects.project_members SET active = true WHERE project_id = '${pid}' AND user_id = '${uid}';
         INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${renewedSession}', '${uid}');`);
    await page.context().addCookies([{ name: 'monoscope_session', value: renewedSession, url: baseURL! }]);
    await page.goto(`${root}/runs/${run}`);
    await page.screenshot({ path: testInfo.outputPath('routine-run-desktop.png') });
    for (const theme of ['light', 'dark']) {
      await page.evaluate(value => document.body.dataset.theme = value, theme);
      await page.setViewportSize({ width: 320, height: 844 });
      await expect.poll(async () => { const box = await page.locator('#side-nav-menu').boundingBox(); return box!.x + box!.width; }).toBeLessThanOrEqual(0);
      expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
      await page.screenshot({ path: testInfo.outputPath(`routine-run-${theme}.png`) });
      await expect(composer).toBeHidden();
      await inspector.focus();
      await page.keyboard.press('Escape');
      await expect(inspector).toBeHidden();
      await expect(composer).toBeVisible();
      await composer.fill('Mobile follow-up stays in the conversation.');
      await page.getByRole('link', { name: 'checkout', exact: true }).click();
      await expect(inspector.getByText('Later conversation edit.', { exact: true })).toBeVisible();
      await page.screenshot({ path: testInfo.outputPath(`conversation-artifact-${theme}.png`) });
      await page.getByRole('link', { name: 'Back to conversation', exact: true }).click();
      await expect(composer).toBeFocused();
      await expect(composer).toHaveValue('Mobile follow-up stays in the conversation.');
      await page.getByRole('region', { name: 'Routine runs', exact: true }).getByRole('link').first().click();
    }
    expect((await page.request.delete(`${root}/${conversation}`)).ok()).toBe(true);
    await page.goto(`${root}/runs/${run}`);
    await expect(page.getByText('Original checkout evidence.', { exact: true })).toBeVisible();
    await expect(page.getByRole('link', { name: 'Open conversation and follow up', exact: true })).toHaveCount(0);
    await expect(page.getByRole('link', { name: 'Back to conversation', exact: true })).toHaveCount(0);
    await expect(page.getByRole('link', { name: 'All routines', exact: true })).toBeVisible();
    await page.keyboard.press('Escape');
    await expect(page).toHaveURL(`${root}/routines`);
    await page.getByRole('link', { name: 'Deleted routine', exact: true }).first().click();
    await expect(page).toHaveURL(`${root}/runs/${run}`);
    await page.getByRole('link', { name: 'All routines', exact: true }).click();
    await expect(page).toHaveURL(`${root}/routines`);
    await page.getByRole('button', { name: 'Add Issue enrichment', exact: true }).click();
    await expect(page.getByText('On a new issue · Active', { exact: true })).toBeVisible();
    await expect(page.getByText('No runs yet. An active routine starts when the next matching event arrives.', { exact: true })).toBeVisible();
    await page.getByText('Routine instructions', { exact: true }).click();
    await expect(page.getByRole('textbox', { name: 'Instructions', exact: true })).toContainText('Update issue metadata');
    expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
    await page.screenshot({ path: testInfo.outputPath('issue-enrichment-narrow.png') });
    await page.goto(`/p/${pid}/repositories/${repository}`);
    const reviewForm = page.locator('form').filter({ has: page.getByRole('button', { name: 'Save review settings', exact: true }) });
    for (const label of await reviewForm.locator('label').all()) {
      expect(await label.evaluate(el => el.scrollWidth <= el.clientWidth)).toBe(true);
      const bounds = await label.boundingBox(), field = await label.locator('..').boundingBox();
      expect(bounds!.x + bounds!.width).toBeLessThanOrEqual(field!.x + field!.width + 1);
    }
    const evidence = reviewForm.getByRole('combobox', { name: 'Production evidence in GitHub', exact: true });
    await evidence.focus();
    await expect(evidence).toBeFocused();
    await page.screenshot({ path: testInfo.outputPath('repository-review-settings-narrow.png') });
  } finally {
    sql(`DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id = '${uid}';`);
  }
});

test('action history and policy controls preserve drafts and check current roles', async ({ page, baseURL }, testInfo) => {
  const uid = randomUUID(), sid = randomUUID(), pid = randomUUID(), conversation = randomUUID(), run = randomUUID(), operation = randomUUID();
  sql(`INSERT INTO users.users (id, email) VALUES ('${uid}', '${uid}@example.test');
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title) VALUES ('${pid}', 'Operation acceptance');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'admin');
       INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type, title)
         VALUES ('${pid}', '${conversation}', 'web', 'Operation acceptance');
       INSERT INTO apis.ai_routines (project_id, conversation_id, requested_by, interval_minutes)
         VALUES ('${pid}', '${conversation}', '${uid}', 15);
       INSERT INTO apis.ai_chat_messages (project_id, conversation_id, role, content)
         VALUES ('${pid}', '${conversation}', 'assistant', 'The action outcome is uncertain.');
       INSERT INTO apis.ai_routine_runs (id, root_run_id, project_id, conversation_id, scheduled_at, started_at, finished_at, status, source, requested_by, input, policy)
         VALUES ('${run}', '${run}', '${pid}', '${conversation}', now(), now(), now(), 'succeeded',
           '{"tag":"InteractiveRun","contents":{"tag":"WebMessage"}}', '${uid}', 'Create a dashboard', 'autonomous');
       INSERT INTO apis.ai_agent_operations (id, project_id, run_id, requested_by, checkpoint_revision, proposal, outcome)
         VALUES ('${operation}', '${pid}', '${run}', '${uid}', 1,
           '{"call":{"id":"create","type":"function","function":{"name":"create_dashboard","arguments":{"body":{"title":"Browser action proposal"}}}},"policy":{"capability":{"tag":"ProjectWrite"},"execution":"autonomous","permission":"admin"}}',
           '{"tag":"OperationUnknown"}');`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }]);
    await page.goto(`/p/${pid}/ai/${conversation}`);
    const composer = page.getByRole('textbox', { name: 'Message Monoscope AI', exact: true });
    await composer.fill('Keep this follow-up draft.');
    const policy = page.getByRole('combobox', { name: 'Execution policy', exact: true });
    await expect(policy).toHaveValue('autonomous');
    for (const value of ['observe', 'autonomous']) {
      await policy.selectOption(value);
      await Promise.all([
        page.waitForResponse(response => response.url().endsWith('/routine/policy') && response.request().method() === 'POST'),
        page.getByRole('button', { name: 'Save policy', exact: true }).click(),
      ]);
      await expect(composer).toHaveValue('Keep this follow-up draft.');
      await expect(page).toHaveURL(`/p/${pid}/ai/${conversation}`);
      expect(sql(`SELECT policy FROM apis.ai_routines WHERE project_id = '${pid}' AND conversation_id = '${conversation}'`).toString().trim()).toBe(value);
    }
    await page.getByRole('region', { name: 'Routine runs', exact: true }).getByRole('link').first().click();
    const history = page.getByRole('region', { name: 'Action history', exact: true });
    await expect(history.getByText('Outcome uncertain', { exact: true })).toBeVisible();
    await history.getByText('Proposal and outcome', { exact: true }).click();
    await expect(history.getByText(/Browser action proposal/)).toBeVisible();
    sql(`UPDATE apis.ai_agent_operations SET outcome = '{"tag":"OperationReturned","contents":{"formatted":"Dashboard created.","rawData":null}}' WHERE id = '${operation}';`);
    await Promise.all([
      page.waitForResponse(response => response.url().includes(`/ai/runs/${run}?conversation=`) && response.request().method() === 'GET'),
      history.getByRole('link', { name: 'Refresh actions', exact: true }).click(),
    ]);
    await expect(history.getByRole('link', { name: 'Refresh actions', exact: true })).not.toHaveClass(/htmx-request/);
    await expect(history.getByText('Response saved', { exact: true })).toBeVisible();
    await expect(composer).toHaveValue('Keep this follow-up draft.');
    await expect(page).toHaveURL(`/p/${pid}/ai/${conversation}`);
    sql(`UPDATE projects.project_members SET permission = 'view' WHERE project_id = '${pid}' AND user_id = '${uid}';`);
    await Promise.all([
      page.waitForResponse(response => response.url().includes(`/ai/runs/${run}?conversation=`) && response.request().method() === 'GET'),
      history.getByRole('link', { name: 'Refresh actions', exact: true }).click(),
    ]);
    await expect(history.getByRole('link', { name: 'Refresh actions', exact: true })).not.toHaveClass(/htmx-request/);
    await expect(history.getByText('Action details require edit permission.', { exact: true })).toBeVisible();
    await expect(history.getByText('Proposal and outcome', { exact: true })).toHaveCount(0);
    await expect(history.getByText(/Browser action proposal/)).toHaveCount(0);
    await expect(composer).toHaveValue('Keep this follow-up draft.');
    await page.screenshot({ path: testInfo.outputPath('action-history.png') });
  } finally {
    sql(`DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id = '${uid}';`);
  }
});

test('restricted conversation evidence is not served after a role downgrade', async ({ page, baseURL }) => {
  const uid = randomUUID(), sid = randomUUID(), pid = randomUUID(), conversation = randomUUID(), run = randomUUID();
  const secret = 'restricted-evidence-browser-canary';
  sql(`INSERT INTO users.users (id, email) VALUES ('${uid}', '${uid}@example.test');
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title) VALUES ('${pid}', 'Evidence acceptance');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'admin');
       INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type, title, evidence_permission)
         VALUES ('${pid}', '${conversation}', 'web', 'Restricted administrative findings', 'admin');
       INSERT INTO apis.ai_chat_messages (project_id, conversation_id, role, content)
         VALUES ('${pid}', '${conversation}', 'assistant', '${secret}');
       INSERT INTO apis.ai_routine_runs (id, root_run_id, project_id, conversation_id, scheduled_at, started_at, finished_at, status, source, requested_by, input, evidence_permission)
         VALUES ('${run}', '${run}', '${pid}', '${conversation}', now(), now(), now(), 'succeeded',
           '{"tag":"InteractiveRun","contents":{"tag":"WebMessage"}}', '${uid}', 'Inspect administrative evidence', 'admin');`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }]);
    await page.goto(`/p/${pid}/ai/${conversation}`);
    await expect(page.locator('#ai-thread-messages')).toContainText(secret);
    sql(`UPDATE projects.project_members SET permission = 'view' WHERE project_id = '${pid}' AND user_id = '${uid}';`);
    for (const path of [`/p/${pid}/ai/${conversation}`, `/p/${pid}/ai/runs/${run}`]) {
      const response = await page.request.get(path);
      expect(response.status()).toBe(403);
      expect(await response.text()).not.toContain(secret);
      await page.goto(path).catch(error => expect(error.message).toContain('ERR_HTTP_RESPONSE_CODE_FAILURE'));
      await expect(page.locator('body')).not.toContainText(secret);
    }
    await page.goto(`/p/${pid}/ai`);
    await expect(page.getByText('Restricted administrative findings', { exact: true })).toHaveCount(0);
  } finally {
    sql(`DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id = '${uid}';`);
  }
});
