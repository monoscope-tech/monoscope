import { test, expect, type Page } from '@playwright/test';
import { randomUUID } from 'node:crypto';
import { sql } from './helpers';

// A routine's policy and instructions sit in its collapsed Settings disclosure.
const openSettings = async (page: Page) => {
  const settings = page.locator('details[aria-label="Routine configuration"]');
  if (!(await settings.evaluate(element => (element as HTMLDetailsElement).open))) await settings.locator('summary').first().click();
};

test('verified fix evidence stays in the conversation run inspector', async ({ page, baseURL }, testInfo) => {
  const [uid, sid, pid, conversation, run, fix, issue, credential, mapping, attempt, candidate, check, image] = Array.from({ length: 13 }, () => randomUUID());
  const result = { projectId: pid, runId: fix, issueId: issue, attemptId: attempt, candidateId: candidate, base: 'a'.repeat(40), head: 'b'.repeat(40), patchSha256: 'a'.repeat(64), bundleSha256: 'b'.repeat(64), rootCause: 'The runtime dropped the original retry token.', checkId: check, regressionSha256: 'c'.repeat(64), summary: 'Preserve retry tokens' };
  const explanation = { version: 1, source: 'a'.repeat(64), rendererSha256: 'b'.repeat(64), projectId: pid, runId: fix, issueId: issue, candidateId: candidate, head: result.head, checkId: check, regressionSha256: result.regressionSha256, durationMs: 36000, width: 1280, height: 720, renderMs: 1, mediaBytes: 7, videoBytes: 1, resourceCostStatus: 'unpriced', tools: { ffmpeg: 'fixture', graphviz: 'fixture' }, files: Object.fromEntries(['explanation.mp4', 'diagram.svg', 'captions.vtt', 'before.png', 'cause.png', 'change.png', 'after.png'].map(name => [name, 'a'.repeat(64)])) };
  const setup = { mappingId: mapping, credentialId: credential, installationId: 42, service: 'checkout', owner: 'fixture', repo: 'checkout', base: result.base, imageBuildId: image };
  // These retained rows exercise the real read handlers. Native verification and
  // media transport have separate coverage; this case owns inspector behavior.
  sql(`INSERT INTO users.users (id, email) VALUES ('${uid}', '${uid}@example.test');
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title) VALUES ('${pid}', 'Native fix inspector');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'admin');
       INSERT INTO projects.git_credentials (id, project_id, account, installation_id) VALUES ('${credential}', '${pid}', 'fixture', 42);
       INSERT INTO projects.code_mappings (id, project_id, credential_id, owner, repo, service) VALUES ('${mapping}', '${pid}', '${credential}', 'fixture', 'checkout', 'checkout');
       INSERT INTO apis.issues (id, project_id, issue_type, service, target_hash, title) VALUES ('${issue}', '${pid}', 'runtime_exception', 'checkout', 'native-inspector', 'Retain retry tokens');
       INSERT INTO projects.issue_fix_runs (id, project_id, issue_id, setup, billing_provider) VALUES ('${fix}', '${pid}', '${issue}', '${JSON.stringify(setup)}', 'stripe');
       INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type, title) VALUES ('${pid}', '${conversation}', 'web', 'Fix checkout retries');
       INSERT INTO apis.ai_routine_runs (id, root_run_id, project_id, conversation_id, requested_by, input, policy, source, scheduled_at, started_at, finished_at, status, permission_ceiling)
       VALUES ('${run}', '${run}', '${pid}', '${conversation}', '${uid}', 'Fix checkout retries', '"autonomous"', '{"tag":"FixRun","contents":"${fix}"}', now(), now(), now(), 'succeeded', 'admin');
       INSERT INTO projects.issue_fix_jobs (run_id, requested_by, agent_run_id, result, investigation, explanation, media_retained_at, finished_at)
       VALUES ('${fix}', '${uid}', '${run}', '${JSON.stringify(result)}', '{}', '${JSON.stringify(explanation)}', now(), now());
       INSERT INTO apis.ai_artifacts (project_id, conversation_id, key, title, content) VALUES ('${pid}', '${conversation}', 'latest-result', 'Verified issue fix', 'Preserve retry tokens. Retained verification evidence is available in run details.');
       INSERT INTO apis.ai_routine_run_artifacts (run_id, key, title, content) VALUES ('${run}', 'latest-result', 'Verified issue fix', 'Preserve retry tokens. Retained verification evidence is available in run details.');
       INSERT INTO apis.ai_chat_messages (project_id, conversation_id, role, content) VALUES ('${pid}', '${conversation}', 'assistant', 'Preserve retry tokens. Retained verification evidence is available in run details.');`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }, { name: 'isSidebarClosed', value: 'true', url: baseURL! }]);
    expect((await page.request.get(`/p/${pid}/issue-fixes/${fix}`)).status()).toBe(200);
    await page.route('**/issue-fixes/*/media/*', route => {
      const asset = new URL(route.request().url()).pathname.split('/').pop();
      return asset === 'video' ? route.fulfill({ status: 204 })
        : asset === 'captions' ? route.fulfill({ contentType: 'text/vtt', body: 'WEBVTT\n\n00:00:00.000 --> 00:00:01.000\nRetained evidence\n' })
        : route.fulfill({ contentType: 'image/svg+xml', body: '<svg xmlns="http://www.w3.org/2000/svg" width="1280" height="720" viewBox="0 0 1280 720"><rect width="1280" height="720" fill="#172338"/><text x="80" y="360" fill="#f8fafc" font-size="48">Retained verification fixture</text></svg>' });
    });
    const path = `/p/${pid}/ai/${conversation}`;
    for (const width of [320, 1280]) for (const theme of ['light', 'dark']) for (const zoom of [100, 200]) {
      await page.setViewportSize({ width, height: 900 });
      await page.goto(path);
      await page.evaluate(({ theme, zoom }) => { document.body.dataset.theme = theme; document.documentElement.style.fontSize = `${zoom}%`; }, { theme, zoom });
      const composer = page.getByRole('textbox', { name: 'Message Monoscope AI', exact: true });
      await composer.fill('Keep this draft while I review the verified fix.');
      await page.getByRole('region', { name: 'Routine runs', exact: true }).getByRole('link').first().press('Enter');
      const inspector = page.getByRole('complementary', { name: 'Conversation details', exact: true });
      await expect(inspector).toBeVisible();
      await expect(inspector).toBeFocused();
      await expect(inspector.locator('video[aria-label="Bug reproduction and verified fix"]')).toBeVisible();
      await inspector.locator('video').scrollIntoViewIfNeeded();
      await page.screenshot({ path: testInfo.outputPath(`native-fix-evidence-${theme}-${width}-${zoom}.png`) });
      await expect(inspector.getByRole('heading', { name: result.summary, exact: true })).toBeVisible();
      await expect(inspector.locator('video track')).toHaveAttribute('kind', 'captions');
      await inspector.getByRole('img', { name: 'Diagram of the reproduced failure, inferred cause, change, and passing regression', exact: true }).evaluate(element => (element as HTMLImageElement).decode());
      const download = inspector.getByRole('link', { name: 'Download PR package', exact: true });
      await expect(download).toHaveAttribute('href', `/p/${pid}/issue-fixes/${fix}/publication.tar`);
      await expect(download).toHaveAttribute('download', 'publication.tar');
      await download.scrollIntoViewIfNeeded();
      const bounds = await download.boundingBox();
      expect(bounds).not.toBeNull();
      expect(bounds!.x).toBeGreaterThanOrEqual(0);
      expect(bounds!.x + bounds!.width).toBeLessThanOrEqual(width);
      expect(await inspector.evaluate(element => element.scrollWidth <= element.clientWidth)).toBe(true);
      await page.screenshot({ path: testInfo.outputPath(`native-fix-inspector-${theme}-${width}-${zoom}.png`) });
      expect(new URL(page.url()).pathname).toBe(path);
      await inspector.focus();
      await page.keyboard.press('Escape');
      await expect(inspector).toBeHidden();
      await expect(composer).toBeFocused();
      await expect(composer).toHaveValue('Keep this draft while I review the verified fix.');
    }
    await page.getByRole('navigation', { name: 'Documents', exact: true }).getByRole('link', { name: /^Verified issue fix / }).press('Enter');
    await expect(page.locator('#ai-document')).toContainText('Preserve retry tokens.');
    expect(new URL(page.url()).pathname).toBe(path);
    sql(`UPDATE projects.issue_fix_jobs SET media_retained_at = NULL WHERE run_id = '${fix}';`);
    await page.goto(path);
    await page.getByRole('region', { name: 'Routine runs', exact: true }).getByRole('link').first().press('Enter');
    const inspector = page.getByRole('complementary', { name: 'Conversation details', exact: true });
    await expect(inspector.getByText('No explanation is available for this fix.', { exact: true })).toBeVisible();
    await expect(inspector.locator('video')).toHaveCount(0);
    await page.screenshot({ path: testInfo.outputPath('native-fix-unavailable.png') });
  } finally {
    sql(`DELETE FROM projects.issue_fix_jobs WHERE run_id = '${fix}'; DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id = '${uid}';`);
  }
});

test('retained run widgets open inside the conversation inspector', async ({ page, baseURL }, testInfo) => {
  const [uid, sid, pid, conversation, run] = Array.from({ length: 5 }, () => randomUUID());
  const answer = { response: JSON.stringify({ explanation: 'Synthetic chart evidence for interface review.', widgets: [
    { type: 'timeseries_line', id: 'reused-model-id', title: 'Synthetic request volume', query: 'summarize count() by bin_auto(timestamp)' },
    { type: 'stat', id: 'reused-model-id', title: 'Synthetic request count', query: 'summarize count()', dataset: { source: [], value: 44 } },
  ] }), toolCalls: [] };
  sql(`INSERT INTO users.users (id, email) VALUES ('${uid}', '${uid}@example.test');
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title) VALUES ('${pid}', 'Retained widget review');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'admin');
       INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type, title) VALUES ('${pid}', '${conversation}', 'web', 'Review retained charts');
       INSERT INTO apis.ai_routine_runs (id, root_run_id, project_id, conversation_id, requested_by, input, policy, source, scheduled_at, started_at, finished_at, status, permission_ceiling, findings)
       VALUES ('${run}', '${run}', '${pid}', '${conversation}', '${uid}', 'Show synthetic charts', '"observe"', '{"tag":"TriggeredRun","contents":{"tag":"SlackCommand","contents":"${conversation}"}}', now(), now(), now(), 'succeeded', 'admin', '${JSON.stringify(answer)}');`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }]);
    const to = Date.now();
    await page.route('**/chart_data/stream?**', route => route.fulfill({ contentType: 'application/x-ndjson', body: JSON.stringify({ type: 'complete', data: { from: to - 60000, to, headers: ['timestamp', 'count'], dataset: [[to - 45000, 12], [to - 30000, 18], [to - 15000, 14]], rows_per_min: 44, stats: { count: 44, sum: 44, max: 18, max_group_sum: 18 } } }) + '\n' }));
    const path = `/p/${pid}/ai/${conversation}`;
    for (const width of [320, 1280]) for (const theme of ['light', 'dark']) for (const zoom of [100, 200]) {
      await page.setViewportSize({ width, height: 900 });
      await page.goto(path);
      await page.evaluate(({ theme, zoom }) => { document.body.dataset.theme = theme; document.documentElement.style.fontSize = `${zoom}%`; }, { theme, zoom });
      const composer = page.getByRole('textbox', { name: 'Message Monoscope AI', exact: true });
      await composer.fill('Keep this draft while I review the charts.');
      await page.getByRole('region', { name: 'Routine runs', exact: true }).getByRole('link').first().press('Enter');
      const inspector = page.getByRole('complementary', { name: 'Conversation details', exact: true });
      const charts = inspector.getByRole('region', { name: 'Run visualizations', exact: true });
      await expect(charts.getByRole('heading', { name: 'Visualizations', exact: true })).toBeVisible();
      await charts.scrollIntoViewIfNeeded();
      await expect(charts.getByText('Synthetic request volume', { exact: true })).toBeVisible();
      await expect(charts.getByText('Synthetic request count', { exact: true })).toBeVisible();
      await expect(charts.locator('strong[id$="Value"]')).toHaveText('44');
      await expect(charts.locator('strong[id$="Value"]')).toBeVisible();
      await expect(charts.locator('[data-chart-widget]')).toHaveAttribute('aria-busy', 'false');
      await expect(charts.locator('[data-chart-widget] canvas').first()).toBeVisible();
      await page.screenshot({ path: testInfo.outputPath(`retained-widgets-${theme}-${width}-${zoom}.png`) });
      expect(await inspector.evaluate(element => element.scrollWidth <= element.clientWidth)).toBe(true);
      expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
      expect(new URL(page.url()).pathname).toBe(path);
      await inspector.focus();
      await page.keyboard.press('Escape');
      await expect(inspector).toBeHidden();
      await expect(composer).toBeFocused();
      await expect(composer).toHaveValue('Keep this draft while I review the charts.');
    }
  } finally {
    sql(`DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id = '${uid}';`);
  }
});

test('conversation detail controls remain reachable when text size is doubled', async ({ page, baseURL }, testInfo) => {
  const uid = randomUUID(), sid = randomUUID(), pid = randomUUID(), conversation = randomUUID(), run = randomUUID();
  sql(`INSERT INTO users.users (id, email) VALUES ('${uid}', '${uid}@example.test');
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title) VALUES ('${pid}', 'Text resize review');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'admin');
       INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES ('${pid}', '${conversation}', 'web');
       INSERT INTO apis.ai_artifacts (project_id, conversation_id, key, title, content)
       VALUES ('${pid}', '${conversation}', 'checkout', 'Checkout evidence', '## Findings\n\nRetained checkout evidence.');
       INSERT INTO apis.ai_routine_runs (id, root_run_id, project_id, conversation_id, scheduled_at, started_at, finished_at, status, source, requested_by, input, instructions)
       VALUES ('${run}', '${run}', '${pid}', '${conversation}', now(), now(), now(), 'succeeded',
         '{"tag":"ConversationRun","contents":{"tag":"WebConversation"}}', '${uid}', 'Review checkout', 'Read the retained checkout evidence.');`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }]);
    for (const width of [1280, 320]) for (const theme of ['light', 'dark']) {
      await page.setViewportSize({ width, height: 844 });
      await page.goto(`/p/${pid}/ai/${conversation}`);
      await page.evaluate(value => document.body.dataset.theme = value, theme);
      const composer = page.getByRole('textbox', { name: 'Message Monoscope AI', exact: true });
      await composer.fill('Keep this draft while I inspect the enlarged text.');
      const targets = [
        ['artifact', page.getByRole('navigation', { name: 'Documents', exact: true }).getByRole('link', { name: /^Checkout evidence / })],
        ['run', page.getByRole('region', { name: 'Routine runs', exact: true }).getByRole('link').first()],
      ] as const;
      for (const [kind, trigger] of targets) {
        await page.evaluate(() => document.documentElement.style.fontSize = '100%');
        await trigger.press('Enter');
        const inspector = page.getByRole('complementary', { name: 'Conversation details', exact: true });
        await expect(inspector).toBeVisible();
        await expect(inspector).toBeFocused();
        const close = inspector.getByRole('link', { name: 'Close details', exact: true });
        const originalSize = await close.evaluate(element => parseFloat(getComputedStyle(element).fontSize));
        await page.evaluate(() => document.documentElement.style.fontSize = '200%');
        const enlargedSize = await close.evaluate(element => parseFloat(getComputedStyle(element).fontSize));
        expect(enlargedSize).toBeCloseTo(originalSize * 2, 1);
        await page.keyboard.press('Tab');
        await expect(close).toBeFocused();
        await page.screenshot({ path: testInfo.outputPath(`details-${kind}-text-200-${theme}-${width}.png`) });
        const bounds = await close.boundingBox();
        expect.soft(bounds).not.toBeNull();
        if (bounds) {
          expect.soft(bounds.x, `Close starts inside the ${width}px viewport`).toBeGreaterThanOrEqual(0);
          expect.soft(bounds.x + bounds.width, `Close ends inside the ${width}px viewport`).toBeLessThanOrEqual(width);
        }
        expect.soft(await inspector.evaluate(element => element.scrollWidth <= element.clientWidth), `Details fit with doubled text in ${theme} at ${width}px`).toBe(true);
        await page.keyboard.press('Escape');
        await expect(inspector).toBeHidden();
        await expect(composer).toBeFocused();
        await expect(composer).toHaveValue('Keep this draft while I inspect the enlarged text.');
      }
    }
  } finally {
    sql(`DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id = '${uid}';`);
  }
});

test('inspector failures explain recovery and release loading feedback without losing drafts', async ({ page, baseURL }, testInfo) => {
  const uid = randomUUID(), sid = randomUUID(), pid = randomUUID(), conversation = randomUUID();
  sql(`INSERT INTO users.users (id, email) VALUES ('${uid}', '${uid}@example.test');
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title) VALUES ('${pid}', 'Inspector recovery');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'admin');
       INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type, title) VALUES ('${pid}', '${conversation}', 'web', 'Loading review');
       INSERT INTO apis.ai_chat_messages (project_id, conversation_id, role, content) VALUES ('${pid}', '${conversation}', 'assistant', 'Ready to inspect the retained evidence.');
       INSERT INTO apis.ai_artifacts (project_id, conversation_id, key, title, content) VALUES
         ('${pid}', '${conversation}', 'first', 'First evidence', 'First retained evidence.'),
         ('${pid}', '${conversation}', 'second', 'Second evidence', 'Second retained evidence.');`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }, { name: 'isSidebarClosed', value: 'true', url: baseURL! }]);
    const path = `/p/${pid}/ai/${conversation}`;
    const details = (url: URL) => url.pathname === path && url.searchParams.get('doc') === 'second';
    for (const [width, theme] of [[1280, 'light'], [320, 'dark']] as const) for (const failure of ['server', 'network']) {
      await page.setViewportSize({ width, height: 844 });
      await page.goto(path);
      await page.evaluate(value => document.body.dataset.theme = value, theme);
      const composer = page.getByRole('textbox', { name: 'Message Monoscope AI', exact: true });
      await composer.fill('Keep this draft while details recover.');
      const navigation = page.getByRole('navigation', { name: 'Documents', exact: true });
      const source = navigation.getByRole('link', { name: /^Second evidence / });
      const inspector = page.getByRole('complementary', { name: 'Conversation details', exact: true });
      if (width === 1280) {
        await navigation.getByRole('link', { name: /^First evidence / }).press('Enter');
        await expect(inspector.getByText('First retained evidence.', { exact: true })).toBeVisible();
      }
      await page.route(details, route => failure === 'server'
        ? route.fulfill({ status: 503, contentType: 'text/html', body: 'Temporarily unavailable' })
        : route.abort('failed'));
      await source.press('Enter');
      const alert = page.getByRole('alert').filter({ hasText: 'Could not load the details. Try again.' });
      await expect.soft(alert, `${failure} failure provides recovery feedback at ${width}px`).toBeVisible({ timeout: 1000 });
      await expect.soft(source).not.toHaveAttribute('aria-busy', 'true', { timeout: 1000 });
      await expect.soft(page.locator('#htmx-progress')).toHaveCSS('opacity', '0');
      if (width === 1280) await expect(inspector.getByText('First retained evidence.', { exact: true })).toBeVisible();
      else await expect(composer).toBeVisible();
      await expect(composer).toHaveValue('Keep this draft while details recover.');
      await expect(alert).toHaveCSS('opacity', '1');
      await page.screenshot({ path: testInfo.outputPath(`inspector-${failure}-${theme}-${width}.png`) });
      if (await alert.count()) await alert.getByRole('button', { name: 'Dismiss notification', exact: true }).click();
      await page.unroute(details);
      let release!: () => void, started!: () => void;
      const pending = new Promise<void>(resolve => release = resolve), requested = new Promise<void>(resolve => started = resolve);
      await page.route(details, async route => { started(); await pending; await route.continue(); });
      try {
        await source.press('Enter');
        await requested;
        await expect.soft(source).toHaveAttribute('aria-busy', 'true');
        await expect.soft(page.locator('#htmx-progress')).toHaveCSS('opacity', '1');
        if (width === 1280) {
          await page.getByRole('button', { name: 'Actions for Loading review', exact: true }).press('Enter');
          const dismissed = page.waitForEvent('dialog').then(dialog => dialog.dismiss());
          await page.getByRole('button', { name: 'Delete chat', exact: true }).click();
          await dismissed;
          await expect.soft(page.locator('#htmx-progress'), 'Cancelling a confirmation keeps the other request loading').toHaveCSS('opacity', '1');
        }
        await expect(composer).toHaveValue('Keep this draft while details recover.');
        await page.screenshot({ path: testInfo.outputPath(`inspector-retry-${failure}-${theme}-${width}.png`) });
      } finally {
        release();
      }
      await expect(inspector.getByText('Second retained evidence.', { exact: true })).toBeVisible();
      await expect.soft(page.locator('#htmx-progress')).toHaveCSS('opacity', '0');
      await inspector.focus();
      await page.keyboard.press('Escape');
      await expect(inspector).toBeHidden();
      await expect.soft(source).not.toHaveAttribute('aria-busy', 'true');
      await expect.soft(page.locator('#htmx-progress')).toHaveCSS('opacity', '0');
      await expect(composer).toBeFocused();
      await expect(composer).toHaveValue('Keep this draft while details recover.');
      await expect(page).toHaveURL(path);
      await page.unroute(details);
    }
  } finally {
    sql(`DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id = '${uid}';`);
  }
});

test('artifact contents navigate inside the inspector when the conversation has the same heading', async ({ page, baseURL }, testInfo) => {
  const uid = randomUUID(), sid = randomUUID(), pid = randomUUID(), conversation = randomUUID();
  const content = `## Evidence\n\n[Skip to findings](#findings) [Read source](https://example.test/source#findings) [Other page](/manual#findings)\n\n${'Measured checkout evidence.\n\n'.repeat(90)}## Findings\n\nArtifact findings.`;
  sql(`INSERT INTO users.users (id, email) VALUES ('${uid}', '${uid}@example.test');
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title) VALUES ('${pid}', 'Artifact navigation');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'admin');
       INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES ('${pid}', '${conversation}', 'web');
       INSERT INTO apis.ai_chat_messages (project_id, conversation_id, role, content) VALUES ('${pid}', '${conversation}', 'assistant', '## Findings\n\nConversation findings.');
       INSERT INTO apis.ai_artifacts (project_id, conversation_id, key, title, content) VALUES ('${pid}', '${conversation}', 'checkout', 'Checkout evidence', '${content}');`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }]);
    for (const width of [1280, 320]) for (const theme of ['light', 'dark']) {
      await page.setViewportSize({ width, height: 844 });
      await page.goto(`/p/${pid}/ai/${conversation}`);
      await page.evaluate(value => document.body.dataset.theme = value, theme);
      const composer = page.getByRole('textbox', { name: 'Message Monoscope AI', exact: true });
      await composer.fill('Keep this draft while I read the evidence.');
      await page.getByRole('navigation', { name: 'Documents', exact: true }).getByRole('link', { name: /^Checkout evidence / }).focus();
      await page.keyboard.press('Enter');
      const inspector = page.getByRole('complementary', { name: 'Conversation details', exact: true });
      await expect(inspector).toBeVisible();
      await inspector.getByText('On this page', { exact: true }).focus();
      await page.keyboard.press('Enter');
      const heading = inspector.getByRole('heading', { name: 'Findings', exact: true });
      const anchor = await heading.getAttribute('id');
      await expect(inspector.getByRole('link', { name: 'Read source', exact: true })).toHaveAttribute('href', 'https://example.test/source#findings');
      await expect(inspector.getByRole('link', { name: 'Other page', exact: true })).toHaveAttribute('href', '/manual#findings');
      for (const name of ['Findings', 'Skip to findings']) {
        const link = inspector.getByRole('link', { name, exact: true });
        await expect(link).toHaveAttribute('href', `#${anchor}`);
        await link.focus();
        await page.keyboard.press('Enter');
        await expect.poll(async () => {
          const target = await heading.boundingBox(), panel = await inspector.boundingBox();
          return target !== null && panel !== null && target.y >= panel.y && target.y + target.height <= panel.y + panel.height;
        }, { message: `The artifact heading scrolls into the inspector in ${theme} at ${width}px through ${name}` }).toBe(true);
        await expect(heading).toBeFocused();
      }
      await expect(page.locator(`[id="${anchor}"]`)).toHaveCount(1);
      expect(new URL(page.url()).pathname).toBe(`/p/${pid}/ai/${conversation}`);
      await page.screenshot({ path: testInfo.outputPath(`artifact-contents-${theme}-${width}.png`) });
      await page.keyboard.press('Escape');
      await expect(inspector).toBeHidden();
      await expect(composer).toBeFocused();
      await expect(composer).toHaveValue('Keep this draft while I read the evidence.');
    }
  } finally {
    sql(`DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id = '${uid}';`);
  }
});

test('failed and cancelled runs preserve the conversation and explain missing artifacts', async ({ page, baseURL }, testInfo) => {
  const uid = randomUUID(), sid = randomUUID(), pid = randomUUID();
  sql(`INSERT INTO users.users (id, email) VALUES ('${uid}', '${uid}@example.test');
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title) VALUES ('${pid}', 'Run failure states');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'admin');`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }]);
    await page.goto(`/p/${pid}/ai/routines`);
    await page.getByRole('button', { name: 'Add Slack investigation', exact: true }).click();
    // Installing redirects to the new conversation; reading the row before then raced the insert.
    await page.waitForURL(new RegExp(`/p/${pid}/ai/[0-9a-f-]{36}$`));
    const [routine, conversation] = sql(`SELECT id, conversation_id FROM apis.ai_routines WHERE project_id = '${pid}';`).toString().trim().split('|');
    const states = [
      ['failed', 'Failed', 'The provider did not return a response. Retry this request.'],
      ['timed_out', 'Timed out', 'The investigation exceeded its time budget. Retry this request.'],
      ['cancelled', 'Cancelled', 'The investigation stopped before a result was saved.'],
    ] as const;
    const rootRun = randomUUID();
    for (const [attempt, [status, , error]] of states.entries()) {
      const run = attempt === 0 ? rootRun : randomUUID();
      sql(`INSERT INTO apis.ai_routine_runs (id, root_run_id, routine_id, project_id, conversation_id, scheduled_at, started_at, finished_at, status, source, requested_by, input, instructions, error, attempt)
           SELECT '${run}', '${rootRun}', id, project_id, conversation_id, now(), now(), now(), '${status}',
             '{"tag":"TriggeredRun","contents":{"tag":"SlackCommand","contents":"${conversation}"}}', '${uid}', 'Investigate checkout', instructions, '${error}', ${attempt + 1}
           FROM apis.ai_routines WHERE id = '${routine}';`);
    }
    for (const width of [1280, 320]) for (const theme of ['light', 'dark']) {
      await page.setViewportSize({ width, height: 844 });
      await page.goto(`/p/${pid}/ai/${conversation}`);
      await page.evaluate(value => document.body.dataset.theme = value, theme);
      const composer = page.getByRole('textbox', { name: 'Message Monoscope AI', exact: true });
      await composer.fill('Keep my follow-up after an interrupted run.');
      for (const [status, label, error] of states) {
        await page.getByRole('region', { name: 'Routine runs', exact: true }).getByRole('link').filter({ has: page.getByText(label, { exact: true }) }).focus();
        await page.keyboard.press('Enter');
        const inspector = page.getByRole('complementary', { name: 'Conversation details', exact: true });
        await expect(inspector.getByText(error, { exact: true })).toBeVisible();
        await expect(inspector.getByText('This run did not produce an artifact.', { exact: true })).toBeVisible();
        await expect(page.locator('#routine-run')).not.toHaveAttribute('hx-trigger');
        expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
        await page.screenshot({ path: testInfo.outputPath(`run-${status}-${theme}-${width}.png`) });
        await page.keyboard.press('Escape');
        await expect(inspector).toBeHidden();
        await expect(composer).toBeFocused();
        await expect(composer).toHaveValue('Keep my follow-up after an interrupted run.');
        await expect(page).toHaveURL(`/p/${pid}/ai/${conversation}`);
      }
    }
  } finally {
    sql(`DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id = '${uid}';`);
  }
});

test('routine text controls stay legible on mobile without enlarging desktop controls', async ({ page, baseURL }, testInfo) => {
  const uid = randomUUID(), sid = randomUUID(), pid = randomUUID();
  sql(`INSERT INTO users.users (id, email) VALUES ('${uid}', '${uid}@example.test');
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title) VALUES ('${pid}', 'Mobile routine controls');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'admin');`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }]);
    await page.setViewportSize({ width: 390, height: 844 });
    await page.goto(`/p/${pid}/ai`);
    await page.getByRole('button', { name: 'New chat', exact: true }).last().click();
    const modal = page.locator('#ai-composer-modal');
    await modal.getByText('Routine', { exact: true }).click();
    const placeholderMetrics = (element: HTMLElement | SVGElement) => {
      const style = getComputedStyle(element), placeholder = getComputedStyle(element, '::placeholder');
      const context = document.createElement('canvas').getContext('2d')!;
      context.font = `${style.fontWeight} ${style.fontSize} ${style.fontFamily}`;
      const fits = context.measureText(element.getAttribute('placeholder') ?? '').width <= element.clientWidth - parseFloat(style.paddingLeft) - parseFloat(style.paddingRight);
      const backgrounds = [];
      for (let node: Element | null = element; node; node = node.parentElement) backgrounds.unshift(getComputedStyle(node).backgroundColor);
      context.fillStyle = 'white'; context.fillRect(0, 0, 1, 1);
      for (const color of backgrounds) { context.fillStyle = color; context.fillRect(0, 0, 1, 1); }
      const luminance = (pixels: Uint8ClampedArray) => Array.from(pixels).slice(0, 3).map(value => { value /= 255; return value <= 0.04045 ? value / 12.92 : ((value + 0.055) / 1.055) ** 2.4; }).reduce((sum, value, index) => sum + value * [0.2126, 0.7152, 0.0722][index], 0);
      const background = luminance(context.getImageData(0, 0, 1, 1).data);
      context.fillStyle = placeholder.color; context.globalAlpha = parseFloat(placeholder.opacity); context.fillRect(0, 0, 1, 1);
      const foreground = luminance(context.getImageData(0, 0, 1, 1).data);
      return { fits, contrast: (Math.max(background, foreground) + 0.05) / (Math.min(background, foreground) + 0.05) };
    };
    const measurements: { control: string; theme: string; width: number; size: number }[] = [];
    for (const width of [320, 390]) {
      await page.setViewportSize({ width, height: 844 });
      for (const theme of ['light', 'dark']) {
        await page.evaluate(value => document.body.dataset.theme = value, theme);
        for (const name of ['Instructions', 'Run every']) {
          const control = modal.getByLabel(name, { exact: true });
          await expect(control).toBeVisible();
          measurements.push({ control: `New routine: ${name}`, theme, width, size: await control.evaluate(element => parseFloat(getComputedStyle(element).fontSize)) });
          if (name === 'Instructions') expect.soft((await control.evaluate(placeholderMetrics)).contrast, `New routine hint in ${theme} at ${width}px`).toBeGreaterThanOrEqual(4.5);
        }
      }
    }
    await page.setViewportSize({ width: 1280, height: 900 });
    for (const [name, size] of [['Instructions', 14], ['Run every', 12]] as const) expect(await modal.getByLabel(name, { exact: true }).evaluate(element => parseFloat(getComputedStyle(element).fontSize)), name).toBe(size);
    await page.setViewportSize({ width: 390, height: 844 });
    await modal.getByRole('textbox', { name: 'Instructions', exact: true }).fill('Compare checkout latency each day.');
    await modal.getByRole('button', { name: 'Start routine', exact: true }).click();
    await expect(page).toHaveURL(new RegExp(`/p/${pid}/ai/[0-9a-f-]+$`));
    await openSettings(page);
    await page.getByRole('combobox', { name: 'Execution policy', exact: true }).selectOption('review');
    await page.locator('details').filter({ has: page.locator('input[name="intervalMinutes"]') }).locator('summary').click();
    const controls = [
      ['Message Monoscope AI', page.getByRole('textbox', { name: 'Message Monoscope AI', exact: true }), 14],
      ['Instructions', page.getByRole('textbox', { name: 'Instructions', exact: true }), 14],
      ['Execution policy', page.getByRole('combobox', { name: 'Execution policy', exact: true }), 12],
      ['Required reviewer', page.getByRole('combobox', { name: 'Required reviewer', exact: true }), 12],
      ['Routine interval in minutes', page.getByRole('spinbutton', { name: 'Routine interval in minutes', exact: true }), 12],
    ] as const;
    for (const width of [320, 390]) {
      await page.setViewportSize({ width, height: 844 });
      for (const theme of ['light', 'dark']) {
        await page.evaluate(value => document.body.dataset.theme = value, theme);
        for (const [name, control] of controls) {
          await expect(control).toBeVisible();
          measurements.push({ control: name, theme, width, size: await control.evaluate(element => parseFloat(getComputedStyle(element).fontSize)) });
          if (name === 'Message Monoscope AI') {
            const metrics = await control.evaluate(placeholderMetrics);
            expect.soft(metrics.fits, `The empty composer prompt fits its single row at ${width}px`).toBe(true);
            expect.soft(metrics.contrast, `Composer hint in ${theme} at ${width}px`).toBeGreaterThanOrEqual(4.5);
          }
        }
        expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
        await page.screenshot({ path: testInfo.outputPath(`routine-controls-${theme}-${width}.png`), fullPage: true });
      }
    }
    await testInfo.attach('mobile-font-sizes', { body: JSON.stringify(measurements, null, 2), contentType: 'application/json' });
    for (const measurement of measurements) expect.soft(measurement.size, `${measurement.control} in ${measurement.theme} at ${measurement.width}px`).toBeGreaterThanOrEqual(16);
    await page.setViewportSize({ width: 1280, height: 900 });
    for (const [name, control, size] of controls) expect(await control.evaluate(element => parseFloat(getComputedStyle(element).fontSize)), name).toBe(size);
  } finally {
    sql(`DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id = '${uid}';`);
  }
});

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
    await openSettings(page);
    await page.getByRole('textbox', { name: 'Instructions', exact: true }).fill('Compare the deployment with checkout latency.');
    // Saving redirects to this same conversation; a goto racing that reload was interrupted by it.
    await Promise.all([
      page.waitForEvent('load'),
      page.waitForResponse(r => r.request().method() === 'POST' && r.url().endsWith('/routine/instructions')),
      page.getByRole('button', { name: 'Save instructions', exact: true }).click(),
    ]);
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
    await page.getByRole('link', { name: 'Close details', exact: true }).click();
    await expect(inspector).toBeHidden();
    await page.getByRole('link', { name: 'checkout', exact: true }).click();
    await expect(inspector.getByText('Later conversation edit.', { exact: true })).toBeVisible();
    await page.screenshot({ path: testInfo.outputPath('conversation-artifact-desktop.png') });
    await expect(page).toHaveURL(`${root}/${conversation}`);
    await expect(composer).toHaveValue('Keep this follow-up while I inspect the run.');
    await page.getByRole('link', { name: 'Close details', exact: true }).click();
    await expect(inspector).toBeHidden();
    await expect(composer).toHaveValue('Keep this follow-up while I inspect the run.');
    await page.getByRole('region', { name: 'Routine runs', exact: true }).getByRole('link').last().click();
    await expect(inspector.getByText('Retained evidence from a deleted source conversation.', { exact: true })).toBeVisible();
    await expect(page.getByRole('link', { name: 'All routines', exact: true })).toHaveCount(0);
    await page.getByRole('link', { name: 'Close details', exact: true }).click();
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
      await page.getByRole('link', { name: 'Close details', exact: true }).click();
      await expect(composer).toBeFocused();
      await expect(composer).toHaveValue('Mobile follow-up stays in the conversation.');
      await page.getByRole('region', { name: 'Routine runs', exact: true }).getByRole('link').first().click();
    }
    expect((await page.request.delete(`${root}/${conversation}`)).ok()).toBe(true);
    await page.goto(`${root}/runs/${run}`);
    await expect(page.getByText('Original checkout evidence.', { exact: true })).toBeVisible();
    await expect(page.getByRole('link', { name: 'Open conversation and follow up', exact: true })).toHaveCount(0);
    await expect(page.getByRole('link', { name: 'Close details', exact: true })).toHaveCount(0);
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
    await openSettings(page);
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
           '{"tag":"InteractiveRun","contents":{"tag":"WebMessage"}}', '${uid}', 'Create a dashboard', '"autonomous"');
       INSERT INTO apis.ai_agent_operations (id, project_id, run_id, requested_by, checkpoint_revision, proposal, outcome)
         VALUES ('${operation}', '${pid}', '${run}', '${uid}', 1,
           '{"call":{"id":"create","type":"function","function":{"name":"create_dashboard","arguments":{"body":{"title":"Browser action proposal"}}}},"policy":{"capability":{"tag":"ProjectWrite"},"execution":"autonomous","permission":"admin"}}',
           '{"tag":"OperationUnknown"}');`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }]);
    await page.goto(`/p/${pid}/ai/${conversation}`);
    const composer = page.getByRole('textbox', { name: 'Message Monoscope AI', exact: true });
    await composer.fill('Keep this follow-up draft.');
    await openSettings(page);
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
      expect(JSON.parse(sql(`SELECT policy FROM apis.ai_routines WHERE project_id = '${pid}' AND conversation_id = '${conversation}'`).toString().trim())).toBe(value);
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

test('selected review decisions stay in the conversation and respect current reviewer roles', async ({ page, baseURL }, testInfo) => {
  const uid = randomUUID(), sid = randomUUID(), pid = randomUUID(), conversation = randomUUID();
  const policy = JSON.stringify({ review_selected: { classes: ['project_changes'], approver: 'ReviewAdministrators' } });
  sql(`INSERT INTO users.users (id, email) VALUES ('${uid}', '${uid}@example.test');
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title) VALUES ('${pid}', 'Selected review');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${pid}', '${uid}', 'admin');
       INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type, title)
         VALUES ('${pid}', '${conversation}', 'web', 'Review this action');
       INSERT INTO apis.ai_chat_messages (project_id, conversation_id, role, content)
         VALUES ('${pid}', '${conversation}', 'assistant', 'The selected action is waiting for review.');`);
  try {
    await page.context().addCookies([{ name: 'monoscope_session', value: sid, url: baseURL! }]);
    for (const [width, theme, decision] of [[1280, 'light', 'approved'], [320, 'dark', 'rejected']] as const) {
      const run = randomUUID(), operation = randomUUID();
      sql(`INSERT INTO apis.ai_routine_runs (id, root_run_id, project_id, conversation_id, scheduled_at, started_at, status, source, requested_by, input, policy, permission_ceiling)
           VALUES ('${run}', '${run}', '${pid}', '${conversation}', now(), now(), 'running',
             '{"tag":"InteractiveRun","contents":{"tag":"WebMessage"}}', '${uid}', 'Review the dashboard action', '${policy}', 'admin');
           INSERT INTO apis.ai_agent_operations (id, project_id, run_id, requested_by, checkpoint_revision, proposal, outcome, review)
           VALUES ('${operation}', '${pid}', '${run}', '${uid}', 1,
             '{"call":{"id":"create","type":"function","function":{"name":"create_dashboard","arguments":{"body":{"title":"Reviewed dashboard"}}}},"policy":{"capability":{"tag":"ProjectWrite"},"execution":${policy},"permission":"admin"}}',
             '{"tag":"OperationUnstarted"}', '{"tag":"ReviewPending","contents":{"expiresAt":"2099-01-01T00:00:00Z","approver":"ReviewAdministrators"}}');`);
      await page.setViewportSize({ width, height: 844 });
      await page.goto(`/p/${pid}/ai/${conversation}`);
      await page.evaluate(value => document.body.dataset.theme = value, theme);
      const composer = page.getByRole('textbox', { name: 'Message Monoscope AI', exact: true });
      await composer.fill('Keep my review follow-up.');
      await page.getByRole('region', { name: 'Routine runs', exact: true }).getByRole('link').first().click();
      const inspector = page.getByRole('complementary', { name: 'Conversation details', exact: true });
      const action = inspector.locator(`#operation-${operation}`);
      await expect(action.getByText('Awaiting review', { exact: true })).toBeVisible();
      await expect(action.getByRole('button', { name: 'Approve action', exact: true })).toBeVisible();
      sql(`UPDATE projects.project_members SET permission = 'edit' WHERE project_id = '${pid}' AND user_id = '${uid}';`);
      const decisionButton = action.getByRole('button', { name: decision === 'approved' ? 'Approve action' : 'Decline action', exact: true });
      const [rejected] = await Promise.all([
        page.waitForResponse(response => response.url().endsWith(`/operations/${operation}/review`) && response.request().method() === 'POST'),
        decisionButton.click(),
      ]);
      expect(rejected.status()).toBe(403);
      const denied = page.getByRole('alert').filter({ hasText: 'Could not confirm the decision. Refresh actions to check its status.' });
      await expect.soft(denied).toBeVisible({ timeout: 1000 });
      await expect(decisionButton).toBeEnabled();
      await expect(page.locator('textarea[aria-label="Message Monoscope AI"]')).toHaveValue('Keep my review follow-up.');
      if (await denied.count()) await denied.getByRole('button', { name: 'Dismiss notification', exact: true }).click();
      expect(JSON.parse(sql(`SELECT review FROM apis.ai_agent_operations WHERE id = '${operation}'`).toString().trim()).tag).toBe('ReviewPending');
      sql(`UPDATE projects.project_members SET permission = 'admin' WHERE project_id = '${pid}' AND user_id = '${uid}';`);
      const reviewRoute = `**/operations/${operation}/review`;
      await page.route(reviewRoute, route => route.abort('failed'));
      await decisionButton.click();
      await expect(denied).toBeVisible();
      await expect(decisionButton).toBeEnabled();
      expect(JSON.parse(sql(`SELECT review FROM apis.ai_agent_operations WHERE id = '${operation}'`).toString().trim()).tag).toBe('ReviewPending');
      await expect(denied).toHaveCSS('opacity', '1');
      await page.screenshot({ path: testInfo.outputPath(`selected-review-error-${width}.png`) });
      await denied.getByRole('button', { name: 'Dismiss notification', exact: true }).click();
      await page.unroute(reviewRoute);
      await Promise.all([
        page.waitForResponse(response => response.url().endsWith(`/operations/${operation}/review`) && response.request().method() === 'POST'),
        action.getByRole('button', { name: decision === 'approved' ? 'Approve action' : 'Decline action', exact: true }).click(),
      ]);
      await expect(action.getByText(decision === 'approved' ? 'Approved' : 'Declined', { exact: true })).toBeVisible();
      await expect(action.getByRole('button')).toHaveCount(0);
      const review = JSON.parse(sql(`SELECT review FROM apis.ai_agent_operations WHERE id = '${operation}'`).toString().trim());
      expect(review.tag).toBe('ReviewResolved');
      expect(review.contents[1].decision).toBe(decision === 'approved' ? 'ReviewApproved' : 'ReviewRejected');
      expect(review.contents[1].decidedBy).toBe(uid);
      await expect(page.locator('#toastsParent .toast-animate').filter({ hasText: 'Decision saved.' })).toHaveCSS('opacity', '1');
      await page.screenshot({ path: testInfo.outputPath(`selected-review-${decision}-${width}.png`) });
      await inspector.focus();
      await page.keyboard.press('Escape');
      await expect(composer).toBeFocused();
      await expect(composer).toHaveValue('Keep my review follow-up.');
      await expect(page).toHaveURL(`/p/${pid}/ai/${conversation}`);
      sql(`UPDATE apis.ai_routine_runs SET status = 'cancelled', finished_at = now() WHERE id = '${run}';`);
    }
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
