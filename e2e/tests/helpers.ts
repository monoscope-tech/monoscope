import { execFileSync } from "node:child_process";
import { randomUUID } from "node:crypto";
import { Page, expect } from "@playwright/test";

export const REAL_PROVIDERS = process.env.E2E_REAL_PROVIDERS === "true";
export const DEMO_PROJECT = "00000000-0000-0000-0000-000000000000";

export async function assertStripeCheckout(
  page: Page,
  planButtonSelector: string,
) {
  if (REAL_PROVIDERS) {
    await page.locator(planButtonSelector).click();
    await page.waitForURL(/checkout\.stripe\.com/, { timeout: 15000 });
    expect(page.url()).toContain("checkout.stripe.com");
  } else {
    const [response] = await Promise.all([
      page.waitForResponse((r) => r.url().includes("/stripe_checkout")),
      page.locator(planButtonSelector).click(),
    ]);
    expect(response.url()).toContain("/stripe_checkout");
  }
}

export function sql(query: string) {
  return execFileSync('psql', ['-At', '-h', process.env.E2E_PGHOST ?? process.env.DB_HOST ?? 'localhost',
    '-p', process.env.E2E_PGPORT ?? process.env.DB_PORT ?? '5432', '-U', 'postgres',
    '-d', process.env.E2E_DB ?? 'monoscope_e2e', '-v', 'ON_ERROR_STOP=1', '-c', query],
  { env: { ...process.env, PGPASSWORD: process.env.E2E_PGPASSWORD ?? 'postgres' }, stdio: 'pipe' });
}

/** SQL against the TimeFusion the server's charts read; a no-op when there is none. */
export function tfSql(query: string) {
  const url = process.env.TIMEFUSION_PG_TEST_URL;
  return url ? execFileSync('psql', [url, '-At', '-v', 'ON_ERROR_STOP=1', '-c', query], { stdio: 'pipe' }).toString() : '';
}

// What TimeFusion requires that a Postgres-seeded row may leave out.
const tfDefaults: Record<string, (row: Record<string, unknown>) => unknown> = {
  date: (r) => String(r.timestamp).slice(0, 10), summary: () => [], ingested_at: () => new Date().toISOString(),
  metric_unit: () => '', flags: () => 0, dropped_attributes_count: () => 0, message_size_bytes: () => 0,
};
const tfRequired = { otel_logs_and_spans: ['date', 'summary'], otel_metrics: ['date', 'ingested_at', 'metric_unit', 'flags', 'dropped_attributes_count', 'message_size_bytes'] };

const tfLiteral = (v: unknown): string =>
  v === null || v === undefined ? 'NULL'
    : typeof v === 'number' || typeof v === 'boolean' ? String(v)
    : Array.isArray(v) ? `ARRAY[${v.map(tfLiteral).join(', ')}]`
    : `'${(typeof v === 'object' ? JSON.stringify(v) : String(v)).replaceAll("'", "''")}'`;

/** Copies rows a spec seeded into Postgres to the TimeFusion charts read, column for column. */
export function mirrorToTf(table: keyof typeof tfRequired, where: string) {
  if (!process.env.TIMEFUSION_PG_TEST_URL) return;
  const known = new Set(tfSql(`SELECT column_name FROM information_schema.columns WHERE table_name = '${table}'`).split('\n').filter(Boolean));
  const rows: Record<string, unknown>[] = JSON.parse(sql(`SELECT coalesce(json_agg(t), '[]') FROM (SELECT * FROM ${table} WHERE ${where}) t`).toString());
  for (const row of rows) {
    for (const c of tfRequired[table]) row[c] ??= tfDefaults[c](row);
    const cols = Object.keys(row).filter((c) => known.has(c) && row[c] !== null);
    tfSql(`INSERT INTO ${table} (${cols.join(', ')}) VALUES (${cols.map((c) => tfLiteral(row[c])).join(', ')})`);
  }
}

/**
 * Signs the page in as a fresh admin of the demo project. The anonymous demo guest edits the
 * demo but may not manage teams, members or notification targets.
 */
export async function asDemoAdmin(page: Page, baseURL: string): Promise<void> {
  const [uid, sid] = [randomUUID(), randomUUID()];
  sql(`INSERT INTO users.users (id, email) VALUES ('${uid}', '${uid}@example.test');
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.project_members (project_id, user_id, permission) VALUES ('${DEMO_PROJECT}', '${uid}', 'admin');`);
  await page.context().addCookies([{ name: "monoscope_session", value: sid, url: baseURL }]);
}

export type Dash = { id: string; title: string };

/**
 * A blank dashboard of this run's own, deleted at the end of the test.
 *
 * Tests that count widgets cannot share one: a run that fails midway leaves widgets it
 * never cleaned up behind, after which "one more than before" measures the last failure
 * rather than anything the test did. Titles carry a timestamp so a leftover from a failed
 * run never collides with a live one.
 */
export async function makeDashboard(page: Page, title: string, template = "Blank dashboard"): Promise<Dash> {
  await page.goto(`/p/${DEMO_PROJECT}/dashboards`);
  await page.locator('#newDashboardMdl-trigger').first().click();
  await page.locator("#dashListItemParent").getByText(template, { exact: true }).click();
  await page.getByRole("textbox", { name: "Dashboard name *", exact: true }).fill(title);
  await page.getByRole("button", { name: "Create" }).first().click();
  await page.waitForURL(/\/dashboards\/[0-9a-f-]{36}/i, { timeout: 60000 });
  return { id: page.url().match(/\/dashboards\/([0-9a-f-]{36})/i)![1], title };
}

/** Delete a dashboard through the UI, which is also the delete path's only coverage. */
export async function deleteDashboard(page: Page, dash: Dash) {
  await page.goto(`/p/${DEMO_PROJECT}/dashboards/${dash.id}`);
  page.once("dialog", (d) => d.accept());
  await page.locator('[aria-label="Open context menu"]').first().click();
  // Wait for the DELETE itself: the handler redirects to the list on its own, and
  // navigating there in parallel aborts that redirect rather than following it.
  await Promise.all([
    page.waitForResponse(
      (r) => r.request().method() === "DELETE" && r.url().includes(dash.id) && r.status() < 400,
      { timeout: 20000 },
    ),
    page.getByText("Delete dashboard").click(),
  ]);
  await expect
    .poll(
      async () => {
        // The handler's own post-DELETE redirect can still be in flight, and a goto that
        // collides with it aborts (net::ERR_ABORTED) — which threw out of the poll and
        // failed the whole test. A failed navigation just polls again.
        const resp = await page.goto(`/p/${DEMO_PROJECT}/dashboards`).catch(() => null);
        if (!resp) return -1;
        return page.getByText(dash.title).count();
      },
      { timeout: 20000 },
    )
    .toBe(0);
}
