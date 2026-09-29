import { test, expect, type Page } from "@playwright/test";
import { DEMO_PROJECT, sql } from "./helpers";

const clearSessions = `DELETE FROM projects.replay_sessions WHERE project_id = '${DEMO_PROJECT}' AND session_id::text LIKE 'ec2e0000-0000-4000-8000-%'`;

const refreshSessionList = (page: Page) => page.evaluate(() => new Promise<void>(resolve => {
  const settled = (event: Event) => {
    if ((event.target as HTMLElement).id !== "rum-sessions-list") return;
    document.removeEventListener("htmx:after:settle", settled);
    resolve();
  };
  document.addEventListener("htmx:after:settle", settled);
  window.dispatchEvent(new Event("update-query"));
}));

// scripts/e2e.sh supplies the isolated server URL; local browser probes use existing data.
test.beforeAll(() => {
  if (!process.env.E2E_BASE_URL) return;
  sql(`${clearSessions};
    INSERT INTO projects.replay_sessions (session_id, project_id, created_at, last_event_at, event_file_count, user_name)
    SELECT ('ec2e0000-0000-4000-8000-' || lpad(i::text, 12, '0'))::uuid, '${DEMO_PROJECT}',
      now() - interval '30 seconds' - i * interval '1 second', now() - i * interval '1 second',
      1, 'Demo shopper ' || lpad(i::text, 8, '0') FROM generate_series(1, 200) AS i`);
});

test.afterAll(() => {
  if (process.env.E2E_BASE_URL) sql(clearSessions);
});

test("desktop session list keeps session identities readable", async ({ page }) => {
  await page.setViewportSize({ width: 1440, height: 900 });
  await page.goto(`/p/${DEMO_PROJECT}/rum?tab=sessions`);
  const identity = page.locator(".rum-session-link").first();
  await expect(identity).toBeVisible();
  expect(await identity.evaluate(element => {
    const text = document.createRange();
    text.selectNodeContents(element);
    return text.getBoundingClientRect().width <= element.getBoundingClientRect().width;
  })).toBe(true);
});

test("live session refresh preserves keyboard focus and list scroll", async ({ page }) => {
  await page.setViewportSize({ width: 1440, height: 900 });
  await page.goto(`/p/${DEMO_PROJECT}/rum?tab=sessions`);
  const list = page.locator("#rum-sessions-list");
  const session = list.locator(".rum-session-link").nth(50);
  await session.scrollIntoViewIfNeeded();
  await session.focus();
  const sessionId = await session.getAttribute("data-session-id");
  const scrollTop = await list.evaluate(element => element.scrollTop);
  expect(scrollTop).toBeGreaterThan(0);
  await refreshSessionList(page);
  await expect(list.locator(`.rum-session-link[data-session-id="${sessionId}"]`)).toBeFocused();
  expect(Math.abs(await list.evaluate(element => element.scrollTop) - scrollTop)).toBeLessThanOrEqual(1);
});

test("mobile session list keeps the replay workspace within reach", async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.goto(`/p/${DEMO_PROJECT}/rum?tab=sessions`);
  const list = page.locator("#rum-sessions-list");
  await expect(list.locator(".rum-session-link").first()).toBeVisible();
  const workspace = page.locator("#rum-replay-workspace");
  await expect.poll(() => workspace.evaluate(element => element.getBoundingClientRect().top)).toBeLessThan(844);
  expect(await list.evaluate(element => element.scrollHeight > element.clientHeight)).toBe(true);
  await list.locator(".rum-session-link").first().click();
  await expect(workspace.getByRole("link", { name: "Inspect telemetry" })).toBeVisible();
});

test("session search survives filter changes and keeps the replay workspace", async ({ page }) => {
  await page.goto(`/p/${DEMO_PROJECT}/rum?tab=sessions`);
  const search = page.getByRole("searchbox", { name: "Search sessions" });
  const filters = page.getByRole("navigation", { name: "Filter sessions" });
  await expect(filters).toBeVisible();
  await page.keyboard.press("/");
  await expect(search).toBeFocused();
  const searchResponse = page.waitForResponse(r => {
    const url = new URL(r.url());
    return url.pathname.endsWith("/rum") && url.searchParams.get("q") === "e2e-missing-session";
  });
  await search.fill("e2e-missing-session");
  expect((await searchResponse).status()).toBe(200);
  await expect(page.getByText("No sessions match this filter", { exact: true })).toBeVisible();
  const workspace = page.locator("#rum-replay-workspace");
  await expect(workspace).toBeVisible();
  await workspace.evaluate(element => element.setAttribute("data-e2e-preserved", "true"));
  for (const [label, filter] of [["With errors", "errors"], ["With replay", "replays"], ["All sessions", ""]]) {
    const response = page.waitForResponse(r => {
      const url = new URL(r.url());
      return url.pathname.endsWith("/rum") && url.searchParams.get("filter") === filter && url.searchParams.get("q") === "e2e-missing-session";
    });
    await filters.getByRole("link", { name: label, exact: true }).click();
    expect((await response).status()).toBe(200);
    await expect(filters.getByRole("link", { name: label, exact: true })).toHaveAttribute("aria-current", "page");
    await expect(search).toHaveValue("e2e-missing-session");
    await expect(page.locator("#rum-session-filter")).toHaveValue(filter);
    await expect(workspace).toHaveAttribute("data-e2e-preserved", "true");
    const refresh = page.waitForResponse(r => {
      const url = new URL(r.url());
      return url.pathname.endsWith("/rum") && url.searchParams.get("panel") === "sessions"
        && r.request().headers()["hx-source"] === "div#rum-panel-sessions";
    });
    await refreshSessionList(page);
    const refreshedUrl = new URL((await refresh).url());
    expect(refreshedUrl.searchParams.get("q")).toBe("e2e-missing-session");
    expect(refreshedUrl.searchParams.get("filter") ?? "").toBe(filter);
    await expect(filters.getByRole("link", { name: label, exact: true })).toHaveAttribute("aria-current", "page");
    await expect(page.getByText("No sessions match this filter", { exact: true })).toBeVisible();
    await expect(workspace).toHaveAttribute("data-e2e-preserved", "true");
  }
});

test("page links preserve regex query characters through Explorer preload", async ({ page }) => {
  test.skip(!process.env.E2E_BASE_URL, "Requires the disposable e2e database");
  const cleanup = `DELETE FROM otel_logs_and_spans WHERE project_id='${DEMO_PROJECT}' AND resource___deployment___environment___name='e2e-page-links'`;
  sql(`${cleanup};
    INSERT INTO otel_logs_and_spans (project_id,summary,name,kind,timestamp,start_time,end_time,duration,attributes,resource,
      attributes___url___full,resource___user_agent___original,resource___deployment___environment___name,context___trace_id,context___span_id)
    SELECT '${DEMO_PROJECT}',ARRAY['documentLoad'],'documentLoad','span',now(),now(),now()+interval '10 milliseconds',10000000,
      jsonb_build_object('url',jsonb_build_object('full',url)),
      jsonb_build_object('user_agent',jsonb_build_object('original','Mozilla/5.0'),'deployment',jsonb_build_object('environment',jsonb_build_object('name','e2e-page-links'))),
      url,'Mozilla/5.0','e2e-page-links',repeat('f',31)||i::text,repeat('f',15)||i::text
    FROM unnest(ARRAY['https://shop.example/cart&ready','https://shop.example/cart&ready?q=x','https://shop.example/cart&ready#section',
      'https://shop.example/cart&ready-later','https://shop.example/Cart&ready','https://shop.example/other/cart&ready','https://shop.example/search?q=/cart&ready']) WITH ORDINALITY AS paths(url,i)`);
  try {
    await page.goto(`/p/${DEMO_PROJECT}/rum?tab=performance&since=1H&environment=e2e-page-links`);
    const link = page.locator("#rum-panel-pages").getByRole("link", { name: "/cart&ready", exact: true });
    await expect(link).toBeVisible();
    const query = new URL((await link.getAttribute("href"))!, page.url()).searchParams.get("query");
    const preload = page.waitForResponse(r => new URL(r.url()).pathname.endsWith("/log_explorer/data"));
    await link.click();
    const response = await preload;
    expect(new URL(response.url()).searchParams.get("query")).toBe(query);
    const events = await response.json();
    expect(events.error).toBeUndefined();
    expect(events.queryResultCount).toBe(3);
    await expect(page.locator("#resultTable [data-row-id]")).toHaveCount(3);
  } finally {
    sql(cleanup);
  }
});

test("settled session search preserves scope and survives reload and a shared URL", async ({ page, context }) => {
  test.skip(!process.env.E2E_BASE_URL, "Requires the disposable e2e database");
  const cleanup = `DELETE FROM otel_logs_and_spans WHERE project_id='${DEMO_PROJECT}' AND resource___deployment___environment___name='e2e-session-search'`;
  sql(`${cleanup};
    INSERT INTO otel_logs_and_spans (project_id,summary,name,kind,timestamp,start_time,end_time,duration,attributes,resource,
      attributes___session___id,resource___telemetry___sdk___language,resource___service___name,resource___deployment___environment___name)
    SELECT '${DEMO_PROJECT}',ARRAY['documentLoad'],'documentLoad','span',now(),now(),now()+interval '10 milliseconds',10000000,
      jsonb_build_object('session',jsonb_build_object('id',sid),'url',jsonb_build_object('path','/scoped-session-proof')),
      jsonb_build_object('telemetry',jsonb_build_object('sdk',jsonb_build_object('language','webjs')),'service',jsonb_build_object('name',svc),
        'deployment',jsonb_build_object('environment',jsonb_build_object('name','e2e-session-search'))),
      sid,'webjs',svc,'e2e-session-search'
    FROM (VALUES ('ec2e0000-0000-4000-8000-000000000001','e2e-session-search'),
      ('ec2e0000-0000-4000-8000-000000000002','other-service')) AS scopes(sid,svc)`);
  try {
    const initialPanel = page.waitForResponse(r => {
      const url = new URL(r.url());
      return url.pathname.endsWith("/rum") && url.searchParams.get("panel") === "sessions"
        && r.request().headers()["hx-source"] === "div#rum-panel-sessions";
    });
    await page.goto(`/p/${DEMO_PROJECT}/rum?tab=sessions&since=1H&environment=e2e-session-search&service_scope=e2e-session-search`);
    const initialUrl = new URL((await initialPanel).url());
    expect(initialUrl.searchParams.get("service_scope")).toBe("e2e-session-search");
    await expect(page.locator(".rum-session-link")).toHaveCount(1);
    await page.locator(".rum-session-link").click();
    const workspace = page.locator("#rum-replay-workspace");
    await expect(workspace.locator("session-replay")).toBeVisible();
    await page.waitForURL(url => url.searchParams.has("session"));
    const session = new URL(page.url()).searchParams.get("session");
    await page.getByRole("link", { name: "With replay", exact: true }).click();
    await expect(page.getByRole("link", { name: "With replay", exact: true })).toHaveAttribute("aria-current", "page");
    await page.waitForURL(url => url.searchParams.get("filter") === "replays");
    const query = "e2e missing session + % &";
    const response = page.waitForResponse(r => {
      const url = new URL(r.url());
      return url.pathname.endsWith("/rum") && url.searchParams.get("q") === query
        && r.request().headers()["hx-source"] === "form#rum-session-search-form";
    });
    await page.getByRole("searchbox", { name: "Search sessions" }).fill(query);
    await response;
    await expect(page.getByText("No sessions match this filter", { exact: true })).toBeVisible();
    await page.waitForURL(url => url.searchParams.get("q") === query);
    await page.getByRole("link", { name: "All sessions", exact: true }).click();
    await expect(page.getByRole("link", { name: "All sessions", exact: true })).toHaveAttribute("aria-current", "page");
    await page.waitForURL(url => !url.searchParams.has("filter"));
    const url = new URL(page.url());
    for (const [key, value] of Object.entries({q:query, session, since:"1H", environment:"e2e-session-search", service_scope:"e2e-session-search"})) {
      expect(url.searchParams.get(key)).toBe(value);
    }
    expect(url.searchParams.get("filter") ?? "").toBe("");
    for (const parameter of ["panel", "deferred", "refresh"]) expect(url.searchParams.has(parameter)).toBe(false);
    await page.reload();
    await expect(page.getByRole("searchbox", { name: "Search sessions" })).toHaveValue(query);
    await expect(page.getByText("No sessions match this filter", { exact: true })).toBeVisible();
    await expect(workspace.locator("session-replay")).toBeVisible();
    const shared = await context.newPage();
    await shared.goto(url.toString());
    await expect(shared.getByRole("searchbox", { name: "Search sessions" })).toHaveValue(query);
    await expect(shared.getByText("No sessions match this filter", { exact: true })).toBeVisible();
    await expect(shared.locator("#rum-replay-workspace session-replay")).toBeVisible();
    await page.goBack();
    await expect(page.getByRole("searchbox", { name: "Search sessions" })).toHaveValue(query);
    await expect(page.getByRole("link", { name: "With replay", exact: true })).toHaveAttribute("aria-current", "page");
    await page.goBack();
    await expect(page.getByRole("searchbox", { name: "Search sessions" })).toHaveValue("");
    await expect(page.getByRole("link", { name: "All sessions", exact: true })).toHaveAttribute("aria-current", "page");
    await expect(workspace.locator("session-replay")).toBeVisible();
  } finally {
    sql(cleanup);
  }
});
