import { test, expect, type Page } from "@playwright/test";
import { DEMO_PROJECT, sql } from "./helpers";

const fixtureUser = `e2e-session-fixture-${Date.now()}`;
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
    INSERT INTO projects.replay_sessions (session_id, project_id, created_at, last_event_at, event_file_count, user_name, user_id)
    SELECT ('ec2e0000-0000-4000-8000-' || lpad(i::text, 12, '0'))::uuid, '${DEMO_PROJECT}',
      now() - interval '30 seconds' - i * interval '1 second', now() - i * interval '1 second',
      1, 'Demo shopper ' || lpad(i::text, 8, '0'), '${fixtureUser}' FROM generate_series(1, 200) AS i`);
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
  await page.goto(`/p/${DEMO_PROJECT}/rum?tab=sessions${process.env.E2E_BASE_URL ? `&q=${fixtureUser}` : ""}`);
  const list = page.locator("#rum-sessions-list");
  const session = list.locator(".rum-session-link").nth(50);
  await expect(session).toBeAttached();
  await expect(list.locator('[hx-get*="refresh=1"]')).toHaveCount(0);
  if (process.env.E2E_BASE_URL) await expect(list.getByText("200 active now", { exact: true })).toBeVisible();
  await page.evaluate(() => document.fonts.ready);
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

test("selecting a session does not wait for a full list response", async ({ page }) => {
  test.skip(!process.env.E2E_BASE_URL, "Requires the disposable e2e database");
  await page.goto(`/p/${DEMO_PROJECT}/rum?tab=sessions&since=1H&filter=replays&q=${fixtureUser}`);
  const list = page.locator("#rum-sessions-list");
  const session = list.locator(".rum-session-link").first();
  await expect(session).toBeVisible();
  await page.locator("[data-time-transport]").getByRole("button", { name: /^Pause live (data|updates)$/ }).click();
  await expect(page.locator("[data-time-transport]")).toHaveAttribute("data-interval", "0");
  const sid = await session.getAttribute("data-session-id");
  await list.evaluate(element => element.setAttribute("data-e2e-preserved", "true"));
  let fullListRequests = 0;
  let release!: () => void;
  const held = new Promise<void>(resolve => { release = resolve; });
  await page.route("**/rum?**", async route => {
    const url = new URL(route.request().url());
    if (url.searchParams.get("session") !== sid || url.searchParams.get("panel") !== "sessions") return route.continue();
    fullListRequests++;
    const response = await route.fetch();
    await held;
    await route.fulfill({ response });
  });
  try {
    await session.click();
    await expect(page.locator("#rum-replay-workspace session-replay")).toBeVisible();
    expect(fullListRequests).toBe(0);
    await expect(list).toHaveAttribute("data-e2e-preserved", "true");
    await expect(session).toHaveAttribute("aria-current", "true");
    const url = new URL(page.url());
    for (const [key, value] of Object.entries({ session: sid, q: fixtureUser, filter: "replays", since: "1H" })) expect(url.searchParams.get(key)).toBe(value);
    for (const key of ["panel", "deferred", "refresh"]) expect(url.searchParams.has(key)).toBe(false);
  } finally {
    release();
    await page.unrouteAll({ behavior: "wait" });
  }
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

test("session time changes survive live refresh, search, filters and reload", async ({ page, context }) => {
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
    const changedWindow = page.waitForResponse(r => r.request().headers()["hx-source"] === "div#rum-panel-sessions");
    await page.locator("[data-live-range]").click();
    await page.locator('#n-timepicker-popover button[data-value="24H"]').click();
    expect(new URL((await changedWindow).url()).searchParams.get("since")).toBe("24H");
    await expect(page.locator("#n-currentRange")).toHaveText("Last 24 hours");
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
    for (const [key, value] of Object.entries({q:query, session, since:"24H", environment:"e2e-session-search", service_scope:"e2e-session-search"})) {
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
    const historicalResponse = page.waitForResponse(r => r.request().headers()["hx-source"] === "div#rum-panel-sessions");
    await page.getByRole("button", { name: "Previous time window", exact: true }).first().click();
    const historical = new URL(page.url());
    const historicalRequest = new URL((await historicalResponse).url());
    for (const key of ["from", "to"]) {
      expect(historical.searchParams.get(key)).toBeTruthy();
      expect(historicalRequest.searchParams.get(key)).toBe(historical.searchParams.get(key));
    }
    expect(historicalRequest.searchParams.get("since") ?? "").toBe("");
    await page.getByRole("searchbox", { name: "Search sessions" }).fill("historical missing session");
    await page.waitForURL(url => url.searchParams.get("q") === "historical missing session");
    for (const key of ["from", "to", "session", "environment", "service_scope"]) {
      expect(new URL(page.url()).searchParams.get(key)).toBe(historical.searchParams.get(key));
    }
    expect(new URL(page.url()).searchParams.get("since") ?? "").toBe("");
    await page.reload();
    await expect(page.getByRole("searchbox", { name: "Search sessions" })).toHaveValue("historical missing session");
    for (const key of ["from", "to"]) expect(new URL(page.url()).searchParams.get(key)).toBe(historical.searchParams.get(key));
  } finally {
    sql(cleanup);
  }
});

test("Overview counts a session once when its events span chart intervals", async ({ page }) => {
  test.skip(!process.env.E2E_BASE_URL, "Requires the disposable e2e database");
  const scope = `e2e-overview-session-${Date.now()}`;
  const cleanup = `DELETE FROM otel_logs_and_spans WHERE project_id='${DEMO_PROJECT}' AND resource___deployment___environment___name='${scope}'`;
  sql(`${cleanup};
    INSERT INTO otel_logs_and_spans (project_id,summary,name,kind,timestamp,start_time,end_time,duration,attributes,resource,
      attributes___session___id,resource___telemetry___sdk___language,resource___service___name,resource___deployment___environment___name)
    SELECT '${DEMO_PROJECT}',ARRAY['documentLoad'],'documentLoad','span',at,at,at+interval '10 milliseconds',10000000,
      jsonb_build_object('session',jsonb_build_object('id','overview-shared-session'),'url',jsonb_build_object('path','/overview-count')),
      jsonb_build_object('telemetry',jsonb_build_object('sdk',jsonb_build_object('language','webjs')),'service',jsonb_build_object('name','${scope}'),
        'deployment',jsonb_build_object('environment',jsonb_build_object('name','${scope}'))),
      'overview-shared-session','webjs','${scope}','${scope}'
    FROM (VALUES (now()-interval '40 minutes'),(now()-interval '10 minutes')) AS events(at)`);
  try {
    await page.goto(`/p/${DEMO_PROJECT}/rum?since=1H&environment=${scope}&service_scope=${scope}`);
    await expect(page.locator("#rum-stat-pageviewsValue")).toHaveText("2 views");
    await expect(page.locator("#rum-stat-p75Value")).toHaveText("10.0ms");
    await expect(page.locator("#rum-stat-sessionsValue")).toHaveText("1 sessions");
  } finally {
    sql(cleanup);
  }
});

test("Overview does not prewarm hidden Performance panels while visible panels load", async ({ page }) => {
  test.skip(!process.env.E2E_BASE_URL, "Requires the disposable e2e database");
  const scope = `e2e-overview-prewarm-${Date.now()}`;
  const cleanup = `DELETE FROM otel_logs_and_spans WHERE project_id='${DEMO_PROJECT}' AND resource___service___name='${scope}'; DELETE FROM otel_metrics WHERE project_id='${DEMO_PROJECT}' AND resource___service___name='${scope}'`;
  sql(`INSERT INTO otel_logs_and_spans (project_id,summary,name,kind,timestamp,start_time,end_time,duration,attributes,resource,
    attributes___url___path,resource___telemetry___sdk___language,resource___service___name)
    VALUES('${DEMO_PROJECT}',ARRAY['documentLoad'],'documentLoad','span',now()-interval '10 seconds',now()-interval '10 seconds',now(),10000000,
      '{"url":{"path":"/overview-visible"}}',jsonb_build_object('service',jsonb_build_object('name','${scope}')),'/overview-visible','webjs','${scope}');
    INSERT INTO otel_metrics (project_id,id,series_id,timestamp,metric_name,metric_type,metric_unit,value,resource,resource___service___name)
    VALUES('${DEMO_PROJECT}',gen_random_uuid(),'${scope}',now()-interval '10 seconds','browser.web_vital.fcp','GAUGE','ms',22.5,
      jsonb_build_object('service',jsonb_build_object('name','${scope}')),'${scope}')`);
  let release = () => {};
  const held = new Promise<void>(resolve => { release = resolve; });
  const hiddenRequests: string[] = [];
  page.on("request", request => {
    const params = new URL(request.url()).searchParams;
    if (params.get("tab") === "performance" && params.has("panel")) hiddenRequests.push(request.url());
  });
  await page.route("**/rum?**", async route => {
    if (new URL(route.request().url()).searchParams.get("panel") !== "pages") return route.continue();
    const response = await route.fetch();
    await held;
    await route.fulfill({ response });
  });
  try {
    await page.goto(`/p/${DEMO_PROJECT}/rum?tab=overview&since=24H&service_scope=${scope}`);
    await expect(page.locator("#rum-stat-pageviewsValue")).toHaveText("1 views");
    await expect(page.locator("#rum-panel-vitals")).toContainText("22.5 ms");
    // Observe the existing eight-second prewarm boundary while a real visible response is held.
    await page.evaluate(() => new Promise<void>(resolve => setTimeout(resolve, 8500)));
    await expect(page.locator("#rum-panel-pages")).toHaveAttribute("data-deferred-shell", "");
    expect(hiddenRequests).toEqual([]);
    release();
    await expect(page.locator("#rum-panel-pages")).toContainText("/overview-visible");
    await page.unrouteAll({ behavior: "wait" });
    await page.getByRole("link", { name: "Performance", exact: true }).click();
    await expect(page.locator("#rum-panel-vitals")).toContainText("22.5 ms");
  } finally {
    release();
    try { await page.unrouteAll({ behavior: "wait" }); } finally { sql(cleanup); }
  }
});

test("Overview keeps its loaded values during refresh after the first browser event arrives", async ({ page }) => {
  test.skip(!process.env.E2E_BASE_URL, "Requires the disposable e2e database");
  const scope = `e2e-overview-live-${Date.now()}`;
  const cleanup = `DELETE FROM otel_logs_and_spans WHERE project_id='${DEMO_PROJECT}' AND resource___deployment___environment___name='${scope}'`;
  let release = () => {};
  const pending = new Promise<void>(resolve => { release = resolve; });
  const refreshPulse = () => page.evaluate(() => new Promise<void>(resolve => {
    const panel = document.getElementById("rum-panel-pulse");
    const reloadsPanel = panel?.matches("[hx-get], [data-hx-get]");
    const settled = (event: Event) => {
      if ((event.target as HTMLElement).id !== "rum-panel-pulse") return;
      document.removeEventListener("htmx:after:settle", settled);
      resolve();
    };
    if (reloadsPanel) document.addEventListener("htmx:after:settle", settled);
    window.dispatchEvent(new Event("update-query"));
    if (!reloadsPanel) resolve();
  }));
  try {
    await page.goto(`/p/${DEMO_PROJECT}/rum?since=1H&environment=${scope}&service_scope=${scope}`);
    await expect(page.getByText(`No browser telemetry for ${scope} in this range`, { exact: true })).toBeVisible();
    sql(`INSERT INTO otel_logs_and_spans (project_id,summary,name,kind,timestamp,start_time,end_time,duration,attributes,resource,
      attributes___session___id,resource___telemetry___sdk___language,resource___service___name,resource___deployment___environment___name)
      VALUES ('${DEMO_PROJECT}',ARRAY['documentLoad'],'documentLoad','span',now()-interval '10 seconds',now()-interval '10 seconds',now()-interval '10 seconds'+interval '10 milliseconds',10000000,
        '{"session":{"id":"overview-first-event"},"url":{"path":"/first-event"}}',
        '{"telemetry":{"sdk":{"language":"webjs"}},"service":{"name":"${scope}"},"deployment":{"environment":{"name":"${scope}"}}}',
        'overview-first-event','webjs','${scope}','${scope}')`);
    await refreshPulse();
    const value = page.locator("#rum-stat-p75Value");
    await expect(value).toHaveText("10.0ms");
    await page.route("**/widget?**", async route => {
      if (route.request().headers()["hx-source"] !== "div#rum-stat-p75_stat") return route.continue();
      const response = await route.fetch();
      await pending;
      await route.fulfill({ response });
    });
    const requested = page.waitForRequest(request => request.headers()["hx-source"] === "div#rum-stat-p75_stat");
    const refreshed = refreshPulse();
    await requested;
    await refreshed;
    await expect(value).toHaveText("10.0ms");
    const recovered = page.waitForResponse(response => response.request().headers()["hx-source"] === "div#rum-stat-p75_stat");
    release();
    expect((await recovered).status()).toBe(200);
    await page.unrouteAll({ behavior: "wait" });
    await expect(value).toHaveText("10.0ms");
  } finally {
    release();
    await page.unrouteAll({ behavior: "wait" });
    sql(cleanup);
  }
});

test("Performance and Overview panels use the current picker window", async ({ page }) => {
  test.skip(!process.env.E2E_BASE_URL, "Requires the disposable e2e database");
  const scope = `e2e-panel-window-${Date.now()}`;
  const cleanup = `DELETE FROM otel_logs_and_spans WHERE project_id='${DEMO_PROJECT}' AND resource___service___name='${scope}'`;
  sql(`INSERT INTO otel_logs_and_spans (project_id,summary,name,kind,timestamp,start_time,end_time,duration,attributes,resource,
    attributes___url___path,resource___telemetry___sdk___language,resource___service___name,resource___deployment___environment___name)
    SELECT '${DEMO_PROJECT}',ARRAY['documentLoad'],'documentLoad','span',at,at,at+interval '10 milliseconds',10000000,
      '{"url":{"path":"/picker-window"}}',
      jsonb_build_object('telemetry',jsonb_build_object('sdk',jsonb_build_object('language','webjs')),
        'service',jsonb_build_object('name','${scope}'),'deployment',jsonb_build_object('environment',jsonb_build_object('name','${scope}'))),
      '/picker-window','webjs','${scope}','${scope}' FROM (VALUES(now()-interval '10 minutes'),(now()-interval '2 hours')) AS events(at)`);
  let release = () => {};
  try {
    for (const tab of ["performance", "overview"]) {
      await page.goto(`/p/${DEMO_PROJECT}/rum?tab=${tab}&since=1H&service_scope=${scope}&environment=${scope}`);
      const row = page.locator("#rum-panel-pages tbody tr").filter({ has: page.getByRole("link", { name: "/picker-window", exact: true }) });
      await expect(row.locator("td").nth(1)).toHaveText("1");
      const changed = page.waitForResponse(response => response.request().headers()["hx-source"] === "div#rum-panel-pages");
      await page.locator("[data-live-range]").click();
      await page.locator('#n-timepicker-popover button[data-value="24H"]').click();
      const changedUrl = new URL((await changed).url());
      await expect(row.locator("td").nth(1)).toHaveText("2");
      expect(changedUrl.searchParams.get("since")).toBe("24H");
      for (const key of ["service_scope", "environment"]) expect(changedUrl.searchParams.get(key)).toBe(scope);
      const previous = page.waitForResponse(response => response.request().headers()["hx-source"] === "div#rum-panel-pages");
      await page.getByRole("button", { name: "Previous time window", exact: true }).first().click();
      const previousUrl = new URL((await previous).url());
      for (const key of ["from", "to"]) {
        expect(previousUrl.searchParams.get(key)).toBeTruthy();
        expect(previousUrl.searchParams.get(key)).toBe(new URL(page.url()).searchParams.get(key));
      }
      expect(previousUrl.searchParams.get("since") ?? "").toBe("");
      for (const key of ["service_scope", "environment"]) expect(previousUrl.searchParams.get(key)).toBe(scope);
      await expect(row).toHaveCount(0);
    }
    await page.goto(`/p/${DEMO_PROJECT}/rum?tab=overview&service_scope=${scope}&environment=${scope}`);
    const defaultRow = page.locator("#rum-panel-pages tbody tr").filter({ has: page.getByRole("link", { name: "/picker-window", exact: true }) });
    await expect(defaultRow.locator("td").nth(1)).toHaveText("2");
    const defaultRefresh = page.waitForResponse(response => response.request().headers()["hx-source"] === "div#rum-panel-pages");
    await page.evaluate(() => window.dispatchEvent(new Event("update-query")));
    expect(new URL((await defaultRefresh).url()).searchParams.get("since")).toBe("24H");
    await expect(defaultRow.locator("td").nth(1)).toHaveText("2");
    for (const [tab, panel] of [["performance", "pages"], ["sessions", "sessions"]]) {
      const pending = new Promise<void>(resolve => { release = resolve; });
      let fetched = () => {};
      const initialFetched = new Promise<void>(resolve => { fetched = resolve; });
      let held = false;
      const cancelled: string[] = [];
      page.on("requestfailed", request => {
        const url = new URL(request.url());
        if (url.searchParams.get("panel") === panel && url.searchParams.get("since") === "1H") cancelled.push(request.url());
      });
      await page.route("**/rum?**", async route => {
        if (held || new URL(route.request().url()).searchParams.get("panel") !== panel) return route.continue();
        held = true;
        const response = await route.fetch();
        fetched();
        await pending;
        await route.fulfill({ response });
      });
      await page.context().clearCookies();
      const scopeParams = tab === "sessions" ? `q=${fixtureUser}` : `service_scope=${scope}&environment=${scope}`;
      await page.goto(`/p/${DEMO_PROJECT}/rum?tab=${tab}&since=1H&${scopeParams}`);
      await initialFetched;
      const responses: number[] = [];
      page.on("response", response => {
        const url = new URL(response.url());
        if (url.searchParams.get("panel") === panel && url.searchParams.get("since") === "24H") responses.push(response.status());
      });
      await page.locator("[data-live-range]").click();
      await page.locator('#n-timepicker-popover button[data-value="24H"]').click();
      await expect(page.locator("#n-currentRange")).toHaveText("Last 24 hours");
      release();
      if (tab === "performance") await expect(defaultRow.locator("td").nth(1)).toHaveText("2");
      else {
        await expect(page.locator(".rum-session-link")).toHaveCount(200);
        await expect(page.locator("#rum-replay-workspace")).toBeVisible();
      }
      await expect.poll(() => responses).toEqual([200]);
      await expect.poll(() => cancelled.length).toBe(1);
      await page.unrouteAll({ behavior: "wait" });
    }
  } finally {
    release();
    await page.unrouteAll({ behavior: "wait" });
    sql(cleanup);
  }
});

test("failed RUM queries show an error after the panel swap and recover on retry", async ({ page }) => {
  test.skip(!process.env.E2E_BASE_URL, "Requires the disposable e2e database");
  const scope = `e2e-panel-failure-${Date.now()}`;
  const cleanup = `DELETE FROM otel_metrics WHERE project_id='${DEMO_PROJECT}' AND resource___service___name='${scope}'`;
  sql(`INSERT INTO otel_metrics (project_id,id,series_id,timestamp,metric_name,metric_type,metric_unit,value,resource,resource___service___name,attributes)
    VALUES ('${DEMO_PROJECT}',gen_random_uuid(),'${scope}',now()-interval '10 seconds','browser.web_vital.fcp','GAUGE','ms',10,
      jsonb_build_object('service',jsonb_build_object('name','${scope}')),'${scope}','{"page.url":"/panel-recovery"}')`);
  let renamed = false;
  try {
    sql("ALTER TABLE otel_metrics RENAME TO rum_failed_metrics_fixture");
    renamed = true;
    const failed = page.waitForResponse(response => new URL(response.url()).searchParams.get("panel") === "vitals");
    await page.goto(`/p/${DEMO_PROJECT}/rum?tab=performance&since=1H&service_scope=${scope}`);
    expect((await failed).status()).toBe(200);
    const panel = page.locator("#rum-panel-vitals");
    await expect(panel.getByRole("alert")).toContainText("Some RUM data could not be loaded.");
    await expect(panel.getByText("No data", { exact: true })).toHaveCount(0);
    sql("ALTER TABLE rum_failed_metrics_fixture RENAME TO otel_metrics");
    renamed = false;
    const recovered = page.waitForResponse(response => new URL(response.url()).searchParams.get("panel") === "vitals");
    await panel.getByRole("button", { name: "Retry", exact: true }).click();
    expect((await recovered).status()).toBe(200);
    await expect(panel.getByRole("alert")).toHaveCount(0);
    await expect(panel.getByRole("row").filter({ hasText: "First Contentful Paint" })).toContainText("10.0 ms");
  } finally {
    if (renamed) sql("ALTER TABLE rum_failed_metrics_fixture RENAME TO otel_metrics");
    sql(cleanup);
  }
});

test("mobile vital tables keep measurements and page identities readable", async ({ page }) => {
  test.skip(!process.env.E2E_BASE_URL, "Requires the disposable e2e database");
  const scope = `e2e-mobile-vitals-${Date.now()}`;
  const cleanup = `DELETE FROM otel_metrics WHERE project_id='${DEMO_PROJECT}' AND resource___service___name='${scope}'`;
  sql(`INSERT INTO otel_metrics (project_id,id,series_id,timestamp,start_timestamp,metric_name,metric_type,metric_unit,value,aggregation_temporality,distribution_count,hist_bucket_counts,hist_explicit_bounds,resource,resource___service___name,attributes)
    SELECT '${DEMO_PROJECT}',gen_random_uuid(),'${scope}' || name,now()-interval '10 seconds',now()-interval '1 minute',
      'browser.web_vital.' || name,type,unit,value,'DELTA',count,buckets,bounds,jsonb_build_object('service',jsonb_build_object('name','${scope}')),'${scope}',
      '{"page.url":"https://shop.example/item/ABCDEF12?variant=long-mobile-page-identity"}'
    FROM (VALUES ('lcp','HISTOGRAM','ms',NULL::float8,100::bigint,ARRAY[80,20]::bigint[],ARRAY[100]::float8[]),
      ('fcp','GAUGE','ms',22.5,NULL,NULL,NULL),('ttfb','SUM','ms',100,NULL,NULL,NULL)) AS points(name,type,unit,value,count,buckets,bounds)`);
  try {
    for (const width of [390, 320, 1440]) {
      await page.setViewportSize({ width, height: 844 });
      await page.goto(`/p/${DEMO_PROJECT}/rum?tab=performance&since=1H&service_scope=${scope}`);
      const table = page.getByRole("heading", { name: "Web Vitals field performance", exact: true }).locator("xpath=ancestor::section[1]").getByRole("table");
      await expect(table.getByRole("row")).toHaveCount(6);
      await expect(table.getByRole("cell")).toHaveCount(35);
      await expect(table).toContainText("Estimated within");
      await expect(table).toContainText("Exact quantile");
      await expect(table).toContainText("Unsupported population");
      for (const theme of ["light", "dark"]) {
        await page.evaluate(theme => { if (document.body.dataset.theme !== theme) (window as any).toggleDarkMode(); }, theme);
        expect(await table.locator("tbody tr").evaluateAll(rows => rows.every(row => [...row.querySelectorAll("td")].every(cell => {
          const bounds = cell.getBoundingClientRect();
          return bounds.width > 0 && bounds.left >= 0 && bounds.right <= window.innerWidth && cell.scrollWidth <= cell.clientWidth;
        })))).toBe(true);
        await table.locator("tbody tr").first().screenshot({ path: test.info().outputPath(`vitals-${theme}-${width}.png`) });
        const pages = page.getByRole("heading", { name: "Web Vitals by page", exact: true }).locator("xpath=ancestor::section[1]").getByRole("table");
        await expect(pages.getByRole("cell")).toHaveCount(7);
        await pages.locator("tbody tr").first().screenshot({ path: test.info().outputPath(`page-vitals-${theme}-${width}.png`) });
        expect(await pages.locator("tbody td").evaluateAll(cells => cells.every((cell, index) => {
          const bounds = cell.getBoundingClientRect();
          const text = document.createRange();
          text.selectNodeContents(cell);
          return bounds.width > 0 && bounds.left >= 0 && bounds.right <= window.innerWidth && cell.scrollWidth <= cell.clientWidth
            && (window.innerWidth >= 640 && index === 0 || [...text.getClientRects()].every(rect => rect.left >= bounds.left - 1 && rect.right <= bounds.right + 1));
        }))).toBe(true);
        await expect(pages.getByRole("columnheader")).toHaveCount(7);
        await expect(pages.getByRole("columnheader", { name: "Observations", exact: true })).toHaveCount(1);
        if (width < 640) {
          await expect(pages.getByText("Unsupported population", { exact: true })).toBeVisible();
          await expect(pages.getByText("Exact quantile", { exact: true })).toBeVisible();
        }
        const link = pages.getByRole("link", { name: "https://shop.example/item/ABCDEF12?variant=long-mobile-page-identity", exact: true });
        if (width < 640) expect(await link.evaluate(element => element.getBoundingClientRect().height)).toBeGreaterThanOrEqual(44);
        const destination = new URL((await link.getAttribute("href"))!, page.url());
        expect(destination.searchParams.get("query")).toContain('attributes.url.full == "https://shop.example/item/ABCDEF12?variant=long-mobile-page-identity"');
        expect(destination.searchParams.get("query")).toContain(`service=="${scope}"`);
        expect(destination.searchParams.get("since")).toBe("1H");
      }
      await expect(table.getByRole("columnheader", { name: "Coverage", exact: true })).toHaveCount(1);
    }
  } finally {
    sql(cleanup);
  }
});

test("singleton vital trend keeps the selected time axis", async ({ page }) => {
  test.skip(!process.env.E2E_BASE_URL, "Requires the disposable e2e database");
  const scope = `e2e-vital-axis-${Date.now()}`;
  const cleanup = `DELETE FROM otel_metrics WHERE project_id='${DEMO_PROJECT}' AND resource___service___name='${scope}'`;
  // Keep the short window inside one five-minute bucket even at a wall-clock boundary.
  const to = new Date(Math.floor(Date.now() / 300000) * 300000 - 60000);
  const from = new Date(to.getTime() - 90 * 60 * 1000);
  sql(`INSERT INTO otel_metrics (project_id,id,series_id,timestamp,metric_name,metric_type,metric_unit,value,resource,resource___service___name)
    VALUES ('${DEMO_PROJECT}',gen_random_uuid(),'${scope}','${new Date(to.getTime() - 10000).toISOString()}','browser.web_vital.lcp','GAUGE','ms',1200,
      jsonb_build_object('service',jsonb_build_object('name','${scope}')),'${scope}')`);
  try {
    for (const [range, span] of [[{ since: "1H" }, 60 * 60 * 1000], [{ from: from.toISOString(), to: to.toISOString() }, 90 * 60 * 1000], [{ from: new Date(to.getTime() - 30000).toISOString(), to: to.toISOString() }, 30000]] as const) {
      await page.goto(`/p/${DEMO_PROJECT}/rum?${new URLSearchParams({ tab: "performance", service_scope: scope, ...range })}`);
      const chart = page.locator("#rum-vital-trend-lcp");
      await expect(chart).toBeVisible();
      await chart.scrollIntoViewIfNeeded();
      await expect.poll(() => chart.evaluate(element => {
        const instance = (window as any).echarts?.getInstanceByDom(element);
        const extent = instance?.getModel().getComponent("xAxis").axis.scale.getExtent();
        return extent ? extent[1] - extent[0] : null;
      })).toBe(span);
      await expect.poll(() => chart.evaluate(element => {
        const instance = (window as any).echarts?.getInstanceByDom(element);
        const series = instance?.getModel().getSeriesByIndex(0);
        if (!series || series.getData().count() !== 1 || !series.get("showSymbol")) return false;
        const datum = instance.getOption().dataset[0].source[1];
        const [x, y] = instance.convertToPixel({ seriesIndex: 0 }, datum);
        const plot = instance.getModel().getComponent("grid").coordinateSystem.getRect();
        return datum[1] === 1200 && x >= plot.x && x <= plot.x + plot.width && y >= plot.y && y <= plot.y + plot.height;
      })).toBe(true);
    }
  } finally {
    sql(cleanup);
  }
});

test("explicit RUM window survives reload and shared navigation without since", async ({ page, context }) => {
  const to = new Date();
  const from = new Date(to.getTime() - 90 * 60 * 1000);
  const params = new URLSearchParams({ tab: "performance", from: from.toISOString(), to: to.toISOString(), environment: "e2e-explicit-window", service_scope: "e2e-explicit-window" });
  const url = `/p/${DEMO_PROJECT}/rum?${params}`;
  for (const target of [page, await context.newPage()]) {
    const response = await target.goto(url);
    const initialRange = (await response!.text()).match(/id="n-currentRange">([^<]*)/)?.[1];
    expect(initialRange).toBeTruthy();
    expect(initialRange).not.toBe("Last 24 hours");
    await expect(target.locator("#n-currentRange")).not.toHaveText("Last 24 hours");
    await target.reload();
    await expect(target.locator("#n-currentRange")).not.toHaveText("Last 24 hours");
    const current = new URL(target.url());
    for (const [key, value] of params) expect(current.searchParams.get(key)).toBe(value);
    expect(current.searchParams.has("since")).toBe(false);
  }
});

test("initial RUM field panel does not depend on application globals", async ({ page }) => {
  test.skip(!process.env.E2E_BASE_URL, "Requires the disposable e2e database");
  const scope = `e2e-initial-rum-${Date.now()}`;
  const cleanup = `DELETE FROM otel_metrics WHERE project_id='${DEMO_PROJECT}' AND resource___service___name='${scope}'`;
  sql(`INSERT INTO otel_metrics (project_id,id,series_id,timestamp,metric_name,metric_type,metric_unit,value,resource,resource___service___name)
    SELECT '${DEMO_PROJECT}',gen_random_uuid(),'${scope}',at,'browser.web_vital.fcp','GAUGE','ms',value,
      jsonb_build_object('service',jsonb_build_object('name','${scope}')),'${scope}'
    FROM (VALUES(now()-interval '10 seconds',22.5),(now()-interval '2 hours',99)) AS points(at,value)`);
  let release = () => {};
  const held = new Promise<void>(resolve => { release = resolve; });
  const errors: string[] = [];
  page.on("console", message => { if (message.type() === "error") errors.push(message.text()); });
  page.on("pageerror", error => errors.push(error.message));
  await page.route(/\/public\/assets\/(?:js\/main\.|web-components\/dist\/js\/index\.).*\.js$/, async route => { await held; await route.continue(); });
  const responses: string[] = [];
  page.on("response", response => { if (new URL(response.url()).searchParams.get("panel") === "vitals" && response.status() === 200) responses.push(response.url()); });
  try {
    await page.goto(`/p/${DEMO_PROJECT}/rum?tab=performance&since=24H&since=1H&service_scope=${scope}`, { waitUntil: "commit" });
    const shell = page.locator("#rum-panel-vitals");
    await expect(shell).toBeAttached();
    await expect.poll(() => page.evaluate(() => typeof (window as any).htmx)).toBe("object");
    expect(await page.evaluate(() => typeof (window as any).params)).toBe("undefined");
    await shell.evaluate(element => (window as any).htmx.process(element));
    const table = page.getByRole("heading", { name: "Web Vitals field performance", exact: true }).locator("xpath=ancestor::section[1]").getByRole("table");
    await expect(table).toContainText("22.5 ms", { timeout: 5_000 });
    expect(responses).toHaveLength(1);
    const url = new URL(responses[0]);
    expect(url.searchParams.getAll("since")).toEqual(["1H"]);
    expect(url.searchParams.get("service_scope")).toBe(scope);
    expect(errors).toEqual([]);
  } finally {
    release();
    try { await page.unrouteAll({ behavior: "wait" }); } finally { sql(cleanup); }
    await test.info().attach("initial-rum-errors", { body: JSON.stringify(errors), contentType: "application/json" });
  }
});


test("automatic live updates do not cancel the first RUM panel response", async ({ page }) => {
  test.skip(!process.env.E2E_BASE_URL, "Requires the disposable e2e database");
  const scope = `e2e-initial-live-${Date.now()}`;
  const cleanup = `DELETE FROM otel_metrics WHERE project_id='${DEMO_PROJECT}' AND resource___service___name='${scope}'`;
  sql(`INSERT INTO otel_metrics (project_id,id,series_id,timestamp,metric_name,metric_type,metric_unit,value,resource,resource___service___name)
    VALUES('${DEMO_PROJECT}',gen_random_uuid(),'${scope}',now()-interval '10 seconds','browser.web_vital.fcp','GAUGE','ms',22.5,
      jsonb_build_object('service',jsonb_build_object('name','${scope}')),'${scope}')`);
  let release = () => {};
  try {
    for (const [tab, panel, scopeParams] of [["performance", "vitals", `service_scope=${scope}`], ["sessions", "sessions", `q=${fixtureUser}`]]) {
      const pending = new Promise<void>(resolve => { release = resolve; });
      let fetched = () => {};
      const initialFetched = new Promise<void>(resolve => { fetched = resolve; });
      const requests: string[] = [];
      const cancelled: string[] = [];
      const trackCancellation = (request: import("@playwright/test").Request) => {
        if (new URL(request.url()).searchParams.get("panel") === panel) cancelled.push(request.url());
      };
      page.on("requestfailed", trackCancellation);
      await page.route("**/rum?**", async route => {
        if (new URL(route.request().url()).searchParams.get("panel") !== panel) return route.continue();
        requests.push(route.request().url());
        if (requests.length !== 1) return route.continue();
        const response = await route.fetch();
        fetched();
        await pending;
        await route.fulfill({ response });
      });
      await page.goto(`/p/${DEMO_PROJECT}/rum?tab=${tab}&since=24H&${scopeParams}`);
      await initialFetched;
      await expect(page.locator("[data-time-transport]")).toHaveAttribute("data-interval", "15000");
      await expect(page.locator(`#rum-panel-${panel}`)).toHaveAttribute("data-deferred-shell", "");
      await page.evaluate(() => window.dispatchEvent(new CustomEvent("update-query", { detail: { source: "auto-refresh" } })));
      release();
      if (tab === "performance") await expect(page.locator("#rum-panel-vitals")).toContainText("22.5 ms");
      else await expect(page.locator(".rum-session-link")).toHaveCount(200);
      expect(cancelled).toEqual([]);
      expect(requests).toHaveLength(1);
      const loaded = page.waitForResponse(response => new URL(response.url()).searchParams.get("panel") === panel);
      await page.evaluate(() => window.dispatchEvent(new CustomEvent("update-query", { detail: { source: "auto-refresh" } })));
      expect((await loaded).status()).toBe(200);
      await page.unrouteAll({ behavior: "wait" });
      page.off("requestfailed", trackCancellation);
    }
  } finally {
    release();
    try { await page.unrouteAll({ behavior: "wait" }); } finally { sql(cleanup); }
  }
});

test("refreshed vital charts remain registered on their current elements", async ({ page }) => {
  test.skip(!process.env.E2E_BASE_URL, "Requires the disposable e2e database");
  const scope = `e2e-vital-registry-${Date.now()}`;
  const cleanup = `DELETE FROM otel_metrics WHERE project_id='${DEMO_PROJECT}' AND resource___service___name='${scope}'`;
  sql(`INSERT INTO otel_metrics (project_id,id,series_id,timestamp,metric_name,metric_type,metric_unit,value,resource,resource___service___name)
    SELECT '${DEMO_PROJECT}',gen_random_uuid(),'${scope}' || name,at,'browser.web_vital.' || name,'GAUGE','ms',value,
      jsonb_build_object('service',jsonb_build_object('name','${scope}')),'${scope}'
    FROM (VALUES ('fcp',now()-interval '10 minutes',20),('ttfb',now()-interval '10 minutes',100),
      ('fcp',now()-interval '2 hours',80),('ttfb',now()-interval '2 hours',500)) AS points(name,at,value)`);
  const assertCharts = async (span: number, samples: number) => {
    for (const name of ["fcp", "ttfb"]) {
      const chart = page.locator(`#rum-vital-trend-${name}`);
      await chart.scrollIntoViewIfNeeded();
      await expect.poll(() => chart.evaluate(element => {
        const instance = (window as any).echarts?.getInstanceByDom(element);
        const extent = instance?.getModel().getComponent("xAxis").axis.scale.getExtent();
        const data = instance?.getOption().dataset[0].source;
        return [Boolean(instance && instance.getDom() === element && !instance.isDisposed()),
          element.querySelectorAll("canvas").length, extent ? extent[1] - extent[0] : null,
          data?.slice(1).map((point: number[]) => point[1]).sort((a: number, b: number) => a - b) ?? []];
      })).toEqual([true, 1, span, (name === "fcp" ? [20, 80] : [100, 500]).slice(0, samples)]);
    }
  };
  const waitForTrend = () => page.evaluate(() => new Promise<void>(resolve => {
    const settled = (event: Event) => {
      if ((event.target as HTMLElement).id !== "rum-panel-vital_trend") return;
      document.removeEventListener("htmx:after:settle", settled);
      resolve();
    };
    document.addEventListener("htmx:after:settle", settled);
  }));
  try {
    await page.setViewportSize({ width: 1440, height: 1000 });
    await page.goto(`/p/${DEMO_PROJECT}/rum?tab=performance&since=1H&service_scope=${scope}`);
    await expect(page.getByRole("link", { name: "Performance", exact: true })).toHaveAttribute("aria-current", "page");
    await expect(page.getByRole("heading", { name: "Web Vitals over time", exact: true })).toBeVisible();
    await page.locator("[data-time-transport]").getByRole("button", { name: /^Pause live (data|updates)$/ }).click();
    await expect(page.locator("[data-time-transport]")).toHaveAttribute("data-interval", "0");
    await assertCharts(60 * 60 * 1000, 1);
    await page.locator("[data-live-range]").click();
    const changed = waitForTrend();
    await page.locator('#n-timepicker-popover button[data-value="24H"]').click();
    await changed;
    await expect(page.locator("#n-currentRange")).toHaveText("Last 24 hours");
    await assertCharts(24 * 60 * 60 * 1000, 2);
    await page.evaluate(() => {
      (window as any).__rumPriorCharts = ["fcp", "ttfb"].map(name => (window as any).echarts.getInstanceByDom(document.getElementById(`rum-vital-trend-${name}`)));
    });
    const refreshed = waitForTrend();
    await page.evaluate(() => window.dispatchEvent(new Event("update-query")));
    await refreshed;
    await assertCharts(24 * 60 * 60 * 1000, 2);
    expect(await page.evaluate(() => ["fcp", "ttfb"].every((name, index) => {
      const instance = (window as any).__rumPriorCharts[index];
      return !instance.isDisposed() && (window as any).echarts.getInstanceByDom(document.getElementById(`rum-vital-trend-${name}`)) === instance;
    }))).toBe(true);
  } finally {
    sql(cleanup);
  }
});

test("automatic session ticks keep an in-flight list request while search replaces it", async ({ page }) => {
  test.skip(!process.env.E2E_BASE_URL, "Requires a disposable fixture database");
  await page.setViewportSize({ width: 390, height: 900 });
  await page.goto(`/p/${DEMO_PROJECT}/rum?tab=sessions&since=1H`);
  const list = page.locator("#rum-sessions-list");
  await expect(list.locator(".rum-session-link").first()).toBeVisible();
  await page.locator('[popovertarget="n-timepicker-popover"]').click();
  await page.locator("[data-mobile-live-toggle]").click();
  await expect(page.locator("[data-time-transport]")).toHaveAttribute("data-interval", "0");
  await page.locator('[popovertarget="n-timepicker-popover"]').click();
  const workspace = page.locator("#rum-replay-workspace");
  await workspace.evaluate(element => element.setAttribute("data-e2e-preserved", "true"));
  let release = () => {};
  const held = new Promise<void>(resolve => { release = resolve; });
  let releaseSearch = () => {};
  const heldSearch = new Promise<void>(resolve => { releaseSearch = resolve; });
  const automatic: import("@playwright/test").Request[] = [];
  await page.route("**/rum?**", async route => {
    const url = new URL(route.request().url());
    if (url.searchParams.get("panel") === "sessions" && route.request().headers()["hx-source"] === "div#rum-panel-sessions") {
      automatic.push(route.request());
      if (automatic.length === 1) await held;
    }
    if (url.searchParams.get("q") === "e2e-missing-session" && route.request().headers()["hx-source"] === "form#rum-session-search-form") await heldSearch;
    await route.continue();
  });
  try {
    const first = page.waitForRequest(request => new URL(request.url()).searchParams.get("panel") === "sessions");
    await page.evaluate(() => window.dispatchEvent(new CustomEvent("update-query", { detail: { source: "auto-refresh" } })));
    const pending = await first;
    await expect(page.locator("#rum-panel-sessions")).toHaveClass(/htmx-request/);
    await page.evaluate(() => window.dispatchEvent(new CustomEvent("update-query", { detail: { source: "auto-refresh" } })));
    await page.waitForTimeout(300); // Let an unwanted replacement reach the real HTTP route.
    expect(automatic).toHaveLength(1);
    expect(pending.failure()).toBeNull();
    const search = page.getByRole("searchbox", { name: "Search sessions" });
    const replaced = page.waitForEvent("requestfailed", request => request === pending);
    const searching = page.waitForRequest(request => new URL(request.url()).searchParams.get("q") === "e2e-missing-session");
    const searched = page.waitForResponse(response => new URL(response.url()).searchParams.get("q") === "e2e-missing-session");
    await search.fill("e2e-missing-session");
    const pendingSearch = await searching;
    release();
    await replaced;
    await expect(page.locator("#rum-session-search-form")).toHaveClass(/htmx-request/);
    await page.evaluate(() => window.dispatchEvent(new CustomEvent("update-query", { detail: { source: "auto-refresh" } })));
    await page.waitForTimeout(300);
    expect(automatic).toHaveLength(1);
    expect(pendingSearch.failure()).toBeNull();
    releaseSearch();
    expect((await searched).status()).toBe(200);
    await expect(page.getByText("No sessions match this filter", { exact: true })).toBeVisible();
    await expect(search).toBeFocused();
    await expect(workspace).toHaveAttribute("data-e2e-preserved", "true");
    await expect(page.locator("#rum-session-search-form")).not.toHaveClass(/htmx-request/);
    const resumed = page.waitForResponse(response => response.request().headers()["hx-source"] === "div#rum-panel-sessions");
    await page.evaluate(() => window.dispatchEvent(new CustomEvent("update-query", { detail: { source: "auto-refresh" } })));
    expect((await resumed).status()).toBe(200);
    expect(automatic).toHaveLength(2);
    await expect(search).toHaveValue("e2e-missing-session");
    await expect(workspace).toHaveAttribute("data-e2e-preserved", "true");
  } finally { release(); releaseSearch(); await page.unrouteAll({ behavior: "wait" }); }
});

test("manual session replacements keep cancellation attached to the newest request", async ({ page }) => {
  test.skip(!process.env.E2E_BASE_URL, "Requires a disposable fixture database");
  await page.setViewportSize({ width: 390, height: 900 });
  await page.goto(`/p/${DEMO_PROJECT}/rum?tab=sessions&since=1H`);
  await expect(page.locator(".rum-session-link").first()).toBeVisible();
  await page.locator('[popovertarget="n-timepicker-popover"]').click();
  await page.locator("[data-mobile-live-toggle]").click();
  await expect(page.locator("[data-time-transport]")).toHaveAttribute("data-interval", "0");
  await page.locator('[popovertarget="n-timepicker-popover"]').click();
  let releaseFirst = () => {};
  const firstGate = new Promise<void>(resolve => { releaseFirst = resolve; });
  let releaseSecond = () => {};
  const secondGate = new Promise<void>(resolve => { releaseSecond = resolve; });
  await page.route("**/rum?**", async route => {
    const query = new URL(route.request().url()).searchParams.get("q");
    if (query === "e2e-first-pending") await firstGate;
    if (query === "e2e-second-pending") await secondGate;
    await route.continue();
  });
  try {
    const search = page.getByRole("searchbox", { name: "Search sessions" });
    const firstIssued = page.waitForRequest(request => new URL(request.url()).searchParams.get("q") === "e2e-first-pending");
    await search.fill("e2e-first-pending");
    const first = await firstIssued;
    const firstCancelled = page.waitForEvent("requestfailed", request => request === first);
    const secondIssued = page.waitForRequest(request => new URL(request.url()).searchParams.get("q") === "e2e-second-pending");
    await search.fill("e2e-second-pending");
    const second = await secondIssued;
    const secondSettled = Promise.race([
      page.waitForEvent("requestfinished", request => request === second),
      page.waitForEvent("requestfailed", request => request === second),
    ]);
    releaseFirst();
    await firstCancelled;
    await expect(page.locator("#rum-session-search-form")).toHaveClass(/htmx-request/);
    const latest = page.waitForResponse(response => new URL(response.url()).searchParams.get("q") === fixtureUser);
    await search.fill(fixtureUser);
    expect((await latest).status()).toBe(200);
    await expect(page.locator(".rum-session-link")).toHaveCount(200);
    await expect.poll(() => new URL(page.url()).searchParams.get("q")).toBe(fixtureUser);
    await expect(search).toBeFocused();
    releaseSecond();
    await secondSettled;
    await page.waitForTimeout(300);
    await expect(page.locator(".rum-session-link")).toHaveCount(200);
    expect(second.failure()).not.toBeNull();
    await expect(search).toHaveValue(fixtureUser);
    await expect.poll(() => new URL(page.url()).searchParams.get("q")).toBe(fixtureUser);
    await expect(search).toBeFocused();
  } finally {
    releaseFirst();
    releaseSecond();
    await page.unrouteAll({ behavior: "wait" });
  }
});
