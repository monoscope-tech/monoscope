import { test, expect } from "@playwright/test";
import { DEMO_PROJECT, sql } from "./helpers";

const clearSessions = `DELETE FROM projects.replay_sessions WHERE project_id = '${DEMO_PROJECT}' AND session_id::text LIKE 'ec2e0000-0000-4000-8000-%'`;

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
    const previousList = await page.locator("#rum-sessions-list").elementHandle();
    const refresh = page.waitForResponse(r => {
      const url = new URL(r.url());
      return url.pathname.endsWith("/rum") && url.searchParams.get("panel") === "sessions"
        && r.request().headers()["hx-source"] === "div#rum-panel-sessions";
    });
    await page.evaluate(() => window.dispatchEvent(new Event("update-query")));
    const refreshedUrl = new URL((await refresh).url());
    expect(refreshedUrl.searchParams.get("q")).toBe("e2e-missing-session");
    expect(refreshedUrl.searchParams.get("filter") ?? "").toBe(filter);
    await expect.poll(() => previousList!.evaluate(element => element.isConnected)).toBe(false);
    await expect(filters.getByRole("link", { name: label, exact: true })).toHaveAttribute("aria-current", "page");
    await expect(page.getByText("No sessions match this filter", { exact: true })).toBeVisible();
    await expect(workspace).toHaveAttribute("data-e2e-preserved", "true");
  }
});
