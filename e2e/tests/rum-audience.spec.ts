import { test, expect } from "@playwright/test";
import { DEMO_PROJECT, sql } from "./helpers";

test("audience groups filter sessions, search users, and persist dashboard views", async ({ page }) => {
  const app = `e2e-rum-audience-${Date.now()}`;
  const segment = `${app} Mobile Safari`;
  const cleanup = () => sql(`DELETE FROM otel_logs_and_spans WHERE project_id='${DEMO_PROJECT}' AND resource___service___name='${app}'; UPDATE projects.dashboards SET saved_views=ARRAY(SELECT v FROM unnest(saved_views) v WHERE v->>'name' <> '${segment}') WHERE project_id='${DEMO_PROJECT}' AND base_template='rum.yaml';`);
  sql(`INSERT INTO otel_logs_and_spans (project_id,summary,name,timestamp,start_time,end_time,resource___service___name,resource___user_agent___original,attributes___session___id,attributes___user___id,attributes___user___email,attributes___exception___type)
    VALUES
      ('${DEMO_PROJECT}',ARRAY['RUM audience'],'documentLoad',now(),now(),now(),'${app}','iPhone Mobile Safari/604','${app}-alice','alice','alice@example.com',NULL),
      ('${DEMO_PROJECT}',ARRAY['RUM audience'],'browser error',now(),now(),now(),'${app}','iPhone Mobile Safari/604','${app}-alice','alice','alice@example.com','TypeError'),
      ('${DEMO_PROJECT}',ARRAY['RUM audience'],'documentLoad',now(),now(),now(),'${app}','Windows Chrome/151 Safari/537','${app}-bob','bob','bob@example.com',NULL),
      ('${DEMO_PROJECT}',ARRAY['RUM audience'],'documentLoad',now(),now(),now(),'${app}',NULL,'${app}-anonymous',NULL,NULL,NULL);`);
  try {
    const errors: string[] = [];
    page.on("pageerror", error => errors.push(error.message));
    page.on("console", message => { if (message.type() === "error" && /SyntaxError|parse error/i.test(message.text())) errors.push(message.text()); });
    await page.goto(`/p/${DEMO_PROJECT}/rum?tab=audience&var-app=${app}&since=1H`);
    const toolbar = page.locator("#dashboard-views");
    const views = toolbar.getByRole("button", { name: /^Saved views/ });
    await expect(views).toBeVisible();
    const browsers = page.getByRole("table").filter({ has: page.getByRole("columnheader", { name: "Browser", exact: true }) });
    const systems = page.getByRole("table").filter({ has: page.getByRole("columnheader", { name: "Operating system", exact: true }) });
    await expect(browsers.getByText("Unknown", { exact: true })).toBeVisible();
    await expect(browsers.locator("tbody tr").filter({ hasText: "Safari" })).toContainText("100 %");
    for (const width of [1920, 1280, 390]) {
      await page.setViewportSize({ width, height: 1000 });
      await expect.poll(async () => {
        const [browser, system] = await Promise.all([browsers.boundingBox(), systems.boundingBox()]);
        return !!browser && !!system && (width === 1920 ? Math.abs(browser.y - system.y) < 3 : system.y > browser.y + browser.height);
      }).toBe(true);
      for (const table of [browsers, systems]) {
        expect(await table.evaluate(el => el.getBoundingClientRect().right <= innerWidth && el.scrollWidth <= el.clientWidth + 1)).toBe(true);
      }
    }
    await page.setViewportSize({ width: 1280, height: 1000 });
    await page.locator("[data-widget]").filter({ has: page.getByText("Identified users", { exact: true }) }).scrollIntoViewIfNeeded();
    const users = page.getByRole("table").filter({ has: page.getByRole("columnheader", { name: "User", exact: true }) });
    await expect(users.getByRole("link", { name: "alice@example.com" })).toBeVisible();
    await page.getByRole("searchbox", { name: "Search Identified users" }).fill("bob");
    await expect(users.locator("tbody tr").filter({ visible: true })).toHaveCount(1);
    await expect(users.locator("tbody tr").filter({ visible: true })).toContainText("bob@example.com");
    await page.getByRole("searchbox", { name: "Search Identified users" }).fill("");
    await expect(users.locator("tbody tr").filter({ visible: true })).toHaveCount(2);
    const safari = browsers.locator("tbody tr").filter({ hasText: "Safari" });
    await safari.focus();
    await safari.press("Enter");
    await expect(page).toHaveURL(/var-browser=Safari/);
    await views.click();
    await toolbar.getByLabel("View name").fill(segment);
    await toolbar.getByRole("button", { name: "Save view", exact: true }).click();
    await expect(views).toHaveAccessibleName(`Saved views: ${segment}`);
    await page.reload();
    await expect(views).toHaveAccessibleName(`Saved views: ${segment}`);
    await page.getByRole("tab", { name: "Errors", exact: true }).click();
    await expect(page).toHaveURL(/\/tab\/errors/);
    await expect(page).toHaveURL(/var-browser=Safari/);
    await page.getByRole("tab", { name: "Audience", exact: true }).click();
    await users.getByRole("link", { name: "alice@example.com" }).click();
    await expect(page).toHaveURL(/viz_type=sessions/);
    expect(new URL(page.url()).searchParams.get("query")).toContain("alice");
    expect(errors).toEqual([]);
  } finally { cleanup(); }
});

test("saved views work on custom dashboards across tabs and retain the time range", async ({ page }) => {
  const id = crypto.randomUUID();
  const schema = {
    title: "Shared dashboard views", widgets: [],
    variables: [
      { key: "service", title: "Service", type: "values", options: [["api"], ["worker"]] },
      { key: "region", title: "Region", type: "values", options: [["eu"], ["us"]] },
    ],
    tabs: [
      { name: "Overview", variables: ["service"], widgets: [] },
      { name: "Errors", variables: ["service", "region"], widgets: [] },
    ],
  };
  sql(`INSERT INTO projects.dashboards (id,project_id,created_by,title,schema) VALUES ('${id}','${DEMO_PROJECT}','00000000-0000-0000-0000-000000000000','Shared dashboard views','${JSON.stringify(schema)}');`);
  try {
    const root = `/p/${DEMO_PROJECT}/dashboards/${id}`;
    const toolbar = page.locator("#dashboard-views");
    const views = toolbar.getByRole("button", { name: /^Saved views/ });
    await page.goto(`${root}/tab/errors?var-service=worker&var-region=us&since=7D`);
    await views.click();
    await toolbar.getByLabel("View name").fill("US worker");
    await toolbar.getByRole("button", { name: "Save view", exact: true }).click();
    await expect(views).toHaveAccessibleName("Saved views: US worker");
    await page.reload();
    await expect(views).toHaveAccessibleName("Saved views: US worker");
    await page.getByRole("tab", { name: "Overview", exact: true }).click();
    await expect(page.locator('#dashboard-variables [name="region"]')).toHaveCount(0);
    await views.click();
    await toolbar.getByRole("link", { name: "Clear filters", exact: true }).click();
    await expect(page).not.toHaveURL(/var-service=/);
    await expect(views).toHaveAccessibleName("Saved views");
    await views.click();
    await toolbar.getByRole("link", { name: "US worker", exact: true }).click();
    await expect(page).toHaveURL(/\/tab\/overview.*var-service=worker/);
    expect(new URL(page.url()).searchParams.get("var-service")).toBe("worker");
    expect(new URL(page.url()).searchParams.get("var-region")).toBe("us");
    expect(new URL(page.url()).searchParams.get("since")).toBe("7D");
    // A variable edited after picking a view marks it as edited, without a page load.
    await expect(views).toHaveAccessibleName("Saved views: US worker");
    await page.evaluate(() => { const u = new URL(location.href); u.searchParams.set("var-service", "api"); history.pushState({}, "", u); window.dispatchEvent(new Event("update-query")); });
    await expect(views).toHaveAccessibleName("Saved views: US worker (edited)");
    await page.getByRole("tab", { name: "Errors", exact: true }).click();
    await expect(page.locator('#dashboard-variables [name="region"]')).toHaveCount(1);
    page.once("dialog", dialog => dialog.accept());
    await views.click();
    await toolbar.getByRole("button", { name: "Delete view US worker", exact: true }).click();
    await expect(toolbar.getByRole("link", { name: "US worker", exact: true })).toHaveCount(0);
  } finally {
    sql(`DELETE FROM projects.dashboards WHERE id='${id}';`);
  }
});
