import { test, expect } from "@playwright/test";
import { DEMO_PROJECT, sql } from "./helpers";

test("audience groups filter sessions, search users, and persist named segments", async ({ page }) => {
  const app = `e2e-rum-audience-${Date.now()}`;
  const segment = `${app} Mobile Safari`;
  const cleanup = () => sql(`DELETE FROM otel_logs_and_spans WHERE project_id='${DEMO_PROJECT}' AND resource___service___name='${app}'; DELETE FROM projects.rum_segments WHERE project_id='${DEMO_PROJECT}' AND name='${segment}';`);
  sql(`INSERT INTO otel_logs_and_spans (project_id,name,timestamp,start_time,end_time,resource___service___name,resource___user_agent___original,attributes___session___id,attributes___user___id,attributes___user___email,attributes___exception___type)
    VALUES
      ('${DEMO_PROJECT}','documentLoad',now(),now(),now(),'${app}','iPhone Mobile Safari/604','${app}-alice','alice','alice@example.com',NULL),
      ('${DEMO_PROJECT}','browser error',now(),now(),now(),'${app}','iPhone Mobile Safari/604','${app}-alice','alice','alice@example.com','TypeError'),
      ('${DEMO_PROJECT}','documentLoad',now(),now(),now(),'${app}','Windows Chrome/151 Safari/537','${app}-bob','bob','bob@example.com',NULL),
      ('${DEMO_PROJECT}','documentLoad',now(),now(),now(),'${app}',NULL,'${app}-anonymous',NULL,NULL,NULL);`);
  try {
    const errors: string[] = [];
    page.on("pageerror", error => errors.push(error.message));
    page.on("console", message => { if (message.type() === "error" && /SyntaxError|parse error/i.test(message.text())) errors.push(message.text()); });
    await page.goto(`/p/${DEMO_PROJECT}/rum?tab=audience&var-app=${app}&since=1H`);
    const toolbar = page.locator("#rum-segments");
    await expect(toolbar.getByRole("link", { name: "View sessions", exact: true })).toBeVisible();
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
    await toolbar.getByText("Save segment", { exact: true }).click();
    await toolbar.getByLabel("Segment name").fill(segment);
    await toolbar.getByRole("button", { name: "Save segment", exact: true }).click();
    await expect(toolbar.getByRole("link", { name: segment, exact: true })).toBeVisible();
    await page.reload();
    await expect(toolbar.getByRole("link", { name: segment, exact: true })).toBeVisible();
    await toolbar.getByRole("link", { name: "View errors", exact: true }).click();
    await expect(page).toHaveURL(/\/tab\/errors/);
    await expect(page).toHaveURL(/var-browser=Safari/);
    await toolbar.getByRole("link", { name: "View sessions", exact: true }).click();
    await expect(page).toHaveURL(/viz_type=sessions/);
    expect(new URL(page.url()).searchParams.get("query")).toContain("Safari/");
    expect(errors).toEqual([]);
  } finally { cleanup(); }
});
