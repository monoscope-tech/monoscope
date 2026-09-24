import { test, expect } from "@playwright/test";
import { DEMO_PROJECT, sql } from "./helpers";

const cleanup = `DELETE FROM apis.endpoints WHERE project_id='${DEMO_PROJECT}' AND hash='e2e-browser-endpoint';`;
test.beforeAll(() => sql(cleanup + `INSERT INTO apis.endpoints (project_id,url_path,url_params,method,host,hash,outgoing)
  VALUES ('${DEMO_PROJECT}','/e2e-browser','{}','GET','browser.example','e2e-browser-endpoint',false);`));
test.afterAll(() => sql(cleanup));

// Endpoint Analytics is a template-backed redirect, not a fixed dashboard id. Supplying
// its required variables is important: otherwise the intentional variable picker replaces
// the canvas and this test would never exercise the investigation tabs.
test("Endpoint Analytics exposes real-user impact and direct dependency investigation", async ({ page }) => {
  await page.goto(`/p/${DEMO_PROJECT}/endpoints/details?var-host=browser.example&var-endpointHash=e2e-browser-endpoint`);
  await page.waitForURL(new RegExp(`/p/${DEMO_PROJECT}/dashboards/[0-9a-f-]+`, "i"));

  await expect(page.getByText("Experience", { exact: true })).toBeVisible();
  await expect(page.getByText("Dependencies", { exact: true })).toBeVisible();
  for (const name of ["Operations", "Dependencies"]) {
    await expect(page.getByRole("tab", { name }).locator("svg path")).not.toHaveCount(0);
  }

  await page.getByText("Experience", { exact: true }).click();
  await expect(page.getByText("Real-user impact", { exact: true })).toBeVisible();
  await expect(page.getByText("Endpoint Sessions", { exact: true })).toBeVisible();
  await expect(page.getByText("Browser Request Outcomes", { exact: true })).toBeVisible();

  // The tab remains a normal document URL, but HTMX must use the shared fragment
  // response, update the active tab, and preserve the browser's investigation history.
  // Measure the actual click-to-visible transition in the browser. Driver
  // scheduling and assertion polling can otherwise dominate this budget.
  await page.getByRole("tab", { name: "Dependencies" }).evaluate(tab => {
    tab.addEventListener("click", () => {
      const start = performance.now();
      const recordWhenVisible = () => {
        const active = document.querySelector("#dashboard-tabs-container .tab-active");
        const visible = ["downstream-health_widgetEl", "dependency-regressions_widgetEl"].every(id => {
          const element = document.getElementById(id);
          return element && element.getClientRects().length > 0 && getComputedStyle(element).visibility === "visible";
        });
        if (active?.textContent?.includes("Dependencies") && visible) {
          performance.measure("endpoint-tab-switch", { start, end: performance.now() });
        } else {
          requestAnimationFrame(recordWhenVisible);
        }
      };
      requestAnimationFrame(recordWhenVisible);
    }, { once: true });
  });
  await page.getByRole("tab", { name: "Dependencies" }).click();
  await expect(page.getByText("Downstream health", { exact: true })).toBeVisible();
  await expect(page.getByText("Dependency Regressions", { exact: true })).toBeVisible();
  await expect(page.getByRole("tab", { name: "Dependencies" })).toHaveClass(/tab-active/);
  expect(page.url()).toMatch(/\/tab\/dependencies/);
  await expect.poll(() => page.evaluate(() => performance.getEntriesByName("endpoint-tab-switch").length)).toBe(1);
  const switchMs = await page.evaluate(() => performance.getEntriesByName("endpoint-tab-switch")[0].duration);
  console.info("endpoint_tab_switch_ms", switchMs);
  expect(switchMs).toBeLessThan(1_500);

  // Dependency rollups live in Postgres. This widget is lazy, so scroll it into
  // view and pin its actual browser request: losing db_source here silently
  // defaults SQL to TimeFusion, where the rollup table is not present.
  const downstreamRequest = page.waitForRequest(request => {
    const url = new URL(request.url());
    return url.pathname === "/chart_data/stream" && url.searchParams.get("query_sql")?.includes("endpoint_dependency_edges") === true;
  });
  await page.getByText("Downstream Time by Operation", { exact: true }).scrollIntoViewIfNeeded();
  const downstreamUrl = new URL((await downstreamRequest).url());
  expect(downstreamUrl.searchParams.get("db_source")).toBe("postgres");
  await expect(page.locator("#downstream-time-by-operation_error")).toBeHidden();

  await page.goBack();
  await expect(page.getByRole("tab", { name: "Experience" })).toHaveClass(/tab-active/);
  await expect(page.getByText("Real-user impact", { exact: true })).toBeVisible();
  expect(page.url()).toMatch(/\/tab\/experience/);
});
