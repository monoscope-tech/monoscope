import { test, expect } from "@playwright/test";
import { DEMO_PROJECT, sql } from "./helpers";

const cleanup = `DELETE FROM apis.endpoints WHERE project_id='${DEMO_PROJECT}' AND hash IN ('e2e-browser-endpoint','e2e-browser-long');`;
test.beforeAll(() => sql(cleanup + `INSERT INTO apis.endpoints (project_id,url_path,url_params,method,host,hash,outgoing)
  VALUES ('${DEMO_PROJECT}','/e2e-browser','{}','GET','browser.example','e2e-browser-endpoint',false),
         ('${DEMO_PROJECT}','/e2e-browser/a/very/long/route/that/is/wider/than/the/variable/chip/{param}','{}','POST','browser.example','e2e-browser-long',false);`));
test.afterAll(() => sql(cleanup));

test("lazy endpoint widgets forward filters before the web component bundle loads", async ({ page }) => {
  const widgetRequests: URL[] = [];
  await page.route("**/web-components/dist/js/index.*.js", async route => {
    await new Promise(resolve => setTimeout(resolve, 3_000));
    await route.continue();
  });
  await page.route("**/widget?*", route => {
    widgetRequests.push(new URL(route.request().url()));
    return route.abort();
  });
  await page.goto(`/p/${DEMO_PROJECT}/endpoints/details?var-host=browser.example&var-endpointHash=e2e-browser-endpoint`);
  await expect.poll(() => widgetRequests.length).toBeGreaterThanOrEqual(2);
  expect(widgetRequests.every(url =>
    url.searchParams.get("const-endpointFilter")?.includes("browser.example") &&
    url.searchParams.get("var-endpointHash") === "e2e-browser-endpoint"
  )).toBe(true);
});

test("dashboard variables recover when Tagify fails to load", async ({ page }) => {
  let requests = 0;
  await page.route("**/public/assets/deps/tagify/*.js", route => {
    requests++;
    return requests <= 2 ? route.abort() : route.continue();
  });
  await page.goto(`/p/${DEMO_PROJECT}/endpoints/details?var-host=browser.example&var-endpointHash=e2e-browser-endpoint`);
  await expect.poll(() => requests).toBe(3);
  await expect(page.locator(".dash-variable .tagify")).toHaveCount(2);
});

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

// The variable chips sit at the toolbar's trailing edge; a dropdown wider than its chip
// used to open rightward past the viewport and widen the page.
test("variable dropdown opens inside the viewport", async ({ page }) => {
  await page.setViewportSize({ width: 1280, height: 800 });
  await page.goto(`/p/${DEMO_PROJECT}/endpoints/details?var-host=browser.example&var-endpointHash=e2e-browser-endpoint`);
  await page.waitForURL(new RegExp(`/p/${DEMO_PROJECT}/dashboards/[0-9a-f-]+`, "i"));
  await page.locator(".dash-variable .tagify").last().click();
  const dropdown = page.locator(".tagify__dropdown");
  await expect(dropdown.getByText("/e2e-browser/a/very/long", { exact: false })).toBeVisible();
  const box = (await dropdown.boundingBox())!;
  expect(box.x).toBeGreaterThanOrEqual(0);
  expect(box.x + box.width).toBeLessThanOrEqual(1280);
  expect(await page.evaluate(() => document.documentElement.scrollWidth)).toBeLessThanOrEqual(1280);
});
