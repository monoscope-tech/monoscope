import { test, expect, Page } from "@playwright/test";
import { DEMO_PROJECT } from "./helpers";

// Every widget type, on a real canvas.
//
// DashboardWidgetsSpec already pins the server side: all 19 types round-trip through the
// database and render without throwing. What it cannot see is the half that happens in the
// browser — whether the element GridStack is handed actually gets adopted. A widget that
// decodes, renders, and is then dropped by the grid looks perfect in an integration test
// and is invisible on the page, which is exactly the failure this covers.
//
// The types come from WidgetType in Pkg/Components/Widget.hs (constructor minus `WT`,
// camelCase to snake_case). Keep them in step: that enum is `Bounded`/`Enum` precisely so
// adding a constructor extends the server specs automatically, and this list is the one
// place the browser side has to be told.
const WIDGET_TYPES = [
  "timeseries", "timeseries_line", "timeseries_stat", "stat", "list", "top_list",
  "distribution", "geomap", "funnel", "tree_map", "pie_chart", "anomalies", "table",
  "traces", "flamegraph", "service_map", "heatmap", "logs",
] as const;

const QUERY = "summarize count() by bin_auto(timestamp)";

/** One dashboard carrying every type: the flat ones, plus a group proving nesting works. */
const everyWidgetYaml = () =>
  [
    "title: All Widget Types",
    "widgets:",
    ...WIDGET_TYPES.flatMap((type, i) => [
      `  - type: '${type}'`,
      `    title: 'W ${type}'`,
      `    query: |`,
      `      ${QUERY}`,
      `    layout: { x: ${(i % 4) * 3}, y: ${Math.floor(i / 4) * 2}, w: 3, h: 2 }`,
    ]),
    "  - type: 'group'",
    "    title: 'W group'",
    "    layout: { x: 0, y: 24, w: 12, h: 4 }",
    "    children:",
    "      - type: 'stat'",
    "        title: 'W nested'",
    `        query: |`,
    `          ${QUERY}`,
    "        layout: { w: 3, h: 2 }",
  ].join("\n");

/** Types of every widget on the canvas, root and nested alike. */
const renderedTypes = (page: Page) =>
  page.$$eval(".grid-stack-item", els =>
    els.map((e: any) => { try { return JSON.parse(e.dataset.widget).type; } catch { return null; } })
       .filter(Boolean),
  ) as Promise<string[]>;

/** Widgets the grid never adopted — present in the DOM but with no gridstackNode. */
const unadopted = (page: Page) =>
  page.$$eval(".grid-stack-item", els =>
    els.filter((e: any) => !e.gridstackNode)
       .map((e: any) => { try { return JSON.parse(e.dataset.widget).type; } catch { return e.id; } }),
  ) as Promise<string[]>;

async function openDashboard(page: Page, title: string) {
  await page.goto(`/p/${DEMO_PROJECT}/dashboards`);
  const link = page.locator(`a[href^="/p/${DEMO_PROJECT}/dashboards/"]`, { hasText: title });
  if ((await link.count()) === 0) {
    await page.locator('label[for="newDashboardMdl"]').first().click();
    await page.getByText("Blank dashboard").click();
    await page.locator('input[name="title"]').fill(title);
    await page.getByRole("button", { name: "Create" }).click();
    await page.waitForURL(/\/dashboards\/[0-9a-f-]{36}/i, { timeout: 60000 });
  } else {
    await link.first().click();
  }
  await dismissVariablePickers(page);
  await page.waitForSelector(".grid-stack:not(.nested-grid).grid-stack-initialized", { timeout: 20000 });
}

async function dismissVariablePickers(page: Page) {
  const backdrops = page.locator(".var-picker-backdrop");
  for (let i = 0; i < 6 && (await backdrops.count()) > 0; i++) {
    await page.mouse.click(5, 5);
    await page.waitForTimeout(250);
  }
}

test.describe.configure({ mode: "serial" });

test.describe("every widget type on a dashboard canvas", () => {
  test("saving a dashboard of all widget types renders and adopts each one", async ({ page }) => {
    test.setTimeout(120000);
    await openDashboard(page, "All Widget Types");

    // The YAML drawer is the supported way to author a whole dashboard at once, and it is
    // the only route that can place types the add-widget picker deliberately does not
    // offer (group, traces, flamegraph, and logs — which is hidden there on purpose).
    // The drawer is a DaisyUI checkbox drawer whose only labels live inside a dropdown
    // menu. Toggling the checkbox is the same state change without driving menu chrome
    // this spec is not about; the editor still loads through its own hx-trigger.
    await page.evaluate(() => {
      const t = document.getElementById("yaml-editor-drawer") as HTMLInputElement;
      t.checked = true;
      t.dispatchEvent(new Event("change", { bubbles: true }));
    });
    // Monaco is deferred until the user reaches for the editor (see deferredComponents in
    // index.ts), so opening the drawer alone leaves <yaml-editor> inert and window.yamlEditor
    // undefined. Reach for it the way a click would.
    await page.locator("yaml-editor").waitFor({ state: "attached", timeout: 30000 });
    await page.evaluate(() => {
      const el = document.querySelector("yaml-editor")!;
      el.dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
      el.dispatchEvent(new FocusEvent("focusin", { bubbles: true }));
    });
    await expect.poll(() => page.evaluate(() => !!(window as any).yamlEditor), { timeout: 30000 }).toBe(true);
    await page.evaluate(y => (window as any).yamlEditor.setValue(y), everyWidgetYaml());
    // The save answers 200 with an empty body (it targets #yaml-status, which stays blank
    // on success), so the response itself is the only signal worth waiting on.
    const saved = page.waitForResponse(
      r => r.request().method() === "PUT" && r.url().includes("/yaml"),
      { timeout: 30000 },
    );
    await page.getByRole("button", { name: "Save Changes" }).click();
    expect((await saved).status()).toBe(200);

    // Saving kicks off its own client-side navigation, so reload() races it and aborts.
    // Let that settle, then go to the dashboard by URL.
    const dashUrl = page.url();
    await page.waitForTimeout(1500);
    await page.goto(dashUrl);
    await dismissVariablePickers(page);
    await page.waitForSelector(".grid-stack:not(.nested-grid).grid-stack-initialized", { timeout: 20000 });
    await page.waitForTimeout(2500); // widgets stream their charts in after init

    const present = await renderedTypes(page);
    expect([...WIDGET_TYPES, "group"].filter(t => !present.includes(t))).toEqual([]);
    expect(await unadopted(page)).toEqual([]);
  });

  test("the canvas survives a refresh unchanged", async ({ page }) => {
    // The layout round-trips through the widget-order PATCH and is rebuilt server-side, so
    // a reload is where a persistence bug shows up rather than in the first render.
    await openDashboard(page, "All Widget Types");
    await page.waitForTimeout(2000);
    const before = await page.$$eval(".grid-stack:not(.nested-grid) > .grid-stack-item", els =>
      els.map((e: any) => ({ id: e.id, ...(({ x, y, w, h }) => ({ x, y, w, h }))(e.gridstackNode ?? {}) })),
    );
    expect(before.length).toBeGreaterThan(0);

    await page.reload();
    await dismissVariablePickers(page);
    await page.waitForSelector(".grid-stack:not(.nested-grid).grid-stack-initialized", { timeout: 20000 });
    await page.waitForTimeout(2000);
    const after = await page.$$eval(".grid-stack:not(.nested-grid) > .grid-stack-item", els =>
      els.map((e: any) => ({ id: e.id, ...(({ x, y, w, h }) => ({ x, y, w, h }))(e.gridstackNode ?? {}) })),
    );

    expect(after).toEqual(before);
  });

  test("a failed chart shows a local error and a successful refresh clears it", async ({ page }) => {
    await openDashboard(page, "All Widget Types");
    const chart = page.locator("[data-chart-widget]").first();
    await chart.scrollIntoViewIfNeeded();
    const chartId = await chart.getAttribute("id");
    expect(chartId).toBeTruthy();
    const banner = page.locator(`[id="${chartId}_error"]`);

    await page.route("**/chart_data/stream?**", route => route.fulfill({
      contentType: "application/x-ndjson",
      body: JSON.stringify({ type: "error", error: "deterministic backend failure" }) + "\n",
    }));
    await page.evaluate(() => window.dispatchEvent(new CustomEvent("update-query")));
    await expect(banner).toBeVisible();
    await expect(banner).toContainText("deterministic backend failure");

    await page.unroute("**/chart_data/stream?**");
    await page.evaluate(() => window.dispatchEvent(new CustomEvent("update-query")));
    await expect(banner).toBeHidden();
  });
});
