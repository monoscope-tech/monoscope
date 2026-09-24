import { test, expect, Page } from "@playwright/test";
import { DEMO_PROJECT } from "./helpers";

// Covers getting a widget *onto* a dashboard, which dashboard-grid.spec.ts assumes has
// already happened. Three routes in:
//
//   1. the "Add a new widget" drawer on a dashboard,
//   2. "Copy to dashboard" from a widget that already lives on one,
//   3. "Add to dashboard" from the log explorer's chart, which belongs to no dashboard.
//
// (3) is the one that was broken: the dashboards list only becomes a picker when the
// request carries BOTH copy_widget_id and source_dashboard_id, and a log-explorer chart
// has no source dashboard. The modal opened onto an ordinary list, so clicking a row
// navigated to that dashboard instead of adding the widget — a dead end that looked like
// a working feature.

const ROOT_GRID = ".grid-stack:not(.nested-grid)";
const ROOT_ITEMS = `${ROOT_GRID} > .grid-stack-item`;

const FIXTURE = "E2E Add Widget Fixture";

/** Open this spec's own blank dashboard, creating it the first time. */
async function openFixture(page: Page): Promise<string> {
  await page.goto(`/p/${DEMO_PROJECT}/dashboards`);
  const links = page.locator(`a[href^="/p/${DEMO_PROJECT}/dashboards/"]`, { hasText: FIXTURE });
  if ((await links.count()) === 0) {
    await page.locator('label[for="newDashboardMdl"]').first().click();
    // Blank, not a template: these tests count widgets, and a template's own widgets
    // would make "did one get added?" a question about arithmetic rather than behaviour.
    await page.getByText("Blank dashboard").click();
    await page.locator('input[name="title"]').first().fill(FIXTURE);
    await page.getByRole("button", { name: "Create" }).first().click();
    await page.waitForURL(/\/dashboards\/[0-9a-f-]{36}/i, { timeout: 60000 });
  } else {
    await links.first().click();
    await page.waitForURL(/\/dashboards\/[0-9a-f-]{36}/i, { timeout: 20000 });
  }
  await page.waitForSelector(`${ROOT_GRID}.grid-stack-initialized`, { timeout: 20000 });
  return page.url().match(/\/dashboards\/([0-9a-f-]{36})/i)![1];
}

type Dash = { id: string; title: string };

/**
 * A blank dashboard of this run's own, deleted at the end of the test.
 *
 * Tests that count widgets cannot share one: a run that fails midway leaves widgets it
 * never cleaned up behind, after which "one more than before" measures the last failure
 * rather than anything the test did. Titles carry a timestamp so a leftover from a failed
 * run never collides with a live one.
 */
async function makeDashboard(page: Page, title: string, template = "Blank dashboard"): Promise<Dash> {
  await page.goto(`/p/${DEMO_PROJECT}/dashboards`);
  await page.locator('label[for="newDashboardMdl"]').first().click();
  await page.getByText(template, { exact: true }).click();
  await page.locator('input[name="title"]').first().fill(title);
  await page.getByRole("button", { name: "Create" }).first().click();
  await page.waitForURL(/\/dashboards\/[0-9a-f-]{36}/i, { timeout: 60000 });
  return { id: page.url().match(/\/dashboards\/([0-9a-f-]{36})/i)![1], title };
}

/** Delete a dashboard through the UI, which is also the delete path's only coverage. */
async function deleteDashboard(page: Page, dash: Dash) {
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

/** Open the "Add a new widget" drawer and wait for the editor to be interactive. */
async function openWidgetDrawer(page: Page) {
  await page.locator('[aria-label="Add a new widget"]').first().click();
  await expect(page.locator("#visualizationTabs")).toBeVisible({ timeout: 20000 });
}

test.describe("adding widgets to a dashboard", () => {
  // Every test here writes to one shared dashboard.
  test.describe.configure({ mode: "serial" });

  test("the new-widget drawer leads with a chart, not the log table", async ({ page }) => {
    await openFixture(page);
    await openWidgetDrawer(page);

    const tabs = page.locator("#visualizationTabs label[data-value]");
    await expect(tabs.first()).toBeVisible();
    const drawer = page.locator("#page-data-drawer-panel");
    await expect(drawer.locator('[aria-label="Close drawer"]:visible')).toHaveCount(1);

    await expect.poll(async () => {
      const box = await drawer.boundingBox();
      return box ? Math.abs(box.x + box.width - page.viewportSize()!.width) : Infinity;
    }, { message: "the widget drawer must settle at the right viewport edge" }).toBeLessThanOrEqual(1);

    // A dashboard widget is a chart. Logs is a full log table — the most expensive thing
    // on a dashboard and the wrong thing to offer first — so it must not lead the strip.
    const order = await tabs.evaluateAll((els) => els.map((e) => (e as HTMLElement).dataset.value));
    expect(order[0], `viz tabs led with "${order[0]}"`).not.toBe("logs");
    expect(order, "the chart types must come before logs").toEqual(
      [...order].sort((a, b) => Number(a === "logs") - Number(b === "logs")),
    );

    // ...and Logs is still offered, just not first.
    expect(order).toContain("logs");

    // The preselected tab is a chart too, so the drawer opens on a chart preview.
    const checked = await page
      .locator("#visualizationTabs input[type=radio]:checked")
      .getAttribute("value");
    expect(checked).not.toBe("logs");
  });

  test("AI search preserves the widget visualization and programmatic changes stay in sync", async ({ page }) => {
    await page.route("**/log_explorer/ai_search", (route) =>
      route.fulfill({
        contentType: "application/json",
        body: JSON.stringify({ query: 'level == "ERROR"', visualization_type: "timeseries" }),
      }),
    );
    await openFixture(page);
    await openWidgetDrawer(page);

    const tabs = page.locator("#visualizationTabs").last();
    await tabs.locator('label[data-value="logs"]').click();
    await expect.poll(() => page.evaluate(() => (window as any).widgetJSON.type)).toBe("logs");
    await page.locator("#ai-search-input").fill("level is error");
    await page.locator("#ai-search-input").press("Enter");
    await expect
      .poll(() => page.locator("#filterElement").last().evaluate((el: any) => el.getValue?.()))
      .toBe('level == "ERROR"');

    await expect(tabs.locator('input[value="logs"]')).toBeChecked();
    expect(await page.evaluate(() => (window as any).widgetJSON.type)).toBe("logs");

    const previewId = await page.locator("#ai-search-input").getAttribute("data-container-id");
    await page.evaluate((id) => {
      const decoy = document.createElement("div");
      decoy.id = "visualizationTabs";
      decoy.innerHTML = '<input type="radio" value="timeseries">';
      document.body.prepend(decoy);
      (window as any).handleVisualizationUpdate("timeseries", id);
    }, previewId);

    await expect(tabs.locator('input[value="timeseries"]')).toBeChecked();
    expect(await page.evaluate(() => (window as any).widgetJSON.type)).toBe("timeseries");
  });

  test("the logs preview stays inside the preview frame", async ({ page }) => {
    await openFixture(page);
    await openWidgetDrawer(page);

    // Switching to Logs re-renders the preview as a log table. It has no natural height,
    // so it used to spill out of the bounded preview frame and cover the editor.
    await page.locator('#visualizationTabs label[data-value="logs"]').click();
    const frame = page.locator(".widget-preview-container");
    await expect(frame).toBeVisible();
    await page.waitForTimeout(3000); // the preview is an HTMX round-trip

    // Measure the frame's own scroll extent, not a child's box: the preview div is
    // `h-full`, so its *box* always matches the frame no matter how far its contents
    // spill. scrollHeight is what actually reveals the overflow.
    const fit = await frame.evaluate((e) => ({
      overflowBy: e.scrollHeight - e.clientHeight,
      clipped: getComputedStyle(e).overflowY !== "visible",
    }));

    // A few pixels are sub-pixel layout rounding; the bug was ~260px of table hanging out
    // of the box and painting over the numbered steps underneath.
    expect(fit.overflowBy, "the logs preview does not fit the widget preview frame").toBeLessThanOrEqual(4);
    // Belt and braces: even if a future log table grows again, the frame must clip rather
    // than paint over the rest of the form.
    expect(fit.clipped, "the preview frame lets its contents escape").toBe(true);

    // And the frame really did get taller for logs than it is for a chart.
    const frameBox = (await frame.boundingBox())!;
    await page.locator('#visualizationTabs label[data-value="timeseries"]').click();
    await page.waitForTimeout(1500);
    const chartBox = (await frame.boundingBox())!;
    expect(frameBox.height, "logs and charts get the same preview height").toBeGreaterThan(chartBox.height);
  });

  test("a log explorer chart can be added to a dashboard", async ({ page }) => {
    const dash = await makeDashboard(page, `E2E Add Target ${Date.now()}`);
    const dashId = dash.id;

    await page.goto(`/p/${DEMO_PROJECT}/log_explorer`);
    // The chart only renders for a non-logs viz; the explorer opens on Logs.
    await page.locator('#visualizationTabs label[data-value="timeseries"]').click();
    const chart = page.locator("#visualization-widget-container");
    await expect(chart).toBeVisible({ timeout: 20000 });

    await chart.locator('button[aria-label="Widget menu"]').first().click();
    // The label says "Add" here rather than "Copy": this chart lives on no dashboard, so
    // there is nothing to copy it from.
    const addItem = chart.getByText(/Add to dashboard/i);
    await expect(addItem, "the log explorer chart offers no way onto a dashboard").toBeVisible();
    await addItem.click();

    // The modal must open as a *picker*, not as the ordinary dashboards list.
    const row = page.locator("#dashboards-modal-content").getByText(dash.title).first();
    await expect(row).toBeVisible({ timeout: 20000 });
    await expect(
      page.locator("#dashboards-modal-content").getByText("Select a dashboard"),
      "the modal opened as a plain list, so picking a dashboard just navigates away",
    ).toBeVisible();

    await Promise.all([
      // The upsert carries no widget_id, which is what makes it append under a fresh id
      // instead of overwriting whatever already answers to "visualization-widget".
      page.waitForResponse(
        (r) =>
          r.request().method() === "PUT" &&
          new URL(r.url()).pathname.endsWith(`/dashboards/${dashId}`) &&
          r.status() < 400,
        { timeout: 20000 },
      ),
      row.click(),
    ]);

    // It was persisted, not just appended client-side.
    await page.goto(`/p/${DEMO_PROJECT}/dashboards/${dashId}`);
    await page.waitForSelector(`${ROOT_GRID}.grid-stack-initialized`, { timeout: 20000 });
    // Assert on the widget itself rather than on a count: an empty dashboard renders a
    // placeholder grid item that goes away once it holds a real widget, so "one more than
    // before" is 1 → 1 and says nothing either way.
    //
    // The checks below are what distinguishes a persisted dashboard widget from the
    // standalone explorer chart: the upsert strips `standalone` and assigns an id of its
    // own, which is what lets gridstack adopt it and the menu offer Duplicate/Delete.
    await expect
      .poll(
        () =>
          page.$$eval(ROOT_ITEMS, (els) =>
            els
              .map((e) => {
                try {
                  return JSON.parse((e as HTMLElement).dataset.widget!);
                } catch {
                  return null;
                }
              })
              .filter((w) => w?.title === "Visualization"),
          ),
        { timeout: 20000 },
      )
      .toHaveLength(1);

    const w = (
      await page.$$eval(ROOT_ITEMS, (els) =>
        els.map((e) => JSON.parse((e as HTMLElement).dataset.widget!)),
      )
    ).find((x: any) => x.title === "Visualization");
    expect(w.standalone ?? null).toBeNull();
    expect(w.id).not.toBe("visualization-widget");
    expect(w.type).toBe("timeseries");
    expect(w._dashboard_id).toBe(dashId);

    await deleteDashboard(page, dash);
  });

  test("a widget is saved from the drawer, then copied to another dashboard", async ({ page }) => {
    // The other half of the picker: a widget that already lives on a dashboard is copied
    // across by id rather than sent as JSON. Both branches render from the same rowAttrs,
    // so this is what stops a change to one from silently breaking the other.
    const source = await makeDashboard(page, `E2E Copy Source ${Date.now()}`);
    const target = await makeDashboard(page, `E2E Copy Target ${Date.now()}`);

    // Save a widget out of the "Add a new widget" drawer — the drawer's own write path,
    // which nothing else covers.
    await page.goto(`/p/${DEMO_PROJECT}/dashboards/${source.id}`);
    await page.waitForSelector(`${ROOT_GRID}.grid-stack-initialized`, { timeout: 20000 });
    await openWidgetDrawer(page);
    const widgetTitle = "Copied Widget";
    await page.locator('input[placeholder="Throughput"]').fill(widgetTitle);
    await Promise.all([
      page.waitForResponse(
        (r) => r.request().method() === "PUT" && r.url().includes(source.id) && r.status() < 400,
        { timeout: 20000 },
      ),
      page.getByRole("button", { name: "Add widget" }).first().click(),
    ]);

    // The PUT returns a complete grid item. It must be adopted directly rather than
    // inserted as another item's content; the latter leaves a visibly collapsed card
    // until refresh even though persistence succeeded.
    let saved = page.locator(ROOT_ITEMS).filter({ hasText: widgetTitle }).first();
    await expect(saved, "the drawer's Add widget action did not persist the widget").toBeVisible({
      timeout: 20000,
    });
    const inserted = await saved.evaluate((el) => {
      const gridItem = el as HTMLElement & { gridstackNode?: { h?: number } };
      return {
        height: gridItem.getBoundingClientRect().height,
        rows: gridItem.gridstackNode?.h,
        nestedItems: gridItem.querySelectorAll(":scope > .grid-stack-item-content > .grid-stack-item").length,
      };
    });
    expect(inserted.rows).toBe(3);
    expect(inserted.height, "the newly inserted widget is collapsed").toBeGreaterThan(150);
    expect(inserted.nestedItems, "the response was nested inside a second grid item").toBe(0);

    // A reload still proves the same widget was persisted, rather than only inserted in
    // the live grid by the response handler.
    await page.reload();
    await page.waitForSelector(`${ROOT_GRID}.grid-stack-initialized`, { timeout: 20000 });
    saved = page.locator(ROOT_ITEMS).filter({ hasText: widgetTitle }).first();
    await expect(saved).toBeVisible({ timeout: 20000 });

    // Now copy it across. This widget *is* on a dashboard, so the menu offers "Copy".
    await saved.locator('button[aria-label="Widget menu"]').first().click();
    const copyItem = saved.getByText("Copy to dashboard");
    await expect(copyItem).toBeVisible();
    await copyItem.click();

    const row = page.locator("#dashboards-modal-content").getByText(target.title).first();
    await expect(row).toBeVisible({ timeout: 20000 });
    await Promise.all([
      page.waitForResponse(
        (r) =>
          r.url().includes(`/dashboards/${target.id}/widgets/`) &&
          r.url().includes("/duplicate") &&
          r.url().includes(`source_dashboard_id=${source.id}`) &&
          r.status() < 400,
        { timeout: 20000 },
      ),
      row.click(),
    ]);

    // It landed on the target and stayed on the source — a copy, not a move.
    await page.goto(`/p/${DEMO_PROJECT}/dashboards/${target.id}`);
    await page.waitForSelector(`${ROOT_GRID}.grid-stack-initialized`, { timeout: 20000 });
    await expect(page.locator(ROOT_ITEMS).filter({ hasText: widgetTitle })).toHaveCount(1, {
      timeout: 20000,
    });
    await page.goto(`/p/${DEMO_PROJECT}/dashboards/${source.id}`);
    await page.waitForSelector(`${ROOT_GRID}.grid-stack-initialized`, { timeout: 20000 });
    await expect(page.locator(ROOT_ITEMS).filter({ hasText: widgetTitle })).toHaveCount(1, {
      timeout: 20000,
    });

    await deleteDashboard(page, source);
    await deleteDashboard(page, target);
  });
});

test("a dashboard needing a variable asks for it instead of covering itself", async ({ page }) => {
  // Endpoint Analytics reports on one endpoint at a time, so its `host` variable is
  // genuinely required — there is no all-endpoints rendering of it. What was wrong was
  // the shape of the ask: a full-screen picker over a grid whose widgets had all run
  // with the variable interpolated to '' and were reporting "no data in the selected
  // time range". Dismissing it left that as the entire page.
  const title = `E2E Variable Prompt ${Date.now()}`;
  await page.goto(`/p/${DEMO_PROJECT}/dashboards`);
  await page.locator('label[for="newDashboardMdl"]').first().click();
  await page.getByText("Endpoint Analytics").first().click();
  await page.locator('input[name="title"]').first().fill(title);
  await page.getByRole("button", { name: "Create" }).first().click();
  await page.waitForURL(/\/dashboards\/[0-9a-f-]{36}/i, { timeout: 60000 });
  const dash = {
    id: page.url().match(/\/dashboards\/([0-9a-f-]{36})/i)![1],
    title,
  };

  // The ask is the page, not something on top of it.
  await expect(page.locator(".var-picker-backdrop")).toHaveCount(0);
  await expect(page.locator(".var-picker-page, .var-picker-none").first()).toBeVisible({
    timeout: 20000,
  });
  await expect(page.getByText(/Select Domain/i).first()).toBeVisible();

  // And no widget ran. This is the half that made the old page a lie: a chart that
  // says "no data" is a claim about the data, not a prompt.
  await expect(page.locator(ROOT_ITEMS)).toHaveCount(0);
  await expect(page.getByText("No events match in the selected time range")).toHaveCount(0);

  await deleteDashboard(page, dash);
});


test("dashboard bulk assignment asks for a team before saving", async ({ page }) => {
  const title = `E2E Teams ${Date.now()}`;
  for (const suffix of ["A", "B"]) await makeDashboard(page, `${title} ${suffix}`);
  const team = `e2e-team-${Date.now()}`;
  await page.goto(`/p/${DEMO_PROJECT}/manage_teams`);
  await page.getByText("New Team", { exact: true }).click();
  await page.locator('[name="teamName"]').fill(team);
  await page.locator('[name="teamHandle"]').fill(team);
  await page.getByRole("button", { name: "Create Team", exact: true }).click();
  await expect(page.getByRole("link", { name: team, exact: true })).toBeVisible();
  await page.goto(`/p/${DEMO_PROJECT}/dashboards`);
  const rows = page.getByRole("row").filter({ has: page.getByRole("link", { name: title }) });
  await expect(rows).toHaveCount(2);
  for (const checkbox of await rows.getByRole("checkbox").all()) await checkbox.check();
  await page.getByRole("button", { name: "Add teams", exact: true }).click();
  const picker = page.getByRole("group", { name: "Teams", exact: true });
  await page.getByRole("button", { name: "Apply teams", exact: true }).click();
  await expect(page.getByText("Select at least one team", { exact: true })).toBeVisible();
  await picker.getByRole("checkbox", { name: team, exact: true }).check();
  await page.getByRole("button", { name: "Apply teams", exact: true }).click();
  for (const row of await rows.all()) await expect(row.getByText(team, { exact: true }).filter({ visible: true })).toBeVisible();
  await page.reload();
  for (const row of await rows.all()) await expect(row.getByText(team, { exact: true }).filter({ visible: true })).toBeVisible();
});

async function openWidgetMonitor(page: Page, dash: Dash) {
  await page.goto(`/p/${DEMO_PROJECT}/dashboards/${dash.id}`);
  await page.getByRole("button", { name: "Expand widget", exact: true }).first().click();
  await page.getByRole("tab", { name: "Monitors", exact: true }).click();
  return page.locator('form[id$="-alert-form"]');
}

test("widget monitors persist independently across dashboards and reopen with saved settings", async ({ page }) => {
  const dashboards: Dash[] = [];
  for (const suffix of ["A", "B"]) {
    const title = `E2E Widget Monitor ${Date.now()} ${suffix}`;
    const dash = await makeDashboard(page, title, "Apache HTTP Server");
    dashboards.push(dash);
    const form = await openWidgetMonitor(page, dash);
    await page.getByRole("checkbox", { name: /Enable Alert/ }).check();
    await form.locator('[name="title"]').fill(title);
    await form.locator('[name="alertThreshold"]').fill("10");
    await form.getByText("Notification Settings", { exact: true }).click();
    await form.locator('[name="notifyAfter"]').selectOption("10m");
    await form.getByRole("button", { name: "Create monitor", exact: true }).click();
    await expect(page.getByText("Widget monitor configured successfully")).toBeVisible();
  }
  await page.goto(`/p/${DEMO_PROJECT}/monitors`);
  for (const dash of dashboards) await expect(page.getByText(dash.title, { exact: true })).toBeVisible();
  const form = await openWidgetMonitor(page, dashboards[0]);
  await expect(page.getByRole("checkbox", { name: /Enable Alert/ })).toBeChecked();
  await expect(form.getByRole("button", { name: "Update monitor", exact: true })).toBeVisible();
  await expect(form.locator('[name="title"]')).toHaveValue(dashboards[0].title);
  await expect(form.locator('[name="alertThreshold"]')).toHaveValue("10.0");
  await expect(form.locator('[name="notifyAfterCheck"]')).toBeChecked();
  await expect(form.locator('[name="notifyAfter"]')).toHaveValue("10m");
  await form.locator('[name="alertThreshold"]').fill("15");
  await form.getByRole("button", { name: "Update monitor", exact: true }).click();
  await expect(page.getByText("Widget monitor configured successfully")).toBeVisible();
  await openWidgetMonitor(page, dashboards[0]);
  await expect(form.locator('[name="alertThreshold"]')).toHaveValue("15.0");
  await form.getByRole("button", { name: "Remove monitor", exact: true }).click();
  await expect(page.getByText("Monitor removed from widget")).toBeVisible();
  await openWidgetMonitor(page, dashboards[0]);
  await expect(page.getByRole("checkbox", { name: /Enable Alert/ })).not.toBeChecked();
  await openWidgetMonitor(page, dashboards[1]);
  await expect(form.locator('[name="alertThreshold"]')).toHaveValue("10.0");
  await expect(form.getByRole("button", { name: "Update monitor", exact: true })).toBeVisible();
});
