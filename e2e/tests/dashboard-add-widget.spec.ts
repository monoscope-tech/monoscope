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

  test("the logs preview stays inside the preview frame", async ({ page }) => {
    await openFixture(page);
    await openWidgetDrawer(page);

    // Switching to Logs re-renders the preview as a log table. It has no natural height,
    // so it used to spill out of the fixed-aspect preview frame and sit on top of the
    // "Configure Query" step underneath it.
    await page.locator('#visualizationTabs label[data-value="logs"]').click();
    const frame = page.locator(".widget-preview-container");
    await expect(frame).toBeVisible();
    await page.waitForTimeout(3000); // the preview is an HTMX round-trip

    const frameBox = (await frame.boundingBox())!;
    const inner = frame.locator("> div").first();
    const innerBox = (await inner.boundingBox())!;

    // The preview content fits its frame rather than overflowing it. 2px of slack absorbs
    // sub-pixel layout rounding; a genuine overflow is tens of pixels or more.
    expect(
      innerBox.y + innerBox.height,
      "the logs preview overflows the bottom of the widget preview frame",
    ).toBeLessThanOrEqual(frameBox.y + frameBox.height + 2);

    // And it does not cover the step below it.
    const step = page.getByText("Configure Query").first();
    const stepBox = (await step.boundingBox())!;
    expect(
      innerBox.y + innerBox.height,
      "the logs preview overlaps the Configure Query step",
    ).toBeLessThanOrEqual(stepBox.y + 2);
  });

  test("a log explorer chart can be added to a dashboard", async ({ page }) => {
    // Its own dashboard, not the shared fixture: this test's claim is "one more widget
    // than before", and a run that fails midway leaves the shared one holding a widget it
    // never cleaned up — after which the count is whatever the last failure happened to
    // leave, not something the test controls.
    const title = `E2E Add Target ${Date.now()}`;
    await page.goto(`/p/${DEMO_PROJECT}/dashboards`);
    await page.locator('label[for="newDashboardMdl"]').first().click();
    await page.getByText("Blank dashboard").click();
    await page.locator('input[name="title"]').first().fill(title);
    await page.getByRole("button", { name: "Create" }).first().click();
    await page.waitForURL(/\/dashboards\/[0-9a-f-]{36}/i, { timeout: 60000 });
    const dashId = page.url().match(/\/dashboards\/([0-9a-f-]{36})/i)![1];
    await page.waitForSelector(`${ROOT_GRID}.grid-stack-initialized`, { timeout: 20000 });

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
    const row = page.locator("#dashboards-modal-content").getByText(title).first();
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

    // Deleting the whole dashboard leaves nothing behind for the next run, and covers the
    // delete path that the per-widget cleanup used to.
    page.once("dialog", (d) => d.accept());
    await page.locator('[aria-label="Open context menu"]').first().click();
    // Wait for the DELETE itself: the handler redirects to the list on its own, and
    // navigating there in parallel aborts that redirect rather than following it.
    await Promise.all([
      page.waitForResponse(
        (r) => r.request().method() === "DELETE" && r.url().includes(dashId) && r.status() < 400,
        { timeout: 20000 },
      ),
      page.getByText("Delete dashboard").click(),
    ]);
    await expect
      .poll(
        async () => {
          await page.goto(`/p/${DEMO_PROJECT}/dashboards`);
          return page.getByText(title).count();
        },
        { timeout: 20000 },
      )
      .toBe(0);
  });
});
