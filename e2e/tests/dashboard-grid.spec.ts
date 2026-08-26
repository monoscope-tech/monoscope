import { test, expect, Page, Locator } from "@playwright/test";
import { DEMO_PROJECT } from "./helpers";

// Covers the gridstack integration on dashboards: initialisation/adoption, drag,
// resize, and "Duplicate widget". Gridstack is vendored as a prebuilt bundle under
// static/public/assets/deps/gridstack, so nothing — no dependency resolution, no type
// check — stands between a version bump and production. This spec is what catches an
// API that changed shape.
//
// Assertions read position/size off `el.gridstackNode` rather than pixel geometry:
// gridstack owns the column maths, and all we need to know is that it still registers,
// moves, resizes and adopts the elements we hand it.
//
// Every interaction here is persisted by the app (drag/resize debounce into a
// widget-order PATCH; duplicate writes a widget), so the mutating tests restore what
// they touched — otherwise each run leaves the demo dashboard more scrambled.

// Backend/data failures are the server's problem and are surfaced by the page itself;
// this spec is about the client-side grid. Same split as assets.spec.ts.
const TRANSIENT = /Query execution failed|Failed to load resource|net::ERR_/;

/** gridstack's removed-API shim logs this rather than throwing — see addWidget in the bundle. */
const V11_DEPRECATION = /does not support HTMLElement anymore/;

// `.grid-stack` also matches the nested group grids, so anything meaning "the page's
// main grid" has to exclude them explicitly.
const ROOT_GRID = ".grid-stack:not(.nested-grid)";
const ROOT_ITEMS = `${ROOT_GRID} > .grid-stack-item`;

type Layout = { id: string; x: number; y: number; w: number; h: number };

test("a dashboard can be created without assigning a team", async ({ page }) => {
  // The team picker is optional, and an empty one posts `teams=` — a single empty value.
  // Decoding that into [UUID] failed, so the whole form 400'd with a raw "Bad Request"
  // page rather than the toast the handler raises for other invalid input. Creating a
  // dashboard is the entry point to everything else in this spec.
  const responses: number[] = [];
  page.on("response", r => { if (r.request().method() === "POST") responses.push(r.status()); });

  await page.goto(`/p/${DEMO_PROJECT}/dashboards`);
  await page.locator('label[for="newDashboardMdl"]').first().click();
  await page.getByText("Blank dashboard").click();
  await page.locator('input[name="title"]').first().fill("No Team Dashboard");
  await page.getByRole("button", { name: "Create" }).first().click();
  await page.waitForURL(/\/dashboards\/[0-9a-f-]{36}/i, { timeout: 20000 });
  expect(responses).not.toContain(400);
  await page.goto(`/p/${DEMO_PROJECT}/dashboards`);
  await expect(page.getByText("No Team Dashboard").first()).toBeVisible();
});

const FIXTURE = "E2E Grid Fixture";

/**
 * Open this spec's own dashboard, creating it the first time. Ids are per-database, so
 * never hardcode one.
 *
 * It is addressed by title rather than "the first dashboard in the list": the project also
 * holds dashboards other tests create, and opening whichever sorted first meant this spec's
 * assertions ran against someone else's canvas. It also created nothing itself, so it only
 * passed on a database an earlier run had already populated — the opposite of reproducible.
 */
async function openFirstDashboard(page: Page) {
  await page.goto(`/p/${DEMO_PROJECT}/dashboards`);

  const links = page.locator(`a[href^="/p/${DEMO_PROJECT}/dashboards/"]`, { hasText: FIXTURE });
  if ((await links.count()) === 0) {
    await page.locator('label[for="newDashboardMdl"]').first().click();
    // A template, not "Blank dashboard": this spec is about the grid adopting widgets, and
    // the nested-group case needs a canvas that actually contains a group widget. Apache
    // ships one and — unlike Endpoint Analytics — declares no variables, so opening it
    // never raises the picker that blocks grid initialisation.
    await page.getByText("Apache HTTP Server").first().click();
    // `title` is required; without it HTML validation blocks the submit and HTMX never
    // fires, so the click looks like it worked and nothing is created.
    await page.locator('input[name="title"]').fill(FIXTURE);
    await page.getByRole("button", { name: "Create" }).click();
    // The handler redirects to the new dashboard. Wait for that rather than polling the
    // list — navigating while the redirect is in flight aborts it (net::ERR_ABORTED).
    await page.waitForURL(/\/dashboards\/[0-9a-f-]{36}/i, { timeout: 60000 });
    await page.goto(`/p/${DEMO_PROJECT}/dashboards`);
  }

  await links.first().click();

  // An unanswered required variable used to open a full-screen picker over the grid,
  // and this spec dismissed it by clicking the backdrop. It is now rendered as the tab's
  // content instead, so there is nothing to dismiss — and nothing covers the grid. The
  // fixture this spec opens declares no variables, so it never prompts; asserting the
  // backdrop is gone keeps that true if someone reintroduces the overlay.
  await expect(page.locator(".var-picker-backdrop")).toHaveCount(0);

  // The detail route redirects to /tab/<slug>; a DOMContentLoaded handler adds this
  // class once GridStack.init returns.
  await page.waitForSelector(`${ROOT_GRID}.grid-stack-initialized`, { timeout: 15000 });

  // Widgets stream their charts in after init and the grid reflows as they land.
  // Measuring a drag handle before that settles yields a stale box and the gesture
  // misses. The resize handles are injected by gridstack once it owns the items.
  await page.locator(".ui-resizable-se").first().waitFor({ state: "attached", timeout: 15000 });
  await page.waitForTimeout(2500);

  // Nothing re-opened a picker while the widgets were loading.
  await expect(page.locator(".var-picker-backdrop")).toHaveCount(0);
}

/** Live gridstack position/size for every root widget. */
const snapshot = (page: Page): Promise<Layout[]> =>
  page.$$eval(ROOT_ITEMS, (els) =>
    els.map((e: any) => ({ id: e.id, ...(({ x, y, w, h }) => ({ x, y, w, h }))(e.gridstackNode ?? {}) })),
  ) as Promise<Layout[]>;

/** Live gridstack node for one element, or null when it was never adopted. */
const layoutOf = (el: Locator) =>
  el.evaluate((e: any) => {
    const n = e.gridstackNode;
    return n ? { x: n.x, y: n.y, w: n.w, h: n.h } : null;
  });

/** Locate a widget by element id. Attribute form so no CSS escaping is needed in Node. */
const byId = (page: Page, id: string) => page.locator(`[id="${id}"]`);

/**
 * Put every widget back where it started. float:false means one drag displaces
 * neighbours too, so restoring only the widget under test is not enough.
 */
async function restore(page: Page, layout: Layout[]) {
  await page.evaluate(
    ([sel, items]) => {
      const grid = (document.querySelector(sel as string) as any)?.gridstack;
      if (!grid) return;
      grid.batchUpdate();
      for (const i of items as any[]) {
        const el = document.getElementById(i.id);
        if (el) grid.update(el, { x: i.x, y: i.y, w: i.w, h: i.h });
      }
      grid.batchUpdate(false);
    },
    [ROOT_GRID, layout] as const,
  );
  await page.waitForTimeout(800); // let the debounced widget-order PATCH go out
}

/**
 * First root widget that is not a group. Groups own a nested sub-grid, have no widget
 * menu, and auto-fit their height to their children — which would fight a resize.
 */
async function plainWidget(page: Page): Promise<Locator> {
  const idx = await page.$$eval(ROOT_ITEMS, (els) =>
    els.findIndex((e: any) => {
      try {
        return JSON.parse(e.dataset.widget).type !== "group";
      } catch {
        return false;
      }
    }),
  );
  expect(idx, "dashboard has no non-group widget to exercise").toBeGreaterThanOrEqual(0);
  return page.locator(ROOT_ITEMS).nth(idx);
}

async function chartWidget(page: Page): Promise<Locator> {
  const idx = await page.$$eval(ROOT_ITEMS, (els) =>
    els.findIndex((e: any) => {
      try {
        return JSON.parse(e.dataset.widget).type !== "group" && !!e.querySelector("[data-chart-widget]");
      } catch {
        return false;
      }
    }),
  );
  expect(idx, "dashboard has no chart widget to resize").toBeGreaterThanOrEqual(0);
  return page.locator(ROOT_ITEMS).nth(idx);
}

/**
 * Gridstack implements its own drag on raw mouse events, so Playwright's dragTo()
 * (HTML5 DnD) does not drive it. Move in steps — a single jump can be discarded by the
 * drag-threshold logic.
 */
async function dragBy(page: Page, handle: Locator, dx: number, dy: number) {
  const b = await handle.boundingBox();
  if (!b) throw new Error("drag handle has no box (not visible?)");
  const x = b.x + b.width / 2;
  const y = b.y + b.height / 2;
  await page.mouse.move(x, y);
  await page.mouse.down();
  for (let i = 1; i <= 10; i++) {
    await page.mouse.move(x + (dx * i) / 10, y + (dy * i) / 10, { steps: 2 });
  }
  await page.mouse.up();
}

test.describe("dashboard gridstack", () => {
  // These share one dashboard and three of them write to it, so they must not
  // interleave (the project config is fullyParallel).
  test.describe.configure({ mode: "serial" });

  test("adopts root and nested widgets without client errors", async ({ page }) => {
    const errors: string[] = [];
    page.on("pageerror", (e) => errors.push(String(e)));
    page.on("console", (m) => m.type() === "error" && errors.push(m.text()));

    await openFirstDashboard(page);
    await page.waitForTimeout(2000); // lazy chunks import on htmx:afterSettle

    const unadopted = await page.$$eval(`${ROOT_ITEMS}, .nested-grid > .grid-stack-item`, (els) =>
      els.filter((e: any) => !e.gridstackNode).map((e) => e.id),
    );
    expect(unadopted, `items with no gridstackNode: ${unadopted.join(", ")}`).toEqual([]);
    expect(await page.locator(ROOT_ITEMS).count()).toBeGreaterThan(0);
    expect(await page.locator(".nested-grid.grid-stack-initialized").count()).toBeGreaterThan(0);

    const real = errors.filter((e) => !TRANSIENT.test(e));
    // addWidget(HTMLElement) was removed in v11: it logs and forwards to makeWidget,
    // silently dropping its options argument. A caller only shows up as this log.
    expect(real.filter((e) => V11_DEPRECATION.test(e)), "removed API still in use").toEqual([]);
    expect(real, real.join("\n")).toEqual([]);
  });

  // NOT covered: dragging a widget *within* a group's sub-grid. It does not work today
  // — grabbing a nested widget's header fires dragstart on the ROOT grid, i.e. it drags
  // the whole group. The sub-grid is built by gridstack cloning the root options, so it
  // inherits handleClass 'grid-stack-handle', which nested widgets never render (they
  // render .nested-grid-stack-handle, deliberately distinct so a nested header does not
  // double as the parent group's drag handle). The later GridStack.init on .nested-grid
  // returns the existing instance and discards its options; passing subGridOpts on the
  // root init does not override it either. A test here is easy to write and easy to get
  // wrong: nested x/y drifts on its own as charts settle, so a "position changed" assert
  // passes without any drag happening. Assert on a sub-grid dragstart event instead.

  test("resizing a widget resizes its chart", async ({ page }) => {
    await openFirstDashboard(page);
    const before = await snapshot(page);

    const widget = await chartWidget(page);
    await widget.scrollIntoViewIfNeeded();
    const size = await layoutOf(widget);
    const chart = widget.locator("[data-chart-widget]").first();
    await expect.poll(() => chart.evaluate((el: any) => !!(window as any).echarts?.getInstanceByDom(el))).toBe(true);
    const beforeChart = await chart.evaluate((el: any) => {
      const instance = (window as any).echarts.getInstanceByDom(el);
      return { width: instance.getWidth(), height: instance.getHeight() };
    });

    // gridstack injects the resize handles after init (they are not in server markup)
    // and marks items ui-resizable-autohide, so the corner grip only becomes visible —
    // and only then has a box to aim at — once the widget is hovered.
    const handle = widget.locator(".ui-resizable-se").first();
    await expect(handle).toBeAttached();
    await widget.hover();
    await expect(handle).toBeVisible();

    // Shrink rather than grow: growing can be clamped at the 12-column edge, while
    // every widget here sits well above the default minimum.
    await dragBy(page, handle, -160, -80);

    await expect
      .poll(
        async () => {
          const l = await layoutOf(widget);
          return !!l && (l.w !== size!.w || l.h !== size!.h);
        },
        { timeout: 5000 },
      )
      .toBe(true);

    await expect.poll(() => chart.evaluate((el: any, old) => {
      const instance = (window as any).echarts.getInstanceByDom(el);
      return instance && {
        changed: instance.getWidth() !== old.width || instance.getHeight() !== old.height,
        fitted: Math.abs(instance.getWidth() - el.clientWidth) <= 1 && Math.abs(instance.getHeight() - el.clientHeight) <= 1,
      };
    }, beforeChart)).toEqual({ changed: true, fitted: true });

    await restore(page, before);
  });

  test("duplicating a widget appends it to the grid and adopts it", async ({ page }) => {
    await openFirstDashboard(page);

    const items = page.locator(ROOT_ITEMS);
    const idsBefore = (await snapshot(page)).map((l) => l.id);
    const widget = await plainWidget(page);

    // Scope the menu item to this widget's own subtree: every widget renders its own
    // popover menu, so a page-wide getByText would act on whichever appears first.
    await widget.locator('button[aria-label="Widget menu"]').first().click();
    const duplicate = widget.getByText("Duplicate widget");
    await expect(duplicate).toBeVisible();

    // htmx appends the rendered widget (hx-target="closest .grid-stack",
    // hx-swap="beforeend") and the element's hyperscript then hands it to
    // gridstack.makeWidget. Both halves must work for the count to land at +1 with the
    // new node adopted.
    await Promise.all([
      page.waitForResponse((r) => r.url().includes("/duplicate") && r.status() < 400),
      duplicate.click(),
    ]);
    await expect(items).toHaveCount(idsBefore.length + 1, { timeout: 10000 });

    // Identify the copy by id rather than position: since 13.1.0 gridstack reorders
    // the DOM to match the visual layout, so "the last child" is not dependable.
    const addedId = (await snapshot(page)).map((l) => l.id).find((id) => !idsBefore.includes(id));
    expect(addedId, "no new widget element appeared").toBeTruthy();
    const added = byId(page, addedId!);

    const layout = await layoutOf(added);
    expect(layout, "the duplicate was appended but never adopted by gridstack").not.toBeNull();
    // Size comes from the gs-w/gs-h the server rendered, never from a layout argument:
    // makeWidget reads the attributes, and addWidget's options are dropped since v11.
    expect(layout!.w).toBeGreaterThan(0);
    expect(layout!.h).toBeGreaterThan(0);

    // Duplicating writes to the database, so put the dashboard back. This also
    // exercises removeWidget, the other half of the gridstack mutation API.
    page.once("dialog", (d) => d.accept());
    await added.locator('button[aria-label="Widget menu"]').first().click();
    await added.getByText("Delete widget").click();
    await expect(items, "cleanup failed — the duplicate is still on the dashboard").toHaveCount(
      idsBefore.length,
      { timeout: 10000 },
    );
  });

  // The drag and resize tests above prove gridstack moved the widget; they say nothing
  // about whether the move survived. Everything the reader does to a canvas is debounced
  // into a widget-order PATCH, and that handler rebuilds the dashboard's widget list
  // purely from the patch — so "it moved" and "it was saved" are genuinely separate
  // claims, and only the second one is what the reader gets back tomorrow.
  test("a move survives a reload, and the server rebuilds the same canvas", async ({ page }) => {
    await openFirstDashboard(page);
    const before = await snapshot(page);

    const target = before.find((l) => l.x > 0);
    expect(target, "dashboard has no widget offset from column 0 to drag").toBeTruthy();
    const widget = byId(page, target!.id);
    await widget.scrollIntoViewIfNeeded();
    const box = (await widget.boundingBox())!;

    // Wait for the PATCH itself rather than a timeout: it is the whole point of the test,
    // and a silent 4xx would otherwise read as a passing drag.
    const saved = page.waitForResponse(
      (r) => r.url().includes("/widgets_order") && r.request().method() === "PATCH" && r.status() < 400,
      { timeout: 15000 },
    );
    await dragBy(page, widget.locator(".grid-stack-handle").first(), -box.width * 0.8, 0);
    await saved;

    const moved = await layoutOf(widget);
    expect(moved, "the drag did not land").not.toBeNull();
    expect(moved!.x !== target!.x || moved!.y !== target!.y, "the widget never moved").toBe(true);

    await page.reload();
    await page.waitForSelector(`${ROOT_GRID}.grid-stack-initialized`, { timeout: 15000 });
    await page.locator(".ui-resizable-se").first().waitFor({ state: "attached", timeout: 15000 });

    // Same widget, same cell, straight off the server's own markup.
    const reloaded = await layoutOf(byId(page, target!.id));
    expect(reloaded, "the widget is gone after a reload — the patch dropped it").not.toBeNull();
    expect({ x: reloaded!.x, y: reloaded!.y }).toEqual({ x: moved!.x, y: moved!.y });

    // And nothing else fell off the canvas: the patch rebuilds the whole list, so a
    // widget missing from it is deleted rather than left alone.
    const afterIds = (await snapshot(page)).map((l) => l.id).sort();
    expect(afterIds).toEqual(before.map((l) => l.id).sort());

    await restore(page, before);
  });
});
