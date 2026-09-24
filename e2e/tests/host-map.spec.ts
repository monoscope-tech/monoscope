import { test, expect } from "@playwright/test";
import { DEMO_PROJECT, sql } from "./helpers";

const HOST_MAP_URL = `/p/${DEMO_PROJECT}/infrastructure/host-map?since=5M`;

const HOST = "e2e-host-without-usage";
const cleanup = `DELETE FROM otel_metrics WHERE project_id='${DEMO_PROJECT}' AND resource___host___name='${HOST}';`;
test.beforeAll(() => sql(cleanup + `
  INSERT INTO otel_metrics (project_id,id,series_id,timestamp,metric_name,metric_type,value,resource___host___name,resource)
  VALUES ('${DEMO_PROJECT}',gen_random_uuid(),'e2e-host',now(),'system.uptime','GAUGE',60,'${HOST}',
    '{"host":{"name":"${HOST}"},"os":{"type":"linux"}}');`));
test.afterAll(() => sql(cleanup));

async function openHostMap(page: import("@playwright/test").Page) {
  await page.goto(HOST_MAP_URL, { waitUntil: "domcontentloaded" });
  await expect(page.locator("[data-deferred-shell]")).toHaveCount(0);
  await expect(page.getByRole("button", { name: `${HOST}, CPU usage: no data`, exact: true })).toBeVisible();
}

test.describe("Host map", () => {
  test.describe.configure({ mode: "serial" });

  test("the desktop host inspector stays labelled and leaves the map interactive", async ({ page }) => {
    await openHostMap(page);
    await expect(page.locator(`[data-visible-host-label="${HOST}"]`)).toBeVisible();
    const host = page.getByRole("button", { name: new RegExp(`${HOST}, CPU usage:`) });
    await host.click();

    const dialog = page.getByRole("dialog");
    await expect(dialog).toBeVisible();
    await expect(dialog).not.toHaveAttribute("aria-modal", "true");
    await expect(dialog).toHaveAccessibleName(/vps-|host/i);
    const header = dialog.locator("header");
    await expect(header).toBeVisible();
    await expect.poll(() => dialog.evaluate(panel =>
      panel.querySelector('[aria-label="Close drawer"]')!.getBoundingClientRect().right
      - panel.querySelector('header')!.getBoundingClientRect().right,
    )).toBeLessThanOrEqual(-12);
    await expect
      .poll(() => page.locator("#main-content").evaluate((element) => (element as HTMLElement).inert))
      .toBe(false);

    await page.keyboard.press("Escape");
    await expect(dialog).toBeHidden();
    await expect(host).toBeFocused();
  });

  test("missing host metrics collapse into one actionable recovery state", async ({ page }) => {
    await openHostMap(page);
    await page.getByRole("button", { name: new RegExp(`${HOST}, CPU usage:`) }).click();

    const dialog = page.getByRole("dialog");
    await expect(dialog.getByText("Metrics coverage: 0 of 4")).toBeVisible();
    await expect(dialog.getByText("No host metrics in this time range", { exact: true })).toBeVisible();
    await expect(dialog.getByRole("link", { name: "Try last 1 hour" })).toBeVisible();
    const setup = dialog.getByRole("link", { name: "Set up host metrics" });
    await expect(setup).toBeVisible();
    await expect(setup).toHaveClass(/\bbtn-primary\b/);
    await expect(setup).toHaveAttribute("href", "https://monoscope.tech/docs/sdks/infrastructure/");
    await expect(dialog.getByText("No data for the selected time range")).toHaveCount(0);
  });

  test("the mobile inspector uses the full viewport and replaces the log table", async ({ page }) => {
    await page.setViewportSize({ width: 390, height: 844 });
    await openHostMap(page);
    for (const select of await page.locator("main form select").all()) {
      await expect.poll(async () => (await select.boundingBox())?.height).toBeGreaterThanOrEqual(44);
    }
    await page.getByRole("button", { name: new RegExp(`${HOST}, CPU usage:`) }).click();

    const dialog = page.getByRole("dialog");
    await expect(dialog).toHaveAttribute("aria-modal", "true");
    await expect(page.locator("#main-content")).toHaveJSProperty("inert", true);
    await expect(dialog).toHaveCSS("width", "390px");
    await expect(dialog.getByText("Open this host in Explorer to search, filter, and inspect its logs.")).toBeVisible();
    await expect(dialog.locator("log-list")).toBeHidden();
    const title = dialog.locator("#host-detail-title");
    await title.evaluate((element) => {
      element.textContent = "خادم الإنتاج الرئيسي — äußerst-langer-hostname-mit-emoji-🚀-und-mehrsprachigen-zeichen.example.internal".repeat(2);
      element.setAttribute("dir", "auto");
    });
    await expect(title).toHaveCSS("overflow-wrap", "break-word");
    for (const control of [
      dialog.getByRole("button", { name: "Close drawer" }),
      dialog.getByRole("link", { name: "View containers", exact: true }),
      dialog.getByRole("link", { name: "Summary", exact: true }),
      dialog.getByRole("link", { name: "Recent logs", exact: true }),
      dialog.getByRole("link", { name: "Metrics", exact: true }),
      dialog.getByRole("link", { name: "Open logs in Explorer", exact: true }),
      dialog.getByRole("link", { name: "Try last 1 hour", exact: true }),
      dialog.getByRole("link", { name: "Set up host metrics", exact: true }),
    ]) {
      await expect.poll(async () => (await control.boundingBox())?.height).toBeGreaterThanOrEqual(44);
    }
    await expect
      .poll(() => page.evaluate(() => document.documentElement.scrollWidth === window.innerWidth))
      .toBe(true);
    await page.keyboard.press("Escape");
    await expect(dialog).toBeHidden();
    await expect(page.locator("#global-data-drawer-panel")).not.toHaveAttribute("aria-modal", "true");
    await expect(page.locator("#main-content")).toHaveJSProperty("inert", false);
  });
});
