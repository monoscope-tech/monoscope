import { test, expect } from "@playwright/test";
import { DEMO_PROJECT } from "./helpers";

const HOST_MAP_URL = `/p/${DEMO_PROJECT}/infrastructure/host-map?since=5M`;

test.describe("Host map", () => {
  test.describe.configure({ mode: "serial" });

  test("the host inspector is a labelled modal that isolates and restores the map", async ({ page }) => {
    await page.goto(HOST_MAP_URL, { waitUntil: "domcontentloaded" });
    await expect(page.locator('[data-visible-host-label="vps-d6d7e318"]')).toBeVisible();
    const host = page.getByRole("button", { name: /vps-d6d7e318, CPU usage:/ });
    await host.click();

    const dialog = page.getByRole("dialog");
    await expect(dialog).toBeVisible();
    await expect(dialog).toHaveAttribute("aria-modal", "true");
    await expect(dialog).toHaveAccessibleName(/vps-|host/i);
    const header = dialog.locator("header");
    await expect(header).toBeVisible();
    const closeBox = (await dialog.getByRole("button", { name: "Close drawer" }).boundingBox())!;
    const headerBox = (await header.boundingBox())!;
    expect(closeBox.x + closeBox.width).toBeLessThanOrEqual(headerBox.x + headerBox.width - 12);
    await expect
      .poll(() => page.locator("#main-content").evaluate((element) => (element as HTMLElement).inert))
      .toBe(true);

    await page.keyboard.press("Escape");
    await expect(dialog).toBeHidden();
    await expect(host).toBeFocused();
  });

  test("missing host metrics collapse into one actionable recovery state", async ({ page }) => {
    await page.goto(HOST_MAP_URL, { waitUntil: "domcontentloaded" });
    await page.getByRole("button", { name: /vps-d6d7e318, CPU usage:/ }).click();

    const dialog = page.getByRole("dialog");
    await expect(dialog.getByText("Metrics coverage: 0 of 4")).toBeVisible();
    await expect(dialog.getByRole("heading", { name: "No host metrics in this time range" })).toBeVisible();
    await expect(dialog.getByRole("link", { name: "Try last 1 hour" })).toBeVisible();
    const setup = dialog.getByRole("link", { name: "Set up host metrics" });
    await expect(setup).toBeVisible();
    await expect(setup).toHaveCSS("background-color", "oklch(0.52 0.22 261)");
    await expect(dialog.getByText("No data for the selected time range")).toHaveCount(0);
  });

  test("the mobile inspector uses the full viewport and replaces the log table", async ({ page }) => {
    await page.setViewportSize({ width: 390, height: 844 });
    await page.goto(HOST_MAP_URL, { waitUntil: "domcontentloaded" });
    await expect(page.locator("[data-deferred-shell]")).toHaveCount(0);
    for (const select of await page.locator("main form select").all()) {
      expect((await select.boundingBox())?.height).toBeGreaterThanOrEqual(44);
    }
    await page.getByRole("button", { name: /vps-d6d7e318, CPU usage:/ }).click();

    const dialog = page.getByRole("dialog");
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
      expect((await control.boundingBox())?.height).toBeGreaterThanOrEqual(44);
    }
    await expect
      .poll(() => page.evaluate(() => document.documentElement.scrollWidth === window.innerWidth))
      .toBe(true);
  });
});
