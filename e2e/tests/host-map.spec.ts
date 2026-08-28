import { test, expect } from "@playwright/test";
import { DEMO_PROJECT } from "./helpers";

const HOST_MAP_URL = `/p/${DEMO_PROJECT}/infrastructure/host-map?since=5M`;

test.describe("Host map", () => {
  test("the host inspector is a labelled modal that isolates and restores the map", async ({ page }) => {
    await page.goto(HOST_MAP_URL, { waitUntil: "domcontentloaded" });
    const host = page.getByRole("button", { name: /vps-d6d7e318, CPU usage:/ });
    await host.click();

    const dialog = page.getByRole("dialog");
    await expect(dialog).toBeVisible();
    await expect(dialog).toHaveAttribute("aria-modal", "true");
    await expect(dialog).toHaveAccessibleName(/vps-|host/i);
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
    await expect(dialog.getByText("Signal coverage: 0 of 4")).toBeVisible();
    await expect(dialog.getByRole("heading", { name: "No host metrics in this time range" })).toBeVisible();
    await expect(dialog.getByRole("link", { name: "Try last 1 hour" })).toBeVisible();
    await expect(dialog.getByRole("link", { name: "Set up host metrics" })).toBeVisible();
    await expect(dialog.getByText("No data for the selected time range")).toHaveCount(0);
  });

  test("the mobile inspector uses the full viewport and replaces the log table", async ({ page }) => {
    await page.setViewportSize({ width: 390, height: 844 });
    await page.goto(HOST_MAP_URL, { waitUntil: "domcontentloaded" });
    await page.getByRole("button", { name: /vps-d6d7e318, CPU usage:/ }).click();

    const dialog = page.getByRole("dialog");
    await expect(dialog).toHaveCSS("width", "390px");
    await expect(dialog.getByText("Open this host in Explorer to inspect its logs on a smaller screen.")).toBeVisible();
    await expect(dialog.locator("log-list")).toBeHidden();
    await expect
      .poll(() => page.evaluate(() => document.documentElement.scrollWidth === window.innerWidth))
      .toBe(true);
  });
});
