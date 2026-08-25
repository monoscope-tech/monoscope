import { test, expect } from "@playwright/test";
import { DEMO_PROJECT } from "./helpers";

test("essential dashboard and explorer actions survive a 320px viewport", async ({ page }) => {
  await page.setViewportSize({ width: 320, height: 800 });
  const fitsViewport = () => page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth);

  await page.goto(`/p/${DEMO_PROJECT}/dashboards`);
  await expect(page.locator('label[for="newDashboardMdl"]').first()).toBeVisible();
  await expect.poll(fitsViewport).toBe(true);

  await page.goto(`/p/${DEMO_PROJECT}/log_explorer`, { waitUntil: "domcontentloaded" });
  await expect(page.locator("#filterElement")).toBeVisible();
  await expect.poll(fitsViewport).toBe(true);
});
