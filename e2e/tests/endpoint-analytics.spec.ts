import { test, expect } from "@playwright/test";
import { DEMO_PROJECT } from "./helpers";

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

  await page.getByText("Dependencies", { exact: true }).click();
  await expect(page.getByText("Downstream health", { exact: true })).toBeVisible();
  await expect(page.getByText("Dependency Regressions", { exact: true })).toBeVisible();
});
