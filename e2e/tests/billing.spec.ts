import { test, expect } from "@playwright/test";
import { DEMO_PROJECT, assertStripeCheckout } from "./helpers";

const BILLING_URL = `/p/${DEMO_PROJECT}/manage_billing`;

test.describe("Billing page", () => {
  test("shows usage and opens the complete plan picker", async ({ page }) => {
    await page.goto(BILLING_URL);
    await expect(page.getByRole("heading", { name: "Billing" }).first()).toBeVisible();
    await expect(page.getByText(/Billing cycle \(since/)).toBeVisible();
    await expect(page.getByText("Estimated this cycle")).toBeVisible();
    await expect(page.getByText(/\d+ requests/)).toBeVisible();
    await page.getByText("Change plan").click();
    await expect(page.getByText("Compare Plans")).toBeVisible();
    await expect(page.getByText("Free tier")).toBeVisible();
    await expect(page.getByText("Bring nothing", { exact: true })).toBeVisible();
    await expect(page.getByText("Bring your own storage")).toBeVisible();
  });

  for (const [name, selector] of [
    ["managed storage", "#GraduatedPricing button"],
    ["bring-your-own storage", "#SystemsPricing button"],
  ] as const) {
    test(`${name} starts checkout`, async ({ page }) => {
      await page.goto(BILLING_URL);
      await page.getByText("Change plan").click();
      await expect(page.getByText("Compare Plans")).toBeVisible();
      await assertStripeCheckout(page, selector);
    });
  }
});
