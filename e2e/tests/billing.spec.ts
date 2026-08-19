import { test, expect } from "@playwright/test";
import { DEMO_PROJECT, assertStripeCheckout } from "./helpers";

const BILLING_URL = `/p/${DEMO_PROJECT}/manage_billing`;

test.describe("Billing page", () => {
  test.beforeEach(async ({ page }) => {
    await page.goto(BILLING_URL);
  });

  test("shows plan info and change plan button", async ({ page }) => {
    // Two headings legitimately carry this name: the settings shell's page title and the
    // section's own h2.
    await expect(page.getByRole("heading", { name: "Billing" }).first()).toBeVisible();
    await expect(page.getByText(/Billing cycle \(since/)).toBeVisible();
    await expect(page.getByText("Estimated this cycle")).toBeVisible();
    await expect(page.getByText(/\d+ requests/)).toBeVisible();
    await expect(page.getByText("/mo")).toBeVisible();
    await expect(page.getByText("Change plan")).toBeVisible();
  });

  test("plan picker modal opens with plan cards", async ({ page }) => {
    await page.getByText("Change plan").click();
    await expect(page.getByText("Compare Plans")).toBeVisible();
    await expect(page.locator("#freePricing")).toBeVisible();
    await expect(page.locator("#GraduatedPricing")).toBeVisible();
    await expect(page.locator("#SystemsPricing")).toBeVisible();
    await expect(page.getByText("Free tier")).toBeVisible();
    await expect(
      page.getByText("Bring nothing", { exact: true }),
    ).toBeVisible();
    await expect(page.getByText("Bring your own storage")).toBeVisible();
  });

  test("clicking upgrade posts to stripe_checkout", async ({ page }) => {
    await page.getByText("Change plan").click();
    await expect(page.getByText("Compare Plans")).toBeVisible();
    await assertStripeCheckout(page, "#GraduatedPricing button");
  });

  test("BYOS plan posts to stripe_checkout", async ({ page }) => {
    await page.getByText("Change plan").click();
    await expect(page.getByText("Compare Plans")).toBeVisible();
    await assertStripeCheckout(page, "#SystemsPricing button");
  });
});
