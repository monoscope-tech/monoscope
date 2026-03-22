import { test, expect } from "@playwright/test";
import { DEMO_PROJECT, assertStripeCheckout } from "./helpers";

const ONBOARDING_URL = `/p/${DEMO_PROJECT}/onboarding`;

// Onboarding tests mutate shared demo project state — run serially
test.describe.configure({ mode: "serial" });

test.describe("Onboarding flow", () => {
  test("step 1: info form submits and advances to survey", async ({
    page,
  }) => {
    await page.goto(ONBOARDING_URL);
    await expect(page.getByText("Step 1 of 5")).toBeVisible();
    await expect(
      page.getByText("Tell us a little bit about you"),
    ).toBeVisible();

    await page.locator('input[name="firstName"]').fill("Test");
    await page.locator('input[name="lastName"]').fill("User");
    await page.locator('input[name="companyName"]').fill("Acme Inc");
    await page.locator('select[name="companySize"]').selectOption("1 - 4");
    await page
      .locator('select[name="whereDidYouHearAboutUs"]')
      .selectOption("google");

    await page.getByRole("button", { name: "Proceed" }).click();
    await expect(page.getByText("Step 2 of 5")).toBeVisible();
  });

  test("step 2: survey form submits and advances to notifications", async ({
    page,
  }) => {
    await page.goto(`${ONBOARDING_URL}?step=Survey`);
    await expect(page.getByText("Step 2 of 5")).toBeVisible();
    await expect(
      page.getByText("Let's configure your project"),
    ).toBeVisible();

    await page.locator('input[name="location"][value="usa"]').check();
    await page.locator('input[name="functionality"][value="logs"]').check();

    await page.getByRole("button", { name: "Proceed" }).click();
    await expect(page.getByText("Step 3 of 5")).toBeVisible();
  });

  test("step 3: notification channels form submits", async ({ page }) => {
    await page.goto(`${ONBOARDING_URL}?step=NotifChannel`);
    await expect(page.getByText("Step 3 of 5")).toBeVisible();
    await expect(
      page.getByText("How should we notify you about issues?"),
    ).toBeVisible();

    await expect(page.getByText("Slack")).toBeVisible();
    await expect(page.getByText("Discord")).toBeVisible();

    await page.getByRole("button", { name: "Proceed" }).click();
    await expect(page.getByText("Test notification sent")).toBeVisible();
  });

  test("step 4: integration page shows API key and language options", async ({
    page,
  }) => {
    await page.goto(`${ONBOARDING_URL}?step=Integration`, {
      waitUntil: "domcontentloaded",
    });
    await expect(page.getByText("Step 4 of 5")).toBeVisible();
    await expect(
      page.getByText("Instrument your apps or servers"),
    ).toBeVisible();
    await expect(
      page.getByText("Your API Key", { exact: true }),
    ).toBeVisible();
    await expect(page.getByText("Applications")).toBeVisible();
    await expect(page.getByText("Infrastructure")).toBeVisible();
  });

  test("step 5: pricing page shows plans", async ({ page }) => {
    await page.goto(`${ONBOARDING_URL}?step=Pricing`);
    await expect(page.getByText("Step 5 of 5")).toBeVisible();
    await expect(page.locator("#freePlan")).toBeVisible();
    await expect(page.locator("#popularPlan")).toBeVisible();
    await expect(page.locator("#systemsPlan")).toBeVisible();
  });

  test("step 5: selecting a paid plan triggers checkout", async ({ page }) => {
    await page.goto(`${ONBOARDING_URL}?step=Pricing`);
    await expect(page.locator("#popularPlan")).toBeVisible();
    await assertStripeCheckout(page, "#popularPlan button");
  });

  test("complete step shows success and dashboard link", async ({ page }) => {
    await page.goto(`${ONBOARDING_URL}?step=Complete`);
    await expect(page.getByText("Go to your dashboard")).toBeVisible();
  });

  test("full onboarding flow end-to-end", async ({ page }) => {
    await page.goto(ONBOARDING_URL);
    await page.locator('input[name="firstName"]').fill("E2E");
    await page.locator('input[name="lastName"]').fill("Test");
    await page.locator('input[name="companyName"]').fill("TestCorp");
    await page.locator('select[name="companySize"]').selectOption("5 - 10");
    await page
      .locator('select[name="whereDidYouHearAboutUs"]')
      .selectOption("twitter");
    await page.getByRole("button", { name: "Proceed" }).click();

    await expect(page.getByText("Step 2 of 5")).toBeVisible();
    await page.locator('input[name="location"][value="eu"]').check();
    await page.locator('input[name="functionality"][value="logs"]').check();
    await page
      .locator('input[name="functionality"][value="analytics"]')
      .check();
    await page.getByRole("button", { name: "Proceed" }).click();

    await expect(page.getByText("Step 3 of 5")).toBeVisible();
    await page.getByRole("button", { name: "Proceed" }).click();
    await expect(page.getByText("Test notification sent")).toBeVisible();

    await page
      .getByLabel("Modal dialog")
      .getByRole("button", { name: "Proceed" })
      .click();
    await expect(page.getByText("Step 4 of 5")).toBeVisible();
  });
});
