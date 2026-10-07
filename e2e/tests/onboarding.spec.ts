import { test, expect } from "@playwright/test";
import { DEMO_PROJECT, assertStripeCheckout } from "./helpers";

const ONBOARDING_URL = `/p/${DEMO_PROJECT}/onboarding`;

test.describe("Onboarding flow", () => {
  test("loads integration docs with HTML negotiation and switches framework guides", async ({ page }) => {
    const initialDocs = page.waitForResponse(response => new URL(response.url()).pathname === "/proxy/docs/sdks/nodejs/expressjs");
    await page.goto(`${ONBOARDING_URL}?step=Integration`);
    // Guides load when their panel scrolls into view, not with the page.
    await page.locator("#check-js").check();
    expect((await initialDocs).status()).toBe(200);
    const article = page.locator("#fw-content-js #mainArticle");
    await expect(article.getByRole("heading", { level: 1 })).toContainText(/Express/i);
    const notice = page.locator("#toastsParent > div").filter({ hasText: "No events found yet" });
    while (await notice.count()) await notice.first().getByRole("button", { name: "Dismiss notification", exact: true }).click();

    const nextDocs = page.waitForResponse(response => new URL(response.url()).pathname === "/proxy/docs/sdks/nodejs/fastifyjs");
    await page.locator('label[for="fw-tab-js-2"]').click();
    expect((await nextDocs).status()).toBe(200);
    await expect(article.getByRole("heading", { level: 1 })).toContainText(/Fastify/i);
  });

  test("moves from profile setup to an instrumentable project", async ({ page }) => {
    await page.goto(ONBOARDING_URL);
    await expect(page.getByText("Step 1 of 5")).toBeVisible();
    await page.locator('input[name="firstName"]').fill("E2E");
    await page.locator('input[name="lastName"]').fill("Test");
    await page.locator('input[name="companyName"]').fill("TestCorp");
    await page.locator('select[name="companySize"]').selectOption("5 - 10");
    await page.locator('select[name="whereDidYouHearAboutUs"]').selectOption("twitter");
    await page.getByRole("button", { name: "Proceed" }).click();

    await expect(page.getByText("Step 2 of 5")).toBeVisible();
    await page.locator('input[name="location"][value="eu"]').check();
    await page.locator('input[name="functionality"][value="logs"]').check();
    await page.locator('input[name="functionality"][value="analytics"]').check();
    await page.getByRole("button", { name: "Proceed" }).click();

    await expect(page.getByText("Step 3 of 5")).toBeVisible();
    await expect(page.getByText("Slack")).toBeVisible();
    await expect(page.getByText("Discord")).toBeVisible();
    await page.getByRole("button", { name: "Proceed" }).click();
    await expect(page.getByText("Test notification sent")).toBeVisible();
    await page.getByLabel("Modal dialog").getByRole("button", { name: "Proceed" }).click();

    await expect(page.getByText("Step 4 of 5")).toBeVisible();
    await expect(page.getByText("Your API Key", { exact: true })).toBeVisible();
    await expect(page.getByText("Applications")).toBeVisible();
    await expect(page.getByText("Infrastructure")).toBeVisible();
  });

  test("shows every plan and starts paid checkout", async ({ page }) => {
    await page.goto(`${ONBOARDING_URL}?step=Pricing`);
    await expect(page.getByText("Step 5 of 5")).toBeVisible();
    await expect(page.locator("#freePricing")).toBeVisible();
    await expect(page.locator("#GraduatedPricing")).toBeVisible();
    await expect(page.locator("#SystemsPricing")).toBeVisible();
    await assertStripeCheckout(page, "#GraduatedPricing button");
  });
});
