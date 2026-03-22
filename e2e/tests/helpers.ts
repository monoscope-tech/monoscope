import { Page, expect } from "@playwright/test";

export const REAL_PROVIDERS = process.env.E2E_REAL_PROVIDERS === "true";
export const DEMO_PROJECT = "00000000-0000-0000-0000-000000000000";

export async function assertStripeCheckout(
  page: Page,
  planButtonSelector: string,
) {
  if (REAL_PROVIDERS) {
    await page.locator(planButtonSelector).click();
    await page.waitForURL(/checkout\.stripe\.com/, { timeout: 15000 });
    expect(page.url()).toContain("checkout.stripe.com");
  } else {
    const [response] = await Promise.all([
      page.waitForResponse((r) => r.url().includes("/stripe_checkout")),
      page.locator(planButtonSelector).click(),
    ]);
    expect(response.url()).toContain("/stripe_checkout");
  }
}
