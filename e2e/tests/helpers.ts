import { execFileSync } from "node:child_process";
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

// These fixtures only use the disposable database owned by scripts/e2e.sh.
export function sql(query: string) {
  execFileSync('psql', ['-h', process.env.E2E_PGHOST ?? process.env.DB_HOST ?? 'localhost',
    '-p', process.env.E2E_PGPORT ?? process.env.DB_PORT ?? '5432', '-U', 'postgres',
    '-d', process.env.E2E_DB ?? 'monoscope_e2e', '-v', 'ON_ERROR_STOP=1', '-c', query],
  { env: { ...process.env, PGPASSWORD: process.env.E2E_PGPASSWORD ?? 'postgres' }, stdio: 'pipe' });
}
