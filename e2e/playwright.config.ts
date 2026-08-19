import { defineConfig, devices } from "@playwright/test";

export default defineConfig({
  testDir: "./tests",
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  retries: 0,
  use: {
    // Deliberately NOT 8080. That is the port `make live-reload` serves on, and the dev
    // server reads .env — whose DATABASE_URL points at monoscope-prod-eu-pg. These specs
    // create dashboards, drag widgets and POST to stripe_checkout, so defaulting to 8080
    // would write test data into production the moment someone runs `npx playwright test`
    // with the watcher up. Point this at a server started against monoscope_e2e.
    baseURL: process.env.E2E_BASE_URL ?? "http://localhost:8081",
    trace: "on-first-retry",
  },
  projects: [
    { name: "chromium", use: { ...devices["Desktop Chrome"] } },
  ],
});
