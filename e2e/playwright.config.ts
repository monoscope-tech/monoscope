import { defineConfig, devices } from "@playwright/test";

export default defineConfig({
  testDir: "./tests",
  fullyParallel: true,
  // Keep browser concurrency bounded so layout and latency checks have headroom.
  workers: 2,
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
    { name: "chromium", use: { ...devices["Desktop Chrome"] }, testIgnore: /(container-identity|endpoint-analytics|host-map|infrastructure-time-window|metric-exemplars|metrics-catalog|rum-sessions|tabs)\.spec\.ts$/ },
    { name: "chromium-fixtures", use: { ...devices["Desktop Chrome"] }, testMatch: /(container-identity|endpoint-analytics|host-map|infrastructure-time-window|metric-exemplars|metrics-catalog|rum-sessions|tabs)\.spec\.ts$/, dependencies: ["chromium"], workers: 1 },
  ],
});
