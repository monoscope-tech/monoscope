import { defineConfig, devices } from "@playwright/test";

export default defineConfig({
  testDir: "./tests",
  fullyParallel: true,
  // Keep browser concurrency bounded so layout and latency checks have headroom. Local
  // sign-off raises it (E2E_WORKERS) on machines with the cores for it.
  workers: Number(process.env.E2E_WORKERS) || 2,
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
    // Keep specs that mutate or count shared DEMO_PROJECT rows serial;
    // the 3 scoped metrics specs (host-map, metric-exemplars, metrics-catalog)
    // isolate their fixture keys and run in parallel.
    { name: "chromium", use: { ...devices["Desktop Chrome"] }, testIgnore: /(container-identity|endpoint-analytics|infrastructure-time-window|rum-audience|tabs)\.spec\.ts$/ },
    { name: "chromium-fixtures", use: { ...devices["Desktop Chrome"] }, testMatch: /(container-identity|endpoint-analytics|infrastructure-time-window|rum-audience|tabs)\.spec\.ts$/, dependencies: ["chromium"], workers: 1 },
  ],
});
