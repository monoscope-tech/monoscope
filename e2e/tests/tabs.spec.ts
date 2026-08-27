import { test, expect } from "@playwright/test";
import { DEMO_PROJECT } from "./helpers";

// Tab switching used to run through a global `navigatable()` that toggled `hidden`
// from JS. It is now DOM state — a checked radio — read by CSS, so an htmx morph
// can no longer desync the visible panel from the active tab.
//
// Worth a real browser because the panel-side selectors are Tailwind arbitrary
// variants (`group-has-[#id:checked]/inv:flex`). A variant that fails to compile
// leaves an inert class: nothing errors, every panel just stays visible at once.
// Only computed style catches that, which is what toBeVisible/toBeHidden assert.
//
// Clicks target the LABEL, not the radio: the radio is `sr-only` and the label
// covers it, which is also how a user switches tabs.
//
// READ-ONLY: navigates and clicks. Safe against a server on production data.

test("api key tabs swap between active and archived", async ({ page }) => {
  await page.goto(`/p/${DEMO_PROJECT}/apis`);
  // Two elements carry each id — the panel and the table inside it (the table's
  // elemID is the panel id). `.tab-content` picks the panel.
  const active = page.locator("#active_content.tab-content");
  const revoked = page.locator("#revoked_content.tab-content");
  await expect(active).toBeVisible({ timeout: 20000 });
  await expect(revoked).toBeHidden();

  await page.locator("label:has(#revoked_content)").or(page.getByRole("tab", { name: /Archived keys/ })).first().click();
  await expect(revoked).toBeVisible();
  await expect(active).toBeHidden();
});

test("issue investigation tabs swap between trace and logs", async ({ page }) => {
  const errors: string[] = [];
  page.on("pageerror", e => errors.push(e.message));

  // Reached from the list rather than a fixed id: issues age out of the demo data.
  await page.goto(`/p/${DEMO_PROJECT}/issues`);
  // Same reason as metric-exemplars: an empty demo project has no issue to open.
  await page.waitForLoadState("networkidle").catch(() => {});
  test.skip((await page.locator('a[href*="/issues/"]').count()) === 0, "no issues — environment has no telemetry");

  await page.locator('a[href*="/issues/"]').first().click({ timeout: 30000 });
  await page.locator("#error-details-container").waitFor({ timeout: 30000 });

  const trace = page.locator("#span-content");
  const logs = page.locator("#log-content");
  const initialTraceVisible = await trace.isVisible();

  // Whichever starts open, the other must be closed, and clicking swaps them.
  await expect(logs).toBeVisible({ visible: !initialTraceVisible });
  await page.locator(initialTraceVisible ? "label:has(#err-tab-logs)" : "label:has(#err-tab-trace)").click();
  await expect(trace).toBeVisible({ visible: !initialTraceVisible });
  await expect(logs).toBeVisible({ visible: initialTraceVisible });

  expect(errors.join("\n")).not.toMatch(/is not defined|is not a function/i);
});
