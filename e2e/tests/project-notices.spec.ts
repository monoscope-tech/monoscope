import { randomUUID } from "node:crypto";
import { test, expect } from "@playwright/test";
import { sql } from "./helpers";

test("customer notices keep fullscreen panels and sidebar flyouts aligned", async ({ page, baseURL }) => {
  const uid = randomUUID(), sid = randomUUID(), pid = randomUUID();
  sql(`INSERT INTO users.users (id, email, is_sudo) VALUES ('${uid}', '${uid}@example.com', true);
       INSERT INTO users.persistent_sessions (id, user_id) VALUES ('${sid}', '${uid}');
       INSERT INTO projects.projects (id, title, payment_plan, sub_id, customer_id, billing_provider, onboarding_steps_completed)
       VALUES ('${pid}', 'Customer banner test', 'Free', 'sub_banner', 'cus_banner', 'stripe_provider', ARRAY['checklist_dismissed']);`);
  try {
    await page.context().addCookies([{ name: "monoscope_session", value: sid, url: baseURL! }]);
    await page.route("**/log_explorer/data**", route => route.fulfill({
      json: { logsData: [], cols: [], colIdxMap: {}, traces: [], count: 0, hasMore: false },
    }));
    await page.route("**/traces/**", route => route.fulfill({ contentType: "text/html", body: "Trace details" }));
    for (const width of [1280, 390]) {
      await page.setViewportSize({ width, height: 800 });
      await page.goto(`/p/${pid}/log_explorer?fullscreen=trace&showTrace=test-trace`);
      const admin = page.locator("#super-admin-banner"), billing = page.locator("#billing-downgrade-banner");
      await expect(admin).toContainText("You are a super admin in a customer's project");
      await expect(billing.getByRole("link", { name: "Update payment method" })).toBeVisible();
      await expect(page.locator("#apiLogsPage")).toHaveAttribute("data-fullscreen", "trace");
      const noticeBottom = (await billing.boundingBox())!;
      const panel = await page.locator("#trace_expanded_view").boundingBox();
      expect((await admin.boundingBox())!.y).toBe(0);
      expect(panel!.y).toBeGreaterThanOrEqual(noticeBottom.y + noticeBottom.height);
      expect(panel!.y + panel!.height).toBeLessThanOrEqual(801);
      expect(await page.evaluate(() => document.documentElement.scrollWidth)).toBeLessThanOrEqual(width);
      await billing.getByRole("link", { name: "Review billing or change plan" }).click();
      await expect(page).toHaveURL(new RegExp(`/p/${pid}/manage_billing$`));
      await expect(page.getByText("Manage subscription", { exact: true })).toBeVisible();
      if (width === 1280) {
        const issues = page.locator(`#main-sidenav .main-nav-link[href="/p/${pid}/issues"]`);
        await expect(issues.locator("..")).toHaveAttribute("data-hyperscript-powered", "true");
        for (const _ of [0, 1]) {
          await issues.hover();
          const flyout = issues.locator("..").locator(".nav-flyout");
          await expect(flyout).toBeVisible();
          await expect.poll(async () => (await flyout.boundingBox())!.y - (await issues.boundingBox())!.y).toBeCloseTo(0, 0);
          await page.getByRole("button", { name: "Toggle sidebar", exact: true }).click();
        }
      }
    }
  } finally {
    sql(`DELETE FROM projects.projects WHERE id = '${pid}'; DELETE FROM users.users WHERE id = '${uid}';`);
  }
});
