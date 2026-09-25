import { test, expect } from "@playwright/test";
import { DEMO_PROJECT, sql } from "./helpers";

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
// Fixtures and key mutations use the local e2e database.

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

test("an API key can be created, copied, revoked, and reactivated", async ({ page, context }) => {
  await context.grantPermissions(["clipboard-read", "clipboard-write"]);
  const title = `E2E collector ${Date.now()}`;
  await page.goto(`/p/${DEMO_PROJECT}/apis`);
  await page.getByText("New Key", { exact: true }).click();
  await page.getByRole("textbox", { name: "Key title" }).fill(title);
  await page.getByRole("button", { name: "Create key", exact: true }).click();
  await expect(page.getByText("API Key was generated successfully")).toBeVisible();
  const key = await page.locator("#newKey").innerText();
  await page.getByRole("button", { name: "Dismiss", exact: true }).click();
  await page.getByRole("button", { name: `Show value for ${title}`, exact: true }).press("Enter");
  const row = page.getByRole("row").filter({ hasText: title }).filter({ visible: true });
  await expect(row.locator('[id^="key-value-"]')).toBeVisible();
  await page.getByRole("button", { name: `Copy ${title}`, exact: true }).click();
  expect(await page.evaluate(() => navigator.clipboard.readText())).toBe(key);
  await expect(page.locator("#toastsParent").getByText("Value copied to the Clipboard", { exact: true })).toBeVisible();
  page.on("dialog", dialog => dialog.accept());
  for (const [action, tab] of [["Revoke", "Archived keys"], ["Activate", "Active keys"], ["Revoke", "Archived keys"]]) {
    await page.getByRole("button", { name: `${action} ${title}`, exact: true }).click();
    const message = `${action === "Revoke" ? "Revoked" : "Activated"} API Key Successfully`;
    await expect(page.getByText(message, { exact: true })).toBeVisible();
    await page.getByRole("tab", { name: new RegExp(tab) }).click();
    await expect(row).toBeVisible();
    await page.locator("#toastsParent > div").filter({ hasText: message }).getByRole("button", { name: "Dismiss notification" }).press("Enter");
    await expect(page.getByText(message, { exact: true })).toBeHidden();
  }
  await page.reload();
  await page.getByRole("tab", { name: /Archived keys/ }).click();
  await expect(row).toBeVisible();
});

test("issue event card pins its navigator and jumps between trace and logs", async ({ page }) => {
  const errors: string[] = [];
  page.on("pageerror", e => errors.push(e.message));

  const hash = "e2e-investigation-tabs";
  const traceId = "e2e00000000000000000000000000002";
  const cleanup = `DELETE FROM apis.issues WHERE project_id='${DEMO_PROJECT}' AND target_hash='${hash}';
    DELETE FROM apis.error_patterns WHERE project_id='${DEMO_PROJECT}' AND hash='${hash}';
    DELETE FROM background_jobs WHERE payload->'contents'->>1='${hash}';
    DELETE FROM otel_logs_and_spans WHERE project_id='${DEMO_PROJECT}' AND context___trace_id='${traceId}';`;
  sql(cleanup + `
    INSERT INTO apis.error_patterns (project_id,error_type,message,stacktrace,hash,first_trace_id,recent_trace_id,error_data)
    VALUES ('${DEMO_PROJECT}','TypeError','E2E investigation failure','at checkout (app.js:1)','${hash}','${traceId}','${traceId}',
      jsonb_build_object('when',now(),'error_type','TypeError','root_error_type','TypeError','message','E2E investigation failure',
        'root_error_message','E2E investigation failure','stack_trace','at checkout (app.js:1)','hash','${hash}','is_framework',false));
    INSERT INTO apis.issues (project_id,issue_type,title,target_hash,service,issue_data)
    VALUES ('${DEMO_PROJECT}','runtime_exception','E2E investigation failure','${hash}','e2e-checkout',
      jsonb_build_object('error_type','TypeError','error_message','E2E investigation failure','stack_trace','at checkout (app.js:1)',
        'occurrence_count',1,'first_seen',now(),'last_seen',now()));
    INSERT INTO otel_logs_and_spans (project_id,summary,name,kind,timestamp,start_time,end_time,duration,context___trace_id,context___span_id,resource___service___name)
    VALUES ('${DEMO_PROJECT}',ARRAY['GET /e2e-checkout'],'GET /e2e-checkout','server',now(),now(),now()+interval '10 milliseconds',10000000,'${traceId}','e2e0000000000002','e2e-checkout');`);
  try {
    await page.goto(`/p/${DEMO_PROJECT}/issues?type=runtime_exception`);
    await page.getByRole("link").filter({ hasText: "E2E investigation failure" }).first().click();
    const nav = page.locator("#issue-event-nav");
    await expect(page.locator("#span-content")).toBeVisible();
    await expect(page.locator("#log-content")).toBeVisible();
    await nav.getByRole("link", { name: "Logs" }).click();
    await expect(page.locator("#issue-logs")).toBeInViewport();
    // Sticky: the navigator stays at the top of the scroller while its sections scroll under it.
    const [navTop, scrollerTop] = await Promise.all([
      nav.evaluate(el => el.getBoundingClientRect().top),
      nav.evaluate(el => el.closest(".overflow-y-auto")!.getBoundingClientRect().top),
    ]);
    expect(Math.abs(navTop - scrollerTop)).toBeLessThan(2);
    await nav.getByRole("link", { name: "Trace" }).click();
    await expect(page.locator("#issue-trace")).toBeInViewport();
    expect(errors.join("\n")).not.toMatch(/is not defined|is not a function/i);
  } finally { sql(cleanup); }
});
