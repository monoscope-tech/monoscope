import { test, expect } from "@playwright/test";
import { DEMO_PROJECT, sql } from "./helpers";

const METRIC = "e2e.exemplar.duration";
const TRACE = "e2e00000000000000000000000000001";
const cleanup = `DELETE FROM otel_metrics WHERE project_id='${DEMO_PROJECT}' AND metric_name='${METRIC}';
DELETE FROM otel_metrics_meta WHERE project_id='${DEMO_PROJECT}' AND metric_name='${METRIC}';`;
test.beforeAll(() => sql(cleanup + `
  INSERT INTO otel_metrics_meta (project_id,metric_name,metric_type,metric_unit,metric_description,service_name,scope_name,metric_labels,first_seen_at,last_seen_at,first_timestamp,last_timestamp)
  VALUES ('${DEMO_PROJECT}','${METRIC}','GAUGE','ms','','e2e-exemplar','e2e','{}',now(),now(),now(),now());
  INSERT INTO otel_metrics (project_id,id,series_id,timestamp,metric_name,metric_type,metric_unit,value,resource___service___name,exemplars)
  VALUES ('${DEMO_PROJECT}',gen_random_uuid(),'e2e-exemplar',now(),'${METRIC}','GAUGE','ms',12,'e2e-exemplar',
    jsonb_build_array(jsonb_build_object('trace_id','${TRACE}','span_id','e2e0000000000001','timestamp',now(),'value',12)));`));
test.afterAll(() => sql(cleanup));

test.describe("metric detail — Exemplars tab", () => {
  test("lazy fragment reveals on tab click and clears aria-busy", async ({ page }) => {
    const errors: string[] = [];
    page.on("pageerror", e => errors.push(e.message));

    await page.goto(`/p/${DEMO_PROJECT}/metrics?q=${METRIC}`);
    // The shell must be live, or the tab handlers below cannot exist.
    await expect
      .poll(() => page.evaluate(() => typeof (window as any).htmx), { timeout: 20000 })
      .toBe("object");

    await page.getByRole("button", { name: "Expand widget", exact: true }).first().click();

    const panel = page.locator("#ex-content");
    await expect(panel).toHaveCount(1, { timeout: 20000 });
    await expect(panel).toBeHidden();
    await expect(panel).toHaveAttribute("aria-busy", "true");

    await page.getByRole("tab", { name: "Exemplars", exact: true }).check();

    await expect(panel).toBeVisible({ timeout: 20000 });
    await expect(panel).not.toHaveAttribute("aria-busy", "true", { timeout: 30000 });
    await expect(panel.getByRole("link", { name: new RegExp(TRACE) })).toBeVisible();
    expect(errors.join("\n")).not.toMatch(/is not defined|is not a function/i);
  });
});
