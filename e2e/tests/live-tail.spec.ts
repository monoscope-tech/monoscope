import { test, expect } from "@playwright/test";
import { DEMO_PROJECT } from "./helpers";

const LIVE_TAIL_URL = `/p/${DEMO_PROJECT}/live_tail`;

/**
 * Live Tail against the real page and the real endpoints.
 *
 * These exist because of a specific class of bug the unit tests structurally cannot see. The
 * vitest suite stubs `fetch`, so it happily passes while the component reads a response key the
 * server never sends — which is exactly what shipped: `loadFacets` asked for a top-level
 * `services` array, the schema endpoint returns `{fields: {...}}`, and the service dropdown was
 * therefore always empty — so narrowing a tail was impossible, and every server-side test
 * still passed.
 *
 * So the assertions here are deliberately about *seams*, not matching logic: the page mounts,
 * selectors fill from the live schema response, the row composition survives the real CSS,
 * and the drawer shows a complete stored record. Ingest matching remains server-tested.
 */
test.describe("Live Tail", () => {
  // The seam is checked in two halves so neither depends on the box having telemetry in it.
  // A dev database with no spans has no facet values, so "the dropdown has options" would fail
  // for a reason that has nothing to do with the code.

  test("the schema endpoint really does carry service values under fields[].examples", async ({ page }) => {
    // Half one: the server's actual contract — the half that was wrong. The reader asked for a
    // top-level `services` array this endpoint has never returned.
    await page.goto(LIVE_TAIL_URL, { waitUntil: "domcontentloaded" });
    const shape = await page.evaluate(async (pid) => {
      const res = await fetch(`/p/${pid}/log_explorer/schema`);
      const body = await res.json();
      return {
        hasTopLevelServices: "services" in body,
        hasServiceField: !!body?.fields?.["resource.service.name"],
        hasEnvField: !!body?.fields?.["resource.deployment.environment.name"],
        serviceExamplesIsArray: Array.isArray(body?.fields?.["resource.service.name"]?.examples ?? []),
      };
    }, DEMO_PROJECT);

    expect(shape.hasServiceField).toBe(true);
    expect(shape.hasEnvField).toBe(true);
    expect(shape.serviceExamplesIsArray).toBe(true);
    // Pins the actual mistake: if this ever goes true the contract changed, and the reader has
    // to change with it rather than silently yielding [].
    expect(shape.hasTopLevelServices).toBe(false);
  });

  test("fills both selectors from that shape", async ({ page }) => {
    // Half two: the reader, against a payload in the server's real shape. Stubbed only to
    // supply values a dev database does not have — the shape itself is asserted for real above.
    await page.route("**/log_explorer/schema", async (route) => {
      await route.fulfill({
        json: {
          fields: {
            "resource.service.name": { field_type: "string", description: "", examples: ["checkout", "billing"] },
            "resource.deployment.environment.name": { field_type: "string", description: "", examples: ["prod"] },
          },
        },
      });
    });

    await page.goto(LIVE_TAIL_URL, { waitUntil: "domcontentloaded" });
    const component = page.locator("live-tail");
    await expect(component).toBeVisible();

    await expect(component.getByLabel("Service", { exact: true }).locator("option")).toHaveText([/All services/, "checkout", "billing"]);
    await expect(component.getByLabel("Environment", { exact: true }).locator("option")).toHaveText([/All environments/, "prod"]);
  });

  // "The selection is the control" — there is deliberately no start button and no service
  // gate (see the header comment in live-tail.ts): the page opens tailing everything and
  // narrowing is a choice, not a precondition. What is worth pinning is the other half of
  // that decision — the selection round-trips through the URL, so a tail pasted into an
  // incident channel opens on the same stream, while an unnarrowed link stays plain.
  test("the selection is the control: narrowing writes the URL, the default stays plain", async ({ page }) => {
    await page.route("**/log_explorer/schema", async (route) => {
      await route.fulfill({
        json: { fields: { "resource.service.name": { field_type: "string", description: "", examples: ["checkout"] } } },
      });
    });
    await page.goto(LIVE_TAIL_URL, { waitUntil: "domcontentloaded" });
    const component = page.locator("live-tail");
    await expect(component).toBeVisible();

    // A plain link carries no filter params.
    expect(new URL(page.url()).searchParams.get("service")).toBeNull();

    await component.getByLabel("Service", { exact: true }).selectOption("checkout");

    await expect
      .poll(() => new URL(page.url()).searchParams.get("service"))
      .toBe("checkout");
  });

  test("AI search opens from Live Tail and applies the generated KQL", async ({ page }) => {
    let prompt = "";
    await page.route("**/log_explorer/ai_search", async route => {
      prompt = (await route.request().postDataJSON()).input;
      await route.fulfill({ json: { query: 'level == "ERROR"' } });
    });
    await page.goto(LIVE_TAIL_URL, { waitUntil: "domcontentloaded" });
    const component = page.locator("live-tail");
    await component.locator("query-editor").click();
    // Renamed from "Open AI search" when the control became the "Ask AI" affordance; it is
    // still the same standalone-ai-search button, still keyboard-reachable.
    await component.getByRole("button", { name: "Ask AI" }).click();
    await component.getByLabel("AI search prompt").fill("errors in checkout");
    await component.getByRole("button", { name: "Submit AI search" }).click();

    await expect.poll(() => prompt).toBe("errors in checkout");
    await expect.poll(() => new URL(page.url()).searchParams.get("query")).toBe('level == "ERROR"');
    await expect(component.getByLabel("AI search prompt")).toHaveCount(0);
  });

  test("keeps metadata in two lanes, selected fields in one message, and the complete record in the drawer", async ({ page }) => {
    await page.route("**/live_tail/records/**", async (route) => {
      await route.fulfill({
        json: {
          id: "00000000-0000-4000-8000-000000000001",
          timestamp: "2024-01-01T00:00:00.000Z",
          level: "info",
          body: { message: "complete record" },
          attributes: { order: { id: "ord-42" }, private: { token: "not-enabled" } },
          resource: { service: { name: "checkout" } },
        },
      });
    });
    await page.goto(LIVE_TAIL_URL, { waitUntil: "domcontentloaded" });
    const component = page.locator("live-tail");
    await expect(component).toBeVisible();
    await page.waitForFunction(() => customElements.get("live-tail"));
    await component.evaluate((element: any) => {
      element.teardown();
      element.buffer = [];
      element.rows = [];
      element.appendRows([
        {
          id: "00000000-0000-4000-8000-000000000001",
          timestamp: "2024-01-01T00:00:00.000Z",
          level: "info",
          service: "checkout",
          trace_id: null,
          span_id: null,
          name: null,
          body: "plain message",
          fields: { "attributes.order.id": "ord-42" },
          truncated: false,
        },
      ]);
    });

    await component.getByText("Fields 2", { exact: true }).click();
    await component.getByLabel("Find a field").fill("attributes.order.id");
    await component.getByLabel("Show attributes.order.id in each row").check();
    await component.getByText("Fields 3", { exact: true }).click();

    const row = component.locator("[data-row]");
    await expect(row.locator("[data-service]")).toHaveText("checkout");
    await expect(row.locator("[data-time]")).toHaveText("00:00:00.000");
    await expect(row.locator("[data-message]")).toContainText('level="info"');
    await expect(row.locator("[data-message]")).toContainText('attributes.order.id="ord-42"');

    await row.click();
    const dialog = page.getByRole("dialog", { name: "Record details" });
    await expect(dialog).toContainText("Complete stored JSON");
    await expect(dialog).toContainText("not-enabled");
    await expect(dialog.getByLabel("Show attributes.private.token in each row")).toBeVisible();
    await expect(row.locator("[data-message]")).not.toContainText("attributes.private.token");
  });

  test("the Explorer tab strip leads with Live Tail but still lands on Events", async ({ page }) => {
    // Ordering is a product decision ("what is happening" is opt-in, "what happened" is the
    // default), and it is the kind of thing a nav refactor silently reverses.
    await page.goto(`/p/${DEMO_PROJECT}/log_explorer`, { waitUntil: "domcontentloaded" });
    const tabs = page.getByRole("tablist").first();
    await expect(tabs.getByRole("tab").first()).toHaveText(/live tail/i);
    await expect(tabs.getByRole("tab", { name: /^Events$/i })).toHaveAttribute("aria-selected", "true");
  });
});
