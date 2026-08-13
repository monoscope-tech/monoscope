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
 * therefore always empty. Since a service is a hard gate, Live Tail could not be started at
 * all, and every server-side test still passed.
 *
 * So the assertions here are deliberately about *seams*, not logic: the page mounts, the
 * selector fills from the live schema response, and the gate refuses an empty selection. Row
 * streaming is not asserted — that needs ingest traffic, and the server-side specs already pin
 * matching and delivery.
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

    await expect(component.locator("[data-service] option")).toHaveText([/Select a service/, "checkout", "billing"]);
    await expect(component.locator("[data-environment] option")).toHaveText([/All environments/, "prod"]);
  });

  test("refuses to start without a service, and says why", async ({ page }) => {
    await page.goto(LIVE_TAIL_URL, { waitUntil: "domcontentloaded" });
    const component = page.locator("live-tail");
    await expect(component).toBeVisible();

    await component.getByRole("button", { name: "Start tail" }).click();

    // The gate is what bounds Live Tail's volume, so a silent no-op here would be worse than
    // an error: the user would think the tail was running.
    await expect(component).toContainText(/select a service/i);
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
