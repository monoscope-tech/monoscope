import { test, expect } from "@playwright/test";
import { DEMO_PROJECT } from "./helpers";

// The Exemplars tab is fetched on first reveal rather than with the page: an
// exemplar lookup is a text scan over raw metric rows and most visits never open
// it. That laziness is the fragile part — the panel is a DaisyUI `.tab-content`,
// hidden until its radio is checked, and a display:none container never fires
// `intersect`. So the fetch hangs off the radio's own `change` via htmx `from:`;
// if that binding regresses the panel hangs at aria-busy forever. Only a real
// browser can prove it — the reveal is CSS plus an htmx swap.
//
// MUST open it from the metrics LIST, not by navigating to the detail URL.
// `/metrics/details/<name>` is an htmx fragment that the list loads into the
// global drawer; fetched directly it returns a bare partial with no
// BodyWrapper shell, so `htmx` is undefined and nothing wires up. That looks
// exactly like a broken page and is not one.
//
// READ-ONLY: navigates and clicks. Safe against a server pointed at the
// production database, which is how it reaches a metric that has exemplars.
// Any metric will do — the tab wiring is per-page, not per-metric. The list
// renders each metric as a widget whose expand button loads the detail
// fragment into the global drawer; that button is the only entry point.
const EXPAND = 'button[data-tippy-content="Expand widget"]';

test.describe("metric detail — Exemplars tab", () => {
  test("lazy fragment reveals on tab click and clears aria-busy", async ({ page }) => {
    const errors: string[] = [];
    page.on("pageerror", e => errors.push(e.message));

    await page.goto(`/p/${DEMO_PROJECT}/metrics?since=14d`);
    // The shell must be live, or the tab handlers below cannot exist.
    await expect
      .poll(() => page.evaluate(() => typeof (window as any).htmx), { timeout: 20000 })
      .toBe("object");

    await page.locator(EXPAND).first().click();

    const panel = page.locator("#ex-content");
    await expect(panel).toHaveCount(1, { timeout: 20000 });
    await expect(panel).toBeHidden();
    await expect(panel).toHaveAttribute("aria-busy", "true");

    await page.getByRole("tab", { name: "Exemplars", exact: true }).check();

    await expect(panel).toBeVisible({ timeout: 20000 });
    await expect(panel).not.toHaveAttribute("aria-busy", "true", { timeout: 30000 });
    expect((await panel.innerHTML()).trim().length).toBeGreaterThan(0);
    expect(errors.join("\n")).not.toMatch(/is not defined|is not a function/i);
  });
});
