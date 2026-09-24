import { test, expect } from "@playwright/test";
import { DEMO_PROJECT } from "./helpers";

test("session search survives filter changes and keeps the replay workspace", async ({ page }) => {
  await page.goto(`/p/${DEMO_PROJECT}/rum?tab=sessions`);
  const search = page.getByRole("searchbox", { name: "Search sessions" });
  const filters = page.getByRole("navigation", { name: "Filter sessions" });
  await expect(filters).toBeVisible();
  await page.keyboard.press("/");
  await expect(search).toBeFocused();
  await search.fill("e2e-missing-session");
  await expect(page.getByText("No sessions match this filter", { exact: true })).toBeVisible();
  const workspace = page.locator("#rum-replay-workspace");
  await expect(workspace).toBeVisible();
  await workspace.evaluate(element => element.setAttribute("data-e2e-preserved", "true"));
  for (const [label, filter] of [["With errors", "errors"], ["With replay", "replays"], ["All sessions", ""]]) {
    const response = page.waitForResponse(r => {
      const url = new URL(r.url());
      return url.pathname.endsWith("/rum") && url.searchParams.get("filter") === filter && url.searchParams.get("q") === "e2e-missing-session";
    });
    await filters.getByRole("link", { name: label, exact: true }).click();
    expect((await response).status()).toBe(200);
    await expect(filters.getByRole("link", { name: label, exact: true })).toHaveAttribute("aria-current", "page");
    await expect(search).toHaveValue("e2e-missing-session");
    await expect(workspace).toHaveAttribute("data-e2e-preserved", "true");
  }
});
