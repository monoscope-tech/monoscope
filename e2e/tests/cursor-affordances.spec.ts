import { test, expect } from "@playwright/test";
import { DEMO_PROJECT } from "./helpers";

const routes = [
  "live_tail",
  "log_explorer",
  "dashboards",
  "monitors",
  "containers",
  "infrastructure/hosts",
  "endpoints",
  "issues",
  "settings",
  "service_map",
];

test("visible controls use a pointer cursor across the app", async ({ page }) => {
  const failures: string[] = [];

  for (const route of routes) {
    await page.goto(`/p/${DEMO_PROJECT}/${route}`, { waitUntil: "domcontentloaded" });
    await page.waitForTimeout(500);
    failures.push(
      ...(await page.evaluate(() => {
        const selector = [
          "button:not([disabled]):not(.cursor-not-allowed):not(.cursor-default)",
          "summary",
          "select:not([disabled])",
          '[role="button"]:not([aria-disabled="true"]):not(.cursor-not-allowed):not(.cursor-default)',
        ].join(",");
        return [...document.querySelectorAll<HTMLElement>(selector)]
          .filter(element => {
            const box = element.getBoundingClientRect();
            const style = getComputedStyle(element);
            return box.width > 0 && box.height > 0 && style.display !== "none" && style.visibility !== "hidden" && style.pointerEvents !== "none" && style.cursor !== "pointer";
          })
          .map(element => `${location.pathname}: <${element.tagName.toLowerCase()}> ${(element.getAttribute("aria-label") || element.textContent || "").trim().replace(/\s+/g, " ").slice(0, 60)}`);
      }))
    );
  }

  expect(failures).toEqual([]);
});
