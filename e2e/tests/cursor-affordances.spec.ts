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
    // Pages whose body is deferred answer first with a skeleton. Auditing that would check
    // nothing, so wait for the real body before looking at any control.
    await expect(page.locator("[data-deferred-shell]")).toHaveCount(0);
    await page.waitForTimeout(500);
    failures.push(
      ...(await page.evaluate(() => {
        const selector = [
          // `aria-disabled` counts as disabled here exactly as it does for `role="button"`
          // below: a control in that state correctly renders `cursor-not-allowed`, and it
          // does so through the `aria-disabled:` variant, which the `.cursor-not-allowed`
          // class exclusion cannot see.
          'button:not([disabled]):not([aria-disabled="true"]):not(.cursor-not-allowed):not(.cursor-default)',
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

test("closed global scope menus cannot intercept log row actions", async ({ page }) => {
  await page.goto(`/p/${DEMO_PROJECT}/log_explorer`, { waitUntil: "domcontentloaded" });

  await page.evaluate(() => {
    const menu = document.createElement("ul");
    menu.id = "closed-scope-menu-fixture";
    menu.popover = "auto";
    menu.className = "dropdown menu flex flex-col";
    menu.innerHTML = "<li><button>checkout</button></li>";
    document.body.append(menu);
  });
  const serviceMenu = page.locator("#closed-scope-menu-fixture");
  await expect(serviceMenu).toBeHidden();
});

test("log query controls use the quiet resting border", async ({ page }) => {
  await page.goto(`/p/${DEMO_PROJECT}/log_explorer`, { waitUntil: "domcontentloaded" });

  const colors = await page.evaluate(() => {
    const probe = document.body.appendChild(document.createElement("span"));
    probe.style.color = "var(--color-strokeWeak)";
    const weak = getComputedStyle(probe).color;
    probe.remove();
    return {
      weak,
      actual: ["#queryBox", "#queryBuilder > div", "#spans-toggle"].map(selector =>
        getComputedStyle(document.querySelector(selector)!).borderTopColor
      ),
    };
  });

  expect(colors.actual).toEqual([colors.weak, colors.weak, colors.weak]);
});
