import { test, expect, Page } from "@playwright/test";
import { DEMO_PROJECT } from "./helpers";

// Pages that between them pull in every lazily-imported chunk (log-list,
// query-editor, session-replay, widgets).
const PAGES = ["log_explorer", "dashboards", "apis", "endpoints"].map(
  (p) => `/p/${DEMO_PROJECT}/${p}`,
);

const BUNDLE = /\/web-components\/dist\/(js|assets)\/.*\.js/;

// Data-layer failures (a dropped pool connection, a slow query) are the backend's
// problem and are surfaced to the user by the page itself. This spec is about
// client-side integrity, so they must not turn it red.
const TRANSIENT = /Query execution failed|Failed to load resource|net::ERR_/;

/**
 * A bundle's logical name: basename minus the query and the content hash Rollup
 * appends (`log-list.CG7UWd99.js` and `log-list.Bkh4dgi-.js` are both `log-list`).
 * Comparing URLs is not enough — two module graphs built from different revisions
 * collide on name while every URL stays unique.
 */
const bundleName = (url: string) =>
  new URL(url).pathname
    .split("/")
    .pop()!
    .replace(/\.js$/, "")
    .replace(/[.-][A-Za-z0-9_-]{8}$/, "");

/** Load a page and collect its JS requests plus anything the console/runtime complained about. */
async function load(page: Page, url: string) {
  const requests: string[] = [];
  const errors: string[] = [];
  page.on("request", (r) => BUNDLE.test(r.url()) && requests.push(r.url()));
  page.on("pageerror", (e) => errors.push(String(e)));
  page.on("console", (m) => m.type() === "error" && errors.push(m.text()));

  await page.goto(url, { waitUntil: "networkidle" });
  // Lazy chunks are imported on htmx:afterSettle, after the initial network settles.
  await page.waitForTimeout(2000);

  return { requests, errors: errors.filter((e) => !TRANSIENT.test(e)) };
}

for (const url of PAGES) {
  // A bundle fetched twice means it was evaluated twice — two module graphs for one
  // document. That silently duplicates every custom element registration, worker and
  // htmx listener in it. Regression guard for the ?v=-on-the-Vite-entry bug, where
  // chunks importing the entry back as a bare ./index.js produced a second graph.
  test(`each bundle loads exactly once on ${url}`, async ({ page }) => {
    const { requests } = await load(page, url);
    const byName = new Map<string, string[]>();
    for (const u of requests)
      byName.set(bundleName(u), (byName.get(bundleName(u)) ?? []).concat(u));
    const twice = [...byName.values()].filter((us) => us.length > 1);
    expect(twice, `loaded more than once:\n${twice.join("\n")}`).toEqual([]);
  });

  test(`no client-side errors on ${url}`, async ({ page }) => {
    const { errors } = await load(page, url);
    expect(errors, errors.join("\n")).toEqual([]);
  });
}

// The entry must be cache-busted by its filename, never a ?v= query: Rollup emits
// cross-chunk imports back into the entry as a bare ./index.<hash>.js, so a queried
// entry URL is a second module identity for the same file.
test("the module entry is hashed in its filename, not a query", async ({
  page,
}) => {
  await page.goto(PAGES[0], { waitUntil: "domcontentloaded" });
  const src = await page
    .locator('script[type="module"][src*="web-components"]')
    .getAttribute("src");
  expect(src).toMatch(/\/js\/index\.[A-Za-z0-9_-]+\.js$/);
});
