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

// Strip only query strings: two different modules can both be named "dist".
// The served build manifest, rather than an unhashed basename, identifies assets.
const bundlePath = (url: string) => new URL(url).pathname;

/** Load a page and collect its JS requests plus anything the console/runtime complained about. */
async function load(page: Page, url: string) {
  const requests: string[] = [];
  const errors: string[] = [];
  page.on("request", (r) => BUNDLE.test(r.url()) && requests.push(r.url()));
  // The stack, not just the message: a bare "SyntaxError: Expected ',' or '}'"
  // names neither the file nor the call that threw, so a red run here says only
  // that some JSON on the page is malformed and leaves you bisecting for it.
  page.on("pageerror", (e) => errors.push(e.stack ?? String(e)));
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
    const manifestResponse = await page.request.get("/public/assets/web-components/dist/manifest.json");
    expect(manifestResponse.ok()).toBe(true);
    const manifest = await manifestResponse.json() as Record<string, { file: string; assets?: string[] }>;
    const currentAssets = new Set(Object.values(manifest).flatMap(entry =>
      [entry.file, ...(entry.assets ?? [])].map(file => `/public/assets/web-components/dist/${file}`),
    ));
    const byPath = new Map<string, string[]>();
    for (const u of requests)
      byPath.set(bundlePath(u), (byPath.get(bundlePath(u)) ?? []).concat(u));
    const twice = [...byPath.values()].filter((us) => us.length > 1);
    expect(twice, `loaded more than once:\n${twice.join("\n")}`).toEqual([]);
    // Vite lists module chunks in the manifest; worker entry files under assets/
    // are emitted separately. Duplicate requests are checked for both above.
    const stale = [...byPath.keys()].filter(path => path.includes("/dist/js/") && !currentAssets.has(path));
    expect(stale, "every module chunk must belong to the current build").toEqual([]);
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

// Page chrome (tab switching, cookies, the log-explorer field filters) lives in the bundle
// but is invoked from inline attributes Lucid renders — onclick="filterByField(...)".
// A module's top-level declarations are not globals, so the
// contract is the explicit Object.assign(window, …) in page-chrome.ts.
//
// Regression guard for a real break: when that code moved out of BodyWrapper's inline
// <script> into the bundle, a stale dist left every one of these undefined and silently
// broke tab switching across the app. Nothing failed at load — the errors only appear when
// a user clicks — so asserting the globals exist is the only cheap way to catch it.
test("page-chrome publishes the globals inline handlers call", async ({
  page,
}) => {
  await page.goto(PAGES[0], { waitUntil: "networkidle" });
  const missing = await page.evaluate(() =>
    [
      "filterByField",
      "viewFieldPatterns",
      // Inline in BodyWrapper, not the bundle — the theme script needs them mid-parse.
      "setCookie",
      "getCookie",
    ].filter((n) => typeof (window as never as Record<string, unknown>)[n] !== "function"),
  );
  expect(
    missing,
    `not on window (stale web-components build?): ${missing.join(", ")}`,
  ).toEqual([]);
});
