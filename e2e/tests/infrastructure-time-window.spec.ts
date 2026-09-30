import { test, expect } from "@playwright/test";
import { DEMO_PROJECT, sql } from "./helpers";

const namespace = "e2e-infra-preset";
const cluster = "8b1deb4d-3b7d-4bad-9bdd-2b0d7b3dcb6d";
const provider = "e2e-infra-provider";
const cleanup = `DELETE FROM otel_metrics WHERE project_id='${DEMO_PROJECT}' AND resource->'k8s'->'namespace'->>'name'='${namespace}';`;
test.beforeAll(async ({ request }) => {
  if (!process.env.E2E_BASE_URL) return;
  sql(cleanup + [30, 600].map(age => `INSERT INTO otel_metrics (project_id,id,series_id,timestamp,metric_name,metric_type,value,resource___k8s___container___name,resource___k8s___pod___name,resource___k8s___namespace___name,resource)
    VALUES ('${DEMO_PROJECT}',gen_random_uuid(),'infra-preset-${age}','${new Date(Date.now() - age * 1000).toISOString()}','container.cpu.usage','GAUGE',1,'infra-preset-${age}','infra-preset-pod-${age}','${namespace}',
      '${JSON.stringify({ k8s: { container: { name: `infra-preset-${age}` }, pod: { name: `infra-preset-pod-${age}` }, namespace: { name: namespace }, cluster: { uid: cluster }, node: { name: `infra-preset-host-${age}` } }, container: { image: { name: `registry.example/infra-preset-${age}` } }, cloud: { provider, region: "e2e-infra-region" } })}');`).join(""));
  // Match the page's raw cache key; earlier fixture pages omit from/to.
  await expect.poll(async () => {
    const response = await request.get(`/p/${DEMO_PROJECT}/infrastructure/hosts?${new URLSearchParams({ provider, group: "region", from: "", to: "", since: "5m", deferred: "1" })}`);
    expect(response.status()).toBe(200);
    return (await response.text()).includes("infra-preset-host-30");
  }, { timeout: 40_000 }).toBe(true);
});
test.afterAll(() => { if (process.env.E2E_BASE_URL) sql(cleanup); });

const views = [
  ["hosts", "hostsContainer", { provider, group: "region" }, "Search hosts"],
  ["images", "imagesContainer", { registry: "registry.example", runtime: "kubernetes" }, "Search images"],
  ["kubernetes", "kubernetesContainer", { resource: "pods", namespace, cluster }, "Search pods"],
  ["host-map", "hostMapContainer", { provider, group: "region", fill: "memory" }, ""],
] as const;
for (const [route, container, filters, searchName] of views) {
  test(`${route} time presets refresh scoped inventory`, async ({ page }) => {
    test.skip(!process.env.E2E_BASE_URL, "Requires a disposable fixture database");
    await page.setViewportSize({ width: 390, height: 900 });
    const url = `/p/${DEMO_PROJECT}/infrastructure/${route}?${new URLSearchParams({ ...filters, from: "", to: "", since: "5m" })}`;
    await page.goto(url, { waitUntil: "domcontentloaded" });
    await expect(page.locator("[data-deferred-shell]")).toHaveCount(0, { timeout: 5_000 });
    const entries = page.locator(`#${container} ${route === "host-map" ? "button[data-hx-get]" : 'tr[role="button"]'}`);
    await expect(entries).toHaveCount(1);
    await page.locator('[popovertarget="n-timepicker-popover"]').click();
    await page.locator("[data-mobile-live-toggle]").click();
    await expect(page.locator("[data-time-transport]")).toHaveAttribute("data-interval", "0");
    await page.clock.install();
    const refresh = page.waitForRequest(request => { const url = new URL(request.url()); return url.pathname.endsWith(`/infrastructure/${route}`) && url.searchParams.get("deferred") === "1" && url.searchParams.get("since") === "1H"; });
    await page.locator('#n-timepicker-popover button[data-value="1H"]').click();
    await expect(page).toHaveURL(/since=1H/);
    await expect(entries).toHaveCount(2);
    const params = new URL((await refresh).url()).searchParams;
    expect(params.getAll("since")).toEqual(["1H"]);
    expect(params.getAll("from")).toEqual([""]);
    expect(params.getAll("to")).toEqual([""]);
    for (const [key, value] of Object.entries(filters)) expect(params.getAll(key)).toEqual([value]);
    expect(await entries.evaluateAll(rows => rows.every(row => new URL(row.getAttribute("data-hx-get")!, location.origin).searchParams.get("since") === "1H"))).toBe(true);
    const focusTarget = searchName ? page.getByRole("textbox", { name: searchName, exact: true }) : page.locator(`#${container} select[name="fill"]`);
    if (searchName) await focusTarget.fill("600");
    for (const [preset, count] of [["5M", 1], ["1H", 2]] as const) {
      await page.locator('[popovertarget="n-timepicker-popover"]').click();
      await page.locator(`#n-timepicker-popover button[data-value="${preset}"]`).click();
      await expect(entries).toHaveCount(count);
      if (searchName) {
        await expect(focusTarget).toHaveValue("600");
        await expect(page.locator(`#${container} tr[role="button"]:visible`)).toHaveCount(preset === "1H" ? 1 : 0);
      }
      expect(new URL((await entries.first().getAttribute("data-hx-get"))!, page.url()).searchParams.get("since")).toBe(preset);
    }
    if (route === "hosts" || route === "host-map") await expect(page.locator(`#${container} select[name="group"]`)).toHaveValue("region");
    if (route === "host-map") await expect(focusTarget).toHaveValue("memory");
    await page.locator('[popovertarget="n-timepicker-popover"]').click();
    await page.locator("[data-mobile-live-toggle]").click();
    await expect(page.locator("[data-time-transport]")).toHaveAttribute("data-interval", "15000");
    await page.locator('[popovertarget="n-timepicker-popover"]').click();
    await focusTarget.focus();
    const tick = page.waitForResponse(response => {
      const url = new URL(response.url());
      return url.pathname.endsWith(`/infrastructure/${route}`) && url.searchParams.get("deferred") === "1" && url.searchParams.get("since") === "1H";
    });
    await page.clock.fastForward(15_000);
    await tick;
    await expect(focusTarget).toBeFocused();
    await expect(focusTarget).toHaveValue(searchName ? "600" : "memory");
    if (searchName) await expect(page.locator(`#${container} tr[role="button"]:visible`)).toHaveCount(1);
    await page.locator('[popovertarget="n-timepicker-popover"]').click();
    await page.getByRole("button", { name: "Previous time window", exact: true }).click();
    await expect.poll(() => new URL(page.url()).searchParams.get("from")).toBeTruthy();
    await expect(entries).toHaveCount(0);
    await page.locator('#n-timepicker-popover button[data-value="1H"]').click();
    await expect(entries).toHaveCount(2);
    const restored = new URL((await entries.first().getAttribute("data-hx-get"))!, page.url()).searchParams;
    expect(restored.get("since")).toBe("1H");
    expect(restored.get("from") || restored.get("to")).toBeFalsy();
  });
  test(`${route} time presets replace held initial inventory`, async ({ page }) => {
    test.skip(!process.env.E2E_BASE_URL, "Requires a disposable fixture database");
    await page.setViewportSize({ width: 390, height: 900 });
    let release = () => {};
    const held = new Promise<void>(resolve => { release = resolve; });
    await page.route(`**/infrastructure/${route}?**`, async route => {
      const params = new URL(route.request().url()).searchParams;
      if (params.get("deferred") === "1" && params.get("since") === "5M") await held;
      await route.continue();
    });
    try {
      const initial = page.waitForRequest(request => new URL(request.url()).searchParams.get("deferred") === "1");
      await page.goto(`/p/${DEMO_PROJECT}/infrastructure/${route}?${new URLSearchParams({ ...filters, since: "5M" })}`, { waitUntil: "domcontentloaded" });
      await initial;
      await page.locator('[popovertarget="n-timepicker-popover"]').click();
      await page.locator("[data-mobile-live-toggle]").click();
      await expect(page.locator("[data-time-transport]")).toHaveAttribute("data-interval", "0");
      await page.locator('#n-timepicker-popover button[data-value="1H"]').click();
      release();
      await expect(page.locator("[data-deferred-shell]")).toHaveCount(0, { timeout: 5_000 });
      const entries = page.locator(`#${container} ${route === "host-map" ? "button[data-hx-get]" : 'tr[role="button"]'}`);
      await expect(entries).toHaveCount(2);
      expect(new URL((await entries.first().getAttribute("data-hx-get"))!, page.url()).searchParams.get("since")).toBe("1H");
    } finally { release(); }
  });
}

for (const [route, container, filters] of [...views, ["containers", "containersContainer", { namespace, cluster }]] as const) {
  test(`${route} initial inventory does not wait for application script arrival`, async ({ page }) => {
    test.skip(!process.env.E2E_BASE_URL, "Requires a disposable fixture database");
    let release = () => {};
    const held = new Promise<void>(resolve => { release = resolve; });
    await page.route(/\/public\/assets\/(?:js\/main\.|web-components\/dist\/js\/index\.).*\.js$/, async route => { await held; await route.continue(); });
    try {
      await page.goto(`/p/${DEMO_PROJECT}/infrastructure/${route}?${new URLSearchParams({ ...filters, from: "", to: "", since: "5M" })}&since=5m`, { waitUntil: "commit" });
      const entries = page.locator(`#${container} ${route === "host-map" ? "button[data-hx-get]" : 'tr[role="button"]'}`);
      await page.waitForFunction(() => Boolean((window as any).htmx && document.querySelector("[data-deferred-shell]")));
      await expect.poll(() => page.evaluate(() => typeof (window as any).params)).toBe("undefined");
      // Process the actual shell before deferred application globals arrive.
      await page.evaluate(() => (window as any).htmx.process(document.querySelector("[data-deferred-shell]")));
      await expect(entries).toHaveCount(1);
      expect(new URL((await entries.first().getAttribute("data-hx-get"))!, page.url()).searchParams.get("since")).toBe("5m");
    } finally { release(); }
  });
}
