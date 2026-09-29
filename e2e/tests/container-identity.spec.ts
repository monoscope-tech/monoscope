import { test, expect } from "@playwright/test";
import { DEMO_PROJECT, sql } from "./helpers";

const CONTAINER = "e2e-duplicate-container";
const CLUSTER = "8b1deb4d-3b7d-4bad-9bdd-2b0d7b3dcb6d";
const identities = [
  ["identity-a", CLUSTER, "identity-node-a", "identity-pod"],
  ["identity-b", CLUSTER, "identity-node-a", "identity-pod"],
  ["identity-a", "9b1deb4d-3b7d-4bad-9bdd-2b0d7b3dcb6d", "identity-node-a", "identity-pod"],
  ["identity-a", CLUSTER, "identity-node-b", "identity-pod"],
  ["identity-a", CLUSTER, "identity-node-a", "identity-pod-b"],
];
const cleanup = `DELETE FROM otel_metrics WHERE project_id='${DEMO_PROJECT}' AND resource->'k8s'->'container'->>'name'='${CONTAINER}';`;
test.beforeAll(() => {
  if (!process.env.E2E_BASE_URL) return;
  sql(cleanup + identities.map(([namespace, cluster, node, pod], index) => `
  INSERT INTO otel_metrics (project_id,id,series_id,timestamp,metric_name,metric_type,value,resource___k8s___container___name,resource___k8s___pod___name,resource___k8s___namespace___name,resource)
  VALUES ('${DEMO_PROJECT}',gen_random_uuid(),'e2e-container-${index}','2024-12-31T23:59:00Z','container.cpu.usage','GAUGE',${index + 1},'${CONTAINER}','${pod}','${namespace}',
    '${JSON.stringify({ k8s: { container: { name: CONTAINER }, pod: { name: pod }, namespace: { name: namespace }, cluster: { uid: cluster }, node: { name: node } } })}');`).join(""));
});
test.afterAll(() => {
  if (process.env.E2E_BASE_URL) sql(cleanup);
});

test.describe.configure({ mode: "serial" });

for (const width of [390, 1280]) {
  test(`duplicate containers expose their identity before opening a drawer at ${width}px`, async ({ page }) => {
    await page.setViewportSize({ width, height: 900 });
    await page.goto(`/p/${DEMO_PROJECT}/infrastructure/containers?from=2024-12-31T23:00:00Z&to=2025-01-01T00:00:00Z`, { waitUntil: "domcontentloaded" });
    await expect(page.locator("[data-deferred-shell]")).toHaveCount(0);
    const rows = page.locator(`tr[role="button"][data-hx-get*="container=${CONTAINER}"]`);
    await expect(rows).toHaveCount(identities.length);
    const visible = await rows.evaluateAll(rows => rows.flatMap(row => {
      const params = new URL(row.getAttribute("data-hx-get")!, "http://localhost").searchParams;
      const metadata = Array.from(row.querySelector("td")!.querySelectorAll("span"));
      return [["pod", "Pod"], ["namespace", "Namespace"], ["cluster", "Cluster"], ["node", "Node / host"]].map(([parameter, label]) => {
        const value = metadata.find(element => element.textContent === `${label}: ${params.get(parameter)}`);
        return value ? value.getClientRects().length > 0 : null;
      });
    }));
    expect(visible).toEqual(Array(identities.length * 4).fill(width < 1024));
    await expect.poll(() => page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth)).toBe(true);
  });
}

test("a Kubernetes namespace drilldown keeps containers within its cluster", async ({ page }) => {
  await page.goto(`/p/${DEMO_PROJECT}/infrastructure/kubernetes?resource=namespaces&from=2024-12-31T23:00:00Z&to=2025-01-01T00:00:00Z`, { waitUntil: "domcontentloaded" });
  await expect(page.locator("[data-deferred-shell]")).toHaveCount(0);
  const namespace = page.locator(`tr[role="button"][data-hx-get*="name=identity-a"][data-hx-get*="cluster=${CLUSTER}"]`);
  await namespace.locator("td").first().click();
  const drawer = page.locator("#global-data-drawer-content");
  await expect(drawer.getByText(new RegExp(`^Cluster:\\s*${CLUSTER}$`))).toBeVisible();
  const containers = drawer.getByRole("link", { name: "View containers", exact: true });
  expect(new URL((await containers.getAttribute("href"))!, "http://localhost").searchParams.get("cluster")).toBe(CLUSTER);
  await containers.click();
  await expect(page.locator("[data-deferred-shell]")).toHaveCount(0);
  const rows = page.locator(`tr[role="button"][data-hx-get*="container=${CONTAINER}"]`);
  await expect(rows).toHaveCount(3);
  expect(await rows.evaluateAll((rows, cluster) => rows.every(row => new URL(row.getAttribute("data-hx-get")!, "http://localhost").searchParams.get("cluster") === cluster), CLUSTER)).toBe(true);
});

for (const width of [390, 1280]) {
  test(`container inventory leaves scoped charts in the selected drawer at ${width}px`, async ({ page }) => {
    await page.setViewportSize({ width, height: 900 });
    const chartRequests: string[] = [];
    page.on("request", request => { if (new URL(request.url()).pathname.startsWith("/chart_data")) chartRequests.push(request.url()); });
    await page.goto(`/p/${DEMO_PROJECT}/infrastructure/containers?namespace=identity-b&from=2024-12-31T23:00:00Z&to=2025-01-01T00:00:00Z`, { waitUntil: "domcontentloaded" });
    await expect(page.locator("[data-deferred-shell]")).toHaveCount(0);
    const rows = page.locator(`tr[role="button"][data-hx-get*="container=${CONTAINER}"]`);
    await expect(rows).toHaveCount(1);
    await expect(rows.locator("td").nth(4)).toHaveText("2.000");
    await expect(page.locator("#containersContainer [data-widget]")).toHaveCount(0);
    expect(chartRequests).toHaveLength(0);
    expect(await page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth)).toBe(true);
    const cpuResponse = page.waitForResponse(response => {
      const query = new URL(response.url()).searchParams.get("query");
      return response.url().includes("/chart_data") && (query?.includes("container.cpu.usage") ?? false);
    });
    await rows.locator("td").first().click();
    const drawer = page.locator("#global-data-drawer-content");
    await expect(drawer.getByText(/^Namespace:\s*identity-b$/)).toBeVisible();
    const url = new URL((await cpuResponse).url());
    expect(url.searchParams.get("from")).toBe("2024-12-31T23:00:00Z");
    expect(url.searchParams.get("to")).toBe("2025-01-01T00:00:00Z");
    const response = await page.request.get(url.toString().replace("/chart_data/stream", "/chart_data"));
    expect(response.ok()).toBe(true);
    const data = await response.json();
    expect(data.error ?? null).toBeNull();
    expect(data.dataset.map((row: number[]) => row.slice(1))).toEqual([[2]]);
    await expect(drawer.locator("#container-detail-cpu canvas")).toBeVisible();
  });
}

for (const theme of ["dark", "light"]) {
  test(`a lone container metric observation has a visible point in ${theme} appearance`, async ({ page }) => {
    await page.setViewportSize({ width: 1280, height: 900 });
    await page.goto(`/p/${DEMO_PROJECT}/infrastructure/containers?namespace=identity-b&from=2024-12-31T23:00:00Z&to=2025-01-01T00:00:00Z`, { waitUntil: "domcontentloaded" });
    await expect(page.locator("[data-deferred-shell]")).toHaveCount(0);
    if (await page.locator("body").getAttribute("data-theme") !== theme) await page.evaluate(() => (window as any).toggleDarkMode());
    await page.locator(`tr[role="button"][data-hx-get*="container=${CONTAINER}"] td`).first().click();
    await expect(page.locator("#global-data-drawer-content #container-detail-cpu canvas")).toBeVisible();
    await expect(page.locator("#container-detail-cpu")).toHaveAttribute("aria-busy", "false");
    const rendered = await page.evaluate(() => {
      const chart = (window as any).echarts.getInstanceByDom(document.getElementById("container-detail-cpu"));
      const data = chart.getModel().getSeriesByIndex(0)?.getData();
      if (!data) return { values: [], symbols: 0 };
      const dimension = data.mapDimension("y");
      const values = Array.from({ length: data.count() }, (_, index) => data.get(dimension, index)).filter(Number.isFinite);
      const symbols = Array.from({ length: data.count() }, (_, index) => data.getItemGraphicEl(index)).filter(element => element && !element.ignore && element.getBoundingRect().width > 0 && element.getBoundingRect().height > 0).length;
      return { values, symbols };
    });
    expect(rendered.values).toEqual([2]);
    expect(rendered.symbols).toBe(1);
    const point = await page.evaluate(() => {
      const element = document.getElementById("container-detail-cpu")!;
      const chart = (window as any).echarts.getInstanceByDom(element);
      const pixel = chart.convertToPixel({ seriesIndex: 0 }, chart.getOption().dataset[0].source[1]);
      const bounds = element.getBoundingClientRect();
      return { x: bounds.left + pixel[0], y: bounds.top + pixel[1] };
    });
    await page.mouse.move(point.x, point.y + 1);
    await expect(page.getByText("2 cores", { exact: true })).toBeVisible();
  });
}

test("mobile namespace facet preserves the selected cluster", async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 900 });
  await page.goto(`/p/${DEMO_PROJECT}/infrastructure/containers?cluster=${CLUSTER}&from=2024-12-31T23:00:00Z&to=2025-01-01T00:00:00Z`, { waitUntil: "domcontentloaded" });
  await expect(page.locator("[data-deferred-shell]")).toHaveCount(0);
  const rows = page.locator(`tr[role="button"][data-hx-get*="container=${CONTAINER}"]`);
  await expect(rows).toHaveCount(4);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth)).toBe(true);
  await page.locator("#containersContainer summary").filter({ hasText: /^Filters$/ }).focus();
  await page.keyboard.press("Enter");
  await page.locator('[data-component="facet-section"] summary').filter({ hasText: /^Namespace$/ }).focus();
  await page.keyboard.press("Enter");
  const changed = page.waitForResponse(response => new URL(response.url()).searchParams.get("namespace") === "identity-a");
  await page.getByRole("radio", { name: "identity-a", exact: true }).focus();
  await page.keyboard.press("Space");
  const url = new URL((await changed).url());
  await expect(rows).toHaveCount(3);
  expect(url.searchParams.getAll("cluster")).toEqual([CLUSTER]);
  expect(url.searchParams.getAll("namespace")).toEqual(["identity-a"]);
  expect(url.searchParams.get("from")).toBe("2024-12-31T23:00:00Z");
  expect(url.searchParams.get("to")).toBe("2025-01-01T00:00:00Z");
  expect(await rows.evaluateAll((rows, cluster) => rows.every(row => new URL(row.getAttribute("data-hx-get")!, "http://localhost").searchParams.get("cluster") === cluster), CLUSTER)).toBe(true);
  await page.locator("#containersContainer summary").filter({ hasText: /^Filters$/ }).focus();
  await page.keyboard.press("Enter");
  const replaced = page.waitForResponse(response => new URL(response.url()).searchParams.get("namespace") === "identity-b");
  await page.getByRole("radio", { name: "identity-b", exact: true }).focus();
  await page.keyboard.press("Space");
  const replacedUrl = new URL((await replaced).url());
  await expect(rows).toHaveCount(1);
  expect(replacedUrl.searchParams.getAll("namespace")).toEqual(["identity-b"]);
  expect(replacedUrl.searchParams.getAll("cluster")).toEqual([CLUSTER]);
  await page.getByRole("button", { name: "Remove filter Namespace: identity-b", exact: true }).focus();
  await page.keyboard.press("Enter");
  await expect(rows).toHaveCount(4);
  await expect(page.getByRole("button", { name: `Remove filter Cluster: ${CLUSTER}`, exact: true })).toBeVisible();
  await page.locator("#containersContainer summary").filter({ hasText: /^Filters$/ }).focus();
  await page.keyboard.press("Enter");
  const cleared = page.waitForResponse(response => new URL(response.url()).pathname.endsWith("/infrastructure/containers"));
  await page.getByRole("button", { name: "Clear all", exact: true }).focus();
  await page.keyboard.press("Enter");
  const clearUrl = new URL((await cleared).url());
  await expect(rows).toHaveCount(5);
  for (const key of ["runtime", "cluster", "namespace", "node", "image"]) expect(clearUrl.searchParams.has(key)).toBe(false);
  expect(clearUrl.searchParams.get("from")).toBe("2024-12-31T23:00:00Z");
  expect(clearUrl.searchParams.get("to")).toBe("2025-01-01T00:00:00Z");
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth)).toBe(true);
});

test("mobile container search remains reachable and preserves drawer identity", async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 900 });
  await page.goto(`/p/${DEMO_PROJECT}/infrastructure/containers?cluster=${CLUSTER}&from=2024-12-31T23:00:00Z&to=2025-01-01T00:00:00Z`, { waitUntil: "domcontentloaded" });
  await expect(page.locator("[data-deferred-shell]")).toHaveCount(0);
  const rows = page.locator(`tr[role="button"][data-hx-get*="container=${CONTAINER}"]:visible`);
  await expect(rows).toHaveCount(4);
  const search = page.getByPlaceholder("Search containers", { exact: true });
  await expect(search).toBeVisible();
  expect(await search.evaluate(input => parseFloat(getComputedStyle(input).fontSize))).toBeGreaterThanOrEqual(16);
  await search.focus();
  await expect(search).toBeFocused();
  await page.keyboard.type("identity-node-b");
  await expect(rows).toHaveCount(1);
  await expect(rows.locator("td").nth(4)).toHaveText("4.000");
  await page.keyboard.press("ControlOrMeta+A");
  await page.keyboard.press("Backspace");
  await expect(rows).toHaveCount(4);
  await page.keyboard.type("identity-node-b");
  await expect(rows).toHaveCount(1);
  const url = new URL(page.url());
  expect(url.searchParams.get("cluster")).toBe(CLUSTER);
  expect(url.searchParams.get("from")).toBe("2024-12-31T23:00:00Z");
  expect(url.searchParams.get("to")).toBe("2025-01-01T00:00:00Z");
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth)).toBe(true);
  await rows.focus();
  await page.keyboard.press("Enter");
  const drawer = page.locator("#global-data-drawer-content");
  await expect(drawer.getByText(/^Node \/ host:\s*identity-node-b$/)).toBeVisible();
  await expect(drawer.getByText(/^Namespace:\s*identity-a$/)).toBeVisible();
});
