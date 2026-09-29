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
