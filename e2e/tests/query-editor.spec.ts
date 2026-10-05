import { test, expect, Page } from "@playwright/test";
import { DEMO_PROJECT, sql } from "./helpers";

const LOG_EXPLORER_URL = `/p/${DEMO_PROJECT}/log_explorer`;

async function waitForEditor(page: Page) {
  await page.goto(LOG_EXPLORER_URL, { waitUntil: "domcontentloaded" });
  const component = page.locator("#filterElement");
  await component.waitFor({ state: "attached", timeout: 15000 });
  await component.click().catch(() => {});
  await component.evaluate((el) => el.dispatchEvent(new FocusEvent("focusin", { bubbles: true })));
  await page.waitForFunction(() => {
    const data = (window as any).schemaManager?.getSchemaData?.("spans", document.getElementById("filterElement")?.getAttribute("project-id") || "");
    return data?.fields && Object.keys(data.fields).length > 0;
  });
  await page.keyboard.press("Escape");
}

async function suggestions(page: Page, query: string) {
  await page.locator("#filterElement").evaluate((node, text) => {
    const el = node as any;
    el.setValue(text);
    el.focusEditor();
  }, query);
  await page.keyboard.press("Control+Space");
  await expect(page.locator('#filterElement .cm-tooltip-autocomplete')).toBeVisible();
  return page.locator('#filterElement').evaluate((el: any) =>
    (window as any).schemaManager.complete(el.getValue(), el.getAttribute('query-source') || 'spans', el.getAttribute('project-id') || '', -1),
  );
}

const labels = (items: { label: string }[]) => items.map(({ label }) => label);

test("facet multi-selection uses OR within a field and AND between fields", async ({ page }) => {
  test.skip(!process.env.E2E_BASE_URL, "Requires a disposable fixture database");
  const original = sql(`SELECT doc FROM apis.schema_summary WHERE project_id='${DEMO_PROJECT}'`).toString().trim();
  const doc = {
    fields: {
      "service.name": { types: [], formats: [], category: "resource", is_enum: false },
      level: { types: [], formats: [], category: "top_level", is_enum: false },
    },
    services: ["facet-api", "facet-worker"],
    top_values_by_field: { "service.name": { distinct: 2, top: { "facet-api": 3, "facet-worker": 2 } }, level: { distinct: 1, top: { ERROR: 1 } } },
  };
  sql(`INSERT INTO apis.schema_summary (project_id,doc) VALUES ('${DEMO_PROJECT}','${JSON.stringify(doc)}') ON CONFLICT (project_id) DO UPDATE SET doc=EXCLUDED.doc`);
  try {
    await page.goto(LOG_EXPLORER_URL, { waitUntil: "domcontentloaded" });
    const api = page.getByRole("checkbox", { name: 'resource.service.name equals facet-api', exact: true });
    const worker = page.getByRole("checkbox", { name: 'resource.service.name equals facet-worker', exact: true });
    const error = page.getByRole("checkbox", { name: 'level equals ERROR', exact: true });
    await api.check();
    await worker.check();
    const query = () => page.locator("#filterElement").evaluate((el: any) => el.getValue());
    await expect.poll(query).toBe('(resource.service.name == "facet-api" or resource.service.name == "facet-worker")');
    await error.check();
    await expect.poll(query).toBe('(resource.service.name == "facet-api" or resource.service.name == "facet-worker") and level == "ERROR"');
    await api.uncheck();
    await expect(worker).toBeChecked();
    await expect(error).toBeChecked();
    await expect.poll(query).toBe('resource.service.name == "facet-worker" and level == "ERROR"');
    await worker.uncheck();
    await error.uncheck();
    await expect.poll(query).toBe('');
  } finally {
    sql(original
      ? `UPDATE apis.schema_summary SET doc='${original.replace(/'/g, "''")}' WHERE project_id='${DEMO_PROJECT}'`
      : `DELETE FROM apis.schema_summary WHERE project_id='${DEMO_PROJECT}'`);
  }
});

// Outside the describe below on purpose: its beforeEach waits for CodeMirror, and the whole
// point here is the window before CodeMirror exists. The server-rendered skeleton
// (queryEditorSkeleton_) stands in for the editor then, and it used to be 32px tall
// inside a box with room for 30 — so the row sat 2px out of line on every page load
// until the editor upgraded. Blocking the chunk is what makes that state hold still.
test("keeps the row aligned while the editor is still loading", async ({ page }) => {
  await page.route("**/query-editor*.js", (route) => route.abort());
  await page.goto(`/p/${DEMO_PROJECT}/log_explorer`, { waitUntil: "domcontentloaded" });
  await page.locator("#filterElement").waitFor({ state: "attached" });

  const geometry = await page.locator("#filterElement").evaluate((el) => ({
    editorHeight: el.parentElement!.getBoundingClientRect().height,
    controlHeight: document.getElementById("spans-toggle")!.getBoundingClientRect().height,
    hasEditor: !!el.querySelector('.cm-editor'),
  }));

  expect(geometry.hasEditor).toBe(false);
  expect(geometry.editorHeight).toBe(geometry.controlHeight);
});

test.describe("Query editor", () => {
  test.beforeEach(async ({ page }) => waitForEditor(page));

  test("clicking reopens the compact dropdown with guidance and field types", async ({ page }) => {
    const component = page.locator('#filterElement');
    await component.locator('.cm-content').click();
    const popup = component.locator('.query-completion-dropdown');
    await expect(popup).toBeVisible();
    await expect(popup.locator('.query-completion-hint')).toBeVisible();
    await expect(popup.getByRole('option', { name: /status_code.*string/ })).toBeVisible();
    await expect(popup.getByRole('link', { name: 'Syntax guide ↗' })).toBeVisible();
    const width = await component.locator('.cm-editor').evaluate(el => el.getBoundingClientRect().width);
    expect(await popup.evaluate(el => el.getBoundingClientRect().width)).toBeCloseTo(Math.min(width, 640), 0);
  });

  test("matches the query controls' height and centers its text", async ({ page }) => {
    const geometry = await page.locator("#filterElement").evaluate((el) => {
      // The bordered box is the editor's *wrapper*, not a child of it: the border moved out
      // so the Ask-AI affordance sits inside the same outline as the editor. What has to
      // line up with the controls is that visible box — the editor itself is 2px shorter,
      // being inside the border.
      const shell = el.parentElement!.getBoundingClientRect();
      const line = el.querySelector(".cm-line")!.getBoundingClientRect();
      const select = document.getElementById("spans-toggle")!.getBoundingClientRect();
      return {
        editorHeight: shell.height,
        controlHeight: select.height,
        topInset: line.top - shell.top,
        bottomInset: shell.bottom - line.bottom,
      };
    });

    expect(geometry.editorHeight).toBe(geometry.controlHeight);
    expect(Math.abs(geometry.topInset - geometry.bottomInset)).toBeLessThanOrEqual(1);
  });

  test("offers the grammar from fields through chained conditions", async ({ page }) => {
    const fields = labels(await suggestions(page, ""));
    expect(fields.slice(0, 8)).toEqual([
      "status_code", "level", "kind", "name", "duration", "timestamp", "severity", "body",
    ]);
    expect(fields.indexOf("attributes")).toBeGreaterThan(fields.indexOf("body"));

    const operators = labels(await suggestions(page, "status_code "));
    for (const [positive, negative] of [["==", "!="], ["in", "!in"], ["has", "!has"], ["contains", "!contains"]])
      expect(operators.indexOf(positive)).toBeLessThan(operators.indexOf(negative));

    expect(labels(await suggestions(page, "status_code == "))).toEqual(expect.arrayContaining(["OK", "ERROR", "UNSET"]));
    expect(labels(await suggestions(page, 'status_code == "OK" '))).toEqual(expect.arrayContaining(["and", "or", "|"]));

    const chainedFields = labels(await suggestions(page, 'status_code == "OK" and '));
    expect(chainedFields).toEqual(expect.arrayContaining(["level", "duration", "attributes"]));
    expect(chainedFields).not.toContain("==");
    expect(labels(await suggestions(page, 'status_code == "OK" and level '))).toEqual(
      expect.arrayContaining(["==", "!=", "contains"]),
    );

    const nested = labels(await suggestions(page, "resource."));
    expect(nested).toContain("service");
    expect(nested).not.toContain("status_code");
    expect(labels(await suggestions(page, "spans | status_code "))).toEqual(expect.arrayContaining(["==", "!="]));
  });

  test("selects completions with the keyboard and keeps focus in the editor", async ({ page }) => {
    const items = await suggestions(page, "stat");
    expect(items[0]).toMatchObject({ label: "status_code", insertText: "status_code " });

    await page.keyboard.press("ArrowDown");
    await expect(page.locator('#filterElement .cm-tooltip-autocomplete [role="option"]').first()).toHaveAttribute("aria-selected", "true");
    await page.keyboard.press("Enter");

    await expect.poll(() => page.locator("#filterElement").evaluate((el: any) => el.getValue())).toBe("status_code ");
    expect(await page.locator("#filterElement").evaluate((el: any) => el.contains(document.activeElement))).toBe(true);
    await expect(page.locator("#filterElement .cm-tooltip-autocomplete")).toBeVisible();
    await expect(page.locator('#filterElement .cm-tooltip-autocomplete [role="option"]', { hasText: "==" }).first()).toBeVisible();

    const insertions = Object.fromEntries((await suggestions(page, "")).map((item) => [item.label, item.insertText]));
    expect(insertions).toMatchObject({ attributes: "attributes.", context: "context.", level: "level " });
  });

  test("moves from a popular query into the library", async ({ page }) => {
    const chips = page.locator("#popular-search-chips");
    await expect(chips.getByText("Show errors")).toBeVisible();
    await expect(chips.getByText("Show 5xx responses")).toBeVisible();
    await chips.getByText("Show errors").click();
    await expect.poll(() => page.locator("#filterElement").evaluate((el: any) => el.getValue())).toContain('level == "ERROR"');

    await page.keyboard.press("Escape");
    await page.getByRole("button", { name: "Query library" }).click();
    const library = page.locator("#queryLibraryPopover");
    await expect(library).toBeVisible();
    for (const tab of ["Popular", "Saved", "Recent"])
      await expect(library.getByRole("tab", { name: tab })).toBeVisible();
  });
});

// Regression: the explorer ships without Tagify, so the monitor panel's teams picker threw
// "Tagify is not a constructor" and never initialised. Also the only coverage of creating a
// monitor from the explorer.
test("the explorer creates a monitor with a working teams picker", async ({ page }) => {
  const title = `E2E Explorer Monitor ${Date.now()}`;
  await page.goto(`/p/${DEMO_PROJECT}/log_explorer#create-alert-toggle`);
  const form = page.locator("#alert-form");
  await expect(form).toBeVisible({ timeout: 20000 });
  await expect.poll(() => page.evaluate(() => Boolean((document.getElementById("alert-form-teams") as any)?._tagifyInstance))).toBe(true);
  await expect(form.locator(".tagify__tag")).toContainText("@everyone");
  await expect(form.getByRole("checkbox", { name: "Send to all team members" })).toHaveCount(0);
  await form.locator('[name="title"]').fill(title);
  await form.locator('[name="alertThreshold"]').fill("10");
  const [response] = await Promise.all([
    page.waitForResponse(r => new URL(r.url()).pathname.endsWith("/monitors/alerts") && r.request().method() === "POST"),
    form.getByRole("button", { name: /Create monitor/ }).click(),
  ]);
  expect(response.ok()).toBe(true);
  await page.goto(`/p/${DEMO_PROJECT}/monitors`);
  await expect(page.getByText(title, { exact: true })).toBeVisible();
});
