import { test, expect, Page } from "@playwright/test";
import { DEMO_PROJECT } from "./helpers";

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
    await expect(chips.getByText("HTTP 5xx responses")).toBeVisible();
    await chips.getByText("Show errors").click();
    await expect.poll(() => page.locator("#filterElement").evaluate((el: any) => el.getValue())).toContain('level == "ERROR"');

    await page.keyboard.press("Escape");
    await page.getByRole("button", { name: "Library" }).click();
    const library = page.locator("#queryLibraryPopover");
    await expect(library).toBeVisible();
    for (const tab of ["Popular", "Saved", "Recent"])
      await expect(library.getByRole("tab", { name: tab })).toBeVisible();
  });
});
