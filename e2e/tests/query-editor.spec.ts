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
    const data = (window as any).schemaManager?.getSchemaData?.("spans");
    return data?.fields && Object.keys(data.fields).length > 0;
  });
  await page.keyboard.press("Escape");
}

async function suggestions(page: Page, query: string) {
  await page.locator("#filterElement").evaluate((node, text) => {
    const el = node as any;
    el.completionItems = [];
    el.editor.setValue(text);
    const model = el.editor.getModel();
    const line = model.getLineCount();
    el.editor.setPosition({ lineNumber: line, column: model.getLineMaxColumn(line) });
    el.editor.focus();
    el.triggerSuggestions();
  }, query);
  await page.waitForFunction(() => (document.getElementById("filterElement") as any)?.completionItems?.length > 0);
  return page.locator("#filterElement").evaluate((el: any) =>
    el.completionItems.map((item: any) => ({ label: item.label, insertText: item.insertText })),
  );
}

const labels = (items: { label: string }[]) => items.map(({ label }) => label);

test.describe("Query editor", () => {
  test.beforeEach(async ({ page }) => waitForEditor(page));

  test("matches the query controls' height and centers its text", async ({ page }) => {
    const geometry = await page.locator("#filterElement").evaluate((el) => {
      const shell = el.firstElementChild!.getBoundingClientRect();
      const line = el.querySelector(".view-line")!.getBoundingClientRect();
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
    await expect(page.locator('#query-suggestions [role="option"]').first()).toHaveAttribute("aria-selected", "true");
    await page.keyboard.press("Enter");

    await expect.poll(() => page.locator("#filterElement").evaluate((el: any) => el.editor.getValue())).toBe("status_code ");
    expect(await page.locator("#filterElement").evaluate((el: any) => el.editor.hasTextFocus())).toBe(true);
    await expect(page.locator("#query-suggestions")).toBeVisible();
    await expect(page.locator('#query-suggestions [role="option"]', { hasText: "==" }).first()).toBeVisible();

    const insertions = Object.fromEntries((await suggestions(page, "")).map((item) => [item.label, item.insertText]));
    expect(insertions).toMatchObject({ attributes: "attributes.", context: "context.", level: "level " });
  });

  test("moves from a popular query into the library", async ({ page }) => {
    const chips = page.locator("#popular-search-chips");
    await expect(chips.getByText("Show errors")).toBeVisible();
    await expect(chips.getByText("HTTP 5xx responses")).toBeVisible();
    await chips.getByText("Show errors").click();
    await expect.poll(() => page.locator("#filterElement").evaluate((el: any) => el.editor.getValue())).toContain('level == "ERROR"');

    await page.keyboard.press("Escape");
    await page.getByRole("button", { name: "Library" }).click();
    const library = page.locator("#queryLibraryPopover");
    await expect(library).toBeVisible();
    for (const tab of ["Popular", "Saved", "Recent"])
      await expect(library.getByRole("tab", { name: tab })).toBeVisible();
  });
});
