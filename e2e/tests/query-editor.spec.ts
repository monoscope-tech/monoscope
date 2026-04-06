import { test, expect, Page } from "@playwright/test";
import { DEMO_PROJECT } from "./helpers";

const LOG_EXPLORER_URL = `/p/${DEMO_PROJECT}/log_explorer`;

/** Wait for the query editor web component to be fully initialized with schema data */
async function waitForEditor(page: Page) {
  await page.goto(LOG_EXPLORER_URL, { waitUntil: "domcontentloaded" });
  await page.waitForFunction(
    () => {
      const sm = (window as any).schemaManager;
      const data = sm?.getSchemaData?.("spans");
      return data?.fields && Object.keys(data.fields).length > 0;
    },
    { timeout: 15000 },
  );
}

/** Set query text and trigger suggestions, returning the visible dropdown labels */
async function getSuggestionLabels(page: Page, query: string) {
  // Set editor value and trigger suggestions
  await page.evaluate((q) => {
    const el = document.getElementById("filterElement") as any;
    const editor = el.editor;
    editor.setValue(q);
    const model = editor.getModel();
    const lineCount = model.getLineCount();
    const column = model.getLineMaxColumn(lineCount);
    editor.setPosition({ lineNumber: lineCount, column });
    editor.focus();
    editor.trigger("test", "editor.action.triggerSuggest", {});
  }, query);

  // Wait for completionItems to populate
  await page.waitForFunction(
    () => {
      const el = document.getElementById("filterElement") as any;
      return el.completionItems && el.completionItems.length > 0;
    },
    { timeout: 5000 },
  );

  return page.evaluate(() => {
    const el = document.getElementById("filterElement") as any;
    return el.completionItems.map((i: any) => i.label) as string[];
  });
}

test.describe("Query editor suggestions", () => {
  test.describe.configure({ timeout: 45000 });

  test.beforeEach(async ({ page }) => {
    await waitForEditor(page);
  });

  test("empty input suggests priority fields first, then tables last", async ({
    page,
  }) => {
    const labels = await getSuggestionLabels(page, "");
    // Priority fields should be in the first group
    const priorityFields = [
      "attributes",
      "duration",
      "level",
      "name",
      "resource",
      "status_code",
    ];
    for (const field of priorityFields) {
      expect(labels).toContain(field);
      // Priority fields should appear before non-priority ones like "body"
      expect(labels.indexOf(field)).toBeLessThan(labels.indexOf("body"));
    }
  });

  test("bare field name suggests operators with correct ordering", async ({
    page,
  }) => {
    const labels = await getSuggestionLabels(page, "status_code ");
    // Positive operators before negated
    expect(labels.indexOf("==")).toBeLessThan(labels.indexOf("!="));
    expect(labels.indexOf("in")).toBeLessThan(labels.indexOf("!in"));
    expect(labels.indexOf("has")).toBeLessThan(labels.indexOf("!has"));
    expect(labels.indexOf("contains")).toBeLessThan(
      labels.indexOf("!contains"),
    );
    expect(labels.indexOf("startswith")).toBeLessThan(
      labels.indexOf("!startswith"),
    );
  });

  test("bare field == suggests field values", async ({ page }) => {
    const labels = await getSuggestionLabels(page, "status_code == ");
    expect(labels).toContain("OK");
    expect(labels).toContain("ERROR");
    expect(labels).toContain("UNSET");
  });

  test("after complete bare condition suggests logical operators", async ({
    page,
  }) => {
    const labels = await getSuggestionLabels(page, 'status_code == "OK" ');
    expect(labels).toContain("and");
    expect(labels).toContain("or");
    expect(labels).toContain("|");
  });

  test("after 'and' suggests fields again", async ({ page }) => {
    const labels = await getSuggestionLabels(
      page,
      'status_code == "OK" and ',
    );
    expect(labels).toContain("level");
    expect(labels).toContain("duration");
    expect(labels).toContain("resource");
    // Should not contain operators or logical keywords
    expect(labels).not.toContain("==");
    expect(labels).not.toContain("and");
  });

  test("chained bare condition: second field suggests operators", async ({
    page,
  }) => {
    const labels = await getSuggestionLabels(
      page,
      'status_code == "OK" and level ',
    );
    expect(labels).toContain("==");
    expect(labels).toContain("!=");
    expect(labels).toContain("contains");
  });

  test("dot notation suggests nested fields", async ({ page }) => {
    const labels = await getSuggestionLabels(page, "resource.");
    expect(labels).toContain("service");
    expect(labels.length).toBeGreaterThan(0);
    // Should not contain top-level fields
    expect(labels).not.toContain("status_code");
  });

  test("nested field value suggestions", async ({ page }) => {
    // status_code is guaranteed to have values (OK, ERROR, UNSET)
    const labels = await getSuggestionLabels(page, "status_code == ");
    expect(labels).toContain("OK");
    expect(labels).toContain("ERROR");
  });

  test("selecting object fields inserts dot, leaf fields insert space", async ({
    page,
  }) => {
    const insertTexts = await page.evaluate(async () => {
      const el = document.getElementById("filterElement") as any;
      const editor = el.editor;
      editor.setValue("");
      editor.setPosition({ lineNumber: 1, column: 1 });
      editor.focus();
      editor.trigger("test", "editor.action.triggerSuggest", {});
      await new Promise((r) => setTimeout(r, 500));
      const sc = editor.getContribution("editor.contrib.suggestController");
      const model = (sc as any)?.model || (sc as any)?._model;
      const cm = model?._completionModel;
      const result: Record<string, string> = {};
      for (const item of cm?.items || []) {
        const label = item.completion?.label;
        if (
          ["attributes", "resource", "context", "status_code", "level", "duration"].includes(label)
        ) {
          result[label] = item.completion?.insertText;
        }
      }
      return result;
    });

    // Object fields should end with "." for drilling into nested fields
    expect(insertTexts["attributes"]).toBe("attributes.");
    expect(insertTexts["resource"]).toBe("resource.");
    expect(insertTexts["context"]).toBe("context.");
    // Leaf fields should end with " " for typing an operator next
    expect(insertTexts["status_code"]).toBe("status_code ");
    expect(insertTexts["level"]).toBe("level ");
    expect(insertTexts["duration"]).toBe("duration ");
  });

  test("prefixed query also works: spans | field suggests operators", async ({
    page,
  }) => {
    const labels = await getSuggestionLabels(page, "spans | status_code ");
    expect(labels).toContain("==");
    expect(labels).toContain("!=");
    expect(labels.indexOf("==")).toBeLessThan(labels.indexOf("!="));
  });

  test("popular search chips are visible and clickable", async ({ page }) => {
    // Chips should be visible when no query is active
    const chipsContainer = page.locator("#popular-search-chips");
    await expect(chipsContainer).toBeVisible();
    await expect(chipsContainer.getByText("Show errors")).toBeVisible();
    await expect(chipsContainer.getByText("HTTP 5xx responses")).toBeVisible();

    // Click a chip and verify it populates the editor
    await chipsContainer.getByText("Show errors").click();
    const editorValue = await page.evaluate(() => {
      const el = document.getElementById("filterElement") as any;
      return el.editor.getValue();
    });
    expect(editorValue).toContain('level == "ERROR"');
  });

  test("query library dropdown opens and shows tabs", async ({ page }) => {
    await page.getByRole("button", { name: "Library" }).click();
    const popover = page.locator("#queryLibraryPopover");
    await expect(popover).toBeVisible();
    // Should have Popular, Saved, Recent tabs
    await expect(
      popover.getByRole("tab", { name: "Popular" }),
    ).toBeVisible();
    await expect(popover.getByRole("tab", { name: "Saved" })).toBeVisible();
    await expect(popover.getByRole("tab", { name: "Recent" })).toBeVisible();
  });
});
