import { test, expect } from "@playwright/test";
import { DEMO_PROJECT } from "./helpers";

const LOG_EXPLORER_URL = `/p/${DEMO_PROJECT}/log_explorer`;
const HARNESS_URL = process.env.LOG_LIST_HARNESS_URL;

// This is intentionally a browser test rather than another jsdom geometry stub. It drives the
// production <lit-virtualizer> with its real external scroll container through repeated keyed
// remounts — the combination that used to clamp a deep reader to the newest edge for one frame.
test("deep paging and live delivery preserve the row under the reader", async ({ page }) => {
  test.setTimeout(60_000);
  if (HARNESS_URL) {
    await page.goto(HARNESS_URL, { waitUntil: "domcontentloaded" });
  } else {
    await page.route("**/log_explorer/data**", (route) =>
      route.fulfill({
        json: { logsData: [], cols: [], colIdxMap: {}, traces: [], count: 0, hasMore: false },
      }),
    );
    await page.goto(LOG_EXPLORER_URL, { waitUntil: "domcontentloaded" });
    await expect(page.locator("log-list")).toBeVisible();
  }

  const result = await page.evaluate(async () => {
    let list = document.querySelector("log-list") as any;
    if (!list) {
      await import("/public/assets/web-components/dist/src/log-list.ts");
      list = document.createElement("log-list") as any;
      list.fetchInitialData = async () => {};
      document.body.innerHTML = '<style>#logs_list_container_inner{height:420px!important;min-height:0!important;overflow-y:auto!important}</style>';
      document.body.append(list);
      await list.updateComplete;
    }
    const COUNT = 2_500;
    const PAGE = 400;
    const columns = ["id", "timestamp", "service", "summary", "latency_breakdown", "trace_id", "parent_id", "kind", "start_time_ns", "duration"];
    const colIdxMap = Object.fromEntries(columns.map((column, index) => [column, index]));
    const makeRows = (prefix: string, count: number, start: number) =>
      Array.from({ length: count }, (_, index) => {
        const id = `${prefix}-${String(index).padStart(5, "0")}`;
        const ms = start - index * 1_000;
        return {
          id,
          data: [id, new Date(ms).toISOString(), "checkout", [`span_name;text-textStrong⇒${id}`], id, id, "", "log", ms * 1e6, 0],
          depth: 0,
          children: 0,
          traceId: id,
          parentIds: [],
          show: true,
          expanded: false,
          isLastChild: true,
          siblingsArr: [],
          childErrors: false,
          hasErrors: false,
          isNew: false,
          startNs: ms * 1e6,
          duration: 0,
          traceStart: ms * 1e6,
          traceEnd: ms * 1e6,
          childrenTimeSpans: [],
          type: "log",
        };
      });
    const settle = async () => {
      await list.updateComplete;
      const virtualizer = list.querySelector("lit-virtualizer") as any;
      if (virtualizer?.layoutComplete) {
        await Promise.race([virtualizer.layoutComplete.catch(() => {}), new Promise((resolve) => setTimeout(resolve, 500))]);
      }
      await new Promise<void>((resolve) => requestAnimationFrame(() => requestAnimationFrame(() => resolve())));
    };
    const topRowElement = () => {
      const container = list.querySelector("#logs_list_container_inner") as HTMLElement;
      const top = container.getBoundingClientRect().top;
      return [...container.querySelectorAll<HTMLElement>("[data-row-id]")]
        .filter((row) => row.getBoundingClientRect().bottom > top)
        .sort((a, b) => a.getBoundingClientRect().top - b.getBoundingClientRect().top)[0];
    };
    const topRow = () => topRowElement()?.dataset.rowId;
    const topRowOffset = () => {
      const container = list.querySelector("#logs_list_container_inner") as HTMLElement;
      return (topRowElement()?.getBoundingClientRect().top ?? 0) - container.getBoundingClientRect().top;
    };

    list.colIdxMap = colIdxMap;
    list.logsColumns = columns;
    list.spanListTree = makeRows("seed", COUNT, Date.parse("2026-08-28T12:00:00Z"));
    list.seenIds = new Set(list.spanListTree.map((row: any) => row.id));
    list.hasMore = true;
    list.hasNewer = false;
    list.updateVisibleItems();
    await settle();

    const container = list.querySelector("#logs_list_container_inner") as HTMLElement;
    container.style.height = "420px";
    container.style.minHeight = "0";
    container.style.overflowY = "auto";
    await settle();
    container.scrollTop = container.scrollHeight;
    container.dispatchEvent(new Event("scroll"));
    await settle();

    const anchors: Array<{ before?: string; after?: string; scrollTop: number }> = [];
    const calls: string[] = [];
    let pageNumber = 0;
    list.transport = async (url: string) => {
      calls.push(url);
      const tree = makeRows(`older-${pageNumber}`, PAGE, Date.parse("2026-08-01T12:00:00Z") - pageNumber * PAGE * 1_000);
      pageNumber++;
      return {
        tree,
        meta: { cols: columns, colIdxMap, traces: [], serviceColors: {}, count: COUNT + pageNumber * PAGE, hasMore: false },
      };
    };

    for (let pageIndex = 0; pageIndex < 6; pageIndex++) {
      const before = topRow();
      await list.fetchData(`older-${pageIndex}`, false, false, true);
      for (let attempt = 0; attempt < 20; attempt++) {
        await settle();
        if (list.scrollSettling === 0) break;
        await new Promise((resolve) => setTimeout(resolve, 25));
      }
      anchors.push({ before, after: topRow(), scrollTop: container.scrollTop });
    }
    // Give a wrongly exposed top sentinel enough time to issue its fetch.
    await new Promise((resolve) => setTimeout(resolve, 350));

    container.scrollTop = Math.min(container.scrollHeight - container.clientHeight, 120 * 28);
    container.dispatchEvent(new Event("scroll"));
    await settle();

    const refreshAnchor = topRow();
    const refreshScrollTop = container.scrollTop;
    const refreshRowOffset = topRowOffset();
    list.transport = async (url: string) => {
      calls.push(url);
      return {
        tree: makeRows("refresh", 200, Date.parse("2026-08-28T12:02:00Z")),
        meta: { cols: columns, colIdxMap, traces: [], serviceColors: {}, count: COUNT + 200, hasMore: true },
      };
    };
    window.dispatchEvent(new CustomEvent("update-query", { detail: { source: "auto-refresh" } }));
    for (let attempt = 0; attempt < 20; attempt++) {
      await settle();
      if (calls.length === 7 && !list.isFetchingRecent && list.scrollSettling === 0) break;
      await new Promise((resolve) => setTimeout(resolve, 25));
    }
    const afterRefreshAnchor = topRow();
    const afterRefreshScrollTop = container.scrollTop;
    const afterRefreshRowOffset = topRowOffset();
    const refreshRowsVisible = list.spanListTree.some((row: any) => row.id.startsWith("refresh-"));
    const bufferedAfterRefresh = list.recentDataToBeAdded.length;

    const liveAnchor = topRow();
    const liveScrollTop = container.scrollTop;
    list.isLiveStreaming = true;
    list.handleLiveRows(
      Array.from({ length: 200 }, (_, index) => {
        const id = `live-${index}`;
        const ms = Date.parse("2026-08-28T12:01:00Z") + index;
        return {
          shape: "table",
          cols: {
            id,
            timestamp: new Date(ms).toISOString(),
            service: "checkout",
            summary: [`span_name;text-textStrong⇒${id}`],
            latency_breakdown: id,
            trace_id: id,
            parent_id: "",
            kind: "log",
            start_time_ns: ms * 1e6,
            duration: 0,
          },
        };
      }),
    );
    await settle();

    return {
      anchors,
      calls,
      retained: list.spanListTree.length,
      refreshAnchor,
      afterRefreshAnchor,
      refreshScrollTop,
      afterRefreshScrollTop,
      refreshRowOffset,
      afterRefreshRowOffset,
      refreshRowsVisible,
      bufferedAfterRefresh,
      refreshUrl: calls[6],
      liveAnchor,
      afterLiveAnchor: topRow(),
      liveScrollTop,
      afterLiveScrollTop: container.scrollTop,
      buffered: list.recentDataToBeAdded.length,
      liveRowsVisible: list.spanListTree.some((row: any) => row.id.startsWith("live-")),
    };
  });

  expect(result.anchors).toHaveLength(6);
  for (const page of result.anchors) {
    expect(page.after).toBe(page.before);
    expect(page.scrollTop).toBeGreaterThan(0);
  }
  expect(result.retained).toBe(2_500);
  expect(result.calls.slice(0, 6)).toEqual(["older-0", "older-1", "older-2", "older-3", "older-4", "older-5"]);
  expect(new URL(result.refreshUrl).searchParams.get("direction")).toBe("newer");
  expect(result.afterRefreshAnchor).toBe(result.refreshAnchor);
  expect(result.afterRefreshScrollTop).toBe(result.refreshScrollTop);
  expect(result.afterRefreshRowOffset).toBe(result.refreshRowOffset);
  expect(result.refreshRowsVisible).toBe(false);
  expect(result.bufferedAfterRefresh).toBe(200);
  expect(result.afterLiveAnchor).toBe(result.liveAnchor);
  expect(result.afterLiveScrollTop).toBe(result.liveScrollTop);
  expect(result.buffered).toBe(400);
  expect(result.liveRowsVisible).toBe(false);
});
