# Project wiki delivery sequence

## 1. Wiki foundation — merged

[PR #689](https://github.com/monoscope-tech/monoscope/pull/689) merged on 2026-10-22 as `d8b3bf853ab39006967344bdab2b31972bf9948b`. All required checks, including the production image, passed. Its description includes the captioned walkthrough. This delivers project-scoped pages, permissions, immutable revisions and concurrent-edit protection.

## 2. Page paths and tree navigation — merged

[PR #690](https://github.com/monoscope-tech/monoscope/pull/690) merged on 2026-10-22 as `e4888f6f018c607b650287bf2661db1ad5b36dc4`. All required checks passed, including CI verification of the production image built on our native amd64 server. Its description includes the paths/tree walkthrough. The change is independent of the larger impact-agent branch.

- Give each page a project-scoped path, such as `runbooks/payments/retries`, and a canonical URL under `/p/<project>/wiki/<page-path>`.
- Keep UUID page identity stable. Add a new migration for paths; preserve existing content, authors and revisions. Backfill unique paths and enforce project/path uniqueness.
- Parse paths into a bounded Haskell type at the boundary. Reject empty segments, traversal segments, backslashes and control characters. Handle URL encoding once.
- Show an explorable folder/page tree with selected-page state and breadcrumbs. Use Lucid, native HTML and HTMX. Users must be able to reach pages beyond the current 100-page listing cap.
- Allow users to choose a page path when creating or editing it. Preserve existing `/memories?page=<uuid>` links and agent references by stable page ID.
- Preserve authorization, stale-draft retention and revision history. Keep agent memory reads/writes consistent with the same page identity; new paths must not introduce another content store.

Verify nested creation, direct links, reload, path collisions, cross-project isolation, tree traversal and concurrent edits through handlers and the browser. Include a short walkthrough in the PR.

## 3. Markdown rich-text editing — merged

[PR #691](https://github.com/monoscope-tech/monoscope/pull/691) merged on 2026-10-22 as `420da48982721bb0d846a7b9772c87ecc9355d20`. All required GitHub checks passed. Its description retains the captioned editor walkthrough and the review decisions.

- Provide formatted editing with headings, bold/italic, links, lists, block quotes, fenced code and undo/redo. A plain textarea or syntax highlighting alone does not meet this requirement.
- Store Markdown as the canonical page body, so human and agent edits share the same representation and revision history. Provide a Markdown source mode; do not silently discard unsupported syntax.
- Reuse an established, locally bundled editor. Evaluate [Milkdown](https://github.com/Milkdown/milkdown), which is designed for WYSIWYG Markdown editing, against [Tiptap Markdown](https://tiptap.dev/docs/editor/markdown). Tiptap currently labels its Markdown bridge beta and documents round-trip limitations. Select after testing our supported Markdown fixtures.
- Keep the editor adapter small and lazy-loaded. Haskell continues to own storage, permissions, validation, history and page rendering. No managed editor or collaboration service is required.
- Preserve unsaved content across validation errors and stale-version responses. Support keyboard operation, accessible toolbar controls, dark mode, and correct mount/disposal during HTMX navigation.

Verify formatted editing, Markdown save/reload round-trips, source-mode switching, code/lists/links, hostile HTML/URLs, stale edits and browser navigation. Check the frontend bundle and production image; attach a short editor walkthrough.

### Editor evaluation — 2026-10-22

A local Chromium prototype of `@milkdown/crepe` 7.22.3 passed nine Markdown load/edit/export cases: basic formatting, task lists, tables, reference links, footnotes, raw HTML, unsafe URLs, images and Mermaid fences. Reference links become inline links and list/table whitespace is normalized; preserve the original Markdown until the user changes the document. Raw HTML remains an escaped editor atom, and the tested JavaScript link has no active destination. These checks cover the editor only; server-rendered HTML still needs separate sanitization tests.

Use Milkdown's core editor with CommonMark/GFM, history, clipboard and trailing-paragraph plugins. The Crepe top bar failed a keyboard regression: icon buttons lacked accessible names and pointer-only handlers ignored Enter. Render labeled native formatting buttons in Haskell and use a small delegated adapter for Milkdown commands. Keep only Crepe's document styles. The resulting lazy chunk measures 140 KB compressed; it omits the ready-made menus, tooltips and upload plugin.

Do not enable temporary browser blob image uploads. Durable image uploads remain outside this slice; existing Markdown image URLs are supported.

Prototype evidence is retained locally in `scripts/local/wiki-editor-spike/roundtrip.log`, `results.json`, and `bundle-minimal.log`.

The editor changes are isolated on `work/project-wiki-editor`, based on the merged paths/tree commit. Its browser regression covers undo/redo, bold formatting, source-mode switching, Markdown persistence and HTMX navigation into a fresh editor. The focused watcher passed all 62 repository-context examples; TypeScript checking and all 1,028 frontend assertions passed. Local light/dark screenshots and a recording are retained. The editor imports on opening the edit panel and preserves untouched source bytes. The existing widget-navigation hook now also replaces Milkdown instances when HTMX morphs server markup.

Saved pages and revision history now render in Haskell with [commonmark](https://hackage.haskell.org/package/commonmark-0.2.6.1) and its GitHub Markdown extensions. Embedded HTML is displayed as text through the parser's public syntax hooks; [xss-sanitize](https://hackage.haskell.org/package/xss-sanitize-0.3.7.2) filters the resulting markup and URL attributes. The previous MMark renderer turned HTML-like text into relative links, which failed the retained history assertion. The global renderer is unchanged.

The handler regression covers formatting, code blocks, tables, tasks, footnotes, script/form markup, encoded and mixed-case JavaScript links, rejected image URLs, and unchanged stored Markdown. The browser flow also saves a reference-style link through source and rich modes, verifies formatted read-back, and pins the exact Markdown bytes in the database. All 62 repository-context examples pass after these changes.

The editor was isolated on `work/project-wiki-editor`. Its final reviewed head was `993b8939ab6e28ea70dc70431901e91f696e51cf`. All eleven local check entries passed for that tree: Linux build, 1,874 doctests, 336 unit examples, 1,225 integration examples, 1,028 frontend assertions, 176 E2E cases, HLint, Weeder, CLI and release checks. Two integration examples require unavailable credentials and remain pending.

The follow-up browser assertion proves that navigation opens an empty rich document, alongside empty server fields. The final review retained the local renderer, explicit toolbar parameters, separate initial/toggle load triggers and external Markdown image support. PR #691 records the reasons. The requested sequence remains in this file.

E2E passed with one worker and unchanged assertions/timeouts. Earlier concurrent runs failed dashboard timing/cleanup checks. Combined UI/integration setup also raced while replacing `node_modules`; both checks passed separately. Failed runs were not attested. Final `make ci-signoff` passed after recovery from a host disk-space failure.

The native amd64 production image includes normal and profiling builds: `sha256:61e09e4ce64c38a6f89c76c6f8d6c1661422fb5cf01741c12b3f83b8965b4be9`. GitHub verified it before merge. The captioned 12-second walkthrough shows keyboard formatting, source mode, safe read-back and tree navigation. Production deployment was not manually triggered or verified in this work.

The larger worker migration, complete resource accounting, chat-agent memory authorization, sandbox maintenance and deployment conformance remain separate work.

Merge each PR only after its review loop and required checks pass. Keep worker migration and billing/sandbox changes out of both wiki PRs.
