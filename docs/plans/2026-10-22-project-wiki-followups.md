# Project wiki delivery sequence

## 1. Wiki foundation — merged

[PR #689](https://github.com/monoscope-tech/monoscope/pull/689) merged on 2026-10-22 as `d8b3bf853ab39006967344bdab2b31972bf9948b`. All required checks, including the production image, passed. Its description includes the captioned walkthrough. This delivers project-scoped pages, permissions, immutable revisions and concurrent-edit protection.

## 2. Page paths and tree navigation — PR #690

[PR #690](https://github.com/monoscope-tech/monoscope/pull/690) starts from the merged foundation, independently of the larger impact-agent branch. Its description includes the paths/tree walkthrough. All local sign-off checks passed; the production image is still building.

- Give each page a project-scoped path, such as `runbooks/payments/retries`, and a canonical URL under `/p/<project>/wiki/<page-path>`.
- Keep UUID page identity stable. Add a new migration for paths; preserve existing content, authors and revisions. Backfill unique paths and enforce project/path uniqueness.
- Parse paths into a bounded Haskell type at the boundary. Reject empty segments, traversal segments, backslashes and control characters. Handle URL encoding once.
- Show an explorable folder/page tree with selected-page state and breadcrumbs. Use Lucid, native HTML and HTMX. Users must be able to reach pages beyond the current 100-page listing cap.
- Allow users to choose a page path when creating or editing it. Preserve existing `/memories?page=<uuid>` links and agent references by stable page ID.
- Preserve authorization, stale-draft retention and revision history. Keep agent memory reads/writes consistent with the same page identity; new paths must not introduce another content store.

Verify nested creation, direct links, reload, path collisions, cross-project isolation, tree traversal and concurrent edits through handlers and the browser. Include a short walkthrough in the PR.

## 3. Markdown rich-text editing — subsequent PR

Base this PR on the merged paths/tree change.

- Provide formatted editing with headings, bold/italic, links, lists, block quotes, fenced code and undo/redo. A plain textarea or syntax highlighting alone does not meet this requirement.
- Store Markdown as the canonical page body, so human and agent edits share the same representation and revision history. Provide a Markdown source mode; do not silently discard unsupported syntax.
- Reuse an established, locally bundled editor. Evaluate [Milkdown](https://github.com/Milkdown/milkdown), which is designed for WYSIWYG Markdown editing, against [Tiptap Markdown](https://tiptap.dev/docs/editor/markdown). Tiptap currently labels its Markdown bridge beta and documents round-trip limitations. Select after testing our supported Markdown fixtures.
- Keep the editor adapter small and lazy-loaded. Haskell continues to own storage, permissions, validation, history and page rendering. No managed editor or collaboration service is required.
- Preserve unsaved content across validation errors and stale-version responses. Support keyboard operation, accessible toolbar controls, dark mode, and correct mount/disposal during HTMX navigation.

Verify formatted editing, Markdown save/reload round-trips, source-mode switching, code/lists/links, hostile HTML/URLs, stale edits and browser navigation. Check the frontend bundle and production image; attach a short editor walkthrough.

### Editor evaluation — 2026-10-22

A local Chromium prototype of `@milkdown/crepe` 7.22.3 passed nine Markdown load/edit/export cases: basic formatting, task lists, tables, reference links, footnotes, raw HTML, unsafe URLs, images and Mermaid fences. Reference links become inline links and list/table whitespace is normalized; preserve the original Markdown until the user changes the document. Raw HTML remains an escaped editor atom, and the tested JavaScript link has no active destination. These checks cover the editor only; server-rendered HTML still needs separate sanitization tests.

Use `CrepeBuilder` with top bar, links, lists, tables, placeholders and cursor support. The isolated build measured 213 KB JavaScript and 3.5 KB CSS compressed, versus 466 KB JavaScript for the general Crepe entry point. Import only those feature styles to omit math fonts. Lazy-load the editor on wiki editing; Haskell owns the shell and form, and Markdown remains the submitted value.

Do not use the builder's default file uploader: it creates temporary browser blob URLs, which cannot survive saving and reopening a page. Keep durable image uploads outside this slice and allow existing Markdown image URLs. The builder requires its upload configuration even when uploads are disabled; removing the entire upload plugin prevents initialization.

Prototype evidence is retained locally in `scripts/local/wiki-editor-spike/roundtrip.log`, `results.json`, and `bundle-minimal.log`.

The editor adapter is implemented on the larger checkpoint branch, separately from #690. Its browser regression covers undo/redo, bold formatting, source-mode switching, Markdown persistence and HTMX navigation into a fresh editor. The focused watcher passed all 62 repository-context examples; TypeScript checking and all 1,028 frontend assertions passed. Local light/dark screenshots and a recording are retained. The editor imports on opening the edit panel and preserves untouched source bytes. The existing widget-navigation hook now also replaces Milkdown instances when HTMX morphs server markup.

Saved pages and revision history now render in Haskell with [commonmark](https://hackage.haskell.org/package/commonmark-0.2.6.1) and its GitHub Markdown extensions. Embedded HTML is displayed as text through the parser's public syntax hooks; [xss-sanitize](https://hackage.haskell.org/package/xss-sanitize-0.3.7.2) filters the resulting markup and URL attributes. The previous MMark renderer turned HTML-like text into relative links, which failed the retained history assertion. The global renderer is unchanged.

The handler regression covers formatting, code blocks, tables, tasks, footnotes, script/form markup, encoded and mixed-case JavaScript links, rejected image URLs, and unchanged stored Markdown. The browser flow also saves a reference-style link through source and rich modes, verifies formatted read-back, and pins the exact Markdown bytes in the database. All 62 repository-context examples pass after these changes.

The editor is still awaiting its independent PR, final local CI, production image and attached walkthrough. Keep it separate from the larger agent branch.

Merge each PR only after its review loop and required checks pass. Keep worker migration and billing/sandbox changes out of both wiki PRs.
