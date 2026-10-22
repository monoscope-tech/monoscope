# Project wiki delivery sequence

## 1. Wiki foundation — merged

[PR #689](https://github.com/monoscope-tech/monoscope/pull/689) merged on 2026-10-22 as `d8b3bf853ab39006967344bdab2b31972bf9948b`. All required checks, including the production image, passed. Its description includes the captioned walkthrough. This delivers project-scoped pages, permissions, immutable revisions and concurrent-edit protection.

## 2. Page paths and tree navigation — next PR

Start from the merged foundation, independently of the larger impact-agent branch.

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

Merge each PR only after its review loop and required checks pass. Keep worker migration and billing/sandbox changes out of both wiki PRs.
