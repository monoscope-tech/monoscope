# Shared details panel verification

Use this matrix on Issues, Log Explorer, and a dashboard logs widget before a
release. Each rendered page must contain exactly one `#log_details_container`.
The integration tests enforce that invariant for all three surfaces.

| Check | Desktop inline | Drawer | Mobile |
| --- | --- | --- | --- |
| Open a row | Panel opens and selected row is highlighted | Drawer and backdrop open | Full-screen panel opens |
| Resize | Width persists after close/reopen | Drawer width ignores stale inline width | No horizontal overflow |
| Deep link | `target_event` opens the requested row | Same content after switching mode | Opens after responsive transition |
| Escape | Closes unless focus is in an editor or another modal/popover is open | Closes drawer and backdrop | Closes full-screen panel |
| Close/reopen | Clears URL state and restores the inline width | Backdrop and close button both work | Reopening restores content and scroll |
| Responsive transition | Inline width survives desktop → mobile → desktop | Switching back to inline restores resizer | One panel remains mounted |

Automated coverage lives in `web-components/test/log-detail-panel.test.ts`,
`web-components/test/log-list-row-selection.test.ts`, and the Issues, Log
Explorer, and dashboard integration specs. Complete the visual/resizing portion
in a real browser because jsdom has no layout engine.
