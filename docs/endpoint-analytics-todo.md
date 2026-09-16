# Endpoint Analytics follow-up

- [x] Restore the `sitemap` dashboard icon and add its source symbol to the regular SVG sprite.
- [x] Make **Slowest Operations** order by a defined average-latency alias.
- [x] Make **Browser Errors** and **Sessions with Errors** parse and execute with supported error predicates.
- [x] Make **Browser Request Outcomes** parse and execute with a supported outcome grouping.
- [x] Verify `iff` end-to-end in the KQL parser before relying on it in the template.
- [ ] Measure the supplied Dependencies-tab route and identify the dominant server/query cost.
- [x] Reduce initial render and tab-switch latency without weakening endpoint/dependency scoping.
- [x] Run focused browser and parser regression checks; record any locally unavailable integration check.

Verification: the focused Endpoint Analytics Playwright test passed before the final
query/performance-only template edits. `make ci-signoff CHECKS="unit-tests"` passed
after them. The local arm64 TimeFusion image cannot start, so the TimeFusion-backed
integration check remains for GitHub.
