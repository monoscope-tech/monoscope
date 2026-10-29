# Dashboard views and tab filters

A saved view names a set of dashboard variable values. It applies those filters
while keeping the current tab and time range. Use **Save view** on any dashboard
with variables; views are shared by that dashboard's project members.

Views saved in the UI live separately from the dashboard schema, so saving a view
keeps template-backed dashboards receiving template updates. Existing RUM audience
segments migrate into views on their project's RUM dashboard.

YAML can also declare views and choose which filters each tab displays:

```yaml
variables:
  - {key: service, title: Service, type: values, options: [[api], [worker]]}
  - {key: region, title: Region, type: values, options: [[eu], [us]]}
views:
  - name: Europe API
    variables: {service: api, region: eu}
tabs:
  - name: Overview
    variables: [service]
    widgets: []
  - name: Errors
    variables: [service, region]
    widgets: []
widgets: []
```

Omitting a tab's `variables` displays all dashboard filters. An empty list hides
them all. Hidden filters retain their values and continue filtering queries.
UI-saved views override YAML views with the same name; deleting the saved override
reveals the YAML view again. Change YAML to remove a declared view.

Widget layouts accept `min_width` in pixels. For example,
`layout: {w: 4, h: 5, min_width: 500}` requests at least 500 pixels for a widget
occupying one third of a twelve-column grid. The grid stacks its widgets when its
available width falls below 1500 pixels. Tables wrap long values, and groups of
stat widgets use two columns on narrow screens.
