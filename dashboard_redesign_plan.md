# Overview Dashboard Redesign Plan

## Overview
Transform `_overview.yaml` into a comprehensive multi-tab dashboard that replaces the current service and resource pages. The dashboard will use tabs as a top-level feature (not a widget) to organize different views.

## Dashboard Structure

### Top-Level Elements
1. **Title, Icon, Description** - Keep existing
2. **Variables Section** - Dynamic variables that update based on context
   - `service` - Selected service name
   - `resource` - Selected resource/endpoint
   - `database` - Selected database connection
   - Time range variables
3. **Tabs Section** - New dashboard-level feature
   - Listed below variables, sticky to top like variables
   - Each tab contains its own set of widgets

### Tab Structure

#### 1. Overview Tab (Default)
- **Purpose**: High-level system overview
- **Key Widgets**:
  - Services table with clickable rows
    - Columns: Service Name, Throughput, Error Rate, P95 Latency, Status
    - Clicking a row sets `service` variable and navigates to Service Summary tab
  - System-wide metrics grid
    - Total requests
    - Error rate
    - Active services count
    - Database connections
  - Time series charts
    - Request volume by service
    - Error rates by service
    - Latency percentiles across all services

#### 2. Service Summary Tab
- **Purpose**: Detailed view of a specific service
- **Activation**: Requires `service` variable to be set
- **Key Widgets**:
  - Service health indicators
  - Resources/endpoints table for the service
    - Columns: Endpoint, Method, Requests, Errors, P95 Latency
    - Clicking a row sets `resource` variable and navigates to Resources tab
  - Service-specific time series
    - Request volume
    - Error breakdown
    - Latency percentiles
  - Dependency map showing connected services
  - Top errors for the service

#### 3. Resources Tab
- **Purpose**: Deep dive into specific endpoints/resources
- **Activation**: Requires `resource` variable to be set
- **Key Widgets**:
  - Resource performance metrics
  - Request/response size distribution
  - Status code breakdown
  - Latency histogram
  - Sample traces for the resource
  - Error logs specific to the resource

#### 4. Traces Tab
- **Purpose**: Trace analysis for selected context
- **Dynamic Content**: Shows traces based on selected service/resource
- **Key Widgets**:
  - Trace list/search
  - Flamegraph widget (existing implementation)
  - Waterfall widget (existing implementation)
  - Trace statistics
  - Common trace patterns starting from selected service

#### 5. Logs Tab
- **Purpose**: Log exploration with context
- **Dynamic Filtering**: Automatically filtered by service/resource if selected
- **Key Widgets**:
  - Log stream with search
  - Log level distribution
  - Error pattern analysis
  - Log volume time series

#### 6. Databases Tab
- **Purpose**: Database performance monitoring
- **Key Widgets**:
  - Database connections table
    - Clicking sets `database` variable for filtering
  - Query performance metrics
    - Slow queries list
    - Query frequency
    - Average query time by operation type
  - Database-specific time series
    - Query volume
    - Connection pool usage
    - Transaction duration
    - Error rates

## Implementation Details

### YAML Structure
```yaml
title: Overview
icon: qrcode
description: Comprehensive system overview with service, resource, and database insights
refresh_interval: 60s

variables:
  - name: service
    type: dropdown
    query: "extract unique service.name from spans"
    default: ""
    
  - name: resource
    type: dropdown
    query: "extract unique span.name where service.name == $service"
    default: ""
    depends_on: service
    
  - name: database
    type: dropdown  
    query: "extract unique db.system from spans"
    default: ""

tabs:
  - name: Overview
    icon: grid
    widgets: [...]
    
  - name: Service Summary
    icon: server
    requires: service
    widgets: [...]
    
  - name: Resources
    icon: routes
    requires: resource
    widgets: [...]
    
  - name: Traces
    icon: activity
    widgets: [...]
    
  - name: Logs
    icon: file-text
    widgets: [...]
    
  - name: Databases
    icon: database
    widgets: [...]
```

### Navigation Flow
1. User lands on Overview tab
2. Clicks a service in the services table
3. `service` variable is set, Service Summary tab activates and opens
4. User can then select a resource from the resources table
5. `resource` variable is set, Resources tab activates
6. User can navigate between tabs freely, with context preserved

### Variable Behavior
- Variables persist across tab switches
- Clearing a parent variable (e.g., service) clears dependent variables (e.g., resource)
- Tabs that require variables show a prompt to select from dropdown if accessed directly

### Widget Requirements
1. **Table widgets** need `on_row_click` action support to set variables
2. **All widgets** should support variable interpolation in queries
3. **Tab visibility** should support `requires` condition based on variables

## Migration Strategy
1. Identify all widgets from current service/resource pages
2. Map them to appropriate tabs in the new structure
3. Update queries to use variable filtering
4. Implement tab navigation logic
5. Add interactive elements (clickable tables)

## Benefits
- Single dashboard replaces multiple pages
- Context-aware navigation
- Unified experience with drill-down capabilities
- Better performance (single dashboard load)
- Easier maintenance (one YAML file)

## Detailed Widget Implementation

### Overview Tab Widgets

```yaml
widgets:
  - type: table
    title: Services Overview
    layout: { w: 12, h: 6 }
    on_row_click:
      set_variable: service
      value: "{{row.service_name}}"
      navigate_to_tab: "Service Summary"
    columns:
      - field: service_name
        title: Service
      - field: throughput
        title: Throughput
        unit: req/s
      - field: error_rate
        title: Error Rate
        unit: "%"
      - field: p95_latency
        title: P95 Latency
        unit: ms
    sql: |
      SELECT 
        attributes___service___name as service_name,
        COUNT(*)::float / (EXTRACT(EPOCH FROM (MAX(timestamp) - MIN(timestamp)))) as throughput,
        (COUNT(*) FILTER (WHERE status___code = 'ERROR') * 100.0 / COUNT(*))::float as error_rate,
        (approx_percentile(0.95, percentile_agg(duration)) / 1e6)::float as p95_latency
      FROM otel_logs_and_spans
      WHERE project_id='{{project_id}}' 
        AND attributes___service___name IS NOT NULL
        AND kind = 'server'
        {{time_filter}}
      GROUP BY attributes___service___name
      ORDER BY throughput DESC
      
  - type: group
    title: System Metrics
    layout: { w: 12, h: 5 }
    children:
      - type: 'timeseries_stat'
        title: 'Total Requests'
        icon: list-tree
        query: kind=="server" OR name=="apitoolkit-http-span" OR name=="monoscope.http" | summarize count() by bin_auto(timestamp)
        unit: reqs
        eager: true
        layout: { w: 3, h: 2 }
        
      - type: 'stat'
        title: 'Error Rate'
        icon: bug
        query: (kind=="server" OR name=="apitoolkit-http-span") AND status.code=="ERROR" | summarize (count() / sum(count())) * 100
        unit: '%'
        eager: true
        layout: { w: 3, h: 2 }
        
      - type: 'stat'
        title: 'Active Services'
        icon: server
        query: attributes.service.name != null | summarize count(distinct(attributes.service.name))
        unit: services
        eager: true
        layout: { w: 3, h: 2 }
        
      - type: 'stat'
        title: 'DB Connections'
        icon: database
        query: attributes.db.system != null | summarize count(distinct(attributes.db.connection_string))
        unit: connections
        eager: true
        layout: { w: 3, h: 2 }
```

### Service Summary Tab Widgets

```yaml
widgets:
  - type: table
    title: Service Endpoints
    layout: { w: 12, h: 6 }
    on_row_click:
      set_variable: resource
      value: "{{row.endpoint_hash}}"
      navigate_to_tab: "Resources"
    columns:
      - field: endpoint
        title: Endpoint
      - field: method
        title: Method
      - field: requests
        title: Requests
        unit: reqs
      - field: errors
        title: Errors
        unit: reqs
      - field: p95_latency
        title: P95 Latency
        unit: ms
    sql: |
      SELECT 
        hashes[1] as endpoint_hash,
        attributes___http___request___method || ' ' || attributes___url___path as endpoint,
        attributes___http___request___method as method,
        COUNT(*) as requests,
        COUNT(*) FILTER (WHERE attributes___http___response___status_code >= 400) as errors,
        (approx_percentile(0.95, percentile_agg(duration)) / 1e6)::float as p95_latency
      FROM otel_logs_and_spans
      WHERE project_id='{{project_id}}' 
        AND attributes___service___name = '{{var-service}}'
        AND kind = 'server'
        AND attributes___http___request___method IS NOT NULL
        {{time_filter}}
      GROUP BY endpoint_hash, endpoint, method
      ORDER BY requests DESC
      
  - type: 'timeseries'
    title: 'Service Request Volume'
    query: attributes.service.name=="{{var-service}}" AND kind=="server" | summarize count() by bin_auto(timestamp), attributes.http.response.status_code
    unit: reqs
    layout: { w: 6, h: 4 }
    
  - type: 'timeseries_line'
    title: 'Service Latency Percentiles (ms)'
    hide_subtitle: true
    summarize_by: max
    sql: |
      SELECT timeB, quantile, value
      FROM (
        SELECT extract(epoch from time_bucket('1h', timestamp))::integer AS timeB,
               ARRAY[
                 (approx_percentile(0.50, percentile_agg(duration)) / 1e6)::float,
                 (approx_percentile(0.75, percentile_agg(duration)) / 1e6)::float,
                 (approx_percentile(0.90, percentile_agg(duration)) / 1e6)::float,
                 (approx_percentile(0.95, percentile_agg(duration)) / 1e6)::float,
                 (approx_percentile(0.99, percentile_agg(duration)) / 1e6)::float
               ] AS values,
               ARRAY['p50', 'p75', 'p90', 'p95', 'p99'] AS quantiles
        FROM otel_logs_and_spans
        WHERE project_id='{{project_id}}' 
          AND attributes___service___name = '{{var-service}}'
          AND kind = 'server'
          {{time_filter}}
        GROUP BY timeB
      ) s,
      LATERAL unnest(s.values, s.quantiles) AS u(value, quantile);
    layout: { w: 6, h: 4 }
```

### Database Tab Widgets

```yaml
widgets:
  - type: table
    title: Database Connections
    layout: { w: 12, h: 5 }
    on_row_click:
      set_variable: database
      value: "{{row.db_system}}"
    columns:
      - field: db_system
        title: Database Type
      - field: db_name
        title: Database Name
      - field: operations
        title: Operations
      - field: avg_duration
        title: Avg Duration
        unit: ms
      - field: error_rate
        title: Error Rate
        unit: "%"
    sql: |
      SELECT 
        attributes___db___system as db_system,
        attributes___db___name as db_name,
        COUNT(*) as operations,
        AVG(duration) / 1e6 as avg_duration,
        (COUNT(*) FILTER (WHERE status___code = 'ERROR') * 100.0 / COUNT(*))::float as error_rate
      FROM otel_logs_and_spans
      WHERE project_id='{{project_id}}' 
        AND attributes___db___system IS NOT NULL
        {{time_filter}}
      GROUP BY db_system, db_name
      ORDER BY operations DESC
      
  - type: 'timeseries'
    title: 'Database Query Performance'
    query: attributes.db.system != null | summarize avg(duration) / 1e6 by bin_auto(timestamp), attributes.db.operation, attributes.db.system
    unit: ms
    layout: { w: 6, h: 4 }
    
  - type: 'timeseries'
    title: 'Query Volume by Operation'
    query: attributes.db.operation != null | summarize count() by bin_auto(timestamp), attributes.db.operation
    layout: { w: 6, h: 4 }
    
  - type: list
    title: 'Slowest Queries'
    layout: { w: 12, h: 6 }
    sql: |
      SELECT 
        attributes___db___statement as query,
        attributes___db___operation as operation,
        duration / 1e6 as duration_ms,
        attributes___service___name as service
      FROM otel_logs_and_spans
      WHERE project_id='{{project_id}}' 
        AND attributes___db___statement IS NOT NULL
        {{time_filter}}
      ORDER BY duration DESC
      LIMIT 20
```

## Next Steps

1. **Backend Implementation**:
   - Add tabs support to dashboard schema
   - Implement variable dependencies and tab requirements
   - Add table widget with row click actions
   - Implement tab navigation logic

2. **Frontend Implementation**:
   - Create tab UI component
   - Handle variable state management across tabs
   - Implement table row click handlers
   - Add tab switching animations

3. **Widget Development**:
   - Implement missing widget types (table, list, distribution)
   - Add flamegraph/waterfall widgets as dashboard widgets
   - Create service dependency map widget

4. **Testing**:
   - Test variable cascading behavior
   - Verify tab navigation flows
   - Performance test with large datasets
   - User acceptance testing

5. **Migration**:
   - Update _overview.yaml with new structure
   - Redirect old service/resource pages to new dashboard
   - Update documentation