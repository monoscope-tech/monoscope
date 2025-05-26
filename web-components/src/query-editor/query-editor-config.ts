import { schemaManager, SchemaData } from './query-editor';

export function initializeDefaultSchema(): void {
  // Schema is now provided by the Haskell backend
  // This function sets up the resolvers for the schema provided via setSchema()
  
  // Set the available schemas
  schemaManager.setSchemas(['spans', 'metrics']);
  schemaManager.setDefaultSchema('spans');


  // Set up a nested field resolver for flattened schema
  schemaManager.setNestedResolver(async (schema: string, prefix: string) => {
    console.log(`Resolving nested fields for schema: ${schema}, prefix: ${prefix}`);

    // Get the schema data
    const currentSchema = schemaManager.getSchemaData(schema) || schemaManager.getSchemaData('spans');
    if (!currentSchema?.fields) return [];

    if (!prefix) {
      // Top-level fields - return all fields that don't contain dots (direct fields only)
      return Object.entries(currentSchema.fields)
        .filter(([name]) => !name.includes('.'))
        .map(([name, info]) => ({
          name,
          type: info.field_type,
          examples: info.examples,
          // Check if this field has nested fields by looking for dotted versions
          fields: Object.keys(currentSchema.fields).some(key => key.startsWith(name + '.')) ? {} : undefined,
        }));
    }

    // Handle nested fields - find all fields that start with prefix + "."
    const prefixWithDot = prefix + '.';
    const nestedFields = Object.entries(currentSchema.fields)
      .filter(([name]) => name.startsWith(prefixWithDot))
      .map(([name, info]) => {
        // Extract the immediate child field name
        const remainder = name.substring(prefixWithDot.length);
        const nextDotIndex = remainder.indexOf('.');
        const childName = nextDotIndex === -1 ? remainder : remainder.substring(0, nextDotIndex);
        
        return {
          childName,
          fullName: name,
          info,
          hasNestedFields: nextDotIndex !== -1,
        };
      });

    // Group by immediate child name and deduplicate
    const childMap = new Map();
    nestedFields.forEach(({ childName, fullName, info, hasNestedFields }) => {
      if (!childMap.has(childName)) {
        childMap.set(childName, {
          name: childName,
          type: info.field_type,
          examples: info.examples,
          // Mark as having nested fields if there are deeper levels or if it's an object type
          fields: hasNestedFields || 
                 nestedFields.some(f => f.fullName.startsWith(prefixWithDot + childName + '.')) ? {} : undefined,
        });
      }
    });

    return Array.from(childMap.values());
  });

  // Set up a value resolver for field-specific values with flattened schema
  schemaManager.setValueResolver(async (schema: string, field: string) => {
    console.log(`Resolving values for field: ${field} in schema: ${schema}`);

    // Get the schema data
    const currentSchema = schemaManager.getSchemaData(schema) || schemaManager.getSchemaData('spans');
    if (!currentSchema?.fields) return [];

    // Direct field lookup in flattened schema
    const fieldInfo = currentSchema.fields[field];
    
    // Check for examples
    if (fieldInfo && fieldInfo.examples) {
      return fieldInfo.examples.map((v) => String(v));
    }

    // For specific common fields, return useful values
    const fieldSpecificValues: Record<string, string[]> = {
      status_code: ['OK', 'ERROR', 'UNSET'],
      'attributes.http.request.method': ['GET', 'POST', 'PUT', 'DELETE', 'PATCH', 'HEAD', 'OPTIONS'],
      'resource.service.name': ['frontend', 'backend', 'api', 'auth-service'],
      level: ['TRACE', 'DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL'],
      'severity.text': ['TRACE', 'DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL'],
      kind: ['logs', 'span', 'request'],
      'context.trace_id': ['trace-123', 'trace-456'],
      'context.span_id': ['span-abc', 'span-def'],
      'attributes.http.response.status_code': ['200', '201', '400', '401', '403', '404', '500', '502', '503'],
      'resource.telemetry.sdk.language': ['javascript', 'python', 'java', 'go', 'rust', 'csharp'],
      'attributes.user.email': ['user@example.com', 'admin@company.com'],
    };

    // Check for exact match or partial match
    if (fieldSpecificValues[field]) {
      return fieldSpecificValues[field];
    }

    // Check if any part of the field matches known patterns
    for (const [key, values] of Object.entries(fieldSpecificValues)) {
      if (field.includes(key.split('.').pop() || '')) {
        return values;
      }
    }

    // Default fallback values based on field type
    if (fieldInfo) {
      switch (fieldInfo.field_type) {
        case 'string':
          return ['example-value', 'test-string', 'sample-data'];
        case 'number':
          return ['100', '200', '500', '1000'];
        case 'boolean':
          return ['true', 'false'];
        default:
          return ['value1', 'value2', 'value3'];
      }
    }

    // Ultimate fallback
    return ['value1', 'value2', 'value3'];
  });
}

// Export popular OpenTelemetry queries
export function getPopularQueries() {
  return [
    // Error and exception queries
    ['level == "ERROR"', 'Show all error-level logs'],
    ['attributes.exception.type != null', 'Find logs with exceptions'],
    ['level == "ERROR" and attributes.http.response.status_code >= 500', 'Server errors with HTTP 5xx status codes'],
    
    // HTTP-related queries
    ['attributes.http.request.method == "POST"', 'All POST requests'],
    ['attributes.http.response.status_code >= 400', 'HTTP errors (4xx and 5xx)'],
    ['attributes.http.response.status_code == 404', 'Not found errors'],
    ['attributes.http.request.method == "GET" and attributes.http.response.status_code == 200', 'Successful GET requests'],
    
    // Performance queries
    ['duration > 5000000000', 'Slow requests (>5 seconds)'],
    ['duration > 1000000000', 'Requests taking more than 1 second'],
    ['kind == "span" and duration > 500000000', 'Slow spans (>500ms)'],
    
    // Service and trace queries
    ['resource.service.name == "api"', 'Logs from API service'],
    ['kind == "span"', 'All span data'],
    ['kind == "logs"', 'All log entries'],
    ['parent_id != null', 'Child spans with parent relationships'],
    
    // Database queries
    ['attributes.db.operation.name != null', 'Database operations'],
    ['attributes.db.system.name == "postgresql"', 'PostgreSQL database operations'],
    ['attributes.db.query.text contains "SELECT"', 'Database SELECT queries'],
    
    // User and session queries
    ['attributes.user.id != null', 'Logs with user information'],
    ['attributes.session.id != null', 'Logs with session tracking'],
    
    // Time-based and filtering
    ['timestamp > ago(1h)', 'Logs from the last hour'],
    ['timestamp > ago(24h) and level == "WARN"', 'Warnings from the last 24 hours'],
    
    // Complex filtering examples
    ['attributes.http.request.method == "POST" and attributes.http.response.status_code < 400', 'Successful POST requests'],
    ['resource.service.name == "frontend" and level in ["ERROR", "WARN"]', 'Frontend service errors and warnings'],
    ['attributes.url.path contains "/api/" and duration > 2000000000', 'Slow API endpoint calls']
  ].map(([query, description]) => ({ query, description }));
}