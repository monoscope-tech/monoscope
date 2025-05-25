import { schemaManager, SchemaData } from './query-editor';

export function initializeDefaultSchema(): void {
  // Configure the OTel schema using flattened key-value pairs
  const otelSchema = {
    fields: {
      // Top-level fields
      timestamp: {
        type: 'string',
        description: 'Timestamp when the event occurred',
      },
      observed_timestamp: {
        type: 'string',
        description: 'Timestamp when the event was observed',
      },
      id: {
        type: 'string',
        description: 'Unique identifier for the log/span',
      },
      parent_id: {
        type: 'string',
        description: 'Parent span ID',
      },
      hashes: {
        type: 'array',
        description: 'All relevant hashes for item identification',
      },
      name: {
        type: 'string',
        description: 'Name of the span or log',
      },
      kind: {
        type: 'string',
        description: 'Type of telemetry data (logs, span, request)',
        enum: ['logs', 'span', 'request'],
      },
      status_code: {
        type: 'string',
        description: 'Status code of the span',
        enum: ['OK', 'ERROR', 'UNSET'],
      },
      status_message: {
        type: 'string',
        description: 'Status message',
      },
      level: {
        type: 'string',
        description: 'Log level (same as severity text)',
        enum: ['TRACE', 'DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL'],
      },
      body: {
        type: 'object',
        description: 'Body content of the log/span',
      },
      duration: {
        type: 'duration',
        description: 'Duration of the span in nanoseconds',
      },
      start_time: {
        type: 'string',
        description: 'Start time of the span',
      },
      end_time: {
        type: 'string',
        description: 'End time of the span',
      },
      events: {
        type: 'array',
        description: 'Events associated with the span',
      },
      links: {
        type: 'string',
        description: 'Links to other spans',
      },
      project_id: {
        type: 'string',
        description: 'Project ID',
      },
      date: {
        type: 'string',
        description: 'Date',
      },

      // Severity fields (flattened)
      severity: {
        type: 'object',
        description: 'Severity information',
      },
      'severity.text': {
        type: 'string',
        description: 'Text representation of severity',
        enum: ['TRACE', 'DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL'],
      },
      'severity.number': {
        type: 'string',
        description: 'Numeric representation of severity',
      },

      // Context fields (flattened)
      context: {
        type: 'object',
        description: 'Context information',
      },
      'context.trace_id': {
        type: 'string',
        description: 'Unique identifier for the trace',
      },
      'context.span_id': {
        type: 'string',
        description: 'Unique identifier for the span',
      },
      'context.trace_state': {
        type: 'string',
        description: 'Trace state',
      },
      'context.trace_flags': {
        type: 'string',
        description: 'Trace flags',
      },
      'context.is_remote': {
        type: 'string',
        description: 'Whether this span is remote',
      },

      // Attributes fields (flattened)
      attributes: {
        type: 'object',
        description: 'Attributes of the log/span',
      },
      'attributes.client': {
        type: 'object',
        description: 'Client information',
      },
      'attributes.client.address': {
        type: 'string',
        description: 'Client address',
      },
      'attributes.client.port': {
        type: 'number',
        description: 'Client port',
      },
      'attributes.server': {
        type: 'object',
        description: 'Server information',
      },
      'attributes.server.address': {
        type: 'string',
        description: 'Server address',
      },
      'attributes.server.port': {
        type: 'number',
        description: 'Server port',
      },
      'attributes.network': {
        type: 'object',
        description: 'Network information',
      },
      'attributes.network.local': {
        type: 'object',
        description: 'Local network information',
      },
      'attributes.network.local.address': {
        type: 'string',
        description: 'Local address',
      },
      'attributes.network.local.port': {
        type: 'number',
        description: 'Local port',
      },
      'attributes.network.peer': {
        type: 'object',
        description: 'Peer network information',
      },
      'attributes.network.peer.address': {
        type: 'string',
        description: 'Peer address',
      },
      'attributes.network.peer.port': {
        type: 'number',
        description: 'Peer port',
      },
      'attributes.network.protocol': {
        type: 'object',
        description: 'Protocol information',
      },
      'attributes.network.protocol.name': {
        type: 'string',
        description: 'Protocol name',
      },
      'attributes.network.protocol.version': {
        type: 'string',
        description: 'Protocol version',
      },
      'attributes.network.transport': {
        type: 'string',
        description: 'Transport type',
      },
      'attributes.network.type': {
        type: 'string',
        description: 'Network type',
      },
      'attributes.code': {
        type: 'object',
        description: 'Source code attributes',
      },
      'attributes.code.number': {
        type: 'number',
        description: 'Code number',
      },
      'attributes.code.file': {
        type: 'object',
        description: 'File information',
      },
      'attributes.code.file.path': {
        type: 'string',
        description: 'File path',
      },
      'attributes.code.function': {
        type: 'object',
        description: 'Function information',
      },
      'attributes.code.function.name': {
        type: 'string',
        description: 'Function name',
      },
      'attributes.code.line': {
        type: 'object',
        description: 'Line information',
      },
      'attributes.code.line.number': {
        type: 'number',
        description: 'Line number',
      },
      'attributes.code.stacktrace': {
        type: 'string',
        description: 'Stack trace',
      },
      'attributes.log_record': {
        type: 'object',
        description: 'Log record attributes',
      },
      'attributes.log_record.original': {
        type: 'string',
        description: 'Original log record',
      },
      'attributes.log_record.uid': {
        type: 'string',
        description: 'Log record UID',
      },
      'attributes.error': {
        type: 'object',
        description: 'Error information',
      },
      'attributes.error.type': {
        type: 'string',
        description: 'Error type',
      },
      'attributes.exception': {
        type: 'object',
        description: 'Exception information',
      },
      'attributes.exception.type': {
        type: 'string',
        description: 'Exception type',
      },
      'attributes.exception.message': {
        type: 'string',
        description: 'Exception message',
      },
      'attributes.exception.stacktrace': {
        type: 'string',
        description: 'Exception stack trace',
      },
      'attributes.url': {
        type: 'object',
        description: 'URL information',
      },
      'attributes.url.fragment': {
        type: 'string',
        description: 'URL fragment',
      },
      'attributes.url.full': {
        type: 'string',
        description: 'Full URL',
      },
      'attributes.url.path': {
        type: 'string',
        description: 'URL path',
      },
      'attributes.url.query': {
        type: 'string',
        description: 'URL query',
      },
      'attributes.url.scheme': {
        type: 'string',
        description: 'URL scheme',
      },
      'attributes.user_agent': {
        type: 'object',
        description: 'User agent information',
      },
      'attributes.user_agent.original': {
        type: 'string',
        description: 'Original user agent string',
      },
      'attributes.http': {
        type: 'object',
        description: 'HTTP information',
      },
      'attributes.http.request': {
        type: 'object',
        description: 'HTTP request attributes',
      },
      'attributes.http.request.method': {
        type: 'string',
        description: 'HTTP method',
        enum: ['GET', 'POST', 'PUT', 'DELETE', 'PATCH', 'HEAD', 'OPTIONS'],
      },
      'attributes.http.request.method_original': {
        type: 'string',
        description: 'Original HTTP method',
      },
      'attributes.http.request.resend_count': {
        type: 'number',
        description: 'Number of times request was resent',
      },
      'attributes.http.request.body': {
        type: 'object',
        description: 'Request body information',
      },
      'attributes.http.request.body.size': {
        type: 'number',
        description: 'Request body size',
      },
      'attributes.http.response': {
        type: 'object',
        description: 'HTTP response attributes',
      },
      'attributes.http.response.status_code': {
        type: 'number',
        description: 'HTTP status code',
      },
      'attributes.session': {
        type: 'object',
        description: 'Session information',
      },
      'attributes.session.id': {
        type: 'string',
        description: 'Session ID',
      },
      'attributes.session.previous': {
        type: 'object',
        description: 'Previous session information',
      },
      'attributes.session.previous.id': {
        type: 'string',
        description: 'Previous session ID',
      },
      'attributes.db': {
        type: 'object',
        description: 'Database information',
      },
      'attributes.db.system': {
        type: 'object',
        description: 'Database system',
      },
      'attributes.db.system.name': {
        type: 'string',
        description: 'Database system name',
      },
      'attributes.db.collection': {
        type: 'object',
        description: 'Collection information',
      },
      'attributes.db.collection.name': {
        type: 'string',
        description: 'Collection name',
      },
      'attributes.db.namespace': {
        type: 'string',
        description: 'Database namespace',
      },
      'attributes.db.operation': {
        type: 'object',
        description: 'Operation information',
      },
      'attributes.db.operation.name': {
        type: 'string',
        description: 'Operation name',
      },
      'attributes.db.operation.batch': {
        type: 'object',
        description: 'Batch information',
      },
      'attributes.db.operation.batch.size': {
        type: 'number',
        description: 'Batch size',
      },
      'attributes.db.response': {
        type: 'object',
        description: 'Database response',
      },
      'attributes.db.response.status_code': {
        type: 'string',
        description: 'Response status code',
      },
      'attributes.db.query': {
        type: 'object',
        description: 'Query information',
      },
      'attributes.db.query.summary': {
        type: 'string',
        description: 'Query summary',
      },
      'attributes.db.query.text': {
        type: 'string',
        description: 'Query text',
      },
      'attributes.user': {
        type: 'object',
        description: 'User information',
      },
      'attributes.user.id': {
        type: 'string',
        description: 'User ID',
      },
      'attributes.user.email': {
        type: 'string',
        description: 'User email',
      },
      'attributes.user.full_name': {
        type: 'string',
        description: 'User full name',
      },
      'attributes.user.name': {
        type: 'string',
        description: 'User name',
      },
      'attributes.user.hash': {
        type: 'string',
        description: 'User hash',
      },

      // Resource fields (flattened)
      resource: {
        type: 'object',
        description: 'Resource attributes',
      },
      'resource.service': {
        type: 'object',
        description: 'Service information',
      },
      'resource.service.name': {
        type: 'string',
        description: 'Service name',
      },
      'resource.service.version': {
        type: 'string',
        description: 'Service version',
      },
      'resource.service.instance': {
        type: 'object',
        description: 'Service instance',
      },
      'resource.service.instance.id': {
        type: 'string',
        description: 'Service instance ID',
      },
      'resource.service.namespace': {
        type: 'string',
        description: 'Service namespace',
      },
      'resource.telemetry': {
        type: 'object',
        description: 'Telemetry information',
      },
      'resource.telemetry.sdk': {
        type: 'object',
        description: 'SDK information',
      },
      'resource.telemetry.sdk.language': {
        type: 'string',
        description: 'SDK language',
      },
      'resource.telemetry.sdk.name': {
        type: 'string',
        description: 'SDK name',
      },
      'resource.telemetry.sdk.version': {
        type: 'string',
        description: 'SDK version',
      },
      'resource.user_agent': {
        type: 'object',
        description: 'User agent information',
      },
      'resource.user_agent.original': {
        type: 'string',
        description: 'Original user agent string',
      },
    },
  };

  // Set the available schemas
  schemaManager.setSchemas(['spans', 'metrics']);
  schemaManager.setDefaultSchema('spans');
  
  // Set schema data for spans
  schemaManager.setSchemaData('spans', otelSchema as SchemaData);

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
          type: info.type,
          examples: info.enum,
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
          type: info.type,
          examples: info.enum,
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
    
    // Check for enum values
    if (fieldInfo && fieldInfo.enum) {
      return fieldInfo.enum.map((v) => String(v));
    }

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
      switch (fieldInfo.type) {
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