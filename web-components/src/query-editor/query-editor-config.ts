import { schemaManager, SchemaData } from './query-editor';

export function initializeDefaultSchema(): void {
  // Configure the OTel schema based on the provided struct
  const otelSchema = {
    fields: {
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
      severity: {
        type: 'object',
        description: 'Severity information',
        fields: {
          severity_text: { type: 'string', description: 'Text representation of severity' },
          severity_number: { type: 'string', description: 'Numeric representation of severity' },
        },
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
      context: {
        type: 'object',
        description: 'Context information',
        fields: {
          trace_id: { type: 'string', description: 'Unique identifier for the trace' },
          span_id: { type: 'string', description: 'Unique identifier for the span' },
          trace_state: { type: 'string', description: 'Trace state' },
          trace_flags: { type: 'string', description: 'Trace flags' },
          is_remote: { type: 'string', description: 'Whether this span is remote' },
        },
      },
      events: {
        type: 'array',
        description: 'Events associated with the span',
      },
      links: {
        type: 'string',
        description: 'Links to other spans',
      },
      attributes: {
        type: 'object',
        description: 'Attributes of the log/span',
        fields: {
          client: {
            type: 'object',
            description: 'Client information',
            fields: {
              address: { type: 'string', description: 'Client address' },
              port: { type: 'number', description: 'Client port' },
            },
          },
          server: {
            type: 'object',
            description: 'Server information',
            fields: {
              address: { type: 'string', description: 'Server address' },
              port: { type: 'number', description: 'Server port' },
            },
          },
          network: {
            type: 'object',
            description: 'Network information',
            fields: {
              local: {
                type: 'object',
                description: 'Local network information',
                fields: {
                  address: { type: 'string', description: 'Local address' },
                  port: { type: 'number', description: 'Local port' },
                },
              },
              peer: {
                type: 'object',
                description: 'Peer network information',
                fields: {
                  address: { type: 'string', description: 'Peer address' },
                  port: { type: 'number', description: 'Peer port' },
                },
              },
              protocol: {
                type: 'object',
                description: 'Protocol information',
                fields: {
                  name: { type: 'string', description: 'Protocol name' },
                  version: { type: 'string', description: 'Protocol version' },
                },
              },
              transport: { type: 'string', description: 'Transport type' },
              type: { type: 'string', description: 'Network type' },
            },
          },
          code: {
            type: 'object',
            description: 'Source code attributes',
            fields: {
              number: { type: 'number', description: 'Code number' },
              file: {
                type: 'object',
                description: 'File information',
                fields: {
                  path: { type: 'number', description: 'File path' },
                },
              },
              function: {
                type: 'object',
                description: 'Function information',
                fields: {
                  name: { type: 'number', description: 'Function name' },
                },
              },
              line: {
                type: 'object',
                description: 'Line information',
                fields: {
                  number: { type: 'number', description: 'Line number' },
                },
              },
              stacktrace: { type: 'number', description: 'Stack trace' },
            },
          },
          log_record: {
            type: 'object',
            description: 'Log record attributes',
            fields: {
              original: { type: 'string', description: 'Original log record' },
              uid: { type: 'string', description: 'Log record UID' },
            },
          },
          error: {
            type: 'object',
            description: 'Error information',
            fields: {
              type: { type: 'string', description: 'Error type' },
            },
          },
          exception: {
            type: 'object',
            description: 'Exception information',
            fields: {
              type: { type: 'string', description: 'Exception type' },
              message: { type: 'string', description: 'Exception message' },
              stacktrace: { type: 'string', description: 'Exception stack trace' },
            },
          },
          url: {
            type: 'object',
            description: 'URL information',
            fields: {
              fragment: { type: 'string', description: 'URL fragment' },
              full: { type: 'string', description: 'Full URL' },
              path: { type: 'string', description: 'URL path' },
              query: { type: 'string', description: 'URL query' },
              scheme: { type: 'string', description: 'URL scheme' },
            },
          },
          user_agent: {
            type: 'object',
            description: 'User agent information',
            fields: {
              original: { type: 'string', description: 'Original user agent string' },
            },
          },
          http: {
            type: 'object',
            description: 'HTTP information',
            fields: {
              request: {
                type: 'object',
                description: 'HTTP request attributes',
                fields: {
                  method: {
                    type: 'string',
                    description: 'HTTP method',
                    enum: ['GET', 'POST', 'PUT', 'DELETE', 'PATCH', 'HEAD', 'OPTIONS'],
                  },
                  method_original: { type: 'string', description: 'Original HTTP method' },
                  resend_count: { type: 'number', description: 'Number of times request was resent' },
                  body: {
                    type: 'object',
                    description: 'Request body information',
                    fields: {
                      size: { type: 'number', description: 'Request body size' },
                    },
                  },
                },
              },
              response: {
                type: 'object',
                description: 'HTTP response attributes',
                fields: {
                  status_code: { type: 'number', description: 'HTTP status code' },
                },
              },
            },
          },
          session: {
            type: 'object',
            description: 'Session information',
            fields: {
              id: { type: 'string', description: 'Session ID' },
              previous: {
                type: 'object',
                description: 'Previous session information',
                fields: {
                  id: { type: 'string', description: 'Previous session ID' },
                },
              },
            },
          },
          db: {
            type: 'object',
            description: 'Database information',
            fields: {
              system: {
                type: 'object',
                description: 'Database system',
                fields: {
                  name: { type: 'string', description: 'Database system name' },
                },
              },
              collection: {
                type: 'object',
                description: 'Collection information',
                fields: {
                  name: { type: 'string', description: 'Collection name' },
                },
              },
              namespace: { type: 'string', description: 'Database namespace' },
              operation: {
                type: 'object',
                description: 'Operation information',
                fields: {
                  name: { type: 'string', description: 'Operation name' },
                  batch: {
                    type: 'object',
                    description: 'Batch information',
                    fields: {
                      size: { type: 'number', description: 'Batch size' },
                    },
                  },
                },
              },
              response: {
                type: 'object',
                description: 'Database response',
                fields: {
                  status_code: { type: 'string', description: 'Response status code' },
                },
              },
              query: {
                type: 'object',
                description: 'Query information',
                fields: {
                  summary: { type: 'string', description: 'Query summary' },
                  text: { type: 'string', description: 'Query text' },
                },
              },
            },
          },
          user: {
            type: 'object',
            description: 'User information',
            fields: {
              id: { type: 'string', description: 'User ID' },
              email: { type: 'string', description: 'User email' },
              full_name: { type: 'string', description: 'User full name' },
              name: { type: 'string', description: 'User name' },
              hash: { type: 'string', description: 'User hash' },
            },
          },
        },
      },
      resource: {
        type: 'object',
        description: 'Resource attributes',
        fields: {
          service: {
            type: 'object',
            description: 'Service information',
            fields: {
              name: { type: 'string', description: 'Service name' },
              version: { type: 'string', description: 'Service version' },
              instance: {
                type: 'object',
                description: 'Service instance',
                fields: {
                  id: { type: 'string', description: 'Service instance ID' },
                },
              },
              namespace: { type: 'string', description: 'Service namespace' },
            },
          },
          telemetry: {
            type: 'object',
            description: 'Telemetry information',
            fields: {
              sdk: {
                type: 'object',
                description: 'SDK information',
                fields: {
                  language: { type: 'string', description: 'SDK language' },
                  name: { type: 'string', description: 'SDK name' },
                  version: { type: 'string', description: 'SDK version' },
                },
              },
            },
          },
          user_agent: {
            type: 'object',
            description: 'User agent information',
            fields: {
              original: { type: 'string', description: 'Original user agent string' },
            },
          },
        },
      },
      project_id: {
        type: 'string',
        description: 'Project ID',
      },
      date: {
        type: 'string',
        description: 'Date',
      },
    },
  };

  // Set the available schemas
  schemaManager.setSchemas(['spans', 'metrics']);
  schemaManager.setDefaultSchema('spans');
  
  // Set schema data for spans
  schemaManager.setSchemaData('spans', otelSchema as SchemaData);

  // Set up a nested field resolver
  schemaManager.setNestedResolver(async (schema: string, prefix: string) => {
    console.log(`Resolving nested fields for schema: ${schema}, prefix: ${prefix}`);

    // Get the schema data
    const currentSchema = schemaManager.getSchemaData(schema) || schemaManager.getSchemaData('spans');
    if (!currentSchema?.fields) return [];

    if (!prefix) {
      // Top-level fields
      return Object.entries(currentSchema.fields).map(([name, info]) => ({
        name,
        type: info.type,
        examples: info.enum,
        fields: info.fields,
      }));
    }

    // Handle nested fields
    const parts = prefix.split('.');
    let current = currentSchema.fields;

    // Navigate to the requested prefix path
    for (let i = 0; i < parts.length; i++) {
      if (!current[parts[i]] || !current[parts[i]].fields) {
        // Path doesn't exist or doesn't have nested fields
        return [];
      }
      current = current[parts[i]].fields!;
    }

    // Return fields at this level
    return Object.entries(current).map(([name, info]) => ({
      name,
      type: info.type,
      examples: info.enum,
      fields: info.fields,
    }));
  });

  // Set up a value resolver for field-specific values
  schemaManager.setValueResolver(async (schema: string, field: string) => {
    console.log(`Resolving values for field: ${field} in schema: ${schema}`);

    // Get the schema data
    const currentSchema = schemaManager.getSchemaData(schema) || schemaManager.getSchemaData('spans');
    if (!currentSchema?.fields) return [];

    // Navigate the schema to find the field
    const parts = field.split('.');
    let current = currentSchema.fields;
    let fieldInfo = null;

    for (let i = 0; i < parts.length; i++) {
      const part = parts[i];
      if (!current[part]) {
        break;
      }

      if (i === parts.length - 1) {
        // Found our target field
        fieldInfo = current[part];
        break;
      }

      if (!current[part].fields) {
        break;
      }

      current = current[part].fields!;
    }

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
      'attributes.http.request.method': ['GET', 'POST', 'PUT', 'DELETE', 'PATCH'],
      'resource.service.name': ['frontend', 'backend', 'api', 'auth-service'],
      level: ['DEBUG', 'INFO', 'WARN', 'ERROR'],
    };

    // Check if any part of the field matches known fields
    for (const [key, values] of Object.entries(fieldSpecificValues)) {
      if (field.includes(key)) {
        return values;
      }
    }

    // Default fallback values
    return ['value1', 'value2', 'value3'];
  });
}