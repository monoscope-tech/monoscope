import { schemaManager, SchemaData } from './query-editor';

export function initializeDefaultSchema(): void {
  // Schema is now provided by the Haskell backend
  // This function sets up the resolvers for the schema provided via setSchema()

  // Set the available schemas
  schemaManager.setSchemas(['spans', 'metrics']);
  schemaManager.setDefaultSchema('spans');

  // Set up a nested field resolver for flattened schema
  schemaManager.setNestedResolver(async (schema: string, prefix: string) => {
    // Get the schema data
    const currentSchema = schemaManager.getSchemaData(schema) || schemaManager.getSchemaData('spans');
    if (!currentSchema?.fields) return [];

    if (!prefix) {
      // Top-level fields - return all fields that don't contain dots (direct fields only)
      return Object.entries(currentSchema.fields)
        .filter(([name]) => !name.includes('.'))
        .map(([name, info]) => ({
          name,
          type: info.type || info.field_type,
          examples: info.examples,
          // Check if this field has nested fields by looking for dotted versions
          fields: Object.keys(currentSchema.fields).some((key) => key.startsWith(name + '.')) ? {} : undefined,
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
          type: info.type || info.field_type,
          examples: info.examples,
          // Mark as having nested fields if there are deeper levels or if it's an object type
          fields: hasNestedFields || nestedFields.some((f) => f.fullName.startsWith(prefixWithDot + childName + '.')) ? {} : undefined,
        });
      }
    });

    return Array.from(childMap.values());
  });

  // Set up a value resolver for field-specific values with flattened schema
  schemaManager.setValueResolver(async (schema: string, field: string) => {
    // Get the schema data
    const currentSchema = schemaManager.getSchemaData(schema) || schemaManager.getSchemaData('spans');
    if (!currentSchema?.fields) return [];

    // Direct field lookup in flattened schema
    const fieldInfo = currentSchema.fields[field];

    // Check for examples
    if (fieldInfo && fieldInfo.examples) {
      return fieldInfo.examples.map((v) => String(v));
    }

    // Spec-constant and syntax-helper fallbacks (always valid regardless of project data)
    const fieldSpecificValues: Record<string, string[]> = {
      status_code: ['OK', 'ERROR', 'UNSET'],
      kind: ['logs', 'span', 'request'],
      level: ['TRACE', 'DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL'],
      'severity.text': ['TRACE', 'DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL'],
    };

    if (fieldSpecificValues[field]) return fieldSpecificValues[field];

    // Time helper functions with descriptions (returned as "value|description" pairs)
    const timeHelpers: Record<string, string[]> = {
      timestamp: ['ago(1h)|1 hour ago', 'ago(30m)|30 minutes ago', 'ago(6h)|6 hours ago', 'ago(1d)|1 day ago', 'ago(7d)|7 days ago', 'now()|current time'],
      observed_timestamp: ['ago(1h)|1 hour ago', 'ago(30m)|30 minutes ago', 'ago(6h)|6 hours ago', 'ago(1d)|1 day ago', 'ago(7d)|7 days ago', 'now()|current time'],
    };

    if (timeHelpers[field]) return timeHelpers[field];

    if (fieldInfo?.type === 'boolean') return ['true', 'false'];

    return [];
  });
}
