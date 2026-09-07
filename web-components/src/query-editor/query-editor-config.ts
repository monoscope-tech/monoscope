import { schemaManager } from './schema-manager';
/** Kept for integrations that initialize the shared schema before mounting. */
export function initializeDefaultSchema(): void {
  schemaManager.setSchemas(['spans', 'metrics']);
  schemaManager.setDefaultSchema('spans');
}
