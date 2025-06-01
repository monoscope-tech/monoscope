import { LitElement, html, css } from 'lit';
import { customElement, property, state } from 'lit/decorators.js';
import { schemaManager } from './query-editor';

/**
 * Query builder component for GROUP BY, AGG, SORT, LIMIT operations
 * Works with the query-editor component to update the query
 */
@customElement('query-builder')
export class QueryBuilderComponent extends LitElement {
  // No shadow DOM to allow styles from parent to apply
  protected createRenderRoot = () => this;

  @property({ type: String }) queryEditorSelector = '#filterElement';

  @state() private groupByFields: string[] = [];
  @state() private aggregations: { function: string; field: string }[] = [];
  @state() private sortFields: { field: string; direction: 'asc' | 'desc' }[] = [];
  @state() private limitValue: number = 100;
  @state() private showMoreSettings: boolean = false;
  @state() private fieldsOptions: { label: string; value: string }[] = [];
  @state() private newGroupByField: string = '';
  @state() private newAggFunction: string = 'count';
  @state() private newAggField: string = '';
  @state() private newSortField: string = '';
  @state() private newSortDirection: 'asc' | 'desc' = 'asc';

  // Available aggregation functions
  private readonly aggFunctions = ['count', 'sum', 'avg', 'min', 'max', 'median', 'stdev', 'range', 'p50', 'p75', 'p90', 'p95', 'p99'];

  // Connects to the query editor when first updated
  async firstUpdated(): Promise<void> {
    await this.initializeFields();
    this.extractQueryParts();

    // Listen to changes in the query editor
    const queryEditor = document.querySelector(this.queryEditorSelector);
    if (queryEditor) {
      queryEditor.addEventListener('update-query', () => {
        this.extractQueryParts();
      });
    }

    // Set up click event handler to close popovers when clicking outside
    document.addEventListener('click', (e: MouseEvent) => {
      const target = e.target as HTMLElement;
      if (!target.closest('[popovertarget]') && !target.closest('[popover]')) {
        // Close any open popovers
        document.querySelectorAll('[popover].showing').forEach((popover: Element) => {
          (popover as HTMLElement).hidePopover?.();
        });
      }
    });
  }

  /**
   * Load available fields from the schema manager
   */
  private async initializeFields(): Promise<void> {
    try {
      const fields = await schemaManager.resolveNested(schemaManager.getDefaultSchema(), '');
      this.fieldsOptions = fields.map((field) => ({
        label: field.name,
        value: field.name,
      }));
    } catch (error) {
      console.error('Failed to load schema fields:', error);
    }
  }

  /**
   * Extract parts from the existing query
   */
  private extractQueryParts(): void {
    const queryEditor = document.querySelector(this.queryEditorSelector) as any;
    if (!queryEditor?.editor) return;

    const query = queryEditor.editor.getValue();

    // Extract GROUP BY fields
    const groupByMatch = query.match(/\bgroup\s+by\s+([^|]+?)(?:\||$)/i);
    if (groupByMatch) {
      this.groupByFields = groupByMatch[1]
        .split(',')
        .map((field) => field.trim())
        .filter(Boolean);
    } else {
      this.groupByFields = [];
    }

    // Extract SUMMARIZE/AGG functions
    const summarizeMatch = query.match(/\|?\s*summarize\s+([^|]+?)(?:\||$)/i);
    if (summarizeMatch) {
      const summarizeParts = summarizeMatch[1]
        .split(',')
        .map((part) => part.trim())
        .filter(Boolean);

      this.aggregations = summarizeParts
        .map((part) => {
          const funcMatch = part.match(/(\w+)\(([^)]+)\)/);
          if (funcMatch) {
            return {
              function: funcMatch[1].trim(),
              field: funcMatch[2].trim(),
            };
          }
          return null;
        })
        .filter(Boolean) as { function: string; field: string }[];
    } else {
      this.aggregations = [];
    }

    // Extract ORDER BY
    const orderByMatch = query.match(/\|?\s*order\s+by\s+([^|]+?)(?:\||$)/i);
    if (orderByMatch) {
      const orderParts = orderByMatch[1]
        .split(',')
        .map((part) => part.trim())
        .filter(Boolean);

      this.sortFields = orderParts.map((part) => {
        const [field, direction] = part.split(/\s+/);
        return {
          field: field.trim(),
          direction: (direction?.toLowerCase() === 'desc' ? 'desc' : 'asc') as 'asc' | 'desc',
        };
      });
    } else {
      this.sortFields = [];
    }

    // Extract LIMIT
    const limitMatch = query.match(/\|?\s*limit\s+(\d+)/i);
    this.limitValue = limitMatch ? parseInt(limitMatch[1], 10) : 100;
  }

  /**
   * Update the query in the editor
   */
  private updateQuery(): void {
    const queryEditor = document.querySelector(this.queryEditorSelector) as any;
    if (!queryEditor?.editor) return;

    let query = queryEditor.editor.getValue();

    // Replace or add GROUP BY clause
    if (this.groupByFields.length > 0) {
      const groupByStr = `group by ${this.groupByFields.join(', ')}`;
      if (query.match(/\bgroup\s+by\s+[^|]+/i)) {
        query = query.replace(/\bgroup\s+by\s+[^|]+?(?=\||$)/i, groupByStr);
      } else {
        query = query.trim();
        query += query && !query.endsWith('|') ? ' | ' : ' ';
        query += groupByStr;
      }
    } else {
      // Remove GROUP BY clause if empty
      query = query.replace(/\|?\s*group\s+by\s+[^|]+?(?=\||$)/i, '');
    }

    // Replace or add SUMMARIZE clause
    if (this.aggregations.length > 0) {
      const aggStr = `summarize ${this.aggregations.map((agg) => `${agg.function}(${agg.field})`).join(', ')}`;
      if (query.match(/\|?\s*summarize\s+[^|]+/i)) {
        query = query.replace(/\|?\s*summarize\s+[^|]+?(?=\||$)/i, ` | ${aggStr}`);
      } else {
        query = query.trim();
        query += query && !query.endsWith('|') ? ' | ' : ' ';
        query += aggStr;
      }
    } else {
      // Remove SUMMARIZE clause if empty
      query = query.replace(/\|?\s*summarize\s+[^|]+?(?=\||$)/i, '');
    }

    // Replace or add ORDER BY clause
    if (this.sortFields.length > 0) {
      const orderStr = `order by ${this.sortFields.map((sort) => `${sort.field} ${sort.direction}`).join(', ')}`;
      if (query.match(/\|?\s*order\s+by\s+[^|]+/i)) {
        query = query.replace(/\|?\s*order\s+by\s+[^|]+?(?=\||$)/i, ` | ${orderStr}`);
      } else {
        query = query.trim();
        query += query && !query.endsWith('|') ? ' | ' : ' ';
        query += orderStr;
      }
    } else {
      // Remove ORDER BY clause if empty
      query = query.replace(/\|?\s*order\s+by\s+[^|]+?(?=\||$)/i, '');
    }

    // Replace or add LIMIT clause
    if (this.limitValue) {
      const limitStr = `limit ${this.limitValue}`;
      if (query.match(/\|?\s*limit\s+\d+/i)) {
        query = query.replace(/\|?\s*limit\s+\d+(?=\||$)/i, ` | ${limitStr}`);
      } else {
        query = query.trim();
        query += query && !query.endsWith('|') ? ' | ' : ' ';
        query += limitStr;
      }
    } else {
      // Remove LIMIT clause if empty
      query = query.replace(/\|?\s*limit\s+\d+(?=\||$)/i, '');
    }

    // Clean up extra pipes and spaces
    query = query.replace(/\|\s*\|/g, '|').trim();

    // Update the editor
    queryEditor.handleAddQuery(query, true);
  }

  /**
   * Add a new GROUP BY field
   */
  private addGroupByField(): void {
    if (this.newGroupByField && !this.groupByFields.includes(this.newGroupByField)) {
      this.groupByFields = [...this.groupByFields, this.newGroupByField];
      this.newGroupByField = '';
      // Close the popover
      const popover = document.getElementById('group-by-popover');
      if (popover) {
        (popover as any).hidePopover?.();
      }
      this.updateQuery();
    }
  }

  /**
   * Remove a GROUP BY field
   */
  private removeGroupByField(index: number): void {
    this.groupByFields = this.groupByFields.filter((_, i) => i !== index);
    this.updateQuery();
  }

  /**
   * Add a new aggregation
   */
  private addAggregation(): void {
    if (this.newAggFunction && this.newAggField) {
      this.aggregations = [
        ...this.aggregations,
        {
          function: this.newAggFunction,
          field: this.newAggField,
        },
      ];
      this.newAggFunction = 'count';
      this.newAggField = '';
      // Close the popover
      const popover = document.getElementById('agg-popover');
      if (popover) {
        (popover as any).hidePopover?.();
      }
      this.updateQuery();
    }
  }

  /**
   * Remove an aggregation
   */
  private removeAggregation(index: number): void {
    this.aggregations = this.aggregations.filter((_, i) => i !== index);
    this.updateQuery();
  }

  /**
   * Add a new sort field
   */
  private addSortField(): void {
    if (this.newSortField) {
      this.sortFields = [
        ...this.sortFields,
        {
          field: this.newSortField,
          direction: this.newSortDirection,
        },
      ];
      this.newSortField = '';
      this.updateQuery();
    }
  }

  /**
   * Remove a sort field
   */
  private removeSortField(index: number): void {
    this.sortFields = this.sortFields.filter((_, i) => i !== index);
    this.updateQuery();
  }

  /**
   * Update the limit value
   */
  private updateLimit(): void {
    this.updateQuery();
  }

  /**
   * Toggle a popover
   */
  private togglePopover(id: string): void {
    const popover = document.getElementById(id);
    if (popover) {
      if ((popover as any).matches?.(':popover-open')) {
        (popover as any).hidePopover?.();
      } else {
        (popover as any).showPopover?.();
      }
    }
  }

  /**
   * Render the component
   */
  render() {
    return html`
      <div class="flex flex-wrap items-center gap-2 text-sm">
        <!-- GROUP BY Section -->
        <div class="flex items-center gap-1">
          <span class="text-xs text-textDisabled monospace">group by:</span>
          <div class="flex flex-wrap gap-1">
            ${this.groupByFields.map(
              (field, index) => html`
                <div class="badge badge-sm badge-outline gap-1 bg-fillWeak">
                  ${field}
                  <button type="button" class="btn btn-xs btn-ghost btn-circle" @click="${() => this.removeGroupByField(index)}">
                    <svg class="w-3 h-3">
                      <use href="/public/assets/svgs/fa-sprites/regular.svg#xmark"></use>
                    </svg>
                  </button>
                </div>
              `
            )}
            <button
              type="button"
              class="text-xs text-textDisabled monospace bg-bgWeaker rounded hover:bg-fillHover cursor-pointer"
              popovertarget="group-by-popover"
              style="anchor-name: --group-by-anchor"
            >
              [+]
            </button>

            <!-- GROUP BY Dropdown -->
            <div
              popover
              id="group-by-popover"
              class="dropdown menu p-2 shadow bg-bgRaised rounded-box w-64 z-50"
              style="position: absolute; position-anchor: --group-by-anchor"
            >
              <h3 class="font-medium mb-2 text-center">Add GROUP BY Field</h3>
              <div class="mb-2">
                <select
                  class="select select-bordered select-sm w-full"
                  .value="${this.newGroupByField}"
                  @change="${(e: Event) => (this.newGroupByField = (e.target as HTMLSelectElement).value)}"
                >
                  <option value="" disabled selected>Select a field</option>
                  ${this.fieldsOptions.map((field) => html` <option value="${field.value}">${field.label}</option> `)}
                </select>
              </div>
              <div class="flex justify-end gap-2">
                <button
                  type="button"
                  class="btn btn-sm btn-primary w-full"
                  ?disabled="${!this.newGroupByField}"
                  @click="${this.addGroupByField}"
                >
                  Add
                </button>
              </div>
            </div>
          </div>
        </div>

        <!-- AGG Section -->
        <div class="flex items-center ml-4 gap-1">
          <span class="text-xs text-textDisabled monospace">agg:</span>
          <div class="flex flex-wrap gap-1">
            ${this.aggregations.map(
              (agg, index) => html`
                <div class="badge badge-sm badge-outline gap-1 bg-fillWeak">
                  ${agg.function}(${agg.field})
                  <button type="button" class="btn btn-xs btn-ghost btn-circle" @click="${() => this.removeAggregation(index)}">
                    <svg class="w-3 h-3">
                      <use href="/public/assets/svgs/fa-sprites/regular.svg#xmark"></use>
                    </svg>
                  </button>
                </div>
              `
            )}
            <button
              type="button"
              class="text-xs text-textDisabled monospace bg-bgWeaker rounded hover:bg-fillHover cursor-pointer"
              popovertarget="agg-popover"
              style="anchor-name: --agg-anchor"
            >
              [+]
            </button>

            <!-- AGG Dropdown -->
            <div
              popover
              id="agg-popover"
              class="dropdown menu p-2 shadow bg-bgRaised rounded-box w-72 z-50"
              style="position: absolute; position-anchor: --agg-anchor"
            >
              <h3 class="font-medium mb-2 text-center">Add Aggregation</h3>
              <div class="form-control mb-2">
                <label class="label">
                  <span class="label-text">Function:</span>
                </label>
                <select
                  class="select select-bordered select-sm w-full"
                  .value="${this.newAggFunction}"
                  @change="${(e: Event) => (this.newAggFunction = (e.target as HTMLSelectElement).value)}"
                >
                  ${this.aggFunctions.map((func) => html` <option value="${func}">${func}</option> `)}
                </select>
              </div>
              <div class="form-control mb-2">
                <label class="label">
                  <span class="label-text">Field:</span>
                </label>
                <select
                  class="select select-bordered select-sm w-full"
                  .value="${this.newAggField}"
                  @change="${(e: Event) => (this.newAggField = (e.target as HTMLSelectElement).value)}"
                >
                  <option value="" disabled selected>Select a field</option>
                  ${this.fieldsOptions.map((field) => html` <option value="${field.value}">${field.label}</option> `)}
                </select>
              </div>
              <div class="flex justify-end gap-2">
                <button
                  type="button"
                  class="btn btn-sm btn-primary w-full"
                  ?disabled="${!this.newAggFunction || !this.newAggField}"
                  @click="${this.addAggregation}"
                >
                  Add
                </button>
              </div>
            </div>
          </div>
        </div>

        <!-- More Options Button -->
        <div class="ml-auto">
          <button
            type="button"
            class="text-xs text-textDisabled monospace bg-bgWeaker px-2 py-1 rounded hover:bg-fillHover cursor-pointer"
            popovertarget="more-settings-popover"
            style="anchor-name: --more-settings-anchor"
          >
            [more ▾]
          </button>

          <!-- More Settings Dropdown -->
          <div
            popover
            id="more-settings-popover"
            class="dropdown menu p-2 shadow bg-bgRaised rounded-box w-80 z-50"
            style="position: absolute; position-anchor: --more-settings-anchor"
          >
            <h3 class="text-sm font-medium mb-2">More Settings</h3>

            <!-- Sort By -->
            <div class="flex flex-wrap items-center gap-2 mb-3">
              <span class="text-xs monospace">📐 sort by:</span>

              <!-- Existing Sort Fields -->
              <div class="flex flex-wrap gap-1">
                ${this.sortFields.map(
                  (sort, index) => html`
                    <div class="badge badge-sm badge-outline gap-1 bg-fillWeaker">
                      ${sort.field} ${sort.direction}
                      <button type="button" class="btn btn-xs btn-ghost btn-circle" @click="${() => this.removeSortField(index)}">
                        <svg class="w-3 h-3">
                          <use href="/public/assets/svgs/fa-sprites/regular.svg#xmark"></use>
                        </svg>
                      </button>
                    </div>
                  `
                )}
              </div>

              <!-- Add Sort Field -->
              <div class="flex gap-1">
                <select
                  class="select select-bordered select-xs"
                  .value="${this.newSortField}"
                  @change="${(e: Event) => (this.newSortField = (e.target as HTMLSelectElement).value)}"
                >
                  <option value="" disabled selected>Select field</option>
                  ${this.fieldsOptions.map((field) => html` <option value="${field.value}">${field.label}</option> `)}
                </select>

                <select
                  class="select select-bordered select-xs"
                  .value="${this.newSortDirection}"
                  @change="${(e: Event) => (this.newSortDirection = (e.target as HTMLSelectElement).value as 'asc' | 'desc')}"
                >
                  <option value="asc">asc</option>
                  <option value="desc">desc</option>
                </select>

                <button
                  type="button"
                  class="btn btn-xs btn-ghost btn-square"
                  ?disabled="${!this.newSortField}"
                  @click="${this.addSortField}"
                >
                  <span class="text-xs">✓</span>
                </button>
              </div>
            </div>

            <!-- Limit -->
            <div class="flex items-center gap-2 mt-2">
              <span class="text-xs monospace">🔢 limit:</span>
              <input
                type="number"
                class="input input-bordered input-xs w-24"
                min="1"
                max="10000"
                .value="${this.limitValue.toString()}"
                @change="${(e: Event) => {
                  this.limitValue = parseInt((e.target as HTMLInputElement).value);
                  this.updateLimit();
                }}"
              />
              <button type="button" class="btn btn-xs btn-primary" @click="${this.updateLimit}">Set</button>
            </div>
          </div>
        </div>
      </div>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    'query-builder': QueryBuilderComponent;
  }
}

