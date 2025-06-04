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
  @state() private limitValue: number | null = null;
  @state() private showMoreSettings: boolean = false;
  @state() private fieldsOptions: { label: string; value: string; type: string }[] = [];
  @state() private newGroupByField: string = '';
  @state() private newAggFunction: string = '';
  @state() private newAggField: string = '';
  @state() private selectedAggFunction: string = '';
  @state() private aggSearchTerm: string = '';
  @state() private filteredAggFunctions: string[] = [];
  @state() private filteredFields: { label: string; value: string }[] = [];
  @state() private showFieldsColumn: boolean = false;
  @state() private visualizationType: string = '';
  @state() private newSortField: string = '';
  @state() private newSortDirection: 'asc' | 'desc' = 'asc';
  
  // New Group By UI state
  @state() private groupBySearchTerm: string = '';
  @state() private showGroupByFieldsColumn: boolean = false;
  @state() private filteredGroupByFields: { label: string; value: string }[] = [];
  @state() private enableBinning: boolean = false;
  @state() private enableAutoBin: boolean = false;
  @state() private binValue: number = 5;

  // Available aggregation functions
  private readonly aggFunctions = ['count', 'sum', 'avg', 'min', 'max', 'median', 'stdev', 'range', 'p50', 'p75', 'p90', 'p95', 'p99'];

  // Connects to the query editor when first updated
  async firstUpdated(): Promise<void> {
    // Manual method binding is not needed with arrow functions in the template
    await this.initializeFields();
    this.extractQueryParts();

    // Listen to changes in the query editor
    const queryEditor = document.querySelector(this.queryEditorSelector);
    if (queryEditor) {
      queryEditor.addEventListener('update-query', () => {
        this.extractQueryParts();
      });

      // Set up a schema change observer
      if ((window as any).schemaManager) {
        const originalSetSchemaData = (window as any).schemaManager.setSchemaData;
        (window as any).schemaManager.setSchemaData = (...args: any[]) => {
          // Call the original method
          const result = originalSetSchemaData.apply((window as any).schemaManager, args);

          // Then refresh our field suggestions
          setTimeout(() => this.refreshFieldSuggestions(), 100);

          return result;
        };
      }
    }

    // Add direct event handlers to add buttons after the component is rendered
    setTimeout(() => {
      const addGroupByBtn = this.querySelector('#add-group-by-btn');
      if (addGroupByBtn) {
        console.log('Found add group by button, adding direct event listener');
        addGroupByBtn.addEventListener('click', () => {
          console.log('Group by button clicked directly');
          this.addGroupByField();
        });
      }

      const addAggBtn = this.querySelector('#add-agg-btn');
      if (addAggBtn) {
        console.log('Found add agg button, adding direct event listener');
        addAggBtn.addEventListener('click', () => {
          console.log('Agg button clicked directly');
          this.addAggregation();
        });
      }

      const addSortBtn = this.querySelector('#add-sort-btn');
      if (addSortBtn) {
        console.log('Found add sort button, adding direct event listener');
        addSortBtn.addEventListener('click', () => {
          console.log('Sort button clicked directly');
          this.addSortField();
        });
      }
    }, 500);

    // Set up click event handler to close popovers when clicking outside
    document.addEventListener('click', (e: MouseEvent) => {
      const target = e.target as HTMLElement;
      if (!target.closest('[popovertarget]') && !target.closest('[popover]') && !target.closest('#sort-by-button')) {
        // Close any open popovers
        document.querySelectorAll('[popover].showing').forEach((popover: Element) => {
          (popover as HTMLElement).hidePopover?.();
        });
      }
    });
    
    // Set up additional behavior for nested popovers
    setTimeout(() => {
      const moreSettingsPopover = document.getElementById('more-settings-popover');
      const sortByButton = document.getElementById('sort-by-button');
      const sortByPopover = document.getElementById('sort-by-popover') as HTMLElement;
      
      if (moreSettingsPopover && sortByButton && sortByPopover) {
        // When more settings popover is hidden, also hide the sort popover
        moreSettingsPopover.addEventListener('beforetoggle', (e) => {
          if (e.newState === 'closed') {
            if (sortByPopover && (sortByPopover as any).matches?.(':popover-open')) {
              (sortByPopover as any).hidePopover?.();
            }
          }
        });
        
        // Position the sort popover relative to the button
        const positionSortPopover = () => {
          const buttonRect = sortByButton.getBoundingClientRect();
          // Position to the right of the button
          sortByPopover.style.left = `${buttonRect.right + 5}px`;
          sortByPopover.style.top = `${buttonRect.top}px`;
        };
        
        // Click event for the sort by button
        sortByButton.addEventListener('click', (e) => {
          e.stopPropagation();
          positionSortPopover();
          
          // Toggle the popover
          if ((sortByPopover as any).matches?.(':popover-open')) {
            sortByPopover.hidePopover?.();
          } else {
            sortByPopover.showPopover?.();
          }
        });
        
        // Event to make the subpopover keyboard-accessible
        sortByButton.addEventListener('keydown', (e) => {
          if (e.key === 'Enter' || e.key === ' ') {
            e.preventDefault();
            positionSortPopover();
            sortByPopover.showPopover?.();
          }
        });
        
        // Stop propagation for clicks inside the sort popover without closing it
        sortByPopover.addEventListener('click', (e) => {
          // Don't close popovers when selecting options - we'll close them after selection is complete
          // The actual closing will happen in the @click handler for each field item
          e.stopPropagation();
        });
        
        // Add event listener to close the sort popover only when clicking outside of sort-by-popover AND more-settings-popover
        document.addEventListener('click', (e) => {
          if (sortByPopover.matches?.(':popover-open')) {
            const target = e.target as HTMLElement;
            // Only close if the click is outside both popovers and not on the trigger button
            if (!target.closest('#sort-by-popover') && 
                !target.closest('#sort-by-button') && 
                !target.closest('#more-settings-popover')) {
              sortByPopover.hidePopover?.();
            }
          }
        });
      }
    }, 500); // Delay to ensure elements are loaded
  }

  /**
   * Load available fields from the schema manager
   */
  private async initializeFields(): Promise<void> {
    try {
      // Get raw schema data to include nested fields
      const schemaData = schemaManager.getSchemaData(schemaManager.getDefaultSchema());
      const fields = this.extractAllFields(schemaData);
      console.log('Loaded initial fields:', fields.length);

      // Update fields options array
      this.fieldsOptions = fields;

      // Ensure UI updates with the new options
      this.requestUpdate();

      // Schedule another update after render is complete to ensure datalists exist
      setTimeout(() => this.refreshFieldSuggestions(), 100);
    } catch (error) {
      console.error('Failed to load schema fields:', error);
    }
  }
  
  /**
   * Extract all fields including deeply nested ones from schema data
   */
  private extractAllFields(schema: any, prefix: string = ''): { label: string; value: string; type: string }[] {
    let fields: { label: string; value: string; type: string }[] = [];
    
    if (!schema || typeof schema !== 'object') return fields;
    
    // Check if this is a schema with 'fields' property (format from schema manager)
    if (schema.fields && typeof schema.fields === 'object') {
      for (const [key, info] of Object.entries(schema.fields)) {
        const fieldType = (info as any).fieldType || (info as any).type || 'string';
        
        fields.push({
          label: `${key} (${fieldType})`,
          value: key,
          type: fieldType
        });
      }
      return fields;
    }
    
    // Process properties if this is an object with properties
    if (schema.properties && typeof schema.properties === 'object') {
      for (const [key, value] of Object.entries(schema.properties)) {
        const fieldPath = prefix ? `${prefix}.${key}` : key;
        const fieldType = (value as any).type || 'object';
        
        // Add this field
        fields.push({
          label: `${fieldPath} (${fieldType})`,
          value: fieldPath,
          type: fieldType
        });
        
        // Recursively process nested objects
        if (fieldType === 'object' && (value as any).properties) {
          fields = fields.concat(this.extractAllFields((value as any), fieldPath));
        }
        
        // Handle arrays with items
        if (fieldType === 'array' && (value as any).items) {
          const itemsType = (value as any).items.type || 'any';
          
          // For array of objects, extract their properties too
          if (itemsType === 'object' && (value as any).items.properties) {
            fields = fields.concat(this.extractAllFields((value as any).items, `${fieldPath}[]`));
          }
        }
      }
    }
    
    // Log fields count
    console.log(`Extracted ${fields.length} fields with prefix: ${prefix || 'root'}`);
    return fields;
  }

  /**
   * Refreshes field suggestions based on current schema
   * This method is public to allow external components to trigger a refresh
   */
  public async refreshFieldSuggestions(path: string = ''): Promise<void> {
    try {
      console.log('Refreshing field suggestions for path:', path || 'root');

      // Get raw schema data
      const schemaData = schemaManager.getSchemaData(schemaManager.getDefaultSchema());
      
      // If we're refreshing root fields, update the fieldsOptions property
      if (!path) {
        const fields = this.extractAllFields(schemaData);
        this.fieldsOptions = fields;
        this.requestUpdate();
      } else {
        // For specific paths, navigate to that path in the schema
        let targetSchema = schemaData;
        const pathParts = path.split('.');
        
        for (const part of pathParts) {
          if (targetSchema?.properties?.[part]) {
            targetSchema = targetSchema.properties[part];
          } else {
            // Path not found
            console.warn(`Path ${path} not found in schema`);
            return;
          }
        }
        
        // Extract fields from this specific path
        const fields = this.extractAllFields(targetSchema, path);
        // Add these fields to the options without replacing everything
        this.fieldsOptions = [...this.fieldsOptions, ...fields];
        this.requestUpdate();
      }
    } catch (error) {
      console.error(`Failed to refresh schema fields for path ${path}:`, error);
    }
  }

  /**
   * Handles input in field suggestions
   */
  private async handleFieldInput(e: Event, fieldType: 'group' | 'agg' | 'sort'): Promise<void> {
    try {
      const input = e.target as HTMLInputElement;
      const value = input.value;
      console.log(`Field input (${fieldType}):`, value);

      // Update the appropriate field value
      switch (fieldType) {
        case 'group':
          this.newGroupByField = value;
          break;
        case 'agg':
          this.newAggField = value;
          break;
        case 'sort':
          this.newSortField = value;
          break;
      }

      // Check if we need to update the suggestions based on dot notation
      if (value.endsWith('.')) {
        // Extract the parent path
        const parentPath = value.substring(0, value.length - 1);
        console.log(`Getting nested fields for path: ${parentPath}`);

        // Fetch nested fields from schema manager
        try {
          const nestedFields = await schemaManager.resolveNested(schemaManager.getDefaultSchema(), parentPath);
          console.log(`Found ${nestedFields.length} nested fields for ${parentPath}`);

          // Update the fieldsOptions property directly for template rendering
          // This is safer than direct DOM manipulation
          this.fieldsOptions = nestedFields.map((field) => ({
            label: `${field.name} (${field.type})`,
            value: `${parentPath}.${field.name}`,
          }));

          // Request a UI update
          this.requestUpdate();
        } catch (error) {
          console.error(`Failed to get nested fields for ${parentPath}:`, error);
        }
      }
    } catch (error) {
      console.error(`Error in handleFieldInput for ${fieldType}:`, error);
    }
  }

  /**
   * Extract parts from the existing query
   */
  private extractQueryParts(): void {
    const queryEditor = document.querySelector(this.queryEditorSelector) as any;
    if (!queryEditor?.editor) return;

    const query = queryEditor.editor.getValue();

    // Extract GROUP BY fields from summarize statement
    // Look for both KQL style summarize and older group by format
    const summarizeMatch = query.match(/\|?\s*summarize\s+([^|]+?)(?:by\s+([^|]+?))?(?:\||$)/i);
    const groupByMatch = query.match(/\bgroup\s+by\s+([^|]+?)(?:\||$)/i);
    
    // First try to extract from summarize statement (KQL style)
    if (summarizeMatch && summarizeMatch[2]) {
      // We have a "by" clause in the summarize statement
      const groupByText = summarizeMatch[2].trim();
      const fields: string[] = [];
      let currentField = '';
      let parenCount = 0;
      
      // Process character by character to handle nested parentheses correctly
      for (let i = 0; i < groupByText.length; i++) {
        const char = groupByText[i];
        
        if (char === '(') {
          parenCount++;
          currentField += char;
        } else if (char === ')') {
          parenCount--;
          currentField += char;
        } else if (char === ',' && parenCount === 0) {
          // Only split on commas at the top level
          if (currentField.trim()) {
            fields.push(currentField.trim());
          }
          currentField = '';
        } else {
          currentField += char;
        }
      }
      
      // Add the last field if there is one
      if (currentField.trim()) {
        fields.push(currentField.trim());
      }
      
      this.groupByFields = fields.filter(Boolean);
    } 
    // If not found in summarize statement, check for standalone group by (backward compatibility)
    else if (groupByMatch) {
      // Process group by fields with special handling for bin functions
      const groupByText = groupByMatch[1].trim();
      const fields: string[] = [];
      let currentField = '';
      let parenCount = 0;
      
      // Process character by character to handle nested parentheses correctly
      for (let i = 0; i < groupByText.length; i++) {
        const char = groupByText[i];
        
        if (char === '(') {
          parenCount++;
          currentField += char;
        } else if (char === ')') {
          parenCount--;
          currentField += char;
        } else if (char === ',' && parenCount === 0) {
          // Only split on commas at the top level
          if (currentField.trim()) {
            fields.push(currentField.trim());
          }
          currentField = '';
        } else {
          currentField += char;
        }
      }
      
      // Add the last field if there is one
      if (currentField.trim()) {
        fields.push(currentField.trim());
      }
      
      this.groupByFields = fields.filter(Boolean);
    } else {
      this.groupByFields = [];
    }

    // Extract SUMMARIZE/AGG functions
    // We've already captured the summarize match above, no need to match again
    if (summarizeMatch && summarizeMatch[1]) {
      // For the aggregation part, we need to handle the case where there's a "by" clause
      // Extract just the aggregation part by splitting on " by " if it exists
      const aggregationPart = summarizeMatch[1].split(/\s+by\s+/i)[0].trim();
      
      const summarizeParts = aggregationPart
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

    // Extract SORT BY / ORDER BY
    // Support both KQL "sort by" and SQL-style "order by"
    const sortByMatch = query.match(/\|?\s*(sort|order)\s+by\s+([^|]+?)(?:\||$)/i);
    if (sortByMatch) {
      const sortParts = sortByMatch[2]
        .split(',')
        .map((part) => part.trim())
        .filter(Boolean);

      this.sortFields = sortParts.map((part) => {
        const [field, direction] = part.split(/\s+/);
        return {
          field: field.trim(),
          direction: (direction?.toLowerCase() === 'desc' ? 'desc' : 'asc') as 'asc' | 'desc',
        };
      });
    } else {
      this.sortFields = [];
    }

    // Extract TAKE - Only support KQL "take" syntax
    const takeMatch = query.match(/\|?\s*take\s+(\d+)/i);
    this.limitValue = takeMatch ? parseInt(takeMatch[1], 10) : null;
  }

  /**
   * Update the query in the editor
   */
  private updateQuery(): void {
    console.log('Updating query with:', {
      groupByFields: this.groupByFields,
      aggregations: this.aggregations,
      sortFields: this.sortFields,
      limitValue: this.limitValue,
    });

    const queryEditor = document.querySelector(this.queryEditorSelector) as any;
    if (!queryEditor?.editor) {
      console.error('Query editor not found or missing editor instance');
      return;
    }

    let query = queryEditor.editor.getValue();

    // Make summarize clause with GROUP BY
    if (this.aggregations.length > 0) {
      // The summarize clause will include both aggregations AND group by
      const aggPart = this.aggregations.map((agg) => `${agg.function}(${agg.field})`).join(', ');
      const groupByPart = this.groupByFields.length > 0 ? ` by ${this.groupByFields.join(', ')}` : '';
      
      const summarizeStr = `summarize ${aggPart}${groupByPart}`;
      
      // Check if a summarize clause already exists in the query
      if (query.match(/\|?\s*summarize\s+[^|]+/i)) {
        query = query.replace(/\|?\s*summarize\s+[^|]+?(?=\||$)/i, ` | ${summarizeStr}`);
      } else {
        query = query.trim();
        query += query && !query.endsWith('|') ? ' | ' : ' ';
        query += summarizeStr;
      }
    } else if (this.groupByFields.length > 0) {
      // If we have group by fields but no aggregations, add count(*) aggregation
      const groupByPart = this.groupByFields.join(', ');
      const summarizeStr = `summarize count(*) by ${groupByPart}`;
      
      // Check if a summarize clause already exists in the query
      if (query.match(/\|?\s*summarize\s+[^|]+/i)) {
        query = query.replace(/\|?\s*summarize\s+[^|]+?(?=\||$)/i, ` | ${summarizeStr}`);
      } else {
        query = query.trim();
        query += query && !query.endsWith('|') ? ' | ' : ' ';
        query += summarizeStr;
      }
    } else {
      // Remove any summarize clause if no aggregations or group by fields
      query = query.replace(/\|?\s*summarize\s+[^|]+?(?=\||$)/i, '');
    }

    // Summarize clause already handled above with group by integration

    // Replace or add SORT BY clause (KQL uses "sort by" instead of "order by")
    if (this.sortFields.length > 0) {
      const sortStr = `sort by ${this.sortFields.map((sort) => `${sort.field} ${sort.direction}`).join(', ')}`;
      
      // Match both "sort by" and "order by" for backward compatibility
      if (query.match(/\|?\s*(sort|order)\s+by\s+[^|]+/i)) {
        query = query.replace(/\|?\s*(sort|order)\s+by\s+[^|]+?(?=\||$)/i, ` | ${sortStr}`);
      } else {
        query = query.trim();
        query += query && !query.endsWith('|') ? ' | ' : ' ';
        query += sortStr;
      }
    } else {
      // Remove SORT BY or ORDER BY clause if empty
      query = query.replace(/\|?\s*(sort|order)\s+by\s+[^|]+?(?=\||$)/i, '');
    }

    // Replace or add TAKE clause (KQL syntax)
    // Only add if limitValue is explicitly set (not null)
    if (this.limitValue !== null) {
      const takeStr = `take ${this.limitValue}`;
      
      // Check if there's an existing take clause
      if (query.match(/\|?\s*take\s+\d+/i)) {
        query = query.replace(/\|?\s*take\s+\d+(?=\||$)/i, ` | ${takeStr}`);
      } else {
        query = query.trim();
        query += query && !query.endsWith('|') ? ' | ' : ' ';
        query += takeStr;
      }
    } else {
      // Remove TAKE clause if not set
      query = query.replace(/\|?\s*take\s+\d+(?=\||$)/i, '');
    }

    // Clean up extra pipes and spaces
    query = query.replace(/\|\s*\|/g, '|').trim();

    // Update the editor
    queryEditor.handleAddQuery(query, true);
  }

  /**
   * Add a new GROUP BY field
   */
  public addGroupByField(): void {
    // Debug logging
    console.log('Adding group by field:', this.newGroupByField);

    // Make sure we have a non-empty field that's not already in the list
    if (this.newGroupByField?.trim()) {
      try {
        let fieldValue = this.newGroupByField;
        
        // If binning is enabled, add bin function wrapper
        if (this.enableBinning) {
          if (this.enableAutoBin) {
            fieldValue = `bin_auto(${fieldValue})`;
          } else {
            fieldValue = `bin(${fieldValue}, ${this.binValue})`;
          }
        }
        
        // Check if the exact field or binned version of it already exists
        const isDuplicate = this.groupByFields.some(existingField => {
          if (existingField === fieldValue) {
            return true; // Exact match
          }
          
          // Check if it's the same field but with different binning
          if (this.enableBinning) {
            const fieldWithoutBin = fieldValue.replace(/bin(_auto)?\([^,]+(?:, \d+)?\)/, '').trim();
            const existingWithoutBin = existingField.replace(/bin(_auto)?\([^,]+(?:, \d+)?\)/, '').trim();
            return fieldWithoutBin === existingWithoutBin;
          }
          
          return false;
        });
        
        if (isDuplicate) {
          console.warn('Cannot add duplicate group by field:', fieldValue);
          return;
        }
        
        // Add to fields array
        this.groupByFields = [...this.groupByFields, fieldValue];
        // Clear the input
        this.newGroupByField = '';
        // Reset binning options to default (no binning)
        this.enableBinning = false;
        this.enableAutoBin = false;
        // Make sure the radio button for "no bin" is selected after adding
        setTimeout(() => {
          const noBinRadio = document.querySelector('input[name="bin-type"]:first-of-type') as HTMLInputElement;
          if (noBinRadio) {
            noBinRadio.checked = true;
          }
        }, 50);
        // Force UI update
        this.requestUpdate();
        // Close the popover if open
        const popover = document.getElementById('group-by-popover');
        if (popover) {
          (popover as any).hidePopover?.();
        }
        // Update the query in the editor
        this.updateQuery();

        // Directly render the updated group by fields
        console.log('Group by fields now:', this.groupByFields);
      } catch (error) {
        console.error('Error adding group by field:', error);
      }
    } else {
      console.warn('Cannot add empty group by field:', this.newGroupByField);
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
   * Handle clicking on an aggregation function
   */
  private handleAggFunctionClick(func: string): void {
    // Clear previous selection if different function is chosen
    if (this.newAggFunction !== func) {
      this.newAggField = '';
    }
    
    // Set new function
    this.newAggFunction = func;
    
    if (func === 'count') {
      // For count(), complete immediately
      this.completeAggregation();
    } else {
      // For other functions, show fields and focus on search
      this.showFieldsColumn = true;
      this.aggSearchTerm = ''; // Clear search to start fresh for fields
      this.filteredFields = []; // Reset filtered fields
      
      // Make sure we have fields loaded
      if (this.fieldsOptions.length === 0) {
        // If fields aren't loaded yet, try loading them again
        this.initializeFields().then(() => {
          console.log(`Fields loaded after selecting ${func}, count:`, this.fieldsOptions.length);
          this.requestUpdate();
        });
      }
      
      this.requestUpdate();
    }
  }

  /**
   * Show the fields selection for group by
   */
  private showGroupByFieldsSelector(): void {
    this.showGroupByFieldsColumn = true;
    this.groupBySearchTerm = ''; // Clear search term
    this.filteredGroupByFields = []; // Reset filtered fields
    
    // Make sure we have fields loaded
    if (this.fieldsOptions.length === 0) {
      this.initializeFields().then(() => {
        console.log(`Fields loaded for group by, count:`, this.fieldsOptions.length);
        this.requestUpdate();
      });
    }
    
    this.requestUpdate();
  }
  
  /**
   * Reset the aggregation state
   */
  private resetAggregationState(): void {
    this.newAggFunction = 'count';
    this.newAggField = '';
    this.selectedAggFunction = '';
    this.aggSearchTerm = '';
    this.filteredAggFunctions = [];
    this.filteredFields = [];
    this.showFieldsColumn = false;
    this.requestUpdate();
  }
  
  /**
   * Filter aggregation options based on search term
   */
  private filterAggregationOptions(e: Event): void {
    const searchTerm = (e.target as HTMLInputElement).value.toLowerCase();
    this.aggSearchTerm = searchTerm;
    
    // When the fields column is showing, filter fields
    if (this.showFieldsColumn) {
      if (this.fieldsOptions.length === 0) {
        // Try to load fields if they're not available
        console.log("Field options are empty, trying to load them");
        this.initializeFields().then(() => {
          this.filterFieldsBySearchTerm(searchTerm);
          this.requestUpdate();
        });
      } else {
        this.filterFieldsBySearchTerm(searchTerm);
      }
    }
    
    this.requestUpdate();
  }
  
  /**
   * Filter group by field options based on search term
   */
  private filterGroupByOptions(e: Event): void {
    const searchTerm = (e.target as HTMLInputElement).value.toLowerCase();
    this.groupBySearchTerm = searchTerm;
    
    if (this.fieldsOptions.length === 0) {
      // Try to load fields if they're not available
      console.log("Field options are empty, trying to load them");
      this.initializeFields().then(() => {
        this.filterGroupByFieldsBySearchTerm(searchTerm);
        this.requestUpdate();
      });
    } else {
      this.filterGroupByFieldsBySearchTerm(searchTerm);
    }
    
    this.requestUpdate();
  }
  
  /**
   * Filter group by fields by search term
   */
  private filterGroupByFieldsBySearchTerm(searchTerm: string): void {
    this.filteredGroupByFields = this.fieldsOptions.filter(field => 
      field.value.toLowerCase().includes(searchTerm) || 
      field.label.toLowerCase().includes(searchTerm)
    );
    console.log(`Filtered group by fields by "${searchTerm}": found ${this.filteredGroupByFields.length} matches out of ${this.fieldsOptions.length} total fields`);
  }
  
  /**
   * Filter fields by search term
   */
  private filterFieldsBySearchTerm(searchTerm: string): void {
    this.filteredFields = this.fieldsOptions.filter(field => 
      field.value.toLowerCase().includes(searchTerm) || 
      field.label.toLowerCase().includes(searchTerm)
    );
    console.log(`Filtered fields by "${searchTerm}": found ${this.filteredFields.length} matches out of ${this.fieldsOptions.length} total fields`);
  }
  
  /**
   * Handle Enter key in the aggregation search
   */
  private handleAggregationEnter(): void {
    if (!this.showFieldsColumn) {
      // If we're still selecting an aggregation function
      if (this.filteredAggFunctions.length === 1) {
        // If there's only one matching function, select it
        this.handleAggFunctionClick(this.filteredAggFunctions[0]);
      }
    } else {
      // If we're selecting a field
      if (this.filteredFields.length === 1) {
        // If there's only one matching field, select it and complete
        this.newAggField = this.filteredFields[0].value;
        this.completeAggregation();
      }
    }
  }
  
  /**
   * Get an icon for a field based on its type and name
   */
  private getFieldIcon(fieldType: string, fieldValue: string): string {
    // First check by field name
    if (fieldValue.includes('duration')) return '⏱';
    if (fieldValue.includes('time')) return '⏱';
    if (fieldValue.includes('status')) return 'N';
    if (fieldValue.includes('code')) return 'N';
    if (fieldValue.includes('error')) return 'N';
    
    // Then check by field type
    if (fieldType === 'number' || fieldType === 'integer') return 'N';
    if (fieldType === 'string') return '';
    if (fieldType === 'boolean') return '';
    if (fieldType === 'object') return '';
    if (fieldType === 'array') return '';
    
    return '';
  }
  
  /**
   * Completes the aggregation process
   */
  private completeAggregation(): void {
    // For count(), we don't need a field
    if (this.newAggFunction === 'count') {
      this.aggregations = [
        ...this.aggregations,
        {
          function: this.newAggFunction,
          field: '*',
        },
      ];
    } else if (this.newAggFunction && this.newAggField?.trim()) {
      // For other functions, we need both function and field
      this.aggregations = [
        ...this.aggregations,
        {
          function: this.newAggFunction,
          field: this.newAggField,
        },
      ];
    } else {
      // If we don't have required info, return without doing anything
      return;
    }
    
    // Reset form fields
    this.resetAggregationState();
    
    // Close the popover if open
    const popover = document.getElementById('agg-popover');
    if (popover) {
      (popover as any).hidePopover?.();
    }
    
    // Update the query in the editor
    this.updateQuery();
  }

  /**
   * Add a new aggregation (for backward compatibility)
   */
  public addAggregation(): void {
    // Debug logging
    console.log('Adding aggregation:', this.newAggFunction, this.newAggField);

    if (this.newAggFunction && this.newAggField?.trim()) {
      // Add to aggregations array
      this.aggregations = [
        ...this.aggregations,
        {
          function: this.newAggFunction,
          field: this.newAggField,
        },
      ];
      // Reset form fields
      this.newAggFunction = 'count';
      this.newAggField = '';
      this.selectedAggFunction = '';
      // Force UI update
      this.requestUpdate();
      // Close the popover if open
      const popover = document.getElementById('agg-popover');
      if (popover) {
        (popover as any).hidePopover?.();
      }
      // Update the query in the editor
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
  public addSortField(): void {
    // Debug logging
    console.log('Adding sort field:', this.newSortField, this.newSortDirection);

    if (this.newSortField?.trim()) {
      // Check if this field is already being sorted
      const existingIndex = this.sortFields.findIndex(sort => sort.field === this.newSortField);
      
      if (existingIndex >= 0) {
        // If field already exists, update its direction instead of adding a duplicate
        this.sortFields[existingIndex].direction = this.newSortDirection;
      } else {
        // Add to sort fields array
        this.sortFields = [
          ...this.sortFields,
          {
            field: this.newSortField,
            direction: this.newSortDirection,
          },
        ];
      }
      
      // Clear input
      this.newSortField = '';
      // Force UI update
      this.requestUpdate();
      // Update the query
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
  // Removed updated method to avoid rendering issues

  connectedCallback(): void {
    super.connectedCallback();

    // Add direct click handler for the whole component
    this.addEventListener('click', this.handleComponentClick);
  }

  disconnectedCallback(): void {
    // Clean up event listeners
    this.removeEventListener('click', this.handleComponentClick);
    super.disconnectedCallback();
  }

  // Handle all clicks in the component
  private handleComponentClick = (e: MouseEvent): void => {
    const target = e.target as HTMLElement;

    // Check which button was clicked
    if (target.closest('#add-group-by-btn')) {
      console.log('Add group by button clicked via direct listener');
      e.preventDefault();
      e.stopPropagation();
      this.addGroupByField();
    } else if (target.closest('#add-agg-btn')) {
      console.log('Add aggregation button clicked via direct listener');
      e.preventDefault();
      e.stopPropagation();
      this.completeAggregation();
    } else if (target.closest('#add-sort-btn')) {
      console.log('Add sort button clicked via direct listener');
      e.preventDefault();
      e.stopPropagation();
      this.addSortField();
    } else if (target.closest('#more-settings-popover') || target.closest('#sort-by-popover')) {
      // Don't propagate clicks within either popover to avoid closing them
      e.stopPropagation();
    }
  };
  
  /**
   * Toggle between sort directions
   */
  private toggleSortDirection(index: number): void {
    // Toggle between 'asc' and 'desc'
    this.sortFields[index].direction = 
      this.sortFields[index].direction === 'asc' ? 'desc' : 'asc';
    
    // Update query
    this.updateQuery();
  }

  render() {
    return html`
      <div class="flex flex-wrap items-center gap-2 text-sm">
        <!-- AGG Section -->
        <div class="flex items-center gap-1">
          <span class="text-xs text-textDisabled monospace" data-tippy-content="Apply aggregation functions like count, sum, avg, etc.">agg:</span>
          <div class="flex flex-wrap gap-1">
            ${this.aggregations.map(
              (agg, index) => html`
                <div class="text-xs text-textDisabled monospace bg-bgWeaker">
                  [<span class="text-textStrong">${agg.function}(${agg.field})</span>
                  <span class="cursor-pointer" data-tippy-content="Remove aggregation" @click="${() => this.removeAggregation(index)}">✕</span>]
                </div>
              `
            )}
            <button
              type="button"
              class="text-xs text-textDisabled monospace bg-bgWeaker rounded hover:bg-fillHover cursor-pointer"
              popovertarget="agg-popover"
              style="anchor-name: --agg-anchor"
              data-tippy-content="Add an aggregation function"
            >
              [+]
            </button>

            <!-- AGG Dropdown -->
            <div
              popover
              id="agg-popover"
              class="dropdown menu p-2 shadow-md bg-bgRaised rounded-box w-[500px] z-50 border border-strokeWeak"
              style="position: absolute; position-anchor: --agg-anchor"
            >
              <!-- Visualization Type Input -->
              <div class="mb-3">
                <input
                  type="text"
                  class="input input-bordered input-md w-full px-3 py-2 focus:outline-none"
                  placeholder="Visualization type..."
                  .value="${this.visualizationType}"
                  @input="${(e: Event) => this.visualizationType = (e.target as HTMLInputElement).value}"
                  autofocus
                />
              </div>
              
              <!-- Two-column layout -->
              <div class="flex mb-3 border rounded">
                <!-- Aggregations column -->
                <div class="w-1/2 border-r">
                  <div class="p-1 bg-bgWeaker font-medium border-b monospace">Aggregation</div>
                  <div class="max-h-60 overflow-y-auto">
                    ${this.aggFunctions.map((func) => html`
                      <div 
                        class="p-2 hover:bg-fillHover cursor-pointer monospace ${this.newAggFunction === func ? 'bg-fillHover font-medium' : ''}"
                        @click="${() => this.handleAggFunctionClick(func)}"
                      >
                        ${this.newAggFunction === func ? '✓ ' : ''}${func}${func !== 'count' ? '(...)' : '(*)'}
                      </div>
                    `)}
                  </div>
                </div>
                
                <!-- Fields column -->
                <div class="w-1/2">
                  <div class="p-1 bg-bgWeaker font-medium border-b monospace">Fields</div>
                  <div class="max-h-60 overflow-y-auto">
                    ${this.showFieldsColumn && this.fieldsOptions.length > 0 ? 
                      (this.filteredFields.length > 0 ? this.filteredFields : this.fieldsOptions).map((field) => html`
                        <div 
                          class="p-2 hover:bg-fillHover cursor-pointer monospace ${this.newAggField === field.value ? 'bg-fillHover font-medium' : ''}"
                          @click="${() => { 
                            this.newAggField = field.value; 
                            this.completeAggregation();
                          }}"
                        >
                          ${field.value} 
                          <span class="float-right text-xs text-textDisabled p-1 rounded-sm bg-bgWeaker">
                            ${this.getFieldIcon(field.type, field.value)}
                          </span>
                        </div>
                      `)
                      : html`<div class="p-2 text-center text-textDisabled">Select an aggregation first</div>`
                    }
                  </div>
                </div>
              </div>
              
              <!-- No action buttons -->
            </div>
          </div>
        </div>

        <!-- GROUP BY Section -->
        ${this.aggregations.length > 0 ? html`
        <div class="flex items-center ml-4 gap-1">
          <span class="text-xs text-textDisabled monospace" data-tippy-content="Group results by field value">by:</span>
          <div class="flex flex-wrap gap-1">
            ${this.groupByFields.map(
              (field, index) => {
                // Determine if this is a binned field
                const isBinned = field.includes('bin(') || field.includes('bin_auto(');
                // For display purposes, ensure we show the full function with parameter
                return html`
                  <div class="text-xs text-textDisabled monospace bg-bgWeaker">
                    [<span class="text-textStrong">${field}</span>
                    <span class="cursor-pointer" data-tippy-content="Remove group by field" @click="${() => this.removeGroupByField(index)}">✕</span>]
                  </div>
                `;
              }
            )}
            <button
              type="button"
              class="text-xs text-textDisabled monospace bg-bgWeaker rounded hover:bg-fillHover cursor-pointer"
              popovertarget="group-by-popover"
              style="anchor-name: --group-by-anchor"
              data-tippy-content="Add a group by field"
            >
              [+]
            </button>

            <!-- GROUP BY Dropdown (Now styled like AGG dropdown) -->
            <div
              popover
              id="group-by-popover"
              class="dropdown menu p-2 shadow-md bg-bgRaised rounded-box w-[500px] z-50 border border-strokeWeak"
              style="position: absolute; position-anchor: --group-by-anchor"
            >
              <!-- Compact Binning Options at the Very Top -->
              <div class="flex items-center mb-3 p-2 border rounded bg-bgWeaker whitespace-nowrap">
                <label class="flex items-center cursor-pointer mr-4">
                  <input 
                    type="radio" 
                    name="bin-type" 
                    class="radio radio-sm mr-2" 
                    ?checked="${!this.enableBinning}"
                    @change="${() => {
                      this.enableBinning = false;
                      this.requestUpdate();
                    }}"
                  />
                  <span class="label-text">no bin</span>
                </label>
                
                <label class="flex items-center cursor-pointer mr-4">
                  <input 
                    type="radio" 
                    name="bin-type" 
                    class="radio radio-sm mr-2" 
                    ?checked="${this.enableBinning && !this.enableAutoBin}"
                    @change="${() => {
                      this.enableBinning = true;
                      this.enableAutoBin = false;
                      this.requestUpdate();
                    }}"
                  />
                  <span class="label-text inline-flex items-center">bin(_,
                    <select
                      class="select select-xs bg-bgWeaker border-none p-0 mx-1 focus:outline-none"
                      @change="${(e: Event) => {
                        this.binValue = parseInt((e.target as HTMLSelectElement).value) || 5;
                      }}"
                    >
                      <option value="5">5</option>
                      <option value="10">10</option>
                      <option value="20">20</option>
                      <option value="30">30</option>
                      <option value="60">1h</option>
                      <option value="120">2h</option>
                      <option value="360">6h</option>
                      <option value="720">12h</option>
                      <option value="1440">1d</option>
                    </select>)</span>
                </label>
                
                <label class="flex items-center cursor-pointer">
                  <input 
                    type="radio" 
                    name="bin-type" 
                    class="radio radio-sm mr-2" 
                    ?checked="${this.enableBinning && this.enableAutoBin}"
                    @change="${() => {
                      this.enableBinning = true;
                      this.enableAutoBin = true;
                      this.requestUpdate();
                    }}"
                  />
                  <span class="label-text">bin_auto(_)</span>
                </label>
              </div>
              
              <div class="mb-3">
                <input
                  type="text"
                  class="input input-bordered input-md w-full px-3 py-2 focus:outline-none"
                  placeholder="Search fields..."
                  .value="${this.groupBySearchTerm}"
                  @input="${(e: Event) => this.filterGroupByOptions(e)}"
                  autofocus
                />
              </div>
              
              <!-- Fields List -->
              <div class="border rounded">
                <div class="p-1 bg-bgWeaker font-medium border-b monospace">Fields</div>
                <div class="max-h-60 overflow-y-auto">
                  ${this.fieldsOptions.length > 0 ? 
                    ((this.groupBySearchTerm && this.filteredGroupByFields.length > 0) ? this.filteredGroupByFields : this.fieldsOptions).map((field) => html`
                      <div 
                        class="p-2 hover:bg-fillHover cursor-pointer monospace ${this.newGroupByField === field.value ? 'bg-fillHover font-medium' : ''}"
                        @click="${() => { 
                          this.newGroupByField = field.value; 
                          this.addGroupByField();
                        }}"
                      >
                        ${field.value} 
                        <span class="float-right text-xs text-textDisabled p-1 rounded-sm bg-bgWeaker">
                          ${this.getFieldIcon(field.type, field.value)}
                        </span>
                      </div>
                    `)
                    : html`<div class="p-2 text-center text-textDisabled">No fields available</div>`
                  }
                </div>
              </div>
              
              <!-- Add Group button is now removed as we're using direct field selection -->
            </div>
          </div>
        </div>
        ` : ''}

        <!-- Sort Section (if sort fields exist) -->
        ${this.sortFields.length > 0 ? html`
          <div class="flex items-center ml-4 gap-1">
            <div class="flex flex-wrap gap-1">
              ${this.sortFields.map(
                (sort, index) => html`
                  <div class="text-xs text-textDisabled monospace bg-bgWeaker">
                    [<span class="text-textDisabled">sort:</span> <span class="text-textStrong">${sort.field} 
                    <span class="cursor-pointer hover:bg-fillHover px-1" 
                      data-tippy-content="Toggle sort direction" 
                      @click="${() => {
                        // Toggle direction
                        this.sortFields[index].direction = this.sortFields[index].direction === 'asc' ? 'desc' : 'asc';
                        this.updateQuery();
                        this.requestUpdate();
                      }}">${sort.direction}</span></span>
                    <span class="cursor-pointer" data-tippy-content="Remove sort field" @click="${() => this.removeSortField(index)}">✕</span>]
                  </div>
                `
              )}
            </div>
          </div>
        ` : ''}

        <!-- Limit Section (if take is set) -->
        ${this.limitValue !== null ? html`
          <div class="flex items-center ml-4 gap-1">
            <div class="flex flex-wrap gap-1">
              <div class="text-xs text-textDisabled monospace bg-bgWeaker">
                [<span class="text-textDisabled">limit:</span> <input
                  type="number"
                  class="w-16 bg-transparent border-none focus:outline-none text-textStrong"
                  min="1"
                  max="10000"
                  .value="${this.limitValue.toString()}"
                  @change="${(e: Event) => {
                    const value = parseInt((e.target as HTMLInputElement).value);
                    this.limitValue = isNaN(value) ? null : value;
                    this.updateLimit();
                  }}"
                />
                <span class="cursor-pointer" data-tippy-content="Remove limit" @click="${() => {
                  this.limitValue = null; // Remove limit
                  this.updateLimit();
                }}">✕</span>]
              </div>
            </div>
          </div>
        ` : ''}

        <!-- More Options Button - Hide when all options are already set -->
        ${(this.sortFields.length === 0 || this.limitValue === null) ? html`
        <div class="ml-auto">
          <button
            type="button"
            class="text-xs text-textDisabled monospace bg-bgWeaker rounded hover:bg-fillHover cursor-pointer"
            popovertarget="more-settings-popover"
            style="anchor-name: --more-settings-anchor"
            data-tippy-content="Additional options: sorting and limits"
          >
            [more ▾]
          </button>
        </div>
        ` : ''}

          <!-- More Settings Dropdown -->
          <div
            popover
            id="more-settings-popover"
            class="dropdown menu p-2 shadow-md bg-bgRaised rounded-box w-64 z-50 border border-strokeWeak"
            style="position: absolute; position-anchor: --more-settings-anchor"
          >
            <!-- Main Menu Options -->
            <div id="more-main-menu">
              <!-- Sort By Option - only show if no sort fields exist -->
              ${this.sortFields.length === 0 ? html`
                <div 
                  class="p-2 hover:bg-fillHover cursor-pointer monospace flex items-center justify-between"
                  id="sort-by-button"
                >
                  <span>Sort by...</span>
                  <span class="text-xs">▶</span>
                </div>
              ` : ''}

              <!-- Take Option - only show if take is not set -->
              ${this.limitValue === null ? html`
                <div 
                  class="p-2 hover:bg-fillHover cursor-pointer monospace"
                  @click="${() => {
                    // Add take with a reasonable default value
                    this.limitValue = 1000;
                    this.updateLimit();
                    // Close popover
                    const popover = document.getElementById('more-settings-popover');
                    if (popover) {
                      (popover as any).hidePopover?.();
                    }
                  }}"
                >
                  Limit results...
                </div>
              ` : ''}
              
              <!-- Show a message if all options are set -->
              ${this.sortFields.length > 0 && this.limitValue !== null ? html`
                <div class="p-2 text-center text-textDisabled monospace">
                  All options are set
                </div>
              ` : ''}
            </div>
          </div>

          <!-- Sort By Popover (nested inside more-settings-popover) -->
          <div
            popover="manual"
            id="sort-by-popover"
            class="dropdown menu p-2 shadow-md bg-bgRaised rounded-box w-[500px] z-50 border border-strokeWeak"
            style="position: fixed; left: auto; top: auto;"
          >

            <div class="mb-3">
              <input
                type="text"
                class="input input-bordered input-md w-full px-3 py-2 focus:outline-none"
                placeholder="Search fields..."
                .value="${this.aggSearchTerm}"
                @input="${(e: Event) => this.filterAggregationOptions(e)}"
                autofocus
              />
            </div>
            
            <!-- Fields List -->
            <div class="border rounded">
              <div class="p-1 bg-bgWeaker font-medium border-b monospace">Fields</div>
              <div class="max-h-60 overflow-y-auto">
                ${this.fieldsOptions.length > 0 ? 
                  ((this.aggSearchTerm && this.filteredFields.length > 0) ? this.filteredFields : this.fieldsOptions).map((field) => html`
                    <div 
                      class="p-2 hover:bg-fillHover cursor-pointer monospace ${this.newSortField === field.value ? 'bg-fillHover font-medium' : ''}"
                      @click="${(e: Event) => { 
                        e.stopPropagation(); // Stop event propagation
                        
                        this.newSortField = field.value; 
                        // Always use asc as default direction
                        this.newSortDirection = 'asc';
                        
                        // Set field value first, then add sort field, and only close popovers after the operation is complete
                        setTimeout(() => {
                          this.addSortField();
                          // Only close popovers after the sort field has been added
                          setTimeout(() => {
                            const popover = document.getElementById('sort-by-popover');
                            if (popover) {
                              (popover as any).hidePopover?.();
                            }
                            const morePopover = document.getElementById('more-settings-popover');
                            if (morePopover) {
                              (morePopover as any).hidePopover?.();
                            }
                          }, 100);
                        }, 0);
                      }}"
                    >
                      ${field.value} 
                      <span class="float-right text-xs text-textDisabled p-1 rounded-sm bg-bgWeaker">
                        ${this.getFieldIcon(field.type, field.value)}
                      </span>
                    </div>
                  `)
                  : html`<div class="p-2 text-center text-textDisabled">No fields available</div>`
                }
              </div>
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
