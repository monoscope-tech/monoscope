import{s as e,t}from"./lit.CSPR5HIn.js";import{a as n,i as r,r as i,t as a}from"./decorate.B_Y0KyeD.js";import{schemaManager as o}from"./query-editor.QL13TP0R.js";function s(e,t){let n;return((...r)=>{clearTimeout(n),n=setTimeout(()=>e(...r),t)})}var c=class extends t{constructor(...e){super(...e),this.createRenderRoot=()=>this,this.queryEditorSelector=`#filterElement`,this.groupByFields=[],this.aggregations=[],this.sortFields=[],this.limitValue=null,this.showMoreSettings=!1,this.fieldsOptions=[],this.newGroupByField=``,this.newAggFunction=``,this.newAggField=``,this.selectedAggFunction=``,this.aggSearchTerm=``,this.filteredAggFunctions=[],this.filteredFields=[],this.showFieldsColumn=!1,this.visualizationType=``,this.newSortField=``,this.newSortDirection=`asc`,this.groupBySearchTerm=``,this.showGroupByFieldsColumn=!1,this.filteredGroupByFields=[],this.enableBinning=!1,this.enableAutoBin=!1,this.binValue=5,this.aggFunctions=[`count`,`sum`,`avg`,`min`,`max`,`median`,`stdev`,`range`,`p50`,`p75`,`p90`,`p95`,`p99`],this.QUERY_PATTERNS={summarize:/\|\s*summarize\s+([^|]+?)(?:by\s+([^|]+?))?(?:\||$)/i,groupBy:/\bgroup\s+by\s+([^|]+?)(?:\||$)/i,sortBy:/\|\s*(sort|order)\s+by\s+([^|]+?)(?:\||$)/i,take:/\|\s*take\s+(\d+)/i,summarizeClause:/\|\s*summarize\s+[^|]+/i,summarizeClauseWithCapture:/\|\s*summarize\s+[^|]+?(?=\||$)/i,sortByClause:/\|\s*(sort|order)\s+by\s+[^|]+/i,sortByClauseWithCapture:/\|\s*(sort|order)\s+by\s+[^|]+?(?=\||$)/i,takeClause:/\|\s*take\s+\d+/i,takeClauseWithCapture:/\|\s*take\s+\d+(?=\||$)/i,multiplePipes:/\|\s*\|/g,binnedTimestamp:/bin(_auto)?\(timestamp(?:,\s*\d+)?\)/},this.handleComponentClick=e=>{let t=e.target;t.closest(`#add-group-by-btn`)?(e.preventDefault(),e.stopPropagation(),this.addGroupByField()):t.closest(`#add-agg-btn`)?(e.preventDefault(),e.stopPropagation(),this.completeAggregation()):t.closest(`#add-sort-btn`)?(e.preventDefault(),e.stopPropagation(),this.addSortField()):(t.closest(`#more-settings-popover`)||t.closest(`#sort-by-popover`))&&e.stopPropagation()}}async firstUpdated(){await this.initializeFields(),this.extractQueryParts();let e=document.querySelector(this.queryEditorSelector);if(e&&(e.addEventListener(`update-query`,()=>{this.extractQueryParts()}),window.schemaManager)){let e=window.schemaManager.setSchemaData;window.schemaManager.setSchemaData=(...t)=>{let n=e.apply(window.schemaManager,t);return setTimeout(()=>this.refreshFieldSuggestions(),100),n}}setTimeout(()=>{let e=this.querySelector(`#add-group-by-btn`);e&&e.addEventListener(`pointerdown`,()=>{this.addGroupByField()});let t=this.querySelector(`#add-agg-btn`);t&&t.addEventListener(`pointerdown`,()=>{this.addAggregation()});let n=this.querySelector(`#add-sort-btn`);n&&n.addEventListener(`pointerdown`,()=>{this.addSortField()})},500),document.addEventListener(`pointerdown`,e=>{let t=e.target;!t.closest(`[popovertarget]`)&&!t.closest(`[popover]`)&&!t.closest(`#sort-by-button`)&&document.querySelectorAll(`[popover].showing`).forEach(e=>{e.hidePopover?.()})}),setTimeout(()=>{let e=document.getElementById(`more-settings-popover`),t=document.getElementById(`sort-by-button`),n=document.getElementById(`sort-by-popover`);if(e&&t&&n){e.addEventListener(`beforetoggle`,e=>{e.newState===`closed`&&n&&n.matches?.(`:popover-open`)&&n.hidePopover?.()});let r=null,i=()=>{r=t.getBoundingClientRect()},a=()=>{r||i(),r&&(n.style.left=`${r.right+5}px`,n.style.top=`${r.top}px`)},o=s(()=>{r=null},100);window.addEventListener(`scroll`,o,!0),window.addEventListener(`resize`,o),t.addEventListener(`pointerdown`,e=>{e.stopPropagation(),a(),n.matches?.(`:popover-open`)?n.hidePopover?.():n.showPopover?.()}),t.addEventListener(`keydown`,e=>{(e.key===`Enter`||e.key===` `)&&(e.preventDefault(),a(),n.showPopover?.())}),n.addEventListener(`pointerdown`,e=>{e.stopPropagation()}),document.addEventListener(`pointerdown`,e=>{if(n.matches?.(`:popover-open`)){let t=e.target;!t.closest(`#sort-by-popover`)&&!t.closest(`#sort-by-button`)&&!t.closest(`#more-settings-popover`)&&n.hidePopover?.()}})}},500)}async initializeFields(){try{let e=o.getSchemaData(o.getDefaultSchema()),t=this.extractAllFields(e);this.fieldsOptions=t,this.requestUpdate(),setTimeout(()=>this.refreshFieldSuggestions(),100)}catch(e){console.error(`Failed to load schema fields:`,e)}}extractAllFields(e,t=``){let n=[];if(!e||typeof e!=`object`)return n;if(e.fields&&typeof e.fields==`object`){for(let[t,r]of Object.entries(e.fields)){let e=r.fieldType||r.type||`string`;n.push({label:`${t} (${e})`,value:t,type:e})}return n}if(e.properties&&typeof e.properties==`object`)for(let[r,i]of Object.entries(e.properties)){let e=t?`${t}.${r}`:r,a=i.type||`object`;n.push({label:`${e} (${a})`,value:e,type:a}),a===`object`&&i.properties&&(n=n.concat(this.extractAllFields(i,e))),a===`array`&&i.items&&(i.items.type||`any`)===`object`&&i.items.properties&&(n=n.concat(this.extractAllFields(i.items,`${e}[]`)))}return n}async refreshFieldSuggestions(e=``){try{let t=o.getSchemaData(o.getDefaultSchema());if(e){let n=t,r=e.split(`.`);for(let t of r)if(n?.properties?.[t])n=n.properties[t];else{console.warn(`Path ${e} not found in schema`);return}let i=this.extractAllFields(n,e);this.fieldsOptions=[...this.fieldsOptions,...i],this.requestUpdate()}else{let e=this.extractAllFields(t);this.fieldsOptions=e,this.requestUpdate()}}catch(t){console.error(`Failed to refresh schema fields for path ${e}:`,t)}}extractQueryParts(){let e=document.querySelector(this.queryEditorSelector);if(!e?.editor)return;let t=e.editor.getValue(),n=t.match(this.QUERY_PATTERNS.summarize),r=t.match(this.QUERY_PATTERNS.groupBy);if(n&&n[2]){let e=n[2].trim(),t=[],r=``,i=0;for(let n=0;n<e.length;n++){let a=e[n];a===`(`?(i++,r+=a):a===`)`?(i--,r+=a):a===`,`&&i===0?(r.trim()&&t.push(r.trim()),r=``):r+=a}r.trim()&&t.push(r.trim()),this.groupByFields=t.filter(Boolean)}else if(r){let e=r[1].trim(),t=[],n=``,i=0;for(let r=0;r<e.length;r++){let a=e[r];a===`(`?(i++,n+=a):a===`)`?(i--,n+=a):a===`,`&&i===0?(n.trim()&&t.push(n.trim()),n=``):n+=a}n.trim()&&t.push(n.trim()),this.groupByFields=t.filter(Boolean)}else this.groupByFields=[];if(n&&n[1]){let e=n[1].split(/\s+by\s+/i)[0].trim().split(`,`).map(e=>e.trim()).filter(Boolean);this.aggregations=e.map(e=>{let t=e.match(/(\w+)\(([^)]*)\)/);return t?{function:t[1].trim(),field:t[2].trim()}:null}).filter(Boolean)}else this.aggregations=[];let i=t.match(this.QUERY_PATTERNS.sortBy);if(i){let e=i[2].split(`,`).map(e=>e.trim()).filter(Boolean);this.sortFields=e.map(e=>{let[t,n]=e.split(/\s+/);return{field:t.trim(),direction:n?.toLowerCase()===`desc`?`desc`:`asc`}})}else this.sortFields=[];let a=t.match(this.QUERY_PATTERNS.take);this.limitValue=a?parseInt(a[1],10):null}updateQuery(){let e=document.querySelector(this.queryEditorSelector);if(!e?.editor){console.error(`Query editor not found or missing editor instance`);return}let t=e.editor.getValue();if(this.aggregations.length>0){let e=`summarize ${this.aggregations.map(e=>e.function===`count`&&e.field===``?`count()`:`${e.function}(${e.field})`).join(`, `)}${this.groupByFields.length>0?` by ${this.groupByFields.join(`, `)}`:``}`;t.match(this.QUERY_PATTERNS.summarizeClause)?t=t.replace(this.QUERY_PATTERNS.summarizeClauseWithCapture,` | ${e}`):(t=t.trim(),t+=t&&!t.endsWith(`|`)?` | `:` `,t+=e)}else if(this.groupByFields.length>0){let e=`summarize count() by ${this.groupByFields.join(`, `)}`;t.match(this.QUERY_PATTERNS.summarizeClause)?t=t.replace(this.QUERY_PATTERNS.summarizeClauseWithCapture,` | ${e}`):(t=t.trim(),t+=t&&!t.endsWith(`|`)?` | `:` `,t+=e)}else t=t.replace(this.QUERY_PATTERNS.summarizeClauseWithCapture,``);if(this.sortFields.length>0){let e=`sort by ${this.sortFields.map(e=>`${e.field} ${e.direction}`).join(`, `)}`;t.match(this.QUERY_PATTERNS.sortByClause)?t=t.replace(this.QUERY_PATTERNS.sortByClauseWithCapture,` | ${e}`):(t=t.trim(),t+=t&&!t.endsWith(`|`)?` | `:` `,t+=e)}else t=t.replace(this.QUERY_PATTERNS.sortByClauseWithCapture,``);if(this.limitValue!==null){let e=`take ${this.limitValue}`;t.match(this.QUERY_PATTERNS.takeClause)?t=t.replace(this.QUERY_PATTERNS.takeClauseWithCapture,` | ${e}`):(t=t.trim(),t+=t&&!t.endsWith(`|`)?` | `:` `,t+=e)}else t=t.replace(this.QUERY_PATTERNS.takeClauseWithCapture,``);t=t.replace(this.QUERY_PATTERNS.multiplePipes,`|`).trim(),e.handleAddQuery(t,!0),e.dispatchEvent(new CustomEvent(`update-query`))}addGroupByField(){if(this.newGroupByField?.trim())try{let e=this.newGroupByField;if(this.enableBinning&&(e=this.enableAutoBin?`bin_auto(${e})`:`bin(${e}, ${this.binValue})`),this.groupByFields.some(t=>t===e?!0:this.enableBinning?e.replace(/bin(_auto)?\([^,]+(?:, \d+)?\)/,``).trim()===t.replace(/bin(_auto)?\([^,]+(?:, \d+)?\)/,``).trim():!1)){console.warn(`Cannot add duplicate group by field:`,e);return}this.groupByFields=[...this.groupByFields,e],this.newGroupByField=``,this.enableBinning=!1,this.enableAutoBin=!1,setTimeout(()=>{let e=document.querySelector(`input[name="bin-type"]:first-of-type`);e&&(e.checked=!0)},50),this.requestUpdate();let t=document.getElementById(`group-by-popover`);t&&t.hidePopover?.(),this.updateQuery()}catch(e){console.error(`Error adding group by field:`,e)}else console.warn(`Cannot add empty group by field:`,this.newGroupByField)}removeGroupByField(e){this.groupByFields=this.groupByFields.filter((t,n)=>n!==e),this.updateQuery()}updateBinInQuery(e,t){let n=document.querySelector(this.queryEditorSelector);if(!n?.editor)return;let r=n.editor.getValue(),i=RegExp(`bin(_auto)?\\(\\s*${e}\\s*(?:,\\s*[^)]+)?\\)`,`gi`);r=r.replace(i,`bin(${e}, ${t})`),n.handleAddQuery(r,!0),n.dispatchEvent(new CustomEvent(`update-query`)),setTimeout(()=>this.extractQueryParts(),0)}toggleGroupByField(e){let t=document.querySelector(this.queryEditorSelector);if(!t?.editor)return;let n=window.currentVisualizationType;[`timeseries`,`timeseries_line`].includes(n)||window.handleVisualizationUpdate(`timeseries`);let r=e=>e.replace(/bin(_auto)?\([^,]+(?:, \d+)?\)/,``).trim(),i=r(e),a=this.groupByFields.findIndex(e=>r(e)===i);if(a>=0){if(this.groupByFields.splice(a,1),this.groupByFields.length===0&&this.aggregations.length===0){let e=t.editor.getValue().replace(this.QUERY_PATTERNS.summarizeClauseWithCapture,``);t.handleAddQuery(e,!0),setTimeout(()=>this.extractQueryParts(),0);return}}else this.groupByFields.some(e=>this.QUERY_PATTERNS.binnedTimestamp.test(e))?this.groupByFields.push(e):this.groupByFields=[`bin_auto(timestamp)`,e],this.aggregations.length===0&&(this.aggregations=[{function:`count`,field:``}]);this.updateQuery(),setTimeout(()=>this.extractQueryParts(),0)}handleAggFunctionClick(e){this.newAggFunction!==e&&(this.newAggField=``),this.newAggFunction=e,e===`count`?this.completeAggregation():(this.showFieldsColumn=!0,this.aggSearchTerm=``,this.filteredFields=[],this.fieldsOptions.length===0&&this.initializeFields().then(()=>{this.requestUpdate()}),this.requestUpdate())}resetAggregationState(){this.newAggFunction=`count`,this.newAggField=``,this.selectedAggFunction=``,this.aggSearchTerm=``,this.filteredAggFunctions=[],this.filteredFields=[],this.showFieldsColumn=!1,this.requestUpdate()}filterAggregationOptions(e){let t=e.target.value.toLowerCase();this.aggSearchTerm=t,this.showFieldsColumn&&(this.fieldsOptions.length===0?this.initializeFields().then(()=>{this.filterFieldsBySearchTerm(t),this.requestUpdate()}):this.filterFieldsBySearchTerm(t)),this.requestUpdate()}filterGroupByOptions(e){let t=e.target.value.toLowerCase();this.groupBySearchTerm=t,this.fieldsOptions.length===0?this.initializeFields().then(()=>{this.filterGroupByFieldsBySearchTerm(t),this.requestUpdate()}):this.filterGroupByFieldsBySearchTerm(t),this.requestUpdate()}filterGroupByFieldsBySearchTerm(e){this.filteredGroupByFields=this.fieldsOptions.filter(t=>t.value.toLowerCase().includes(e)||t.label.toLowerCase().includes(e))}filterFieldsBySearchTerm(e){this.filteredFields=this.fieldsOptions.filter(t=>t.value.toLowerCase().includes(e)||t.label.toLowerCase().includes(e))}getFieldIcon(e,t){return t.includes(`duration`)||t.includes(`time`)?`⏱`:t.includes(`status`)||t.includes(`code`)||t.includes(`error`)||e===`number`||e===`integer`?`N`:``}completeAggregation(){if(this.newAggFunction===`count`)this.aggregations=[...this.aggregations,{function:this.newAggFunction,field:``}];else if(this.newAggFunction&&this.newAggField?.trim())this.aggregations=[...this.aggregations,{function:this.newAggFunction,field:this.newAggField}];else return;this.resetAggregationState();let e=document.getElementById(`agg-popover`);e&&e.hidePopover?.(),this.updateQuery()}addAggregation(){if(this.newAggFunction&&this.newAggField?.trim()){this.aggregations=[...this.aggregations,{function:this.newAggFunction,field:this.newAggField}],this.newAggFunction=`count`,this.newAggField=``,this.selectedAggFunction=``,this.requestUpdate();let e=document.getElementById(`agg-popover`);e&&e.hidePopover?.(),this.updateQuery()}}removeAggregation(e){this.aggregations=this.aggregations.filter((t,n)=>n!==e),this.updateQuery()}addSortField(){if(this.newSortField?.trim()){let e=this.sortFields.findIndex(e=>e.field===this.newSortField);e>=0?this.sortFields[e].direction=this.newSortDirection:this.sortFields=[...this.sortFields,{field:this.newSortField,direction:this.newSortDirection}],this.newSortField=``,this.requestUpdate(),this.updateQuery()}}removeSortField(e){this.sortFields=this.sortFields.filter((t,n)=>n!==e),this.updateQuery()}updateLimit(){this.updateQuery()}connectedCallback(){super.connectedCallback(),this.addEventListener(`pointerdown`,this.handleComponentClick)}disconnectedCallback(){this.removeEventListener(`pointerdown`,this.handleComponentClick),super.disconnectedCallback()}render(){return e`
      <div class="flex flex-wrap items-center gap-2 text-sm max-md:gap-1">
        <!-- AGG Section -->
        <div class="flex items-center gap-1 max-md:hidden">
          <span class="text-xs text-textDisabled monospace" data-tippy-content="Apply aggregation functions like count, sum, avg, etc.">agg:</span>
          <div class="flex flex-wrap gap-1">
            ${this.aggregations.map((t,n)=>e`
                <div class="text-xs text-textDisabled monospace bg-fillWeaker">
                  [<span class="text-textStrong">${t.function}(${t.field})</span>
                  <span class="cursor-pointer" data-tippy-content="Remove aggregation" @pointerdown="${()=>this.removeAggregation(n)}"
                    >✕</span
                  >]
                </div>
              `)}
            <button
              type="button"
              class="text-xs text-textDisabled monospace bg-fillWeaker rounded hover:bg-fillHover cursor-pointer"
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
                  @input="${e=>this.visualizationType=e.target.value}"
                  autofocus
                />
              </div>

              <!-- Two-column layout -->
              <div class="flex mb-3 border rounded">
                <!-- Aggregations column -->
                <div class="w-1/2 border-r">
                  <div class="p-1 bg-fillWeaker font-medium border-b monospace">Aggregation</div>
                  <div class="max-h-60 overflow-y-auto">
                    ${this.aggFunctions.map(t=>e`
                        <div
                          class="p-2 hover:bg-fillHover cursor-pointer monospace ${this.newAggFunction===t?`bg-fillHover font-medium`:``}"
                          @pointerdown="${()=>this.handleAggFunctionClick(t)}"
                        >
                          ${this.newAggFunction===t?`✓ `:``}${t}${t===`count`?`(*)`:`(...)`}
                        </div>
                      `)}
                  </div>
                </div>

                <!-- Fields column -->
                <div class="w-1/2">
                  <div class="p-1 bg-fillWeaker font-medium border-b monospace">Fields</div>
                  <div class="max-h-60 overflow-y-auto">
                    ${this.showFieldsColumn&&this.fieldsOptions.length>0?(this.filteredFields.length>0?this.filteredFields:this.fieldsOptions).map(t=>e`
                              <div
                                class="p-2 hover:bg-fillHover cursor-pointer monospace ${this.newAggField===t.value?`bg-fillHover font-medium`:``}"
                                @pointerdown="${()=>{this.newAggField=t.value,this.completeAggregation()}}"
                              >
                                ${t.value}
                                <span class="float-right text-xs text-textDisabled p-1 rounded-sm bg-fillWeaker">
                                  ${this.getFieldIcon(t.type,t.value)}
                                </span>
                              </div>
                            `):e`<div class="p-2 text-center text-textDisabled">Select an aggregation first</div>`}
                  </div>
                </div>
              </div>

              <!-- No action buttons -->
            </div>
          </div>
        </div>

        <!-- GROUP BY Section -->
        ${this.aggregations.length>0?e`
                <div class="flex items-center ml-4 gap-1 max-md:hidden">
                  <span class="text-xs text-textDisabled monospace" data-tippy-content="Group results by field value">by:</span>
                  <div class="flex flex-wrap gap-1">
                    ${this.groupByFields.map((t,n)=>(t.includes(`bin(`)||t.includes(`bin_auto(`),e`
                        <div class="text-xs text-textDisabled monospace bg-fillWeaker">
                          [<span class="text-textStrong">${t}</span>
                          <span
                            class="cursor-pointer"
                            data-tippy-content="Remove group by field"
                            @pointerdown="${()=>this.removeGroupByField(n)}"
                            >✕</span
                          >]
                        </div>
                      `))}
                    <button
                      type="button"
                      class="text-xs text-textDisabled monospace bg-fillWeaker rounded hover:bg-fillHover cursor-pointer"
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
                      <div class="flex items-center mb-3 p-2 border rounded bg-fillWeaker whitespace-nowrap">
                        <label class="flex items-center cursor-pointer mr-4">
                          <input
                            type="radio"
                            name="bin-type"
                            class="radio radio-sm mr-2"
                            ?checked="${!this.enableBinning}"
                            @change="${()=>{this.enableBinning=!1,this.requestUpdate()}}"
                          />
                          <span class="label-text">no bin</span>
                        </label>

                        <label class="flex items-center cursor-pointer mr-4">
                          <input
                            type="radio"
                            name="bin-type"
                            class="radio radio-sm mr-2"
                            ?checked="${this.enableBinning&&!this.enableAutoBin}"
                            @change="${()=>{this.enableBinning=!0,this.enableAutoBin=!1,this.requestUpdate()}}"
                          />
                          <span class="label-text inline-flex items-center"
                            >bin(_,
                            <select
                              class="select select-xs bg-fillWeaker border-none p-0 mx-1 focus:outline-none"
                              @change="${e=>{this.binValue=parseInt(e.target.value)||5}}"
                            >
                              <option value="5">5</option>
                              <option value="10">10</option>
                              <option value="20">20</option>
                              <option value="30">30</option>
                              <option value="60">1h</option>
                              <option value="120">2h</option>
                              <option value="360">6h</option>
                              <option value="720">12h</option>
                              <option value="1440">1d</option></select
                            >)</span
                          >
                        </label>

                        <label class="flex items-center cursor-pointer">
                          <input
                            type="radio"
                            name="bin-type"
                            class="radio radio-sm mr-2"
                            ?checked="${this.enableBinning&&this.enableAutoBin}"
                            @change="${()=>{this.enableBinning=!0,this.enableAutoBin=!0,this.requestUpdate()}}"
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
                          @input="${e=>this.filterGroupByOptions(e)}"
                          autofocus
                        />
                      </div>

                      <!-- Fields List -->
                      <div class="border rounded">
                        <div class="p-1 bg-fillWeaker font-medium border-b monospace">Fields</div>
                        <div class="max-h-60 overflow-y-auto">
                          ${this.fieldsOptions.length>0?(this.groupBySearchTerm&&this.filteredGroupByFields.length>0?this.filteredGroupByFields:this.fieldsOptions).map(t=>e`
                                  <div
                                    class="p-2 hover:bg-fillHover cursor-pointer monospace ${this.newGroupByField===t.value?`bg-fillHover font-medium`:``}"
                                    @pointerdown="${()=>{this.newGroupByField=t.value,this.addGroupByField()}}"
                                  >
                                    ${t.value}
                                    <span class="float-right text-xs text-textDisabled p-1 rounded-sm bg-fillWeaker">
                                      ${this.getFieldIcon(t.type,t.value)}
                                    </span>
                                  </div>
                                `):e`<div class="p-2 text-center text-textDisabled">No fields available</div>`}
                        </div>
                      </div>

                      <!-- Add Group button is now removed as we're using direct field selection -->
                    </div>
                  </div>
                </div>
              `:``}

        <!-- Sort Section (if sort fields exist) -->
        ${this.sortFields.length>0?e`
                <div class="flex items-center ml-4 gap-1 max-md:hidden">
                  <div class="flex flex-wrap gap-1">
                    ${this.sortFields.map((t,n)=>e`
                        <div class="text-xs text-textDisabled monospace bg-fillWeaker">
                          [<span class="text-textDisabled">sort:</span>
                          <span class="text-textStrong"
                            >${t.field}
                            <span
                              class="cursor-pointer hover:bg-fillHover px-1"
                              data-tippy-content="Toggle sort direction"
                              @pointerdown="${()=>{this.sortFields[n].direction=this.sortFields[n].direction===`asc`?`desc`:`asc`,this.updateQuery(),this.requestUpdate()}}"
                              >${t.direction}</span
                            ></span
                          >
                          <span
                            class="cursor-pointer"
                            data-tippy-content="Remove sort field"
                            @pointerdown="${()=>this.removeSortField(n)}"
                            >✕</span
                          >]
                        </div>
                      `)}
                  </div>
                </div>
              `:``}

        <!-- Limit Section (if take is set) -->
        ${this.limitValue===null?``:e`
                <div class="flex items-center ml-4 gap-1 max-md:hidden">
                  <div class="flex flex-wrap gap-1">
                    <div class="text-xs text-textDisabled monospace bg-fillWeaker">
                      [<span class="text-textDisabled">limit:</span>
                      <input
                        type="number"
                        class="w-16 bg-transparent border-none focus:outline-none text-textStrong"
                        min="1"
                        max="10000"
                        .value="${this.limitValue.toString()}"
                        @change="${e=>{let t=parseInt(e.target.value);this.limitValue=isNaN(t)?null:t,this.updateLimit()}}"
                      />
                      <span
                        class="cursor-pointer"
                        data-tippy-content="Remove limit"
                        @pointerdown="${()=>{this.limitValue=null,this.updateLimit()}}"
                        >✕</span
                      >]
                    </div>
                  </div>
                </div>
              `}

        <!-- More Options Button - Hide when all options are already set -->
        ${this.sortFields.length===0||this.limitValue===null?e`
                <div class="ml-auto">
                  <button
                    type="button"
                    class="text-xs text-textDisabled monospace bg-fillWeaker rounded hover:bg-fillHover cursor-pointer"
                    popovertarget="more-settings-popover"
                    style="anchor-name: --more-settings-anchor"
                    data-tippy-content="Additional options: sorting and limits"
                  >
                    [more ▾]
                  </button>
                </div>
              `:``}

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
              ${this.sortFields.length===0?e`
                      <div class="p-2 hover:bg-fillHover cursor-pointer monospace flex items-center justify-between" id="sort-by-button">
                        <span>Sort by...</span>
                        <span class="text-xs">▶</span>
                      </div>
                    `:``}

              <!-- Take Option - only show if take is not set -->
              ${this.limitValue===null?e`
                      <div
                        class="p-2 hover:bg-fillHover cursor-pointer monospace"
                        @pointerdown="${()=>{this.limitValue=1e3,this.updateLimit();let e=document.getElementById(`more-settings-popover`);e&&e.hidePopover?.()}}"
                      >
                        Limit results...
                      </div>
                    `:``}

              <!-- Show a message if all options are set -->
              ${this.sortFields.length>0&&this.limitValue!==null?e` <div class="p-2 text-center text-textDisabled monospace">All options are set</div> `:``}
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
                @input="${e=>this.filterAggregationOptions(e)}"
                autofocus
              />
            </div>

            <!-- Fields List -->
            <div class="border rounded">
              <div class="p-1 bg-fillWeaker font-medium border-b monospace">Fields</div>
              <div class="max-h-60 overflow-y-auto">
                ${this.fieldsOptions.length>0?(this.aggSearchTerm&&this.filteredFields.length>0?this.filteredFields:this.fieldsOptions).map(t=>e`
                          <div
                            class="p-2 hover:bg-fillHover cursor-pointer monospace ${this.newSortField===t.value?`bg-fillHover font-medium`:``}"
                            @pointerdown="${e=>{e.stopPropagation(),this.newSortField=t.value,this.newSortDirection=`asc`,setTimeout(()=>{this.addSortField(),setTimeout(()=>{let e=document.getElementById(`sort-by-popover`);e&&e.hidePopover?.();let t=document.getElementById(`more-settings-popover`);t&&t.hidePopover?.()},100)},0)}}"
                          >
                            ${t.value}
                            <span class="float-right text-xs text-textDisabled p-1 rounded-sm bg-fillWeaker">
                              ${this.getFieldIcon(t.type,t.value)}
                            </span>
                          </div>
                        `):e`<div class="p-2 text-center text-textDisabled">No fields available</div>`}
              </div>
            </div>
          </div>
        </div>
      </div>
    `}};a([r({type:String})],c.prototype,`queryEditorSelector`,void 0),a([i()],c.prototype,`groupByFields`,void 0),a([i()],c.prototype,`aggregations`,void 0),a([i()],c.prototype,`sortFields`,void 0),a([i()],c.prototype,`limitValue`,void 0),a([i()],c.prototype,`showMoreSettings`,void 0),a([i()],c.prototype,`fieldsOptions`,void 0),a([i()],c.prototype,`newGroupByField`,void 0),a([i()],c.prototype,`newAggFunction`,void 0),a([i()],c.prototype,`newAggField`,void 0),a([i()],c.prototype,`selectedAggFunction`,void 0),a([i()],c.prototype,`aggSearchTerm`,void 0),a([i()],c.prototype,`filteredAggFunctions`,void 0),a([i()],c.prototype,`filteredFields`,void 0),a([i()],c.prototype,`showFieldsColumn`,void 0),a([i()],c.prototype,`visualizationType`,void 0),a([i()],c.prototype,`newSortField`,void 0),a([i()],c.prototype,`newSortDirection`,void 0),a([i()],c.prototype,`groupBySearchTerm`,void 0),a([i()],c.prototype,`showGroupByFieldsColumn`,void 0),a([i()],c.prototype,`filteredGroupByFields`,void 0),a([i()],c.prototype,`enableBinning`,void 0),a([i()],c.prototype,`enableAutoBin`,void 0),a([i()],c.prototype,`binValue`,void 0),c=a([n(`query-builder`)],c);export{c as QueryBuilderComponent};