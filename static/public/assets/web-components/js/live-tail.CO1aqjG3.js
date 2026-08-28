import{r as e,s as t,t as n}from"./lit.CSPR5HIn.js";import{a as r,r as i,t as a}from"./decorate.B_Y0KyeD.js";import{t as o}from"./repeat.BgPYKyza.js";import{t as s}from"./live-stream.oFD_XLIq.js";var c=1e3,l=e=>{let t=(e??``).toLowerCase();return t.includes(`error`)||t.includes(`fatal`)?`text-textError`:t.includes(`warn`)?`text-textWarning`:t.includes(`debug`)||t.includes(`trace`)?`text-textWeak`:t.includes(`info`)?`text-textStrong`:`text-textWeak`},u={idle:`Not connected`,connecting:`Connecting…`,live:`Live`,reconnecting:`Reconnecting…`,expired:`Expired`,error:`Error`},d=[[`logs`,`Logs`],[`spans`,`Spans`],[`any`,`Logs & spans`]],f=class extends n{constructor(...e){super(...e),this.streamState=`idle`,this.paused=!1,this.rows=[],this.droppedServer=0,this.droppedClient=0,this.statusMessage=``,this.services=[],this.environments=[],this.service=``,this.environment=``,this.kind=`logs`,this.query=``,this.buffer=[],this.stream=null,this.projectId=``,this.leaseSecs=45,this.stickToBottom=!0,this.onVisibility=()=>{document.hidden||this.streamState!==`live`||this.stream?.renew()},this.restart=()=>{this.teardown(),this.buffer=[],this.rows=[],this.droppedServer=0,this.droppedClient=0;let e=new URL(location.href);[[`service`,this.service],[`env`,this.environment],[`kind`,this.kind===`logs`?``:this.kind]].forEach(([t,n])=>n?e.searchParams.set(t,n):e.searchParams.delete(t)),history.replaceState({},``,e),this.stream=new s({projectId:this.projectId,leaseSecs:this.leaseSecs,body:()=>({service:this.service||null,environment:this.environment||null,kind:this.kind,query:this.query||null}),onRows:e=>this.appendRows(e.map(e=>e.log).filter(Boolean)),onDropped:e=>this.droppedServer=e,onState:(e,t)=>{this.streamState=e,this.statusMessage=t??``}}),this.stream.start()}}createRenderRoot(){return this}connectedCallback(){super.connectedCallback(),this.projectId=this.dataset.projectId??``,this.leaseSecs=Number(this.dataset.leaseSecs??`45`);let e=new URLSearchParams(location.search);this.service=e.get(`service`)??``,this.environment=e.get(`env`)??``;let t=e.get(`kind`)??``;this.kind=d.some(([e])=>e===t)?t:`logs`,this.query=e.get(`query`)??``,this.classList.add(`group/lt`),document.addEventListener(`visibilitychange`,this.onVisibility)}disconnectedCallback(){document.removeEventListener(`visibilitychange`,this.onVisibility),this.teardown(),super.disconnectedCallback()}firstUpdated(){this.restart(),this.loadFacets(),this.dispatchEvent(new CustomEvent(`arm-deferred-components`,{bubbles:!0}))}updated(){if(this.dataset.state=this.streamState,this.dataset.paused=String(this.paused),this.stickToBottom){let e=this.querySelector(`[data-rows]`);e&&(e.scrollTop=e.scrollHeight)}}get running(){return this.streamState===`live`||this.streamState===`connecting`||this.streamState===`reconnecting`}async loadFacets(){try{let e=(await(await fetch(`/p/${this.projectId}/log_explorer/schema`)).json())?.fields??{},t=t=>e[t]?.examples??[];this.services=t(`resource.service.name`),this.environments=t(`resource.deployment.environment.name`)}catch{this.services=[],this.environments=[]}}teardown(){this.stream?.stop(),this.stream=null}appendRows(e){e.length&&(this.buffer=[...this.buffer,...e],this.buffer.length>c&&(this.droppedClient+=this.buffer.length-c,this.buffer.splice(0,this.buffer.length-c)),this.paused||(this.rows=this.buffer))}togglePause(){this.paused=!this.paused,this.paused||(this.rows=this.buffer)}render(){let n=this.droppedServer+this.droppedClient;return t`
      <div class="w-full h-full flex flex-col">
        <div class="flex flex-wrap items-center gap-2 px-4 py-3 border-b border-strokeWeak">
          <select
            class="select select-sm w-48"
            aria-label="Service"
            @change=${e=>{this.service=e.target.value,this.restart()}}
          >
            <option value="" ?selected=${!this.service}>All services</option>
            ${this.services.map(e=>t`<option value=${e} ?selected=${e===this.service}>${e}</option>`)}
          </select>
          <select
            class="select select-sm w-40"
            aria-label="Environment"
            @change=${e=>{this.environment=e.target.value,this.restart()}}
          >
            <option value="" ?selected=${!this.environment}>All environments</option>
            ${this.environments.map(e=>t`<option value=${e} ?selected=${e===this.environment}>${e}</option>`)}
          </select>
          <!-- Kind is a control, not a hidden kind == "log" inside the filter: it decides what
               the page is showing, and a mode the user cannot see is one they cannot undo. -->
          <select
            class="select select-sm w-36"
            aria-label="Signal kind"
            @change=${e=>{this.kind=e.target.value,this.restart()}}
          >
            ${d.map(([e,n])=>t`<option value=${e} ?selected=${e===this.kind}>${n}</option>`)}
          </select>
          <!-- The same editor the Events tab uses, so a filter written in one is valid in the
               other: KQL autocomplete against this project's schema, and server-side
               validation as you type. It emits update-query once the query settles. -->
          <query-editor
            class="flex-1 min-w-64 flex items-center min-h-[38px]"
            project-id=${this.projectId}
            default-value=${this.query}
            @update-query=${e=>{let t=(e.detail?.value??``).trim();t!==this.query&&(this.query=t,this.restart())}}
          ></query-editor>
          <button class="btn btn-sm" ?disabled=${!this.running} @click=${()=>this.togglePause()}>
            <span class="group-data-[paused=true]/lt:hidden">Pause</span>
            <span class="hidden group-data-[paused=true]/lt:inline">Resume</span>
          </button>
          <button
            class="btn btn-sm btn-ghost"
            @click=${()=>{this.buffer=[],this.rows=[],this.droppedServer=0,this.droppedClient=0}}
          >
            Clear
          </button>
        </div>

        <div class="px-4 py-1.5 text-xs border-b border-strokeWeak" role="status" aria-live="polite">
          <span class="inline-flex items-center gap-1.5 text-textWeak">
            <span
              aria-hidden="true"
              class="inline-block w-1.5 h-1.5 rounded-full bg-fillWarning-strong group-data-[state=idle]/lt:bg-fillWeak group-data-[state=live]/lt:bg-fillSuccess-strong group-data-[state=error]/lt:bg-fillError-strong group-data-[state=expired]/lt:bg-fillError-strong"
            ></span>
            <span>${u[this.streamState]}${this.paused&&this.streamState===`live`?` — display paused`:``}</span>
            <span>· ${this.rows.length} shown</span>
            ${n>0?t`<span class="text-textWarning">· ${n.toLocaleString()} dropped — narrow your filter</span>`:e}
          </span>
          ${this.statusMessage?t`<span class="ml-2 text-textError">${this.statusMessage}</span>`:e}
        </div>

        <div
          class="flex-1 overflow-y-auto c-scroll font-mono text-xs"
          data-rows
          @scroll=${e=>{let t=e.target;this.stickToBottom=t.scrollTop+t.clientHeight>=t.scrollHeight-40}}
        >
          ${this.rows.length?o(this.rows,e=>e.id,e=>this.rowTemplate(e)):t`<div class="p-8 text-center text-textWeak font-sans text-sm">
                ${this.streamState===`live`?`Waiting for matching records…`:u[this.streamState]}
              </div>`}
        </div>
      </div>
    `}rowTemplate(n){return t`<div class="flex gap-3 px-4 py-1 border-b border-strokeWeak/40 hover:bg-fillWeaker">
      <span class="text-textWeak shrink-0 tabular-nums">${n.timestamp.slice(11,23)}</span>
      <span class="shrink-0 w-14 uppercase ${l(n.level)}">${n.level??``}</span>
      <span class="shrink-0 w-32 truncate text-textWeak">${n.service??``}</span>
      <!-- A span has no body; its name is the line. Without the fallback a spans tail renders
           a column of blank rows and reads as a broken stream rather than a working one. -->
      <span class="flex-1 whitespace-pre-wrap break-all"
        >${n.body||n.name||``}${n.truncated?t`<span class="text-textWeak"> …truncated</span>`:e}</span
      >
    </div>`}};a([i()],f.prototype,`streamState`,void 0),a([i()],f.prototype,`paused`,void 0),a([i()],f.prototype,`rows`,void 0),a([i()],f.prototype,`droppedServer`,void 0),a([i()],f.prototype,`droppedClient`,void 0),a([i()],f.prototype,`statusMessage`,void 0),a([i()],f.prototype,`services`,void 0),a([i()],f.prototype,`environments`,void 0),f=a([r(`live-tail`)],f);export{f as LiveTail};