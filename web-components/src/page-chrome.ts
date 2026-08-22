// Page chrome: tab switching, cookie helpers, tooltips, toasts, HTMX progress and the
// log-explorer field-filter helpers. Previously ~195 lines inlined into every single page
// by BodyWrapper (~8.5KB of uncacheable HTML per request); it is fully static, so it lives
// in the versioned bundle instead.
//
// The functions below are invoked from inline attributes rendered by Lucid
// setCookie/getCookie deliberately stay inline in BodyWrapper: the theme script calls
// getCookie mid-parse, before this deferred module runs.
// (onpointerdown="navigatable(...)", onclick="filterByField(...)"), so they MUST be
// published on window — a module's top-level declarations are not globals.


        function navigatable(me: HTMLElement, target: string, container: string, activeClass: string, tabPrefix?: string) {
            const tabeName = tabPrefix ? tabPrefix + "-tab" : "a-tab";
            const contentName = tabPrefix ? tabPrefix + "-tab-content" : "a-tab-content";
            const nav = document.querySelector(container)!;
            const tabs = nav.querySelectorAll("." + tabeName);
            const contents = nav.querySelectorAll("." + contentName);
            const targetElement = document.querySelector(target)!;
            
            // Batch DOM updates using requestAnimationFrame
            requestAnimationFrame(() => {
              tabs.forEach((tab: Element) => {
                tab.classList.remove(activeClass);
              });
              me.classList.add(activeClass);
              contents.forEach((content: Element) => content.classList.add("hidden"));
              targetElement.classList.remove("hidden");
              targetElement.dispatchEvent(new CustomEvent("tab-visible", { bubbles: true }));
            });
        }



        const onReady = (fn: () => void) => document.readyState === 'loading'
  ? document.addEventListener('DOMContentLoaded', fn)
  : fn();

onReady(function(){
          // Tooltip warmth tracking - skip delay when moving between tooltips
          let tooltipWarmTimeout: ReturnType<typeof setTimeout>;
          let isTooltipWarm = false;

          // Tooltips are daisyUI (`class="tooltip tooltip-<pos>" data-tip="…"`, pure CSS,
          // zero listeners) wherever it renders correctly. `data-tippy-content` is the
          // fallback for the three cases daisyUI structurally cannot serve, because its
          // bubble is a ::before on the element itself:
          //   1. `<input>` and other replaced elements have no ::before.
          //   2. `truncate`/`line-clamp` sets overflow:hidden, which clips the element's
          //      own bubble — and "show me the value that's cut off" is why the tooltip
          //      is there in the first place.
          //   3. Narrow/short scroll containers (facets sidebar, virtual log list) clip
          //      an absolutely-positioned bubble; tippy's popper escapes to document.body.
          // Prefer daisyUI; reach for this only when one of those applies.
          //
          // At most ONE live tippy instance page-wide. Instances are never garbage:
          // each costs ~19 listeners on its reference plus a popper node that outlives
          // the element, so the old keep-one-per-element scheme grew without bound as
          // users swept the log list (500 hovers = +9.5k listeners, +500 popper nodes).
          // Only one tooltip is ever visible, so destroying the previous is free.
          let liveTip: any = null;
          const destroyTip = () => {
            if (!liveTip) return;
            try { liveTip.destroy(); } catch { /* already torn down with its element */ }
            liveTip = null;
          };

          // Event delegation for tooltips - single listener, no querySelectorAll per afterSettle
          document.body.addEventListener('mouseover', function(e: Event) {
            const element = (e.target as Element)?.closest?.('[data-tippy-content]') as (HTMLElement & { _tippy?: unknown }) | null;
            if (!element || element._tippy) return;
            destroyTip();

            const content = element.getAttribute('data-tippy-content') || '';
            const isMultiline = content.length > 80 || content.includes('\n');
            if (typeof (window as any).tippy !== 'function') return;
            const instance = (window as any).tippy(element, {
              delay: [isTooltipWarm ? 0 : 100, 0],
              duration: 0,
              updateDuration: 0,
              animateFill: false,
              moveTransition: '',
              animation: false,
              touch: false,
              followCursor: false,
              flipOnUpdate: false,
              lazy: true,
              maxWidth: isMultiline ? 720 : 350,
              // Preserve newlines / monospace alignment for SQL and other long text.
              onCreate(inst: any) {
                if (isMultiline) {
                  inst.popper.querySelector('.tippy-content')!.style.whiteSpace = 'pre-wrap';
                  inst.popper.querySelector('.tippy-content')!.style.fontFamily = 'ui-monospace, SFMono-Regular, Menlo, monospace';
                  inst.popper.querySelector('.tippy-content')!.style.fontSize = '12px';
                  inst.popper.querySelector('.tippy-content')!.style.textAlign = 'left';
                }
              },
              onShow() {
                isTooltipWarm = true;
                clearTimeout(tooltipWarmTimeout);
              },
              onHide() {
                tooltipWarmTimeout = setTimeout(() => { isTooltipWarm = false; }, 300);
              },
              popperOptions: {
                strategy: 'absolute',
                modifiers: [{
                  name: 'computeStyles',
                  options: { gpuAcceleration: true, adaptive: false },
                }],
              },
            });
            liveTip = instance;
            instance.show();
          });

          // Clear tooltip warmth timeout on page unload and HTMX navigation to prevent memory leak
          window.addEventListener('beforeunload', () => { clearTimeout(tooltipWarmTimeout); destroyTip(); });
          // A swap can remove the tooltip's reference element; drop the instance with it
          // so the detached node and its popper aren't retained until the next hover.
          document.body.addEventListener('htmx:before:swap', () => { clearTimeout(tooltipWarmTimeout); destroyTip(); });

          // Animate stat values on HTMX content swap for delightful updates
          document.body.addEventListener('htmx:after:swap', (e: any) => {
            e.target.querySelectorAll('.stat-value[data-value]').forEach((el: HTMLElement) => {
              const newVal = parseFloat(el.dataset.value!);
              if (!isNaN(newVal) && typeof (window as any).animateStatValue === 'function') {
                (window as any).animateStatValue(el, newVal, 400);
              }
            });
          });

          // Add aria-busy during HTMX requests for screen reader feedback
          document.body.addEventListener('htmx:before:request', (e: any) => {
            e.target.setAttribute('aria-busy', 'true');
          });
          document.body.addEventListener('htmx:after:request', (e: any) => {
            e.target.removeAttribute('aria-busy');
          });

          // Progress bar for user-visible navigation/actions. Initial dashboard
          // widget hydration has its own reserved loading UI and must not churn a
          // shared global indicator eight times in parallel.
          const progressBar = document.getElementById('htmx-progress');
          if (progressBar) {
            let activeRequests = 0;
            const isWidgetHydration = (e: any) =>
              Boolean(e.detail?.elt?.closest?.('[data-widget], [data-chart-widget]'));
            document.body.addEventListener('htmx:before:request', (e: any) => {
              if (isWidgetHydration(e)) return;
              activeRequests += 1;
              if (activeRequests !== 1) return;
              progressBar.classList.remove('htmx-settling');
              progressBar.classList.add('htmx-request');
            });
            document.body.addEventListener('htmx:after:request', (e: any) => {
              if (isWidgetHydration(e)) return;
              activeRequests = Math.max(0, activeRequests - 1);
              if (activeRequests !== 0) return;
              progressBar.classList.remove('htmx-request');
              progressBar.classList.add('htmx-settling');
            });
          }


          // Cmd+Enter / Ctrl+Enter form submission for textareas
          document.addEventListener('keydown', function(e: KeyboardEvent) {
            if ((e.metaKey || e.ctrlKey) && e.key === 'Enter' && (e.target as HTMLElement).tagName === 'TEXTAREA') {
              const form = (e.target as HTMLElement).closest('form');
              if (form) {
                e.preventDefault();
                form.requestSubmit();
              }
            }
          });

          // Notyf and tippy are vendor globals loaded by separate script tags. This module
          // is bundled and can execute before them, and a missing global must not take the
          // rest of the page chrome (tooltips, progress bar, shortcuts) down with it.
          const Notyf = (window as any).Notyf;
          var notyf = Notyf ? new Notyf({
              duration: 5000,
              position: {x: 'right', y: 'top'},
          }) : { success: (_: string) => {}, error: (_: string) => {} };
          const toastAnnouncer = document.getElementById('toast-announcer');
          document.body.addEventListener("successToast", (e: any)=> {
            e.detail.value.map((v: string) => {
              notyf.success(v);
              if (toastAnnouncer) toastAnnouncer.textContent = v;
            });
          });
          document.body.addEventListener("errorToast", (e: any)=> {
            e.detail.value.map((v: string) => {
              notyf.error(v);
              if (toastAnnouncer) toastAnnouncer.textContent = 'Error: ' + v;
            });
          });
        });
        
    function filterByField(event: Event, operation: string) {
        const pathsToRemap = [
          ["request_headers", "attributes.http.request.header"],
          ["response_headers", "attributes.http.response.header"],
          ["response_body", "body.response_body"],
          ["request_body", "body.request_body"],
          ["method", "attributes.http.request.method"],
          ["query_params", "attributes.http.request.query_params"],
          ["path_params", "attributes.http.request.path_params"],
          ["host", "attributes.server.address"],
          ["urlPath", "attributes.http.route"],
          ["raw_url", "attributes.url.full"],
          ["status_code", "attributes.http.response.status_code"],
        ]
        let { fieldPath: path, fieldValue: value } = ((event.target as HTMLElement).closest('[data-field-path]') as HTMLElement).dataset as Record<string, string>;

        pathsToRemap.forEach(([from, to]) => {
          if (path.startsWith(from)) {
            path = path.replace(from, to)
          }
        })

        const operator = operation === 'NotEq' ? '!=' : '==';
        (window as any).queryEditorCall('handleAddQuery', path + ' ' + operator + ' ' + value, operation === 'Replace');
    }

    function viewFieldPatterns(fieldPath: string) {
        const url = new URL(window.location.href);
        url.searchParams.set('viz_type', 'patterns');
        url.searchParams.set('pattern_target', fieldPath);
        url.searchParams.delete('aggregate_skip');
        window.location.href = url.toString();
    }
Object.assign(window, { navigatable, filterByField, viewFieldPatterns });
