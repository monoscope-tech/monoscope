// <hardware-monitor> — digital-twin POC for the Hardware page.
//
// Owns: telemetry generation, time cursor, play/pause/speed, view mode (3D|2D),
// active joint + metric, sparkline rendering, timeline scrubber, anomaly list.
// Delegates visuals to scene.ts (three.js procedural arm) and schematic2d.ts
// (SVG fallback). Both expose the same setJointAngles/setJointStates/onJointClick
// API so swapping views is a drop-in replacement.

import { html, LitElement, PropertyValues, TemplateResult } from 'lit';
import { query, state } from 'lit/decorators.js';
import {
  generateTelemetry,
  sampleAt,
  severity,
  METRICS,
  METRIC_KINDS,
  JOINT_IDS,
  SEVERITY_COLORS,
  type JointId,
  type MetricKind,
  type Severity,
  type Telemetry,
  type TimelineEvent,
} from './hardware/data';
// Static imports — dynamic imports create chunks that re-execute the main
// bundle (cache-bust ?v= mismatch) and crash on re-registering custom elements.
import { buildScene, type SceneAPI } from './hardware/scene';
import { buildSchematic, type SchematicAPI } from './hardware/schematic2d';

type ViewMode = '3d' | '2d';
type ViewAPI = SceneAPI | SchematicAPI;

export class HardwareMonitor extends LitElement {
  // Lit shadow DOM hides Tailwind — we render to light DOM so global CSS applies
  // and ECharts (loaded globally) can find canvases by ID.
  protected createRenderRoot() { return this; }

  projectId = '';

  @state() private telemetry: Telemetry | null = null;
  @state() private currentTime = 0;          // seconds from start
  @state() private playing = false;
  @state() private speed: 1 | 2 | 4 | 8 | 16 = 4;
  @state() private view: ViewMode = '3d';
  @state() private activeMetric: MetricKind = 'temp';
  @state() private activeJoint: JointId = 3;
  @state() private headerSeverity: Severity = 'ok';

  @query('#hw-stage') private stage!: HTMLDivElement;
  @query('#hw-timeline') private timelineEl!: HTMLDivElement;
  @query('#hw-spark-host') private sparkHost!: HTMLDivElement;

  private viewApi: ViewAPI | null = null;
  private timelineChart: any = null;
  private sparkCharts: Record<MetricKind, any> = {} as any;
  private rafId = 0;
  private lastTickMs = 0;
  private resizeObs: ResizeObserver | null = null;

  connectedCallback() {
    super.connectedCallback();
    this.projectId = this.dataset.projectId ?? '';
    this.telemetry = generateTelemetry();
    // Start near the start of the incident so the demo lands fast
    this.currentTime = 120;
  }

  disconnectedCallback() {
    super.disconnectedCallback();
    cancelAnimationFrame(this.rafId);
    this.viewApi?.dispose?.();
    this.timelineChart?.dispose?.();
    Object.values(this.sparkCharts).forEach((c: any) => c?.dispose?.());
    this.resizeObs?.disconnect();
  }

  protected firstUpdated(_: PropertyValues): void {
    this.mountView();
    this.mountTimeline();
    this.mountSparklines();
    this.refreshAll();
  }

  protected updated(changed: PropertyValues): void {
    if (changed.has('view')) this.mountView();
    if (changed.has('activeJoint')) {
      this.viewApi?.setHighlight(this.activeJoint);
      this.updateSparklines();
    }
    if (changed.has('activeMetric')) {
      this.refreshAll();
      this.updateTimelineSeries();
    }
  }

  // ─── View (3D / 2D) ─────────────────────────────────────────────────────

  private async mountView() {
    if (!this.stage) return;
    this.viewApi?.dispose?.();
    this.viewApi = null;
    this.stage.innerHTML = '';
    if (this.view === '3d') {
      this.viewApi = await buildScene(this.stage);
    } else {
      this.viewApi = buildSchematic(this.stage);
    }
    this.viewApi.onJointClick((id) => { this.activeJoint = id; });
    this.viewApi.setHighlight(this.activeJoint);
    this.refreshAll();
  }

  // ─── Timeline (ECharts) ─────────────────────────────────────────────────

  private mountTimeline() {
    const ec = (window as any).echarts;
    if (!ec || !this.timelineEl || !this.telemetry) return;
    this.timelineChart = ec.init(this.timelineEl, null, { renderer: 'canvas' });
    this.updateTimelineSeries();

    this.timelineChart.on('click', (p: any) => {
      if (typeof p.value?.[0] === 'number') this.scrubTo(p.value[0]);
    });
    // Click on grid scrubs too
    this.timelineEl.addEventListener('click', (ev) => {
      const rect = this.timelineEl.getBoundingClientRect();
      const xPct = (ev.clientX - rect.left) / rect.width;
      const t = xPct * this.telemetry!.durationSec;
      this.scrubTo(t);
    });
  }

  private updateTimelineSeries() {
    if (!this.timelineChart || !this.telemetry) return;
    const tel = this.telemetry;
    const isDark = document.documentElement.dataset.theme === 'dark';
    const axisColor = isDark ? '#9aa4b0' : '#5b6573';
    const gridColor = isDark ? 'rgba(255,255,255,0.06)' : 'rgba(0,0,0,0.06)';

    // Distinct per-joint palette — picks colors that survive both themes.
    const jointColors = ['#3b82f6', '#10b981', '#f59e0b', '#a855f7', '#ec4899', '#06b6d4'];

    // One line per joint at the active metric, plus event markers
    const series = JOINT_IDS.map((j, i) => ({
      name: `J${j}`,
      type: 'line',
      symbol: 'none',
      smooth: true,
      lineStyle: {
        width: j === this.activeJoint ? 2.6 : 1.4,
        color: jointColors[i],
        opacity: j === this.activeJoint ? 1 : 0.75,
      },
      emphasis: { focus: 'series' },
      data: Array.from(tel.joints[j][this.activeMetric]).map((v, i2) => [i2 / tel.hz, v]),
    }));

    const meta = METRICS[this.activeMetric];
    // Render second-of-cycle as mm:ss for legibility.
    const fmtSec = (s: number) => {
      const mm = Math.floor(s / 60).toString().padStart(2, '0');
      const ss = Math.floor(s % 60).toString().padStart(2, '0');
      return `${mm}:${ss}`;
    };

    this.timelineChart.setOption({
      animation: false,
      grid: { top: 22, left: 56, right: 20, bottom: 38, containLabel: false },
      tooltip: {
        trigger: 'axis', confine: true,
        backgroundColor: isDark ? 'rgba(20,24,30,0.95)' : 'rgba(255,255,255,0.97)',
        borderColor: isDark ? 'rgba(255,255,255,0.1)' : 'rgba(0,0,0,0.08)',
        textStyle: { color: isDark ? '#e6eaf0' : '#1a1f26', fontSize: 12 },
        axisPointer: { type: 'line', lineStyle: { color: axisColor, type: 'dashed' } },
      },
      xAxis: {
        type: 'value', min: 0, max: tel.durationSec,
        axisLabel: { fontSize: 11, color: axisColor, formatter: fmtSec },
        axisLine: { lineStyle: { color: axisColor } },
        splitLine: { show: true, lineStyle: { color: gridColor } },
        splitNumber: 6,
      },
      yAxis: {
        type: 'value', min: meta.yMin, max: meta.yMax, name: meta.unit, nameTextStyle: { color: axisColor, fontSize: 11 },
        axisLabel: { fontSize: 11, color: axisColor },
        axisLine: { lineStyle: { color: axisColor } },
        splitLine: { show: true, lineStyle: { color: gridColor } },
      },
      legend: { show: false },
      series: [
        ...series,
        // Event markers — pin glyph on a dedicated z so they sit on top
        {
          name: 'events', type: 'scatter', symbolSize: 16,
          data: tel.events.map((e: TimelineEvent) => ({
            value: [e.t, METRICS[e.metric].yMax * 0.92],
            itemStyle: { color: SEVERITY_COLORS[e.severity], borderColor: '#fff', borderWidth: 1.5 },
            symbol: 'pin', symbolRotate: 180,
            __event: e,
          })),
          tooltip: { formatter: (p: any) => `<b>${p.data.__event.title}</b><br>${p.data.__event.detail}` },
          z: 5,
        },
        // Current cursor — vertical line via markLine
        {
          name: '__cursor', type: 'line', data: [],
          markLine: {
            silent: true, symbol: ['none', 'circle'], symbolSize: 8,
            lineStyle: { color: '#ef4444', width: 2, type: 'solid' },
            label: {
              show: true, position: 'insideEndTop',
              backgroundColor: '#ef4444', color: '#fff', padding: [2, 5],
              borderRadius: 3, fontSize: 11, fontWeight: 'bold',
              formatter: () => fmtSec(this.currentTime),
            },
            data: [{ xAxis: this.currentTime }],
          },
        },
      ],
    });
  }

  private setCursor() {
    if (!this.timelineChart) return;
    // Mutate the cursor line in-place — cheap
    this.timelineChart.setOption({
      series: [
        ...new Array(JOINT_IDS.length + 1).fill({}),
        { name: '__cursor', markLine: { data: [{ xAxis: this.currentTime }] } },
      ],
    });
  }

  // ─── Sparklines (per-joint mini ECharts) ────────────────────────────────

  private mountSparklines() {
    const ec = (window as any).echarts;
    if (!ec || !this.sparkHost) return;
    for (const m of METRIC_KINDS) {
      const el = this.sparkHost.querySelector(`[data-spark="${m}"]`) as HTMLElement;
      if (el) this.sparkCharts[m] = ec.init(el, null, { renderer: 'canvas' });
    }
    this.updateSparklines();
  }

  private updateSparklines() {
    if (!this.telemetry) return;
    const tel = this.telemetry;
    const j = this.activeJoint;
    for (const m of METRIC_KINDS) {
      const chart = this.sparkCharts[m];
      if (!chart) continue;
      const data = Array.from(tel.joints[j][m]);
      const meta = METRICS[m];
      chart.setOption({
        grid: { top: 6, left: 4, right: 4, bottom: 4 },
        xAxis: { type: 'value', min: 0, max: tel.durationSec, show: false },
        yAxis: { type: 'value', min: meta.yMin, max: meta.yMax, show: false },
        series: [
          {
            type: 'line', symbol: 'none', smooth: true,
            lineStyle: { width: 1.5, color: '#2a6df5' },
            areaStyle: { color: 'rgba(42,109,245,0.12)' },
            data: data.map((v, i) => [i / tel.hz, v]),
          },
          {
            type: 'line', data: [],
            markLine: {
              silent: true, symbol: ['none', 'none'],
              lineStyle: { color: '#dc2626', width: 1.2 },
              label: { show: false },
              data: [{ xAxis: this.currentTime }],
            },
          },
        ],
      });
    }
  }

  // ─── Time + state refresh ───────────────────────────────────────────────

  private refreshAll() {
    if (!this.telemetry || !this.viewApi) return;
    const tel = this.telemetry;

    const angles = {} as Record<JointId, number>;
    const states = {} as Record<JointId, Severity>;
    const values = {} as Record<JointId, string>;
    let worst: Severity = 'ok';

    for (const j of JOINT_IDS) {
      angles[j] = sampleAt(tel.joints[j].position, tel.hz, this.currentTime);
      const v = sampleAt(tel.joints[j][this.activeMetric], tel.hz, this.currentTime);
      const sev = severity(this.activeMetric, v);
      states[j] = sev;
      values[j] = `${v.toFixed(1)}${METRICS[this.activeMetric].unit}`;
      if (sev === 'crit') worst = 'crit';
      else if (sev === 'warn' && worst === 'ok') worst = 'warn';
    }
    this.viewApi.setJointAngles(angles);
    this.viewApi.setJointStates(states);
    this.viewApi.setJointValues(values);
    this.headerSeverity = worst;
    this.setCursor();
    // Update sparkline cursors
    for (const m of METRIC_KINDS) {
      this.sparkCharts[m]?.setOption({
        series: [{}, { markLine: { data: [{ xAxis: this.currentTime }] } }],
      });
    }
  }

  private scrubTo(t: number) {
    if (!this.telemetry) return;
    this.currentTime = Math.max(0, Math.min(this.telemetry.durationSec, t));
    this.refreshAll();
  }

  // ─── Play loop ──────────────────────────────────────────────────────────

  private togglePlay = () => {
    this.playing = !this.playing;
    if (this.playing) {
      // If we're at the end, rewind
      if (this.telemetry && this.currentTime >= this.telemetry.durationSec - 0.5) this.currentTime = 0;
      this.lastTickMs = performance.now();
      this.rafId = requestAnimationFrame(this.tick);
    } else {
      cancelAnimationFrame(this.rafId);
    }
  };

  private tick = (now: number) => {
    if (!this.playing || !this.telemetry) return;
    const dt = (now - this.lastTickMs) / 1000;
    this.lastTickMs = now;
    this.currentTime += dt * this.speed;
    if (this.currentTime >= this.telemetry.durationSec) {
      this.currentTime = this.telemetry.durationSec;
      this.playing = false;
    }
    this.refreshAll();
    if (this.playing) this.rafId = requestAnimationFrame(this.tick);
  };

  private setSpeed = (s: 1 | 2 | 4 | 8 | 16) => { this.speed = s; };

  private jumpToEvent = (e: TimelineEvent) => {
    this.activeJoint = e.joint;
    this.activeMetric = e.metric;
    this.scrubTo(Math.max(0, e.t - 8));
  };

  // ─── Render ─────────────────────────────────────────────────────────────

  protected render(): TemplateResult {
    const tel = this.telemetry;
    if (!tel) return html`<div class="p-8 text-textWeak">Loading telemetry…</div>`;

    const ct = this.currentTime;
    const formatT = (s: number) => {
      const mm = Math.floor(s / 60).toString().padStart(2, '0');
      const ss = Math.floor(s % 60).toString().padStart(2, '0');
      return `${mm}:${ss}`;
    };

    return html`
      <div class="flex flex-col w-full h-full overflow-hidden" data-hardware-root>
        <!-- Header bar -->
        <div class="flex items-center gap-3 px-4 py-2 border-b border-strokeWeak bg-bgRaised">
          <div class="flex items-center gap-2">
            <span class="inline-block w-2.5 h-2.5 rounded-full"
                  style="background:${SEVERITY_COLORS[this.headerSeverity]}"></span>
            <span class="font-semibold">UR5-Demo · Cell A3</span>
            <span class="text-xs text-textWeak">6-axis arm · pick-and-place</span>
          </div>
          <div class="ml-auto flex items-center gap-1 text-xs">
            ${(['3d', '2d'] as const).map(v => html`
              <button @click=${() => { this.view = v; }}
                      class="px-2 py-1 rounded ${this.view === v ? 'bg-fillBrand-weak text-textBrand font-medium' : 'text-textWeak hover:bg-fillWeak'}">
                ${v.toUpperCase()}
              </button>
            `)}
          </div>
        </div>

        <!-- Body: stage + right rail -->
        <div class="flex flex-1 min-h-0">
          <div class="flex-1 relative bg-bgBase" id="hw-stage"></div>

          <aside class="w-80 shrink-0 border-l border-strokeWeak bg-bgRaised flex flex-col overflow-y-auto">
            <!-- Metric switcher -->
            <div class="p-3 border-b border-strokeWeak">
              <div class="text-xs uppercase tracking-wide text-textWeak mb-1.5">Metric layer</div>
              <div class="grid grid-cols-2 gap-1">
                ${METRIC_KINDS.map(m => html`
                  <button @click=${() => { this.activeMetric = m; }}
                          class="px-2 py-1.5 rounded text-xs text-left ${this.activeMetric === m ? 'bg-fillBrand-weak text-textBrand font-medium' : 'hover:bg-fillWeak text-textStrong'}">
                    ${METRICS[m].label}
                  </button>
                `)}
              </div>
            </div>

            <!-- Active joint detail -->
            <div class="p-3 border-b border-strokeWeak">
              <div class="flex items-center justify-between mb-2">
                <div class="text-xs uppercase tracking-wide text-textWeak">Joint detail</div>
                <select @change=${(e: Event) => { this.activeJoint = Number((e.target as HTMLSelectElement).value) as JointId; }}
                        class="text-xs bg-bgBase border border-strokeWeak rounded px-1.5 py-0.5">
                  ${JOINT_IDS.map(j => html`<option value=${j} ?selected=${j === this.activeJoint}>Joint ${j}</option>`)}
                </select>
              </div>
              <div id="hw-spark-host" class="space-y-2">
                ${METRIC_KINDS.map(m => {
                  const v = sampleAt(tel.joints[this.activeJoint][m], tel.hz, ct);
                  const sev = severity(m, v);
                  return html`
                    <div class="rounded border border-strokeWeak p-2">
                      <div class="flex items-baseline justify-between">
                        <span class="text-xs text-textWeak">${METRICS[m].label}</span>
                        <span class="text-sm font-mono font-semibold" style="color:${SEVERITY_COLORS[sev]}">
                          ${v.toFixed(1)}<span class="text-xs text-textWeak ml-0.5">${METRICS[m].unit}</span>
                        </span>
                      </div>
                      <div data-spark="${m}" style="width:100%;height:36px"></div>
                    </div>
                  `;
                })}
              </div>
            </div>

            <!-- Events / anomalies -->
            <div class="p-3 flex-1">
              <div class="text-xs uppercase tracking-wide text-textWeak mb-2">Events</div>
              <div class="space-y-1.5">
                ${tel.events.map(e => html`
                  <button @click=${() => this.jumpToEvent(e)}
                          class="w-full text-left rounded border border-strokeWeak hover:bg-fillWeak p-2">
                    <div class="flex items-center gap-2">
                      <span class="inline-block w-2 h-2 rounded-full" style="background:${SEVERITY_COLORS[e.severity]}"></span>
                      <span class="text-xs font-mono text-textWeak">${formatT(e.t)}</span>
                      <span class="text-xs font-medium">${e.title}</span>
                    </div>
                    <div class="text-xs text-textWeak mt-0.5 pl-4">${e.detail}</div>
                  </button>
                `)}
              </div>
            </div>
          </aside>
        </div>

        <!-- Bottom timeline strip -->
        <div class="border-t border-strokeWeak bg-bgRaised">
          <div class="flex items-center gap-2 px-3 py-1.5 text-xs">
            <button @click=${this.togglePlay}
                    class="w-7 h-7 inline-flex items-center justify-center rounded bg-fillBrand-strong text-white hover:opacity-90">
              ${this.playing
                ? html`<svg width="10" height="10" viewBox="0 0 8 8"><rect x="0" y="0" width="3" height="8" fill="currentColor"/><rect x="5" y="0" width="3" height="8" fill="currentColor"/></svg>`
                : html`<svg width="10" height="10" viewBox="0 0 8 8"><path d="M0 0 L8 4 L0 8 Z" fill="currentColor"/></svg>`}
            </button>
            <span class="font-mono tabular-nums">${formatT(ct)} / ${formatT(tel.durationSec)}</span>
            <div class="ml-2 flex items-center gap-0.5">
              ${([1, 2, 4, 8, 16] as const).map(s => html`
                <button @click=${() => this.setSpeed(s)}
                        class="px-1.5 py-0.5 rounded ${this.speed === s ? 'bg-fillBrand-weak text-textBrand font-medium' : 'text-textWeak hover:bg-fillWeak'}">
                  ${s}×
                </button>
              `)}
            </div>
            <span class="ml-auto text-textWeak">${METRICS[this.activeMetric].label}</span>
          </div>
          <div id="hw-timeline" style="width:100%;height:180px;cursor:crosshair"></div>
        </div>
      </div>
    `;
  }
}

