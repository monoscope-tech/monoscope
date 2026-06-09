// Binding adapter — the seam between the visual layer and the data source.
//
// Today the hardware-monitor reads from generateTelemetry() (synthetic). The
// next step is to swap that for a real monoscope KQL query: one query per
// metric, one series per joint, sampled at the scrubber's resolution.
//
// This file does NOT yet wire to KQL — it's a contract sketch so the
// next change can drop in an adapter and the component stays unchanged.
//
// Expected real-world shape: KQL produces a result that we coerce into the
// same `Telemetry` shape generateTelemetry() returns. Sketch query (per
// joint, per metric):
//
//   resource.service.name == "ur5-cell-a3"
//   | where attributes.joint_id == 3 and metric.name == "joint.temperature"
//   | summarize avg(value) by bin(timestamp, 1s)
//   | order by timestamp asc
//
// One KQL call per metric, fan out across joints in Promise.all. Events
// (the synthetic anomalies) would come from monoscope alert monitors
// keyed on the same dimensions.

import type { JointId, JointSeries, MetricKind, Telemetry, TimelineEvent } from './data';
import { JOINT_IDS, METRIC_KINDS } from './data';

export interface TelemetryBinding {
  /** Project ID, used to scope queries and alerts. */
  projectId: string;
  /** Machine identifier; in the demo this is the cell+arm slug. */
  machineId: string;
  /** Wall-clock window for the displayed timeline. */
  from: Date;
  to: Date;
  /** Sample rate (Hz) for downsampling KQL output to a Float32Array. */
  hz: number;
}

export interface TelemetrySource {
  load(b: TelemetryBinding): Promise<Telemetry>;
}

/**
 * Stub that returns synthetic telemetry. Replace with a real KQL adapter
 * when the API is ready. Kept as a class so the component can be passed a
 * source instead of calling generateTelemetry() directly.
 */
export class SyntheticSource implements TelemetrySource {
  constructor(private generate: (opts?: { durationSec?: number; hz?: number; startMs?: number }) => Telemetry) {}
  async load(b: TelemetryBinding): Promise<Telemetry> {
    const durationSec = Math.max(1, Math.round((b.to.getTime() - b.from.getTime()) / 1000));
    return this.generate({ durationSec, hz: b.hz, startMs: b.from.getTime() });
  }
}

/**
 * Future shape for the KQL-backed source. Not implemented — kept here so
 * the next change can fill in `runKql` and the component reads identically.
 */
export class KqlSource implements TelemetrySource {
  constructor(private runKql: (query: string, b: TelemetryBinding) => Promise<{ t: number; v: number }[]>) {}

  async load(b: TelemetryBinding): Promise<Telemetry> {
    const samples = b.hz * Math.max(1, Math.round((b.to.getTime() - b.from.getTime()) / 1000));
    const joints = {} as Record<JointId, JointSeries>;

    // Fan out one query per (joint, metric). In practice we'd batch with
    // `summarize ... by joint_id` to get fewer round-trips.
    await Promise.all(
      JOINT_IDS.flatMap((j) =>
        METRIC_KINDS.map(async (m) => {
          if (!joints[j]) {
            joints[j] = {
              temp: new Float32Array(samples),
              current: new Float32Array(samples),
              vibration: new Float32Array(samples),
              torque: new Float32Array(samples),
              position: new Float32Array(samples),
            };
          }
          const series = await this.runKql(this.queryFor(j, m, b), b);
          this.fillSeries(joints[j][m], series, b);
        })
      )
    );

    // Position would come from a `joint.position` metric; events from the
    // alert monitor list. Both omitted in the stub.
    return {
      durationSec: samples / b.hz,
      hz: b.hz,
      samples,
      startMs: b.from.getTime(),
      joints,
      events: [] as TimelineEvent[],
    };
  }

  private queryFor(joint: JointId, metric: MetricKind, b: TelemetryBinding): string {
    const metricName = `joint.${metric === 'temp' ? 'temperature' : metric}`;
    return `
      metric.name == "${metricName}"
      and resource.machine_id == "${b.machineId}"
      and attributes.joint_id == ${joint}
      | summarize avg(value) by bin(timestamp, ${Math.round(1000 / b.hz)}ms)
      | order by timestamp asc
    `.trim();
  }

  private fillSeries(out: Float32Array, samples: { t: number; v: number }[], b: TelemetryBinding) {
    if (samples.length === 0) return;
    const startMs = b.from.getTime();
    for (let i = 0; i < out.length; i++) {
      const tMs = startMs + (i * 1000) / b.hz;
      // Linear search OK at POC scale; switch to binary search for >10k points.
      let k = 0;
      while (k + 1 < samples.length && samples[k + 1].t <= tMs) k++;
      out[i] = samples[k].v;
    }
  }
}
