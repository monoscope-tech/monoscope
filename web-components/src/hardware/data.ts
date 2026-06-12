// Synthetic telemetry for the hardware-monitor POC.
//
// Generates ~5 min @ 1 Hz of per-joint signals for a 6-axis arm running a
// pick-and-place loop, with a scripted incident: J3 over-temp climb from
// t=140s peaking at t=180s, and a J5 vibration spike at t=210s.
//
// The shape mirrors what real telemetry from monoscope/OTLP would look like
// once we bind to KQL — same names, same dimensions, swap the source.

export type JointId = 1 | 2 | 3 | 4 | 5 | 6;
export type MetricKind = 'temp' | 'current' | 'vibration' | 'torque';

export const JOINT_IDS: JointId[] = [1, 2, 3, 4, 5, 6];
export const METRIC_KINDS: MetricKind[] = ['temp', 'current', 'vibration', 'torque'];

export interface MetricMeta {
  label: string;
  unit: string;
  // thresholds for color mapping
  warn: number;
  crit: number;
  // visible y-axis range for sparklines
  yMin: number;
  yMax: number;
}

export const METRICS: Record<MetricKind, MetricMeta> = {
  temp:      { label: 'Temperature', unit: '°C',  warn: 65, crit: 80, yMin: 30, yMax: 100 },
  current:   { label: 'Current',     unit: 'A',   warn: 4.5, crit: 6, yMin: 0, yMax: 8 },
  vibration: { label: 'Vibration',   unit: 'mm/s', warn: 3.0, crit: 5.0, yMin: 0, yMax: 8 },
  torque:    { label: 'Torque',      unit: 'Nm',  warn: 35, crit: 50, yMin: 0, yMax: 60 },
};

export type Severity = 'ok' | 'warn' | 'crit';

export function severity(metric: MetricKind, v: number): Severity {
  const m = METRICS[metric];
  if (v >= m.crit) return 'crit';
  if (v >= m.warn) return 'warn';
  return 'ok';
}

export interface JointSeries {
  temp: Float32Array;
  current: Float32Array;
  vibration: Float32Array;
  torque: Float32Array;
  // position in radians, drives geometry rotation
  position: Float32Array;
}

export interface TimelineEvent {
  t: number;          // seconds from start
  joint: JointId;
  metric: MetricKind;
  severity: Severity;
  title: string;
  detail: string;
}

export interface Telemetry {
  durationSec: number;
  hz: number;
  samples: number;
  // wall-clock start (only used for display)
  startMs: number;
  joints: Record<JointId, JointSeries>;
  events: TimelineEvent[];
}

// Linear ramp helpers
const lerp = (a: number, b: number, t: number) => a + (b - a) * Math.max(0, Math.min(1, t));
const smooth = (a: number, b: number, t: number) => {
  const x = Math.max(0, Math.min(1, t));
  return a + (b - a) * x * x * (3 - 2 * x);
};

// Tuned per-joint nominal baselines for the NON-position metrics.
// J1 = base yaw, J2/J3 = shoulder/elbow (high load), J4-J6 = wrist (low load).
const NOMINAL = {
  1: { temp: 38, current: 1.2, vibration: 0.6, torque: 12 },
  2: { temp: 42, current: 2.8, vibration: 0.8, torque: 28 },
  3: { temp: 45, current: 3.2, vibration: 0.9, torque: 32 },
  4: { temp: 36, current: 0.9, vibration: 0.5, torque: 6 },
  5: { temp: 35, current: 0.8, vibration: 0.7, torque: 5 },
  6: { temp: 34, current: 0.4, vibration: 0.4, torque: 2 },
} satisfies Record<JointId, { temp: number; current: number; vibration: number; torque: number }>;

// Cycle period in seconds — one pick-and-place pass.
const CYCLE = 12;

// Pick-and-place waypoint program — what an industrial robot actually does.
// Each waypoint is (cycle-seconds, [J1..J6 angles in radians]). The arm
// moves linearly in joint space between adjacent waypoints with an S-curve
// (ease-in-out) so accelerations are smooth instead of bang-bang.
// Dwell waypoints (same angles, time apart) hold the pose while the
// (imaginary) gripper opens/closes.
const WAYPOINTS: { t: number; q: [number, number, number, number, number, number] }[] = [
  { t:  0.0, q: [ 0.0, -0.7,  1.3,  0.0,  0.0,  0.0] }, // home
  { t:  1.5, q: [ 0.9, -0.7,  1.3,  0.0,  0.0,  0.0] }, // base rotate over pick
  { t:  2.8, q: [ 0.9, -0.3,  1.7, -0.5,  0.0,  0.0] }, // descend to pick
  { t:  3.6, q: [ 0.9, -0.3,  1.7, -0.5,  0.0,  0.0] }, // dwell — gripper close
  { t:  4.6, q: [ 0.9, -0.7,  1.3,  0.0,  0.0,  0.0] }, // ascend
  { t:  5.5, q: [ 0.0, -0.9,  1.0,  0.2,  0.6,  0.0] }, // transit waypoint
  { t:  6.5, q: [-0.9, -0.7,  1.3,  0.0,  0.6,  1.2] }, // base rotate over place, wrist rolled
  { t:  7.8, q: [-0.9, -0.3,  1.7, -0.5,  0.6,  1.2] }, // descend to place
  { t:  8.6, q: [-0.9, -0.3,  1.7, -0.5,  0.6,  1.2] }, // dwell — gripper open
  { t:  9.6, q: [-0.9, -0.7,  1.3,  0.0,  0.6,  1.2] }, // ascend
  { t: 11.0, q: [ 0.0, -0.9,  1.0,  0.2,  0.0,  0.0] }, // transit back
  { t: 12.0, q: [ 0.0, -0.7,  1.3,  0.0,  0.0,  0.0] }, // home (closes the cycle)
];

// S-curve interpolation: x ∈ [0,1] → eased ∈ [0,1] with zero velocity at ends.
const sCurve = (x: number) => {
  const t = Math.max(0, Math.min(1, x));
  return t * t * (3 - 2 * t);
};

function jointAnglesAt(tSec: number): [number, number, number, number, number, number] {
  // Modulo into one cycle
  const t = ((tSec % CYCLE) + CYCLE) % CYCLE;
  let i = 0;
  while (i + 1 < WAYPOINTS.length && WAYPOINTS[i + 1].t <= t) i++;
  const a = WAYPOINTS[i];
  const b = WAYPOINTS[Math.min(i + 1, WAYPOINTS.length - 1)];
  const dt = Math.max(1e-6, b.t - a.t);
  const u = sCurve((t - a.t) / dt);
  const out: [number, number, number, number, number, number] = [0, 0, 0, 0, 0, 0];
  for (let k = 0; k < 6; k++) out[k] = a.q[k] + (b.q[k] - a.q[k]) * u;
  return out;
}

// Joint velocity (rad/s) at time tSec — drives current/torque/vibration so
// the metrics react to actual motion, not just elapsed time.
function jointVelocitiesAt(tSec: number): [number, number, number, number, number, number] {
  const h = 0.05;
  const a = jointAnglesAt(tSec - h);
  const b = jointAnglesAt(tSec + h);
  const out: [number, number, number, number, number, number] = [0, 0, 0, 0, 0, 0];
  for (let k = 0; k < 6; k++) out[k] = (b[k] - a[k]) / (2 * h);
  return out;
}

// Deterministic pseudo-noise so the demo replays identically across reloads.
function noise(seed: number) {
  let s = seed >>> 0;
  return () => {
    s = (s * 1664525 + 1013904223) >>> 0;
    return ((s >>> 8) / 0x01000000) - 0.5; // [-0.5, 0.5)
  };
}

export function generateTelemetry(opts?: { durationSec?: number; hz?: number; startMs?: number }): Telemetry {
  const durationSec = opts?.durationSec ?? 300;
  const hz = opts?.hz ?? 1;
  const samples = durationSec * hz;
  const startMs = opts?.startMs ?? (Date.now() - durationSec * 1000);

  const joints = {} as Record<JointId, JointSeries>;
  const rnd = noise(42);

  for (const j of JOINT_IDS) {
    joints[j] = {
      temp: new Float32Array(samples),
      current: new Float32Array(samples),
      vibration: new Float32Array(samples),
      torque: new Float32Array(samples),
      position: new Float32Array(samples),
    };
  }

  for (let i = 0; i < samples; i++) {
    const t = i / hz; // seconds

    // Scripted incident envelopes
    const j3HeatRamp = smooth(0, 1, (t - 140) / 40); // 0→1 between 140s and 180s
    const j3StallEnvelope = t >= 175 && t <= 195 ? smooth(0, 1, (t - 175) / 5) * smooth(1, 0, (t - 190) / 5) : 0;
    const j5VibSpike = Math.exp(-Math.pow((t - 210) / 4, 2));

    // Programmed joint angles + velocities from the waypoint program. During
    // the stall window, freeze J3 in place to simulate a position-tracking
    // error (motor commanded to move, joint won't).
    const targetAngles = jointAnglesAt(t);
    const velocities = jointVelocitiesAt(t);
    if (j3StallEnvelope > 0.05) {
      // Hold J3 at its position from the start of the stall window
      const holdAngle = jointAnglesAt(175)[2];
      targetAngles[2] = holdAngle;
      // Velocity still reads as commanded → torque spikes (motor pushing into a stuck joint)
    }

    for (const j of JOINT_IDS) {
      const n = NOMINAL[j];
      const angVel = Math.abs(velocities[j - 1]);

      joints[j].position[i] = targetAngles[j - 1];

      // Current: proportional to |angular velocity| + load + noise
      let current = n.current + angVel * 0.8 + rnd() * 0.08;
      if (j === 3) current += j3HeatRamp * 2.2 + j3StallEnvelope * 1.8;
      joints[j].current[i] = current;

      // Torque tracks current with load multiplier
      let torque = n.torque + angVel * 7 + rnd() * 0.7;
      if (j === 3) torque += j3HeatRamp * 12 + j3StallEnvelope * 12;
      joints[j].torque[i] = torque;

      // Vibration: low baseline + cycle modulation + noise; J5 has the spike
      let vib = n.vibration + angVel * 0.25 + Math.abs(rnd()) * 0.35;
      if (j === 5) vib += j5VibSpike * 5.5;
      if (j === 3) vib += j3StallEnvelope * 2.5;
      joints[j].vibration[i] = vib;

      // Temp: slow thermal model — integrates current^2 with cooling
      const prev = i > 0 ? joints[j].temp[i - 1] : n.temp;
      const heatIn = current * current * 0.04;
      const cool = (prev - n.temp) * 0.015;
      let temp = prev + heatIn - cool;
      // J3 incident: extra heat injection
      if (j === 3) temp += j3HeatRamp * 0.35;
      joints[j].temp[i] = temp;
    }
  }

  const events: TimelineEvent[] = [
    { t: 152, joint: 3, metric: 'temp',      severity: 'warn', title: 'J3 temperature rising',
      detail: 'Joint 3 (elbow) crossed 65°C while load profile is nominal.' },
    { t: 172, joint: 3, metric: 'temp',      severity: 'crit', title: 'J3 over-temperature',
      detail: 'Joint 3 exceeded 80°C critical threshold. Thermal cutoff imminent.' },
    { t: 180, joint: 3, metric: 'torque',    severity: 'crit', title: 'J3 motion stall',
      detail: 'Position tracking error spiked — joint 3 hitching under load.' },
    { t: 210, joint: 5, metric: 'vibration', severity: 'crit', title: 'J5 vibration spike',
      detail: 'Wrist roll vibration exceeded 5 mm/s — possible bearing damage.' },
  ];

  return { durationSec, hz, samples, startMs, joints, events };
}

// Sample a joint metric at fractional time (linear interp).
export function sampleAt(s: Float32Array, hz: number, t: number): number {
  const idx = t * hz;
  const i0 = Math.max(0, Math.min(s.length - 1, Math.floor(idx)));
  const i1 = Math.min(s.length - 1, i0 + 1);
  const frac = idx - i0;
  return s[i0] * (1 - frac) + s[i1] * frac;
}

// Aggregate severity across joints at time t for a metric — drives header health.
export function aggregateSeverity(tel: Telemetry, metric: MetricKind, t: number): Severity {
  let worst: Severity = 'ok';
  for (const j of JOINT_IDS) {
    const v = sampleAt(tel.joints[j][metric], tel.hz, t);
    const s = severity(metric, v);
    if (s === 'crit') return 'crit';
    if (s === 'warn') worst = 'warn';
  }
  return worst;
}

export const SEVERITY_COLORS: Record<Severity, string> = {
  ok:   '#16a34a', // green-600
  warn: '#f59e0b', // amber-500
  crit: '#dc2626', // red-600
};
