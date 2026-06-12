// 2D SVG fallback view of the arm. Same joint IDs (1..6), same setters, so the
// host component can swap views without changing state. Side-view projection;
// joints visualised as labelled circles in a chain, segments as connecting
// trapezoids that rotate with their parent joint.

import type { JointId, Severity } from './data';
import { SEVERITY_COLORS } from './data';

const SVG_NS = 'http://www.w3.org/2000/svg';

// Segment lengths in SVG units (px). Tuned so a fully-extended chain fits
// the 600×600 viewBox with room above the floor line at any pose.
const SEG_LEN: Record<JointId, number> = { 1: 30, 2: 130, 3: 115, 4: 55, 5: 30, 6: 28 };
const JOINT_R: Record<JointId, number> = { 1: 18, 2: 14, 3: 13, 4: 11, 5: 9, 6: 8 };

interface Handle {
  groupEl: SVGGElement;        // rotates around the joint center
  circleEl: SVGCircleElement;  // halo (color = severity)
  innerEl: SVGCircleElement;   // inner solid joint
  labelEl: SVGTextElement;
  valueEl: SVGTextElement;
}

export interface SchematicAPI {
  setJointAngles(angles: Record<JointId, number>): void;
  setJointStates(states: Record<JointId, Severity>): void;
  setJointValues(values: Record<JointId, string>): void;
  onJointClick(cb: (id: JointId) => void): void;
  setHighlight(id: JointId | null): void;
  dispose(): void;
}

export function buildSchematic(container: HTMLElement): SchematicAPI {
  container.innerHTML = '';
  const svg = document.createElementNS(SVG_NS, 'svg');
  // Fixed viewBox — sized so the arm fits at any pose without scaling.
  // (Auto-fit made the whole arm bob around as joints rotated; users found
  // it more disorienting than helpful.)
  svg.setAttribute('viewBox', '0 0 600 600');
  svg.setAttribute('preserveAspectRatio', 'xMidYMid meet');
  svg.style.cssText = 'width:100%;height:100%;display:block;';
  container.appendChild(svg);

  // Background grid
  const defs = document.createElementNS(SVG_NS, 'defs');
  defs.innerHTML = `
    <pattern id="hw-grid" width="30" height="30" patternUnits="userSpaceOnUse">
      <path d="M 30 0 L 0 0 0 30" fill="none" stroke="rgba(120,120,140,0.18)" stroke-width="1"/>
    </pattern>
  `;
  svg.appendChild(defs);
  const bg = document.createElementNS(SVG_NS, 'rect');
  bg.setAttribute('width', '600'); bg.setAttribute('height', '600');
  bg.setAttribute('fill', 'url(#hw-grid)');
  svg.appendChild(bg);

  // Floor line
  const floor = document.createElementNS(SVG_NS, 'line');
  floor.setAttribute('x1', '40'); floor.setAttribute('y1', '550');
  floor.setAttribute('x2', '560'); floor.setAttribute('y2', '550');
  floor.setAttribute('stroke', 'rgba(80,80,100,0.6)'); floor.setAttribute('stroke-width', '2');
  svg.appendChild(floor);

  // Base plate
  const baseRect = document.createElementNS(SVG_NS, 'rect');
  baseRect.setAttribute('x', '260'); baseRect.setAttribute('y', '530');
  baseRect.setAttribute('width', '80'); baseRect.setAttribute('height', '20');
  baseRect.setAttribute('fill', '#4a5159');
  baseRect.setAttribute('rx', '4');
  svg.appendChild(baseRect);

  // Build the joint chain. Each joint group is nested inside its parent.
  // Translation moves to "next joint position" (along segment); rotation pivots there.
  // Base is at (300, 530). Y grows downward in SVG, but we want the arm to extend "up"
  // (toward smaller Y). So we'll negate the Y axis: each segment moves by (0, -SEG_LEN[j]).
  const joints = {} as Record<JointId, Handle>;
  let clickCb: ((id: JointId) => void) | null = null;
  let current: SVGGElement = (() => {
    const g = document.createElementNS(SVG_NS, 'g');
    g.setAttribute('transform', 'translate(300, 530)');
    svg.appendChild(g);
    return g;
  })();

  for (const j of [1, 2, 3, 4, 5, 6] as JointId[]) {
    // Rotation group — rotates the entire downstream chain around the joint center
    const rotG = document.createElementNS(SVG_NS, 'g');
    rotG.setAttribute('data-joint', String(j));
    current.appendChild(rotG);

    // Segment (drawn from joint up to next joint position)
    const segLen = SEG_LEN[j];
    const segPath = document.createElementNS(SVG_NS, 'path');
    const w0 = JOINT_R[j] * 0.85, w1 = (j < 6 ? JOINT_R[(j + 1) as JointId] : 6) * 0.85;
    segPath.setAttribute(
      'd',
      `M ${-w0} 0 L ${w0} 0 L ${w1} ${-segLen} L ${-w1} ${-segLen} Z`
    );
    segPath.setAttribute('fill', '#d8dce0');
    segPath.setAttribute('stroke', 'rgba(40,40,50,0.3)');
    segPath.setAttribute('stroke-width', '1');
    rotG.appendChild(segPath);

    // Halo (severity color)
    const halo = document.createElementNS(SVG_NS, 'circle');
    halo.setAttribute('cx', '0'); halo.setAttribute('cy', '0');
    halo.setAttribute('r', String(JOINT_R[j] + 4));
    halo.setAttribute('fill', 'none');
    halo.setAttribute('stroke', SEVERITY_COLORS.ok);
    halo.setAttribute('stroke-width', '3');
    halo.setAttribute('opacity', '0.85');
    rotG.appendChild(halo);

    // Inner joint disc
    const inner = document.createElementNS(SVG_NS, 'circle');
    inner.setAttribute('cx', '0'); inner.setAttribute('cy', '0');
    inner.setAttribute('r', String(JOINT_R[j]));
    inner.setAttribute('fill', '#2a6df5');
    inner.style.cursor = 'pointer';
    inner.setAttribute('data-joint', String(j));
    rotG.appendChild(inner);

    // Label "J1"
    const label = document.createElementNS(SVG_NS, 'text');
    label.setAttribute('x', String(JOINT_R[j] + 14));
    label.setAttribute('y', '4');
    label.setAttribute('font-size', '12');
    label.setAttribute('font-family', 'ui-sans-serif,system-ui,sans-serif');
    label.setAttribute('font-weight', '600');
    label.setAttribute('fill', 'currentColor');
    label.textContent = `J${j}`;
    rotG.appendChild(label);

    // Value
    const value = document.createElementNS(SVG_NS, 'text');
    value.setAttribute('x', String(JOINT_R[j] + 14));
    value.setAttribute('y', '18');
    value.setAttribute('font-size', '10');
    value.setAttribute('font-family', 'ui-sans-serif,system-ui,sans-serif');
    value.setAttribute('fill', 'rgba(100,100,120,0.9)');
    value.textContent = '';
    rotG.appendChild(value);

    joints[j] = { groupEl: rotG, circleEl: halo, innerEl: inner, labelEl: label, valueEl: value };

    // Next joint: translate up by segLen, parent becomes a translation group inside rotG.
    const trans = document.createElementNS(SVG_NS, 'g');
    trans.setAttribute('transform', `translate(0, ${-segLen})`);
    rotG.appendChild(trans);
    current = trans;
  }

  // End effector marker at the final translated position
  const tcp = document.createElementNS(SVG_NS, 'circle');
  tcp.setAttribute('r', '5'); tcp.setAttribute('fill', '#ff5555');
  current.appendChild(tcp);

  svg.addEventListener('click', (ev) => {
    const tgt = ev.target as Element;
    const id = tgt?.getAttribute('data-joint');
    if (id && clickCb) clickCb(Number(id) as JointId);
  });

  return {
    setJointAngles(angles) {
      // Side-view projection: only J2/J3/J4 (pitch axes) actually rotate the
      // chain in the screen plane. J1 is base yaw (around vertical → not
      // visible from the side), J5 is wrist roll (around the link axis), J6
      // is tool yaw — none of these swing the chain in a true side view.
      // Show their angle as a small in-place marker rotation on each joint
      // so the user still sees them moving, without throwing the chain.
      const SIDE_VIEW_JOINTS: JointId[] = [2, 3, 4];
      for (const j of [1, 2, 3, 4, 5, 6] as JointId[]) {
        const a = angles[j];
        if (typeof a !== 'number') continue;
        const deg = (a * 180) / Math.PI;
        if (SIDE_VIEW_JOINTS.includes(j)) {
          joints[j].groupEl.setAttribute('transform', `rotate(${deg})`);
        } else {
          // chain-untouched, but rotate the inner joint disc to hint motion
          joints[j].groupEl.setAttribute('transform', 'rotate(0)');
          joints[j].innerEl.setAttribute('transform', `rotate(${deg})`);
        }
      }
    },
    setJointStates(states) {
      for (const j of [1, 2, 3, 4, 5, 6] as JointId[]) {
        const sev = states[j];
        if (!sev) continue;
        const col = SEVERITY_COLORS[sev];
        joints[j].circleEl.setAttribute('stroke', col);
        joints[j].circleEl.setAttribute('opacity', sev === 'crit' ? '1' : '0.85');
      }
    },
    setJointValues(values) {
      for (const j of [1, 2, 3, 4, 5, 6] as JointId[]) {
        const v = values[j];
        if (v === undefined) continue;
        joints[j].valueEl.textContent = v;
      }
    },
    onJointClick(cb) { clickCb = cb; },
    setHighlight(id) {
      for (const j of [1, 2, 3, 4, 5, 6] as JointId[]) {
        const r = JOINT_R[j] + (j === id ? 7 : 4);
        joints[j].circleEl.setAttribute('r', String(r));
        joints[j].circleEl.setAttribute('stroke-width', j === id ? '4' : '3');
      }
    },
    dispose() {
      container.innerHTML = '';
    },
  };
}
