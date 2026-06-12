// 3D scene for the hardware-monitor. Builds a procedural 6-axis arm out of
// primitives so we have zero asset dependency for the POC. Joints are named
// `joint_1`..`joint_6` and parented in a chain — same shape a GLTF UR5 would
// have, so swapping in a real CAD model later only changes loadModel().

import type { JointId, Severity } from './data';
import { SEVERITY_COLORS } from './data';
import * as THREE from 'three';
import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls.js';

export interface SceneAPI {
  setJointAngles(angles: Record<JointId, number>): void;
  setJointStates(states: Record<JointId, Severity>): void;
  setJointValues(values: Record<JointId, string>): void;
  onJointClick(cb: (id: JointId) => void): void;
  setHighlight(id: JointId | null): void;
  dispose(): void;
}

interface JointHandle {
  pivot: any;       // Object3D — rotates here, drives the children
  halo: any;        // TorusGeometry mesh, color = severity
  haloMat: any;     // MeshBasicMaterial — color mutable
  labelEl: HTMLDivElement;
  rotationAxis: 'x' | 'y' | 'z';
}

// Geometry: revolute axes alternate to mimic a UR-style 6DOF chain
// J1 base yaw (Y), J2 shoulder pitch (Z), J3 elbow pitch (Z),
// J4 wrist pitch (Z), J5 wrist roll (X), J6 tool yaw (Y)
const AXIS: Record<JointId, 'x' | 'y' | 'z'> = { 1: 'y', 2: 'z', 3: 'z', 4: 'z', 5: 'x', 6: 'y' };
const SEG_LEN: Record<JointId, number> = { 1: 0.4, 2: 1.2, 3: 1.0, 4: 0.5, 5: 0.3, 6: 0.25 };
const SEG_RAD: Record<JointId, number> = { 1: 0.18, 2: 0.13, 3: 0.11, 4: 0.09, 5: 0.08, 6: 0.07 };

export async function buildScene(container: HTMLElement): Promise<SceneAPI> {
  // Theme-aware palette — sampled once at scene build. Re-call buildScene
  // to repick if the user toggles theme.
  const isDark = document.documentElement.dataset.theme === 'dark';
  const palette = isDark
    ? {
        sceneBg: 0x0e1116,
        gridMajor: 0x2a3340,
        gridMinor: 0x1a2128,
        gridOpacity: 0.55,
        baseColor: 0x6a737d,
        segColor: 0xb8c0c9,
        jointColor: 0x4d8cff,
        hemiSky: 0xc8d4ff,
        hemiGround: 0x16181f,
        keyIntensity: 1.1,
        labelBg: 'rgba(20,24,28,0.92)',
        labelText: '#f0f4f8',
        labelBorder: 'rgba(255,255,255,0.18)',
      }
    : {
        sceneBg: 0xfafbfc,
        gridMajor: 0x9aa2ad,
        gridMinor: 0xd2d7de,
        gridOpacity: 0.5,
        baseColor: 0x4a5159,
        segColor: 0xd8dce0,
        jointColor: 0x2a6df5,
        hemiSky: 0xffffff,
        hemiGround: 0x303040,
        keyIntensity: 0.9,
        labelBg: 'rgba(18,22,28,0.85)',
        labelText: '#ffffff',
        labelBorder: 'rgba(255,255,255,0.18)',
      };

  const scene = new THREE.Scene();
  scene.background = new THREE.Color(palette.sceneBg);
  scene.fog = new THREE.Fog(palette.sceneBg, 8, 22);

  const camera = new THREE.PerspectiveCamera(45, 1, 0.05, 100);
  camera.position.set(3.5, 2.2, 3.5);

  const renderer = new THREE.WebGLRenderer({ antialias: true, alpha: false });
  renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
  renderer.setSize(container.clientWidth, container.clientHeight);
  renderer.domElement.style.display = 'block';
  container.appendChild(renderer.domElement);

  // Lights — three-point so the metal-grey arm has form
  scene.add(new THREE.HemisphereLight(palette.hemiSky, palette.hemiGround, 0.7));
  const key = new THREE.DirectionalLight(0xffffff, palette.keyIntensity); key.position.set(4, 6, 3); scene.add(key);
  const fill = new THREE.DirectionalLight(0xa0b8ff, 0.4); fill.position.set(-3, 2, -2); scene.add(fill);

  // Floor grid for spatial reference
  const grid = new THREE.GridHelper(8, 16, palette.gridMajor, palette.gridMinor);
  (grid.material as any).opacity = palette.gridOpacity;
  (grid.material as any).transparent = true;
  scene.add(grid);

  // Base plate
  const base = new THREE.Mesh(
    new THREE.CylinderGeometry(0.45, 0.5, 0.12, 32),
    new THREE.MeshStandardMaterial({ color: palette.baseColor, metalness: 0.6, roughness: 0.5 })
  );
  base.position.y = 0.06;
  scene.add(base);

  const segMat = new THREE.MeshStandardMaterial({ color: palette.segColor, metalness: 0.45, roughness: 0.45 });
  const jointMat = new THREE.MeshStandardMaterial({ color: palette.jointColor, metalness: 0.55, roughness: 0.4 });

  const joints = {} as Record<JointId, JointHandle>;
  const pickables: any[] = [];
  let parent: any = scene;
  let yCursor = 0.12; // top of base

  // Label overlay layer — HTML positioned over canvas
  const labelLayer = document.createElement('div');
  labelLayer.style.cssText = 'position:absolute;inset:0;pointer-events:none;overflow:hidden;';
  container.style.position = 'relative';
  container.appendChild(labelLayer);

  for (const j of [1, 2, 3, 4, 5, 6] as JointId[]) {
    // Pivot lives at the start of this segment; children of pivot rotate with it.
    const pivot = new THREE.Object3D();
    pivot.name = `joint_${j}`;
    pivot.position.y = yCursor;
    parent.add(pivot);

    // Halo (severity ring) around the joint
    const haloMat = new THREE.MeshBasicMaterial({ color: SEVERITY_COLORS.ok, transparent: true, opacity: 0.85 });
    const halo = new THREE.Mesh(new THREE.TorusGeometry(SEG_RAD[j] + 0.05, 0.022, 12, 32), haloMat);
    // Orient halo around the rotation axis
    if (AXIS[j] === 'y') halo.rotation.x = Math.PI / 2;
    else if (AXIS[j] === 'x') halo.rotation.y = Math.PI / 2;
    halo.userData.jointId = j;
    pivot.add(halo);
    pickables.push(halo);

    // Joint housing — short cylinder along the rotation axis
    const housingLen = SEG_RAD[j] * 1.6;
    const housing = new THREE.Mesh(
      new THREE.CylinderGeometry(SEG_RAD[j], SEG_RAD[j], housingLen, 24),
      jointMat
    );
    if (AXIS[j] === 'x') housing.rotation.z = Math.PI / 2;
    else if (AXIS[j] === 'z') housing.rotation.x = Math.PI / 2;
    housing.userData.jointId = j;
    pivot.add(housing);
    pickables.push(housing);

    // Segment — connects from this joint to the next, along Y in the pivot frame
    const segLen = SEG_LEN[j];
    const seg = new THREE.Mesh(
      new THREE.CylinderGeometry(SEG_RAD[j] * 0.85, SEG_RAD[j] * 0.7, segLen, 20),
      segMat
    );
    seg.position.y = segLen / 2 + housingLen / 2;
    pivot.add(seg);

    // HTML label for the joint — larger + higher contrast so the demo reads
    // from across the room.
    const labelEl = document.createElement('div');
    labelEl.dataset.jointId = String(j);
    labelEl.style.cssText = [
      'position:absolute', 'transform:translate(-50%,-50%)',
      'font:600 14px/1.15 ui-sans-serif,system-ui,sans-serif',
      'letter-spacing:0.01em',
      `background:${palette.labelBg}`,
      `color:${palette.labelText}`,
      'padding:5px 9px', 'border-radius:8px', 'white-space:nowrap',
      'pointer-events:auto', 'cursor:pointer',
      `border:1px solid ${palette.labelBorder}`,
      'box-shadow:0 2px 8px rgba(0,0,0,0.25)',
      'transition:transform 120ms ease',
      'backdrop-filter:blur(4px)',
    ].join(';');
    labelEl.textContent = `J${j}`;
    labelLayer.appendChild(labelEl);

    joints[j] = { pivot, halo, haloMat, labelEl, rotationAxis: AXIS[j] };

    // Next iteration starts at the top of the just-added segment
    parent = pivot;
    yCursor = segLen + housingLen / 2;
  }

  // End-effector cap (tool tip) — visual anchor for "the working end"
  const tcp = new THREE.Mesh(
    new THREE.ConeGeometry(0.05, 0.12, 16),
    new THREE.MeshStandardMaterial({ color: 0xff5555, metalness: 0.3, roughness: 0.6 })
  );
  tcp.position.y = yCursor + 0.06;
  parent.add(tcp);

  // OrbitControls
  const controls = new OrbitControls(camera, renderer.domElement);
  controls.target.set(0, 1.0, 0);
  controls.enableDamping = true;
  controls.dampingFactor = 0.08;
  controls.minDistance = 1.5;
  controls.maxDistance = 12;
  controls.maxPolarAngle = Math.PI / 2 + 0.1;
  controls.update();

  // Click picking via raycaster
  const raycaster = new THREE.Raycaster();
  const mouseV = new THREE.Vector2();
  let clickCb: ((id: JointId) => void) | null = null;

  function pickFromEvent(ev: MouseEvent): JointId | null {
    const rect = renderer.domElement.getBoundingClientRect();
    mouseV.x = ((ev.clientX - rect.left) / rect.width) * 2 - 1;
    mouseV.y = -((ev.clientY - rect.top) / rect.height) * 2 + 1;
    raycaster.setFromCamera(mouseV, camera);
    const hits = raycaster.intersectObjects(pickables, false);
    for (const h of hits) {
      const id = (h.object.userData && h.object.userData.jointId) as JointId | undefined;
      if (id) return id;
    }
    return null;
  }

  renderer.domElement.addEventListener('click', (ev) => {
    const id = pickFromEvent(ev);
    if (id && clickCb) clickCb(id);
  });

  // Labels also trigger pick on click
  labelLayer.addEventListener('click', (ev) => {
    const tgt = ev.target as HTMLElement;
    const id = tgt?.dataset?.jointId ? (Number(tgt.dataset.jointId) as JointId) : null;
    if (id && clickCb) clickCb(id);
  });

  // Project a world-space Vector3 to a label layer pixel position
  const tmpV = new THREE.Vector3();
  function projectLabel(j: JointId) {
    const handle = joints[j];
    handle.pivot.updateWorldMatrix(true, false);
    handle.pivot.getWorldPosition(tmpV);
    tmpV.project(camera);
    const w = renderer.domElement.clientWidth, h = renderer.domElement.clientHeight;
    const x = (tmpV.x * 0.5 + 0.5) * w;
    const y = (-tmpV.y * 0.5 + 0.5) * h;
    handle.labelEl.style.left = `${x}px`;
    handle.labelEl.style.top = `${y}px`;
    handle.labelEl.style.display = tmpV.z > 1 ? 'none' : 'block';
  }

  // Resize observer
  const ro = new ResizeObserver(() => {
    const w = container.clientWidth, h = container.clientHeight;
    if (!w || !h) return;
    renderer.setSize(w, h);
    camera.aspect = w / h;
    camera.updateProjectionMatrix();
  });
  ro.observe(container);

  let highlighted: JointId | null = null;

  // Render loop
  let running = true;
  function tick() {
    if (!running) return;
    controls?.update();
    renderer.render(scene, camera);
    for (const j of [1, 2, 3, 4, 5, 6] as JointId[]) projectLabel(j);
    requestAnimationFrame(tick);
  }
  requestAnimationFrame(tick);

  return {
    setJointAngles(angles) {
      for (const j of [1, 2, 3, 4, 5, 6] as JointId[]) {
        const a = angles[j];
        if (typeof a !== 'number') continue;
        const h = joints[j];
        h.pivot.rotation[h.rotationAxis] = a;
      }
    },
    setJointStates(states) {
      for (const j of [1, 2, 3, 4, 5, 6] as JointId[]) {
        const sev = states[j];
        if (!sev) continue;
        const col = SEVERITY_COLORS[sev];
        joints[j].haloMat.color.set(col);
        // Pulse opacity slightly on crit so it draws the eye
        joints[j].haloMat.opacity = sev === 'crit' ? 0.95 : sev === 'warn' ? 0.9 : 0.7;
        joints[j].labelEl.style.borderColor = col;
      }
    },
    setJointValues(values) {
      for (const j of [1, 2, 3, 4, 5, 6] as JointId[]) {
        const v = values[j];
        if (v === undefined) continue;
        joints[j].labelEl.textContent = `J${j} · ${v}`;
      }
    },
    onJointClick(cb) { clickCb = cb; },
    setHighlight(id) {
      highlighted = id;
      for (const j of [1, 2, 3, 4, 5, 6] as JointId[]) {
        joints[j].labelEl.style.transform =
          j === highlighted ? 'translate(-50%,-50%) scale(1.15)' : 'translate(-50%,-50%)';
        joints[j].labelEl.style.zIndex = j === highlighted ? '10' : '1';
      }
    },
    dispose() {
      running = false;
      ro.disconnect();
      renderer.dispose();
      renderer.domElement.remove();
      labelLayer.remove();
    },
  };
}
