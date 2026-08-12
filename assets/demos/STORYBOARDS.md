# Monoscope landing-page video storyboards

## Goal

Use short, evidence-led product films to help an engineer understand Monoscope before asking them to trust it.

The page should leave a visitor with three convictions:

1. **I can get from an alert to the cause quickly.**
2. **My logs, traces, metrics, and code context work as one system.**
3. **I keep control of my telemetry and its storage.**

The emotional arc is **pressure → orientation → evidence → relief**. The product is the spectacle. Motion should reveal relationships in the data, not decorate the page.

## Audience and tone

- Primary viewer: developers, SREs, platform engineers, and engineering leaders evaluating an observability platform.
- Viewing context: a fast landing-page scan, often muted and possibly on a phone.
- Voice: precise, trustworthy, calm, technically credible.
- Delight strategy: the satisfying moment when several signals resolve into one explanation.
- Avoid: fake terminal activity, impossible metrics, generic dashboards, mascots, confetti, neon glows, frantic zooms, and claims the footage does not prove.

## Page narrative

| Order | Section | Visitor question | Video's job |
| --- | --- | --- | --- |
| 1 | Hero | What does Monoscope help me do? | Resolve one production incident end to end. |
| 2 | Unified investigation | Is this more than another log viewer? | Show logs, traces, errors, and code as connected evidence. |
| 3 | AI investigation | Does AI save time or merely summarize? | Show a question becoming a transparent, editable query and useful evidence. |
| 4 | Proactive detection | Will it find issues before customers report them? | Show anomaly → grouped issue → actionable report. |
| 5 | Data ownership | What is structurally different? | Make the OpenTelemetry-to-S3 architecture legible. |
| 6 | Setup | How much work is adoption? | Show first telemetry arriving with a short, believable setup. |

The hero film carries the central promise. Later films deepen belief; they should not repeat the same dashboard tour.

---

## Film 01 — From alert to root cause

**Placement:** Hero  
**Purpose:** Convince  
**Length:** 18–22 seconds  
**Claim:** Follow one failing request from symptom to the exact cause without changing tools.  
**Poster frame:** A trace waterfall with the failed span selected and the exception visible.  
**On-page line:** **From alert to root cause, in one investigation.**  
**Supporting copy:** Follow a request across services, logs, and code while the context stays with you.

### Storyboard

| Time | Picture | Motion and focus | Caption |
| --- | --- | --- | --- |
| 0:00–0:03 | Issues view. A new spike in checkout failures is already grouped into one issue. | Cursor selects `checkout-api: 500s`. Keep timestamp, affected service, and error count readable. | `Checkout failures started 8 minutes ago` |
| 0:03–0:06 | Issue detail opens with the error-rate change and a representative trace. | One restrained push-in toward **View trace**. | `One issue. 29 related errors.` |
| 0:06–0:10 | Trace waterfall. The slow path crosses gateway, checkout, inventory, and payments. One failed span has an error icon as well as red state. | Rows reveal in execution order; the failed payment span becomes selected. | `The failed request is already connected` |
| 0:10–0:14 | Correlated logs appear beside the selected span. The exception is visible without another search. | Highlight one log row, then settle. | `Logs and exceptions, in context` |
| 0:14–0:18 | Code context opens at the implicated line and commit. | A single underline lands on the timeout configuration. No simulated typing. | `Cause: a 500 ms upstream timeout` |
| 0:18–0:22 | Pull back to the complete investigation view. | Hold for comprehension; CTA appears outside the video. | `See the whole request. Fix the right thing.` |

### Production notes

- Use one coherent seeded incident and preserve the same service, trace ID, time range, and error throughout.
- Do not claim automatic root cause unless the product actually makes that determination. The UI can surface evidence; the viewer draws the conclusion.
- The last frame must work as a static screenshot and loop cleanly back to the issue list.

---

## Film 02 — Every signal keeps its context

**Placement:** Unified observability section  
**Purpose:** Educate  
**Length:** 12–15 seconds  
**Claim:** Moving between telemetry signals does not mean starting the investigation again.  
**Poster frame:** Split view with a selected log and its corresponding trace span.  
**On-page line:** **Every signal tells the same story.**  
**Supporting copy:** Logs, traces, metrics, and session evidence stay linked by request, service, and time.

### Storyboard

| Time | Picture | Motion and focus | Caption |
| --- | --- | --- | --- |
| 0:00–0:04 | A latency chart shows a clear deviation. | Drag-select the spike's time window. | `Start with the symptom` |
| 0:04–0:08 | The event stream filters to the same window and service. | Results update in place; one error row is selected. | `Keep the time and service context` |
| 0:08–0:12 | The associated trace opens with the exact span focused. | A connecting line or match-cut links log timestamp to waterfall position. | `Open the request behind the event` |
| 0:12–0:15 | The trace, fields, and log context remain visible together. | Brief hold. | `No copy-paste. No tab archaeology.` |

### Delight moment

Use a precise match-cut: the selected point on the metric becomes the selected log timestamp, which becomes the selected span position. This visual continuity is the payoff.

---

## Film 03 — Ask plainly, inspect precisely

**Placement:** AI search section  
**Purpose:** Convince and educate  
**Length:** 14–17 seconds  
**Claim:** Natural-language investigation produces a transparent query and inspectable results.  
**Poster frame:** Plain-language prompt above the generated query and result distribution.  
**On-page line:** **Ask the question. Keep control of the query.**  
**Supporting copy:** Turn an investigation question into an editable query, then inspect the events behind the answer.

### Storyboard

| Time | Picture | Motion and focus | Caption |
| --- | --- | --- | --- |
| 0:00–0:04 | Search prompt receives: “Show payment errors after the last deploy, grouped by exception.” | Use a fast paste, not theatrical character-by-character typing. | `Ask in the language of the incident` |
| 0:04–0:08 | Monoscope generates a readable query. | Query clauses highlight as their matching plain-language phrase is referenced. | `See exactly what will run` |
| 0:08–0:12 | Results appear grouped by exception; a dominant timeout is visible. | Bars settle once, with values readable. | `Find the pattern, not a summary` |
| 0:12–0:17 | Select the timeout group to inspect its source events and trace. | Results transition into evidence without hiding the query. | `Every answer leads back to evidence` |

### Production notes

- Use a question a real on-call engineer would ask; avoid “analyze my system” prompts.
- Keep generated query text large enough to verify. Transparency is the differentiator.
- If generation takes noticeable time, show the real state: `Building a query from service, deploy, and exception fields…`.

---

## Film 04 — Detect, group, explain

**Placement:** Issues, agents, and reports section  
**Purpose:** Convince  
**Length:** 15–18 seconds  
**Claim:** Monoscope reduces noisy anomalies into an issue an engineer can act on.  
**Poster frame:** A grouped issue with affected services, first-seen time, and representative evidence.  
**On-page line:** **Know what changed before the queue fills up.**  
**Supporting copy:** Detect unusual behavior, group related events, and deliver the evidence to the team that owns it.

### Storyboard

| Time | Picture | Motion and focus | Caption |
| --- | --- | --- | --- |
| 0:00–0:04 | Normal request-rate and error-rate lines; error rate departs its expected range. | The anomaly band appears with a timestamp marker. | `A real change, separated from normal noise` |
| 0:04–0:08 | Several related events collapse into one issue. | Rows group into a single issue using position and labels, not color alone. | `Related failures become one investigation` |
| 0:08–0:13 | Issue summary shows affected services, suspected change, and representative traces. | Reveal the evidence in priority order. | `Scope and evidence are ready` |
| 0:13–0:18 | A concise Slack or email report links back to the live issue. | One notification arrives; do not animate a notification storm. | `Send the right context to the right team` |

---

## Film 05 — Your telemetry, your storage

**Placement:** Architecture / ownership section  
**Purpose:** Educate and differentiate  
**Length:** 12–15 seconds  
**Claim:** OpenTelemetry data flows into storage the customer controls while Monoscope provides the investigation layer.  
**Poster frame:** A simple architecture with customer services, OpenTelemetry, Monoscope, and the customer's S3 bucket.  
**On-page line:** **Observability without surrendering your data.**  
**Supporting copy:** Send OpenTelemetry once. Store years of telemetry in your S3-compatible bucket. Self-host or use Monoscope Cloud.

### Storyboard

| Time | Picture | Motion and focus | Caption |
| --- | --- | --- | --- |
| 0:00–0:04 | Application services emit logs, traces, and metrics through OpenTelemetry. | Signal paths converge at the collector. | `OpenTelemetry-native` |
| 0:04–0:09 | Data flows to a clearly labeled **Your S3 bucket** while Monoscope indexes and queries it. | Keep ownership boundary continuously visible. | `Your bucket. Your retention.` |
| 0:09–0:12 | Toggle between **Monoscope Cloud** and **Self-hosted** deployment paths. | Only the compute boundary changes; storage remains anchored. | `Choose how you run it` |
| 0:12–0:15 | Transition from architecture into a real query result. | One stored event resolves into the product UI. | `Keep control without losing speed` |

### Production notes

- This is the only film that should use a schematic rather than pure product capture.
- Use straight, restrained paths and product typography. Avoid floating 3D cubes and decorative data particles.
- Verify every infrastructure claim against the current deployment model before publishing.

---

## Film 06 — First telemetry to first answer

**Placement:** Final setup / CTA section  
**Purpose:** Remove adoption anxiety  
**Length:** 15–20 seconds  
**Claim:** A team can connect an OpenTelemetry service and immediately inspect real telemetry.  
**Poster frame:** Setup instructions beside a live event arriving in the explorer.  
**On-page line:** **Bring the telemetry you already emit.**  
**Supporting copy:** Connect an OpenTelemetry collector, verify the first event, and start investigating.

### Storyboard

| Time | Picture | Motion and focus | Caption |
| --- | --- | --- | --- |
| 0:00–0:04 | Integration setup with a language or collector selected. | Copy the actual minimal configuration. | `Use your existing OpenTelemetry setup` |
| 0:04–0:09 | Terminal runs the documented verification command. | Show one command and its real successful response. | `Send a test event` |
| 0:09–0:14 | The event arrives in live tail with service and trace context. | New row enters once; focus moves to it. | `See telemetry arrive` |
| 0:14–0:20 | Open the event and its trace; the final CTA sits beside the film. | Hold on useful evidence. | `Start with your own system` |

---

## Shared filming system

### Capture rules

- Record real UI against a stable seeded project. Never composite states the product cannot produce.
- Use a plausible incident vocabulary across all films: `checkout-api`, `payment-api`, `inventory-api`, deployment `checkout@4f28c1d`.
- Keep timestamps, counts, trace IDs, and error messages internally consistent.
- Capture at 2× resolution; deliver desktop in 16:10 and a separately composed mobile crop. Do not rely on center-cropping dense UI.
- Target 24 or 30 fps. Use pointer movement only when it explains causality.
- Remove secrets, customer data, personal names, tokens, internal hosts, and unstable browser chrome.

### Motion language

- Prefer match-cuts, selection states, and focus changes over camera zooms.
- Animate transforms and opacity; avoid layout shifts.
- One emphasized action per shot. Hold every proof point long enough to read.
- Transitions should usually finish within 200–500 ms with a smooth ease-out.
- Avoid bouncing, elastic easing, parallax for its own sake, simulated latency, and perpetual motion.

### Captions and accessibility

- Assume autoplay is muted. Every film must make sense without narration.
- Burn in only short claim captions; provide proper HTML text nearby and a full text transcript.
- Captions must describe the conclusion, not narrate the cursor: `Cause: upstream timeout`, not `Clicking the span`.
- Provide descriptive poster images and accessible play/pause controls.
- Never autoplay audio. Pause off-screen video with `IntersectionObserver`.
- Under `prefers-reduced-motion`, show the poster frame or a user-initiated, non-looping version.
- Error states always need text or an icon in addition to red.

### Delivery and performance

- Provide AV1 or VP9 WebM plus H.264 MP4 fallback.
- Keep hero video short and aggressively compressed; lazy-load every below-the-fold film.
- Reserve aspect ratio before loading to prevent layout shift.
- Use a meaningful poster frame rather than the first frame.
- Each loop needs a deliberate ending and a quiet cut; never snap from a resolved state back to mid-action.

## Page interaction recommendations

- Autoplay only the hero when it is substantially visible, muted, and motion is allowed.
- Below the fold, use click-to-play poster frames or play once on first entry. Six simultaneous loops would create noise and waste bandwidth.
- Pair each film with one claim, one sentence of evidence, and one CTA. Do not add a feature checklist beside every clip.
- Let viewers expand a film for detail, but keep the central proof legible inline.
- Use chapter labels such as `01 — Investigate`, not generic labels like `Powerful features`.

## Recommended production order

1. **Film 01 — From alert to root cause:** establishes the campaign's incident, visual language, and central promise.
2. **Film 02 — Every signal keeps its context:** proves the unified workflow with minimal additional capture.
3. **Film 05 — Your telemetry, your storage:** explains the architectural differentiation.
4. **Film 03 — Ask plainly, inspect precisely:** demonstrates AI with appropriate transparency.
5. **Film 06 — First telemetry to first answer:** lowers setup friction near the CTA.
6. **Film 04 — Detect, group, explain:** publish after its grouping and notification claims are verified in the current product.

If production capacity is limited, ship Films 01, 02, and 05 first. Together they answer **why Monoscope**, **how investigation feels**, and **what makes the architecture different**.

## Pre-publish checklist

- [ ] Every film proves exactly one claim.
- [ ] All shown workflows work in the current product.
- [ ] The seeded incident is internally consistent across films.
- [ ] Text is readable at the intended embed size.
- [ ] The story works muted and without cursor narration.
- [ ] Poster frames communicate value before playback.
- [ ] Mobile has an intentional composition.
- [ ] Reduced-motion behavior is verified.
- [ ] Dark and light footage use semantic colors consistently.
- [ ] Videos stop when off-screen and do not compete for attention.
- [ ] Each section has a next action tied to the demonstrated value.
