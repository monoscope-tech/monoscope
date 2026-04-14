import { html, LitElement, nothing, PropertyValues, TemplateResult } from 'lit';
import { customElement, property, query, state } from 'lit/decorators.js';
import { EventType, eventWithTime } from '@rrweb/types';
import { ConsoleEvent } from './types/types';
import { Replayer } from '@rrweb/replay';

export function faSprite_(iconName: string, kind: string, classes: string): TemplateResult {
  return html`<svg class="${classes}"><use href="/public/assets/svgs/fa-sprites/${kind}.svg#${iconName}"></use></svg>`;
}
const MS_10 = 10000;
const MS_30 = 30000;
// Speed presets, shared between keyboard shortcuts and the speed dropdown so
// ↑/↓ step through exactly the same values the user sees.
const SPEED_STEPS = [0.5, 1, 1.5, 2, 3, 4, 8, 16] as const;
@customElement('session-replay')
export class SessionReplay extends LitElement {
  @property({ type: String }) private projectId: string = '';
  @property({ type: String }) private containerId: string = '';
  @property({ type: String }) private initialSession: string = '';

  @state() private activityWidth = 0;
  @query('#replayerOuterContainer') private replayerOuterContainer: HTMLElement;
  @query('#progressBar') private progressBar: HTMLElement;
  @state() private playSpeed = 1;
  @state() private skipInactive = true;
  @state() private consoleEventsEnable = [true, true, true]; // error, warn, info;
  @state() private paused = false;
  @state() private isLoading = false;
  @state() private loadError: string | null = null;

  @state() private consoleEvents: ConsoleEvent[] = [];
  @state() private currentEventTime: number = 0;
  @state() private currentTime = 0;
  @state() private finished = false;
  @state() private syncScrolling = true;
  @state() private trickTarget = 0;
  @state() private currentSessionId: string = '';
  @state() private userEmail: string | null = null;
  @state() private userName: string | null = null;
  @state() private userIdLabel: string | null = null;
  @state() private sessionIdCopied = false;
  @state() private momentLinkCopied = false;
  @state() private showShortcuts = false;
  // Timeline markers: errors/warnings rendered as ticks on the scrubber,
  // page navigations rendered as dashes below. Times are offsets from start.
  @state() private errorTicks: number[] = [];
  @state() private warnTicks: number[] = [];
  @state() private navMarkers: { offset: number; href: string }[] = [];

  private startX: number | null = null;
  private player: Replayer | null = null;
  private events: eventWithTime[] = [];
  private containerWidth = 1024;
  private timeout: any = null;

  private containerHeight = 550;
  private iframeWidth = 1117;
  private trickPlayer: Replayer | null = null;
  private trickPlayerCleanupTimer: number | null = null;
  private observer = new MutationObserver((mutations) => {
    for (const mutation of mutations) {
      if (mutation.type === 'attributes' && (mutation.attributeName === 'width' || mutation.attributeName === 'height')) {
        const iframe = this.player?.iframe;
        if (iframe) {
          this.iframeWidth = Number(iframe.getAttribute('width'));
          this.iframeHeight = Number(iframe.getAttribute('height'));
          this.updateScale();
        }
      }
    }
  });

  private iframeHeight = 927;
  private timer: number | null = null;
  private resizeObserver: ResizeObserver | null = null;
  private consoleTypesCounts = { error: 0, warn: 0, info: 0 };
  private metaData: { startTime: number; endTime: number; totalTime: number } = { startTime: 0, endTime: 0, totalTime: 0 };
  constructor() {
    super();
    // Hydrate console-panel width from prior session so users don't have to re-size it
    const savedWidth = Number(localStorage.getItem('replay-activity-width') ?? '');
    this.activityWidth = Number.isFinite(savedWidth) && savedWidth >= 0 ? Math.min(savedWidth, 600) : 0;
    document.addEventListener('mouseup', () => {
      document.body.style.userSelect = '';
      if (this.startX !== null) {
        // Persist the panel width, not the cursor position — previous behavior stored
        // e.clientX which was meaningless to replay on next mount.
        localStorage.setItem('replay-activity-width', String(Math.max(0, Math.round(this.activityWidth))));
      }
      this.startX = null;
    });

    document.addEventListener('DOMContentLoaded', () => {
      this.makeDraggable();
    });

    this.updateScale = this.updateScale.bind(this);
    this.handleConsoleEvents = this.handleConsoleEvents.bind(this);
    this.loopTimer = this.loopTimer.bind(this);
    this.stopTimer = this.stopTimer.bind(this);
    this.play = this.play.bind(this);
    this.pause = this.pause.bind(this);
    this.goTo = this.goTo.bind(this);
    this.fetchNewSessionData = this.fetchNewSessionData.bind(this);
    this.initiatePlayer = this.initiatePlayer.bind(this);
    this.handleTimeSeek = this.handleTimeSeek.bind(this);
    this.closePlayerWindow = this.closePlayerWindow.bind(this);
    document.addEventListener('mousemove', (e) => {
      if (this.startX !== null) {
        const diff = this.startX - e.clientX;
        // Clamp between fully closed and 600px — prevents panels that eat the player
        // or drag off into negative territory when the user overshoots.
        this.activityWidth = Math.min(600, Math.max(0, this.activityWidth + diff));
        this.startX = e.clientX;
      }
    });

    document.addEventListener('keydown', this.handleKeydown);

    window.addEventListener('loadSessionReplay', (e) => {
      const { sessionId } = (e as CustomEvent<{ sessionId: string }>).detail;
      this.fetchNewSessionData(sessionId);
      updateUrlState('session_replay', sessionId);
    });
  }

  // Keyboard shortcuts — only fire when the player window is visible and the
  // user isn't typing in an input. Mirrors conventions from YouTube/video.js.
  private handleKeydown = (e: KeyboardEvent) => {
    const container = document.querySelector(`#${this.containerId}`);
    if (!container || container.classList.contains('hidden')) return;

    // Escape closes the shortcuts panel first (if open), otherwise the player.
    if (e.key === 'Escape') {
      if (this.showShortcuts) this.showShortcuts = false;
      else this.closePlayerWindow();
      e.preventDefault();
      return;
    }
    // ? toggles the keyboard shortcuts panel — works even while loading so users
    // can discover the hotkeys without needing a session to be ready.
    if (e.key === '?') {
      this.showShortcuts = !this.showShortcuts;
      e.preventDefault();
      return;
    }
    if (this.isLoading || this.loadError) return;

    const t = e.target as HTMLElement | null;
    const tag = t?.tagName;
    if (tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT' || t?.isContentEditable) return;

    // Don't hijack Cmd/Ctrl/Alt combos — those belong to the browser/OS
    if (e.metaKey || e.ctrlKey || e.altKey) return;

    const total = this.metaData.totalTime || 0;
    switch (e.key) {
      case ' ':
      case 'k':
        this.paused = !this.paused;
        e.preventDefault();
        break;
      case 'ArrowLeft':
      case 'j':
        this.goTo(Math.max(0, this.currentTime - MS_10));
        e.preventDefault();
        break;
      case 'ArrowRight':
      case 'l':
        this.goTo(Math.min(total, this.currentTime + MS_10));
        e.preventDefault();
        break;
      // Vertical arrows = larger time jumps than ←/→. ↑ rewinds (earlier,
      // matches "scroll up = older content"), ↓ advances. Gives users two
      // seek magnitudes (10s fine, 30s coarse) without leaving the home row.
      case 'ArrowUp':
        this.goTo(Math.max(0, this.currentTime - MS_30));
        e.preventDefault();
        break;
      case 'ArrowDown':
        this.goTo(Math.min(total, this.currentTime + MS_30));
        e.preventDefault();
        break;
      // < / > match YouTube's speed shortcuts — the dropdown remains the
      // primary speed UI, these are for keyboard-forward power users.
      case '<':
      case ',': {
        const i = (SPEED_STEPS as readonly number[]).indexOf(this.playSpeed);
        this.playSpeed = SPEED_STEPS[Math.max(i - 1, 0)] ?? this.playSpeed;
        e.preventDefault();
        break;
      }
      case '>':
      case '.': {
        const i = (SPEED_STEPS as readonly number[]).indexOf(this.playSpeed);
        this.playSpeed = SPEED_STEPS[Math.min(i + 1, SPEED_STEPS.length - 1)] ?? this.playSpeed;
        e.preventDefault();
        break;
      }
      case 'Home':
        this.goTo(0);
        e.preventDefault();
        break;
      case 'End':
        this.goTo(total);
        e.preventDefault();
        break;
      case 'n':
        this.jumpToNextError();
        e.preventDefault();
        break;
      case 'N':
        this.jumpToPrevError();
        e.preventDefault();
        break;
      default:
        if (e.key >= '0' && e.key <= '9') {
          // 0–9 jump to 0%, 10%, …, 90% — matches YouTube
          this.goTo(total * (Number(e.key) / 10));
          e.preventDefault();
        }
    }
  };

  disconnectedCallback() {
    super.disconnectedCallback();
    // Clean up global listeners and resources
    this.observer?.disconnect();
    this.resizeObserver?.disconnect();
    this.stopTimer();
    this.player?.destroy();
    this.trickPlayer?.destroy();
    if (this.trickPlayerCleanupTimer) {
      clearTimeout(this.trickPlayerCleanupTimer);
      this.trickPlayerCleanupTimer = null;
    }
    document.removeEventListener('keydown', this.handleKeydown);
  }

  handleConsoleEvents(event: eventWithTime) {
    if (event.type === EventType.Plugin && event.data.plugin === 'rrweb/console@1') {
      this.currentEventTime = event.timestamp;
    }
  }

  play(tm?: number) {
    const to = tm !== undefined ? tm : this.currentTime;
    const seek = Math.max(0, Math.min(to, this.metaData.totalTime));
    this.player?.play(seek);
    this.loopTimer();
  }

  pause() {
    this.player?.pause();
    this.stopTimer();
  }

  goTo(tm: number, paused?: boolean) {
    this.pause();
    if (!paused) {
      this.play(tm);
    }
  }

  loopTimer() {
    this.stopTimer();

    const update = () => {
      this.currentTime = Math.min(this.metaData.totalTime, this.player?.getCurrentTime() || 0);

      if (this.currentTime < this.metaData.totalTime) {
        this.finished = false;
        this.timer = requestAnimationFrame(update);
      } else {
        this.finished = true;
      }
    };

    this.timer = requestAnimationFrame(update);
  }

  stopTimer() {
    if (this.timer) {
      cancelAnimationFrame(this.timer);
      this.timer = null;
    }
  }

  debounce(fn: () => void) {
    if (this.timeout) {
      clearTimeout(this.timeout);
    }
    this.timeout = setTimeout(() => fn(), 10);
  }

  updateContainerWidths() {
    const frameContainer = document.querySelector('.player-frame') as HTMLElement;
    frameContainer.style.width = `${this.containerWidth}px`;
    frameContainer.style.height = `${this.containerHeight}px`;
  }
  updateScale = () => {
    this.updateContainerWidths();
    const el = this.player?.wrapper;
    const widthScale = this.containerWidth / this.iframeWidth;
    const heightScale = this.containerHeight / this.iframeHeight;
    if (el) {
      el.style.transform = `scale(${Math.min(widthScale, heightScale)}) translate(-50%, -50%)`;
    }
  };

  protected updated(changedProperties: PropertyValues): void {
    // Focus the cheatsheet's close button each time the panel opens so
    // keyboard and screen-reader users land inside the dialog without
    // having to mouse in first. Runs outside the player guard because the
    // panel can be opened before any session has loaded.
    if (changedProperties.has('showShortcuts') && this.showShortcuts) {
      requestAnimationFrame(() => {
        this.renderRoot.querySelector<HTMLElement>('[aria-label="Close shortcuts panel"]')?.focus();
      });
    }
    if (this.player) {
      if (changedProperties.has('activityWidth')) {
        this.debounce(() => {
          const mContainer = Number(getComputedStyle(this.replayerOuterContainer).width.replace('px', ''));
          this.containerWidth = mContainer - this.activityWidth;
          this.updateScale();
        });
      }
      if (changedProperties.has('skipInactive')) {
        this.player?.setConfig({ skipInactive: this.skipInactive });
      }
      if (changedProperties.has('playSpeed')) {
        this.pause();
        this.player?.setConfig({ speed: this.playSpeed });
        this.play();
      }
      if (changedProperties.has('paused')) {
        if (this.paused) {
          this.currentTime = this.player.getCurrentTime();
        }
        this.paused ? this.pause() : this.play();
      }
      if (changedProperties.has('currentEventTime')) {
        if (this.syncScrolling) {
          // Defer scrollIntoView to avoid forced reflow during property changes
          requestAnimationFrame(() => {
            const element = document.querySelector('#a-' + this.currentEventTime);
            if (element) {
              // Use smooth scrolling to reduce reflow impact
              element.scrollIntoView({ behavior: 'smooth', block: 'center' });
            }
          });
        }
      }
    }
  }

  static formatTime(timestamp: number): string {
    const totalSeconds = Math.floor(timestamp / 1000);
    const hours = Math.floor(totalSeconds / 3600);
    const minutes = Math.floor((totalSeconds % 3600) / 60);
    const seconds = totalSeconds % 60;

    const pad = (num: number) => String(num).padStart(2, '0');

    if (hours > 0) {
      return `${pad(hours)}:${pad(minutes)}:${pad(seconds)}`;
    } else {
      return `${pad(minutes)}:${pad(seconds)}`;
    }
  }

  toggleConsoleEvent(indx: number) {
    this.consoleEventsEnable[indx] = !this.consoleEventsEnable[indx];
    this.consoleEventsEnable = [...this.consoleEventsEnable];
  }

  async initiatePlayer(events: eventWithTime[] | undefined) {
    if (!events || events.length < 2) {
      this.loadError = 'This recording has too few events to play back — the session likely ended before recording started.';
      return;
    }
    this.loadError = null;
    this.events = events;
    const target = document.querySelector('#playerWrapper') as HTMLElement;
    this.currentTime = 0;
    this.consoleEvents = [];
    // Collect timeline markers relative to the first event's timestamp — rrweb
    // seeks use offsets, not wall-clock. Meta events past the first represent
    // full-page navigations (SPAs fire custom events instead).
    const sessionStart = events[0].timestamp;
    const errs: number[] = [];
    const warns: number[] = [];
    const navs: { offset: number; href: string }[] = [];
    let seenFirstMeta = false;
    events.forEach((event) => {
      if (event.type === EventType.Plugin && event.data.plugin === 'rrweb/console@1') {
        const ev = event as ConsoleEvent;
        const level = ev.data.payload.level;
        this.consoleTypesCounts[level] += 1;
        this.consoleEvents.push(ev);
        const offset = event.timestamp - sessionStart;
        if (level === 'error') errs.push(offset);
        else if (level === 'warn') warns.push(offset);
      } else if (event.type === EventType.Meta) {
        if (seenFirstMeta) {
          navs.push({ offset: event.timestamp - sessionStart, href: (event.data as any).href ?? '' });
        }
        seenFirstMeta = true;
      }
    });
    this.errorTicks = errs;
    this.warnTicks = warns;
    this.navMarkers = navs;

    this.player = new Replayer(events, { root: target, plugins: [{ handler: this.handleConsoleEvents }], skipInactive: this.skipInactive });
    this.trickPlayer = null;
    this.metaData = this.player.getMetaData();
    this.updateScale();
    // Honor ?t=SECONDS from the URL exactly once per fresh load — so a shared
    // link lands on the right moment. Clamp to session bounds in case events
    // changed since the link was copied.
    const params = new URLSearchParams(window.location.search);
    const tRaw = params.get('t');
    const startMs = tRaw ? Math.max(0, Math.min(this.metaData.totalTime, Number(tRaw) * 1000)) : 0;
    this.play(startMs);
    this.observer.disconnect();
    this.observer.observe(this.player.iframe, { attributes: true, attributeFilter: ['width', 'height'] });

    // Create ResizeObserver only when player is active
    const container = this.renderRoot.querySelector<HTMLDivElement>('#replayerOuterContainer');
    if (container && !this.resizeObserver) {
      this.resizeObserver = new ResizeObserver((entries) => {
        this.debounce(() => {
          for (let entry of entries) {
            const { width, height } = entry.contentRect;
            const comp = getComputedStyle(this.replayerOuterContainer);
            const mContainer = Number(comp.width.replace('px', ''));
            const mHeight = Number(comp.height.replace('px', ''));
            this.containerWidth = mContainer - this.activityWidth;
            this.containerHeight = mHeight - 124;

            this.updateScale();
          }
        });
      });
      this.resizeObserver.observe(container);
    }
  }

  fetchNewSessionData(sessionId: string) {
    if (this.isLoading) return;
    this.pause();
    try {
      this.player?.destroy();
    } catch (error) {
      console.warn('Replayer destroy failed:', error);
    }
    this.isLoading = true;
    this.loadError = null;
    this.currentSessionId = sessionId;
    // Reset identity so stale values don't flash across a failed load
    this.userEmail = null;
    this.userName = null;
    this.userIdLabel = null;
    // Reset timeline markers from the previous session
    this.errorTicks = [];
    this.warnTicks = [];
    this.navMarkers = [];
    this.consoleTypesCounts = { error: 0, warn: 0, info: 0 };
    const url = `/p/${this.projectId}/replay_session/${sessionId}`;
    // Capture locally so responses for abandoned sessions don't overwrite
    // state the user has already moved on from.
    const requestedId = sessionId;
    const isStale = () => this.currentSessionId !== requestedId;

    fetch(url, { method: 'GET', headers: { Accept: 'application/json' } })
      .then((response) => {
        if (!response.ok) {
          throw new Error(`Couldn’t reach the replay service (HTTP ${response.status}).`);
        }
        return response.json();
      })
      .then((data) => {
        if (isStale()) return;
        this.userEmail = data.userEmail ?? null;
        this.userName = data.userName ?? null;
        this.userIdLabel = data.userId ?? null;
        if (data.error) {
          this.loadError = data.error;
          return;
        }
        if (!data.events || !Array.isArray(data.events)) {
          this.loadError = 'The replay service returned an unexpected response. Retry, or contact support with the session ID below.';
          return;
        }
        if (data.events.length < 2) {
          this.loadError = 'This recording has too few events to play back — the session likely ended before recording started (closed tab, network drop, or SDK misconfiguration).';
          return;
        }
        this.initiatePlayer(data.events);
      })
      .catch((error) => {
        if (isStale()) return;
        console.error('Failed to load session replay:', error);
        this.loadError = error.message || 'We couldn’t load this session. Check your connection and retry.';
      })
      .finally(() => {
        if (isStale()) return;
        this.isLoading = false;
      });
  }

  // Human label for the session's user, falling back through the identity fields
  // we persist on the session row. Returns null if we know nothing about them.
  private userLabel(): string | null {
    return this.userEmail ?? this.userName ?? this.userIdLabel ?? null;
  }

  // ISO-ish starting timestamp for the header. rrweb's metaData.startTime is only
  // known after the player initializes, so before that we render "—".
  private formattedStartedAt(): string {
    if (!this.metaData.startTime) return '—';
    const d = new Date(this.metaData.startTime);
    return d.toLocaleString(undefined, { dateStyle: 'medium', timeStyle: 'short' });
  }

  private copySessionId = async () => {
    if (!this.currentSessionId) return;
    try {
      await navigator.clipboard.writeText(this.currentSessionId);
      this.sessionIdCopied = true;
      setTimeout(() => (this.sessionIdCopied = false), 1500);
    } catch (err) {
      // Clipboard blocked (insecure context, iframe without permission, Safari private).
      // The "Copied!" state stays false, giving implicit visual feedback; log so devs notice.
      console.warn('clipboard.writeText failed for session id:', err);
    }
  };

  // Copy a shareable URL pointing at the current moment. Support will paste
  // this in a ticket and the link-opener lands on exactly this timestamp —
  // the delight of a bug report that replays itself.
  private shareAtTimestamp = async () => {
    if (!this.currentSessionId) return;
    const url = new URL(window.location.href);
    url.searchParams.set('session_replay', this.currentSessionId);
    url.searchParams.set('t', String(Math.max(0, Math.floor(this.currentTime / 1000))));
    try {
      await navigator.clipboard.writeText(url.toString());
      this.momentLinkCopied = true;
      setTimeout(() => (this.momentLinkCopied = false), 1500);
    } catch (err) {
      console.warn('clipboard.writeText failed for share link:', err);
    }
  };

  private retryLoad = () => {
    if (this.currentSessionId) this.fetchNewSessionData(this.currentSessionId);
  };

  // Seek to the next / previous error tick past (or before) the current time.
  // A 250ms slop stops "next" from landing on the tick we're already sitting on.
  private jumpToNextError = () => {
    const next = this.errorTicks.find((t) => t > this.currentTime + 250);
    if (next !== undefined) this.goTo(next);
  };
  private jumpToPrevError = () => {
    const prev = [...this.errorTicks].reverse().find((t) => t < this.currentTime - 250);
    if (prev !== undefined) this.goTo(prev);
  };

  protected firstUpdated(_changedProperties: PropertyValues): void {
    const mContainer = Number(getComputedStyle(this.replayerOuterContainer).width.replace('px', ''));
    this.containerWidth = mContainer - this.activityWidth;
    if (this.initialSession) {
      this.fetchNewSessionData(this.initialSession);
    }
  }

  handleTimeSeek(e: any) {
    const x = e.clientX;
    const bounding = this.progressBar.getBoundingClientRect();
    const toWidth = x - bounding.x;
    const toGo = (toWidth * this.metaData.totalTime) / bounding.width;
    this.goTo(toGo);
  }

  handleTrickPlay(e: any) {
    // Clear any pending cleanup timer
    if (this.trickPlayerCleanupTimer) {
      clearTimeout(this.trickPlayerCleanupTimer);
      this.trickPlayerCleanupTimer = null;
    }

    if (!this.trickPlayer) {
      this.trickPlayer = new Replayer(this.events, { root: document.querySelector('#trickPlayerContainer')! });
    }
    const x = e.clientX;
    const bounding = this.progressBar.getBoundingClientRect();
    const toWidth = x - bounding.x;
    const toGo = (toWidth * this.metaData.totalTime) / bounding.width;
    const el = this.trickPlayer.wrapper;
    const iframe = this.trickPlayer.iframe;
    const widthSc = 240 / Number(iframe.getAttribute('width'));
    const heightSc = 160 / Number(iframe.getAttribute('height'));
    if (el) {
      el.style.transform = `scale(${Math.min(widthSc, heightSc)}) translate(-50%, -50%)`;
    }
    this.trickPlayer?.play(toGo);
    this.trickTarget = toGo;
  }

  closePlayerWindow() {
    document.querySelector(`#${this.containerId}`)?.classList.add('hidden');
    this.currentTime = 0;
    this.stopTimer();
    this.consoleEvents = [];
    this.loadError = null;
    this.userEmail = null;
    this.userName = null;
    this.userIdLabel = null;
    this.currentSessionId = '';
    this.player?.destroy();
    this.resizeObserver?.disconnect();
    this.resizeObserver = null;
  }

  displayConsoleEvent = (event: ConsoleEvent) => {
    const payload = event.data.payload;
    if (payload.level === 'error' && !this.consoleEventsEnable[0]) return nothing;
    if (payload.level === 'warn' && !this.consoleEventsEnable[1]) return nothing;
    if (payload.level === 'info' && !this.consoleEventsEnable[2]) return nothing;

    let bgColor = '';
    let textColor = '';
    let hoverColor = '';

    switch (payload.level) {
      case 'error':
        bgColor = 'bg-fillError-weak';
        textColor = 'text-textError';
        hoverColor = 'hover:bg-fillError-weak';
        break;
      case 'warn':
        bgColor = 'bg-fillWarning-weak';
        textColor = 'text-textWarning';
        hoverColor = 'hover:bg-fillWarning-weak';
        break;
      case 'info':
        bgColor = 'bg-fillBrand-weak';
        textColor = 'text-textBrand';
        hoverColor = 'hover:bg-fillBrand-weak';
        break;
      default:
        return nothing;
    }

    return html`
      <div class="text-sm flex flex-col min-w-0 event-container" id="a-${event.timestamp}">
        <div class="flex items-center w-full">
          <span class="h-2 w-2 shrink-0 rounded-full ${this.currentEventTime === event.timestamp ? 'bg-fillBrand-strong' : ''} ml-1"></span>
          <span class="text-xs font-medium text-center text-textWeak min-w-11">
            ${SessionReplay.formatTime(event.timestamp - this.metaData.startTime)}
          </span>
          ${faSprite_('console', 'regular', 'w-3 h-3 mr-1 shrink-0 font-semibold')}
          <span class="w-full min-w-0 truncate px-2 pb-1 overflow-ellipsis ${hoverColor} ${bgColor}">
            ${payload.payload.join('').substring(0, 100)}
          </span>
          <button
            @click=${(e: any) => {
              const container = e.currentTarget.closest('.event-container');
              container?.classList.toggle('expanded');
            }}
            class="cursor-pointer h-full flex flex-col px-1 rounded-lg shrink-0 items-center justify-center hover:bg-fillWeak"
          >
            ${faSprite_('chevron-up', 'regular', 'w-2.5 h-2.5 fill-current')}
          </button>
        </div>
        <div class="flex-col ${bgColor} p-2 w-full min-w-0 hidden event-detail">
          <div class="bg-bgBase rounded-xl p-3 whitespace-pre-wrap relative ${textColor} font-medium">
            <button
              class="absolute right-2 cursor-pointer"
              @click=${() => {
                navigator.clipboard.writeText(payload.payload.join('\n'));
              }}
            >
              ${faSprite_('copy', 'regular', 'h-5 w-5')}
            </button>
            <span>${payload.payload.join('\n')}</span>
          </div>
          <span class="mt-4 mb-1 font-semibold">Stack trace</span>
          <div class="bg-bgBase rounded-xl p-3 whitespace-pre-wrap relative font-medium">
            <button
              class="absolute right-2 cursor-pointer"
              @click=${() => {
                navigator.clipboard.writeText(payload.trace.join('\n'));
              }}
            >
              ${faSprite_('copy', 'regular', 'h-5 w-5')}
            </button>
            <span>${payload.trace.join('\n')}</span>
          </div>
        </div>
      </div>
    `;
  };

  render() {
    // Below ~760px the identity row gets cramped: user + timestamp + duration +
    // "N errors" + "Copy session ID" + "Share moment" can't all breathe.
    // Demote the two utility buttons to icon-only; tooltips carry the label.
    const compactHeader = this.containerWidth + this.activityWidth < 760;
    return html`<div
      class="flex overflow-hidden rr-block resize relative rounded shadow-lg min-w-[640px] max-w-[90vw] min-h-[400px]"
      id="replayerOuterContainer"
      style="height:${this.containerHeight + 124}px; width:${this.containerWidth + this.activityWidth}px"
    >
      <!-- Intentional SE resize hint: the native handle still drives resize, this is just the affordance -->
      <div
        class="absolute bottom-0 right-0 w-3 h-3 pointer-events-none opacity-50"
        aria-hidden="true"
        style="background: linear-gradient(135deg, transparent 0 50%, var(--color-strokeStrong) 50% 60%, transparent 60% 70%, var(--color-strokeStrong) 70% 80%, transparent 80% 100%);"
      ></div>
      <div class="w-full flex flex-col justify-start shrink-1 min-w-0 overflow-hidden">
        <div class="bg-fillWeak w-full px-3 h-10 min-h-10 flex items-center border-b gap-4 cursor-move justify-between playerHeader">
          <!-- Identity block: who, when, how long, how many errors -->
          <div class="flex items-center gap-3 min-w-0 shrink">
            <div class="flex flex-col min-w-0 leading-tight">
              <span class="text-sm font-semibold truncate" title=${this.userLabel() ?? 'Anonymous session — no user was identified when this was recorded'}>
                ${this.userLabel() ?? 'Anonymous session'}
              </span>
              <span class="text-[11px] text-textWeak font-medium tabular-nums flex items-center gap-1.5 truncate">
                <span title="Session started">${this.formattedStartedAt()}</span>
                <span aria-hidden="true">·</span>
                <span title="Session duration" class="whitespace-nowrap">${SessionReplay.formatTime(this.metaData.totalTime || 0)}</span>
                ${this.consoleTypesCounts.error > 0
                  ? html`<span aria-hidden="true">·</span>
                      <button
                        @click=${() => {
                          if (this.activityWidth <= 0) this.activityWidth = 300;
                          this.jumpToNextError();
                        }}
                        class="text-textError font-semibold whitespace-nowrap cursor-pointer hover:underline"
                        title="Jump to the next error and open the console"
                      >
                        ${this.consoleTypesCounts.error} error${this.consoleTypesCounts.error === 1 ? '' : 's'}
                      </button>`
                  : nothing}
              </span>
            </div>
            <!-- Copy / Share utilities: icon group, text labels collapse at narrow widths -->
            <div class="flex items-center gap-1 shrink-0">
              <button
                @click=${this.copySessionId}
                class="flex items-center gap-1 px-1.5 h-6 rounded cursor-pointer hover:bg-fillWeaker text-textWeak hover:text-textStrong text-[11px] font-medium"
                title="Copy session ID"
                aria-label="Copy session ID"
              >
                ${faSprite_('copy', 'regular', 'w-3 h-3')}
                ${compactHeader
                  ? nothing
                  : html`<span class="whitespace-nowrap">${this.sessionIdCopied ? 'Copied' : 'Copy session ID'}</span>`}
              </button>
              <button
                @click=${this.shareAtTimestamp}
                class="flex items-center gap-1 px-1.5 h-6 rounded cursor-pointer hover:bg-fillWeaker text-textWeak hover:text-textStrong text-[11px] font-medium disabled:opacity-50 disabled:cursor-not-allowed"
                title="Copy a link that opens the player at this moment"
                aria-label="Share this moment"
                ?disabled=${this.isLoading || !!this.loadError}
              >
                ${faSprite_('link-simple', 'regular', 'w-3 h-3')}
                ${compactHeader
                  ? nothing
                  : html`<span class="whitespace-nowrap">${this.momentLinkCopied ? 'Link copied' : 'Share moment'}</span>`}
              </button>
            </div>
          </div>

          <div class="flex items-center gap-4 text-xs font-semibold">
            <div class="dropdown" title="Playback speed (press &lt; or &gt; to step)">
              <div
                tabindex="0"
                role="button"
                aria-label="Playback speed"
                class="cursor-pointer flex items-center gap-1 tabular-nums ${this.playSpeed !== 1 ? 'text-textBrand' : ''}"
              >
                ${faSprite_('gauge', 'regular', 'w-3 h-3')} Speed ${this.playSpeed}×
              </div>
              <ul tabindex="0" class="dropdown-content menu bg-base-100 border text-xs rounded-box z-1 w-max p-2 shadow">
                ${SPEED_STEPS.map(
                  (s) =>
                    html`<li>
                      <button
                        class="px-4 rounded py-1 cursor-pointer hover:bg-fillWeak tabular-nums ${this.playSpeed === s
                          ? 'bg-fillBrand-strong text-textInverse-strong'
                          : ''}"
                        @click=${() => (this.playSpeed = s)}
                      >
                        ${s}×
                      </button>
                    </li>`
                )}
              </ul>
            </div>
            <button
              class="flex items-center cursor-pointer gap-1 ${this.skipInactive ? 'text-textBrand' : ''}"
              title="Skip idle stretches longer than 2 seconds"
              aria-pressed=${this.skipInactive}
              @click=${() => (this.skipInactive = !this.skipInactive)}
            >
              ${faSprite_('skip', 'regular', 'w-3 h-3')}
              <span>Skip idle</span>
            </button>
            <button
              @click=${() => {
                this.activityWidth = this.activityWidth <= 0 ? 300 : 0;
              }}
              class="cursor-pointer flex items-center gap-1 ${this.activityWidth > 0 ? 'text-textBrand' : ''}"
              title="Toggle the console panel (errors, warnings, logs)"
              aria-pressed=${this.activityWidth > 0}
            >
              ${faSprite_('side-chevron-left-in-box', 'regular', 'w-3 h-3')}
              <span>Console</span>
              ${this.consoleTypesCounts.error > 0
                ? html`<span class="text-textError tabular-nums">(${this.consoleTypesCounts.error})</span>`
                : nothing}
            </button>
            ${this.activityWidth === 0
              ? html`<button
                  class="hover:bg-fillWeak cursor-pointer"
                  @click=${this.closePlayerWindow}
                  title="Close player (Esc)"
                  aria-label="Close session player"
                >
                  ${faSprite_('circle-xmark', 'regular', 'w-4 h-4')}
                </button>`
              : nothing}
          </div>
        </div>
        <!-- End nav controls -->
        <div class="relative">
          <div class="border-b relative bg-black">
            <div
              class="player-frame border-y overflow-hidden"
              id="playerWrapper"
              style="height:${this.containerHeight}px; width:${this.containerWidth}px"
            ></div>
            <div
              class="absolute inset-0 flex items-center justify-center ${this.loadError || this.isLoading ? 'bg-black/75' : 'bg-black/25'} ${this.paused || this.finished || this.isLoading || this.loadError
                ? ''
                : 'hidden'}"
            >
              ${this.isLoading
                ? html`
                    <div class="flex flex-col items-center gap-3 text-center px-8" role="status" aria-live="polite">
                      <div class="flex items-center gap-2 text-base font-medium text-gray-100">
                        <span class="inline-block h-2 w-2 rounded-full bg-gray-100 motion-safe:animate-pulse"></span>
                        Loading session recording
                      </div>
                      ${this.currentSessionId
                        ? html`<div class="text-xs text-gray-300 tabular-nums">Session ${this.currentSessionId.slice(0, 8)}…</div>`
                        : nothing}
                    </div>
                  `
                : this.loadError
                ? html`
                    <div class="flex flex-col items-center gap-4 text-center px-8 max-w-lg" role="alert" aria-live="assertive">
                      ${faSprite_('triangle-exclamation', 'regular', 'w-10 h-10 text-textError')}
                      <div class="text-lg font-semibold text-gray-100">Can’t play this session</div>
                      <div class="text-sm text-gray-200">${this.loadError}</div>
                      <div class="flex items-center gap-2 mt-2">
                        <button
                          @click=${this.retryLoad}
                          class="flex items-center gap-1.5 px-3 py-1.5 rounded bg-white/10 hover:bg-white/20 text-sm font-medium text-gray-100 cursor-pointer"
                          title="Retry loading this session"
                        >
                          ${faSprite_('replay', 'regular', 'w-3.5 h-3.5')}
                          Retry
                        </button>
                        <button
                          @click=${this.copySessionId}
                          class="flex items-center gap-1.5 px-3 py-1.5 rounded bg-white/5 hover:bg-white/15 text-sm font-medium text-gray-200 cursor-pointer"
                          title="Copy session ID"
                        >
                          ${faSprite_('copy', 'regular', 'w-3.5 h-3.5')}
                          ${this.sessionIdCopied ? 'Copied' : 'Copy session ID'}
                        </button>
                      </div>
                      ${this.currentSessionId
                        ? html`<div class="text-[11px] text-gray-400 font-mono tabular-nums select-all">${this.currentSessionId}</div>`
                        : nothing}
                    </div>
                  `
                : this.finished
                ? html`
                    <div class="flex flex-col items-stretch gap-3 max-w-sm w-full px-6">
                      <!-- Instrument-grade end card: numbers first, navigation second -->
                      <div class="text-[10px] uppercase tracking-wider text-gray-400 font-semibold">End of session</div>
                      <div class="flex items-baseline gap-5 text-gray-100">
                        <div class="flex flex-col">
                          <span class="text-2xl font-semibold tabular-nums leading-none">${SessionReplay.formatTime(this.metaData.totalTime)}</span>
                          <span class="text-[10px] uppercase tracking-wider text-gray-400 mt-1">Duration</span>
                        </div>
                        <div class="flex flex-col">
                          <span class="text-2xl font-semibold tabular-nums leading-none ${this.consoleTypesCounts.error > 0 ? 'text-textError' : ''}">${this.consoleTypesCounts.error}</span>
                          <span class="text-[10px] uppercase tracking-wider text-gray-400 mt-1">Errors</span>
                        </div>
                        <div class="flex flex-col">
                          <span class="text-2xl font-semibold tabular-nums leading-none">${this.navMarkers.length + 1}</span>
                          <span class="text-[10px] uppercase tracking-wider text-gray-400 mt-1">Pages</span>
                        </div>
                      </div>
                      ${this.errorTicks.length > 0
                        ? html`<div class="flex flex-col gap-1 mt-1">
                            <div class="text-[10px] uppercase tracking-wider text-gray-400 font-semibold">Jump to error</div>
                            <div class="flex flex-wrap gap-1">
                              ${this.errorTicks.slice(0, 6).map(
                                (t, i) => html`<button
                                  @click=${() => this.goTo(t)}
                                  class="cursor-pointer flex items-center gap-1 px-2 py-1 rounded bg-white/5 hover:bg-white/15 text-xs text-gray-100 tabular-nums"
                                  title="Jump to error ${i + 1}"
                                >
                                  <span class="h-1.5 w-1.5 rounded-full bg-textError"></span>
                                  ${SessionReplay.formatTime(t)}
                                </button>`
                              )}
                              ${this.errorTicks.length > 6
                                ? html`<span class="text-xs text-gray-400 px-1 self-center tabular-nums">+${this.errorTicks.length - 6} more</span>`
                                : nothing}
                            </div>
                          </div>`
                        : nothing}
                      <div class="flex gap-2 mt-1">
                        <button
                          @click=${() => this.goTo(0)}
                          class="flex-1 cursor-pointer flex items-center justify-center gap-2 px-3 py-2 rounded bg-white/15 hover:bg-white/25 text-sm font-medium text-gray-100"
                          title="Replay from the beginning (Home)"
                        >
                          ${faSprite_('replay', 'regular', 'w-3.5 h-3.5')}
                          Replay from start
                        </button>
                        <button
                          @click=${this.shareAtTimestamp}
                          class="cursor-pointer flex items-center justify-center gap-1.5 px-3 py-2 rounded bg-white/5 hover:bg-white/15 text-sm font-medium text-gray-200"
                          title="Copy a link that opens the player at this moment"
                        >
                          ${faSprite_('link-simple', 'regular', 'w-3.5 h-3.5')}
                          ${this.momentLinkCopied ? 'Link copied' : 'Share moment'}
                        </button>
                      </div>
                    </div>
                  `
                : html`
                    <button
                      @click=${() => (this.paused = false)}
                      class="cursor-pointer text-gray-100"
                      title="Resume playback"
                      aria-label="Resume playback"
                    >
                      ${faSprite_('p-play', 'regular', 'w-14 h-14')}
                    </button>
                  `}
            </div>
          </div>
          <div class="flex flex-col items-center w-full px-4 pt-3 pb-1 gap-1">
            <div
              id="progressBar"
              @click=${this.handleTimeSeek}
              @mouseover=${this.handleTrickPlay}
              @mousemove=${this.handleTrickPlay}
              @mouseleave=${() => {
                this.trickPlayer?.pause();
                this.trickPlayerCleanupTimer = window.setTimeout(() => {
                  if (this.trickPlayer) {
                    this.trickPlayer.destroy();
                    this.trickPlayer = null;
                  }
                  this.trickPlayerCleanupTimer = null;
                }, 1000);
              }}
              class="relative progress-container h-2.5 cursor-pointer rounded-sm group bg-fillWeak w-full"
            >
              <div
                class="bg-bgInverse absolute text-sm font-medium rounded -translate-x-1/2 overflow-hidden border border-strokeStrong hidden group-hover:block"
                style="left:${(this.trickTarget / this.metaData.totalTime) * 100}%; top:-200px"
              >
                <div id="trickPlayerContainer" class="h-40 w-60 bg-black overflow-hidden"></div>
                <div class="text-textInverse text-center w-full py-2 tabular-nums">${SessionReplay.formatTime(this.trickTarget)}</div>
              </div>
              <div class="relative h-full bg-fillBrand-strong rounded-sm" style="width:${(this.currentTime / (this.metaData.totalTime || 1)) * 100}%">
                <span class="absolute -right-1.5 h-3.5 w-3.5 top-1/2 -translate-y-1/2 rounded-full bg-fillBrand-strong ring-2 ring-bgBase shadow-sm"></span>
              </div>
              <!-- Warning ticks: half-height amber, rendered beneath errors -->
              ${this.warnTicks.map(
                (t) => html`<span
                  class="absolute top-0 h-1/2 w-0.5 bg-fillWarning-strong pointer-events-none"
                  style="left:${(t / (this.metaData.totalTime || 1)) * 100}%"
                  title="Warning at ${SessionReplay.formatTime(t)}"
                ></span>`
              )}
              <!-- Error ticks: full-height red, sit on top -->
              ${this.errorTicks.map(
                (t) => html`<span
                  class="absolute top-0 h-full w-0.5 bg-textError pointer-events-none"
                  style="left:${(t / (this.metaData.totalTime || 1)) * 100}%"
                  title="Error at ${SessionReplay.formatTime(t)}"
                ></span>`
              )}
            </div>
            <!-- Navigation lane: page loads past the first meta event. Only
                 render when there are navs — otherwise we just add dead gutter -->
            ${this.navMarkers.length > 0
              ? html`<div class="relative w-full h-2" aria-hidden="true">
                  ${this.navMarkers.map(
                    (m) => html`<span
                      class="absolute top-0 h-1.5 w-px bg-strokeStrong pointer-events-none"
                      style="left:${(m.offset / (this.metaData.totalTime || 1)) * 100}%"
                      title="Page ${m.href} at ${SessionReplay.formatTime(m.offset)}"
                    ></span>`
                  )}
                </div>`
              : nothing}
            <div class="flex py-2 w-full items-center gap-3">
              <div
                class="w-24 shrink-0 text-xs flex gap-0.5 font-medium flex-nowrap tabular-nums"
                title="Current time / total session length"
                aria-label="Current time over total session length"
              >
                <span>${SessionReplay.formatTime(this.currentTime)}</span>
                <span>/</span>
                <span class="text-textWeak">${SessionReplay.formatTime(this.metaData.totalTime)}</span>
              </div>
              <div class="flex-1 gap-3 flex items-center justify-center">
                ${this.errorTicks.length > 0
                  ? html`<button
                      class="cursor-pointer flex items-center gap-1 text-textError disabled:opacity-30 disabled:cursor-not-allowed"
                      @click=${this.jumpToPrevError}
                      ?disabled=${!this.errorTicks.some((t) => t < this.currentTime - 250)}
                      title="Previous error (Shift+N)"
                      aria-label="Previous error"
                    >
                      ${faSprite_('chevron-left', 'regular', 'h-3.5 w-3.5')}
                      <span class="h-1.5 w-1.5 rounded-full bg-textError"></span>
                    </button>`
                  : nothing}
                <button
                  class="relative cursor-pointer"
                  @click=${() => this.goTo(this.currentTime - MS_10)}
                  title="Back 10 seconds (←)"
                  aria-label="Back 10 seconds"
                >
                  <span class="absolute top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2 font-semibold text-[8px]">10</span>
                  ${faSprite_('time-skip', 'regular', 'h-5 w-5')}
                </button>
                ${this.finished
                  ? html`<button
                      class="flex justify-center cursor-pointer items-center"
                      @click=${() => this.goTo(0)}
                      title="Replay from start"
                      aria-label="Replay session from start"
                    >
                      ${faSprite_('replay', 'regular', 'h-5 w-5')}
                    </button>`
                  : html`
                      <button
                        class="flex justify-center cursor-pointer items-center"
                        @click=${() => (this.paused = !this.paused)}
                        title=${this.paused ? 'Play (Space)' : 'Pause (Space)'}
                        aria-label=${this.paused ? 'Play' : 'Pause'}
                      >
                        ${this.paused ? faSprite_('p-play', 'regular', 'h-5 w-5') : faSprite_('p-pause', 'regular', 'h-5 w-5')}
                      </button>
                    `}
                <button
                  class="relative cursor-pointer"
                  @click=${() => this.goTo(this.currentTime + MS_10)}
                  title="Forward 10 seconds (→)"
                  aria-label="Forward 10 seconds"
                >
                  <span class="absolute top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2 font-semibold text-[8px]">10</span>
                  ${faSprite_('time-skip', 'regular', 'h-5 w-5 rotate-y-180')}
                </button>
                ${this.errorTicks.length > 0
                  ? html`<button
                      class="cursor-pointer flex items-center gap-1 text-textError disabled:opacity-30 disabled:cursor-not-allowed"
                      @click=${this.jumpToNextError}
                      ?disabled=${!this.errorTicks.some((t) => t > this.currentTime + 250)}
                      title="Next error (N)"
                      aria-label="Next error"
                    >
                      <span class="h-1.5 w-1.5 rounded-full bg-textError"></span>
                      ${faSprite_('chevron-right', 'regular', 'h-3.5 w-3.5')}
                    </button>`
                  : nothing}
              </div>
              <!-- Keyboard-shortcut hint: always-visible circular button so
                   touch users (who can't press ?) can still discover shortcuts,
                   and narrow embeds don't lose the affordance. Kept inside a
                   w-24 shrink-0 wrapper so the center cluster stays optically
                   centered at all widths. -->
              <div class="w-24 shrink-0 flex justify-end">
                <button
                  @click=${() => (this.showShortcuts = !this.showShortcuts)}
                  class="inline-flex items-center justify-center h-6 min-w-6 px-1.5 rounded-full border border-strokeWeak bg-fillWeak text-textWeak font-mono text-[11px] tabular-nums cursor-pointer hover:bg-fillStrong/10 hover:text-textStrong hover:border-strokeStrong"
                  title="Keyboard shortcuts (?)"
                  aria-label="Show keyboard shortcuts"
                  aria-expanded=${this.showShortcuts}
                >
                  ?
                </button>
              </div>
            </div>
          </div>
        </div>
      </div>

      <div class="shrink-0 h-full relative flex items-start border-l" id="replay-activity-bar" style="width:${this.activityWidth}px">
        <div
          class="w-1 h-full absolute z-10 cursor-col-resize left-0 top-0 hover:bg-fillBrand-strong"
          @mousedown=${(e: any) => {
            document.body.style.userSelect = 'none';
            this.startX = e.clientX;
          }}
        ></div>
        <div class="w-full h-full overflow-hidden">
          <div class="bg-fillWeak w-full px-4 h-10 cursor-move flex items-center border-b gap-4 justify-between playerHeader">
            <div class="flex items-center gap-4 text-xs font-semibold">
              <span class="font-semibold">Console</span>
              <div class="dropdown" title="Show or hide event levels">
                <div
                  tabindex="0"
                  role="button"
                  class="cursor-pointer flex items-center gap-1 text-textWeak hover:text-textStrong"
                  aria-label="Filter console events by level"
                >
                  ${faSprite_('filter', 'regular', 'w-3 h-3')} Filter
                </div>
                <ul
                  tabindex="0"
                  class="dropdown-content menu flex flex-col gap-1 text-xs bg-base-100 border w-max rounded-box z-1 p-1 shadow"
                >
                  ${(
                    [
                      { label: 'Errors', idx: 0, count: this.consoleTypesCounts.error },
                      { label: 'Warnings', idx: 1, count: this.consoleTypesCounts.warn },
                      { label: 'Info', idx: 2, count: this.consoleTypesCounts.info },
                    ] as const
                  ).map(
                    ({ label, idx, count }) => html`
                      <li>
                        <button
                          class="px-4 py-1 flex items-center justify-between gap-4 rounded hover:bg-fillWeak ${this.consoleEventsEnable[idx] ? 'bg-fillBrand-weak text-textBrand' : 'text-textWeak'}"
                          @click=${() => this.toggleConsoleEvent(idx)}
                          title="${this.consoleEventsEnable[idx] ? 'Hide' : 'Show'} ${label.toLowerCase()}"
                          aria-pressed=${this.consoleEventsEnable[idx]}
                        >
                          <span>${this.consoleEventsEnable[idx] ? '✓ ' : ''}${label}</span>
                          <span class="tabular-nums text-textWeak">${count}</span>
                        </button>
                      </li>
                    `
                  )}
                </ul>
              </div>
            </div>
            ${this.activityWidth > 0
              ? html`<button
                  class="cursor-pointer hover:bg-fillWeak"
                  @click=${this.closePlayerWindow}
                  title="Close player"
                  aria-label="Close session player"
                >
                  ${faSprite_('circle-xmark', 'regular', 'w-4 h-4')}
                </button>`
              : nothing}
          </div>
          <div
            class="flex flex-col h-full overflow-y-auto w-full overflow-x-hidden c-scroll scroll-smooth"
            style="height:calc(100% - 80px)"
          >
            ${this.consoleEvents.length === 0
              ? html`<div class="px-4 py-6 text-xs text-textWeak">
                  No console output captured in this session. Errors, warnings, and <code>console.*</code> calls made in the browser tab will appear here as the recording plays.
                </div>`
              : this.consoleEvents.map((e) => this.displayConsoleEvent(e))}
          </div>
          <div class="flex items-center h-10 bg-fillWeak border-t">
            <button
              @click=${() => (this.syncScrolling = !this.syncScrolling)}
              class="text-xs font-semibold h-full px-2 cursor-pointer hover:bg-fillWeaker ${this.syncScrolling ? 'text-textBrand' : ''}"
              title="Automatically scroll this list to match the current playback time"
              aria-pressed=${this.syncScrolling}
            >
              ${this.syncScrolling ? '✓ ' : ''}Auto-follow playback
            </button>
          </div>
        </div>
      </div>
      ${this.showShortcuts ? this.renderShortcutsPanel() : nothing}
    </div>`;
  }
  // Keyboard cheatsheet — pinned over the player, dismissed by click-outside,
  // Esc, or the close button. Grouped so users can scan by intent, not by key.
  private renderShortcutsPanel() {
    const row = (keys: string[], label: string) => html`
      <div class="flex items-center justify-between gap-3 py-1">
        <span class="text-textStrong">${label}</span>
        <span class="flex items-center gap-1 shrink-0">
          ${keys.map(
            (k) => html`<kbd
              class="px-1.5 py-0.5 rounded border border-strokeStrong bg-fillWeak font-mono text-[10px] tabular-nums text-textStrong"
              >${k}</kbd
            >`
          )}
        </span>
      </div>
    `;
    return html`
      <div
        class="absolute inset-0 flex items-center justify-center bg-black/50 z-20 p-6"
        role="dialog"
        aria-modal="true"
        aria-label="Keyboard shortcuts"
        @click=${(e: Event) => {
          if (e.target === e.currentTarget) this.showShortcuts = false;
        }}
      >
        <div
          class="bg-bgBase border border-strokeStrong rounded shadow-lg w-full max-w-lg max-h-full overflow-y-auto"
          @click=${(e: Event) => e.stopPropagation()}
        >
          <div class="flex items-baseline justify-between px-5 py-4 border-b">
            <div>
              <div class="text-[10px] uppercase tracking-wider text-textWeak font-semibold">Reference</div>
              <div class="text-sm font-semibold text-textStrong">Keyboard shortcuts</div>
            </div>
            <button
              @click=${() => (this.showShortcuts = false)}
              class="text-textWeak hover:text-textStrong cursor-pointer"
              aria-label="Close shortcuts panel"
              title="Close (Esc)"
              autofocus
            >
              ${faSprite_('xmark', 'regular', 'w-4 h-4')}
            </button>
          </div>
          <div class="grid grid-cols-1 sm:grid-cols-2 gap-x-8 gap-y-5 px-5 py-4 text-xs">
            <section>
              <div class="text-[10px] uppercase tracking-wider text-textWeak font-semibold mb-1.5">Playback</div>
              ${row(['Space', 'K'], 'Play / pause')}
              ${row(['Home'], 'Jump to start')}
              ${row(['End'], 'Jump to end')}
            </section>
            <section>
              <div class="text-[10px] uppercase tracking-wider text-textWeak font-semibold mb-1.5">Seek</div>
              ${row(['←', 'J'], 'Back 10 seconds')}
              ${row(['→', 'L'], 'Forward 10 seconds')}
              ${row(['↑'], 'Back 30 seconds')}
              ${row(['↓'], 'Forward 30 seconds')}
              ${row(['0–9'], 'Jump to 0%, 10% … 90%')}
            </section>
            <section>
              <div class="text-[10px] uppercase tracking-wider text-textWeak font-semibold mb-1.5">Speed</div>
              ${row(['<'], 'Slow down')}
              ${row(['>'], 'Speed up')}
            </section>
            <section>
              <div class="text-[10px] uppercase tracking-wider text-textWeak font-semibold mb-1.5">Errors</div>
              ${row(['N'], 'Next error')}
              ${row(['Shift', 'N'], 'Previous error')}
            </section>
            <section class="sm:col-span-2">
              <div class="text-[10px] uppercase tracking-wider text-textWeak font-semibold mb-1.5">Panel</div>
              ${row(['?'], 'Toggle this panel')}
              ${row(['Esc'], 'Close panel, then close player')}
            </section>
          </div>
          <div class="px-5 py-3 border-t text-[11px] text-textWeak">
            Shortcuts follow YouTube and media-player conventions.
          </div>
        </div>
      </div>
    `;
  }
  createRenderRoot() {
    return this;
  }

  makeDraggable() {
    const element = document.querySelector('#sessionPlayerWrapper') as HTMLElement;
    const headers = element.querySelectorAll<HTMLElement>('.playerHeader');

    let isDragging = false;
    let offsetX: number, offsetY: number;

    headers.forEach((h) =>
      h.addEventListener('mousedown', (e: any) => {
        isDragging = true;
        offsetX = e.clientX - element.getBoundingClientRect().left;
        offsetY = e.clientY - element.getBoundingClientRect().top;

        // Add temporary event listeners
        document.addEventListener('mousemove', onMouseMove);
        document.addEventListener('mouseup', onMouseUp);
        document.body.style.userSelect = 'none';
      })
    );

    const onMouseMove = (e: any) => {
      if (!isDragging) return;
      // Clamp drag so the user can always grab the header back: keep at least
      // 40px of the element on-screen horizontally, and never let the top edge
      // go above the viewport (otherwise the header is unreachable).
      const margin = 40;
      const maxLeft = window.innerWidth - margin;
      const maxTop = window.innerHeight - margin;
      const minLeft = margin - element.offsetWidth;
      const minTop = 0;
      const nextLeft = Math.min(Math.max(e.clientX - offsetX, minLeft), maxLeft);
      const nextTop = Math.min(Math.max(e.clientY - offsetY, minTop), maxTop);
      element.style.left = nextLeft + 'px';
      element.style.top = nextTop + 'px';
    };

    const onMouseUp = () => {
      isDragging = false;
      document.body.style.userSelect = '';
      document.removeEventListener('mousemove', onMouseMove);
      document.removeEventListener('mouseup', onMouseUp);
    };
  }
}
