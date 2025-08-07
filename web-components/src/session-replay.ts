import { html, LitElement, nothing, PropertyValues } from 'lit';
import { customElement, property, query, state } from 'lit/decorators.js';
import rrwebPlayer from 'rrweb-player';
import { EventType, eventWithTime } from '@rrweb/types';
import { faSprite_ } from './monitors/test-editor-utils';
import { ConsoleEvent } from './types/types';
// import 'rrweb-player/dist/style.css';
import { Replayer } from '@rrweb/replay';

const MS_10 = 10000;
@customElement('session-replay')
export class SessionReplay extends LitElement {
  @property({ type: String }) private events: string = '[]';
  @property({ type: String }) private projectId: string = '';

  @state() private activityWidth = 0;
  @query('#replayerOuterContainer') private replayerOuterContainer;
  @state() private playSpeed = 1;
  @state() private skipInactive = true;
  @state() private consoleEventsEnable = [true, true, true]; // error, warn, info;
  @state() private paused = false;
  @state() private isLoading = true;

  @state() private consoleEvents: ConsoleEvent[] = [];
  @state() private currentEventTime: number = 0;
  @state() private currentTime = 0;
  @state() private finished = false;

  private startX: number | null = null;
  private player: Replayer | null = null;
  private containerWidth = 1124;
  private containerHeight = 650;
  private iframeWidth = 1117;

  private iframeHeight = 927;
  private timer: number | null = null;
  private consoleTypesCounts = { error: 0, warn: 0, info: 0 };
  private metaData: { startTime: number; endTime: number; totalTime: number } = { startTime: 0, endTime: 0, totalTime: 0 };
  constructor() {
    super();
    this.activityWidth = 0;
    document.addEventListener('mouseup', () => {
      document.body.style.userSelect = '';
      if (this.startX !== null) {
        localStorage.setItem('replay-activing-width', String(this.startX));
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

    document.addEventListener('mousemove', (e) => {
      if (this.startX !== null) {
        const diff = this.startX - e.clientX;
        const curr = this.activityWidth + diff;
        this.activityWidth = curr;
        this.startX = e.clientX;
      }
    });

    window.addEventListener('loadingSessionReplay', (e) => {
      const { sessionId } = (e as CustomEvent<{ sessionId: string }>).detail;
      this.fetchNewSessionData(sessionId);
      updateUrlState('session_replay', sessionId);
    });
  }

  handleConsoleEvents(event: eventWithTime) {
    if (event.type === EventType.Plugin && event.data.plugin === 'rrweb/console@1') {
      this.currentEventTime = event.timestamp;
    }
  }

  play(tm?: number) {
    this.player?.play(tm !== undefined ? tm : this.currentTime);
    this.loopTimer();
  }

  pause() {
    this.player?.pause();
    this.stopTimer();
  }

  goTo(tm: number) {
    this.pause();
    this.play(tm);
  }

  loopTimer() {
    this.stopTimer();

    const update = () => {
      this.currentTime = this.player?.getCurrentTime() || 0;

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
      el.style.transform = `scale(${Math.min(widthScale, heightScale)})` + 'translate(-50%, -50%)';
    }
  };

  protected updated(changedProperties: PropertyValues): void {
    if (this.player) {
      const mContainer = Number(getComputedStyle(this.replayerOuterContainer).width.replace('px', ''));

      if (changedProperties.has('activityWidth')) {
        this.containerWidth = mContainer - this.activityWidth;
        this.updateScale();
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

  initiatePlayer(events: eventWithTime[]) {
    const target = document.querySelector('#playerWrapper') as HTMLElement;
    this.player?.destroy();
    events.forEach((event) => {
      if (event.type === EventType.Plugin && event.data.plugin === 'rrweb/console@1') {
        const level = (event as ConsoleEvent).data.payload.level;
        this.consoleTypesCounts[level] += 1;
        this.consoleEvents = [...this.consoleEvents, event as ConsoleEvent];
      }
    });
    if (events.length < 2) {
      console.error('Need at least two events to play session');
      return;
    }
    this.player = new Replayer(events, {
      root: target,
      plugins: [{ handler: this.handleConsoleEvents }],
      skipInactive: this.skipInactive,
    });
    this.metaData = this.player.getMetaData();
    this.updateScale();
    this.play();
  }

  fetchNewSessionData(sessionId: string) {
    if (this.isLoading) return;
    this.isLoading = true;
    this.player?.destroy();
    const url = `/p/${this.projectId}/replay_session/${sessionId}`;

    fetch(url, { method: 'GET', headers: { Accept: 'application/json' } })
      .then((response) => response.json())
      .then((data) => {
        const events = data.events;
        this.initiatePlayer(events);
      })
      .finally(() => {
        this.isLoading = false;
      });
  }

  protected firstUpdated(_changedProperties: PropertyValues): void {
    const mContainer = Number(getComputedStyle(this.replayerOuterContainer).width.replace('px', ''));
    this.containerWidth = mContainer - this.activityWidth;

    const events = JSON.parse(localStorage.getItem('qq') || '[]') as eventWithTime[];
    this.initiatePlayer(events);
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
        bgColor = 'bg-red-100';
        textColor = 'text-textError';
        hoverColor = 'hover:bg-fillError-weak';
        break;
      case 'warn':
        bgColor = 'bg-yellow-100';
        textColor = 'text-textWarn';
        hoverColor = 'hover:bg-fillWarn-weak';
        break;
      default:
        bgColor = 'bg-blue-100';
        textColor = 'text-textInfo';
        hoverColor = 'hover:bg-fillBrand-weak';
        break;
    }

    return html`
      <div class="text-sm flex flex-col min-w-0 event-container">
        <div class="flex items-center w-full">
          <span class="h-2 w-2 shrink-0 rounded-full ${this.currentEventTime === event.timestamp ? 'bg-blue-500' : ''} ml-1"></span>
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
            ${faSprite_('chevron-up', 'regular', 'w-2.5 h-2.5 text-textWeak')}
          </button>
        </div>
        <div class="flex-col ${bgColor} p-2 w-full min-w-0 hidden event-detail">
          <div class="bg-bgBase rounded-xl p-3 whitespace-pre-wraps relative ${textColor} font-medium">
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
          <div class="bg-bgBase rounded-xl p-3 whitespace-pre-wraps relative font-medium">
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
    return html`<div class="flex overflow-x-hidden " id="replayerOuterContainer" style="width:1124px; height:750px">
      <div class="w-full h-full flex flex-col justify-start shrink-1 min-w-0 overflow-x-hidden">
        <div class="bg-fillWeak w-full px-2 h-10 flex items-center border-b gap-4 justify-between playerHeader">
          <div class="flex items-center gap-4 shrink-1">
            <h3 class="font-medium h-full truncate overflow-ellipsis min-w-0">Session recording</h3>
          </div>

          <div class="flex items-center gap-4 text-xs font-semibold">
            <div class="dropdown">
              <div tabindex="0" role="button" class="cursor-pointer flex items-center gap-1 ${this.playSpeed != 1 ? 'text-blue-500' : ''}">
                ${faSprite_('gauge', 'regular', 'w-3 h-3')} Speed ${this.playSpeed}x
              </div>
              <ul tabindex="0" class="dropdown-content menu bg-base-100 border text-xs rounded-box z-1 w-max p-2 shadow">
                ${[0.5, 1, 1.5, 2, 3, 4, 8, 16].map(
                  (s) =>
                    html`<li>
                      <button
                        class="px-4 rounded py-1 hover:bg-fillWeak ${this.playSpeed == s ? 'bg-blue-500 text-white' : ''}"
                        @click=${() => (this.playSpeed = s)}
                      >
                        ${s}x
                      </button>
                    </li>`
                )}
              </ul>
            </div>
            <button
              class="flex items-center cursor-pointer gap-1 ${this.skipInactive ? 'text-blue-500' : ''}"
              @click=${() => (this.skipInactive = !this.skipInactive)}
            >
              ${faSprite_('skip', 'regular', 'w-3 h-3')}
              <span>Skip inactive</span>
            </button>
            <button
              @click=${() => {
                this.activityWidth = this.activityWidth <= 0 ? 300 : 0;
              }}
              class="cursor-pointer flex items-center gap-1 ${this.activityWidth > 0 ? 'text-blue-500' : ''}"
            >
              ${faSprite_('side-chevron-left-in-box', 'regular', 'w-3 h-3')} Activity
            </button>
          </div>
        </div>
        <!-- End nav controls -->
        <div class="relative" style="height:${this.containerHeight + 80}px">
          <div class="border-b relative bg-black">
            <div
              class="player-frame border-y overflow-hidden"
              id="playerWrapper"
              style="height:${this.containerHeight}px; width: ${this.containerWidth}px"
            ></div>
            <div
              class="absolute inset-0 flex bg-black opacity-25 items-center  justify-center ${this.paused || this.finished || this.isLoading
                ? ''
                : 'hidden'}"
            >
              ${this.isLoading
                ? html`<div class="italic text-7xl font-medium text-red-500">Loading...</div>`
                : this.finished
                ? html` <button @click=${() => this.goTo(0)}>${faSprite_('replay', 'regular', 'w-14 h-14 text-textInverse-weak')}</button> `
                : html`
                    <button @click=${() => (this.paused = false)}>
                      ${faSprite_('p-play', 'regular', 'w-14 h-14 text-textInverse-weak')}
                    </button>
                  `}
            </div>
          </div>
          <div class="flex p-2  items-center justify-between">
            <div>
              <div class="text-xs flex gap-0.5 font-mono font-medium flex-nowrap w-max">
                <span>${SessionReplay.formatTime(this.currentTime)}</span>
                <span>/</span>
                <span>${SessionReplay.formatTime(this.metaData.totalTime)}</span>
              </div>
            </div>
            <div class="w-full gap-3 flex items-center justify-center">
              <button class="relative" @click=${() => this.goTo(this.currentTime - MS_10)}>
                <span style="font-size:8px" class="absolute top-1/2  left-1/2 -translate-x-1/2 -translate-y-1/2 font-semibold">10</span>
                ${faSprite_('time-skip', 'regular', 'h-5 w-5')}
              </button>
              ${this.finished
                ? html`<button class="flex justify-center items-center" @click=${() => this.goTo(0)}>
                    ${faSprite_('replay', 'regular', 'h-5 w-5')}
                  </button>`
                : html`
                    <button class="flex justify-center items-center" @click=${() => (this.paused = !this.paused)}>
                      ${this.paused ? faSprite_('p-play', 'regular', 'h-5 w-5') : faSprite_('p-pause', 'regular', 'h-5 w-5')}
                    </button>
                  `}
              <button class="relative" @click=${() => this.goTo(this.currentTime + MS_10)}>
                <span style="font-size: 8px" class="absolute top-1/2  left-1/2 -translate-x-1/2 -translate-y-1/2 font-semibold">10</span>
                ${faSprite_('time-skip', 'regular', 'h-5 w-5 rotate-y-180')}
              </button>
            </div>
            <div></div>
          </div>
        </div>
      </div>

      <div class="shrink-0 h-full relative flex items-start border-l" id="replay-activity-bar" style="width:${this.activityWidth}px">
        <div
          class="w-1 h-full absolute z-10 left-0 top-0 hover:bg-blue-400"
          @mousedown=${(e: any) => {
            document.body.style.userSelect = 'none';
            this.startX = e.clientX;
          }}
        ></div>
        <div class="w-full h-full overflow-hidden">
          <div class="bg-fillWeak w-full px-4 h-10  flex items-center border-b gap-4 justify-between playerHeader">
            <div class="flex items-center gap-4 text-xs font-semibold">
              <div class="dropdown">
                <div tabindex="0" role="button" class="cursor-pointer">Console</div>
                <ul tabindex="0" class="dropdown-content menu space-y-1 text-xs bg-base-100 border w-max rounded-box z-1 p-1 shadow">
                  <li>
                    <button
                      class="px-4 hover:bg-fillweak py-1 ${this.consoleEventsEnable[0] ? 'bg-blue-100 text-blue-700' : ''}"
                      @click=${() => this.toggleConsoleEvent(0)}
                    >
                      Error (${this.consoleTypesCounts.error})
                    </button>
                  </li>
                  <li>
                    <button
                      class="px-4 rounded hover:bg-fillweak py-1 ${this.consoleEventsEnable[1] ? 'bg-blue-100 text-blue-700' : ''}"
                      @click=${() => this.toggleConsoleEvent(1)}
                    >
                      Warn (${this.consoleTypesCounts.warn})
                    </button>
                  </li>
                  <li>
                    <button
                      class="px-4 rounded hover:bg-fillweak py-1 ${this.consoleEventsEnable[2] ? 'bg-blue-100 text-blue-700' : ''}"
                      @click=${() => this.toggleConsoleEvent(2)}
                    >
                      Info (${this.consoleTypesCounts.info})
                    </button>
                  </li>
                </ul>
              </div>
              <div class="dropdown">
                <div tabindex="0" role="button" class="cursor-pointer">Network</div>
                <ul tabindex="0" class="dropdown-content menu bg-base-100 rounded-box z-1 w-52 p-2 shadow-sm">
                  <li><a>Item 1</a></li>
                  <li><a>Item 2</a></li>
                </ul>
              </div>
            </div>
          </div>
          <div class="flex flex-col h-full overflow-y-auto w-full overflow-x-hidden c-scroll" style="height:calc(100% - 40px)">
            ${this.consoleEvents.map((e) => this.displayConsoleEvent(e))}
          </div>
        </div>
      </div>
    </div>`;
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
        // Prevent text selection during drag
        // e.preventDefault();
      })
    );

    const onMouseMove = (e: any) => {
      if (isDragging) {
        element.style.left = Math.max(e.clientX - offsetX, 0) + 'px';
        element.style.top = Math.max(e.clientY - offsetY, 0) + 'px';
      }
    };

    const onMouseUp = () => {
      isDragging = false;
      document.body.style.userSelect = '';
      document.removeEventListener('mousemove', onMouseMove);
      document.removeEventListener('mouseup', onMouseUp);
    };
  }
}
